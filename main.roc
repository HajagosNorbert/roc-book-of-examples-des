app "des"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        random: "./roc-random/package/main.roc",
    }
    imports [pf.Task, random.Random, PQueue, Queue]
    provides [main] to pf

seed = 42
interArrivalTime = 50
examinationTime = 4
timeWhenGeneratingStops = 100
initialAvailableDoctors = 1
contactTime = 5
initialPatients =
    res =
        q1 <- Queue.empty 100 |> Queue.enqueue { id: 0, arrivedAt: 0, state: Sick } |> Result.try
        q1 |> Queue.enqueue { id: 1, arrivedAt: 0, state: Sick }
    when res is
        Ok a -> a
        Err QueueWasFull -> crash "we know at comp time this isn't happening"

initialEvents = PQueue.empty |> PQueue.enqueue { time: 0, type: GeneratesPatient 2 }

initialWorld = {
    time: 0,
    patientsWaiting: initialPatients,
    events: initialEvents,
    availableDoctors: initialAvailableDoctors,
    patientsProcessed: [],
    random: Random.seed seed,
}

main =
    simResult = executeEvents initialWorld
    dbg simResult
    processResults simResult

processResults = \simResult ->
    dbg simResult.patientsProcessed
    Task.ok {}

executeEvents = \world ->
    if world.time > timeWhenGeneratingStops then
        world
    else
        when world.events |> PQueue.dequeue is
            Err QueueWasEmpty -> world
            Ok (newQueue, e) ->
                newWorld = handleEvent e { world & time: e.time, events: newQueue }
                executeEvents newWorld

handleEvent = \{ type: eventType }, world ->
    when eventType is
        PatientContacts id -> handlePatientContacts world id
        GeneratesPatient id -> handleGeneratesPatient world id
        PatientProcessed patient -> handlePatientProcessed world patient

handlePatientContacts = \world, id ->
    choice = chooseContact world id
    when choice is
        Err TooFewPatients -> world
        Err (SourceNotFound r) -> { world & random: r }
        Ok { patientChoices, newRandom } ->
            patientsAfterContact = contactPatients world.patientsWaiting patientChoices
            { world &
                random: newRandom,
                patientsWaiting: patientsAfterContact,
            }

chooseContact = \{ patientsWaiting, random }, sourcePatientId ->
    patientsWaitingCount = Queue.len patientsWaiting
    if patientsWaitingCount < 2 then
        Err TooFewPatients
    else
        lastPatientIdx = Num.toU32 (patientsWaitingCount - 1)
        randomGen = Random.u32 0 lastPatientIdx
        { state: newRandom, value: targetIdxU32 } = randomGen random
        targetIdx = Num.toU64 (targetIdxU32)
        target =
            when Queue.getAt patientsWaiting targetIdx is
                Ok patient -> patient
                Err OutOfBounds -> crash "random index generated incorrectly"

        src = Queue.find patientsWaiting (\patient -> patient.id == sourcePatientId)
        when src is
            Err NotFound -> Err (SourceNotFound newRandom)
            Ok (sourceIdx, source) -> Ok { newRandom, patientChoices: { sourceIdx, source, target, targetIdx } }

contactPatients = \patients, { sourceIdx, source, targetIdx, target } ->
    when (source.state, target.state) is
        (Healthy, Sick) -> Queue.setAt patients sourceIdx { source & state: Sick }
        (Sick, Healthy) -> Queue.setAt patients targetIdx { target & state: Sick }
        _ -> patients

handleGeneratesPatient = \world, id ->
    { time, events, random } = world
    if time < timeWhenGeneratingStops then
        # could be random
        generationEvent = { time: time + interArrivalTime, type: GeneratesPatient (id + 1) }
        queueWithGeneration = events |> PQueue.enqueue generationEvent
        worldWithGeneration = { world & events: queueWithGeneration }

        randomGen = Random.u32 0 1
        {state: newRandom, value: healthyOrSick} = randomGen random
        state = 
            if healthyOrSick == 0 then
                Sick
            else
                Healthy

        patient = { id, arrivedAt: time, state }
        worldWithNewRandom = {worldWithGeneration & random: newRandom}

        patientArrived worldWithNewRandom patient
    else
        world

patientArrived = \world, patient ->
    { time, patientsWaiting, events } = world
    when tryProcessingPatient world patient is
        Ok worldWithProcessedPatient -> worldWithProcessedPatient
        Err NoAvailableDoctors ->
            newPatients = Queue.enqueue patientsWaiting patient |> Result.withDefault patientsWaiting
            # could be random
            contactEvent = { time: time + contactTime, type: PatientContacts patient.id }
            queueWithContact = events |> PQueue.enqueue contactEvent
            { world & events: queueWithContact, patientsWaiting: newPatients }

tryProcessingPatient = \world, patient ->
    { availableDoctors } = world
    if availableDoctors > 0 then
        Ok (startProcessingPatient world patient)
    else
        Err NoAvailableDoctors

startProcessingPatient = \world, patient ->
    { time, events, availableDoctors } = world
    patientProcessedEvent = { time: time + examinationTime, type: PatientProcessed patient }
    newEventQueue = events |> PQueue.enqueue patientProcessedEvent
    { world & events: newEventQueue, availableDoctors: availableDoctors - 1 }

handlePatientProcessed = \world, patient ->
    { patientsWaiting, patientsProcessed, availableDoctors, time } = world

    newPatientsProcessed = List.append patientsProcessed {patient & processedAt: time }
    worldWithDoneProcessing = { world & availableDoctors: availableDoctors + 1, patientsProcessed: newPatientsProcessed }
    when Queue.dequeue patientsWaiting is
        Ok (newQueue, patientNextInLine) -> startProcessingPatient { worldWithDoneProcessing & patientsWaiting: newQueue } patientNextInLine
        Err QueueWasEmpty -> worldWithDoneProcessing
