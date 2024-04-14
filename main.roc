app "des"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        random: "./roc-random/package/main.roc",
    }
    imports [pf.Stdout, random.Random, PQueue, Queue]
    provides [main] to pf

timeAfterGeneratingStops = 100
examinationTime = 4
contactTime = 5
interArrivalTime = 2
defaultWorld = 
    time = 0
    seed = 42
    availableDoctors = 1
    waitingRoomCapacity = 50
    {
        time,
        random: Random.seed seed,
        availableDoctors,
        patientsWaiting: Queue.empty waitingRoomCapacity,
        events: PQueue.empty {} |> PQueue.enqueue { time: 0, type: GeneratesPatient 0 },
        patientsProcessed: [],
    }

main =
    simulatedWorld = executeEvents defaultWorld
    processResults simulatedWorld

processResults = \{ patientsProcessed: patients, time } ->
    healthyCount = List.countIf patients \p -> p.state == Healthy
    infectedCount = List.countIf patients \p -> p.state == Infected
    healthyAtArrivalCount = healthyCount + infectedCount
    infectedRatioRaw = Num.toFrac infectedCount / Num.toFrac healthyAtArrivalCount
    # workaround for the lack of Frac to Str formatting
    infectedRatioFormatted = infectedRatioRaw * 10000 |> Num.round |> Num.toFrac |> Num.div 100

    patientCount = List.len patients
    avgWaitTime =
        waitTimes = List.map patients \p -> p.processedAt - p.arrivedAt
        (List.sum waitTimes |> Num.toFrac) / (Num.toFrac patientCount)

    report =
        """
        Processed $(Num.toStr patientCount) patients in $(Num.toStr time) minutes.
        $(Num.toStr healthyAtArrivalCount) arrived healthy, $(Num.toStr infectedCount) were infected while waiting, which is $(Num.toStr infectedRatioFormatted)% of the healthy arrivals """
    Stdout.line report

executeEvents = \world ->
    nextEvent = world.events |> PQueue.dequeue
    when nextEvent is
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
        Err (SourceNotFound newRandom) -> { world & random: newRandom }
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
        (Healthy, Sick) | (Healthy, Infected) -> Queue.setAt patients sourceIdx { source & state: Infected }
        (Sick, Healthy) | (Infected, Healthy) -> Queue.setAt patients targetIdx { target & state: Infected }
        _ -> patients

handleGeneratesPatient = \world, id ->
    { time, events, random } = world
    if time > timeAfterGeneratingStops then
        world
    else
        # could be random
        generationEvent = { time: time + interArrivalTime, type: GeneratesPatient (id + 1) }
        queueWithGeneration = events |> PQueue.enqueue generationEvent
        worldWithGeneration = { world & events: queueWithGeneration }

        randomGen = Random.u32 0 1
        { state: newRandom, value: healthyOrSick } = randomGen random
        state =
            if healthyOrSick == 0 then
                Sick
            else
                Healthy

        patient = { id, arrivedAt: time, state }
        worldWithNewRandom = { worldWithGeneration & random: newRandom }

        patientArrived worldWithNewRandom patient

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

    patientDetails = { state: patient.state, arrivedAt: patient.arrivedAt, processedAt: time }
    newPatientsProcessed = List.append patientsProcessed patientDetails
    worldWithDoneProcessing = { world & availableDoctors: availableDoctors + 1, patientsProcessed: newPatientsProcessed }
    when Queue.dequeue patientsWaiting is
        Ok (newQueue, patientNextInLine) -> startProcessingPatient { worldWithDoneProcessing & patientsWaiting: newQueue } patientNextInLine
        Err QueueWasEmpty -> worldWithDoneProcessing

