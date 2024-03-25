app "des"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
    }
    imports [pf.Task, PQueue, Queue]
    provides [main] to pf

interArrivalTime = 50
docProcessTime = 4
timeWhenGeneratingStops = 100
interactionTime = 5
initialPatients =
    res =
        q1 <- Queue.empty 100 |> Queue.enqueue { id: 0, arrivalTime: 0, state: Sick } |> Result.try
        q1 |> Queue.enqueue { id: 1, arrivalTime: 0, state: Sick }
    when res is
        Ok a -> a
        Err QueueWasFull -> crash "we know at comp time this isn't happening"

main =
    eventQueue = PQueue.empty |> PQueue.enqueue { time: 0, type: GeneratesPatient 2 }
    world = {
        time: 0,
        patientsWaiting: initialPatients,
        eventQueue,
        availableDoctors: 1,
        patientsProcessed: []
    }
    simRes = executeEvents world
    dbg simRes

    Task.ok {}

executeEvents = \world ->
    if world.time > timeWhenGeneratingStops then
        world
    else
        when world.eventQueue |> PQueue.dequeue is
            Err QueueWasEmpty -> world
            Ok (newQueue, e) ->
                newWorld = handleEvent e { world & time: e.time, eventQueue: newQueue }
                executeEvents newWorld

handleEvent = \{ type: eventType }, world ->
    when eventType is
        PatientInteracts id -> handlePatientInteracts world id
        GeneratesPatient id -> handleGeneratesPatient world id
        PatientProcessed patient -> handlePatientProcessed world patient

handlePatientInteracts = \world, id ->
    { patientsWaiting } = world
    patientList = Queue.toList patientsWaiting
    newWorld =
        # not random
        sourceRes = List.walkWithIndexUntil patientList (Err NoFound) \state, elem, idx ->
            if elem.id == id then
                Break (Ok (idx, elem))
            else
                Continue state

        # random
        targetRes = List.walkWithIndexUntil patientList (Err NoFound) \state, elem, idx ->
            if elem.id != id then
                Break (Ok (idx, elem))
            else
                Continue state

        (sourceIdx, source) <- sourceRes |> Result.try
        (targetIdx, target) <- targetRes |> Result.try

        when (source.state, target.state) is
            (Healthy, Sick) ->
                newPatients = Queue.setAt patientsWaiting sourceIdx { source & state: Sick }
                Ok { world & patientsWaiting: newPatients }

            (Sick, Healthy) ->
                newPatients = Queue.setAt patientsWaiting targetIdx { target & state: Sick }
                Ok { world & patientsWaiting: newPatients }

            _ -> Ok world
    when newWorld is
        Err _ -> world
        Ok modifiedWorld -> modifiedWorld

handleGeneratesPatient = \world, id ->
    { time, patientsWaiting, eventQueue, availableDoctors } = world
    if time < timeWhenGeneratingStops then
        # random
        generationEvent = { time: time + interArrivalTime, type: GeneratesPatient (id + 1) }
        queueWithGeneration = eventQueue |> PQueue.enqueue generationEvent
        worldWithGeneration = {world & eventQueue: queueWithGeneration }

        # random healthy or not
        patient = { id, arrivalTime: time, state: Healthy }
        patientArrived worldWithGeneration patient
    else
        world

patientArrived = \world, patient ->
    { time, patientsWaiting, eventQueue, availableDoctors } = world
    when tryProcessingPatient world patient is
        Ok worldWithProcessedPatient -> worldWithProcessedPatient
        Err NoAvailableDoctors ->
            newPatients = Queue.enqueue patientsWaiting patient |> Result.withDefault patientsWaiting 
            # random
            interactionEvent = { time: time + interactionTime, type: PatientInteracts patient.id }
            queueWithInteraction = eventQueue |> PQueue.enqueue interactionEvent
            {world & eventQueue: queueWithInteraction, patientsWaiting: newPatients }


tryProcessingPatient = \ world, patient ->
    { time, patientsWaiting, eventQueue, availableDoctors } = world
    if availableDoctors > 0 then
        Ok (processPatient world patient)
    else
        Err NoAvailableDoctors

processPatient = \world, patient ->
    { time, patientsWaiting, eventQueue, availableDoctors } = world
    patientProcessedEvent = {time: time + docProcessTime, type: PatientProcessed patient}
    newEventQueue = eventQueue |> PQueue.enqueue patientProcessedEvent
    {world & eventQueue: newEventQueue, availableDoctors: availableDoctors-1}
    

handlePatientProcessed = \world, patient ->
    { patientsWaiting, patientsProcessed, availableDoctors } = world

    newPatientsProcessed = List.append patientsProcessed patient
    worldWithDoneProcessing = {world & availableDoctors: availableDoctors + 1, patientsProcessed: newPatientsProcessed}
    when Queue.dequeue patientsWaiting is
         Ok (newQueue, patientNextInLine) -> processPatient {worldWithDoneProcessing & patientsWaiting: newQueue} patientNextInLine
         Err QueueWasEmpty -> worldWithDoneProcessing
