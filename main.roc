app "des"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br"
    }
    imports [pf.Task, PQueue, Queue]
    provides [main] to pf

interArrivalTime = 50
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
        patients: initialPatients,
        eventQueue,
        availableDoctors: 1,
        #waitingLine: 
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

handleEvent = \{type: eventType}, world ->
    when eventType is
        PatientInteracts id -> handlePatientInteracts world id
        GeneratesPatient id -> handleGeneratesPatient world id

handlePatientInteracts = \ world, id-> 
    {patients} = world
    patientList = Queue.toList patients
    newWorld = 
        #not random
        sourceRes = List.walkWithIndexUntil patientList (Err NoFound) \state, elem, idx -> 
            if elem.id == id then 
                Break (Ok (idx, elem))
            else
                Continue state

        #random
        targetRes = List.walkWithIndexUntil patientList (Err NoFound) \state, elem, idx -> 
            if elem.id != id then 
                Break (Ok (idx, elem))
            else
                Continue state

        (sourceIdx, source) <- sourceRes |> Result.try
        (targetIdx, target) <- targetRes |> Result.try

        when (source.state, target.state) is
            (Healthy, Sick) -> 
                newPatients = Queue.setAt patients sourceIdx {source & state: Sick }
                Ok { world & patients: newPatients}
            (Sick, Healthy) -> 
                newPatients = Queue.setAt patients targetIdx {target & state: Sick }
                Ok { world & patients: newPatients}
            _ -> Ok world
    when newWorld is
        Err _ -> world
        Ok modifiedWorld -> modifiedWorld

handleGeneratesPatient = \ world, id ->
    {time, patients, eventQueue} = world 
    if time < timeWhenGeneratingStops then
        patient = { id, arrivalTime: time, state: Healthy }
        #random
        newPatients = Queue.enqueue patients patient |> Result.withDefault patients

        #random
        queueWithInteraction = eventQueue |> PQueue.enqueue { time: time + interactionTime, type: PatientInteracts id}

        #random
        newQueue = queueWithInteraction |> PQueue.enqueue { time: time + interArrivalTime, type: GeneratesPatient (id + 1)}

        { world & eventQueue: newQueue, patients: newPatients}
    else
        world

