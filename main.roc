app "des"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    imports [pf.Task, PQueue.{ dequeue, enqueue, empty }]
    provides [main] to pf

interArrivalTime = 50
timeWhenGeneratingStops = 100
interactionTime = 5
initialPatients = [
    { id: 0, arrivalTime: 0, state: Sick },
    { id: 1, arrivalTime: 0, state: Sick },
]

main =
    eventQueue = empty |> enqueue { time: 0, type: GeneratesPatient 2 }
    world = {
        time: 0,
        patients: initialPatients,
        eventQueue,
    }
    simRes = executeEvents world
    dbg simRes

    Task.ok {}

executeEvents = \world ->
    if world.time > timeWhenGeneratingStops then
        world
    else
        when world.eventQueue |> dequeue is
            Err QueueWasEmpty -> world
            Ok (newQueue, e) ->
                newWorld = handleEvent e { world & time: e.time, eventQueue: newQueue }
                executeEvents newWorld

handleEvent = \event, world ->
    { time, type } = event
    when type is
        PatientInteracts patientId ->
            newWorld = 
                #not random
                sourceRes = List.walkWithIndexUntil world.patients (Err NoFound) \state, elem, idx -> 
                    if elem.id == patientId then 
                        Break (Ok (idx, elem))
                    else
                        Continue state

                #random
                targetRes = List.walkWithIndexUntil world.patients (Err NoFound) \state, elem, idx -> 
                    if elem.id != patientId then 
                        Break (Ok (idx, elem))
                    else
                        Continue state
                (sourceIdx, source) <- sourceRes |> Result.try
                (targetIdx, target) <- targetRes |> Result.try
                #dbg Interaction source target
                when (source.state, target.state) is
                    (Healthy, Sick) -> 
                        newPatients = List.set world.patients sourceIdx {source & state: Sick }
                        Ok { world & patients: newPatients}
                    (Sick, Healthy) -> 
                        newPatients = List.set world.patients targetIdx {target & state: Sick }
                        Ok { world & patients: newPatients}
                    _ -> Ok world
            when newWorld is
                Err _ -> world
                Ok wrld -> wrld
        GeneratesPatient id ->
            if world.time < timeWhenGeneratingStops then
                newPatients = List.append world.patients { id, arrivalTime: time, state: Healthy }
                #random
                queueWithInteraction = world.eventQueue |> enqueue { time: time + interactionTime, type: PatientInteracts id}
                #random
                newQueue = queueWithInteraction |> enqueue { time: time + interArrivalTime, type: GeneratesPatient (id + 1)}
                { world & eventQueue: newQueue, patients: newPatients}
            else
                world
