interface PQueue
    exposes [empty, enqueue, dequeue]
    imports []

empty = []

enqueue = \q, element ->
    List.append q element

dequeue = \q ->
    qTime = List.map q .time
    res =
        minTime <- List.min qTime |> Result.try
        minIdx <- List.findFirstIndex q (\event -> event.time == minTime) |> Result.try
        element <- List.get q minIdx |> Result.map
        (List.dropAt q minIdx, element)
    res |> Result.mapErr (\_ -> QueueWasEmpty)

expect dequeue events == Ok ([{ time: 3 }, { time: 5 }], { time: 1 })
expect enqueue [] { time: 1 } == [{ time: 1 }]

expect
    queue = empty |> enqueue {time : 2} |> enqueue {time: 1} |> enqueue {time: 3}
    List.map queue .time == [1, 2, 3]

events = [
    { time: 1 },
    { time: 3 },
    { time: 5 },
]
