interface PQueue
    exposes [empty, enqueue, dequeue]
    imports []

# important: tell why this is a fn, not a constant
empty = \ {} -> []

enqueue = \q, item ->
    newQueue = List.append q item
    prioritizeAfterEnqueuing newQueue item

prioritizeAfterEnqueuing =\ initialQueue, enqueuedItem-> 
    prioritizeItemAt =\ q, item, idx ->
        if idx == 0 then
            q
        else
            parentIdx = ((idx - 1) // 2)
            parentItemRes = List.get q parentIdx
            when parentItemRes is
                Err OutOfBounds -> crash "Out of bounds. Call this function through"
                Ok parentItem -> 
                    if item.time < parentItem.time then
                        newQueue = swap q {item, idx} {item: parentItem, idx: parentIdx}
                        prioritizeItemAt newQueue parentItem parentIdx
                    else
                        q
    enqueuedItemIdx = ((List.len initialQueue) - 1)
    prioritizeItemAt initialQueue enqueuedItem enqueuedItemIdx

swap = \q, {item: item1, idx: idx1}, {item: item2, idx: idx2} ->
    q |> List.set idx1 item2 |> List.set idx2 item1

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
    queue = empty {} |> enqueue {time : 2} |> enqueue {time: 1} |> enqueue {time: 3}
    when dequeue queue is
        Err _ -> Bool.false
        Ok (_, item) ->
            item == {time: 1}

events = [
    { time: 1 },
    { time: 3 },
    { time: 5 },
]
