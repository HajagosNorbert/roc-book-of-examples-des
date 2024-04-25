interface PrioQueue
    exposes [empty, enqueue, dequeue]
    imports []

# important: tell why this is a fn, not a constant
empty = \{} -> []

enqueue = \q, item ->
    newQueue = List.append q item
    prioritizeEnqueuedItem newQueue item

prioritizeEnqueuedItem = \initialQueue, enqueuedItem ->
    prioritizeItemAt = \q, item, idx ->
        if idx == 0 then
            q
        else
            parentIdx = ((idx - 1) // 2)
            parentItemRes = List.get q parentIdx
            when parentItemRes is
                Err OutOfBounds -> crash "Unexpected out of bounds"
                Ok parentItem ->
                    if item.time < parentItem.time then
                        newQueue = swap q { item, idx } { item: parentItem, idx: parentIdx }
                        prioritizeItemAt newQueue item parentIdx
                    else
                        q

    enqueuedItemIdx = ((List.len initialQueue) - 1)
    prioritizeItemAt initialQueue enqueuedItem enqueuedItemIdx

swap = \q, { item: item1, idx: idx1 }, { item: item2, idx: idx2 } ->
    q |> List.set idx1 item2 |> List.set idx2 item1

dequeue = \q ->
    queueAndItemRes =
        firstItem <- List.first q |> Result.try
        lastItem <- List.last q |> Result.map
        newQueue = q |> List.set 0 lastItem |> List.dropLast 1 |> restorePriority
        (newQueue, firstItem)
    queueAndItemRes |> Result.mapErr \_ -> QueueWasEmpty

restorePriority = \initialQueue ->
    restorePriorityAt = \q, idx ->
        when List.get q idx is
            Err OutOfBounds -> q
            Ok itemToPrioritize ->
                rightIdx = (idx + 1) * 2
                leftIdx = rightIdx - 1
                rightItemRes = List.get q rightIdx
                leftItemRes = List.get q leftIdx
                when (leftItemRes, rightItemRes) is
                    (Err OutOfBounds, Ok _) ->
                        crash "Binary heaps (Full binary trees) can't have a righ branch if there isn't a left one"

                    (Ok left, Err OutOfBounds) if left.time < itemToPrioritize.time ->
                        newQueue = swap q { item: itemToPrioritize, idx } { item: left, idx: leftIdx }
                        restorePriorityAt newQueue leftIdx

                    (Ok left, Ok right) if left.time < itemToPrioritize.time || right.time < itemToPrioritize.time ->
                        if left.time <= right.time then
                            newQueue = swap q { item: itemToPrioritize, idx } { item: left, idx: leftIdx }
                            restorePriorityAt newQueue leftIdx
                        else
                            newQueue = swap q { item: itemToPrioritize, idx } { item: right, idx: rightIdx }
                            restorePriorityAt newQueue rightIdx

                    _ -> q
    restorePriorityAt initialQueue 0

expect
    swap [1, 2, 3, 4] { idx: 0, item: 1 } { idx: 3, item: 4 } == [4, 2, 3, 1]

expect
    queue = empty {}
    dequeue queue == Err QueueWasEmpty

expect
    queue = empty {} |> enqueue { time: 2 } |> enqueue { time: 1 } |> enqueue { time: 3 }
    success =
        (q1, item1) <- dequeue queue |> Result.try
        (q2, item2) <- dequeue q1 |> Result.try
        (_, item3) <- dequeue q2 |> Result.map
        (item1, item2, item3) == ({ time: 1 }, { time: 2 }, { time: 3 })
    Result.withDefault success Bool.false

expect
    queue = empty {} |> enqueue { time: 2 }
    success =
        (q1, item1) <- dequeue queue |> Result.try
        q2 = q1 |> enqueue { time: 3 } |> enqueue { time: 4 }
        (q3, item2) <- dequeue q2 |> Result.try
        q4 = q3 |> enqueue { time: 5 } |> enqueue { time: 6 }
        (_, item3) <- dequeue q4 |> Result.map
        (item1, item2, item3) == ({ time: 2 }, { time: 3 }, { time: 4 })
    Result.withDefault success Bool.false
