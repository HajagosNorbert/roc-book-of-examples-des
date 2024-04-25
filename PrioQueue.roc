interface PrioQueue
    exposes [empty, enqueue, dequeue]
    imports []

# important: tell why this is a fn, not a constant
# todo: make them have a custom comperator fn that is used for prioritizing

PrioQueue item a := { data : List item, priority : item -> Num a }

empty = \priority -> @PrioQueue { data: [], priority }

enqueue = \@PrioQueue { data: q, priority }, item ->
    enqueuedItemIdx = List.len q
    newQueue = List.append q item
    @PrioQueue { data: heapifyUpAt newQueue item enqueuedItemIdx, priority }

heapifyUpAt = \q, item, idx ->
    if idx == 0 then
        List.set q idx item
    else
        parentIdx = ((idx - 1) // 2)
        parentItemRes = List.get q parentIdx
        when parentItemRes is
            Err OutOfBounds -> crash "Unexpected out of bounds"
            Ok parentItem ->
                if item.time < parentItem.time then
                    newQueue = List.set q idx parentItem
                    heapifyUpAt newQueue item parentIdx
                else
                    List.set q idx item

dequeue = \@PrioQueue { data: q, priority } ->
    queueAndItemRes =
        firstItem <- List.first q |> Result.try
        lastItem <- List.last q |> Result.map
        newQueue = @PrioQueue { data: (q |> List.set 0 lastItem |> List.dropLast 1 |> heapifyDown), priority }
        (newQueue, firstItem)
    queueAndItemRes |> Result.mapErr \_ -> QueueWasEmpty

heapifyDown = \initialQueue ->
    heapifyDownAt = \q, item, idx ->
        rightIdx = (idx + 1) * 2
        leftIdx = rightIdx - 1
        rightChildRes = List.get q rightIdx
        leftChildRes = List.get q leftIdx
        when (leftChildRes, rightChildRes) is
            (Err _, Ok _) -> crash "Binary heaps are full binary trees. Can't have a righ branch if there isn't a left one"
            (Ok left, Err _) if left.time < item.time ->
                newQueue = List.set q idx left
                heapifyDownAt newQueue item leftIdx

            (Ok left, Ok right) if left.time < item.time || right.time < item.time ->
                if left.time <= right.time then
                    newQueue = List.set q idx left
                    heapifyDownAt newQueue item leftIdx
                else
                    newQueue = List.set q idx right
                    heapifyDownAt newQueue item rightIdx

            _ -> List.set q idx item

    when initialQueue is
        [] -> initialQueue
        [head, ..] -> heapifyDownAt initialQueue head 0

# todo: this test seems good, but crashes the compiler on `roc test`
# probably related to [this conversation](https://roc.zulipchat.com/#narrow/stream/231634-beginners/topic/Opaque.20type.20compiler.20bug)
# expect
#     queue = @PrioQueue (List.map [2, 3, 4] \time -> {time})
#     actual = queue |> enqueue {time: 1}
#     expected = List.map [1, 2, 4, 3] \time -> {time}
#     expected == actual

# expect
#     queue = empty {} |> enqueue { time: 2 }
#     success =
#         (q1, item1) <- dequeue queue |> Result.try
#         q2 = q1 |> enqueue { time: 3 } |> enqueue { time: 4 }
#         (q3, item2) <- dequeue q2 |> Result.try
#         q4 = q3 |> enqueue { time: 5 } |> enqueue { time: 6 }
#         (_, item3) <- dequeue q4 |> Result.map
#         (item1, item2, item3) == ({ time: 2 }, { time: 3 }, { time: 4 })
#     Result.withDefault success Bool.false
