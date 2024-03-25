interface Queue
    exposes [empty, toList, getAt, setAt, len, enqueue, dequeue]
    imports []

empty = \capacity -> 
    {
        data: List.withCapacity capacity,
        front: 0,
        back: 0,
        len: 0,
        capacity
    } 

toList = .data

getAt = \q, index ->
    List.get q.data index

setAt = \q, index, value ->
    data = List.set q.data index value
    {q & data}

len = .len

enqueue = \ q, element ->
    if q.len == q.capacity then
        Err QueueWasFull
    else
        newData = 
            if List.len q.data < q.capacity then
                List.append q.data element
            else
                List.set q.data q.back element
        Ok {q & data: newData, len: q.len + 1, back: (q.back + 1) % q.capacity}

dequeue = \q ->
    if q.len == 0 then
        Err QueueWasEmpty
    else
        newQueue = { q & front: (q.front + 1) % q.capacity, len: q.len - 1 } 
        element =
            when List.get q.data q.front is
                Err OutOfBounds -> crash "front of queue was pointing outside of the queue"
                Ok elem -> elem
        Ok (newQueue, element)

expect
    queue = empty 1 |> enqueue A |> Result.try \q -> q |> enqueue B
    queue == Err QueueWasFull

expect
    queue = empty 1 |> dequeue
    queue == Err QueueWasEmpty

expect
    capacity = 1
    queue = empty capacity |> enqueue A 
    queue == Ok { data: [A], front: 0, back: 0, len: 1, capacity: capacity}

expect
    capacity = 1
    dequeued = empty capacity |> enqueue A |> Result.try \q -> q |> dequeue
    dequeued == Ok ({ data: [A], front: 0, back: 0, len: 0, capacity: capacity}, A)

#en, deq, deq

# en, enq
expect
    capacity = 1
    queue = 
        q <- empty capacity |> enqueue A |> Result.try
        (deq, _) <- q |> dequeue |> Result.try 
        deq |> enqueue B
    queue == Ok { data: [B], front: 0, back: 0, len: 1, capacity: capacity}
