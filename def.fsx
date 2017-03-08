
open System
open System.Threading

// Type for the producer/consumer pattern. Both producers and consumers
// can be multiple and run in parallel.
// Construct with a sequence of input sequences, which are the producers and 
// will be enumerated in parallel, and the queue size.
type ProducerConsumer<'T>(inputSeqs : seq<seq<'T>>, qSize : int) as this =
    let queue            = Array.create qSize (None : option<'T>)  // The queue is a wrap-around array.
    let qNextItemToRead  = ref 0  // Next item no to read from the input stream.
    let qNextItemToQueue = ref 0  // Next item no to put in the queue.
    let emptyCount       = new Semaphore(qSize, qSize)  // Empty slots in the queue.
    let fullCount        = new Semaphore(0, qSize)      // Filled slots in the queue.
    let endOfInput       = new EventWaitHandle(false, EventResetMode.ManualReset)
    let canceled         = new EventWaitHandle(false, EventResetMode.ManualReset)

    // Copied from F# source code for Seq.takeWhile and modified so that p's
    // argument is unit and p is checked before e.MoveNext instead of after.
    let takeWhilePre p (source: seq<_>) = 
        seq { use e = source.GetEnumerator() 
              let latest = ref Unchecked.defaultof<_>
              while p() && e.MoveNext() do
                  latest := e.Current
                  yield !latest }

    let produce (inputSeq : seq<'T>) =
        let cancellableSeq = inputSeq |> takeWhilePre (fun _ -> not this.IsCanceled)
        for item in cancellableSeq do
            let isCanceled = WaitHandle.WaitAny([|canceled; emptyCount|]) = 0
            if not isCanceled then
                let itemToQueue = System.Threading.Interlocked.Increment(&qNextItemToQueue.contents) - 1
                queue.[itemToQueue % qSize] <- Some item
                fullCount.Release() |> ignore

    let produceAll (inputSeqs : seq<seq<'T>>) =
        [ for i in inputSeqs -> async { produce i } ]
        |> Async.Parallel
        |> Async.Ignore
        |> Async.RunSynchronously
        endOfInput.Set() |> ignore

    do Async.Start (async { produceAll inputSeqs })

    member x.Cancel() = canceled.Set() |> ignore
    member x.IsCanceled = canceled.WaitOne(0)

    // Get the next item.
    // Returns None if the ProducerConsumer is canceled or there are no more items,
    // oterwise returns Some 'T.
    member x.ConsumeOne() =
        // The order of WaitHandles in WaitAny arg is important.
        let waitHandleIndex = WaitHandle.WaitAny([|canceled; fullCount; endOfInput|])
        let isCanceledOrEndOfInput = waitHandleIndex = 0 || waitHandleIndex = 2

        if isCanceledOrEndOfInput then
            None
        else 
            let itemToRead = System.Threading.Interlocked.Increment(&qNextItemToRead.contents) - 1
            let result = queue.[itemToRead % qSize]
            queue.[itemToRead % qSize] <- None
            emptyCount.Release() |> ignore
            result

    // Analogous to Seq.fold.
    // Returns a result state for each degree of parallelism.
    member x.FoldConsume(folder, state, degreeOfParallelism) =
        let rec consumeLoop folder state =
            let item = x.ConsumeOne()
            match item with
            | Some i ->
                let state' = folder state i
                consumeLoop folder state'
            | None -> state

        [ for parallelSlot in 0..degreeOfParallelism-1 -> async { return (consumeLoop folder state) } ]
        |> Async.Parallel
        |> Async.RunSynchronously

    // Analogous to List.map.
    // If degreeOfParallelism = 1 and there is a single input sequence, the 
    // results are in the same order as the input sequence, otherwise the
    // order is nondeterministic.
    member x.MapConsume(mapping, degreeOfParallelism) =
        let result =
            x.FoldConsume((fun acc i -> (mapping i)::acc), [], degreeOfParallelism)
            |> List.concat
        if degreeOfParallelism = 1 then result |> List.rev else result

    // Analogous to Seq.iter.
    member x.IterConsume(action, degreeOfParallelism) =
        x.FoldConsume((fun _ i -> action i), (), degreeOfParallelism)
        |> ignore

let mutexPrintLock = new Object()
let mutexPrint s = lock mutexPrintLock (fun () -> printf "%s" s)

let producerSeq items (sleepTime : int) = 
    seq { for i in items do
                mutexPrint (sprintf "Generating value %d.\n" i)
                Thread.Sleep(sleepTime)  // Simulate work, e.g. database access.
                yield i }

let list1 = [1..20]
let list2 = [1000..1000..4000]

let pc = ProducerConsumer([producerSeq list1 500; producerSeq list2 2000], 8)

// Uncomment to try cancellation.
// let cancelTimer = new Timer((fun _ -> mutexPrint "Cancelling!\n"; pc.Cancel()), null, 4900, Timeout.Infinite)

let consumer i =
    mutexPrint (sprintf "Thread %d got %d from the queue.\n" Thread.CurrentThread.ManagedThreadId i)
    Thread.Sleep(3000)  // Simulate work.
    i

let result = pc.MapConsume(consumer, 4)
let sortedResult = result |> List.sort

printfn "\nResult: %A" result
printfn "Sorted result: %A\n" sortedResult

if sortedResult = list1 @ list2 then printfn "Results test OK."
else printfn "Incorrect results generated."
