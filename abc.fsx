open System
open System.Collections.Generic

#r @"./packages/Microsoft.Tpl.Dataflow/lib/portable-net45+win8+wp8+wpa81/System.Threading.Tasks.Dataflow.dll"
open System.Threading.Tasks.Dataflow
open System.Threading.Tasks


module AAA =

    let createDelayBlock<'T>(delay:TimeSpan) : IPropagatorBlock<'T,Async<'T>>   =

        let lastItem: DateTime = DateTime.MinValue
        
        let f x = async {

            let waitTime = lastItem + delay - DateTime.UtcNow
            if    waitTime > TimeSpan.Zero
            then  Async.AwaitTask ( System.Threading.Tasks.Task.Delay(waitTime) ) |> ignore

            return x
        }

        TransformBlock<'T, Async<'T>>(f, (new ExecutionDataflowBlockOptions( BoundedCapacity = 1 )  ) )
        :> IPropagatorBlock<'T,Async<'T>>

    let Producer ( target:ITargetBlock<int> ) : Task<bool> =
        let mutable i:int = 0
        while Async.AwaitTask (target.SendAsync(i)) |> Async.RunSynchronously do 
            i <- i + i
        target.SendAsync(9999)
        
    let Consumer( i:Async<int> ) : unit  = 
        stdout.WriteLine( i |> Async.RunSynchronously) 

    let delayBlock = createDelayBlock<int>(TimeSpan.FromMilliseconds(500.))
    let consumerBlock : ITargetBlock<Async<int>> =
        new ActionBlock<Async<int>>( Consumer, (new ExecutionDataflowBlockOptions( MaxDegreeOfParallelism = DataflowBlockOptions.Unbounded ) ) ) 
        :> ITargetBlock<Async<int>>
    delayBlock.LinkTo(consumerBlock, new DataflowLinkOptions( PropagateCompletion = true ) )
    System.Threading.Tasks.Task.WaitAll(Producer(delayBlock), consumerBlock.Completion)











