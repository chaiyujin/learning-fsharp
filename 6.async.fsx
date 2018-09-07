open System

//This shows the use of AutoResetEvent as a synchronization mechanism.
// - A lambda is registered with the Timer.Elapsed event, and when the event is triggered, the AutoResetEvent is signalled.
// - The main thread starts the timer, does something else while waiting, and then blocks until the event is triggered.
// - Finally, the main thread continues, about 2 seconds later.
let userTimerWithCallback = 
    let event = new System.Threading.AutoResetEvent(false)

    let timer = new System.Timers.Timer(2000.0)
    timer.Elapsed.Add( fun _ -> event.Set() |> ignore )

    printfn "Waiting for timer at %O" DateTime.Now.TimeOfDay
    timer.Start()

    printfn "Doing somthing useful while waiting for event"

    event.WaitOne() |> ignore

    printfn "Timer ticked at %O" DateTime.Now.TimeOfDay

userTimerWithCallback


//open Microsoft.FSharp.Control // Async.* is in this module.

// Introducing asynchronous workflows
// F# has a built-in construct called “asynchronous workflows” which makes async code much easier to write. These workflows are objects that encapsulate a background task, and provide a number of useful operations to manage them.
// Here’s the previous example rewritten to use one:
let userTimerWithAsync = 

    // create a timer and associated async event
    let timer = new System.Timers.Timer(2000.0)
    let timerEvent = Async.AwaitEvent (timer.Elapsed) |> Async.Ignore

    // start
    printfn "Waiting for timer at %O" DateTime.Now.TimeOfDay
    timer.Start()

    // keep working
    printfn "Doing something useful while waiting for event"

    // block on the timer event now by waiting for the async to complete
    Async.RunSynchronously timerEvent

    // done
    printfn "Timer ticked at %O" DateTime.Now.TimeOfDay

userTimerWithAsync

let fileWriteWithAsync = 

    // create a stream to write to
    use stream = new System.IO.FileStream("test.txt",System.IO.FileMode.Create)

    // start
    printfn "Starting async write"
    let asyncResult = stream.BeginWrite(Array.empty,0,0,null,null)
    // create an async wrapper around an IAsyncResult
    let async = Async.AwaitIAsyncResult(asyncResult) |> Async.Ignore

    // keep working
    printfn "Doing something useful while waiting for write to complete"

    // block on the timer now by waiting for the async to complete
    Async.RunSynchronously async 

    // done
    printfn "Async write completed"
fileWriteWithAsync