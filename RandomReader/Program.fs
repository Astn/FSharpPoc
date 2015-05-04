open System.Collections.Generic
open Newtonsoft.Json

// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
type read = {
    bib:int32;
    checkpoint: int32;
    time:int32; // Up to 24 days
}

let simulation checkpoints distance runnerCount spacing rnd = 
    let runner bib awesomeness delay = seq {
        for c in [0 .. checkpoints] -> 
            let time = delay + int32 (float (distance * c) * (1.0 - awesomeness))
            let ret = { bib=bib; checkpoint=c; time=time }
            // This shows that the for loop is processed asyncronously:
            // printfn "Runner %d Checkpoint %d" bib c  
            ret
    }

    let yieldRunnersByTime = seq {
        //for time in [0 .. spacing .. System.Int32.MaxValue] ->
        for r in [0 .. runnerCount] -> (r * spacing, runner r (rnd()) (r * spacing))
    }

    // while (fst rt < time) yield snd rt, enum.moveNext()
    seq {
        // Runners needs to be a collection that can be ammended at any time, 
        // and have elements removed from any index.
        let runners = new System.Collections.Generic.List<bool ref * IEnumerator<read>>()
        let time = ref 0
        let runnersFinished = ref 0
        let lastRunnerToStart = ref 0
        let nextTime = ref 1

        while (runnersFinished.Value < runnerCount) do
            System.Console.Title <- runners.Count.ToString()
            time := nextTime.Value

            nextTime := (time.Value / spacing) * spacing + spacing
            // Runners cross the start line
            let runnersThatShouldHaveStarted = min (time.Value / spacing) runnerCount
            while (lastRunnerToStart.Value < runnersThatShouldHaveStarted) do
                lastRunnerToStart := lastRunnerToStart.Value + 1
                let newRunner = runner lastRunnerToStart.Value (rnd()) time.Value
                printfn "// Runner %d is crossing the starting line" lastRunnerToStart.Value
                let newRunnerEnum = newRunner.GetEnumerator()
                let nret = (ref(newRunnerEnum.MoveNext()), newRunnerEnum)
                // Add runner to list of running runners
                if ((fst nret).Value) then // Only add if MoveNext() returned true
                    runners.Add(nret)

            // Yield results from each runner that has occured at or before time.
            let i = ref 0
            while (i.Value < runners.Count) do
                let runner = runners.Item(i.Value)
                let readResult = fst runner
                let enumerator = snd runner
                // ReadResult must be true at this point
                while (readResult.Value && enumerator.Current.time <= time.Value) do
                    yield enumerator.Current
                    if enumerator.Current.checkpoint = checkpoints then
                        printfn "// Runner %d has crossed the finish line" enumerator.Current.bib
                    readResult := enumerator.MoveNext() 
                // Figure out next time
                if readResult.Value then
                    if enumerator.Current.time < nextTime.Value then
                        nextTime := enumerator.Current.time
                // Remove runners that have completed their run
                if not readResult.Value then
                    runners.RemoveAt(i.Value);
                    runnersFinished := runnersFinished.Value + 1
                    i := i.Value - 1
                i := i.Value + 1 // Increment time by 1 ms
    }
    

[<EntryPoint>]
let main (argv : string[]) =
    let argl = Array.toList argv
    let checkpoints = ref 5
    let distance = ref 10000 //  600000 // 10 minute
    let runners = ref System.Int32.MaxValue
    let spacing = ref 1000

    let rec parse (lst : string list) = 
        if not lst.IsEmpty then 
            match lst.Head with 
            | "/c" -> 
                parse lst.Tail.Tail
                checkpoints := int32 lst.Tail.Head
            | "/d" ->
                parse lst.Tail.Tail
                distance := int32 lst.Tail.Head
            | "/r" ->
                parse lst.Tail.Tail
                runners := int32 lst.Tail.Head
            | "/s" ->
                parse lst.Tail.Tail
                spacing := int32 lst.Tail.Head
            | _ -> failwith "usage"
    
    parse argl

    let rnd = 
        let rng = new System.Random(0)
        let next() = 
            rng.NextDouble()
        next
        

    let race = simulation checkpoints.Value distance.Value runners.Value spacing.Value rnd
    let ser = JsonSerializer.Create();
    let tojson obj = 
        let sw = new System.IO.StringWriter()
        ser.Serialize(sw, obj)
        let ret = sw.ToString()
        sw.Dispose()
        ret

    for read in race do 
        let json = tojson read
        json |> ignore 
        printfn "%s" json

    System.Console.ReadKey(true) |> ignore
    0 // return an integer exit code
