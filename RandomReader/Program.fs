open System.Collections.Generic
open Newtonsoft.Json
//open Microsoft.VisualStudio.TestTools.UnitTesting

type read = {
    bib:int32;
    checkpoint: int32;
    time:int32; // Up to 24 days
}
type args = {
    checkpoints : int
    distance : int
    runners : int
    spacing : int
    timed : bool
    }

let simulation (options: args)rnd =
    let runner bib awesomeness delay = seq {
        for c in [0 .. options.checkpoints] -> 
            let time = delay + int32 (float (options.distance * c) * (1.0 - awesomeness))
            let ret = { bib=bib; checkpoint=c; time=time }
            // This shows that the for loop is processed asyncronously:
            // printfn "Runner %d Checkpoint %d" bib c  
            ret
    }

    seq {
        // Runners needs to be a collection that can be ammended at any time, 
        // and have elements removed from any index.
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        let runners = new System.Collections.Generic.List<bool ref * IEnumerator<read>>()
        let time = ref 0
        let runnersFinished = ref 0
        let lastRunnerToStart = ref 0
        let nextTime = ref 1

        while (runnersFinished.Value < options.runners) do
            if options.timed then
                System.Threading.Thread.Sleep(max 0 (time.Value - (int32 stopwatch.ElapsedMilliseconds)))
            System.Console.Title <- runners.Count.ToString()
            time := nextTime.Value

            nextTime := (time.Value / options.spacing) * options.spacing + options.spacing
            // Runners cross the start line
            let runnersThatShouldHaveStarted = min (time.Value / options.spacing) options.runners
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
                    if enumerator.Current.checkpoint = options.checkpoints then
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



type parseResult =
    | Args of args
    | ParseError of string

let defaults = { checkpoints = 5; distance = 60000; runners = System.Int32.MaxValue; spacing = 1000; timed = false }
    
let rec parseArgs (lst : string list) (defaults:parseResult) : parseResult = 
    match defaults with
    | ParseError message -> defaults
    | Args def-> 
        match lst with 
        | "/c"::tail -> 
            parseArgs tail.Tail <| Args {def with checkpoints = int32 tail.Head}
        | "/d"::tail ->
            parseArgs tail.Tail <| Args {def with distance = int32 tail.Head}
        | "/r"::tail ->
            parseArgs tail.Tail <| Args {def with runners = int32 tail.Head}
        | "/s"::tail ->
            parseArgs tail.Tail <| Args {def with spacing = int32 tail.Head}
        | "/t"::tail ->
            parseArgs tail.Tail <| Args {def with timed = true }
        | [] -> defaults
        | _ -> ParseError ("Invalid parameter: " + lst.Head)

let usage = """
Usage: RandomReader [/c <checkpoints>] [/d <distance>] [/r <runners>]
                    [/s <spacing>] [/t]

/c <checkpoints> -  Default: 5. Number of checkpoints, including finish, the
                    race should include.
/d <distance>    -  Default: 600000. Distance in any unit between checkpoints.
/r <runners>     -  Default: 2147483617. Number of runners in the race.
/s <spacing>     -  Default: 1000. Interval in time between runners crossing
                    the start line.
/t               -  Runs the simulation in real-time, assuming 1 ms per time.
            """
        
let main (argv : string[]) =
    let args = parseArgs <| Array.toList argv <| Args defaults
    match args with
    | ParseError message -> 
        System.Console.WriteLine message
        1
    | Args options ->
        let rnd =
            let rng = new System.Random(0)
            let next() =
                rng.NextDouble()
            next
        

        let race = simulation options rnd
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
   
//
//[<TestClass>]
//type randomReaderTests() = 
//    let eq a b = JsonConvert.SerializeObject(a) = JsonConvert.SerializeObject(b)
// 
//    [<TestInitialize>]
//    member x.setup() =
//        0|>ignore
//
//    [<TestMethod>]
//    member x.simulationTest() =
//        let rnd() = 0.5        
//        let sim = simulation 1 1 1 1 false rnd
//        let arr = Seq.toList(sim)
//        let expected = [{bib = 1; checkpoint = 0; time = 1};{bib = 1;checkpoint = 1; time = 1}]
//        Assert.IsTrue(eq arr expected, "One Runner Test");
//

[<EntryPoint>]
let main2 (argv : string[]) = 
    
    main argv
