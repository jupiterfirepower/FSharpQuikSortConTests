open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let rec quicksort = function
    [] -> []
    | h::t -> quicksort ([ for x in t do if x <= h then yield x]) 
              @ [h] @ quicksort ([ for x in t do if x > h then yield x])
              
let rec quicksort7 = function
    [] -> []
    | h::t -> ( quicksort7 t |> List.filter (fun x -> x <= h) ) 
              @ [h] @ ( quicksort7 t |> List.filter (fun x -> x > h) )
              
//val qsort: 'a list -> 'a list
let rec qsortShort = function
        | [] -> []
        | x::xs' -> let (l, r) = List.partition ((>) x) xs'
                    List.concat [(qsortShort l);[x];(qsortShort r)]

// Tail Recursion
let qsortTail xs =   
    let rec loop xs cont =
            match xs with
            | [] -> cont []
            | x::xs' -> let (l, r) = List.partition ((>) x) xs'                          
                        loop l (fun lacc -> loop r (fun racc -> cont (lacc @ x :: racc)))
    loop xs (fun x -> x)
    
                 
let rec quicksort2 list =
    match list with
    | [] -> []
    | x::xs -> quicksort [for item in xs do if (compare item x) <= 0 then yield item] 
                @ [x] @ quicksort [for item in xs do if (compare item x) > 0 then yield item]
              
let rec quicksort3 = function
        | head :: tail -> 
        (tail |> List.filter (fun item -> item <= head) |> quicksort)
        @ [head] @ (tail |> List.filter (fun item -> item > head) |> quicksort)
        | _ -> []

let rec qsort L =
    match L with
    | [] -> []
    | x::xs -> 
    let smaller = [for i in xs do if i <= x then yield i] in
    let larger  = [for i in xs do if i > x  then yield i] in
    qsort smaller @ [x] @ qsort larger

let rec qsort3 L = 
    match L with
    | [] -> []
    | pivot::rest ->
    let is_less x = x < pivot in
    let left, right = List.partition is_less rest in
    quicksort left @ [pivot] @ quicksort right 

let rec qsort2 xs =
    match xs with
    | x::xs -> (qsort2 (List.filter (fun n -> n <= x) xs)) 
                @ [x] @ (qsort2 (List.filter (fun n -> n > x) xs))
    | [] -> []
    
let rec quickSort sequence =
  match sequence with
  | [] -> sequence
  | x :: t -> List.partition (fun a -> a < x) t |> (fun (a,b) -> quickSort a @ x::quickSort b)  

let rec simpleQuickSort(inL)= 
    match inL with
    | []->[]
    | h::t->List.partition(fun e->e<=h) t|> fun(l,r)->simpleQuickSort(l) @ h::simpleQuickSort(r)
    
let rec simpleQuickSort_= function
    | []->[]
    | h::t->List.partition(fun e->e<=h) t|> fun(l,r)->simpleQuickSort(l) @ h::simpleQuickSort(r)

// Concurrent Programming
let asyncPartition(l,f)= async{ return l|>List.filter(f)}

let rec asyncQuickSort(inL)= 
       match inL with
       |[]->[]
       |h::t-> 
       let asynTasks  = [asyncPartition(t,fun(x)->x<=h) ; asyncPartition(t,fun(x)->x>h)]
       let asynResult = Async.RunSynchronously(Async.Parallel asynTasks)// execute
       asyncQuickSort(asynResult.[0])@h::asyncQuickSort(asynResult.[1])//join          

let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    returnValue

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    let inputdata = [1; 2; 5; 9; 8; 4; 3; 7; 23; 54; 65; 87; 100; 22; 31; 68]

    let resultq  = duration(fun () -> quicksort inputdata)
    let resultq1 = duration(fun () -> quicksort7 inputdata)
    let resultq2 = duration(fun () -> qsortShort inputdata)
    let resultq3 = duration(fun () -> qsortTail inputdata)
    let resultq4 = duration(fun () -> quicksort2 inputdata)
    let resultq5 = duration(fun () -> quicksort3 inputdata)
    let resultq6 = duration(fun () -> qsort inputdata)
    let resultq7 = duration(fun () -> qsort2 inputdata)
    let resultq8 = duration(fun () -> qsort3 inputdata)
    let resultq9 = duration(fun () -> quickSort inputdata)
    let resultq10 = duration(fun () -> simpleQuickSort inputdata)
    let resultq11 = duration(fun () -> simpleQuickSort_ inputdata)
    let resultq12 = duration(fun () -> asyncQuickSort inputdata)
     
    printfn "Original: %A" inputdata
    printfn "Sorted: %A" resultq
    printfn "Sorted: %A" resultq1
    printfn "Sorted: %A" resultq2
    printfn "Sorted: %A" resultq3
    printfn "Sorted: %A" resultq4
    printfn "Sorted: %A" resultq5
    printfn "Sorted: %A" resultq6
    printfn "Sorted: %A" resultq7
    printfn "Sorted: %A" resultq8
    printfn "Sorted: %A" resultq9
    printfn "Sorted: %A" resultq10
    printfn "Sorted: %A" resultq11
    printfn "Sorted: %A" resultq12
    
    Console.ReadLine() |> ignore;
    0 // return an integer exit code
