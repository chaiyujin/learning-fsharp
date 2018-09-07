
let myInt = 5
let myFloat = 3.14
let myString = "hello"

let twoToFive = [2;3;4;5]
let oneToFive = 1 :: twoToFive  // :: cretes list with new 1st elemnt
let zeroToFive = [0;1] @ twoToFive  // @ concats two lists

let square x = x * x    // no parens are used
square 3

let add x y = x + y
add 2 3

let makeLing (choice:int) (y:int) = choice + y

let evens list = 
    let isEven x = x%2 = 0
    List.filter isEven list

evens oneToFive

// use parens to clarify precedence.
let sumOfSquaresTo100 =
    List.sum ( List.map square [1..100] )  // List.sumBy square [1..100]

// pipe the output of one operation to the next using "|>"

let sumOfSquaresTo100piped = 
    [1..100] |> List.map square |> List.sum

// anonymous founction using "fun" keyword
let sumOfSquaresTo100withFun = 
    [1..100] |> List.map (fun x -> x*x) |> List.sum


// ========== Pattern Matching =============
let simplePatternMatch = 
    let x = "a"
    match x with
    | "a" -> printfn "x is a"
    | "b" -> printfn "x is b"
    | _ -> printfn "x is something else"

simplePatternMatch

// Some(..) and None are roughly analogous to Nullable wrappers
let validValue = Some(99)
let invalidValue = None

let optionPatternMatch input = 
    match input with
    | Some i -> printfn "input is an int=%d" i
    | None   -> printfn "input is missing"

optionPatternMatch validValue
optionPatternMatch invalidValue

// === complex data types ===
let twoTuple = 1, 2
let threeTuple = "a", 2, true

type Person = {First:string; Last:string}
let person1 = {First="john"; Last="Doe"}

// Union types have choices. Vertical bars are separators
type Temp = 
    | DegreesC of float
    | DegreesF of float
let temp = DegreesF 98.6

type Employee = 
    | Worker of Person
    | Manager of Employee list

let jdoe = {First="John"; Last="Doe"}
let worker = Worker jdoe

// === printing ===
printfn "Printing an int %i, a float %f, a bool %b" 1 2.0 true
printfn "A string %s, and something generic %A" "hello, world!" [1;2;3;4]

// complex types
printfn "twoTuple=%A\nPerson=%A\nTemp=%A\nEmployee=%A\n"
        twoTuple person1 temp worker

// === changing ===

let jchai = { jdoe with Person.Last="Chai" }
printfn "Person=%A\n" jchai

// === List functions ===
// List.iter fun :  run function and return nothin
// List.map  fun :   run function and return a list
// List.partition fun : run function (return bool), partition into two parts, true part, and false part.
