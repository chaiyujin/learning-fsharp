let add2 x = x + 2
let mult3 x = x * 3
let square x = x * x

let add2ThenMult3 = add2 >> mult3
let mult3ThenSquare = mult3 >> square
let add2ThenMult3V2 x = mult3 (add2 x )


let logMsg msg x = printf "%s%i" msg x; x
let logMsgN msg x = printfn "%s%i" msg x; x

let mult3ThenSquareLogged
    =  logMsg "before="
    >> mult3
    >> logMsg " after mult3="
    >> square
    >> logMsgN " result="

mult3ThenSquareLogged 5
[1..2] |> List.map mult3ThenSquareLogged


// use List.reduce to compose functions from a list
let listOfFunctions = [
    mult3;
    square;
    add2;
    logMsgN "result=";
]

let allFunctions = List.reduce (>>) listOfFunctions

allFunctions 5
