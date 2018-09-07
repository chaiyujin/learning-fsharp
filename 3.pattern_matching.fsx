
let firstPart, secondPart, _ = (1, 2, 3)

let elem1::elem2::rest = [1..10]

let listMatcher aList = 
    match aList with
    | [] -> printfn "the list is empty"
    | [firstElement] -> printfn "the list has one element %A" firstElement
    | [first; second] -> printfn "list is %A and %A" first second
    | _ -> printfn "the list has more than two elements"


listMatcher [1;2;3;4]
listMatcher [1;2]
listMatcher [1]
listMatcher []


type Address = { Street: string; City: string; }
type Customer = { ID: int; Name: string; Address: Address }

let customer1 = {ID = 1; Name = "Bob"; Address={Street="123 Main"; City="NY"}}

// extract name only
let {Name=name1} = customer1
printfn "The customer is called %s" name1

let {ID=id2; Name=name2;} = customer1
printfn "The customer called %s has id %i" name2 id2

let { Name=name3;  Address={Street=street3}  } =  customer1   
printfn "The customer is called %s and lives on %s" name3 street3


// =========== Active Patterns ===========
let (|Int|_|) str = 
    match System.Int32.TryParse(str) with
    | (true, int) -> Some int
    | _ -> None

let (|Bool|_|) str = 
    match System.Boolean.TryParse(str) with
    | (true,bool) -> Some bool
    | _ -> None

// create a function to call the patterns
let testParse inputStr = 
    match inputStr with
    | Int i -> printfn "The value is an int '%i'" i
    | Bool b -> printfn "The value is a bool '%b'" b
    | _ -> printfn "The value '%s' is something else" inputStr

// test
testParse "12"
testParse "true"
testParse "abc"

// create an active pattern
open System.Text.RegularExpressions
let (|FirstRegexGroup|_|) pattern input =
   let m = Regex.Match(input,pattern) 
   if (m.Success) then Some m.Groups.[1].Value else None

// create a function to call the pattern
let testRegex str = 
    match str with
    | FirstRegexGroup "http://(.*?)/(.*)" host -> 
           printfn "The value is a url and the host is %s" host
    | FirstRegexGroup ".*?@(.*)" host -> 
           printfn "The value is an email and the host is %s" host
    | _ -> printfn "The value '%s' is something else" str
   
// test
testRegex "http://google.com/test"
testRegex "alice@hotmail.com"


let (|MultOf3|_|) i = if i % 3 = 0 then Some MultOf3 else None
let (|MultOf5|_|) i = if i % 5 = 0 then Some MultOf5 else None
// the main function
let fizzBuzz i = 
  match i with
  | MultOf3 & MultOf5 -> printf "FizzBuzz, " 
  | MultOf3 -> printf "Fizz, " 
  | MultOf5 -> printf "Buzz, " 
  | _ -> printf "%i, " i
// test
[1..20] |> List.iter fizzBuzz
printfn ""


// === Exception ===

// define a "union" of two different alternatives
// generic 'a and 'b
type Result<'a, 'b> = 
    | Success of 'a  // 'a means generic type. The actual type
                     // will be determined when it is used.
    | Failure of 'b  // generic failure type as well

// define all possible errors
type FileErrorReason = 
    | FileNotFound of string
    | UnauthorizedAccess of string * System.Exception


// define a low level function in the bottom layer
let performActionOnFile action filePath =
   try
      //open file, do the action and return the result
      use sr = new System.IO.StreamReader(filePath:string)
      let result = action sr  //do the action to the reader
      sr.Close()
      Success (result)        // return a Success
   with      // catch some exceptions and convert them to errors
      | :? System.IO.FileNotFoundException as ex 
          -> Failure (FileNotFound filePath)      
      | :? System.Security.SecurityException as ex 
          -> Failure (UnauthorizedAccess (filePath,ex))  
      // other exceptions are unhandled
