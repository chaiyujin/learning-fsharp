// recursive with "rec" keyword
let rec quickSort list =
    match list with
    | [] -> []
    | firstElement::otherElements ->
        let smallerElements =
            otherElements
            |> List.filter (fun e -> e < firstElement)
            |> quickSort
        let largerElements = 
            otherElements
            |> List.filter (fun e -> e >= firstElement)
            |> quickSort
        List.concat [smallerElements; [firstElement]; largerElements]

let rec quickSort2 list = 
    match list with
    | [] -> []
    | first::rest ->
        let smaller, larger = List.partition ((>) first) rest
        List.concat [quickSort2 smaller; [first]; quickSort2 larger]

printfn "%A" (quickSort [1;5;23;18;9;1;3])
printfn "%A" (quickSort2 [1;5;23;18;9;1;3])
