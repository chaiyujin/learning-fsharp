
type Animal(noiseMakingStrategy) = 
    member this.MakeNoise = 
        noiseMakingStrategy() |> printfn "Making noise %s"


let meowing() = "Meow"
let cat = Animal(meowing)
cat.MakeNoise

let woofOrBark() = if (System.DateTime.Now.Second % 2 = 0)
                   then "woof" else "bark"
let dog = Animal(woofOrBark)
dog.MakeNoise
dog.MakeNoise
