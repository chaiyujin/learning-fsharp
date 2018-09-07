// F# has the ability to define units of measure and associate them with floats.
// The unit of measure is then “attached” to the float as a type and prevents mixing different types.
// This is another feature that can be very handy if you need it.

// define some measures
[<Measure>] 
type cm

[<Measure>] 
type inches

[<Measure>] 
type feet =
   // add a conversion function
   static member toInches(feet : float<feet>) : float<inches> = 
      feet * 12.0<inches/feet>


// define some values
let meter = 100.0<cm>
let yard = 3.0<feet>

//convert to different measure
let yardInInches = feet.toInches(yard)

// can't mix and match!
// yard + meter

// deny comparison
[<NoEquality; NoComparison>]
type CustomerAccount = {CustomerAccountId: int}

let x = {CustomerAccountId = 1}

x = x       // error!
x.CustomerAccountId = x.CustomerAccountId // no error