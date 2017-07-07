[<AutoOpen>]
module RoP = 

    type Result<'TSuccess, 'TError> = 
        | Success of 'TSuccess
        | Failure of 'TError
        
    let map f x =
        match x with
            | Success s -> Success(f s)
            | Failure f -> Failure f

    let bind f x =
        match x with
            | Success s -> f s
            | Failure f -> Failure f

    let getOption r = 
        match r with
            | Success (Some a) -> Some a
            |  _ -> None