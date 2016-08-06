[<AutoOpen>]
module Lenses = 
    
    type Lens<'a, 'b> = {
        get: 'a -> 'b
        set: 'b -> 'a -> 'a
    }
    with member this.update (transform: 'b -> 'b) (a: 'a) : 'a =
            let currentInnerValue = this.get a
            let newInnerValue = transform currentInnerValue 
            this.set newInnerValue a
            
    let inline (>>|) (lens1: Lens<'a,'b>) (lens2: Lens<'b,'c>) = {
        get = lens1.get >> lens2.get
        set = lens2.set >> lens1.update
    }