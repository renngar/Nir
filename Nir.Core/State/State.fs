module Fesl.State

[<Struct>]
type S<'State, 'Value> = S of ('State -> ('Value * 'State))

/// encapsulate the function call that "runs" the state
let runS (S f) state = f state
let evalS f state = runS f state |> fst
let execS f state = runS f state |> snd
let mapS f (S xS) = (xS >> fun (x, state) -> (f x), state) |> S

/// lift a value
let returnS x = (fun state -> x, state) |> S

/// lift a monadic function
let bindS (f: 'a -> S<'State, 'b>) xS =
    (runS xS >> fun (x, state) -> runS (f x) state) |> S

type StateBuilder() =
    member _.Return(x) = returnS x
    member _.ReturnFrom(xS) = xS
    member _.Bind(xS, f) = bindS f xS

let state = StateBuilder()

let getS = S(fun state -> state, state)
let putS newState = S(fun _ -> (), newState)
