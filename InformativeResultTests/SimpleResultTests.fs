module SimpleResultTests

open InformativeResult

open Xunit

[<Fact>]
let ``SimpleResult smoke test``() =
    let simpleDivide num denom : SimpleResult<int,string*int,exn> =
        try
            match denom with
            | 0 -> Failure ("Cannot divide by zero!", denom) // 'TError
            | _ -> Success (num / denom)
        with
        | :? System.Exception as ex ->
                CriticalFailure ex

    Assert.Equal(Success 5, simpleDivide 10 2)
    Assert.Equal(Failure ("Cannot divide by zero!",0), simpleDivide 10 0)

    let simpleExponent a b : SimpleResult<int,string*int,exn> =
        let rec simplepow c d =
            match d with
            | 0 -> 1
            | _ ->
                match d > 0 with
                | true -> (simplepow c (d - 1)) * c
                | false -> (simplepow c (d + 1)) / c
        try
            match b > 10 with
            | true -> Failure ("exponents larger than 10 unsupported",b)
            | false -> Success (simplepow a b)
        with
        | :? System.Exception as ex ->
                CriticalFailure ex
    match simpleExponent 5 2 with
        | Success 25 -> Assert.True(true)
        | _ -> Assert.True(false)
    match simpleExponent 10 25 with
        | Failure _ -> Assert.True(true)
        | _ -> Assert.True(false)
    match simpleExponent 0 (-1) with
        | CriticalFailure _ -> Assert.True(true)
        | _ -> Assert.True(false)
