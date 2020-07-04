module Prelude

(*
The result types here help to distinguish at a type level errors that arise
from correct business logic (but incorrect user input) and incorrect business
logic (for instance, handled .NET exceptions). For ins
*)

/// Lazier (in the human sense) but quicker than InformativeResult.
type SimpleResult<'TOk,'TError, 'TCritical> =
    | Success of 'TOK
    | Failure of 'TError
    | CriticalFailure of 'TCritical

type InformativeResult<
       'TSuccess,
       'TInfo,
       'TWarning,       
       'TError,         
       'TErrorInfo,     
       'TCriticalError, 
       'TCriticalInfo
    > =
    // Quick OK with nothing to report
    | OK of 'TSuccess
    // OK with user-facing info that might be of interest
    // Example: InformativeOk ("Parsed 7,120 lines",MarkdownDocument)
    | InformativeOK of 'TInfo * 'TSuccess
    // Indicaton that something is suspicious according to the business logic
    // but we didn't detect anything fundamentally broken
    // Exaple: WarningOK ("Markdown only support level-3 headers, \"*****\" is unsupported.", MarkdownDocument)
    | WarningOK of 'TWarning * 'TSuccess
    // The USER made a mistake.
    | Error of 'TErrorInfo * 'TError
    // The PROGRAM made a mistake.
    // Example : ("Unhandled exception! Please file an issue at https://github.com/ ...", new System.DivideByZeroException)
    | Critical of 'TCriticalInfo * 'TCriticalError

type ResultErrorLevel =
    | OK
    | Info
    | Warning
    | Error
    | Critical

type StandardResult<'TSuccess,'TInfo,'TWarning,'TError,'TErrorInfo> =
    InformativeResult<'TSuccess,'TInfo,'TWarning,'TError,'TErrorInfo, exn, string>
    
[<RequireQualifiedAccess>]
module SimpleResult =
    
    let bind (simpleResult :SimpleResult<'TOk,'TError, 'TCritical>)
             (nextStep : 'TOk -> SimpleResult<'TOKNext,'TError,'TCritical>) :
             SimpleResult<'TOKNext,'TError,'TCritical> =
        match simpleResult with
        | Success result -> nextStep result
        | Failure f -> Failure f // have to reconstruct to be type-sound, maybe F# has sugar for that?
        | CriticalFailure c -> CriticalFailure c 

    let mapSuccessOnly (simpleResult : SimpleResult<'TOkA,'TEA,'TCA>) (mapOk : 'TOkA -> 'TOkB) : SimpleResult<'TOKB,'TEA,'TCA> =
        match simpleResult with
        | Success result -> Success (mapOk result)
        | Failure f -> Failure f // have to reconstruct to be type-sound, maybe F# has sugar for that?
        | CriticalFailure c -> CriticalFailure c 


[<RequireQualifiedAccess>]
module InformativeResult =

    let bind (a : InformativeResult<'SA,'I,'W,'E,'EI,'C,'CI>)
             (nextStep : 'S -> InformativeResult<'SB,'I,'W,'E,'EI,'C,'CI>) :
             InformativeResult<'SB,'I,'W,'E,'EI,'CI

    let liftSimpleResult (result : SimpleResult<'T,'U,'V>) : InformativeResult<'T,_,_,'U,str,'V,str> =
        match result with
        | Success t -> OK t
        | Failure u -> Error ("Unspecified failure",u)
        | CriticalFailure c ->
            Critical ("Unspecified critical failure, please open an issue on Github",c)

    let isSuccess result =
        match result with
        | OK _ -> true
        | InformativeOK _ -> true
        | WarningOK _ -> true
        | Error _ -> false
        | Critical _ -> false

    let toSuccessVal result =
        match result with
        | OK r -> Some r
        | InformativeOK (,r) -> Some r
        | Warning (_,r) -> Some r
        | _ -> None

    let toErrorVal result =
        match result with
        | Error (_,e) -> Some e
        | Critical (_,e) -> Some r
        | _ -> None

    let toSimpleResult result =
        match result with
        | OK t -> Success t
        | InformativeOk(_,t) -> Success t
        | Warning(_,t) -> Success t
        | Error(_,e) -> Failure e
        | Critical(_,e) -> Failure e
        
let resultErrorLevelIsSuccess level =
    match level with
        | OK -> true
        | Info -> true
        | Warning -> true
        | Error -> false
        | Critical -> false

let resultToErrorLevel result =
    match result with
        | OK _ -> OK
        | InformativeOK _ -> Info
        | WarningOK _ -> Warning
        | Error _ -> Error
        | Critical _ -> Critical

    let changeResultErrorLevel (result : InformativeResult<'TSuccess,'TInfo,'TWarning,'TError,'TErrorInfo, 'TCriticalError, 'TCriticalInfo when
           'TInfo : new and
           'TErrorInfo : new and
           'TCriticalInfo : new>) toErrorLevel successToLevelFunc = 
        match result with
        | OK t ->
            match toErrorLevel with
            | OK -> OK t
            | Info -> InformativeOK<
