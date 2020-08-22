namespace DDDLondonCode

// type Result<'TSuccess, 'TFailure> =
//     | Ok of 'TSuccess
//     | Error of 'TFailure

module ResultPart1 =

    let divide x y = x / y

    let goodDivide = divide 4 2 // 2
    let badDivide = divide 4 0 // DivideByZeroException 

module ResultPart2 =

    open System
    
    let divide x y = 
        try
            x / y
        with 
        | :? DivideByZeroException -> 0

    let goodDivide = divide 4 2 // 2
    let badDivide = divide 4 0 // 0 

module ResultPart3 =

    open System
    
    let divide x y = 
        try
            x / y |> Some
        with 
        | :? DivideByZeroException -> None

    let goodDivide = divide 4 2 // Some 2
    let badDivide = divide 4 0 // None 

module ResultPart4 =

    open System
    
    let divide x y = 
        try
            x / y |> Ok
        with 
        | :? DivideByZeroException as ex -> Error ex

    let goodDivide = divide 4 2 // Ok 2
    let badDivide = divide 4 0 // Error DivideByZeroException 

module ResultPart5 =

    open System

    type DivisionError =
        | InvalidInput
    
    let divide x y = 
        try
            x / y |> Ok
        with 
        | :? DivideByZeroException -> Error InvalidInput

    let goodDivide = divide 4 2 // Ok 2
    let badDivide = divide 4 0 // Error InvalidInput 

module ResultPart6 =

    type DivisionError =
        | InvalidInput
    
    let divide x y = 
        match y with
        | 0 -> Error InvalidInput
        | _ -> x / y |> Ok

    let goodDivide = divide 4 2 // Ok 2
    let badDivide = divide 4 0 // Error InvalidInput 

