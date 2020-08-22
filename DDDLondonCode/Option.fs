namespace DDDLondonCode

open System

module OptionPart1 =

    type CustomerName = {
        First : string
        Middle : string 
        Last : string
    }

    let withMiddlename = { First = "Ian"; Middle = "????"; Last = "Russell" }
    let noMiddlename = { First = "Ian"; Middle = ""; Last = "Russell" }
    let nullMiddlename = { First = "Ian"; Middle = null; Last = "Russell" }

    let (success, value) = DateTime.TryParse ""

    let validDate = DateTime.TryParse "2020-08-20 18:00:00"
    let inValidDate = DateTime.TryParse "Foo"

    type User = {
        UserName : string
        Password : string
    }

    let tryGetUser userName =
        [
            { UserName = "Ian"; Password = "MyPassword"}
            { UserName = "Pippa"; Password = "SomeOtherPassword"}
        ]
        |> List.find (fun x -> x.UserName = userName)

    let user = tryGetUser "Pippa"
    let notUser = tryGetUser "Fred" // Throws KeyNotFoundException

module OptionPart2 =

    // type Option<'T> =
    //     | Some of 'T
    //     | None

    open System.Collections.Generic

    type CustomerName = {
        First : string
        Middle : Option<string>
        Last : string
    }

    let withMiddlename = { First = "Ian"; Middle = Some "????"; Last = "Russell" }
    let noMiddlename = { First = "Ian"; Middle = None; Last = "Russell" }

    let tryParseDateTime (input:string) =
        let (success, value) = DateTime.TryParse input
        if success then Some value else None

    let isDateTime = tryParseDateTime "2020-08-20 17:00:34" // Some ...
    let isNotDateTime = tryParseDateTime "foo" // None

    type User = {
        UserName : string
        Password : string
    }

    let tryGetUser userName =
        try
            [
                { UserName = "Ian"; Password = "MyPassword"}
                { UserName = "Pippa"; Password = "SomeOtherPassword"}
            ]
            |> List.find (fun x -> x.UserName = userName)
            |> Some
        with
        | :? KeyNotFoundException -> None  

    let user = tryGetUser "Pippa" // Some { UserName = "Pippa"; Password = "SomeOtherPassword"}
    let notUser = tryGetUser "Fred" // None

module OptionPart3 =

    type CustomerName = {
        First : string
        Middle : string option
        Last : string
    }

    let withMiddlename = { First = "Ian"; Middle = Some "????"; Last = "Russell" }
    let noMiddlename = { First = "Ian"; Middle = None; Last = "Russell" }

    type User = {
        UserName : string
        Password : string
    }

    let tryGetUser userName =
        [
            { UserName = "Ian"; Password = "MyPassword"}
            { UserName = "Pippa"; Password = "SomeOtherPassword"}
        ]
        |> List.tryFind (fun x -> x.UserName = userName)

    let user = tryGetUser "Pippa" // Some { UserName = "Pippa"; Password = "SomeOtherPassword"}
    let notUser = tryGetUser "Fred" // None

