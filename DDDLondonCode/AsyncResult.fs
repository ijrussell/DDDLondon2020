namespace DDDLondonCode

// Source: https://demystifyfp.gitbook.io/fstoolkit-errorhandling/asyncresult/ce

module AsyncResultV1 =

    open System
    open FsToolkit.ErrorHandling

    type AuthError =
        | UserSuspended

    type TokenError =
        | BadThingHappened of string

    type LoginError = 
        | InvalidUser
        | InvalidPwd
        | Unauthorized of AuthError
        | TokenErr of TokenError

    type AuthToken = AuthToken of Guid

    type UserStatus =
        | Active
        | Suspended

    type User = {
        Name : string
        Password : string
        Status : UserStatus
    } 

    [<Literal>]
    let ValidPassword = "password"
    [<Literal>]
    let ValidUser = "isvalid"
    [<Literal>]
    let SuspendedUser = "issuspended"
    [<Literal>]
    let BadLuckUser = "hasbadluck"
    [<Literal>]
    let AuthErrorMessage = "Earth's core stopped spinning"

    let tryGetUser (username:string) : Async<User option> =
        async {
            let user = { Name = username; Password = ValidPassword; Status = Active }
            return
                match username with
                | ValidUser -> Some user
                | SuspendedUser -> Some { user with Status = Suspended }
                | BadLuckUser -> Some user
                | _ -> None
        }

    let isPwdValid (password:string) (user:User) : bool =
        password = user.Password

    let authorize (user:User) : Async<Result<unit, AuthError>> =
        async {
            return 
                match user.Status with
                | Active -> Ok ()
                | _ -> UserSuspended |> Error 
        }

    let createAuthToken (user:User) : Result<AuthToken, TokenError> =
        try
            if user.Name = BadLuckUser then failwith AuthErrorMessage
            else Guid.NewGuid() |> AuthToken |> Ok
        with
        | ex -> ex.Message |> BadThingHappened |> Error

    let login (username: string) (password: string) : Async<Result<AuthToken, LoginError>> =
        asyncResult {
            let! user = username |> tryGetUser |> AsyncResult.requireSome InvalidUser
            do! user |> isPwdValid password |> Result.requireTrue InvalidPwd
            do! user |> authorize |> AsyncResult.mapError Unauthorized
            return! user |> createAuthToken |> Result.mapError TokenErr
        }

    // Tests =======================================
    [<Literal>]
    let BadPassword = "notpassword"
    [<Literal>]
    let NotValidUser = "notvalid"

    let isOk (input:Result<_,_>) : bool =
        match input with
        | Ok _ -> true
        | _ -> false
    
    let matchError (error:LoginError) (input:Result<_,LoginError>) =
        match input with
        | Error ex -> ex = error
        | _ -> false  

    let runWithValidPassword (username:string) = 
        login username ValidPassword |> Async.RunSynchronously
    
    let success =
        let result = runWithValidPassword ValidUser
        result |> isOk 

    let badPassword =
        let result = login ValidUser BadPassword |> Async.RunSynchronously
        result |> matchError InvalidPwd

    let invalidUser =
        let result = runWithValidPassword NotValidUser
        result |> matchError InvalidUser

    let isSuspended =
        let result = runWithValidPassword SuspendedUser
        result |> matchError (UserSuspended |> Unauthorized)

    let hasBadLuck =
        let result = runWithValidPassword BadLuckUser
        result |> matchError (AuthErrorMessage |> BadThingHappened |> TokenErr)

module AsyncResultV2 =

    open System
    open FsToolkit.ErrorHandling

    type AuthError =
        | UserSuspended

    type TokenError =
        | BadThingHappened of string

    type LoginError = 
        | InvalidUser
        | InvalidPwd
        | Unauthorized of AuthError
        | TokenErr of TokenError

    type AuthToken = AuthToken of Guid

    type UserStatus =
        | Active
        | Suspended

    type User = {
        Name : string
        Password : string
        Status : UserStatus
    } 

    [<Literal>]
    let ValidPassword = "password"
    [<Literal>]
    let ValidUser = "isvalid"
    [<Literal>]
    let SuspendedUser = "issuspended"
    [<Literal>]
    let BadLuckUser = "hasbadluck"
    [<Literal>]
    let AuthErrorMessage = "Earth's core stopped spinning"

    let tryGetUser (username:string) : Async<User option> =
        async {
            let user = { Name = username; Password = ValidPassword; Status = Active }
            return
                match username with
                | ValidUser -> Some user
                | SuspendedUser -> Some { user with Status = Suspended }
                | BadLuckUser -> Some user
                | _ -> None
        }

    let isPwdValid (password:string) (user:User) : bool =
        password = user.Password

    let authorize (user:User) : Async<Result<unit, AuthError>> =
        async {
            return 
                match user.Status with
                | Active -> Ok ()
                | _ -> UserSuspended |> Error 
        }

    let createAuthToken (user:User) : Result<AuthToken, TokenError> =
        try
            if user.Name = BadLuckUser then failwith AuthErrorMessage
            else Guid.NewGuid() |> AuthToken |> Ok
        with
        | ex -> ex.Message |> BadThingHappened |> Error

    type LoginServices = {
        GetUserService : string -> Async<User option>
        PasswordValidator : string -> User -> bool
        AuthorizeService : User -> Async<Result<unit, AuthError>> 
        AuthTokenCreator : User -> Result<AuthToken, TokenError> 
    }

    let loginServices = {
        GetUserService = tryGetUser
        PasswordValidator = isPwdValid
        AuthorizeService = authorize
        AuthTokenCreator = createAuthToken
    }

    let login (services:LoginServices) (username: string) (password: string) 
        : Async<Result<AuthToken, LoginError>> =
        asyncResult {
            let! user = username |> services.GetUserService |> AsyncResult.requireSome InvalidUser
            do! user |> services.PasswordValidator password |> Result.requireTrue InvalidPwd
            do! user |> services.AuthorizeService |> AsyncResult.mapError Unauthorized
            return! user |> services.AuthTokenCreator |> Result.mapError TokenErr
        }

    [<Literal>]
    let BadPassword = "notpassword"
    [<Literal>]
    let NotValidUser = "notvalid"

    let isOk (input:Result<_,_>) : bool =
        match input with
        | Ok _ -> true
        | _ -> false
    
    let matchError (error:LoginError) (input:Result<_,LoginError>) =
        match input with
        | Error ex -> ex = error
        | _ -> false  

    let runWithValidPassword (username:string) = 
        login loginServices username ValidPassword |> Async.RunSynchronously
    
    let success =
        let result = runWithValidPassword ValidUser
        result |> isOk 

    let badPassword =
        let result = login loginServices ValidUser BadPassword |> Async.RunSynchronously
        result |> matchError InvalidPwd

    let invalidUser =
        let result = runWithValidPassword NotValidUser
        result |> matchError InvalidUser

    let isSuspended =
        let result = runWithValidPassword SuspendedUser
        result |> matchError (UserSuspended |> Unauthorized)

    let hasBadLuck =
        let result = runWithValidPassword BadLuckUser
        result |> matchError (AuthErrorMessage |> BadThingHappened |> TokenErr)

