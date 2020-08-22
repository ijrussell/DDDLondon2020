namespace DDDLondonCode

module StructuralEquality =

    type User = {
        UserName : string
        Password : string
    }

    let user1 = { UserName = "Ian"; Password = "MyPassword"}
    let user2 = { UserName = "Ian"; Password = "MyPassword"}
    let user3 = { UserName = "Ian"; Password = "NotMyPassword"}

    user1 = user2 // true
    user2 = user3 // false

module CustomEquality =

    [<CustomEquality; NoComparison>]
    type User = {
        UserName : string
        Password : string
    }
    with
        override this.Equals(obj) =
            match obj with
            | :? User as c -> this.UserName = c.UserName
            | _ -> false
        override this.GetHashCode() =
            hash this.UserName

    let user1 = { UserName = "Ian"; Password = "MyPassword"}
    let user2 = { UserName = "Ian"; Password = "MyPassword"}
    let user3 = { UserName = "Ian"; Password = "NotMyPassword"}

    user1 = user2 // true
    user2 = user3 // true
