namespace DDDLondonCode

// Feature: Applying a discount
// Scenario: Eligible Registered Customers get 10% discount when they spend £100 or more

// Given the following Registered Customers
// |Customer Id|Is Eligible|
// |John       |true       |
// |Mary       |true       |
// |Richard    |false      |

// When <Customer Id> spends <Spend>
// Then their order total will be <Total>

// Examples:
// |Customer Id|   Spend|   Total|
// |Mary       |   99.00|   99.00|
// |John       |  100.00|   90.00|
// |Richard    |  100.00|  100.00|
// |Sarah      |  100.00|  100.00|

[<RequireQualifiedAccessAttribute>]
module DiscountV1 =

    type Customer = {
        Name : string
        IsRegistered : bool
        IsEligible : bool
    }

    type CalculateTotal = Customer -> decimal -> decimal

    let calculateTotal : CalculateTotal =
        fun customer spend ->
            let discount = 
                let rate = if customer.IsRegistered && customer.IsEligible && spend >= 100.0M then 0.1M else 0.0M
                spend * rate
            spend - discount

    let validate =
        let john = { Name = "John"; IsRegistered = true; IsEligible = true }
        let mary = { Name = "Mary"; IsRegistered = true; IsEligible = true }
        let richard = { Name = "Richard"; IsRegistered = true; IsEligible = false }
        let sarah = { Name = "Sarah"; IsRegistered = false; IsEligible = true }
        
        let doCalculateTotal customer spend expected =
            let result = calculateTotal customer spend = expected
            printfn "%s: %b" customer.Name result

        doCalculateTotal john 100.0M 90.0M
        doCalculateTotal mary 99.0M 99.0M
        doCalculateTotal richard 100.0M 100.0M
        doCalculateTotal sarah 100.0M 100.0M
    
module DiscountV2 =

    type RegisteredCustomer = {
        Name : string
        IsEligible : bool
    }

    type Customer = 
        | Registered of RegisteredCustomer
        | Guest of Name:string

    type CalculateTotal = Customer -> decimal -> decimal

    let calculateTotal : CalculateTotal =
        fun customer spend ->
            let discount = 
                let rate =
                    match customer with
                    | Registered c when c.IsEligible && spend >= 100.0M -> 0.1M
                    | _ -> 0.0M
                spend * rate
            spend - discount

    let validate =
        let john = Registered { Name = "John"; IsEligible = true }
        let mary = Registered { Name = "Mary"; IsEligible = true }
        let richard = Registered { Name = "Richard"; IsEligible = false }
        let sarah = Guest "Sarah"

        let doCalculateTotal customer spend expected =
            let result = calculateTotal customer spend = expected
            match customer with
            | Registered c -> printfn "%s: %b" c.Name result
            | Guest c -> printfn "%s: %b" c result

        doCalculateTotal john 100.0M 90.0M
        doCalculateTotal mary 99.0M 99.0M
        doCalculateTotal richard 100.0M 100.0M
        doCalculateTotal sarah 100.0M 100.0M

module DiscountV3 =

    type RegisteredCustomer = {
        Name : string
    }

    type Customer = 
        | Eligible of RegisteredCustomer
        | Registered of RegisteredCustomer
        | Guest of Name:string

    type CalculateTotal = Customer -> decimal -> decimal

    let calculateTotal : CalculateTotal =
        fun customer spend ->
            let discount =
                let rate =
                    match customer with
                    | Eligible _ when spend >= 100.0M -> 0.1M
                    | _ -> 0.0M
                spend * rate
            spend - discount

    let validate =
        let john = Eligible { Name = "John" }
        let mary = Eligible { Name = "Mary" }
        let richard = Registered { Name = "Richard" }
        let sarah = Guest "Sarah"

        let doCalculateTotal customer spend expected =
            let result = calculateTotal customer spend = expected
            match customer with
            | Eligible c -> printfn "%s: %b" c.Name result
            | Registered c -> printfn "%s: %b" c.Name result
            | Guest c -> printfn "%s: %b" c result

        doCalculateTotal john 100.0M 90.0M
        doCalculateTotal mary 99.0M 99.0M
        doCalculateTotal richard 100.0M 100.0M
        doCalculateTotal sarah 100.0M 100.0M

module DiscountV4 =

    type CustomerName = string
    type Spend = decimal
    type Total = decimal

    type RegisteredCustomer = {
        Name : CustomerName
    }

    type Customer = 
        | Eligible of RegisteredCustomer
        | Registered of RegisteredCustomer
        | Guest of Name:CustomerName

    type CalculateTotal = Customer -> Spend -> Total

    let calculateTotal : CalculateTotal =
        fun customer spend ->
            let discount = 
                let rate =
                    match customer with
                    | Eligible _ when spend >= 100.0M -> 0.1M
                    | _ -> 0.0M
                spend * rate
            spend - discount

    let validate =
        let john = Eligible { Name = "John" }
        let mary = Eligible { Name = "Mary" }
        let richard = Registered { Name = "Richard" }
        let sarah = Guest "Sarah"

        let doCalculateTotal customer spend expected =
            let result = calculateTotal customer spend = expected
            match customer with
            | Eligible c -> printfn "%s: %b" c.Name result
            | Registered c -> printfn "%s: %b" c.Name result
            | Guest c -> printfn "%s: %b" c result

        doCalculateTotal john 100.0M 90.0M
        doCalculateTotal mary 99.0M 99.0M
        doCalculateTotal richard 100.0M 100.0M
        doCalculateTotal sarah 100.0M 100.0M

module DiscountV5 =

    type CustomerName = CustomerName of string
    type Spend = decimal
    type Total = decimal

    type RegisteredCustomer = {
        Name : CustomerName
    }

    type Customer = 
        | Eligible of RegisteredCustomer
        | Registered of RegisteredCustomer
        | Guest of Name:CustomerName

    type CalculateTotal = Customer -> Spend -> Total

    let calculateTotal : CalculateTotal =
        fun customer spend ->
            let discount = 
                let rate =
                    match customer with
                    | Eligible _ when spend >= 100.0M -> 0.1M
                    | _ -> 0.0M
                spend * rate
            spend - discount

    let validate =
        let john = Eligible { Name = CustomerName "John" }
        let mary = Eligible { Name = CustomerName "Mary" }
        let richard = Registered { Name = CustomerName "Richard" }
        let sarah = Guest (CustomerName "Sarah")

        let doCalculateTotal customer spend expected =
            let result = calculateTotal customer spend = expected
            match customer with
            | Eligible c -> 
                let (CustomerName name) = c.Name
                printfn "%s: %b" name result
            | Registered c -> 
                let (CustomerName name) = c.Name    
                printfn "%s: %b" name result
            | Guest c ->         
                let (CustomerName name) = c
                printfn "%s: %b" name result

        doCalculateTotal john 100.0M 90.0M
        doCalculateTotal mary 99.0M 99.0M
        doCalculateTotal richard 100.0M 100.0M
        doCalculateTotal sarah 100.0M 100.0M

module DiscountV6 =

    type CustomerName = private CustomerName of string

    [<RequireQualifiedAccess>]
    module CustomerName =
        let create input =
            CustomerName input
        let value (CustomerName input) = input

    type Spend = decimal
    type Total = decimal

    type RegisteredCustomer = {
        Name : CustomerName
    }

    type Customer = 
        | Eligible of RegisteredCustomer
        | Registered of RegisteredCustomer
        | Guest of Name:CustomerName

    type CalculateTotal = Customer -> Spend -> Total

    let calculateTotal : CalculateTotal =
        fun customer spend ->
            let discount = 
                let rate =
                    match customer with
                    | Eligible _ when spend >= 100.0M -> 0.1M
                    | _ -> 0.0M
                spend * rate
            spend - discount

    let validate =
        let john = Eligible { Name = CustomerName.create "John" }
        let mary = Eligible { Name = CustomerName.create "Mary" }
        let richard = Registered { Name = CustomerName.create "Richard" }
        let sarah = Guest (CustomerName.create "Sarah")

        let doCalculateTotal customer spend expected =
            let result = calculateTotal customer spend = expected
            match customer with
            | Eligible c -> printfn "%s: %b" (CustomerName.value c.Name) result
            | Registered c -> printfn "%s: %b" (CustomerName.value c.Name) result
            | Guest n -> printfn "%s: %b" (CustomerName.value n) result

        doCalculateTotal john 100.0M 90.0M
        doCalculateTotal mary 99.0M 99.0M
        doCalculateTotal richard 100.0M 100.0M
        doCalculateTotal sarah 100.0M 100.0M

