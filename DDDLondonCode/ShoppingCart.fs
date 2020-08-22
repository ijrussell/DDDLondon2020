namespace DDDLondonCode

module ShoppingCartVersion1 =

    type ProductCode = ProductCode of string

    type Item = {
        ProductCode : ProductCode
        Quantity : int
    }

    type ActiveCart = {
        Items : Item list
    }

    type CompletedCart = {
        Items : Item list
    }

    type ShoppingCart = 
        | Empty
        | Active of ActiveCart
        | Completed of CompletedCart

    type AddItem = ShoppingCart -> Item -> ShoppingCart
    type RemoveItem = ShoppingCart -> Item -> ShoppingCart
    type ClearItems = ShoppingCart -> ShoppingCart
    type Complete = ShoppingCart -> ShoppingCart

    let addItem : AddItem =
        fun cart item ->
            match cart with
            | Empty -> Active { Items = [ item ] }
            | Active c -> Active { c with Items = item :: c.Items }
            | Completed _ -> cart 

    let removeItem : RemoveItem =
        fun cart item ->
            match cart with
            | Empty -> cart
            | Active c -> 
                match c.Items.Length with
                | 1 -> Empty
                | _ -> Active { c with Items = c.Items.Tail }
            | Completed _ -> cart 

    let clearItems : ClearItems =
        fun cart ->
            match cart with
            | Active _ -> Empty
            | _ -> cart

    let complete : Complete =
        fun cart ->
            match cart with
            | Active c -> Completed { Items = c.Items }
            | _ -> cart

    let newcart = Empty
    let emptyActive = Active { Items = List.empty }

module ShoppingCartVersion2 =

    type ProductCode = ProductCode of string

    type Item = {
        ProductCode : ProductCode
        Quantity : int
    }

    type NonEmptyList = {
        First : Item
        Rest : Item list
    }

    [<RequireQualifiedAccessAttribute>]
    module NonEmptyList =

        let create (first:Item) (rest:Item list) =
            { First = first; Rest = rest }
        
        let length (list:NonEmptyList) = 
            list.Rest.Length + 1

        let addFirst (first:Item) (existing:NonEmptyList) =
            { First = first; Rest = existing.First :: existing.Rest }

        let add (item:Item) (existing:NonEmptyList) =
            { First = existing.First; Rest = item :: existing.Rest }

    type ActiveCart = {
        Items : NonEmptyList
    }

    type CompletedCart = {
        Items : NonEmptyList
    }

    type ShoppingCart = 
        | Empty
        | Active of ActiveCart
        | Completed of CompletedCart

    type AddItem = ShoppingCart -> Item -> ShoppingCart
    type RemoveItem = ShoppingCart -> Item -> ShoppingCart
    type ClearItems = ShoppingCart -> ShoppingCart
    type Complete = ShoppingCart -> ShoppingCart

    let addItem : AddItem =
        fun cart item ->
            match cart with
            | Empty -> Active { Items = NonEmptyList.create item [] }
            | Active c -> Active { c with Items = NonEmptyList.addFirst item c.Items }
            | Completed _ -> cart 

    let removeItem : RemoveItem =
        fun cart item ->
            match cart with
            | Empty -> cart
            | Active c -> 
                match NonEmptyList.length c.Items with
                | 1 -> Empty
                | _ -> Active { Items = NonEmptyList.create c.Items.Rest.Head c.Items.Rest.Tail }
            | Completed _ -> cart 

    let clearItems : ClearItems =
        fun cart ->
            match cart with
            | Active _ -> Empty
            | _ -> cart

    let complete : Complete =
        fun cart ->
            match cart with
            | Active c -> Completed { Items = c.Items }
            | _ -> cart

    let newcart = Empty
    let emptyActive = Active { Items = NonEmptyList.create { ProductCode = ProductCode "T1"; Quantity = 0 } [] }

module ShoppingCartVersion3 =

    type ProductCode = ProductCode of string
    type Quantity = Quantity of int

    [<RequireQualifiedAccess>]
    module Quantity =
        let tryCreate input =
            if input <= 0 then
                failwith "Quantity must be greater than zero"
            Quantity input
        let value (Quantity input) = input

    type Item = {
        ProductCode : ProductCode
        Quantity : Quantity
    }

    type NonEmptyList = {
        First : Item
        Rest : Item list
    }

    [<RequireQualifiedAccessAttribute>]
    module NonEmptyList =

        let create (first:Item) (rest:Item list) =
            { First = first; Rest = rest }
        
        let length (list:NonEmptyList) = 
            list.Rest.Length + 1

        let addFirst (first:Item) (existing:NonEmptyList) =
            { First = first; Rest = existing.First :: existing.Rest }

        let add (item:Item) (existing:NonEmptyList) =
            { First = existing.First; Rest = item :: existing.Rest }

    type ActiveCart = {
        Items : NonEmptyList
    }

    type CompletedCart = {
        Items : NonEmptyList
    }

    type ShoppingCart = 
        | Empty
        | Active of ActiveCart
        | Completed of CompletedCart

    type AddItem = ShoppingCart -> Item -> ShoppingCart
    type RemoveItem = ShoppingCart -> Item -> ShoppingCart
    type ClearItems = ShoppingCart -> ShoppingCart
    type Complete = ShoppingCart -> ShoppingCart

    let addItem : AddItem =
        fun cart item ->
            match cart with
            | Empty -> Active { Items = NonEmptyList.create item [] }
            | Active c -> Active { c with Items = NonEmptyList.addFirst item c.Items }
            | Completed _ -> cart 

    let removeItem : RemoveItem =
        fun cart item ->
            match cart with
            | Empty -> cart
            | Active c -> 
                match NonEmptyList.length c.Items with
                | 1 -> Empty
                | _ -> Active { Items = NonEmptyList.create c.Items.Rest.Head c.Items.Rest.Tail }
            | Completed _ -> cart 

    let clearItems : ClearItems =
        fun cart ->
            match cart with
            | Active _ -> Empty
            | _ -> cart

    let complete : Complete =
        fun cart ->
            match cart with
            | Active c -> Completed { Items = c.Items }
            | _ -> cart

    let newcart = Empty
    let emptyActive = // Exception
        Active { 
            Items = NonEmptyList.create { 
                ProductCode = ProductCode "T1"
                Quantity = Quantity.tryCreate 0 } [] 
        }
