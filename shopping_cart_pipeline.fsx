open System

//RoP
type Result<'TSuccess, 'TError> = 
    | Success of 'TSuccess
    | Failure of 'TError
    
let bind f x =
    match x with
        | Success s -> f s
        | Failure f -> Failure f

let map f x =
    match x with
        | Success s -> Success(f s)
        | Failure f -> Failure f

//Types
type OrderLine = {
    Id: Guid
    ProductId: string
    ProductName: string
    Quantity: decimal
    ListPrice: decimal
    Discount: decimal option
    DiscountedPrice: decimal
    Total: decimal
}

type Order = {
    Lines: OrderLine list
    TotalDiscount: decimal
    Total: decimal
}

type Error = 
    | ProductNotFound of string
    | ProductOutOfStock of string * decimal 

//TODO Lenses

//Pipeline steps

let checkProductsExist getProduct order = 
    let notFound = order.Lines 
                        |> List.choose (fun l -> match getProduct l.ProductId with 
                                               | None -> Some(ProductNotFound(l.ProductId))
                                               | Some(_) -> None)
    match notFound with
    | [] -> Success(order)
    | n -> Failure(n)

let checkEnoughStock getProductStock order = 
    let outOfStock = order.Lines 
                        |> List.groupBy (fun l -> l.ProductId)
                        |> List.map(fun (id, lines) -> (id, lines |> List.sumBy (fun l -> l.Quantity))) 
                        |> List.choose (fun (id, quantity) -> match getProductStock id with 
                                                                | Some(s) when s < quantity -> Some(ProductOutOfStock((id, s)))
                                                                | _ -> None)
    match outOfStock with
    | [] -> Success(order)
    | n -> Failure(n)

let updateNames getProductName order = 
    let lines = order.Lines
                |> List.map(fun l -> match getProductName(l.ProductId) with
                                     | Some(name) -> {l with ProductName = name}
                                     | None -> l)
    
    {order with Lines = lines}

let updatePrices getProductPrice order = 
    let lines = order.Lines
                |> List.map(fun l -> match getProductPrice(l.ProductId) with
                                     | Some(price) -> {l with ListPrice = price; DiscountedPrice = price}
                                     | None -> l)
    
    {order with Lines = lines}

let applyDiscounts getProductDiscount order = 
    let lines = order.Lines
                |> List.map(fun l -> match getProductDiscount(l.ProductId) with
                                     | Some(discount) -> {l with Discount = Some(discount)
                                                                 DiscountedPrice = l.ListPrice - l.ListPrice * discount }
                                     | None -> {l with Discount = None}) 
    
    {order with Lines = lines}
    
let calculateLinesTotal order = 
    let lines = order.Lines
                |> List.map(fun l -> {l with Total = l.DiscountedPrice * l.Quantity })
    
    {order with Lines = lines}
    
let calcualteOrderTotal order = 
    {order with Total = order.Lines |> List.sumBy (fun l -> l.Total)}:Order

//Sample pipeline

type Product = {
    ProductId: string
    Name: string
    Price: decimal
    Discount: decimal option
}

let catalog = [
    {ProductId="SKU111"; Name="Green Pants"; Price=20m; Discount=None}
    {ProductId="SKU222"; Name="White Shirt"; Price=15m; Discount=Some(0.1m)}
    {ProductId="SKU333"; Name="Blue Dress";  Price=150m; Discount=Some(0.15m)}
]

let getProduct id = catalog |> List.tryFind (fun p -> p.ProductId = id)
let getProductStock id = Some(100m)
let getProductName id = id |> getProduct |> Option.map (fun p -> p.Name)
let getProductPrice id = id |> getProduct |> Option.map (fun p -> p.Price)
let getProductDiscount id = id |> getProduct |> Option.bind (fun p -> p.Discount)
      
let order = {
    Lines = [
        {Id=Guid.NewGuid(); ProductId="SKU111"; Quantity=3m; ProductName=""; ListPrice=0m; Discount=None; DiscountedPrice=0m; Total=0m; }
        {Id=Guid.NewGuid(); ProductId="SKU333"; Quantity=5m; ProductName=""; ListPrice=0m; Discount=None; DiscountedPrice=0m; Total=0m; }
    ]
    TotalDiscount=0m
    Total=0m
}

let pipeline =
    checkProductsExist getProduct
    >> bind (checkEnoughStock getProductStock)
    >> map (updateNames getProductName)
    >> map (updatePrices getProductPrice)
    >> map (applyDiscounts getProductDiscount)
    >> map (calculateLinesTotal)
    >> map (calcualteOrderTotal)
    
let order' = pipeline order