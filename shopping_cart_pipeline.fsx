open System

#load "RoP.fsx"
open RoP

#load "Lenses.fsx"
open Lenses

//Types
type OrderLine = {
    Id: Guid
    ProductId: string
    ProductName: string
    Quantity: decimal
    ListPrice: decimal
    Discount: decimal option
    DiscountedPrice: decimal
    TotalDiscount: decimal
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

//Lenses

let Lines: Lens<Order, OrderLine list> = {
    get = fun order -> order.Lines
    set = fun newLines order -> {order with Lines = newLines}
}

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
    order |> Lines.update (List.map (fun l -> match getProductName l.ProductId with
                                              | Some(name) -> {l with ProductName = name}
                                              | None -> l))

let updatePrices getProductPrice order = 
    order |> Lines.update (List.map (fun l -> match getProductPrice l.ProductId with
                                              | Some(price) -> {l with ListPrice = price; DiscountedPrice = price}
                                              | None -> l))

let applyDiscounts getProductDiscount order = 
    order |> Lines.update (List.map (fun l -> match getProductDiscount l.ProductId with
                                              | Some(discount) -> {l with Discount = Some(discount)
                                                                          DiscountedPrice = l.ListPrice - l.ListPrice * discount }
                                              | None -> {l with Discount = None})) 
    
let calculateLinesTotal order = 
    order |> Lines.update (List.map (fun l -> {l with Total = l.DiscountedPrice * l.Quantity 
                                                      TotalDiscount = (l.ListPrice - l.DiscountedPrice) * l.Quantity }))
    
let calcualteOrderTotal order = 
    {order with Total = order.Lines |> List.sumBy (fun l -> l.Total)
                TotalDiscount=order.Lines |> List.sumBy (fun l -> l.TotalDiscount)}:Order

//Pipeline

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
let getProductName = getProduct >> Option.map (fun p -> p.Name)
let getProductPrice = getProduct >> Option.map (fun p -> p.Price)
let getProductDiscount = getProduct >> Option.bind (fun p -> p.Discount)

let addLine productId quantity = 
    {Id=Guid.NewGuid(); ProductId=productId; Quantity=quantity; ProductName=""; ListPrice=0m; Discount=None; DiscountedPrice=0m; TotalDiscount=0m; Total=0m; }

let order = {
    Lines = [
        (addLine "SKU111" 3m)
        (addLine "SKU222" 1m)
        (addLine "SKU333" 5m)
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