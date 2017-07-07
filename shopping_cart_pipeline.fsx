open System

#load "RoP.fsx"
open RoP

#load "Lenses.fsx"
open Lenses


(* ---- requirements ----
implement a shopping cart/order processing pipeline with following steps:
1. check that all products in order still exist in the catalog
2. check that all products in order have enough stock
3. update order with latest product info (product names and prices)
4. apply discounts if any
5. calculate totals for the order
6. check that order total is more than minimum allowed value (10$) 
*)


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
    Id: Guid
    Lines: OrderLine list
    TotalDiscount: decimal
    Total: decimal
}

type Product = {
    ProductId: string
    Name: string
    Price: decimal
    Discount: decimal option
}

type Error = 
    | ProductNotFound of string
    | ProductOutOfStock of string * decimal * decimal 
    | OrderTotalIsTooLow of decimal * decimal

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
                                                            | None -> Some(ProductOutOfStock((id, quantity, 0m)))
                                                            | Some(s) when s < quantity -> Some(ProductOutOfStock((id, quantity, s)))
                                                            | _ -> None)
    match outOfStock with
    | [] -> Success(order)
    | n -> Failure(n)

let Lines: Lens<Order, OrderLine list> = {
    get = fun order -> order.Lines
    set = fun newLines order -> {order with Lines = newLines}
}
let updateProductInfo (getProduct: string -> Product option) = 
    Lines.update (List.map (fun l -> match getProduct l.ProductId with
                                      | Some(p) -> {l with ProductName = p.Name
                                                           ListPrice = p.Price
                                                           Discount = p.Discount}
                                      | None -> l))

let calculateDiscounts = 
    Lines.update 
        (List.map (fun l -> match l.Discount with
                              | Some(discount) -> {l with DiscountedPrice = l.ListPrice - l.ListPrice * discount}
                              | None -> l))

let calculateLinesTotal = 
    Lines.update (List.map (fun l -> {l with Total = l.DiscountedPrice * l.Quantity 
                                             TotalDiscount = (l.ListPrice - l.DiscountedPrice) * l.Quantity }))
    
let calcualteOrderTotal order = 
    {order with Total = order.Lines |> List.sumBy (fun l -> l.Total)
                TotalDiscount=order.Lines |> List.sumBy (fun l -> l.TotalDiscount)}:Order

let checkOrderTotal minAllowedTotal order = 
    match order.Total with
    | t when t < minAllowedTotal -> Failure ([OrderTotalIsTooLow (t, minAllowedTotal)])
    | _ -> Success(order)

//Pipeline

let catalog = [
    {ProductId="SKU111"; Name="Green Pants"; Price=5m; Discount=None}
    {ProductId="SKU222"; Name="White Shirt"; Price=15m; Discount=Some(0.1m)}
    {ProductId="SKU333"; Name="Blue Dress";  Price=150m; Discount=Some(0.15m)}
]

let getProduct id = catalog |> List.tryFind (fun p -> p.ProductId = id)
let getProductStock id = Some(100m)

let pipeline =
    checkProductsExist getProduct
    >> bind (checkEnoughStock getProductStock)
    >> map (updateProductInfo getProduct)
    >> map calculateDiscounts
    >> map calculateLinesTotal
    >> map calcualteOrderTotal
    >> bind (checkOrderTotal 10m)

let addLine productId quantity = 
    {Id=Guid.NewGuid(); ProductId=productId; Quantity=quantity; ProductName=""; ListPrice=0m; Discount=None; DiscountedPrice=0m; TotalDiscount=0m; Total=0m; }

let order = {
    Id = Guid.NewGuid()
    Lines = [
        (addLine "SKU111" 3m)
        (addLine "SKU222" 1m)
        (addLine "SKU333" 50m)
        (addLine "SKU333" 51m)
    ]
    TotalDiscount=0m
    Total=0m
}
let order' = pipeline order

(*
this is all good, but what if we don't want to stop on the first error?
New requirement:
- pipeline must accumulate all cart errors, e.g. product is out of stock AND cart total is too low 

Solution:
we should differenciate between "validation errors" and "failures"
validation error = cart total is too low
failure = connection to "inventory" service failed 
*)

type ValidationError = 
    | ProductNotFound of string
    | ProductOutOfStock of string * decimal * decimal 
    | OrderTotalIsTooLow of decimal * decimal

type Failure = 
    | ServiceUnawailable

type ValidatedOrder = ValidatedOrder of Order * (ValidationError list)

let checkProductsExist' getProduct vOrder : Result<ValidatedOrder, Failure> = 
    let (ValidatedOrder(order, errors)) = vOrder
    let addProduct id products = match getProduct id with
                                 | Success p -> Success((id,p)::products)
                                 | Failure e -> Failure e
    order.Lines 
    |> List.scan (fun r line -> r |> bind (addProduct line.ProductId)) (Success [])
    |> List.last
    |> map (List.choose (function
                         | id, None -> Some(ProductNotFound(id))
                         | _ -> None))
    |> map (fun e -> ValidatedOrder(order, errors @ e))

let checkEnoughStock' getProductStock vOrder : Result<ValidatedOrder, Failure> = 
    let (ValidatedOrder(order, errors)) = vOrder
    let addStock id quantity stocks = match getProductStock id with
                                         | Success stock -> Success((id, quantity, stock)::stocks)
                                         | Failure e -> Failure e
    order.Lines 
    |> List.groupBy (fun l -> l.ProductId)
    |> List.map(fun (id, lines) -> (id, lines |> List.sumBy (fun l -> l.Quantity))) 
    |> List.scan (fun r (id, q) -> r |> bind (addStock id q)) (Success [])
    |> List.last
    |> map (List.choose (function
                         | (id, quantity, None)
                            -> Some(ProductOutOfStock((id, quantity, 0m)))
                         | (id, quantity, Some(stock)) when stock < quantity 
                            -> Some(ProductOutOfStock((id, quantity, stock)))
                         | _ -> None))
    |> map (fun e -> ValidatedOrder(order, errors @ e))

let ValidatedLines: Lens<ValidatedOrder, OrderLine list> = {
    get = fun order -> match order with | ValidatedOrder(o, e) -> o.Lines
    set = fun newLines order -> match order with | ValidatedOrder(o, e) -> ValidatedOrder({o with Lines = newLines}, e)
}

let ValidatedOrderLense: Lens<ValidatedOrder, Order> = {
    get = fun order -> match order with | ValidatedOrder(o, e) -> o
    set = fun newOrder order -> match order with | ValidatedOrder(o, e) -> ValidatedOrder(newOrder, e)
}

let updateProductInfo' getProduct = 
    ValidatedLines.update 
        (List.map (fun l -> match getProduct l.ProductId with
                              | Some(p) -> {l with ProductName = p.Name
                                                   ListPrice = p.Price
                                                   DiscountedPrice = p.Price}
                              | None -> l))

let calculateDiscounts' = 
    ValidatedLines.update 
        (List.map (fun l -> match l.Discount with
                              | Some(discount) -> {l with DiscountedPrice = l.ListPrice - l.ListPrice * discount }
                              | None -> {l with Discount = None})) 
    
let calculateLinesTotal' = 
    ValidatedLines.update (List.map (fun l -> {l with Total = l.DiscountedPrice * l.Quantity 
                                                      TotalDiscount = (l.ListPrice - l.DiscountedPrice) * l.Quantity }))
    
let calcualteOrderTotal' = 
    ValidatedOrderLense.update (fun o -> {o with Total = o.Lines |> List.sumBy (fun l -> l.Total)
                                                 TotalDiscount=o.Lines |> List.sumBy (fun l -> l.TotalDiscount)}:Order)

let checkOrderTotal' minAllowedTotal vOrder = 
    let (ValidatedOrder(order, errors)) = vOrder
    match order.Total with
    | t when t < minAllowedTotal -> ValidatedOrder(order, [OrderTotalIsTooLow (t, minAllowedTotal)] @ errors)
    | _ -> vOrder

let tryGetProduct = getProduct >> Success
let tryGetProductStock id = Success(Some(100m))

let pipelineWithValidation =
    checkProductsExist' tryGetProduct
    >> bind (checkEnoughStock' tryGetProductStock)
    >> map (updateProductInfo' (tryGetProduct >> getOption))
    >> map calculateDiscounts'
    >> map (calculateLinesTotal')
    >> map (calcualteOrderTotal')
    >> map (checkOrderTotal' 10m)

let order2 = {
    Id = Guid.NewGuid()
    Lines = [
        (addLine "SKU111" 1m)
        (addLine "SKU222" 2m)
        (addLine "SKU333" 5m)
    ]
    TotalDiscount=0m
    Total=0m
}
let order2' = (ValidatedOrder(order2, [])) |> pipelineWithValidation