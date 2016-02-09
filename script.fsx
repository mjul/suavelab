#I "./packages/Suave/lib/net40"
#I "./packages/Chiron/lib/net40"
#I "./packages/Aether/lib/net35"
#I "./packages/FParsec/lib/net40-client"
#r "Suave"
#r "Chiron"
#r "Aether"
#r "FParsec"
#r "System.Runtime.Serialization"

open Suave
open Suave.Http
open Suave.Json
open Suave.Files
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.SuaveConfig
open Suave.WebPart
open Chiron

type Category = {Id: int; Name: string}
type InvestmentFund = { Id: int; Name: string; AnnualCostInPercent: decimal; Isin: string; IsRestricted: bool; Category: Category}

    
// some dummy data
let loadAllProducts () =
    let danishStocks = {Category.Id=1; Name="Danish Stocks"}
    let globalStocks = {Category.Id=2; Name="Global Stocks"}
    let categories = [danishStocks; globalStocks]
    let products =
        [
            {Id=1; Name="Golddigger Danish"; AnnualCostInPercent=1.25m; Isin="DK11111111"; IsRestricted=false; Category=danishStocks}
            {Id=2; Name="Starmanager Danish"; AnnualCostInPercent=1.75m; Isin="DK22222222"; IsRestricted=false; Category=danishStocks}
            {Id=3; Name="Thrifty Danish"; AnnualCostInPercent=0.55m; Isin="DK33333333"; IsRestricted=false; Category=globalStocks}
            {Id=4; Name="Golden Globals"; AnnualCostInPercent=2.25m; Isin="DK44444444"; IsRestricted=false; Category=globalStocks}
            {Id=5; Name="Thrifty Globals"; AnnualCostInPercent=0.55m; Isin="DK55555555"; IsRestricted=false; Category=globalStocks}
            ]
    (categories, products)

let products () =
    let cats, prods = loadAllProducts()
    let p = Seq.head prods
    let c = Seq.head cats
    let serialiseProduct p =
        Object <| Map.ofList [
            "id", Number (decimal p.Id)
            "type", String "product"
            "attributes", Object <| Map.ofList [
                "name", String p.Name
                "annualCostInPercent", Number p.AnnualCostInPercent
                "isin", String p.Isin
                "isRestricted", Bool p.IsRestricted
                ]
            "links", Object <| Map.ofList [
                "category", Object <| Map.ofList [
                    "data", Object <| Map.ofList [
                        "type", String "category"
                        "id", Number (decimal p.Category.Id)]
                    ]
                ]
            ]
    let serialiseCategory (c:Category) =
        Object <| Map.ofList [
            "id", Number (decimal c.Id)
            "type", String "category"
            "attributes", Object <| Map.ofList [
                "name", String c.Name
                ]
            ]
    let dto =
        Object <| Map.ofList [
            "data", Array [for p in prods -> serialiseProduct p]
            "included", Array [for c in cats -> serialiseCategory c]
            "links", Object <| Map.ofList ["self", String "/products"]
            ]
    dto
    


let jsonResponse dto =
    OK(Json.format dto)
    >=> Suave.Writers.setMimeType "application/json; charset=utf-8"
    >=> Suave.Writers.addHeader "Access-Control-Allow-Origin" "*"


let app : WebPart =
    choose
        [Filters.GET >=> choose [
            path "/" >=> OK "Welcome!"
            pathScan "/post/%d" (fun (id) -> OK(string id))
            path "/api/products" >=> jsonResponse(products())
            pathScan "/static/%s" (fun (fname) -> sendFile (sprintf "./static/%s" fname) true)]]



startWebServer defaultConfig app

