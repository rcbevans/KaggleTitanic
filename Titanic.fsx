//
// Titanic: Machine Learning from Disaster 
//

#load "TallyHo.fs"
#load "DecisionTree.fs"
#r "lib\FSharp.Data.dll"
open TallyHo
open DecisionTree
open FSharp.Data

// Load training data

let [<Literal>] path = "train.csv"
type Train = CsvProvider<path,InferRows=0>
type Passenger = Train.Row

let passengers : Passenger[] = 
    Train.Load(path).Take(600).Data 
    |> Seq.toArray

// 1. Discover statistics - simple features

let female (passenger:Passenger) = (passenger.Sex = "female")
let male (passenger:Passenger) = (passenger.Sex = "male")

let survived (passenger:Passenger) = (passenger.Survived = 1)
let died (passenger:Passenger) = (passenger.Survived = 0)

let underTen (passenger:Passenger) = (passenger.Age < 10.0)
let overFifty (passenger:Passenger) = (passenger.Age > 50.0)

let upperClass (passenger:Passenger) = (passenger.Pclass = 1)

// Female passengers

let females = passengers |> where female
let femaleSurvivors = females |> tally survived
let femaleSurvivorsPc = females |> percentage survived

// Male passengers
let males = passengers |> where male
let maleSurvivors = males |> tally survived
let maleSurvivorsPC = males |> percentage survived

// a) Children under 10
let childrenUnderTen = passengers |> where underTen
let childUnderTenSurvivedPC = childrenUnderTen |> percentage survived

let boysUnderTen = childrenUnderTen |> where male
let boysUnderTenSurvivedPC = boysUnderTen |> percentage survived

let boysUnderTenWhoSurvived = boysUnderTen |> where survived

let girlsUnderTen = childrenUnderTen |> where female
let girlsUnderTenSurvivedPC = girlsUnderTen |> percentage survived
let girldUnderTenWhoSurvived = girlsUnderTen |> where survived

// b) Passesngers over 50
let passengersOverFifty = passengers |> where overFifty
let passengersOverFiftySurvivedPC = passengersOverFifty |> percentage survived

let malePassengersOverFifty = passengersOverFifty |> where male
let malePassengersOverFiftySurvivedPC = malePassengersOverFifty |> percentage survived
let malePassengersOverFiftyWhoSurvived = malePassengersOverFifty |> where survived

let femalePassengersOverFifty = passengersOverFifty |> where female
let femalePassengersOverFiftySurvivedPC = femalePassengersOverFifty |> percentage survived
let femalePassengersOverFiftyWhoSurvived = femalePassengersOverFifty |> where survived

// c) Upper class passengers
let upperClassPassengers = passengers |> where upperClass
let upperClassPassengersSurvivedPC = upperClassPassengers |> percentage survived

let maleUpperClassPassengers = upperClassPassengers |> where male
let maleUpperClassPassengersSurvivedPC = maleUpperClassPassengers |> percentage survived
let maleUpperClassPassengersWhoSurvived = maleUpperClassPassengers |> where survived

let femaleUpperClassPassengers = upperClassPassengers |> where female
let femaleUpperClassPassengersSurvivedPC = femaleUpperClassPassengers |> percentage survived
let femaleUpperClassPassengersWhoSurvived = femaleUpperClassPassengers |> where survived

// 2. Discover statistics - groups  

/// Survival rate of a criterias group
let survivalRate criteria = 
    passengers |> Array.groupBy criteria 
    |> Array.map (fun (key,matching) -> 
        key, matching |> Array.percentage survived
    )

let embarked = survivalRate (fun p -> p.Embarked)

// a) By passenger class
// Your code here <---

// b) By age group (under 10, adult, over 50)
// Your code here <---


// 3. Scoring

let testPassengers : Passenger[] =
    Train.Load(path).Skip(600).Data 
    |> Seq.toArray

let score f = testPassengers |> Array.percentage (fun p -> f p = survived p)

let notSurvived (p:Passenger) = false

let notSurvivedRate = score notSurvived

// a) Score by embarked point
// Your code here <---

// b) Construct function to score over 80%
// Your code here <---


// 4. Decision trees

let labels = 
    [|"sex"; "class"|]

let features (p:Passenger) : obj[] = 
    [|p.Sex; p.Pclass|]

let dataSet : obj[][] =
    [|for p in passengers ->
        [|yield! features p; 
          yield box (p.Survived = 1)|] |]

let tree = createTree(dataSet, labels)

// Classify

let test (p:Passenger) = 
    match classify(tree, labels, features p) with
    | Some(x) -> x
    | None -> mode dataSet
    :?> bool

let treeRate = score test

// a) Optimize features