module Passengers

module internal String =
    let split separator (s:string) =
        let values = ResizeArray<_>()
        let rec gather start i =
            let add () = s.Substring(start,i-start) |> values.Add
            if i = s.Length then add()
            elif s.[i] = '"' then inQuotes start (i+1) 
            elif s.[i] = separator then add(); gather (i+1) (i+1) 
            else gather start (i+1)
        and inQuotes start i =
            if s.[i] = '"' then gather start (i+1)
            else inQuotes start (i+1)
        gather 0 0
        values.ToArray()

[<AutoOpen>]
module internal Array =
    /// Tally up items that match specified criteria
    let tally criteria items = 
        items |> Array.filter criteria |> Array.length
    /// Percentage of items that match specified criteria
    let percentage criteria items =
        let total = items |> Array.length
        let count = items |> tally criteria
        float count * 100.0 / float total
    let where f xs = Array.filter f xs
    let groupBy f xs =
        xs 
        |> Seq.groupBy f |> Seq.toArray 
        |> Array.map (fun (k,vs) -> k, vs |> Seq.toArray)

open System.IO

let lines = File.ReadAllLines(@"C:\titanic\train.csv").[1..]

type Name = string
type Sex = Male | Female

type Passenger = {
    Survived : bool
    Class : int
    Name : Name
    Sex : Sex
    Age : float option
    SibSp : int
    Parch : int
    Ticket: string
    Fare : float
    Cabin : string
    Embarked : string
    }

let (|Int|) s = int s

let toPassenger = function
    | [|Int passengerId; Int survived; Int pclass; name; sex; age; Int sibSp; Int parch; ticket; fare; cabin; embarked|] ->
        {Survived = survived = 1
         Class = pclass
         Name = name
         Sex = match sex with | "male" -> Male | "female" -> Female | s -> invalidOp s
         Age = match System.Double.TryParse(age) with true, n -> Some n | _ -> None
         SibSp = sibSp
         Parch = parch
         Ticket = ticket
         Fare = float fare
         Cabin = cabin
         Embarked = embarked
        }
        |> Some
    | _ -> None

let allPassengers = lines |> Array.map (String.split ',') |> Array.choose toPassenger