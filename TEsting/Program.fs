// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.



module Learning = 
    let Name = "Dylan"
    printfn "Your name is: \n%A" Name
    let addString a b = a + b
    let test = addString 2 3
    printfn "Add strings: \n%A" test


module Stuff =     
    type Person =
        {Name : string;
         Age : int;}
    let Dylan = {Name = ""; Age = 0}
    let people = [|{Name = "Matthew"; Age = 30}; {Name = "Dylan"; Age = 21}|]
    people.[0] <- {Name = "Bob"; Age = 10}
    printf "%A" people.[0].Name

module Application =
    type DateOfBirth = 
        {Month : string;
        Date : int;
        Year : int;}
    type Person = 
        {Name : string;
        Age : int;
        DOB : DateOfBirth;}
    let Months = ["Januray"; "Febuary"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"]    
    let IsValidBirthdate (p:Person) = List.exists(fun elem -> elem = p.DOB.Month) Months
    let Dylan = {Name = "Dylan"; Age = 21; DOB = {Month = "July"; Date = 3; Year = 1995}}
    let validStuff = IsValidBirthdate Dylan
    printf "%A" validStuff

                                     
    

    

