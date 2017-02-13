// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.



module Learning = 
    let Name = "Dylan"
    printfn "Your name is: \n%A" Name
    let addString a b = a + b
    printfn "Add strings: \n%A"
    


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

module SimpleMath = 
    let Add (a:int,b:int) = a + b;
    let Subtraction (a:int, b:int) = a - b
    let Multiplication (a:int, b:int) = a * b
    let Division(a:int, b:int) = a / b
    let Square(a:int) = a * a   
                           
                           
module VectorMath =
    type Vector2D(xPos : float, yPos : float) =
        let magnitude = sqrt(xPos*xPos + yPos*yPos)
        member this.X = xPos
        member this.Y = yPos
        member this.Magnitude = magnitude
        member this.Dot(rhs : Vector2D) = xPos * rhs.X + yPos * rhs.Y
        member this.Add(rhs : Vector2D) = Vector2D(xPos + rhs.X, yPos + rhs.Y)
        member this.Subtraction(rhs : Vector2D) = Vector2D(xPos - rhs.X, yPos - rhs.Y)
        member this.Scale(scalar : float) = Vector2D(xPos * scalar, yPos * scalar)
        member this.Normalize = Vector2D(xPos / magnitude, yPos / magnitude)
        member this.PrintInfo = printf "X = %f Y = %f" xPos yPos

    type Vector3D(xPos : float, yPos : float, zPos : float) =
        let magnitude = sqrt(xPos*xPos + yPos*yPos + zPos*zPos)
        member this.X = xPos
        member this.Y = yPos
        member this.Z = zPos
        member this.Magnitude = magnitude
        member this.Dot(rhs : Vector3D) = xPos * rhs.X + yPos * rhs.Y + zPos * rhs.Z
        member this.Add(rhs : Vector3D) = Vector3D(xPos + rhs.X, yPos + rhs.Y, zPos + rhs.Z)
        member this.Subtraction(rhs : Vector3D) = Vector3D(xPos - rhs.X, yPos - rhs.Y, zPos - rhs.Z)
        member this.Scale(scalar : float) = Vector3D(xPos * scalar, yPos * scalar, zPos * scalar)
        member this.Normalize = Vector3D(xPos / magnitude, yPos / magnitude, zPos / magnitude)
        member this.Cross(rhs : Vector3D) = Vector3D(yPos * rhs.Z - zPos * rhs.Y, zPos*rhs.X - xPos*rhs.Z, xPos*rhs.Y - yPos * rhs.X)
        member this.PrintInfo = printf "X = %f Y = %f Z = %f" xPos yPos zPos
        
    let test = Vector3D(1.0,2.0,3.0)
    test.PrintInfo

module WhatToDo =
    type Player(name : string) =
        let health = 100        
        member this.Health = health
        member this.TakeDamage(amount : int) = 
    

    

