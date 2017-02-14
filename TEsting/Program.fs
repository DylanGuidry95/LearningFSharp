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

module Combat =
    type Player(name : string) =
        let mutable health = 100   
        member this.Name = name     
        member this.Health = health
        member this.TakeDamage(amount : int) =  health <- health - amount
        member this.Attack(target : Player) = target.TakeDamage(10);
    
    let Fighters = [ Player("Bob"); Player("George"); Player("Jose") ]

    let fighterAlive = Fighters.[0].Health > 0 && Fighters.[1].Health > 0 && Fighters.[2].Health > 0 

    let printInfo(player : Player) = printf "\nFighter: %A Health: %i" player.Name player.Health

    let Fight =  for player in Fighters do
                        if player.Health > 0 then
                            Player("Enemy").Attack(player)
                            printInfo(player)             
                            
module MoreLearning = 
    let AddStrings(a : string , b : string, c: string) = a + b + c
    type State (name : string) = 
        member this.Name = name
    type FSM(initState: State) =
        let mutable transitions = []
        let mutable states = [ initState ]
        let mutable currentState = initState
        member this.AddState newState = 
            states <- newState :: states
        member this.GetStates = states
        member this.GetTransitions = transitions
        member this.GetCurrentState = currentState
        member this.AddTransition (start : State, goal : State) = 
                                                    transitions <- AddStrings(start.Name, ">", goal.Name) :: transitions
        member this.TryTransition (goal : State)  = for trans in transitions do
                                                        if trans = AddStrings(currentState.Name, ">", goal.Name) then
                                                            currentState <- goal
                                                            printf "%A" currentState.Name

    let init = State("init")
    let idle = State("idle")
    let walk = State("walk")
    let dead = State("dead")
    let exit = State("exit")

    let stateMachine = FSM init
    stateMachine.AddState idle
    stateMachine.AddState walk
    stateMachine.AddState dead
    stateMachine.AddState exit
    stateMachine.AddTransition(init, idle)
    stateMachine.AddTransition(idle, walk)
    stateMachine.AddTransition(idle, dead)
    stateMachine.AddTransition(walk, idle)
    stateMachine.AddTransition(dead, exit)

    stateMachine.TryTransition(idle)
    stateMachine.TryTransition(init)
    stateMachine.TryTransition(walk)
    stateMachine.TryTransition(dead)
    stateMachine.TryTransition(idle)
    stateMachine.TryTransition(dead)
    stateMachine.TryTransition(init)
    stateMachine.TryTransition(walk)
    stateMachine.TryTransition(exit)

module ReadFromFile = 
    type Readfile() = 
        let file = new System.IO.StreamReader(@"C:\Users\dylan.guidry\Documents\Visual Studio 2015\Projects\LearningFSharp\TEsting\readme1.txt")
        member this.ReadLine = file.ReadToEnd()                                 
        member this.CloseFile = file.Close()

    let a = Readfile()
    printf "%A" a.ReadLine
    a.CloseFile   
     
      