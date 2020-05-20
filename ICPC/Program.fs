module ICPC
open System

let check_characters input char =
    let l = String.length input
    let rec check input charac k length prev = //work through input as a list checking each character
        match charac with
        | [] -> true
        | x::xs -> 
            match k=(length-1) with // end  of input check if last character is '.'
            | false -> 
                match x with 
                |t when t >= 'a' && t <= 'z' -> 
                    match prev with 
                    |'.' -> false
                    |',' -> false
                    |_ -> check input xs (k+1) length x
                |',' -> 
                    match k with
                    |0 -> false //If first character is a comma invalid input string
                    |_ -> check input xs (k+1) length x 
                |' ' -> 
                    match k with
                    |0 -> false //If first character is a comma invalid input string
                    |_ ->  
                        match prev with 
                        |' ' -> false
                        |_ -> check input xs (k+1) length x
                |'.' -> 
                    match k with
                    |0 -> false //If first character is a comma invalid input string
                    |_ -> 
                        match prev with
                        |'.' -> false 
                        |',' -> false 
                        |' ' -> false
                        |_ ->check input xs (k+1) length x
                |_ -> false
            |true ->    
                match x with
                |'.' -> 
                    match prev with 
                    | ' ' -> false 
                    | ',' -> false
                    |'.' -> false
                    |_ -> true
                | _ -> false          
    check input char 0 l ' '


let valid_input input = //checks if the input is valid

    match String.length input >= 2 with
    |true -> 
        let characters = Seq.toList input
        Console.WriteLine(input)
        Console.WriteLine(sprintf "%A" characters)
        match check_characters input characters with
        |true -> true
        |false -> false
    |false -> false

let edit input = 
    let splitline = (fun (line : string) -> Seq.toList (line.Split ','))
    let split = splitline input
    Console.WriteLine(sprintf "%A" split)
    
    (*let rec edit split edited = 
        

    edit split []*)
    Some input 

let commaSprinkler input = 
    match valid_input input with 
    |true -> edit input
    |false -> None

let rivers_characters input char =
    let l = String.length input
    let rec check charac prev k space_count length = //work through input as a list checking each character
        match charac with
        | [] -> 
            match space_count with 
            |0 -> false
            |_ -> true
        | x::xs -> 
            match k = length with
            |false -> 
                match x with
                | t when t >= 'a' && t <= 'z' -> check xs x (k+1) space_count length
                | t when t >= 'A' && t <= 'Z' -> check xs x (k+1) space_count length
                |' ' -> 
                    match k with
                    |0 -> false //space at start
                    |_ ->   
                        match prev with 
                        | ' ' -> false //double space
                        | _ -> check xs x (k+1) (space_count+1) length 
                | _ -> false //invalid character
            |true -> 
                 match x with
                 |' ' -> false 
                 | t when t >= 'a' && t <= 'z' -> check xs x (k+1) space_count length
                 | t when t >= 'A' && t <= 'Z' -> check xs x (k+1) space_count length 
                 |_ -> false
    check char ' ' 0 0 (l-1)

let rivers_input_valid input =
    match String.length input <= 80 with
    |true -> 
        let characters = Seq.toList input
        Console.WriteLine(input)
        Console.WriteLine(sprintf "%A" characters)
        match rivers_characters input characters with
        |true -> true
        |false -> false
    |false -> false

let count_spaces input = 
    let char = Seq.toList input
    let rec count char k =
        match char with
        |[] -> k
        |x::xs -> 
            match x with
            |' ' -> count xs (k+1)
            |_ -> count xs k
    count char 0

let rivers input =
    match rivers_input_valid input with
    |true -> Some (String.length input, count_spaces input)
    |false -> None

[<EntryPoint>]
let main argv =
    Console.WriteLine(commaSprinkler "please sit spot. sit spot, sit. spot here now here.")
    0 // return an integer exit code
