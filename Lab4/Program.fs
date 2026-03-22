open System
open System.IO

type IntTree =
    | IntEmpty
    | IntNode of int * IntTree * IntTree

type StrTree =
    | StrEmpty
    | StrNode of string * StrTree * StrTree

let rec printIntInOrder tree level =
    match tree with
    | IntEmpty -> ()
    | IntNode (value, left, right) ->
        printIntInOrder right (level + 1)
        for n in 1 .. (level * 4) do 
            printf " "
        printfn "%d" value
        printIntInOrder left (level + 1)

let rec printStrInOrder tree level =
    match tree with
    | StrEmpty -> ()
    | StrNode (value, left, right) ->
        printStrInOrder right (level + 1)
        for n in 1 .. (level * 4) do 
            printf " "
        printfn "%s" value
        printStrInOrder left (level + 1)

let rec insert value tree =
    match tree with
    | IntEmpty -> IntNode(value, IntEmpty, IntEmpty)
    | IntNode(v, left, right) ->
        if value < v then
            IntNode(v, insert value left, right)
        else
            IntNode(v, left, insert value right)

//вставка не рандомная
//let rec inputIntTree tree =
//    printfn "Введите значение узла(целое число)"
//    let input = Console.ReadLine()
//    if input = "" then
//        tree
//    else
//        match Int32.TryParse(input) with
//        | true, value ->
//            let newTree = insert value tree
//            inputIntTree newTree
//        | _ ->
//            printfn "Введите целое число"
//            inputIntTree tree

let rec inputIntTree input tree =
    if input <= 0 then tree
    else
        let rnd = Random()
        let value = rnd.Next(1, 100) // числа от 1 до 99
        inputIntTree (input - 1) (insert value tree)

let rec mapTree func tree =
    match tree with
    | IntEmpty -> IntEmpty
    | IntNode(value, left, right) ->
        IntNode(func value, mapTree func left, mapTree func right)

let transformNumber n =
    let rec loop n acc place =
        if n = 0 then acc
        else
            let digit = n % 10
            let newDigit = 
                if digit = 0 then 
                    1 
                else
                    digit - 1
            loop (n / 10) (acc + newDigit * place) (place * 10)
    if n = 0 then 
        1 
    else 
        loop n 0 1

let mapTransformTree tree =
    mapTree transformNumber tree

//let rec inputStrTree () =
//    printfn "Введите значение узла(строка)"
//    let input = Console.ReadLine()
//    if input = "" then
//        StrEmpty
//    else
//        let value = input
//        printfn "Левый потомок %s:" value
//        let left = inputStrTree()
//        printfn "Правый потомок %s:" value
//        let right = inputStrTree()
//        StrNode(value, left, right)

let rec insertStr value tree =
    match tree with
    | StrEmpty -> StrNode(value, StrEmpty, StrEmpty)
    | StrNode(v, left, right) ->
        if value < v then StrNode(v, insertStr value left, right)
        else StrNode(v, left, insertStr value right)

let rec inputStrTree n tree =
    if n <= 0 then tree
    else
        let rnd = Random()
        let value =
            // случайная строка длиной 3 буквы
            [for _ in 1..3 -> char (rnd.Next(int 'A', int 'Z' + 1)) ]
            |> Array.ofList
            |> String
        inputStrTree (n - 1) (insertStr value tree)

let rec foldTransformTree func acc tree =
    match tree with
    | StrEmpty -> acc
    | StrNode(value, left, right) ->
        let acc' = func acc value
        let acc'' = foldTransformTree func acc' left
        foldTransformTree func acc'' right

let countEndingCharTree (ch : string) acc (value : string) =
    if value.EndsWith(ch) then acc + 1 else acc

[<EntryPoint>]
let main argvs =
    printfn "Введите номер задания"
    let task = Console.ReadLine()
    // надо ввод рандомных значений
    match task with
    | "1" ->
        printf "Введите количество узлов в дереве: "
        let input = Console.ReadLine()
        match Int32.TryParse(input) with
        | true, n when n > 0 ->
            let tree = inputIntTree n IntEmpty
            let newTree = mapTransformTree tree
            printfn "Изначальное дерево"
            printIntInOrder tree 0
            printfn "Изменённое дерево"
            printIntInOrder newTree 0
        | _ ->
            printfn "Введите корректное положительное число"
    | "2" ->
        printf "Введите количество узлов в дереве: "
        let input = Console.ReadLine()
        match Int32.TryParse(input) with
        | true, n when n > 0 ->
            let tree = inputStrTree n StrEmpty
            printfn "Изначальное дерево"
            printStrInOrder tree 0
            printfn "Введите символ для поиска"
            let inputChar = Console.ReadLine()
            let keyChar = string inputChar[0]
            let treeQuantity = foldTransformTree (countEndingCharTree keyChar) 0 tree
            printfn "Количество узлов с символом в конце: %i" treeQuantity
        | _ ->
            printfn "Введите корректное положительное число"
    | _ ->
        ()
    0