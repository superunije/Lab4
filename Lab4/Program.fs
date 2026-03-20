open System
open System.IO

type IntTree =
    | IntEmpty
    | IntNode of int * IntTree * IntTree

type StrTree =
    | StrEmpty
    | StrNode of string * StrTree * StrTree

let rec printInOrder tree level =
    match tree with
    | IntEmpty -> ()
    | IntNode (value, left, right) ->
        printInOrder right (level + 1)
        printfn "%s%d" (String.replicate (level * 4) " ") value
        printInOrder left (level + 1)

let rec insert value tree =
    match tree with
    | IntEmpty -> IntNode(value, IntEmpty, IntEmpty)
    | IntNode(v, left, right) ->
        if value < v then
            IntNode(v, insert value left, right)
        else
            IntNode(v, left, insert value right)

let rec inputIntTree tree =
    printfn "Введите значение узла(целое число)"
    let input = Console.ReadLine()
    if input = "" then
        tree
    else
        match Int32.TryParse(input) with
        | true, value ->
            let newTree = insert value tree
            inputIntTree newTree
        | _ ->
            printfn "Введите целое число"
            inputIntTree tree

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

let rec inputStrTree () =
    printfn "Введите значение узла(строка)"
    let input = Console.ReadLine()
    if input = "" then
        StrEmpty
    else
        let value = input
        printfn "Левый потомок %s:" value
        let left = inputStrTree()
        printfn "Правый потомок %s:" value
        let right = inputStrTree()
        StrNode(value, left, right)

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
    match task with
    | "1" ->
        printfn "Введите дерево"
        let tree = inputIntTree IntEmpty
        let newTree = mapTransformTree tree
        printfn "Изначальное дерево"
        printInOrder tree 0
        printfn "Изменённое дерево"
        printInOrder newTree 0
    | "2" ->
        printfn "Введите дерево"
        let tree = inputStrTree()
        printfn "Введите символ для поиска"
        let inputChar = Console.ReadLine()
        let keyChar = string inputChar[0]
        let treeQuantity = foldTransformTree (countEndingCharTree keyChar) 0 tree
        printfn "Количество узлов с символом в конце: %i" treeQuantity
    | _ ->
        ()
    0