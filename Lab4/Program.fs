open System
open System.IO

type IntTree =
    | IntEmpty
    | IntNode of int * IntTree * IntTree

type StrTree =
    | StrEmpty
    | StrNode of string * StrTree * StrTree

let rec printInOrder tree =
    match tree with
    | IntNode (data, left, right) ->
        printInOrder left
        printfn "Node %d" data
        printInOrder right
    | IntEmpty ->
        ()

let rec inputIntTree () =
    printfn "Введите значение узла(целое число)"
    let input = Console.ReadLine()
    if input = "" then
        IntEmpty
    else
        match Int32.TryParse(input) with
        | true, input ->
            let value = input
            printfn "Левый потомок %d:" value
            let left = inputIntTree()
            printfn "Правый потомок %d:" value
            let right = inputIntTree()
            IntNode(value, left, right)
        | _ ->
            printfn "Введите целое число"
            inputIntTree()

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
        let tree = inputIntTree()
        let newTree = mapTransformTree tree
        printfn "Изначальное дерево"
        printInOrder tree
        printfn "Изменённое дерево"
        printInOrder newTree
    | "2" ->
        printfn "Введите дерево"
        let tree = inputStrTree()
        printfn "Введите символ для поиска"
        let keyChar = Console.ReadLine()
        let treeQuantity = foldTransformTree (countEndingCharTree keyChar) 0 tree
        printfn "Количество узлов с символом в конце: %i" treeQuantity
    | _ ->
        ()
    0