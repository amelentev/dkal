// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

open System.Text
open System.Collections.Generic
open Fuchu

open Microsoft.Research.Dkal.Ast.Infon.Syntax.Simple
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Syntax.Parsing
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.LogicEngine.PPIL
open Microsoft.Research.Dkal.LogicEngine.PPIL.AST
open Microsoft.Research.Dkal.LogicEngine.PPIL.Stage2
open Microsoft.Research.Dkal.LogicEngine.PPIL.Stage4

let parse s =
    let parser = SimpleParser() :> IInfonParser
    parser.SetParsingContext (new ParsingContext("me"))
    let rels = parser.ParseSignature "relation a() \n relation b() \n relation c() \n relation d() \n relation x() \n relation y() \n relation z() \n relation r(X:System.Int32)"
    parser.ParseInfon s

let translate hyp que =
    let h = hyp |> List.map parse
    let q = que |> List.map parse
    let (H, Q, input) = Stage1.stage1 h q
    H, Q, input

let debugHomonomy s (H:array<Option<AST>>) =
    printf "%s\n" s
    let tostr (h:AST) = s.Substring(h.Key, h.Length)
    H |> Array.iteri (fun i h ->
                    h |> Option.iter (fun h ->
                            if i = h.Key then
                                printf "leader: %d %s\n" i (tostr h)
                            else
                                printf "%d point to %d: %s\n" i h.Key (tostr h)
                            )
                        )

let checkHomonomy (N:IDictionary<int,AST>) (H:IDictionary<int,AST>) =
    let count = ref 0
    H |> Seq.iter (fun kv -> // todo: check N instead
                    let i,h = kv.Key, kv.Value
                    Assert.Equal("homonomy fails", N.[i].ToString(), h.ToString())
                    if i = h.Key then
                        incr count
                  )
    !count

let solve hyp que =
    let (H,Q,inp) = translate hyp que
    let (N, V) = Stage2.constructNodesnVertices (H@Q) inp.Length
    let HO = Stage3.homonomy inp (N,V)
    let T = Stage4.preprocess HO H
    Stage5.stage5 HO T
    Q |> List.map (fun q -> T.[q.Key].Status<>Raw)

[<Tests>]
let tests = 
    "testsuite" =>> [
        "stage1" =>
            fun _ ->
                let (h,q,input) = translate ["p1 said (a() && q said p said b())"] []
                Assert.Equal("ast serialization", "(p1 said (a()&(q said (p said b()))))", input)
                Assert.Equal("ast tostring", "p1 said (a()&q said p said b())", h.Head.ToString())

        "stage2" => 
            fun _ ->
                let (h,q,s) = translate ["p1 said (p2 said a() && q3 said p4 said b())"] []
                let trie = Trie()
                let lst = Stage2.constructTrie trie [] h.Head
                Assert.Equal("trie", "(p1(p2(),q3(p4())))", trie.ToString())

        "stage3 1" =>
            fun _ ->
                let (h,q,s) = translate ["a() && b() -> a() && b()"] []
                let (N, V) = Stage2.constructNodesnVertices h s.Length
                let H = Stage3.homonomy s (N,V)
                //debugHomonomy s H
                Assert.Equal("", 4, checkHomonomy N H)
        "stage3 2" =>
            fun _ ->
                let (h,q,s) = translate ["p said a() && b() -> p said a() && b() -> a() && p said b()"] []
                let (N, V) = Stage2.constructNodesnVertices h s.Length
                let H = Stage3.homonomy s (N,V)
                //debugHomonomy s H
                Assert.Equal("", 8, checkHomonomy N H)

        "stage4" =>
            fun _ ->
                let (h,q,s) = translate ["r(1) && r(2) -> r(3)"; "r(1)"; "r(2)"] []
                let (N, V) = Stage2.constructNodesnVertices h s.Length
                let H = Stage3.homonomy s (N,V)
                //debugHomonomy s H
                let T = Stage4.preprocess H h
                //printf "%A\n" T
                // todo: check
                ()
        "stage5 1" =>
            fun _ ->
                Assert.Equal("->e", 
                  [true],
                  solve ["a()"; "a() -> b()"] 
                        ["b()"])
        "stage5 2" =>
            fun _ ->
                Assert.Equal("saids",
                  [true; true; false],
                  solve ["r said ((p said x()) && (r said x()))"; "r said p said (x() -> y())"; "r said p said (y() -> z())"]
                        ["r said p said z()"; "r said r said x()"; "r said r said z()"])
        "or" =>
            fun _ ->
                Assert.Equal("derivation", 
                  [true; true; true; true; true; true; false; false; true],
                  solve ["a() && b()"; "b()->c()"]
                        ["a() || b()"; "a()||c()"; "c()||d()"; "b()||a()"; "b()&&a()"; "d()||b()"; "d()"; "x() || y()"; "x() -> a()"])
        "said and" =>
            fun _ ->
                Assert.Equal("said and", 
                  [true],
                  solve ["p said r(1)"; "p said p said r(2)"; "p said r(1) -> r(1)"; "p said p said r(2) -> r(2)"]
                        ["r(1) && r(2)"])
    ]

[<EntryPoint>]
let main argv = 
    let r = tests 
          //|> Test.filter (fun x -> x.EndsWith "4") 
          |> run
    System.Console.ReadLine() |> ignore
    r