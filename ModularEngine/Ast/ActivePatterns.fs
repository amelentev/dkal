﻿[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.ActivePatterns

  open Microsoft.Research.Dkal.Ast

  // Rule patterns
  let (|Rule|_|) mt = match mt with
                      | App(f, [cs; cw; a]) when f = primitives.["rule"] -> Some (cs, cw, a)
                      | _ -> None

  // Action patterns
  let (|Seq|_|) mt =  match mt with
                      | App(f, [a1; a2]) when f = primitives.["seq"] -> Some (a1, a2)
                      | _ -> None
  let (|Send|_|) mt = match mt with
                      | App(f, [ppal; mt']) when f = primitives.["send"] -> Some (ppal, mt')
                      | _ -> None
  let (|Learn|_|) mt =  match mt with
                        | App(f, [mt']) when f = primitives.["learn"] -> Some mt'
                        | _ -> None

  // Infon patterns
  let (|AsInfon|_|) mt =  match mt with 
                          | App(f, [mt']) when f = primitives.["asInfon"] -> Some mt'
                          | _ -> None
  let (|AndInfon|_|) mt = match mt with
                          | App(f, mts) when f = primitives.["andInfon"] -> Some mts
                          | _ -> None
  let (|ImpliesInfon|_|) mt = match mt with
                              | App(f, [mt1; mt2]) when f = primitives.["impliesInfon"] -> Some (mt1, mt2)
                              | _ -> None
  let (|SaidInfon|_|) mt = match mt with
                            | App(f, [ppal; mt']) when f = primitives.["saidInfon"] -> Some (ppal, mt')
                            | _ -> None

  // Literal patterns
  let (|Principal|_|) mt =  match mt with
                            | Const(PrincipalConstant(p)) -> Some p
                            | _ -> None
  let (|True|_|) mt = match mt with
                      | Const(BoolConstant(true)) -> Some ()
                      | _ -> None
  let (|False|_|) mt =  match mt with
                        | Const(BoolConstant(false)) -> Some ()
                        | _ -> None
