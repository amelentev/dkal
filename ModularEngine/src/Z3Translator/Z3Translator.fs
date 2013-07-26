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

namespace Microsoft.Research.Dkal.Ast.Translations.Z3Translator

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Substrate
open Microsoft.Z3

open System
open System.Collections.Generic

type Z3TranslatorUtil=

  /// Given a sequence of substitutions that may not have substitutions for some free variable in an infon, it returns
  /// a set of extended substitutions that bind these free variables.
  /// The chosen values are taken from the known domain of the variable type
  static member completeSubstitutions (infon: ITerm) (varDomains: Dictionary<IType,HashSet<IConst>>) (substs : ISubstitution seq) =
    let rec extendSubstitutionForVars substitution (vars:IVar seq) = 
      vars |>
        Seq.fold (fun subsAcc var -> subsAcc |>
                                     Seq.collect (fun (sub:ISubstitution) -> let domain= try 
                                                                                           varDomains.[var.Type]
                                                                                         with
                                                                                           | _ -> HashSet<IConst>()                                     
                                                                             domain |>
                                                                                    Seq.collect (fun mapping -> [sub.Extend (var, Constant(mapping))])
                                                 )
                 ) (seq [substitution])
    substs |> 
      Seq.collect (fun sub -> extendSubstitutionForVars sub (List.filter (fun var -> not (sub.DomainContains(var))) infon.Vars))


type Z3Translator(ctx: Context, types: Z3TypeDefinition, rels: Dictionary<string, FuncDecl>) =
  let _ctx= ctx
  let _rels= rels
  let _types = types
  let mutable _domains= Dictionary<IType, HashSet<IConst>>()
  let mutable _accFun: FuncDecl option = None
  let mutable _initialWorld = None
  let mutable freshWorld = 0
  let WORLD= "newWorld"
  let INITIAL_WORLD=  "__initialworld__"

  do
      _initialWorld <- Some(_ctx.MkConst(INITIAL_WORLD, Z3TypesUtil.getZ3TypeSort(_types.getZ3WorldSort(), _ctx)))

  member tr.setVariableDomains(domains: Dictionary<IType, HashSet<IConst>>) =
    _domains <- domains

  member tr.setAccessibilityFunction(accFun: FuncDecl) =
    _accFun <- Some accFun

  member tr.getZ3TypeSortForDkalType(typ: IType) =
    Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType(typ.FullName), _ctx)

  member tr.domainRestrictionFromQuery (query:ISubstrateQueryTerm) =
    let substs = Z3TranslatorUtil.completeSubstitutions (query) _domains ([Substitution.Id])
    let substs = SubstrateDispatcher.Solve [query] substs
    let expr= substs |>
                      Seq.fold (fun acc sub -> let subDomainTranslation= sub.Domain |> Seq.fold (fun acc var -> let translateVarValue =
                                                                                                                  _ctx.MkEq((tr :> ITranslator).translate(var :> ITerm).getUnderlyingExpr() :?> Expr,
                                                                                                                    (tr :> ITranslator).translate(sub.Apply(var)).getUnderlyingExpr() :?> Expr)
                                                                                                                _ctx.MkAnd([|translateVarValue;acc|])) (_ctx.MkTrue())
                                               (_ctx.MkOr([|acc;subDomainTranslation|]))
                               ) (_ctx.MkFalse())
    Z3Expr(expr) :> ITranslatedExpr

  member private tr.translateSaidInfon(ppal: ITerm, t: ITerm, world: Expr) =
    // "said" modality is translated through the accessibility function of the principal // TODO
    let ppalName,isConstant = match ppal with
                              | :? IConst as ppalConstant -> ppalConstant.Value :?> string, true
                              | :? IVar as var -> var.Name, false
                              | _ -> failwithf "Term {0} does not identify a principal and cannot \"say\"" ppal

    let nextWorld= _ctx.MkConst(WORLD + freshWorld.ToString(), Z3TypesUtil.getZ3TypeSort(_types.getZ3WorldSort(), _ctx))
    freshWorld <- freshWorld + 1
    let ppalConst= _ctx.MkConst(ppalName, Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType("Dkal.Principal"), _ctx))
    let moveWorld = _ctx.MkApp(_accFun.Value, [|ppalConst; world; nextWorld|]) :?> BoolExpr
    let translatedSequent= tr.doBasicTranslation(t, nextWorld)
    let saidImplies= _ctx.MkImplies(moveWorld, (translatedSequent :> ITranslatedExpr).getUnderlyingExpr() :?> BoolExpr)
    Z3Expr(_ctx.MkForall([|nextWorld|], saidImplies))

  member private tr.doBasicTranslation(term: ITerm, world:Expr) =
    let translatedTerm=
        match term with
        | PrincipalConstant(t) -> Z3Expr(_ctx.MkConst(t, Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType("Dkal.Principal"), _ctx))) :> ITranslatedExpr
        | SubstrateConstant(t) -> match t with
                                    | :? int as n -> Z3Expr(_ctx.MkInt(n)) :> ITranslatedExpr
                                    | :? double as n -> Z3Expr(_ctx.MkReal(n.ToString())) :> ITranslatedExpr
                                    | :? string as n -> Z3Expr(_ctx.MkConst(n, Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType("System.String"), _ctx))) :> ITranslatedExpr
                                    | :? System.DateTime as n -> Z3Expr(_ctx.MkConst(n.ToString(), Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType("System.DateTime"), _ctx))) :> ITranslatedExpr
                                    | :? IConst as c -> tr.doBasicTranslation(c, world)
                                    | _ -> failwith (String.Format("Const type unrecognized {0}", t.GetType().FullName))
        | EmptyInfon(t) -> Z3Expr(_ctx.MkTrue()) :> ITranslatedExpr
        | SaidInfon(ppal, t) -> tr.translateSaidInfon(ppal, t, world) :> ITranslatedExpr // TODO ignore for now, results may not make sense in these cases
        | AsInfon(t) -> tr.domainRestrictionFromQuery(t)
        | AndInfon(t) -> Z3Expr(_ctx.MkAnd(t |>
                                            List.map (fun x -> tr.doBasicTranslation(x, world).getUnderlyingExpr() :?> BoolExpr) |>
                                            List.toArray)) :> ITranslatedExpr
        | NotInfon(t) -> Z3Expr(_ctx.MkNot(tr.doBasicTranslation(t, world).getUnderlyingExpr() :?> BoolExpr)) :> ITranslatedExpr
        | OrInfon(t) -> Z3Expr(_ctx.MkOr(t |>
                                            List.map (fun x -> tr.doBasicTranslation(x, world).getUnderlyingExpr() :?> BoolExpr) |>
                                            List.toArray)) :> ITranslatedExpr
        | ImpliesInfon(a, b) -> Z3Expr(_ctx.MkImplies(tr.doBasicTranslation(a, world).getUnderlyingExpr() :?> BoolExpr,
                                                      tr.doBasicTranslation(b, world).getUnderlyingExpr() :?> BoolExpr))
                                :> ITranslatedExpr
        | Var(t) ->
            Z3Expr(_ctx.MkConst(t.Name, Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType(t.Type.FullName), _ctx))) :> ITranslatedExpr
        | Forall(var, term) ->
            Z3Expr(_ctx.MkForall(
                                [|((Z3Expr(_ctx.MkConst(var.Name, Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType(var.Type.FullName), _ctx))) :> ITranslatedExpr).getUnderlyingExpr()) :?> Expr|],
                                tr.doBasicTranslation(term, world).getUnderlyingExpr() :?> BoolExpr)) :> ITranslatedExpr
        // Relational app infons need to be framed into a particular world, as well as any predicate
        | App(f, args) -> Z3Expr(_ctx.MkApp(_rels.[f.Name], args |>
                                                                 List.map (fun x -> tr.doBasicTranslation(x, world).getUnderlyingExpr() :?> Expr) |>
                                                                 List.toArray |>
                                                                 Array.append [|world|] )) :> ITranslatedExpr
        | t -> failwith (String.Format("Translation not implemented {0}", t))
    translatedTerm


  member private tr.doModalTranslation(term: ITerm, world: Expr) =
    let escapeAndTerminate (str: string) =
      // not sure which characters are accepted
      // "_" + str.Replace(" ", "_") + "_"
      str
    tr.doBasicTranslation(term, world)

  interface ITranslator with
    member translator.translate(term: ITerm) =
      // translator.doModalTranslation(term, _initialWorld)
      translator.doBasicTranslation(term, _initialWorld.Value)
