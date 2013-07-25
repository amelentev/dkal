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
  let _initialWorld = _ctx.MkConst("__initialworld__", Z3TypesUtil.getZ3TypeSort(_types.getZ3WorldSort(), _ctx))
  let mutable _domains= Dictionary<IType, HashSet<IConst>>()
  
  member tr.setVariableDomains(domains: Dictionary<IType, HashSet<IConst>>) =
    _domains <- domains

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

  member private tr.translateSaidInfon(ppal: ITerm, t: ITerm) =
    // "said" modality actually needs to be translated folded back from the beginning
    // that is, given "A said B said X" the first accessibility is to the world of "A said" and from then to "B said"
    // ie, [A][B]X --> [X accA accB]
    Z3Expr(_ctx.MkTrue()) :> ITranslatedExpr

  interface ITranslator with
    member translator.translate(term: ITerm) =
      let escapeAndTerminate (str: string) =
        // not sure which characters are accepted
        // "_" + str.Replace(" ", "_") + "_"
        str

      let rec doBasicTranslation(term: ITerm) =
          let translatedTerm=
              match term with
                | PrincipalConstant(t) -> Z3Expr(_ctx.MkConst(t, Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType("Dkal.Principal"), _ctx))) :> ITranslatedExpr
                | SubstrateConstant(t) -> match t with
                                            | :? int as n -> Z3Expr(_ctx.MkInt(n)) :> ITranslatedExpr
                                            | :? double as n -> Z3Expr(_ctx.MkReal(n.ToString())) :> ITranslatedExpr
                                            | :? string as n -> Z3Expr(_ctx.MkConst(n, Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType("System.String"), _ctx))) :> ITranslatedExpr
                                            | :? System.DateTime as n -> Z3Expr(_ctx.MkConst(n.ToString(), Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType("System.DateTime"), _ctx))) :> ITranslatedExpr
                                            | :? IConst as c -> doBasicTranslation(c)
                                            | _ -> failwith (String.Format("Const type unrecognized {0}", t.GetType().FullName))
                | EmptyInfon(t) -> Z3Expr(_ctx.MkTrue()) :> ITranslatedExpr
                | SaidInfon(ppal, t) -> translator.translateSaidInfon(ppal, t) // TODO ignore for now, results may not make sense in these cases
                | AsInfon(t) -> translator.domainRestrictionFromQuery(t)   // TODO needs to be queried into the substrate to know if true or not
                | AndInfon(t) -> Z3Expr(_ctx.MkAnd(t |>
                                                   List.map (fun x -> doBasicTranslation(x).getUnderlyingExpr() :?> BoolExpr) |>
                                                   List.toArray)) :> ITranslatedExpr
                | NotInfon(t) -> Z3Expr(_ctx.MkNot(doBasicTranslation(t).getUnderlyingExpr() :?> BoolExpr)) :> ITranslatedExpr
                | OrInfon(t) -> Z3Expr(_ctx.MkOr(t |>
                                                   List.map (fun x -> doBasicTranslation(x).getUnderlyingExpr() :?> BoolExpr) |>
                                                   List.toArray)) :> ITranslatedExpr
                | ImpliesInfon(a, b) -> Z3Expr(_ctx.MkImplies(doBasicTranslation(a).getUnderlyingExpr() :?> BoolExpr,
                                                              doBasicTranslation(b).getUnderlyingExpr() :?> BoolExpr))
                                     :> ITranslatedExpr
                | Var(t) ->
                  Z3Expr(_ctx.MkConst(t.Name, Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType(t.Type.FullName), _ctx))) :> ITranslatedExpr
                | Forall(t) ->
                  Z3Expr(_ctx.MkForall(
                                      [|((Z3Expr(_ctx.MkConst((fst t).Name, Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType((fst t).Type.FullName), _ctx))) :> ITranslatedExpr).getUnderlyingExpr()) :?> Expr|],
                                      doBasicTranslation(snd t).getUnderlyingExpr() :?> BoolExpr)) :> ITranslatedExpr
                // TODO Relational app infons need to be framed into a particular world (universally quantified)
                | App(f, args) -> Z3Expr(_ctx.MkApp(_rels.[f.Name], args |>
                                                                         List.map (fun x -> doBasicTranslation(x).getUnderlyingExpr() :?> Expr) |>
                                                                         List.toArray |>
                                                                         Array.append [|_initialWorld|] )) :> ITranslatedExpr
                | t -> failwith (String.Format("Translation not implemented {0}", t))
          translatedTerm

      // as a general rule, we need an "initial" world over which the infon holds, therefore every translation will be quantified for a special initial world
      // unless it is a basic term (variable / constant)
      let translatedTerm= doBasicTranslation(term)
      match term with
        | App(f, args) -> Z3Expr(_ctx.MkForall([|_initialWorld|], translatedTerm.getUnderlyingExpr() :?> Expr)) :> ITranslatedExpr
        | _ -> translatedTerm