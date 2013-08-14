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

namespace Microsoft.Research.Dkal.LogicEngine.Datalog

open System.Collections.Generic
open NLog

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Z3
open Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator
open InfonSimplifier

/// The SimpleEngine uses backwards propagation to derive all possible 
/// Substitutions that will satisfy the given query. Each Substitution will 
/// have an accompanying list of side conditions to be checked against the 
/// substrate(s). Only those Substitutions that pass the side conditions are
/// returned
type DatalogLogicEngine() = 

  let log = LogManager.GetLogger("LogicEngine.Datalog")

  let mutable _signatureProvider: ISignatureProvider option = None
  let mutable _infostrate: IInfostrate option = None
  let mutable _freshVarId = 0

  let _context= new Context()

  interface ILogicEngine with
    member se.Start () = ()
    member se.Stop () = ()

    member se.set_Infostrate (infostrate: IInfostrate) =
      _infostrate <- Some infostrate

    member se.get_Infostrate () =
      _infostrate.Value

    member se.set_SignatureProvider (signatureProvider: ISignatureProvider) =
      _signatureProvider <- Some signatureProvider

    member se.get_SignatureProvider () =
      _signatureProvider.Value

    /// Obtain a list of Substitution with accompanying side conditions (AsInfon
    /// ITerms). Then return only those Substitutions that satisfy all their 
    /// side conditions.
    member se.Derive (target: ITerm) (substs: ISubstitution seq) = 
      log.Debug("Derive {0}", target)
      let datalogTranslator= DatalogTranslator()
      let knowledge= _infostrate.Value.Knowledge
      // TODO solve asInfon instances
      seq {
        for subst in substs do
            let fp= _context.MkFixedpoint()
            let program= datalogTranslator.translateInferenceProblem ( (knowledge |> Seq.map (fun kn -> kn.Apply(subst))
                                                                                  |> Seq.map (fun term -> simplify(term)) |> Seq.toList,
                                                                        [simplify(target.Apply(subst))])
                                                                     )
            
            ()
            // TODO separate thesis from hypotheses in program
            // TODO register relations as func declarations in the fp
            // TODO add rules as rules in the fp
            // TODO add thesis as query in the fp
            // TODO solve
      }

    member se.DeriveJustification (infon: ITerm) (proofTemplate: ITerm) (substs: ISubstitution seq) =
        failwith "Not implemented"

    member se.CheckJustification (evidence: ITerm) = 
        failwith "Not implemented"


