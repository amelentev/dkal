namespace Microsoft.Research.GeneralPDP.Translations.ToDKAL

open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.DkalEngine
open Microsoft.Research.DkalEngine.Ast

open System.Collections.Generic

module Context =

  /// Stores variable assignments used when translating XACML to DKAL.
  /// Variables are used to extract information from attributes with attribute Infons
  /// and later referring to them inside Terms
  type Context(pctx: ParsingCtx) =
    let mutable id = 0
    let ads_vars = new Dictionary<AttributeDesignator, Var>()


    /// Adds a new attribute designator and assign it a unique variable.
    /// Returns the assigned variable
    member c.Add (ad: AttributeDesignator) = 
      if c.ContainsAttributeDesignator ad then failwith ("Expression already in context: " + ad.ToString())
      id <- id + 1
      let varType = match ad with
                    | AttributeDesignatorExp(_, dt, _, _, _) -> 
                        match dt with
                        | IntDatatype -> Type.Int
                        | StringDatatype -> Type.Text
                        | BoolDatatype -> Type.Bool
                        | DoubleDatatype -> Type.Float
                        // | _ -> failwith ("Datatype not supported by DKAL: " + dt.ToString())
                    | _ -> failwith "Expecting Attribute designator when adding to context"
      let var = pctx.MakeVar ("Var" + id.ToString()) varType
      ads_vars.Add(ad, var)
      

    /// Returns true iff the attribute designator is already contained
    /// (i.e., already has a variable assigned to it)
    member c.ContainsAttributeDesignator (ad: AttributeDesignator) =
      ads_vars.ContainsKey(ad)


    /// Returns the variable assigned to an attribute designator
    member c.GetVar (ad: AttributeDesignator) =
      ads_vars.[ad]

                   
    override c.ToString() = sprintf "%A" ads_vars
