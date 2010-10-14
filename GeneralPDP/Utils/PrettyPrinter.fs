namespace Microsoft.Research.GeneralPDP.Utils


module PrettyPrinter = 

  type PrettyPrintToken = TabToken | UntabToken | TextToken of string | ManyTokens of List<PrettyPrintToken>

  let prettyPrint ps = 
    let rec doPrettyPrint ps i = 
      match (ps, i) with
        | [], _ -> ""
        | TabToken::ps, i -> doPrettyPrint ps (i+1)
        | UntabToken::ps, i -> doPrettyPrint ps (i-1)
        | (TextToken s)::ps, i -> (String.replicate i "    ") + s + "\r\n" + doPrettyPrint ps i
        | (ManyTokens ps1)::ps2, i -> doPrettyPrint ps1 i + doPrettyPrint ps2 i
    doPrettyPrint ps 0
  