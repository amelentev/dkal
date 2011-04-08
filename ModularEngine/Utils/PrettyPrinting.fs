namespace Microsoft.Research.Dkal.Utils.PrettyPrinting

  type PrettyPrintToken = 
  | TabToken 
  | UntabToken 
  | NewLineToken 
  | TextToken of string 
  | ManyTokens of List<PrettyPrintToken>

  type PrettyPrinter() =
    static member PrettyPrint ps = 
      let rec doPrettyPrint ps i = 
        match ps, i with
          | [], _ -> ""
          | TabToken::ps, i -> doPrettyPrint ps (i+1)
          | UntabToken::ps, i -> doPrettyPrint ps (i-1)
          | NewLineToken::ps, i -> "\r\n" + (String.replicate i "    ") + doPrettyPrint ps i
          | (TextToken s)::ps, i -> s + doPrettyPrint ps i
          | (ManyTokens ps1)::ps2, i -> doPrettyPrint ps1 i + doPrettyPrint ps2 i
      doPrettyPrint ps 0

