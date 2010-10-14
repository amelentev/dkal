namespace Microsoft.Research.GeneralPDP.Utils

module String =

  let winEndOfLines (s: string) = 
    if s.Contains("\r\n") then
      failwith "string already contains Windows style EOLs"
    else
      s.Replace("\n", "\r\n")