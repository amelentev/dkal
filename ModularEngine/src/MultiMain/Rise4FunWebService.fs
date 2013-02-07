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

namespace Microsoft.Research.Dkal

open System.Net
open System.IO
open System.Runtime.Serialization.Json
open System.Xml
open System.Xml.Linq
open System.Xml.XPath
open System.Diagnostics
open NLog

open Microsoft.Research.Dkal

open System.Reflection

/// Web service for RISE4fun.
/// see http://rise4fun.com/dev/
module Rise4FunWebService =
  let private log = LogManager.GetLogger("Rise4FunWebService")

  let sendMetadata (context: HttpListenerContext) =
    log.Info "sending metadata"
    use jw = JsonReaderWriterFactory.CreateJsonWriter(context.Response.OutputStream)
    jw.WriteStartDocument()
    jw.WriteStartElement("root")
    jw.WriteAttributeString("type", "object")
    jw.WriteElementString("Name", "DKAL")
    jw.WriteElementString("DisplayName","DKAL")
    jw.WriteElementString("Description","Demo of DKAL")
    jw.WriteElementString("Question","How do these principals interact?")
    jw.WriteElementString("Institution","Microsoft Research")
    jw.WriteElementString("InstitutionImageUrl","http://research.microsoft.com/en-us/um/people/gurevich/dkal.png")
    jw.WriteElementString("InstitutionUrl","http://dkal.codeplex.com")
    jw.WriteElementString("MimeType","text/x-csharp")
    jw.WriteElementString("SupportsLanguageSyntax", "true");
    jw.WriteElementString("PrivacyUrl","http://rise4fun.com/privacy")
    jw.WriteStartElement("Samples")
    jw.WriteAttributeString("type", "array")

    for file in Directory.EnumerateFiles(@".", "*.mdkal") do
      jw.WriteStartElement("item")
      jw.WriteAttributeString("type", "object")
      jw.WriteElementString("Name", file.Substring(2, file.Length - ".mdkal".Length - 2))
      jw.WriteElementString("Source", File.ReadAllText(file))
      jw.WriteEndElement()

    jw.WriteEndElement()
    jw.WriteElementString("TermsOfUseUrl","http://rise4fun.com/termsofuse")
    jw.WriteElementString("Title","Distributed Knowledge Authorization Language")
    jw.WriteElementString("Url","http://dkal.codeplex.com")
    jw.WriteElementString("Version","1.0")
    jw.WriteElementString("Email", "dkal@dkal.dkal")
    jw.WriteElementString("SupportEmail", "dkal@dkal.dkal")
    jw.WriteEndElement()
    jw.WriteEndDocument()
    jw.Flush()

  let sendLanguage (context: HttpListenerContext) =
    log.Info "sending language"
    let assemblydir = Directory.GetParent(Assembly.GetEntryAssembly().Location).FullName
    let lang = File.ReadAllBytes(assemblydir + "/language.js")

    context.Response.OutputStream.Write(lang, 0, lang.Length)
    context.Response.OutputStream.Flush()

  let exec args source =
    let file = Path.GetTempFileName()
    try
      File.WriteAllText(file, source)
      use proc = new Process()
      proc.StartInfo.FileName <- "DkalMulti.exe"
      proc.StartInfo.Arguments <- file + " " + (String.concat " " args)
      proc.StartInfo.UseShellExecute <- false
      proc.StartInfo.RedirectStandardOutput <- true
      proc.StartInfo.CreateNoWindow <- true      
      if proc.Start() then
        let output = proc.StandardOutput.ReadToEnd()
        proc.WaitForExit()
        proc.Close()
        output
      else
        "Can't start DkalMulti.exe with args "+proc.StartInfo.Arguments
    finally
      File.Delete file

  let run args (context: HttpListenerContext) =  
    use json = JsonReaderWriterFactory.CreateJsonReader(context.Request.InputStream, XmlDictionaryReaderQuotas())
    let source = XElement.Load(json).XPathSelectElement("//Source").Value
    json.Close()
    log.Info("#### Input:\n{0}", source)

    let output = exec args source
    log.Info("#### Output:\n{0}", output)
  
    use jw = JsonReaderWriterFactory.CreateJsonWriter(context.Response.OutputStream)
    jw.WriteStartDocument()
    jw.WriteStartElement("root")
    jw.WriteAttributeString("type", "object")
    jw.WriteElementString("Version", "1.0")
    jw.WriteStartElement("Outputs")
    jw.WriteAttributeString("type", "array")
    jw.WriteStartElement("item")
    jw.WriteAttributeString("type", "object")
    jw.WriteElementString("MimeType", "text/plain")
    jw.WriteElementString("Value", output)
    jw.WriteEndElement()
    jw.WriteEndElement()
    jw.WriteEndElement()
    jw.WriteEndDocument()
    jw.Flush()

  let startWebService args =
    use listener = new HttpListener()
    listener.Prefixes.Add("http://+:8080/")
    listener.Start() // if access denied then try "> netsh http add urlacl url=http://+:8080/ user=<DOMAIN\user>"
    // also "> netsh advfirewall firewall add rule name="dkal4fun" dir=in action=allow protocol=TCP localport=8080"
    log.Info "Listening 8080 port ..."
    while true do
      let context = listener.GetContext(); // Note: The GetContext method blocks while waiting for a request. 

      match context.Request.HttpMethod, context.Request.Url.AbsolutePath with
      | "GET", "/metadata" -> sendMetadata context
      | "GET", "/language" -> sendLanguage context
      | "POST", "/run" -> run args context
      | m -> 
        log.Info("unknown request {0} {1}", m, context.Request.RawUrl)
        context.Response.StatusCode <- 404

      context.Response.Close()
    listener.Stop();