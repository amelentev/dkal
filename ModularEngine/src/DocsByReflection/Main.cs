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

using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Xml;
using JimBlackler.DocsByReflection;

namespace Microsoft.Research.Dkal.Documentation
{

    class MainClass
    {

        // To store the collected data
        static Dictionary<Assembly, AssemblyData> data = new Dictionary<Assembly, AssemblyData>();

        // Quick access to any type using its assembly.name
        static Dictionary<string, MyTypeData> name2type = new Dictionary<string, MyTypeData>();

        // Points to output directory
        static string outputDir = @"..\..\output";

        // Namespaces that we are intersted in
        static string namespaceOfInterest = "Microsoft.Research.Dkal";

        static string runningPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) + Path.DirectorySeparatorChar;
        // List of assemblies to process
        static Assembly[] assemblies = {  Assembly.LoadFrom(runningPath + "Interfaces.dll"),
                                          Assembly.LoadFrom(runningPath + "Ast.dll"),
                                          Assembly.LoadFrom(runningPath + "Ast.Tree.dll"),
                                          Assembly.LoadFrom(runningPath + "Ast.Infon.dll"), 
                                          Assembly.LoadFrom(runningPath + "Ast.Syntax.Parsing.dll"), 
                                          Assembly.LoadFrom(runningPath + "Ast.Infon.Syntax.Simple.dll"), 
                                          Assembly.LoadFrom(runningPath + "Ast.Infon.Syntax.Typed.dll"), 
                                          Assembly.LoadFrom(runningPath + "Ast.Infon.Syntax.Factories.dll"), 
                                          Assembly.LoadFrom(runningPath + "Ast.Substrate.Basic.Syntax.Simple.dll"), 
                                          Assembly.LoadFrom(runningPath + "Ast.Substrate.Basic.Syntax.Typed.dll"), 
                                          Assembly.LoadFrom(runningPath + "Ast.Substrate.Sql.Syntax.Simple.dll"), 
                                          Assembly.LoadFrom(runningPath + "Ast.Substrate.Sql.Syntax.Typed.dll"), 
                                          Assembly.LoadFrom(runningPath + "Ast.Substrate.Xml.Syntax.Simple.dll"), 
                                          Assembly.LoadFrom(runningPath + "Ast.Substrate.Xml.Syntax.Typed.dll"), 
                                          Assembly.LoadFrom(runningPath + "Executor.Simple.dll"), 
                                          Assembly.LoadFrom(runningPath + "Executor.Factories.dll"), 
                                          Assembly.LoadFrom(runningPath + "Infostrate.Simple.dll"), 
                                          Assembly.LoadFrom(runningPath + "Infostrate.Factories.dll"), 
                                          Assembly.LoadFrom(runningPath + "LogicEngine.Simple.dll"), 
                                          Assembly.LoadFrom(runningPath + "LogicEngine.Factories.dll"), 
                                          Assembly.LoadFrom(runningPath + "MailBox.Simple.dll"), 
                                          Assembly.LoadFrom(runningPath + "MailBox.Factories.dll"), 
                                          Assembly.LoadFrom(runningPath + "Router.dll"), 
                                          Assembly.LoadFrom(runningPath + "Router.Local.dll"), 
                                          Assembly.LoadFrom(runningPath + "Router.Simple.dll"), 
                                          Assembly.LoadFrom(runningPath + "Router.Factories.dll"), 
                                          Assembly.LoadFrom(runningPath + "SignatureProvider.Simple.dll"), 
                                          Assembly.LoadFrom(runningPath + "SignatureProvider.Factories.dll"), 
                                          Assembly.LoadFrom(runningPath + "Substrate.dll"), 
                                          Assembly.LoadFrom(runningPath + "Substrate.Basic.dll"), 
                                          Assembly.LoadFrom(runningPath + "Substrate.Crypto.dll"), 
                                          Assembly.LoadFrom(runningPath + "Substrate.FSharp.dll"), 
                                          Assembly.LoadFrom(runningPath + "Substrate.Sql.dll"), 
                                          Assembly.LoadFrom(runningPath + "Substrate.Xml.dll"), 
                                          Assembly.LoadFrom(runningPath + "Substrate.Factories.dll"), 
                                          Assembly.LoadFrom(runningPath + "Factories.Initializer.dll"), 
                                          Assembly.LoadFrom(runningPath + "Globals.dll"), 
                                          Assembly.LoadFrom(runningPath + "Utils.dll"), 
                                          Assembly.LoadFrom(runningPath + "Dkal.exe"), 
                                          Assembly.LoadFrom(runningPath + "DkalGui.exe"), 
                                          Assembly.LoadFrom(runningPath + "DkalMulti.exe"), 
                                          Assembly.LoadFile(runningPath + "Z3Translator.dll")
                                       };

        class AssemblyData
        {
            public string Name;
            public List<TypeData> Types = new List<TypeData>();
        }

        class TypeData
        {
            public TypeData(Type t)
            {
                if (t == null || t.Name == "Object")
                {
                    this.Name = "Object";
                    this.AssemblyName = "System";
                }
                else
                {
                    // populate name
                    if (t.IsGenericType)
                        this.Name = t.Name.Substring(0, t.Name.IndexOf('`'));
                    else
                        this.Name = t.Name;

                    // populate assembly name
                    this.AssemblyName = t.Assembly.GetName().Name;

                    // populate generic type params
                    var typeParams = new List<TypeData>();
                    foreach (Type paramType in t.GetGenericArguments())
                        typeParams.Add(new TypeData(paramType));
                    this.GenericTypeParameters = typeParams.ToArray();
                }
            }

            public string Name;
            public string AssemblyName;
            public TypeData[] GenericTypeParameters = { };
        }

        class MyTypeData : TypeData
        {
            public MyTypeData(Type t) : base(t) {
                // populate base type
                this.BaseType = new TypeData(t.BaseType);

                // populate interfaces
                var ifaces = new List<TypeData>();
                foreach (Type iface in t.GetInterfaces())
                    ifaces.Add(new TypeData(iface));
                this.Interfaces = ifaces.ToArray();
            }

            public TypeData BaseType;
            public TypeData[] Interfaces = { };
            public AssemblyData DefinedAssembly;
            public List<MethodData> Methods = new List<MethodData>();
            public List<PropertyData> Properties = new List<PropertyData>();
            public string Description;
        }

        class MethodData
        {
            public string Name;
            public TypeData DefinedType;
            public TypeData ReturnType;
            public TypeData[] Parameters;
            public string Description;
            public bool IsStatic = false;
        }

        class PropertyData
        {
            public string Name;
            public TypeData DefinedType;
            public TypeData ReturnType;
            public string Description;
        }

        static void Main(string[] args)
        {
            // Collect data            
            foreach (Assembly a in assemblies)
            {
                data[a] = new AssemblyData();
                data[a].Name = a.GetName().Name;

                foreach (Type t in a.GetExportedTypes())
                {
                    if (
                        // ignore inner classes
                        !t.Name.StartsWith("$") && 
                        // focus on the namespace of interest
                        t.Namespace.StartsWith(namespaceOfInterest) &&
                        // ignore fslex & fsyacc generated code
                        t.Name != "Lexer" && t.Name != "Parser" && t.Name != "token" && t.Name != "Tags" &&
                        !t.FullName.Contains("Parser+")
                    )
                    {
                        MyTypeData td = new MyTypeData(t);
                        data[a].Types.Add(td);
                        name2type[data[a].Name + "." + t.Name] = td;
                        td.DefinedAssembly = data[a];

                        XmlElement tDoc = DocsByReflection.XMLFromType(t);
                        if (tDoc == null || tDoc["summary"] == null)
                        {
                            // this type has no documentation
                            continue;
                        }
                        td.Description = PrepareString(tDoc["summary"].InnerText);

                        foreach (MethodInfo m in t.GetMethods(BindingFlags.DeclaredOnly | BindingFlags.Instance 
                                                                | BindingFlags.Static | BindingFlags.Public))
                        {
                            if (m.DeclaringType != typeof(object) && !m.Name.StartsWith("get_") && !m.Name.StartsWith("set_"))
                            {
                                MethodData md = new MethodData();
                                td.Methods.Add(md);

                                md.Name = m.Name;
                                md.DefinedType = td;
                                md.ReturnType = new TypeData(m.ReturnType);
                                md.Parameters = ParametersToTypeData(m.GetParameters());
                                XmlElement mDoc = DocsByReflection.XMLFromMember(m);
                                if (mDoc != null && mDoc["summary"] != null)
                                    md.Description = PrepareString(mDoc["summary"].InnerText);
                                else
                                    md.Description = "";
                                if (m.IsStatic) 
                                    md.IsStatic = true;
                            }
                        }

                        foreach (PropertyInfo p in t.GetProperties(BindingFlags.DeclaredOnly | BindingFlags.Instance
                                                                    | BindingFlags.Static | BindingFlags.Public))
                        {
                            PropertyData pd = new PropertyData();
                            td.Properties.Add(pd);

                            pd.Name = p.Name;
                            pd.DefinedType = td;
                            pd.ReturnType = new TypeData(p.PropertyType);
                            XmlElement pDoc = DocsByReflection.XMLFromMember(p);
                            if (pDoc != null && pDoc["summary"] != null)
                                pd.Description = PrepareString(pDoc["summary"].InnerText);
                            else
                                pd.Description = "";
                        }
                    }
                }
            }

            // Do a second pass trying to resolve DummyTypeData to MyTypeData when possible
            foreach (AssemblyData a in data.Values)
            {
                foreach (MyTypeData t in a.Types)
                {
                    foreach (MethodData m in t.Methods)
                    {
                        m.ReturnType = ResolveDummyType(m.ReturnType);
                        for (int i=0; i < m.Parameters.Length; i++) {
                            m.Parameters[i] = ResolveDummyType(m.Parameters[i]);
                        }
                    }
                    foreach (PropertyData p in t.Properties)
                    {
                        p.ReturnType = ResolveDummyType(p.ReturnType);
                    }
                }
            }

            // Clean output directory
            if (Directory.Exists(outputDir))
                Directory.Delete(outputDir, true);
            Directory.CreateDirectory(outputDir);

            // Dump data on output directory
            TextWriter index = new StreamWriter(outputDir + @"\index.txt");
            index.WriteLine("* Modules");
            foreach (AssemblyData a in data.Values)
            {
                TextWriter output = new StreamWriter(outputDir + @"\" + a.Name + ".txt");
                output.WriteLine("! {{anchor:top}} {{{{{0}}}}} Module", a.Name);
                output.WriteLine("----");
                index.WriteLine("** [{0}|{0} Module]", a.Name);

                if (a.Types.Count > 0) 
                {
                    output.WriteLine("This module defines the following types:");
                    foreach (MyTypeData t in a.Types)
                    {
                        output.WriteLine("* {0}", OutputType(t));
                    }
                    output.WriteLine("----");

                    foreach (MyTypeData t in a.Types)
                    {
                        output.WriteLine("!! {{anchor:{0}}} {{{{{0}}}}} Type", t.Name);
                        if (t.BaseType.Name != "Object")
                            output.WriteLine("*Base type*: {0}", OutputType(t.BaseType));
                        output.WriteLine(t.Description);

                        if (t.Interfaces.Length > 0)
                        {
                            output.WriteLine();
                            output.WriteLine("!!! Implemented Interfaces");
                            foreach (TypeData it in t.Interfaces)
                            {
                                output.WriteLine("* {0}", OutputType(it));
                            }
                        }

                        if (t.Methods.Count > 0)
                        {
                            output.WriteLine();
                            output.WriteLine("!!! Methods");
                            output.WriteLine("|| Modifiers || Return type || Method name || Method parameters || Description ||");

                            foreach (MethodData m in t.Methods)
                            {
                                string modifiers = "";
                                if (m.IsStatic)
                                    modifiers += "{{static}}";
                                string parameters = "";
                                foreach (TypeData parameter in m.Parameters)
                                {
                                    if (parameters.Length > 0)
                                        parameters += ", ";
                                    parameters += OutputType(parameter);
                                }
                                output.WriteLine("| {0} | {1} | {{{{{2}}}}} | {3} | {4} |", modifiers, OutputType(m.ReturnType), m.Name, parameters, m.Description);
                            }
                        }

                        if (t.Properties.Count > 0)
                        {
                            output.WriteLine();
                            output.WriteLine("!!! Properties");
                            output.WriteLine("|| Return type || Property name || Description ||");

                            foreach (PropertyData p in t.Properties)
                            {
                                output.WriteLine("| {0} | {{{{{1}}}}} | {2} |", OutputType(p.ReturnType), p.Name, p.Description);
                            }
                        }
                        output.WriteLine(">{{[Back to top|#top]}}>", a.Name);
                        output.WriteLine("----");
                    }
                }
                output.WriteLine(">{{Automatically generated on {0}}}>", DateTime.Now);
                output.Close();
            }
            index.Close();

        }

        private static string OutputType(TypeData td)
        {
            if (td is MyTypeData)
            {
                var myTd = ((MyTypeData)td);
                return "[" + myTd.Name + "|" + myTd.DefinedAssembly.Name + " Module#" + myTd.Name + "]";
            }
            else 
            {
                string ret;
                if (td.Name == "Unit")
                    ret = "unit";
                else if (td.Name == "Boolean")
                    ret = "bool";
                else if (td.Name == "String")
                    ret = "string";
                else if (td.Name == "Void")
                    ret = "unit";
                else if (td.Name == "Int32")
                    ret = "int";
                else if (td.Name == "IEnumerable")
                    ret = OutputType(td.GenericTypeParameters[0]) + " seq";
                else if (td.Name == "FSharpOption")
                    ret = OutputType(td.GenericTypeParameters[0]) + " option";
                else if (td.Name == "FSharpList")
                    ret = OutputType(td.GenericTypeParameters[0]) + " list";
                else if (td.Name == "FSharpFunc")
                    ret = OutputType(td.GenericTypeParameters[0]) + " -> " + OutputType(td.GenericTypeParameters[1]);
                else if (td.Name == "Tuple")
                    ret = OutputType(td.GenericTypeParameters[0]) + " * " + OutputType(td.GenericTypeParameters[1]);
                else
                {
                    ret = td.Name;
                    if (td.GenericTypeParameters.Length > 0)
                    {
                        ret += "<";
                        string genericTypeParameters = "";
                        foreach (TypeData genTd in td.GenericTypeParameters)
                        {
                            if (genericTypeParameters.Length > 0)
                                genericTypeParameters += ", ";
                            genericTypeParameters += OutputType(genTd);
                        }
                        ret += genericTypeParameters;
                        ret += ">";
                    }
                }
                return ret;
            }
        }

        private static TypeData ResolveDummyType(TypeData td)
        {
            for (int i = 0; i < td.GenericTypeParameters.Length; i++)
                td.GenericTypeParameters[i] = ResolveDummyType(td.GenericTypeParameters[i]);
                        
            if (td is MyTypeData) {
                MyTypeData myTd = (MyTypeData) td;
                if (myTd.BaseType.Name != "Object")
                    myTd.BaseType = ResolveDummyType(myTd.BaseType);
                for (int i = 0; i < myTd.Interfaces.Length; i++)
                    myTd.Interfaces[i] = ResolveDummyType(myTd.Interfaces[i]);
                return myTd;
            }
            else 
            {
                string fullname = td.AssemblyName + "." + td.Name;
                if (name2type.ContainsKey(fullname))
                {
                    return name2type[fullname];
                }
                else
                {
                    return td;
                }
            }
        }

        private static string PrepareString(string text)
        {
            return text.Trim().Replace("\r\n", " ").Replace("  ", " ");
        }

        private static TypeData[] ParametersToTypeData(ParameterInfo[] parameters)
        {
            var ret = new List<TypeData>();
            foreach (ParameterInfo parameterInfo in parameters)
                ret.Add(new TypeData(parameterInfo.ParameterType));
            return ret.ToArray();
        }

    }
}
