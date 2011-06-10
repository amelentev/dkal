using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Xml;
using JimBlackler.DocsByReflection;

namespace DocsByReflectionDemo
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

        // List of assemblies to process
        static Assembly[] assemblies = {  Assembly.LoadFrom("Interfaces.dll"),
                                          Assembly.LoadFrom("Ast.dll"),
                                          Assembly.LoadFrom("Ast.Tree.dll"),
                                          Assembly.LoadFrom("Ast.Infon.dll"), 
                                          Assembly.LoadFrom("Ast.Syntax.Parsing.dll"), 
                                          Assembly.LoadFrom("Ast.Infon.Syntax.Simple.dll"), 
                                          Assembly.LoadFrom("Ast.Infon.Syntax.Typed.dll"), 
                                          Assembly.LoadFrom("Ast.Infon.Syntax.Factories.dll"), 
                                          Assembly.LoadFrom("Ast.Substrate.Basic.Syntax.Simple.dll"), 
                                          Assembly.LoadFrom("Ast.Substrate.Basic.Syntax.Typed.dll"), 
                                          Assembly.LoadFrom("Ast.Substrate.Sql.Syntax.Simple.dll"), 
                                          Assembly.LoadFrom("Ast.Substrate.Sql.Syntax.Typed.dll"), 
                                          Assembly.LoadFrom("Ast.Substrate.Xml.Syntax.Simple.dll"), 
                                          Assembly.LoadFrom("Ast.Substrate.Xml.Syntax.Typed.dll"), 
                                          Assembly.LoadFrom("Executor.Simple.dll"), 
                                          Assembly.LoadFrom("Executor.Factories.dll"), 
                                          Assembly.LoadFrom("Infostrate.Simple.dll"), 
                                          Assembly.LoadFrom("Infostrate.Factories.dll"), 
                                          Assembly.LoadFrom("LogicEngine.Simple.dll"), 
                                          Assembly.LoadFrom("LogicEngine.Factories.dll"), 
                                          Assembly.LoadFrom("MailBox.Simple.dll"), 
                                          Assembly.LoadFrom("MailBox.Factories.dll"), 
                                          Assembly.LoadFrom("Router.dll"), 
                                          Assembly.LoadFrom("Router.Local.dll"), 
                                          Assembly.LoadFrom("Router.Simple.dll"), 
                                          Assembly.LoadFrom("Router.Factories.dll"), 
                                          Assembly.LoadFrom("SignatureProvider.Simple.dll"), 
                                          Assembly.LoadFrom("SignatureProvider.Factories.dll"), 
                                          Assembly.LoadFrom("Substrate.dll"), 
                                          Assembly.LoadFrom("Substrate.Basic.dll"), 
                                          Assembly.LoadFrom("Substrate.Crypto.dll"), 
                                          Assembly.LoadFrom("Substrate.FSharp.dll"), 
                                          Assembly.LoadFrom("Substrate.Sql.dll"), 
                                          Assembly.LoadFrom("Substrate.Xml.dll"), 
                                          Assembly.LoadFrom("Substrate.Factories.dll"), 
                                          Assembly.LoadFrom("Factories.Initializer.dll"), 
                                          Assembly.LoadFrom("Globals.dll"), 
                                          Assembly.LoadFrom("Utils.dll"), 
                                          Assembly.LoadFrom("Dkal.exe"), 
                                          Assembly.LoadFrom("DkalGui.exe"), 
                                          Assembly.LoadFrom("DkalMulti.exe"), 
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
                output.WriteLine("! {{{{{0}}}}} Module", a.Name);
                output.WriteLine("----");
                index.WriteLine("** [{0}|{0} Module]", a.Name);

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
                        output.WriteLine("|| Return type || Method name || Method parameters || Description ||");

                        foreach (MethodData m in t.Methods)
                        {
                            string parameters = "";
                            foreach (TypeData parameter in m.Parameters)
                            {
                                if (parameters.Length > 0)
                                    parameters += ", ";
                                parameters += OutputType(parameter);
                            }
                            output.WriteLine("| {0} | {{{{{1}}}}} | {2} | {3} |", OutputType(m.ReturnType), m.Name, parameters, m.Description);
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
                    output.WriteLine("----");
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
