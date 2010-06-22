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
using System.Data;
using System.Data.Common;
using System.Configuration;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VisualStudio.TeamSystem.Data.UnitTesting;

namespace DKAL
{
    [TestClass()]
    public class DatabaseSetup
    {

        [AssemblyInitialize()]
        public static void IntializeAssembly(TestContext ctx)
        {
            //   Setup the test database based on setting in the
            // configuration file
            DatabaseTestClass.TestService.DeployDatabaseProject();
            DatabaseTestClass.TestService.GenerateData();
        }

    }
}
