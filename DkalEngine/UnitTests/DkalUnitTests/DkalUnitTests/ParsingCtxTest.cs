﻿using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;
using DkalController;
using E = Microsoft.Research.DkalEngine;
using Microsoft.Research.DkalEngine;

namespace DkalUnitTest
{
    /// <summary>
    /// Summary description for ParsingCtxTest
    /// </summary>
    [TestClass]
    public class ParsingCtxTest
    {
        public ParsingCtxTest()
        {
            //
            // TODO: Add constructor logic here
            //
        }

        private TestContext testContextInstance;
        //private static MessageController msgcntroller;

        /// <summary>
        ///Gets or sets the test context which provides
        ///information about and functionality for the current test run.
        ///</summary>
        public TestContext TestContext
        {
            get
            {
                return testContextInstance;
            }
            set
            {
                testContextInstance = value;
            }
        }

        #region Additional test attributes
        //
        // You can use the following additional attributes as you write your tests:
        //
        // Use ClassInitialize to run code before running the first test in the class
        // [ClassInitialize()]
        // public static void MyClassInitialize(TestContext testContext) { }
        //
        // Use ClassCleanup to run code after all tests in a class have run
        // [ClassCleanup()]
        // public static void MyClassCleanup() { }
        //
        // Use TestInitialize to run code before running each test 
        // [TestInitialize()]
        // public void MyTestInitialize() { }
        //
        // Use TestCleanup to run code after each test has run
        // [TestCleanup()]
        // public void MyTestCleanup() { }
        //
        #endregion

        /// <summary>
        /// Test verifies a list of Assertions after passing an Input dkal stream as argument
        /// </summary>
        [TestMethod]
        public void ParseStreamTest()
        {
            string dkalContext = @"..\..\..\DkalUnitTests\dkalfiles\test.dkal";

            if (!System.IO.File.Exists(dkalContext))
                Assert.Fail("File not found: " + dkalContext);

            MessageController msgcntroller = new MessageController(dkalContext);
            List<E.Ast.Assertion> assertionsList = msgcntroller.AssertionsList;

            if (assertionsList == null)
                Assert.Fail("Assertions List is null");

            bool val = (assertionsList.Count > 0);
            Assert.IsTrue(val, "Assertions not found");
        }

        /// <summary>
        /// Test verifies Term object by passing an infon as argument
        /// </summary>
        [TestMethod]
        public void ParseInfonTest()
        {
            string dkalContext = @"..\..\..\DkalUnitTests\dkalfiles\test.dkal";

            if (!System.IO.File.Exists(dkalContext))
                Assert.Fail("File not found: " + dkalContext);

            try
            {
                MessageController msgcntroller = new MessageController(dkalContext);
                ParsingCtx pctx = msgcntroller.ParsingContext;

                string infon = "_testDriverEngine said hello to _dkalTestEngine";
                string expected = "_testDriverEngine said hello-to (_dkalTestEngine)";
                Ast.Term termObj = pctx.ParseInfon(infon);

                if (termObj == null)
                    Assert.Fail("Term object is null");
                Assert.AreEqual(expected, termObj.ToString());
            }
            catch (Exception ex)
            {
                Assert.Fail("Exception Occurred: " + ex.Message);
            }
        }

        /// <summary>
        /// Test verifies Principal object by passing a name as argument. This should return a Principal. 
        /// The Name property should match the name in the substrate
        /// </summary>
        [TestMethod]
        public void LookupOrAddPrincipalTest()
        {
            string dkalContext = @"..\..\..\DkalUnitTests\dkalfiles\test.dkal";

            if (!System.IO.File.Exists(dkalContext))
                Assert.Fail("File not found: " + dkalContext);

            try
            {
                MessageController msgcntroller = new MessageController(dkalContext);
                ParsingCtx pctx = msgcntroller.ParsingContext;
                string principalName = "_dkalTestEngine";
                Ast.Principal principalObj = pctx.LookupOrAddPrincipal(principalName);

                if (principalObj == null)
                    Assert.Fail("Principal object is null");

                Assert.AreEqual(principalName, principalObj.Name);
            }
            catch (Exception ex)
            {
                Assert.Fail("Exception Occurred: " + ex.Message);
            }
        }

    }
}
