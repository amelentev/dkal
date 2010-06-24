using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;
using DkalController;
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

        //[ClassInitialize]
        //public static void InitMessageController()
        //{
        //    msgcntroller = new MessageController();

        //}

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
        /// Test a list of Assertions after passing an Input dkal stream  as argument
        /// </summary>
        [TestMethod]
        public void ParseStreamTest()
        {
            //
            // TODO: Add test logic here
            //
        }

        /// <summary>
        /// Verify Term object by passing an infon as argument
        /// </summary>
        [TestMethod]
        [DeploymentItem("crocom.dkal")] 
        public void ParseInfonTest()
        {
            MessageController msgcntroller = new MessageController();
            ParsingCtx pctx = msgcntroller.ParsingContext;
            string infon = "_cro implied SITE can read R in records of trial T under the authority of _site";
            string expected = "_cro implied (SITE) can-read (R) in-records-of-trial (T) under-the-authority-of (_site)";
            Ast.Term termObj = pctx.ParseInfon(infon);

            if (termObj == null)
                Assert.Fail("Term object is null");
            Assert.AreEqual(expected, termObj.ToString());
        }

        /// <summary>
        /// Verify Principal object by passing a name as argument. This should return a Principal. The Name property should match the name in the substrate
        /// </summary>
        [TestMethod]
        public void LookupOrAddPrincipalTest()
        {
            //
            // TODO: Add test logic here
            //
        }

    }
}
