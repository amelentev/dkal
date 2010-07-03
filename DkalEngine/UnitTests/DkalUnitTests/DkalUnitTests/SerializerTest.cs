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
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using DkalController;
using Microsoft.Research.DkalEngine;
using System.Threading;

namespace DkalUnitTest
{
    /// <summary>
    /// Summary description for SerializerTest
    /// </summary>
    [TestClass]
    public class SerializerTest
    {
        Microsoft.Research.DkalEngine.Ast.Message msg;
        public SerializerTest()
        {
            //
            // TODO: Add constructor logic here
            //
        }

        private TestContext testContextInstance;

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
        /// Serialize a Term object into a string. We could pass an infon and create a Term object 
        /// and then Serialize it in a string. We could verify success by Deserializing 
        /// the Serialized string in Term object and comparing objects
        /// </summary>
        [TestMethod]
        public void SerializeDeserializeTermTest()
        {
            string dkalContext = @"..\..\..\DkalUnitTests\dkalfiles\test.dkal";

            if (!System.IO.File.Exists(dkalContext))
                Assert.Fail("File not found: " + dkalContext);

            try
            {
                MessageController msgcntroller = new MessageController(dkalContext);
                ParsingCtx pctx = msgcntroller.ParsingContext;
                string infon = "_testDriverEngine said hello to _dkalTestEngine";
                Ast.Term termObj = pctx.ParseInfon(infon);

                if (termObj == null)
                    Assert.Fail("Term object is null");

                Serializer obj = new Serializer(pctx);

                string serializedTerm=obj.SerializeTerm(termObj);
                Ast.Term resultTermObj = obj.DeserializeTerm(serializedTerm);

                Assert.AreEqual(termObj, resultTermObj);
            }
            catch (Exception ex)
            {
                Assert.Fail("Exception Occurred: " + ex.Message);
            }
        }

        /// <summary>
        /// Verifies Serialized calls on Message object
        /// </summary>
        [TestMethod]
        public void SerializeDeserializeMessageTest()
        {
            string dkalContext = @"..\..\..\DkalUnitTests\dkalfiles\test.dkal";

            if (!System.IO.File.Exists(dkalContext))
                Assert.Fail("File not found: " + dkalContext);

            try
            {
                MessageController msgcntroller = new MessageController(dkalContext);
                msgcntroller.OnInfonProcessed += new MessageController.InfonProcessedHandler(msgcntroller_OnInfonProcessed);

                ParsingCtx pctx = msgcntroller.ParsingContext;
                string infon = "_testDriverEngine said hello to _dkalTestEngine";

                msgcntroller.SendMessage(infon, null);
                while (this.msg == null)
                    Thread.Sleep(1000);

                Microsoft.Research.DkalEngine.Ast.Message message = this.msg;
                Serializer obj = new Serializer(pctx);

                string serializedMessage = obj.SerializeMessage(this.msg);
                Ast.Message resultMessageObj = obj.DeserializeMessage(serializedMessage);

                Assert.AreEqual(this.msg, resultMessageObj);
            }
            catch (Exception ex)
            {
                Assert.Fail("Exception Occurred: " + ex.Message);
            }
        }

        void msgcntroller_OnInfonProcessed(object sender, DkalInfoEventArgs e)
        {
            if (e.DkalMessage == null)
                throw new ArgumentNullException("DkalMessage");
            msg = e.DkalMessage;
        }

        /// <summary>
        /// Cleanup code
        /// </summary>
        [TestCleanup]
        public void CleanUp()
        {
            this.msg = null;
        }
    }
}
