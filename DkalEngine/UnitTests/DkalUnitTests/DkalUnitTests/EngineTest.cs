using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Data.SqlClient;
using DkalController;
using Microsoft.Research.DkalEngine;
using System.Threading;
using System.Configuration;

namespace DkalUnitTest
{
    /// <summary>
    /// Summary description for EngineTest
    /// </summary>
    [TestClass]
    public class EngineTest
    {
        Microsoft.Research.DkalEngine.Ast.Message msg;
        public EngineTest()
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
        /// Test validates Substrate Sql Connection String
        /// </summary>
        [TestMethod]
        public void TestConnectionString()
        {
            string conString = ConfigurationManager.AppSettings["SubstrateConnectionString"];

            if (String.IsNullOrEmpty(conString))
                Assert.Fail("SubstrateConnectionString not found in App.Config");

            SqlConnection con = new SqlConnection();
            bool isConnected = false;
            try
            {
                con.ConnectionString = conString;
                con.Open();
                isConnected = true;
                con.Close();
            }
            catch (Exception ex)
            {
                Assert.Fail("Exception occurred in Sql Connection: " + ex.Message);
            }
            finally
            {
                con.Close();
            }
            if (!isConnected)
                Assert.Fail("Sql Connection Failed");

        }

        /// <summary>
        /// Test exercises creation and deletion of Engine 
        /// </summary>
        [TestMethod]
        public void TestEngineCreation()
        {
            string dkalContext = @"..\..\..\DkalUnitTests\dkalfiles\test.dkal";

            if (!System.IO.File.Exists(dkalContext))
                Assert.Fail("File not found: " + dkalContext);

            try
            {
                MessageController msgcntroller = new MessageController(dkalContext);
                Engine e = msgcntroller.EngineInstance;

                Thread.Sleep(2000); // giving some time for the Engine to initalize

                bool val = (e.communications.Count() > 0 && e.filters.Count() > 0);
                Assert.IsTrue(val, "Failed to initialize Engine");
            }
            catch (Exception ex)
            {
                Assert.Fail("Exception Occurred: " + ex.Message);
            }
        }

        /// <summary>
        /// Test verifies AddInfon call to an Engine. This call should result in a Message object to a callback SendMessage() implemented from ICommunicator interface 
        /// </summary>
        [TestMethod]
        public void SendMessageTest()
        {
            string dkalContext = @"..\..\..\DkalUnitTests\dkalfiles\test.dkal";

            if (!System.IO.File.Exists(dkalContext))
                Assert.Fail("File not found: " + dkalContext);

            try
            {
                MessageController msgcntroller = new MessageController(dkalContext);
                msgcntroller.OnInfonProcessed += new MessageController.MyEventHandler(msgcntroller_OnInfonProcessed);

                //ParsingCtx pctx = msgcntroller.ParsingContext;
                string infon = "42 is a good number";
                string expected = "i-like (42)";

                msgcntroller.SendMessage(infon, null);
                while (this.msg == null)
                    Thread.Sleep(1000);

                Microsoft.Research.DkalEngine.Ast.Message message = this.msg;
                Assert.AreEqual(expected, this.msg.message.ToString());
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
        /// Test verifies the Engine instance is not functional anymore by invoking its Close method
        /// </summary>
        [TestMethod]
        public void KillEngineTest()
        {
            string dkalContext = @"..\..\..\DkalUnitTests\dkalfiles\test.dkal";

            if (!System.IO.File.Exists(dkalContext))
                Assert.Fail("File not found: " + dkalContext);

            try
            {
                MessageController msgcntroller = new MessageController(dkalContext);
                Engine e = msgcntroller.EngineInstance;

                Thread.Sleep(2000); // giving some time for the Engine to initalize

                e.Close();
                bool val = (e.communications.Count() == 0 && e.filters.Count() == 0);
                Assert.IsTrue(val, "Engine is still running");
            }
            catch (Exception ex)
            {
                Assert.Fail("Exception Occurred: " + ex.Message);
            }
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
