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
        /// Validate Substrate Connection String
        /// </summary>
        [TestMethod]
        public void SubstrateConnectionStringTest()
        {
            string conString = ConfigurationManager.AppSettings["SubstrateConnectionString"];

            if (string.IsNullOrEmpty(conString))
                throw new Exception("SubstrateConnectionString is null");
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
            }
            finally
            {
                con.Close();
            }
            if (!isConnected)
                Assert.Fail("Sql Connection Failed");

        }

        /// <summary>
        /// Verify Engine object 
        /// </summary>
        [TestMethod]
        public void CreateEngineTest()
        {
            //
            // TODO: Add test logic here
            //
        }

        /// <summary>
        /// Verify AddInfon call to an Engine. This call should result in a Message object to a callback SendMessage() implemented from ICommunicator interface 
        /// </summary>
        [TestMethod]
        public void SendMessageTest()
        {
            string dkalContext = @"..\..\..\DkalUnitTests\dkalfiles\test.dkal";
            MessageController msgcntroller = new MessageController(dkalContext);
            msgcntroller.OnInfonProcessed += new MessageController.MyEventHandler(msgcntroller_OnInfonProcessed);

            ParsingCtx pctx = msgcntroller.ParsingContext;
            string infon = "42 is a good number";
            string expected = "i-like (42)";

            msgcntroller.SendMessage(infon, null);
            while(this.msg==null)
                Thread.Sleep(1000);

            Microsoft.Research.DkalEngine.Ast.Message message= this.msg;
            Assert.AreEqual(expected, this.msg.message.ToString());
        }

        void msgcntroller_OnInfonProcessed(object sender, DkalInfoEventArgs e)
        {
            if (e.DkalMessage == null)
                throw new ArgumentNullException("DkalMessage");
            msg = e.DkalMessage;
        }

        /// <summary>
        /// Create 2 Engines and verify the message flow as a complete cycle
        /// </summary>
        [TestMethod]
        public void EnginesCycleTest()
        {
            //
            // TODO: Add test logic here
            //
        }

        /// <summary>
        /// Verify the Engine instance is not functional anymore by invoking its Close()
        /// </summary>
        [TestMethod]
        public void KillEngineTest()
        {
            //
            // TODO: Add test logic here
            //
        }

        [TestCleanup]
        public void CleanUp()
        {
            this.msg = null;
        }
    }
}
