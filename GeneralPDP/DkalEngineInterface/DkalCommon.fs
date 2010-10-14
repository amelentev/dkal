namespace Microsoft.Research.GeneralPDP.DKAL.Engine

module DkalCommon =

  // edit the following line so that it points to a SQL server on which you have permissions to run queries
  // replace MACHINE, PORT, DATABASE, USER, PASSWORD
  let privateSql = "#set private_sql \"Server=tcp:MACHINE,PORT;Database=DATABASE;User ID=USER;Password=PASSWORD;TrustServerCertificate=true;Trusted_Connection=False;Encrypt=True;\""

  let identity (name: string) = "if " + name + " knows\n  asInfon(0==1)\nthen\n  to " + name + "\n    asInfon(true)"

  let xacmlAwareDkalAssertions = @"
// responses
attribute [16] req {int} by {principal} decision {text}

// requests: issued by (presence)
attribute [16] req {int} issued by {principal}

// requests: attributes
attribute [16] req {int} by {principal} has {text} attribute string {text} valued {text}
attribute [16] req {int} by {principal} has {text} attribute int {text} valued {int}

"

  let basicTrustDkalAssertions dkal pep = 
    dkal + " knows\n" + 

      // trust the PEP on issuing requests
      "  " + pep + " tdonS req R2 issued by " + pep + "\n" +

      // trust the PEP on attributes from his/her requests
      "  " + pep + " tdonS req R3 by " + pep + " has CATEGORY3 attribute string NAME3 valued VALUE3\n" +
      "  " + pep + " tdonS req R4 by " + pep + " has CATEGORY4 attribute int NAME4 valued VALUE4\n" +
      ""

  let singleCommRuleDkalAssertions dkal pap = 
    dkal + " knows\n" + 
      // trust the PAP
      "  " + pap + " tdonI req R1 by PEP1 decision D1\n" +

      // send responses back to PEP
      "if " + dkal + " knows\n" +
      "  " + "req R issued by PEP\n" +
      "  " + "req R by PEP decision D\n" +
      "then\n" +
      "  " + "say with justification to PEP\n" +
      "    " + "req R by PEP decision D\n" +
      "" 
