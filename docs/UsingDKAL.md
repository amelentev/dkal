# Using DKAL 
Currently DKAL has 3 front ends:
# [Dkal.exe](#usingDkalMain). Use this executable if you intend to deploy a process running a single DKAL principal. This front end uses web services to communicate with other principals which may be running on the same computer or remotely over the network.
# [DkalGui.exe](#usingDkalGuiMain). Use this executable if you intend to deploy a process running a single DKAL principal provided with a GUI (graphical user interface) that enables more advanced interaction. Similarly to the Dkal.exe front end, it uses web services to communicate with other principals (either locally or remotely).
# [DkalMulti.exe](#usingDkalMultiMain). Use this executable if you intend to run several principals in the same process. This front end uses the local memory for communication and is intended for pedagogic uses, as well as policy debugging, testing and analysis.

**Important:** If you installed DKAL using a binary release, then your {{PATH}} environment variable has already been modified so that these binaries are available for you to use. If you compiled DKAL then you will need to modify your {{PATH}} variable manually to point to the location where your binaries are being output by Visual Studio (usuarlly {{ModularEngine\src\GuiMain\bin\Debug}}, {{ModularEngine\src\Main\bin\Debug}}, {{ModularEngine\src\MultiMain\bin\Debug}}).

## {anchor:usingDkalMain} Using Dkal.exe
Dkal.exe should be invoked as follows: 
{{
Dkal.exe <routingTableFile> <policyFile> <stepOption>
}}
Where:
* {{<routingTableFile>}} points to an XML file containing routing information. Refer to the [routing table syntax guide](SyntaxRouting) for more details.
* {{<policyFile>}} points to a DKAL file containing the DKAL policy that the principal will follow. Refer to the [policy language syntax guide](SyntaxPolicy) for more details.
* {{<stepOption>}} must take one of the following two values: {{step}} or {{noStep}}. If the {{step}} option is passed then a key will need to be pressed to make the engine execute each policy step. On the other hand, if the {{noStep}} option is provided the engine executes without need for user interaction.

## {anchor:usingDkalGuiMain} Using DkalGui.exe
DkalGui.exe should be invoked as follows: 
{{
DkalGui.exe <routingTableFile> <policyFile>
}}
Where:
* {{<routingTableFile>}} points to an XML file containing routing information. Refer to the [routing table syntax guide](SyntaxRouting) for more details.
* {{<policyFile>}} points to a DKAL file containing the DKAL policy that the principal will follow. Refer to the [policy language syntax guide](SyntaxPolicy) for more details.

Once the engine is started, the GUI will appear. It features three tabs: "Main", "Policy" and "Log". 
* The "Main" tab features a console, a "Go" button, an "Ask" button and an "Add" button. 
	* The console will display any incoming message from other principals and any action performed by the principal running the engine.
	* The "Go" button acts as a trigger to execute a single policy step. Notice that sometimes **you might need to hit the "Go" button more than once** to see the engine communicating with the outside world. For instance, after the first time you hit "Go" the engine might learn something, and after you hit "Go" a second time the engine will actually use this new knowledge to engage in communication. In principle there is no a-priori bound to the number of times you will need to hit "Go" before the engine reaches a fixed point and no new actions would make it change. When a fixed point is reached the engine will no longer respond to the "Go" button, and it will continue halted until an incoming communication arrives, breaking the fixed point and forcing the execution to resume.
	* The "Ask" button can be used to query the engine. You can use the provided field to write an infon query and hit "Ask". The engine will return all the substitutions that make your query true with respect to the principal's knowledge. For instance, if {{r(X: System.Int32)}} is an infon relation with an integer parameter and the principal knows {{r(24)}}, {{r(25)}} and {{r(26)}}, then the infon query {{with Y: System.Int32 R(Y)}} will yield the substitutions {{[Y -> 24](Y--_-24), [Y -> 25](Y--_-25), [Y -> 26](Y--_-26)}}. Additionally, you may enter the query "ALL", in which case all of the principal's knowledge (infostrate) will be listed on the console.
	* The "Add" button allows you to extend the principal's knowledge with an arbitrary infon that is read from the input field. This infon will form part of the knowledge base of the principal and will be used together with the rest of the knowledge on the following execution round (after the "Go" button is hit).

## {anchor:usingDkalMultiMain} Using DkalMulti.exe
DkalMulti.exe should be invoked as follows: 
{{
DkalMulti.exe <multiPolicyFile> <timeLimit> <messagesLimit>
}}
Where:
* {{<multiPolicyFile>}} points to a single MDKAL file containing a DKAL policy for each of the principals that will be executed. Refer to the [multi policy language syntax guide](SyntaxMultiPolicy) for more details.
* {{<timeLimit>}} is an integer value expressing a time limit for the execution in milliseconds.
* {{<messagesLimit>}} is an integer value expressing a limit to the number of messages that can be exchanged among all of the principals referred in {{<multiPolicyFile>}}.

**Important note:** DkalMulti.exe will quit when any of the following conditions is met:
# The execution has lasted longer than the time limit determined by the command-line parameter.
# The total number of messages exchanged by all the principals has reached the limit determined by the command-line parameter.
# All of the principals are sleeping waiting for new messages to arrive. A situation which we call a _global fixed-point_.
