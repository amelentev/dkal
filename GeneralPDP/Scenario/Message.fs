namespace Microsoft.Research.GeneralPDP.Scenario

open Basics

module Message =

  type Message = 
    { sender: EndPointId; 
      receiver: EndPointId;
      content: Content }

    interface IMessage with
      member m.Sender = m.sender
      member m.Receiver = m.receiver
      member m.Content = m.content

    override m.ToString() = 
      m.sender + " to " + m.receiver + ":\r\n" + m.content.ToString()