# DKAL Challenges
This page will probably be always in construction. You're free to pick any (or all) of the challenges listed below and work with us in solving it. If you're interested, please [contact our team](http://dkal.codeplex.com/team/view).

## DKAL policy sanity checker
The idea is to construct a module to perform basic checks on a policy (or a set of policies for many agents) and discover errors that may have been introduced due to common misconceptions or simply typos.

* For instance, the policy for principal Alice may try to match against a message that is never sent to her by any of the other principals. Detecting this situation is relatively easy to do, and understanding this type of mistake may lead to discovering more interesting errors in the policy.
* Another example is when we try to match against an infon relation {{r(X)}} on the wire, when in fact we need to match who said it, that is {{P said r(X)}}. 

Most of the basic things to check can be solved syntactically. Maybe some other, more elaborate ones require some form of static analysis framework.

## Merging the concept of infostrate and substrate
If you take a look at the interfaces for infostrate (where a principal's knowledge is stored) and substrate (where data is stored) they are really similar. An interesting possibility that emerges is implementing the infostrate as a particular type of substrate. 

Following this idea, the infon logic engine would then become a substrate that not only contains data, but also performs some derivations from the data it contains (namely, the derivations indicated by the primal/full infon logic calculus). 

## How do messages get removed from mailbox?
The idea in the current engine is that the mailbox keeps the messages that are checked using {{upon}} conditions. Currently, these messages are stored until an explicit {{drop}} action is invoked on them. 

We envision that in many scenarios it would be natural to discard (or archive) old/irrelevant messages. Implementing such a mechanism would require designing criteria for removal based on facts like time of arrival, how often has it been used, etc.
