# Using justification

Here are some reasons to send justified infons:
* [Forward evidence that you got from someone else](#forwardEvidence)
* [Construct new evidence using your current knowledge](#newEvidence)
* [Sign something you say](#sayEvidence)
* [Sign something you say, but condition it on receiver's knowledge](#conditionSayEvidence)

## {anchor:forwardEvidence} Forward evidence that you got from someone else
Let's assume that you want to write a policy that whenever it gets an incoming infon {{foo(X)}} with justification, it forwards it to mike. You need to write a rule as follows:
{{
upon 
    foo(X) [E](E)
do
    send to mike: 
        foo(X) [E](E)
}}

## {anchor:newEvidence} Construct new evidence using your current knowledge
If you want to send {{bar(11)}} to mike with justification, whenever your infostrate entails {{foo(7)}}, you will be tempted to write this:
{{
if 
    foo(7)
do
    send to mike: 
        bar(11) [E](E)
}}

The problem here is that you are not binding the {{E}} variable, therefore you are sending bogus evidence to mike. You need to explicitly construct evidence for {{bar(11)}} (if possible) from your existing infostrate knowledge by fixing your rule as follows:
{{
if 
    foo(7)
if 
    bar(11) [E](E)
do
    send to mike: 
        bar(11) [E](E)
}}

## {anchor:sayEvidence} Sign something you say
Now let's assume that you know {{foo(7)}} but you have no proof for it. Now you want to send {{foo(7)}} to mike. Since you have no proof for {{foo(7)}}, maybe {{me said foo(7)}} is enough for mike (assuming mike trusts you). 

You can achieve this by signing your message as follows:
{{
if 
    foo(7)
do
    say with justification to mike: 
        foo(7)
}}

## {anchor:conditionSayEvidence} Sign something you say, but condition it on receiver's knowledge
Finally, let's assume you want mike to act as if you said {{foo(7)}} but only if {{bar(11)}} is part of mike's knowledge. You may be tempted to write a rule as follows:
{{
if 
    bar(11)
do
    say with justification to mike: 
        foo(7)
}}

The problem here is that you're checking {{bar(11)}} against **your** infostrate, not mike's. In order to move the checking of {{bar(11)}} from your side to mike's side you write your rule as follows:
{{
do
    send with justification to mike:
        bar(11) -> me said foo(7)
}}



