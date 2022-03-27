# DKAL Engine General Information

Before proceeding and starting to make changes to the DKAL engine code, there are a couple of things you need to know.

## {anchor:implementationLanguage} Implementation Language
DKAL is primarily implemented in [F#](http://www.fsharp.net/), a succinct, expressive and efficient functional and object-oriented language for .NET. If you're not familiar with F# we suggest you do some [preliminary reading and learning](http://msdn.microsoft.com/en-us/fsharp/cc835246) before you start with DKAL.

F# is fully supported in Visual Studio starting from the 2010 version. F# can also be compiled outside of Visual Studio using a [previously released standalone compiler](http://www.microsoft.com/downloads/en/details.aspx?FamilyID=effc5bc4-c3df-4172-ad1c-bc62935861c5&displaylang=en) which we haven't tried (and probably never will).

## {anchor:prerequisites} Prerequisites
There are two prerequisites before you proceed with the developer documentation.
# Read the [DKAL user documentation](UserDocumentation). It is mainly targeted at people that want to use DKAL as a policy language (as opposed to people who want to develop DKAL further), but it contains a lot of information on how DKAL source code is obtained and compiled, how the concrete syntax is and how to run the examples, among other things. It will therefore be **assumed as read** in the rest of the developer documentation.
# In particular, pay particular attention to the user documentation section on [compiling DKAL](CompilingDKAL). You will find all the necessary information on how to set up your environment in order to compile DKAL.

## {anchor:backgroundTheory} Background Theory
It shouldn't be strictly necessary for you to learn some theory about DKAL before proceeding, but it wouldn't hurt either! We suggest you visit our section on [suggested reading](SuggestedReading).
