# Compiling DKAL 
If you are reading this is because you either synced with our Mercurial repository or you downloaded a snapshot of it. In any case, you will notice that the repository only contains source code (i.e., no binaries or executables) so you will need to compile the DKAL engine yourself. 

First, let's go over the prerequisites. You will need:
* [Visual Studio 2012](http://www.microsoft.com/visualstudio/eng/products/visual-studio-overview) (or later). Alternatively you can probably also compile DKAL with [any other F# compiler](http://research.microsoft.com/en-us/um/cambridge/projects/fsharp/release.aspx), but it has never been attempted and is certainly **not recommended** unless you really know what you're doing.
* NuGet version >= 2.7. NuGet is a part of Visual Studio 2012. Update it if it is older than 2.7: _TOOLS -> Extensions and Updates -> Updates_.

Once you've successfully installed these, you can proceed to compile the DKAL engine. You will need to: 
* open the _"ModularEngine\ModularEngine.sln"_ Visual Studio solution 
* compile it ({{Ctrl+Shift+B}}). 
Please [contact us](http://dkal.codeplex.com/team/view) if you're having problems compiling DKAL.

Once DKAL is successfully compiled, you can proceed to to our documentation on [using the DKAL engine](UsingDKAL).