The sample provided here is a POC on Managed Extensibility Framework released in .Net 4.0
under System.ComponetModel.Composition namespace

Sample is divided as below:

Contract
=========
DkalLib

Parts
======
1. DkalHostPart
2. SamplePart

CompositionContainer
====================
TestConsole



Description:

* MEF allows parts to compose dynamically in a composition container using Lazy Loading.
* Parts following the contract are composed dynamically by the container.
* MEF support attribute driven programming.
* Users can filter and reach specific parts using attributes.
* In the present solution parts following the contract can be dropped in a custom Plugins directory. 
  The MessageFactory from ImplParts Proj will compose them dynamically.
* Please also follow the presentation MEF for more clarity.
