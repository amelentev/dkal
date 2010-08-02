// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

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
* To successfully compile the solution, please add reference to DkalEngine.dll from DkalEngine directory to DkalHostPart and MessageCaller projects
