# Obtaining DKAL 
## {anchor:beforeYouStart} Before You Start

Before downloading DKAL please take a second and review the DKAL engine License. DKAL is distributed under the Apache License Version 2.0. For more information about the DKAL license, [visit our License page](http://dkal.codeplex.com/license).

There are a few system requirements that need to be met before installing DKAL on your computer. You need to download and install the following components:
* [Microsoft .NET Framework 4](http://www.microsoft.com/download/en/details.aspx?id=17851)

## {anchor:waysToGetDkal} Three Ways to Get DKAL

There are three ways to obtain DKAL:
# [Downloading a DKAL binary release](#obtainingDkalBinaryRelease). **Recommended** if you only want to write and execute DKAL policies, without modifying the engine.
# [Syncing with the DKAL Mercurial repository](#obtainingDkalSyncingMercurial). **Recommended** if you plan to extend or modify the engine.
# [Downloading a snapshot of the DKAL Mercurial repository](#obtainingDkalSnapshotMercurial). **Not recommended** unless the other two options fail.

Each option is explained in detail in the following sections.

### {anchor:obtainingDkalBinaryRelease} Option 1: Downloading a DKAL Binary Release 
Visit our [Downloads page](http://dkal.codeplex.com/releases) and obtain the latest recommended release. Follow the instructions in the self-installable {{Setup.exe}} file.

Once DKAL is successfully installed, please proceed to our documentation on [using the DKAL engine](UsingDKAL).

### {anchor:obtainingDkalSyncingMercurial} Option 2: Syncing with the DKAL Mercurial Repository 
First of all, before you proceed you will need to install the [Mercurial SCM](http://mercurial.selenic.com/) and familiarize yourself with it. There are excellent Mercurial front-ends for most of the major platforms. In Windows we recommend you use [TortoiseHg](http://tortoisehg.bitbucket.org/) and we suggest you also consider using [VisualHG](http://visualhg.codeplex.com/) for Visual Studio integration.

You will also need to open a Codeplex account, if you haven't already done so. In order to open a Codeplex account please [proceed to our the Codeplex register page](http://www.codeplex.com/site/register/new) and follow the instructions there.

Once you have a Codeplex account and your Mercurial client is installed, you can [proceed to our Source Code page](http://dkal.codeplex.com/SourceControl/list/changesets) and click the _Connection instructions_ link (or the _Mercurial_ link, if the other one does not appear). You will be given a specific Mercurial URL that you can use with your client to point to our repository and clone it. When prompted by your Mercurial client, provide your Codeplex user and password.

Once you have synced with the DKAL Mercurial repository, please proceed to our documentation on [compiling the DKAL engine](CompilingDKAL).

**Important:** If you plan to share your changes with the rest of the DKAL developers in [our team](http://dkal.codeplex.com/team/view), please contact any of the coordinators and request to be added as a developer in the DKAL CodePlex project.

### {anchor:obtainingDkalSnapshotMercurial} Option 3: Downloading a Snapshot of the DKAL Mercurial Repository 
This option is **not recommended** unless the other two options didn't work for you. 

* If you plan to use DKAL as an end user, please consider [downloading a DKAL binary release](#obtainingDkalBinaryRelease). This will be **much easier** for you, since you won't need to have Visual Studio installed and you won't have to compile our source code.
* If you plan to develop DKAL, modify any of its modules, or extend its functionality, please consider [syncing with the DKAL Mercurial repository](#obtainingDkalSyncingMercurial). This will allow you to **share your extensions, improvements and bug fixes** with all the other DKAL users.

If you reached this far is because you are determined to obtain a snapshot of the DKAL Mercurial repository. If that is the case, [proceed to our Source Code page](http://dkal.codeplex.com/SourceControl/list/changesets) and click the _Download_ link. You will be asked to read and accept our [license](http://dkal.codeplex.com/license).

Once you have downloaded the snapshot, please proceed to our documentation on [compiling the DKAL engine](CompilingDKAL).