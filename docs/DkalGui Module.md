# {anchor:top} {{DkalGui}} Module
----
This module defines the following types:
* [CommunicationWindow](DkalGui-Module#CommunicationWindow)
* [LogSink](DkalGui-Module#LogSink)
----
## {anchor:CommunicationWindow} {{CommunicationWindow}} Type
**Base type**: Form
The Form used as GUI

### Implemented Interfaces
* IComponent
* IDisposable
* IOleControl
* IOleObject
* IOleInPlaceObject
* IOleInPlaceActiveObject
* IOleWindow
* IViewObject
* IViewObject2
* IPersist
* IPersistStreamInit
* IPersistPropertyBag
* IPersistStorage
* IQuickActivate
* ISupportOleDropSource
* IDropTarget
* ISynchronizeInvoke
* IWin32Window
* IArrangedElement
* IBindableComponent
* IContainerControl

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{SetPosition}} | int, int, int, int | Set the position of the window to the given values |
|  | unit | {{Say}} | string | Write to the main textbox, replacing markup by actual formatting |
>{[Back to top](#top)}>
----
## {anchor:LogSink} {{LogSink}} Type
**Base type**: StringWriter
Used to save the output of the internal processes (DKAL engine) on a       textbox

### Implemented Interfaces
* IDisposable

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{Update}} |  | Print out on the textbox, keeping only SizeLimit bytes on screen and       deleting the old ones |
|  | unit | {{Write}} | Char | Write to the log and update the screen |
|  | unit | {{Write}} | Char[](), int, int | Write to the log and update the screen |
|  | unit | {{Write}} | string | Write to the log and update the screen |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
