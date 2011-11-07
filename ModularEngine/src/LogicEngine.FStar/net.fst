module Net
open TypeHeaders
open Types

type Received :: 'a::* => 'a => E
assume forall (x:'a) (l:list 'a). In x l && Received l => Received x

val receive: unit -> b:bytes{Received b}
val send: principal -> bytes -> unit
