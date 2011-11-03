module Net
open TypeHeaders
open Types

val receive: unit -> bytes
val send: principal -> bytes -> unit
