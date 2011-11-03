module State
open TypeHeaders
open Types

private val infos : ref infostrate
let infos = ref []

val addToInfostrate : i:polyterm{Knows i} -> unit
let addToInfostrate i = infos := i::!infos

val getInfostrate : unit -> infostrate
let getInfostrate x = !infos

val getSubstrate: unit -> substrate

