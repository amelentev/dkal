module State
open Types

private val infos : Ref infostrate
let infos = newref []

val addToInfostrate : i:infon{Knows i} -> unit
let addToInfostrate i = infos := i::!infos

val getInfostrate : unit -> infostrate
let getInfostrate x = !infos

val getSubstrate: unit -> substrate
let getSubstrate x = ()

