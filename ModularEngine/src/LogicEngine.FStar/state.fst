module State
open TypeHeaders
open Types

type Knows :: polyterm => E
type infostrate = list (i:polyterm{Knows i}) 

private val infos : ref infostrate
let infos = ref []

val addToInfostrate : i:polyterm{Valid i} -> unit
let addToInfostrate i = infos := i::!infos

val getInfostrate : unit -> infostrate
let getInfostrate x = !infos

val getSubstrate: unit -> substrate
