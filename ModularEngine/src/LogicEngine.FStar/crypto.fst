module Crypto
open Types

(* read creds from a config file *)
type IsMe :: principal => E
val lookup_my_credentials : unit -> p:principal{IsMe p}

type dsig


