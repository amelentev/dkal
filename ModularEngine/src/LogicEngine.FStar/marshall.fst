module Marshall
open TypeHeaders
open Types

type message = {msg_from:principal;
                msg_to: principal;
                msg_payload: polyterm}
type msg = 
  | Justified : message -> msg
  | Forwarded : message -> msg

val polyterm2bytes : polyterm -> bytes
val bytes2polyterm : bytes -> polyterm
val parse: bytes -> list polyterm
val concat : list bytes -> bytes
val unconcat: bytes -> list bytes

val msg2bytes: msg -> bytes
