module Marshall
open TypeHeaders
open Types

type message :: _ =  fun ('P::principal => principal => polyterm => E) => 
    ((pfrom:principal *
        pto:principal *
          i:polyterm{'P pfrom pto i}))

type SaysTo :: principal => principal => polyterm => E
type trivial :: _ = (fun (p:principal) (q:principal) (i:polyterm) => True)

type msg = 
  | Forwarded : message trivial -> msg
  | Justified : message SaysTo -> msg


val polyterm2bytes : polyterm -> bytes
val bytes2polyterm : bytes -> polyterm
val parse: b:bytes -> p:polyterm{Net.Received b => Net.Received p}
val concat : list bytes -> bytes
val unconcat: bytes -> list bytes

val msg2bytes: msg -> bytes
