#light "off"

module Microsoft.FStar.Crypto
open System
open System.Security.Cryptography

let sha1_instance = SHA1.Create ()
let sha1 (x:byte []) : byte [] = sha1_instance.ComputeHash x

let rsa_new_key_pair (b:bool) : Prims.DepTuple2SS<string, string>  = 
  failwith "Not yet implemented" 

let rsa_sign_impl (v:byte []) (skey:string) = 
  let rsa = new RSACryptoServiceProvider() in
  let _ = rsa.FromXmlString(skey) in
  let oid = CryptoConfig.MapNameToOID "sha1" in
    rsa.SignHash(sha1 v, oid)

let rsa_verify_impl (v:byte []) (x:byte []) (pkey:string) =
  let rsa = new RSACryptoServiceProvider() in
  let _ = rsa.FromXmlString(pkey) in
  let oid = CryptoConfig.MapNameToOID "sha1" in
    rsa.VerifyHash(sha1 v, oid, x)        
