# Multi policy Language Syntax Guide 
Multi policy files used with [MultiMain.exe](#usingDkalMultiMain) contain several parts, separated by a dashed separator. Any line satisfying the following conditions is considered a dashed separator:
# It must start with at least 3 dashes {{---}}.
# It must continue with a the name of a principal.
# It must end with at least 3 dashes {{---}}.

For instance, the following are all valid separators:
* {{-----david---}}
* {{---carol--------}}
* {{---sue---}}

The policy fragment between the beginning of the file and the first separator is called the **common policy** section. This common policy is defined using the [DKAL policy language](#syntaxPolicy) and is prepended to the policy of each of the principals defined in the rest of the multi policy file.

Each subsequent policy fragment defines the individual policy for the principal named in the preceding separator.

Also you can optionally set [logic engine](Logic-Engines) kind in first line of multi policy file in format "/// logic: <kind>". Default logic kind is "simple".

For instance:
{{
/// logic: simple
// common policy
// ...

-----david---
// david's policy
// ...

---carol--------
// carols's policy
// ...

---sue---
// sue's policy
// ...
}}
