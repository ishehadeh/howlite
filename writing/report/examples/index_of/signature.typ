#import "../../templates/example.typ": code-example


#code-example(caption: "Function Signature",
```
func index_of[LenT: NatI32](str: &[char; LenT], c: char): 0..Max[LenT] | -1
```)

