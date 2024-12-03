#import "../../templates/example.typ": code-example


#code-example(caption: "Range Types",
```
type char = 0..127;
type Uint32 = 0..0xffffffff;
type NatI32 = s32[0..0x7fffffff];
```)

