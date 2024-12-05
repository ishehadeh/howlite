#import "../templates/example.typ": code-sample

#let code = ```
let a : 0..10 = 3;
while a < 5 { a = a + 1; }
let b: 0..5 = a; // type error - a is treated as a 0..10
```


#figure(code-sample(code),
        caption: "While Loop Narrowing",
        kind: "code",
        supplement: [Example])<ex-while-loop-narrowing>
