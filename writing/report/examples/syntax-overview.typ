#import "../templates/example.typ": code-sample

#let code = ```
type u32 = 0..0xffffffff;

func fib(n: u32): u32 {
    if n == 0 { 0 }
    else if n == 1 { 1 }
    else {
      fib(n - 1) + fib(n - 2)
    }
}
```


#figure(code-sample(code),
        caption: "Recursive Fibonacci",
        kind: "code",
        supplement: [Example])<ex-syntax-overview>
