
#import "../templates/example.typ": *

#code-example(caption: "Howlite",
```
func to_num(c: char): -1 | 0..9 {
  if ('0' <= c && c <= '9') {
    c - '0'
  } else { -1 }
}
```)