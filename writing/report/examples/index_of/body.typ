#import "../../templates/example.typ": code-example


#code-example(caption: "Expression",
```
{
  let mut i: u32 = 0;
  while i < str.len {
    if str[i] == chr {
      return i;
    }

    i = i + 1;
  }

  -1
}
```)
