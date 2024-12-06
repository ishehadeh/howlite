#import "../../templates/example.typ": code-example


#code-example(caption: "Expression",
```
{
  let i: UInt32 = 0;
  while i < str.len {
    if str[i] == c {
        return i;
    };

    i = i + 1;
  };

  -1
}
```)
