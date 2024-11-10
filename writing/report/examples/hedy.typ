#import "../templates/example.typ": code-sample, code-example

#let en = ```
print Hello!
ask What is your name?
```

#let ar = align(right, text(dir: rtl, ```
قول Hello!
اسأل What is your name?
```))

#grid(columns: 1, rows: 2, gutter: 2em,
    code-example(en, caption: "Hedy (English)"),
    code-example(ar, caption: "Hedy (Arabic)"))
