#import "../templates/example.typ": code-sample

#let en = `print Hello!
ask What is your name?
`

#let ar = `
قول Hello!
اسأل What is your name?
`

#figure(caption: "Hedy", kind: "code", supplement: "Example")[
  #grid(columns: 2, rows: 1, column-gutter: 5em,
    code-sample(en),
    code-sample(ar))
]<ex-hedy>
