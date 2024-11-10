#let wrapped-figure(left: none, right: none, under: none, marginleft: 0em, margintop: 0.5em) = {
  set par(justify: true)
  grid(columns: 2, column-gutter: 1em, left, right)
  set par(justify: false)
  block(inset: (left: marginleft, top: -margintop), under)
}
// source: https://github.com/typst/typst/discussions/1069#discussioncomment-8040136