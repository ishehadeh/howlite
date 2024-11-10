#import "@preview/fletcher:0.5.1" as fletcher: diagram, node, edge
#import "colors.typ": colors

#stack(dir: ltr, spacing: 1em,
  diagram({
    node((0, 0), `+`, fill: colors.l3, name: <add2>)
    edge("-|>")
    node((-.8, 1), `x : 0..10`, fill: colors.l4)
    edge(<add2>, (.8, 1), "-|>")
    node((.8, 1), `y : 0..10`, fill: colors.l4)
  }, node-outset: 3pt, spacing: 1.5em),
  $-->$,
  diagram({
    node((0, 0), `+ : 0..20 `, fill: colors.l3, name: <add2>)
    edge("-|>")
    node((-.66, 1), `x : 0..10`, fill: colors.l4)
    edge(<add2>, (.66, 1), "-|>")
    node((.66, 1), `y : 0..10`, fill: colors.l4)
  }, node-outset: 3pt, spacing: 1.5em)
)