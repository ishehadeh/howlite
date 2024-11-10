#import "@preview/fletcher:0.5.1" as fletcher: diagram, node, edge
#import "colors.typ": colors

#stack(dir: ltr, spacing: .5em,
  diagram({
    node((0, 0), `/`, fill: colors.l1, name: <add1>)
    edge("-|>")
    node((-.8, 1), `+ : 0..30`, fill: colors.l2)
    edge(<add1>, (.8, 1), "-|>")
    node((.8, 1), `3`, fill: colors.l2)
  }, node-outset: 3pt, spacing: 1.5em),
  $-->_1$,
  diagram({
    node((0, 0), `/`, fill: colors.l1, name: <add1>)
    edge("-|>")
    node((-.8, 1), `+ : 0..30`, fill: colors.l2)
    edge(<add1>, (.8, 1), "-|>")
    node((.8, 1), `3 : 3`, fill: colors.l2)
  }, node-outset: 3pt, spacing: 1.5em),
  $-->_2$,
  diagram({
    node((0, 0), `/ : 0..10`, fill: colors.l1, name: <add1>)
    edge("-|>")
    node((-.8, 1), `+ : 0..30`, fill: colors.l2)
    edge(<add1>, (.8, 1), "-|>")
    node((.8, 1), `3 : 3`, fill: colors.l2)
  }, node-outset: 3pt, spacing: 1.5em),
)