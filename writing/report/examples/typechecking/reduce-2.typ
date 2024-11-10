#import "@preview/fletcher:0.5.1" as fletcher: diagram, node, edge
#import "colors.typ": colors

#stack(dir: ltr, spacing: .5em,
  diagram({
    node((0, 0), `+`, fill: colors.l2, name: <add1>)
    edge("-|>")
    node((-.8, 1), `+ : 0..20`, fill: colors.l3)
    edge(<add1>, (.8, 1), "-|>")
    node((.8, 1), `z`, fill: colors.l3)
  }, node-outset: 3pt, spacing: 1.5em),
  $-->_1$,
  diagram({
    node((0, 0), `+`, fill: colors.l2, name: <add1>)
    edge("-|>")
    node((-.8, 1), `+ : 0..20`, fill: colors.l3)
    edge(<add1>, (.8, 1), "-|>")
    node((.8, 1), `z : 0..10`, fill: colors.l3)
  }, node-outset: 3pt, spacing: 1.5em),
  $-->_2$,
  diagram({
    node((0, 0), `+ : 0..30`, fill: colors.l2, name: <add1>)
    edge("-|>")
    node((-.6, 1), `+ : 0..20`, fill: colors.l3)
    edge(<add1>, (.8, 1), "-|>")
    node((.6, 1), `z : 0..10`, fill: colors.l3)
  }, node-outset: 3pt, spacing: 1.5em),
)