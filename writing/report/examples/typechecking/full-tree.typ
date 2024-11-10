#import "@preview/fletcher:0.5.1" as fletcher: diagram, node, edge
#import  "./colors.typ": colors

#figure(diagram({
  node((0, 0), `/`, fill: colors.l1, name: <div>)
  edge("-|>")
  node((-.6, 1), `+`, fill: colors.l2, name: <add1>)
  edge("-|>")
  node((-1.2, 2), `+`, fill: colors.l3, name: <add2>)
  edge("-|>")
  node((-1.8, 3), `x`, fill: colors.l4)
  edge(<add2>, (-.6, 3), "-|>")
  node((-.6, 3), `y`, fill: colors.l4)

  edge(<add1>, (0, 2), "-|>")
  node((0, 2), `z`, fill: colors.l3)


  edge(<div>, (.6, 1), "-|>")
  node((.6, 1), `3`, fill: colors.l2)
}, node-outset: 3pt, spacing: 1.5em), caption: "AST")<ast>