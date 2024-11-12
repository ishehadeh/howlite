#import "@preview/fletcher:0.5.2" as fletcher: diagram, node, edge
#import "../templates/colors.typ": nth-color, colors

#let edge_colors = (rgb("#dc8a78"), rgb("#ea76cb"), rgb("#ea76cb"))

#{
  let t = (1,2,3,)
  let u = (-5, -7,)
  let x = t.map((a) => u.map((b) => a + b)).flatten()
  let width = (t, u, x,).map(arr => arr.len()).reduce((a, b) => if a > b { a } else { b })
  let u_pos(i, y: 1) = ((width - u.len()) / 2 + i, y)
  let t_pos(i, y: 0) = ((width - t.len()) / 2 + i, y)
  let x_pos(i, y: 2) = ((width - x.len()) / 2 + i, y)
  diagram({
    t
      .enumerate()
      .map(((i, v)) => {
        node(t_pos(i), raw(str(v)), fill: colors.flamingo.transparentize(60%),  stroke: 0.5pt + black, width: 20pt)
      })

    u
      .enumerate()
      .map(((i, v)) => {
        node(u_pos(i), raw(str(v)), fill: colors.teal.transparentize(80%),  stroke: 0.5pt + black, width: 20pt)
      })

    range(t.len())
      .map(i => {
        range(u.len())
          .map(j => {
            let x-index = (i * u.len() + j);
            let color-index = 2 * x-index;
            node(x_pos(x-index), raw(str(x.at(x-index))),width: 20pt, fill: white, stroke: 1pt + nth-color(color-index))
            edge(t_pos(i), u_pos(j),
              marks: (none, (inherit: "solid", size: 5 )),
              stroke: nth-color(color-index) + 1.75pt,
              crossing: true, crossing-thickness: 2)
            edge(u_pos(j), x_pos(i * u.len() + j),
              marks: (none, (inherit: "solid", size: 5 )),
              stroke: nth-color(color-index) + 1.75pt,
              crossing: true, crossing-thickness: 2)
          })
      })


  },
   node-shape: circle, node-outset: 2pt )
}