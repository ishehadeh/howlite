#let colors = (
  l1: red.lighten(50%),
  l2: green.lighten(75%),
  l3: blue.lighten(75%),
  l4: yellow.lighten(75%)
)

#let noderef(label, color) = {
  if label.text.len() == 1 { 
    box(height: 16pt, baseline: 4pt, outset: -0.5pt, width: 16pt, align(center + horizon, circle(fill: color, label)))
  } else {
    box(height: 16pt, fill:color, baseline: 3.5pt, outset: -0.5pt, inset: 5pt, align(center + horizon, label))
  }
}

#let noderef-1(label) = noderef(label, colors.l1)
#let noderef-2(label) = noderef(label, colors.l2)
#let noderef-3(label) = noderef(label, colors.l3)
#let noderef-4(label) = noderef(label, colors.l4)