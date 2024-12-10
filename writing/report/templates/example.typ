#let slugify(s) = lower(s.replace(regex("[^a-z0-9A-Z]+"), "-").trim("-"))
#let code-sample(raw-code, outline: true) = {
  rect(align(left, pad(x: 6pt, y: 4pt, raw-code)),
    stroke:  if outline == true { black } 
        else if outline == false { none }
        else { outline })
}

#let code-example(content, caption: "", outline: true) = [
  #figure(caption: caption,
          kind: "code",
          supplement: "Example",
          code-sample(content, outline: outline))
  #label("ex-" + slugify(caption))
]
