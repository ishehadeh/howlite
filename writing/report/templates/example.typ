#let slugify(s) = lower(s.replace(regex("[^a-z0-9A-Z]+"), "-").trim("-"))
#let code-sample(raw-code) = {
  rect(align(left, pad(x: 6pt, y: 4pt, raw-code)))
}

#let code-example(content, caption: "") = [
  #figure(caption: caption,
          kind: "code",
          supplement: "Example",
          code-sample(content))
  #label("ex-" + slugify(caption))
]