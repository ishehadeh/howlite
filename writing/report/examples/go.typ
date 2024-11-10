#import "../templates/example.typ": code-sample

#let code = `func indexOf(str string, c rune) int {
	codepoints := []rune(str);

	for i := 0; i < len(codepoints); i++ {
		if codepoints[i] == c { return i; }
	}
	
	return -1;
}`

#figure(code-sample(code),
        caption: "Go",
        kind: "code",
        supplement: "Example")<ex-go>
