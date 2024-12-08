#import "../templates/example.typ": code-sample

#let code = ```go
func indexOf(str string, c rune) int {
	runes := []rune(str);

	for i := 0; i < len(runes); i++ {
		if runes[i] == c { return i; }
	}
	
	return -1;
}
```

#figure(code-sample(code),
        caption: "Go",
        kind: "code",
        supplement: "Example")<ex-go>
