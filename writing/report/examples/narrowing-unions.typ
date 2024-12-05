
#import "../templates/example.typ": *

#code-example(caption: "Type Narrowing and Unions",
```
type FsError = { kind: 0, filename: Str, errno: Int32 };
type ParseError = { kind: 1, filename: Str, token: Str, line: UInt32 };
type LexError = { kind: 2, filename: Str, char: Str, offset: UInt32 };
type Error = FsError | ParseError | LexError;

func print_error(err: Error): unit {
    print(&"Error in ");
    print(err.filename);
    if err.kind == 1 {
        print(&" parse error at line ");
        print(int2str(err.line));
    };
    print(&"\n");
}
```)