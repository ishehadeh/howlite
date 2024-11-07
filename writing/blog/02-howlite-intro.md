# Programming with Precise Integers

Howlite closely tracks the value of integers.
You can declare a type that may only have the value `1`, and another type that only allows the value `0`.
Using this scheme, we can define a boolean (note `|` is type union):

```
type Boolean = 0 | 1

let true: Boolean = 0;
let false: Boolean = 1;
```

That's nice, but booleans aren't a particular interesting language feature.
How do these actually help us write programs?

Take a look at this implementation of `index_of`, which looks for an a character in an ASCII string.
```
type char = 0..127;
type i32 = -2_147_483_648..2_147_483_647;
type PositiveI32 = 0..2_147_483_647;

func index_of[LenT: PositiveI32](str: &[char; LenT], chr: char): 0..LenT | -1 {
  let i: u32 = 0;
  while i < str.len {
    if str[i] == chr {
      return i;
    }

    i += 1;
  }

  -1
}
```

There's quite a bit here, let's unpack it line by line. First,
the type definitions

- `type char = 0..127;` - represents a 7-bit ascii digit, notice we don't have to round up to 8 bits here.
- `type i32 = -2_147_483_648..2_147_483_647;` - This is just a normal 32-bit integer.
- `type PositiveI32 = 0..2_147_483_647;` - We need a number that's a 32-bit signed integer under the hood, but it can't be negative, we'll use negative numbers to signal no value.


This function is generic, the `[LenT: PositiveI32]` section says "for any subset of the positive, signed, 32-bit integers, there is an instance of `index_of`".
Whatever that type is, we'll call it `LenT`.

Moving on to the parameter list, notice the type of `str`: `&[char; LenT]`.
This `&[...]` is a special type called a _slice_ (also know as a fat pointer).
Really, under the hood it's just a pointer and length pair, practically it functions like an array.
These types are common, they're primitives in Rust, Go, and Zig.
Although it's not a primitive type, the C++ STL's `std::string_view` is a similar data structure.
What sets our slice's apart is that the type of the length can be set.
So, for example say we take a slice of some ASCII string, from index 3 to 10, the result would have the type `&[char; 7]`.

By using a generic parameter, `LenT`, then giving `str` the type `&[char; LenT]`.
We're saying "this function works on a string of any lenght less than or equal to 2,147,483,647",
Since it's impossible to find a character outside of those bounds, we know the return type can't exceed the maximum value of `LenT`.

Finally, let's look at the core of this function.
```
let i: u32 = 0;
while i < str.len {
  if str[i] == chr {
    return i;
  }

  i = i + 1;
}

-1
```

Most of this is fairly typicall, but remeber, we're only allowed to return values that are less than or equal to the max value of `LenT`, or `-1`.
How do we know that `i` is always a subset of the return type, since `u32` certainly exceeds `LenT`.
The condition `while i < str.len`, narrows `i`'s type from `u32` to `0..LenT-1`.
Within the body of that loop, `i` can be used as if it had the type `0..LenT-1`.
Arithmetic changes this, of course, after running `i += 1`, `i`'s type is now `1..LenT`.
If we changed the code to check some other condition, for example `chr < str.len`, this wouldn't compile.

There's a lot more you can do with this type system - are those things useful?
Who knows, but it's certainly an interesting exercise.
Stay tuned for Storage Classes, arithmetic on sparse sets, and constraint solving.
