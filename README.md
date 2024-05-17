#  Tau

Tau is a functional scripting language written in Rust.

```hs
# Comments start with a tag
exclam text = 
    text ++ "!"
    : String -> String

exclam' =
    \x -> exclam x
    : String -> String
```

Features:
- Typechecking with full type inference
- Haskell-esque syntax and layout

## Credits
- Grammar and parser implemented using [LALRPOP][] (See [grammar.md](src/grammar.lalrpop))
- Typechecking and inference algorithm based on [CubiML][]'s implementation of [Algebraic Subtyping][]

[LALRPOP]: https://github.com/lalrpop/lalrpop/
[CubiML]: https://blog.polybdenum.com/2020/07/04/subtype-inference-by-example-part-1-introducing-cubiml.html
[Algebraic Subtyping]: https://www.cs.tufts.edu/~nr/cs257/archive/stephen-dolan/thesis.pdf

## License
Licensed under either of
- Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution
Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
