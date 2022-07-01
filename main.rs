// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

use derive_builder::Builder;

#[derive(Builder)]
struct Foo {
    a: u32,
    b: u32
}

fn main() {
}
