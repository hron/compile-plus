mod add;
mod sub;

use crate::add::add;

fn main() {
    let x = add(2, 2);
    println!("Hello, world! {}", &x);
}
