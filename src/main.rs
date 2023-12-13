mod ast;
mod parser;

use std::fs;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let src = fs::read_to_string("ultra-simple-00.core")?;
    println!("{:?}", parser::module(&src));
    let src = fs::read_to_string("ultra-simple-01.core")?;
    println!("{:?}", parser::module(&src));
    let src = fs::read_to_string("ultra-simple-02.core")?;
    println!("{:?}", parser::module(&src));
    let src = fs::read_to_string("ultra-simple-03.core")?;
    println!("{:?}", parser::module(&src));
    let src = fs::read_to_string("ultra-simple-04.core")?;
    println!("{:?}", parser::module(&src));
    let src = fs::read_to_string("ultra-simple-05.core")?;
    println!("{:?}", parser::module(&src));
    let src = fs::read_to_string("ultra-simple-06.core")?;
    println!("{:?}", parser::module(&src));
    let src = fs::read_to_string("ultra-simple-07.core")?;
    println!("{:?}", parser::module(&src));
    let src = fs::read_to_string("simple-00.core")?;
    println!("{:?}", parser::module(&src));
    let src = fs::read_to_string("simple-01.core")?;
    println!("{:?}", parser::module(&src));
    Ok(())
}
