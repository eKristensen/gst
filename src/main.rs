mod ast;
mod parser;

use std::fs;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let src = fs::read_to_string("ultra-simple.core")?;
    println!("{:?}", parser::module(&src));
    
    println!("Hello, world!");
    Ok(())
}
