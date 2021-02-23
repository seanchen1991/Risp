use std::io;

use risp::{
    
};

const PROMPT: &str = "risp >";

fn main() {
    let env = Env::default();

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut expr = String::new();
    
        io::stdin().read_line(&mut expr)
            .expect("> Error: Failed to read line");

        if expr.trim() == ":q" | ":quit" {
            break;
        }

        match parse_eval(expr, env) {
            Ok(res) => println!("> {}", res),
            Err(e) => match e {
                RispErr::Reason(msg) => println!("> Error: {}", msg),
            }
        }
    }
}
