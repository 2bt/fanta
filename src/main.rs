
mod lexer;
use lexer::*;

fn main() {
    let mut lexer = Lexer::new(
        "
a
b# comment
c
\"Hello, world!\"
a1= (r + 123) == 3
",
    );

    loop {
        let t = lexer.next_token();

        println!("{:?}", t);
        match t.kind {
            TokenKind::Ident => {
                println!("name = {}", lexer.ident_name(&t));
            }
            TokenKind::Number => {
                println!("number = {}", lexer.number_value(&t));
            }
            TokenKind::String => {
                println!("string = {:?}", lexer.string_value(&t));
            }
            _ => {}
        }

        if t.kind == TokenKind::EOF {
            break;
        }
    }
}
