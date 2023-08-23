
mod lexer;
use lexer::*;

fn main() {
    let mut lexer = Lexer::new(
        "

=
      + _x + r 123
      *
",
    );

    loop {
        let t = lexer.next_token();

        println!("{:?}", t);
        match t.kind {
            TokenKind::Ident => {
                println!("name = {:?}", lexer.ident_name(&t));
            }
            TokenKind::Number => {
                println!("number = {:?}", lexer.number_value(&t));
            }
            _ => {}
        }

        if t.kind == TokenKind::EOF {
            break;
        }
    }
}
