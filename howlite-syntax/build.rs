use cfgrammar::yacc::YaccKind;
use lrlex::CTLexerBuilder;

fn main() -> Result<(), ()> {
    CTLexerBuilder::new()
        .lrpar_config(|ctp| {
            ctp.yacckind(YaccKind::Grmtools)
                .grammar_in_src_dir("howlite.y")
                .unwrap()
        })
        .lexer_in_src_dir("howlite.l")
        .unwrap()
        .build()
        .unwrap();
    Ok(())
}
