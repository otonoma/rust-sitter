use rust_sitter::Language;
use std::{fmt::Debug, io::Write};

use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use rust_sitter::error::ParseError;

mod arithmetic;
mod optionals;
mod repetitions;
mod words;

fn convert_parse_error_to_diagnostics(file_span: &codemap::Span, error: &ParseError) -> Diagnostic {
    let mut message = format!("syntax error. reason: {:?}", error.reason);
    if !error.lookaheads.is_empty() {
        message += &format!(
            "\nPossible expected inputs: {}",
            error.lookaheads.join(" | ")
        );
    }

    Diagnostic {
        level: Level::Error,
        spans: vec![SpanLabel {
            span: file_span.subspan(
                error.error_position.bytes.start as u64,
                error.error_position.bytes.end as u64,
            ),
            style: SpanStyle::Primary,
            label: None, // TODO
        }],
        code: None,
        message,
    }
}

fn main() {
    env_logger::init();
    let args: Vec<_> = std::env::args().collect();
    let grammar = if args.len() == 1 {
        "Expression"
    } else if args.len() == 2 {
        &args[1]
    } else {
        panic!("Unexpected inputs")
    };
    let stdin = std::io::stdin();

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        stdin.read_line(&mut input).unwrap();
        let input = input.trim();
        if input.is_empty() {
            break;
        }

        match grammar {
            "Expression" => process_input::<arithmetic::grammar::Expression>(input),
            "Repetition" => process_input::<repetitions::grammar::Repetitions>(input),
            "Optional" => process_input::<optionals::grammar::Language>(input),
            "Word" => process_input::<words::grammar::Words>(input),
            _ => {}
        }
    }
}

fn process_input<T: Debug + Language>(input: &str) {
    match T::parse(input).into_result() {
        Ok(expr) => println!("{expr:#?}"),
        Err(errs) => {
            let mut codemap = CodeMap::new();
            let file_span = codemap.add_file("<input>".to_string(), input.to_string());
            let mut diagnostics = vec![];
            for error in errs {
                let d = convert_parse_error_to_diagnostics(&file_span.span, &error);
                diagnostics.push(d);
            }

            let mut emitter = Emitter::stderr(ColorConfig::Always, Some(&codemap));
            emitter.emit(&diagnostics);
        }
    };
}
