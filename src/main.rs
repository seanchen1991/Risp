use std::io;
use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;
use std::num::ParseFloatError;

static PROMPT: &str = "risp >";

#[derive(Clone)]
enum Expression {
    Bool(bool),
    Symbol(String),
    Number(f64),
    List(Vec<Expression>),
    Func(fn(&[Expression]) -> Result<Expression, RispErr>),
    Lambda(Lambda),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Expression::Bool(b) => b.to_string(),
            Expression::Symbol(s) => s.clone(),
            Expression::Number(n) => n.to_string(),
            Expression::Lambda(_) => "Lambda {}".to_string(),
            Expression::List(list) => {
                let xs: Vec<String> = list 
                    .iter()
                    .map(|x| x.to_string())
                    .collect();
                format!("({})", xs.join(","))
            },
            Expression::Func(_) => "Function {}".to_string(),
        };

        write!(f, "{}", str)
    }
}

#[derive(Debug)]
enum RispErr {
    Reason(String),
}

impl fmt::Display for RispErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            RispErr::Reason(r) => r.clone(),
        };

        write!(f, "{}", str)
    }
}

#[derive(Clone)]
struct Env<'a> {
    operations: HashMap<String, Expression>,
    outer: Option<&'a Env<'a>>,
}

macro_rules! ensure_tonicity {
    ($check_fn:expr) => {
        |args: &[Expression]| -> Result<Expression, RispErr> {
            let floats = parse_list_of_floats(args)?;
            let first = floats.first().ok_or(
                RispErr::Reason("expected at least one number".to_string())
            )?;
            let rest = &floats[1..];

            fn f(prev: &f64, xs: &[f64]) -> bool {
                match xs.first() {
                    Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                    None => true,
                }
            };
            Ok(Expression::Bool(f(first, rest)))
        }
    };
}

#[derive(Clone)]
struct Lambda {
    params_exp: Rc<Expression>,
    body_exp: Rc<Expression>,
}

fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse<'a>(tokens: &'a [String]) -> Result<(Expression, &'a [String]), RispErr> {
    let (token, rest) = tokens.split_first().ok_or(
        RispErr::Reason("Could not get Expression".to_string())
    )?;
    
    match &token[..] {
        "(" => read_sequence(rest),
        ")" => Err(RispErr::Reason("Unexpected `)`".to_string())),
        _   => Ok((parse_atom(token), rest)),
    }
}

fn read_sequence<'a>(tokens: &'a [String]) -> Result<(Expression, &'a [String]), RispErr> {
    let mut result: Vec<Expression> = vec![];
    let mut ts = tokens;

    loop {
        let (next, rest) = ts.split_first().ok_or(
            RispErr::Reason("Could not find closing `)`".to_string())
        )?;
        
        if next == ")" {
            return Ok((Expression::List(result), rest));
        }

        let (exp, new_ts) = parse(&ts)?;
        result.push(exp);
        ts = new_ts;
    }
}

fn parse_atom(token: &str) -> Expression {
    match token.as_ref() {
        "true" => Expression::Bool(true),
        "false" => Expression::Bool(false),
        _ => {
            match token.parse() {
                Ok(n)  => Expression::Number(n),
                Err(_) => Expression::Symbol(token.to_string().clone())
            }
        }
    }
}

fn init_env<'a>() -> Env<'a> {
    let mut operations: HashMap<String, Expression> = HashMap::new();

    operations.insert(
        "+".to_string(),
        Expression::Func(
            |args: &[Expression]| -> Result<Expression, RispErr> {
                let sum = parse_list_of_floats(args)?.iter().sum();

                Ok(Expression::Number(sum))
            }
        )
    );

    operations.insert(
        "-".to_string(),
        Expression::Func(
            |args: &[Expression]| -> Result<Expression, RispErr> {
                let floats = parse_list_of_floats(args)?;
                let (first, rest) = floats.split_first().ok_or(RispErr::Reason("Expected at least one number".to_string()))?;
                let sum_of_rest: f64 = rest.iter().sum();

                Ok(Expression::Number(first - sum_of_rest))
            }
        )
    );

    operations.insert(
        "*".to_string(),
        Expression::Func(
            |args: &[Expression]| -> Result<Expression, RispErr> {
                let product = parse_list_of_floats(args)?.iter().product();

                Ok(Expression::Number(product))
            }
        )
    );

    operations.insert(
        "/".to_string(),
        Expression::Func(
            |args: &[Expression]| -> Result<Expression, RispErr> {
                let floats = parse_list_of_floats(args)?;
                let first = *floats.first().ok_or(RispErr::Reason("Expected at least one number".to_string()))?;
                let result = floats[1..].iter()
                    .filter(|x| **x != 0.0)
                    .fold(first, |num, div| num / div);

                Ok(Expression::Number(result))
            }
        ) 
    );

    operations.insert(
        "max".to_string(),
        Expression::Func(
            |args: &[Expression]| -> Result<Expression, RispErr> {
                let floats = parse_list_of_floats(args)?;
                let first = *floats.first().ok_or(RispErr::Reason("max expects at least one number".to_string()))?;
                let max = floats.iter().fold(first, |acc, curr| acc.max(*curr));

                Ok(Expression::Number(max))
            }
        )
    );

    operations.insert(
        "min".to_string(),
        Expression::Func(
            |args: &[Expression]| -> Result<Expression, RispErr> {
                let floats = parse_list_of_floats(args)?;
                let first = *floats.first().ok_or(RispErr::Reason("min expects at least one number".to_string()))?;
                let min = floats.iter().fold(first, |acc, curr| acc.min(*curr));

                Ok(Expression::Number(min))
            }
        )
    );

    operations.insert(
        "abs".to_string(),
        Expression::Func(
            |arg: &[Expression]| -> Result<Expression, RispErr> {
                let float = parse_list_of_floats(arg)?; 

                if float.len() > 1 {
                    return Err(RispErr::Reason("abs expects a single number".to_string()));
                }

                Ok(Expression::Number(float[0].abs()))
            }
        ) 
    );

    operations.insert(
        "expt".to_string(),
        Expression::Func(
            |args: &[Expression]| -> Result<Expression, RispErr> {
                let floats = parse_list_of_floats(args)?;

                if floats.len() != 2 {
                    return Err(RispErr::Reason("expt expects exactly two numbers".to_string()));
                }

                let operand = floats.first().unwrap();
                let exponent = floats.last().unwrap();

                Ok(Expression::Number(operand.powf(*exponent)))
            }
        )
    );

    operations.insert(
        "round".to_string(),
        Expression::Func(
            |arg: &[Expression]| -> Result<Expression, RispErr> {
                let float = parse_list_of_floats(arg)?; 

                if float.len() > 1 {
                    return Err(RispErr::Reason("round expects a single number".to_string()));
                }

                Ok(Expression::Number(float[0].round()))
            }
        )  
    );

    operations.insert(
        "=".to_string(),
        Expression::Func(ensure_tonicity!(|a, b| a == b))
    );

    operations.insert(
        ">".to_string(),
        Expression::Func(ensure_tonicity!(|a, b| a > b))
    );

    operations.insert(
        "<".to_string(),
        Expression::Func(ensure_tonicity!(|a, b| a < b))
    );

    operations.insert(
        ">=".to_string(),
        Expression::Func(ensure_tonicity!(|a, b| a >= b))
    );

    operations.insert(
        "<=".to_string(),
        Expression::Func(ensure_tonicity!(|a, b| a <= b))
    );

    Env { operations, outer: None }
}

fn parse_list_of_floats(args: &[Expression]) -> Result<Vec<f64>, RispErr> {
    args.iter()
        .map(|x| parse_single_float(x))
        .collect()
}

fn parse_single_float(exp: &Expression) -> Result<f64, RispErr> {
    match exp {
        Expression::Number(num) => Ok(*num),
        _ => Err(RispErr::Reason("Expected a number".to_string())),
    }
}

fn env_get(k: &str, env: &Env) -> Option<Expression> {
    match env.operations.get(k) {
        Some(exp) => Some(exp.clone()),
        None => {
            match &env.outer {
                Some(outer_env) => env_get(k, &outer_env),
                None => None,
            }
        }
    }
}

fn eval(exp: &Expression, env: &mut Env) -> Result<Expression, RispErr> {
    match exp {
        Expression::Bool(_) => Ok(exp.clone()),
        Expression::Symbol(k) => {
            env_get(k, env).ok_or(RispErr::Reason(format!("unexpected symbol k={}", k)))
        },
        Expression::Number(_) => Ok(exp.clone()),
        Expression::List(list) => {
            let first_form = list.first().ok_or(
                RispErr::Reason("expected a non-empty list".to_string())
            )?;
            let arg_forms = &list[1..];

            match eval_built_in_form(first_form, arg_forms, env) {
                Some(res) => res,
                None => {
                    let first_eval = eval(first_form, env)?;

                    match first_eval {
                        Expression::Func(f) => {
                            f(&eval_forms(arg_forms, env)?)
                        },
                        Expression::Lambda(lambda) => {
                            let new_env = &mut env_for_lambda(lambda.params_exp, arg_forms, env)?;
                            eval(&lambda.body_exp, new_env)
                        },
                        _ => Err(
                            RispErr::Reason("first form must be a function".to_string())
                        ),
                    }
                }
            }
        },
        Expression::Func(_) => Err(
            RispErr::Reason("unexpected function".to_string())
        ),
        Expression::Lambda(_) => Err(RispErr::Reason("unexpected form".to_string())),
    }
}

fn eval_forms(arg_forms: &[Expression], env: &mut Env) -> Result<Vec<Expression>, RispErr> {
    arg_forms.iter()
        .map(|x| eval(x, env))
        .collect()
}

fn env_for_lambda<'a>(params: Rc<Expression>, arg_forms: &[Expression], outer_env: &'a mut Env) -> Result<Env<'a>, RispErr> {
    let ks = parse_list_of_symbol_strings(params)?;

    if ks.len() != arg_forms.len() {
        return Err(RispErr::Reason(format!("expected {} arguments, got {}", ks.len(), arg_forms.len())));
    }

    let vs = eval_forms(arg_forms, outer_env)?;
    let mut operations: HashMap<String, Expression> = HashMap::new();

    for (k, v) in ks.iter().zip(vs.iter()) {
        operations.insert(k.clone(), v.clone());
    }

    Ok(
        Env {
            operations,
            outer: Some(outer_env),
        }
    )
}

fn parse_list_of_symbol_strings(form: Rc<Expression>) -> Result<Vec<String>, RispErr> {
    let list = match form.as_ref() {
        Expression::List(s) => Ok(s.clone()),
        _ => Err(RispErr::Reason("expected args form to be a list".to_string()))
    }?;

    list.iter()
        .map(|x| {
            match x {
                Expression::Symbol(s) => Ok(s.clone()),
                _ => Err(RispErr::Reason("expected symbols in the argument list".to_string()))
            }
        })
        .collect()
}

fn eval_built_in_form(exp: &Expression, arg_forms: &[Expression], env: &mut Env) -> Option<Result<Expression, RispErr>> {
    match exp {
        Expression::Symbol(s) => {
            match s.as_ref() {
                "if" => Some(eval_if_args(arg_forms, env)),
                "def" => Some(eval_def_args(arg_forms, env)),
                "lambda" => Some(eval_lambda_args(arg_forms)),
                _ => None,
            }
        },
        _ => None,
    }
}

fn eval_if_args(arg_forms: &[Expression], env: &mut Env) -> Result<Expression, RispErr> {
    let test_form = arg_forms.first().ok_or(
        RispErr::Reason("expected test form".to_string())
    )?;
    let test_eval = eval(test_form, env)?;

    match test_eval {
        Expression::Bool(b) => {
            let form_idx = if b { 1 } else { 2 };
            let res_form = arg_forms.get(form_idx).ok_or(
                RispErr::Reason(format!("expected form idx={}", form_idx))
            )?;
            let res_eval = eval(res_form, env);

            res_eval
        },
        _ => Err(
            RispErr::Reason(format!("unexpected test form={}", test_form.to_string()))
        )
    }
}

fn eval_def_args(arg_forms: &[Expression], env: &mut Env) -> Result<Expression, RispErr> {
    let first_form = arg_forms.first().ok_or(
        RispErr::Reason("expected first form".to_string())
    )?;
    let first_str = match first_form {
        Expression::Symbol(s) => Ok(s.clone()),
        _ => Err(
            RispErr::Reason("expected first form to be a symbol".to_string())
        )
    }?;
    let second_form = arg_forms.get(1).ok_or(
        RispErr::Reason("expected second form".to_string())
    )?;

    if arg_forms.len() > 2 {
        return Err(
            RispErr::Reason("def can only take two forms".to_string())
        )
    }

    let second_eval = eval(second_form, env)?;
    env.operations.insert(first_str, second_eval);

    Ok(first_form.clone())
}

fn eval_lambda_args(arg_forms: &[Expression]) -> Result<Expression, RispErr> {
    let params_exp = arg_forms.first().ok_or(
        RispErr::Reason("expected args form".to_string())
    )?;
    let body_exp = arg_forms.get(1).ok_or(
        RispErr::Reason("expected second form".to_string())
    )?;

    if arg_forms.len() > 2 {
        return Err(
            RispErr::Reason("lambda definition can only have two forms".to_string())
        )
    }

    Ok(Expression::Lambda(
        Lambda {
            body_exp: Rc::new(body_exp.clone()),
            params_exp: Rc::new(params_exp.clone()),
        }
    ))
}

fn parse_eval(expr: String, env: &mut Env) -> Result<Expression, RispErr> {
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    let evaluated_exp = eval(&parsed_exp, env)?;

    Ok(evaluated_exp)
}

fn main() {
    let env = &mut init_env();

    loop {
        println!("{}", PROMPT);
        let mut expr = String::new();
    
        io::stdin().read_line(&mut expr).expect("failed to read line");

        if expr.trim() == "quit" {
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
