use super::ast::{Ast, Expr, Identifier, Literal, Statement, TypeExpr};
use core::TypeCheckerCore;
use std::collections::HashMap;
use thiserror::Error;

pub mod core;

// TODO: Better error handling

pub type ID = usize;

#[derive(Debug, Clone, Copy)]
pub struct Value(ID);
#[derive(Debug, Clone, Copy)]
pub struct Use(ID);

#[derive(Debug, Error)]
pub enum TypecheckError {
    #[error("Typeerror occured")]
    TypeError,
}

#[derive(Debug, Clone)]
pub struct Bindings<'input> {
    map: HashMap<&'input str, Value>,
    changes: Vec<(&'input str, Option<Value>)>,
}

impl<'input> Bindings<'input> {
    fn new() -> Self {
        Self {
            changes: vec![],
            map: HashMap::new(),
        }
    }

    fn get(&self, k: &str) -> Option<Value> {
        self.map.get(k).copied()
    }

    fn set(&mut self, k: &'input str, v: Value) {
        let old = self.map.insert(k, v);
        self.changes.push((k, old));
    }

    fn unwind(&mut self, n: usize) {
        while self.changes.len() > n {
            let (k, old) = self.changes.pop().unwrap();

            match old {
                Some(v) => {
                    self.set(k, v);
                    None
                }
                None => self.map.remove(k),
            };
        }
    }

    fn in_child_scope<T>(&mut self, cb: impl FnOnce(&mut Self) -> T) -> T {
        let n = self.changes.len();
        let res = cb(self);
        self.unwind(n);
        res
    }
}

pub fn parse_type(
    engine: &mut TypeCheckerCore,
    type_expr: TypeExpr,
) -> Result<(Value, Use), TypecheckError> {
    match type_expr {
        TypeExpr::Int => Ok((engine.int(), engine.int_use())),
        TypeExpr::Float => Ok((engine.float(), engine.float_use())),
        TypeExpr::String => Ok((engine.string(), engine.string_use())),
        TypeExpr::Bool => Ok((engine.bool(), engine.bool_use())),

        TypeExpr::Fn { lhs, rhs } => {
            let lhs = parse_type(engine, *lhs)?;
            let rhs = parse_type(engine, *rhs)?;

            let utype = engine.func_use(lhs.0, rhs.1);
            let vtype = engine.func(lhs.1, rhs.0);

            Ok((vtype, utype))
        }

        _ => Err(TypecheckError::TypeError),
    }
}

pub fn parse_type_signature(
    engine: &mut TypeCheckerCore,
    type_expr: TypeExpr,
) -> Result<(Value, Use), TypecheckError> {
    parse_type(engine, type_expr)
}

pub fn check_expr<'input>(
    engine: &mut TypeCheckerCore,
    bindings: &mut Bindings<'input>,
    expr: Expr<'input>,
) -> Result<Value, TypecheckError> {
    let val = match expr {
        Expr::Literal(lit) => match lit {
            Literal::Bool(_) => engine.bool(),
            Literal::String(_) => engine.string(),
            Literal::Int(_) => engine.int(),
            Literal::Float(_) => engine.float(),
        },

        Expr::Identifier(Identifier(i)) => bindings.get(i).ok_or(TypecheckError::TypeError)?,

        Expr::Lambda { params, func } => {
            let (arg_type, arg_bound) = engine.var();
            let body_type = bindings.in_child_scope(|b| {
                b.set(params[0].0, arg_type);
                check_expr(engine, b, *func)
            })?;

            engine.func(arg_bound, body_type)
        }

        Expr::Application { lhs, rhs } => {
            let func_type = check_expr(engine, bindings, *lhs)?;
            let arg_type = check_expr(engine, bindings, *rhs)?;

            let (ret_type, ret_bound) = engine.var();
            let bound = engine.func_use(arg_type, ret_bound);

            engine.flow(func_type, bound)?;
            ret_type
        }

        _ => todo!(""),
    };

    Ok(val)
}

fn construct_function_expr<'input>(
    params: &mut Vec<Identifier<'input>>,
    func: Expr<'input>,
) -> Expr<'input> {
    if !params.is_empty() {
        Expr::Lambda {
            params: vec![params.pop().unwrap()],
            func: Box::new(construct_function_expr(params, func)),
        }
    } else {
        func
    }
}

pub fn check_statement<'input>(
    engine: &mut TypeCheckerCore,
    bindings: &mut Bindings<'input>,
    statement: Statement<'input>,
) -> Result<Value, TypecheckError> {
    match statement {
        Statement::Declaration {
            name,
            patterns,
            expr,
            sig,
        } => {
            let var_type = if !patterns.is_empty() {
                // Desugar into lambdas
                // Ex:
                // ```hs
                // foo a b = a + b
                // ```
                // becomes
                // ```
                // foo = (\a -> (\b -> a + b))
                // ```
                // Could parhaps do this in the parsing step however it isn't too bad here
                let mut params = patterns.clone();
                let expr = construct_function_expr(&mut params, expr);

                check_expr(engine, bindings, expr)?
            } else {
                check_expr(engine, bindings, expr)?
            };

            bindings.set(name.0, var_type);

            if let Some(ty) = sig {
                let sig = parse_type_signature(engine, ty)?;
                engine.flow(var_type, sig.1)?;
            };

            Ok(var_type)
        }
        _ => todo!(""),
    }
}

#[derive(Debug, Clone)]
pub struct TypeCheckState<'input> {
    core: TypeCheckerCore<'input>,
    bindings: Bindings<'input>,
}

impl<'input> TypeCheckState<'input> {
    pub fn check_ast(
        &mut self,
        ast: Ast<'input>,
    ) -> Result<TypeCheckerCore<'input>, TypecheckError> {
        let mut temp = self.core.clone();

        for item in ast.0 {
            let v = check_statement(&mut self.core, &mut self.bindings, item);
            if let Err(err) = v {
                std::mem::swap(&mut self.core, &mut temp);
                self.bindings.unwind(0);
                return Err(err);
            }
        }

        self.bindings.changes.clear();
        Ok(self.core.clone())
    }
}

impl<'input> Default for TypeCheckState<'input> {
    fn default() -> Self {
        TypeCheckState {
            core: TypeCheckerCore::default(),
            bindings: Bindings::new(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::TypeCheckState;
    use crate::frontend::ast::Ast;

    #[test]
    fn type_checking() {
        let source = r#"
            my_str =
                "ok"
                : String

            inner_string a b =
                a b

            main =
                inner_string (\a -> my_str) "unused"
        "#;

        let ast = Ast::parse(source);
        let mut typechecker = TypeCheckState::default();

        let result = typechecker.check_ast(ast);
        assert!(result.is_ok(), "Failed to typecheck ast!")
    }

    #[test]
    fn invalid_type() {
        let source = r#"
            my_str =
                10
                : String
        "#;

        let ast = Ast::parse(source);
        let mut typechecker = TypeCheckState::default();

        let result = typechecker.check_ast(ast);
        assert!(result.is_err(), "Failed to typecheck ast!")
    }
}
