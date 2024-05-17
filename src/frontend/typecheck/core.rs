use std::collections::{HashMap, HashSet};

use super::{TypecheckError, Use, Value, ID};

#[derive(Debug, Clone)]
pub enum VTypeHead<'input> {
    Bool,
    Str,
    Int,
    Float,
    Func { arg: Use, ret: Value },
    Obj { fields: HashMap<&'input str, Value> },
    Case { case: (&'input str, Value) },
}

#[derive(Debug, Clone)]
pub enum UTypeHead<'input> {
    Bool,
    Str,
    Int,
    Float,
    Func { arg: Value, ret: Use },
    Obj { field: (&'input str, Use) },
    Case { case: HashMap<&'input str, Use> },
}

#[derive(Debug, Clone)]
pub enum TypeNode<'input> {
    Var,
    Value(VTypeHead<'input>),
    Use(UTypeHead<'input>),
}

#[derive(Debug, Clone, Default)]
struct OrderedSet<T> {
    v: Vec<T>,
    s: HashSet<T>,
}

impl<T: Eq + std::hash::Hash + Clone> OrderedSet<T> {
    fn insert(&mut self, value: T) -> bool {
        if self.s.insert(value.clone()) {
            self.v.push(value);
            true
        } else {
            false
        }
    }

    fn iter(&self) -> std::slice::Iter<T> {
        self.v.iter()
    }
}

#[derive(Debug, Clone, Default)]
pub struct Reachability {
    upsets: Vec<OrderedSet<ID>>,
    downsets: Vec<OrderedSet<ID>>,
}

impl Reachability {
    fn add_node(&mut self) -> ID {
        let i = self.upsets.len();
        self.upsets.push(Default::default());
        self.downsets.push(Default::default());
        i
    }

    fn add_edge(&mut self, lhs: ID, rhs: ID, out: &mut Vec<(ID, ID)>) {
        let mut work = vec![(lhs, rhs)];
        while let Some((lhs, rhs)) = work.pop() {
            if !self.downsets[lhs].insert(rhs) {
                continue;
            }

            self.upsets[rhs].insert(lhs);
            out.push((lhs, rhs));

            for &lhs2 in self.upsets[lhs].iter() {
                work.push((lhs2, rhs));
            }

            for &rhs2 in self.upsets[rhs].iter() {
                work.push((lhs, rhs2));
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct TypeCheckerCore<'input> {
    r: Reachability,
    types: Vec<TypeNode<'input>>,
}

impl<'input> TypeCheckerCore<'input> {
    fn new_val(&mut self, val_type: VTypeHead<'input>) -> Value {
        let i = self.r.add_node();
        assert!(i == self.types.len());
        self.types.push(TypeNode::Value(val_type));
        Value(i)
    }

    fn new_use(&mut self, constraint: UTypeHead<'input>) -> Use {
        let i = self.r.add_node();
        assert!(i == self.types.len());
        self.types.push(TypeNode::Use(constraint));
        Use(i)
    }

    pub fn var(&mut self) -> (Value, Use) {
        let i = self.r.add_node();
        assert!(i == self.types.len());
        self.types.push(TypeNode::Var);
        (Value(i), Use(i))
    }

    pub fn bool(&mut self) -> Value {
        self.new_val(VTypeHead::Bool)
    }

    pub fn bool_use(&mut self) -> Use {
        self.new_use(UTypeHead::Bool)
    }

    pub fn string(&mut self) -> Value {
        self.new_val(VTypeHead::Str)
    }

    pub fn string_use(&mut self) -> Use {
        self.new_use(UTypeHead::Str)
    }

    pub fn int(&mut self) -> Value {
        self.new_val(VTypeHead::Int)
    }

    pub fn int_use(&mut self) -> Use {
        self.new_use(UTypeHead::Int)
    }

    pub fn float(&mut self) -> Value {
        self.new_val(VTypeHead::Float)
    }

    pub fn float_use(&mut self) -> Use {
        self.new_use(UTypeHead::Float)
    }

    pub fn func(&mut self, arg: Use, ret: Value) -> Value {
        self.new_val(VTypeHead::Func { arg, ret })
    }

    pub fn func_use(&mut self, arg: Value, ret: Use) -> Use {
        self.new_use(UTypeHead::Func { arg, ret })
    }

    pub fn flow(&mut self, lhs: Value, rhs: Use) -> Result<(), TypecheckError> {
        let mut pending_edges = vec![(lhs, rhs)];
        let mut type_pairs_to_check = Vec::new();
        while let Some((lhs, rhs)) = pending_edges.pop() {
            self.r.add_edge(lhs.0, rhs.0, &mut type_pairs_to_check);

            // Check if adding that edge resulted in any new type pairs needing to be checked
            while let Some((lhs, rhs)) = type_pairs_to_check.pop() {
                if let TypeNode::Value(lhs_head) = &self.types[lhs] {
                    if let TypeNode::Use(rhs_head) = &self.types[rhs] {
                        check_heads(lhs_head, rhs_head, &mut pending_edges)?;
                    }
                }
            }
        }
        assert!(pending_edges.is_empty() && type_pairs_to_check.is_empty());
        Ok(())
    }

    pub fn debug_v(&mut self, lhs: Value) -> TypeNode {
        self.types[lhs.0].clone()
    }
}

pub fn check_heads(
    lhs: &VTypeHead,
    rhs: &UTypeHead,
    out: &mut Vec<(Value, Use)>,
) -> Result<(), TypecheckError> {
    use UTypeHead as U;
    use VTypeHead as V;

    match (lhs, rhs) {
        (&V::Bool, &U::Bool) => Ok(()),
        (&V::Str, &U::Str) => Ok(()),
        (&V::Int, &U::Int) => Ok(()),
        (&V::Float, &U::Float) => Ok(()),
        (
            &V::Func { arg, ret },
            &U::Func {
                arg: uarg,
                ret: uret,
            },
        ) => {
            out.push((ret, uret));
            out.push((uarg, arg));

            Ok(())
        }

        _ => Err(TypecheckError::TypeError),
    }
}
