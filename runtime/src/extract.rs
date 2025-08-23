use super::Node;
pub mod field;
pub use field::{ExtractFieldState, ExtractFieldContext, ExtractFieldIterator};
pub use crate::error::ExtractError;
pub type Result<'a, T> = std::result::Result<T, ExtractError<'a>>;

pub trait Extractor<E: Extract> {
    fn do_extract<'tree>(
        self,
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
    ) -> Result<'tree, E>;
}

/// Defines the logic used to convert a node in a Tree Sitter tree to
/// the corresponding Rust type.
pub trait Extract: Sized {
    fn extract<'tree>(
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
    ) -> Result<'tree, Self>;

    fn extract_field<'cursor, 'tree>(
        ctx: &mut ExtractContext,
        it: &mut ExtractFieldIterator<'cursor, 'tree>,
        source: &[u8],
    ) -> Result<'tree, Self> {
        let node = it.next_node()?;
        assert!(it.current_node().is_none());
        Self::extract(ctx, node, source)
    }
}

pub struct ExtractContext {
    pub last_idx: usize,
    pub last_pt: tree_sitter::Point,
    pub field_name: &'static str,
    // TODO: Remove this, clean it up.
    pub node_kind: &'static str,
}

pub struct RuleExtractor {}

impl<E: Extract> Extractor<E> for RuleExtractor {
    fn do_extract<'tree>(
        self,
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
    ) -> Result<'tree, E> {
        E::extract(ctx, node, source)
    }
}

pub struct WithLeafExtractor<E, B, F> {
    _e: std::marker::PhantomData<E>,
    base: B,
    f: F,
}

impl<E, B, F> WithLeafExtractor<E, B, F> {
    pub fn new(base: B, f: F) -> WithLeafExtractor<E, B, F> {
        WithLeafExtractor {
            _e: std::marker::PhantomData,
            base,
            f,
        }
    }
}

impl<B, E, F, O> Extractor<O> for WithLeafExtractor<E, B, F>
where
    B: Extractor<E>,
    E: Extract,
    O: Extract,
    F: FnOnce(E) -> O,
{
    fn do_extract<'tree>(
        self,
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
    ) -> Result<'tree, O> {
        Ok((self.f)(self.base.do_extract(ctx, node, source)?))
    }
}


#[derive(Debug, Clone, Copy)]
pub struct NodeExt<'a> {
    pub node: Node<'a>,
    pub source: &'a [u8],
    pub last_idx: usize,
    pub last_pt: tree_sitter::Point,
}

pub trait StrOrNode {
    type Output;
    fn apply(
        self,
        source: &[u8],
        node: Node<'_>,
        last_idx: usize,
        last_pt: tree_sitter::Point,
    ) -> Self::Output;
}

impl<L> StrOrNode for fn(&str) -> L {
    type Output = L;
    fn apply(
        self,
        source: &[u8],
        node: Node<'_>,
        _last_idx: usize,
        _last_pt: tree_sitter::Point,
    ) -> L {
        let text = node.utf8_text(source).expect("Could not get text");
        self(text)
    }
}

impl<L> StrOrNode for fn(&NodeExt<'_>) -> L {
    type Output = L;
    fn apply(
        self,
        source: &[u8],
        node: Node<'_>,
        last_idx: usize,
        last_pt: tree_sitter::Point,
    ) -> L {
        let node = NodeExt {
            node,
            source,
            last_idx,
            last_pt,
        };
        self(&node)
    }
}

// pub trait Handler<Input, Output> {
//     fn extract(
//         self,
//         node: Option<Node>,
//         source: &[u8],
//         last_idx: usize,
//         last_pt: tree_sitter::Point,
//     ) -> Output;
// }
//
// macro_rules! handler_fn {
//     ($($t:ident),*) => {
//        impl<F, O, $($t: Extract<$t>),*> Handler<($($t),*), O> for F
//            where F: FnOnce($($t),*) -> O,
//        {
//            fn extract(
//                self,
//                 node: Option<Node>,
//                 source: &[u8],
//                 last_idx: usize,
//                 last_pt: tree_sitter::Point,
//             ) -> O {
//                 let node = node.expect("No node found");
//                 let mut c = node.walk();
//                 let mut it = node.children(&mut c);
//                 self(
//                     $(
//                         $t::extract(it.next(), source, last_idx, last_pt, None)
//                     ),*
//                 )
//             }
//        }
//
//     };
// }
//
// handler_fn!(T1, T2);

// /// Map for `#[with(...)]`
// pub struct WithLeaf<L, F> {
//     _phantom: std::marker::PhantomData<L>,
//     _f: std::marker::PhantomData<F>,
// }
//
// impl<L: 'static, F> Extract<L> for WithLeaf<L, F>
// where
//     F: StrOrNode<Output = L> + Clone,
// {
//     type LeafFn<'a> = F;
//
//     fn extract<'a, 'tree>(
//         ctx: &mut ExtractContext<'_, 'tree>,
//         node: Option<Node<'tree>>,
//         source: &[u8],
//         leaf_fn: Option<Self::LeafFn<'a>>,
//     ) -> Result<'tree, L> {
//         let node = match node {
//             Some(n) => n,
//             None => return Err(ExtractError::missing_node(ctx, "WithLeaf")),
//         };
//         // TODO: Consider if this should be fallible as well.
//         Ok(leaf_fn.expect("No leaf function on WithLeaf").apply(
//             source,
//             node,
//             ctx.last_idx,
//             ctx.last_pt,
//         ))
//     }
// }

// #[derive(Clone)]
// pub struct MappedExtract<F, L0, L1> {
//     _type: std::marker::PhantomData<F>,
//     _prev: std::marker::PhantomData<L0>,
//     _curr: std::marker::PhantomData<L1>,
// }
//
// #[derive(Clone)]
// pub struct MappedLeaf<P, F> {
//     prev: Option<P>,
//     curr: F,
// }
//
// impl<F, L0: 'static, L1: 'static> Extract<L1> for MappedExtract<F, L0, L1>
// where
//     F: Extract<L0>,
// {
//     type LeafFn<'a> = MappedLeaf<F::LeafFn<'a>, &'a dyn Fn(L0) -> L1>;
//     fn extract<'a>(
//         node: Option<Node>,
//         source: &[u8],
//         last_idx: usize,
//         last_pt: tree_sitter::Point,
//         leaf_fn: Option<Self::LeafFn<'a>>,
//     ) -> L1 {
//         let mapped = leaf_fn.unwrap();
//         let prev = F::extract(node, source, last_idx, last_pt, mapped.prev);
//         (mapped.curr)(prev)
//     }
// }

// Common implementations for various types.

impl Extract for () {
    fn extract<'a, 'tree>(
        _ctx: &mut ExtractContext,
        _node: Option<Node<'tree>>,
        _source: &[u8],
    ) -> Result<'tree, ()> {
        Ok(())
    }
}

impl<T: Extract> Extract for Option<T> {
    fn extract<'a, 'tree>(
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
    ) -> Result<'tree, Option<T>> {
        node.map(|n| T::extract(ctx, Some(n), source)).transpose()
    }

    fn extract_field<'cursor, 'tree>(
        ctx: &mut ExtractContext,
        it: &mut ExtractFieldIterator<'cursor, 'tree>,
        source: &[u8],
    ) -> Result<'tree, Self> {
        if it.current_node().is_some() {
            Ok(Some(T::extract_field(ctx, it, source)?))
        } else {
            Ok(None)
        }
    }
}

impl<T: Extract> Extract for Box<T> {
    fn extract<'a, 'tree>(
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
    ) -> Result<'tree, Box<T>> {
        Ok(Box::new(T::extract(ctx, node, source)?))
    }

    fn extract_field<'cursor, 'tree>(
        ctx: &mut ExtractContext,
        it: &mut ExtractFieldIterator<'cursor, 'tree>,
        source: &[u8],
    ) -> Result<'tree, Self> {
        Ok(Box::new(T::extract_field(ctx, it, source)?))
    }
}

impl<T: Extract> Extract for Vec<T> {
    fn extract<'a, 'tree>(
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
    ) -> Result<'tree, Vec<T>> {
        let node = match node {
            Some(node) => node,
            None => return Ok(vec![]),
        };
        let mut out = vec![];
        let mut cursor = node.walk();
        let mut error = ExtractError::empty();
        if cursor.goto_first_child() {
            loop {
                // Try and parse the error specially.
                let n = cursor.node();
                if n.is_error() {
                    // println!("Processing error... for {}", ctx.field_name);
                    // TODO: Do some error handling here instead.
                    // For now we just ignore it.
                } else if cursor.field_name().is_some() {
                    match T::extract(ctx, Some(n), source) {
                        Ok(t) => out.push(t),
                        Err(e) => error.merge(e),
                    }
                }
                ctx.last_idx = n.end_byte();
                ctx.last_pt = n.end_position();
                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }
        error.prop()?;
        Ok(out)
    }
}

macro_rules! extract_from_str {
    ($t:ty) => {
        impl Extract for $t {
            fn extract<'tree>(
                _ctx: &mut ExtractContext,
                node: Option<Node<'tree>>,
                source: &[u8],
            ) -> Result<'tree, Self> {
                let node = match node {
                    Some(n) => n,
                    None => {
                        panic!("Better error");
                        // return Err(ExtractError::missing_node(ctx, stringify!($t)));
                    }
                };
                let text = node.utf8_text(source).expect("No text found for node");
                match text.parse() {
                    Ok(t) => Ok(t),
                    Err(e) => Err(ExtractError::type_conversion(node, e)),
                }
            }
        }
    };
}

extract_from_str!(u8);
extract_from_str!(i8);
extract_from_str!(u16);
extract_from_str!(i16);
extract_from_str!(u32);
extract_from_str!(i32);
extract_from_str!(u64);
extract_from_str!(i64);
// NOTE: These two may not work as intended due to rounding issues.
extract_from_str!(f32);
extract_from_str!(f64);
// Sort of silly, but keeps it general.
extract_from_str!(String);

macro_rules! extract_for_tuple {
    ($($t:ident),*) => {
       impl<$($t: Extract),*> Extract for ($($t),*) {
           fn extract<'tree>(
               _ctx: &mut ExtractContext,
               _node: Option<Node<'tree>>,
               _source: &[u8],
           ) -> Result<'tree, Self> {
               panic!("Cannot be implemented on tuples")
           }

           fn extract_field<'cursor, 'tree>(ctx: &mut ExtractContext, it: &mut ExtractFieldIterator<'cursor, 'tree>, source: &[u8]) -> Result<'tree, Self> {
               // NOTE: Nested tuples are not supported as it stands.
               log::debug!("extract_field on tuple");
               Ok((
                   $(
                       $t::extract(ctx, it.next_node()?, source)?
                    ),*
               ))
           }

       }

    };
}

extract_for_tuple!(T1, T2);
extract_for_tuple!(T1, T2, T3);
extract_for_tuple!(T1, T2, T3, T4);
extract_for_tuple!(T1, T2, T3, T4, T5);
extract_for_tuple!(T1, T2, T3, T4, T5, T6);
extract_for_tuple!(T1, T2, T3, T4, T5, T6, T7);
extract_for_tuple!(T1, T2, T3, T4, T5, T6, T7, T8);
// Good enough, can maybe generate all of these with a macro if we are clever enough.

// Would like this to extract optionals specifically if they exist - probably means if a node is
// present then it is true. Might be too magic though.
// impl Extract<bool> for bool {
//     type LeafFn = ();
//     fn extract(
//             node: Option<tree_sitter::Node>,
//             source: &[u8],
//             last_idx: usize,
//             leaf_fn: Option<&Self::LeafFn>,
//         ) -> bool {
//     }
// }
