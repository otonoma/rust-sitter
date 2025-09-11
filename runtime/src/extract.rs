use super::Node;
pub mod field;
pub use crate::error::ExtractError;
pub use field::{ExtractFieldContext, ExtractFieldIterator, ExtractFieldState};

pub type Result<'a, T> = std::result::Result<T, ExtractError<'a>>;

/// Structs which can perform extractions. This allows an extractor to carry additional state
/// around the extraction (see for example, `WithLeafExtractor`).
pub trait Extractor<E: Extract> {
    fn do_extract<'tree>(
        self,
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
        leaf_fn: E::LeafFn,
    ) -> Result<'tree, E::Output>;

    fn do_extract_field<'cursor, 'tree>(
        self,
        ctx: &mut ExtractContext,
        it: &mut ExtractFieldIterator<'cursor, 'tree>,
        source: &[u8],
        leaf_fn: E::LeafFn,
    ) -> Result<'tree, E::Output>;

    fn map<F, O>(self, next: F) -> MapExtractor<E, Self, F>
    where
        F: FnOnce(E) -> O,
        Self: Sized,
    {
        MapExtractor::new(self, next)
    }
}

/// Defines the logic used to convert a node in a Tree Sitter tree to
/// the corresponding Rust type.
pub trait Extract: Sized {
    type LeafFn;
    type Output;
    fn extract<'tree>(
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
        leaf_fn: Self::LeafFn,
    ) -> Result<'tree, Self::Output>;

    fn extract_field<'cursor, 'tree>(
        ctx: &mut ExtractContext,
        it: &mut ExtractFieldIterator<'cursor, 'tree>,
        source: &[u8],
        leaf_fn: Self::LeafFn,
    ) -> Result<'tree, Self::Output> {
        let node = it.next_node()?;
        Self::extract(ctx, node, source, leaf_fn)
    }
}

pub struct ExtractContext {
    pub last_idx: usize,
    pub last_pt: tree_sitter::Point,
    pub field_name: &'static str,
    pub struct_name: &'static str,
}

/// Default extractor which simply delegates to the `Extract` implementation.
#[derive(Default)]
pub struct BaseExtractor {}

impl<E: Extract> Extractor<E> for BaseExtractor {
    fn do_extract<'tree>(
        self,
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
        leaf_fn: E::LeafFn,
    ) -> Result<'tree, E::Output> {
        E::extract(ctx, node, source, leaf_fn)
    }

    fn do_extract_field<'cursor, 'tree>(
        self,
        ctx: &mut ExtractContext,
        it: &mut ExtractFieldIterator<'cursor, 'tree>,
        source: &[u8],
        leaf_fn: E::LeafFn,
    ) -> Result<'tree, E::Output> {
        E::extract_field(ctx, it, source, leaf_fn)
    }
}

/// Transforms leaf nodes from one output type to another.
pub struct MapExtractor<E, B, F> {
    _e: std::marker::PhantomData<E>,
    base: B,
    f: F,
}

impl<E, B, F> MapExtractor<E, B, F> {
    pub fn new(base: B, f: F) -> MapExtractor<E, B, F> {
        MapExtractor {
            _e: std::marker::PhantomData,
            base,
            f,
        }
    }
}

impl<B, E, F, O> Extractor<O> for MapExtractor<E, B, F>
where
    B: Extractor<E>,
    E: Extract,
    O: Extract<LeafFn = E::LeafFn>,
    F: FnOnce(E::Output) -> O::Output,
{
    fn do_extract<'tree>(
        self,
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
        leaf_fn: O::LeafFn,
    ) -> Result<'tree, O::Output> {
        Ok((self.f)(self.base.do_extract(ctx, node, source, leaf_fn)?))
    }

    fn do_extract_field<'cursor, 'tree>(
        self,
        _ctx: &mut ExtractContext,
        _it: &mut ExtractFieldIterator<'cursor, 'tree>,
        _source: &[u8],
        _leaf_fn: E::LeafFn,
    ) -> Result<'tree, O::Output> {
        todo!()
    }
}

/// Map for `#[with(...)]`
pub struct WithLeaf<L, F> {
    _phantom: std::marker::PhantomData<L>,
    _f: std::marker::PhantomData<F>,
}

impl<L: 'static, F> Extract for WithLeaf<L, F>
where
    F: FnOnce(&str) -> L,
{
    type LeafFn = F;
    type Output = L;

    fn extract<'tree>(
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
        leaf_fn: Self::LeafFn,
    ) -> Result<'tree, L> {
        let node = match node {
            Some(n) => n,
            None => return Err(ExtractError::missing_node(ctx)),
        };
        let text = node.utf8_text(source).unwrap();
        Ok(leaf_fn(text))
    }
}

// Common implementations for various types.

impl Extract for () {
    type LeafFn = ();
    type Output = ();
    fn extract<'tree>(
        _ctx: &mut ExtractContext,
        _node: Option<Node<'tree>>,
        _source: &[u8],
        _l: (),
    ) -> Result<'tree, ()> {
        Ok(())
    }
}

impl<T: Extract> Extract for Option<T> {
    type LeafFn = T::LeafFn;
    type Output = Option<T::Output>;
    fn extract<'tree>(
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
        l: T::LeafFn,
    ) -> Result<'tree, Option<T::Output>> {
        node.map(|n| T::extract(ctx, Some(n), source, l))
            .transpose()
    }

    fn extract_field<'cursor, 'tree>(
        ctx: &mut ExtractContext,
        it: &mut ExtractFieldIterator<'cursor, 'tree>,
        source: &[u8],
        l: T::LeafFn,
    ) -> Result<'tree, Option<T::Output>> {
        if it.current_node().is_some() {
            Ok(Some(T::extract_field(ctx, it, source, l)?))
        } else {
            it.advance_state()?;
            Ok(None)
        }
    }
}

impl<T: Extract> Extract for Box<T> {
    type LeafFn = T::LeafFn;
    type Output = Box<T::Output>;
    fn extract<'tree>(
        ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        source: &[u8],
        l: Self::LeafFn,
    ) -> Result<'tree, Self::Output> {
        Ok(Box::new(T::extract(ctx, node, source, l)?))
    }

    fn extract_field<'cursor, 'tree>(
        ctx: &mut ExtractContext,
        it: &mut ExtractFieldIterator<'cursor, 'tree>,
        source: &[u8],
        l: Self::LeafFn,
    ) -> Result<'tree, Self::Output> {
        Ok(Box::new(T::extract_field(ctx, it, source, l)?))
    }
}

impl<T: Extract> Extract for Vec<T>
where
    T::LeafFn: Clone,
{
    type LeafFn = T::LeafFn;
    type Output = Vec<T::Output>;
    fn extract<'tree>(
        _ctx: &mut ExtractContext,
        node: Option<Node<'tree>>,
        _source: &[u8],
        _l: Self::LeafFn,
    ) -> Result<'tree, Self::Output> {
        match node {
            None => Ok(vec![]),
            Some(n) if n.child_count() == 0 => Ok(vec![]),
            _ => panic!("Cannot be implemented on Vec"),
        }
    }

    fn extract_field<'cursor, 'tree>(
        ctx: &mut ExtractContext,
        it: &mut ExtractFieldIterator<'cursor, 'tree>,
        source: &[u8],
        leaf_fn: Self::LeafFn,
    ) -> Result<'tree, Self::Output> {
        let mut out = vec![];
        let mut error = ExtractError::empty();
        while it.is_valid() {
            let n = it.current_node();
            match T::extract_field(ctx, it, source, leaf_fn.clone()) {
                Ok(t) => out.push(t),
                Err(e) => error.merge(e),
            }
            if let Some(n) = n {
                ctx.last_idx = n.end_byte();
                ctx.last_pt = n.end_position();
            }
        }
        error.prop()?;
        Ok(out)
    }
}

macro_rules! extract_from_str {
    ($t:ty) => {
        impl Extract for $t {
            type LeafFn = ();
            type Output = $t;
            fn extract<'tree>(
                ctx: &mut ExtractContext,
                node: Option<Node<'tree>>,
                source: &[u8],
                _l: (),
            ) -> Result<'tree, Self> {
                let node = match node {
                    Some(n) => n,
                    None => {
                        return Err(ExtractError::missing_node(ctx));
                    }
                };
                let text = node.utf8_text(source).expect("No text found for node");
                match text.parse() {
                    Ok(t) => Ok(t),
                    Err(e) => Err(ExtractError::type_conversion(ctx, node, e)),
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
       impl<$($t: Extract<Output = $t>),*> Extract for ($($t),*)
           where
               $(<$t as Extract>::LeafFn: Default),*
       {
           type LeafFn = ();
           type Output = Self;
           fn extract<'tree>(
               _ctx: &mut ExtractContext,
               _node: Option<Node<'tree>>,
               _source: &[u8],
               _l: (),
           ) -> Result<'tree, Self> {
               panic!("Cannot be implemented on tuples")
           }

           fn extract_field<'cursor, 'tree>(ctx: &mut ExtractContext, it: &mut ExtractFieldIterator<'cursor, 'tree>, source: &[u8], _l: ()) -> Result<'tree, Self> {
               // NOTE: Nested tuples are not supported as it stands.
               log::debug!("extract_field on tuple");
               Ok((
                   $(
                       $t::extract_field(ctx, it, source, Default::default())?
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
