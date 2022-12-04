use std::{marker::PhantomData, ops::ControlFlow, path::PathBuf};

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};
use syn::{
    fold::Fold, Expr, ExprRepeat, ItemConst, ItemFn, ItemImpl, ItemStatic, Pat, Type, Variant,
};

use crate::{
    display_or_empty_if_none,
    mutable::{self, Mutable},
    mutable_id::CrateId,
    CrateLocalMutableId, MutableId, PathSegment,
};

pub const MUTABLE_DEFINITIONS_CSV_HEAD: &str = "attr_id,id,kind,code,file,path,attr_span,span\n";

pub struct MuttestTransformer {
    pub conf: TransformerConf,
    mut_count: usize,
    definitions: Vec<String>,
    pub path: Vec<PathSegment>,
}
pub struct TransformerConf {
    pub attr_id: usize,
    pub span: Span,
    pub mutables: MutablesConf,
    pub muttest_api: TokenStream,
    pub pkg_name: &'static str,
    pub crate_name: String,
}
pub enum MutablesConf {
    All,
    One(String),
}

impl MuttestTransformer {
    pub fn new(conf: TransformerConf) -> Self {
        Self {
            conf,
            mut_count: 0,
            definitions: vec![],
            path: vec![],
        }
    }

    pub fn definitions(&self) -> &[String] {
        &self.definitions
    }
    pub fn num_mutables(&self) -> usize {
        self.mut_count
    }

    pub fn should_mutate<'a, M: Mutable<'a>>(&self) -> bool {
        match &self.conf.mutables {
            MutablesConf::One(m) => m == M::NAME,
            MutablesConf::All => true,
        }
    }

    pub(crate) fn new_mutable<'b, M: Mutable<'b>>(
        &mut self,
        m: &M,
        code: &str,
    ) -> TransformSnippets {
        self.mut_count += 1;
        let next_id = self.mut_count;
        let id = MutableId {
            crate_id: CrateId {
                pkg_name: self.conf.pkg_name.to_owned(),
                crate_name: self.conf.crate_name.clone(),
            },
            id: CrateLocalMutableId {
                attr_id: self.conf.attr_id,
                id: next_id,
            },
        };

        let span = m.span();
        // TODO: use csv crate instead.
        self.definitions.push(format!(
            r#"{},{},{},{},{},{},{},{}"#,
            id.id.attr_id,
            id.id.id,
            M::NAME,
            csv_quote(code),
            source_file_path(span).unwrap_or_default().display(),
            self.path
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(":"),
            display_or_empty_if_none(&crate::Span::from(self.conf.span)),
            display_or_empty_if_none(&crate::Span::from(span)),
        ));

        let muttest_api = self.muttest_api(span);
        let pkg_name = &id.crate_id.pkg_name;
        let crate_name = &id.crate_id.crate_name;
        let attr_id = id.id.attr_id;
        let id = id.id.id;
        let m_id = quote_spanned! {span=>
            #muttest_api::BakedMutableId {
                pkg_name: #pkg_name,
                crate_name: #crate_name,
                attr_id: #attr_id,
                id: #id,
            }
        };
        let attr_span = self.bake_span(self.conf.span);
        let mutable_span = self.bake_span(span);

        let loc = quote_spanned! {span=>
            #muttest_api::BakedLocation {
                file: #muttest_api::file!(),
                module: #muttest_api::module_path!(),
                attr_span: #attr_span,
                span: #mutable_span,
            }
        };

        TransformSnippets {
            m_id,
            muttest_api,
            loc,
        }
    }

    fn bake_span(&self, span: Span) -> TokenStream {
        // TODO: use Span::before/after when available to get the entire range
        let muttest_api = self.muttest_api(span);
        quote_spanned! {span=>
            #muttest_api::Span {
                start: #muttest_api::LineColumn {line: #muttest_api::line!(), column: #muttest_api::column!() },
                end: #muttest_api::Option::None,
            }
        }
    }

    pub fn muttest_api(&self, span: Span) -> TokenStream {
        fn relocate_token_stream(input: TokenStream, span: Span) -> TokenStream {
            use proc_macro2::{Group, TokenTree};
            input
                .into_iter()
                .map(|mut tt| {
                    match &mut tt {
                        TokenTree::Group(g) => {
                            g.set_span(span);
                            let s = relocate_token_stream(g.stream(), span);
                            *g = Group::new(g.delimiter(), s);
                        }
                        TokenTree::Ident(i) => i.set_span(span),
                        TokenTree::Punct(p) => p.set_span(span),
                        TokenTree::Literal(l) => l.set_span(span),
                    };
                    tt
                })
                .collect()
        }

        relocate_token_stream(self.conf.muttest_api.clone(), span)
    }
}

fn mutable_name<'a, M: Mutable<'a>>() -> &'static str {
    M::NAME
}

macro_rules! try_mutate {
    ($s:ident, $e:expr, $($m:path,)*) => {
        {
            $(
                if $s.should_mutate::<$m>() {
                    let m = {
                        #[allow(unused_imports)]
                        use is_match_mutable::{NotMatch, YesMatch};
                        (&(PhantomData::<$m>, crate::api::phantom_for_type($e))).get_match($e)
                    };
                    if let Some(m) = m {
                        match syn::parse2(m.transform($s)) {
                            Ok(m) => return ControlFlow::Break(m),
                            Err(e) => panic!("mutable `{}` transform syntax error: {}", mutable_name::<$m>(), e)
                        }
                    }
                }
            )*
            ControlFlow::Continue(())
        }
    };
}

macro_rules! try_mutate_all {
    ($s:ident, $ast_node:expr) => {
        try_mutate!(
            $s,
            $ast_node,
            mutable::lit_char::Mutable,
            mutable::lit_int::Mutable,
            mutable::lit_str::Mutable,
            mutable::binop_eq::Mutable,
            mutable::binop_cmp::Mutable,
            mutable::binop_calc::Mutable,
            mutable::assign_op::Mutable,
            mutable::binop_bool::Mutable,
            mutable::extreme::Mutable,
        )
    };
}

impl MuttestTransformer {
    // TODO: inline these when try-blocks are stable
    fn try_mutate_all_expr(&mut self, e: &Expr) -> ControlFlow<Expr> {
        try_mutate_all!(self, e)
    }
    fn try_mutate_all_item_fn(&mut self, i: &ItemFn) -> ControlFlow<ItemFn> {
        try_mutate_all!(self, i)
    }
}
impl Fold for MuttestTransformer {
    // TODO: inspect & preserve attrs

    fn fold_item_const(&mut self, item_const: ItemConst) -> ItemConst {
        item_const
    }
    fn fold_item_static(&mut self, item_static: ItemStatic) -> ItemStatic {
        item_static
    }
    fn fold_pat(&mut self, pat: Pat) -> Pat {
        pat
    }
    fn fold_expr_repeat(&mut self, mut expr_repeat: ExprRepeat) -> ExprRepeat {
        // only mutate expression not len
        expr_repeat.expr = Box::new(self.fold_expr(*expr_repeat.expr));
        expr_repeat
    }
    fn fold_type(&mut self, node: Type) -> Type {
        node
    }
    fn fold_variant(&mut self, v: Variant) -> Variant {
        v
    }

    fn fold_item_fn(&mut self, f: ItemFn) -> ItemFn {
        if f.sig.constness.is_some() {
            return f;
        }

        self.path.push(PathSegment::Fn(f.sig.ident.to_string()));

        let f = syn::fold::fold_item_fn(self, f);

        let f = match self.try_mutate_all_item_fn(&f) {
            ControlFlow::Break(f) => f,
            ControlFlow::Continue(_) => f,
        };
        self.path.pop();

        f
    }

    fn fold_item_impl(&mut self, node: ItemImpl) -> ItemImpl {
        self.path.push(PathSegment::Impl(
            node.self_ty.to_token_stream().to_string(),
        ));

        let i = syn::fold::fold_item_impl(self, node);

        self.path.pop();

        i
    }

    fn fold_expr(&mut self, e: Expr) -> Expr {
        let e = syn::fold::fold_expr(self, e);

        match self.try_mutate_all_expr(&e) {
            ControlFlow::Break(e) => e,
            ControlFlow::Continue(_) => e,
        }
    }
}

pub fn strip_expr_parens(e: &Expr) -> &Expr {
    match e {
        // TODO: this loses attrs
        // TODO: make this recursive?
        Expr::Paren(ep) => &ep.expr,
        _ => e,
    }
}

fn csv_quote(s: &str) -> String {
    if s.contains('\"') {
        format!(r#""{}""#, s.replace('"', r#""""#))
    } else {
        s.to_owned()
    }
}

// TODO: "match" makes less sense with this construct
mod is_match_mutable {
    use std::marker::PhantomData;

    use syn::parse::Parse;

    use crate::mutable::MatchMutable;

    pub trait NotMatch<'a, A, T> {
        fn get_match(&self, ast_node: &'a A) -> Option<T>;
    }
    pub trait YesMatch<'a, A, T> {
        fn get_match(&self, ast_node: &'a A) -> Option<T>;
    }
    impl<'a, A, T> NotMatch<'a, A, T> for &(PhantomData<T>, PhantomData<A>) {
        fn get_match(&self, _: &'a A) -> Option<T> {
            None
        }
    }
    impl<'a, A: Parse, T: MatchMutable<'a, A>> YesMatch<'a, A, T> for (PhantomData<T>, PhantomData<A>) {
        fn get_match(&self, ast_node: &'a A) -> Option<T> {
            T::try_match(ast_node)
        }
    }
}

pub(crate) struct TransformSnippets {
    pub(crate) m_id: TokenStream,
    pub(crate) muttest_api: TokenStream,
    pub(crate) loc: TokenStream,
}

#[cfg(procmacro2_semver_exempt)]
fn source_file_path(span: Span) -> Option<PathBuf> {
    Some(span.source_file().path())
}
#[cfg(not(procmacro2_semver_exempt))]
#[allow(unused_variables)]
fn source_file_path(span: Span) -> Option<PathBuf> {
    None
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn correct_spans_extreme() {
        #[muttest_codegen::mutate_isolated]
        fn f() {}
        let report = call_isolated! {f()}.report;
        assert_eq!(report.attrs.len(), 1);
        let attr = report.attrs.values().next().unwrap();
        assert!(attr.span.is_some());

        assert_eq!(report.mutables.len(), 1);
        let mutable = report.mutables.values().next().unwrap();

        assert_ne!(mutable.location.span, attr.span);

        assert_eq!(
            mutable.location.span.unwrap().start.line,
            attr.span.unwrap().start.line + 1
        );
    }

    #[test]
    fn correct_spans_ints() {
        #[muttest_codegen::mutate_isolated("lit_int")]
        fn f() -> isize {
            1 + 2 + 3
        }
        let report = call_isolated! {f()}.report;
        assert_eq!(report.attrs.len(), 1);
        let attr = report.attrs.values().next().unwrap();
        assert!(attr.span.is_some());

        assert_eq!(report.mutables.len(), 3);
        let first_col = report
            .mutables
            .values()
            .next()
            .unwrap()
            .location
            .span
            .unwrap()
            .start
            .column;

        for (id, m) in &report.mutables {
            assert_eq!(
                m.location.span.unwrap().start.line,
                attr.span.unwrap().start.line + 2
            );
            assert_eq!(
                m.location.span.unwrap().start.column,
                first_col + 4 * (id.id as u32 - 1)
            );
        }
    }
}
