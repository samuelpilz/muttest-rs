use std::{collections::BTreeMap, io::Write, ops::ControlFlow};

use proc_macro2::{Group, Span, TokenStream, TokenTree};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    fold::Fold, parse::Parse, parse_quote, Arm, Attribute, Expr, ExprRepeat, ImplItemConst,
    ImplItemFn, Item, ItemConst, ItemFn, ItemImpl, ItemStatic, Meta, Pat, Type, Variant,
};

use crate::{
    api::CrateLocalMutableId,
    mutator::{self, MutatorFns},
    report::{CrateId, PathSegment},
    Error,
};

pub mod test_transformer;

pub const TEST_PREFIX: &str = "__muttest_test_";
pub const SPAN_TEST_PREFIX: &str = "__muttest_span_";

pub struct MuttestTransformer {
    pub conf: TransformerConf,
    pub mut_count: u32,
    pub definitions: BTreeMap<u32, (&'static str, String, Span, Option<String>)>,
    pub errors: Vec<Error>,
    pub path: Vec<PathSegment>,
    pub mutables_conf: MutablesConf,
}

pub struct TransformerConf {
    pub attr_id: u32,
    pub span: Span,
    pub muttest_api: TokenStream,
    pub crate_id: CrateId,
}

/// Possible settings for detecting mutables
pub enum MutablesConf {
    All,
    One(String),
    None,
}
// TODO: better name
pub struct Mutated {
    pub transformed: TokenStream,
    pub code: String,
    pub type_info: Option<String>,
    pub span: Span,
}

impl MuttestTransformer {
    pub fn new(conf: TransformerConf, mutables_conf: MutablesConf) -> Self {
        Self {
            conf,
            mut_count: 0,
            definitions: BTreeMap::new(),
            errors: vec![],
            path: vec![],
            mutables_conf,
        }
    }

    pub fn should_mutate(&self, name: &str) -> bool {
        match &self.mutables_conf {
            MutablesConf::One(m) => m == name,
            MutablesConf::All => true,
            MutablesConf::None => false,
        }
    }

    pub(crate) fn new_mutable(&mut self, span: Span) -> TransformSnippets {
        self.mut_count += 1;
        self.mutable_id_code(self.mut_count, span)
    }

    fn mutable_id_code(&self, id: u32, span: Span) -> TransformSnippets {
        let muttest_api = self.muttest_api(span);
        let pkg_name = &self.conf.crate_id.pkg_name;
        let crate_name = &self.conf.crate_id.crate_name;
        let attr_id = self.conf.attr_id;
        let m_id = quote_spanned! {span=>
            #muttest_api::MutableId {
                crate_id: #muttest_api::CrateId {
                    pkg_name: #muttest_api::Cow::Borrowed(#pkg_name),
                    crate_name: #muttest_api::Cow::Borrowed(#crate_name),
                },
                id: #muttest_api::CrateLocalMutableId {
                    attr_id: #attr_id,
                    id: #id,
                },
            }
        };
        TransformSnippets { m_id, muttest_api }
    }

    pub fn muttest_api(&self, span: Span) -> TokenStream {
        fn relocate_token_stream(input: TokenStream, span: Span) -> TokenStream {
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

    pub fn make_span_test(&self) -> Item {
        let test_ident = format_ident!("{}{}", SPAN_TEST_PREFIX, self.conf.attr_id);
        if self.definitions.is_empty() {
            return parse_quote! {
                // TODO: emit warning
                #[test]
                fn #test_ident() {}
            };
        }

        let mut spans = vec![];
        for (&id, &(_, _, span, _)) in &self.definitions {
            let TransformSnippets { m_id, muttest_api } = self.mutable_id_code(id, span);
            let span_code = span_code(&muttest_api, span);
            spans.push(quote_spanned!(span=>
                #muttest_api::write_mutable_location(#m_id, #span_code);
            ))
        }

        let muttest_api = &self.conf.muttest_api;
        let attr_span = span_code(muttest_api, self.conf.span);
        let pkg_name = &self.conf.crate_id.pkg_name;
        let crate_name = &self.conf.crate_id.crate_name;
        let attr_id = self.conf.attr_id;
        let crate_id = quote! {
            #muttest_api::CrateId {
                pkg_name: #muttest_api::Cow::Borrowed(#pkg_name),
                crate_name: #muttest_api::Cow::Borrowed(#crate_name),
            }
        };

        parse_quote! {
            #[test]
            fn #test_ident() {
                #muttest_api::write_attr_location(
                    #crate_id,
                    #attr_id,
                    #muttest_api::file!(),
                    #attr_span,
                );
                #(#spans)*
            }
        }
    }

    pub fn write_csv<W: Write>(self, mut writer: csv::Writer<W>) -> Result<(), csv::Error> {
        let attr_id = self.conf.attr_id;
        for (id, (kind, code, _, type_info)) in self.definitions {
            writer.write_record([
                &*CrateLocalMutableId { attr_id, id }.to_string(),
                kind,
                &code,
                type_info.as_deref().unwrap_or_default(),
            ])?;
        }
        Ok(())
    }
}

fn span_code(muttest_api: &TokenStream, span: Span) -> TokenStream {
    quote_spanned!(span=>
        #muttest_api::Span {
            start: #muttest_api::LineColumn {line: #muttest_api::line!(), column: #muttest_api::column!() },
            end: #muttest_api::Option::None,
        }
    )
}
impl Fold for MuttestTransformer {
    fn fold_item_const(&mut self, i: ItemConst) -> ItemConst {
        i
    }
    fn fold_impl_item_const(&mut self, i: ImplItemConst) -> ImplItemConst {
        i
    }
    fn fold_item_static(&mut self, i: ItemStatic) -> ItemStatic {
        i
    }
    fn fold_pat(&mut self, pat: Pat) -> Pat {
        pat
    }
    fn fold_expr_repeat(&mut self, mut expr_repeat: ExprRepeat) -> ExprRepeat {
        // only mutate expression, not len
        expr_repeat.expr = Box::new(self.fold_expr(*expr_repeat.expr));
        expr_repeat
    }
    fn fold_type(&mut self, node: Type) -> Type {
        node
    }
    fn fold_variant(&mut self, v: Variant) -> Variant {
        v
    }
    fn fold_attribute(&mut self, i: Attribute) -> Attribute {
        i
    }

    fn fold_item_mod(&mut self, mut i: syn::ItemMod) -> syn::ItemMod {
        match extract_muttest_attrs(&mut i.attrs) {
            Ok(a)
                if a.iter().any(|a| {
                    a.path().get_ident().map(ToString::to_string).as_deref() == Some("ignore")
                }) =>
            {
                return i;
            }
            Err(e) => {
                self.errors.push(e);
                return i;
            }
            _ => {}
        }

        syn::fold::fold_item_mod(self, i)
    }

    fn fold_item_fn(&mut self, mut f: ItemFn) -> ItemFn {
        if f.sig.constness.is_some() || has_attr(&f.attrs, "test") {
            return f;
        }
        match extract_muttest_attrs(&mut f.attrs) {
            Ok(a)
                if a.iter().any(|a| {
                    a.path().get_ident().map(ToString::to_string).as_deref() == Some("ignore")
                }) =>
            {
                return f;
            }
            Err(e) => {
                self.errors.push(e);
                return f;
            }
            _ => {}
        }

        self.path.push(PathSegment::Fn(f.sig.ident.to_string()));

        let f = syn::fold::fold_item_fn(self, f);
        let f = self.mutate_all_with(<dyn MutatorFns>::transform_item_fn, f);
        self.path.pop();

        f
    }

    fn fold_impl_item_fn(&mut self, mut f: ImplItemFn) -> ImplItemFn {
        if f.sig.constness.is_some() {
            return f;
        }
        match extract_muttest_attrs(&mut f.attrs) {
            Ok(a)
                if a.iter().any(|a| {
                    a.path().get_ident().map(ToString::to_string).as_deref() == Some("ignore")
                }) =>
            {
                return f;
            }
            Err(e) => {
                self.errors.push(e);
                return f;
            }
            _ => {}
        }

        self.path.push(PathSegment::Fn(f.sig.ident.to_string()));
        let f = syn::fold::fold_impl_item_fn(self, f);
        let f = self.mutate_all_with(<dyn MutatorFns>::transform_impl_item_fn, f);
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

        self.mutate_all_with(<dyn MutatorFns>::transform_expr, e)
    }

    fn fold_arm(&mut self, a: Arm) -> Arm {
        let a = syn::fold::fold_arm(self, a);

        self.mutate_all_with(<dyn MutatorFns>::transform_arm, a)
    }
}

impl MuttestTransformer {
    fn mutate_all_with<T: Parse>(
        &mut self,
        f: fn(&(dyn MutatorFns + 'static), &mut Self, &T) -> ControlFlow<Mutated>,
        t: T,
    ) -> T {
        for (name, m) in mutator::MUTATORS {
            if self.should_mutate(name) {
                if let ControlFlow::Break(mutated) = f(*m, self, &t) {
                    match syn::parse2(mutated.transformed) {
                        Ok(t) => {
                            self.definitions.insert(
                                self.mut_count,
                                (name, mutated.code, mutated.span, mutated.type_info),
                            );
                            return t;
                        }
                        Err(e) => {
                            self.errors.push(Error::MutatorTransform(name, e));
                            return t;
                        }
                    }
                }
            }
        }
        t
    }
}

pub fn strip_expr_parens(e: &Expr) -> &Expr {
    match e {
        Expr::Paren(ep) if ep.attrs.is_empty() => strip_expr_parens(&ep.expr),
        _ => e,
    }
}

pub(crate) struct TransformSnippets {
    pub(crate) m_id: TokenStream,
    pub(crate) muttest_api: TokenStream,
}

fn has_attr(a: &[Attribute], name: &str) -> bool {
    has_attr_with(a, |p| {
        p.get_ident().map(ToString::to_string).as_deref() == Some(name)
    })
}

fn has_attr_with<F: FnMut(&syn::Path) -> bool>(a: &[Attribute], mut predicate: F) -> bool {
    a.iter().any(|a| predicate(a.path()))
}
fn extract_muttest_attrs(attrs: &mut Vec<Attribute>) -> Result<Vec<Meta>, Error> {
    // TODO: use extract_mapped when stable
    let mut mutate_attrs = vec![];
    let mut i = 0;
    while i < attrs.len() {
        let a = &attrs[i];
        i += 1;

        // checks to detect #[cfg_attr(muttest, ...)] attributes
        let Meta::List(l) = &a.meta else { continue };
        if l.path.get_ident().map(ToString::to_string).as_deref() != Some("cfg_attr") {
            continue;
        }
        let mut tokens = l.tokens.clone().into_iter().collect::<Vec<_>>();
        if tokens.len() != 3
            || !matches!(&tokens[0], TokenTree::Ident(i) if &i.to_string() == "muttest")
            || !matches!(&tokens[1], TokenTree::Punct(p) if p.as_char() == ',')
        {
            continue;
        }

        mutate_attrs.push(syn::parse2(tokens.remove(2).into())?);
        i -= 1;
        attrs.remove(i);
    }
    Ok(mutate_attrs)
}

#[cfg(test)]
#[cfg_attr(feature = "selftest", muttest::tests)]
mod tests {
    use crate::tests::*;

    // TODO: tests for spans

    #[test]
    fn const_items_skipped() {
        #[muttest_codegen::mutate_isolated("lit_int")]
        #[allow(unused)]
        mod x {
            const Y1: usize = 1;
            struct X;
            impl X {
                const Y2: usize = 1;
                const fn x() -> usize {
                    1
                }
            }
        }
        let data = data_isolated!(x);
        assert!(data.mutables.is_empty());
    }

    #[test]
    fn test_functions_skipped() {
        #[muttest_codegen::mutate_isolated]
        #[allow(unused, unnameable_test_items)]
        mod x {
            #[test]
            fn x() {}
        }
        let data = data_isolated!(x);
        assert!(data.mutables.is_empty());
    }
}
