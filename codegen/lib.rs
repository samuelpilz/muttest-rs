use std::{
    env::VarError,
    fs,
    io::Write,
    ops::{ControlFlow, Deref, DerefMut},
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering::SeqCst},
        Mutex,
    },
};

use lazy_static::lazy_static;
use muttest_core::{
    context::ENV_VAR_MUTTEST_DIR,
    mutable::{
        assign_op, binop_bool, binop_calc, binop_cmp, binop_eq, extreme, lit_char, lit_int,
        lit_str, Mutable,
    },
    transformer::{
        MutablesConf, MuttestTransformer, TransformerConf, MUTABLE_DEFINITIONS_CSV_HEAD,
    },
    Error, PathSegment,
};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{
    fold::Fold, parse_macro_input, parse_quote, spanned::Spanned, BinOp, Expr, ExprAssignOp,
    ExprBinary, ExprLit, ExprRepeat, File, Item, ItemConst, ItemFn, ItemImpl, ItemStatic, Lit,
    LitStr, Pat, Type, Variant,
};

lazy_static! {
    static ref MUTTEST_DIR: Option<PathBuf> = {
        match std::env::var(ENV_VAR_MUTTEST_DIR) {
            Ok(d) => Some(PathBuf::from(d)),
            Err(VarError::NotPresent) => None,
            Err(e) => panic!("{}", e),
        }
    };
    static ref PKG_NAME: String = std::env::var("CARGO_PKG_NAME").expect("unable to get env var");
    static ref CRATE_NAME: String =
        std::env::var("CARGO_CRATE_NAME").expect("unable to get env var");
    static ref MUTABLE_DEFINITIONS_FILE: Mutex<Option<fs::File>> =
        Mutex::new(open_definitions_file().expect("unable to open definitions file"));
}
static ATTR_ID: AtomicUsize = AtomicUsize::new(1);

/// isolated mutation for unit testing of `muttest-core` itself.
#[proc_macro_attribute]
pub fn mutate_isolated(attr: TokenStream, input: TokenStream) -> TokenStream {
    static ISOLATED_MUTATION: AtomicUsize = AtomicUsize::new(1);

    // TODO: hide behind feature (unnecessary codegen)
    let input = parse_macro_input!(input as ItemFn);

    if std::env::var("CARGO_PKG_NAME").unwrap() != "muttest-core" {
        // TODO: compiler error instead of panic
        panic!("`mutate_isolated` should only be used for internal testing");
    }
    let is_lib_test = std::env::var("CARGO_CRATE_NAME").unwrap() == "muttest_core";

    let crate_name = format!("#{}", ISOLATED_MUTATION.fetch_add(1, SeqCst));
    let mut conf = TransformerConf {
        attr_id: 0,
        span: Span::call_site(),
        mutables: MutablesConf::All,
        muttest_api: if is_lib_test {
            quote!(crate::api)
        } else {
            quote!(::muttest_core::api)
        },
        pkg_name: "#isolated",
        crate_name: crate_name.to_owned(),
    };
    if !attr.is_empty() {
        let s = parse_macro_input!(attr as LitStr);
        conf.mutables = MutablesConf::One(s.value());
    }

    // perform transformation
    let mut transformer = MuttestTransformer::new(conf);
    let result = FoldImpl(&mut transformer).fold_item_fn(input);

    let mut definitions_csv = MUTABLE_DEFINITIONS_CSV_HEAD.to_owned();
    for l in transformer.definitions() {
        definitions_csv.push_str(l);
        definitions_csv.push('\n');
    }
    let num_mutables = transformer.num_mutables();

    // write context of transformation next to transformed function
    let mut mod_ident = result.sig.ident.clone();
    mod_ident.set_span(Span::call_site());
    quote! {
        #[allow(clippy::all)]
        #result
        mod #mod_ident {
            pub const MUTABLES_CSV: &str = #definitions_csv;
            pub const NUM_MUTABLES: usize = #num_mutables;
            pub const PKG_NAME: &str = "#isolated";
            pub const CRATE_NAME: &str = #crate_name;
        }
    }
    .into()
}

/// Transformer macro to be used to perform mutation testing on `muttest-core` itself.
///
/// This macro is not exported in `muttest` and is only intended for internal use.
#[proc_macro_attribute]
pub fn mutate_selftest(attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as File);

    let muttest_api = quote! {crate::api};
    let conf = TransformerConf {
        attr_id: ATTR_ID.fetch_add(1, SeqCst),
        span: Span::call_site(),
        mutables: MutablesConf::All,
        muttest_api: muttest_api.clone(),
        pkg_name: &PKG_NAME,
        crate_name: CRATE_NAME.clone(),
    };

    let mut transformer = MuttestTransformer::new(conf);
    let mut result = FoldImpl(&mut transformer).fold_file(input);

    if let Some(f) = &mut *MUTABLE_DEFINITIONS_FILE.lock().unwrap() {
        for l in transformer.definitions() {
            writeln!(f, "{l}").expect("unable to write to definitions file");
        }
    }

    // add code for early-return to mutated function
    if !attr.is_empty() {
        // TODO: this is not a good interface
        let default_expr = parse_macro_input!(attr as Expr);

        let item_fn = match &mut *result.items {
            [Item::Fn(i)] => i,
            _ => panic!("early-returns only applicable fns"),
        };

        let block = item_fn.block.clone();
        item_fn.block = parse_quote!(
            {
                crate::tests::return_early_if_nesting!(
                    m_id,
                    // TODO: use this macro's id for instead
                    #muttest_api::concat!(#muttest_api::file!(), ":", #muttest_api::line!(), ":", #muttest_api::column!()),
                    #default_expr
                );
                #block
            }
        );
    }
    quote! {
        #[allow(clippy::all)] // TODO: this does not work when using as inner macro
        #result
    }
    .into()
}

/// Macro to enable mutation testing.
#[proc_macro_attribute]
pub fn mutate(_attr: TokenStream, input: TokenStream) -> TokenStream {
    // TODO: maybe only transform if env-vars set
    // TODO: maybe honor `CARGO_PRIMARY_PACKAGE` env var
    let input = parse_macro_input!(input as File);

    let conf = TransformerConf {
        attr_id: ATTR_ID.fetch_add(1, SeqCst),
        span: Span::call_site(),
        mutables: MutablesConf::All,
        muttest_api: quote!(muttest),
        pkg_name: &PKG_NAME,
        crate_name: CRATE_NAME.clone(),
    };

    let mut transformer = MuttestTransformer::new(conf);
    let result = FoldImpl(&mut transformer).fold_file(input);

    if let Some(f) = &mut *MUTABLE_DEFINITIONS_FILE.lock().unwrap() {
        for l in transformer.definitions() {
            writeln!(f, "{l}").expect("unable to write to definitions file");
        }
    }

    result.into_token_stream().into()
}

fn open_definitions_file() -> Result<Option<fs::File>, Error> {
    match &*MUTTEST_DIR {
        Some(dir) => {
            let file_name = format!("mutable-definitions-{}:{}.csv", &*PKG_NAME, &*CRATE_NAME);
            let mut file = fs::File::create(PathBuf::from(dir).join(file_name))?;
            write!(&mut file, "{MUTABLE_DEFINITIONS_CSV_HEAD}")?;
            file.flush()?;
            file.sync_all()?;
            Ok(Some(file))
        }
        _ => Ok(None),
    }
}

struct FoldImpl<'a>(&'a mut MuttestTransformer);
impl Deref for FoldImpl<'_> {
    type Target = MuttestTransformer;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}
impl DerefMut for FoldImpl<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

/// trait to extract a mutable from AST nodes
trait MatchMutable<'m, T: syn::parse::Parse>: Sized + Mutable<'m> {
    fn try_match<'a: 'm>(ast_node: &'a T) -> Option<Self>;
}

impl<'a> MatchMutable<'a, Expr> for lit_int::Mutable<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Lit(ExprLit {
                lit: Lit::Int(l), ..
            }) => Some(Self {
                base10_digits: l.base10_digits(),
                span: l.span(),
                lit: l,
            }),
            _ => None,
        }
    }
}
impl<'a> MatchMutable<'a, Expr> for lit_char::Mutable<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Lit(ExprLit {
                lit: Lit::Char(l), ..
            }) => Some(Self {
                c: l.value(),
                span: l.span(),
                lit: l,
            }),
            _ => None,
        }
    }
}
impl<'a> MatchMutable<'a, Expr> for lit_str::Mutable<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Lit(ExprLit {
                lit: Lit::Str(l), ..
            }) => Some(Self {
                value: l.value(),
                span: l.span(),
                lit: l,
            }),
            _ => None,
        }
    }
}
impl<'a> MatchMutable<'a, Expr> for binop_eq::Mutable<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_eq_op(*op) => Some(Self {
                left,
                right,
                op,
                span: op.span(),
            }),
            _ => None,
        }
    }
}
impl<'a> MatchMutable<'a, Expr> for binop_cmp::Mutable<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_cmp_op(*op) => Some(Self {
                left: strip_expr_parens(left),
                right: strip_expr_parens(right),
                op,
                span: op.span(),
            }),
            _ => None,
        }
    }
}
impl<'a> MatchMutable<'a, Expr> for binop_calc::Mutable<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_calc_op(*op) => Some(Self {
                left: strip_expr_parens(left),
                right: strip_expr_parens(right),
                op,
                span: op.span(),
            }),
            _ => None,
        }
    }
}
impl<'a> MatchMutable<'a, Expr> for assign_op::Mutable<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::AssignOp(ExprAssignOp {
                left, op, right, ..
            }) => Some(Self {
                left: strip_expr_parens(left),
                right: strip_expr_parens(right),
                op,
                span: op.span(),
            }),
            _ => None,
        }
    }
}
impl<'a> MatchMutable<'a, Expr> for binop_bool::Mutable<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_bool_op(*op) => Some(Self {
                left: strip_expr_parens(left),
                right: strip_expr_parens(right),
                op,
                span: op.span(),
            }),
            _ => None,
        }
    }
}
impl<'a> MatchMutable<'a, ItemFn> for extreme::Mutable<'a> {
    fn try_match<'b: 'a>(item_fn: &'b ItemFn) -> Option<Self> {
        let ItemFn {
            vis, sig, block, ..
        } = item_fn;
        Some(Self {
            vis,
            sig,
            block,
            span: item_fn.span(), // TODO: smaller span instead?
        })
    }
}

impl FoldImpl<'_> {
    fn try_mutate<'a, 'b: 'a, T: syn::parse::Parse, M: MatchMutable<'a, T>>(
        &mut self,
        expr: &'b T,
    ) -> ControlFlow<T> {
        if !self.should_mutate(M::NAME) {
            return ControlFlow::Continue(());
        }
        if let Some(m) = M::try_match(expr) {
            return ControlFlow::Break(match syn::parse2(m.transform(self)) {
                Ok(m) => m,
                Err(e) => panic!("mutable `{}` transform syntax error: {}", M::NAME, e),
            });
        }
        ControlFlow::Continue(())
    }

    fn try_all_mutate_expr(&mut self, e: &Expr) -> ControlFlow<Expr> {
        self.try_mutate::<Expr, lit_char::Mutable>(e)?;
        self.try_mutate::<Expr, lit_int::Mutable>(e)?;
        self.try_mutate::<Expr, lit_str::Mutable>(e)?;
        self.try_mutate::<Expr, binop_eq::Mutable>(e)?;
        self.try_mutate::<Expr, binop_cmp::Mutable>(e)?;
        self.try_mutate::<Expr, binop_calc::Mutable>(e)?;
        self.try_mutate::<Expr, assign_op::Mutable>(e)?;
        self.try_mutate::<Expr, binop_bool::Mutable>(e)?;

        ControlFlow::Continue(())
    }

    fn try_all_mutate_item_fn(&mut self, item_fn: &ItemFn) -> ControlFlow<ItemFn> {
        self.try_mutate::<ItemFn, extreme::Mutable>(item_fn)?;

        ControlFlow::Continue(())
    }
}

impl Fold for FoldImpl<'_> {
    // TODO: inspect & preserve attrs
    // TODO: tests ...
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

        let f = match self.try_all_mutate_item_fn(&f) {
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

        match self.try_all_mutate_expr(&e) {
            ControlFlow::Break(e) => e,
            ControlFlow::Continue(_) => e,
        }
    }
}

fn strip_expr_parens(e: &Expr) -> &Expr {
    match e {
        // TODO: this loses attrs
        Expr::Paren(ep) => &ep.expr,
        _ => e,
    }
}

fn is_bool_op(op: BinOp) -> bool {
    matches!(op, BinOp::And(_) | BinOp::Or(_))
}

fn is_eq_op(op: BinOp) -> bool {
    matches!(op, BinOp::Eq(_) | BinOp::Ne(_))
}

fn is_cmp_op(op: BinOp) -> bool {
    matches!(
        op,
        BinOp::Lt(_) | BinOp::Le(_) | BinOp::Ge(_) | BinOp::Gt(_)
    )
}

fn is_calc_op(op: BinOp) -> bool {
    matches!(
        op,
        BinOp::Add(_)
            | BinOp::Sub(_)
            | BinOp::Mul(_)
            | BinOp::Div(_)
            | BinOp::Rem(_)
            | BinOp::BitOr(_)
            | BinOp::BitAnd(_)
            | BinOp::BitXor(_)
            | BinOp::Shl(_)
            | BinOp::Shr(_)
    )
}
