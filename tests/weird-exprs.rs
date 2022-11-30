#![allow(unreachable_code, dead_code, unused_parens, clippy::all)]
//! from: https://github.com/rust-lang/rust/blob/master/src/test/ui/weird-exprs.rs

// #[muttest_codegen::mutate_isolated]
fn weird() {
    use std::cell::Cell;

    fn strange() -> bool {
        let _x: bool = return true;
    }

    fn funny() {
        fn f(_x: ()) {}
        f(return);
    }

    fn what() {
        fn the(x: &Cell<bool>) {
            return while !x.get() {
                x.set(true);
            };
        }
        let i = &Cell::new(false);
        let dont = { || the(i) };
        dont();
        assert!((i.get()));
    }

    fn zombiejesus() {
        loop {
            while (return) {
                if (return) {
                    match (return) {
                        1 => {
                            if (return) {
                                return;
                            } else {
                                return;
                            }
                        }
                        _ => return,
                    };
                } else if (return) {
                    return;
                }
            }
            if (return) {
                break;
            }
        }
    }

    fn notsure() {
        let mut _x: isize;
        let mut _y = (_x = 0) == (_x = 0);
        let mut _z = (_x = 0) < (_x = 0);
        let _a = (_x += 0) == (_x = 0);
        let _b = std::mem::swap(&mut _y, &mut _z) == std::mem::swap(&mut _y, &mut _z);
    }

    fn canttouchthis() -> usize {
        fn p() -> bool {
            true
        }
        let _a = (assert!((true)) == (assert!(p())));
        let _c = (assert!((p())) == ());
        let _b: bool = (println!("{}", 0) == (return 0));
    }

    fn angrydome() {
        loop {
            if break {}
        }
        let mut i = 0;
        loop {
            i += 1;
            if i == 1 {
                match (continue) {
                    1 => {}
                    _ => panic!("wat"),
                }
            }
            break;
        }
    }

    fn evil_lincoln() {
        let _evil = println!("lincoln");
    }
}
