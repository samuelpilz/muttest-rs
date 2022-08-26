
        #[muttest_codegen::mutate_isolated]
        fn empty_str() -> &'static str {
            ""
        }
        #[muttest_codegen::mutate_isolated]
        fn some_str() -> &'static str {
            "mutation testing!"
        }

        #[test]
        fn empty_str_mutables() {
            assert_eq!(empty_str::NUM_MUTABLES, 1);
            assert_eq!(empty_str::MUTABLES_CSV.lines().count(), 2);
        }

        #[test]
        fn empty_str_unchanged() {
            assert_eq!(super::without_mutation(empty_str), "");
        }

        #[test]
        fn empty_str_one() {
            assert_eq!(super::with_mutation(1, "1", empty_str), "1");
        }

        #[test]
        fn some_str_mutables() {
            assert_eq!(some_str::NUM_MUTABLES, 1);
            assert_eq!(some_str::MUTABLES_CSV.lines().count(), 2);
        }

        #[test]
        fn some_str_unchanged() {
            assert_eq!(super::without_mutation(some_str), "mutation testing!");
        }

        #[test]
        fn some_str_empty() {
            assert_eq!(super::with_mutation(1, "", some_str), "");
        }