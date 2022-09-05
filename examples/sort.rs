#[muttest::mutate]
fn bubblesort(arr: &mut [u8]) {
    let mut i = arr.len();
    while i != 0 {
        i -= 1;
        let mut j = 1;
        while j <= i {
            println!("test {} {}", j - 1, j);
            if arr[j - 1] > arr[j] {
                println!("swap {} {}", j - 1, j);
                arr.swap(j - 1, j);
            }
            j += 1;
        }
    }
}

#[muttest::mutate]
fn stooge_sort(arr: &mut [u8]) {
    if arr.len() <= 1 {
        return;
    }
    let last = arr.len() - 1;
    if arr[0] > arr[last] {
        arr.swap(0, last);
    }
    if last <= 1 {
        return;
    }
    let t = (last + 1) / 3;
    stooge_sort(&mut arr[..2 * t]);
    stooge_sort(&mut arr[t..]);
    stooge_sort(&mut arr[..2 * t]);
}

// TODO: apply test suite

macro_rules! sort_tests {
    ($sort_fn:ident) => {
        mod $sort_fn {
            #[test]
            pub fn empty() {
                let mut arr = [];
                super::$sort_fn(&mut arr);
                assert_eq!(arr, [])
            }

            #[test]
            pub fn rev() {
                let mut arr = [3, 2, 1];
                super::$sort_fn(&mut arr);
                assert_eq!(arr, [1, 2, 3])
            }
        }
    };
}

sort_tests!(bubblesort);
sort_tests!(stooge_sort);
