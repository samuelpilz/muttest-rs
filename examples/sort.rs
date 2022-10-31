#[muttest::mutate]
fn bubblesort(arr: &mut [u8]) {
    let mut i = arr.len();
    while i != 0 {
        i -= 1;
        let mut j = 1;
        while j <= i {
            if arr[j - 1] > arr[j] {
                arr.swap(j - 1, j);
            }
            j += 1;
        }
    }
}

#[muttest::mutate]
fn stooge_sort(arr: &mut [u8]) {
    let len = arr.len();
    if len <= 1 {
        return;
    }
    if len == 2 {
        if arr[0] > arr[1] {
            arr.swap(0, 1);
        }
        return;
    }
    let t = len / 3;
    stooge_sort(&mut arr[..2 * t]);
    stooge_sort(&mut arr[t..]);
    stooge_sort(&mut arr[..2 * t]);
}

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
