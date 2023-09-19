#[muttest::mutate]
fn insertion_sort(arr: &mut [u8]) {
    for i in 1..arr.len() {
        let x = arr[i];
        let mut j = i;
        while j > 0 && arr[j - 1 as usize] > x {
            arr[j] = arr[j - 1];
            j -= 1;
        }
        arr[j] = x;
    }
}

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
    stooge_sort(&mut arr[..len - t]);
    stooge_sort(&mut arr[t..]);
    stooge_sort(&mut arr[..len - t]);
}

#[muttest::mutate]
fn merge_sort(arr: &mut [u8]) {
    let len = arr.len();
    if len <= 1 {
        return;
    }
    let m = arr.len() / 2;
    let (a1, a2) = arr.split_at_mut(m);
    merge_sort(a1);
    merge_sort(a2);
    let a1 = a1.to_vec();
    let a2 = a2.to_vec();

    let mut i = 0;
    let mut j = 0;
    let mut k = 0;
    while j < a1.len() || k < a2.len() {
        if k == a2.len() || (j < a1.len() && a1[j] < a2[k]) {
            arr[i] = a1[j];
            j += 1;
        } else {
            arr[i] = a2[k];
            k += 1;
        }
        i += 1;
    }
}

macro_rules! sort_tests {
    ($sort_fn:ident) => {
        #[muttest::tests]
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

            #[test]
            pub fn arr5() {
                let mut arr = [5, 3, 1, 2, 4];
                super::$sort_fn(&mut arr);
                assert_eq!(arr, [1, 2, 3, 4, 5])
            }
        }
    };
}

sort_tests!(insertion_sort);
sort_tests!(bubblesort);
sort_tests!(stooge_sort);
sort_tests!(merge_sort);
