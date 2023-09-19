#[muttest::mutate]
pub fn find(arr: &mut [u8], f: isize) {
    if arr.is_empty() {
        return;
    }
    let mut m = 0isize;
    let mut n = arr.len() as isize - 1;
    while m < n {
        let r = arr[f as usize];
        let mut i = m;
        let mut j = n;

        while i <= j {
            while arr[i as usize] < r {
                i += 1;
            }
            while r < arr[j as usize] {
                j -= 1;
            }
            if i <= j {
                arr.swap(i as usize, j as usize);
                i += 1;
                j -= 1;
            }
        }
        if f > j {
            if i > f {
                break;
            }
            m = i;
        } else {
            n = j;
        }
    }
}

#[cfg(test)]
#[muttest::tests]
mod tests {

    use super::*;

    fn testcase(arr: &mut [u8], f: isize) {
        find(arr, f);

        let f = f as usize;
        for i in 0..f {
            assert!(arr[i] <= arr[f]);
        }
        for i in f + 1..arr.len() {
            assert!(arr[f] <= arr[i]);
        }
    }

    #[test]
    fn test1() {
        testcase(&mut [3, 2, 5, 1], 2);
    }

    #[test]
    fn test2() {
        testcase(&mut [3, 3, 3, 5, 5], 2);
    }

    // #[test]
    // fn all_permutations() {
    //     use itertools::Itertools;

    //     let arr = [1, 2, 3, 4, 5];
    //     for arr in arr.iter().permutations(arr.len()) {
    //         for f in 0..arr.len() as isize {
    //             let mut arr = arr.iter().map(|&&x| x).collect::<Vec<_>>();
    //             testcase(&mut arr, f);
    //         }
    //     }
    // }
}
