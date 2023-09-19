use std::cmp::Ordering::*;

#[muttest::mutate]
fn triangle(x: u32, y: u32, z: u32) -> &'static str {
    match (x.cmp(&y), y.cmp(&z)) {
        // input validation
        (Greater, _) | (_, Greater) => "lengths not sorted",
        _ if x + y <= z => "illegal",
        // classification
        (Equal, Equal) => "equilateral",
        (Equal, _) | (_, Equal) => "isosceles",
        (Less, Less) => match (x * x + y * y).cmp(&(z * z)) {
            Less => "obtuse angled",
            Equal => "right angled",
            Greater => "acute angled",
        },
    }
}

#[cfg(test)]
#[muttest::tests]
mod tests {

    use super::*;

    #[test]
    pub fn illegal_triangle_1() {
        assert_eq!(triangle(1, 2, 3), "illegal");
    }

    #[test]
    pub fn illegal_triangle_2() {
        assert_eq!(triangle(3, 3, 7), "illegal");
    }

    #[test]
    pub fn lengths_not_sorted() {
        assert_eq!(triangle(4, 4, 3), "lengths not sorted");
    }

    #[test]
    pub fn right_triangle() {
        assert_eq!(triangle(3, 4, 5), "right angled");
    }

    #[test]
    pub fn acute_triangle() {
        assert_eq!(triangle(4, 5, 6), "acute angled");
    }

    #[test]
    pub fn obtuse_triangle() {
        assert_eq!(triangle(2, 4, 5), "obtuse angled");
    }

    #[test]
    pub fn isosceles_triangle_1() {
        assert_eq!(triangle(3, 4, 4), "isosceles");
    }

    #[test]
    pub fn isosceles_triangle_2() {
        assert_eq!(triangle(3, 3, 4), "isosceles");
    }

    #[test]
    pub fn equilateral_triangle() {
        assert_eq!(triangle(3, 3, 3), "equilateral");
    }
}
