#[muttest::mutate]
fn triangle(x: u32, y: u32, z: u32) -> &'static str {
    if x > y || y > z {
        return "lengths not sorted";
    }
    if x + y <= z {
        return "illegal";
    }
    if x == y || y == z {
        return if x == z { "equilateral" } else { "isosceles" };
    }
    let x2y2 = x * x + y * y;
    let z2 = z * z;
    if x2y2 == z2 {
        return "right angled";
    }
    if x2y2 < z2 {
        return "obtuse angled";
    }
    return "acute angled";
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
