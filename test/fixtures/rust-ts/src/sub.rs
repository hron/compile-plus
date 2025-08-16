pub fn sub(a: usize, b: usize) -> usize {
    a - b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sub_foo() {
        assert_eq!(4, sub(6, 2));
    }

    #[test]
    fn test_sub_bar() {
        let a = 4;
        let b = 2;
        assert_eq!(0, sub(1, 1));
        assert_eq!(2, sub(4, 2));
    }
}
