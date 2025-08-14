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
        assert_eq!(0, sub(1, 1));
    }
}
