pub fn multi(left: u64, right: u64) -> u64 {
    left * right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_multi() {
        assert_eq!(4, multi(2, 2));
    }
}
