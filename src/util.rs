pub(crate) trait StrExt {
    fn to_uppercase_first(&self) -> String;
}

impl StrExt for str {
    fn to_uppercase_first(&self) -> String {
        let mut chars = self.chars();
        if let Some(first) = chars.next() {
            format!("{}{}", first.to_uppercase(), chars.collect::<String>())
        } else {
            String::default()
        }
    }
}

/// Fully deduplicates a vector, without `dedup`'s limit of adjacent entries.
///
/// For each set of equal values in the array, this function removes all but the leftmost.
pub fn full_dedup<T: PartialEq>(mut v: Vec<T>) -> Vec<T> {
    for i in (0..v.len()).rev() {
        if v[..i].into_iter().any(|other| &v[i] == other) {
            v.remove(i);
        }
    }
    v
}
