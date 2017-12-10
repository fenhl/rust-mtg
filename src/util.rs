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
