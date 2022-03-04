/// A token validity predicate.
pub fn vp(
    token: &Address,
    keys_changed: &HashSet<Key>,
    verifiers: &HashSet<Address>,
) -> bool {
    let mut change: Change = 0;
    let all_checked = keys_changed.iter().all(|key| {
        match token::is_balance_key(token, key) {
            None => {
                // deny any other keys
                false
            }
            Some(owner) => {
                // accumulate the change
                let key = key.to_string();
                let pre: Amount = vp::read_pre(&key).unwrap_or_default();
                let post: Amount = vp::read_post(&key).unwrap_or_default();
                let this_change = post.change() - pre.change();
                change += this_change;
                // make sure that the spender approved the transaction
                if this_change < 0 {
                    return verifiers.contains(owner);
                }
                true
            }
        }
    });
    all_checked && change == 0
}