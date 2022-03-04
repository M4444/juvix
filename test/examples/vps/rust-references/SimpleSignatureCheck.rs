#[cfg(feature = "vp_check_sig")]
pub mod main {
    use anoma_vm_env::vp_prelude::*;

    #[validity_predicate]
    fn validate_tx(
        tx_data: Vec<u8>,
        _owner: Address,
        _keys_changed: HashSet<storage::Key>,
        _verifiers: HashSet<Address>,
    ) -> bool {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let addr = Address::try_from_slice(&signed.data.unwrap()[..]).unwrap();
        let pk = read(key::ed25519::pk_key(&addr).to_string()).unwrap();
        key::ed25519::verify_tx_sig(&pk, &tx, &signed.sig).is_ok()
    }
}
