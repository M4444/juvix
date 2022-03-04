#[cfg(feature = "vp_check_multi_sig")]
pub mod main {
    use anoma_vm_env::vp_prelude::*;

    #[validity_predicate]
    fn validate_tx(
        tx_data: Vec<u8>,
        addr: Address,
        _keys_changed: HashSet<storage::Key>,
        _verifiers: HashSet<Address>,
    ) -> bool {
        let signed =
            key::ed25519::MultiSignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let tx = Tx::try_from_slice(&signed.data.unwrap()[..]).unwrap();
        let pks = signed
            .signers
            .iter()
            .map(|addr| read(key::ed25519::pk_key(&addr).to_string()).unwrap())
            .collect::<Vec<key::ed255199::PublicKey>>();
        key::ed25519::verify_tx_multi_sig(&pks, &tx, &signed.sig).is_ok()
    }
}