# Distributed Ledger Integration

## Incremental compilation

It might be prudent to consider an "incremental compilation" model where new code is compiled into an existing runtime (e.g. a set of smart contracts, which could be encoded as functions being accepted as input provided by a user at runtime). Of course this is possible with a model like Ethereum's, where code is stored per-contract and loaded only when the contract is run, but that renders cross-contract optimisation quite difficult (I don't think any Ethereum clients do it) and would potentially require expensive data format conversions for Juvix terms.

In principle it should be possible to alter an existing interaction net representation (e.g. in memory) by simply compiling the new term to an interaction net of its own, which then is connected to the existing term at the appropriate location. This will require, however, a fixed set of choices for ADT encoding, certain optimisation passes, etc. unless we track metadata and can elect to rewrite other parts of the net if a different choice makes sense.

## Compiler & typechecker metering

For future integration with a distributed ledger, it will be helpful to have the ability to run the typechecker on arbitrary user-input terms, which we must meter in order to do safely. Thus it will be required to compile the typechecker (or all of the Juvix library, ideally) to a target VM which can be metered at runtime (so gas can be charged for typechecking terms). This doesn't have to be particularly efficient since terms only need to be typechecked once.

I think the ideal target is WASM, which can be metered in a VM (e.g. Life) and as a bonus runs in the browser. Asterius is a Haskell-to-WASM compiler which appears to be relatively functional & actively developed. We should investigate whether that will work for the Juvix typechecker (both the core typechecker & the EAL typechecker) - or ideally even for the whole compiler (including EAL inference, interaction net interpreter, etc.) - though the latter isn't as critical.

I am not sure whether it is necessary to also meter other semantics-preserving parts of the compiler (e.g. optimisation passes) - it might be possible for nodes to run these individually (and they might be on different platforms), but that could be problematic if particular passes could be DoS attack vectors or if cost semantics (of terms - #45) are dependent on optimisation passes (in which case they would need to be specified & checked in consensus).

## Persistent interaction system state

1. Defined equivalence semantics but implementation can change later
1. Contracts themselves can call the compiler (needs more R&D)
1. Bounties for proofs, sub-contract-upgrades, etc.
1. More efficient than read-back after execution, just persist the graph of the state machine, many more optimisations automatically happen.
1. Will be helpful for Juvix to be self-hosting or packaged as a runtime which can typecheck untrusted input.

## Architectural optimisations

### Fusing across sequentially applied transactions

Consider a sequence of transactions applied in order which read & write various state values. The transactions reads & writes can be fused prior to actually touching state, e.g. if two transactions increment the balance of a destination account the increment amounts can be added prior to applying a single addition operation to state. This should work even if there are dependencies on state reads - it may result in unnecessary evaluation, but the cost of reading & writing Merkleized state is likely to be relatively quite expensive.

This fusion can be performed over any logical block of ordered, subsequent transactions for which computation of the exact following state (e.g. resulting in a Merkle tree root) can be delayed - at minimum, this will be a block, and could be several if finality is delayed or pipelined anyways.
