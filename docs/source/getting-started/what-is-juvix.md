# What is Juvix?

Juvix is a dependently functional programming language for writing efficient formally-verified smart contracts, which can be deployed to various distributed ledgers. Juvix addresses many issues that we have experienced while trying to write and deploy decentralised applications present in the ecosystem of smart-contracts:

- the difficulty of adequate program verification,
- the ceiling of compositional complexity,
- the illegibility of execution costs, and
- the lock-in to particular backends.

The Juvix compiler synthesises a high-level frontend syntax with support for dependent-linearly types and several other cutting-edge research ideas from programming language design and type theory.

## Motivation

Selected out by the twin Darwinian reapers of language network-effect path-dependence and latency-over-correctness content delivery incentives, secure, legible, large-scale, long-running digital systems are a virtually nonexistent breed. Cutting corners on security reduces costs up front, but the total costs of insecure systems are higher, borne later and unevenly — often by individuals who end up rendered vulnerable instead of empowered by poorly engineered technology. Although the underlying cryptographic primitives can in complexity-theoretic principle provide a high degree of individual protection, the imprecisions and inaccuracies in complex networked multi-party protocols have caused the economics of privacy & legibility to devolve into base power asymmetries, where even individuals possessed of relevant domain expertise stand little chance of retaining their privacy & autonomy against a nation state, surveillance capitalist firm, terrorist group, or moderately skilled bounty hunter, and mainstream users stand none at all.

This sorry result is overdetermined & difficult to precisely allocate responsibility for, but certainly a substantial contributor is the sheer difficulty and cost of formally verifying complex software systems. Most frequently, security is trial-and-error. At best, models are written and checked separately from the code, an expensive and error-prone process. An approach to security which can be expected to result in a different outcome must be constructive, unifying code & proofs into a single language, and must be compositional, so that sets of proofs can be imported & reused along with the libraries which comprise most of modern software. The approach must provide a typed language for succinct proofs which can tightly constrain the opaque behaviour of complex backend codebases, typecheckers which can be embedded by the manufacturers of user-facing software, such as web browsers, operating systems, or cybercoin wallets. The approach must reduce the costs of formally verifying software, and increase the legible user-facing benefits of doing so, to a degree where formal verification is the economically rational decision for networked software which handles sensitive data or the exchange of value.

Smart contracts running on distributed ledgers are an archetypal example of a security-critical application, and one where the popular conceit of security-through-obscurity holds little sway, yet results so far have not been promising {footcite}`parity-wallet-postmortem` {footcite}`zero-ex-postmortem`. Luckily, the field has not yet been locked into particular technologies whose network effects could not be overcome, and the necessity of verifiable & verified software systems is widely-recognised. A radically different language is necessary: one that treats verifiability as a design problem, not a feature to be tacked on later, one that provides succinct, expressive, and composable proofs which can tightly constraint the behaviour of complex logic, and one that reduces the cost of verification to the point where not doing so for security-critical software will be considered simply irresponsible. Juvix aims to realise this ideal.


```{footbibliography}
```
