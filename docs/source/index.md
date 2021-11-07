Documentation for the Juvix Language
======================================

Juvix synthesises a high-level frontend syntax, dependent-linearly-typed core language, and multi-backend
low-level execution model into a single unified stack for writing formally verifiable, efficiently executable
smart contracts and zero-knowledge circuits which can be deployed to a variety of distributed ledgers.

Juvix's compiler architecture is purpose-built from the ground up for the particular requirements and economic trade-offs
of the smart contract use case — the design prioritises behavioural verifiability, semantic precision, and output code efficiency over compilation speed,
syntactical familiarity, and backwards compatibility with existing systems.

Machine-assisted proof search, declarative deployment tooling, and type & usage inference
facilitate integration of low-developer-overhead property verification into the development process.

This document describes the structure of the Juvix language and the architecture of the Juvix compiler, and should be considered canonical.
Changes to the language or to the compiler must be accompanied by corresponding changes to this document.

```{toctree}
---
maxdepth: 1
---

getting-started/index
preliminaries/index
compiler/index
distributed-ledger-integration
future-work/index
haddock/index
```

