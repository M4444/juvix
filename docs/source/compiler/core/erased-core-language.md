# Erased Core Language

The erased core captures the operational semantics of the core language, erasing contemplated (zero-usage) terms and dropping type & usage annotations which are not computationally necessary. Core terms can be erased directly or first passed through the elementary affine stratification checker in order to test compatibility with the abstract algorithm.

Analogously to core, the erased core language is parameterised over a set $C$ of primitive constants (including functions) and a set $→_{f}$ of associated reduction rules.

## Syntax

Let $u, v, w$ be terms.

- TODO: add the other syntax

## Erased core syntax
$$
\newcommand\OR{\mkern17mu | \mkern12mu}
\begin{aligned}
u, v, w &::= x & \text{variable} \\
&\OR c \in C & \text{primitive constant} \\
&\OR λx.u & \text{abstraction} \\
&\OR u\ v & \text{application} \\
\end{aligned}
$$

## Erasure from core

Define the core erasure operator $▶$.

Erasure judgements take the form $Γ ⊢ t \overset{σ}{:} S \ ▶\  u$ with $t \overset{σ}{:} S$ a core judgement and $u$ an erased core term.

Computationally relevant terms are preserved, while terms which are only contemplated are erased.

```{note} 
$σ \ne 0$ must hold, as the erasure of a computationally irrelevant term is nothing.
```

### Primitives & lambda terms

$$
\begin{prooftree}
\AxiomC{$c \overset{σ}{:} S$}
\AxiomC{$σ \ne 0$}
\RightLabel{Prim-Const-Erase-+}
\BinaryInfC{$c \overset{σ}{:} S \ ▶\  c$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$⊢ 0Γ,x \overset{σ}{:} S, 0Γ′$}
\AxiomC{$σ \ne 0$}
\RightLabel{Var-Erase-+}
\BinaryInfC{$0Γ,x \overset{σ}{:} S, 0Γ′ ⊢ x \overset{σ}{:} S \ ▶\  x$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$t \overset{σ}{:} T \ ▶\  u$}
\AxiomC{$σπ = 0$}
\RightLabel{Lam-Erase-0}
\BinaryInfC{$λx.t : (x \overset{π}{:} S) → T \ ▶\  u$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$t \overset{σ}{:} T \ ▶\  u$}
\AxiomC{$σπ \ne 0$}
\RightLabel{Lam-Erase-+}
\BinaryInfC{$λx.t : (x \overset{π}{:} S) → T \ ▶\  λx.u$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$Γ_1 ⊢ M \overset{σ}{:} (x \overset{π}{:} S) → T \ ▶\  u$}
\AxiomC{$Γ_2 ⊢ N \overset{0}{:} S$}
\AxiomC{$σπ = 0$}
\RightLabel{App-Erase-0}
\TrinaryInfC{$Γ_1 ⊢ M N \overset{σ}{:} T[x := N] \ ▶\  u$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$Γ_1 ⊢ M \overset{σ}{:} (x \overset{π}{:} S) → T \ ▶\  u$}
\AxiomC{$Γ_2 ⊢ N \overset{σπ}{:} S \ ▶\  v$}
\AxiomC{$σπ \ne 0$}
\RightLabel{App-Erase-+}
\TrinaryInfC{$Γ_1 + Γ_2 ⊢ M N \overset{σ}{:} T[x := N] \ ▶\  u\ v$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ s \overset{π}{:} S$}
\AxiomC{$s \ ▶\ u$}
\AxiomC{$π \ne 0$}
\RightLabel{Ann-Erase-+}
\TrinaryInfC{$Γ ⊢ s \overset{π}{:} S \ ▶\ u$}
\end{prooftree}
$$

In the *Lam-Erase-0* rule, the variable $x$ bound in $t$ will not occur in the corresponding $u$, since it is bound with usage $0$, with which it will remain regardless of how the context splits, so the rule *Var-Erase-+* cannot consume it.


## Reduction semantics

As core, sans the types.
