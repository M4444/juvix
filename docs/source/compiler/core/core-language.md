# Core Language

## Basics

Juvix Core is the core language in the Juvix compiler stack, defining the canonical syntax & semantics on which all compilers & interpreters must agree. Lower-level evaluation choices may safely differ between implementations as long as they respect the core semantics.

Juvix Core is primarily inspired by Quantitative Type Theory {footcite}`quantitative-type-theory`, Formality {footcite}`formality`, and Cedille {footcite}`the-calculus-of-dependent-lambda-eliminations`. It fuses full-spectrum dependent types (types & terms can depend on types & terms) with linear logic using the contemplation — computation distinction introduced by QTT, adds the self-types of Cedille & Formality to enable derivation of induction for pure lambda terms, introduces the full connective set of linear logic, dependent where appropriate, to express different varieties of conjunction & disjunction, and defines an extension system for opaque user-defined datatypes & primitives (such as integers with addition & multiplication, bytestrings with concatenation & indexing, or cryptographic keys with construction & signature checking).

:::{note}
At present, the substructural typing in the core language is not required for optimal reduction — separate elementary affine logic assignments are inferred in the lower-level stage. Substructural typing is used in Juvix Core to provide additional precision to the programmer and enable optimisations in the bespoke compilation path to custom rewrite rules (such as avoiding garbage collection). In the future a closer fusion allowing the more precise usage information to inform interaction net construction is expected; this is an open research question.
:::

## Changes from QTT

### Primitive types, constants, and functions

Nothing too exciting here. These will vary based on the machine target.

### Usage polymorphism

:::{warning}
Usage polymorphism has not yet been integrated into the rest of this document,
much less the compiler.
:::

In Juvix, usages are simply terms of a builtin type `usage`, which can be
quantified over like any other.

An example, with Church-encoded naturals:

```
one :: 1 (1 (a -> a) -> 1 a -> a)

two :: 1 (2 (a -> a) -> 1 a -> a)

three :: 1 (3 (a -> a) -> 1 a -> a)
```

We can type the successor function as:

```
succ :: 1 (1 (n (a -> a) -> 1 a -> a) -> ((n + 1) (a -> a) -> 1 a -> a)
```

where `n` is a variable of type `usage`.

### Record types

TODO

### Let-definitions

Not strictly necessary for expressivity, but local definitions are maintained through the core so that they can appear in the same place in the generated code. TODO

## Preliminaries

$$
\newcommand\OR{\mkern17mu | \mkern12mu}
\newcommand\ORI{\mathrel|} % "inline OR"
\newcommand\IN{\mkern5mu \in \mkern5mu}
\newcommand\PP\mathtt
\newcommand\undef{\mathord\perp}
$$

A *semiring* $R$ is a set $R$ with binary operations $+$ (addition) and $⋅$
(multiplication), such that $(R, +)$ is a commutative monoid with identity $0$,
$(R, ⋅)$ is a monoid with identity $1$, multiplication left and right
distribute over addition, and multiplication by $0$ annihilates $R$.

The core type theory must be instantiated over a particular semiring. In Juvix
Core we use the semiring of natural numbers plus ω, which is the most
expressive option — terms can be $0$-usage ("contemplated", i.e. maybe used in
types, but erased at run time), $n$-usage (used at run time exactly $n$ times),
or $ω$-usage (usage untracked).

The type rules and implementation also assume a partial subtraction function
$\mathord- : R^2 \rightharpoonup R$, defined as usual for finite naturals
and, for $\omega$,

$$
\begin{aligned}
\omega - \rho   & = \omega && \text{for any $\rho$,} \\
\pi    - \omega & = \undef && \text{for $\pi \ne \omega$.}
\end{aligned}
$$

<!--
FIXME: do we need to include this?
Other choices include the boolean semiring $(0, 1)$, which tracks variables
which can be erased, the zero-one-many semiring $(0, 1, ω)$, which can also
track linear variables, and the natural numbers with addition and
multiplication, which counts the exact number of uses.
-->

Let $S$ be a set of universe levels with a total order. In Juvix, this is just
the natural numbers.

$$
\begin{aligned}
\pi, \rho, \sigma & \in \mathbb{N} \cup \{\omega\} & \text{usage}          \\
i, j, k           & \in \mathbb{N}                 & \text{universe level} \\
\end{aligned}
$$

Let $K$ be the set of atomic primitive types, $C$ be the set of primitive
constants, and $\mathord{::} \subset C \times \mathbf{K}$ be the typing
relation between primitive constants and primitive types $\mathbf{K}$ (below).
A single primitive $p$ may have several possible types, but all types for
a single $\PP{p}$ must have the same arity. For primitive functions $f$ with
(non-zero) arity $i$, there is also a (partial) reduction operation
$\mathord{\Downarrow_f} : C^i \rightharpoonup C$, defined when the arguments
have a type permitted by $::$ and corresponding to evaluating the function.

$$
\begin{aligned}
\PP{A}         & \IN K \\
\PP{f}, \PP{p} & \IN C \\
\mathbf{K}     & ::= \PP{A} \ORI \PP{A} \to_\pi \mathbf{K}
\end{aligned}
$$

:::{warning}
The implementation doesn't currently support usages on primitive types,
and accepts any usages for any primitive.
:::

The rest of the theory is parameterised over $K$, $C$, $::$, and the various $\Downarrow_f$.

## Syntax

Inspired by the bidirectional syntax of Conor McBride in I Got Plenty o’ Nuttin’ {footcite}`plenty-o-nuttin`.

Let $R, S, T, s, t$ be types & terms and $d, e, f$ be eliminations. Types can be synthesised for eliminations but must be specified in advance for terms.

- TODO: add records
- TODO: add `let`
- TODO: add Σ and unit?
- TODO: add cat (co)product
- TODO: add concrete syntax

$$
\begin{aligned}
R, S, T, s, t & ::= \star_i                  & \text{sort $i$}             \\
              & \OR \PP{A} \ORI \PP{p}       & \text{primitive type/value} \\
              & \OR (\pi \cdot x : S) \to T  & \text{function type}        \\
              & \OR λx.t                     & \text{abstraction}          \\
              & \OR \underline e             & \text{elimination}          \\[.5em]
%
d, e, f & ::= x               & \text{variable}              \\
        & \OR f \; s          & \text{application}           \\
        & \OR \pi \cdot s : S & \text{type/usage annotation} \\
\end{aligned}
$$

Type universes are explicit $\star_i$. Dependent function types and type
annotations include a usage annotation $π$.

Unlike in McBride, the type and usage components of a local (pre)context are separate:

$$
\begin{aligned}
\Gamma & ::= \diamond \ORI \Gamma, x: T        & \text{type context} \\
\Theta & ::= \diamond \ORI \Theta, \pi \cdot x & \text{usage context}
\end{aligned}
$$

The reason for this is that the type rules are presented (and implemented) in terms of an incoming context of available usages and an outgoing context of leftovers.

Scaling a usage context, $\pi\Theta$, is simply defined pointwise:

$$
\begin{aligned}
\pi(\diamond)             & = \diamond                   \\
\pi(\Theta, \rho \cdot x) & = \pi\Theta, \pi\rho \cdot x \\
\end{aligned}
$$

By the definition of a semiring, $0\Theta$ sets all usage annotations to $0$. In addition, the notation $0 \Gamma$ refers to a usage context with the same names as $\Gamma$, with a zero annotation for each. For example:

$$ 0 (A: \star_0, x: A, y: A) = (0 \cdot A, 0 \cdot x, 0 \cdot y). $$

Addition of two usage contexts $\Theta_1 + \Theta_2$ is defined only when they bind the same names in the same order:

$$
\begin{aligned}
⋄ + ⋄ & = ⋄\\
(\Theta_1, \rho_1 \cdot x) + (\Theta_2, \rho_2 \cdot x) & =
  (\Theta_1 + \Theta_2), (\rho_1 + \rho_2)
\end{aligned}
$$

$$
\newcommand\Syn\Rightarrow
\newcommand\Chk\Leftarrow
\newcommand\Q{\mathrel|}
\newcommand\WF\checkmark
$$

A type context $\Gamma$ is a precontext which is _well formed_, written
$\Gamma \vdash \WF$, and defined according to the following rules:

$$
\begin{prooftree}
\AxiomC{}
\RightLabel{Emp}
\UnaryInfC{$⋄ \vdash \WF$}
\end{prooftree}
\qquad
\begin{prooftree}
\AxiomC{$\Gamma \vdash \WF$}
\AxiomC{$\Gamma \vdash_0 S \Chk \star_i$}
\RightLabel{Ext}
\BinaryInfC{$\Gamma, x : S ⊢ \WF$}
\end{prooftree}
$$

The typing judgement $\Gamma \vdash_0 s \Chk A$ is defined in the next section.

:::{note}
Currently, usage contexts are _always_ well formed, because usages can only be simple contexts. This will no longer be true in the future in the presence of usage polymorphism.
:::

## Typing rules

$$
\newcommand\DD\mathsf
\newcommand\HL{\textcolor{red}}
\newcommand\Empty{\;\mathbf{empty}}
$$

Typing judgements come in two forms:

- $\Gamma \Q \Theta \vdash \sigma \cdot s \Chk A \dashv \Theta'$ indicates that, in the type context $\Gamma$, and with $\Theta$ resources avaiable, the *term* $s$ can be checked against the type A, and $\Theta'$ resources remain after.
- $\Gamma \Q \Theta \vdash \sigma \cdot e \Syn A \dashv \Theta'$ means that in the same situation, the *elimination* $e$ can synthesise the type $A$.

Since certain constructions like types cannot exist at run time (currently),
there is an abbreviated form
$\Gamma \vdash_0 s \Chk A$,
standing for
$\Gamma, \Theta \vdash 0 \cdot s \Chk A \dashv \Theta$.
The subject is being checked in a zero context, and so the usage context is
passed through unchanged. The same is true for
$\Gamma \vdash_0 e \Syn A$.

A judgement with $σ = 0$ constructs a term with no computational content, which
will be erased and not appear at run time.

For example, consider the following judgement:

$$
n : \DD{Nat}, x : \DD{Fin} \; n \Q
  0 \cdot n, 1 \cdot x \vdash
  \HL{0} \cdot x : \DD{Fin} \; n \dashv
  0 \cdot n, \HL{1} \cdot x
$$

Because the subject is annotated as zero, this represents "contemplation"
(compile-time-only information such as type signatures), not (run-time)
computation. When type checking, $n$ and $x$ can appear arbitrarily many times,
and $x$ remains available to use in a run time computation elsewhere.

In the following variation:

$$
n : \DD{Nat}, x : \DD{Fin} \; n \Q
  0 \cdot n, 1 \cdot x \vdash
  \HL{1} \cdot x : \DD{Fin} \; n \dashv
  0 \cdot n, \HL{0} \cdot x
$$

This time, the subject is annotated as $1$. This means that this use of $x$ persists at run time, and there are no more available in the output context.

#### Emptiness

When a variable leaves scope, we need to make sure that it has been correctly
used. This is done with an auxiliary judgement, which currently needs only two
rules:

$$
\begin{prooftree}
\AxiomC{}
\RightLabel{EZero}
\UnaryInfC{$0 \Empty$}
\end{prooftree}
\qquad
\begin{prooftree}
\RightLabel{EAny}
\AxiomC{}
\UnaryInfC{$\omega \Empty$}
\end{prooftree}
$$

Once first class usages are integrated this judgement will have to become
more sophisticated.

### Universe (set type)

$$
\begin{prooftree}
\AxiomC{$\Gamma \vdash \WF$}
\AxiomC{$i < j$}
\RightLabel{Star}
\BinaryInfC{$\Gamma \vdash_0 \star_i \Chk \star_j$}
\end{prooftree}
$$

### Primitive constants, functions & types

$$
\begin{prooftree}
\AxiomC{$\Gamma \vdash \WF$}
\RightLabel{PrimTy}
\UnaryInfC{$\Gamma \vdash_0 \PP{A} : \star_i$}
\end{prooftree}
\qquad
\begin{prooftree}
\AxiomC{$\Gamma \vdash \WF$}
\AxiomC{$\PP{p} :: \PP{A}$}
\RightLabel{PrimVal}
\BinaryInfC{
  $\Gamma \Q \Theta \vdash
      \sigma \cdot \PP{p} \Chk \PP{A}
      \dashv \Theta$
}
\end{prooftree}
$$

Primitive constants are typed according to the primitive typing relation $::$,
and they can be produced in any computational quantity wherever desired.


### Dependent function types

Function types $(\pi \cdot x : S) \to T$ record the usage of the argument in
the body of the function with $\pi.$

$$
\begin{prooftree}
\AxiomC{$\Gamma \vdash_0 A \Chk \star_i$}
\AxiomC{$\Gamma, x: A \vdash_0 B \Chk \star_i$}
\RightLabel{Pi}
\BinaryInfC{$\Gamma \vdash_0 (\pi \cdot x : A) → B \Chk \star_i$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$\Gamma, x: S \Q \Theta, \pi \sigma \cdot x \vdash
  \sigma \cdot t \Chk T \dashv \Theta', \zeta \cdot x$}
\AxiomC{$\zeta \Empty$}
\RightLabel{Lam}
\BinaryInfC{$\Gamma \Q \Theta \vdash
  \lambda x. t \Chk (\pi \cdot x: A) \to B \dashv \Theta'$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$\Gamma \Q \Theta \vdash \sigma \cdot f \Syn (\pi \cdot x: A) \to B
  \dashv \Theta'$}
\AxiomC{$\Gamma \Q \Theta' \vdash \pi \sigma \cdot s \Chk A \dashv \Theta''$}
\RightLabel{App}
\BinaryInfC{$\Gamma \Q \Theta \vdash \sigma \cdot f \; s \Chk B[x := s]
  \dashv \Theta''$}
\end{prooftree}
$$

- In the introduction rule, the abstracted variable $x$ has usage $\sigma \pi$,
  because we are checking $\sigma$ copies of a function which each use $x$
  $\pi$ times.
- In the elimination rule, the output usage context of the function is used as
  the input for the argument, so that *its* output is the total usage of both.


### Variables

The variable rule selects an individual variable, type, and usage annotation
from the context:

$$
\begin{prooftree}
\AxiomC{$\Gamma_1, x : A, \Gamma_2 \vdash \WF$}
\RightLabel{Var}
\UnaryInfC{$
  \Gamma_1, x : A, \Gamma_2 \Q
    \Theta_1, \pi \cdot x, \Theta_2
  \vdash \sigma \cdot x \Syn A
  \dashv \Theta_1, (\pi - \sigma) \cdot x, \Theta_2
$}
\end{prooftree}
$$


### Conversion & subtyping

The subtyping rule allows judgementally equal types to be used
interchangeably. It is used when checking an elimination against
a given type, to ensure it is compatible with the one synthesised.

$$
\begin{prooftree}
\AxiomC{$\Gamma \Q \Theta \vdash \sigma \cdot e \Syn A \dashv \Theta'$}
\AxiomC{$\Gamma \vdash A <: B$}
\RightLabel{Sub}
\BinaryInfC{$
  \Gamma \Q \Theta \vdash
    \sigma \cdot \underline e \Chk
    B \dashv \Theta'$}
\end{prooftree}
$$

#### Conversion

$$
\newcommand\Step\hookrightarrow
\newcommand\Red\rightarrowtail
\newcommand\RedR\leftarrowtail
$$

Computation $\Red$ is the reflexive transitive congruence closure of the
following rules:

- $(\lambda x. t : (\pi \cdot x : S) \to T) \; s \Step_\beta (t: T)[x := (s: S)]$
  - TODO: β for other constructions
- $\underline{t : T} \Step_\upsilon t$

Two terms are convertible $s \equiv t$ if there exists a term $u$ with $s
\Red u \RedR t$.

#### Subtyping

:::{warning}
The usage part of subtyping, and for that matter subtyping itself,
is experimental and may be removed if it causes problems.
:::

Subtyping concerns itself with universe levels, and with casting
function types with exact usages to those without. It does not (yet?)
deal with record fields.

$$
\begin{prooftree}
\AxiomC{}
\RightLabel{SURefl}
\UnaryInfC{$\pi <: \pi$}
\end{prooftree}
\qquad
\begin{prooftree}
\AxiomC{}
\RightLabel{SUOmega}
\UnaryInfC{$\pi <: \omega$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$\Gamma \vdash S \equiv T$}
\RightLabel{SRefl}
\UnaryInfC{$\Gamma \vdash S <: T$}
\end{prooftree}
\qquad
\begin{prooftree}
\AxiomC{$i \le j$}
\RightLabel{SRefl}
\UnaryInfC{$\Gamma \vdash \star_i <: \star_j$}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$\Gamma \vdash S_2 <: S_1$}
\AxiomC{$\Gamma \vdash T_1 <: T_2$}
\AxiomC{$\pi_1 <: \pi_2$}
\RightLabel{SFun}
\TrinaryInfC{$
  \Gamma \vdash
    (\pi_1 \cdot x: S_1) \to T_1 <:
    (\pi_2 \cdot x: S_2) \to T_2$}
\end{prooftree}
$$

:::{note}
Subtyping doesn't currently recurse into structures other than function types
and $\star$.
:::


:::{footbibliography}
:::
