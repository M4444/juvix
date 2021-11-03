# Core Language

## Basics

Juvix Core is the core language in the Juvix compiler stack, defining the canonical syntax & semantics on which all compilers & interpreters must agree. Lower-level evaluation choices may safely differ between implementations as long as they respect the core semantics.

Juvix Core is primarily inspired by Quantitative Type Theory {footcite}`quantitative-type-theory`, Formality {footcite}`formality`, and Cedille {footcite}`the-calculus-of-dependent-lambda-eliminations`. It fuses full-spectrum dependent types (types & terms can depend on types & terms) with linear logic using the contemplation — computation distinction introduced by QTT, adds the self-types of Cedille & Formality to enable derivation of induction for pure lambda terms, introduces the full connective set of linear logic, dependent where appropriate, to express different varieties of conjunction & disjunction, and defines an extension system for opaque user-defined datatypes & primitives (such as integers with addition & multiplication, bytestrings with concatenation & indexing, or cryptographic keys with construction & signature checking).

```{note}
At present, the substructural typing in the core language is not required for optimal reduction — separate elementary affine logic assignments are inferred in the lower-level stage. Substructural typing is used in Juvix Core to provide additional precision to the programmer and enable optimisations in the bespoke compilation path to custom rewrite rules (such as avoiding garbage collection). In the future a closer fusion allowing the more precise usage information to inform interaction net construction is expected; this is an open research question.
```

## Changes from QTT

### Primitive types, constants, and functions

Nothing too exciting here. These will vary based on the machine target.

### Usage polymorphism

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

## Preliminaries

A *semiring* $R$ is a set $R$ with binary operations $+$ (addition) and $⋅$ (multiplication), such that $(R, +)$ is a commutative monoid with identity $0$,
$(R, ⋅)$ is a monoid with identity $1$, multiplication left and right distribute over addition, and multiplication by $0$ annihilates $R$.

The core type theory must be instantiated over a particular semiring. Choices include the boolean semiring $(0, 1)$, the zero-one-many semiring $(0, 1, ω)$, and the natural numbers with addition and multiplication.

In canonical Juvix Core the type theory is instantiated over the semiring of natural numbers plus ω, which is the most expressive option — terms can be $0$-usage ("contemplated"), $n$-usage ("computed $n$ times"), or $ω$-usage ("computed any number of times"). 

Let $S$ be a set of sorts $(i, j, k)$ with a total order. 

Let $K$ be the set of primitive types, $C$ be the set of primitive constants, and $⋮$ be the typing relation between primitive constants and primitive types, which must assign to each primitive constant a unique primitive type and usage.

Let $F$ be the set of primitive functions, where each $f$ is related to a function type, including an argument usage annotation, by the $⋮$ relation and endowed with a reduction operation $→_{f}$, which provided an argument of the function input type computes an argument of the function output type.

Primitive types, primitive constants, and primitive functions are threaded-through to the untyped lambda calculus to which Core is erased, so they must be directly supported by the low-level execution model. The core type theory and subsequent compilation pathways are parameterised over $K$, $C$, $F$, $⋮$, and the reduction operations $→_{f}$, which are assumed to be available as implicit parameters.

## Syntax

Inspired by the bidirectional syntax of Conor McBride in I Got Plenty o’ Nuttin’ {footcite}`plenty-o-nuttin`.

Let $R, S, T, s, t$ be types & terms and $d, e, f$ be eliminations, where types can be synthesised for eliminations but must be specified in advance for terms.

The three columns, in order, are: syntax utilised in this paper, text description, and syntax utilised in the ASCII parser.

### Core Syntax

- TODO: add records
- TODO: add `let`
- TODO: add Σ and unit?
- TODO: add cat (co)product
- TODO: add concrete syntax

$$
\newcommand\OR{\mkern17mu | \mkern12mu}
\begin{aligned}
R, S, T, s, t &::= ∗_i\ & \text{sort $i$} \\
&\OR \kappa \in K & \text{primitive type} \\
&\OR c \in C & \text{primitive constant} \\
&\OR (x \overset{π}{:} S) → T\ & \text{function type} \\
&\OR λx.t\ & \text{abstraction} \\
&\OR e\ & \text{elimination} \\
\end{aligned}
$$

$$
\begin{aligned}
d, e, f &::= x\ & \text{variable} \\
&\OR f s\ & \text{application} \\
&\OR s \overset{π}{:} S & \text{type/usage annotation} \\
\end{aligned}
$$

Sorts $∗_i$ are explicitly levelled. Dependent function types, dependent conjunction types, and type annotations include a usage annotation $π$.

<!-- Judgements have the following form:

$$ x_1 \overset{ρ_1}{:} S_1, ..., x_n \overset{ρ_n}{:} S_n \vdash\ M \overset{σ}{:} T $$

where $ρ_1 ... ρ_n$ are elements of the semiring and $σ$ is either the $0$ or $1$ of the semiring. -->

Further define the syntactic categories of usages $ρ, π$ and precontexts $Γ$:

$$
\newcommand\IN{\mkern5mu \in \mkern5mu}
\begin{aligned}
ρ,π,σ & \IN  R \\
Γ   & ::= ⋄\ |\ Γ,x \overset{ρ}{:} S
\end{aligned}
$$

The symbol $⋄$ denotes the empty precontext.

Precontexts contain usage annotations $ρ$ on constituent variables. Scaling a precontext, $πΓ$, is defined as follows:

$$
\begin{aligned}
π(⋄)                    & = ⋄ \\
π(Γ,x \overset{ρ}{:} S) & = πΓ,x \overset{πρ}{:} S
\end{aligned}
$$

Usage annotations in types are not affected.

By the definition of a semiring, $0Γ$ sets all usage annotations to $0$.

Addition of two precontexts $Γ_1 + Γ_2$ is defined only when $0Γ_1 = 0Γ_2$:

$$
\begin{aligned}
⋄ + ⋄ & = ⋄\\
(Γ_1,x \overset{ρ_1}{:} S) + (Γ_2,x \overset{ρ_2}{:} S)
  & = (Γ_1+Γ_2), x \overset{ρ_1 + ρ_2}{:} S
\end{aligned}
$$

Contexts are identified within precontexts by the judgement $Γ\vdash$, defined by the following rules:

$$
\begin{prooftree}
\AxiomC{}
\RightLabel{Emp}
\UnaryInfC{⋄ ⊢}
\end{prooftree}
$$

$$
\begin{prooftree}
\AxiomC{$Γ ⊢$}
\AxiomC{$0Γ ⊢ S$}
\RightLabel{Ext}
\BinaryInfC{$Γ,x \overset{ρ}{:} S ⊢$}
\end{prooftree}
$$

$0Γ ⊢ S$ indicates that $S$ is well-formed as a type in the context of $0Γ$.
$Emp$, for "empty", builds the empty context, and $Ext$, for "extend", extends a context $Γ$ with a new variable $x$ of type $S$ and usage annotation $ρ$.
All type formation rules yield judgements where all usage annotations in $Γ$ are $0$ — that is to say, type formation requires no computational resources).

Typing judgements have the form $Γ ⊢ M \overset{σ}{:} S$.
A judgement with $σ = 0$ constructs a term with no computational content, which
will be erased and not appear at run time.

For example, consider the following judgement:

$$
n \overset{0}{:} Nat, x \overset{1}{:} Fin(n) ⊢ x \overset{\sigma}{:} Fin(n)
$$

When $σ = 0$, the judgement expresses that the term can be typed:

$$
n \overset{0}{:} Nat, x \overset{1}{:} Fin(n) ⊢
  x \overset{\color{red}{0}}{:} Fin(n)
$$

Because the final colon is annotated to zero, this represents "contemplation"
(compile-time-only information such as type signatures), not (run-time)
computation. When type checking, $n$ and $x$ can appear arbitrarily many times.

In the computational judgement:

$$
n \overset{0}{:} Nat, x \overset{1}{:} Fin(n) ⊢
  x \overset{\color{red}{1}}{:} Fin(n)
$$

Because the final colon is annotated to one, during computation, $n$ is used
exactly $0$ times, $x$ is used exactly one time. $x$ can also be annotated as
any other natural number, or as $ω$, which indicates that it can be used
(computed with) an arbitrary number of times.

## Typing rules

### Universe (set type)

Let $S$ be a set of sorts ${i, j, k}$ with a total order.

#### Formation rule

$$
\begin{prooftree}
\AxiomC{$0Γ ⊢$}
\AxiomC{$i < j$}
\RightLabel{∗}
\BinaryInfC{$0Γ ⊢ ∗_i \overset{0}{:} ∗_j$}
\end{prooftree}
$$

#### Introduction rule

$$
\begin{prooftree}
\AxiomC{$0Γ ⊢ V \overset{0}{:} ∗_i$}
\AxiomC{$0Γ, x \overset{0}{:} V ⊢ R \overset{0}{:} ∗_i$}
\RightLabel{∗-Pi}
\BinaryInfC{$Γ ⊢ (x \overset{π}{:} V) → R \overset{0}{:} ∗_i$}
\end{prooftree}
$$

$σ = 0$ fragment only.

### Primitive constants, functions & types

#### Constants

##### Formation & introduction rule

$$
\begin{prooftree}
\AxiomC{$c \in C$}
\AxiomC{$\kappa \in K$}
\AxiomC{$c ⋮ \kappa$}
\RightLabel{Prim-Const}
\TrinaryInfC{$⊢ c \overset{γ}{:} \kappa$}
\end{prooftree}
$$

Primitive constants are typed according to the primitive typing relation $⋮$,
and they can be produced in any computational quantity wherever desired.


### Dependent function types

Function types $(x \overset{π}{:} S) → T$ record usage of the argument.

#### Formation rule

$$
\begin{prooftree}
\AxiomC{$0Γ ⊢ S$}
\AxiomC{$0Γ,x \overset{0}{:} S ⊢ T$}
\RightLabel{Pi}
\BinaryInfC{$0Γ ⊢ (x \overset{π}{:} S) → T$}
\end{prooftree}
$$

#### Introduction rule

$$
\begin{prooftree}
\AxiomC{$Γ,x \overset{σπ}{:} S ⊢ M \overset{σ}{:} T$}
\RightLabel{Lam}
\UnaryInfC{$Γ ⊢ λx.M \overset{σ}{:} (x \overset{π}{:} S) → T$}
\end{prooftree}
$$

The usage annotation $π$ is not used in judgement of whether $T$ is a well-formed type. It is used
in the introduction and elimination rules to track how $x$ is used, and how to multiply the resources
required for the argument, respectively:

#### Elimination rule

$$
\begin{prooftree}
\AxiomC{$Γ_1 ⊢ M \overset{σ}{:} (x \overset{π}{:} S) → T$}
\AxiomC{$Γ_2 ⊢ N \overset{σ'}{:} S$}
\AxiomC{$0Γ_1 = 0Γ_2$}
\AxiomC{$σ' = 0 ⇔ (π = 0 ∨ σ = 0)$}
\RightLabel{App}
\QuaternaryInfC{$Γ_1 + π Γ_2 ⊢ M N \overset{σ}{:} T[x := N]$}
\end{prooftree}
$$

- $0Γ_1 = 0Γ_2$ means that $Γ_1$ and $Γ_2$ have the same variables with the same types
- In the introduction rule, the abstracted variable $x$ has usage $σπ$ so that non-computational production requires no computational input
- In the elimination rule, the resources required by the function and its argument, scaled to the amount required by the function, are summed
- The function argument $N$ may be judged in the 0-use fragment of the system if and only if we are already in the 0-use fragment ($σ = 0$) or the function will not use the argument ($π = 0$).


### Variable & conversion rules

The variable rule selects an individual variable, type, and usage annotation from the context:

$$
\begin{prooftree}
\AxiomC{$⊢ 0Γ,x \overset{σ}{:} S, 0Γ′$}
\RightLabel{Var}
\UnaryInfC{$0Γ,x \overset{σ}{:} S, 0Γ′ ⊢ x \overset{σ}{:} S$}
\end{prooftree}
$$

The conversion rule allows conversion between judgementally equal types:

$$
\begin{prooftree}
\AxiomC{$Γ ⊢ M \overset{σ}{:} S$}
\AxiomC{$0Γ ⊢ S≡T$}
\RightLabel{Conv}
\BinaryInfC{$Γ ⊢ M \overset{σ}{:} T$}
\end{prooftree}
$$

```{note}
Type equality is judged in a context with no resources.
```


### Sub-usaging

To-do: check if we can safely allow sub-usaging if the ring is the natural numbers, discuss here.

## Reduction semantics

Contraction is $(λx.t : (π x : S) → T)\ s ⇝_{β} (t:T)[x := (s:S)]$.

- TODO: β for other constructions

De-annotation is $(t : T) ⇝_υ t$.

The reflexive transitive congruence closure of $⇝_{β}$ and $⇝_υ$ yields
computation $\searrow$. Two terms are convertible $s \equiv t$ if
there exists a term $u$ with $s \searrow u \swarrow t$.

## Typechecking

TODO: Lay out syntax-directed typechecker following McBride's paper.

```{footbibliography}
```
