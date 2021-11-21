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

$$
\newcommand\DD\mathrm % defined name (tycon, etc)
\newcommand\FF{\mathrm} % field name
\newcommand\KW\mathsf % keywords
\newcommand\PP\mathtt % primitives
\newcommand\Meta\mathbf % metaoperations
$$

### Records

$$
\newcommand\RT[1]{\{#1\}} % record type
\newcommand\RV[1]{\langle #1 \rangle} % record value
%\newcommand\LetR[4]{ % record match
%  \KW{let} \; \RV{#1} = #2 \; \KW{return} \; #3 \; \KW{in} \; #4
%}
\newcommand\LetR[4]{ % record match
  \KW{case} \; #2 \; \KW{return} \; #3 \; \KW{of} \;
    \RV{#1} \mapsto #4
}
$$

Like Juvix itself, the core language contains first class records in the style
of e.g. Standard ML: a value like
$\RV{\FF{x} = \PP{1}, \FF{y} = \PP{2}}$
can be given a type like
$\RT{1 \cdot \FF{x} : \PP{nat}, \omega \cdot \FF{y} : \PP{nat}}$.
The types can depend on the previous fields, so values can be packaged up with
proofs about them, for example, a triple of numbers together with a
(compile-time only) proof that one is the sum of the others:

$$
\RT{
  1 \cdot \FF{a} : \PP{nat},
  1 \cdot \FF{b} : \PP{nat},
  1 \cdot \FF{c} : \PP{nat},
  0 \cdot \FF{p} : \FF{a} + \FF{b} = \FF{c}
}.
$$

Records can also be used to pack up multiple usages of values, for example in
return types or in a variant of a [coproduct](#coproducts).

Unlike in the frontend language, the only way to decompose a record in core is
to match on the whole thing at once, such as:

$$
\begin{gathered}
  \newcommand\Show[1]{\PP{show}\;\FF{#1}}
  \newcommand\App{\mathbin{+\mkern-10mu+}}
  \newcommand\Str[1]{\texttt{"#1"}}
  \LetR{\FF{a}, \FF{b}, \FF{c}, \FF{p}}
       {r}
       {\_. \PP{str}}
       {\\\Show{a} \App \Str+ \App \Show{b} \App \Str= \App \Show{c}}.
\end{gathered}
$$

Programs written in terms of projections are rewritten into this form.

Record types do not interact with [subtyping](#subtyping): it is not
(currently?) possible to derive e.g.
$r : \RT{1 \cdot \FF{a} : A}$ from
$r : \RT{1 \cdot \FF{a} : A, 0 \cdot \FF{b} : B}$.

### Let-definitions

$$
\newcommand\Let[4]{\KW{let} \; #1 \cdot #2 = #3 \; \KW{in} \; #4}
$$

Local definitions are maintained through the core so that they can appear in the
same place in the generated code as in the input program. Core's `let` does not
support pattern matching or recursion, but only binds an expression to a single
name, like in

$$\Let{2}{x}{f \; a \; b}{x + x}.$$

### Coproducts

$$
\newcommand\Inl{\KW{inl}\;}
\newcommand\Inr{\KW{inr}\;}
%
\newcommand\CaseBase[7]{
  \KW{case} \; #2 \; \KW{return} \; #3 \; \KW{of} \; #1{#4}{#5}{#6}{#7}
}
\newcommand\Case{
  \newcommand\BODY[4]{
    \left\{ \begin{aligned}
        \Inl #1 &\mapsto #2 \\
        \Inr #3 &\mapsto #4
    \end{aligned} \right\}
  }
  \CaseBase\BODY
}
\newcommand\CaseOL{
  \newcommand\BODY[4]{
    \{ \Inl #1 \mapsto #2 \mathrel| \Inr #3 \mapsto #4 \}
  }
  \CaseBase\BODY
}
$$

A value of coproduct type $A \oplus B$ is either an $A$ or a $B$, like `Either`
in Haskell. Since only one is present at a time neither type depends on the
other.

A coproduct is eliminated with a `case` expression. The return type is specified
since it might depend on the value of the coproduct being matched, and both
branches must have identical usage behaviour; for example, if
$e : \PP{nat} \oplus \PP{str}$:

$$\Case{e}{\_. \PP{str}}{n}{\PP{show} \; n}{s}{s}.$$


## Preliminaries

$$
\newcommand\OR[1][]{\mkern17mu #1| \mkern12mu}
\newcommand\ORI{\mathrel|} % "inline OR"
\newcommand\IN{\mkern5mu \in \mkern5mu}
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
the natural numbers. The name $\zeta$ (for zero) is used by convention to denote
the usage of a local variable that must be [empty](#emptiness) by the end of its
scope.

$$
\begin{aligned}
\pi, \rho, \sigma, \zeta & \in \mathbb{N} \cup \{\omega\} & \text{usage}          \\
i, j, k                  & \in \mathbb{N}                 & \text{universe level} \\
\end{aligned}
$$

For primitive types, we require the following pieces of data:

- $K$, the set of atomic primitive types
- $C$, the set of primitive constants
- $\Meta{ar} : K \to \mathbb{N}$, the arity of type constructors
- $\mathord{::} \subset C \times \PP{A}$, the typing relation between primitive
  constants and primitive types $\PP{A}$ (defined below).

The arity of a primitive type constructor is usually zero as in the case of e.g.
`nat`, `string`, but some backends might support polymorphic types such as
`list`.

A single primitive $p$ may have several possible types, but all types for
a single $\PP{p}$ must have the same arity. For primitive functions $f$ with
(non-zero) arity $i$, there is also a (partial) reduction operation
$\mathord{\Downarrow_f} : C^i \rightharpoonup C$, defined when the arguments
have a type permitted by $::$ and corresponding to evaluating the function.

$$
\begin{aligned}
\PP{K}         & \IN K \\
\PP{f}, \PP{p} & \IN C \\
\PP{A},\PP{B}  & ::= \hat{\PP{K}} \to_\pi \PP{A} \ORI \hat{\PP{K}} \\
\hat{\PP{K}}   & ::= \hat{\PP{K}} \; \PP{K} \ORI \PP{K}
\end{aligned}
$$

:::{warning}
The implementation doesn't currently support usages on primitive types,
and accepts any usages for any primitive.
:::

The rest of the theory is parameterised over $K$, $C$, $::$, and the various $\Downarrow_f$.

## Syntax

Inspired by the bidirectional syntax of Conor McBride in I Got Plenty o’ Nuttin’ {footcite}`plenty-o-nuttin`.

Let $A, B, S, T, s, t, u$ be types & terms, and $d, e, f$ be eliminations. Types
can be synthesised for eliminations but must be specified in advance for terms.

- TODO: add concrete syntax

$$
\newcommand\FunTy[4]{(#1 \cdot #2 : #3) \to #4}
%
\begin{aligned}
A, B, S, T, s, t, u
  & ::= \star_i
    & \text{sort $i$} \\
  & \OR \PP{A} \ORI \PP{p}
    & \text{primitive type/value} \\
  & \OR \FunTy \pi x A B \ORI λx.t
    & \text{function type/abstraction} \\
  & \OR A \oplus B \ORI \Inl s \ORI \Inr t
    & \text{coproduct type/value} \\
  & \OR \RT\Delta \ORI \RV\delta
    & \text{record type/value} \\
  & \OR \Let{\pi}{x}{e}{s}
    & \text{local binding} \\
  & \OR \underline e
    & \text{elimination} \\[.5em]
%
d, e, f
  & ::= x
    & \text{variable} \\
  & \OR f \; s
    & \text{application} \\
  & \OR[\Big] \Case{e}{z.A}{x}{u_1}{y}{u_2}
    & \text{coproduct match} \\
  & \OR \LetR{\bar x}{e}{z.A}{s}
    & \text{record match} \\
  & \OR \pi \cdot s : S
    & \text{type/usage annotation} \\[.5em]
%
\Delta & ::= \diamond \ORI \Delta, \pi \cdot x : A
  & \text{field usages/types} \\
\delta & ::= \diamond \ORI \delta, x = s
  & \text{field assignments}
\end{aligned}
$$

A non-dependent function type can be abbreviated as $A \to_\pi B$.

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
\newcommand\Rule[3]{
  \frac{
    \begin{gathered} #2 \end{gathered}
  }{ #3 } \; [\textsf{#1}]
}
\newcommand\Syn\Rightarrow
\newcommand\Chk\Leftarrow
\newcommand\WF\checkmark
\newcommand\Ctx[2]{\begin{smallmatrix} #1 \\ #2 \end{smallmatrix}}
$$

A type context $\Gamma$ is a precontext which is _well formed_, written
$\Gamma \vdash \WF$, and defined according to the following rules:

$$
\Rule{emp}{}{\diamond \vdash \WF}
\qquad
\Rule{ext}{
  \Gamma \vdash \WF \qquad
  \Gamma \vdash_0 S \Chk \star_i
}{
  \Gamma, x: S \vdash \WF
}
$$

The typing judgement $\Gamma \vdash_0 s \Chk A$ is defined in the next section.

:::{note}
Currently, usage contexts are _always_ well formed, because usages can only be simple contexts. This will no longer be true in the future in the presence of usage polymorphism.
:::

## Typing rules

$$
\newcommand\HL{\textcolor{red}}
\newcommand\Empty{\;\Meta{empty}}
$$

Typing judgements come in two forms. In a derivable judgement, $\Gamma$,
$\Theta$, and $\Theta'$ always bind the same variables in the same order.

- $\Ctx{\Gamma}{\Theta} \vdash \sigma \cdot s \Chk A \dashv \Theta'$ indicates
  that, in the type context $\Gamma$, and with $\Theta$ resources avaiable, the
  *term* $s$ can be checked against the type A, and $\Theta'$ resources remain
  after. In a type checking algorithm, $\Theta'$ is an output and the rest are
  inputs.
- $\Ctx{\Gamma}{\Theta} \vdash \sigma \cdot e \Syn A \dashv \Theta'$ means that
  in the same situation, the *elimination* $e$ can synthesise the type $A$. This
  time, $A$ and $\Theta'$ are both outputs.

Since certain constructions like types cannot exist at run time (currently),
there is an abbreviated form
$\Gamma \vdash_0 s \Chk A$,
standing for
$\Ctx{\Gamma}{\Theta} \vdash 0 \cdot s \Chk A \dashv \Theta$.
The subject is being checked in a zero context, and so the usage context is
passed through unchanged. The same is true for
$\Gamma \vdash_0 e \Syn A$.

A judgement with $σ = 0$ constructs a term with no computational content, which
will be erased and not appear at run time.

For example, consider the following judgement:

$$
\Ctx{n : \DD{Nat}, & x : \DD{Fin} \; n}
    {0 \cdot n,    & 1 \cdot x}
  \vdash \HL{0} \cdot x \Syn \DD{Fin} \; n
  \dashv 0 \cdot n, \HL{1} \cdot x
$$

Because the subject is annotated as zero, this represents "contemplation"
(compile-time-only information such as type signatures), not (run-time)
computation. When type checking, $n$ and $x$ can appear arbitrarily many times,
and $x$ remains available to use in a run time computation elsewhere.

In the following variation:

$$
\Ctx{n : \DD{Nat}, & x : \DD{Fin} \; n}
    {0 \cdot n,    & 1 \cdot x}
  \vdash \HL{1} \cdot x \Syn \DD{Fin} \; n
  \dashv 0 \cdot n, \HL{0} \cdot x
$$

This time, the subject is annotated as $1$. This means that this use of $x$ persists at run time, and there are no more available in the output context.

#### Emptiness

When a variable leaves scope, we need to make sure that it has been correctly
used. This is done with an auxiliary judgement, which currently needs only two
rules:

$$
\Rule{ezero}{}{0 \Empty}
\qquad
\Rule{eany}{}{\omega \Empty}
$$

Once first class usages are integrated this judgement will have to become
more sophisticated.

### Universe (set type)

$$
\Rule{star}{
  \Gamma \vdash \WF \qquad
  i < j
}{
  \Gamma \vdash_0 \star_i \Chk \star_j
}
$$

### Primitive constants, functions & types

$$
\Rule{primty}{
  \Gamma \vdash \WF \qquad
  \Meta{ar}(\PP{K}) = k
}{
  \Gamma \vdash_0 \PP{K} \Chk
    \underbrace{\star_0 \to_\omega \dotsb}_k \to_\omega \star_i
}
\qquad
\Rule{primval}{
  \Gamma \vdash \WF \qquad
  \PP{p} :: \PP{A}
}{
  \Ctx{\Gamma}{\Theta} \vdash
      \sigma \cdot \PP{p} \Chk \PP{A}
      \dashv \Theta
}
$$

Primitive constants are typed according to the primitive typing relation $::$,
and they can be produced in any computational quantity wherever desired.


### Dependent function types

Function types $\FunTy \pi x S T$ record the usage of the argument in
the body of the function with $\pi.$

$$
\Rule{pi}{
  \Gamma \vdash_0 A \Chk \star_i \qquad
  \Gamma, x: A \vdash_0 B \Chk \star_i
}{
  \Gamma \vdash_0 (\pi \cdot x : A) → B \Chk \star_i
}
$$

$$
\Rule{lam}{
  \Ctx{\Gamma, & x: S}{\Theta, & \pi \sigma \cdot x} \vdash
    \sigma \cdot t \Chk T \dashv \Theta', \zeta \cdot x \qquad
  \zeta \Empty
}{
  \Ctx\Gamma\Theta \vdash \lambda x. t \Chk \FunTy \pi x A B \dashv \Theta'
}
$$

$$
\Rule{app}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot f \Syn \FunTy \pi x A B
    \dashv \Theta' \qquad
  \Ctx{\Gamma}{\Theta'} \vdash \pi \sigma \cdot s \Chk A \dashv \Theta''
}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot f \; s \Chk B[x := s]
    \dashv \Theta''
}
$$

- In the introduction rule, the abstracted variable $x$ has usage $\sigma \pi$,
  because we are checking $\sigma$ copies of a function which each use $x$
  $\pi$ times.
- In the elimination rule, the output usage context of the function is used as
  the input for the argument, so that *its* output is the total usage of both.


### Coproducts

Constructing a coproduct simply uses the resources of the argument. When
matching against one, each arm of the **case** expression must have the same
resource behaviour, and then the usage of the entire expression is that of the
scrutinee and of one of the branches.

In the elimination form, the $\KW{return} \; z. C$ part specifies the return
type of the whole expression. The variable $z$ is bound in $C$, and stands for
the value of the coproduct itself, in case the return type depends on it. If it
does, it cannot in general be inferred, which is why it is specified in the
first place.


$$
\Rule{coprod}{
  \Gamma \vdash_0 A \Chk \star_i \qquad
  \Gamma \vdash_0 B \Chk \star_i
}{
  \Gamma \vdash_0 A \oplus B \Chk \star_i
}
$$

$$
\Rule{inl}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot s \Chk A \dashv \Theta'
}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot \Inl s \Chk A \oplus B \dashv \Theta'
}
\qquad
\Rule{inr}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot t \Chk B \dashv \Theta'
}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot \Inr t \Chk A \oplus B \dashv \Theta'
}
$$

$$
\Rule{case}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot e \Syn A \oplus B
    \dashv \Theta' \qquad
  \Gamma, z : A \oplus B \vdash_0 C \Chk \star_i \\
  \Ctx{\Gamma,& x: A}{\Theta',& 1 \cdot x} \vdash \sigma \cdot
    u_1 \Chk C[z := \Inl x] \dashv \Theta'', \zeta_1 \cdot x \\[.3em]
  \Ctx{\Gamma,& y: B}{\Theta',& 1 \cdot y} \vdash \sigma \cdot
    u_2 \Chk C[z := \Inr y] \dashv \Theta'', \zeta_2 \cdot y \\
  \zeta_1, \zeta_2 \Empty
}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot
    \left(\Case{e}{z.C}{x}{u_1}{y}{u_2}\right) \Syn C[z := e]
    \dashv \Theta''
}
$$

### Records

$$
\newcommand\Tel{\Meta{tel}}
\newcommand\Names{\Meta{names}}
\newcommand\TY[1]{#1^{\mathsf{T}}}
\newcommand\US[1]{#1^{\mathsf{U}}}
\newcommand\Fresh{\mathrel\#}
$$

A record type consists of a sequence of types and usages, which can depend on
previously mentioned fields. For example, if
$\DD{Even} : \DD{Nat} \to_\omega \star_0$
has already been defined, an even number can be encoded as
$\RT{1 \cdot \FF{val} : \DD{Nat}, 0 \cdot \FF{prf} : \DD{Even} \; \FF{val}}$.
A value of this type would look like
$\RV{\FF{val} = 4, \FF{prf} = \dotsb}$.

In core, extracting the fields of a record is done all at once. In this example,
to extract the value $e$ of the above even number record you could write
$\LetR{\FF{val}, \FF{prf}}{e}{\DD{\_.Nat}}{\FF{val}}$.

The `return` clause here has the same purpose as in the eliminator for
coproducts.

Given a field type list $\Delta$, the notation $\TY{\Delta}$ refers to the type
components of each binding, and $\US{\Delta}$ refers to the usage components.
The operation $\Names(\Delta)$ produces an ordered list of the field names in
$\Delta$. $x \Fresh \Delta$ is an abbreviation for $x \notin \Names(\Delta)$.

The typing of records uses two auxiliary judgements for sequences:

- $\Gamma \vdash \Delta \Chk \Tel_i$, saying that the types in $\Delta$ are
  all well-formed in the context of $\Gamma$ plus the preceding bindings, and
  all fit into universe $i$.
- $\Ctx{\Gamma}{\Theta} \vdash \sigma \cdot \delta
   \Chk \Delta \dashv \Theta'$,
  saying that the fields in $\delta$ have the types in $\Delta$, and altogether
  they take the usages in $\Theta$ to $\Theta'$.

$$
\begin{gathered}
\Rule{rec-ty}{
  \Gamma \vdash \Delta \Chk \Tel_i
}{
  \Gamma \vdash_0 \RT{\Delta} \Chk \star_i
}
\qquad
\Rule{rec-val}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot \delta \Chk \Delta \dashv \Theta'
}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot \RV{\delta} \Chk
    \RT{\Delta} \dashv \Theta'
}
\\[1.5em]
\Rule{tel-nil}{\Gamma \vdash \WF}{\Gamma \vdash \diamond \Chk \Tel_i}
\\[.5em]
\Rule{tel-cons}{
  \Gamma \vdash \Delta \Chk \Tel_i \qquad
  \Gamma, \TY\Delta \vdash_0 A \Chk \star_i \qquad
  x \Fresh \Delta
}{
  \Gamma \vdash \Delta, \pi \cdot x : A \Chk \Tel_i
}
\\[1.5em]
\Rule{fld-nil}{}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot \diamond \Chk \diamond \dashv \Theta
}
\\[.5em]
\Rule{fld-cons}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot \delta \Chk \Delta \dashv \Theta'
  \qquad
  \Ctx{\Gamma, & \TY\Delta}{\Theta', & 0\US\Delta} \vdash
    \sigma \pi \cdot s \Chk A \dashv \Theta''
  \qquad
  x \Fresh \Delta
}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot (\delta, x = s)
    \Chk (\Delta, \pi \cdot x : A)
    \dashv \Theta''
}
\\[1.5em]
\Rule{rec-let}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot e \Syn \RT{\Delta} \dashv \Theta'
  \qquad
  \bar x = \Names(\Delta)
  \\
  \Ctx{\Gamma, & \TY\Delta}{\Theta', & \sigma\US\Delta} \vdash
    \sigma \cdot s \Chk A[z := \RV{\overline{x = x}}]
    \dashv \Theta'', \bar\zeta
  \qquad
  \overline{\zeta \Empty}
}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot
    (\LetR{\bar x}{e}{z. A}{s}) \Syn A[z := e]
    \dashv \Theta''
}
\end{gathered}
$$

:::{admonition} Implementation notes
:class: hint
- If primitives end up with their own universe(s), then **rec-ty** will need a
  side contition on $i$ preventing it from being one of those. But right now
  that is not the case.
- The substitution $A[z := \RV{\overline{x = x}}]$ is replacing index 0
  (referring to $z$) with a record whose fields are indices
  $(|\bar x| - 1), \dotsc, 1, 0$, corresponding to $\bar x$, and weakening all
  other indices by $|\bar x| - 1$.
:::

:::{warning}
At present the names bound in $\bar x$ must be *all* of the fields in $e$, in
the same order. In future it is planned to loosen both of these restrictions by
having the type checker do the necessary reordering and insertions.
:::


### Variables

The variable rule selects an individual variable, type, and usage annotation
from the context:

$$
\Rule{var}{
  \Gamma_1, x : A, \Gamma_2 \vdash \WF
}{
  \Ctx{\Gamma_1, & x : A, & \Gamma_2}{\Theta_1, & \pi \cdot x, & \Theta_2}
  \vdash \sigma \cdot x \Syn A
  \dashv \Theta_1, (\pi - \sigma) \cdot x, \Theta_2
}
$$


### Local bindings

The RHS of a local binding is an elimination, so that its type can be
synthesised. However, the usage cannot be, so it is given explicitly.

- TODO maybe the usage can be inferred somehow if things are arranged
  differently?


$$
\Rule{let}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \pi \cdot e \Syn A \dashv \Theta' \qquad
  \Ctx{\Gamma, & x : A}{\Theta', & \pi \cdot x} \vdash
    \sigma \cdot s \Chk B \dashv \Theta'', \zeta \cdot x \qquad
  \zeta \Empty
}{
  \Ctx{\Gamma}{\Theta} \vdash
    \sigma \cdot \Let{\pi}{x}{e}{s} \Chk B
    \dashv \Theta''
}
$$


### Conversion & subtyping

The subtyping rule allows judgementally equal types to be used
interchangeably. It is used when checking an elimination against
a given type, to ensure it is compatible with the one synthesised.

$$
\Rule{sub}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot e \Syn A \dashv \Theta' \qquad
  \Gamma \vdash A <: B
}{
  \Ctx{\Gamma}{\Theta} \vdash \sigma \cdot \underline e \Chk B \dashv \Theta'
}
$$

#### Conversion

$$
\newcommand\Step\hookrightarrow
\newcommand\Red\twoheadrightarrow
\newcommand\RedR\twoheadleftarrow
$$

Computation $\Red$ is the reflexive transitive context closure of the
following rules:

- $(\lambda x. t : \FunTy \pi x A B) \; s \Step_\beta (t: B)[x := (s: A)]$
- $(\CaseOL{\Inl s}{\_}{x}{u_1}{\_}{\_}) \Step_\beta u_1[x := s]$
- $(\CaseOL{\Inr s}{\_}{\_}{\_}{y}{u_2}) \Step_\beta u_2[y := t]$
- $(\LetR{\bar x}{\RV{\overline{x=t}}}{\_}{s}) \Step_\beta
    s[\overline{x := t}]$
- $(\Let{\pi}{x}{e}{s}) \Step_\delta s[x := e]$
- $\underline{t : T} \Step_\upsilon t$

Two terms are convertible $s \equiv t$ if there exists a term $u$ with $s
\Red u \RedR t$.

- TODO talk about conversion in terms of normalisation, and integrate $\eta$

#### Subtyping

:::{warning}
The usage part of subtyping, and for that matter subtyping itself,
is experimental and may be removed if it causes problems.
:::

Subtyping concerns itself with universe levels, and with casting
function types with exact usages to those without. It does not (yet?)
deal with record fields.

$$
\Rule{surefl}{}{\pi \preceq \pi}
\qquad
\Rule{suany}{}{\pi \preceq \omega}
$$

$$
\Rule{seq}{\Gamma \vdash S \equiv T}{\Gamma \vdash S <: T}
\qquad
\Rule{suni}{i \le j}{\Gamma \vdash \star_i <: \star_j}
$$

$$
\Rule{sfun}{
  \Gamma \vdash S_2 <: S_1 \qquad
  \Gamma \vdash T_1 <: T_2 \qquad
  \pi_1 \preceq \pi_2
}{
  \Gamma \vdash
    (\pi_1 \cdot x: S_1) \to T_1 <:
    (\pi_2 \cdot x: S_2) \to T_2
}
$$

:::{note}
Subtyping doesn't currently recurse into structures other than function types
and $\star$.
:::


:::{footbibliography}
:::
