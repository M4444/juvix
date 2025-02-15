# BNF Syntax

The bnf diagram below, is the BNF of the Juvix Programming Language

## BNF Syntax

```bnf
  header ::= mod <symbol> where <top-level>*
           | <top-level>*

  top-level ::= <type>
              | <module-open>
              | <type-class>
              | <type-class-instance>
              ; These four last, due to lack of words before
              | <module-signature>
              | <infix-declare>
              | <module>
              | <module-open>
              | <signature>
              | <inlcude>
              | <alias>
              | <function>

  ;;;; Declarations ======================================================

  declarations ::= declare <declare-adt>

  declare-adt ::= <infix-declare>

  infix-declare ::= infix <symbol> <number>
                  | infixl <symbol> <number>
                  | infixr <symbol> <number>

  declaration-expression ::= <declarations> in <expression>

  ;;;; Module Manipulators ============================================

  include ::= include <symbol>
  alias   ::= alias   <symbol> = <symbol>


  ;;;; Functions ======================================================

  ;; let until indent sensitivity is gone
  function ::= let  <function-body>

  function-body ::= <name> <args>+ = <expression>
                  | <name> <args>+ <cond-logic>*

  ;; Match-logic could just be a name, and so is thus fine
  ;; # is for implicit
  ;; Should we have implicit pattern matching like here?
  args ::= <match-logic>
         | #<match-logic>

  ;; sig only here while indent sensitivity is not here
  ;; if we only have usage in arrows, we should allow a way to have
  ;; usage information of variables as well!
  signature ::= sig <name>         :                  <expression>
              | sig <name>         : <constraint> =\> <expression>
              | sig <name> <usage> :                  <expression>
              | sig <name> <usage> : <constraint> =\> <expression>

  constraint ::= <type-name>
               | ( <constBody>* )

  constBody ::= <type-name> , <type-name>
              | <type-name>
  ;;;; Types ==========================================================

  type ::= <data-declaration>

  ;; Should we let there be a refinement in adt type declaration?
  ;; ? means it may or may not be there
  ;; it makes sense to have the number here, as it can't be confused with an
  ;; expression type
  data-declaration
    ::= type u#<usage>? <non-capital-name> <symbols>+ : <expression> = <adt>
      | type u#<usage>? <non-capital-name> <symbols>+                = <adt>

  adt ::= <sum1> <sum>*
        | <product1>


  sum1 ::= <name>
         | <name> <product>

  ;; | is special in bnf
  sum ::= \| <name>
        | \| <name> <product>

  product1 ::= { <name-type-comma>+ <name-type>* } -\> <type-name>
             | { <name-type-comma>* }              -\> <type-name>
             | { <name-type-comma>+ <name-type>* }
             | { <name-type-comma>* }


  product ::= { <name-type-comma>+ <name-type>* } -\> <type-name>
            | { <name-type-comma>* }              -\> <type-name>
            | { <name-type-comma>+ <name-type>* }
            | { <name-type-comma>* }
            | : <expression>
            | <expression''>*

  name-type      ::= <name> <usage>?  <type-signature>
                   | #<name> <usage>? <type-signature>
  name-type-comma ::= <name-type> <comma>


  ;; { } here are a refinement type!
  type-refine ::= <expression> { <expression> }
                | <expression>
                | <name> : <expression>
                | #<name> : <expression>
                | <name> : <expression> { <expression> }
                | #<name> : <expression> { <expression> }

  ;;;; Arrows =========================================================

  arrow ::= -<usage>-\>

  ;;; Modules ========================================================

  ;; For all intensive purposes, modules are the same as values, just with
  ;; top level expressions, and a minimal amount of sugar

  ;; This doubles as our import
  module-open ::= open <module-name>


  module-open-expression ::= open <module-name> in
                           | <module-name>.( <expression> )

  ;; We are going to make modules a bit more interesting, syntax wise
  ;; imagine modules were functions with capital name to delineate
  ;; thus module signatures have the same signature look as functions

  ;; Interestingly enough module "functors" can take more than just modules
  ;; they can take any value, however for examples, we will encourage the use
  ;; of taking (and thus parameterizing) modules

  ;; let and end are there until indent sensitivity is in
  module ::= mod <name> <args>+ = <top-level>* end
           | mod <name> <args>+ <cond-top>* end


  ;; we use mod here to disambiguate it for parsing speed
  moduleE ::= mod <name> <args>+ = <top-level>* end in <expression>
            | mod <name> <args>+ <cond-top>* end in <expression>

  ;; what should we allow in a module signature?
  ;; begin and end are only here while we aren't indent sensitive



  module-signature ::= <module-signature-helper> Module <sigs-and-types>+ end

  sigs-and-types ::= <sig>
                   | <module-signature>
                   | <type>

  module-signature-helper
    ::= sig <name>           :                 <expression>
      | sig <name>           : <type-name> =\> <expression>
      | sig <name> <usage-f> :                 <expression>
      | sig <name> <usage-f> : <type-name> =\> <expression>


  cond-top ::= \| <expression> = <top-level>*
  ;;;; Types Classes ==================================================

  ;; Need end if we are indent sensitive!
  type-class ::= class <type-name> where
               | class <type-name> =\> <type-name> where

  ;; Need end if we are indent sensitive!
  type-class-instance ::= instance <type-name> where

  ;;;; Expressions ====================================================

  ;; See comments about which to keep and which to maybe remove
  expression'' ::= <match>
                 | <if>
                 | <cond>
                 | <record-access>
                 | <module-lookup>
                 | <let>
                 | <moduleE>
                 | <let-type>
                 | <module-open-expression>
                 | <where>
                 | <string>
                 | <number>
                 | <lambda>
                 | <tuple>
                 | <list>
                 | <parens>
                 | <symbol>
                 ; This is useful for having nested do's or matchs
                 | <block>
                 | <do>
                 | <comments>
                 | <arrow>
                 | <infix>
                 | <record-creation>
                 | <type-refine>
                 ; TODO
                 | <record-update>
                 | <declaration-expression>

  expression ::= <application>
              | <expression''>

  usage ::= <expression>

  usage-f ::= <constant> | ( <expression> )

  record-access ::= <name>.<name>

  module-lookup ::= <module-name>.<name>

  application ::= <name> <expressions>*

  lambda ::= \\ <match-logic>* -\> <expression>

  symbol ::= <name>

  ;; useful for match, and nested do's!
  block ::= begin <expression> end

  do ::= <do-body>*

  do-body ::= <exprsesion> \; <expression>

  list ::= [ <command-list>* ]

  commad-list ::= <exprsesion> , <expression>


  tuple ::= ( <command-tuple>* )

  commad-tuple ::= <exprsesion> , <expression>

  parens ::= ( <expression> )

  comments ::= -- <any-text-not-new-line> \n
             | \n-- <any-text-not-new-line> \n
             | <comments-rec>

  comments-rec ::= <multi-comments>
                 | {- <comments-rec> -}

  multi-comments ::= {- <any-text-not-{-> -}

  infix ::= <expression> <inifx-name> <expression>

  record-creation ::= { <name-set-comma>* }


  name-set-comma ::= <name-set-creation> ,
                   | <name-set-creation>

  name-set-creation ::= <name> = <expression>
                      | <name>


  ;;; Matching ===================================

  match ::= case <expression> of <match-l>*

  match-l ::= \| <match-logic> -\> <expression>

  match-logic ::= <name>@<match-logic'>
                | <match-logic'>

  match-logic' ::= ( <match-logic''> )
                 | <match-logic''>

  match-logic'' ::= <record-match>
                  | <constructor-name> <match-args>+
                  | <constant>

  match-args ::= <name>
               | <match-logic>
               | <constant>

  record-match ::= { <name-set-comma-match>* }

  name-set-comma-match ::= <name-set> ,
                         | <name-set>


  name-set ::= <name> = <match-logic>
             | <name>

  ;; we should remove either if or cond!?
  if   ::= if   <cond-logic>*
  cond ::= cond <cond-logic>*

  constant ::= <string>
             | <number>
             | <char>

  ;;; Bindings ===================================

  ;; Due to trying to be less indent sensitive,
  ;; we only look for the in alternative,
  ;; is that we only have a single binding per let.
  let ::= let <function-body> in <expression>

  type-let ::= let <type> in <expression>

  ;; Does this even make sense to have?
  ;; Juvix is not lazy, how is order determined?
  ;; is it only for pure values???
  where ::= <expression> where <bindings>*

  binding ::= <match-logic> = <expression>


  ;; note it's fine to use |,
  ;; as matches have to be a pattern,
  ;; and thus not some expression

  ;; note in stdlib else and otherwise will both be true

  cond-logic ::= \| <expression> = <expression>

  ;;; Numbers ====================================

  number ::= <digits>*.<digits>*
           | <digits>*<exp>
           | <digits>*.<digits>*<exp>


  digits ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9


  exp ::= e <digits>*
  ;;; Strings ====================================

  ;; Give nicer string syntax?
  string ::= " <escaped-string>+ "

  escaped-string ::= <ascii-no-quotes-no-backslash> <escaped-string>+
                   | \" <escaped-string>+
                   | \ <escaped-string>+

  ;;; Universe ====================================

  ;; for now, set it up to what F* has, expand it later
  universe-expression ::= u#<name>
                       | u#<name> + u#<name>
                       | max u#<name>*

  ;;;; Misc ===========================================================
  ;; ; is comment in bnf
  comma            ::= ,
  semi             ::= \;
  name             ::= <utf8-non-reserved>
  non-capital-name ::= <utf8-no-capital>
  capital-name     ::= <utf8-capital>
  ;; may want m e or Map.t int string?
  type-name   ::= <name> <others-names>+
  infix-symbol ::= <utf8-infix>

  module-name ::= <name> ; enforce capital names?

  constructor-name ::= <capital-name-and-symbols>

  utf8-infix        ::= `<utf-non-reserved>`
                      | <UTF.Symbol>
  utf8-non-reserved ::= <UTF.Alpha>
                      | (<utf8-infix>)
  utf8-no-capital   ::=
  utf8-capital      ::=
```
