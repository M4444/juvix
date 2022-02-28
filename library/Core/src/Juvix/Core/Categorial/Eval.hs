module Juvix.Core.Categorial.Eval
  ( reduce,
  )
where

import qualified Control.Monad.Trans.Except as ExceptT
import Juvix.Core.Categorial.Errors (EvalError)
import Juvix.Core.Categorial.Private.TermPrivate (MinimalInstanceAlgebra, Term)
import Juvix.Library
  ( Monad,
    return,
    (.),
  )
import Prelude
  ( id,
  )

type EvalResultT m a carrier = ExceptT.ExceptT (EvalError carrier) m a

-- | This is a no-op at the moment, but it is a single place where
-- | compile-time transformations can be performed on categorial
-- | terms, such as to reduce elim/intro combinations and perform
-- | optimizations such as fusion.
reduce ::
  ( Monad m,
    MinimalInstanceAlgebra carrier
  ) =>
  Term carrier ->
  EvalResultT m (Term carrier) carrier
reduce = return . id
