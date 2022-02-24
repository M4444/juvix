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

type EvalResultT m a freeAlgObj = ExceptT.ExceptT (EvalError freeAlgObj) m a

-- | This is a no-op at the moment, but it is a single place where
-- | compile-time transformations can be performed on categorial
-- | terms, such as to reduce elim/intro combinations and perform
-- | optimizations such as fusion.
reduce ::
  ( Monad m,
    MinimalInstanceAlgebra freeAlgObj
  ) =>
  Term freeAlgObj ->
  EvalResultT m (Term freeAlgObj) freeAlgObj
reduce = return . id
