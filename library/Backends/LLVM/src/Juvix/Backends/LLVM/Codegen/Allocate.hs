module Juvix.Backends.LLVM.Codegen.Allocate where

import qualified Juvix.Backends.LLVM.Codegen.Block as Block
import qualified Juvix.Backends.LLVM.Codegen.Types as Types
import Juvix.Library
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as Type

-- TODO :: Abstract out the repeat node pool code as best as you can
-- (modules wanted ☹)

--------------------------------------------------------------------------------
-- Node Pool Allocation Information
--------------------------------------------------------------------------------

registerNodePool ::
  HasState "moduleDefinitions" [AST.Definition] m => m ()
registerNodePool =
  -- should this really be address space 0?
  Block.addGlobal nodePoolName (Types.pointerOf nodeReference)

initializeNodePool :: Types.Call m => Integer -> m ()
initializeNodePool initialSize = do
  allocation <- Block.malloc (initialSize * nodeLength) (Types.pointerOf nodeReference)
  nodePoolGlobal Block.<-- allocation

nodePoolGlobal :: AST.Operand
nodePoolGlobal =
  AST.ConstantOperand (C.GlobalReference (Types.pointerOf nodeReference) nodePoolName)

nodePoolName :: AST.Name
nodePoolName =
  "JU-node-pool"

registerNode :: HasState "moduleDefinitions" [AST.Definition] m => m ()
registerNode =
  Block.addType nodeName nodeType

nodeReference :: Type.Type
nodeReference = Type.NamedTypeReference nodeName

nodeName :: AST.Name
nodeName = "node"

-- | @nodeLength@ serves as the size of one @nodeType@
-- Note that 1 + 8 + 8 = 8 / 8 + 64 / 8 + 64 / 8.
nodeLength :: Integer
nodeLength = 1 + 8 + 8

-- we are doing an array of struct allocation.. we really should move
-- this to a struct of arrays at some point

-- | @nodeType@ serves as a node in our allocation procedure
nodeType :: Type.Type
nodeType =
  Type.StructureType
    { isPacked = True,
      elementTypes =
        [ -- Tag for various purposes
          Type.IntegerType 8,
          -- First data location/offset
          Type.IntegerType 64,
          -- Second data location/offset
          Type.IntegerType 64
        ]
    }

--------------------------------------------------------------------------------
-- Vector Pool Allocation Information
--------------------------------------------------------------------------------

registerVectorPool ::
  HasState "moduleDefinitions" [AST.Definition] m => m ()
registerVectorPool =
  -- should this really be address space 0?
  Block.addGlobal vectorPoolName vectorType

initializeVectorPool :: Types.Call m => Integer -> m ()
initializeVectorPool initialSize = do
  allocation <- Block.malloc (initialSize * 8) vectorType
  nodePoolGlobal Block.<-- allocation

vectorPoolGlobal :: AST.Operand
vectorPoolGlobal =
  AST.ConstantOperand (C.GlobalReference vectorType nodePoolName)

vectorPoolName :: AST.Name
vectorPoolName =
  "JU-vector-pool"


-- | @vectorType@ serves as the base unit in our vector allocation pool.
-- We will refer to it as a pointer to i8, with the following layout
-- [ size₁ : i64 |  data : size₁ | … | ]
vectorType :: Type.Type
vectorType = Types.pointerOf Type.i8
