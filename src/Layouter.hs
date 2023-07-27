module Layouter
  (

  ) where

import           Barbies
import           Barbies.Bare
import           Data.Kind (Type)
import           Data.Text (Text)
import qualified Data.Text as Text
import           HGeometry.Properties
import           HGeometry.Unbounded
import           HGeometry.Vector
import           Layouter.Constraints
import           Layouter.Elements

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- * UI Elements

-- newtype Label t f = Label (Wear t f Text)



-- newtype Button t f content = Button (Wear t f content)
--   -- deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

-- newtype Row t f content = Row [Wear t f content]
--   -- deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

-- newtype FlexRow t f content = FlexRow [Wear t f content]
--   -- deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

--------------------------------------------------------------------------------



-- myRow :: FlexRow HKD (Button HKD )
-- myRow = FlexRow


--------------------------------------------------------------------------------
-- * Layout


class ComputeSize (t :: Type -> (Type -> Type) -> Type) where
  -- | We are given some size constraints
  computeSize :: (Num r, Ord r)
              => Constraints r -> t Bare f -> t Covered (Sized (Top r))


-- | default text height, in pixels
textHeight :: Num r => r
textHeight = 10




instance ComputeSize Label where
  computeSize constr (Label t) = Label (Sized size t)
    where
      size = Size Top (ValT textHeight)

instance ComputeSize content => ComputeSize (Button content) where
  computeSize constr (Button content) = Button (sized content')
    where
      content' = computeSize constr content


-- class IsLayoutable t where
--   computeSize :: (r ~ NumType t) => Constraints r -> t -> Sized (Top r) t
--   layout :: (r ~ NumType t) => Constraints r -> t -> Positioned (Top r) t
