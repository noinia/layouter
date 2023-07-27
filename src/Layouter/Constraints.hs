module Layouter.Constraints
  ( Constraints(Constraints)
  , Size(MkSize,Size)
  , Sized(..), sized
  , Offset
  , Positioned(..)

  , HasSize(..)
  ) where

import Control.Lens
import HGeometry.Properties
import HGeometry.Unbounded
import HGeometry.Vector

--------------------------------------------------------------------------------

newtype Constraints r = Constraints (Size (Top r))
  deriving (Show,Eq,Functor,Foldable,Traversable)

-- | Sizes
newtype Size r = MkSize (Vector 2 r)
  deriving (Show,Eq,Functor,Foldable,Traversable)

pattern Size     :: r -> r -> Size r
pattern Size w h = MkSize (Vector2 w h)
{-# COMPLETE Size #-}

--------------------------------------------------------------------------------

-- | Types that have a size
data Sized r a = Sized (Size r) a
  deriving (Show,Eq,Functor,Foldable,Traversable)

sized   :: GetSize a r => a -> Sized r a
sized x = Sized (getSize x) x

class GetSize a r | a -> r where
  getSize :: a -> Size r

class HasSize s t r r' | s -> r, t -> r' where
  -- | Lens to access the size of a particular type.
  size :: Lens s t (Size r) (Size r')

instance HasSize (Sized r a) (Sized s a) r s where
  size = lens (\(Sized s _) -> s) (\(Sized _ x) s -> Sized s x)

instance GetSize (Sized r a) r where
  getSize = view size

instance GetSize (t Covered (Sized r)) r where
  getSize =

--------------------------------------------------------------------------------

-- | Offset with respect to the origin.
type Offset r = Vector 2 r

-- | Types that have a size and an offset
data Positioned r a = Positioned !(Size r) !(Offset r) a
  deriving (Show,Eq,Functor,Foldable,Traversable)
