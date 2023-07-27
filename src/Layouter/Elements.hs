{-# LANGUAGE UndecidableInstances #-}
module Layouter.Elements
  ( Label(..)
  , Button(..)
  ) where

import           Barbies
import           Barbies.Bare
import           Control.Lens
import           Data.Kind (Type)
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics

--------------------------------------------------------------------------------
-- * Label


newtype Label t f = Label (Wear t f Text)
  deriving stock Generic

instance BareB Label
-- instance FieldNamesB (Label Covered) where
--   bfieldNames = Label (Const "foo") (Const "bar")
instance FunctorB (Label Covered)
instance TraversableB (Label Covered)
instance ConstraintsB (Label Covered)

deriving stock instance Show (Label Bare Identity)
deriving via Barbie (Label Covered) h instance
    Show (Barbie (Label Covered) h) => Show (Label Covered h)

deriving stock instance Eq   (Label Bare Identity)
deriving via Barbie (Label Covered) h instance
    Eq (Barbie (Label Covered) h) => Eq (Label Covered h)


test :: Label Bare Identity
test = Label (Text.pack "foo")

--------------------------------------------------------------------------------
-- * Button

-- newtype Button (content :: Type)
--                (t       :: Type)
--                (f       :: Type -> Type)
--                          = Button (Wear t f content)
--   deriving stock Generic

-- instance BareB (Button content)
-- -- instance FieldNamesB (Button Covered) where
-- --   bfieldNames = Button (Const "foo") (Const "bar")
-- instance FunctorB (Button content Covered)
-- instance TraversableB (Button content Covered)
-- instance ConstraintsB (Button content Covered)

-- deriving stock instance Show content => Show (Button content Bare Identity)
-- deriving via Barbie (Button content Covered) h instance
--     Show (Barbie (Button content Covered) h) => Show (Button content Covered h)

-- deriving stock instance Eq content => Eq   (Button content Bare Identity)
-- deriving via Barbie (Button content Covered) h instance
--     Eq (Barbie (Button content Covered) h) => Eq (Button content Covered h)

-- testButton :: Button (Label Bare Identity) Bare Identity
-- testButton = Button test


type BarbieF = Type -> (Type -> Type) -> Type

newtype Button (content :: BarbieF)
               (t       :: Type)
               (f       :: Type -> Type)
                       = Button (Wear t f (content t f))
  deriving stock Generic

-- instance BareB (Button content)
-- -- instance FieldNamesB (Button Covered) where
-- --   bfieldNames = Button (Const "foo") (Const "bar")
-- instance ( FunctorB (content Covered)
--            content Covered f
--          )
--          => FunctorB (Button content Covered) where

bmap'                     :: forall f g content. (Functor f, FunctorB (content Covered))
                          => (forall a. f a -> g a)
                          -> Button content Covered f -> Button content Covered g
bmap' nt (Button content) = Button $ nt content1
  where
    content0 :: f (content Covered f)
    content0 = content

    content1 :: f (content Covered g)
    content1 = fmap (bmap nt) content0

    -- content' :: g (content Covered g)
    -- content' = fmap (bmap nt) content1


    -- content1 :: g (content Covered f)
    -- content1 = nt content

    -- content' :: g (content Covered g)
    -- content' = fmap (bmap nt) content1

-- instance TraversableB (Button content Covered)
-- instance ConstraintsB (Button content Covered)

-- deriving stock instance Show content => Show (Button content Bare Identity)
-- deriving via Barbie (Button content Covered) h instance
--     Show (Barbie (Button content Covered) h) => Show (Button content Covered h)

-- deriving stock instance Eq content => Eq   (Button content Bare Identity)
-- deriving via Barbie (Button content Covered) h instance
--     Eq (Barbie (Button content Covered) h) => Eq (Button content Covered h)

-- testButton :: Button Label Bare Identity
-- testButton = Button test
