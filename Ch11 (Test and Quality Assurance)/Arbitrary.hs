module Arbitrary where

data Ternary
  = Yes
  | No
  | Unknown
  deriving (Eq, Show)

class Arbitrary a where
  arbitrary :: Gen a

instance Arbitrary Ternary where
    arbitrary elements [Yes, No, Unknown]

instance Arbitrary Ternary where
    arbitrary = do
        n <- choose (0, 2) :: Get Int
        return $ case n of 
            0 -> Yes
            1 -> No
            _ -> Unknown

elements :: [a] -> Gen a
choose :: Random a => (a, a) -> Gen a
oneof :: [Gen a] -> Gen a
