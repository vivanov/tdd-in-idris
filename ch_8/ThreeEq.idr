data ThreeEq : (a: Type) -> (b: Type) -> (c: Type) -> Type where
     Refl3 : ThreeEq t t t
     
allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z Refl3 = Refl3

