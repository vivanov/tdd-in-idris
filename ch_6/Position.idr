import Data.Vect

Position: Type
Position = (Double, Double)

Polygon: Nat -> Type
Polygon n = Vect n Position 

tri: Vect 3 Position
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

tri3: Polygon 3
tri3 = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]
