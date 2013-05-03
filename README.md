ErrorProp
=========

Error propagation library.

```haskell
import Math.ErrorProp

α = var "α"
β = var "β"
γ = var "γ"
x = var "x"
y = var "y"

rot = [[cos(α), -sin(α), 0]
      ,[sin(α),  cos(α), 0]
      ,[0,       0,      1]]

trans = [[1, 0, x]
        ,[0, 1, y]
        ,[0, 0, 1]]


```
