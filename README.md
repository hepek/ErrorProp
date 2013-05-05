ErrorProp
=========

TODO: writeme 

Error propagation library.

Let's simulate a non-linear transformation by combining translations 
and rotations of a robotic arm and evaluating error propagation at different
operation points.

```haskell
import Math.ErrorProp

-- coordinates
x = var "x"
y = var "y"

-- model parameters
a1 = var "a1"
a2 = var "a2"
l1 = var "l1"
l2 = var "l2"

rotGen var = 
  [[cos(var), -sin(var), 0]
  ,[sin(var),  cos(var), 0]
  ,[0,         0,        1]]

transGen x y = 
 [[1, 0, x]
 ,[0, 1, y]
 ,[0, 0, 1]]


-- our two link planar arm can be modeled as two rotations and two translations
arm1 = (rotGen a1) >< (transGen 0 l1) >< (rotGen a2) >< (transGen 0 l2)

-- let's turn that into our non-linear transformation
-- since we are using homogenous coordinates we multiply arm with [x, y, 1]
armT1 = nlt (arm1 >. [x, y, 1])
```

If we print this

```haskell
>print armT1
o1 = (((x * ((cos(a2) * cos(a1)) + (sin(a2) * -sin(a1)))) + (y * ((-sin(a2) * cos(a1)) + (cos(a2) * -sin(a1))))) + ((l2 * ((-sin(a2) * cos(a1)) + (cos(a2) * -sin(a1)))) + (l1 * -sin(a1))))
o2 = (((x * ((cos(a2) * sin(a1)) + (sin(a2) * cos(a1)))) + (y * ((-sin(a2) * sin(a1)) + (cos(a2) * cos(a1))))) + ((l2 * ((-sin(a2) * sin(a1)) + (cos(a2) * cos(a1)))) + (l1 * cos(a1))))
o3 = 1.0

```


```haskell
degrad a = a/180*pi

m1 = um [degrad 45, degrad 0, 20, 5, 0, 0] [0, 0, 0, 0, 0, 0]

res1 = transform armT1 m1
```
