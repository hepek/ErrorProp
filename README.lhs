ErrorProp
=========

TODO: writeme 

Error propagation library.

Installation

    $ git clone git://github.com/hepek/ErrorProp.git
    $ cd ErrorProp
    $ cabal install


Example 1: Two Link Planar Arm
----------------------------


Let's simulate a non-linear transformation by combining translations 
and rotations of a robotic arm and evaluating error propagation at different
operation points.

![Illustration](images/two-link-planar-arm.png "Figure 1.")

Figure 1. A two link planar robotic arm


> import Math.ErrorProp
> import Math.SimpleMx
>

Coordinates

> x = defVar "x"
> y = defVar "y"

Model parameters

> a1 = defVar"a1"
> a2 = defVar"a2"
> l1 = defVar"l1"
> l2 = defVar"l2"

Couple of helper functions for defining rotations and translations

> rotGen var = fromLists
>   [[cos(var), -sin(var), 0]
>   ,[sin(var),  cos(var), 0]
>   ,[0,         0,        1]]
> 
> transGen x y = fromLists
>  [[1, 0, x]
>  ,[0, 1, y]
>  ,[0, 0, 1]]
> 

Our two link planar arm can now be modeled as superposition of two rotations and two translations:

> arm1 = (rotGen a1) >< (transGen l1 0) >< (rotGen a2) >< (transGen l2 0)

To obtain our non-linear transformation we multiply arm1 with [x, y, 1] (homogenous coordinates).

> armT1 = mkTransf $ toList (arm1 >. fromList [x, y, 1])

We can view our set of resulting functions:

> r1 = print armT1

     o1 = (((x * ((cos(a2) * cos(a1)) + (sin(a2) * -sin(a1)))) + (y * ((-sin(a2) * cos(a1)) + (cos(a2) * -sin(a1))))) + ((l2 * ((cos(a2) * cos(a1)) + (sin(a2) * -sin(a1)))) + (l1 * cos(a1))))
     o2 = (((x * ((cos(a2) * sin(a1)) + (sin(a2) * cos(a1)))) + (y * ((-sin(a2) * sin(a1)) + (cos(a2) * cos(a1))))) + ((l2 * ((cos(a2) * sin(a1)) + (sin(a2) * cos(a1)))) + (l1 * sin(a1))))
     o3 = 1.0

We could easily eliminate x and y from the equations as we know the initial coordinates (0,0).
This can be done in two ways:

1. Reevaluate our matrix-vector multiplication from above:

> armT2 = mkTransf $ toList (arm1 >. fromList [0,0,1])

2. Or use our partialEval utility function

> armT2' = (partial armT1 [(x,0), (y,0)])
> r2 = print armT2

    o1 = ((l2 * ((cos(a2) * cos(a1)) + (sin(a2) * -sin(a1)))) + (l1 * cos(a1)))
    o2 = ((l2 * ((cos(a2) * sin(a1)) + (sin(a2) * cos(a1)))) + (l1 * sin(a1)))
    o3 = 1.0


And  the order of input parameters would be:

> r3 = print (variables armT1)

    [a1,a2,l1,l2,x,y]

> r4 = print (variables armT2)

    [a1,a2,l1,l2]


> degrad a = a/180*pi
> r5 = print $ apply armT2 $ 
>       measurement [ degrad 45 +- 0
>                   , degrad 0  +- 0
>                   , 10 +- 0
>                   , 5 +- 0]

    measurement [
      10.606601717798213 +- 0.0,
      10.606601717798211 +- 0.0,
      1.0 +- 0.0]

> r6 =  apply armT1 $ 
>        measurement [ degrad 45 +- degrad 1
>                    , degrad 0  +- degrad 1
>                    , 10 +- 0.01
>                    , 5  +- 0.01
>                    , 0  +- 0
>                    , 0  +- 0] 

    measurement [
      10.606601717798213 +- 0.1953898090314297,
      10.606601717798211 +- 0.1953898090314297,
      1.0 +- 0.0]

> r6' = getCovariance r6

    [4.241908608148729e-3,-4.219686385926508e-3,0.0]
    [-4.219686385926507e-3,4.24190860814873e-3,0.0]
    [0.0,0.0,0.0]


> r7 = print $ apply armT1 $ 
>       measurement [ degrad 0 +- degrad 1
>                   , degrad 0 +- degrad 1
>                   , 10 +- 0.01
>                   , 5  +- 0.01
>                   , 0  +- 0
>                   , 0  +- 0]

    measurement [
      15.0 +- 1.4142135623730952e-2,
      0.0 +- 0.27596078516100275,
      1.0 +- 0.0]

