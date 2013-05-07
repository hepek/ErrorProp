ErrorProp
=========

TODO: writeme 

Error propagation library.

Example 1: Two Link Planar Arm
----------------------------

Let's simulate a non-linear transformation by combining translations 
and rotations of a robotic arm and evaluating error propagation at different
operation points.

![Illustration](images/two-link-planar-arm.png "Figure 1.")
Figure 1. A two link planar robotic arm

\begin{code}
import Math.ErrorProp

-- coordinates
x = defVar "x"
y = defVar"y"

-- model parameters
a1 = defVar"a1"
a2 = defVar"a2"
l1 = defVar"l1"
l2 = defVar"l2"

-- couple of helper functions for defining rotations and translations
rotGen var = 
  [[cos(var), -sin(var), 0]
  ,[sin(var),  cos(var), 0]
  ,[0,         0,        1]]

transGen x y = 
 [[1, 0, x]
 ,[0, 1, y]
 ,[0, 0, 1]]


-- our two link planar arm can now be modeled as superposition of
-- two rotations and two translations
arm1 = (rotGen a1) >< (transGen l1 0) >< (rotGen a2) >< (transGen l2 0)

-- since we are using homogenous coordinates we multiply arm1 with [x, y, 1]
armT1 = nlt (arm1 >. [x, y, 1])
\end{code}

We can view our set of resulting functions:

\begin{code}
r1 = print armT1
\end{code}
o1 = (((x * ((cos(a2) * cos(a1)) + (sin(a2) * -sin(a1)))) + (y * ((-sin(a2) * cos(a1)) + (cos(a2) * -sin(a1))))) + ((l2 * ((cos(a2) * cos(a1)) + (sin(a2) * -sin(a1)))) + (l1 * cos(a1))))
o2 = (((x * ((cos(a2) * sin(a1)) + (sin(a2) * cos(a1)))) + (y * ((-sin(a2) * sin(a1)) + (cos(a2) * cos(a1))))) + ((l2 * ((cos(a2) * sin(a1)) + (sin(a2) * cos(a1)))) + (l1 * sin(a1))))
o3 = 1.0

We could easily eliminate x and y from the equations as we know the initial coordinates (0,0).
This can be done in two ways:

1. Reevaluate our matrix-vector multiplication from above:

\begin{code}
armT2 = nlt (arm1 >. [0,0,1])
\end{code}

2. Or use our partialEval utility function

\begin{code}
armT2' = (partial armT1 [(x,0), (y,0)])
\end{code}

And  the order of input parameters would be:
\begin{code}
r2 = print (variables armT1)
\end{code}

[a1,a2,l1,l2,x,y]


\begin{code}
r3 = print (variables armT2)
\end{code}
[a1,a2,l1,l2]

