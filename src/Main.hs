import Math.ErrorProp
import System.Exit

test1 =
  let
    m = Uncorr (fromList [1,2,3]) (fromList [1,2,3])
    nlt = nt [x1+x2,x2+x3,x3]
    llt = lt [[1,1,0],[0,1,1],[0,0,1]]
  in
    ((linearT llt m), (nonLinearT nlt [] m))


main = do
  let (a,b) = test1
  if (a == b)
    then
    exitSuccess
    else
    exitFailure
