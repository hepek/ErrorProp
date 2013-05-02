ErrorProp
=========

Error propagation library.

  import Math.ErrorProp

  α = Symbol "α"
  β = Symbol "β"
  γ = Symbol "γ"

  mRz = [[cos(α), -sin(α), 0]
        ,[sin(α),  cos(α), 0]
        ,[0,       0,      1]]

  mRy = [[cos(β),  0,      sin(β)]
      	,[0,       1,      0]
	,[-sin(β), 0,      cos(β)]]

  mRx = [[1,       0,      0]
      	,[0,       cos(γ), -sin(γ)]
	,[0,       sin(γ), cos(γ)]]
