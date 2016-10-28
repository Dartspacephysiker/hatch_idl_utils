;;10/28/16
;;This is the solver for Equation 18 in Dors and Kletzing [1999]. The idea is to obtain the ionospheric potential based on kappa or
;;Maxwellian model parameters provided by the user.
;;
;;Zeroth-order terms:
PRO DORS_KLETZING__ODE_18

  COMPILE_OPT IDL2

  @common__dk_ode18.pro

END
