;;10/20/16
;;For use with LSODE
;;X is the current position
;;phi_N is the order of derivative of potential wrt x
;;E field is naturally -phi_N[0], then, since E = -dPhi/dx
PRO KAPPA_1__DORS_KLETZING_EQ_18__MAXWELL,X,phi_N

  COMPILE_OPT IDL2

  COMMON MAX2DPARAMS,M2D__temp,M2D__dens,M2D__R_B

  j_par = KNIGHT_RELATION__DORS_KLETZING_4(M2D__temp,M2D__dens,pot,M2D__R_B, $
                                          IN_POTBAR=in_potBar, $
                                          OUT_POTBAR=potBar)

  ;;Zeroth order
  pot  = phi_N[0]

  ;;First order
  E    = (-1.D) phi_N[1]

  ;;Second order
  ;; eq18 = 1.D/PEDERSEN__HAREL1981(/MAXWELL)
  eq18 = 1.D/PEDERSEN__ROBINSON1987(/MAXWELL) * ( 

END
