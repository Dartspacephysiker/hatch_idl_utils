;2016/10/28
;T_m    :  Temperature in eV
;dens_m :  Density in cm^-3
;pot    :  Potential in eV. (Set POT_IN_JOULES if in joules)
;R_B    :  Mirror ratio B_magnetosphere / B_ionosphere
FUNCTION KAPPA_1__DORS_KLETZING_EQ_14__D_EFLUX_D_POT__MAXWELL,T_m,dens_m,pot,R_B, $
   IN_POTBAR=in_potBar, $
   OUT_POTBAR=potBar, $
   POT_IN_JOULES=pot_in_joules

  COMPILE_OPT idl2
  
  toJ                    = 1.6e-19 ;eV to J

  eCharge                = DOUBLE(1.6e-19)

  speedOfLight           = DOUBLE(299792458.) ;m / s
  electron_mass          = DOUBLE(5.109989e5) / speedOfLight^2.D ;eV/c^2 (where c is in m/s)

  n                      = DOUBLE(dens_m * 1000000.D)  ;dens_m in m^-3
  IF KEYWORD_SET(pot) THEN BEGIN
     pot                 = DOUBLE(pot)
  ENDIF

  IF KEYWORD_SET(in_potBar) THEN BEGIN
     potBar              = in_potBar
  ENDIF ELSE BEGIN
     potBar              = DOUBLE(pot/T_m) ;potential normalized by temperature
     IF KEYWORD_SET(pot_in_joules) THEN BEGIN
        potBar          *= eCharge
     ENDIF
  ENDELSE

  ;;Constant out front
  C                      = n * toJ * R_B * SQRT( T_m^3 / 2.D / !PI / electron_mass )
  dC                     = n * toJ * R_B * SQRT( T_m   / 2.D / !PI / electron_mass )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;; innie = 1.D - EXP( (-1.D) * potBar / ( R_B - 1.D ) ) - ( potBar + 2.D * ( 1.D - 1.D / R_B ) ) * EXP( (-1.D) * potBar / ( R_B - 1.D ) ) / ( 1.D - R_B ) 
  
  F     = dC * ( 1.D - EXP( potBar / ( 1.D - R_B ) )  * ( 1.D + ( potBar + 2.D * ( 1.D - 1.D / R_B ) ) / ( 1.D - R_B ) ) )

  RETURN,F

END

