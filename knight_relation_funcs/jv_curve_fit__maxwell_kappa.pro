;2017/03/18
;    FUNCTION MYFUNCT, X, P
;     ; The independent variable is X
;     ; Parameter values are passed in "P"
;     YMOD = ... computed model values at X ...
;     return, YMOD
;    END
;2016/09/01 
;Now we include a functional form for the horseshoe that depends on mu = v_par/v_tot,
; after work by folks like like Bingham et al. [1999] or Bingham and Cairns [2000]
;
;Previously only one parameter in P, and that was density
; P[0]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; P[1]:   T,         Plasma kinetic temperature (eV)
; P[2]:   n,         Plasma density (cm^-3)
; P[3]: R_B,         Bfield_ratio, as in |B_msphere|/|B_isphere|
; mass is presumed to come from SDT, in eV/c^2 (with c in km/s)
;Returns Jpar (microA/m^2)
FUNCTION JV_CURVE_FIT__MAXWELL_KAPPA,X,P, $
   IN_POTBAR=in_potBar, $
   OUT_POTBAR=potBar, $
   IN_TEMPERATURES=in_temperatures, $
   IN_DENSITIES=in_densities, $
   IN_KAPPAS=in_kappas, $
   NO_MULT_BY_CHARGE=no_mult_by_charge, $
   IS_MAXWELLIAN_FIT=is_maxwellian_fit, $
   UNITS=units, $
   MASS=mass

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;X is pot
  pot    = X

  kappa  = DOUBLE(P[0])
  T_m    = DOUBLE(P[1])
  dens_m = DOUBLE(P[2])
  R_B    = DOUBLE(P[3])

  IF KEYWORD_SET(in_temperatures) THEN BEGIN
     T_m = in_temperatures
  ENDIF

  IF KEYWORD_SET(in_densities) THEN BEGIN
     dens_m = in_densities
  ENDIF

  IF KEYWORD_SET(in_kappas) THEN BEGIN
     kappa  = in_kappas
  ENDIF

  CASE 1 OF
     KEYWORD_SET(is_Maxwellian_fit): BEGIN
        Jpar = KNIGHT_RELATION__DORS_KLETZING_4(T_m,dens_m,pot,R_B, $
                                                IN_POTBAR=in_potBar, $
                                                OUT_POTBAR=potBar, $
                                                NO_MULT_BY_CHARGE=no_mult_by_charge, $
                                                MASS=mass)
     END
     ELSE: BEGIN
        Jpar = KNIGHT_RELATION__DORS_KLETZING_11(kappa,T_m,dens_m,pot,R_B, $
                                                 IN_POTBAR=in_potBar, $
                                                 OUT_POTBAR=potBar, $
                                                 NO_MULT_BY_CHARGE=no_mult_by_charge, $
                                                 MASS=mass)
     END
  ENDCASE

  RETURN,Jpar*1D6
END
