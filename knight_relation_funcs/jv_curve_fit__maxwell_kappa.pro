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
   TIE_R_B_AND_DENS=tie_R_B_and_dens, $
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

  IF KEYWORD_SET(tie_R_B_and_dens) THEN BEGIN

     ;; COMMON tieRB,tRB_RBpairs,tRB_fLine,tRB_nFAST,tRB_nFLine,tRB_fLineRE
     @common__jv_curve_fit__tie_r_b_and_dens.pro

     dens_m = JV_CURVE_FIT__TIE_R_B_AND_DENS__GET_DENS(pot,T_m,R_B)
     PRINT,'wasHere ',kappa,T_m,dens_m,R_B
     ;; R_B_ionos  = tRB_RBpairs[1,VALUE_CLOSEST2(tRB_RBpairs[1,*],R_B,/CONSTRAINED)]
     ;; IF ABS(R_B_ionos - R_B)/R_B_ionos GT 0.2 THEN STOP

     ;; R_B_FAST   = tRB_RBpairs[0,VALUE_CLOSEST2(tRB_RBpairs[1,*],R_B,/CONSTRAINED)]

     ;; dens_m     = DENSITY_FACTOR__BARBOSA_1977(10.D^(MEAN(ALOG10(pot))), $
     ;; tRB_nFLine = DENSITY_FACTOR__BARBOSA_1977(10.D^(MEAN(ALOG10(pot))), $
                                               ;; T_m, $
                                               ;; 0, $
                                               ;; tRB_nFAST, $
                                               ;; R_B_FAST)
                                               ;; tRB_RBpairs[1,*])


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
