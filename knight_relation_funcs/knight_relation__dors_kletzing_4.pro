;;07/05/16
;;Comes from Equation 4 in Dors and Kletzing [1999], the Knight relation
;;T_m   :  Magnetospheric temperature of electrons  (eV)
;;Dens_m:  Magnetospheric plasma density            (cm^-3)
;;R_B   :  Magnetic field ratio, B_ionos/B_msphere  (dimensionless)
;;Pot   ;  Potential drop                           (eV/C)
FUNCTION KNIGHT_RELATION__DORS_KLETZING_4,T_m,dens_m,pot,R_B, $
                                          IN_POTBAR=in_potBar, $
                                          OUT_POTBAR=potBar

  COMPILE_OPT IDL2

  IF N_ELEMENTS(R_B) EQ 0 THEN BEGIN
     mRat                = 30
     PRINT,'Setting R_B to 30 ...'
  ENDIF ELSE BEGIN
     mRat                = R_B
  ENDELSE

  ;;a few constants
  speedOfLight           = DOUBLE(299792458)                  ;m/s
  electron_mass          = DOUBLE(5.1099891e5)/speedOfLight^2 ;eV/c^2
  
  eCharge                = DOUBLE(1.6e-19) ;Coulombs

  ;;Convert input params
  n                      = DOUBLE(Dens_m * 1000000.D) ;dens_m in m^-3
  helpMeNotBeZero        = 0.000001D
  IF KEYWORD_SET(in_potBar) THEN BEGIN
     potBar              = in_potBar
  ENDIF ELSE BEGIN
     potBar              = DOUBLE(eCharge * pot/T_m) ;potential normalized by temperature
  ENDELSE


  ;;Equation segments
  JVinv                  = (-1.D) * eCharge * n
  JV1                    = SQRT( T_m / ( 2.D * !PI * electron_mass ) )
  JV2sub                 = (-1.D) * potBar / ( mRat - 1.D + helpMeNotBeZero )
  JV2                    = mRat * (1.D - (1.D - 1.D/mRat) * EXP(JV2sub) )



  Jpar                   = JVinv * JV1 * JV2

  RETURN,Jpar
END
