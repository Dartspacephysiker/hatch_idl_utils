;;10/28/16
;;This function returns the height-integrated conductivity, in mhos, based on the formulation of Robinson et al. [1987]
;;I am casting it as it is presented in Elphic et al. [1998]

;;charE: Electron characteristic energy in keV
;;JEe  : Electron energy flux in ergs/cm^2/s (or equivalently W/m^2)

;;The DK_ODE18 COMMON vars relate to the differential equation for ionospheric potential given as Equation 18 in Dors and Kletzing [1998]
FUNCTION PEDERSEN_ROBINSON1987,JEe,pot,charE, $
                               IN_JE=je, $
                               KAPPA=kappa, $
                               MAXWELL=maxwell, $
                               DERIVATIVE=derivative, $
                               IN_POTBAR=in_potBar, $
                               OUT_POTBAR=potBar, $
                               POT_IN_JOULES=pot_in_joules

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__dk_ode18.pro

  IF (N_ELEMENTS(charE) EQ 0) AND N_ELEMENTS(je) EQ 0 THEN BEGIN
     PRINT,"Must provide either characteristic energy or current density!"
     RETURN,-1
  ENDIF

  CASE 1 OF
     KEYWORD_SET(kappa): BEGIN
        JEe = KAPPA_1__DORS_KLETZING_EQ_15__EFLUX(DK18__k,DK18__T_m__k,DK18__dens_m__k, $
                                                  pot,DK18__R_B__k, $
                                                  IN_POTBAR=in_potBar, $
                                                  OUT_POTBAR=potBar, $
                                                  POT_IN_JOULES=pot_in_joules)
     END
     KEYWORD_SET(maxwell): BEGIN
        JEe = KAPPA_1__DORS_KLETZING_EQ_14__EFLUX__MAXWELL(DK18__T_m__m,DK18__dens_m__m, $
                                                           pot,DK18__R_B__m, $
                                                           IN_POTBAR=in_potBar, $
                                                           OUT_POTBAR=potBar, $
                                                           POT_IN_JOULES=pot_in_joules)

     END
     ELSE: BEGIN
        IF N_ELEMENTS(JEe) EQ 0 THEN BEGIN
           PRINT,"Must supply JEe!"
           RETURN,-1
        END
     END
  ENDCASE

  IF N_ELEMENTS(charE) EQ 0 THEN charE = JEe / je

  IF KEYWORD_SET(derivative) THEN BEGIN
     RETURN,20.D * charE / ( SQRT(JEe) * ( 16.D + charE^2 ) )
  ENDIF

  ;;Sigma_P in who knows what units
  RETURN,40.D * DOUBLE(charE) * SQRT(DOUBLE(JEe)) / (16.D + charE^2)
END
