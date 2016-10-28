;;10/28/16
;;This function returns the height-integrated conductivity, in mhos, based on the formulation of Robinson et al. [1987]
;;I am casting it as it is presented in Elphic et al. [1998]

;;charE: Electron characteristic energy in keV
;;JEe  : Electron energy flux in ergs/cm^2/s (or equivalently W/m^2)

;;The DK_ODE18 COMMON vars relate to the differential equation for ionospheric potential given as Equation 18 in Dors and Kletzing [1998]
FUNCTION PEDERSEN_ROBINSON1987,JEe,charE, $
                               KAPPA=kappa, $
                               MAXWELL=maxwell, $
                               IN_POTBAR=in_potBar, $
                               OUT_POTBAR=potBar, $
                               POT_IN_JOULES=pot_in_joules

  COMPILE_OPT IDL2

  @common__dk_ode18.pro

  CASE 1 OF
     KEYWORD_SET(kappa): BEGIN
        JEe = KAPPA_1__DORS_KLETZING_EQ_15__EFLUX(DK18__kappa,DK18__T_m__kappa,DK18__dens_m__kappa,pot,DK18__R_B__kappa, $
                                                  IN_POTBAR=in_potBar, $
                                                  OUT_POTBAR=potBar, $
                                                  POT_IN_JOULES=pot_in_joules)
     END
     KEYWORD_SET(maxwell): BEGIN
        JEe = KAPPA_1__DORS_KLETZING_EQ_14__EFLUX__MAXWELL(DK18__T_m__maxwell,DK18__dens_m__maxwell,pot,DK18__R_B__maxwell, $
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

  Sigma_P = 40.D * DOUBLE(charE) * SQRT(DOUBLE(JEe)) / (16.D + charE^2)

END
