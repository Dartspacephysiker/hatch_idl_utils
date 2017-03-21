;2017/03/21
PRO MOMENT_SUITE_2D,diff_eFlux, $
                    ENERGY=energy, $
                    ARANGE__MOMENTS=aRange__moments, $
                    ARANGE__MOMENTS=aRange__charE, $
                    SC_POT=sc_pot, $
                    EEB_OR_EES=eeb_OR_ees, $
                    ERROR_ESTIMATES=error_estimates, $
                    QUIET=quiet, $
                    OUT_N=n, $
                    OUT_J=j, $
                    OUT_JE=je, $
                    OUT_T=T, $
                    OUT_CHARE=charE, $
                    OUT_JJE_COVAR=jje_coVar, $
                    OUT_ERRORS=errors, $
                    OUT_NERR=nErr, $
                    OUT_J=jErr, $
                    OUT_JE=jeErr, $
                    OUT_T=TErr, $
                    OUT_CHARE=charEErr

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;check--is charE getting special treatment?
  specialC = KEYWORD_SET(aRange__charE)
  IF specialC THEN BEGIN
     
     ;;If aRange__charE is the same as aRange__moments, don't calculate charE quantities separately
     specialC = KEYWORD_SET(aRange__moments) ? ~ARRAY_EQUAL(aRange__moments,aRange__charE) : 1

  ENDIF
  
  n        = N_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                   ENERGY=energy, $
                                   ANGLE=aRange__moments, $
                                   SC_POT=sc_pot, $
                                   EEB_OR_EES=eeb_or_ees, $
                                   QUIET=quiet)
  T        = T_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                   ENERGY=energy, $
                                   ANGLE=aRange__moments, $
                                   SC_POT=sc_pot, $
                                   EEB_OR_EES=eeb_or_ees, $
                                   QUIET=quiet)
  j        = J_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                   ENERGY=energy, $
                                   ANGLE=aRange__moments, $
                                   SC_POT=sc_pot, $
                                   EEB_OR_EES=eeb_or_ees, $
                                   QUIET=quiet)
  je       = JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                    ENERGY=energy, $
                                    ANGLE=aRange__moments, $
                                    SC_POT=sc_pot, $
                                    EEB_OR_EES=eeb_or_ees, $
                                    QUIET=quiet)

  IF specialC THEN BEGIN
     jC       = J_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                      ENERGY=energy, $
                                      ANGLE=aRange__charE, $
                                      SC_POT=sc_pot, $
                                      EEB_OR_EES=eeb_or_ees, $
                                      QUIET=quiet)
     jeC      = JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                       ENERGY=energy, $
                                       ANGLE=aRange__charE, $
                                       SC_POT=sc_pot, $
                                       EEB_OR_EES=eeb_or_ees, $
                                       QUIET=quiet)

  ENDIF ELSE BEGIN
     jC  = j 
     jeC = j 
  ENDELSE

  jje_coVar  = (TEMPORARY(JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                 ENERGY=energy, $
                                                 ANGLE=aRange__charE, $
                                                 SC_POT=sc_pot, $
                                                 EEB_OR_EES=eeb_or_ees, $
                                                 /JJE, $
                                                 QUIET=quiet))).y - jeC.y*jC.y

  charE = CHAR_ENERGY((TEMPORARY(jC)).y,(TEMPORARY(jeC)).y)

  IF KEYWORD_SET(error_estimates) THEN BEGIN

     errors          = MOMENTERRORS_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                        ENERGY=energy, $
                                                        ANGLE=aRange__moments, $
                                                        SC_POT=sc_pot, $
                                                        EEB_OR_EES=eeb_or_ees, $
                                                        ;; PRESSURE_COVAR_CALC=pressure_covar_calc, $
                                                        /PRESSURE_COVAR_CALC, $
                                                        ;; HEATFLUX_COVAR_CALC=heatFlux_covar_calc, $
                                                        /HEATFLUX_COVAR_CALC, $
                                                        QUIET=quiet)

           ;; IF KEYWORD_SET(dens_errors) THEN BEGIN
           ;; nerr            = MAKE_ARRAY(nHere,/FLOAT)
           ;; jerr            = MAKE_ARRAY(nHere,/FLOAT)
           ;; Terr            = MAKE_ARRAY(nHere,/FLOAT)

           ERROR_CALC_2D,diff_eFlux,errors,n,j,je,T,nerr,jerr,jeErr,charEErr,Terr,jje_coVar
           ;; ERROR_CALC_2D,diff_eFlux,errors,j,je,n,T,jerr,jeErr,nerr,Terr

  ENDIF

END
