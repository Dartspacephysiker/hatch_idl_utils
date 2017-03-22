;2017/03/21
PRO MOMENT_SUITE_2D,diff_eFlux, $
                    ENERGY=energy, $
                    ARANGE__MOMENTS=aRange__moments, $
                    ARANGE__CHARE=aRange__charE, $
                    SC_POT=sc_pot, $
                    EEB_OR_EES=eeb_or_ees, $
                    ERROR_ESTIMATES=error_estimates, $
                    MAP_TO_100KM=map_to_100km, $ 
                    ORBIT=orbit, $
                    QUIET=quiet, $
                    OUT_N=n, $
                    OUT_J_=j, $
                    OUT_JE=je, $
                    OUT_T=T, $
                    OUT_CHARE=charE, $
                    OUT_CURRENT=cur, $
                    OUT_JJE_COVAR=jje_coVar, $
                    OUT_ERRORS=errors, $
                    OUT_ERR_N=nErr, $
                    OUT_ERR_J_=jErr, $
                    OUT_ERR_JE=jeErr, $
                    OUT_ERR_T=TErr, $
                    OUT_ERR_CURRENT=curErr, $
                    OUT_ERR_CHARE=charEErr

  COMPILE_OPT IDL2,STRICTARRSUBS

  ions           = STRMATCH(STRUPCASE(eeb_or_ees),'IE*')

  ;;check--is charE getting special treatment?
  specialC       = KEYWORD_SET(aRange__charE)
  IF specialC THEN BEGIN
     
     ;;If aRange__charE is the same as aRange__moments, don't calculate charE quantities separately
     specialC    = KEYWORD_SET(aRange__moments) ? ~ARRAY_EQUAL(aRange__moments,aRange__charE) : 1

  ENDIF
  
  n              = N_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                   ENERGY=energy, $
                                   ANGLE=aRange__moments, $
                                   SC_POT=sc_pot, $
                                   EEB_OR_EES=eeb_or_ees, $
                                   QUIET=quiet)
  T              = T_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                   ENERGY=energy, $
                                   ANGLE=aRange__moments, $
                                   SC_POT=sc_pot, $
                                   EEB_OR_EES=eeb_or_ees, $
                                   QUIET=quiet)
  j              = J_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                   ENERGY=energy, $
                                   ANGLE=aRange__moments, $
                                   SC_POT=sc_pot, $
                                   EEB_OR_EES=eeb_or_ees, $
                                   QUIET=quiet)
  je             = JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                    ENERGY=energy, $
                                    ANGLE=aRange__moments, $
                                    SC_POT=sc_pot, $
                                    EEB_OR_EES=eeb_or_ees, $
                                    QUIET=quiet)

  IF specialC THEN BEGIN
     jC          = J_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                      ENERGY=energy, $
                                      ANGLE=aRange__charE, $
                                      SC_POT=sc_pot, $
                                      EEB_OR_EES=eeb_or_ees, $
                                      QUIET=quiet)
     jeC         = JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                       ENERGY=energy, $
                                       ANGLE=aRange__charE, $
                                       SC_POT=sc_pot, $
                                       EEB_OR_EES=eeb_or_ees, $
                                       QUIET=quiet)

  ENDIF ELSE BEGIN
     jC          = j 
     jeC         = je 
  ENDELSE

  jje_coVar      = (TEMPORARY(JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                 ENERGY=energy, $
                                                 ANGLE=aRange__charE, $
                                                 SC_POT=sc_pot, $
                                                 EEB_OR_EES=eeb_or_ees, $
                                                 /JJE, $
                                                 QUIET=quiet))).y - jeC.y*jC.y

  charE          = CHAR_ENERGY((TEMPORARY(jC)).y,(TEMPORARY(jeC)).y)

  IF KEYWORD_SET(error_estimates) THEN BEGIN

     errors      = MOMENTERRORS_2D__FROM_DIFF_EFLUX(diff_eFlux, $
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
     ;; nerr     = MAKE_ARRAY(nHere,/FLOAT)
     ;; jerr     = MAKE_ARRAY(nHere,/FLOAT)
     ;; Terr     = MAKE_ARRAY(nHere,/FLOAT)

     ERROR_CALC_2D,diff_eFlux,errors,n,j,je,T,nerr,jerr,jeErr,charEErr,Terr,jje_coVar
     ;; ERROR_CALC_2D,diff_eFlux,errors,j,je,n,T,jerr,jeErr,nerr,Terr

  ENDIF

  GET_ALT_MLT_ILAT_FROM_FAST_EPHEM,orbit,j.x, $
                                   OUT_TSORTED_I=tSort_i, $
                                   OUT_ALT=alt, $
                                   OUT_MLT=mlt, $
                                   OUT_ILAT=ilat, $
                                   OUT_MAPRATIO=mapRatio, $
                                   OUT_NEVENTS=nEvents, $
                                   LOGLUN=logLun
  IF N_ELEMENTS(tSort_i) GT 0 THEN STOP

  north_south     = ABS(ilat)/ilat

  flip            = WHERE(north_south LT 0,nFlip)
  IF nFlip GT 0 THEN BEGIN
     j.y[flip]   *= (-1.)
     je.y[flip]  *= (-1.)
  ENDIF

  ;; CASE N_ELEMENTS(WHERE(north_south LT 0,/NULL)) OF
  ;;    N_ELEMENTS(north_south): BEGIN
  ;;    END
  ;;    0: BEGIN
  ;;    END
  ;;    ELSE: BEGIN
  ;;       STOP
  ;;    END
  ;; ENDCASE

  IF KEYWORD_SET(map_to_100km) THEN BEGIN
     j.y         *= mapRatio
     je.y        *= mapRatio

     IF KEYWORD_SET(error_estimates) THEN BEGIN
        jerr   *= mapRatio
        jeErr  *= mapRatio
     ENDIF

  ENDIF

  ;;Get current (flip sign of current for electrons)
  cur            = j.y  * 1.6D-9 * (ions ? 1. : (-1.))
  IF KEYWORD_SET(error_estimates) THEN BEGIN
     ;;Don't flip sign, children
     curErr      = jerr  * 1.6D-9 ;* (ions ? 1. : (-1.))
  ENDIF

END
