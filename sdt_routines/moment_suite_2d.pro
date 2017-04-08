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
                    NEW_MOMENT_ROUTINE=new_moment_routine, $
                    QUIET=quiet, $
                    OUT_STRUCT=struct, $
                    OUTTIME=time, $
                    OUT_N=n, $
                    OUT_J_=j, $
                    OUT_JE=je, $
                    OUT_T=T, $
                    OUT_CHARE=charE, $
                    OUT_CURRENT=cur, $
                    OUT_JJE_COVAR=jje_coVar, $
                    OUT_PERPJ_=jPerp, $
                    OUT_PERPJE=jePerp, $
                    OUT_PERPCHARE=charEPerp, $
                    OUT_PERPCURRENT=curPerp, $
                    OUT_PERPJJE_COVAR=jjePerp_coVar, $
                    OUT_ERRORS=errors, $
                    OUT_ERR_N=nErr, $
                    OUT_ERR_J_=jErr, $
                    OUT_ERR_JE=jeErr, $
                    OUT_ERR_T=TErr, $
                    OUT_ERR_CURRENT=curErr, $
                    OUT_ERR_CHARE=charEErr, $
                    OUT_ERR_PERPJ_=jPerpErr, $
                    OUT_ERR_PERPJE=jePerpErr, $
                    OUT_ERR_PERPCURRENT=curPerpErr, $
                    OUT_ERR_PERPCHARE=charEPerpErr, $
                    OUT_MAPRATIO=mapRatio


  COMPILE_OPT IDL2,STRICTARRSUBS

  ions           = STRMATCH(STRUPCASE(eeb_or_ees),'IE*')

  ;;check--is charE getting special treatment?
  specialC       = KEYWORD_SET(aRange__charE)
  IF specialC THEN BEGIN
     
     ;;If aRange__charE is the same as aRange__moments, don't calculate charE quantities separately
     specialC    = KEYWORD_SET(aRange__moments) ? ~ARRAY_EQUAL(aRange__moments,aRange__charE) : 1

  ENDIF
  
  CASE 1 OF
     KEYWORD_SET(new_moment_routine): BEGIN

        moms = MOMENTS_2D_NEW__FROM_DIFF_EFLUX(diff_eFlux, $
                                               ENERGY=energy, $
                                               ANGLE=aRange__moments, $
                                               SC_POT=sc_pot, $
                                               EEB_OR_EES=eeb_or_ees, $
                                               QUIET=quiet)

        n    = moms.y[*].n
        j    = moms.y[*].j
        je   = moms.y[*].je
        jje  = moms.y[*].jje
        p    = moms.y[*].p
        T    = moms.y[*].T
        ;; v    = moms[*].v

        jPerp   = moms.y[*].perp.j
        jePerp  = moms.y[*].perp.je
        jjePerp = moms.y[*].perp.jje

        IF specialC THEN BEGIN
           momsC = MOMENTS_2D_NEW__FROM_DIFF_EFLUX(diff_eFlux, $
                                                   ENERGY=energy, $
                                                   ANGLE=aRange__charE, $
                                                   SC_POT=sc_pot, $
                                                   EEB_OR_EES=eeb_or_ees, $
                                                   QUIET=quiet)

           jC         = momsC.y[*].j
           jeC        = momsC.y[*].je
           jjeC       = momsC.y[*].jje

           jPerpC     = momsC.y[*].perp.j
           jePerpC    = momsC.y[*].perp.je
           jjePerpC   = momsC.y[*].perp.jje


        ENDIF ELSE BEGIN
           jC         = j 
           jeC        = je 
           jjeC       = jje 

           jPerpC     = jPerp
           jePerpC    = jePerp
           jjePerpC   = jjePerp
        ENDELSE

        jje_coVar     = jjeC-jeC*jC
        charE         = CHAR_ENERGY(TEMPORARY(jC),TEMPORARY(jeC))

        jjePerp_coVar = jjePerpC-jePerpC*jPerpC
        charEPerp     = CHAR_ENERGY(TEMPORARY(jPerpC),TEMPORARY(jePerpC))

        time          = moms.x
     END
     ELSE: BEGIN

        n              = (N_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                               ENERGY=energy, $
                                               ANGLE=aRange__moments, $
                                               SC_POT=sc_pot, $
                                               EEB_OR_EES=eeb_or_ees, $
                                               QUIET=quiet)).y
        T              = (T_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                               ENERGY=energy, $
                                               ANGLE=aRange__moments, $
                                               SC_POT=sc_pot, $
                                               EEB_OR_EES=eeb_or_ees, $
                                               QUIET=quiet)).y
        j              = (J_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                               ENERGY=energy, $
                                               ANGLE=aRange__moments, $
                                               SC_POT=sc_pot, $
                                               EEB_OR_EES=eeb_or_ees, $
                                               QUIET=quiet)).y
        je             = (JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                ENERGY=energy, $
                                                ANGLE=aRange__moments, $
                                                SC_POT=sc_pot, $
                                                EEB_OR_EES=eeb_or_ees, $
                                                QUIET=quiet)).y

        IF specialC THEN BEGIN
           jC          = (J_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                               ENERGY=energy, $
                                               ANGLE=aRange__charE, $
                                               SC_POT=sc_pot, $
                                               EEB_OR_EES=eeb_or_ees, $
                                               QUIET=quiet)).y
           jeC         = (JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                ENERGY=energy, $
                                                ANGLE=aRange__charE, $
                                                SC_POT=sc_pot, $
                                                EEB_OR_EES=eeb_or_ees, $
                                                QUIET=quiet)).y

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
                                                           QUIET=quiet))).y - jeC*jC

        ;; charE          = CHAR_ENERGY((TEMPORARY(jC)).y,(TEMPORARY(jeC)).y)
        charE          = CHAR_ENERGY(TEMPORARY(jC),TEMPORARY(jeC))

        time           = (diff_eFlux.time+diff_eFlux.end_time)/2.


        jPerp          = 0.D
        jePerp         = 0.D
        jjePerp_coVar  = 0.D
        charEPerp      = 0.D
        curPerp        = 0.D

        jPerpErr       = 0.D
        jePerpErr      = 0.D
        ;; jjePerp_coVar  = 0.D
        charEPerpErr   = 0.D
        curPerpErr     = 0.D
     END
  ENDCASE



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

     ERROR_CALC_2D,diff_eFlux,errors, $
                   N_=n, $
                   JF=j, $
                   JEF=je, $
                   T_=T, $
                   NERR=nerr, $
                   JERR=jerr, $
                   JEERR=jeErr, $
                   CHAREERR=charEErr, $
                   TERR=Terr, $
                   JJE_COVAR=jje_coVar, $ ;; , $
                   JPERPF=jPerp, $
                   JPERPEF=jePerp, $
                   JJEPERP_COVAR=jjePerp_coVar, $
                   PAR_ERRORS=parErrs, $
                   PERP_ERRORS=perpErrs
                   
     jPerpErr     = perpErrs.j
     jePerpErr    = perpErrs.je
     charEPerpErr = perpErrs.charE

     ;; ERROR_CALC_2D,diff_eFlux,errors,j,je,n,T,jerr,jeErr,nerr,Terr

  ENDIF

  GET_ALT_MLT_ILAT_FROM_FAST_EPHEM,orbit,time, $
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
     CASE SIZE(j,/TYPE) OF
        8: BEGIN
           j.y[flip]   *= (-1.)
           je.y[flip]  *= (-1.)
        END
        ELSE: BEGIN
           j[flip]   *= (-1.)
           je[flip]  *= (-1.)
        END
     ENDCASE
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

     CASE SIZE(j,/TYPE) OF
        8: BEGIN
           j.y  *= mapRatio
           je.y *= mapRatio
        END
        ELSE: BEGIN
           j    *= mapRatio
           je   *= mapRatio
        END
     ENDCASE

     IF KEYWORD_SET(error_estimates) THEN BEGIN
        jerr   *= mapRatio
        jeErr  *= mapRatio
     ENDIF

  ENDIF

  ;;Get current (flip sign of current for electrons)
  CASE SIZE(j,/TYPE) OF
     8: BEGIN

        cur = j.y  * 1.6D-9 * (ions ? 1. : (-1.))

        IF KEYWORD_SET(error_estimates) THEN BEGIN
           ;;Don't flip sign, children
           curErr      = jerr  * 1.6D-9 ;* (ions ? 1. : (-1.))
        ENDIF

     END
     ELSE: BEGIN

        cur     = j  * 1.6D-9 * (ions ? 1. : (-1.))
        curPerp = jPerp * 1.6D-9 * (ions ? 1. : (-1.))

        IF KEYWORD_SET(error_estimates) THEN BEGIN
           ;;Don't flip sign, children
           curErr      = jerr     * 1.6D-9 ;* (ions ? 1. : (-1.))
           curPerpErr  = jPerpErr * 1.6D-9 ;* (ions ? 1. : (-1.))
        ENDIF

     END
  ENDCASE
  
  IF ARG_PRESENT(struct) THEN BEGIN
     perp = {j : jPerp, $
             jePerp         : TEMPORARY(jePerp       ) , $
             charEPerp      : TEMPORARY(charEPerp    ) , $
             curPerp        : TEMPORARY(curPerp      ) , $
             jjePerp_coVar  : TEMPORARY(jjePerp_coVar) , $
             jPerpErr       : TEMPORARY(jPerpErr     ) , $
             jePerpErr      : TEMPORARY(jePerpErr    ) , $
             curPerpErr     : TEMPORARY(curPerpErr   ) , $
             charEPerpErr   : TEMPORARY(charEPerpErr )}

     struct = {time           : TEMPORARY(time     ), $
               n              : TEMPORARY(n        ), $
               j              : TEMPORARY(j        ), $
               je             : TEMPORARY(je       ), $
               T              : TEMPORARY(T        ), $
               charE          : TEMPORARY(charE    ), $
               cur            : TEMPORARY(cur      ), $
               jje_coVar      : TEMPORARY(jje_coVar), $
               errors         : TEMPORARY(errors   ), $
               nErr           : TEMPORARY(nErr     ), $
               jErr           : TEMPORARY(jErr     ), $
               jeErr          : TEMPORARY(jeErr    ), $
               TErr           : TEMPORARY(TErr     ), $
               curErr         : TEMPORARY(curErr   ), $
               charEErr       : TEMPORARY(charEErr ), $
               perp           : perp}
     
  ENDIF

END
