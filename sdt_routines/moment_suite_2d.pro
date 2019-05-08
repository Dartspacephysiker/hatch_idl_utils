;2017/03/21
PRO FLIPCURRENTS,j,jPerp,je,jePerp,jerr,jeErr,jPerpErr,jePerpErr,flip,nFlip,mapRatio, $
                 IONS=ions, $
                 ERROR_ESTIMATES=error_estimates, $
                 MAP_TO_100KM=map_to_100km, $
                 OUT_CURRENT=cur, $
                 OUT_CURERR=curErr, $
                 OUT_CURPERP_=curPerp, $
                 OUT_CURPERPERR=curPerpErr


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
        curPerp = N_ELEMENTS(jPerp) GT 0 ? (jPerp * 1.6D-9 * (ions ? 1. : (-1.))) : cur*0.0D

        IF KEYWORD_SET(error_estimates) THEN BEGIN
           ;;Don't flip sign, children
           curErr      = jerr     * 1.6D-9 ;* (ions ? 1. : (-1.))
           curPerpErr  = N_ELEMENTS(jPerpErr) GT 0 ? (jPerpErr * 1.6D-9) : curErr*0.0D ;* (ions ? 1. : (-1.))
        ENDIF

     END
  ENDCASE

END
PRO MOMENT_SUITE_2D,diff_eFlux, $
                    ENERGY=energy, $
                    ARANGE__DENS=aRange__dens, $
                    ARANGE__MOMENTS=aRange__moments, $
                    ARANGE__CHARE=aRange__charE, $
                    ARANGE__TEMP=aRange__temp, $
                    ERANGE__TEMP=eRange__temp, $
                    SC_POT=sc_pot, $
                    EEB_OR_EES=eeb_or_ees, $
                    ERROR_ESTIMATES=error_estimates, $
                    MAP_TO_100KM=map_to_100km, $ 
                    ORBIT=orbit, $
                    NEW_MOMENT_ROUTINE=new_moment_routine, $
                    QUIET=quiet, $
                    MCFADDEN_STYLE_DIFF_EFLUX=McFadden_style_diff_eFlux, $
                    PROVIDING_EPHEM_INFO=providing_ephem_info, $
                    DONT_FLIP_SIGNS_OF_SOUTHHEMI_MOMENTS=dont_flip_signs_of_SH_moments, $
                    IN_ALT=alt, $
                    IN_ILAT=ilat, $
                    IN_MLT=mlt, $
                    INOUT_MAPRATIO=mapRatio, $
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
                    BATCH_MODE=batch_mode

  COMPILE_OPT IDL2,STRICTARRSUBS

  ions           = STRMATCH(STRUPCASE(eeb_or_ees),'IE*')

  ;;check--is charE getting special treatment?
  specialC       = KEYWORD_SET(aRange__charE)
  specialN       = KEYWORD_SET(aRange__dens )
  specialT       = KEYWORD_SET(eRange__temp ) OR KEYWORD_SET(aRange__temp )
  IF specialC THEN BEGIN
     
     ;;If aRange__charE is the same as aRange__moments, don't calculate charE quantities separately
     specialC    = KEYWORD_SET(aRange__moments) ? ~ARRAY_EQUAL(aRange__moments,aRange__charE) : 1

  ENDIF

  IF specialN THEN BEGIN
     
     ;;If aRange__dens is the same as aRange__moments, don't calculate density quantities separately
     specialN    = KEYWORD_SET(aRange__dens) ? ~ARRAY_EQUAL(aRange__moments,aRange__dens) : 1

  ENDIF  

  IF specialT THEN BEGIN
     
     ;;If eRange__temp is the same as energy and aRange__temp is the same as aRange__moments, don't calculate temperature quantities separately
     specialT    = (KEYWORD_SET(aRange__temp) ? ~ARRAY_EQUAL(aRange__moments,aRange__temp) : 1)
     IF ~specialT THEN specialT = (KEYWORD_SET(eRange__temp) ? ~ARRAY_EQUAL(energy,eRange__temp) : 0)

  ENDIF
  CASE 1 OF
     KEYWORD_SET(new_moment_routine): BEGIN

        moms = MOMENTS_2D_NEW__FROM_DIFF_EFLUX(diff_eFlux, $
                                               ENERGY=energy, $
                                               ANGLE=aRange__moments, $
                                               SC_POT=sc_pot, $
                                               EEB_OR_EES=eeb_or_ees, $
                                               MCFADDEN_STYLE_DIFF_EFLUX=McFadden_style_diff_eFlux, $
                                               QUIET=quiet)

        ;; 2017/12/05 You are here, working on orbit 1533 via PROCESS_MCFADDEN_STYLE_ESA_STRUCTS
        ;; this = WHERE(moms.y[*].n GE 2)
        ;; FOR k=0,N_ELEMENTS(this)-1 DO PRINT,t2s(moms.x[this[k]]),moms.y[this[k]].n

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

        jAll    = moms.y[*].all.j
        jeAll   = moms.y[*].all.je
        charEAll= moms.y[*].all.charE
        speedAll= moms.y[*].all.speed

        IF specialC THEN BEGIN
           momsC = MOMENTS_2D_NEW__FROM_DIFF_EFLUX(diff_eFlux, $
                                                   ENERGY=energy, $
                                                   ANGLE=aRange__charE, $
                                                   SC_POT=sc_pot, $
                                                   EEB_OR_EES=eeb_or_ees, $
                                                   MCFADDEN_STYLE_DIFF_EFLUX=McFadden_style_diff_eFlux, $
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

        IF specialN THEN BEGIN

           momsSC = MOMENTS_2D_NEW__FROM_DIFF_EFLUX(diff_eFlux, $
                                                    ENERGY=energy, $
                                                    ANGLE=aRange__dens, $
                                                    SC_POT=sc_pot, $
                                                    EEB_OR_EES=eeb_or_ees, $
                                                    MCFADDEN_STYLE_DIFF_EFLUX=McFadden_style_diff_eFlux, $
                                                    QUIET=quiet)

           nSC    = momsSC.y[*].n
           TSC    = momsSC.y[*].T

           jSC         = momsSC.y[*].j
           jeSC        = momsSC.y[*].je
           jjeSC       = momsSC.y[*].jje

           jPerpSC     = momsSC.y[*].perp.j
           jePerpSC    = momsSC.y[*].perp.je
           jjePerpSC   = momsSC.y[*].perp.jje

           jje_coVarSC = jjeSC-jeSC*jSC
           charESC     = CHAR_ENERGY(jSC,jeSC)
        
           jjePerp_coVarSC = jjePerpSC-jePerpSC*jPerpSC
           charEPerpSC     = CHAR_ENERGY(jPerpSC,jePerpSC)

           jAllSC     = momsSC.y[*].all.j
           jeAllSC    = momsSC.y[*].all.je
           charEAllSC = momsSC.y[*].all.charE
           speedAllSC = momsSC.y[*].all.speed

        ENDIF

        IF specialT THEN BEGIN

           moms_T = MOMENTS_2D_NEW__FROM_DIFF_EFLUX(diff_eFlux, $
                                                    ENERGY=eRange__temp, $
                                                    ANGLE=aRange__temp, $
                                                    SC_POT=sc_pot, $
                                                    EEB_OR_EES=eeb_or_ees, $
                                                    MCFADDEN_STYLE_DIFF_EFLUX=McFadden_style_diff_eFlux, $
                                                    QUIET=quiet)

           n_T    = moms_T.y[*].n
           T_T    = moms_T.y[*].T

           j_T         = moms_T.y[*].j
           je_T        = moms_T.y[*].je
           jje_T       = moms_T.y[*].jje

           jPerp_T     = moms_T.y[*].perp.j
           jePerp_T    = moms_T.y[*].perp.je
           jjePerp_T   = moms_T.y[*].perp.jje

           jje_coVar_T = jje_T-je_T*j_T
           charE_T     = CHAR_ENERGY(j_T,je_T)
        
           jjePerp_coVar_T = jjePerp_T-jePerp_T*jPerp_T
           charEPerp_T     = CHAR_ENERGY(jPerp_T,jePerp_T)

           jAll_T     = moms_T.y[*].all.j
           jeAll_T    = moms_T.y[*].all.je
           charEAll_T = moms_T.y[*].all.charE
           speedAll_T = moms_T.y[*].all.speed

        ENDIF

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

        IF specialT THEN BEGIN

           PRINT,"You've not progged this!"
           STOP
        ENDIF

        jje_coVar      = (TEMPORARY(JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                           ENERGY=energy, $
                                                           ANGLE=aRange__charE, $
                                                           SC_POT=sc_pot, $
                                                           EEB_OR_EES=eeb_or_ees, $
                                                           /JJE, $
                                                           QUIET=quiet))).y - jeC*jC

        ;; charE          = CHAR_ENERGY((TEMPORARY(jC)).y,(TEMPORARY(jeC)).y)
        charE          = CHAR_ENERGY(TEMPORARY(jC),TEMPORARY(jeC))

        ;; time           = (diff_eFlux.time+diff_eFlux.end_time)/2.
        time           = diff_eFlux.time

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

        jAll           = 0.D
        jeAll          = 0.D
        charEAll       = 0.D
        speedAll       = 0.D
        
        IF specialN THEN BEGIN

           nSC         = (N_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                ENERGY=energy, $
                                                ANGLE=aRange__dens, $
                                                SC_POT=sc_pot, $
                                                EEB_OR_EES=eeb_or_ees, $
                                                QUIET=quiet)).y

           jSC         = (J_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                ENERGY=energy, $
                                                ANGLE=aRange__dens, $
                                                SC_POT=sc_pot, $
                                                EEB_OR_EES=eeb_or_ees, $
                                                QUIET=quiet)).y

           jeSC        = (JE_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                 ENERGY=energy, $
                                                 ANGLE=aRange__dens, $
                                                 SC_POT=sc_pot, $
                                                 EEB_OR_EES=eeb_or_ees, $
                                                 QUIET=quiet)).y


           TSC         = (T_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                ENERGY=energy, $
                                                ANGLE=aRange__dens, $
                                                SC_POT=sc_pot, $
                                                EEB_OR_EES=eeb_or_ees, $
                                                QUIET=quiet)).y


           charESC     = CHAR_ENERGY(TEMPORARY(jSC),TEMPORARY(jeSC))


           jAllSC      = 0.D
           jeAllSC     = 0.D
           charEAllSC  = 0.D
           speedAllSC  = 0.D

        ENDIF
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
                                                    /QUIET, $
                                                    MCFADDEN_STYLE_DIFF_EFLUX=McFadden_style_diff_eFlux)

     ;; IF KEYWORD_SET(dens_errors) THEN BEGIN
     ;; nerr     = MAKE_ARRAY(nHere,/FLOAT)
     ;; jerr     = MAKE_ARRAY(nHere,/FLOAT)
     ;; Terr     = MAKE_ARRAY(nHere,/FLOAT)

     ERROR_CALC_2D,errors, $
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

     IF specialN THEN BEGIN

        errorsSC      = MOMENTERRORS_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                         ENERGY=energy, $
                                                         ANGLE=aRange__dens, $
                                                         SC_POT=sc_pot, $
                                                         EEB_OR_EES=eeb_or_ees, $
                                                         /PRESSURE_COVAR_CALC, $
                                                         /HEATFLUX_COVAR_CALC, $
                                                         QUIET=quiet, $
                                                         MCFADDEN_STYLE_DIFF_EFLUX=McFadden_style_diff_eFlux)

        ;;NOTE, this uses the wrong j, je, and T! They correspond to aRange__moments, not aRange__dens!
        ;;I'm not updating it because they won't affect nErr. They WOULD affect other the calculated uncertainty
        ;; of other moments, of course.
        ERROR_CALC_2D,errorsSC, $
                      N_=nSC, $
                      JF=jSC, $
                      JEF=jeSC, $
                      T_=TSC, $
                      JJE_COVAR=jje_coVarSC, $ ;; , $
                      NERR=nerrSC, $
                      JERR=jerrSC, $
                      JEERR=jeErrSC, $
                      CHAREERR=charEErrSC, $
                      TERR=TerrSC, $
                      PERP_ERRORS=perpErrsSC

        jPerpErrSC     = perpErrsSC.j
        jePerpErrSC    = perpErrsSC.je
        charEPerpErrSC = perpErrsSC.charE

     ENDIF

     IF specialT THEN BEGIN

        errors_T      = MOMENTERRORS_2D__FROM_DIFF_EFLUX(diff_eFlux, $
                                                         ENERGY=eRange__temp, $
                                                         ANGLE=aRange__temp, $
                                                         SC_POT=sc_pot, $
                                                         EEB_OR_EES=eeb_or_ees, $
                                                         /PRESSURE_COVAR_CALC, $
                                                         /HEATFLUX_COVAR_CALC, $
                                                         QUIET=quiet, $
                                                         MCFADDEN_STYLE_DIFF_EFLUX=McFadden_style_diff_eFlux)

        ;;NOTE, this uses the wrong j, je, and T! They correspond to aRange__moments, not aRange__dens!
        ;;I'm not updating it because they won't affect nErr. They WOULD affect other the calculated uncertainty
        ;; of other moments, of course.
        ERROR_CALC_2D,errors_T, $
                      N_=n_T, $
                      JF=j_T, $
                      JEF=je_T, $
                      T_=T_T, $
                      JJE_COVAR=jje_coVar_T, $ ;; , $
                      NERR=nerr_T, $
                      JERR=jerr_T, $
                      JEERR=jeErr_T, $
                      CHAREERR=charEErr_T, $
                      TERR=Terr_T, $
                      PERP_ERRORS=perpErrs_T

        jPerpErr_T     = perpErrs_T.j
        jePerpErr_T    = perpErrs_T.je
        charEPerpErr_T = perpErrs_T.charE

     ENDIF

     ;; ERROR_CALC_2D,errors,j,je,n,T,jerr,jeErr,nerr,Terr

  ENDIF ELSE BEGIN
     errors          = !VALUES.F_NaN

     nerr        = !VALUES.F_NaN    
     jerr        = !VALUES.F_NaN
     jeErr       = !VALUES.F_NaN
     charEErr    = !VALUES.F_NaN
     Terr        = !VALUES.F_NaN
     jje_coVar   = !VALUES.F_NaN
     jPerp       = !VALUES.F_NaN
     jePerp      = !VALUES.F_NaN
     jjePerp_coVa= !VALUES.F_NaN
     parErrs     = !VALUES.F_NaN
     perpErrs    = !VALUES.F_NaN

     jje_coVar_T = !VALUES.F_NaN
     nerr_T      = !VALUES.F_NaN
     jerr_T      = !VALUES.F_NaN
     jeErr_T     = !VALUES.F_NaN
     charEErr_T  = !VALUES.F_NaN
     Terr_T      = !VALUES.F_NaN
     perpErrs_T  = !VALUES.F_NaN

     jje_coVarSC = !VALUES.F_NaN
     nerrSC      = !VALUES.F_NaN
     jerrSC      = !VALUES.F_NaN
     jeErrSC     = !VALUES.F_NaN
     charEErrSC  = !VALUES.F_NaN
     TerrSC      = !VALUES.F_NaN
     perpErrsSC  = !VALUES.F_NaN

     jPerpErr        = !VALUES.F_NaN
     jePerpErr       = !VALUES.F_NaN
     charEPerpErr    = !VALUES.F_NaN
     errorsSC        = !VALUES.F_NaN
     jPerpErrSC      = !VALUES.F_NaN
     jePerpErrSC     = !VALUES.F_NaN
     charEPerpErrSC  = !VALUES.F_NaN
     errors_T        = !VALUES.F_NaN
     jPerpErr_T      = !VALUES.F_NaN
     jePerpErr_T     = !VALUES.F_NaN
     charEPerpErr_T  = !VALUES.F_NaN

     curErr          = !VALUES.F_NaN
     curPerpErr      = !VALUES.F_NaN 

     curErrSC        = !VALUES.F_NaN
     curPerpErrSC    = !VALUES.F_NaN

     curPerpErr_T    = !VALUES.F_NaN
     curErr_T        = !VALUES.F_NaN

  ENDELSE

  IF ~(KEYWORD_SET(providing_ephem_info) AND $
       N_ELEMENTS(alt      ) GT 0        AND $
       N_ELEMENTS(ilat     ) GT 0        AND $
       N_ELEMENTS(mlt      ) GT 0        AND $
       N_ELEMENTS(mapRatio ) GT 0           ) $
  THEN BEGIN

     GET_ALT_MLT_ILAT_FROM_FAST_EPHEM,orbit,time, $
                                      OUT_TSORTED_I=tSort_i, $
                                      OUT_ALT=alt, $
                                      OUT_MLT=mlt, $
                                      OUT_ILAT=ilat, $
                                      OUT_MAPRATIO=mapRatio, $
                                      OUT_NEVENTS=nEvents, $
                                      LOGLUN=logLun
     IF N_ELEMENTS(tSort_i) GT 0 THEN IF ~KEYWORD_SET(batch_mode) THEN STOP

  ENDIF

  ;; The following flips signs of moments for SH observations so that
  ;; earthward is pos and anti-earthward is neg for everyone
  IF ~KEYWORD_SET(dont_flip_signs_of_SH_moments) THEN BEGIN


     north_south     = ABS(ilat)/ilat

     flip            = WHERE(north_south LT 0,nFlip)

     FLIPCURRENTS,j,jPerp,je,jePerp,jerr,jeErr,jPerpErr,jePerpErr,flip,nFlip,mapRatio, $
                  IONS=ions, $
                  ERROR_ESTIMATES=error_estimates, $
                  MAP_TO_100KM=map_to_100km, $
                  OUT_CURRENT=cur, $
                  OUT_CURERR=curErr, $
                  OUT_CURPERP_=curPerp, $
                  OUT_CURPERPERR=curPerpErr
  ENDIF

  IF specialN THEN BEGIN

     FLIPCURRENTS,jSC,jPerpSC,jeSC,jePerpSC,jerrSC,jeErrSC,jPerpErrSC,jePerpErrSC,flip,nFlip,mapRatio, $
                  IONS=ions, $
                  ERROR_ESTIMATES=error_estimates, $
                  MAP_TO_100KM=map_to_100km, $
                  OUT_CURRENT=curSC, $
                  OUT_CURERR=curErrSC, $
                  OUT_CURPERP_=curPerpSC, $
                  OUT_CURPERPERR=curPerpErrSC

     all  = {j          : TEMPORARY(jAllSC), $
             je         : TEMPORARY(jeAllSC     ) , $
             charE      : TEMPORARY(charEAllSC  ), $
             speed      : TEMPORARY(speedAllSC)}
             ;; jjePerp_coVar  : TEMPORARY(jjePerp_coVar)}

     perp = {j              : jPerpSC, $
             jePerp         : TEMPORARY(jePerpSC     ) , $
             charEPerp      : TEMPORARY(charEPerpSC  ) , $
             curPerp        : TEMPORARY(curPerpSC    ) , $
             jjePerp_coVar  : TEMPORARY(jjePerp_coVarSC) , $
             jPerpErr       : TEMPORARY(jPerpErrSC   ) , $
             jePerpErr      : TEMPORARY(jePerpErrSC  ) , $
             curPerpErr     : TEMPORARY(curPerpErrSC ) , $
             charEPerpErr   : TEMPORARY(charEPerpErrSC)}

     source = {n              : TEMPORARY(nSC      ), $
               j              : TEMPORARY(jSC      ), $
               je             : TEMPORARY(jeSC     ), $
               T              : TEMPORARY(TSC      ), $
               charE          : TEMPORARY(charESC  ), $
               cur            : TEMPORARY(curSC    ), $
               jje_coVar      : TEMPORARY(jje_coVarSC), $
               errors         : TEMPORARY(errorsSC ), $
               nErr           : TEMPORARY(nErrSC   ), $
               jErr           : TEMPORARY(jErrSC   ), $
               jeErr          : TEMPORARY(jeErrSC  ), $
               TErr           : TEMPORARY(TErrSC   ), $
               curErr         : TEMPORARY(curErrSC ), $
               charEErr       : TEMPORARY(charEErrSC), $
               perp           : TEMPORARY(perp), $
               all            : TEMPORARY(all)}

  ENDIF

  IF specialT THEN BEGIN

     FLIPCURRENTS,j_T,jPerp_T,je_T,jePerp_T,jerr_T,jeErr_T,jPerpErr_T,jePerpErr_T,flip,nFlip,mapRatio, $
                  IONS=ions, $
                  ERROR_ESTIMATES=error_estimates, $
                  MAP_TO_100KM=map_to_100km, $
                  OUT_CURRENT=cur_T, $
                  OUT_CURERR=curErr_T, $
                  OUT_CURPERP_=curPerp_T, $
                  OUT_CURPERPERR=curPerpErr_T

     all  = {j          : TEMPORARY(jAll_T), $
             je         : TEMPORARY(jeAll_T     ) , $
             charE      : TEMPORARY(charEAll_T  ), $
             speed      : TEMPORARY(speedAll_T)}
             ;; jjePerp_coVar  : TEMPORARY(jjePerp_coVar)}

     perp = {j              : jPerp_T, $
             jePerp         : TEMPORARY(jePerp_T     ) , $
             charEPerp      : TEMPORARY(charEPerp_T  ) , $
             curPerp        : TEMPORARY(curPerp_T    ) , $
             jjePerp_coVar  : TEMPORARY(jjePerp_coVar_T) , $
             jPerpErr       : TEMPORARY(jPerpErr_T   ) , $
             jePerpErr      : TEMPORARY(jePerpErr_T  ) , $
             curPerpErr     : TEMPORARY(curPerpErr_T ) , $
             charEPerpErr   : TEMPORARY(charEPerpErr_T)}

     moments_forT = {n              : TEMPORARY(n_T      ), $
                     j              : TEMPORARY(j_T      ), $
                     je             : TEMPORARY(je_T     ), $
                     T              : TEMPORARY(T_T      ), $
                     charE          : TEMPORARY(charE_T  ), $
                     cur            : TEMPORARY(cur_T    ), $
                     jje_coVar      : TEMPORARY(jje_coVar_T), $
                     errors         : TEMPORARY(errors_T ), $
                     nErr           : TEMPORARY(nErr_T   ), $
                     jErr           : TEMPORARY(jErr_T   ), $
                     jeErr          : TEMPORARY(jeErr_T  ), $
                     TErr           : TEMPORARY(TErr_T   ), $
                     curErr         : TEMPORARY(curErr_T ), $
                     charEErr       : TEMPORARY(charEErr_T), $
                     perp           : TEMPORARY(perp), $
                     all            : TEMPORARY(all)}

  ENDIF
  
  IF ARG_PRESENT(struct) THEN BEGIN

     all  = {j          : TEMPORARY(jAll), $
             je         : TEMPORARY(jeAll       ) , $
             charE      : TEMPORARY(charEAll    ), $
             speed      : TEMPORARY(speedAll)}
             ;; jjePerp_coVar  : TEMPORARY(jjePerp_coVar)}

     perp = {j          : TEMPORARY(jPerp), $
             je         : TEMPORARY(jePerp       ) , $
             charE      : TEMPORARY(charEPerp    ) , $
             cur        : TEMPORARY(curPerp      ) , $
             jje_coVar  : TEMPORARY(jjePerp_coVar) , $
             jErr       : TEMPORARY(jPerpErr     ) , $
             jeErr      : TEMPORARY(jePerpErr    ) , $
             curErr     : TEMPORARY(curPerpErr   ) , $
             charEErr   : TEMPORARY(charEPerpErr )}

     struct = {time           : TEMPORARY(time     ), $
               n              : TEMPORARY(n        ), $
               j              : TEMPORARY(j        ), $
               je             : TEMPORARY(je       ), $
               mapRatio       : mapRatio            , $
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
               perp           : TEMPORARY(perp), $
               all            : TEMPORARY(all), $
               info           :{ is_mapped            : KEYWORD_SET(map_to_100km), $
                                 errors_are_fractional : 0B, $
                                 only_leeward_ions     : 0B}}
     
     IF N_ELEMENTS(source) GT 0 THEN BEGIN

        STR_ELEMENT,struct,'source',TEMPORARY(source),/ADD_REPLACE

     ENDIF

     IF N_ELEMENTS(moments_forT) GT 0 THEN BEGIN

        STR_ELEMENT,struct,'moments_forT',TEMPORARY(moments_forT),/ADD_REPLACE

     ENDIF

  ENDIF

END
