;2017/03/23
PRO DAT_EFLUX_TO_DIFF_EFLUX,dat_eFlux,diff_eFlux, $
                            ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                            FIT_EACH_ANGLE=fit_each_angle, $
                            SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                            ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                            TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
                            CALC_GEOM_FACTORS=calc_geom_factors
                            
  COMPILE_OPT IDL2,STRICTARRSUBS

  CASE 1 OF
     KEYWORD_SET(enforce_diff_eFlux_sRate): BEGIN
        
        times       = dat_eFlux.time
        nInds       = N_ELEMENTS(times)-1
        curInd      = 0
        startIndArr = !NULL
        endIndArr   = !NULL
        diffArr     = !NULL
        WHILE curInd LT nInds DO BEGIN
           nextInd = VALUE_CLOSEST2(times-times[curInd],enforce_diff_eFlux_sRate,/CONSTRAINED)

           ;; PRINT,FORMAT='(A25,TR5,I06,":",F8.3)',T2S(times[curInd],/MS),curInd,times[nextInd]-times[curInd]

           startIndArr = [startIndArr,curInd        ]
           endIndArr   = [endIndArr  ,nextInd-1     ]
           diffArr     = [diffArr    ,nextInd-curInd]
           curInd      = nextInd
           
           IF (times[-1] - times[curInd]) LT enforce_diff_eFlux_sRate THEN BREAK

        ENDWHILE

        tmpdat_eFlux = !NULL
        
        thisMany = N_ELEMENTS(startIndArr)
        FOR k=0,thisMany-1 DO BEGIN
           ;; DIAGNOSTIC MODE
           ;; ind1 = startIndArr[k]
           ;; ind2 = endIndArr[k]

           ;; PRINT,FORMAT='(I3,"–",I-3,TR5,":",F0.4,TR5,"(",A25,"–",A25,")")', $
           ;;       ind1,ind2, $
           ;;       dat_eFlux[ind2].end_time-dat_eFlux[ind1].time, $
           ;;       T2S(dat_eFlux[ind1].time,/MS), $
           ;;       T2S(dat_eFlux[ind2].end_time,/MS)

           ;; tmpdat_eFlux = [tmpdat_eFlux,AVERAGE_SUM3D(dat_eFlux[ind1:ind2],diffArr)]

           ;; PRODUCTION MODE
           tmpdat_eFlux = [tmpdat_eFlux,AVERAGE_SUM3D(dat_eFlux[startIndArr[k]:endIndArr[k]],5)]
        ENDFOR

        dat_eFlux = TEMPORARY(tmpdat_eFlux)
        ;; STOP
        
     END
     KEYWORD_SET(spectra_average_interval): BEGIN
        dat_eFlux   = AVERAGE_SUM3D(dat_eFlux,spectra_average_interval)
     END
     ELSE: STOP
  ENDCASE
     
  shiftVals      = !NULL
  FOR i=0,N_ELEMENTS(dat_eFlux)-1 DO BEGIN
     tempDat     = dat_eFlux[i]

     CASE 1 OF
        KEYWORD_SET(fit_each_angle) OR (N_ELEMENTS(fit_each_angle) EQ 0): BEGIN
           tempdiff_eflux = PREP_EFLUX_DATA(tempDat, $
                                            CALIB=calc_geom_factors, $
                                            UNITS=units, $          
                                            RETRACE=1, $ ;;Look up 'retrace' in Carlson et al. [2001] if you want to know why
                                            VEL=vel, $
                                            ANGLE=an, $
                                            ARANGE=ar, $
                                            BINS=bins, $
                                            NO_SORT=no_sort)
        END
        KEYWORD_SET(try_synthetic_SDT_struct): BEGIN
           SPEC2D,tempDat,UNITS='eflux',ANGLE=angle, $
                  OUT_DAT=tempDiff_eFlux, $ ;RETRACE=1, $
                  OUT_FASTSTR=tempDiff_eFluxSDT, $
                  /NO_PLOT
        END
        ELSE: BEGIN
           SPEC2D,tempDat,UNITS='eflux',ANGLE=angle, $
                  OUT_DAT=tempDiff_eFlux, $
                  /NO_PLOT
        END
     ENDCASE

     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
        REDUCE_EFLUX_TO_MIN_ANGLE,tempDiff_eFlux     
        shiftVals                             = [shiftVals,0]
     ENDIF                      ;ELSE BEGIN

     IF ~(KEYWORD_SET(fit_each_angle) OR (N_ELEMENTS(fit_each_angle) EQ 0)) AND $
        ~KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
        shiftMe                            = WHERE(tempDiff_eFlux.angles LT 0)
        shiftVal                           = MAX(shiftMe)
        shiftVals                          = [shiftVals,shiftVal]
        IF shiftVal NE -1 THEN BEGIN

           tempDiff_eFlux.angles[shiftMe]  = tempDiff_eFlux.angles[shiftMe]+360.

           tempDiff_eFlux.angles           = SHIFT(tempDiff_eFlux.angles,(-1)*shiftVal-1)
           tempDiff_eFlux.x                = SHIFT(tempDiff_eFlux.x,0,(-1)*shiftVal-1)
           tempDiff_eFlux.y                = SHIFT(tempDiff_eFlux.y,0,(-1)*shiftVal-1)
        ENDIF

        IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
           shiftMe                            = WHERE(tempDiff_eFluxSDT.theta LT 0)
           shiftVal                           = MAX(shiftMe)
           shiftVals                          = [shiftVals,shiftVal]
           IF shiftVal NE -1 THEN BEGIN

              tempDiff_eFluxSDT.theta[shiftMe]   = tempDiff_eFluxSDT.theta[shiftMe]+360.

              tempDiff_eFluxSDT.theta            = SHIFT(tempDiff_eFluxSDT.theta,(-1)*shiftVal-1)
              tempDiff_eFluxSDT.energy           = SHIFT(tempDiff_eFluxSDT.energy,0,(-1)*shiftVal-1)
              tempDiff_eFluxSDT.data             = SHIFT(tempDiff_eFluxSDT.data,0,(-1)*shiftVal-1)
           ENDIF

           ADD_EFLUX_TO_EFLUX_STRUCT,diff_eFluxSDT,tempDiff_eFluxSDT, $
                                     ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                     TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct


        ENDIF
     ENDIF

     ADD_EFLUX_TO_EFLUX_STRUCT,diff_eFlux,tempDiff_eFlux, $
                               ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                               FIT_EACH_ANGLE=fit_each_angle


  ENDFOR

END
