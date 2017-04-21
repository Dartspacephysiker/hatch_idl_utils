;2017/04/21
PRO GET_FA_FIELD_LINE,tee1,tee2, $
                      USEINDS=useInds, $
                      TIME_ARRAY=time_array, $
                      MRATIO=mRatio

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__jv_curve_fit__tie_r_b_and_dens.pro
  ;; COMMON tieRB,tRB_RBpairs,tRB_fLine,tRB_nFAST,tRB_nFLine,tRB_fLineRE

  RE_to_km        = 6374.D
  ;; RLimFull        = 3000.D ;;For full field line 

  traceErr = 0.0001D             ;Check out http://geo.phys.spbu.ru/~tsyganenko/Examples1_and_2.html. 0.0001 is what Tsyganenko himself uses. Baller, right?
  dsMax    = 0.05D                ;Max R_E step size

  __TRACE_ANTIPARALLEL_B = 1
  __TRACE_PARALLEL_B     = -1

  useInds2 = KEYWORD_SET(useInds) ? useInds : LINDGEN(N_ELEMENTS(tee1))

  gaveStr  = 0
  CASE 1 OF
     KEYWORD_SET(time_array): BEGIN

        CASE SIZE(tee1,/TYPE) OF
           7: BEGIN
              t1      = STR_TO_TIME(tee1)
              ;; t1Str   = TEMPORARY(tee1)
              gaveStr = 1
           END
           5: BEGIN
              ;; t1Str   = TIME_TO_STR(tee1,/MS)
              t1      = TEMPORARY(tee1)
           END
           ELSE: STOP
        ENDCASE

        t1Str         = TIME_TO_STR(MEAN(t1[useInds2]),/MS)

        majicInd      = ROUND(MEAN(useInds2))

     END
     ;; ELSE: BEGIN

     ;;    st1 = SIZE(tee1,/TYPE)
     ;;    st2 = SIZE(tee2,/TYPE)

     ;;    IF (st1 NE st2) AND (st2 NE 0) THEN STOP

     ;;    CASE st1 OF
     ;;       7: BEGIN
     ;;          t1      = STR_TO_TIME(tee1)
     ;;          t1Str   = TEMPORARY(tee1)
     ;;          gaveStr = 1
     ;;       END
     ;;       ELSE: BEGIN
     ;;          t1Str   = TIME_TO_STR(tee1,/MS)
     ;;          t1      = TEMPORARY(tee1)
     ;;       END
     ;;    ENDCASE

     ;;    CASE st2 OF
     ;;       7: BEGIN
     ;;          t2      = STR_TO_TIME(tee2)
     ;;          t2Str   = TEMPORARY(tee2)
     ;;          gaveStr = 1
     ;;       END
     ;;       0:
     ;;       5: BEGIN
     ;;          t2Str   = TIME_TO_STR(tee2,/MS)
     ;;          t2      = TEMPORARY(tee2)
     ;;       END
     ;;       ELSE: STOP
     ;;    ENDCASE

     ;; END
  ENDCASE

  swdat         = GET_SW_CONDS_UTC(t1Str,t2Str, $
                                   TIME_ARRAY=time_array, $
                                   /REABERRATE_VY)

  CONVERT_TIME_STRING_TO_YMDHMS_ARRAYS, $
     t1Str, $
     OUT_YEARARR=yearArr, $
     OUT_DOYARR=DOYArr, $
     OUT_MONTHARR=monthArr, $
     OUT_DAYARR=dayArr, $
     OUT_HOURARR=hourArr, $
     OUT_MINARR=minArr, $
     OUT_SECARR=secArr

  DOYStruct = {year: yearArr, $
               DOY:  DOYArr, $
               month: monthArr, $
               day: dayArr, $
               hour: hourArr, $
               min: minArr, $
               sec: secArr}

  ;; GET_FA_ORBIT,t1,t2, $
  ;;              TIME_ARRAY=time_array, $
  ;;              /NO_STORE, $
  ;;              STRUC=struc

  k = 0

  ;; GEOPACK_RECALC_08,YearArr[k],MonthArr[k],DayArr[k], $
  ;;                   HourArr[k],MinArr[k],SecArr[k], $
  GEOPACK_RECALC_08,YearArr,MonthArr,DayArr, $
                    HourArr,MinArr,SecArr, $
                    /DATE, $
                    ;; VGSE=swDat.v_SW[*,k], $
                    VGSE=swDat.v_SW[*], $
                    TILT=thisTilt


  ;; nIter         = 1
  ;; time_epoch    = MAKE_ARRAY(nIter,/DOUBLE,VALUE=0.0D)
  ;; FOR k=0,nIter-1 DO BEGIN
     GEOPACK_EPOCH,tmpEpoch,YearArr,DOYArr, $
                   HourArr,MinArr,FLOOR(SecArr),SecArr-FLOOR(SecArr), $
                   /DOY, $
                   /COMPUTE_EPOCH
     time_epoch  = TEMPORARY(tmpEpoch)
  ;; ENDFOR
  
  CASE 1 OF
     KEYWORD_SET(mRatio.BModelInfo.T89): BEGIN
        tmpParMod = mRatio.BModelInfo.IOPT_89[majicInd]

        T89       = 1
        T96       = 0
        T01       = 0
        TS04      = 0

     END
     KEYWORD_SET(mRatio.BModelInfo.T96): BEGIN
        tmpParMod = mRatio.BModelInfo.parMod[*,majicInd]

        T89       = 0
        T96       = 1
        T01       = 0
        TS04      = 0

     END
     KEYWORD_SET(mRatio.BModelInfo.T01): BEGIN
        tmpParMod = mRatio.BModelInfo.parMod[*,majicInd]
        tmpParMod[4:5] = mRatio.BModelInfo.GParms[majicInd,*]

        T89       = 0
        T96       = 0
        T01       = 1
        TS04      = 0

     END
     KEYWORD_SET(mRatio.BModelInfo.TS04): BEGIN
        tmpParMod = mRatio.BModelInfo.parMod[*,majicInd]

        T89       = 0
        T96       = 0
        T01       = 0
        TS04      = 1

     END
  ENDCASE

  IF N_ELEMENTS(TS04) EQ 0 AND N_ELEMENTS(IGRF) EQ 0 AND N_ELEMENTS(T01) EQ 0 THEN IGRF = 1

  ;; IF nIter GT 1 THEN BEGIN
     tmpPos = [mRatio.fa_pos[majicInd,0],mRatio.fa_pos[majicInd,1],mRatio.fa_pos[majicInd,2]]
  ;; ENDIF ELSE BEGIN
  ;;    tmpPos = [mRatio.fa_pos[0],mRatio.fa_pos[1],mRatio.fa_pos[2]]
  ;; ENDELSE

  ;; IF mRatio.ilat[k] GT 0 THEN BEGIN
  IF mRatio.ilat[majicInd] GT 0 THEN BEGIN
     trace_to_equator = __TRACE_ANTIPARALLEL_B
     trace_to_ionos   = __TRACE_PARALLEL_B
  ENDIF ELSE BEGIN
     trace_to_equator = __TRACE_PARALLEL_B
     trace_to_ionos   = __TRACE_ANTIPARALLEL_B
  ENDELSE


  GEOPACK_CONV_COORD_08,tmpPos[0],tmpPos[1],tmpPos[2], $
                        FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                        /FROM_GSE,/TO_GSW,EPOCH=time_epoch


  GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                   ;; trace_to_ionos,(KEYWORD_SET(TS04) ? tmpParMod : !NULL), $
                   trace_to_ionos,tmpParMod, $
                   ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                   /REFINE, $
                   R0=R0, $
                   ;; RLIM=RLimFull, $
                   FLINE=fLine_toIonosFull, $
                   /IONOSPHERE, $
                   T89=T89, $
                   T96=T96, $
                   T01=T01, $
                   TS04=TS04, $
                   IGRF=IGRF, $
                   ;; /IGRF, $
                   TILT=thisTilt, $ ;should be in degrees
                   EPOCH=time_epoch, $
                   DSMAX=dsMax, $
                   ERR=traceErr

  ;;Get FAST field stuff
  ;;Calculate external contribution
  CASE 1 OF
     KEYWORD_SET(T89): BEGIN

        GEOPACK_T89,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                    FAST_Bx,FAST_By,FAST_Bz, $
                    TILT=thisTilt, $
                    EPOCH=time_epoch

        GEOPACK_T89,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                    ionos_Bx,ionos_By,ionos_Bz, $
                    TILT=thisTilt, $
                    EPOCH=time_epoch


     END
     KEYWORD_SET(T96): BEGIN

        GEOPACK_T96,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                    FAST_Bx,FAST_By,FAST_Bz, $
                    TILT=thisTilt, $
                    EPOCH=time_epoch

        GEOPACK_T96,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                    ionos_Bx,ionos_By,ionos_Bz, $
                    TILT=thisTilt, $
                    EPOCH=time_epoch

     END
     KEYWORD_SET(T01): BEGIN

        GEOPACK_T01,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                    FAST_Bx,FAST_By,FAST_Bz, $
                    TILT=thisTilt, $
                    EPOCH=time_epoch

        GEOPACK_T01,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                    ionos_Bx,ionos_By,ionos_Bz, $
                    TILT=thisTilt, $
                    EPOCH=time_epoch

     END
     ELSE: BEGIN

        GEOPACK_TS04,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                     FAST_Bx,FAST_By,FAST_Bz, $
                     TILT=thisTilt, $
                     EPOCH=time_epoch, $
                     IOPGEN=IOPGen, $
                     IOPT=IOPT, $
                     IOPB=IOPB, $
                     IOPR=IOPR

        GEOPACK_TS04,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                     ionos_Bx,ionos_By,ionos_Bz, $
                     TILT=thisTilt, $
                     EPOCH=time_epoch, $
                     IOPGEN=IOPGen, $
                     IOPT=IOPT, $
                     IOPB=IOPB, $
                     IOPR=IOPR

     END
  ENDCASE

  ;;Calculate internal contribution
  GEOPACK_IGRF_GSW_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                      FAST_Bx_IGRF,FAST_By_IGRF,FAST_Bz_IGRF, $
                      EPOCH=time_epoch

  GEOPACK_IGRF_GSW_08,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                      ionos_Bx_IGRF,ionos_By_IGRF,ionos_Bz_IGRF, $
                      EPOCH=time_epoch


  FAST_GSM     = [FAST_GSM_x,FAST_GSM_y,FAST_GSM_z]
  ionos_GSM    = [ionos_GSM_x,ionos_GSM_y,ionos_GSM_z]


  FAST_RE             = SQRT(TOTAL(FAST_GSM^2))
  ionos_RE            = SQRT(TOTAL(ionos_GSM^2))

  FAST_km             = (FAST_RE     - 1.D ) * RE_to_km
  ionos_km            = (ionos_RE    - 1.D ) * RE_to_km

  FAST_B              = [FAST_Bx,FAST_By,FAST_Bz]
  ionos_B             = [ionos_Bx,ionos_By,ionos_Bz]

  FAST_B_IGRF         = [FAST_Bx_IGRF,FAST_By_IGRF,FAST_Bz_IGRF]
  ionos_B_IGRF        = [ionos_Bx_IGRF,ionos_By_IGRF,ionos_Bz_IGRF]

  FAST_BMag           = SQRT(TOTAL((FAST_B + FAST_B_IGRF)^2))
  ionos_BMag          = SQRT(TOTAL((ionos_B + ionos_B_IGRF)^2))

  FAST_B_IGRFMag      = SQRT(TOTAL(FAST_B_IGRF^2))
  ionos_B_IGRFMag     = SQRT(TOTAL(ionos_B_IGRF^2))


  RLimFull = 3000 ;;For full field line 
  GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                   ;; trace_to_equator,(KEYWORD_SET(TS04) ? tmpParMod : !NULL), $
                   trace_to_equator,tmpParMod, $
                   dTFull_GSM_x,dTFull_GSM_y,dTFull_GSM_z, $
                   /REFINE, $
                   /EQUATOR, $
                   R0=R0, $
                   RLIM=RLimFull, $
                   FLINE=fLine_toEqFull, $
                   T89=T89, $
                   T96=T96, $
                   T01=T01, $
                   TS04=TS04, $
                   IGRF=IGRF, $
                   ;; /IGRF, $
                   TILT=thisTilt, $ ;should be in degrees
                   EPOCH=time_epoch, $
                   DSMAX=dsMax, $
                   ERR=traceErr

  tRB_fLine   = fLine_toEqFull
  ;;Now LOOP over field line
  nFLineElem  = N_ELEMENTS(fline_toEqFull[*,0])

  ;;Indexed as R_B_Fast,R_B_ionos
  tRB_RBpairs = MAKE_ARRAY(2,nFLineElem,VALUE=0.0D)
  FOR jj=0,nFLineElem-1 DO BEGIN

     ;; tmpPos = [fline_toEqFull[jj,0],fline_toEqFull[jj,1],fline_toEqFull[jj,2]]
     ;; tmpPos = [fline_toEqFull[jj,0],fline_toEqFull[jj,1],fline_toEqFull[jj,2]]
     downTail_GSM_x = fline_toEqFull[jj,0] 
     downTail_GSM_y = fline_toEqFull[jj,1]
     downTail_GSM_z = fline_toEqFull[jj,2]

     ;;Calculate external contribution
     CASE 1 OF
        KEYWORD_SET(T89): BEGIN

           GEOPACK_T89,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                       downTail_Bx,downTail_By,downTail_Bz, $
                       TILT=thisTilt, $
                       EPOCH=time_epoch

        END
        KEYWORD_SET(T96): BEGIN

           GEOPACK_T96,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                       downTail_Bx,downTail_By,downTail_Bz, $
                       TILT=thisTilt, $
                       EPOCH=time_epoch

        END
        KEYWORD_SET(T01): BEGIN

           GEOPACK_T01,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                       downTail_Bx,downTail_By,downTail_Bz, $
                       TILT=thisTilt, $
                       EPOCH=time_epoch

        END
        ELSE: BEGIN

           GEOPACK_TS04,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                        downTail_Bx,downTail_By,downTail_Bz, $
                        TILT=thisTilt, $
                        EPOCH=time_epoch, $
                        IOPGEN=IOPGen, $
                        IOPT=IOPT, $
                        IOPB=IOPB, $
                        IOPR=IOPR

        END
     ENDCASE

     ;;Calculate internal contribution
     GEOPACK_IGRF_GSW_08,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                         downTail_Bx_IGRF,downTail_By_IGRF,downTail_Bz_IGRF, $
                         EPOCH=time_epoch

     downTail_GSM = [downTail_GSM_x,downTail_GSM_y,downTail_GSM_z]

     downTail_RE         = SQRT(TOTAL(downTail_GSM^2))
     downTail_km         = (downTail_RE - 1.D ) * RE_to_km
     downTail_B          = [downTail_Bx,downTail_By,downTail_Bz]
     downTail_B_IGRF     = [downTail_Bx_IGRF,downTail_By_IGRF,downTail_Bz_IGRF]
     downTail_BMag       = SQRT(TOTAL((downTail_B + downTail_B_IGRF)^2))
     downTail_B_IGRFMag  = SQRT(TOTAL(downTail_B_IGRF^2))
     R_B_FAST            = FAST_BMag/downTail_BMag
     R_B_ionos           = ionos_BMag/downTail_BMag

     ;; R_B_IGRF_FAST       = FAST_B_IGRFMag/downTail_B_IGRFMag
     ;; R_B_IGRF_ionos      = ionos_B_IGRFMag/downTail_B_IGRFMag

     tRB_RBpairs[*,jj]   = [TEMPORARY(R_B_FAST),TEMPORARY(R_B_ionos)]

  ENDFOR

  tRB_fLineRE = SQRT(TOTAL(fline_toEqFull^2,2))

  CASE 1 OF
     KEYWORD_SET(time_array): BEGIN

        IF ~gaveStr THEN BEGIN
           tee1 = TEMPORARY(t1)
        ENDIF ELSE BEGIN
           tee1 = TEMPORARY(t1Str)
        ENDELSE

     END
     ELSE: BEGIN

        IF ~gaveStr THEN BEGIN
           tee1 = TEMPORARY(t1)
           IF st2 NE 0 THEN tee2 = TEMPORARY(t2)
        ENDIF ELSE BEGIN
           tee1 = TEMPORARY(t1Str)
           IF st2 NE 0 THEN tee2 = TEMPORARY(t2Str)
        ENDELSE

     END
  ENDCASE

END
