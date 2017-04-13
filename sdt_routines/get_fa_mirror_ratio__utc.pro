;2017/04/13
FUNCTION GET_FA_MIRROR_RATIO__UTC,tee1,tee2, $
                                  TIME_ARRAY=time_array, $
                                  START_AT_EQUATOR=start_at_equator, $
                                  DOWNTAIL_GSE=downTail_GSE, $
                                  IGRF=IGRF, $
                                  TS04=TS04, $
                                  IOPGEN=IOPGen, $
                                  IOPT=IOPT, $
                                  IOPB=IOPB, $
                                  IOPR=IOPR, $
                                  R0=R0, $
                                  RLIM=RLim

  COMPILE_OPT IDL2,STRICTARRSUBS


  RE_to_km = 6370.D

  IF N_ELEMENTS(TS04) EQ 0 AND N_ELEMENTS(IGRF) EQ 0 THEN IGRF = 1

  ;; t1Str         = '1997-02-01/09:26:00.0'
  ;; time_array    = 1

  gaveStr = 0
  CASE 1 OF
     KEYWORD_SET(time_array): BEGIN

        CASE SIZE(tee1,/TYPE) OF
           7: BEGIN
              t1      = STR_TO_TIME(tee1)
              t1Str   = TEMPORARY(tee1)
              gaveStr = 1
           END
           ELSE: BEGIN
              t1Str   = TIME_TO_STR(tee1,/MS)
              t1      = TEMPORARY(tee1)
           END
        ENDCASE

     END
     ELSE: BEGIN

        st1 = SIZE(tee1,/TYPE)
        st2 = SIZE(tee2,/TYPE)

        IF (st1 NE st2) AND (st2 NE 0) THEN STOP

        CASE st1 OF
           7: BEGIN
              t1      = STR_TO_TIME(tee1)
              t1Str   = TEMPORARY(tee1)
              gaveStr = 1
           END
           ELSE: BEGIN
              t1Str   = TIME_TO_STR(tee1,/MS)
              t1      = TEMPORARY(tee1)
           END
        ENDCASE

        CASE st2 OF
           7: BEGIN
              t2      = STR_TO_TIME(tee2)
              t2Str   = TEMPORARY(tee2)
              gaveStr = 1
           END
           0:
           ELSE: BEGIN
              t2Str   = TIME_TO_STR(tee2,/MS)
              t2      = TEMPORARY(tee2)
           END
        ENDCASE

     END
  ENDCASE

  swdat         = GET_SW_CONDS_UTC(t1Str,t2Str, $
                                   TIME_ARRAY=time_array)

  tStrings      = TIME_TO_STR(swdat.times.FAST,/MS)

  ;;PARMOD(0) SOLAR WIND RAM PRESSURE IN NANOPASCALS
  ;;PARMOD(1) DST
  ;;PARMOD(2),PARMOD(3) IMF BY AND BZ

  ;; parMod        = MAKE_ARRAY(10,/DOUBLE)
  ;; parMod[0]     = press
  ;; parMod[1]     = dstVal
  ;; parMod[2]     = By
  ;; parMod[3]     = Bz

  parMod        = MAKE_ARRAY(10,/DOUBLE)
  parMod[0]     = swdat.P
  parMod[1]     = swDat.dst
  parMod[2]     = swDat.IMF[1]
  parMod[3]     = swDat.IMF[2]


  CONVERT_TIME_STRING_TO_YMDHMS_ARRAYS, $
     t1Str, $
     OUT_YEARARR=yearArr, $
     OUT_DOYARR=DOYArr, $
     OUT_MONTHARR=monthArr, $
     OUT_DAYARR=dayArr, $
     OUT_HOURARR=hourArr, $
     OUT_MINARR=minArr, $
     OUT_SECARR=secArr

  GET_FA_ORBIT,t1,t2, $
               TIME_ARRAY=time_array, $
               /NO_STORE, $
               STRUC=struc
  time_epoch    = UTC_TO_CDF_EPOCH(struc.time)


  i=0
  GEOPACK_RECALC_08,YearArr[i],MonthArr[i],DayArr[i], $
                    HourArr[i],MinArr[i],SecArr[i], $
                    /DATE, $
                    TILT=thisTilt

  CASE 1 OF
     KEYWORD_SET(start_at_equator): BEGIN
        ;;GSE to GSW
        GEOPACK_CONV_COORD_08,downTail_GSE[0],downTail_GSE[1],downTail_GSE[2], $
                              downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                              /FROM_GSE,/TO_GSW,EPOCH=time_epoch[0]

        downTail_GSM  = [downTail_GSM_x,downTail_GSM_y,downTail_GSM_z]

        GEOPACK_TRACE_08,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                         -1,parMod, $
                         ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                         /REFINE, $
                         /IONOSPHERE, $
                         /TS04, $
                         TILT=thisTilt ;should be in degrees

     END
     ELSE: BEGIN
        ;;GEO to GEI

        nIter = N_ELEMENTS(struc.alt)
        struc.fa_pos /= 6370.D

        FAST_GSM_arr     = nIter GT 1 ? MAKE_ARRAY(nIter,3,VALUE=0.) : MAKE_ARRAY(3,VALUE=0.)
        ionos_GSM_arr    = nIter GT 1 ? MAKE_ARRAY(nIter,3,VALUE=0.) : MAKE_ARRAY(3,VALUE=0.)
        downTail_GSM_arr = nIter GT 1 ? MAKE_ARRAY(nIter,3,VALUE=0.) : MAKE_ARRAY(3,VALUE=0.)

        FAST_RE_arr             = MAKE_ARRAY(nIter,VALUE=0.)
        downTail_RE_arr         = MAKE_ARRAY(nIter,VALUE=0.)
        ionos_RE_arr            = MAKE_ARRAY(nIter,VALUE=0.)

        FAST_km_arr             = MAKE_ARRAY(nIter,VALUE=0.)
        downTail_km_arr         = MAKE_ARRAY(nIter,VALUE=0.)
        ionos_km_arr            = MAKE_ARRAY(nIter,VALUE=0.)

        FAST_B_arr              = nIter GT 1 ? MAKE_ARRAY(nIter,3,VALUE=0.) : MAKE_ARRAY(3,VALUE=0.)
        downTail_B_arr          = nIter GT 1 ? MAKE_ARRAY(nIter,3,VALUE=0.) : MAKE_ARRAY(3,VALUE=0.)
        ionos_B_arr             = nIter GT 1 ? MAKE_ARRAY(nIter,3,VALUE=0.) : MAKE_ARRAY(3,VALUE=0.)

        FAST_B_IGRF_ARR         = nIter GT 1 ? MAKE_ARRAY(nIter,3,VALUE=0.) : MAKE_ARRAY(3,VALUE=0.)
        downTail_B_IGRF_arr     = nIter GT 1 ? MAKE_ARRAY(nIter,3,VALUE=0.) : MAKE_ARRAY(3,VALUE=0.)
        ionos_B_IGRF_arr        = nIter GT 1 ? MAKE_ARRAY(nIter,3,VALUE=0.) : MAKE_ARRAY(3,VALUE=0.)

        FAST_BMag_Arr           = MAKE_ARRAY(nIter,VALUE=0.)
        downTail_BMag_arr       = MAKE_ARRAY(nIter,VALUE=0.)
        ionos_BMag_arr          = MAKE_ARRAY(nIter,VALUE=0.)

        FAST_B_IGRFMag_Arr      = MAKE_ARRAY(nIter,VALUE=0.)
        downTail_B_IGRFMag_arr  = MAKE_ARRAY(nIter,VALUE=0.)
        ionos_B_IGRFMag_arr     = MAKE_ARRAY(nIter,VALUE=0.)

        R_B_FAST_ARR            = MAKE_ARRAY(nIter,VALUE=0.)
        R_B_ionos_arr           = MAKE_ARRAY(nIter,VALUE=0.)

        R_B_IGRF_FAST_ARR       = MAKE_ARRAY(nIter,VALUE=0.)
        R_B_IGRF_ionos_arr      = MAKE_ARRAY(nIter,VALUE=0.)

        FOR k=0,nIter-1 DO BEGIN

           IF nIter GT 1 THEN BEGIN
              tmpPos = [struc.fa_pos[k,0],struc.fa_pos[k,1],struc.fa_pos[k,2]]
           ENDIF ELSE BEGIN
              tmpPos = [struc.fa_pos[0],struc.fa_pos[1],struc.fa_pos[2]]
           ENDELSE

           GEOPACK_CONV_COORD_08,tmpPos[0],tmpPos[1],tmpPos[2], $
                                 FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                                 /FROM_GSE,/TO_GSW,EPOCH=time_epoch[0]

           GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                            1,parMod, $
                            downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                            /REFINE, $
                            /EQUATOR, $
                            R0=R0, $
                            RLIM=RLim, $
                            TS04=TS04, $
                            IGRF=IGRF, $
                            TILT=thisTilt, $ ;should be in degrees
                            EPOCH=time_epoch[0]

           GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                            -1,parMod, $
                            ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                            /REFINE, $
                            R0=R0, $
                            RLIM=RLim, $
                            /IONOSPHERE, $
                            TS04=TS04, $
                            IGRF=IGRF, $
                            TILT=thisTilt, $ ;should be in degrees
                            EPOCH=time_epoch[0]

           GEOPACK_TS04,parMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                        downTail_Bx,downTail_By,downTail_Bz, $
                        TILT=thisTilt, $
                        EPOCH=time_epoch[0], $
                        IOPGEN=IOPGen, $
                        IOPT=IOPT, $
                        IOPB=IOPB, $
                        IOPR=IOPR

           GEOPACK_TS04,parMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                        FAST_Bx,FAST_By,FAST_Bz, $
                        TILT=thisTilt, $
                        EPOCH=time_epoch[0], $
                        IOPGEN=IOPGen, $
                        IOPT=IOPT, $
                        IOPB=IOPB, $
                        IOPR=IOPR

           GEOPACK_TS04,parMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                        ionos_Bx,ionos_By,ionos_Bz, $
                        TILT=thisTilt, $
                        EPOCH=time_epoch[0], $
                        IOPGEN=IOPGen, $
                        IOPT=IOPT, $
                        IOPB=IOPB, $
                        IOPR=IOPR

           GEOPACK_IGRF_GSW_08,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                               downTail_Bx_IGRF,downTail_By_IGRF,downTail_Bz_IGRF, $
                               EPOCH=time_epoch[0]

           GEOPACK_IGRF_GSW_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                               FAST_Bx_IGRF,FAST_By_IGRF,FAST_Bz_IGRF, $
                               EPOCH=time_epoch[0]

           GEOPACK_IGRF_GSW_08,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                               ionos_Bx_IGRF,ionos_By_IGRF,ionos_Bz_IGRF, $
                               EPOCH=time_epoch[0]


           FAST_GSM     = [FAST_GSM_x,FAST_GSM_y,FAST_GSM_z]
           downTail_GSM = [downTail_GSM_x,downTail_GSM_y,downTail_GSM_z]
           ionos_GSM    = [ionos_GSM_x,ionos_GSM_y,ionos_GSM_z]



           FAST_RE             = SQRT(TOTAL(FAST_GSM^2))
           downTail_RE         = SQRT(TOTAL(downTail_GSM^2))
           ionos_RE            = SQRT(TOTAL(ionos_GSM^2))

           FAST_km             = (FAST_RE     - 1.D ) * RE_to_km
           downTail_km         = (downTail_RE - 1.D ) * RE_to_km
           ionos_km            = (ionos_RE    - 1.D ) * RE_to_km

           FAST_B              = [FAST_Bx,FAST_By,FAST_Bz]
           downTail_B          = [downTail_Bx,downTail_By,downTail_Bz]
           ionos_B             = [ionos_Bx,ionos_By,ionos_Bz]

           FAST_B_IGRF         = [FAST_Bx_IGRF,FAST_By_IGRF,FAST_Bz_IGRF]
           downTail_B_IGRF     = [downTail_Bx_IGRF,downTail_By_IGRF,downTail_Bz_IGRF]
           ionos_B_IGRF        = [ionos_Bx_IGRF,ionos_By_IGRF,ionos_Bz_IGRF]

           FAST_BMag           = SQRT(TOTAL(FAST_B^2))
           downTail_BMag       = SQRT(TOTAL(downTail_B^2))
           ionos_BMag          = SQRT(TOTAL(ionos_B^2))

           FAST_B_IGRFMag      = SQRT(TOTAL(FAST_B_IGRF^2))
           downTail_B_IGRFMag  = SQRT(TOTAL(downTail_B_IGRF^2))
           ionos_B_IGRFMag     = SQRT(TOTAL(ionos_B_IGRF^2))

           R_B_FAST            = FAST_BMag/downTail_BMag
           R_B_ionos           = ionos_BMag/downTail_BMag

           R_B_IGRF_FAST       = FAST_B_IGRFMag/downTail_B_IGRFMag
           R_B_IGRF_ionos      = ionos_B_IGRFMag/downTail_B_IGRFMag

           CASE nIter OF
              1: BEGIN
                 FAST_GSM_arr          = TEMPORARY(FAST_GSM)
                 ionos_GSM_arr         = TEMPORARY(ionos_GSM)
                 downTail_GSM_arr      = TEMPORARY(downTail_GSM)

                 FAST_B_arr            = TEMPORARY(FAST_B)
                 downTail_B_arr        = TEMPORARY(downTail_B)
                 ionos_B_arr           = TEMPORARY(ionos_B)

                 FAST_B_IGRF_arr       = TEMPORARY(FAST_B_IGRF)
                 downTail_B_IGRF_arr   = TEMPORARY(downTail_B_IGRF)
                 ionos_B_IGRF_arr      = TEMPORARY(ionos_B_IGRF)

              END
              ELSE: BEGIN
                 FAST_GSM_arr[k,*]     = TEMPORARY(FAST_GSM)
                 ionos_GSM_arr[k,*]    = TEMPORARY(ionos_GSM)
                 downTail_GSM_arr[k,*] = TEMPORARY(downTail_GSM)

                 FAST_B_arr[k,*]       = TEMPORARY(FAST_B)
                 downTail_B_arr[k,*]   = TEMPORARY(downTail_B)
                 ionos_B_arr[k,*]      = TEMPORARY(ionos_B)

                 FAST_B_IGRF_arr[k,*]     = TEMPORARY(FAST_B_IGRF)
                 downTail_B_IGRF_arr[k,*] = TEMPORARY(downTail_B_IGRF)
                 ionos_B_IGRF_arr[k,*]    = TEMPORARY(ionos_B_IGRF)

              END
           ENDCASE

           FAST_RE_arr[k]             = TEMPORARY(FAST_RE)
           downTail_RE_arr[k]         = TEMPORARY(downTail_RE)
           ionos_RE_arr[k]            = TEMPORARY(ionos_RE)

           FAST_km_arr[k]             = TEMPORARY(FAST_km)
           downTail_km_arr[k]         = TEMPORARY(downTail_km)
           ionos_km_arr[k]            = TEMPORARY(ionos_km)

           FAST_BMag_arr[k]           = TEMPORARY(FAST_BMag)
           downTail_BMag_arr[k]       = TEMPORARY(downTail_BMag)
           ionos_BMag_arr[k]          = TEMPORARY(ionos_BMag)

           FAST_B_IGRFMag_arr[k]      = TEMPORARY(FAST_B_IGRFMag)
           downTail_B_IGRFMag_arr[k]  = TEMPORARY(downTail_B_IGRFMag)
           ionos_B_IGRFMag_arr[k]     = TEMPORARY(ionos_B_IGRFMag)

           R_B_FAST_arr[k]            = TEMPORARY(R_B_FAST)
           R_B_ionos_arr[k]           = TEMPORARY(R_B_ionos)

           R_B_IGRF_FAST_ARR[k]       = TEMPORARY(R_B_IGRF_FAST)
           R_B_IGRF_ionos_arr[k]      = TEMPORARY(R_B_IGRF_ionos)


        ENDFOR

        ;; mRStruc             = {time      : [t1,(KEYWORD_SET(time_array) ? t2 : !NULL)], $
        ;;                        mlt       : struc.mlt, $
        ;;                        ilat      : struc.mlt, $
        mRStruc             = {R_E       : {ionos    : ionos_RE_arr, $
                                            FAST     : FAST_RE_arr, $
                                            downTail : downTail_RE_arr}, $
                               km        : {ionos    : ionos_km_arr, $
                                            FAST     : FAST_km_arr, $
                                            downTail : downTail_km_arr}, $
                               BMag      : {ionos    : ionos_BMag_arr, $
                                            FAST     : FAST_BMag_arr, $
                                            downTail : downTail_BMag_arr}, $
                               B_IGRFMag : {ionos    : ionos_B_IGRFMag_arr, $
                                            FAST     : FAST_B_IGRFMag_arr, $
                                            downTail : downTail_B_IGRFMag_arr}, $
                               R_B       : {ionos    : R_B_ionos_arr, $
                                            FAST     : R_B_FAST_arr, $
                                            downTail : 1}, $
                               R_B_IGRF  : {ionos    : R_B_IGRF_ionos_arr, $
                                            FAST     : R_B_IGRF_FAST_arr, $
                                            downTail : 1}}

        mRStruc = CREATE_STRUCT(struc,mRStruc)

        PRINT_FA_MIRROR_RATIO,mRStruc

     END
  ENDCASE

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

  RETURN,mRStruc


END
