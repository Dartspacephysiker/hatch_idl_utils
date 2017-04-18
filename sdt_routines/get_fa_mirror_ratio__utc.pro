;2017/04/13
FUNCTION GET_FA_MIRROR_RATIO__UTC,tee1,tee2, $
                                  TIME_ARRAY=time_array, $
                                  START_AT_EQUATOR=start_at_equator, $
                                  DOWNTAIL_GSE=downTail_GSE, $
                                  TO_EQUATOR=to_equator, $
                                  TO_POLAR_SATELLITE=to_Polar, $
                                  TO_THIS_RE=to_RE, $
                                  TO_THIS_KM=to_km, $
                                  IGRF=IGRF, $
                                  T89=T89, $
                                  T96=T96, $
                                  T01=T01, $
                                  TS04=TS04, $
                                  IOPGEN=IOPGen, $
                                  IOPT=IOPT, $
                                  IOPB=IOPB, $
                                  IOPR=IOPR, $
                                  R0=R0, $
                                  RLIM=RLim

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; IOPT - A MODEL INDEX; CAN BE USED FOR SPECIFYING A VERSION OF THE EXTERNAL FIELD
  ;;     MODEL (E.G., A NUMBER OF THE KP-INDEX INTERVAL). ALTERNATIVELY, ONE CAN USE THE ARRAY
  ;;     PARMOD FOR THAT PURPOSE (SEE BELOW); IN THAT CASE IOPT IS JUST A DUMMY PARAMETER.


  ;; IOPGEN - GENERAL OPTION FLAG:  IOPGEN=0 - CALCULATE TOTAL FIELD
  ;;                                IOPGEN=1 - DIPOLE SHIELDING ONLY
  ;;                                IOPGEN=2 - TAIL FIELD ONLY
  ;;                                IOPGEN=3 - BIRKELAND FIELD ONLY
  ;;                                IOPGEN=4 - RING CURRENT FIELD ONLY
  ;;                                IOPGEN=5 - INTERCONNECTION FIELD ONLY

  ;; IOPT -  TAIL FIELD FLAG:       IOPT=0  -  BOTH MODES
  ;;                                IOPT=1  -  MODE 1 ONLY
  ;;                                IOPT=2  -  MODE 2 ONLY

  ;; IOPB -  BIRKELAND FIELD FLAG:  IOPB=0  -  ALL 4 TERMS
  ;;                                IOPB=1  -  REGION 1, MODES 1 AND 2
  ;;                                IOPB=2  -  REGION 2, MODES 1 AND 2

  ;; IOPR -  RING CURRENT FLAG:     IOPR=0  -  BOTH SRC AND PRC
  ;;                                IOPR=1  -  SRC ONLY
  ;;                                IOPR=2  -  PRC ONLY

  traceErr = 0.0001             ;Check out http://geo.phys.spbu.ru/~tsyganenko/Examples1_and_2.html. 0.0001 is what Tsyganenko himself uses. Baller, right?
  dsMax    = 0.2                ;Max R_E step size

  __TRACE_ANTIPARALLEL_B = 1
  __TRACE_PARALLEL_B     = -1

  RE_to_km        = 6374.D
  ;; Polar_apogee_km = 100000.D
  Polar_apogee_km = 40000.D

  downTailNavn    = 'downTail'
  FASTNavn        = 'FAST'
  ionosNavn       = 'Ionosphere'
  CASE 1 OF
     KEYWORD_SET(to_RE): BEGIN
        RLim      = to_RE
        downTailNavn = 'utHer'
     END
     KEYWORD_SET(to_km): BEGIN
        RLim      = to_km / RE_to_km
        downTailNavn = 'utHer'
     END
     KEYWORD_SET(to_Polar): BEGIN
        IF N_ELEMENTS(RLim) NE 0 THEN STOP

        RLim = Polar_apogee_km / RE_to_km
        downTailNavn = 'Polar'
     END
     KEYWORD_SET(to_equator):
  ENDCASE

  navn            = {ionos    : ionosNavn, $
                     FAST     : FASTNavn, $
                     downTail : downTailNavn}

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
           5: BEGIN
              t1Str   = TIME_TO_STR(tee1,/MS)
              t1      = TEMPORARY(tee1)
           END
           ELSE: STOP
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
           5: BEGIN
              t2Str   = TIME_TO_STR(tee2,/MS)
              t2      = TEMPORARY(tee2)
           END
           ELSE: STOP
        ENDCASE

     END
  ENDCASE

  swdat         = GET_SW_CONDS_UTC(t1Str,t2Str, $
                                   TIME_ARRAY=time_array, $
                                   /REABERRATE_VY)
  

  tStrings      = TIME_TO_STR(swdat.times.FAST,/MS)

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

  GET_FA_ORBIT,t1,t2, $
               TIME_ARRAY=time_array, $
               /NO_STORE, $
               STRUC=struc

  ;; time_epoch2   = UTC_TO_CDF_EPOCH(struc.time) ;;Doesn't exactly match GEOPACK EPOCH ...

  nIter         = N_ELEMENTS(struc.time)
  time_epoch    = MAKE_ARRAY(nIter,/DOUBLE,VALUE=0.0D)
  FOR k=0,nIter-1 DO BEGIN
     tmpUTC = struc.time[k]
     ;; GEOPACK_EPOCH,tmpEpoch,YearArr[k],MonthArr[k],DayArr[k], $
     ;;               HourArr[k],MinArr[k],SecArr[k], $
     ;;               /COMPUTE_EPOCH
     GEOPACK_EPOCH,tmpEpoch,YearArr[k],DOYArr[k], $
                   HourArr[k],MinArr[k],FLOOR(SecArr[k]),SecArr[k]-FLOOR(SecArr[k]), $
                   /DOY, $
                   /COMPUTE_EPOCH
     time_epoch[k]  = TEMPORARY(tmpEpoch)
  ENDFOR
  
  ;;For TS04, T01, etc.
  itvlSteps_5Min = CEIL((MAX(struc.time)-MIN(struc.time))/300)
  revHour_d5Min  = REVERSE( (INDGEN(12+itvlSteps_5Min)+1) * 300)

  precedingHour_UTC = MAX(struc.time) - revHour_d5Min
  GInds             = VALUE_CLOSEST2(precedingHour_UTC,struc.time,/CONSTRAINED)
  tmpSWDat          = GET_SW_CONDS_UTC(precedingHour_UTC, $
                                       /TIME_ARRAY, $
                                       /REABERRATE_VY)
  

  ;; IF (WHERE(tmpSWDat.P   LT  0.5 OR tmpSWDat.P   GT 10))[0] EQ -1 AND $
  ;;    (WHERE(tmpSWDat.Dst LT -100 OR tmpSWDat.Dst GT 20))[0] EQ -1 AND $
  ;;    (WHERE(ABS(tmpSWDat.IMF[1,*]) GT 10))[0]               EQ -1 AND $
  ;;    (WHERE(ABS(tmpSWDat.IMF[2,*]) GT 10))[0]               EQ -1 $
  ;; THEN BEGIN

  IOPGen = 0
  IF ~tmpSWDat.valid THEN BEGIN

     ;; IOPT_89 = ROUND(swDat.Kp) + 1

     T89  = 1
     T96  = 0
     T01  = 0
     TS04 = 0

  ENDIF ELSE BEGIN

     IF KEYWORD_SET(IOPGen) OR KEYWORD_SET(IOPT) OR KEYWORD_SET(IOPB) OR KEYWORD_SET(IOPR) THEN BEGIN

        T89  = 0
        T96  = 0
        T01  = 0
        TS04 = 1

     ENDIF ELSE BEGIN

        T89  = 0
        T96  = 1
        T01  = 0
        TS04 = 0

     ENDELSE

  ENDELSE

  IF N_ELEMENTS(TS04) EQ 0 AND N_ELEMENTS(IGRF) EQ 0 AND N_ELEMENTS(T01) EQ 0 THEN IGRF = 1

  Bmodelinfo = {T89  : KEYWORD_SET(T89), $
                T96  : KEYWORD_SET(T96), $
                T01  : KEYWORD_SET(T01), $
                TS04 : KEYWORD_SET(TS04), $
                IOPT_89 : ROUND(swDat.Kp)+1}

  ;; IF KEYWORD_SET(T01) THEN BEGIN
  CASE 1 OF
     KEYWORD_SET(storm): BEGIN
        GEOPACK_GETG,tmpSWDat.N,tmpSWDat.vSpeed,tmpSWDat.IMF[2,*],GParms,/STORM
     END
     ELSE: BEGIN
        GEOPACK_GETG,tmpSWDat.vSpeed,tmpSWDat.IMF[1,*],tmpSWDat.IMF[2,*],GParms
     END
  ENDCASE

  GParms = GParms[GInds,*]
  ;; ENDIF

  ;;For TS04
  GEOPACK_GETW,tmpSWDat.N,tmpSWDat.vSpeed,tmpSWDat.IMF[2,*],wParms
  wParms = wParms[GInds,*]


  ;;PARMOD(0) SOLAR WIND RAM PRESSURE IN NANOPASCALS
  ;;PARMOD(1) DST
  ;;PARMOD(2),PARMOD(3) IMF BY AND BZ

  ;; parMod        = MAKE_ARRAY(10,/DOUBLE)
  ;; parMod[0]     = press
  ;; parMod[1]     = dstVal
  ;; parMod[2]     = By
  ;; parMod[3]     = Bz

  ;;For T96 (http://geo.phys.spbu.ru/~tsyganenko/T96.html):
  ;; PDYN=PARMOD(1)
  ;; DST=PARMOD(2)
  ;; BYIMF=PARMOD(3)
  ;; BZIMF=PARMOD(4)


  i=0

  CASE 1 OF
     KEYWORD_SET(start_at_equator): BEGIN
        ;;GSE to GSW

        IF struc.ilat GT 0 THEN BEGIN
           trace_to_equator = __TRACE_ANTIPARALLEL_B
           trace_to_ionos   = __TRACE_PARALLEL_B
        ENDIF ELSE BEGIN
           trace_to_equator = __TRACE_PARALLEL_B
           trace_to_ionos   = __TRACE_ANTIPARALLEL_B
        ENDELSE

        parMod        = MAKE_ARRAY(10,/DOUBLE)
        parMod[0]     = swdat.P
        parMod[1]     = swDat.dst
        parMod[2]     = swDat.IMF[1]
        parMod[3]     = swDat.IMF[2]
        parMod[4:9]   = wParms[0,*]

        GEOPACK_RECALC_08,YearArr[i],MonthArr[i],DayArr[i], $
                          HourArr[i],MinArr[i],SecArr[i], $
                          /DATE, $
                          VGSE=swDat.v_SW[*,i], $
                          TILT=thisTilt

        GEOPACK_CONV_COORD_08,downTail_GSE[0],downTail_GSE[1],downTail_GSE[2], $
                              downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                              /FROM_GSE,/TO_GSW,EPOCH=time_epoch[0]

        downTail_GSM  = [downTail_GSM_x,downTail_GSM_y,downTail_GSM_z]

        GEOPACK_TRACE_08,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                         trace_to_ionos,parMod, $
                         ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                         /REFINE, $
                         /IONOSPHERE, $
                         /TS04, $
                         TILT=thisTilt ;should be in degrees

     END
     ELSE: BEGIN
        ;;GEO to GEI

        struc.fa_pos /= RE_to_km

        IF nIter GT 1 THEN BEGIN

           FAST_GSM_arr         = MAKE_ARRAY(nIter,3,VALUE=0.)
           ionos_GSM_arr        = MAKE_ARRAY(nIter,3,VALUE=0.)
           downTail_GSM_arr     = MAKE_ARRAY(nIter,3,VALUE=0.)

           parMod               = MAKE_ARRAY(10,nIter,/DOUBLE)
           parMod[0,*]          = swdat.P
           parMod[1,*]          = swDat.dst
           parMod[2,*]          = swDat.IMF[1,*]
           parMod[3,*]          = swDat.IMF[2,*]
           parMod[4:9,*]        = wParms


           FAST_B_arr           = MAKE_ARRAY(nIter,3,VALUE=0.)
           downTail_B_arr       = MAKE_ARRAY(nIter,3,VALUE=0.)
           ionos_B_arr          = MAKE_ARRAY(nIter,3,VALUE=0.)

           FAST_B_IGRF_ARR      = MAKE_ARRAY(nIter,3,VALUE=0.)
           downTail_B_IGRF_arr  = MAKE_ARRAY(nIter,3,VALUE=0.)
           ionos_B_IGRF_arr     = MAKE_ARRAY(nIter,3,VALUE=0.)

        ENDIF ELSE BEGIN

           FAST_GSM_arr         = MAKE_ARRAY(3,VALUE=0.)
           ionos_GSM_arr        = MAKE_ARRAY(3,VALUE=0.)
           downTail_GSM_arr     = MAKE_ARRAY(3,VALUE=0.)

           FAST_B_arr           = MAKE_ARRAY(3,VALUE=0.)
           downTail_B_arr       = MAKE_ARRAY(3,VALUE=0.)
           ionos_B_arr          = MAKE_ARRAY(3,VALUE=0.)

           FAST_B_IGRF_ARR      = MAKE_ARRAY(3,VALUE=0.)
           downTail_B_IGRF_arr  = MAKE_ARRAY(3,VALUE=0.)
           ionos_B_IGRF_arr     = MAKE_ARRAY(3,VALUE=0.)

           parMod               = MAKE_ARRAY(10,/DOUBLE)
           parMod[0]            = swdat.P
           parMod[1]            = swDat.dst
           parMod[2]            = swDat.IMF[1,*]
           parMod[3]            = swDat.IMF[2,*]
           parMod[4:9]          = wParms
        ENDELSE

        STR_ELEMENT,Bmodelinfo,'parmod',parMod,/ADD_REPLACE
        STR_ELEMENT,Bmodelinfo,'GParms',GParms,/ADD_REPLACE
        
        FAST_Lshell_arr         = MAKE_ARRAY(nIter,VALUE=0.)
        FAST_Lshell_IGRF_arr    = MAKE_ARRAY(nIter,VALUE=0.)

        FAST_RE_arr             = MAKE_ARRAY(nIter,VALUE=0.)
        downTail_RE_arr         = MAKE_ARRAY(nIter,VALUE=0.)
        ionos_RE_arr            = MAKE_ARRAY(nIter,VALUE=0.)

        FAST_km_arr             = MAKE_ARRAY(nIter,VALUE=0.)
        downTail_km_arr         = MAKE_ARRAY(nIter,VALUE=0.)
        ionos_km_arr            = MAKE_ARRAY(nIter,VALUE=0.)

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

           GEOPACK_RECALC_08,YearArr[k],MonthArr[k],DayArr[k], $
                             HourArr[k],MinArr[k],SecArr[k], $
                             /DATE, $
                             VGSE=swDat.v_SW[*,k], $
                             TILT=thisTilt

           IF nIter GT 1 THEN BEGIN
              tmpPos = [struc.fa_pos[k,0],struc.fa_pos[k,1],struc.fa_pos[k,2]]
           ENDIF ELSE BEGIN
              tmpPos = [struc.fa_pos[0],struc.fa_pos[1],struc.fa_pos[2]]
           ENDELSE

           IF struc.ilat[k] GT 0 THEN BEGIN
              trace_to_equator = __TRACE_ANTIPARALLEL_B
              trace_to_ionos   = __TRACE_PARALLEL_B
           ENDIF ELSE BEGIN
              trace_to_equator = __TRACE_PARALLEL_B
              trace_to_ionos   = __TRACE_ANTIPARALLEL_B
           ENDELSE


           GEOPACK_CONV_COORD_08,tmpPos[0],tmpPos[1],tmpPos[2], $
                                 FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                                 /FROM_GSE,/TO_GSW,EPOCH=time_epoch[k]

           CASE 1 OF
              KEYWORD_SET(T89): BEGIN
                 tmpParMod = Bmodelinfo.IOPT_89[k]
              END
              KEYWORD_SET(T96): BEGIN
                 tmpParMod = parMod[*,k]
              END
              KEYWORD_SET(T01): BEGIN
                 tmpParMod = parMod[*,k]
                 tmpParMod[4:5] = GParms[k,*]
              END
              KEYWORD_SET(TS04): BEGIN
                 tmpParMod = parMod[*,k]
              END
           ENDCASE

           GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                            ;; trace_to_equator,(KEYWORD_SET(TS04) ? tmpParMod : !NULL), $
                            trace_to_equator,tmpParMod, $
                            downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                            /REFINE, $
                            EQUATOR=to_equator, $
                            R0=R0, $
                            RLIM=RLim, $
                            FLINE=fLine_toEq, $
                            T89=T89, $
                            T96=T96, $
                            T01=T01, $
                            TS04=TS04, $
                            IGRF=IGRF, $
                            ;; /IGRF, $
                            TILT=thisTilt, $ ;should be in degrees
                            EPOCH=time_epoch[k], $
                            DSMAX=dsMax, $
                            ERR=traceErr

           GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                            ;; trace_to_ionos,(KEYWORD_SET(TS04) ? tmpParMod : !NULL), $
                            trace_to_ionos,tmpParMod, $
                            ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                            /REFINE, $
                            R0=R0, $
                            RLIM=RLim, $
                            FLINE=fLine_toIonos, $
                            /IONOSPHERE, $
                            T89=T89, $
                            T96=T96, $
                            T01=T01, $
                            TS04=TS04, $
                            IGRF=IGRF, $
                            ;; /IGRF, $
                            TILT=thisTilt, $ ;should be in degrees
                            EPOCH=time_epoch[k], $
                            DSMAX=dsMax, $
                            ERR=traceErr

           plot_field_line = 0
           IF KEYWORD_SET(plot_field_line) THEN BEGIN

              RLimFull = 3000 ;;For full field line 
              GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                               ;; trace_to_equator,(KEYWORD_SET(TS04) ? tmpParMod : !NULL), $
                               trace_to_equator,tmpParMod, $
                               dTFull_GSM_x,dTFull_GSM_y,dTFull_GSM_z, $
                               /REFINE, $
                               EQUATOR=to_equator, $
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
                               EPOCH=time_epoch[k], $
                               DSMAX=dsMax, $
                               ERR=traceErr

              GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                               ;; trace_to_ionos,(KEYWORD_SET(TS04) ? tmpParMod : !NULL), $
                               trace_to_ionos,tmpParMod, $
                               ionosFull_GSM_x,ionosFull_GSM_y,ionosFull_GSM_z, $
                               /REFINE, $
                               R0=R0, $
                               RLIM=RLimFull, $
                               FLINE=fLine_toIonosFull, $
                               /IONOSPHERE, $
                               T89=T89, $
                               T96=T96, $
                               T01=T01, $
                               TS04=TS04, $
                               IGRF=IGRF, $
                               ;; /IGRF, $
                               TILT=thisTilt, $ ;should be in degrees
                               EPOCH=time_epoch[k], $
                               DSMAX=dsMax, $
                               ERR=traceErr

              flip_GSM_x = FAST_GSM_x
              flip_GSM_y = FAST_GSM_y
              flip_GSM_z = FAST_GSM_z * (-1.D)


              GEOPACK_TRACE_08,flip_GSM_x,flip_GSM_y,flip_GSM_z, $
                               ;; trace_to_equator,(KEYWORD_SET(TS04) ? tmpParMod : !NULL), $
                               trace_to_equator*(-1),tmpParMod, $
                               dTFlip_GSM_x,dTFlip_GSM_y,dTFlip_GSM_z, $
                               /REFINE, $
                               EQUATOR=to_equator, $
                               R0=R0, $
                               RLIM=RLimFull, $
                               FLINE=fLine_toEqFlip, $
                               T89=T89, $
                               T96=T96, $
                               T01=T01, $
                               TS04=TS04, $
                               IGRF=IGRF, $
                               ;; /IGRF, $
                               TILT=thisTilt, $ ;should be in degrees
                               EPOCH=time_epoch[k], $
                               DSMAX=dsMax, $
                               ERR=traceErr

              GEOPACK_TRACE_08,flip_GSM_x,flip_GSM_y,flip_GSM_z, $
                               ;; trace_to_ionos,(KEYWORD_SET(TS04) ? tmpParMod : !NULL), $
                               trace_to_ionos*(-1),tmpParMod, $
                               ionosFlip_GSM_x,ionosFlip_GSM_y,ionosFlip_GSM_z, $
                               /REFINE, $
                               R0=R0, $
                               RLIM=RLimFull, $
                               FLINE=fLine_toIonosFlip, $
                               /IONOSPHERE, $
                               T89=T89, $
                               T96=T96, $
                               T01=T01, $
                               TS04=TS04, $
                               IGRF=IGRF, $
                               ;; /IGRF, $
                               TILT=thisTilt, $ ;should be in degrees
                               EPOCH=time_epoch[k], $
                               DSMAX=dsMax, $
                               ERR=traceErr

              plotFL = PLOT3D([fline_toIonosFull[*,0],fline_toEqFull[*,0]], $
                              [fline_toIonosFull[*,1],fline_toEqFull[*,1]], $
                              [fline_toIonosFull[*,2],fline_toEqFull[*,2]], $
                              XTITLE='XGSM', $
                              YTITLE='YGSM', $
                              ZTITLE='ZGSM', $
                              'o', $
                              color='red', $
                              aspect_ratio=1.0, $
                              aspect_z=1.0)
              ;; plotFL = PLOT3D([fline_toIonosFull[*,0],fline_toEqFull[*,0],fline_toEqFlip[*,0],fline_toIonosFlip[*,0]], $
              ;;                 [fline_toIonosFull[*,1],fline_toEqFull[*,1],fline_toEqFlip[*,1],fline_toIonosFlip[*,1]], $
              ;;                 [fline_toIonosFull[*,2],fline_toEqFull[*,2],fline_toEqFlip[*,2],fline_toIonosFlip[*,2]], $
              ;;                 XTITLE='XGSM', $
              ;;                 YTITLE='YGSM', $
              ;;                 ZTITLE='ZGSM', $
              ;;                 'o', $
              ;;                 color='red', $
              ;;                 aspect_ratio=1.0, $
              ;;                 aspect_z=1.0)

              STOP



           ENDIF

           CASE 1 OF
              KEYWORD_SET(T89): BEGIN

                 GEOPACK_T89,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                             downTail_Bx,downTail_By,downTail_Bz, $
                             TILT=thisTilt, $
                             EPOCH=time_epoch[k]

                 GEOPACK_T89,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                             FAST_Bx,FAST_By,FAST_Bz, $
                             TILT=thisTilt, $
                             EPOCH=time_epoch[k]

                 GEOPACK_T89,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                              ionos_Bx,ionos_By,ionos_Bz, $
                              TILT=thisTilt, $
                              EPOCH=time_epoch[k]

              END
              KEYWORD_SET(T96): BEGIN

                 GEOPACK_T96,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                             downTail_Bx,downTail_By,downTail_Bz, $
                             TILT=thisTilt, $
                             EPOCH=time_epoch[k]

                 GEOPACK_T96,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                             FAST_Bx,FAST_By,FAST_Bz, $
                             TILT=thisTilt, $
                             EPOCH=time_epoch[k]

                 GEOPACK_T96,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                             ionos_Bx,ionos_By,ionos_Bz, $
                             TILT=thisTilt, $
                             EPOCH=time_epoch[k]

              END
              KEYWORD_SET(T01): BEGIN

                 GEOPACK_T01,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                             downTail_Bx,downTail_By,downTail_Bz, $
                             TILT=thisTilt, $
                             EPOCH=time_epoch[k]

                 GEOPACK_T01,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                             FAST_Bx,FAST_By,FAST_Bz, $
                             TILT=thisTilt, $
                             EPOCH=time_epoch[k]

                 GEOPACK_T01,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                             ionos_Bx,ionos_By,ionos_Bz, $
                             TILT=thisTilt, $
                             EPOCH=time_epoch[k]

              END
              ELSE: BEGIN

                 GEOPACK_TS04,tmpParMod,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                              downTail_Bx,downTail_By,downTail_Bz, $
                              TILT=thisTilt, $
                              EPOCH=time_epoch[k], $
                              IOPGEN=IOPGen, $
                              IOPT=IOPT, $
                              IOPB=IOPB, $
                              IOPR=IOPR

                 GEOPACK_TS04,tmpParMod,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                              FAST_Bx,FAST_By,FAST_Bz, $
                              TILT=thisTilt, $
                              EPOCH=time_epoch[k], $
                              IOPGEN=IOPGen, $
                              IOPT=IOPT, $
                              IOPB=IOPB, $
                              IOPR=IOPR

                 GEOPACK_TS04,tmpParMod,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                              ionos_Bx,ionos_By,ionos_Bz, $
                              TILT=thisTilt, $
                              EPOCH=time_epoch[k], $
                              IOPGEN=IOPGen, $
                              IOPT=IOPT, $
                              IOPB=IOPB, $
                              IOPR=IOPR

              END
           ENDCASE

           GEOPACK_IGRF_GSW_08,downTail_GSM_x,downTail_GSM_y,downTail_GSM_z, $
                               downTail_Bx_IGRF,downTail_By_IGRF,downTail_Bz_IGRF, $
                               EPOCH=time_epoch[k]

           GEOPACK_IGRF_GSW_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                               FAST_Bx_IGRF,FAST_By_IGRF,FAST_Bz_IGRF, $
                               EPOCH=time_epoch[k]

           GEOPACK_IGRF_GSW_08,ionos_GSM_x,ionos_GSM_y,ionos_GSM_z, $
                               ionos_Bx_IGRF,ionos_By_IGRF,ionos_Bz_IGRF, $
                               EPOCH=time_epoch[k]


           FAST_GSM     = [FAST_GSM_x,FAST_GSM_y,FAST_GSM_z]
           downTail_GSM = [downTail_GSM_x,downTail_GSM_y,downTail_GSM_z]
           ionos_GSM    = [ionos_GSM_x,ionos_GSM_y,ionos_GSM_z]



           Lshell = GET_FA_L_SHELL_FROM_GSM_POS(FAST_GSM_x,FAST_GSM_y,FAST_GSM_z,tmpParMod, $
                                                trace_to_equator, $
                                                T89=T89, $
                                                T96=T96, $
                                                T01=T01, $
                                                TS04=TS04, $
                                                IGRF=IGRF, $
                                                TILT=thisTilt, $ ;should be in degrees
                                                EPOCH=time_epoch[k], $
                                                DSMAX=dsMax, $
                                                ERR=traceErr)

           Lshell_IGRF = GET_FA_L_SHELL_FROM_GSM_POS(FAST_GSM_x,FAST_GSM_y,FAST_GSM_z,tmpParMod, $
                                                     trace_to_equator, $
                                                     /IGRF, $
                                                     TILT=thisTilt, $ ;should be in degrees
                                                     EPOCH=time_epoch[k], $
                                                     DSMAX=dsMax, $
                                                     ERR=traceErr)


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

           FAST_Lshell_arr[k]         = TEMPORARY(Lshell)
           FAST_Lshell_IGRF_arr[k]     = TEMPORARY(Lshell_IGRF)

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
                                            downTail : 1}, $
                               Lshell    : {T        : FAST_Lshell_arr, $
                                            IGRF     : FAST_Lshell_IGRF_arr}, $
                               name      : TEMPORARY(navn)}

        mRStruc = CREATE_STRUCT(struc,mRStruc,'DOY',DOYstruct,'SWDAT',swDat,'BMODELINFO',BModelInfo)

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
