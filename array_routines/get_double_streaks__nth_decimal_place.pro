;;09/30/16
;;This routine will give you streaks of indices for a given resolution. 
;;   For a FA_FIELDS_BUFS-style treatment of double-type time series, please see GET_DOUBLE_BUFS__NTH_DECIMAL_PLACE.
;;
PRO GET_DOUBLE_STREAKS__NTH_DECIMAL_PLACE, $
   nums,decimal_place, $
   MAXIMUS=maximus, $
   CURRENT_FOR_PRINTING=pCurrent, $
   ORBIT_FOR_PRINTING=pOrbit, $
   ALT_FOR_PRINTING=pAlt, $
   MLT_FOR_PRINTING=pMLT, $
   ILAT_FOR_PRINTING=pILAT, $
   NPTS=n, $
   MIN_T_STREAKLEN=min_streakLen_t, $
   GAP_TIME=gap_time, $
   START_I=start_i, $
   STOP_I=stop_i, $
   STREAKLENS=streakLens, $
   T_STREAKLENS=streakLens_t, $
   NSTREAKS=nStreaks, $
   FLOOR=floor, $
   CEILING=ceiling, $
   PRINT_START_STOP_TIMES=print_start_stop_times, $
   PRINT__INCLUDE_CURRENT=print__include_current, $
   PRINT_MAXIMUS__INCLUDE_CURRENT=print_maximus__include_current, $
   SORT_BY_STREAKLEN=sort_by_streakLen, $
   SORT_BY_T_STREAKLEN=sort_by_streakLen_t, $
   SORT_BY_AVG_DT=sort_by_avg_dt, $
   SORT_BY_MAGNITUDE_ESA_CURRENT=ABS_jESA_sort, $
   SORT_BY_MAGNITUDE_MAG_CURRENT=ABS_jMAG_sort, $
   SORT_BY_ESA_CURRENT=jESA_sort, $
   SORT_BY_MAG_CURRENT=jMAG_sort, $
   SORT_REVERSE=sort_reverse, $
   NO_SORT=no_sort, $
   OUTLUN=outLun

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF SIZE(nums,/TYPE) NE 5 THEN BEGIN
     PRINT,'Nums not of type double! Out ...'
     RETURN
  ENDIF

  IF N_ELEMENTS(decimal_place) EQ 0 THEN BEGIN
     PRINT,"Decimal place must be specified to search for streaks. Out!"
     RETURN
  ENDIF

  IF N_ELEMENTS(n) EQ 0 THEN n = 1

  IF n EQ 0 THEN BEGIN
     PRINT,"N must be greater than zero!"
     RETURN
  ENDIF

  IF N_ELEMENTS(nums) LE n THEN BEGIN
     PRINT,'Insufficient data to search for streak length = ' + $
           STRCOMPRESS(n,/REMOVE_ALL) + '! Out ...'
     RETURN
  ENDIF

  IF ~ARRAY_EQUAL(SORT(nums),LINDGEN(N_ELEMENTS(nums))) THEN BEGIN
     PRINT,"Array is unsorted! Exiting ..."
     RETURN
  ENDIF

  rounded = ROUND_TO_NTH_DECIMAL_PLACE(nums,decimal_place, $
                                       FLOOR=floor, $
                                       CEILING=ceiling, $
                                       /DOUBLE, $
                                       /LEAVE_AS_INTEGER)


  diffTot     = rounded - SHIFT(rounded,1)

  ;;Anything less than gap_time gets subsumed
  IF KEYWORD_SET(gap_time) THEN BEGIN
     gapMod   = FIX(ROUND(gap_time*(10.D)^(-1.D*decimal_place)),TYPE=14)

  ENDIF ELSE BEGIN
     gapMod   = 1
  ENDELSE

  streaksHere = 0

  start_i  = WHERE(diffTot GT gapMod,nStart)

  CASE start_i[0] OF
     -1: BEGIN
        start_i = 0
        nStart  = 1
     END
     ELSE: BEGIN
        IF diffTot[0] LT 0 THEN BEGIN
           start_i = [0,start_i]
           nStart++
        ENDIF
     END
  ENDCASE

  CASE nStart OF
     1: BEGIN
        stop_i     = N_ELEMENTS(diffTot)-1
        single_ii = -1

        streaksHere = 1
     END
     ELSE: BEGIN
        stop_i    = [start_i[1:-1]-1,N_ELEMENTS(diffTot)-1]
        single_ii = WHERE(start_i EQ stop_i,nSingle, $
                         COMPLEMENT=keep_ii,NCOMPLEMENT=nKeep)

        IF nKeep GT 0 THEN BEGIN
           start_i      = start_i[keep_ii]
           stop_i       = stop_i[keep_ii]

           streaksHere  = 1
        ENDIF
     END
  ENDCASE

  IF ~streaksHere THEN BEGIN
     start_i       = 0
     stop_i        = 0
     streakLens    = 0
     streakLens_t  = 0
     RETURN
  ENDIF

  tmpStrk        = stop_i-start_i

  goodStreak_ii  = WHERE(tmpStrk GE n,nGoodStreaks)

  IF nGoodStreaks EQ 0 THEN BEGIN
     doQuit      = 1
  ENDIF

  streakLens_t   = nums[stop_i] - nums[start_i]
  IF KEYWORD_SET(min_streakLen_t) AND ~KEYWORD_SET(doQuit) THEN BEGIN
     
     goodTStreak_ii = WHERE(streakLens_t GE min_streakLen_t,nGoodStreaks)

     IF nGoodStreaks EQ 0 THEN BEGIN
        doQuit      = 1
     ENDIF ELSE BEGIN

        goodStreak_ii = CGSETINTERSECTION(goodStreak_ii,goodTStreak_ii,COUNT=nStreaks)

        IF nStreaks EQ 0 THEN BEGIN
           doQuit = 1
        ENDIF
     ENDELSE

  ENDIF

  IF KEYWORD_SET(doQuit) THEN BEGIN
     start_i      = 0
     stop_i       = 0
     streakLens   = 0
     streakLens_t = 0
     RETURN
  ENDIF

  streakLens     = tmpStrk[goodStreak_ii]
  streakLens_t   = streakLens_t[goodStreak_ii]
  start_i        = start_i[goodStreak_ii]
  stop_i         = stop_i[goodStreak_ii]

  ;;Prep for sorting, printing, if requested
  maximus_sorting       = KEYWORD_SET(print_maximus__include_current) AND ISA(maximus)
  IF (KEYWORD_SET(ABS_jESA_sort) OR $
      KEYWORD_SET(ABS_jMAG_sort) OR $
      KEYWORD_SET(jESA_sort    ) OR $
      KEYWORD_SET(jMAG_sort    ))   $
  THEN BEGIN
     IF KEYWORD_SET(print_maximus__include_current) THEN BEGIN
     ENDIF ELSE BEGIN
        maximus_sorting = 0B
        PRINT,"Illegalities! Gotta include maximus if you want this to happen: "
        PRINT,FORMAT='(A0,T25,":",I0)',"ABS_jESA_sort",KEYWORD_SET(ABS_jESA_sort)
        PRINT,FORMAT='(A0,T25,":",I0)',"ABS_jMAG_sort",KEYWORD_SET(ABS_jMAG_sort)
        PRINT,FORMAT='(A0,T25,":",I0)',"jESA_sort    ",KEYWORD_SET(jESA_sort    )
        PRINT,FORMAT='(A0,T25,":",I0)',"jMAG_sort    ",KEYWORD_SET(jMAG_sort    )
        ABS_jESA_sort = !NULL
        ABS_jMAG_sort = !NULL
        jESA_sort     = !NULL
        jMAG_sort     = !NULL
     ENDELSE
  ENDIF

  IF KEYWORD_SET(sort_by_avg_dt) OR $
     KEYWORD_SET(maximus_sorting) THEN BEGIN
     
     mean_dt           = MAKE_ARRAY(nStreaks,VALUE=0.,/FLOAT)
     IF KEYWORD_SET(print_maximus__include_current) THEN BEGIN
        med_jESA       = MAKE_ARRAY(nStreaks,VALUE=0.,/FLOAT)
        med_jMAG       = MAKE_ARRAY(nStreaks,VALUE=0.,/FLOAT)
     ENDIF

     FOR k=0,nStreaks-1 DO BEGIN
        mean_dt[k]     = MEAN((nums[start_i[k]:stop_i[k]])[1:-1]- $
                           (nums[start_i[k]:stop_i[k]])[0:-2])
        IF KEYWORD_SET(print_maximus__include_current) THEN BEGIN
           med_jESA[k] = MEDIAN(maximus.esa_current[start_i[k]:stop_i[k]])
           med_jMAG[k] = MEDIAN(maximus.mag_current[start_i[k]:stop_i[k]])
        ENDIF
     ENDFOR
  ENDIF
  
  ;;Sorting stuff
  CASE 1 OF
     KEYWORD_SET(sort_by_streakLen): BEGIN
        sort     = SORT(streakLens)
     END
     KEYWORD_SET(sort_by_streakLen_t): BEGIN
        sort     = SORT(streakLens_t)
     END
     KEYWORD_SET(no_sort): BEGIN
        sort     = LINDGEN(nStreaks)
     END
     KEYWORD_SET(sort_by_avg_dt): BEGIN
        sort     = SORT(mean_dt)
     END
     KEYWORD_SET(ABS_jESA_sort): BEGIN
        sort     = SORT(ABS(med_jESA))
     END
     KEYWORD_SET(ABS_jMAG_sort): BEGIN
        sort     = SORT(ABS(med_jMAG))
     END
     KEYWORD_SET(jESA_sort    ): BEGIN
        sort     = SORT(med_jESA)
     END
     KEYWORD_SET(jMAG_sort    ): BEGIN     
        sort     = SORT(med_jMAG)
     END
     ELSE: BEGIN
        sort     = SORT(start_i)
     END
  ENDCASE

  IF KEYWORD_SET(sort_reverse) THEN BEGIN
     sort        = REVERSE(sort)
  ENDIF

  streakLens     = streakLens[sort]
  streakLens_t   = streakLens_t[sort]
  start_i        = start_i[sort]
  stop_i         = stop_i[sort]
  IF KEYWORD_SET(print_maximus__include_current) THEN BEGIN
     mean_dt     = mean_dt[sort]
     med_jESA    = med_jESA[sort]
     med_jMAG    = med_jMAG[sort]
  ENDIF

  IF KEYWORD_SET(print_start_stop_times) THEN BEGIN
     printLun = (N_ELEMENTS(outLun) GT 0 ? outLun : -1)
     CASE 1 OF
        KEYWORD_SET(print_maximus__include_current): BEGIN
           ;; PRINTF,printLun,FORMAT='(A0,T10,A0,T35,A0,T60,A0,T72,A0,T82,A0,T92,A0,T102,A0)', $
           ;;        'Orbit', $
           ;;        'Start T', $
           ;;        'Stop T', $
           ;;        'Len (s)', $
           ;;        'N pts', $
           ;;        'Avg dt', $
           ;;        'ESA Cur', $
           ;;        'Mag Cur'
           ;; FOR k=0,N_ELEMENTS(start_i)-1 DO BEGIN
           ;;    PRINTF,printLun, $
           ;;           FORMAT='(I0,T10,A0,T35,A0,T60,G-0.5,T72,I-10,T82,G-0.5,T92,G-0.5,T102,G-0.5)', $
           ;;           maximus.orbit[start_i[k]], $
           ;;           TIME_TO_STR(nums[start_i[k]],/MSEC), $
           ;;           TIME_TO_STR(nums[stop_i[k]],/MSEC), $
           ;;           nums[stop_i[k]]-nums[start_i[k]], $
           ;;           streakLens[k], $
           ;;           mean_dt[k], $
           ;;           med_jESA[k], $
           ;;           med_jMAG[k]
           ;; ENDFOR
           PRINTF,printLun,FORMAT='(A0,T10,A0,T20,A0,T30,A0,T40,A0,T65,A0,T90,A0,T102,A0,T112,A0,T122,A0,T132,A0)', $
                  'Orbit', $
                  'MLT', $
                  'ILAT', $
                  'ALT', $
                  'Start T', $
                  'Stop T', $
                  'Len (s)', $
                  'N pts', $
                  'Avg dt', $
                  'ESA Cur', $
                  'Mag Cur'
           FOR k=0,N_ELEMENTS(start_i)-1 DO BEGIN
              PRINTF,printLun, $
                     FORMAT='(I0,T10,F0.2,T20,F0.2,T30,F0.2,T40,A0,T65,A0,T90,G-0.5,T102,I-10,T112,G-0.5,T122,G-0.5,T132,G-0.5)', $
                     maximus.orbit[start_i[k]], $
                     maximus.mlt[start_i[k]], $
                     maximus.ilat[start_i[k]], $
                     maximus.alt[start_i[k]], $
                     TIME_TO_STR(nums[start_i[k]],/MSEC), $
                     TIME_TO_STR(nums[stop_i[k]],/MSEC), $
                     nums[stop_i[k]]-nums[start_i[k]], $
                     streakLens[k], $
                     mean_dt[k], $
                     med_jESA[k], $
                     med_jMAG[k]
           ENDFOR
        END
        KEYWORD_SET(print__include_current): BEGIN
           PRINTF,printLun,FORMAT='(A5,T7,A4,T13,A4,T19,A4,T25,A19,T46,A8,T56,A8,T66,A5,T73,A6,T81,A8)', $
                  'Orbit', $
                  'MLT', $
                  'ILAT', $
                  'Alt', $
                  'Start T', $
                  'Stop T', $
                  'Len (s)', $
                  'N pts', $
                  'Avg dt', $
                  'Current'
           FOR k=0,N_ELEMENTS(start_i)-1 DO BEGIN
              PRINTF,printLun, $
                     FORMAT='(I05,T7,F04.1,T12,F05.1,T19,I4,T25,A19,T46,A8,T56,G-8.5,T66,I-5,T73,G-6.3,T81,G-8.4)', $
                     MEDIAN(pOrbit[start_i[k]:stop_i[k]]), $
                     MEDIAN(pMLT[start_i[k]:stop_i[k]]), $
                     MEDIAN(pILAT[start_i[k]:stop_i[k]]), $
                     MEDIAN(pALT[start_i[k]:stop_i[k]]), $
                     TIME_TO_STR(nums[start_i[k]]), $
                     STRMID(TIME_TO_STR(nums[stop_i[k]]),11,12), $
                     nums[stop_i[k]]-nums[start_i[k]], $
                     streakLens[k], $
                     MEAN((nums[start_i[k]:stop_i[k]])[1:-1]- $
                          (nums[start_i[k]:stop_i[k]])[0:-2]), $
                     MEDIAN(pCurrent[start_i[k]:stop_i[k]])
           ENDFOR
        END
        ELSE: BEGIN
           PRINTF,printLun,FORMAT='(A0,T25,A0,T50,A0,T62,A0,T72,A0)', $
                  'Start T', $
                  'Stop T', $
                  'Len (s)', $
                  'N pts', $
                  'Avg dt'
           FOR k=0,N_ELEMENTS(start_i)-1 DO BEGIN
              PRINTF,outLun, $
                     FORMAT='(A0,T25,A0,T50,G-0.5,T65,I-10,T80,G-0.5)', $
                     TIME_TO_STR(nums[start_i[k]],/MSEC), $
                     TIME_TO_STR(nums[stop_i[k]],/MSEC), $
                     nums[stop_i[k]]-nums[start_i[k]], $
                     streakLens[k], $
                     MEAN((nums[start_i[k]:stop_i[k]])[1:-1]- $
                          (nums[start_i[k]:stop_i[k]])[0:-2])
           ENDFOR
        END
     ENDCASE
  ENDIF
END
