;;09/30/16
;;This routine will give you streaks of indices for a given resolution. 
;;   For a FA_FIELDS_BUFS-style treatment of double-type time series, please see GET_DOUBLE_BUFS__NTH_DECIMAL_PLACE.
;;
PRO GET_DOUBLE_STREAKS__NTH_DECIMAL_PLACE, $
   nums,decimal_place, $
   MAXIMUS=maximus, $
   CURRENT_FOR_PRINTING=pCurrent, $
   NPTS=n, $
   MIN_T_STREAKLEN=min_streakLen_t, $
   GAP_TIME=gap_time, $
   START_I=start_i, $
   STOP_I=stop_i, $
   STREAKLENS=streakLens, $
   T_STREAKLENS=streakLens_t, $
   FLOOR=floor, $
   CEILING=ceiling, $
   PRINT_START_STOP_TIMES=print_start_stop_times, $
   PRINT__INCLUDE_CURRENT=print__include_current, $
   PRINT_MAXIMUS__INCLUDE_CURRENT=print_maximus__include_current, $
   OUTLUN=outLun

  COMPILE_OPT IDL2

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

  IF diffTot[0] LT 0 THEN BEGIN
     start_i = [0,start_i]
     nStart++
  ENDIF

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

        goodStreak_ii = CGSETINTERSECTION(goodStreak_ii,goodTStreak_ii,COUNT=nGoodStreaks)

        IF nGoodStreaks EQ 0 THEN BEGIN
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

  sort           = SORT(start_i)
  streakLens     = streakLens[sort]
  streakLens_t   = streakLens_t[sort]
  start_i        = start_i[sort]
  stop_i         = stop_i[sort]


  IF KEYWORD_SET(print_start_stop_times) THEN BEGIN
     printLun = (N_ELEMENTS(outLun) GT 0 ? outLun : -1)
     PRINTF,outLun,FORMAT='(A0,T25,A0,T50,A0,T62,A0,T72,A0)','Start T', $
            'Stop T', $
            'Tot T diff', $
            'N Diff', $
            'Avg T diff'
     FOR k=0,N_ELEMENTS(start_i)-1 DO BEGIN
        CASE 1 OF
           KEYWORD_SET(print_maximus__include_current): BEGIN
              PRINTF,outLun, $
                     FORMAT='(A0,T25,A0,T50,G-0.5,T62,I-10,T72,G-0.5,T82,G-0.5,T92,G-0.5)', $
                     TIME_TO_STR(nums[start_i[k]],/MSEC), $
                     TIME_TO_STR(nums[stop_i[k]],/MSEC), $
                     nums[stop_i[k]]-nums[start_i[k]], $
                     streakLens[k], $
                     MEAN((nums[start_i[k]:stop_i[k]])[1:-1]- $
                          (nums[start_i[k]:stop_i[k]])[0:-2]), $
                     MEDIAN(maximus.esa_current[start_i[k]:stop_i[k]]), $
                     MEDIAN(maximus.mag_current[start_i[k]:stop_i[k]])
           END
           KEYWORD_SET(print__include_current): BEGIN
              PRINTF,outLun, $
                     FORMAT='(A0,T25,A0,T50,G-0.5,T62,I-10,T72,G-0.5,T82,G-0.5)', $
                     TIME_TO_STR(nums[start_i[k]],/MSEC), $
                     TIME_TO_STR(nums[stop_i[k]],/MSEC), $
                     nums[stop_i[k]]-nums[start_i[k]], $
                     streakLens[k], $
                     MEAN((nums[start_i[k]:stop_i[k]])[1:-1]- $
                          (nums[start_i[k]:stop_i[k]])[0:-2]), $
                     MEDIAN(pCurrent[start_i[k]:stop_i[k]])

           END
           ELSE: BEGIN
              PRINTF,outLun, $
                     FORMAT='(A0,T25,A0,T50,G-0.5,T65,I-10,T80,G-0.5)', $
                     TIME_TO_STR(nums[start_i[k]],/MSEC), $
                     TIME_TO_STR(nums[stop_i[k]],/MSEC), $
                     nums[stop_i[k]]-nums[start_i[k]], $
                     streakLens[k], $
                     MEAN((nums[start_i[k]:stop_i[k]])[1:-1]- $
                          (nums[start_i[k]:stop_i[k]])[0:-2])
           END
        ENDCASE
     ENDFOR
  ENDIF
END
