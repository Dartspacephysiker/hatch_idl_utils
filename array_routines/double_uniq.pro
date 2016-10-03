;;10/03/16
FUNCTION DOUBLE_UNIQ,nums,decimal_place, $
                     DELTA=delta, $
                     SORT=sort, $
                     NULL=null, $
                     NANS=NaN, $
                     FLOOR=floor, $
                     CEILING=ceiling, $
                     QUIET=quiet, $
                     OUT_DELTMOD=deltMod, $
                     OUT_DIFFTOT=diffTot, $
                     UNIQVALS=uniqVals, $
                     N_EACH_UNIQVAL=n_each_uniqVal, $
                     NUNIQ=nUniq

  COMPILE_OPT IDL2


  retVal = KEYWORD_SET(null) ? !NULL : -1

  type = SIZE(nums,/TYPE)
  IF (type NE 5) AND (type NE 4) THEN BEGIN
     PRINT,'Nums not of type double or float! Out ...'
     RETURN,retVal
  ENDIF

  IF N_ELEMENTS(decimal_place) EQ 0 THEN BEGIN
     PRINT,"Precision must be specified via 'decimal_place' positional param. Out!"
     RETURN,retVal
  ENDIF

  IF KEYWORD_SET(NaN) THEN BEGIN
     good = WHERE(FINITE(nums),nGood,NCOMPLEMENT=nNaN,COMPLEMENT=nan_i)
     IF nGood EQ 0 THEN BEGIN
        MESSAGE,"No good data--they're all NaNs! Out ..."
     ENDIF
     ;; PRINT,"NaN stuff not currently supported ..."
  ENDIF ELSE BEGIN
     good = LINDGEN(N_ELEMENTS(nums))
  ENDELSE

  IF KEYWORD_SET(sort) THEN BEGIN
     good = good[SORT(nums[good])]
  ENDIF ELSE BEGIN
     IF ~ARRAY_EQUAL(SORT(nums[good]),INDGEN(N_ELEMENTS(good))) THEN BEGIN
        PRINT,"Array is unsorted! Exiting ..."
        RETURN,retVal
     ENDIF
  ENDELSE

  rounded = ROUND_TO_NTH_DECIMAL_PLACE(nums[good],decimal_place, $
                                       FLOOR=floor, $
                                       CEILING=ceiling, $
                                       /DOUBLE, $
                                       /LEAVE_AS_INTEGER)


  uniq_ii     = UNIQ(rounded,SORT(rounded))
  
  ;; uniq_iii    = WHERE(rounded[uniq_ii] GT 0)
  ;; uniq_ii     = uniq_ii[uniq_iii]

  IF N_ELEMENTS(delta) GT 0 THEN BEGIN
     deltMod   = FIX(ROUND(delta*(10.D)^(-1.D*decimal_place)),TYPE=14)

     ;;Check for double-sided winners
     IF N_ELEMENTS(uniq_ii) GT 3 THEN BEGIN

        rm_iii = !NULL
        solid      = !NULL
        nRemove    = 0
        FOR k=N_ELEMENTS(uniq_ii)-2,1,-1 DO BEGIN ;;work backwards


           checkMe = ((rounded[uniq_ii[k]] - rounded[uniq_ii[k-1]]) LE deltMod) AND $
                     ((rounded[uniq_ii[k+1]] - rounded[uniq_ii[k]]) LE deltMod)

           IF N_ELEMENTS(rm_iii) GT 0 THEN BEGIN
              checkMe = checkMe AND ( (WHERE(rm_iii EQ k))[0] EQ -1)
           ENDIF

           IF checkMe THEN BEGIN

              solid = [solid,k]

              IF nRemove GT 0 THEN BEGIN
                 IF (WHERE(solid EQ (k-1)))[0] EQ -1 THEN BEGIN
                    rm_iii = [rm_iii,k-1]
                    nRemove++
                 ENDIF
                 
                 IF (WHERE(solid EQ (k+1)))[0] EQ -1 THEN BEGIN
                    rm_iii = [rm_iii,k+1]
                    nRemove++
                 ENDIF

              ENDIF ELSE BEGIN
                 rm_iii = [rm_iii,k-1,k+1]
                 nRemove    = 2
              ENDELSE

           ENDIF

        ENDFOR

        IF N_ELEMENTS(rm_iii) NE 0 THEN BEGIN
           rm_iii = rm_iii[UNIQ(rm_iii,SORT(rm_iii))]
           REMOVE,rm_iii,uniq_ii
        ENDIF

     ENDIF

     uniqMod_iii = !NULL
     FOR k=N_ELEMENTS(uniq_ii)-1,0,-1 DO BEGIN
        check = WHERE(ABS(rounded[uniq_ii[k]]-rounded[uniq_ii]) LE deltMod,nCheck)

        IF nCheck GT 0 THEN BEGIN
           uniqMod_iii = [uniqMod_iii,check[-1]] ;just keep largest unique elem
        ENDIF
     ENDFOR

     uniq_ii = uniq_ii[uniqmod_iii[UNIQ(uniqmod_iii,SORT(uniqmod_iii))]]
  ENDIF ELSE BEGIN
     deltMod = type EQ 4 ? 1e-7 : 1e-16
  ENDELSE

  nUniq       = N_ELEMENTS(uniq_ii)

  IF ARG_PRESENT(uniqVals) THEN BEGIN
     uniqVals = nums[good[uniq_ii]]
     IF KEYWORD_SET(NaN) THEN BEGIN
        uniqVals = [uniqVals,!VALUES.F_NaN]
     ENDIF
  ENDIF

  IF ARG_PRESENT(n_each_uniqVal) THEN BEGIN
     n_each_uniqVal = MAKE_ARRAY(nUniq+KEYWORD_SET(NaN),/LONG,VALUE=0)
     FOR k=0,nUniq-1 DO BEGIN
        n_each_uniqVal[k] = N_ELEMENTS(WHERE(ABS(rounded[uniq_ii[k]]-rounded) LE deltMod))
     ENDFOR

     IF KEYWORD_SET(NaN) THEN BEGIN
        n_each_uniqVal[-1] = nNan
     ENDIF     
  ENDIF

  IF KEYWORD_SET(quiet) THEN RETURN,good[uniq_ii]

  PRINT,FORMAT='(A0,T15,A0,T30,A0,T45,A0,T60,A0)', $
        "Uniq Val #", $
        "Index", $
        "Number", $
        "Rounded", $
        "N Occurrences"

  numString = 'G0.' + STRCOMPRESS(ABS(decimal_place)+3,/REMOVE_ALL)
  fmtString = '(I0,T15,I0,T30,' + numString + ',T45,I0,T60,I0)'

  FOR k=0,nUniq-1 DO BEGIN
     tmpI = (good[uniq_ii])[k]
     PRINT,FORMAT=fmtString, $
           k+1, $
           tmpI, $
           nums[tmpI], $
           rounded[uniq_ii[k]], $
           N_ELEMENTS(WHERE(ABS(rounded[uniq_ii[k]]-rounded) LE deltMod))
  ENDFOR

  IF KEYWORD_SET(NaN) THEN BEGIN
     tmpI = nan_i[0]
     PRINT,FORMAT='(I0,T15,I0,T30,G0.2,T60,I0)', $
           k+1, $
           tmpI, $
           nums[tmpI], $
           nNaN
  ENDIF

  RETURN,good[uniq_ii]
END
