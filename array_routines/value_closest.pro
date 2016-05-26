;;2015/08/13 
;; Wild improvements over VALUE_LOCATE. I want to know WHICH is closest, for crying out loud!
;; This also provides the ability to pick up only indices corresponding to vector values that are
;; less than or equal to the provided array of 'values' (ONLY_LE), or only indices corresponding to
;; vector values that are greater than or equal to the provided array of 'values' (ONLY_GE).
;; They keywords aren't backwards; the point is to ensure that 'values' are all GE than
;;vector vals with /ONLY_GE, and vice versa with /ONLY_LE.

FUNCTION VALUE_CLOSEST,vector,values,diffs,ONLY_GE=only_ge,ONLY_LE=only_LE, $
                       SUCCESS=success, $
                       BATCH_MODE=batch_mode, $
                       QUIET=quiet

  COMPILE_OPT idl2

  BADVAL                        = -9999
  success                       = 0 ;assume no success

  IF N_ELEMENTS(vector) EQ 0 OR N_ELEMENTS(values) EQ 0 THEN BEGIN
     IF ~KEYWORD_SET(QUIET) THEN PRINT,"You gave me junk, son. 'vector' and 'values' have to have more than zero elements."
     RETURN,!NULL

  ENDIF

  check_sorted,vector,is_sorted,/QUIET
  IF ~is_sorted THEN BEGIN
     IF ~KEYWORD_SET(QUIET) THEN PRINT,"'vector' isn't sorted! Can't work with it as it is...Sort, then try again."
     RETURN,!NULL
  ENDIF

  ;; locs                       = VALUE_LOCATE(vector,values)

  nVal                          = N_ELEMENTS(values)
  nVec                          = N_ELEMENTS(vector)

  IF (nVal * nVec) GT 10000000L THEN BEGIN 
     answer                     = ''
     IF KEYWORD_SET(batch_mode) THEN BEGIN
        answer                  = 'y'
     ENDIF ELSE BEGIN
        READ,answer, PROMPT="This involves ten million elements! (" + STRCOMPRESS(nVal*nVec,/REMOVE_ALL) + ", to be exact) Proceed? (y/n)"
     ENDELSE
     IF STRLOWCASE(STRMID(answer,0,1)) NE 'y' THEN BEGIN
        IF ~KEYWORD_SET(quiet) THEN PRINT,"OK, exiting..."
        RETURN,!NULL
     ENDIF
  ENDIF

  closest_i                     = MAKE_ARRAY(nVal,/L64)
  diffs                         = MAKE_ARRAY(nVal,TYPE=SIZE(values[0], /TYPE))

  IF KEYWORD_SET(only_ge) THEN BEGIN
     closest_i                  = VALUE_LOCATE(vector,values)
     diffs                      = values-vector[closest_i]

     bad_i=WHERE(diffs LT 0.)
     IF bad_i[0] NE -1 THEN BEGIN
        diffs[bad_i]            = !VALUES.F_NAN
        closest_i[bad_i]        = BADVAL
     ENDIF

  ENDIF ELSE BEGIN
     IF KEYWORD_SET(only_le) THEN BEGIN
        closest_i               = VALUE_LOCATE(vector,values)
        diffs                   = values-vector[closest_i]

        safety_i                = WHERE(closest_i LT (N_ELEMENTS(vector) - 1) AND ( ABS(diffs) GT 0.) )
        IF safety_i[0] NE -1 THEN BEGIN
           closest_i[safety_i]  = closest_i[safety_i] + 1 ;This line preserves those values that are at the top of the range
        ENDIF

        diffs                   = values-vector[closest_i]

        bad_i                   = WHERE(diffs GT 0.)
        IF bad_i[0] NE -1 THEN BEGIN
           diffs[bad_i]         = !VALUES.F_NAN
           closest_i[bad_i]     = BADVAL
        ENDIF

     ENDIF ELSE BEGIN
        FOR i=0,nVal-1 DO BEGIN
           near                 = MIN(ABS(vector-values[i]),index)
           
           diffs[i]             = near
           closest_i[i]         = index
        ENDFOR
        
        temp                    = VALUE_LOCATE(vector,values)
        lt_vec                  = WHERE(temp LT 0)
        IF lt_vec[0] NE -1 THEN BEGIN
           temp[lt_vec]         = 0
           diffs[lt_vec]        = -1. * diffs[lt_vec]
        ENDIF
        
        flipSign                = WHERE((vector[closest_i] - vector[temp]) GT 0)
        IF flipSign[0] NE -1 THEN BEGIN
           diffs[flipSign]      = -1. * diffs[flipSign]
        ENDIF
     ENDELSE
  ENDELSE
  
  success                       = 1
  RETURN,closest_i

END