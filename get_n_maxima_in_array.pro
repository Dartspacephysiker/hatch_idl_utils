;+
; NAME:                       GET_N_MAXIMA_IN_ARRAY
;
;
;
; PURPOSE:                    Get the first N maxima in this array
;
;
;
; CATEGORY:                   à la Hammertime
;
;
;
; INPUTS:                     data               : An array of data to be searched for maxima.
;
; KEYWORD PARAMETERS:         DO_MINIMA          : Do minima instead of maxima
;
; OUTPUTS:                    maxVec             : The array of (max|min)ima
;
; OPTIONAL OUTPUTS:           OUT_I              : Indices of the located extrema
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY: 2016/01/30 Barnebarn
;
;-

FUNCTION GET_N_MAXIMA_IN_ARRAY,data, $
                               DO_MINIMA=do_minima, $
                               N=n, $
                               OUT_I=out_i

  ;;Take care of bogus sitiations (and I do mean sitiations)
  nData                        = N_ELEMENTS(data)
  IF nData LE 2 THEN BEGIN
     PRINT,"GET_N_MAXIMA_IN_ARRAY: Data must have more than 2 elements! (If you just have one, use MAX)"
        PRINT,"Out"
     RETURN,-1
  ENDIF

  IF N_ELEMENTS(n) EQ 0 THEN BEGIN
     n                         = 2
  ENDIF ELSE BEGIN
     IF nData LE n THEN BEGIN
        PRINT,"GET_N_MAXIMA_IN_ARRAY: You've requested as many or more extrema than there are data points in data!"
        PRINT,"Out"
        RETURN,-1
     ENDIF
  ENDELSE

  tempData                     = data
  maxVec                       = !NULL
  max_i                        = !NULL

  check_these_i                = INDGEN(nData)
  breakOut                     = 0
  i                            = 0

  WHILE i LT n DO BEGIN
     nToCheck               = N_ELEMENTS(check_these_i)

     IF KEYWORD_SET(do_minima) THEN BEGIN
        tempMax                = MIN(tempData[check_these_i],temp_ii)
     ENDIF ELSE BEGIN
        tempMax                = MAX(tempData[check_these_i],temp_ii)
     ENDELSE

     ;;Add to vecs
     maxVec                 = [maxVec,tempMax]
     max_i                  = [max_i,check_these_i[temp_ii]]

     ;;Remove element from check_these_i array
     CASE temp_ii OF
        0: BEGIN
           IF nToCheck EQ 1 THEN BEGIN
              PRINT,"GET_N_MAXIMA_IN_ARRAY: Only one element left to check! Break out"
              breakOut      = 1
           ENDIF ELSE BEGIN
              check_these_i = check_these_i[1:*]
           ENDELSE
        END
        nToCheck-1: BEGIN
           check_these_i    = check_these_i[0:nToCheck-2]
        END
        ELSE: BEGIN
           check_these_i    = [check_these_i[0:temp_ii],check_these_i[temp_ii+1,nToCheck-1]]
        END
     ENDCASE
     i++
     IF breakOut THEN BREAK
  ENDWHILE

  out_i                     = max_i

  RETURN, maxVec

END