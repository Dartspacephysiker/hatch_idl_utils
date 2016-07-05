;2016/07/04
FUNCTION EQUAL_SCALE_ARR,input, $
                         QUIET=quiet

  COMPILE_OPT idl2

  GET_SINGLE_INDEX_STREAKS,input, $
                           N_STREAKS=n_streaks, $
                           START_I=start_i, $
                           STOP_I=stop_i, $
                           SINGLE_I=single_i, $
                           N_SINGLE=nSingle, $
                           OUT_STREAKLENS=streaklens, $
                           /KEEP_SINGLES, $
                           NO_PRINT_SUMMARY=quiet

  nArr    = N_ELEMENTS(input)
  nSpaces = N_ELEMENTS(start_i)
  
  delta   = nArr/FLOAT(nSpaces)

  this    = MAKE_ARRAY(nSpaces,VALUE=FLOOR(delta))

  IF ROUND(TOTAL(this)) NE nArr THEN BEGIN
     n_to_fix = nArr - ROUND(TOTAL(this))
     fixTemp  = INDGEN(n_to_fix)
     fixOrder = !NULL
     FOR k=0,n_to_fix-1 DO BEGIN
        fixOrder = [fixOrder,fixTemp[k],(-1)*fixTemp[k]-1]
     ENDFOR

     FOR k=0,n_to_fix-1 DO BEGIN
        temp_i = fixOrder[k]
        this[temp_i]++
     ENDFOR

  ENDIF

  eqScale_input = !NULL
  FOR k=0,nSpaces-1 DO BEGIN
     eqScale_input = [eqScale_input,MAKE_ARRAY(this[k],VALUE=input[start_i[k]])]
  ENDFOR

  RETURN,eqScale_input

END