;2016/05/10

FUNCTION DERIV_REAL__COMPLEX_STEP,x, $
                                  STEPSIZE=h

  good_data_types     = [2,3,4,5,12,13,14,15]
  dType               = SIZE(x,/TYPE)
  CASE (WHERE(dType EQ good_data_types))[0] OF
     -1: BEGIN
        PRINT,"You've provided a data type I can't deal with! No friend of mine..."
        PRINT,FORMAT='("Data type: ",I0)',dType
        badData       = 1
     END
     ELSE: BEGIN
        badData       = 0
     END
  ENDCASE

  IF N_ELEMENTS(x LE 1) THEN BEGIN
     PRINT,"Can't compute derivative from one element!"
     badData          = 1
  ENDIF

  ;;Drop it!
  IF badData THEN RETURN,-1

  IF ~KEYWORD_SET(h) THEN h = DOUBLE(1e-8)

  cx                  = COMPLEX(x,h)

  RETURN,IMAGINARY(cx)

END