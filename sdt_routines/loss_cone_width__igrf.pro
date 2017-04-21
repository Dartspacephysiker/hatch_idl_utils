FUNCTION LOSS_CONE_WIDTH__IGRF,struc, $
                               JUST_ONE=just_one
;this pro given an altitude in m returns the half angle of the loss cone
;assuming a width of 90 deg at 100km

  IF ~KEYWORD_SET(just_one) THEN BEGIN
     MESSAGE,"Can't handle this",/CONTINUE
     STOP
  ENDIF

     MESSAGE,"Can't handle anything. Go write your thesis",/CONTINUE
     STOP
  

  ;; ratio     = DIPOLEFIELD(100.0*1000.0)/DIPOLEFIELD(altitude)
  ;; halfwidth = ATAN(SQRT(1/(ratio-1)))
  
  RETURN,halfwidth

END