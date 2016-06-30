PRO REDUCE_DIFF_EFLUX,diff_eFlux

  COMPILE_OPT idl2

  IF SIZE(diff_eFlux,/TYPE) NE 8 THEN BEGIN
     PRINT,'diff_eFlux must be a struct!'
     STOP
  ENDIF
  
  ;;New struct elements--they'll be modified
  xNew              = diff_eFlux.x
  yNew              = diff_eFlux.y

  aNew              = diff_eFlux.angles
  shiftMe           = WHERE(aNew GT 180.)
  IF shiftMe[0] NE -1 THEN BEGIN
     aNew[shiftMe]  = aNew[shiftMe] - 360.
  ENDIF

  tNew              = diff_eFlux.time

  sNew              = diff_eFlux.shiftVals
  sNew[*]           = 0

  CASE SIZE(xNew,/N_DIMENSIONS) OF
     ;; 0: BEGIN
     ;; PRINT,"No diff_eFlux struct provided!!"
     ;; STOP
     ;; END
     1: BEGIN
        PRINT,'Huh?'
        STOP
     END
     2: BEGIN
        PRINT,'Huh?'
        STOP
     END
     3: BEGIN
        xNew        = MEAN(xNew,DIMENSION=2) ;Sum over angles
        yNew        = TOTAL(yNew,2)

        nAngles     = N_ELEMENTS(aNew[0,*])
        aNew        = TOTAL(aNew,2)/FLOAT(nAngles)

        diff_eFlux  = {x:xNew, $
                       y:yNew, $
                       angles:aNew, $
                       time:tNew, $
                       shiftVals:sNew}
     END
  ENDCASE

  
END