;;2017/02/14
FUNCTION MOMENTERRORS_2D__FROM_DIFF_EFLUX,diff_eFlux, $
                                          ENERGY=en, $
                                          ERANGE=er, $
                                          EBINS=ebins, $
                                          ANGLE=an, $
                                          ARANGE=ar, $
                                          BINS=bins, $
                                          ;; CONV_TO_CM=conv_to_cm, $
                                          QUIET=quiet

  COMPILE_OPT IDL2

  ex_start    = SYSTIME(1)

  max         = N_ELEMENTS(diff_eFlux.data_name)
  ;; time        = (diff_eFlux.time+diff_eFlux.end_time)/2.
  ;; j           = {x:TEMPORARY(time),y:MAKE_ARRAY(max,/FLOAT)}

  ;; species     = diff_eFlux.mass GT 6e-6 ;0=electron, 1=ion; electron mass is 5.68566e-06 eV/c^2 (c in km/s)
  ;; energy      = diff_eFlux.energy[*,0,0]
  ;; theta       = REFORM(diff_eFlux.theta[0,*,0])

  errThingList = LIST()
  FOR k=0,max-1 DO BEGIN

     errThing    = MOMENTERRORS_2D(MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,k), $
                                   ENERGY=en,ERANGE=er,EBINS=ebins,$
                                   ANGLE=an,ARANGE=ar, $
                                   BINS=bins);; , $
                                   ;; CONV_TO_CM=conv_to_cm
     
     errThingList.Add,TEMPORARY(errThing)

  ENDFOR
  
  ex_time     = SYSTIME(1) - ex_start
  IF ~KEYWORD_SET(quiet) THEN BEGIN
     MESSAGE,STRING(ex_time)+' seconds execution time.',/CONT,/INFO
     PRINT,'Number of data points = ',max
  ENDIF

  RETURN,errThingList

END

