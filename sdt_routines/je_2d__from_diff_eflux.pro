;;2017/02/14
FUNCTION JE_2D__FROM_DIFF_EFLUX,diff_eFlux, $
                                ENERGY=en, $
                                ERANGE=er, $
                                EBINS=ebins, $
                                ANGLE=an, $
                                ARANGE=ar, $
                                BINS=bins, $
                                QUIET=quiet

  COMPILE_OPT IDL2

  ex_start    = SYSTIME(1)

  max         = N_ELEMENTS(diff_eFlux.data_name)
  time        = (diff_eFlux.time+diff_eFlux.end_time)/2.
  je          = {x:TEMPORARY(time),y:MAKE_ARRAY(max,/FLOAT)}

  FOR k=0,max-1 DO BEGIN

     je.y[k]  = JE_2D_FS(MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,k), $
                         ENERGY=en, $
                         ERANGE=er, $
                         EBINS=ebins, $
                         ANGLE=an, $
                         ARANGE=ar, $
                         BINS=bins)
     
  ENDFOR

  ex_time     = SYSTIME(1) - ex_start
  IF ~KEYWORD_SET(quiet) THEN BEGIN
     MESSAGE,STRING(ex_time)+' seconds execution time.',/CONT,/INFO
     PRINT,'Number of data points = ',max
  ENDIF
  
  RETURN,je

END
