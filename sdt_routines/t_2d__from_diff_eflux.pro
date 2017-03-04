;;2017/02/21
FUNCTION T_2D__FROM_DIFF_EFLUX,diff_eFlux, $
                               ENERGY=en, $
                               ERANGE=er, $
                               EBINS=ebins, $
                               ANGLE=an, $
                               ARANGE=ar, $
                               BINS=bins, $
                               SC_POT=sc_pot, $
                               EEB_OR_EES=eeb_or_ees, $
                               QUIET=quiet

  COMPILE_OPT IDL2,STRICTARRSUBS

  ex_start    = SYSTIME(1)

  max         = N_ELEMENTS(diff_eFlux.data_name)
  time        = (diff_eFlux.time+diff_eFlux.end_time)/2.
  T           = {x:TEMPORARY(time),y:MAKE_ARRAY(4,max,/FLOAT)}

  IF N_ELEMENTS(en) GT 1 AND NDIMEN(en) LT 2 THEN BEGIN

     en_arr   = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX(diff_eFlux, $
                                               ENERGY=en, $
                                               SC_POT=sc_pot, $
                                               EEB_OR_EES=eeb_or_ees)
     
     en       = TEMPORARY(en_arr)

  ENDIF

  FOR k=0,max-1 DO BEGIN

     T.y[*,k] = T_2D_FS(MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,k), $
                        ENERGY=N_ELEMENTS(en) GT 0 ? en[*,k] : !NULL , $
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

  RETURN,T

END
