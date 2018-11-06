;;2017/02/14
FUNCTION MOMENTERRORS_2D__FROM_DIFF_EFLUX,diff_eFlux, $
                                          ENERGY=en, $
                                          ERANGE=er, $
                                          EBINS=ebins, $
                                          ANGLE=an, $
                                          ARANGE=ar, $
                                          BINS=bins, $
                                          PRESSURE_COVAR_CALC=pressure_covar_calc, $
                                          HEATFLUX_COVAR_CALC=heatFlux_covar_calc, $
                                          ;; CONV_TO_CM=conv_to_cm, $
                                          SC_POT=sc_pot, $
                                          EEB_OR_EES=eeb_or_ees, $
                                          QUIET=quiet, $
                                          SUMMARY_QUIET=summary_quiet, $
                                          MCFADDEN_STYLE_DIFF_EFLUX=McFadden_style_diff_eFlux

  COMPILE_OPT IDL2,STRICTARRSUBS

  ex_start    = SYSTIME(1)
  max         = N_ELEMENTS(diff_eFlux.data_name)
  N_R         = 4 + (KEYWORD_SET(pressure_covar_calc) ? 6 : 0) + (KEYWORD_SET(heatFlux_covar_calc) ? 3 : 0)
  errThingArr = MAKE_BLANK_GERSHMAN_ERROR_STRUCT(max,N_R, $
                                                 PRESSURE_COVAR_CALC=pressure_covar_calc, $
                                                 HEATFLUX_COVAR_CALC=heatFlux_covar_calc)
  
  IF N_ELEMENTS(en) GT 1 AND NDIMEN(en) LT 2 THEN BEGIN

     en_arr   = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX(diff_eFlux, $
                                               ENERGY=en, $
                                               SC_POT=sc_pot, $
                                               EEB_OR_EES=eeb_or_ees)
     
     en       = TEMPORARY(en_arr)

  ENDIF

  FOR k=0,max-1 DO BEGIN

     errThing    = MOMENTERRORS_2D((KEYWORD_SET(McFadden_style_diff_eFlux) ? $
                                    MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX__MCFADDEN_STYLE(diff_eFlux,k, $
                                                                                       HAS_SC_POT=(SIZE(sc_pot,/TYPE) EQ 8)) : $
                                    MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,k, $
                                                                       HAS_SC_POT=(SIZE(sc_pot,/TYPE) EQ 8))), $
                                   ;; ENERGY=en, $
                                   ENERGY=N_ELEMENTS(en) GT 0 ? en[*,k] : !NULL , $
                                   ERANGE=er, $
                                   EBINS=ebins,$
                                   ANGLE=N_ELEMENTS(an) GT 0 ? (N_ELEMENTS(an) GT 2 ? an[*,k] : an) : !NULL, $
                                   ARANGE=ar, $
                                   BINS=bins, $
                                   PRESSURE_COVAR_CALC=pressure_covar_calc, $
                                   HEATFLUX_COVAR_CALC=heatFlux_covar_calc, $
                                   QUIET=quiet)

                                   ;; CONV_TO_CM=conv_to_cm
     
     ;; errThingList.Add,TEMPORARY(errThing)
	errThingArr.N[k]      = errThing.N   
        errThingArr.Ux[k]     = errThing.Ux  
	errThingArr.Uy[k]     = errThing.Uy  
	errThingArr.Uz[k]     = errThing.Uz  
	errThingArr.Pxx[k]    = errThing.Pxx 
	errThingArr.Pyy[k]    = errThing.Pyy 
	errThingArr.Pzz[k]    = errThing.Pzz 
	errThingArr.Pxy[k]    = errThing.Pxy 
	errThingArr.Pxz[k]    = errThing.Pxz 
	errThingArr.Pyz[k]    = errThing.Pyz 
	errThingArr.Hx[k]     = errThing.Hx  
	errThingArr.Hy[k]     = errThing.Hy  
	errThingArr.Hz[k]     = errThing.Hz  
	errThingArr.R[k,*,*]  = errThing.R   


  ENDFOR
  
  ex_time     = SYSTIME(1) - ex_start
  IF ~KEYWORD_SET(summary_quiet) THEN BEGIN
     MESSAGE,STRING(ex_time)+' seconds execution time.',/CONT,/INFO
     PRINT,'Number of data points = ',max
  ENDIF

  RETURN,errThingArr

END

