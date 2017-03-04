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
                                          QUIET=quiet

  COMPILE_OPT IDL2,STRICTARRSUBS

  ex_start    = SYSTIME(1)

  max         = N_ELEMENTS(diff_eFlux.data_name)
  ;; time        = (diff_eFlux.time+diff_eFlux.end_time)/2.
  ;; j           = {x:TEMPORARY(time),y:MAKE_ARRAY(max,/FLOAT)}

  ;; species     = diff_eFlux.mass GT 6e-6 ;0=electron, 1=ion; electron mass is 5.68566e-06 eV/c^2 (c in km/s)
  ;; energy      = diff_eFlux.energy[*,0,0]
  ;; theta       = REFORM(diff_eFlux.theta[0,*,0])

  ;; errThingList = LIST()
  N_R         = 4 + (KEYWORD_SET(pressure_covar_calc) ? 6 : 0) + (KEYWORD_SET(heatFlux_covar_calc) ? 3 : 0)
  errThingArr = {N   : MAKE_ARRAY(max,/DOUBLE), $
                 Ux  : MAKE_ARRAY(max,/DOUBLE), $
                 Uy  : MAKE_ARRAY(max,/DOUBLE), $
                 Uz  : MAKE_ARRAY(max,/DOUBLE), $
                 Pxx : MAKE_ARRAY(max,/DOUBLE), $
                 Pyy : MAKE_ARRAY(max,/DOUBLE), $
                 Pzz : MAKE_ARRAY(max,/DOUBLE), $
                 Pxy : MAKE_ARRAY(max,/DOUBLE), $
                 Pxz : MAKE_ARRAY(max,/DOUBLE), $
                 Pyz : MAKE_ARRAY(max,/DOUBLE), $
                 Hx  : MAKE_ARRAY(max,/DOUBLE), $
                 Hy  : MAKE_ARRAY(max,/DOUBLE), $
                 Hz  : MAKE_ARRAY(max,/DOUBLE), $
                 R   : MAKE_ARRAY(max,N_R,N_R,/DOUBLE)}

  IF N_ELEMENTS(en) GT 1 AND NDIMEN(en) LT 2 THEN BEGIN

     en_arr   = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX(diff_eFlux, $
                                               ENERGY=en, $
                                               SC_POT=sc_pot, $
                                               EEB_OR_EES=eeb_or_ees)
     
     en       = TEMPORARY(en_arr)

  ENDIF

  FOR k=0,max-1 DO BEGIN

     errThing    = MOMENTERRORS_2D(MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,k), $
                                   ;; ENERGY=en, $
                                   ENERGY=N_ELEMENTS(en) GT 0 ? en[*,k] : !NULL , $
                                   ERANGE=er, $
                                   EBINS=ebins,$
                                   ANGLE=an, $
                                   ARANGE=ar, $
                                   BINS=bins, $
                                   PRESSURE_COVAR_CALC=pressure_covar_calc, $
                                   HEATFLUX_COVAR_CALC=heatFlux_covar_calc)

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
  IF ~KEYWORD_SET(quiet) THEN BEGIN
     MESSAGE,STRING(ex_time)+' seconds execution time.',/CONT,/INFO
     PRINT,'Number of data points = ',max
  ENDIF

  RETURN,errThingArr

END

