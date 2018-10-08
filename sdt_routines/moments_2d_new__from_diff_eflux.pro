;2017/03/29
FUNCTION MOMENTS_2D_NEW__FROM_DIFF_EFLUX,diff_eFlux, $
                                         ENERGY=en, $
                                         ERANGE=er, $
                                         EBINS=ebins, $
                                         ANGLE=an, $
                                         ARANGE=ar, $
                                         BINS=bins, $
                                         SC_POT=sc_pot, $
                                         EEB_OR_EES=eeb_or_ees, $
                                         MCFADDEN_STYLE_DIFF_EFLUX=McFadden_style_diff_eFlux, $
                                         QUIET=quiet, $
                                         SUMMARY_QUIET=summary_quiet

  COMPILE_OPT IDL2,STRICTARRSUBS

  ex_start    = SYSTIME(1)

  max         = N_ELEMENTS(diff_eFlux.data_name)
  ;; time        = (diff_eFlux.time+diff_eFlux.end_time)/2.
  time        = diff_eFlux.time
  templar     = {n   : 0.0D, $
                 j   : 0.0D, $
                 je  : 0.0D, $
                 jje : 0.0D, $
                 p   : REPLICATE(0.0D,6), $
                 T   : REPLICATE(0.0D,4), $
                 v   : 0.D, $
                 charE : 0.D, $
                 perp  : {j     : 0.D, $
                          je    : 0.D, $
                          jje   : 0.D, $
                          v     : 0.D, $
                          charE : 0.D}, $
                 all   : {j     : 0.D, $
                           je   : 0.D, $
                          speed : 0.D, $
                          charE : 0.D}}

  moments           = {x:TEMPORARY(time),y:REPLICATE(templar,max)}

  IF N_ELEMENTS(en) GT 1 AND NDIMEN(en) LT 2 THEN BEGIN

     en_arr   = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX(diff_eFlux, $
                                                   ENERGY=en, $
                                                   SC_POT=sc_pot, $
                                                   EEB_OR_EES=eeb_or_ees)
     
     en       = TEMPORARY(en_arr)

  ENDIF

  FOR k=0,max-1 DO BEGIN

     tmpStruct      = MOMENTS_2D_NEW((KEYWORD_SET(McFadden_style_diff_eFlux) ? $
                                      MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX__MCFADDEN_STYLE(diff_eFlux,k, $
                                                                                         HAS_SC_POT=(SIZE(sc_pot,/TYPE) EQ 8)) : $
                                      MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,k, $
                                                                         HAS_SC_POT=(SIZE(sc_pot,/TYPE) EQ 8))), $
                                     ENERGY=N_ELEMENTS(en) GT 0 ? en[*,k] : !NULL , $
                                     ERANGE=er, $
                                     EBINS=ebins, $
                                     ANGLE=N_ELEMENTS(an) GT 0 ? (N_ELEMENTS(an) GT 2 ? an[*,k] : an) : !NULL, $
                                     ARANGE=ar, $
                                     BINS=bins, $
                                     QUIET=quiet)

     moments.y[k].n           = tmpStruct.n
     moments.y[k].j           = tmpStruct.j
     moments.y[k].je          = tmpStruct.je
     moments.y[k].jje         = tmpStruct.jje
     moments.y[k].p           = tmpStruct.p
     moments.y[k].T           = tmpStruct.T
     moments.y[k].v           = tmpStruct.v
     moments.y[k].charE       = tmpStruct.charE
     moments.y[k].perp.j      = tmpStruct.perp.j
     moments.y[k].perp.je     = tmpStruct.perp.je
     moments.y[k].perp.jje    = tmpStruct.perp.jje
     moments.y[k].perp.v      = tmpStruct.perp.v
     moments.y[k].perp.charE  = tmpStruct.perp.charE
     moments.y[k].all.j       = tmpStruct.all.j
     moments.y[k].all.je      = tmpStruct.all.je
     ;; moments.y[k].perp.jje    = tmpStruct.perp.jje
     moments.y[k].all.speed   = tmpStruct.all.speed
     moments.y[k].all.charE   = tmpStruct.all.charE
     

  ENDFOR
  
  ex_time     = SYSTIME(1) - ex_start
  IF ~KEYWORD_SET(summary_quiet) THEN BEGIN
     MESSAGE,STRING(ex_time)+' seconds execution time.',/CONT,/INFO
     PRINT,'Number of data points = ',max
  ENDIF

  RETURN,moments
  
END
