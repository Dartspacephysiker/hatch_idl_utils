;;2017/03/03
FUNCTION MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX,diff_eFlux, $
   ENERGY=energy, $
   SC_POT=sc_pot, $
   EEB_OR_EES=eeb_or_ees

  COMPILE_OPT IDL2,STRICTARRSUBS

  out_en_arr = [MIN(energy),MAX(energy)] # MAKE_ARRAY(N_ELEMENTS(diff_eFlux.time),VALUE=1.)
  ;; out_en_arr = [[MAKE_ARRAY(N_ELEMENTS(diff_eFlux.time),VALUE=1.)],[MAKE_ARRAY(N_ELEMENTS(diff_eFlux.time),VALUE=1.)]] # $
  ;;              TRANSPOSE([MIN(energy),MAX(energy)])

  IF KEYWORD_SET(sc_pot) THEN BEGIN

     multFac    = STRMATCH(eeb_or_ees,'ee*',/FOLD_CASE) ? -1. : 1.

     sc_pot_min_i = VALUE_CLOSEST2(sc_pot.x,diff_eFlux.time,EXTREME_II=extreme_ii)

     IF extreme_ii[0] NE -1 THEN BEGIN

        IF (WHERE( $
           ABS(sc_pot.x[sc_pot_min_i[extreme_ii]] - diff_eFlux.time[extreme_ii]) $
           GT 3d-4))[0] NE -1 THEN BEGIN
           STOP
        ENDIF
     ENDIF

     checkIt = WHERE((multFac*sc_pot.y[sc_pot_min_i] - out_en_arr[0,*]) GT 0.,nCheckIt)

     IF (nCheckIt NE 0) THEN BEGIN
        
        out_en_arr[0,checkIt] = multFac*sc_pot.y[sc_pot_min_i[checkIt]]

     ENDIF
     
  ENDIF

  RETURN,out_en_arr

END
