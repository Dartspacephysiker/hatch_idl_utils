;2017/03/30
PRO ADD_SC_POT_TO_DIFF_EFLUX,diff_eFlux,sc_pot

  COMPILE_OPT IDL2,STRICTARRSUBS

  diffMax      = MEDIAN(diff_eFlux.time[1:-1]-diff_eFlux.time[0:-2])*2.

  sc_pot_min_i = VALUE_CLOSEST2(sc_pot.x,diff_eFlux.time, $
                                EXTREME_II=extreme_ii, $
                                /CONSTRAINED)

  IF extreme_ii[0] NE -1 THEN BEGIN

     ;;Now handled by CONSTRAINED keyword above
     ;; IF (WHERE(sc_pot_min_i[extreme_ii] EQ -1))[0] NE -1 THEN BEGIN
     ;;    sc_pot_min_i[WHERE(sc_pot_min_i[extreme_ii] EQ -1)] = 0
     ;; ENDIF
     
     ;; diffMax = STRMATCH(STRLOWCASE(eeb_or_ees),'*eb') ? 8D-4 : 1D-3

     IF (WHERE( $
        ABS(sc_pot.x[sc_pot_min_i[extreme_ii]] - diff_eFlux.time[extreme_ii]) $
        GT diffMax))[0] NE -1 THEN BEGIN
        STOP
     ENDIF

  ENDIF

  IF (WHERE( $
     ABS(sc_pot.x[sc_pot_min_i] - diff_eFlux.time) $
     GT diffMax))[0] NE -1 THEN BEGIN
     STOP
  ENDIF


  STR_ELEMENT,diff_eFlux,'sc_pot',sc_pot.y[sc_pot_min_i],/ADD_REPLACE

  ;;And charge
  charge = 1.                     ; charge of species
  value  = 0
  STR_ELEMENT,diff_eFlux,'charge',value
  IF value LE 0 OR value GE 0 THEN value = value ELSE value = 0
  IF value NE 0 THEN charge = diff_eFlux.charge		
  IF ((value EQ 0) AND (diff_eFlux.mass LT 0.00010438871)) then charge = -1. ; this line works for Wind which does not have dat.charge
  STR_ELEMENT,diff_eFlux,'charge',charge,/ADD_REPLACE

END
