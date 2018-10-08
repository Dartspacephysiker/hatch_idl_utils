;2017/03/30
PRO ADD_SC_POT_TO_DIFF_EFLUX,diff_eFlux,sc_pot, $
                                ARRAY_OF_STRUCTS_INSTEAD=array_of_structs_instead
                             

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; CASE 1 OF
  ;;    KEYWORD_SET(array_of_structs_instead): BEGIN
  ;;       ;; diffMax      = MEDIAN(diff_eFlux[1:-1].time-diff_eFlux[0:-2].time)*2.
  ;;    END
  ;;    ELSE: BEGIN
  ;;       diffMax      = MEDIAN(diff_eFlux.time[1:-1]-diff_eFlux.time[0:-2])*2.
  ;;    END
  ;; ENDCASE
  tmpTime = diff_eFlux.time
  diffMax      = MEDIAN(tmpTime[1:-1]-tmpTime[0:-2])*2.

  sc_pot_TMP = sc_pot.y

  sc_pot_min_i = VALUE_CLOSEST2(sc_pot.x,tmpTime, $
                                EXTREME_II=extreme_ii, $
                                /CONSTRAINED)

  IF extreme_ii[0] NE -1 THEN BEGIN

     ;;Now handled by CONSTRAINED keyword above
     ;; IF (WHERE(sc_pot_min_i[extreme_ii] EQ -1))[0] NE -1 THEN BEGIN
     ;;    sc_pot_min_i[WHERE(sc_pot_min_i[extreme_ii] EQ -1)] = 0
     ;; ENDIF
     
     ;; diffMax = STRMATCH(STRLOWCASE(eeb_or_ees),'*eb') ? 8D-4 : 1D-3

     IF (WHERE( $
        ABS(sc_pot.x[sc_pot_min_i[extreme_ii]] - tmpTime[extreme_ii]) $
        GT diffMax))[0] NE -1 THEN BEGIN
        sc_pot_TMP[sc_pot_min_i[extreme_ii]] = !VALUES.F_NaN
     ENDIF

  ENDIF

  bungaLunga = WHERE(ABS(sc_pot.x[sc_pot_min_i] - tmpTime) GT diffMax)
  IF bungaLunga[0] NE -1 THEN BEGIN
     sc_pot_TMP[sc_pot_min_i[bungaLunga]] = !VALUES.F_NaN
  ENDIF


  CASE 1 OF
     KEYWORD_SET(array_of_structs_instead): BEGIN
        charge = 1.             ; charge of species
        IF (diff_eFlux[0].mass LT 0.00010438871) then charge = -1. ; this line works for Wind which does not have dat.charge
        FOR kk=0,N_ELEMENTS(sc_pot_min_i)-1 DO BEGIN
           diff_eFlux[kk].sc_pot = sc_pot_TMP[sc_pot_min_i[kk]]
           diff_eFlux[kk].charge = charge
        ENDFOR
     END
     ELSE: BEGIN
        STR_ELEMENT,diff_eFlux,'sc_pot',sc_pot_TMP[sc_pot_min_i],/ADD_REPLACE

        ;;And charge
        charge = 1.             ; charge of species
        value  = 0
        STR_ELEMENT,diff_eFlux,'charge',value
        IF value LE 0 OR value GE 0 THEN value = value ELSE value = 0
        IF value NE 0 THEN charge = diff_eFlux.charge		
        IF ((value EQ 0) AND (diff_eFlux.mass LT 0.00010438871)) then charge = -1. ; this line works for Wind which does not have dat.charge
        STR_ELEMENT,diff_eFlux,'charge',charge,/ADD_REPLACE
     END
  ENDCASE

END
