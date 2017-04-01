;;2017/03/03
FUNCTION MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX,diff_eFlux, $
   ENERGY=energy, $
   SC_POT=sc_pot, $
   EEB_OR_EES=eeb_or_ees

  COMPILE_OPT IDL2,STRICTARRSUBS

  CASE N_ELEMENTS(energy) OF
     N_ELEMENTS(diff_eFlux.time)*2: BEGIN
        out_en_arr = energy
     END
     1: BEGIN
        STOP
     END
     ELSE: BEGIN
        out_en_arr = [MIN(energy),MAX(energy)] # MAKE_ARRAY(N_ELEMENTS(diff_eFlux.time),VALUE=1.)
     END
  ENDCASE

  ;;Do we have potential?

  dEflux_has_sc_pot = (WHERE(STRUPCASE(TAG_NAMES(diff_eFlux)) EQ 'SC_POT'))[0] NE -1
  have_sc_pot = dEflux_has_sc_pot
  IF have_sc_pot THEN BEGIN
     IF N_ELEMENTS(diff_eFlux.sc_pot) NE N_ELEMENTS(diff_eFlux.time) THEN STOP

     sc_potTmp = diff_eFlux.sc_pot

  ENDIF ELSE BEGIN

     IF KEYWORD_SET(sc_pot) THEN BEGIN

        sc_pot_min_i = VALUE_CLOSEST2(sc_pot.x,diff_eFlux.time, $
                                      EXTREME_II=extreme_ii, $
                                      /CONSTRAINED)

        IF extreme_ii[0] NE -1 THEN BEGIN

           ;;Now handled by CONSTRAINED keyword above
           ;; IF (WHERE(sc_pot_min_i[extreme_ii] EQ -1))[0] NE -1 THEN BEGIN
           ;;    sc_pot_min_i[WHERE(sc_pot_min_i[extreme_ii] EQ -1)] = 0
           ;; ENDIF
           
           diffMax = STRMATCH(STRLOWCASE(eeb_or_ees),'*eb') ? 8D-4 : 1D-3

           IF (WHERE( $
              ABS(sc_pot.x[sc_pot_min_i[extreme_ii]] - diff_eFlux.time[extreme_ii]) $
              GT diffMax))[0] NE -1 THEN BEGIN
              STOP
           ENDIF
        ENDIF

        sc_potTmp = sc_pot.y[sc_pot_min_i]

        have_sc_pot = 1

     ENDIF

  ENDELSE

  ;;Now see what we can do
  IF have_sc_pot THEN BEGIN
     multFac = STRMATCH(eeb_or_ees,'ee*',/FOLD_CASE) ? -1. : 1.
     checkIt = WHERE((multFac*sc_potTmp - out_en_arr[0,*]) GT 0.,nCheckIt)

     IF (nCheckIt NE 0) THEN BEGIN
        
        out_en_arr[0,checkIt] = multFac*sc_potTmp[checkIt]

     ENDIF

     ;;Don't use multFac! Keep it raw for MOMENTS_2D_NEW
     ;; STR_ELEMENT,diff_eFlux,'sc_pot',multFac*sc_pot.y[sc_pot_min_i[checkIt]],/ADD_REPLACE
     IF ~dEflux_has_sc_pot THEN BEGIN
        ;; STOP
        ADD_SC_POT_TO_DIFF_EFLUX,diff_eFlux,sc_pot
        ;; STR_ELEMENT,diff_eFlux,'sc_pot',sc_potTmp,/ADD_REPLACE
     ENDIF

  ENDIF
     
  RETURN,out_en_arr

END
