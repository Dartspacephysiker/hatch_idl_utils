PRO GET_SC_POTENTIAL,T1=t1,T2=t2,DATA=data, $
                     FROM_FILE=from_file, $
                     FROM_FA_POTENTIAL=from_fa_potential, $
                     CHASTON_STYLE=Chaston_style, $
                     REPAIR=repair, $
                     ORBIT=orbit

  IF KEYWORD_SET(from_file) THEN BEGIN
     ;;Lifted from DOWNGOING_IONS__V1
     out_sc_pot_dir           = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/just_potential/'
     newellStuff_pref_sc_pot  = 'Newell_et_al_identification_of_electron_spectra--just_sc_pot--Orbit_'

     jjj    = 0
     done   = 0B
     t1Good = ~KEYWORD_SET(t1)  ;don't worry about checking if t1 and t2 are good if we already have them
     t2Good = ~KEYWORD_SET(t2)
     
     WHILE ~done DO BEGIN

        out_newell_file_sc_pot  = newellStuff_pref_sc_pot + STRCOMPRESS(orbit,/REMOVE_ALL) + $
                                  '_' + STRCOMPRESS(jjj,/REMOVE_ALL) + '.sav'

        IF FILE_TEST(out_sc_pot_dir+out_newell_file_sc_pot) THEN BEGIN

           PRINT,"Restoring S/C pot file: " + out_newell_file_sc_pot
           RESTORE,out_sc_pot_dir+out_newell_file_sc_pot

           IF N_ELEMENTS(sc_pot) EQ 0 THEN BEGIN

              PRINT,"This file has no sc_pot!!"
              STOP
              
              get_potential = 1
              done          = 1
              BREAK

           ENDIF ELSE BEGIN

              ;;Check--are we within time boundaries?
              IF KEYWORD_SET(t1) AND ~t1Good THEN BEGIN
                 IF sc_pot.x[0] LE t1 THEN BEGIN
                    t1Good = 1B
                 ENDIF
              ENDIF ELSE BEGIN
                 t1Good = 1B
              ENDELSE

              IF KEYWORD_SET(t2) AND ~t2Good THEN BEGIN
                 IF sc_pot.x[-1] GE t2 THEN BEGIN
                    t2Good = 1B
                 ENDIF
              ENDIF ELSE BEGIN
                 t2Good = 1B
              ENDELSE

              get_potential = ~(t1Good AND t2Good)

           ENDELSE

           ;;Merge with sc_pots from previous files
           IF get_potential THEN BEGIN

              CASE N_ELEMENTS(tmp_sc_pot) OF
                 1: BEGIN
                    tmp_sc_pot = {x:[tmp_sc_pot.x,sc_pot.x], $
                                  y:[tmp_sc_pot.y,sc_pot.y]}
                    sc_pot     = !NULL
                 END
                 ELSE: BEGIN
                    tmp_sc_pot = TEMPORARY(sc_pot)
                 END
              ENDCASE

              jjj++

           ENDIF ELSE BEGIN

              CASE N_ELEMENTS(tmp_sc_pot) OF
                 1: BEGIN

                    ;;Pick up old tmp_sc_pot
                    sc_pot = {x:[tmp_sc_pot.x,sc_pot.x], $
                              y:[tmp_sc_pot.y,sc_pot.y]}

                 END
                 ELSE: BEGIN
                    ;;No need to pick up old tmp_sc_pot
                 END
              ENDCASE

              done = 1
              
           ENDELSE

        ENDIF ELSE BEGIN

           get_potential = 1
           done          = 1

        ENDELSE
        

     ENDWHILE

     IF ~get_potential THEN BEGIN
        data = TEMPORARY(sc_pot)
        RETURN
     ENDIF

  ENDIF

  CASE 1 OF
     KEYWORD_SET(from_fa_potential): BEGIN

        spacecraft_potential = GET_FA_POTENTIAL(t1,t2,REPAIR=repair,STORE=~ARG_PRESENT(data))

        IF ~spacecraft_potential.valid THEN BEGIN
           data = -1
           RETURN
        ENDIF

        sc_pot = {x:spacecraft_potential.time, $
                  y:spacecraft_potential.comp1, $
                  notch:spacecraft_potential.notch}
        
        IF ARG_PRESENT(data) THEN data = sc_pot

        RETURN

     END
     KEYWORD_SET(Chaston_style): BEGIN

        spacecraft_potential = GET_FA_FIELDS('V8_S',t1,t2)
        ;;get the spacecraft potential per spin
        spin_period   = 4.946   ; seconds

        ;;get_sample_rate
        v8            = {x:spacecraft_potential.time,y:spacecraft_potential.comp1}
        
        v8_dt         = ABS(v8.x-SHIFT(v8.x,-1))
        v8_dt[0]      = v8_dt[1]
        nV8           = N_ELEMENTS(v8.x)
        v8_dt[nV8-1]  = v8_dt[nV8-2]

        ;;get maxima within a 1 spin window
        j_range       = WHERE(v8.x LT v8.x[N_ELEMENTS(v8.x)-1]-spin_period)
        index_max     = MAX(j_range)
        PRINT,index_max
        pot = MAKE_ARRAY(N_ELEMENTS(v8.x),/DOUBLE)
        FOR j=0L,index_max DO BEGIN
           spin_range = j+FINDGEN(CEIL(spin_period/V8_dt[j]))
           pot[j]     = MAX(ABS(v8.y[spin_range]),ind)
           sign       = v8.y[spin_range[ind]]/ABS(v8.y[spin_range[ind]])
           pot[j]     = sign*pot[j]
        ENDFOR

        pot[index_max+1:nV8-1] = pot[j_range[index_max]]
        sc_pot        = {x:v8.x,y:pot}
        STORE_DATA,'S_Pot',DATA=sc_pot ;note this is actually the negative of the s/c potential

        PRINT,'Spacecraft potential stored as ''S_Pot'''
        IF ARG_PRESENT(data) THEN data = sc_pot

        RETURN

     END
     ;; ELSE: BEGIN
     ;; END
  ENDCASE


END