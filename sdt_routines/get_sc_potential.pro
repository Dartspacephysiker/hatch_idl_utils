PRO GET_SC_POTENTIAL,T1=t1,T2=t2,DATA=data

     spacecraft_potential = GET_FA_FIELDS('V8_S',t1,t2)

     ;;get the spacecraft potential per spin
     spin_period=4.946          ; seconds

     ;;get_sample_rate
     v8                   = {x:spacecraft_potential.time,y:spacecraft_potential.comp1}
     
     v8_dt                = ABS(v8.x-SHIFT(v8.x,-1))
     v8_dt[0]             = v8_dt[1]
     v8_dt[N_ELEMENTS(v8.x)-1] = v8_dt[N_ELEMENTS(v8.x)-2]

     ;;get maxima within a 1 spin window
     j_range = WHERE(v8.x LT v8.x[N_ELEMENTS(v8.x)-1]-spin_period)
     index_max = MAX(j_range)
     PRINT,index_max
     pot = MAKE_ARRAY(N_ELEMENTS(v8.x),/DOUBLE)
     FOR j=0L,index_max DO BEGIN
        spin_range = j+FINDGEN(CEIL(spin_period/V8_dt[j]))
        pot[j]     = MAX(ABS(v8.y[spin_range]),ind)
        sign       = v8.y[spin_range[ind]]/ABS(v8.y[spin_range[ind]])
        pot[j]     = sign*pot[j]

     endfor
     pot[index_max+1:N_ELEMENTS(v8.x)-1] = pot[j_range[index_max]]
     sc_pot        = {x:v8.x,y:pot}
     STORE_DATA,'S_Pot',DATA=sc_pot ;note this is actually the negative of the s/c potential

     PRINT,'Spacecraft potential stored as ''S_Pot'''
     IF ARG_PRESENT(data) THEN data = sc_pot

END