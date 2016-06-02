PRO CHECK_DIFF_EFLUX_INPUTS_BEFORE_BEGINNING,eSpecs,jee_out,je_out,alf_mlt,alf_ilat

  PRINT,FORMAT='("N ",A0,T20,": ",I0)',"eSpecs.x[*,0]",N_ELEMENTS(eSpecs.x[*,0])
  PRINT,FORMAT='("N ",A0,T20,": ",I0)',"jee_out",N_ELEMENTS(jee_out)      
  PRINT,FORMAT='("N ",A0,T20,": ",I0)',"je_out",N_ELEMENTS(je_out)       
  PRINT,FORMAT='("N ",A0,T20,": ",I0)',"alf_mlt",N_ELEMENTS(alf_mlt)      
  PRINT,FORMAT='("N ",A0,T20,": ",I0)',"alf_ilat",N_ELEMENTS(alf_ilat)

  proceed = N_ELEMENTS(eSpecs.x[*,0]) EQ N_ELEMENTS(jee_out)   AND $
            N_ELEMENTS(eSpecs.x[*,0]) EQ N_ELEMENTS(je_out)    AND $
            N_ELEMENTS(eSpecs.x[*,0]) EQ N_ELEMENTS(alf_mlt)   AND $
            N_ELEMENTS(eSpecs.x[*,0]) EQ N_ELEMENTS(alf_ilat)  AND $
            N_ELEMENTS(jee_out)       EQ N_ELEMENTS(je_out)    AND $
            N_ELEMENTS(jee_out)       EQ N_ELEMENTS(alf_mlt)   AND $
            N_ELEMENTS(jee_out)       EQ N_ELEMENTS(alf_ilat)  AND $
            N_ELEMENTS(je_out)        EQ N_ELEMENTS(alf_mlt)   AND $
            N_ELEMENTS(je_out)        EQ N_ELEMENTS(alf_ilat)  AND $
            N_ELEMENTS(alf_mlt)       EQ N_ELEMENTS(alf_ilat)

  IF ~proceed THEN BEGIN
     PRINT,"There are issues with the provided data! The number of elements for each are unequal"
     STOP
  ENDIF

     




END