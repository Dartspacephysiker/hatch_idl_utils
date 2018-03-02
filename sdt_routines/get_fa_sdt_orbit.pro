;2018/03/02
PRO GET_FA_SDT_ORBIT,orbit

  COMPILE_OPT IDL2,STRICTARRSUBS

  phase_data = GET_PHASE_FROM_ATTCTRL(DEBUG=0,/QUIET)
  nn = n_elements(phase_data.nadir_zero)-1L
  t1 = phase_data.nadir_zero[0]
  t2 = phase_data.nadir_zero[nn]
  ;; t2 = t1+10
  GET_FA_ORBIT,t1,t2,delta=60.,/definitive,/NO_STORE,STRUC=struc
  nHere = N_ELEMENTS(struc.orbit)
  orbit = (TEMPORARY(struc)).orbit[TEMPORARY(nHere)/2]

END
