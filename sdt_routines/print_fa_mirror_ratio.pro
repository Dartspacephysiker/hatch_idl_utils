;2017/04/13
PRO PRINT_FA_MIRROR_RATIO,mRStruc

  COMPILE_OPT IDL2,STRICTARRSUBS

  nHere = N_ELEMENTS(mRStruc.time)

  ;; PRINT,FORMAT='(A0,T20,": ",A0,A0)',"TIME",t1Str,KEYWORD_SET(t2Str) ? '–' + t2Str : ''
  PRINT,FORMAT='(A0,T20,": ",A0,A0)',"TIME",TIME_TO_STR(mRStruc.time[0],/MS),nHere GT 1 ? '–' + TIME_TO_STR(mRStruc.time[-1],/MS) : ''
  PRINT,FORMAT='(A0,T20,": ",F0.2)',"LAT",mRStruc.ilat
  PRINT,FORMAT='(A0,T20,": ",F0.2)',"MLT",mRStruc.mlt
  PRINT,''
  PRINT,FORMAT='(A0,T20,": ",A0,T45,A0,T60,A0)',"Loc","Ionosphere",'FAST','downTail'
  PRINT,FORMAT='(A0,T20,": ",F0.2,T45,F0.2,T60,F0.2)',"R_E",mRStruc.R_E.ionos,mRStruc.R_E.FAST,mRStruc.R_E.downTail
  PRINT,FORMAT='(A0,T20,": ",G0.2,T45,G0.2,T60,G0.2)',"km",mRStruc.km.ionos,mRStruc.km.FAST,mRStruc.km.downTail
  ;; PRINT,''
  ;; PRINT,FORMAT='(A0,T20,": ",A0,T45,A0,T60,A0)',"Bfield-TS04 (nT)",mRStruc.BMag.ionos,mRStruc.BMag.FAST,mRStruc.BMag.downTail
  ;; PRINT,FORMAT='(A0,T20,": ",A0,T45,A0,T60,A0)',"Bfield-IGRF (nT)",mRStruc.IGRFMag.ionos,mRStruc.IGRFMag.FAST,mRStruc.IGRFMag.downTail
  PRINT,''
  PRINT,'TS04'
  PRINT,FORMAT='(A0,T20,": ",G0.2,T45,G0.2,T60,G0.2)',"Bfield (nT)",mRStruc.BMag.ionos,mRStruc.BMag.FAST,mRStruc.BMag.downTail
  PRINT,FORMAT='(A0,T20,": ",G0.2,T45,G0.2,T60,G0.2)',"R_B",mRStruc.R_B.ionos,mRStruc.R_B.FAST,1
  PRINT,''
  PRINT,'IGRF'
  PRINT,FORMAT='(A0,T20,": ",G0.2,T45,G0.2,T60,G0.2)',"Bfield (nT)",mRStruc.B_IGRFMag.ionos,mRStruc.B_IGRFMag.FAST,mRStruc.B_IGRFMag.downTail
  PRINT,FORMAT='(A0,T20,": ",G0.2,T45,G0.2,T60,G0.2)',"R_B",mRStruc.R_B_IGRF.ionos,mRStruc.R_B_IGRF.FAST,1


END
