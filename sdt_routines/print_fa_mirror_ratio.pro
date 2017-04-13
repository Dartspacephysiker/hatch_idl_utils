;2017/04/13
PRO PRINT_FA_MIRROR_RATIO,mRStruc

  COMPILE_OPT IDL2,STRICTARRSUBS

  nHere = N_ELEMENTS(mRStruc.time)

  CASE nHere OF
     1: BEGIN

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
     ELSE: BEGIN

        fmtString = '(A0,T25,A0,T33,A0,T41,' + $
                    'A0,T60,A0,T83,A0,T96,A0,T112,A0,T135,A0)'

        PRINT,''
        PRINT,'Triplets are "ionos,FAST,downTail"'

        PRINT,FORMAT=fmtString, $
              "TIME","LAT","MLT", $
              "R_E","km","B-TS04","R_B-TS04","B-IGRF","R_B-IGRF"

        FOR k=0,nHere-1 DO BEGIN

           stringR_E      = STRING(FORMAT='(F-5.2,",",F-5.2,",",F-5.2)',mRStruc.R_E.ionos[k],mRStruc.R_E.FAST[k],mRStruc.R_E.downTail[k])
           stringkm       = STRING(FORMAT='(I-3,",",G-8.2,",",G-8.2)',mRStruc.km.ionos[k],mRStruc.km.FAST[k],mRStruc.km.downTail[k])
           ;; stringBMag     = STRING(FORMAT='(G-8.2,",",G-8.2,",",I-4)',mRStruc.BMag.ionos[k],mRStruc.BMag.FAST[k],mRStruc.BMag.downTail[k])
           stringBMag     = STRING(FORMAT='(I-3,",",I-3,",",I-4)',mRStruc.BMag.ionos[k],mRStruc.BMag.FAST[k],mRStruc.BMag.downTail[k])
           stringB_IGRFMag     = STRING(FORMAT='(G-8.2,",",G-8.2,",",F-4.1)',mRStruc.B_IGRFMag.ionos[k],mRStruc.B_IGRFMag.FAST[k],mRStruc.B_IGRFMag.downTail[k])
           stringR_B      = STRING(FORMAT='(F-5.1,",",F-5.1,",",I1)',mRStruc.R_B.ionos[k],mRStruc.R_B.FAST[k],1)
           stringR_B_IGRF = STRING(FORMAT='(F-7.1,",",F-6.1,",",I1)',mRStruc.R_B_IGRF.ionos[k],mRStruc.R_B_IGRF.FAST[k],1)

           PRINT,FORMAT=fmtString, $
                 TIME_TO_STR(mRStruc.time[k],/MS),STRING(FORMAT='(F0.2)',mRStruc.ilat[k]),STRING(FORMAT='(F0.2)',mRStruc.mlt[k]), $
                 stringR_E,stringkm,stringBMag,stringR_B,stringB_IGRFMag,stringR_B_IGRF

        ENDFOR
        
     END
  ENDCASE


END
