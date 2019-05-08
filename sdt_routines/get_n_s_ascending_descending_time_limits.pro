;2019/05/08
;; USED BY examine_ion_conic_vs_all_flux_ratios.pro, get_fa_iesa_ion_beams.pro
PRO GET_N_S_ASCENDING_DESCENDING_TIME_LIMITS, $
     ilat, $
     TLIMN=tLimN, $
     TLIMASCENDN=tLimNAscend, $
     TLIMDESCENDN=tLimNDescend, $
     TLIMS=tLimS, $
     TLIMASCENDS=tLimSAscend, $
     TLIMDESCENDS=tLimSDescend, $
     NN=nN, $
     NASCENDN=nNAscend, $
     NDESCENDN=nNDescend, $
     NS=nS, $
     NASCENDS=nSAscend, $
     NDESCENDS=nSDescend, $
     NORTHI=northI, $
     SOUTHI=southI, $
     SAVETSTRN=saveTStrN, $
     SAVETSTRS=saveTStrS, $
     SAVETSTRASCENDN=saveTStrNAscend, $
     SAVETSTRDESCENDN=saveTStrNDescend, $
     SAVETSTRASCENDS=saveTStrSAscend, $
     SAVETSTRDESCENDS=saveTStrSDescend

  COMPILE_OPT IDL2,STRICTARRSUBS

  saveTStrN        = !NULL
  saveTStrS        = !NULL
  saveTStrNAscend  = !NULL
  saveTStrNDescend = !NULL
  saveTStrSAscend  = !NULL
  saveTStrSDescend = !NULL

  diffILAT = ilat.y[1:-1]-ilat.y[0:-2]
  diffILAT = [diffILAT[0],diffILAT]

  ;; Make sure monotonic
  CHECK_SORTED,ilat.x,is_sorted
  IF ~is_sorted THEN STOP

  northI = where(ilat.y GT 0,nN)
  ;; northI = where(ilat.y GT 10 AND (mlt.y GE 9 AND MLT.y LE 15),nn)
  IF (nN GT 0) then BEGIN
     tLimN=[ilat.x[northI[0]],ilat.x[northI[-1]]]
     tLimN=[DOUBLE(FLOOR(tLimN[0])),DOUBLE(CEIL(tLimN[1]))]
     tLimNStr=T2S(tLimN)
     saveTStrN = STRMID(tLimNStr[0],0,10)                      + "__" $
                 + (STRMID(tLimNStr[0],11,8)).Replace(":","_") + "-"  $
                 + (STRMID(tLimNStr[1],11,8)).Replace(":","_")

     nAscendII = WHERE(diffILAT[northI] GE 0,nNAscend, $
                       COMPLEMENT=nDescendII, $
                       NCOMPLEMENT=nNDescend)
     IF (nNAscend GT 0) THEN BEGIN
        nAscendI = northI[nAscendII]
        tLimNAscend=[ilat.x[nAscendI[0]],ilat.x[nAscendI[-1]]]
        tLimNAscendStr=T2S(tLimNAscend)
        saveTStrNAscend = STRMID(tLimNAscendStr[0],0,10)                + "__" $
                          + (STRMID(tLimNAscendStr[0],11,8)).Replace(":","_") + "-"  $
                          + (STRMID(tLimNAscendStr[1],11,8)).Replace(":","_")
     ENDIF
     IF (nNDescend GT 0) THEN BEGIN
        nDescendI = northI[nDescendII]
        tLimNDescend=[ilat.x[nDescendI[0]],ilat.x[nDescendI[-1]]]
        tLimNDescendStr=T2S(tLimNDescend)
        saveTStrNDescend = STRMID(tLimNDescendStr[0],0,10)                + "__" $
                           + (STRMID(tLimNDescendStr[0],11,8)).Replace(":","_") + "-"  $
                           + (STRMID(tLimNDescendStr[1],11,8)).Replace(":","_")
     ENDIF

  ENDIF

  southI = where(ilat.y LT 0,nS)
  if (nS GT 0) THEN BEGIN
     tLimS=[ilat.x[southI[0]],ilat.x[southI[-1]]]
     tLimS=[DOUBLE(FLOOR(tLimS[0])),DOUBLE(CEIL(tLimS[1]))]
     tLimSStr=T2S(tLimS)
     saveTStrS = STRMID(tLimSStr[0],0,10)                      + "__" $
                 + (STRMID(tLimSStr[0],11,8)).Replace(":","_") + "-"  $
                 + (STRMID(tLimSStr[1],11,8)).Replace(":","_")

     sAscendII = WHERE(diffILAT[southI] GE 0,nSAscend, $
                       COMPLEMENT=sDescendII, $
                       NCOMPLEMENT=nSDescend)
     IF (nSAscend GT 0) THEN BEGIN
        sAscendI = southI[sAscendII]
        tLimSAscend=[ilat.x[sAscendI[0]],ilat.x[sAscendI[-1]]]
        tLimSAscendStr=T2S(tLimSAscend)
        saveTStrSAscend = STRMID(tLimSAscendStr[0],0,10)                + "__" $
                          + (STRMID(tLimSAscendStr[0],11,8)).Replace(":","_") + "-"  $
                          + (STRMID(tLimSAscendStr[1],11,8)).Replace(":","_")
     ENDIF

     IF (nSDescend GT 0) THEN BEGIN
        sDescendI = southI[sDescendII]
        tLimSDescend=[ilat.x[sDescendI[0]],ilat.x[sDescendI[-1]]]
        tLimSDescendStr=T2S(tLimSDescend)
        saveTStrSDescend = STRMID(tLimSDescendStr[0],0,10)                + "__" $
                           + (STRMID(tLimSDescendStr[0],11,8)).Replace(":","_") + "-"  $
                           + (STRMID(tLimSDescendStr[1],11,8)).Replace(":","_")
     ENDIF
  ENDIF

END
