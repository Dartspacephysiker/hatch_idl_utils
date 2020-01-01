;Opprinnelig fra 2018/07/20-dagbokfilen JOURNAL__20180720__LOOK_AT_CONIC_VS_ALL_FLUX_RATIOS
;; This one har nå en egen fil 
;; PRO GET_N_S_ASCENDING_DESCENDING_TIME_LIMITS, $
;;      ilat, $
;;      TLIMN=tLimN, $
;;      TLIMASCENDN=tLimNAscend, $
;;      TLIMDESCENDN=tLimNDescend, $
;;      TLIMS=tLimS, $
;;      TLIMASCENDS=tLimSAscend, $
;;      TLIMDESCENDS=tLimSDescend, $
;;      NN=nN, $
;;      NASCENDN=nNAscend, $
;;      NDESCENDN=nNDescend, $
;;      NS=nS, $
;;      NASCENDS=nSAscend, $
;;      NDESCENDS=nSDescend, $
;;      NORTHI=northI, $
;;      SOUTHI=southI, $
;;      SAVETSTRN=saveTStrN, $
;;      SAVETSTRS=saveTStrS, $
;;      SAVETSTRASCENDN=saveTStrNAscend, $
;;      SAVETSTRDESCENDN=saveTStrNDescend, $
;;      SAVETSTRASCENDS=saveTStrSAscend, $
;;      SAVETSTRDESCENDS=saveTStrSDescend

;;   saveTStrN        = !NULL
;;   saveTStrS        = !NULL
;;   saveTStrNAscend  = !NULL
;;   saveTStrNDescend = !NULL
;;   saveTStrSAscend  = !NULL
;;   saveTStrSDescend = !NULL

;;   diffILAT = ilat.y[1:-1]-ilat.y[0:-2]

;;   ;; Make sure monotonic
;;   CHECK_SORTED,ilat.x,is_sorted
;;   IF ~is_sorted THEN STOP

;;   northI = where(ilat.y GT 0,nN)
;;   ;; northI = where(ilat.y GT 10 AND (mlt.y GE 9 AND MLT.y LE 15),nn)
;;   IF (nN GT 0) then BEGIN
;;      tLimN=[ilat.x[northI[0]],ilat.x[northI[-1]]]
;;      tLimN=[DOUBLE(FLOOR(tLimN[0])),DOUBLE(CEIL(tLimN[1]))]
;;      tLimNStr=T2S(tLimN)
;;      saveTStrN = STRMID(tLimNStr[0],0,10)                      + "__" $
;;                  + (STRMID(tLimNStr[0],11,8)).Replace(":","_") + "-"  $
;;                  + (STRMID(tLimNStr[1],11,8)).Replace(":","_")

;;      nAscendII = WHERE(diffILAT[northI] GE 0,nNAscend, $
;;                        COMPLEMENT=nDescendII, $
;;                        NCOMPLEMENT=nNDescend)
;;      IF (nNAscend GT 0) THEN BEGIN
;;         nAscendI = northI[nAscendII]
;;         tLimNAscend=[ilat.x[nAscendI[0]],ilat.x[nAscendI[-1]]]
;;         tLimNAscendStr=T2S(tLimNAscend)
;;         saveTStrNAscend = STRMID(tLimNAscendStr[0],0,10)                + "__" $
;;                           + (STRMID(tLimNAscendStr[0],11,8)).Replace(":","_") + "-"  $
;;                           + (STRMID(tLimNAscendStr[1],11,8)).Replace(":","_")
;;      ENDIF
;;      IF (nNDescend GT 0) THEN BEGIN
;;         nDescendI = northI[nDescendII]
;;         tLimNDescend=[ilat.x[nDescendI[0]],ilat.x[nDescendI[-1]]]
;;         tLimNDescendStr=T2S(tLimNDescend)
;;         saveTStrNDescend = STRMID(tLimNDescendStr[0],0,10)                + "__" $
;;                            + (STRMID(tLimNDescendStr[0],11,8)).Replace(":","_") + "-"  $
;;                            + (STRMID(tLimNDescendStr[1],11,8)).Replace(":","_")
;;      ENDIF

;;   ENDIF

;;   southI = where(ilat.y LT 0,nS)
;;   if (nS GT 0) THEN BEGIN
;;      tLimS=[ilat.x[southI[0]],ilat.x[southI[-1]]]
;;      tLimS=[DOUBLE(FLOOR(tLimS[0])),DOUBLE(CEIL(tLimS[1]))]
;;      tLimSStr=T2S(tLimS)
;;      saveTStrS = STRMID(tLimSStr[0],0,10)                      + "__" $
;;                  + (STRMID(tLimSStr[0],11,8)).Replace(":","_") + "-"  $
;;                  + (STRMID(tLimSStr[1],11,8)).Replace(":","_")

;;      sAscendII = WHERE(diffILAT[southI] GE 0,nSAscend, $
;;                        COMPLEMENT=sDescendII, $
;;                        NCOMPLEMENT=nSDescend)
;;      IF (nSAscend GT 0) THEN BEGIN
;;         sAscendI = southI[sAscendII]
;;         tLimSAscend=[ilat.x[sAscendI[0]],ilat.x[sAscendI[-1]]]
;;         tLimSAscendStr=T2S(tLimSAscend)
;;         saveTStrSAscend = STRMID(tLimSAscendStr[0],0,10)                + "__" $
;;                           + (STRMID(tLimSAscendStr[0],11,8)).Replace(":","_") + "-"  $
;;                           + (STRMID(tLimSAscendStr[1],11,8)).Replace(":","_")
;;      ENDIF

;;      IF (nSDescend GT 0) THEN BEGIN
;;         sDescendI = southI[sDescendII]
;;         tLimSDescend=[ilat.x[sDescendI[0]],ilat.x[sDescendI[-1]]]
;;         tLimSDescendStr=T2S(tLimSDescend)
;;         saveTStrSDescend = STRMID(tLimSDescendStr[0],0,10)                + "__" $
;;                            + (STRMID(tLimSDescendStr[0],11,8)).Replace(":","_") + "-"  $
;;                            + (STRMID(tLimSDescendStr[1],11,8)).Replace(":","_")
;;      ENDIF
;;   ENDIF

;; END

PRO TPLOT_UP_VS_DOWN_ION_FLUXES, $
   tPlt_vars, $
   EBOUNDVARNAME=eBoundVarName, $
   EBOUNDLOWVARNAME=eBoundLowVarName, $
   UPDOWNRATIOSPECVARNAME=upDownRatioSpecVarName, $
   USE_LOSSCONE=use_losscone, $
   PLOT_ASCENDING_NORTH=plot_ascN, $
   PLOT_ASCENDING_SOUTH=plot_ascS, $
   PLOT_DESCENDING_NORTH=plot_descN, $
   PLOT_DESCENDING_SOUTH=plot_descS, $
   TLIMASCENDN=tLimNAscend, $
   TLIMDESCENDN=tLimNDescend, $
   TLIMS=tLimS, $
   TLIMASCENDS=tLimSAscend, $
   TLIMDESCENDS=tLimSDescend, $
   FNAMENASCEND=fNameAscendN, $
   FNAMENDESCEND=fNameDescendN, $
   FNAMESASCEND=fNameAscendS, $
   FNAMESDESCEND=fNameDescendS, $
   SAVETSTRN=saveTStrN, $
   SAVETSTRS=saveTStrS, $
   SAVETSTRASCENDN=saveTStrNAscend, $
   SAVETSTRDESCENDN=saveTStrNDescend, $
   SAVETSTRASCENDS=saveTStrSAscend, $
   SAVETSTRDESCENDS=saveTStrSDescend, $
   SAVE_PS=save_ps, $
   MAKE_SPECIAL_JGR_PLOT=make_special_JGR_plot, $
   SAVEPREF=savePref

  nPlots = 0
  tLimList = LIST()
  pNameList = LIST()
  saveTStrList = LIST()

  IF KEYWORD_SET(plot_ascN) AND N_ELEMENTS(tLimNAscend) GT 0 THEN BEGIN
     nPlots++
     tLimList.Add,tLimNAscend
     pNameList.Add,fNameAscendN
     saveTStrList.Add,saveTStrNAscend
  ENDIF
  IF KEYWORD_SET(plot_descN) AND N_ELEMENTS(tLimNDescend) GT 0 THEN BEGIN
     nPlots++
     tLimList.Add,tLimNDescend
     pNameList.Add,fNameDescendN
     saveTStrList.Add,saveTStrNDescend
  ENDIF
  IF KEYWORD_SET(plot_ascS) AND N_ELEMENTS(tLimSAscend) GT 0 THEN BEGIN
     nPlots++
     tLimList.Add,tLimSAscend
     pNameList.Add,fNameAscendS
     saveTStrList.Add,saveTStrSAscend
  ENDIF
  IF KEYWORD_SET(plot_descS) AND N_ELEMENTS(tLimSDescend) GT 0 THEN BEGIN
     nPlots++
     tLimList.Add,tLimSDescend
     pNameList.Add,fNameDescendS
     saveTStrList.Add,saveTStrSDescend
  ENDIF

  pIdx = 0

  WHILE pIdx LT nPlots DO BEGIN

     IF ~KEYWORD_SET(save_ps) THEN BEGIN

        wInd = pIdx
        WINDOW,wInd,XSIZE=1200,YSIZE=600

     ENDIF ELSE BEGIN

        oldSize = !P.CHARSIZE
        oldSymSize = !P.SYMSIZE

        !P.CHARSIZE = 3.4
        !P.SYMSIZE  = 2.0

        fName = pNameList[pIdx]
        saveTStr = saveTStrList[pIdx]
        psNavn = fName.Replace(".sav","")

        IF KEYWORD_SET(use_losscone) THEN BEGIN
           psNavn = psNavn + '_LC'
        ENDIF

        IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
           SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/outflow_summaries'
        ENDIF

        count = 0
        WHILE FILE_TEST(plotDir+psNavn+(KEYWORD_SET(eps) ? '.eps' : '.ps')) DO BEGIN
           count++
           psNavn = STRING(FORMAT='(A0,A0,"-",I02)', $
                            savePref, $
                            saveTStr, $
                            count)
        ENDWHILE

        POPEN,plotDir+psNavn, $
              ;; /LAND, $
              /PORT, $
              ;; ASPECT=0.625, $
              FONT=-1, $
              ENCAPSULATED=eps  ;,XSIZE=8,YSIZE=7
        DEVICE,/PALATINO,FONT_SIZE=3

     ENDELSE

     ctNum            = 43
     LOADCT2,ctNum

     TPLOT,REVERSE(tPlt_vars),VAR=['ALT','ILAT','MLT'], $
           WINDOW=wInd, $
     TRANGE=tLimList[pIdx]

     TPLOT_PANEL,VARIABLE=upDownRatioSpecVarName,OPLOTVAR=eBoundVarName
     TPLOT_PANEL,VARIABLE=upDownRatioSpecVarName,OPLOTVAR=eBoundLowVarName

     CASE 1 OF
        KEYWORD_SET(save_png): BEGIN
           CGPS_CLOSE
        END
        KEYWORD_SET(save_ps): BEGIN
           !P.CHARSIZE = TEMPORARY(oldSize)
           !P.SYMSIZE  = TEMPORARY(oldSymSize)

           PCLOSE
        END
        ELSE:
     ENDCASE

     pIdx++

  ENDWHILE



END

PRO EXAMINE_ION_CONIC_VS_ALL_FLUX_RATIOS, $
   TIMES=times, $
   UPDOWNMINRATIO=upDownMinRatio, $
   MINNUMQUALIFYINGECHANNELS=minNumQualifyingEChannels, $
   FRACBELOWTHATMUSTBEUPWARD=fracBelowThatMustBeUpward, $
   THRESH_EFLUX=thresh_eFlux, $
   USE_LOSSCONE=use_losscone, $
   MAKE_IONS_OXYGEN=make_ions_oxygen, $
   QUIT_IF_FILE_EXISTS=quit_if_file_exists, $
   ONLY_LEEWARD_IONS=only_leeward_ions, $
   ONLY_CONE_IONS=only_cone_ions, $
   ENFORCE_THIS_SAMPLE_RATE=enforce_this_sample_rate, $
   DO_NOT_ENFORCE_SAMPLE_RATE=do_not_enforce_sample_rate, $
   SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
   REMAKE_DIFF_EFLUX=remake_diff_eFlux, $
   DEF__INCLUDE_SC_POT=dEF__include_sc_pot, $
   SC_POT=sc_pot, $
   ESPECALL=eSpec, $
   ESPECUP=eSpecUp, $
   ESPECDOWN=eSpecDown, $
   UPDOWNRATIOSPEC=upDownRatioSpec, $
   UPALLRATIOSPEC=upAllRatioSpec, $
   EBOUND=eBound, $
   IONMOMSTRUCT=ionMomStruct, $
   IONDIFFEFLUX=diff_eflux, $
   BAD_ION_TIMES=bad_times, $
   IONUPJ=ionUpJ, $
   UP_ARANGEN=up_aRangeN, $
   DOWN_ARANGEN=down_aRangeN, $
   UP_ARANGES=up_aRangeS, $
   DOWN_ARANGES=down_aRangeS, $
   SAVE_PS=save_ps, $
   MAKE_SPECIAL_JGR_PLOT=make_special_JGR_plot, $
   NO_PLOTS=no_plots, $
   ORBIT=orbit, $
   OUT_ORBIT=out_orbit, $
   OUTSTRUCT_ORBIT=ephemStruct, $
   MISLYKTES=mislyktes, $
   TPLT_VARS=tplt_vars, $
   ADD_EBOUND_INFO_TO_IONMOMSTRUCT=add_eBound_info_to_ionMomStruct, $
   BATCH_MODE=batch_mode


  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; UPFLOWVERSION = '20190116'
  ;; UPFLOWVERSION = '20190117'    ;Forgot to add .type member to ionupj!!

  ;; 20190407
  ;; i.   Added 'only_leeward_ions' to struct.info in MOMENT_SUITE_2D
  ;; ii.  We now ALSO flip signs of j, je, and cur in ionMomStruct (no longer just ionUpJ)
  ;; iii. We now ALSO double n, j, je, and cur in ionMomStruct if only leeward (no longer just ionUpJ)
  ;; iv.  We turn on ionMomStruct.info.only_leeward_ions if only leeward
  ;; v.   To save me time remembering in the future, struct member 'errors_are_fractional' has been added to ionMomStruct.info (= 0B by default) 
  UPFLOWVERSION = '20190407'

  ;; UPFLOWVERSION = '20200101'

  remake_file = 0

  ;; loadDir = "/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/"
  diff_eFlux_dir = "/thelonious_data1/FAST/"
  loadDir = "/thelonious_data1/FAST/conic_vs_flux_ratios/"

  ieb_or_ies = "ies"
  ;; enforce_diff_eFlux_sRate = 1.25
  ;; enforce_diff_eFlux_sRate = KEYWORD_SET(enforce_this_sample_rate) ? enforce_this_sample_rate : 2.5

  IF KEYWORD_SET(do_not_enforce_sample_rate) THEN BEGIN
     PRINT,"EXAMINE_ION_CONIC_VS_ALL_FLUX_RATIOS: Not enforcing sample rate ..."
     enforce_diff_eFlux_sRate = 0
  ENDIF ELSE IF KEYWORD_SET(enforce_this_sample_rate) THEN BEGIN
     enforce_diff_eFlux_sRate = enforce_this_sample_rate
  ENDIF ELSE BEGIN
     PRINT,"enforce_diff_eFlux_sRate = 2.5 by default ..."
     enforce_diff_eFlux_sRate = 2.5
  ENDELSE

  calc_geom_factors      = 1
  clean_the_McFadden_way = 0

  deFlux__array_of_structs  = 1
  save_diff_eFlux_to_file   = N_ELEMENTS(save_diff_eFlux_to_file) GT 0 ? save_diff_eFlux_to_file : 1
  load_diff_eFlux_from_file = N_ELEMENTS(remake_diff_eFlux) GT 0 ? ~remake_diff_eFlux : 1

  plot_ascN = 1
  plot_descN = 1
  plot_ascS = 1
  plot_descS = 1

  makeZeroThreshEFlux = KEYWORD_SET(thresh_eFlux) ? thresh_eFlux : 1e6
  makeZeroVal = 0.001

  up_aRangeN = [90,270]
  down_aRangeN = [270,90]
  up_aRangeS = [270,90]
  down_aRangeS = [90,270]

  ;;eliminate ram ions
  IF KEYWORD_SET(only_leeward_ions) THEN BEGIN
        i_angleS     =[180.0,360.0]
        up_aRangeS   =[270.0,360.0]
        down_aRangeS =[180.0,270.0]
        
        i_angleN     =[0.0,180.0]
        up_aRangeN   =[90.0,180.0]
        down_aRangeN =[0.0 ,90.0]

  ENDIF

  IF KEYWORD_SET(only_cone_ions) THEN BEGIN
        i_angleS     =[180.0,360.0]
        up_aRangeS   =[300.0,360.0]
        down_aRangeS =[180.0,240.0]
        
        i_angleN     =[0.0,180.0]
        up_aRangeN   =[120.0,180.0]
        down_aRangeN =[0.0 ,60.0]

  ENDIF

  calib = 1

  IF N_ELEMENTS(times) EQ 0 THEN BEGIN
     tRange = GET_ESA_TIMERANGES__RASKT(/IONS,OUT_TIME_ARRAY=times)
  ENDIF

  IF N_ELEMENTS(orbit) EQ 0 THEN BEGIN
     GET_FA_ORBIT,times,/TIME_ARRAY

     nHere = N_ELEMENTS(times)
     GET_DATA,"ORBIT",DATA=orbit
     orbit = orbit.y[nHere/2]
  ENDIF
  out_orbit = orbit

  ;; upDownRatioStr = ''
  upDownMinRatio = KEYWORD_SET(upDownMinRatio) ? upDownMinRatio : 10
  minNumQualifyingEChannels = KEYWORD_SET(minNumQualifyingEChannels) ? minNumQualifyingEChannels : 10
  IF KEYWORD_SET(upDownMinRatio) THEN BEGIN
     upDownRatioStr = STRING(FORMAT='("-upDownRatio_",I0)',upDownMinRatio)
  ENDIF
  IF KEYWORD_SET(minNumQualifyingEChannels) THEN BEGIN
     minNQualEStr = STRING(FORMAT='("-minNQualECh_",I0)',minNumQualifyingEChannels)
  ENDIF
  IF makeZeroThreshEFlux EQ 1e6 THEN BEGIN
     threshEFluxStr = ''
  ENDIF ELSE BEGIN
     threshEFluxStr = (STRING(FORMAT='("-threshEFlux",G6.1)',makeZeroThreshEFlux)).Replace("E+","e")
     threshEFluxStr = threshEFluxStr.Replace(".","")
     threshEFluxStr = threshEFluxStr.Replace('0',"")
  ENDELSE

  leewardStr = ''
  IF KEYWORD_SET(only_leeward_ions) THEN BEGIN
     leewardStr = "-leeward"
  ENDIF ELSE IF KEYWORD_SET(only_cone_ions) THEN BEGIN
     leewardStr = "-coneyIons"
  ENDIF

  IF KEYWORD_SET(do_not_enforce_sample_rate) THEN BEGIN
     avgItvlStr = ''
  ENDIF ELSE BEGIN
     avgItvlStr    = '-sRate' + (STRING(FORMAT='(F0.2)',enforce_diff_eFlux_sRate)).Replace('.','_')
  ENDELSE

  savePref = "orb_" + STRING(FORMAT='(I0)',orbit)+"-conic_vs_flux_ratios"$
             +avgItvlStr+threshEFluxStr+upDownRatioStr+minNQualEStr + leewardStr
  saveSuff = ".sav"

  DIFF_EFLUX_FNAME, $
     T1=times[0], $
     T2=times[-1], $
     ORBIT=orbit, $
     EEB_OR_EES=ieb_or_ies, $
     BONUSPREF=bonusPref ,$
     SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file ,$
     SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file,$
     LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file,$
     MCFADDEN_DIFF_EFLUX=McFadden_diff_eFlux, $
     OUT_DIFF_EFLUX_FILE=diff_eFlux_file, $
     ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     LOADDIR=diff_eFlux_dir

  IF (KEYWORD_SET(dEF__include_sc_pot) OR N_ELEMENTS(dEF__include_sc_pot) EQ 0) AND $
     N_ELEMENTS(sc_pot) EQ 0 THEN BEGIN
     ;; GET_SC_POTENTIAL,T1=diff_eFlux.time[0],T2=diff_eFlux.time[-1], $
     GET_SC_POTENTIAL,T1=times[0],T2=times[-1], $
                      DATA=sc_pot, $
                      FROM_FA_POTENTIAL=pot__from_fa_potential, $
                      ALL=pot__all, $
                      /REPAIR, $
                      CHASTON_STYLE=pot__Chaston_style, $
                      FILENAME=pot__fName, $
                      FROM_FILE=pot__from_file, $
                      ORBIT=orbit, $
                      SAVE_FILE=pot__save_file
     
  ENDIF

  GET_DIFF_EFLUX,T1=times[0],T2=times[-1], $
                 EEB_OR_EES=ieb_or_ies, $
                 SC_POT=sc_pot, $
                 ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                 CLEAN_THE_MCFADDEN_WAY=clean_the_McFadden_way, $
                 CALC_GEOM_FACTORS=calc_geom_factors, $
                 ARRAY_OF_STRUCTS_INSTEAD=deFlux__array_of_structs, $
                 SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                 OVERWRITE_EXISTING=overwrite_existing, $
                 DIFF_EFLUX_FILE=diff_eFlux_file, $
                 LOAD_DAT_FROM_FILE=load_diff_eFlux_from_file, $
                 LOAD_DIR=diff_eFlux_dir, $
                 OUT_DIFF_EFLUX=diff_eflux

  IF KEYWORD_SET(make_ions_oxygen) THEN BEGIN
     PRINT,"Assuming they're oxygen!!!"
     diff_eFlux[*].mass = diff_eFlux[0].mass * 16.
  ENDIF

  tDiffs     = diff_eFlux.end_time - diff_eFlux.time

  ;; Now get times corresponding to diff_eFlux
  GET_FA_ORBIT,diff_eFlux.time,/TIME_ARRAY,/ALL,STRUC=ephemStruct

  IF KEYWORD_SET(use_losscone) THEN BEGIN
     GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI, $
        ;; t1,t2, $
        ephemStruct.time[0],ephemStruct.time[-1], $
        ionlc_angleRange, $
        i_angle,i_angle_up, $
        north_southArr, $
        ALLEXCLATM_ARANGE=allExclAtm_aRange, $
        OUT_LCW=lcw, $
        ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
        CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
        OUT_E_ANGLE=e_angle, $
        ANGLESTR=angleStr, $
        SDTSTRUCT=ephemStruct ;; , $
     ;; /JUST_ONE

     ;; Flip lc_angleRange so that it's upgoing
     arange__moments = (360.*((ionlc_angleRange-180)/360.-FLOOR((ionlc_angleRange-180)/360.)))

  ENDIF

  get_data,'ILAT',data=ILAT
  get_data,'MLT',data=MLT

  if (n_elements(ILAT.y) LE 0) then return

  GET_N_S_ASCENDING_DESCENDING_TIME_LIMITS, $
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
     NORTHI=northI, $
     SOUTHI=southI, $
     NASCENDS=nSAscend, $
     NDESCENDS=nSDescend, $
     SAVETSTRN=saveTStrN, $
     SAVETSTRS=saveTStrS, $
     SAVETSTRASCENDN=saveTStrNAscend, $
     SAVETSTRDESCENDN=saveTStrNDescend, $
     SAVETSTRASCENDS=saveTStrSAscend, $
     SAVETSTRDESCENDS=saveTStrSDescend

  ;; Ze filenames
  fName = savePref+saveSuff

  IF N_ELEMENTS(saveTStrN) GT 0 THEN BEGIN
     fNameN = savePref+'-'+saveTStrN+saveSuff
     ;; fNameN = savePref+saveSuff
     ;; PRINT,fNameN
  ENDIF
  IF N_ELEMENTS(saveTStrNAscend) GT 0 THEN BEGIN
     fNameNAscend  = savePref+'-NASC-'+saveTStrNAscend+saveSuff
     ;; PRINT,fNameNAscend
  ENDIF
  IF N_ELEMENTS(saveTStrNDescend) GT 0 THEN BEGIN
     fNameNDescend = savePref+'-NDESC-'+saveTStrNDescend+saveSuff
     ;; PRINT,fNameNDescend
  ENDIF
  IF N_ELEMENTS(saveTStrS) GT 0 THEN BEGIN
     fNameS = savePref+'-'+saveTStrS+saveSuff
     ;; PRINT,fNameS
  ENDIF
  IF N_ELEMENTS(saveTStrSAscend) GT 0 THEN BEGIN
     fNameSAscend  = savePref+'-SASC-'+saveTStrSAscend+saveSuff
     ;; PRINT,fNameSAscend
  ENDIF
  IF N_ELEMENTS(saveTStrSDescend) GT 0 THEN BEGIN
     fNameSDescend = savePref+'-SDESC-'+saveTStrSDescend+saveSuff
     ;; PRINT,fNameSDescend
  ENDIF

  ;; t1Str = '1998-09-25/00:00:00'
  ;; t2Str = '1998-09-25/00:18:00'

  ;; orbit = 8276
  ;; t1 = S2T(t1Str)
  ;; t2 = S2T(t2Str)

  ;; saveTStr = STRMID(t1Str,0,10)                      + "__" $
  ;;            + (STRMID(t1Str,11,8)).Replace(":","_") + "-"  $
  ;;            + (STRMID(t2Str,11,8)).Replace(":","_")
  ;; fName = savePref+saveTStr+saveSuff

  upVarName = 'eSpecUp'
  downVarName = 'eSpecDown'
  allAngleVarName = 'eSpec'

  needN = N_ELEMENTS(tLimN) GT 0 
  needS = N_ELEMENTS(tLimS) GT 0 

  restoredN = 0
  restoredS = 0

  ;; BEGIN POSSIBLY UNNECESSARY LINES
  upVarNameN = 'eSpecUpN'
  downVarNameN = 'eSpecDownN'
  allAngleVarNameN = 'eSpecN'
  upVarNameS = 'eSpecUpS'
  downVarNameS = 'eSpecDownS'
  allAngleVarNameS = 'eSpecS'

  ;; For later
  eSpecTimes = !NULL
  eSpecBad = !NULL

  IF needN AND ~KEYWORD_SET(remake_file) THEN BEGIN

     IF FILE_TEST(loadDir+fNameN) THEN BEGIN
        PRINT,"Restoring " + fNameN + "..."
        RESTORE,loadDir+fNameN

        varName = upVarNameN
        STORE_DATA,varName,DATA=eSpecUpN

        varName = downVarNameN
        STORE_DATA,varName,DATA=eSpecDownN

        varName = allAngleVarNameN
        STORE_DATA,varName,DATA=eSpecN

        IF N_ELEMENTS(SAVEUPFLOWVERSION) GT 0 THEN BEGIN
           restoredN = UPFLOWVERSION EQ SAVEUPFLOWVERSION

           IF ~restoredN THEN PRINT,"Wrong North version: ",SAVEUPFLOWVERSION + '. Remaking ...'

        ENDIF ELSE BEGIN
           PRINT,"No North version! Remaking file ..."
        ENDELSE

        SAVEUPFLOWVERSION = !NULL

     ENDIF

     IF restoredN THEN BEGIN
        eSpecTimes = [eSpecTimes,out_timeN]
        eSpecBad = [eSpecBad,out_bad_timeN]
     ENDIF

  ENDIF

  IF needS AND ~KEYWORD_SET(remake_file) THEN BEGIN
     IF FILE_TEST(loadDir+fNameS) THEN BEGIN
        PRINT,"Restoring " + fNameS + "..."
        RESTORE,loadDir+fNameS

        varName = upVarNameS
        STORE_DATA,varName,DATA=eSpecUpS

        varName = downVarNameS
        STORE_DATA,varName,DATA=eSpecDownS

        varName = allAngleVarNameS
        STORE_DATA,varName,DATA=eSpecS

        IF N_ELEMENTS(SAVEUPFLOWVERSION) GT 0 THEN BEGIN
           restoredS = UPFLOWVERSION EQ SAVEUPFLOWVERSION

           IF ~restoredS THEN PRINT,"Wrong South version: ",SAVEUPFLOWVERSION + '. Remaking ...'

        ENDIF ELSE BEGIN
           PRINT,"No South version! Remaking file ..."
        ENDELSE

        SAVEUPFLOWVERSION = !NULL

     ENDIF

     IF restoredS THEN BEGIN
        eSpecTimes = [eSpecTimes,out_timeS]
        eSpecBad = [eSpecBad,out_bad_timeS]
     ENDIF

  ENDIF
  ;; END POSSIBLY UNNECESSARY LINES

  IF KEYWORD_SET(quit_if_file_exists) THEN BEGIN

     ;; Make sure version is correct
     isCorrectVersion = 0B

     IF FILE_TEST(loadDir+fName) THEN BEGIN

        PRINT,"Restoring " + fName + "..."
        RESTORE,loadDir+fName

        IF N_ELEMENTS(SAVEUPFLOWVERSION) GT 0 THEN BEGIN
           isCorrectVersion = UPFLOWVERSION EQ SAVEUPFLOWVERSION
           SAVEUPFLOWVERSION = !NULL
        ENDIF

        varName = upVarName
        STORE_DATA,varName,DATA=eSpecUp

        varName = downVarName
        STORE_DATA,varName,DATA=eSpecDown

        varName = allAngleVarName
        STORE_DATA,varName,DATA=eSpec

     ENDIF

     ;; Don't use these; I think we can combine eBoundN and eBoundS as well as the ionups

     ;; canQuitN = needN ? $
     ;;            (N_ELEMENTS(eBoundN) GT 0 AND N_ELEMENTS(ionupJN) GT 0) : $
     ;;            1
     ;; canQuitS = needS ? $
     ;;            (N_ELEMENTS(eBoundS) GT 0 AND N_ELEMENTS(ionupJS) GT 0) : $
     ;;            1

     ;; IF haveIonStruct AND canQuitN AND canQuitS THEN RETURN

     canQuit = (N_ELEMENTS(eBound) GT 0) AND (N_ELEMENTS(ionupJ) GT 0) AND isCorrectVersion

     haveIonStruct = N_ELEMENTS(ionMomStruct) GT 0

     IF haveIonStruct AND canQuit THEN RETURN

  ENDIF 

  ;; t1N = tLimN[0]
  ;; t2N = tLimN[1]

  ;; t1S = tLimS[0]
  ;; t2S = tLimS[1]

  SAVEUPFLOWVERSION = UPFLOWVERSION

  ;; Now need to treat Northern and Southern separately, o' course, since the angle ranges are totally flipped
  IF nN GT 0 AND ~restoredN THEN BEGIN

     GET_FA_RATIO_OF_ION_SPECTROGRAMS, $
        T1=tLimN[0], $
        T2=tLimN[1], $
        DOWNVARNAME=downVarNameN, $
        UPVARNAME=upVarNameN, $
        ALLANGLEVARNAME=allAngleVarNameN, $
        UP_ARANGE=up_aRangeN, $
        DOWN_ARANGE=down_aRangeN, $
        UNITS=units, $
        GET_EN_SPEC_PRO=get_en_spec_pro, $
        CALIB=calib, $
        MAKEZEROTHRESHEFLUX=makeZeroThreshEFlux, $
        MAKEZEROVAL=makeZeroVal, $
        OUT_DOWNESPEC=eSpecDownN, $
        OUT_UPESPEC=eSpecUpN, $
        OUT_ALLANGLEESPEC=eSpecN, $
        OUT_UPDOWNRATIOSPEC=upDownRatioSpecN, $
        OUT_UPALLRATIOSPEC=upAllRatioSpecN, $
        OUT_ESPECUP_TIME=out_timeN, $
        OUT_BAD_TIME=out_bad_timeN, $
        /USE_DIFF_EFLUX, $
        DIFF_EFLUX=diff_eFlux, $
        IS_MCFADDEN_DIFF_EFLUX=deFlux__array_of_structs, $
        QUIET=quiet

     eSpecTimes = [eSpecTimes,out_timeN]
     eSpecBad = [eSpecBad,out_bad_timeN]

     PRINT,"Saving " + fNameN + " ..."
     SAVE, $
        eSpecN,eSpecUpN,eSpecDownN,upDownRatioSpecN,upAllRatioSpecN, $
        up_aRangeN,down_aRangeN,out_timeN,out_bad_timeN,SAVEUPFLOWVERSION, $
        FILENAME=loadDir+fNameN

  ENDIF

  IF nS GT 0 AND ~restoredS THEN BEGIN

     GET_FA_RATIO_OF_ION_SPECTROGRAMS, $
        T1=tLimS[0], $
        T2=tLimS[1], $
        DOWNVARNAME=downVarNameS, $
        UPVARNAME=upVarNameS, $
        ALLANGLEVARNAME=allAngleVarNameS, $
        UP_ARANGE=up_aRangeS, $
        DOWN_ARANGE=down_aRangeS, $
        UNITS=units, $
        GET_EN_SPEC_PRO=get_en_spec_pro, $
        CALIB=calib, $
        MAKEZEROTHRESHEFLUX=makeZeroThreshEFlux, $
        MAKEZEROVAL=makeZeroVal, $
        OUT_DOWNESPEC=eSpecDownS, $
        OUT_UPESPEC=eSpecUpS, $
        OUT_ALLANGLEESPEC=eSpecS, $
        OUT_UPDOWNRATIOSPEC=upDownRatioSpecS, $
        OUT_UPALLRATIOSPEC=upAllRatioSpecS, $
        OUT_ESPECUP_TIME=out_timeS, $
        OUT_BAD_TIME=out_bad_timeS, $
        /USE_DIFF_EFLUX, $
        DIFF_EFLUX=diff_eFlux, $
        IS_MCFADDEN_DIFF_EFLUX=deFlux__array_of_structs, $
        QUIET=quiet

     eSpecTimes = [eSpecTimes,out_timeS]
     eSpecBad = [eSpecBad,out_bad_timeS]

     PRINT,"Saving " + fNameS + " ..."
     SAVE, $
        eSpecS,eSpecUpS,eSpecDownS,upDownRatioSpecS,upAllRatioSpecS, $
        up_aRangeS,down_aRangeS,out_timeS,out_bad_timeS,SAVEUPFLOWVERSION, $
        FILENAME=loadDir+fNameS

  ENDIF

  ;; Align the spectrum times with the diff_eFlux measurements
  this = VALUE_CLOSEST2(eSpecTimes,diff_eFlux.time,/CONSTRAINED)
  totDiffs = diff_eFlux.time-eSpecTimes[this]
  refDiff = (tDiffs[WHERE(FINITE(tDiffs),/NULL)])[0]
  OKdiff = WHERE(FINITE(totDiffs) AND (ABS(totDiffs) LT refDiff))

  ;; -1 means "not known"
  bad_times = MAKE_ARRAY(N_ELEMENTS(diff_eFlux.time),/LONG,VALUE=-1L)
  bad_times[OKdiff] = eSpecBad[this[OKdiff]]

  ;; SSDDD
  ;; Moments???
  ion_min_if_nan_scpots = 4.
  energy = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX( $
           diff_eFlux, $
           ENERGY=[4,100], $
           SC_POT=tmpSc_pot, $
           EEB_OR_EES=ieb_or_ies, $
           MIN_IF_NAN_SCPOTS=ion_min_if_nan_scpots)

  IF KEYWORD_SET(only_leeward_ions) THEN BEGIN
     aRange__moments = MAKE_ARRAY(2,N_ELEMENTS(diff_eFlux),VALUE=0.)
  ENDIF

  IF N_ELEMENTS(eSpecN) GT 0 THEN BEGIN

     IDENTIFY_ION_UPFLOW_ENERGY_BOUNDARY, $
        UPDOWNMINRATIO=upDownMinRatio, $
        MINNUMQUALIFYINGECHANNELS=minNumQualifyingEChannels, $
        FRACBELOWTHATMUSTBEUPWARD=fracBelowThatMustBeUpward, $
        DOWNESPEC=eSpecDownN, $
        UPESPEC=eSpecUpN, $
        ALLANGLEESPEC=eSpecN, $
        UPDOWNRATIOSPEC=upDownRatioSpecN, $
        OUT_EBOUND=eBoundN

     nortenyo = WHERE(diff_eflux.ilat GT 10,nNortenyo)
     theseN = VALUE_CLOSEST2(out_timeN,diff_eFlux[nortenyo].time, $
                            /CONSTRAINED)
     ;; IF nNortenyo NE N_ELEMENTS(eBoundN.y) THEN PRINT,"DIE"
     energy[1,nortenyo]   = eBoundN.y[theseN]
     energy[0,nortenyo]   = eBoundN.yLow[theseN]

     IF KEYWORD_SET(only_leeward_ions) THEN BEGIN
        aRange__moments[*,nortenyo] = i_angleN # MAKE_ARRAY(nNortenyo,VALUE=1.)
     ENDIF
  ENDIF

  IF N_ELEMENTS(eSpecS) GT 0 THEN BEGIN

     IDENTIFY_ION_UPFLOW_ENERGY_BOUNDARY, $
        UPDOWNMINRATIO=upDownMinRatio, $
        MINNUMQUALIFYINGECHANNELS=minNumQualifyingEChannels, $
        FRACBELOWTHATMUSTBEUPWARD=fracBelowThatMustBeUpward, $
        DOWNESPEC=eSpecDownS, $
        UPESPEC=eSpecUpS, $
        ALLANGLEESPEC=eSpecS, $
        UPDOWNRATIOSPEC=upDownRatioSpecS, $
        OUT_EBOUND=eBoundS

     sudenyo = WHERE(diff_eflux.ilat LT -10,nSudenyo)

     ;; out_timeS and eBoundS.x have the same number of elements
     theseS = VALUE_CLOSEST2(out_timeS,diff_eFlux[sudenyo].time, $
                            /CONSTRAINED)
     ;; IF nSudenyo NE N_ELEMENTS(eBoundS.y) THEN PRINT,"DIE"

     energy[1,sudenyo]   = eBoundS.y[theseS]
     energy[0,sudenyo]   = eBoundS.yLow[theseS]

     IF KEYWORD_SET(only_leeward_ions) THEN BEGIN
        aRange__moments[*,sudenyo] = i_angleS # MAKE_ARRAY(nSudenyo,VALUE=1.)
     ENDIF

  ENDIF

  these = WHERE(energy[1,*] LT energy[0,*],COMPLEMENT=notThese,/NULL)
  origValid = diff_eflux.valid
  validBef = TOTAL(diff_eflux.valid)

  diff_eflux[these].valid = 0
  validAft = TOTAL(diff_eflux.valid)
  PRINT,FORMAT='(A0,I5,A0,I5,A0)',"Only ",validAft," out of ",validBef," ion obs are upflow"
  quiet = 1

  MOMENT_SUITE_2D,diff_eFlux, $
                  ENERGY=energy, $
                  ARANGE__MOMENTS=aRange__moments, $
                  SC_POT=tmpSc_pot, $
                  EEB_OR_EES=ieb_or_ies, $
                  /ERROR_ESTIMATES, $
                  ;; MAP_TO_100KM=map_to_100km, $ 
                  ORBIT=orbit, $
                  /NEW_MOMENT_ROUTINE, $
                  QUIET=quiet, $
                  OUTTIME=time, $
                  OUT_N=n, $
                  OUT_J_=j, $
                  OUT_JE=je, $
                  OUT_T=T, $
                  OUT_CHARE=charE, $
                  OUT_CURRENT=cur, $
                  OUT_JJE_COVAR=jje_coVar, $
                  OUT_ERRORS=errors, $
                  OUT_ERR_N=nErr, $
                  OUT_ERR_J_=jErr, $
                  OUT_ERR_JE=jeErr, $
                  OUT_ERR_T=TErr, $
                  OUT_ERR_CURRENT=curErr, $
                  OUT_ERR_CHARE=charEErr, $
                  INOUT_MAPRATIO=mapRatio, $
                  OUT_STRUCT=ionMomStruct, $
                  BATCH_MODE=batch_mode, $
                  MCFADDEN_STYLE_DIFF_EFLUX=deFlux__array_of_structs, $
                  /PROVIDING_EPHEM_INFO, $
                  IN_ILAT=ephemStruct.ilat, $
                  IN_MLT=ephemStruct.mlt, $
                  IN_ALT=ephemStruct.alt


  CASE 1 OF
     KEYWORD_SET(eBoundN) AND KEYWORD_SET(eBoundS): BEGIN

        IF eBoundN.x[0] LT eBoundS.x[0] THEN BEGIN
           eBound1           = TEMPORARY(eBoundN         ) 
           out_time1         = TEMPORARY(out_timeN       )
           eSpec1            = TEMPORARY(eSpecN          )
           eSpecUp1          = TEMPORARY(eSpecUpN        )
           eSpecDown1        = TEMPORARY(eSpecDownN      )
           upDownRatioSpec1  = TEMPORARY(upDownRatioSpecN)
           upAllRatioSpec1   = TEMPORARY(upAllRatioSpecN )

           eBound2           = TEMPORARY(eBoundS         ) 
           out_time2         = TEMPORARY(out_timeS       )
           eSpec2            = TEMPORARY(eSpecS          )
           eSpecUp2          = TEMPORARY(eSpecUpS        )
           eSpecDown2        = TEMPORARY(eSpecDownS      )
           upDownRatioSpec2  = TEMPORARY(upDownRatioSpecS)
           upAllRatioSpec2   = TEMPORARY(upAllRatioSpecS )
        ENDIF ELSE BEGIN
           eBound1           = TEMPORARY(eBoundS         )
           out_time1         = TEMPORARY(out_timeS       )
           eSpec1            = TEMPORARY(eSpecS          )
           eSpecUp1          = TEMPORARY(eSpecUpS        )
           eSpecDown1        = TEMPORARY(eSpecDownS      )
           upDownRatioSpec1  = TEMPORARY(upDownRatioSpecS)
           upAllRatioSpec1   = TEMPORARY(upAllRatioSpecS )

           eBound2           = TEMPORARY(eBoundN         )
           out_time2         = TEMPORARY(out_timeN       )
           eSpec2            = TEMPORARY(eSpecN          )
           eSpecUp2          = TEMPORARY(eSpecUpN        )
           eSpecDown2        = TEMPORARY(eSpecDownN      )
           upDownRatioSpec2  = TEMPORARY(upDownRatioSpecN)
           upAllRatioSpec2   = TEMPORARY(upAllRatioSpecN )
        ENDELSE

        eBound = {x   : [eBound1.x  ,eBound2.x  ], $
                  y   : [eBound1.y  ,eBound2.y  ], $
                  ind : [eBound1.ind,eBound2.ind], $
                  yLow   : [eBound1.yLow  ,eBound2.yLow  ], $
                  indLow : [eBound1.indLow,eBound2.indLow], $
                  type   : [eBound1.type  ,eBound2.type  ]}

        out_time = [out_time1,out_time2]

        eSpec = {x : [eSpec1.x,eSpec2.x], $
                 y : [eSpec1.y,eSpec2.y], $
                 v : [eSpec1.v,eSpec2.v], $
                 yerr :  [eSpec1.yerr,eSpec2.yerr], $
                 verr :  [eSpec1.verr,eSpec2.verr]}

        eSpecUp = {x : [eSpecUp1.x,eSpecUp2.x], $
                   y : [eSpecUp1.y,eSpecUp2.y], $
                   v : [eSpecUp1.v,eSpecUp2.v], $
                   yerr :  [eSpecUp1.yerr,eSpecUp2.yerr], $
                   verr :  [eSpecUp1.verr,eSpecUp2.verr]}

        eSpecDown = {x : [eSpecDown1.x,eSpecDown2.x], $
                     y : [eSpecDown1.y,eSpecDown2.y], $
                     v : [eSpecDown1.v,eSpecDown2.v], $
                     yerr :  [eSpecDown1.yerr,eSpecDown2.yerr], $
                     verr :  [eSpecDown1.verr,eSpecDown2.verr]}

        upDownRatioSpec = {x : [upDownRatioSpec1.x,upDownRatioSpec2.x], $
                           y : [upDownRatioSpec1.y,upDownRatioSpec2.y], $
                           v : [upDownRatioSpec1.v,upDownRatioSpec2.v]}

        upAllRatioSpec = {x : [upAllRatioSpec1.x,upAllRatioSpec2.x], $
                          y : [upAllRatioSpec1.y,upAllRatioSpec2.y], $
                          v : [upAllRatioSpec1.v,upAllRatioSpec2.v]}


     END
     KEYWORD_SET(eBoundN): BEGIN
        eBound           = TEMPORARY(eBoundN)
        eSpec            = TEMPORARY(eSpecN          )
        eSpecUp          = TEMPORARY(eSpecUpN        )
        eSpecDown        = TEMPORARY(eSpecDownN      )
        upAllRatioSpec   = TEMPORARY(upAllRatioSpecN )
        upDownRatioSpec  = TEMPORARY(upDownRatioSpecN)
        out_time         = TEMPORARY(out_timeN       )
        
     END
     KEYWORD_SET(eBoundS): BEGIN
        eBound = TEMPORARY(eBoundS)
        eSpec            = TEMPORARY(eSpecS          )
        eSpecUp          = TEMPORARY(eSpecUpS        )
        eSpecDown        = TEMPORARY(eSpecDownS      )
        upAllRatioSpec   = TEMPORARY(upAllRatioSpecS )
        upDownRatioSpec  = TEMPORARY(upDownRatioSpecS)
        out_time         = TEMPORARY(out_timeS       )

     END
  ENDCASE

  ;; these = VALUE_CLOSEST2(out_time,diff_eFlux.time, $
  ;;                         /CONSTRAINED)

  these = WHERE((eSpecBad EQ 1) OR (eSpecBad EQ 0))

  upflow_i = WHERE(diff_eFlux.valid AND (eBound.y[these] GT 4),nUpflow, $
                   COMPLEMENT=notUpflow_i, $
                   NCOMPLEMENT=nNotUpflow)

  IF (N_ELEMENTS(diff_eFlux.valid) NE N_ELEMENTS(ionMomStruct.j )) OR $
     (N_ELEMENTS(diff_eFlux.valid) NE N_ELEMENTS(eBound.x[these])) $
  THEN BEGIN
     PRINT,"TIDSERIENE STEMMER IKKE!"
     PRINT,"TILBAKE ..."
     mislyktes = 1
     RETURN
  ENDIF

  ionupJ = {x    : eBound.x[these], $
            y    :  ionMomStruct.j, $
            type : eBound.type[these]}

  IF KEYWORD_SET(add_eBound_info_to_ionMomStruct) THEN BEGIN
     ionMomStruct = CREATE_STRUCT(ionMomStruct, $
                                  "eRange",TRANSPOSE([[eBound.y[these]], $
                                                      [eBound.yLow[these]]]), $
                                  "eRangeInd",TRANSPOSE([[eBound.ind[these]], $
                                                         [eBound.indLow[these]]]), $
                                  "type",eBound.type[these])
  ENDIF

  IF KEYWORD_SET(use_losscone) THEN BEGIN
     ionMomStruct.info.losscone_ions = 1B
  ENDIF

  IF nNotUpflow GT 0 THEN ionupJ.y[notUpflow_i] = !VALUES.F_NaN

  ;; Make original valids valid
diff_eflux.valid = origValid

  ;; We actually have to flip signs regardless of hemisphere because (shy of the DONT_FLIP_SIGN keyword)
  ;; MOMENT_SUITE_2D makes all earthward fluxes positive and all anti-earthward fluxes negative
  ;; flipMe = WHERE(FINITE(ionupj.y) AND diff_eflux.ilat GT 10,nFlip,/NULL)
  flipMe = WHERE(FINITE(ionupj.y),nFlip,/NULL)

  IF nFlip GT 0 THEN BEGIN

     ionupJ.y[flipMe] = -1. * ionupJ.y[flipMe]

     ionMomStruct.j[flipMe] = -1. * ionMomStruct.j[flipMe]
     ionMomStruct.je[flipMe] = -1. * ionMomStruct.je[flipMe]
     ionMomStruct.cur[flipMe] = -1. * ionMomStruct.cur[flipMe] ;Should this be flipped?

     IF KEYWORD_SET(only_leeward_ions) THEN BEGIN
        ionupJ.y[flipMe] *= 2.

        ionMomStruct.n[flipMe] *= 2.
        ionMomStruct.j[flipMe] *= 2.
        ionMomStruct.je[flipMe] *= 2.
        ionMomStruct.cur[flipMe] *= 2.

        ionMomStruct.nErr[flipMe] *= 2.
        ionMomStruct.jErr[flipMe] *= 2.
        ionMomStruct.jeErr[flipMe] *= 2.
        ionMomStruct.curErr[flipMe] *= 2.

        ionMomStruct.info.only_leeward_ions = 1B
     ENDIF

  ENDIF

  IF ~KEYWORD_SET(isCorrectVersion) THEN BEGIN

     PRINT,"Saving " + fName + " ..."
     SAVE, $
        eSpec,eSpecUp,eSpecDown,upDownRatioSpec,upAllRatioSpec, $
        eBound, ionMomStruct, ionupJ, $
        up_aRangeN,down_aRangeN, $
        up_aRangeS,down_aRangeS,SAVEUPFLOWVERSION, $
        FILENAME=loadDir+fName

  ENDIF

  typiskPanelSize = 2
  IF KEYWORD_SET(make_special_JGR_plot) THEN BEGIN

     typiskPanelSize = 1

     specUnits = 'eflux'
     ionSpecLogLims  = [5.,9.]
     ;; eSpecLogLims = [6.,9.]
     specLogUnitsString = 'Log eV!C/cm!U2!N-s-sr-eV'

     var_name='Iesa_Angle'
     ion_ER = KEYWORD_SET(ion_energyRange) ? ion_energyRange : [4.,30000.]
     GET_PA_SPEC,'fa_' + ieb_or_ies + '_c', $
                 T1=t1, $
                 T2=t2, $
                 UNITS=specUnits, $
                 NAME=var_name, $
                 ENERGY=ion_ER, $
                 /RETRACE, $
                 /CALIB
     GET_DATA,var_name,DATA=data        

     data.y = ALOG10(data.y)
     STORE_DATA,var_name, DATA=data
     OPTIONS,var_name,'spec',1	
     ;; ZLIM,var_name,4,9,0
        ZLIM,var_name, $
             (MIN(data.y[WHERE(FINITE(data.y))]) > ionSpecLogLims[0]), $
             (MAX(data.y[WHERE(FINITE(data.y))]) < ionSpecLogLims[1]),0
     ;; ZLIM,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
     YLIM,var_name,0,360,0
     OPTIONS,var_name,'ytitle','Ions > ' + STRING(FORMAT='(I0)',ion_ER[0]) + ' eV!C!CAngle (Deg.)'
     OPTIONS,var_name,'ztitle',specLogUnitsString
     OPTIONS,var_name,'x_no_interp',1
     OPTIONS,var_name,'y_no_interp',1
     OPTIONS,var_name,'panel_size',bigPanelSize

     GET_DATA,var_name, DATA=data
     bb = WHERE (data.v GT 270.,nb)
     IF (nb GT 0) THEN data.v[bb]=data.v[bb]-360.
     nn = N_ELEMENTS(data.x)
     FOR n = 0,nn-1L DO BEGIN
        bs = SORT (data.v[n,*])
        data.v[n,*]=data.v[n,bs]
        data.y[n,*]=data.y[n,bs]
     endfor
     STORE_DATA,var_name, DATA=data	
     OPTIONS,var_name,'yminor',9
     OPTIONS,var_name,'yticks',4
     OPTIONS,var_name,'ytickv',[-90,0,90,180,270]
     YLIM,var_name,-90,270,0

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[var_name] ELSE tPlt_vars=[var_name,tPlt_vars]
  ENDIF

  varName = upVarName
  STORE_DATA,varName,DATA=eSpecUp
  OPTIONS,varName,'spec',1
  ZLIM,varName,1e4,1e8,1
  ylim,varName,4,24000,1
  OPTIONS,varName,'ytitle','Upward ions!C!CEnergy (eV)'
  OPTIONS,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
  OPTIONS,varName,'x_no_interp',1
  OPTIONS,varName,'y_no_interp',1
  OPTIONS,varName,'panel_size',typiskPanelSize
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  varName = downVarName
  STORE_DATA,varName,DATA=eSpecDown
  OPTIONS,varName,'spec',1
  zlim,varName,1e4,1e8,1
  ylim,varName,4,24000,1
  OPTIONS,varName,'ytitle','Downward ions !C!CEnergy (eV)'
  OPTIONS,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
  OPTIONS,varName,'x_no_interp',1
  OPTIONS,varName,'y_no_interp',1
  OPTIONS,varName,'panel_size',typiskPanelSize
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  ;; varName = allAngleVarNameN
  ;; OPTIONS,varName,'spec',1
  ;; zlim,varName,1e4,1e8,1
  ;; ylim,varName,3,40000,1
  ;; OPTIONS,varName,'ytitle','ions (all angles)!C!CEnergy (eV)'
  ;; OPTIONS,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
  ;; OPTIONS,varName,'x_no_interp',1
  ;; OPTIONS,varName,'y_no_interp',1
  ;; OPTIONS,varName,'panel_size',typiskPanelSize
  ;; IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  ;; varName = "ratioN"
  ;; data = upAllRatioSpecN
  ;; STORE_DATA,varName,DATA=data
  ;; OPTIONS,varName,'spec',1
  ;; YLIM,varName,4,24000,1
  ;; ZLIM,varName,0.1,100,1
  ;; OPTIONS,varName,'ytitle',"Ion energy (eV)"
  ;; OPTIONS,varName,'ztitle','Up/all-angle ion eFlux'
  ;; OPTIONS,varName,'x_no_interp',1
  ;; OPTIONS,varName,'y_no_interp',1

  ;; IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  upDownRatioSpecVarName = "ratioNUpDown"
  varName = upDownRatioSpecVarName
  data = upDownRatioSpec
  STORE_DATA,varName,DATA=data
  OPTIONS,varName,'spec',1
  YLIM,varName,4,24000,1
  ZLIM,varName,0.1,1000,1
  OPTIONS,varName,'ytitle',"Ion energy (eV)"
  OPTIONS,varName,'ztitle','Up/Down ion eFlux'
  OPTIONS,varName,'x_no_interp',1
  OPTIONS,varName,'y_no_interp',1
  OPTIONS,varName,'panel_size',typiskPanelSize

  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  hvit             = 255
  eBoundVarName = "eBound"
  ;; potLStyle = 1 ;dotted
  ;; potLStyle = 2           ;dashed
  potLStyle = 0                 ;solid
  potColor  = hvit
  ;; potLStyle = 3 ;dash dot
  ;; potLStyle = 4 ;dash dot dot
  STORE_DATA,eBoundVarName,DATA={x:eBound.x,y:eBound.y}
  OPTIONS,eBoundVarName,'LINESTYLE',potLStyle
  OPTIONS,eBoundVarName,'colors',potColor
  OPTIONS,eBoundVarName,'thick',3.0

  eBoundLowVarName = "eBoundLow"
  ;; potLStyle = 1 ;dotted
  ;; potLStyle = 2           ;dashed
  potLStyle = 0                 ;solid
  potColor  = 40
  ;; potLStyle = 3 ;dash dot
  ;; potLStyle = 4 ;dash dot dot
  STORE_DATA,eBoundLowVarName,DATA={x:eBound.x,y:eBound.yLow}
  OPTIONS,eBoundLowVarName,'LINESTYLE',potLStyle
  OPTIONS,eBoundLowVarName,'colors',potColor
  OPTIONS,eBoundLowVarName,'thick',3.0

  varName = "ion_upJ"
  STORE_DATA,varName,DATA=ionupJ
  ;; YLIM,varName,6e6,6e9,1
  OPTIONS,varName,'ytitle','Ion flux'
  OPTIONS,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
  OPTIONS,varName,'x_no_interp',1
  OPTIONS,varName,'y_no_interp',1
  OPTIONS,varName,'panel_size',1
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]
  
  IF KEYWORD_SET(make_special_JGR_plot) THEN BEGIN

     varName = "dB_east"
     GET_DATA,"dB_fac",DATA=magData

     IF SIZE(magData,/TYPE) NE 8 THEN BEGIN
        UCLA_MAG_DESPIN
        GET_DATA,"dB_fac",DATA=magData
        IF N_ELEMENTS(magData) EQ 0 THEN STOP
     ENDIF

     dbEast = {x:magData.x, $
               y:magData.y[*,1]}
     STORE_DATA,varName,DATA=dbEast

     ;; YLIM,varName,6e6,6e9,1
     OPTIONS,varName,'ytitle','B!DE-W!N (nT)'


     ;; OPTIONS,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
     ;; OPTIONS,varName,'x_no_interp',1
     ;; OPTIONS,varName,'y_no_interp',1
     OPTIONS,varName,'panel_size',1
     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]
     
  ENDIF

  IF ~KEYWORD_SET(no_plots) THEN BEGIN
     TPLOT_UP_VS_DOWN_ION_FLUXES, $
        tPlt_vars, $
        EBOUNDVARNAME=eBoundVarName, $
        EBOUNDLOWVARNAME=eBoundLowVarName, $
        UPDOWNRATIOSPECVARNAME=upDownRatioSpecVarName, $
        USE_LOSSCONE=use_losscone, $
        PLOT_ASCENDING_NORTH=plot_ascN, $
        PLOT_ASCENDING_SOUTH=plot_ascS, $
        PLOT_DESCENDING_NORTH=plot_descN, $
        PLOT_DESCENDING_SOUTH=plot_descS, $
        TLIMASCENDN=tLimNAscend, $
        TLIMDESCENDN=tLimNDescend, $
        TLIMS=tLimS, $
        TLIMASCENDS=tLimSAscend, $
        TLIMDESCENDS=tLimSDescend, $
        FNAMENASCEND=fNameNAscend, $
        FNAMENDESCEND=fNameNDescend, $
        FNAMESASCEND=fNameSAscend, $
        FNAMESDESCEND=fNameSDescend, $
        SAVETSTRN=saveTStrN, $
        SAVETSTRS=saveTStrS, $
        SAVETSTRASCENDN=saveTStrNAscend, $
        SAVETSTRDESCENDN=saveTStrNDescend, $
        SAVETSTRASCENDS=saveTStrSAscend, $
        SAVETSTRDESCENDS=saveTStrSDescend, $
        SAVE_PS=save_ps, $
        MAKE_SPECIAL_JGR_PLOT=make_special_JGR_plot, $
        SAVEPREF=savePref
  ENDIF
  
END
