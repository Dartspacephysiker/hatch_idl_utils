;2018/07/20
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
     SAVETSTRN=saveTStrN, $
     SAVETSTRS=saveTStrS, $
     SAVETSTRASCENDN=saveTStrNAscend, $
     SAVETSTRDESCENDN=saveTStrNDescend, $
     SAVETSTRASCENDS=saveTStrSAscend, $
     SAVETSTRDESCENDS=saveTStrSDescend

  saveTStrN        = !NULL
  saveTStrS        = !NULL
  saveTStrNAscend  = !NULL
  saveTStrNDescend = !NULL
  saveTStrSAscend  = !NULL
  saveTStrSDescend = !NULL

  diffILAT = ilat.y[1:-1]-ilat.y[0:-2]

  ;; Make sure monotonic
  CHECK_SORTED,ilat.x,is_sorted
  IF ~is_sorted THEN STOP

  northI = where(ilat.y GT 10,nN)
  ;; northI = where(ilat.y GT 10 AND (mlt.y GE 9 AND MLT.y LE 15),nn)
  IF (nN GT 0) then BEGIN
     tLimN=[ilat.x[northI[0]],ilat.x[northI[-1]]]
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

  southI = where(ilat.y LT -10,nS)
  if (nS GT 0) THEN BEGIN
     tLimSouth=[ilat.x[southI[0]],ilat.x[southI[-1]]]
     tLimSouth=T2S(tLimSouth)
     saveTStrS = STRMID(tLimSouthStr[0],0,10)                      + "__" $
                 + (STRMID(tLimSouthStr[0],11,8)).Replace(":","_") + "-"  $
                 + (STRMID(tLimSouthStr[1],11,8)).Replace(":","_")

     sAscendII = WHERE(diffILAT[southI] GE 0,nSAscend, $
                       COMPLEMENT=sDescendII, $
                       NCOMPLEMENT=nSDescend)
     IF (nSAscend GT 0) THEN BEGIN
        sAscendI = southI[sAscendII]
        tLimSAscend=[ilat.x[sAscendI[0]],ilat.x[sAscendI[-1]]]
        tLimSAscendStr=T2S(tLimSAscend)
        saveTStrNAscend = STRMID(tLimSAscendStr[0],0,10)                + "__" $
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

PRO TPLOT_UP_VS_DOWN_ION_FLUXES, $
   tPlt_vars, $
   EBOUNDVARNAME=eBoundVarName, $
   UPDOWNRATIOSPECVARNAME=upDownRatioSpecVarName, $
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

PRO JOURNAL__20180720__LOOK_AT_CONIC_VS_ALL_FLUX_RATIOS, $
   UPDOWNMINRATIO=upDownMinRatio, $
   MINNUMQUALIFYINGECHANNELS=minNumQualifyingEChannels, $
   SAVE_PS=save_ps, $
   NO_PLOTS=no_plots, $
   QUIT_IF_FILE_EXISTS=quit_if_file_exists, $
   ESPECN=eSpecN, $
   ESPECUPN=eSpecUpN, $
   ESPECDOWNN=eSpecDownN, $
   UPDOWNRATIOSPECN=upDownRatioSpecN, $
   UPALLRATIOSPECN=upAllRatioSpecN, $
   EBOUND=eBound, $
   IONMOMSTRUCT=ionMomStruct, $
   IONUPJ=ionUpJ, $
   UP_ARANGEN=up_aRangeN, $
   DOWN_ARANGEN=down_aRangeN

  COMPILE_OPT IDL2,STRICTARRSUBS

  remake_file = 1

  loadDir = "/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/"

  ieb_or_ies = "ies"
  enforce_diff_eFlux_sRate = 2.5
  calc_geom_factors      = 0
  clean_the_McFadden_way = 0

  deFlux__array_of_structs  = 1
  save_diff_eFlux_to_file   = 0
  load_diff_eFlux_from_file = 1

  plot_ascN = 1
  plot_descN = 1

  makeZeroThreshEFlux = 1e6
  makeZeroVal = 0.001

  up_aRangeN = [90,270]
  down_aRangeN = [270,90]
  calib = 1

  tRange = GET_ESA_TIMERANGES__RASKT(/IONS,OUT_TIME_ARRAY=times)

  GET_FA_ORBIT,times,/TIME_ARRAY

  nHere = N_ELEMENTS(times)
  GET_DATA,"ORBIT",DATA=orbit
  orbit = orbit.y[nHere/2]

  ;; upDownRatioStr = ''
  upDownMinRatio = KEYWORD_SET(upDownMinRatio) ? upDownMinRatio : 10
  minNumQualifyingEChannels = KEYWORD_SET(minNumQualifyingEChannels) ? minNumQualifyingEChannels : 10
  IF KEYWORD_SET(upDownMinRatio) THEN BEGIN
     upDownRatioStr = STRING(FORMAT='("-upDownRatio_",I0)',upDownMinRatio)
  ENDIF
  IF KEYWORD_SET(minNumQualifyingEChannels) THEN BEGIN
     minNQualEStr = STRING(FORMAT='("-minNQualECh_",I0)',minNumQualifyingEChannels)
  ENDIF

  savePref = "orb_" + STRING(FORMAT='(I0)',orbit)+"-conic_vs_flux_ratios"$
             +upDownRatioStr+minNQualEStr
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
     LOADDIR=loadDir

  GET_DIFF_EFLUX,T1=times[0],T2=times[-1], $
                 EEB_OR_EES=ieb_or_ies, $
                 ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                 CLEAN_THE_MCFADDEN_WAY=clean_the_McFadden_way, $
                 CALC_GEOM_FACTORS=calc_geom_factors, $
                 ARRAY_OF_STRUCTS_INSTEAD=deFlux__array_of_structs, $
                 SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                 OVERWRITE_EXISTING=overwrite_existing, $
                 DIFF_EFLUX_FILE=diff_eFlux_file, $
                 LOAD_DAT_FROM_FILE=load_diff_eFlux_from_file, $
                 LOAD_DIR=loadDir, $
                 OUT_DIFF_EFLUX=diff_eflux


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
     NASCENDS=nSAscend, $
     NDESCENDS=nSDescend, $
     SAVETSTRN=saveTStrN, $
     SAVETSTRS=saveTStrS, $
     SAVETSTRASCENDN=saveTStrNAscend, $
     SAVETSTRDESCENDN=saveTStrNDescend, $
     SAVETSTRASCENDS=saveTStrSAscend, $
     SAVETSTRDESCENDS=saveTStrSDescend

  IF N_ELEMENTS(saveTStrN) GT 0 THEN BEGIN
     ;; fNameN = savePref+saveTStrN+saveSuff
     fNameN = savePref+saveSuff
     PRINT,fNameN
  ENDIF
  IF N_ELEMENTS(saveTStrNAscend) GT 0 THEN BEGIN
     fNameNAscend = savePref+saveTStrNAscend+saveSuff
     PRINT,fNameNAscend
  ENDIF
  IF N_ELEMENTS(saveTStrNDescend) GT 0 THEN BEGIN
     fNameNDescend = savePref+saveTStrNDescend+saveSuff
     PRINT,fNameNDescend
  ENDIF
  IF N_ELEMENTS(saveTStrS) GT 0 THEN BEGIN
     fNameS = savePref+saveTStrS+saveSuff
     PRINT,fNameS
  ENDIF
  IF N_ELEMENTS(saveTStrSAscend) GT 0 THEN BEGIN
     fNameSAscend = savePref+saveTStrSAscend+saveSuff
     PRINT,fNameSAscend
  ENDIF
  IF N_ELEMENTS(saveTStrSDescend) GT 0 THEN BEGIN
     fNameSDescend = savePref+saveTStrSDescend+saveSuff
     PRINT,fNameSDescend
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

  upVarNameN = 'eSpecUpN'
  downVarNameN = 'eSpecDownN'
  allAngleVarNameN = 'eSpecN'

  IF FILE_TEST(loadDir+fNameN) AND ~KEYWORD_SET(remake_file) THEN BEGIN
     PRINT,"Restoring " + fNameN + "..."
     RESTORE,loadDir+fNameN

     IF KEYWORD_SET(quit_if_file_exists) THEN BEGIN
        IF N_ELEMENTS(ionMomStruct) GT 0 AND N_ELEMENTS(eBound) GT 0 AND N_ELEMENTS(ionupJ) GT 0 THEN RETURN
     ENDIF

     varName = upVarNameN
     STORE_DATA,varName,DATA=eSpecUpN

     varName = downVarNameN
     STORE_DATA,varName,DATA=eSpecDownN

     varName = allAngleVarNameN
     STORE_DATA,varName,DATA=eSpecN

  ENDIF ELSE BEGIN

     t1 = tLimN[0]
     t2 = tLimN[1]

     t1N = tLimN[0]
     t2N = tLimN[1]

     t1S = tLimN[0]
     t2S = tLimN[1]

     GET_FA_ORBIT,diff_eflux.time,/TIME_ARRAY

     ;; Now need to treat Northern and Southern separately, o' course, since the angle ranges are totally flipped
     IF nN GT 0 THEN BEGIN

        ;; conic_aRangeN = [145,215]
        ;; noConic_aRangeN = [215,145]

        conic_aRangeN = [90,270]
        noConic_aRangeN = [270,90]

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
           /USE_DIFF_EFLUX, $
           DIFF_EFLUX=diff_eFlux, $
           IS_MCFADDEN_DIFF_EFLUX=deFlux__array_of_structs

        PRINT,"Saving " + fNameN + " ..."
        SAVE, $
             eSpecN,eSpecUpN,eSpecDownN,upDownRatioSpecN,upAllRatioSpecN, $
             eSpecS,eSpecUpS,eSpecDownS,upDownRatioSpecS,upAllRatioSpecS, $
             up_aRangeS,down_aRangeS, $
             up_aRangeN,down_aRangeN, $
             FILENAME=loadDir+fNameN

     ENDIF

     IF nS GT 0 THEN BEGIN

        STOP

        ;; conic_aRangeS = [-35,35]
        ;; noConic_aRangeS = [35,325]

        conic_aRangeS = [-90,90]
        noConic_aRangeS = [90,270]

     ENDIF

  ENDELSE

  IDENTIFY_ION_UPFLOW_ENERGY_BOUNDARY, $
     UPDOWNMINRATIO=upDownMinRatio, $
     MINNUMQUALIFYINGECHANNELS=minNumQualifyingEChannels, $
     DOWNESPEC=eSpecDownN, $
     UPESPEC=eSpecUpN, $
     ALLANGLEESPEC=eSpecN, $
     UPDOWNRATIOSPEC=upDownRatioSpecN, $
     OUT_EBOUND=eBound

  ;; SSDDD
  ;; Moments???
  energy = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX( $
           diff_eFlux, $
           ENERGY=[4,100], $
           SC_POT=tmpSc_pot, $
           EEB_OR_EES=ieb_or_ies)

  ;; IF KEYWORD_SET(use_peakE_bounds_for_moments) THEN BEGIN
     ;; energy[0,*]               = peak_EBoundsArr[0,*] > energy[0,*]
     energy[1,*]               = eBound.y
  ;; ENDIF

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
                  BATCH_MODE=batch_mode

  upflow_i = WHERE(diff_eFlux.valid AND eBound.y GT 4,nUpflow, $
                   COMPLEMENT=notUpflow_i, $
                   NCOMPLEMENT=nNotUpflow)

  ionupJ = {x  : eBound.x, $
            y  :  ionMomStruct.j}
  ionupJ.y[notUpflow_i] = !VALUES.F_NaN
  ionupJ.y[where(FINITE(ionupj.y),/NULL)] = -1. * ionupJ.y[where(FINITE(ionupj.y),/NULL)]

  ylim,varName,6e6,6e9,1
  
  PRINT,"Saving " + fNameN + " ..."
  SAVE, $
     eSpecN,eSpecUpN,eSpecDownN,upDownRatioSpecN,upAllRatioSpecN, $
     eBound, ionMomStruct, ionupJ, $
     eSpecS,eSpecUpS,eSpecDownS,upDownRatioSpecS,upAllRatioSpecS, $
     up_aRangeS,down_aRangeS, $
     up_aRangeN,down_aRangeN, $
     FILENAME=loadDir+fNameN

  varName = upVarNameN
  OPTIONS,varName,'spec',1
  ZLIM,varName,1e4,1e8,1
  ylim,varName,4,24000,1
  options,varName,'ytitle','Upward ions!C!CEnergy (eV)'
  options,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
  options,varName,'x_no_interp',1
  options,varName,'y_no_interp',1
  options,varName,'panel_size',2
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  varName = downVarNameN
  options,varName,'spec',1
  zlim,varName,1e4,1e8,1
  ylim,varName,4,24000,1
  options,varName,'ytitle','Downward ions !C!CEnergy (eV)'
  options,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
  options,varName,'x_no_interp',1
  options,varName,'y_no_interp',1
  options,varName,'panel_size',2
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  ;; varName = allAngleVarNameN
  ;; options,varName,'spec',1
  ;; zlim,varName,1e4,1e8,1
  ;; ylim,varName,3,40000,1
  ;; options,varName,'ytitle','ions (all angles)!C!CEnergy (eV)'
  ;; options,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
  ;; options,varName,'x_no_interp',1
  ;; options,varName,'y_no_interp',1
  ;; options,varName,'panel_size',2
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
  data = upDownRatioSpecN
  STORE_DATA,varName,DATA=data
  OPTIONS,varName,'spec',1
  YLIM,varName,4,24000,1
  ZLIM,varName,0.1,1000,1
  OPTIONS,varName,'ytitle',"Ion energy (eV)"
  OPTIONS,varName,'ztitle','Up/Down ion eFlux'
  OPTIONS,varName,'x_no_interp',1
  OPTIONS,varName,'y_no_interp',1
  options,varName,'panel_size',2

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

  varName = "ion_upJ"
  STORE_DATA,varName,DATA=ionupJ
  options,varName,'ytitle','Ion flux'
  options,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
  options,varName,'x_no_interp',1
  options,varName,'y_no_interp',1
  options,varName,'panel_size',1
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]
  
  IF ~KEYWORD_SET(no_plots) THEN BEGIN
     TPLOT_UP_VS_DOWN_ION_FLUXES, $
        tPlt_vars, $
        EBOUNDVARNAME=eBoundVarName, $
        UPDOWNRATIOSPECVARNAME=upDownRatioSpecVarName, $
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
        SAVEPREF=savePref
  ENDIF
  
END
