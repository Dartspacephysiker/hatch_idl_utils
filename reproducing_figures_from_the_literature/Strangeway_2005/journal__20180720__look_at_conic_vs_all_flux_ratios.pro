;2018/07/20
PRO JOURNAL__20180720__LOOK_AT_CONIC_VS_ALL_FLUX_RATIOS

  COMPILE_OPT IDL2,STRICTARRSUBS

  remake_file = 1

  loadDir = "/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/"

  ieb_or_ies = "ies"
  enforce_diff_eFlux_sRate = 2.5
  calc_geom_factors      = 0
  clean_the_McFadden_way = 0
  deFlux__array_of_structs = 1

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
  savePref = "orb_" + STRING(FORMAT='(I0)',orbit)+"-conic_vs_flux_ratios-"
  saveSuff = ".sav"

  get_data,'ILAT',data=data
  get_data,'MLT',data=MLT

  if (n_elements(data.y) LE 0) then return

  northI = where(data.y GT 10,nn)
  ;; northI = where(data.y GT 10 AND (mlt.y GE 9 AND MLT.y LE 15),nn)
  nNorth = nn
  if (nn gt 0) then BEGIN
     tlimitN=[data.x[northI[0]],data.x[northI[nn-1L]]]
     tlimitNStr=T2S(tlimitN)
     saveTStrN = STRMID(tlimitNStr[0],0,10)                      + "__" $
                 + (STRMID(tlimitNStr[0],11,8)).Replace(":","_") + "-"  $
                 + (STRMID(tlimitNStr[1],11,8)).Replace(":","_")
     fNameN = savePref+saveTStrN+saveSuff
     PRINT,fNameN
  ENDIF

  southI = where(data.y lt -10,nn)
  nSouth = nn
  if (nn gt 0) then BEGIN
     tlimitSouth=[data.x[southI[0]],data.x[southI[nn-1L]]]
     tlimitSouth=T2S(tlimitSouth)
     saveTStrS = STRMID(tlimitSouthStr[0],0,10)                      + "__" $
                 + (STRMID(tlimitSouthStr[0],11,8)).Replace(":","_") + "-"  $
                 + (STRMID(tlimitSouthStr[1],11,8)).Replace(":","_")
     fNameS = savePref+saveTStrS+saveSuff

     PRINT,fNameS
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

     varName = upVarNameN
     STORE_DATA,varName,DATA=eSpecUpN

     varName = downVarNameN
     STORE_DATA,varName,DATA=eSpecDownN

     varName = allAngleVarNameN
     STORE_DATA,varName,DATA=eSpecN

  ENDIF ELSE BEGIN

     t1 = tLimitN[0]
     t2 = tLimitN[1]
     ;; t1 = S2T('1998-09-24/23:57:58')
     ;; t2 = S2T('1998-09-25/00:15:00')

     t1N = tLimitN[0]
     t2N = tLimitN[1]

     t1S = tLimitN[0]
     t2S = tLimitN[1]

     GET_DIFF_EFLUX,T1=t1,T2=t2, $
                    EEB_OR_EES=ieb_or_ies, $
                    ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                    CLEAN_THE_MCFADDEN_WAY=clean_the_McFadden_way, $
                    CALC_GEOM_FACTORS=calc_geom_factors, $
                    ARRAY_OF_STRUCTS_INSTEAD=deFlux__array_of_structs, $
                    LOAD_DIR=loadDir, $
                    OUT_DIFF_EFLUX=diff_eflux

     ;; See the issue
     ;; this32 = make_sdt_struct_from_prepped_eflux(diff_eFlux,21,  $
     ;;                                             /IS_MCFADDEN_DIFF_EFLUX)
     ;; this64 = make_sdt_struct_from_prepped_eflux(diff_eFlux,22, $
     ;;                                             /IS_MCFADDEN_DIFF_EFLUX)
     ;; contour2d,this32,/POLAR,UNITS="EFLUX",/LABEL
     ;; contour2d,this64,/POLAR,UNITS="EFLUX",/LABEL
     ;; !P.multi = [0,2,1]


     GET_FA_ORBIT,diff_eflux.time,/TIME_ARRAY

     IF nNorth GT 0 THEN BEGIN
        
        ;; conic_aRangeN = [145,215]
        ;; noConic_aRangeN = [215,145]

        conic_aRangeN = [90,270]
        noConic_aRangeN = [270,90]

        GET_FA_RATIO_OF_ION_SPECTROGRAMS, $
           T1=tlimitN[0], $
           T2=tlimitN[1], $
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

        ;; eSpecNorthConic = GET_EN_SPEC__FROM_DIFF_EFLUX( $
        ;;                   ;; diff_eFlux[northI], $
        ;;                   diff_eFlux, $
        ;;                   T1=t1N, $
        ;;                   T2=t2N, $
        ;;                   /RETRACE, $
        ;;                   ANGLE=conic_aRangeN, $
        ;;                   UNITS=eSpecUnits, $
        ;;                   OUT_AVGFACTORARR=avgFactorArr, $
        ;;                   OUT_NORMARR=normArr, $
        ;;                   IS_MCFADDEN_DIFF_EFLUX=clean_the_McFadden_way)
        ;; eSpecNorthNoConic = GET_EN_SPEC__FROM_DIFF_EFLUX( $
        ;;                     ;; diff_eFlux[northI], $
        ;;                     diff_eFlux, $
        ;;                     T1=t1N, $
        ;;                     T2=t2N, $
        ;;                     /RETRACE, $
        ;;                     ANGLE=noConic_aRangeN, $
        ;;                     UNITS=eSpecUnits, $
        ;;                     OUT_AVGFACTORARR=avgFactorArr, $
        ;;                     OUT_NORMARR=normArr, $
        ;;                     IS_MCFADDEN_DIFF_EFLUX=clean_the_McFadden_way)

        ;; eSpecNorth = GET_EN_SPEC__FROM_DIFF_EFLUX( $
        ;;              ;; diff_eFlux[northI], $
        ;;              diff_eFlux, $
        ;;              T1=t1N, $
        ;;              T2=t2N, $
        ;;              /RETRACE, $
        ;;              ;; ANGLE=conic_aRangeN, $
        ;;              UNITS=eSpecUnits, $
        ;;              OUT_AVGFACTORARR=avgFactorArr, $
        ;;              OUT_NORMARR=normArr, $
        ;;              IS_MCFADDEN_DIFF_EFLUX=clean_the_McFadden_way)

        PRINT,"Saving " + fNameN + " ..."
        SAVE,diff_eFlux, $
             eSpecN,eSpecUpN,eSpecDownN,upDownRatioSpecN,upAllRatioSpecN, $
             eSpecS,eSpecUpS,eSpecDownS,upDownRatioSpecS,upAllRatioSpecS, $
             up_aRangeS,down_aRangeS, $
             up_aRangeN,down_aRangeN, $
             FILENAME=loadDir+fNameN

     ENDIF

     IF nSouth GT 0 THEN BEGIN
        
        STOP

        ;; conic_aRangeS = [-35,35]
        ;; noConic_aRangeS = [35,325]

        conic_aRangeS = [-90,90]
        noConic_aRangeS = [90,270]

        eSpecSouthConic = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                          ;; diff_eFlux[southI], $
                          diff_eFlux, $
                          T1=t1S, $
                          T2=t2S, $
                          /RETRACE, $
                          ANGLE=conic_aRangeS, $
                          UNITS=eSpecUnits, $
                          OUT_AVGFACTORARR=avgFactorArr, $
                          OUT_NORMARR=normArr, $
                          IS_MCFADDEN_DIFF_EFLUX=clean_the_McFadden_way)
        eSpecSouthNoConic = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                            ;; diff_eFlux[southI], $
                            diff_eFlux, $
                            T1=t1S, $
                            T2=t2S, $
                            /RETRACE, $
                            ANGLE=noConic_aRangeS, $
                            UNITS=eSpecUnits, $
                            OUT_AVGFACTORARR=avgFactorArr, $
                            OUT_NORMARR=normArr, $
                            IS_MCFADDEN_DIFF_EFLUX=clean_the_McFadden_way)

        eSpecSouth = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                     ;; diff_eFlux[southI], $
                     diff_eFlux, $
                     T1=t1S, $
                     T2=t2S, $
                     /RETRACE, $
                     UNITS=eSpecUnits, $
                     OUT_AVGFACTORARR=avgFactorArr, $
                     OUT_NORMARR=normArr, $
                     IS_MCFADDEN_DIFF_EFLUX=clean_the_McFadden_way)

     ENDIF
     
  ENDELSE

  IDENTIFY_ION_UPFLOW_ENERGY_BOUNDARY, $
     DOWNESPEC=eSpecDownN, $
     UPESPEC=eSpecUpN, $
     ALLANGLEESPEC=eSpecN, $
     UPDOWNRATIOSPEC=upDownRatioSpecN, $
     OUT_EBOUND=eBound

  varName = upVarNameN
  OPTIONS,varName,'spec',1	
  ZLIM,varName,1e4,1e8,1
  ylim,varName,3,40000,1
  options,varName,'ytitle','Upward ions!C!CEnergy (eV)'
  options,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
  options,varName,'x_no_interp',1
  options,varName,'y_no_interp',1
  options,varName,'panel_size',2
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  varName = downVarNameN
  options,varName,'spec',1	
  zlim,varName,1e4,1e8,1
  ylim,varName,3,40000,1
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

  IF ~KEYWORD_SET(save_ps) THEN BEGIN

     wInd = 0
     WINDOW,wInd,XSIZE=1200,YSIZE=600

  ENDIF ELSE BEGIN

     oldSize = !P.CHARSIZE
     oldSymSize = !P.SYMSIZE

     !P.CHARSIZE = 3.4
     !P.SYMSIZE  = 2.0

     psNavnN = fNameN.Replace(".sav","")

     IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/outflow_summaries'
     ENDIF

     count = 0
     WHILE FILE_TEST(plotDir+psNavnN+(KEYWORD_SET(eps) ? '.eps' : '.ps')) DO BEGIN
        count++
        psNavnN = STRING(FORMAT='(A0,A0,"-",I02)', $
                         savePref, $
                         saveTStrN, $
                         count)
     ENDWHILE
     
     POPEN,plotDir+psNavnN, $
           ;; /LAND, $
           /PORT, $
           ;; ASPECT=0.625, $
           FONT=-1, $
           ENCAPSULATED=eps     ;,XSIZE=8,YSIZE=7
     DEVICE,/PALATINO,FONT_SIZE=3

  ENDELSE

  ctNum            = 43
  LOADCT2,ctNum

  TPLOT,REVERSE(tPlt_vars),VAR=['ALT','ILAT','MLT'], $
        WINDOW=wInd;; , $
        ;; TRANGE=[t1,t2]

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

  STOP

END
