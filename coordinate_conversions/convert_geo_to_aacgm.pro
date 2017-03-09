;;10/10/16
PRO CONVERT_GEO_TO_AACGM, $
   COORDFILES=coordFiles, $
   GEICOORDFILES=GEICoordFiles, $
   COORDDIR=coordDir, $
   TMPFILES=tmpFiles, $
   TIMEFILES=timeFiles, $
   EPHEMFILEINDARR=ephemFileIndArr, $
   OUTDIR=outDir, $
   OUTFILES=outFiles, $
   ORIG_ROUTINENAME=orig_routineName, $
   R_E=R_E, $
   ALTITUDE_MAX=altitude_max, $
   ALLOW_FL_TRACE=allow_FL_trace, $
   CHECK_IF_EXISTS=check_IF_exists, $
   CREATE_NOTALTITUDE_FILE=create_notAltitude_file, $
   NOTALTITUDE_SUFF=notAltitude_suff, $
   CONVERT_VARNAMES_AND_RESAVE_OUTFILES=convert_varNames_and_resave_outFiles, $
   FORCE_NEWCHECKITVL=force_newCheckItvl, $
   USER__RESTRICT_II=user__restrict_ii, $
   IN_NAMES=in_names, $
   DEFNAMES=defNames

  COMPILE_OPT IDL2,STRICTARRSUBS

  TIC
  clock = TIC('warnMe')
  FOR i=0,N_ELEMENTS(coordFiles)-1 DO BEGIN

     ;;Convert these var names to standard names
     GEOSphName        =   in_names.GEOSph     
     AACGMSphName      =   in_names.AACGMSph   
     GEOStructName     =   in_names.GEOStruct  
     AACGMStructName   =   in_names.AACGMStruct
     coordStructName   =   in_names.coordStruct
     timeStrName       =   in_names.timeStr    
     DBIndName         =   in_names.DBInd

     timeTmp = GET_TIMES_AND_DECIDE_ALTITUDE_OR_NOT_ALTITUDE(ephemFileIndArr,DBInds,i, $
                                                             ALTITUDE_MAX=altitude_max, $
                                                             R_E=R_E, $
                                                             ALLOW_FL_TRACE=allow_fl_trace, $
                                                             COORDFILES=coordFiles, $
                                                             GEICOORDFILES=GEICoordFiles, $
                                                             COORDDIR=coordDir, $
                                                             CREATE_NOTALTITUDE_FILE=create_notAltitude_file, $
                                                             NOTALTITUDE_SUFF=notAltitude_suff, $
                                                             OUTFILES=outFiles, $
                                                             TMPFILES=tmpFiles, $
                                                             TIMEFILES=timeFiles, $
                                                             RESTRICT_II=restrict_ii, $
                                                             NOTRESTRICT_II=notRestrict_ii, $
                                                             USER__RESTRICT_II=user__restrict_ii, $
                                                             DOEM_II=doEm_ii, $
                                                             NAME__GEOSTRUCT=GEOStructName, $
                                                             NAME__COORDSTRUCT=coordStructName, $
                                                             GEOSTRUCT=geoStruct)

     ;;Check if we've already got it
     IF KEYWORD_SET(check_if_exists) THEN BEGIN
        IF KEYWORD_SET(convert_varnames_and_resave_outFiles) THEN BEGIN
           CONVERT_VARNAMES_AND_RESAVE_OUTFILES,outDir+outFiles[i], $
                                                AACGMSphName,defNames.AACGMSph, $
                                                AACGMStructName,defNames.AACGMStruct, $
                                                defNames.restrictVar,defNames.restrictVar, $
                                                DBIndName,defNames.DBIndName
        ENDIF
        IF CHECK_EXISTS_AND_COMPLETENESS(outDir,outFiles,timeTmp,i,NAME__AACGMSTRUCT=AACGMStructName) THEN BEGIN
           CONTINUE
        ENDIF
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Some need-to-knowables
     nTot = N_ELEMENTS(timeTmp)
     MAKE_GEO_AND_AACGM_SPHCOORD_ARRAYS,timeTmp,nTot,GEOSph,AACGMSph,doEm_ii, $
                                        GEOSTRUCT=geoStruct, $
                                        /DESTROY_GEOSTRUCT
                                        ;; DESTROY_GEOSTRUCT=destroy_geoStruct


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Times in CDF epoch time
     IF CHECK_NEED_TO_RECALC_TSTAMPS(timeFiles[i],timeTmp,nTot, $
                                     ALTITUDE_MAX=altitude_max, $
                                     R_E=R_E, $
                                     ALLOW_FL_TRACE=allow_fl_trace, $
                                     OUT_TIMETMPSTR=timeTmpStr, $
                                     TIMESTRNAME=timeStrName, $
                                     CONVERT_VARNAMES_AND_RESAVE_OUTFILES=convert_varnames_and_resave_outFiles) $
     THEN BEGIN
        timeTmpStr = CREATE_FASTDB_TSTAMPS(timeTmp,nTot,timeFiles[i], $
                                    ALTITUDE_MAX=altitude_max, $
                                    R_E=R_E, $
                                    ALLOW_FL_TRACE=allow_fl_trace)
     ENDIF

     CHOP_UTC_STRINGS_INTO_YMD_HMS,timeTmpStr,year,month,day,hour,min,sec

     PRINT,"Feeding it to AACGM ..."
     IF FILE_TEST(tmpFiles[i]) THEN BEGIN
        PRINT,"Restoring " + tmpFiles[i] + ' ...'
        RESTORE,tmpFiles[i]
        PRINT,'Restored params'
        PRINT,'==============='
        PRINT,"nGotEm        : " + STRCOMPRESS(nGotEm,/REMOVE_ALL)
        PRINT,"lastCheck     : " + STRCOMPRESS(lastCheck,/REMOVE_ALL)
        PRINT,"checkInterval : " + STRCOMPRESS(checkInterval,/REMOVE_ALL)
        
        IF N_ELEMENTS(eEphem_AACGMSph_arr) GT 0 THEN BEGIN
           AACGMSph = TEMPORARY(eEphem_AACGMSph_arr)
        ENDIF

        IF KEYWORD_SET(force_newCheckItvl) THEN BEGIN
           PRINT,"Forcing new check interval: " + STRCOMPRESS(force_newCheckItvl,/REMOVE_ALL)
           checkInterval = force_newCheckItvl
        ENDIF

        startK        = k

        PRINT,""
        PRINT,"Starting from k = " + STRCOMPRESS(startK,/REMOVE_ALL) + " ..."
     ENDIF ELSE BEGIN
        nGotEm        = 0
        lastCheck     = 0
        checkInterval = 10000
        startK        = 0
     ENDELSE


     TIC
     runCName = "AACGM Clock"
     runC     = TIC(runCName)
     FOR k=startK,nTot-1 DO BEGIN 

        e = AACGM_V2_SETDATETIME(year[k],month[k],day[k], $
                                 hour[k],min[k],sec[k])

        tmpAACGM                  = CNVCOORD_V2(geoSph[*,k], $
                                                ALLOW_TRACE=allow_fl_trace)
        AACGM_MLT                 = MLT_V2(tmpAACGM[1,*])
        AACGMSph[*,k]  = [tmpAACGM,AACGM_MLT]

        nGotEm++
        
        IF nGotEm GE (lastCheck+checkInterval) THEN BEGIN
           PRINT,"N completed : " + STRCOMPRESS(nGotEm,/REMOVE_ALL)
           TOC,runC
           lastCheck += checkInterval

           SAVE,AACGMSph,lastCheck,checkInterval,nGotEm,k,FILENAME=tmpFiles[i]
        ENDIF

     ENDFOR
     TOC

     AACGMSph[2,*]     = (AACGMSph[2,*]*R_E-R_E) ;convert back to altitude above sea level

     AACGMStruct   = {ALT:REFORM(AACGMSph[2,*]), $
                      MLT:REFORM(AACGMSph[3,*]), $
                      LAT:REFORM(AACGMSph[0,*])}

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Save it
     PRINT,'Saving ' + outDir + outFiles[i] + '...'
     SAVE,AACGMStruct,AACGMSph,restrict_ii,notRestrict_ii,DBInds,FILENAME=outDir+outFiles[i]

     PRINT,"Did it! Finished with loop " + STRCOMPRESS(i+1,/REMOVE_ALL) + '/' + $
           STRCOMPRESS(N_ELEMENTS(coordFiles),/REMOVE_ALL)

     TOC

  ENDFOR

END

FUNCTION GET_TIMES_AND_DECIDE_ALTITUDE_OR_NOT_ALTITUDE,ephemFileIndArr,DBInds,i, $
   ALTITUDE_MAX=altitude_max, $
   R_E=R_E, $
   ALLOW_FL_TRACE=allow_fl_trace, $
   COORDFILES=coordFiles, $
   GEICOORDFILES=GEICoordFiles, $
   COORDDIR=coordDir, $
   CREATE_NOTALTITUDE_FILE=create_notAltitude_file, $
   NOTALTITUDE_SUFF=notAltitude_suff, $
   OUTFILES=outFiles, $
   TMPFILES=tmpFiles, $
   TIMEFILES=timeFiles, $
   RESTRICT_II=restrict_ii, $
   NOTRESTRICT_II=notRestrict_ii, $
   USER__RESTRICT_II=user__restrict_ii, $
   DOEM_II=doEm_ii, $
   NAME__GEOSTRUCT=GEOStructName, $
   NAME__COORDSTRUCT=coordStructName, $
   GEOSTRUCT=geoStruct

  ;;Load the stuff we need (has GEO coords)
  PRINT,"Restoring " + coordFiles[i] + ' ...'
  RESTORE,coordDir+coordFiles[i]

  IF N_ELEMENTS(GEICoordFiles) GT 0 THEN BEGIN
     PRINT,'Restoring ' + GEICoordFiles[i] + ' ...'
     RESTORE,coordDir+GEICoordFiles[i]
  ENDIF

  ;;Convert varname, if necessary
  geoString   = 'GEOstruct   = TEMPORARY(' + GEOStructName   + ')'
  coordString = 'coordStruct = TEMPORARY(' + coordStructName + ')'
  IF ~(EXECUTE(geoString) AND EXECUTE(coordString)) THEN BEGIN
     PRINT,"Trouble picking up GEOstruct and coordStruct!"
     STOP
  ENDIF

  IF N_ELEMENTS(user__restrict_ii) GT 0 THEN BEGIN
     PRINT,'User has provided restrict_ii, so that does it!'

     restrict_ii = user__restrict_ii

     doEm_ii     = restrict_ii
     nCheck      = N_ELEMENTS(restrict_ii)


  ENDIF ELSE BEGIN
     restrict_ii  = WHERE(GEOstruct.alt LE altitude_max,nAltitude, $
                          COMPLEMENT=notRestrict_ii,NCOMPLEMENT=nNotAltitude)

     IF KEYWORD_SET(create_notAltitude_file) THEN BEGIN
        PRINT,"Doing 'notAltitude' files ..."
        doEm_ii    = notRestrict_ii
        outFiles  += notAltitude_suff
        tmpFiles  += notAltitude_suff
        timeFiles += notAltitude_suff
        nCheck     = nNotAltitude
     ENDIF ELSE BEGIN
        doEm_ii    = restrict_ii
        nCheck     = nAltitude
     ENDELSE
  ENDELSE
  ;;Get indices into eSpec
  IF KEYWORD_SET(ephemFileIndArr) THEN BEGIN
     inds         = ephemFileIndArr[i,*]
  ENDIF ELSE BEGIN
     inds         = [0,N_ELEMENTS(GEOstruct.alt)-1]
  ENDELSE

  STR_ELEMENT,coordStruct,'TIME',SUCCESS=coordSHasTime
  IF coordSHasTime THEN BEGIN     
     DBInds       = [inds[0]:(inds[1] EQ -1 ? N_ELEMENTS(coordStruct.time)-1 : inds[1])]
  ENDIF ELSE BEGIN
     DBInds       = [inds[0]:(inds[1] EQ -1 ? N_ELEMENTS(coords.time)-1 : inds[1])]
  ENDELSE

  IF nCheck EQ 0 THEN BEGIN
     PRINT,'What?? No indices meeting these qualifications????'
     STOP
  ENDIF

  RETURN,(coordSHasTime ? coordStruct.time : coords.time)[doEm_ii]

END

FUNCTION CHECK_EXISTS_AND_COMPLETENESS,outDir,outFiles,timeTmp,i, $
                                       NAME__AACGMSTRUCT=AACGMStructName

  IF FILE_TEST(outDir+outFiles[i]) THEN BEGIN

     PRINT,"File exists:" + outFiles[i]
     PRINT,"Checking for completeness ..."

     AACGMStruct  = !NULL

     RESTORE,outDir+outFiles[i]

     bro = EXECUTE('do_exec = SIZE(' + AACGMStructName + ',/TYPE)')
     IF bro AND (DO_exec NE 0) THEN BEGIN
        IF ~EXECUTE('AACGMStruct = ' + AACGMStructName) THEN STOP
     ENDIF;;  ELSE BEGIN
     ;;    IF N_ELEMENTS(AACGMStruct) EQ 0 THEN STOP
     ;; ENDELSE

     IF N_ELEMENTS(AACGMStruct) GT 0 THEN BEGIN
        IF N_ELEMENTS(AACGMStruct.alt) EQ N_ELEMENTS(timeTmp) THEN BEGIN
           PRINT,"File already complete:" + outFiles[i]
           PRINT,"Skipping ..."
           RETURN,1
        ENDIF ELSE BEGIN
           PRINT,FORMAT='(A0,I0,"/",I0,A0)', $
                 "So file ISN'T complete yet! Vi har ", $
                 N_ELEMENTS(AACGMStruct.alt), $
                 N_ELEMENTS(timeTmp), $
                 " so far."
        ENDELSE
     ENDIF ELSE BEGIN
        PRINT,"Haven't even started this file, children. Here we go."
     ENDELSE
  ENDIF

  RETURN,0

END

PRO MAKE_GEO_AND_AACGM_SPHCOORD_ARRAYS,timeTmp,nTot,GEOSph,AACGMSph,doEm_ii, $
                                       GEOSTRUCT=GEOstruct, $
                                       DESTROY_GEOSTRUCT=destroy_GEOstruct
  

  GEOSph   = TRANSPOSE([[GEOstruct.lat[doEm_ii]],[GEOstruct.lon[doEm_ii]],[GEOstruct.alt[doEm_ii]]])
  AACGMSph = MAKE_ARRAY(4,nTot,/FLOAT) ;;One more for MLT at end

  IF KEYWORD_SET(destroy_GEOstruct) THEN GEOstruct = !NULL
END

FUNCTION CHECK_NEED_TO_RECALC_TSTAMPS,timeFile,timeTmp,nTot, $
                                      ALTITUDE_MAX=altitude_max, $
                                      R_E=R_E, $
                                      ALLOW_FL_TRACE=allow_fl_trace, $
                                      OUT_TIMETMPSTR=timeTmpStr, $
                                      TIMESTRNAME=timeStrName, $
                                      CONVERT_VARNAMES_AND_RESAVE_OUTFILES=convert_varnames_and_resave_outFiles

  nTot          = N_ELEMENTS(timeTmp)

  defTimeStrName = 'timeTmpStr'

  ;;Do we already have them?
  IF FILE_TEST(timeFile) THEN BEGIN
     RESTORE,timeFile

     IF KEYWORD_SET(convert_varnames_and_resave_outFiles) THEN BEGIN
        IF ~EXECUTE(defTimeStrName + ' = TEMPORARY(' + timeStrName + ')') THEN STOP
        PRINT,"Re-saving "  + timeFile + " ..."
        SAVE,timeTmpStr,savedAltitude_max,savedR_E,savedAllow_fl_trace,FILENAME=timeFile
     ENDIF

     IF N_ELEMENTS(timeTmpStr) NE nTot THEN BEGIN
        PRINT,'Wrong number of elements!! Re-converting time stamps ...'
        recalcTime       = 1
     ENDIF ELSE BEGIN
        hateIt           = 0
        IF savedAltitude_max NE altitude_max THEN BEGIN
           PRINT,"Saved altmax (" + STRCOMPRESS(savedAltitude_max,/REMOVE_ALL) + " doesn't match! Recalculating timestamps..."
           recalcTime    = 1
           hateIt        = 1
        ENDIF
        IF savedR_E NE R_E THEN BEGIN
           PRINT,"Saved R_E (" + STRCOMPRESS(savedR_E,/REMOVE_ALL) + " doesn't match! Recalculating timestamps..."
           recalcTime    = 1
           hateIt        = 1
        ENDIF
        IF savedAllow_fl_trace NE allow_fl_trace THEN BEGIN
           PRINT,"Saved allow_fl_trace (" + STRCOMPRESS(savedAllow_fl_trace,/REMOVE_ALL) + " doesn't match! Recalculating timestamps..."
           recalcTime    = 1
           hateIt        = 1
        ENDIF

        PRINT,'Using already-converted timestamps from ' + timeFile + ' ...'
        ;; PRINT,"Kidding! Continuing ..."
        ;; CONTINUE
        IF ~KEYWORD_SET(hateIt) THEN BEGIN
           recalcTime    = 0 
        ENDIF

     ENDELSE
  ENDIF ELSE BEGIN
     recalcTime          = 1
  ENDELSE

  RETURN,recalcTime
END

PRO CHOP_UTC_STRINGS_INTO_YMD_HMS,timeTmpStr,year,month,day,hour,min,sec

  year   = FIX(STRMID(timeTmpStr,0,4))
  month  = FIX(STRMID(timeTmpStr,5,2))
  day    = FIX(STRMID(timeTmpStr,8,2))
  hour   = FIX(STRMID(timeTmpStr,11,2))
  min    = FIX(STRMID(timeTmpStr,14,2))
  sec    = FLOAT(STRMID(timeTmpStr,17,6))

END