;;12/20/16
;;Example:
;; t1 = '1996-10-06/16:26:02.417'
;; t2 = '1996-10-06/16:49:28.917'
;; GET_FA_ORBIT,t1,t2
;; GET_DATA,'ORBIT',DATA=orbit
;; times = (TEMPORARY(orbit)).x
;; COORDINATE_CONVERSION__PARALLEL,times,/CREATE_TIMESTAMPS,ORIG_ROUTINENAME='Example',COORDDIR='~/Desktop/',OUTFILE_PREF='Example_1996'
PRO FASTDB_COORDINATE_CONVERSION__PARALLEL, $
   times, $
   NCPUSTORUN=nCPUsToRun, $
   STARTCPU=startCPU, $
   STOPCPU=stopCPU, $
   CREATE_TIMESTAMPS=create_timeStamps, $
   GET_GEI_COORDS=get_GEI_coords, $
   DO_GEO_MAG_CONVERSIONS=do_GEO_MAG_conversions, $
   DO_AACGM_CONVERSIONS=do_AACGM_conversions, $
   STITCH_FILES=stitch_files, $
   GET_DIPOLETILT_DATA=get_dipoleTilt_data, $
   ORIG_ROUTINENAME=orig_routineName, $
   COORDFILE_PREF=GEO_MAG_file_pref, $
   GEI_COORD_FILENAME_PREF=GEI_coord_filename_pref, $
   COORDDIR=coordDir, $
   TIMEFILE_PREF=timeFile_pref, $
   EPHEMFILEINDARR=ephemFileIndArr, $
   OUTFILE_PREF=outFile_pref, $
   R_E=R_E, $
   ALTITUDE_MAX=altitude_max, $
   ALLOW_FL_TRACE=allow_FL_trace, $
   CHECK_IF_EXISTS=check_if_exists, $
   CREATE_NOTALTITUDE_FILE=create_notAltitude_file, $
   NOTALTITUDE_SUFF=notAltitude_suff, $
   CONVERT_VARNAMES_AND_RESAVE_OUTFILES=convert_varNames_and_resave_outFiles, $
   FORCE_NEWCHECKITVL=force_newCheckItvl, $
   USER__RESTRICT_II=user__restrict_i, $
   IN_NAMES=in_names, $
   DEFNAMES=defNames, $
   DRY_RUN=dry_run, $
   DIAGNOSTIC=diag, $
   OK__CONTINUE_WITH_ONLY_FEW_CPUS=OK__low_CPU_number

  COMPILE_OPT IDL2,STRICTARRSUBS

  @defaults__fastdb_coordinate_conversion.pro

  routineArr = ['fastdb_coordinate_conversion__single.pro', $
                'create_fastdb_tstamps.pro', $
                'get_fast_gei_coords.pro', $
                'convert_gei_coords_to_geo_and_mag_coords.pro', $
                'convert_geo_to_aacgm.pro', $
                'get_dipoletilt_data.pro']

  proDir     = '~/idl/lib/hatch_idl_utils/coordinate_conversions/'        

  nExec      = KEYWORD_SET(create_timeStamps     ) + KEYWORD_SET(get_GEI_coords      ) + $
               KEYWORD_SET(do_GEO_MAG_conversions) + KEYWORD_SET(do_AACGM_conversions) + $
               KEYWORD_SET(stitch_files)           + KEYWORD_SET(get_dipoleTilt_data )

  CASE nExec OF
     0: BEGIN
        PRINT,"COORDINATE_CONVERSION__PARALLEL"
        PRINT,"========================"
        PRINT,""
        PRINT,"Possibilities: "
        PRINT,"CREATE_TIMESTAMPS"
        PRINT,"GET_GEI_COORDS"        
        PRINT,"DO_GEO_MAG_CONVERSIONS"        
        PRINT,"DO_AACGM_CONVERSIONS"        
        PRINT,"STITCH_FILES"        
        PRINT,"GET_DIPOLETILT_DATA"        
        PRINT,""

        RETURN
     END
     1: BEGIN
        CASE 1 OF
           KEYWORD_SET(create_timeStamps): BEGIN
              execType = '/CREATE_TIMESTAMPS'
           END
           KEYWORD_SET(get_GEI_coords): BEGIN
              execType = '/GET_GEI_COORDS'
           END
           KEYWORD_SET(do_GEO_MAG_conversions): BEGIN
              execType = '/DO_GEO_MAG_CONVERSIONS'
           END
           KEYWORD_SET(do_AACGM_conversions): BEGIN
              execType = '/DO_AACGM_CONVERSIONS'
           END
           KEYWORD_SET(stitch_files): BEGIN
              execType = '/STITCH_FILES'
              dry_run  = 1 ;'cause we don't need to do anything in parallel
           END
           KEYWORD_SET(get_dipoleTilt_data): BEGIN
              execType = '/GET_DIPOLETILT_DATA'
           END
        ENDCASE

        pre = 'OK, here it is: '
        PRINT,pre + execType
              
     END
     ELSE: BEGIN
        
        PRINT,"Too many keywords set!"
        PRINT,"Possibilities: "
        PRINT,"CREATE_TIMESTAMPS"
        PRINT,"GET_GEI_COORDS"        
        PRINT,"DO_GEO_MAG_CONVERSIONS"        
        PRINT,"DO_AACGM_CONVERSIONS"        
        PRINT,"STITCH_FILES"        
        PRINT,"GET_DIPOLETILT_DATA"        
        PRINT,""

     END
  ENDCASE

  nCPUs    = !CPU.HW_NCPU-1
  
  IF nCPUs LT 3 AND ~KEYWORD_SET(OK__low_CPU_number) THEN BEGIN
     PRINT,"So you may not derive much benefit from this. Sure you want to continue?"
     STOP
  ENDIF

  oBridge        = OBJARR(nCPUs)
  IDLChildPref   = '/home/spencerh/Desktop/IDL_child_'
  IDLChildOutput = IDLChildPref + STRCOMPRESS(INDGEN(nCPUs),/REMOVE_ALL) + '.txt'

  IF N_ELEMENTS(outFile_pref) EQ 0 THEN BEGIN
     PRINT,"You must provide a name for the output. Cut with the nonsense."
     RETURN
  ENDIF

  IF N_ELEMENTS(coordDir) EQ 0 THEN BEGIN
     PRINT,"Need a coord dir so I know where to store things ..."
     RETURN
  ENDIF

  IF N_ELEMENTS(orig_routineName) EQ 0 THEN BEGIN
     PRINT,"Need an originating routineName so I know where to point people ..."
     RETURN
  ENDIF

  IF ~KEYWORD_SET(GEI_coord_filename_pref) THEN BEGIN
     GEI_coord_filename_pref = outFile_pref + '-GEI'

     PRINT,"No GEI_coord_filename_pref provided, so I'm setting it to this: '" + GEI_coord_filename_pref + "'"

  ENDIF

  IF ~KEYWORD_SET(GEO_MAG_file_pref) THEN BEGIN
     GEO_MAG_file_pref = outFile_pref + '-GEO_MAG'

     PRINT,"No GEO_MAG_file_pref provided, so I'm setting it to this: '" + GEO_MAG_file_pref + "'"

  ENDIF

  IF ~KEYWORD_SET(timeFile_pref) THEN BEGIN
     timeFile_pref = outFile_pref + '-TIME_STRINGS'

     PRINT,"No timeFile_pref provided, so I'm setting it to this: '" + timeFile_pref + "'"

  ENDIF

  IF ~KEYWORD_SET(dpTilt_file_pref) THEN BEGIN
     dpTilt_file_pref = outFile_pref + '-dpTilt'

     PRINT,"No dpTilt_file_pref provided, so I'm setting it to this: '" + dpTilt_file_pref + "'"

  ENDIF

  ;;Now start the show!
  tmpFiles     = !NULL
  outFiles     = !NULL
  timeFiles    = !NULL
  GEI_files    = !NULL
  GEO_MAGFiles = !NULL
  dpTiltFiles  = !NULL
  indArr       = !NULL
  nTot         = N_ELEMENTS(times)
  divFactor    = nTot/nCPUs+1

  IF ~KEYWORD_SET(nCPUsToRun) THEN nCPUsToRun = nCPUs
  IF ~KEYWORD_SET(startCPU  ) THEN startCPU   = 0
  stopCPU = (N_ELEMENTS(stopCPU) GT 0 ? stopCPU : (startCPU + nCPUsToRun - 1) < (nCPUs - 1))

  PRINT,"FASTDB_COORDINATE_CONVERSION__PARALLEL: " + STRCOMPRESS(nTot,/REMOVE_ALL) + " inds total"
  FOR i=0,nCPUs DO BEGIN

     ind1             = i*divFactor
     ind2             = ( ((i+1)*divFactor - 1) < (nTot - 1) )
     indArr           = [[indArr],[ind1,ind2]]

     indSuff          = STRING(FORMAT='("--",I0,"-",I0)',ind1,ind2)

     tmpFiles         = [tmpFiles        ,STRING(FORMAT='("TMP_",A0,"-",I0,A0)',outFile_pref,i,indSuff)]
     outFiles         = [outFiles        ,STRING(FORMAT='(A0,A0,"-",I0,A0)'    ,outFile_pref,"-AACGM",i,indSuff)]
     timeFiles        = [timeFiles       ,STRING(FORMAT='(A0,"-",I0,A0)'       ,timeFile_pref,i,indSuff)]
     GEI_Files        = [GEI_Files       ,STRING(FORMAT='(A0,"-",I0,A0)'       ,GEI_coord_filename_pref,i,indSuff)]
     GEO_MAGFiles     = [GEO_MAGFiles    ,STRING(FORMAT='(A0,"-",I0,A0)'       ,GEO_MAG_file_pref,i,indSuff)]
     dpTiltFiles      = [dpTiltFiles     ,STRING(FORMAT='(A0,"-",I0,A0)'       ,dpTilt_file_pref,i,indSuff)]

  ENDFOR

  IF indArr[-1] NE (nTot-1) THEN BEGIN
     PRINT,"KALSKJDF"
     STOP
  ENDIF

  ;;Show user before beginning
  PRINT,"Here's what I'm going to do: "
  FOR i=startCPU,stopCPU DO BEGIN
     PRINT,"i           : ",i              
     PRINT,"tmpFile     : ",tmpFiles    [i]
     PRINT,"outFile     : ",outFiles    [i]
     PRINT,"timeFile    : ",timeFiles   [i]
     PRINT,"GEI_File    : ",GEI_files   [i]
     PRINT,"GEO_MAGFile : ",GEO_MAGFiles[i]
     PRINT,"dpTiltFile : ",dpTiltFiles[i]

  ENDFOR

  PRINT,"Look OK?"
  response = ''
  cont     = 0
  WHILE ~cont DO BEGIN
     READ,response

     CASE 1 OF
        STRMATCH(STRUPCASE(response),'Y*'): BEGIN
           cont = 1
        END
        STRMATCH(STRUPCASE(response),'N*'): BEGIN
           cont = 1
           PRINT,"OK, leaving ..."
           RETURN
        END
        STRMATCH(STRUPCASE(response),'STOP*'): BEGIN
           cont = 1
           PRINT,"OK, stopping ..."
           STOP
        END
        ELSE: BEGIN
           PRINT,"No, you need to answer 'yes' or 'no' (or 'stop')"
        END
     ENDCASE
  ENDWHILE


  ;; pathString = '"' + !PATH + '"'
  FOR i=startCPU,stopCPU DO BEGIN

     ind1 = indArr[0,i]
     ind2 = indArr[1,i]

     tmpInds = [ind1:ind2]

     IF ~KEYWORD_SET(quiet) THEN BEGIN
        PRINT,""
        PRINT,'Do dat:'
        PRINT,'coordDir          ',coordDir
        PRINT,'orig_routineName  ',orig_routineName
        PRINT,'check_if_exists   ',KEYWORD_SET(check_if_exists)
        PRINT,'times[[0,-1]      ',times[[tmpInds[0],tmpInds[-1]]]
        PRINT,'timesStr[[0,-1]   ',TIME_TO_STR(times[[tmpInds[0],tmpInds[-1]]],/MSEC)
        PRINT,'tmpFile           ',tmpFiles[i]
        PRINT,'outFile           ',outFiles[i]
        PRINT,'timeFile          ',timeFiles[i]
        PRINT,'GEI_coord_filename',GEI_files[i]
        PRINT,'GEO_MAG_fileName  ',GEO_MAGFiles[i]
        PRINT,'dpTilt_fileName  ',dpTiltFiles[i]
     ENDIF

     ;; CASE 1 OF
        ;; KEYWORD_SET(dry_run): BEGIN
        ;;    PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "] = OBJ_NEW('IDL_IDLBridge') "
        ;;    PRINT,"                                                         "
        ;;    PRINT,'Inds: ' + STRCOMPRESS(ind1,/REMOVE_ALL) + ', ' + STRCOMPRESS(ind2,/REMOVE_ALL)
        ;;    PRINT,";;Set all the vars for this environment                  "
        ;;    PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "]->SetVar, 'coordDir'        ," + $
        ;;          "coordDir          "
        ;;    PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "]->SetVar, 'orig_routineName'," + $
        ;;          "orig_routineName  "
        ;;    PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "]->SetVar, 'times'           ," + $
        ;;          "times             "
        ;;    PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "]->SetVar, 'tmpFile'         ," + $
        ;;          "tmpFiles[" + STRCOMPRESS(i,/REMOVE_ALL) + "]       "
        ;;    PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "]->SetVar, 'outFile'         ," + $
        ;;          "outFiles[" + STRCOMPRESS(i,/REMOVE_ALL) + "]       "
        ;;    PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "]->SetVar, 'timeFile'        ," + $
        ;;          "timeFiles[" + STRCOMPRESS(i,/REMOVE_ALL) + "]       "


     ;; END
     ;; ELSE: BEGIN
     IF ~KEYWORD_SET(dry_run) THEN BEGIN
        ;;Temporary inds
        tmpInds = [ind1:ind2]

        oBridge[i] = OBJ_NEW('IDL_IDLBridge',OUTPUT=IDLChildOutput[i])

        ;;Set all the vars for this environment
        oBridge[i]->SetVar, '!PATH'             ,!PATH
        oBridge[i]->SetVar, 'coordDir'          ,coordDir
        oBridge[i]->SetVar, 'orig_routineName'  ,orig_routineName
        oBridge[i]->SetVar, 'check_if_exists'   ,KEYWORD_SET(check_if_exists)
        oBridge[i]->SetVar, 'times'             ,times[tmpInds]
        oBridge[i]->SetVar, 'tmpFile'           ,tmpFiles[i]
        oBridge[i]->SetVar, 'outFile'           ,outFiles[i]
        oBridge[i]->SetVar, 'timeFile'          ,coordDir+timeFiles[i]
        oBridge[i]->SetVar, 'GEI_coord_filename',GEI_files[i]
        oBridge[i]->SetVar, 'GEO_MAG_fileName'  ,GEO_MAGFiles[i]
        oBridge[i]->SetVar, 'dpTilt_fileName'  ,dpTiltFiles[i]
        oBridge[i]->SetVar, 'R_E'               ,R_E
        oBridge[i]->SetVar, 'altitude_max'      ,altitude_max
        oBridge[i]->SetVar, 'allow_fl_trace'    ,allow_fl_trace

        IF KEYWORD_SET(diag) THEN BEGIN
           tmpcoordDir          = oBridge[i]->GetVar('coordDir'          )
           tmporig_routineName  = oBridge[i]->GetVar('orig_routineName'  )
           tmpCheck_if_exists   = oBridge[i]->GetVar('check_if_exists'   )
           tmptimes             = oBridge[i]->GetVar('times'             )
           tmptmpFiles          = oBridge[i]->GetVar('tmpFile'           )
           tmpoutFiles          = oBridge[i]->GetVar('outFile'           )
           tmptimeFiles         = oBridge[i]->GetVar('timeFile'          )
           tmpGEI_files         = oBridge[i]->GetVar('GEI_coord_filename')
           tmpGEO_MAGFiles      = oBridge[i]->GetVar('GEO_MAG_fileName'  )
           tmpdpTiltFiles      = oBridge[i]->GetVar('dpTilt_fileName'  )

           STOP

           FASTDB_COORDINATE_CONVERSION__SINGLE, $
              tmpTimes, $
              CREATE_TIMESTAMPS=create_timeStamps, $
              GET_GEI_COORDS=get_GEI_coords, $
              DO_GEO_MAG_CONVERSIONS=do_GEO_MAG_conversions, $
              DO_AACGM_CONVERSIONS=do_AACGM_conversions, $
              ;; STITCH_FILES=stitch_files, $
              GET_DIPOLETILT_DATA=get_dipoleTilt_data, $
              R_E=R_E, $
              ALTITUDE_MAX=altitude_max, $
              ALLOW_FL_TRACE=allow_fl_trace, $
              TIMEFILE=tmptimeFiles, $
              TMPFILE=tmptmpFiles, $
              OUTFILE=tmpOutFiles, $
              COORDDIR=tmpCoordDir, $
              COORDFILE=tmpGEO_MAGfiles, $
              GEI_COORD_FILENAME=tmpGEI_files, $
              DPTILT_FILENAME=tmpdpTiltFiles, $
              CHECK_IF_EXISTS=tmpCheck_if_exists, $
              ORIG_ROUTINENAME=tmporig_routineName

        ENDIF

        execStr = 'FASTDB_COORDINATE_CONVERSION__SINGLE,times,' + execType + ',' + $
                  'R_E=R_E,' + $
                  'ALTITUDE_MAX=altitude_max,' + $
                  'ALLOW_FL_TRACE=allow_fl_trace,' + $
                  'TIMEFILE=timeFile,' + $
                  'TMPFILE=tmpFile,' + $
                  'OUTFILE=outFile,' + $
                  'COORDDIR=coordDir,' + $
                  'COORDFILE=GEO_MAG_fileName,' + $
                  'GEI_COORD_FILENAME=GEI_coord_filename,' + $
                  'DPTILT_FILENAME=dpTilt_filename,' + $
                  'CHECK_IF_EXISTS=check_if_exists,' + $
                  'ORIG_ROUTINENAME=orig_routineName'

        FOR ll=0,N_ELEMENTS(routineArr)-1 DO BEGIN
           oBridge[i]->Execute,'.compile ' + proDir + routineArr[ll]
        ENDFOR

        oBridge[i]->Execute,'@/home/spencerh/idl/lib/aacgm/compile_aacgm.pro'

        oBridge[i]->Execute,execStr,/NOWAIT
        
        PRINT,'Started that homey'
     ENDIF
     ;; ENDCASE

  ENDFOR


  IF ~KEYWORD_SET(dry_run) THEN BEGIN

     notdone    = 1
     count      = 0LL
     waiting    = MAKE_ARRAY(N_ELEMENTS(oBridge),VALUE=0B,/BYTE)
     retVal     = MAKE_ARRAY(N_ELEMENTS(oBridge),VALUE=0B,/BYTE)
     retString  = MAKE_ARRAY(N_ELEMENTS(oBridge),/STRING)

     waiting[startCPU:stopCPU] = 1B
     WHILE N_ELEMENTS(WHERE(waiting,/NULL)) GT 0 DO BEGIN
        ;; done = 0

        FOR i=startCPU,stopCPU DO BEGIN
           IF waiting[i] THEN BEGIN
              tmpStatus = oBridge[i]->Status(ERROR=retTmp)

              CASE tmpStatus OF
                 0: BEGIN
                    PRINT,"Processor " + STRCOMPRESS(i,/REMOVE_ALL) + " has finished"
                    waiting[i] = 0B
                 END
                 1: BEGIN
                    PRINT,"Processor " + STRCOMPRESS(i,/REMOVE_ALL) + " is executing:"
                    SPAWN,"tail -n 5 " + IDLChildOutput[i],tmpStat
                    PRINT,tmpStat
                 END
                 2: BEGIN
                    PRINT,"Processor " + STRCOMPRESS(i,/REMOVE_ALL) + " finished!"
                    retString[i] = 'Finished!'
                    waiting[i]   = 0B
                 END
                 3: BEGIN
                    PRINT,"Processor " + STRCOMPRESS(i,/REMOVE_ALL) + ": ERROR!"
                    retString[i] = retTmp
                    waiting[i]   = 0B
                 END
              ENDCASE

           ENDIF

           ;; done = done+tmpStatus
           
        ENDFOR

        WAIT,5.0

        count++
        IF (count MOD 120) EQ 0 THEN BEGIN
           PRINT,"Waiting .."
           PRINT,"nFinished = " + STRCOMPRESS(N_ELEMENTS(WHERE(~waiting,/NULL)),/REMOVE_ALL)
        ENDIF
     ENDWHILE

     FOR i=0,N_ELEMENTS(oBridge)-1 DO BEGIN
        OBJ_DESTROY,oBridge[N_ELEMENTS(oBridge)-1-i]
     ENDFOR

     PRINT,"DONE WITH " + execType

  ENDIF

  IF KEYWORD_SET(stitch_files) THEN BEGIN


     fileType = ''

     have_time_files      = 1B
     have_AACGM_files     = 1B
     have_GEO_files       = 1B
     have_GEI_files       = 1B
     have_dpTilt_files    = 1B
     N_TIME               = 0LL
     N_AACGM              = 0LL
     N_GEO                = 0LL
     N_MAG                = 0LL
     N_GEI                = 0LL
     N_dpTilt             = 0LL
     ;; nTrims           = 0
     PRINT,"Checking to make sure files exist + total number of inds"
     nFiles = N_ELEMENTS(outFiles)-1 ;one at end is meaningless
     FOR k=0,nFiles-1 DO BEGIN
        tmpHTIME           = BYTE(FILE_TEST(coordDir+timeFiles[k]   ))
        tmpHAACGM          = BYTE(FILE_TEST(coordDir+outFiles[k]    ))
        tmpHGEO            = BYTE(FILE_TEST(coordDir+GEO_MAGFiles[k]))
        tmpHGEI            = BYTE(FILE_TEST(coordDir+GEI_files[k]   ))
        tmpHdpTilt         = BYTE(FILE_TEST(coordDir+dpTiltFiles[k]   ))

        have_time_files    = have_TIME_files   AND tmpHTIME
        have_AACGM_files   = have_AACGM_files  AND tmpHAACGM
        have_GEO_files     = have_GEO_files    AND tmpHGEO
        have_GEI_files     = have_GEI_files    AND tmpHGEI
        have_dpTilt_files  = have_dpTilt_files AND tmpHdpTilt

        IF tmpHTIME THEN BEGIN
           RESTORE,coordDir+timeFiles[k]
           IF KEYWORD_SET(trim_one_off) THEN BEGIN
              N_TIME     += N_ELEMENTS(timeTmpStr) - ( (k LT (nFiles - 1)) ? 1 : 0)
           ENDIF ELSE BEGIN
              N_TIME     += N_ELEMENTS(timeTmpStr)
           ENDELSE
           AACGMStruct = !NULL
        ENDIF ELSE BEGIN
           PRINT,"Missing TIME file: " + timeFiles[k]
        ENDELSE

        IF tmpHAACGM THEN BEGIN
           RESTORE,coordDir+outFiles[k]

           IF KEYWORD_SET(trim_one_off) THEN BEGIN
              IF (k LT (nFiles-1)) THEN BEGIN
                 TRIM_OFF_LAST_IND,AACGMStruct,'alt'
                 ;; nTrims++
              ENDIF
           ENDIF

           ;; PRINT,'N_AACGM: ',N_AACGM

           N_AACGM    += N_ELEMENTS(AACGMStruct.alt)
           AACGMStruct = !NULL
           
           ;; PRINT,'N_AACGM: ',N_AACGM
        ENDIF ELSE BEGIN
           PRINT,"Missing AACGM file: " + outFiles[k]
        ENDELSE

        IF tmpHGEO THEN BEGIN
           RESTORE,coordDir+GEO_MAGFiles[k]

           IF KEYWORD_SET(trim_one_off) THEN BEGIN
              IF (k LT (nFiles-1)) THEN BEGIN
                 TRIM_OFF_LAST_IND,GEO,'alt'
                 TRIM_OFF_LAST_IND,MAG,'alt'
              ENDIF
           ENDIF

           N_GEO   += N_ELEMENTS(GEO.alt)
           N_MAG   += N_ELEMENTS(MAG.alt)
           GEO      = !NULL
           MAG      = !NULL
        ENDIF ELSE BEGIN
           PRINT,"Missing GEO_MAG file: " + GEO_MAGFiles[k]
        ENDELSE

        IF tmpHGEI THEN BEGIN
           RESTORE,coordDir+GEI_files[k]

           IF KEYWORD_SET(trim_one_off) THEN BEGIN
              IF (k LT (nFiles-1)) THEN BEGIN
                 TRIM_OFF_LAST_IND,GEIcoords,'alt'
              ENDIF
           ENDIF

           N_GEI    += N_ELEMENTS(GEIcoords.alt)
           GEICoords = !NULL
        ENDIF ELSE BEGIN
           PRINT,"Missing GEI file: " + GEI_files[k]
        ENDELSE

        IF tmpHdpTilt THEN BEGIN
           RESTORE,coordDir+dpTiltFiles[k]

           IF KEYWORD_SET(trim_one_off) THEN BEGIN
              IF (k LT (nFiles-1)) THEN BEGIN
                 TRIM_OFF_LAST_IND,tiltAngle,'alt'
              ENDIF
           ENDIF

           N_dpTilt    += N_ELEMENTS(tiltAngle.angle)
           tiltAngle    = !NULL
        ENDIF ELSE BEGIN
           PRINT,"Missing dpTilt file: " + dpTiltFiles[k]
        ENDELSE

     ENDFOR
     ;; PRINT,'nTrims: ',nTrims

     ;;Find out who we can slap together
     stitchable = !NULL

     IF have_TIME_files AND (N_TIME EQ nTot) THEN BEGIN
        stitchable       = [stitchable,'TIME']
        final_TIMEFile  = outFile_pref + '-TIME.sav'
     ENDIF
     
     IF have_AACGM_files AND (N_AACGM EQ nTot) THEN BEGIN
        stitchable       = [stitchable,'AACGM']
        final_AACGMFile  = outFile_pref + '-AACGM.sav'
     ENDIF
     
     IF have_GEO_files THEN BEGIN
        IF (N_GEO EQ nTot) THEN BEGIN
           stitchable    = [stitchable,'GEO']
           final_GEOFile = outFile_pref + '-GEO.sav'
           final_GEOCartFile = outFile_pref + '-GEO_Cartesian.sav'
        ENDIF

        IF (N_MAG EQ nTot) THEN BEGIN
           stitchable    = [stitchable,'MAG']
           final_MAGFile = outFile_pref + '-MAG.sav'
        ENDIF

     ENDIF
     
     IF have_GEI_files THEN BEGIN
        IF (N_GEI EQ nTot) THEN BEGIN
           stitchable    = [stitchable,'GEI']
           final_GEIFile = GEI_coord_filename_pref + '.sav'
        ENDIF
     ENDIF

     IF have_dpTilt_files THEN BEGIN
        IF (N_dpTilt EQ nTot) THEN BEGIN
           stitchable    = [stitchable,'dpTilt']
           final_dpTiltFile = outFile_pref + '-dpTilt.sav'
        ENDIF
     ENDIF

     IF N_ELEMENTS(stitchable) EQ 0 THEN BEGIN
        PRINT,"No stitchable groups of files! Returning ..."
        RETURN
     ENDIF

     PRINT,FORMAT='("Coordinate systems that are stitchable: ",5(A0,:,", "))',stitchable
     PRINT,'Hvilken? (Type all applicable, separated by space--or type QUIT or STOP)'

     stitch_TIME    = 0B
     stitch_AACGM   = 0B
     stitch_GEO     = 0B
     stitch_MAG     = 0B
     stitch_GEI     = 0B
     stitch_dpTilt  = 0B

     cont = 0
     WHILE ~cont DO BEGIN
        reponse = ''            ;celà
        READ,reponse
        this    = STRSPLIT(STRUPCASE(reponse)," ,",/EXTRACT)

        FOR k=0,N_ELEMENTS(this)-1 DO BEGIN
           CASE this[k] OF
              'TIME': BEGIN
                 IF (WHERE(STRUPCASE(stitchable) EQ 'TIME'))[0] NE -1 THEN BEGIN
                    stitch_TIME = 1B
                 ENDIF ELSE BEGIN
                    PRINT,"Can't stitch TIME!"
                 ENDELSE
              END
              'AACGM': BEGIN
                 IF (WHERE(STRUPCASE(stitchable) EQ 'AACGM'))[0] NE -1 THEN BEGIN
                    stitch_AACGM = 1B
                 ENDIF ELSE BEGIN
                    PRINT,"Can't stitch AACGM!"
                 ENDELSE
              END
              'GEO': BEGIN
                 IF (WHERE(STRUPCASE(stitchable) EQ 'GEO'))[0] NE -1 THEN BEGIN
                    stitch_GEO = 1B
                 ENDIF ELSE BEGIN
                    PRINT,"Can't stitch GEO!"
                 ENDELSE
              END
              'MAG': BEGIN
                 IF (WHERE(STRUPCASE(stitchable) EQ 'MAG'))[0] NE -1 THEN BEGIN
                    stitch_MAG = 1B
                 ENDIF ELSE BEGIN
                    PRINT,"Can't stitch MAG!"
                 ENDELSE
              END
              'GEI': BEGIN
                 IF (WHERE(STRUPCASE(stitchable) EQ 'GEI'))[0] NE -1 THEN BEGIN
                    stitch_GEI = 1B
                 ENDIF ELSE BEGIN
                    PRINT,"Can't stitch GEI!"
                 ENDELSE
              END
              'DPTILT': BEGIN
                 IF (WHERE(STRUPCASE(stitchable) EQ 'DPTILT'))[0] NE -1 THEN BEGIN
                    stitch_dpTilt = 1B
                 ENDIF ELSE BEGIN
                    PRINT,"Can't stitch dpTilt!"
                 ENDELSE
              END
              "QUIT": BEGIN
                 PRINT,"OK, quitting ..."
                 RETURN
              END
              "STOP": BEGIN
                 STOP
              END
              ELSE: BEGIN
                 PRINT,"Unknown option: ",this[k]
                 PRINT,'Try again ...'
                 k = N_ELEMENTS(this)-1
              END
           ENDCASE

        ENDFOR

           nStitch = stitch_TIME + stitch_AACGM + stitch_GEO + stitch_MAG + stitch_GEI + stitch_dpTilt
           IF nStitch EQ 0 THEN BEGIN
              PRINT,"You provided no valid options. Try again."
           ENDIF ELSE BEGIN
              cont = 1B
           ENDELSE

     ENDWHILE

     IF stitch_time THEN BEGIN
        timeStr = MAKE_ARRAY(nTot,/STRING)

        curInd = 0LL

        FOR k=0,nFiles-1 DO BEGIN
           RESTORE,coordDir+timeFiles[k]
           
           nHere   = N_ELEMENTS(timeTmpStr)

           IF KEYWORD_SET(trim_one_off) THEN BEGIN

              CASE k OF
                 nFiles-1: BEGIN
                    tmpInds            = [curInd:(curInd+nHere-1)]
                    timeStr[tmpInds]   = timeTmpStr
                    curInd            += nHere
                 END
                 ELSE: BEGIN
                    tmpInds            = [curInd:(curInd+nHere-2)]
                    timeStr[tmpInds]   = timeTmpStr[0:-2]
                    curInd            += nHere-1
                 END
              ENDCASE

           ENDIF ELSE BEGIN

              tmpInds            = [curInd:(curInd+nHere-1)]
              timeStr[tmpInds]   = timeTmpStr
              curInd            += nHere

           ENDELSE

           timeTmpStr = !NULL

        ENDFOR

        PRINT,"Saving stitched TIME file to " + final_TIMEFile
        SAVE,timeStr,FILENAME=coordDir+final_TIMEFile

        timeStr = !NULL
     ENDIF

     IF stitch_AACGM THEN BEGIN

        AACGM = {ALT : MAKE_ARRAY(nTot,/FLOAT), $
                 MLT : MAKE_ARRAY(nTot,/FLOAT), $
                 LAT : MAKE_ARRAY(nTot,/FLOAT)}
        
        curInd = 0LL

        FOR k=0,nFiles-1 DO BEGIN
           RESTORE,coordDir+outFiles[k]
           
           IF KEYWORD_SET(trim_one_off) THEN BEGIN
              IF (k LT (nFiles-1)) THEN BEGIN
                 TRIM_OFF_LAST_IND,AACGMStruct,['alt','mlt','lat']
              ENDIF
           ENDIF

           nHere   = N_ELEMENTS(AACGMStruct.alt)
           tmpInds = [curInd:(curInd+nHere-1)]

           AACGM.alt[tmpInds] = AACGMStruct.alt
           AACGM.MLT[tmpInds] = AACGMStruct.MLT
           AACGM.lat[tmpInds] = AACGMStruct.lat

           curInd += nHere

           AACGMStruct = !NULL

        ENDFOR


        PRINT,"Saving stitched AACGM file to " + final_AACGMFile
        SAVE,AACGM,FILENAME=coordDir+final_AACGMFile
        
        AACGM = !NULL
     ENDIF

     IF stitch_GEO THEN BEGIN

        GEOF = {ALT : MAKE_ARRAY(nTot,/FLOAT), $
                LON : MAKE_ARRAY(nTot,/FLOAT), $
                LAT : MAKE_ARRAY(nTot,/FLOAT)}
        
        GEOCart = MAKE_ARRAY(3,nTot,/FLOAT)

        curInd = 0LL

        FOR k=0,nFiles-1 DO BEGIN
           RESTORE,coordDir+GEO_MAGFiles[k]
           
           IF KEYWORD_SET(trim_one_off) THEN BEGIN
              IF (k LT (nFiles-1)) THEN BEGIN
                 TRIM_OFF_LAST_IND,GEO,['alt','lon','lat']
              ENDIF
           ENDIF
           
           nHere   = N_ELEMENTS(GEO.alt)
           tmpInds = [curInd:(curInd+nHere-1)]

           GEOF.alt[tmpInds] = GEO.alt
           GEOF.lon[tmpInds] = GEO.lon
           GEOF.lat[tmpInds] = GEO.lat

           GEOCart[*,tmpInds] = coords.GEO

           curInd += nHere

           GEO = !NULL

        ENDFOR

        GEO = TEMPORARY(GEOF)

        PRINT,"Saving stitched GEO file to " + final_GEOFile
        SAVE,GEO,FILENAME=coordDir+final_GEOFile

        PRINT,"Saving stitched GEOCart file to " + final_GEOCartFile
        SAVE,GEOCart,FILENAME=coordDir+final_GEOCartFile

        GEO = !NULL
        GEOCart = !NULL
     ENDIF

     IF stitch_MAG THEN BEGIN

        MAGF = {ALT : MAKE_ARRAY(nTot,/FLOAT), $
                LON : MAKE_ARRAY(nTot,/FLOAT), $
                LAT : MAKE_ARRAY(nTot,/FLOAT)}
        
        curInd = 0LL

        FOR k=0,nFiles-1 DO BEGIN
           RESTORE,coordDir+GEO_MAGFiles[k]
           
           IF KEYWORD_SET(trim_one_off) THEN BEGIN
              IF (k LT (nFiles-1)) THEN BEGIN
                 TRIM_OFF_LAST_IND,MAG,['alt','lon','lat']
              ENDIF
           ENDIF

           nHere   = N_ELEMENTS(MAG.alt)
           tmpInds = [curInd:(curInd+nHere-1)]

           MAGF.alt[tmpInds] = MAG.alt
           MAGF.lon[tmpInds] = MAG.lon
           MAGF.lat[tmpInds] = MAG.lat

           curInd += nHere

           MAG = !NULL

        ENDFOR

        MAG = TEMPORARY(MAGF)

        PRINT,"Saving stitched MAG file to " + final_MAGFile
        SAVE,MAG,FILENAME=coordDir+final_MAGFile
        
        MAG = !NULL
     ENDIF

     IF stitch_GEI THEN BEGIN

        GEI = {ALT : MAKE_ARRAY(nTot,/FLOAT), $
               LNG : MAKE_ARRAY(nTot,/FLOAT), $
               LAT : MAKE_ARRAY(nTot,/FLOAT)}
        
        curInd = 0LL

        FOR k=0,nFiles-1 DO BEGIN
           RESTORE,coordDir+GEI_files[k]
           
           IF KEYWORD_SET(trim_one_off) THEN BEGIN
              IF (k LT (nFiles-1)) THEN BEGIN
                 TRIM_OFF_LAST_IND,GEIcoords,['alt','lng','lat']
              ENDIF
           ENDIF

           nHere   = N_ELEMENTS(GEIcoords.alt)
           tmpInds = [curInd:(curInd+nHere-1)]

           GEI.alt[tmpInds] = GEIcoords.alt
           GEI.lng[tmpInds] = GEIcoords.lng
           GEI.lat[tmpInds] = GEIcoords.lat

           curInd   += nHere

           GEIcoords = !NULL

        ENDFOR

        PRINT,"Saving stitched GEI file to " + final_GEIFile
        SAVE,GEI,FILENAME=coordDir+final_GEIFile
        
        GEI = !NULL
     ENDIF

     IF stitch_dpTilt THEN BEGIN

        dpTilt          = {TIME: 'Use sw_data times, silly', $
                           ANGLE: MAKE_ARRAY(nTot,/FLOAT), $
                           CUSPLOC_N_GSM: MAKE_ARRAY(3,nTot,/FLOAT), $
                           CUSPLOC_S_GSM: MAKE_ARRAY(3,nTot,/FLOAT), $
                           CUSPLOC_N_GEO: MAKE_ARRAY(3,nTot,/FLOAT), $
                           CUSPLOC_S_GEO: MAKE_ARRAY(3,nTot,/FLOAT), $
                           CREATED: GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                           ORIGINATING_ROUTINE: orig_routineName}
        
        curInd = 0LL

        FOR k=0,nFiles-1 DO BEGIN
           RESTORE,coordDir+dpTiltFiles[k]
           
           IF KEYWORD_SET(trim_one_off) THEN BEGIN
              IF (k LT (nFiles-1)) THEN BEGIN
                 TRIM_OFF_LAST_IND,dpTiltcoords,['alt','lng','lat']
              ENDIF
           ENDIF

           nHere   = N_ELEMENTS(tiltAngle.angle)
           tmpInds = [curInd:(curInd+nHere-1)]

           dpTilt.angle[tmpInds]           = tiltAngle.angle
           dpTilt.cuspLoc_N_GSM[*,tmpInds] = tiltAngle.cuspLoc_N_GSM
           dpTilt.cuspLoc_S_GSM[*,tmpInds] = tiltAngle.cuspLoc_S_GSM
           dpTilt.cuspLoc_N_GEO[*,tmpInds] = tiltAngle.cuspLoc_N_GEO
           dpTilt.cuspLoc_S_GEO[*,tmpInds] = tiltAngle.cuspLoc_S_GEO

           curInd   += nHere

           tiltAngle = !NULL

        ENDFOR

        PRINT,"Saving stitched dpTilt file to " + final_dpTiltFile
        SAVE,dpTilt,FILENAME=coordDir+final_dpTiltFile
        
        dpTilt = !NULL
     ENDIF

  ENDIF

END

PRO TRIM_OFF_LAST_IND,struct,tagNames

  ;; IF SIZE(struct,/TYPE) NE 8


  FOR k=0,N_ELEMENTS(tagNames)-1 DO BEGIN

     tagInd = WHERE(STRUPCASE(TAG_NAMES(struct)) EQ STRUPCASE(tagNames[k]))


     IF tagInd[0] EQ -1 THEN STOP

     nHere  = N_ELEMENTS(struct.(tagInd))

     STR_ELEMENT,struct,tagNames[k],(struct.(tagInd))[0:(nHere-2)],/ADD_REPLACE

  ENDFOR

END