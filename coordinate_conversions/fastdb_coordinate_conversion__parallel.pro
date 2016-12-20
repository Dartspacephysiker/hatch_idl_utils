;;12/20/16
;;Example:
;; t1 = '1996-10-06/16:26:02.417'
;; t2 = '1996-10-06/16:49:28.917'
;; GET_FA_ORBIT,t1,t2
;; GET_DATA,'ORBIT',DATA=orbit
;; times = (TEMPORARY(orbit)).x
;; COORDINATE_CONVERSION__PARALLEL,times,/CREATE_TIMESTAMPS,ORIG_ROUTINENAME='Example',COORDDIR='~/Desktop/',OUTFILE_PREF='Example_1996'
PRO COORDINATE_CONVERSION__PARALLEL,times, $
                                    CREATE_TIMESTAMPS=create_timeStamps, $
                                    GET_GEI_COORDS=get_GEI_coords, $
                                    DO_GEO_MAG_CONVERSIONS=do_GEO_MAG_conversions, $
                                    DO_AACGM_CONVERSIONS=do_AACGM_conversions, $
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
                                    OK__CONTINUE_WITH_ONLY_FEW_CPUS=OK__low_CPU_number

  COMPILE_OPT IDL2

  @defaults__fastdb_coordinate_conversion.pro

  nExec    = KEYWORD_SET(create_timeStamps) + KEYWORD_SET(get_GEI_coords) + KEYWORD_SET(do_GEO_MAG_conversions) + KEYWORD_SET(do_AACGM_conversions)

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
        PRINT,""

        RETURN
     END
     1: BEGIN
        CASE 1 OF
           KEYWORD_SET(create_timeStamps): BEGIN
              execType = '\CREATE_TIMESTAMPS'
           END
           KEYWORD_SET(get_GEI_coords): BEGIN
              execType = '\GET_GEI_COORDS'
           END
           KEYWORD_SET(do_GEO_MAG_conversions): BEGIN
              execType = '\DO_GEO_MAG_CONVERSIONS'
           END
           KEYWORD_SET(do_AACGM_conversions): BEGIN
              execType = '\DO_AACGM_CONVERSIONS'
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
        PRINT,""

     END
  ENDCASE

  nCPUs    = !CPU.HW_NCPU
  
  IF nCPUs LT 3 AND ~KEYWORD_SET(OK__low_CPU_number) THEN BEGIN
     PRINT,"So you may not derive much benefit from this. Sure you want to continue?"
     STOP
  ENDIF

  oBridge  = OBJARR(nCPUs)

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

  ;;Now start the show!
  tmpFiles     = !NULL
  outFiles     = !NULL
  timeFiles    = !NULL
  GEI_files    = !NULL
  GEO_MAGFiles = !NULL
  indArr       = !NULL
  nTot         = N_ELEMENTS(times)
  divFactor    = nTot/nCPUs

  PRINT,"FASTDB_COORDINATE_CONVERSION__PARALLEL: " + STRCOMPRESS(nTot,/REMOVE_ALL) + " inds total"
  FOR i=0,nCPUs-1 DO BEGIN

     ind1         = i*divFactor
     ind2         = ( ((i+1)*divFactor) < (nTot - 1) )
     indArr       = [[indArr],[ind1,ind2]]

     indSuff      = STRING(FORMAT='("--",I0,"-",I0)',ind1,ind2)

     tmpFiles     = [tmpFiles ,STRING(FORMAT='("TMP_",A0,"-",I0,A0)',outFile_pref,i,indSuff)]
     outFiles     = [outFiles ,STRING(FORMAT='(A0,A0,"-",I0,A0)'    ,outFile_pref,"-AACGM",i,indSuff)]
     timeFiles    = [timeFiles,STRING(FORMAT='(A0,"-",I0,A0)'       ,timeFile_pref,i,indSuff)]
     GEI_Files    = [GEI_Files,STRING(FORMAT='(A0,"-",I0,A0)'       ,GEI_coord_filename_pref,i,indSuff)]
     GEO_MAGFiles = [GEO_MAGFiles,STRING(FORMAT='(A0,"-",I0,A0)'    ,GEO_MAG_file_pref,i,indSuff)]

  ENDFOR

  IF indArr[-1] NE (nTot-1) THEN BEGIN
     PRINT,"KALSKJDF"
     STOP
  ENDIF

  ;;Show user before beginning
  PRINT,"Here's what I'm going to do: "
  FOR i=0,nCPUs-1 DO BEGIN
     PRINT,"i           : ",i              
     PRINT,"tmpFile     : ",tmpFiles    [i]
     PRINT,"outFile     : ",outFiles    [i]
     PRINT,"timeFile    : ",timeFiles   [i]
     PRINT,"GEI_File    : ",GEI_files   [i]
     PRINT,"GEO_MAGFile : ",GEO_MAGFiles[i]

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
        ELSE: BEGIN
           PRINT,"No, you need to answer 'yes' or 'no'"
        END
     ENDCASE
  ENDWHILE


  FOR i=0,nCPUs-1 DO BEGIN

     ind1 = indArr[0,i]
     ind2 = indArr[1,i]

     CASE 1 OF
        KEYWORD_SET(dry_run): BEGIN
           PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "] = OBJ_NEW('IDL_IDLBridge') "
           PRINT,"                                                         "
           PRINT,'Inds: ' + STRCOMPRESS(ind1,/REMOVE_ALL) + ', ' + STRCOMPRESS(ind2,/REMOVE_ALL)
           PRINT,";;Set all the vars for this environment                  "
           PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "]->SetVar, 'coordDir'        ," + $
                 "coordDir          "
           PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "]->SetVar, 'orig_routineName'," + $
                 "orig_routineName  "
           PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "]->SetVar, 'times'           ," + $
                 "times             "
           PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "]->SetVar, 'tmpFile'         ," + $
                 "tmpFiles[" + STRCOMPRESS(i,/REMOVE_ALL) + "]       "
           PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "]->SetVar, 'outFile'         ," + $
                 "outFiles[" + STRCOMPRESS(i,/REMOVE_ALL) + "]       "
           PRINT,"oBridge[" + STRCOMPRESS(i,/REMOVE_ALL) + "]->SetVar, 'timeFile'        ," + $
                 "timeFiles[" + STRCOMPRESS(i,/REMOVE_ALL) + "]       "


        END
        ELSE: BEGIN

           ;;Temporary inds
           tmpInds = [ind1:ind2]

           IF i EQ nCPUs-1 THEN BEGIN

           ENDIF ELSE BEGIN
              
           ENDELSE

           oBridge[i] = OBJ_NEW('IDL_IDLBridge')

           IF ~KEYWORD_SET(quiet) THEN BEGIN
              PRINT,'coordDir'          ,coordDir
              PRINT,'orig_routineName'  ,orig_routineName
              PRINT,'check_if_exists'   ,KEYWORD_SET(check_if_exists)
              PRINT,'times'             ,times[tmpInds]
              PRINT,'tmpFile'           ,tmpFiles[i]
              PRINT,'outFile'           ,outFiles[i]
              PRINT,'timeFile'          ,timeFiles[i]
              PRINT,'GEI_coord_filename',GEI_files[i]
              PRINT,'GEO_MAG_fileName'  ,GEO_MAGFiles[i]
           ENDIF

           ;;Set all the vars for this environment
           oBridge[i]->SetVar, 'coordDir'          ,coordDir
           oBridge[i]->SetVar, 'orig_routineName'  ,orig_routineName
           oBridge[i]->SetVar, 'check_if_exists'   ,KEYWORD_SET(check_if_exists)
           oBridge[i]->SetVar, 'times'             ,times[tmpInds]
           oBridge[i]->SetVar, 'tmpFile'           ,tmpFiles[i]
           oBridge[i]->SetVar, 'outFile'           ,outFiles[i]
           oBridge[i]->SetVar, 'timeFile'          ,timeFiles[i]
           oBridge[i]->SetVar, 'GEI_coord_filename',GEI_files[i]
           oBridge[i]->SetVar, 'GEO_MAG_fileName'  ,GEO_MAGFiles[i]

           execStr = 'FASTDB_COORDINATE_CONVERSION__SINGLE,times,' + execType + ',' + $
                     'R_E=R_E,ALTITUDE_MAX=altitude_max,ALLOW_FL_TRACE=allow_fl_trace,' + $
                     'TIMEFILE=timeFile,OUTFILE=outFile,COORDDIR=coordDir,COORDFILE=GEO_MAG_fileName,' + $
                     'GEI_COORD_FILENAME=GEI_coord_filename,CHECK_IF_EXISTS=check_if_exists,' + $
                     'ORIG_ROUTINENAME=orig_routineName'
           
           oBridge[i]->Execute,execStr,/NOWAIT
           
        END
     ENDCASE

  ENDFOR


  IF ~KEYWORD_SET(dry_run) THEN BEGIN

     notdone = 1
     count   = 0LL
     WHILE notdone GT 0 DO BEGIN
        done = 0
        FOR i=0,N_ELEMENTS(oBridge)-1 DO $
        done = done+oBridge[i]->Status()
        IF done EQ 0 THEN notdone = done

        count++
        count = count MOD 10000
        IF (count MOD 10000) EQ 0 THEN PRINT,"Waiting .."
     ENDWHILE

     FOR i=0,N_ELEMENTS(oBridge)-1 DO BEGIN
        OBJ_DESTROY,oBridge[N_ELEMENTS(oBridge)-1-i]
     ENDFOR

  ENDIF

END
