;12/20/16
PRO FASTDB_COORDINATE_CONVERSION__SINGLE,times, $
                                         TIME_STRINGS=timeStr, $
                                         CREATE_TIMESTAMPS=create_timeStamps, $
                                         GET_GEI_COORDS=get_GEI_coords, $
                                         DO_GEO_MAG_CONVERSIONS=do_GEO_MAG_conversions, $
                                         DO_AACGM_CONVERSIONS=do_AACGM_conversions, $
                                         ORIG_ROUTINENAME=orig_routineName, $
                                         COORDFILE=GEO_MAG_filename, $
                                         GEI_COORD_FILENAME=GEI_coord_filename, $
                                         COORDDIR=coordDir, $
                                         TIMEFILE=timeFile, $
                                         EPHEMFILEINDARR=ephemFileIndArr, $
                                         TMPFILE=tmpFile, $
                                         OUTFILE=outFile, $
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
                                         DEFNAMES=defNames

  COMPILE_OPT IDL2

  @defaults__fastdb_coordinate_conversion.pro

  IF KEYWORD_SET(create_timeStamps) THEN BEGIN
     nTot = N_ELEMENTS(times)

     IF FILE_TEST(timeFile) AND KEYWORD_SET(check_if_exists) THEN BEGIN
        PRINT,timeFile + ' already exists! Not creating ...' 
     ENDIF ELSE BEGIN
        timeStr = CREATE_FASTDB_TSTAMPS(times,nTot,timeFile, $
                                        ALTITUDE_MAX=altitude_max, $
                                        R_E=R_E, $
                                        ALLOW_FL_TRACE=allow_fl_trace, $
                                        DONT_SAVE_TIMESTAMPS=dont_save_timeStamps)

        PRINT,"Finished creating tStamp file!!"
     ENDELSE
     
     RETURN
  ENDIF

  IF KEYWORD_SET(get_GEI_coords) THEN BEGIN
     GET_FAST_GEI_COORDS,times, $
                         /SAVE_GEI_COORDS, $
                         GEI_COORD_DIR=coordDir, $
                         GEI_COORD_FILENAME=GEI_coord_filename, $ 
                         GEI_STRUCT_NAME=defGEIStructName, $
                         ORIG_ROUTINENAME=orig_routineName, $
                         QUIET=quiet

     PRINT,"Finished getting GEI coords!"

     RETURN

  ENDIF

  IF KEYWORD_SET(do_GEO_MAG_conversions) THEN BEGIN
     CONVERT_GEI_COORDS_TO_GEO_AND_MAG_COORDS, $
        times, $
        timeStr, $
        GEI_FILE=GEI_coord_filename, $
        GEI_DIR=coordDir, $
        GEI_STRUCT_NAME=defGEIStructName, $
        OUTFILE=GEO_MAG_filename, $
        OUTDIR=coordDir, $
        ORIG_ROUTINENAME=orig_routineName

     PRINT,"Finished with GEO_MAG conversions!"

     RETURN

  ENDIF


  IF KEYWORD_SET(do_AACGM_conversions) THEN BEGIN

     CONVERT_GEO_TO_AACGM, $
        COORDFILES=GEO_MAG_filename, $
        GEICOORDFILES=GEI_coord_filename, $
        COORDDIR=coordDir, $
        TMPFILES=tmpFile, $
        TIMEFILES=timeFile, $
        EPHEMFILEINDARR=ephemFileIndArr, $
        OUTDIR=coordDir, $
        OUTFILES=outFile, $
        ORIG_ROUTINENAME=orig_routineName, $
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
        DEFNAMES=defNames

     PRINT,'Finished with AACGM conversions!'

     RETURN

  ENDIF

END
