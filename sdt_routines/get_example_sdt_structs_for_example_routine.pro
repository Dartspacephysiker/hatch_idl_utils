;2016/07/16
PRO GET_EXAMPLE_SDT_STRUCTS_FOR_EXAMPLE_ROUTINE,time, $
   QUIET=quiet, $
   UNITS=units

  COMPILE_OPT IDL2,STRICTARRSUBS

  outDir       = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/example_SDT_structs/'
  outFilePref  = 'example_SDT_structs'

  CASE SIZE(time,/TYPE) OF
     7: BEGIN
        t      = STR_TO_TIME(time)
     END
     ELSE: BEGIN
        t      = KEYWORD_SET(time) ? time : 0.D
        start  = 1
     END
  ENDCASE

  eebExample   = GET_FA_EEB(t,START=start)
  iebExample   = GET_FA_IEB(t,START=start)

  eesExample   = GET_FA_EES(t,START=start)
  iesExample   = GET_FA_IES(t,START=start)

  IF ~eebExample.valid THEN BEGIN
     PRINT,"Invalid eebExample! Aborting ..."
     RETURN
  ENDIF

  IF ~eesExample.valid THEN BEGIN
     PRINT,"Invalid eesExample! Aborting ..."
     RETURN
  ENDIF

  IF ~iebExample.valid THEN BEGIN
     PRINT,"Invalid iebExample! Aborting ..."
     RETURN
  ENDIF

  IF ~iesExample.valid THEN BEGIN
     PRINT,"Invalid iesExample! Aborting ..."
     RETURN
  ENDIF

  IF KEYWORD_SET(units) THEN BEGIN
     CASE SIZE(units,/TYPE) OF
        7: BEGIN
           PRINT,'Converting example SDT structs to ' + units + ' ...'
           CALL_PROCEDURE,eebExample.units_procedure,eebExample,units
           CALL_PROCEDURE,iebExample.units_procedure,iebExample,units
           CALL_PROCEDURE,eesExample.units_procedure,eesExample,units
           CALL_PROCEDURE,iesExample.units_procedure,iesExample,units
        END
        ELSE: BEGIN
           PRINT,'Invalid units; leaving as counts ...'
        END
     ENDCASE
     
  ENDIF

  GET_FA_ORBIT,t,t+1

  GET_DATA,'ORBIT',DATA=orbit
  orb                  = orbit.y[0]
  
  outFile = outFilePref + '--orb_' + STRCOMPRESS(orb,/REMOVE_ALL) + '.sav'

  IF ~KEYWORD_SET(quiet) THEN PRINT,"Saving " + outFile + '...'
  SAVE,eesExample,eebExample,iesExample,iebExample,FILENAME=outDir+outFile
     
END