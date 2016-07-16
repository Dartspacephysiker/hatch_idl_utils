;2016/07/16
;In case you just want to take a peek!
FUNCTION LOAD_EXAMPLE_SDT_STRUCT,EES=ees,EEB=eeb,IES=ies,IEB=ieb, $
                                ORBIT=orbit

  COMPILE_OPT idl2 

  dir          = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/example_SDT_structs/'
  defExampleFile  = 'example_SDT_structs.sav'

  IF KEYWORD_SET(orbit) THEN BEGIN
     exampleFile = STRING(FORMAT='("example_SDT_structs--orb_",I0,".sav")',orbit)
     IF ~KEYWORD_SET(quiet) THEN PRINT,"Attempting to get " + exampleFile + ' ...'
     IF FILE_TEST(dir+defExampleFIle) THEN BEGIN
        RESTORE,dir+exampleFile
     ENDIF ELSE BEGIN
        IF ~KEYWORD_SET(quiet) THEN PRINT,"Couldn't get it! Restoring default: " + defExampleFile + ' ...'
        restore_default = 1
     ENDELSE
  ENDIF ELSE BEGIN
     restore_default = 1
  ENDELSE
  
  IF KEYWORD_SET(restore_default) THEN BEGIN
     RESTORE,dir+defExampleFile
  ENDIF


  CASE 1 OF
     KEYWORD_SET(ees): BEGIN
        struct = eesExample
     END
     KEYWORD_SET(eeb): BEGIN
        struct = eebExample
     END
     KEYWORD_SET(ies): BEGIN
        struct = eesExample
     END
     KEYWORD_SET(ieb): BEGIN
        struct = iebExample
     END
     ELSE: BEGIN
        PRINT,'Default: eesExample'
        struct = eesExample
     END
  ENDCASE

  RETURN,struct

END