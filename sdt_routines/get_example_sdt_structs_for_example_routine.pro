;2016/07/16
PRO GET_EXAMPLE_SDT_STRUCTS_FOR_EXAMPLE_ROUTINE,time, $
   QUIET=quiet

  COMPILE_OPT idl2

  outDir  = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/example_SDT_structs/'
  outFilePref = 'example_SDT_structs'

  t = KEYWORD_SET(time) ? time : 0.D

  eebExample = GET_FA_EEB(t,/ST)
  iebExample = GET_FA_IEB(t,/ST)

  eesExample = GET_FA_EES(t,/ST)
  iesExample = GET_FA_IES(t,/ST)

  GET_FA_ORBIT,t,t+1

  GET_DATA,'ORBIT',DATA=orbit
  orb                  = orbit.y[0]
  
  outFile = outFilePref + '--orb_' + STRCOMPRESS(orb,/REMOVE_ALL) + '.sav'

  IF ~KEYWORD_SET(quiet) THEN PRINT,"Saving " + outFile + '...'
  SAVE,eesExample,eebExample,iesExample,iebExample,FILENAME=outDir+outFile
     
END