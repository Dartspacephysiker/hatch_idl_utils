;;2016/12/20
;;Really, this thing wants to be called by CONVERT_GEO_TO_AACGM, but you know ... do whatever
FUNCTION CREATE_FASTDB_TSTAMPS,timeTmp,nTot,timeFile, $
                               ALTITUDE_MAX=altitude_max, $
                               R_E=R_E, $
                               ALLOW_FL_TRACE=allow_fl_trace, $
                               DONT_SAVE_TIMESTAMPS=dont_save_timeStamps

  COMPILE_OPT idl2

  @defaults__fastdb_coordinate_conversion.pro

  IF N_ELEMENTS(nTot) EQ 0 THEN nTot = N_ELEMENTS(timeTmp)

  PRINT,"Creating " + STRCOMPRESS(nTot,/REMOVE_ALL) + "FASTDB timestamps ..."

  ;;Convert
  divFactor     = 10000L   ;No more than 10000 at once
  timeTmpStr    = MAKE_ARRAY(nTot,/STRING)
  FOR kk=0L,(nTot/divFactor) DO BEGIN
     ind1       = kk*divFactor
     ind2       = ( ((kk+1)*divFactor) < (nTot - 1) )
     PRINT,'Inds: ' + STRCOMPRESS(ind1,/REMOVE_ALL) + ', ' + STRCOMPRESS(ind2,/REMOVE_ALL)
     tempI      = [ind1:ind2]
     timeTmpStr[tempI] = TIME_TO_STR(timeTmp[tempI],/MSEC)
  ENDFOR

  IF ~KEYWORD_SET(dont_save_timeStamps) THEN BEGIN

     IF ~KEYWORD_SET(timeFile) THEN BEGIN
        PRINT,"Can't save timestamps! no timeStamp filename provided!"
        RETURN,-1
     ENDIF

     PRINT,'Saving time strings to ' + timeFile + ' ...'
     savedAltitude_max   = altitude_max
     savedR_E            = R_E
     savedAllow_fl_trace = allow_fl_trace
     SAVE,timeTmpstr,savedAltitude_max,savedR_E,savedAllow_fl_trace,FILENAME=timeFile

  ENDIF

  RETURN,timeTmpStr

END

