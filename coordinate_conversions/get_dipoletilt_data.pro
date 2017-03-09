;2017/02/08
PRO GET_DIPOLETILT_DATA,timeStr,times, $
                        OUTFILE=outFile, $
                        OUTDIR=outDir, $
                        ORIG_ROUTINENAME=orig_routineName

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(outFile) EQ 0 OR N_ELEMENTS(outDir) EQ 0 OR N_ELEMENTS(orig_routineName) EQ 0 THEN STOP

  IF FILE_TEST(outDir+outFile) THEN BEGIN
     PRINT,"File exists: " + outDir+outFile
     PRINT,"Tell me you want it."
     STOP
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;cusp stuff
  cuspLocN_MAG     = [0,0,1]
  cuspLocS_MAG     = [0,0,-1]

  ;; GEOPACK_CONV_COORD
  ;; Description: Convert between a variety of commonly used coordinate systems.
  ;; Calling Sequence: geopack_conv_coord(_08), s1, s2, s3, d1, d2, d3.
  ;; Inputs: s1, s2, s3: Coordinates in system of origin.
  ;; Outputs: d1, d2, d3: Coordinates in target system.
  ;; Keywords: FROM_GEO: Specify source in geopgraphic coordinates. 
  ;;  FROM_MAG: Specify source in geomagnetic coordinates.
  ;;  FROM_GEI: Specify source in geocentric equatorial inertial coordinates.
  ;;  FROM_SM: Specify source in solar magnetic coordinates.
  ;;  FROM_GSM: Specify source in geocentric solar magnetospheric
  ;;  coordinates.
  ;;  FROM_GSE: Specify source in geocentric solar ecliptic coordinates.
  ;;  TO_GEO: Specify destination in geopgraphic coordinates.
  ;;  TO_MAG: Specify destination in geomagnetic coordinates.
  ;;  TO_GEI: Specify destination in geocentric equatorial inertial coordinates.
  ;;  TO_SM: Specify destination in solar magnetic coordinates.
  ;;  TO_GSM: Specify destination in geocentric solar magnetospheric
  ;;  coordinates. 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Times in CDF epoch time
  time_epoch    = UTC_TO_CDF_EPOCH(TEMPORARY(times))

  CONVERT_TIME_STRING_TO_YMDHMS_ARRAYS,timeStr, $
                                       OUT_YEARARR=yearArr, $
                                       OUT_DOYARR=DOYArr, $
                                       OUT_MONTHARR=monthArr, $
                                       OUT_DAYARR=dayArr, $
                                       OUT_HOURARR=hourArr, $
                                       OUT_MINARR=minArr, $
                                       OUT_SECARR=secArr

  ;; YearArr       = FIX(STRMID(timeStr,0,4))
  ;; MonthArr      = FIX(STRMID(timeStr,5,2))
  ;; DayArr        = FIX(STRMID(timeStr,8,2))
  ;; HourArr       = FIX(STRMID(timeStr,11,2))
  ;; MinArr        = FIX(STRMID(timeStr,14,2))
  ;; SecArr        = FLOAT(STRMID(timeStr,17,6))

  ;;Free up dat mem
  timeStr       = !NULL

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;feed it to GEOPACK
  nTot          = N_ELEMENTS(time_epoch)

  totStr        = STRCOMPRESS(nTot,/REMOVE_ALL)
  tiltArr            = !NULL
  clN_GEO_arr        = MAKE_ARRAY(3,nTot,/FLOAT)
  clN_GSM_arr        = MAKE_ARRAY(3,nTot,/FLOAT)
  clS_GEO_arr        = MAKE_ARRAY(3,nTot,/FLOAT)
  clS_GSM_arr        = MAKE_ARRAY(3,nTot,/FLOAT)
  PRINT,"Feeding " + totStr + " inds to GEOPACK ..."
  FOR i=0,nTot-1 DO BEGIN

     GEOPACK_RECALC,yearArr[i],DOYArr[i],hourArr[i],minArr[i],secArr[i],TILT=tempTilt

     ;;do those cusps
     GEOPACK_CONV_COORD,cuspLocN_MAG[0],cuspLocN_MAG[1],cuspLocN_MAG[2],clngeo_x,clngeo_y,clngeo_z,/FROM_MAG,/TO_GEO,EPOCH=time_epoch[i]
     GEOPACK_CONV_COORD,cuspLocN_MAG[0],cuspLocN_MAG[1],cuspLocN_MAG[2],clngsm_x,clngsm_y,clngsm_z,/FROM_MAG,/TO_GSM,EPOCH=time_epoch[i]

     GEOPACK_CONV_COORD,cuspLocS_MAG[0],cuspLocS_MAG[1],cuspLocS_MAG[2],clsgeo_x,clsgeo_y,clsgeo_z,/FROM_MAG,/TO_GEO,EPOCH=time_epoch[i]
     GEOPACK_CONV_COORD,cuspLocS_MAG[0],cuspLocS_MAG[1],cuspLocS_MAG[2],clsgsm_x,clsgsm_y,clsgsm_z,/FROM_MAG,/TO_GSM,EPOCH=time_epoch[i]

     ;;update
     tiltArr    = [tiltArr,tempTilt]
     clN_GEO_arr[*,i] = [clngeo_x,clngeo_y,clngeo_z]
     clN_GSM_arr[*,i] = [clngsm_x,clngsm_y,clngsm_z]
     clS_GEO_arr[*,i] = [clsgeo_x,clsgeo_y,clsgeo_z]
     clS_GSM_arr[*,i] = [clsgsm_x,clsgsm_y,clsgsm_z]

     IF (i MOD 10000) EQ 0 THEN PRINT,FORMAT='(I0,"/",I0)',i,nTot

  ENDFOR

  tiltAngle          = {TIME: 'Use sw_data times, silly', $
                        ANGLE: tiltArr, $
                        ;; TSTAMP: tStamp, $
                        ;; JULDAY: julday, $
                        CUSPLOC_N_GSM: clN_GSM_arr, $
                        CUSPLOC_S_GSM: clS_GSM_arr, $
                        CUSPLOC_N_GEO: clN_GEO_arr, $
                        CUSPLOC_S_GEO: clS_GEO_arr, $
                        CREATED: GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                        ORIGINATING_ROUTINE: orig_routineName}

  ;; IF KEYWORD_SET(orig_routineName) THEN BEGIN
  ;;    tiltAngle.originating_routine = orig_routineName
  ;; ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Save it
  PRINT,'Saving ' + outDir + outFile + '...'
  SAVE,tiltAngle,FILENAME=outDir+outFile

  PRINT,"Did it!"

END
