;;11/22/16
;;For example usage of this program, visit JOURNAL__20160827__CONVERT_ESPECDB_ILATS_TO_AACGM__ALL_OF_EM in the eSpec_identification repository
PRO CONVERT_GEI_COORDS_TO_GEO_AND_MAG_COORDS,timeStr,times, $
   GEI_FILE=GEI_file, $
   GEI_DIR=GEI_dir, $
   GEI_STRUCT_NAME=defGEIStructName, $
   OUTFILE=outFile, $
   OUTDIR=outDir, $
   ORIG_ROUTINENAME=orig_routineName

  COMPILE_OPT IDL2

  IF N_ELEMENTS(outFile) EQ 0 OR N_ELEMENTS(outDir) EQ 0 THEN STOP

  IF FILE_TEST(outDir+outFile) THEN BEGIN
     PRINT,"File exists: " + outDir+outFile
     PRINT,"Tell me you want it."
     STOP
  ENDIF
  R_E              = 6371.2D    ;Earth radius in km, from IGRFLIB_V2.pro

  ;;Load the stuff we need 
  RESTORE,GEI_dir+GEI_file
  
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
  time_epoch         = UTC_TO_CDF_EPOCH(times)

  YearArr       = FIX(STRMID(timeStr,0,4))
  MonthArr      = FIX(STRMID(timeStr,5,2))
  DayArr        = FIX(STRMID(timeStr,8,2))
  HourArr       = FIX(STRMID(timeStr,11,2))
  MinArr        = FIX(STRMID(timeStr,14,2))
  SecArr        = FLOAT(STRMID(timeStr,17,6))

  ;;Free up dat mem
  timeStr       = !NULL

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;feed it to GEOPACK
  nTot          = N_ELEMENTS(times)

  MAG_arr       = MAKE_ARRAY(3,nTot,/FLOAT)
  GEO_arr       = MAKE_ARRAY(3,nTot,/FLOAT)
  MAGSph_arr    = MAKE_ARRAY(3,nTot,/FLOAT)
  GEOSph_arr    = MAKE_ARRAY(3,nTot,/FLOAT)

  totStr        = STRCOMPRESS(nTot,/REMOVE_ALL)
  PRINT,"Feeding " + totStr + " FAST inds to GEOPACK ..."
  FOR i=0,nTot-1 DO BEGIN

     GEOPACK_RECALC,YearArr[i],MonthArr[i],DayArr[i],HourArr[i],MinArr[i],SecArr[i],/DATE

     ;;do that dance
     ;;To MAG
     GEOPACK_CONV_COORD,GEICoords.fa_pos[i,0],GEICoords.fa_pos[i,1],GEICoords.fa_pos[i,2], $
                        faPosmag_x,faPosmag_y,faPosmag_z, $
                        /FROM_GEI,/TO_MAG,EPOCH=time_epoch[i]
     ;;To GEO
     GEOPACK_CONV_COORD,GEICoords.fa_pos[i,0],GEICoords.fa_pos[i,1],GEICoords.fa_pos[i,2], $
                        faPosgeo_x,faPosgeo_y,faPosgeo_z, $
                        /FROM_GEI,/TO_GEO,EPOCH=time_epoch[i]

     GEOPACK_SPHCAR,faPosgeo_x,faPosgeo_y,faPosgeo_z,geo_r,geo_theta,geo_phi,/TO_SPHERE,/DEGREE
     GEOPACK_SPHCAR,faPosmag_x,faPosmag_y,faPosmag_z,mag_r,mag_theta,mag_phi,/TO_SPHERE,/DEGREE

     ;;Lat, long, height
     MAGSph_arr[*,i]    = [mag_theta,mag_phi,mag_r] 
     GEOSph_arr[*,i]    = [geo_theta,geo_phi,geo_r] 

     ;;update
     ;; TiltArr    = [TiltArr,tempTilt]
     MAG_arr[*,i] = [faPosmag_x,faPosmag_y,faPosmag_z]
     GEO_arr[*,i] = [faPosgeo_x,faPosgeo_y,faPosgeo_z]

     ;;update
     MAG_arr[*,i] = [faPosmag_x,faPosmag_y,faPosmag_z]
     GEO_arr[*,i] = [faPosgeo_x,faPosgeo_y,faPosgeo_z]

     IF (i MOD 10000) EQ 0 THEN PRINT,"i = " + STRCOMPRESS(i,/REMOVE_ALL) + '/' + totStr
  ENDFOR

  ;;Lat, long, height
  MAGSph_arr    = [ $
                         [90.-REFORM(MAGSph_arr[0,*])], $
                         [REFORM(MAGSph_arr[1,*])], $
                         [REFORM(MAGSph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
                         ]   

  GEOSph_arr    = [ $
                         [90.-REFORM(GEOSph_arr[0,*])], $
                         [REFORM(GEOSph_arr[1,*])], $
                         [REFORM(GEOSph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
                         ]

  GEO     = {ALT:GEOSph_arr[*,2], $
                 LON:GEOSph_arr[*,1], $
                 LAT:GEOSph_arr[*,0]}

  MAG     = {ALT:MAGSph_arr[*,2], $
                 LON:MAGSph_arr[*,1], $
                 LAT:MAGSph_arr[*,0]}

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;make struct
  coords = {TIME   : times, $
            MAG    : MAG_arr, $
            GEO    : GEO_arr, $
            CREATED: GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
            ORIGINATING_ROUTINE:''}

  IF KEYWORD_SET(orig_routineName) THEN BEGIN
     coords.originating_routine = orig_routineName
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Save it
  PRINT,'Saving ' + outDir + outFile + '...'
  SAVE,coords,GEO,MAG,FILENAME=outDir+outFile

  PRINT,"Did it!"
  STOP

END
