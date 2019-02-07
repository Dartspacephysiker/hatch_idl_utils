;;11/22/16
;;For example usage of this program, visit JOURNAL__20160827__CONVERT_ESPECDB_ILATS_TO_AACGM__ALL_OF_EM in the eSpec_identification repository
PRO CONVERT_GEI_COORDS_TO_GEO_AND_MAG_COORDS,timeStr,times, $
   GEI_FILE=GEI_file, $
   GEI_DIR=GEI_dir, $
   GEI_STRUCT_NAME=defGEIStructName, $
   IN_GEI_STRUCT=GEICoords, $
   INCLUDE_GEI_TO_GEO_MATRIX=include_gei_to_geo_matrix, $
   OUTFILE=outFile, $
   OUTDIR=outDir, $
   ORIG_ROUTINENAME=orig_routineName, $
   FORCE_OVERWRITE=force_overwrite, $
   OVERWRITE_IF_NO_GEI_TO_GEO=overwrite_if_no_GEI2GEO

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(outFile) EQ 0 OR N_ELEMENTS(outDir) EQ 0 THEN STOP

  IF FILE_TEST(outDir+outFile) THEN BEGIN

     IF KEYWORD_SET(overwrite_if_no_GEI2GEO) THEN BEGIN

        RESTORE,outDir+outFile

        tNames = STRUPCASE(TAG_NAMES(coords))

        IF (WHERE(tNames EQ "GEI2GEO_COORD"))[0] NE -1 THEN BEGIN
           PRINT,"Already have GEI2GEO stuff! Out ..."
           RETURN
        ENDIF ELSE BEGIN
           PRINT,"Not have GEI2GEO yet -- skal bli!"
        ENDELSE

     ENDIF ELSE IF KEYWORD_SET(force_overwrite) THEN BEGIN
        PRINT,"Forced overwrite!"
     ENDIF ELSE BEGIN
        PRINT,"File exists: " + outDir+outFile
        PRINT,"Tell me you want it."
        STOP
     ENDELSE
  ENDIF
  R_E              = 6371.2D    ;Earth radius in km, from IGRFLIB_V2.pro

  ;;Load the stuff we need 
  IF N_ELEMENTS(GEICoords) EQ 0 THEN BEGIN
     RESTORE,GEI_dir+GEI_file
  ENDIF
  
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
  time_epoch    = UTC_TO_CDF_EPOCH(times)

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

  IF KEYWORD_SET(include_gei_to_geo_matrix) THEN BEGIN

     ;;for vector transforms
     ident            = IDENTITY(3,/DOUBLE)

     GEI2GEO_coord    = MAKE_ARRAY(3,3,nTot,/DOUBLE)
     GEI2GEO_vec      = MAKE_ARRAY(3,3,nTot,/DOUBLE)

  ENDIF

  totStr        = STRCOMPRESS(nTot,/REMOVE_ALL)
  PRINT,"Feeding " + totStr + " FAST inds to GEOPACK ..."
  FOR i=0,nTot-1 DO BEGIN

     GEOPACK_RECALC_08,YearArr[i],MonthArr[i],DayArr[i],HourArr[i],MinArr[i],SecArr[i],/DATE

     ;;do that dance
     ;;To MAG
     GEOPACK_CONV_COORD_08,GEICoords.fa_pos[i,0],GEICoords.fa_pos[i,1],GEICoords.fa_pos[i,2], $
                        faPosmag_x,faPosmag_y,faPosmag_z, $
                        /FROM_GEI,/TO_MAG,EPOCH=time_epoch[i]
     ;;To GEO
     GEOPACK_CONV_COORD_08,GEICoords.fa_pos[i,0],GEICoords.fa_pos[i,1],GEICoords.fa_pos[i,2], $
                        faPosgeo_x,faPosgeo_y,faPosgeo_z, $
                        /FROM_GEI,/TO_GEO,EPOCH=time_epoch[i]

     GEOPACK_SPHCAR_08,faPosgeo_x,faPosgeo_y,faPosgeo_z,geo_r,geo_theta,geo_phi,/TO_SPHERE,/DEGREE
     GEOPACK_SPHCAR_08,faPosmag_x,faPosmag_y,faPosmag_z,mag_r,mag_theta,mag_phi,/TO_SPHERE,/DEGREE

     IF KEYWORD_SET(include_gei_to_geo_matrix) THEN BEGIN

        ;;GEO to GEI
        GEOPACK_CONV_COORD_08,ident[0,0],ident[0,1],ident[0,2], $
                              geo_x0,geo_y0,geo_z0, $
                              /FROM_GEI,/TO_GEO,EPOCH=time_epoch[i]

        GEOPACK_CONV_COORD_08,ident[1,0],ident[1,1],ident[1,2], $
                              geo_x1,geo_y1,geo_z1, $
                              /FROM_GEI,/TO_GEO,EPOCH=time_epoch[i]

        GEOPACK_CONV_COORD_08,ident[2,0],ident[2,1],ident[2,2], $
                              geo_x2,geo_y2,geo_z2, $
                              /FROM_GEI,/TO_GEO,EPOCH=time_epoch[i]


        GEI2GEO_coord[*,*,i]    = [[geo_x0,geo_y0,geo_z0], $
                                   [geo_x1,geo_y1,geo_z1], $
                                   [geo_x2,geo_y2,geo_z2]]

        GEI2GEO_vec[*,*,i]      = INVERT(GEI2GEO_coord[*,*,i])

     ENDIF

     ;; Da kan GEI2GEO brukes sånn:
     ;; B_GEO_arr[*,i]        = GEI2GEO_vec[*,*,i]    # tmpB_GEI


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
            R_E    : R_E, $
            INFO   : "Components of FAST position vector in this struct are CARTESIAN MAG and GEO. The self-standing structs give FAST pos vec in SPHERICAL coords (lat, lon, R-R_E)", $
            ORIGINATING_ROUTINE:''}

  IF KEYWORD_SET(include_gei_to_geo_matrix) THEN BEGIN

     PRINT,"Adding GEI2GEO matrices ..."
     coords = CREATE_STRUCT(coords, $
                            "GEI2GEO_coord",GEI2GEO_coord, $
                            "GEI2GEO_vec",GEI2GEO_vec)

  ENDIF

  IF KEYWORD_SET(orig_routineName) THEN BEGIN
     coords.originating_routine = orig_routineName
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Save it
  PRINT,'Saving ' + outDir + outFile + '...'
  SAVE,coords,GEO,MAG,FILENAME=outDir+outFile

  PRINT,"Did it!"

END
