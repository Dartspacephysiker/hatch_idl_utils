;2016/03/12 Spence needs this
;Example: Surf. Area of Utah in two squares
; ;;:Upper square: 
; lonLat1  = [-114.043579, 41.951320]
; lonLat2  = [-111.055298, 41.021355]
; upArea   = LATLONG_PAIR_AREA(lonLat1[0],lonLat1[1],lonLat2[0],lonLat2[1])
;
; ;;:Lower square: 
; lonLat1  = [-114.043579, 41.021355]
; lonLat2  = [-109.046173, 37.009133]
; downArea = LATLONG_PAIR_AREA(lonLat1[0],lonLat1[1],lonLat2[0],lonLat2[1])
;
;uparea+downarea = 212565.86 km^2
;actual area of Utah = 84899 sq. mi. * (1.60934 km/mi)^2 = 219886.3 sq. km.
;Error = 3.33% (7320 sq. km.). Not bad!

;Get area (in km^2) of a rectangle on a sphere defined by two lat/lon pairs
FUNCTION LATLONG_PAIR_AREA,lon1, lat1, $
                           lon2, lat2, $
                           RADIUS_METERS=radius_meters

  COMPILE_OPT idl2

  height  = GEO_DIST(lon1, lat1, $
                     lon1, lat2, $
                     RADIUS_METERS=radius_meters)
  
  width   = GEO_DIST(lon1, lat1, $
                     lon2, lat1, $
                     RADIUS_METERS=radius_meters)

  RETURN, height*width

END
