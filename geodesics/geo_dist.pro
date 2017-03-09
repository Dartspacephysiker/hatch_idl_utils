;+
; NAME:
;	GEO_DIST
;
; PURPOSE:
;	This function returns the great circle distance (in km) between
;	two geographical points.
;
; CATEGORY:
;	Geographical
;
; CALLING SEQUENCE:
;	Result = GEO_DIST( Longitude1, Latitude1, Longitude2, Latitude2 )
;
; INPUTS:
;	Longitude1:  The scalar longitude, of type floating point, of
;		the first point.  North is positive.
;	Latitude1:  The scalar latitude, of type floating point, of the
;		first point.  East is positive.
;	Longitude2:  The scalar or vector longitude(s), of type floating
;		point, of the second point(s).  North is positive.
;	Latitude2:  The scalar or vector latitude(s), of type floating
;		point, of the second point(s).  East is positive.
;
; KEYWORD PARAMETERS:
;	SINGLE:  Forces the calculation to be performed using single-
;		precision arithmetic (default is double).
;
; OUTPUTS:
;	Result:  Returns the great circle distance between Point 1 and
;		Point(s) 2.
;
; USES:
;	CONSTANTS.pro
;
; PROCEDURE:
;	The function uses the Haversine formula to calculate the
;	arclength between the points.
;
; EXAMPLE:
;	Calculate the distance between Ottawa, ON (45.25N, 75.43W) and
;	Victoria, BC (48.25N, 123.22W).
;	  results = geo_dist( -75.43, 45.25, -123.22, 48.25 )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@uvic.ca), 2000-06-29.
;	Modified:	DAS, 2000-07-06 (removed LENGTH.pro, added
;			DIMENSION.pro).
;	Modified:	DAS, 2000-07-24 (added Degrad constant).
;	Modified:	DAS, 2000-08-17 (coverted from Cosine to Haversine
;			Haversine formula, added SINGLE keyword).
;	Modified:	DAS, 2002-08-12 (complies with CONSTANTS.pro revision)
;       Modified:       SMH, 2016-03-12 Made my own stuff
;-

;***********************************************************************

FUNCTION GEO_DIST, $
	Longitude1, Latitude1, $
	Longitude2, Latitude2, $
        RADIUS_METERS=radius_meters, $
	SINGLE=singleopt

  COMPILE_OPT IDL2,STRICTARRSUBS

;***********************************************************************
;Define Constants

  pi     = 3.141592741

  rearth = KEYWORD_SET(radius_meters)? radius_meters : 6.370949e+6 ;default to earth radius in meters

  rearth = rearth / 1000.

  ;;Number of degrees in a circle
  ndegree = 360

  ;;Degrees to radians conversion coefficient
  degrad = pi / ndegree * 2.

;Double-precision arithmetic option
IF KEYWORD_SET( singleopt ) THEN degrad = float( degrad )

;***********************************************************************
;Cosine Formula (not used, but I thought I would leave it in)

;;Convert to Cartesian coordinates
;x1 = cos(longitude1*degrad) * cos(latitude1*degrad)
;y1 = sin(longitude1*degrad) * cos(latitude1*degrad)
;z1 = sin(latitude1*degrad)
;x2 = cos(longitude2*degrad) * cos(latitude2*degrad)
;y2 = sin(longitude2*degrad) * cos(latitude2*degrad)
;z2 = sin(latitude2*degrad)
;
;;Direction cosine
;dx = x1*x2+y1*y2+z1*z2
;
;;A fix if the |cosine| > 1
;id = where( dx gt 1, siz )
;if siz gt 0 then dx[id] = 1.
;
;;Output (distance)
;dist = float(rearth*acos(dx))

;***********************************************************************
;Haversine Formula

;Difference coordinates
dlon = degrad * ( longitude2 - longitude1 )
dlat = degrad * ( latitude2 - latitude1 )

;Main calculation
a = ( sin( dlat / 2. ) )^2 + cos( degrad * latitude1 ) $
    * cos( degrad * latitude2 ) * ( sin( dlon / 2. ) )^2
a = sqrt( a )

;A fix if a>1
id = where( a GT 1, nid )
IF nid GT 0 THEN a[id] = 1

;Convert to distance
dist = float( rearth * 2 * asin( a ) )

;***********************************************************************
;The End

RETURN, dist
END
