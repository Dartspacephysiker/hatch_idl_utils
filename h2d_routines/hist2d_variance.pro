;2017/07/03
FUNCTION HIST2D_VARIANCE, V1, V2, Weight, means, BINSIZE1=Binsize1, BINSIZE2=Binsize2, $
                     INPUT=Input, MAX1=Max1, MAX2=Max2, MIN1=Min1, MIN2=Min2,$
                     OMAX1=Omax1, OMAX2=Omax2, OMIN1=Omin1, OMIN2=Omin2, $
                     OBIN1=Obin1, OBIN2=Obin2, DENSITY=Density, $
                     BINEDGE1=Binedge1, BINEDGE2=Binedge2

  COMPILE_OPT IDL2,STRICTARRSUBS

  ON_ERROR, 2                   ; on error, return control to caller

;   Check dimensions
  s1   = SIZE(V1)
  s2   = SIZE(V2)
  IF (s1[1] NE s2[1]) THEN $
     MESSAGE,'Array sizes of histogram variables incompatible.'

  wh   = N_ELEMENTS( Weight )   ;Check/initialize weighting option
  IF wh GT 0 THEN BEGIN
     sw   = SIZE(weight)
     IF (sw[1] NE s1[1]) THEN $
        MESSAGE,'Array size of weighted variables incompatible.'
     wgtc = weight
  ENDIF ELSE $
     wgtc = REPLICATE( 1.,s1[1] )

  IF N_ELEMENTS(means) EQ 0 THEN $
     MESSAGE,"No means (literally, 2D array of mu parameters) provided!"
  
  m1   = MAX(V1, MIN=mm1)
  m2   = MAX(V2, MIN=mm2)

;   Take care of INPUT KEYWORDS
  IF N_ELEMENTS( MAX1 ) EQ 0 THEN Max1 = m1
  IF N_ELEMENTS( MAX2 ) EQ 0 THEN Max2 = m2
  IF N_ELEMENTS( MIN1 ) EQ 0 THEN Min1 = mm1
  IF N_ELEMENTS( MIN2 ) EQ 0 THEN Min2 = mm2
  IF N_ELEMENTS( BINSIZE1 ) EQ 0 THEN Binsize1 = 1.0
  IF N_ELEMENTS( BINSIZE2 ) EQ 0 THEN Binsize2 = 1.0

;   Remove data points outside MAX/MIN range
  ;; iout = WHERE( (V1 gt Max1) or (V1 lt Min1) or $
  ;;               (V2 gt Max2) or (V2 lt Min2), nout )
  iout = WHERE( (V1 LE Max1) AND (V1 GE Min1) AND $
                (V2 LE Max2) AND (V2 GE Min2), nout )

  IF nout GT 0 THEN BEGIN
     V1c  = V1[iout]
     V2c  = V2[iout]
     Wgtc = Wgtc[iout]
  ENDIF ELSE BEGIN
     V1c  = V1
     V2c  = V2
  ENDELSE

;   DefiNE histogram parameters
  d1              = DOUBLE(Binsize1)
  d2              = DOUBLE(Binsize2)
  span1           = DOUBLE(Max1 - Min1)
  span2           = DOUBLE(Max2 - Min2)
  span1bNorm  = FLOOR(span1/d1)
  span2bNorm  = FLOOR(span2/d2)
  n1              = span1bNorm + 1L
  n2              = span2bNorm + 1L

  IF ARRAY_EQUAL(SIZE(means,/DIM),[n1,n2]) THEN BEGIN
     means1D      = REFORM(means,N_ELEMENTS(means))
     meansInds    = LINDGEN(N_ELEMENTS(means))
  ENDIF ELSE BEGIN
     STOP
     MESSAGE,"Means will never match the data you gots"
  ENDELSE
  
;   Take care of OUTPUT KEYWORDS
  Omax1 = Max1 & Omax2 = Max2
  Omin1 = Min1 & Omin2 = Min2

  IF (N_ELEMENTS( Binedge1 ) EQ 0) THEN Binedge1 = 0
  IF (N_ELEMENTS( Binedge2 ) EQ 0) THEN Binedge2 = 0
  offset1   = (Binedge1+1)*0.5
  offset2   = (Binedge2+1)*0.5
  Obin1     = Omin1 + (LINDGEN(n1)+offset1)*binsize1
  Obin2     = Omin2 + (LINDGEN(n2)+offset2)*binsize2

;   Scale V1c, V2c arrays into longword integer arrays
  V1scl   = FLOOR( span1bNorm*( V1c - Min1 )/span1 )
  V2scl   = FLOOR( span2bNorm*( V2c - Min2 )/span2 )

  
;   Fold into 1D histogram ...
  sum  = n1 * V2scl + V1scl

;  Align with means, subtract means, sqwar
  meanIntoWgt_i = VALUE_LOCATE(meansInds,sum)
  Wgtc          = (Wgtc-means1D[meanIntoWgt_i])^2

; ... and unfold back into 2D histogram
  h  = HIST1D(sum, Wgtc, MIN=0, MAX= n1 * n2 -1, DENSITY=Density )
  h  = REFORM(h, n1, n2, /OVERWRITE )
  Density = REFORM(Density, n1, n2, /OVERWRITE )

  nz_i = WHERE(density GT 0,nNZ)
  IF nNZ GT 0 THEN BEGIN
     h[nz_i] /= density[nz_i]
  ENDIF

  IF KEYWORD_SET( INPUT ) THEN h = h + input
  RETURN, h

END

