;+
; NAME:                        GET_H2D_BIN_LENGTHS
;
; PURPOSE:                     And now you wanna know the lengths, too?!?!?!
;
; CATEGORY:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: (From HIST2D, no doubt)

;     BINSIZEn:    The size of the bin to use for the Vn array. If this
;             keyword is not specified, a bin size of 1 is used. (n = 1,2)
;
;         MAXn:    The maximum value to consider in the histogram of the Vn
;             array.  If this keyword is not specified, Vn is searched for
;             its largest value. (n = 1,2)
;
;         MINn:    The minimum value to consider in the histogram of the Vn
;             array.  If this keyword is not specified, and Vn is of type
;             byte, 0 is used. If this keyword is not specified and Vn is
;             not of byte type, Vn is searched for its smallest value. (n=1,2)
;
;       SHIFTn:    Yes, you could just shift MAX and MIN appropriately, but 
;             I'd rather keep things straight in my head.

;     BINEDGEn:    This keyword specfies the location of the bin values returned
;             by OBINn. The values are:
;
;                  0 : Center of each bin, [DEFAULT].
;                 -1 : Left  or lower edge of each bin.
;                  1 : Right or upper edge of each bin.
;
; OUTPUTS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:      2016/03/12 Barnebarn
;
;-
PRO GET_H2D_BIN_LENGTHS,lengths, $
                        LONGITUDINAL=longitudinal, $
                        LATITUDINAL=latitudinal, $
                        CENTERS1=centers1,CENTERS2=centers2, $
                        BINSIZE1=Binsize1, BINSIZE2=Binsize2, $
                        MIN1=Min1, MIN2=Min2,$
                        MAX1=Max1, MAX2=Max2,  $
                        SHIFT1=shift1,SHIFT2=shift2, $
                        EQUAL_AREA_BINNING=EA_binning, $
                        OMIN1=Omin1, OMIN2=Omin2, $
                        OMAX1=Omax1, OMAX2=Omax2,  $
                        OBIN1=Obin1, OBIN2=Obin2, $
                        NBINS1=nBins1, NBINS2=nBins2 ;, $
  ;; BINEDGE1=Binedge1, BINEDGE2=Binedge2
  
  COMPILE_OPT idl2 

  ON_ERROR, 2                   ; on error, return control to caller

  ;;These are both set to -1 because we use the lower and lefthand edges as well as the respective binsizes to calculate area
  Binedge1                             = -1
  Binedge2                             = -1

  ;; Take care of INPUT KEYWORDS
  if n_elements( MAX1 ) eq 0 then Max1 = m1
  if n_elements( MAX2 ) eq 0 then Max2 = m2
  if n_elements( MIN1 ) eq 0 then Min1 = mm1
  if n_elements( MIN2 ) eq 0 then Min2 = mm2
  if n_elements( BINSIZE1 ) eq 0 then Binsize1 = 1.0
  if n_elements( BINSIZE2 ) eq 0 then Binsize2 = 1.0
  if n_elements( SHIFT1 ) eq 0 then shift1 = 0
  if n_elements( SHIFT2 ) eq 0 then shift2 = 0

  CASE 1 OF
     KEYWORD_SET(EA_binning): BEGIN

        LOAD_EQUAL_AREA_STRUCT,EA
        nBins = N_ELEMENTS(EA.minI)

        EA.minM *= 15.
        EA.maxM *= 15.

;   Take care of OUTPUT KEYWORDS
        Omax1 = Max1 & Omax2 = Max2
        Omin1 = Min1 & Omin2 = Min2

        if (N_ELEMENTS( Binedge1 ) eq 0) then Binedge1 = 0
        if (N_ELEMENTS( Binedge2 ) eq 0) then Binedge2 = 0
        offset1   = (Binedge1+1)*0.5
        offset2   = (Binedge2+1)*0.5
        Obin1     = EA.minM + offset1*binsize1+shift1
        Obin2     = EA.minI + offset2*binsize2+shift2

        centers1  = TEMPORARY(Obin1)
        centers2  = TEMPORARY(Obin2)
        centers   = TRANSPOSE([[[centers1]],[[centers2]]],[2,0,1])

        nBins1    = nBins
        nBins2    = nBins

        lengths     = MAKE_ARRAY(nBins,/DOUBLE)
        ;;first index is MLT, second index is ILAT
        FOR i=0,nBins1-1 DO BEGIN
              lon1 = EA.minM[i]
              lat1 = EA.minI[i]
              lon2 = EA.maxM[i]
              lat2 = EA.maxI[i]
              CASE 1 OF
                 KEYWORD_SET(latitudinal): BEGIN
                    lengths[i] = GEO_DIST(lon1,lat1, $
                                          lon1,lat2, $
                                          RADIUS_METERS=6.470949e+6) ;use radius at 100 km
                 END
                 KEYWORD_SET(longitudinal): BEGIN
                    lengths[i] = GEO_DIST(lon1,lat1, $
                                          lon2,lat1, $
                                          RADIUS_METERS=6.470949e+6) ;use radius at 100 km
                 END
              ENDCASE
        ENDFOR

     END
     ELSE: BEGIN
;   Define histogram parameters
        d1   = double(Binsize1)
        d2   = double(Binsize2)
        w1   = double(Max1 - Min1)
        w2   = double(Max2 - Min2)
        I1m  = floor(w1/d1)
        I2m  = floor(w2/d2)
        n1   = I1m + 1L
        n2   = I2m + 1L

        ;;   Take care of OUTPUT KEYWORDS
        Omax1 = Max1 & Omax2 = Max2
        Omin1 = Min1 & Omin2 = Min2

        if (N_ELEMENTS( Binedge1 ) eq 0) then Binedge1 = 0
        if (N_ELEMENTS( Binedge2 ) eq 0) then Binedge2 = 0
        offset1   = (Binedge1+1)*0.5
        offset2   = (Binedge2+1)*0.5
        Obin1     = Omin1 + (lindgen(n1)+offset1)*binsize1+shift1
        Obin2     = Omin2 + (lindgen(n2)+offset2)*binsize2+shift2

        centers1  = rebin(obin1,n1,n2)
        centers2  = rebin(reform(obin2,1,n2),n1,n2)
        centers   = transpose([[[centers1]],[[centers2]]],[2,0,1])

        nBins1    = n1
        nBins2    = n2

        lengths   = MAKE_ARRAY(n1,n2,/DOUBLE)
        ;;first index is MLT, second index is ILAT
        FOR i1 = 0,n1-1 DO BEGIN
           FOR i2 = 0,n2-1 DO BEGIN
              lon1 = centers1[i1,i2]
              lat1 = centers2[i1,i2]
              lon2 = centers1[i1,i2]+binsize1
              lat2 = centers2[i1,i2]+binsize2
              CASE 1 OF
                 KEYWORD_SET(latitudinal): BEGIN
                    lengths[i1,i2] = GEO_DIST(lon1,lat1, $
                                              lon1,lat2, $
                                              RADIUS_METERS=6.470949e+6) ;use radius at 100 km
                 END
                 KEYWORD_SET(longitudinal): BEGIN
                    lengths[i1,i2] = GEO_DIST(lon1,lat1, $
                                              lon2,lat1, $
                                              RADIUS_METERS=6.470949e+6) ;use radius at 100 km
                 END
              ENDCASE
           ENDFOR
        ENDFOR
     END
  ENDCASE
END