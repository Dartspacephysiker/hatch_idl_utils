;+
; NAME:                        GET_H2D_BIN_CENTERS
;
; PURPOSE:                     We're tired of not knowing what MLTs and ILATs we're dealing with.
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
; MODIFICATION HISTORY:      2016/01/30 Barnebarn
;                            2016/02/08 Added NBINS keywords
;
;-
PRO GET_H2D_BIN_CENTERS_OR_EDGES,centers, $
                                 EQUAL_AREA_BINNING=EA_binning, $
                                 CENTERS1=centers1,CENTERS2=centers2, $
                                 BINSIZE1=Binsize1, BINSIZE2=Binsize2, $
                                 MIN1=Min1, MIN2=Min2,$
                                 MAX1=Max1, MAX2=Max2,  $
                                 SHIFT1=shift1,SHIFT2=shift2, $
                                 OMIN1=Omin1, OMIN2=Omin2, $
                                 OMAX1=Omax1, OMAX2=Omax2,  $
                                 OBIN1=Obin1, OBIN2=Obin2, $
                                 NBINS1=nBins1, NBINS2=nBins2, $
                                 BINEDGE1=Binedge1, BINEDGE2=Binedge2

  ON_ERROR, 2                   ; on error, return control to caller
  
  IF KEYWORD_SET(EA_binning) THEN BEGIN
     
     @common__ea_binning.pro
     
     IF N_ELEMENTS(EA__s) EQ 0 THEN BEGIN
        LOAD_EQUAL_AREA_BINNING_STRUCT
     ENDIF

     IF (N_ELEMENTS( Binedge1 ) EQ 0) THEN Binedge1 = 0
     IF (N_ELEMENTS( Binedge2 ) EQ 0) THEN Binedge2 = 0

     centers      = MAKE_ARRAY(2,N_ELEMENTS(EA__s.minM))
     CASE binEdge1 OF
        -1: BEGIN
           centers[0,*] = EA__s.minM
        END
        0: BEGIN
           centers[0,*] = MEAN([[EA__s.minM],[EA__s.maxM]],DIMENSION=2)
        END
        1: BEGIN
           centers[0,*] = EA__s.maxM
        END
     ENDCASE

     CASE binEdge2 OF
        -1: BEGIN
           centers[1,*] = EA__s.minI
        END
        0: BEGIN
           centers[1,*] = MEAN([[EA__s.minI],[EA__s.maxI]],DIMENSION=2)
           ;; centers[1,*] = MEAN(EA__s.minI+EA__s.maxI)
        END
        1: BEGIN
           centers[1,*] = EA__s.maxI
        END
     ENDCASE

     RETURN
  ENDIF

;   Take care of INPUT KEYWORDS
         if n_elements( MAX1 ) eq 0 then Max1 = m1
         if n_elements( MAX2 ) eq 0 then Max2 = m2
         if n_elements( MIN1 ) eq 0 then Min1 = mm1
         if n_elements( MIN2 ) eq 0 then Min2 = mm2
         if n_elements( BINSIZE1 ) eq 0 then Binsize1 = 1.0
         if n_elements( BINSIZE2 ) eq 0 then Binsize2 = 1.0
         if n_elements( SHIFT1 ) eq 0 then shift1 = 0
         if n_elements( SHIFT2 ) eq 0 then shift2 = 0

;   Define histogram parameters
         d1   = double(Binsize1)
         d2   = double(Binsize2)
         w1   = double(Max1 - Min1)
         w2   = double(Max2 - Min2)
         I1m  = floor(w1/d1)
         I2m  = floor(w2/d2)
         n1   = I1m + 1L
         n2   = I2m + 1L

;   Take care of OUTPUT KEYWORDS
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
END