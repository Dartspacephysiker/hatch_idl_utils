;+
; NAME:
;        HIST2D__EQUAL_AREA_BINNING
;
; PURPOSE:
;        Return the weighted density function (histogram) of two variables, with each latitude and MLT bin being of equal area.
;
; CATEGORY:
;        Bin up that polar region
;
; CALLING SEQUENCE:
;        result = HIST2D__EQUAL_AREA_BINNING(MLTs, ILATS [,Weight, ...histogram keywords])
; INPUTS:
;        MLTs and ILATS = arrays containing the variables.  They MAY be
;             of ANY type, and MAY contain negative elements.
;
; OPTIONAL INPUTS:
;       Weight:    1D array of the same dimension as MLTs and ILATS which holds the
;             weighted values associated with each MLTs and ILATS element.
;
; INPUT KEYWORD PARAMETERS:
;        INPUT:    2D array to be added to the output of HIST2D.
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
;     BINEDGEn:    This keyword specfies the location of the bin values returned
;             by OBINn. The values are:
;
;                  0 : Center of each bin, [DEFAULT].
;                 -1 : Left  or lower edge of each bin.
;                  1 : Right or upper edge of each bin.
;
; OUTPUTS:
;        The two dimensional weighted density function of the two variables.
;        If Weight is not specified, then the usual, unweighted 2D histogram
;        is returned.
;
; OUTPUT KEYWORD PARAMETERS:
;
;      DENSITY:    Density function of MLTs and ILATS; i.e. the unweighted 2D histogram.
;
;        OBINn:    An array holding the bin values of the histogram of Vn. (n=1,2)
;
;        OMAXn:    A named variable that, upon exit, contains the maximum data
;             value used in constructing the histogram of Vn. (n=1,2)
;
;        OMINn:    A named variable that, upon exit, contains the minimum data
;             value used in constructing the histogram of Vn. (n=1,2)
;
; COMMON BLOCKS:
;        None.
; SIDE EFFECTS:
;        None.
; RESTRICTIONS:
;        None.
;
; EXAMPLE:
;        Return the 2D histogram of two byte images:
;             R = HIST_2D(image1, image2)
;        Return the 2D histogram made from two floating point images
;        with range of -1 to +1, and with 100 bins:
;             R = HIST_2D(f1,f2)
;
; MODIFICATION HISTORY:
;        Written by:
;        DMS, Sept, 1992     Written, IDL
;        28-JUL-1994         H.C. Wen, Formerly, HIST_2D, Expanded input
;                            array types, added weight option and added
;                            HISTOGRAM keywords
;        28-FEB-1996         Added the BINEDGE1, BINEDGE2 keywords.
;        14-OCT-2016         Spencer Hatch: Copped to use an equal-area binning scheme provided by Ryan McGranaghan. Stud!
;-
FUNCTION HIST2D__EQUAL_AREA_BINNING, MLTs, ILATS, Weight, $
                                     INPUT=Input, $
                                     MAX1=MaxMLT,MAX2=MaxILAT, $
                                     MIN1=MinMLT,MIN2=MinILAT,$
                                     OMAX1=OmaxMLT,OMAX2=OmaxILAT,OMIN1=OminMLT,OMIN2=OminILAT, $
                                     OBIN1=Obin1,OBIN2=Obin2, $
                                     DENSITY=Density, $
                                     BINEDGE1=Binedge1,BINEDGE2=Binedge2;; , $
                                     ;; EQUAL_AREA_STRUCT=EA

  COMPILE_OPT idl2

  @common__ea_binning.pro

  ;; ON_ERROR, 2                   ; on error, return control to caller


  ;; testFile = '/SPENCEdata/Research/database/equal-area_binning/tmpTest.sav'
  ;; LOAD_MAXIMUS_AND_CDBTIME,maximus,cdbtime, $
  ;;                          GOOD_I=good_i, $
  ;;                          HEMI__GOOD_I='NORTH', $
  ;;                          /DO_DESPUNDB
  ;; mlt          = maximus.mlt[good_i]
  ;; ilat         = maximus.ilat[good_i]
  ;; pflux        = maximus.pfluxest[good_i]
  ;; SAVE,mlt,ilat,pflux,FILENAME=testFile

  IF N_ELEMENTS(EA__s) EQ 0 THEN LOAD_EQUAL_AREA_BINNING_STRUCT
  ;; RESTORE,testFile

  ;; mlts = TEMPORARY(mlt)
  ;; ilats = TEMPORARY(ilat)
  ;; weight = TEMPORARY(pflux)

  ;; good_i = WHERE((pFlux GT 0 ) AND FINITE(pFlux))
  ;; mlts = (TEMPORARY(mlt))[good_i]
  ;; ilats = (TEMPORARY(ilat))[good_i]
  ;; weight = ALOG10((TEMPORARY(pflux))[good_i])


;   Check dimensions
         s1   = size(MLTs)
         s2   = size(ILATS)
         if (s1[1] ne s2[1]) then $
              message,'Array sizes of histogram variables incompatible.'

         wh   = N_ELEMENTS( Weight )   ;Check/initialize weighting option
         if wh gt 0 then begin
              sw   = size(weight)
              if (sw[1] ne s1[1]) then $
                   message,'Array size of weighted variables incompatible.'
              wgtc = weight
         endif else $
              wgtc = replicate( 1.,s1[1] )

         m1   = max(MLTs , min=mm1)
         m2   = max(ILATS, min=mm2)

;   Take care of INPUT KEYWORDS
         if n_elements( MAXMLT )  eq 0 then MaxMLT   = MAX([EA__s.maxM,EA__s.minM])
         if n_elements( MAXILAT ) eq 0 then MaxILAT  = MAX([EA__s.maxI,EA__s.minI])
         if n_elements( MINMLT )  eq 0 then MinMLT   = MIN([EA__s.maxM,EA__s.minM])
         if n_elements( MINILAT ) eq 0 then MinILAT  = MIN([EA__s.maxI,EA__s.minI])
         ;; if n_elements( BINSIZE1 ) eq 0 then Binsize1  = 1.0
         ;; if n_elements( BINSIZE2 ) eq 0 then Binsize2 = 1.0

;   Take care of OUTPUT KEYWORDS
         Obin1     = EA__s.minM + MEAN([[EA__s.minM],[EA__s.maxM]],DIMENSION=2)
         Obin2     = EA__s.minI + MEAN([[EA__s.minI],[EA__s.maxI]],DIMENSION=2)


;   Remove data points outside MAX/MIN range
         iin = WHERE( (MLTs le MaxMLT) AND (MLTs ge MinMLT) AND $
                       (ILATS le MaxILAT) AND (ILATS ge MinILAT), nin )


         if nin gt 0 then begin
              MLTsc   = MLTs[iin]
              ILATSc  = ILATS[iin]
              Wgtc    = Wgtc[iin]
         endif else begin
              MLTsc   = MLTs
              ILATSc  = ILATS
         endelse

         ;;Setup
         sum          = MAKE_ARRAY(N_ELEMENTS(MLTsc),VALUE=-2L,/LONG)
         latSwitch_i  = [0,WHERE((EA__s.mini[1:-1]-EA__s.mini[0:-2]) NE 0),N_ELEMENTS(EA__s.mini)-1] ;Because rows of latitudes are of unequal lengths

         FOR k=0,N_ELEMENTS(latSwitch_i)-2 DO BEGIN
            tmpInds        = [latSwitch_i[k]:(latSwitch_i[k+1]-1)]

            FOR kk=0,N_ELEMENTS(tmpInds)-1 DO BEGIN
               match       = WHERE((MLTsc GE EA__s.minM[tmpInds[kk]]) AND (MLTsc LT EA__s.maxM[tmpInds[kk]]) AND $
                             (ILATsc GE EA__s.minI[tmpInds[kk]]) AND (ILATsc LT EA__s.maxI[tmpInds[kk]]),/NULL)
               sum[match]  = tmpInds[kk]
               ;; PRINT,"Bro: ",tmpInds[kk],EA__s.minM[tmpInds[kk]],EA__s.maxM[tmpInds[kk]],EA__s.minI[tmpInds[kk]],EA__s.maxI[tmpInds[kk]],N_ELEMENTS(match)
            ENDFOR
         ENDFOR

         ;;Last by hand; necessary because I use latSwitch_i instead of something more s'fissicated
         match       = WHERE((MLTsc GE EA__s.minM[latSwitch_i[-1]]) AND (MLTsc LT EA__s.maxM[latSwitch_i[-1]]) AND $
                             (ILATsc GE EA__s.minI[latSwitch_i[-1]]) AND (ILATsc LT EA__s.maxI[latSwitch_i[-1]]),/NULL)
         sum[match]  = latSwitch_i[-1]
         

         ;; PRINT,'EEEQQQUUAAALLLLL'
         dat = WHERE(sum EQ -2,nDat)
         IF nDat GT 3 THEN BEGIN 
            FOR k=0,nDat-1 DO BEGIN
               STOP
               PRINT,"Bro: ",dat[k],MLTsc[dat[k]],ILATsc[dat[k]]
            ENDFOR
         ENDIF

         h = HIST1D(sum, Wgtc, MIN=0,MAX=latSwitch_i[-1],DENSITY=Density )

         RETURN,h
end
