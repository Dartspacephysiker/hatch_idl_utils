;Seems like using the code to make those darn orbit plots is a good idear
;We only need to do this ONCE for all median plots
;I've completely ripped this code off and modded it.
;It comes from SLAC's hist2d.pro

;KEYWORDS
;Setting ABSMED will calculate the median of the absolute values of data

;PTRHIST will return the ptrHist used to create the arrays of data for
;a given histo cell

;01/08/2016
; Added MAXIMUM keyword in case we want the max val.
;02/18/2015
;OUTFILE and PLOT_I are intended to provide more information about the statistics involved
;in each dataset corresponding to a given MLT/ILAT bin. PLOT_I is otherwise useless for this routine


FUNCTION MEDIAN_HIST,MLT,ILAT,data, $
                     MAXIMUM=maximum, $
                     MINIMUM=minimum, $
                      BINSIZE1=Binsize1, BINSIZE2=Binsize2, $
                      INPUT=Input, MAX1=Max1, MAX2=Max2, MIN1=Min1, MIN2=Min2,$
                      OMAX1=Omax1, OMAX2=Omax2, OMIN1=Omin1, OMIN2=Omin2, $
                      OBIN1=Obin1, OBIN2=Obin2, $
                      BINEDGE1=Binedge1, BINEDGE2=Binedge2, NONZEROHIST=nonzeroHist,$
                      PTRHIST=ptrHist,ABSMED=absMed,$
                      OUTFILE=outFile,PLOT_I=plot_i,LOGNORMSTATS=logNormStats, $
                      EQUAL_AREA_BINNING=EA_binning

  ON_ERROR, 0                   ; on error, return control to caller

  @common__ea_binning.pro

;   Check dimensions
  s1   = size(MLT)
  s2   = size(ILAT)
  if (s1(1) ne s2(1)) then $
     message,'Array sizes of histogram variables incompatible.'

  wh   = N_ELEMENTS( Data )     ;Check/initialize dataing option
  if wh gt 0 then begin
     sw   = size(data)
     if (sw(1) ne s1(1)) then $
        message,'Array size of dataed variables incompatible.'
     wgtc = data
  endif else $
     wgtc = replicate( 1.,s1(1) )

  m1   = max(MLT, min=mm1)
  m2   = max(ILAT, min=mm2)

;   Take care of INPUT KEYWORDS
  if not keyword_set( MAX1 ) then Max1 = m1
  if not keyword_set( MAX2 ) then Max2 = m2
  ;; if not keyword_set( MIN1 ) then Min1 = mm1
  if MIN1 EQ !NULL  then Min1 = mm1
  if not keyword_set( MIN2 ) then Min2 = mm2
  if not keyword_set( BINSIZE1 ) then Binsize1 = 1.0
  if not keyword_set( BINSIZE2 ) then Binsize2 = 1.0
  IF KEYWORD_SET(medHist) THEN data = ABS(data)

;   Remove data points outside MAX/MIN range
  ;; iout = WHERE( (MLT gt Max1) or (MLT lt Min1) or $
  ;;               (ILAT gt Max2) or (ILAT lt Min2), nout )
  iout = WHERE( (MLT le Max1) AND (MLT ge Min1) AND $
                (ILAT le Max2) AND (ILAT ge Min2), nout )

  if nout gt 0 then begin
     MLTc   = MLT[iout]
     ILATc  = ILAT[iout]
     Wgtc   = Wgtc[iout]
  endif else begin
     MLTc   = MLT
     ILATc  = ILAT
  endelse

  CASE 1 OF
     KEYWORD_SET(EA_binning): BEGIN
        nBins        = N_ELEMENTS(EA__s.minI)

        ptrHist      = PTRARR(nBins,/ALLOCATE_HEAP)
        nonzeroHist  = BYTARR(nBins)
        medHist      = DBLARR(nBins)

        n_loop       = N_ELEMENTS(MLTc)-1
        ;; print,"n_loop is " + str(n_loop)
        FOR i=0,n_loop DO BEGIN
           ;; smallMLT  = MIN(ABS(Obin1-MLTc[i]),bM)
           ;; smallILAT = MIN(ABS(Obin2-ILATc[i]),bI)
           bInd      = WHERE((MLTc[i]  GE EA__s.minM) AND $
                             (MLTc[i]  LT EA__s.maxM) AND $
                             (ILATc[i] GE EA__s.minI) AND $
                             (ILATc[i] LT EA__s.maxI),nInd)
           ;; IF bInd[0] NE -1 AND (nInd EQ 1) THEN BEGIN
           IF bInd[0] NE -1 THEN BEGIN
              
              ;;diagnostic
              IF nInd GT 1 THEN BEGIN
                 PRINT,'What? How can this correspond to more than one bin?'
                 STOP
              ENDIF

              IF *(ptrHist[bInd])[0] EQ !NULL THEN BEGIN
                 *(ptrHist[bInd])[0]   = wgtc[i] 
                 nonzeroHist[bInd] = 1
              ENDIF ELSE BEGIN
                 *(ptrHist[bInd])[0]    = [*(ptrHist[bInd])[0],wgtc[i]]
              ENDELSE
           ENDIF
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

;   Take care of OUTPUT KEYWORDS
        Omax1 = Max1 & Omax2 = Max2
        Omin1 = Min1 & Omin2 = Min2

        if (N_ELEMENTS( Binedge1 ) eq 0) then Binedge1 = 0
        if (N_ELEMENTS( Binedge2 ) eq 0) then Binedge2 = 0
;   Set bin edges to one so that we measure from the right edge of
;   every bin         
;         Binedge1=1
;         Binedge2=2
        offset1   = (Binedge1+1)*0.5
        offset2   = (Binedge2+1)*0.5
        Obin1     = Omin1 + (lindgen(n1)+offset1)*binsize1
        Obin2     = Omin2 + (lindgen(n2)+offset2)*binsize2
        
        ;; PRINT,"Obin1:"
        ;; PRINT,Obin1
        ;; PRINT,"Obin2:"
        ;; PRINT,Obin2
        ptrHist      = PTRARR(n1,n2,/ALLOCATE_HEAP)
        nonzeroHist  = BYTARR(n1,n2)
        medHist      = DBLARR(n1,n2)

        n_loop = N_ELEMENTS(MLTc)-1
        ;; print,"n_loop is " + str(n_loop)
        FOR i=0,n_loop DO BEGIN
           smallMLT  = MIN(ABS(Obin1-MLTc[i]),bM)
           smallILAT = MIN(ABS(Obin2-ILATc[i]),bI)
           IF *ptrHist[bM,bI] EQ !NULL THEN BEGIN
              *ptrHist[bM,bI]    = wgtc[i] 
              nonzeroHist[bM,bI] = 1
           ENDIF ELSE BEGIN
              *ptrHist[bM,bI]    = [*ptrHist[bM,bI],wgtc[i]]
           ENDELSE
;            print,MLTc[i],bM,smallMLT
;            print,ILATc[i],bI,smallILAT
;            print,wgtc[i]
        ENDFOR 

     END
  ENDCASE

  hist_i = WHERE(nonzeroHist GT 0)
  IF KEYWORD_SET(logNormStats) THEN BEGIN
     IF N_ELEMENTS(*(ptrHist(hist_i[i]))) EQ 1 THEN BEGIN
        ;;geometric mean of 1 sample is just the sample itself
        ;;also, geometric standard deviation 
        medHist(hist_i[i]) = *(ptrHist(hist_i[i])) 
     ENDIF ELSE BEGIN
        ;;trickier here
        CASE 1 OF
           KEYWORD_SET(maximum): BEGIN
              medHist(hist_i[i]) = MAX(*ptrHist(hist_i[i]))
           END
           KEYWORD_SET(minimum): BEGIN
              medHist(hist_i[i]) = MIN(*ptrHist(hist_i[i]))
           END
           ELSE: BEGIN
              medHist(hist_i[i]) = MEDIAN(*ptrHist(hist_i[i]))
           END
        ENDCASE
     ENDELSE
     
  ENDIF ELSE BEGIN
;    Now calculate the median of each bin using ptrHist
;    ...if we're not doing log-normal stats, that is
     FOR i=0,N_ELEMENTS(hist_i)-1 DO BEGIN
        IF N_ELEMENTS(*(ptrHist(hist_i[i]))) EQ 1 THEN BEGIN
           medHist(hist_i[i]) = *(ptrHist(hist_i[i])) 
        ENDIF ELSE BEGIN
           CASE 1 OF
              KEYWORD_SET(maximum): BEGIN
                 medHist(hist_i[i]) = MAX(*ptrHist(hist_i[i]))
              END
              KEYWORD_SET(minimum): BEGIN
                 medHist(hist_i[i]) = MIN(*ptrHist(hist_i[i]))
              END
              ELSE: BEGIN
                 medHist(hist_i[i]) = MEDIAN(*ptrHist(hist_i[i]))
              END
           ENDCASE

        ENDELSE
     ENDFOR
  ENDELSE

  IF KEYWORD_SET(outFile) THEN BEGIN
     IF (STRLOWCASE(TYPENAME(outFile)) EQ 'string') THEN BEGIN
        save,ptrHist,nonzeroHist, $
             obin1,obin2,Omax1,Omax2,Omin1,Omin2,Binedge1,Binedge2, $
             plot_i,FILENAME=outFile
     ENDIF ELSE PRINT,"medianHist: 'outFile' must be a string!"
  ENDIF
  
  RETURN,medHist
end
