;Seems like using the code to make those darn orbit plots is a good idear
;We only need to do this ONCE for all median plots
;I've completely ripped this code off and modded it.
;It comes from SLAC's hist2d.pro

;KEYWORDS
;Setting ABSMED will calculate the median of the absolute values of data

;PTRHIST will return the ptrHist used to create the arrays of data for
;a given histo cell


function median_hist, MLT, ILAT, data, $
                 BINSIZE1=Binsize1, BINSIZE2=Binsize2, $
                 INPUT=Input, MAX1=Max1, MAX2=Max2, MIN1=Min1, MIN2=Min2,$
                 OMAX1=Omax1, OMAX2=Omax2, OMIN1=Omin1, OMIN2=Omin2, $
                 OBIN1=Obin1, OBIN2=Obin2, DENSITY=Density, $
                 BINEDGE1=Binedge1, BINEDGE2=Binedge2, NONZERO=nonzeroHist,$
                 PTRHIST=ptrHist,ABSMED=absMed

         ON_ERROR, 2          ; on error, return control to caller

;   Check dimensions
         s1   = size(MLT)
         s2   = size(ILAT)
         if (s1(1) ne s2(1)) then $
              message,'Array sizes of histogram variables incompatible.'

         wh   = N_ELEMENTS( Data )   ;Check/initialize dataing option
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
         if not keyword_set( MIN1 ) then Min1 = mm1
         if not keyword_set( MIN2 ) then Min2 = mm2
         if not keyword_set( BINSIZE1 ) then Binsize1 = 1.0
         if not keyword_set( BINSIZE2 ) then Binsize2 = 1.0
         IF KEYWORD_SET(ABSMED) THEN data = ABS(data)

;   Remove data points outside MAX/MIN range
         iout = WHERE( (MLT gt Max1) or (MLT lt Min1) or $
                       (ILAT gt Max2) or (ILAT lt Min2), nout )

         if nout gt 0 then begin
              MLTc  = MLT(iout)
              ILATc  = ILAT(iout)
              Wgtc = Wgtc(iout)
         endif else begin
              MLTc  = MLT
              ILATc  = ILAT
         endelse

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
         
;         PRINT,"Obin1:"
;         PRINT,Obin1
;         PRINT,"Obin2:"
;         PRINT,Obin2
         ptrHist = PTRARR(n1,n2,/ALLOCATE_HEAP)
         nonzeroHist=BYTARR(n1,n2)
         medHist=DBLARR(n1,n2)

;         PRINT,"n1 is " + string(n1) + " and n2 is " + string(n2)

         FOR i=0, n_elements(MLTc)-1 DO BEGIN
            smallMLT=MIN(ABS(Obin1-MLTc[i]),binMLT)
            smallILAT=MIN(ABS(Obin2-ILATc[i]),binILAT)
            IF *ptrHist[binMLT,binILAT] EQ !NULL THEN $
               *ptrHist[binMLT,binILAT]= wgtc[i] $
            ELSE *ptrHist[binMLT,binILAT]= [*ptrHist[binMLT,binILAT],wgtc[i]]
            nonzeroHist[binMLT,binILAT]=1
;            print,MLTc[i],binMLT,smallMLT
;            print,ILATc[i],binILAT,smallILAT
;            print,wgtc[i]
         ENDFOR 

;    Now calculate the median of each bin using ptrHist
         hist_i=WHERE(nonzeroHist GT 0)
         FOR i=0, N_ELEMENTS(hist_i)-1 DO BEGIN
            IF N_ELEMENTS(*ptrHist(hist_i[i])) GT 1 THEN $
              medHist(hist_i[i])=MEDIAN(*ptrHist(hist_i[i])) $
            ELSE medHist(hist_i[i])=*ptrHist(hist_i[i])       
         ENDFOR   
         RETURN,medHist
end
