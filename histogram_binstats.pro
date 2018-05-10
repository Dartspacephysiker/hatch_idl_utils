;2018/05/02
;
;Suppose you've got a bunch of kappa values,each one associated with an
;invariant latitude. You want to know some stats on the kappa values within each
;invariant latitude bin, right? So here's what you do: call this function with
;the ILAT values in the "histoData" position, and the kappa values in the
;"refData" position. Voilà—kappa stats within each ILAT bin.
FUNCTION HISTOGRAM_BINSTATS,histoData,refData, $
                            BINSIZE=binSize, $
                            MIN=min, $
                            MAX=max, $
                            NAN=NaN, $
                            NBINS=nBins, $
                            OMAX=oMax, $
                            OMIN=oMin, $
                            LOCATIONS=locations, $
                            REVERSE_INDICES=reverse_indices, $
                            HISTOGRAM=histogram, $
                            NMINBINFORINCLUSION=NMinBinForInclusion, $
                            GIVE_DECILES=give_deciles, $
                            GIVE_VENTILES=give_ventiles
                            

  COMPILE_OPT IDL2,STRICTARRSUBS

  NMinBinForI = N_ELEMENTS(NMinBinForInclusion) GT 0 ? NMinBinForInclusion : 1

  histogram = HISTOGRAM(histoData, $
                        BINSIZE=binSize, $
                        MIN=min, $
                        MAX=max, $
                        NAN=NaN, $
                        NBINS=nBins, $
                        OMAX=oMax, $
                        OMIN=oMin, $
                        LOCATIONS=locations, $
                        REVERSE_INDICES=ri)

  nLocs            = N_ELEMENTS(locations)
  hist0            = (MAKE_ARRAY(1,VALUE=0,TYPE=(SIZE(histoData,/TYPE))))[0]

  ;; Are we referencing 
  wasGivenRefData  = N_ELEMENTS(refData) GT 0 
  rD               = wasGivenRefData ? TEMPORARY(refData) : TEMPORARY(histoData)

  this     = MAKE_ARRAY(nLocs,VALUE=0,TYPE=SIZE(rD,/TYPE))
  this     = this[0]
  statTmp  = {lEdge   : hist0, $
              N       : 0L  , $
              min     : this, $
              max     : this, $
              median  : this, $
              mean    : this, $
              stddev  : this, $
              bpd     : MAKE_ARRAY(5,VALUE=this,TYPE=SIZE(rD,/TYPE))}

  IF KEYWORD_SET(give_deciles) THEN BEGIN
     statTmp = CREATE_STRUCT(statTmp, $
                             'decile', $
                             {val:MAKE_ARRAY(9, $
                                             VALUE=this, $
                                             TYPE=SIZE(rD,/TYPE)), $
                              fraction: MAKE_ARRAY(9,VALUE=0.0,/FLOAT)})
  ENDIF
  IF KEYWORD_SET(give_ventiles) THEN BEGIN
     statTmp = CREATE_STRUCT(statTmp, $
                             'ventile', $
                             {val:MAKE_ARRAY(19, $
                                             VALUE=this, $
                                             TYPE=SIZE(rD,/TYPE)), $
                              fraction: MAKE_ARRAY(19,VALUE=0.0,/FLOAT)})
  ENDIF
  stats    = REPLICATE(statTmp,nLocs)

  IF KEYWORD_SET(NaN) THEN BEGIN
     fin_i = WHERE(FINITE(rD),nFinite)

     IF nFinite EQ 0 THEN BEGIN
        PRINT,"ALL BAD!"
        STOP
     ENDIF

  ENDIF

  FOR k=0,nLocs-1 DO BEGIN

     stats[k].lEdge   = locations[k]

     IF ri[k] NE ri[k+1] THEN BEGIN

        tmpInds = ri[ri[k]:(ri[k+1]-1)]

        IF KEYWORD_SET(NaN) THEN BEGIN
           tmpInds = CGSETINTERSECTION(tmpInds,fin_i,COUNT=tmpCount)
        ENDIF

        tmpCount = N_ELEMENTS(tmpInds)

        IF tmpCount LT NMinBinForI THEN BEGIN
           stats[k].N       = tmpCount
           stats[k].min     = !VALUES.F_NaN
           stats[k].max     = !VALUES.F_NaN
           stats[k].median  = !VALUES.F_NaN
           stats[k].mean    = !VALUES.F_NaN
           stats[k].stddev  = !VALUES.F_NaN
           stats[k].bpd[*]  = !VALUES.F_NaN

           IF KEYWORD_SET(give_deciles) THEN BEGIN
              stats[k].decile.val       = !VALUES.F_NaN
              stats[k].decile.fraction  = !VALUES.F_NaN
           ENDIF

           IF KEYWORD_SET(give_ventiles) THEN BEGIN
              stats[k].ventile.val       = !VALUES.F_NaN
              stats[k].ventile.fraction  = !VALUES.F_NaN
           ENDIF

           CONTINUE
        ENDIF

        CASE tmpCount OF
           1: BEGIN
              tmpDat = rD[tmpInds[0]]
              stats[k].N       = 1
              stats[k].min     = tmpDat
              stats[k].max     = tmpDat
              stats[k].median  = tmpDat
              stats[k].mean    = tmpDat
              stats[k].stddev  = tmpDat
              stats[k].bpd[*]  = tmpDat
           END
           ELSE: BEGIN
              stats[k].N       = tmpCount
              stats[k].min     = MIN(rD[tmpInds])
              stats[k].max     = MAX(rD[tmpInds])
              stats[k].median  = MEDIAN(rD[tmpInds])
              stats[k].mean    = MEAN(rD[tmpInds])
              stats[k].stddev  = STDDEV(rD[tmpInds])
              stats[k].bpd     = CREATEBOXPLOTDATA(rD[tmpInds])
           END
        ENDCASE

        IF KEYWORD_SET(give_deciles) THEN BEGIN

           IF N_ELEMENTS(tmpInds) LT 10 THEN BEGIN
              PRINT,"Can't give deciles!"
              CONTINUE
           ENDIF

           predictedInds = ROUND(tmpCount*((FINDGEN(10))[1:-1])/10.)

           sortTmpInds = tmpInds[SORT(rD[tmpInds])]

           ;; Verify 
           FOR kk=1,9 DO BEGIN

              cont = 0
              WHILE ~cont DO BEGIN
                 decile_i = WHERE(rD[sortTmpInds] LE $
                                  rD[sortTmpInds[predictedInds[kk-1]]],nDecile, $
                                  NCOMPLEMENT=nCompDecile)

                 IF (FLOAT(nDecile)/tmpCount) GT (0.1*kk) THEN BEGIN
                    predictedInds[kk-1] = predictedInds[kk-1]-1
                 ENDIF ELSE BEGIN

                    stats[k].decile.val[kk-1] = rD[sortTmpInds[predictedInds[kk-1]]]
                    stats[k].decile.fraction[kk-1] = (FLOAT(nDecile)/tmpCount)

                    cont = 1
                 ENDELSE

              ENDWHILE

           ENDFOR

        ENDIF

        IF KEYWORD_SET(give_ventiles) THEN BEGIN

           IF N_ELEMENTS(tmpInds) LT 20 THEN BEGIN
              PRINT,"Can't give ventiles!"
              CONTINUE
           ENDIF

           predictedInds = ROUND(tmpCount*((FINDGEN(20))[1:-1])/20.)

           sortTmpInds = tmpInds[SORT(rD[tmpInds])]

           ;; Verify 
           FOR kk=1,19 DO BEGIN

              cont = 0
              WHILE ~cont DO BEGIN
                 ventile_i = WHERE(rD[sortTmpInds] LE $
                                   rD[sortTmpInds[predictedInds[kk-1]]], $
                                   nVentile, $
                                   NCOMPLEMENT=nCompVentile)

                 IF (FLOAT(nVentile)/tmpCount) GT (0.1*kk) THEN BEGIN
                    predictedInds[kk-1] = predictedInds[kk-1]-1
                 ENDIF ELSE BEGIN

                    stats[k].ventile.val[kk-1] = rD[sortTmpInds[predictedInds[kk-1]]]
                    stats[k].ventile.fraction[kk-1] = (FLOAT(nVentile)/tmpCount)
                    cont = 1
                 ENDELSE

              ENDWHILE

           ENDFOR

        ENDIF

     ENDIF ELSE BEGIN

        stats[k].N       = 0
        stats[k].min     = !VALUES.F_NaN
        stats[k].max     = !VALUES.F_NaN
        stats[k].median  = !VALUES.F_NaN
        stats[k].mean    = !VALUES.F_NaN
        stats[k].stddev  = !VALUES.F_NaN
        stats[k].bpd[*]  = !VALUES.F_NaN

        IF KEYWORD_SET(give_deciles) THEN BEGIN
           stats[k].decile.val       = !VALUES.F_NaN
           stats[k].decile.fraction  = !VALUES.F_NaN
        ENDIF

        IF KEYWORD_SET(give_ventiles) THEN BEGIN
           stats[k].ventile.val       = !VALUES.F_NaN
           stats[k].ventile.fraction  = !VALUES.F_NaN
        ENDIF

     ENDELSE

  ENDFOR

  IF ARG_PRESENT(reverse_indices) THEN BEGIN
     reverse_indices = TEMPORARY(ri)
  ENDIF

  IF wasGivenRefData THEN BEGIN
     refData = TEMPORARY(rD)
  ENDIF ELSE BEGIN
     histoData = TEMPORARY(rD)
  ENDELSE

  RETURN,stats

END
