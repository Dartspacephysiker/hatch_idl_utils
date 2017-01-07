PRO DISTANCE_HISTO,imgArr, $
                   PIXVAL=pixVal, $
                   MASKVAL=maskVal, $
                   PIXRANGE=pixRange, $
                   PIXELRANGE_AND_VAL=pixRangeAndVal, $
                   DISPLAY_IMAGE=display_image, $
                   HIST_DOUBLECOUNT=doubleCount, $
                   HIST_BINSIZE=binSize, $
                   IMAGE_TITLE=imgTitle, $
                   EXAMPLE=example, $
                   WAYCHEAP_EXAMPLE=wayCheap

  COMPILE_OPT idl2

  IF (N_ELEMENTS(SIZE(imgArr,/DIMENSIONS)) NE 2) THEN BEGIN
     IF ~KEYWORD_SET(example) THEN BEGIN
        PRINT,"Not set up to handle this sitiation (and I do mean sitiation, not situation)!"
        STOP
     ENDIF
  ENDIF



  CASE 1 OF 
     KEYWORD_SET(wayCheap): BEGIN

        ;;ultimately want 4x6 matrix
        xVals = [0,1,4,5]
        yVals = [0,1,2,3,4,5]

        dist = [1, 4, 5, SQRT(2), SQRT(17), SQRT(26), $
                3, 4, SQRT(2), 1, SQRT(10), SQRT(17), $
                1, SQRT(17), SQRT(10), 1, SQRT(2), $
                SQRT(26), SQRT(17), SQRT(2), 1, $
                1, 4, 5, $
                3, 4, $
                1]

        CGHISTOPLOT,dist,BINSIZE=1

     END
     KEYWORD_SET(example): BEGIN

        IF N_ELEMENTS(imgArr) GT 0 THEN BEGIN
           PRINT,"Should I use provided imgArr?"
           STOP
        ENDIF

        pixVal   = 0B
        crossVal = 255B

        nx     = 20
        ny     = 20
        imgArr = MAKE_ARRAY(nx,ny,VALUE=pixVal,/BYTE)

        ;;Make a cross pattern
        ;; begFr = 3/8
        ;; endFr = 5/8
        begFr = FLOAT(2)/8
        endFr = FLOAT(6)/8

        xBegn = FIX(nx*begFr)
        xLast = FIX(nx*endFr)
        yBegn = FIX(ny*begFr)
        yLast = FIX(ny*endFr)
        imgArr[[xBegn:xLast],            *] = crossVal
        imgArr[            *,[yBegn:yLast]] = crossVal

        imgTitle = "Example image"

     END
     ELSE: BEGIN

        nx    = N_ELEMENTS(imgArr[*,0])
        ny    = N_ELEMENTS(imgArr[0,*])    

     END
  ENDCASE
  nTot        = nx*ny

  IF KEYWORD_SET(display_image) THEN BEGIN

     img   = IMAGE(imgArr,TITLE=imgTitle)

  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Figure out which pixels we're measuring distances between

  CASE 1 OF
     KEYWORD_SET(pixRangeAndVal): BEGIN

     END
     KEYWORD_SET(pixRange): BEGIN

        pRDims = SIZE(pixRange,/DIMENSIONS)

        CASE N_ELEMENTS(pRDims) OF
           1: BEGIN

           END
           2: BEGIN
              nIter = pRDims[1]
           END
           ELSE: BEGIN
              STOP
           END
        ENDCASE

        pR = pixRange
        FOR k=0,nIter-1 DO BEGIN
           IF pR[0,k] GT pR[1,k] THEN BEGIN
              tmp = pR[0,k]
              pR[0,k] = pR[1,k]
              pR[1,k] = TEMPORARY(tmp)
           ENDIF
        ENDFOR

        good_i = WHERE(imgArr GE pR[0,0] AND imgArr LE pR[1,0])
        FOR k=1,nIter-1 DO BEGIN
           good_i = CGSETUNION(good_i, $
                               WHERE(imgArr GE pR[0,k] AND imgArr LE pR[1,k]))
        ENDFOR
        
     END
     N_ELEMENTS(pixVal) GT 0: BEGIN
        good_i = WHERE(imgArr EQ pixVal)
     END
     N_ELEMENTS(maskVal) GT 0: BEGIN
        good_i = WHERE(imgArr NE maskVal)      
     END
  ENDCASE

  IF good_i[0] EQ -1 THEN BEGIN
     PRINT,"No indices matching pixel requirement!"
     STOP
  ENDIF

  ;;Rearrange so we have indices into img
  bad_i        = CGSETDIFFERENCE(LINDGEN(nTot),good_i)
  bad_i_arr    = ARRAY_INDICES(imgArr,bad_i )
  good_i_arr   = ARRAY_INDICES(imgArr,good_i)

  mask         = BYTE(imgArr)
  mask[*]      = 0B
  mask[bad_i]  = 1B
  
  ;;Get x and y vals for calculating distance
  xVals        = LINDGEN(nx)            # MAKE_ARRAY(ny,VALUE=1)
  yVals        = MAKE_ARRAY(nx,VALUE=1) # LINDGEN(ny) 
  ;; distances    = MAKE_ARRAY(NCHOOSEK(nTot,2),VALUE=0.,/FLOAT)
  distInd      = 0L

  ;;distances
  IF KEYWORD_SET(HIST1D) THEN BEGIN
     
     

  ENDIF ELSE BEGIN
     CASE 1 OF
        KEYWORD_SET(doubleCount): BEGIN

           distances    = MAKE_ARRAY(NCHOOSEK(nTot,2), $
                                     VALUE=0.,/FLOAT)

           FOR j=0,nTot-1 DO BEGIN

              IF ~mask[j] THEN BEGIN

                 FOR k=0,nTot-1 DO BEGIN

                    IF ~mask[k] THEN BEGIN
                       distances[distInd++] = SQRT((xVals[k]-xVals[j])^2+(yVals[k]-yVals[j])^2)
                    ENDIF

                 ENDFOR

              ENDIF

           ENDFOR

           junk = WHERE(distances EQ 0,COMPLEMENT=notJunk)
           IF junk[0] NE -1 THEN BEGIN
              distances = distances[notJunk]
           ENDIF


        END
        ELSE: BEGIN

           distances    = MAKE_ARRAY(NCHOOSEK(N_ELEMENTS(WHERE(~mask)),2), $
                                     VALUE=0.,/FLOAT)

           FOR j=0,nTot-1 DO BEGIN

              IF ~mask[j] THEN BEGIN

                 FOR k=j+1,nTot-1 DO BEGIN

                    IF ~mask[k] THEN BEGIN
                       distances[distInd++] = SQRT((xVals[k]-xVals[j])^2+(yVals[k]-yVals[j])^2)
                    ENDIF

                 ENDFOR

              ENDIF

           ENDFOR

        END
     ENDCASE

     CGHISTOPLOT,distances,$
                 BINSIZE=binSize, $
                 TITLE="Distance histogram"

  ENDELSE

  STOP

END

