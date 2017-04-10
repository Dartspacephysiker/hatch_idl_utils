;2017/04/08
PRO PLOT_EN_SPECS__DIFF_EFLUX,diff_eFlux_files, $
                              PLOT_T1=plot_t1, $
                              PLOT_T2=plot_t2, $
                              EEB_OR_EES=eeb_or_ees, $
                              ANGLE_RANGES=angle_ranges, $
                              ENERGY_RANGES=energy_ranges, $
                              NAMES=names, $
                              UNITS=units, $
                              SAVEPLOT=savePlot, $
                              SPNAME=spName, $
                              MOVIE=movie, $
                              PLOTDIR=plotDir, $
                              UPGOING=upgoing, $
                              IN_SOURCECONE=sourcecone, $
                              IN_LOSSCONE=losscone, $
                              _EXTRA=e


  COMPILE_OPT IDL2,STRICTARRSUBS

  IF SIZE(spName,/TYPE) EQ 7 THEN BEGIN

     splitter   = STRSPLIT(spName,'.',/EXTRACT)
     use_spName = splitter[0]

     IF N_ELEMENTS(splitter) GT 1 THEN BEGIN
        eps     = (WHERE(STRMATCH(splitter[1],'eps',/FOLD_CASE)))[0] NE -1
     ENDIF
  ENDIF

  eRangeDim = NDIMEN(energy_ranges)
  aRangeDim = NDIMEN(angle_ranges )

  CASE eRangeDim OF
     0: BEGIN
        nERanges = 0
     END
     1: BEGIN
        IF N_ELEMENTS(energy_ranges) NE 2 THEN STOP
        nERanges = 1
     END
     2: BEGIN
        nERanges = N_ELEMENTS(energy_ranges[0,*])
     END
  ENDCASE

  CASE aRangeDim OF
     0: BEGIN
        nARanges  = 1
        aRangeDim = 1
        angle_ranges = [0,360]
     END
     1: BEGIN

        CASE SIZE(angle_ranges,/TYPE) OF
           7: BEGIN

              aRangeDim = 2
              nARanges  = N_ELEMENTS(angle_ranges)

              IF N_ELEMENTS(upgoing) EQ 0 THEN BEGIN
                 upgoing = REPLICATE(0,nARanges)
              ENDIF
              
              stuff_lc  = WHERE(STRMATCH(angle_ranges,'lc*',/FOLD_CASE) AND upgoing,nLC)
              stuff_sc  = WHERE(STRMATCH(angle_ranges,'lc*',/FOLD_CASE) AND ~upgoing,nSC)
              stuff_all = WHERE(STRMATCH(angle_ranges,'all',/FOLD_CASE),nAll)

              IF (nAll+nLC+nSC) NE nARanges THEN STOP


              angle_ranges = MAKE_ARRAY(2,nARanges,/DOUBLE)

              IF nLC GT 0 THEN BEGIN
                 FOR jj=0,nLC-1 DO BEGIN
                    angle_ranges[*,stuff_lc[jj]] = losscone
                 ENDFOR
              ENDIF
              IF nSC GT 0 THEN BEGIN
                 FOR jj=0,nSC-1 DO BEGIN
                    angle_ranges[*,stuff_sc[jj]] = sourcecone
                 ENDFOR
              ENDIF
              IF nAll GT 0 THEN BEGIN
                 ;; angle_ranges[*,stuff_all] = [0,360]
                 FOR jj=0,nAll-1 DO BEGIN
                    angle_ranges[*,stuff_all[jj]] = [0,360]
                 ENDFOR
              ENDIF

           END
           ELSE: BEGIN

              IF N_ELEMENTS(angle_ranges) NE 2 THEN STOP
              nARanges = 1

           END
        ENDCASE

     END
     2: BEGIN
        nARanges = N_ELEMENTS(angle_ranges[0,*])
     END
  ENDCASE

  nEEB  = N_ELEMENTS(eeb_or_ees)
  nNeed = nEEB > nARanges > nERanges

  IF nEEB LT nNeed THEN BEGIN
     IF nEEB EQ 1 THEN BEGIN
        eebs = REPLICATE(eeb_or_ees,nNeed)
     ENDIF ELSE BEGIN
        STOP
     ENDELSE
  ENDIF ELSE BEGIN
     eebs = eeb_or_ees
  ENDELSE

  IF nARanges LT nNeed THEN BEGIN
     IF nARanges EQ 1 THEN BEGIN
        CASE aRangeDim OF
           1: BEGIN
              aRanges = TRANSPOSE([[REPLICATE(angle_ranges[0],nNeed)],[REPLICATE(angle_ranges[1],nNeed)]])
           END
           ELSE: STOP
        ENDCASE
     ENDIF ELSE BEGIN
        STOP
     ENDELSE
  ENDIF ELSE BEGIN
     aRanges = angle_ranges
  ENDELSE

  IF nERanges LT nNeed THEN BEGIN
     IF nERanges EQ 1 THEN BEGIN
        CASE aRangeDim OF
           1: BEGIN
              eRanges = TRANSPOSE([[REPLICATE(energy_ranges[0],nNeed)],[REPLICATE(energy_ranges[1],nNeed)]])
           END
           ELSE: STOP
        ENDCASE
     ENDIF ELSE BEGIN
        STOP
     ENDELSE
  ENDIF ELSE BEGIN
     eRanges = energy_ranges
  ENDELSE

  IF N_ELEMENTS(names) NE nNeed THEN STOP

  FOR k=0,nNeed-1 DO BEGIN

     ar     = aRanges[*,k]
     er     = eRanges[*,k]
     eeb    = eebs[k]

     thisFile = WHERE(STRMATCH(diff_eFlux_files,'*'+eeb+'*',/FOLD_CASE),nFiles)

     IF nFiles NE 1 THEN STOP

     RESTORE,diff_eFlux_files[thisFile]

     enSpec = GET_EN_SPEC__FROM_DIFF_EFLUX(diff_eFlux, $
                                           ANGLE=ar, $
                                           /RETRACE, $
                                           NAME=names[k], $
                                           T1=plot_t1, $
                                           T2=plot_t2, $
                                           /STORE, $
                                           UNITS=units)



  ENDFOR

  CASE 1 OF
     KEYWORD_SET(movie): BEGIN

        
        times  = (diff_eFlux.time+diff_eFlux.end_time)/2.D
        ;; startT = diff_eFlux.time
        ;; stopT  = diff_eFlux.time

        nTime = N_ELEMENTS(times)

        timeFNStrs = STRMID(TIME_TO_STR(times,/MSEC),11,11)
        timeFNStrs = timeFNStrs.REPLACE(':', '_')
        timeFNStrs = timeFNStrs.REPLACE('.', '__')

        FOR k=0,nTime-1 DO BEGIN

           IF KEYWORD_SET(savePlot) THEN BEGIN

              ;; POPEN,plotDir+use_spName+STRING(FORMAT='("_",I03)',k),/PORT, $
              POPEN,plotDir+use_spName+'-'+timeFNStrs[k],/PORT, $
                    ASPECT='1.66', $
                    FONT=-1, $
                    ENCAPSULATED=eps
              DEVICE,/PALATINO,FONT_SIZE=10

           ENDIF ELSE BEGIN

              IF k EQ 0 THEN BEGIN
                 WINDOW,0,XSIZE=600,YSIZE=800
              ENDIF

           ENDELSE

           TPLOT,names
           TIMEBAR,diff_eFlux.time[k],THICK=2,COLOR=250
           TIMEBAR,diff_eFlux.end_time[k],THICK=2,COLOR=250

           IF KEYWORD_SET(savePlot) THEN BEGIN
              PCLOSE
           ENDIF

        ENDFOR

     END
     ELSE: BEGIN

        IF KEYWORD_SET(savePlot) THEN BEGIN

           POPEN,plotDir+use_spName,/PORT, $
                 FONT=-1, $
                 ENCAPSULATED=eps
           DEVICE,/PALATINO,FONT_SIZE=8

        ENDIF ELSE BEGIN

           WINDOW,0,XSIZE=600,YSIZE=800

        ENDELSE

        TPLOT,names

        IF KEYWORD_SET(savePlot) THEN BEGIN
           PCLOSE
        ENDIF

     END
  ENDCASE

END

