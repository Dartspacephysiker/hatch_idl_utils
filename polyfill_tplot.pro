;+
;NAME:
;  polyfill_tplot
;PURPOSE:
;  Fills in lines, because we like that.
;  Can be used by "tplot". 
;  
;-
PRO POLYFILL_TPLOT,x,y,OVERPLOT=overplot,LIMITS=lim,DATA=data
  IF KEYWORD_SET(data) THEN BEGIN
     x = data.x
     y = data.y
     EXTRACT_TAGS,stuff,data,except=['x','y','dy']
  ENDIF
  EXTRACT_TAGS,stuff,lim

  STOP
  STR_ELEMENT,stuff,'nocolor',VALUE=nocolor
  STR_ELEMENT,stuff,'colors',VALUE=colors
  STR_ELEMENT,stuff,'nsums',VALUE=nsums & n_nsums = N_ELEMENTS(nsums) & nsum=1
  STR_ELEMENT,stuff,'linestyles',VALUE=linestyles
  n_linestyles = N_ELEMENTS(linestyles) & linestyle=0
  STR_ELEMENT,stuff,'labflag',VALUE=labflag
  STR_ELEMENT,stuff,'labels',VALUE=labels
  STR_ELEMENT,stuff,'labpos',VALUE=labpos
  STR_ELEMENT,stuff,'labsize',VALUE=lbsize
  STR_ELEMENT,stuff,'bins',VALUE=bins
  STR_ELEMENT,stuff,'charsize',VALUE=charsize
  
  EXTRACT_TAGS,plotstuff,stuff,/plot
  EXTRACT_TAGS,oplotstuff,stuff,/oplot

  STR_ELEMENT,plotstuff,'xrange',VALUE=xrange
  STR_ELEMENT,plotstuff,'xtype',VALUE=xtype
  STR_ELEMENT,plotstuff,'xlog',VALUE=xtype
  STR_ELEMENT,plotstuff,'yrange',VALUE=yrange
  STR_ELEMENT,plotstuff,'ytype',VALUE=ytype
  STR_ELEMENT,plotstuff,'ylog',VALUE=ytype
  STR_ELEMENT,plotstuff,'max_value',VALUE=max_value
  STR_ELEMENT,plotstuff,'min_value',VALUE=min_value

  chsize = !p.charsize
  IF NOT KEYWORD_SET(chsize) THEN chsize = 1.


  IF NOT KEYWORD_SET(overplot) THEN $
     PLOT,/NODATA,x,y,YRANGE=[-1+di,nb+di],/YSTYLE,_EXTRA=plotstuff

  ;;first, get everywhere that the sign changes
  pos_i           = WHERE(y GE 0,nPos)
  neg_i           = WHERE(y LE 0,nNeg)
  IF nPos GT 1 THEN BEGIN
     GET_STREAKS,pos_i,START_I=start_pos_ii,STOP_I=stop_pos_ii,SINGLE_I=single_neg_ii,N_STREAKS=n_pos_streaks
  ENDIF ELSE BEGIN
     n_pos_streaks = 0
  ENDELSE
  IF nNeg GT 1 THEN BEGIN
     GET_STREAKS,neg_i,START_I=start_neg_ii,STOP_I=stop_neg_ii,SINGLE_I=single_neg_ii,N_STREAKS=n_neg_streaks
  ENDIF ELSE BEGIN
     n_neg_streaks = 0
  ENDELSE

  ;;Fill them in separately
  FOR i=0,n_pos_streaks-1 DO BEGIN
     ;;get relevant data, adjust
     xTemp = [x[pos_i[start_pos_ii[i]]],x[pos_i[start_pos_ii[i]:stop_pos_ii[i]]],x[pos_i[stop_pos_ii[i]]]]
     yTemp = [0,y[pos_i[start_pos_ii[i]:stop_pos_ii[i]]],0]
     POLYFILL,xTemp,yTemp,_EXTRA=plotstuff
  ENDFOR

  FOR i=0,n_neg_streaks-1 DO BEGIN
     ;;get relevant data, adjust
     xTemp = [x[neg_i[start_neg_ii[i]]],x[neg_i[start_neg_ii[i]:stop_neg_ii[i]]],x[neg_i[stop_neg_ii[i]]]]
     yTemp = [0,y[neg_i[start_neg_ii[i]:stop_neg_ii[i]]],0]
     POLYFILL,xTemp,yTemp,_EXTRA=plotstuff
  ENDFOR

  IF KEYWORD_SET(labels) AND KEYWORD_SET(labflag) THEN BEGIN
     ypos  = 0.
     IF KEYWORD_SET(nlabpos) THEN ypos = nlabpos[n] ELSE BEGIN
        fooind = WHERE(FINITE(yt),count)
        IF count ne 0 THEN begin
           foo = CONVERT_COORD(xt[fooind],yt[fooind],/data,/to_norm)
           fooind = WHERE( foo[0,*] LE xw[1],count)
           IF count ne 0 THEN mx = MAX(foo[0,fooind],ms)
           IF count ne 0 THEN ypos = foo[1,fooind[ms]]
        ENDIF
     ENDELSE
     IF ypos LE yw[1] AND ypos GE yw[0] THEN $
        xyouts,xpos,ypos,'  '+labels[n],color=c,/norm,charsize=lbsize
  ENDIF

END
