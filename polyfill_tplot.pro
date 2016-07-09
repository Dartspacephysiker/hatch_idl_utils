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

xrange=[0.,0.]
yrange=[0.,0.]
charsize = !p.charsize
if charsize eq 0 then charsize = 1.


extract_tags,stuff,limits

str_element,stuff,'fill_color',value=fill_color
str_element,stuff,'nocolor',value=nocolor
str_element,stuff,'colors',value=colors
str_element,stuff,'nsums',value=nsums & n_nsums = n_elements(nsums) & nsum=1
str_element,stuff,'linestyles',value=linestyles
n_linestyles = n_elements(linestyles) & linestyle=0
str_element,stuff,'labflag',value=labflag
str_element,stuff,'labels',value=labels
str_element,stuff,'labpos',value=labpos
str_element,stuff,'labsize',value=lbsize
str_element,stuff,'bins',value=bins
str_element,stuff,'charsize',value=charsize
 
extract_tags,plotstuff,stuff,/plot
extract_tags,oplotstuff,stuff,/oplot


str_element,plotstuff,'xrange',value=xrange
str_element,plotstuff,'xtype',value=xtype
str_element,plotstuff,'xlog',value=xtype
str_element,plotstuff,'yrange',value=yrange
str_element,plotstuff,'ytype',value=ytype
str_element,plotstuff,'ylog',value=ytype
str_element,plotstuff,'max_value',value=max_value
str_element,plotstuff,'min_value',value=min_value


d1 = dimen1(y)
d2 = dimen2(y)
ndx = ndimen(x)
if n_elements(bins) eq 0 then bins = replicate(1b,d2)


if xrange(0) eq xrange(1) then xrange = minmax_range(x,positive=xtype)

good = where(finite(x),count) 
if count eq 0 then message,'No valid X data.'

ind = where(x(good) ge xrange(0) and x(good) le xrange(1),count)

psym_lim = 0
psym= -1
str_element,stuff,'psym',value=psym
str_element,stuff,'psym_lim',value=psym_lim
if count lt psym_lim then add_str_element,plotstuff,'psym',psym
if count lt psym_lim then add_str_element,oplotstuff,'psym',psym

if count eq 0 then ind = indgen(n_elements(x))  else ind = good(ind)
if yrange(0) eq yrange(1) then begin
    if ndx eq 1 then $
      yrange = minmax_range(y(ind,*),posi=ytype,max=max_value,min=min_value) $
    else $
      yrange = minmax_range(y(ind),posi=ytype,max=max_value,min=min_value)
endif

if keyword_set(noxlab) then $
    add_str_element,plotstuff,'xtickname',replicate(' ',22)

if n_elements(colors) ne 0 then col = colors  $
;else if d2 gt 1 then col=bytescale(pure_col=d2) $
else if d2 gt 1 then col=bytescale(findgen(d2)) $
else col = !p.color

if keyword_set(nocolor) then if nocolor ne 2 or !d.name eq 'PS' then $
   col = !p.color

nc = n_elements(col)
blankstuff = plotstuff


if keyword_set(oplot) eq 0 then $
   plot,/nodata,xrange,yrange,_EXTRA = blankstuff

  ;; IF NOT KEYWORD_SET(overplot) THEN $
  ;;    PLOT,/NODATA,x,y,_EXTRA=plotstuff

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
     POLYFILL,xTemp,yTemp,_EXTRA=plotstuff,/DATA,COLOR=fill_color
  ENDFOR

  FOR i=0,n_neg_streaks-1 DO BEGIN
     ;;get relevant data, adjust
     xTemp = [x[neg_i[start_neg_ii[i]]],x[neg_i[start_neg_ii[i]:stop_neg_ii[i]]],x[neg_i[stop_neg_ii[i]]]]
     yTemp = [0,y[neg_i[start_neg_ii[i]:stop_neg_ii[i]]],0]
     POLYFILL,xTemp,yTemp,_EXTRA=plotstuff,/DATA,COLOR=fill_color
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
