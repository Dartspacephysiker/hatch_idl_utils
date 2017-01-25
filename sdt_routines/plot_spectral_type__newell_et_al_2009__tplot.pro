;+
;PROCEDURE:  mplot, x, y, [,dy]
;INPUT:
;            x:  1 or 2 dimensional array of x values.
;            y:  1 or 2 dimensional array of y values.
;           dy;  error bars for y;  same dimensions as y. (optional)
;PURPOSE:
;    General purpose procedure used to make multi-line plots.
;	   
;KEYWORDS:
;    DATA:     A structure that contains the elements 'x', 'y' ['dy'].  This 
;       is an alternative way of inputing the data  (used by TPLOT).
;    LIMITS:   Structure containing any combination of the following elements:
;          ALL PLOT/OPLOT keywords  (ie. PSYM,SYMSIZE,LINESTYLE,COLOR,etc.)
;          COLORS:  array of colors to be used for each line.
;          NSUMS:   array of NSUM keywords.
;          LINESTYLES:  array of linestyles.
;          LABELS:  array of text labels.
;          LABPOS:  array of positions for LABELS
;          LABFLAG: integer, flag that controls label positioning.
;                 -1: labels placed in reverse order.
;                  0: No labels.
;                  1: labels spaced equally.
;                  2: labels placed according to data.
;                  3: labels placed according to LABPOS.
;          BINS:    flag array specifying which channels to plot.
;    OVERPLOT: If non-zero then data is plotted over last plot.
;    NOXLAB:   if non-zero then xlabel tick marks are supressed.
;    LABELS:   array of strings used to identify the curve(s)
;    COLORS:   array of colors used for each curve.
;    BINS:     array of 0's or 1's that specify if a trace should be plotted
;NOTES: 
;    The values of all the keywords can also be put in the limits structure or
;    in the data structure using the full keyword as the tag name.
;    The structure value will overide the keyword value.
;
;CREATED BY:	Davin Larson
;FILE:  mplot.pro  
;VERSION  1.27
;LAST MODIFICATION: 97/01/26
;
;-
pro plot_spectral_type__newell_et_al_2009__tplot,x,y,dy,    $
   OVERPLOT = overplot,  $
   OPLOT = oplot,  $
   LABELS = labels,$   ;(array of) label(s) for the curve(s)
   LABPOS = labpos,$
   LABFLAG = labflag,$
   COLORS = colors, $   ;(array of) color(s) for the curve(s)
   BINS   = bins,  $
   DATA   = data,  $
   NOXLAB = noxlab,$   ;No xlabels are printed if set
   NOCOLOR = nocolor, $ ;Colors not automatically generated if set
   LIMITS  = limits, $  ;structure containing miscellaneous keyword tags:values
   NO_STRICT_TYPES=no_strict_types

  IF KEYWORD_SET(data) THEN events = data
  
  ;; if keyword_set(data) then begin
  ;;   x = data.x
  ;;   y = data.y
  ;;   str_element,data,'dy',value=dy
  ;;   extract_tags,stuff,data,except=['x','y','dy']
  ;; endif

  STR_ELEMENT,limits,'no_strict_types',value=no_strict_types
  CASE N_ELEMENTS(no_strict_types) OF
     0: BEGIN
        no_strict_types = 0
     END
     1: BEGIN
        IF no_strict_types EQ -1 THEN no_strict_types = 0
     END
  ENDCASE


  nSpectra       = N_ELEMENTS(events.x)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;PLOT PRELIMS
  ;; mVal        = 2
  ;; mSVal       = 3
  ;; bVal        = 4
  ;; bSVal       = 5
  ;; diffuseVal  = 6
  mVal           = 1
  mSVal          = 2
  bVal           = 3
  bSVal          = 4
  dVal           = 5

  mCol           = 'blue'
  mSCol          = 'green'
  bCol           = 'red'
  bSCol          = 'purple'
  dCol           = 'black'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;SORT EVENTS
  ;; time        = MAKE_ARRAY(nSpectra,/DOUBLE ,VALUE=-1)
  mono           = MAKE_ARRAY(nSpectra,/INTEGER,VALUE=-1)
  monoS          = MAKE_ARRAY(nSpectra,/INTEGER,VALUE=-1)
  broad          = MAKE_ARRAY(nSpectra,/INTEGER,VALUE=-1)
  broadS         = MAKE_ARRAY(nSpectra,/INTEGER,VALUE=-1)
  diffuse        = MAKE_ARRAY(nSpectra,/INTEGER,VALUE=-1)

  m_i            = WHERE(events.mono    EQ 1,nM,/NULL)
  mS_i           = WHERE(events.mono    EQ 2,nMS,/NULL)
  b_i            = WHERE(events.broad   EQ 1,nB,/NULL)
  bS_i           = WHERE(events.broad   EQ 2,nBS,/NULL)
  d_i            = WHERE(events.diffuse EQ 1,nDiffuse,/NULL)

  IF KEYWORD_SET(no_strict_types) THEN BEGIN
     IF nM GT 0 THEN BEGIN
        IF nMS GT 0 THEN BEGIN
           m_i   = ([m_i,mS_i])[SORT([m_i,mS_i])]
           nM += nMS
        ENDIF
     ENDIF ELSE BEGIN
        IF nMS GT 0 THEN BEGIN
           m_i   = mS_i
           nM    = nMS
        ENDIF
     ENDELSE

     IF nB GT 0 THEN BEGIN
        IF nBS GT 0 THEN BEGIN
           b_i   = ([b_i,bS_i])[SORT([b_i,bS_i])]
           nB     += nBS
        ENDIF
     ENDIF ELSE BEGIN
        IF nBS GT 0 THEN BEGIN
           b_i   = bS_i
           nB    = nBS
        ENDIF
     ENDELSE

     nMS         = 0
     nBS         = 0
     mS_i        = !NULL
     bS_i        = !NULL
  ENDIF

  mono[m_i]      = mVal
  monoS[mS_i]    = mSVal
  broad[b_i]     = bVal
  broadS[bS_i]   = bSVal
  diffuse[d_i]   = dVal

  ;; datArr              = 
  ;; x                   = [[events.time_e],[events.time_e],[events.time_e],[events.time_e],[events.time_e]]
  x                   = [[events.x],[events.x],[events.x],[events.x],[events.x]]
  y                   = [[mono]    ,[monoS]   ,[broad]   ,[broadS]  ,[diffuse] ]

  if keyword_set(overplot) then oplot=overplot
  overplot = 1
  str_element,limits,'overplot',value=oplot

  xrange=[0.,0.]
  yrange=[0.,0.]
  charsize = !p.charsize
  if charsize eq 0 then charsize = 1.

  extract_tags,stuff,limits

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
  ;; psym= -1
  ;; psym= -1
  psym     = 7  ; or '*'
  ;; str_element,stuff,'psym',value=psym,index=psym_ind
  ;; str_element,stuff,'psym_lim',value=psym_lim,index=psym_lim_ind
  ;; if count lt psym_lim then add_str_element,plotstuff,'psym',psym
  ;; if count lt psym_lim then add_str_element,oplotstuff,'psym',psym
  ;; add_str_element,plotstuff,'psym',psym,REPLACE=(psym_ind NE -1)
  ;; add_str_element,oplotstuff,'psym',psym,REPLACE=(psym_ind NE -1)
  add_str_element,plotstuff,'psym',psym,/REPLACE
  add_str_element,oplotstuff,'psym',psym,/REPLACE

  add_str_element,plotstuff,'symsize',0.7
  add_str_element,oplotstuff,'symsize',0.7


  ;; if count eq 0 then ind = indgen(n_elements(x))  else ind = good(ind)
  ;; if yrange(0) eq yrange(1) then begin
  ;;    if ndx eq 1 then $
  ;;       yrange = minmax_range(y(ind,*),posi=ytype,max=max_value,min=min_value) $
  ;;    else $
  ;;       yrange = minmax_range(y(ind),posi=ytype,max=max_value,min=min_value)
  ;; endif

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;For my purposes, yrange is [0.5,5.5]
  yrange = [0.5,5.5]
  add_str_element,plotstuff,'yrange',yrange,/REPLACE
  add_str_element,oplotstuff,'yrange',yrange,/REPLACE

  ;; add_str_element,plotstuff,'ystyle',9,/REPLACE
  ;; add_str_element,oplotstuff,'ystyle',9,/REPLACE
  ;; add_str_element,plotstuff,'ystyle',1,/REPLACE
  ;; add_str_element,oplotstuff,'ystyle',1,/REPLACE

  add_str_element,plotstuff,'yminor',1,/REPLACE
  add_str_element,oplotstuff,'yminor',1,/REPLACE

  if keyword_set(noxlab) then $
     add_str_element,plotstuff,'xtickname',replicate(' ',22)

  ;; names = ['Monoenergetic','Strict Mono','Broadband','Strict Broad','Diffuse' ]
  IF KEYWORD_SET(no_strict_types) THEN BEGIN
     names = ['Mono','Broad','Diffuse' ]
     add_str_element,plotstuff ,'ytickname',names
     add_str_element,oplotstuff,'ytickname',names
     add_str_element,plotstuff,'yticks',2,/REPLACE
     add_str_element,oplotstuff,'yticks',2,/REPLACE
     add_str_element,plotstuff,'ytickv',[1,3,5],/REPLACE
     add_str_element,oplotstuff,'ytickv',[1,3,5],/REPLACE
     col  =   [ 100      ,250   ,!p.color]
  ENDIF ELSE BEGIN
     names = ['Mono','Strict Mono','Broad','Strict Broad','Diffuse' ]
     add_str_element,plotstuff ,'ytickname',names
     add_str_element,oplotstuff,'ytickname',names
     add_str_element,plotstuff,'yticks',4,/REPLACE
     add_str_element,oplotstuff,'yticks',4,/REPLACE
     add_str_element,plotstuff,'ytickv',[1,2,3,4,5],/REPLACE
     add_str_element,oplotstuff,'ytickv',[1,2,3,4,5],/REPLACE
     col  =   [ 100      ,120   ,250   ,30    ,!p.color]
  ENDELSE
  ;; add_str_element,plotstuff,'ytickv',[2,3,4,5,6],/REPLACE
  ;; add_str_element,oplotstuff,'ytickv',[2,3,4,5,6],/REPLACE


  ;; if n_elements(colors) ne 0 then col = colors  $
  ;; ;else if d2 gt 1 then col=bytescale(pure_col=d2) $
  ;; else if d2 gt 1 then col=bytescale(findgen(d2)) $
  ;; else col = !p.color
  ;; col  =   [mCol       ,mSCol   ,bCol   ,bSCol    ,dCol]
  ;; col  =   [ 100      ,120   ,250   ,30    ,190]

;; if keyword_set(nocolor) then if nocolor ne 2 or !d.name eq 'PS' then $
;;    col = !p.color

  nc = n_elements(col)

  if ~keyword_set(oplot) then $
     plot,/nodata,xrange,yrange,_EXTRA = plotstuff

  if keyword_set(labels) then begin
     nlab = n_elements(labels)
     if nlab ne d2 then message,'Incorrect number of labels',/cont
     yw = !y.window
     xw = !x.window
     if not keyword_set(lbsize) then $
        lbsize = charsize < (yw(1)-yw(0))/(nlab+1) *!d.y_size/!d.y_ch_size $
     else lbsize = lbsize*charsize
     if n_elements(labflag) eq 0 then begin
        if keyword_set(labpos) then labflag = 3 else labflag = 2
     endif
     if labflag eq 1 or labflag eq -1 then $
        nlabpos = findgen(nlab)*(yw(1)-yw(0))/nlab + yw(0)
     if labflag eq -1 then nlabpos = reverse(nlabpos)
     if labflag eq 3 then begin
        foo = convert_coord(/data,/to_norm,findgen(nlab),labpos)
        nlabpos = foo(1,*)
     endif
     xpos = !x.window(1)
  endif else labflag=0

  for n=0,d2-1 do begin
     if bins(n) ne 0 then begin
        if ndx eq 1 then i=0 else i=n
        c = col(n mod nc)
        if n_nsums ne 0 then nsum = nsums(n mod n_nsums)
        if n_linestyles ne 0 then linestyle = linestyles(n mod n_linestyles)
        xt = x(*,i)
        yt = y(*,n)
        oplot,xt,yt,color=c,psym=2,_EXTRA=oplotstuff ;,nsum=nsum
        if n_elements(dy) ne 0 then begin
           tempc = !p.color
           !p.color = c
           upper = yt+dy(*,n)
           lower = yt-dy(*,n)

           if keyword_set(ytype) then lower = lower > yrange(0)/2.
           oplot_err,xt,lower,upper
;          oploterr,xt,yt,dy(*,n),0
           !p.color = tempc
        endif
        if keyword_set(labels) and keyword_set(labflag) then begin
           ypos  = 0.
           if keyword_set(nlabpos) then ypos = nlabpos(n) else begin
              fooind = where(finite(yt),count)
              if count ne 0 then begin
                 foo = convert_coord(xt(fooind),yt(fooind),/data,/to_norm)
                 fooind = where( foo(0,*) le xw(1),count)
                 if count ne 0 then mx = max(foo(0,fooind),ms)
                 if count ne 0 then ypos = foo(1,fooind(ms))
              endif
           endelse
           if ypos le yw(1) and ypos ge yw(0) then $
              xyouts,xpos,ypos,'  '+labels(n),color=c,/norm,charsize=lbsize
        endif
     endif
  endfor

return


end
