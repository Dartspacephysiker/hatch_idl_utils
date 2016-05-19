;+
;NAME:
;  bitplot
;PURPOSE:
;  Plots 'ON' bits for housekeeping type data.
;  Can be used by "tplot". 
;  See "_tplot_example" and "_get_example_dat" for an example.
;-
pro polyfill_tplot,x,y,overplot=overplot,limits=lim,data=data
if keyword_set(data) then begin
  x = data.x
  y = data.y
  extract_tags,stuff,data,except=['x','y','dy']
endif
extract_tags,stuff,lim

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

chsize = !p.charsize
if not keyword_set(chsize) then chsize = 1.


if not keyword_set(overplot) then $
   plot,/nodata,x,y,yrange=[-1+di,nb+di],/ystyle,_extra=plotstuff

polyfill,x,y,_extra=plotstuff

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

end
