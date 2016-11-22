;	@(#)hist.pro	1.9	11/23/04
;+
; NAME: HIST
;
; PURPOSE: To actually produce a histogram plot. This is a radical
;          departure from the IDL HISTOGRAM function, which is for
;          something else, as far as anyone can tell. The default
;          binsize is set such that the middle two quartiles of the
;          data land in five bins.
;
; CALLING SEQUENCE: hist, my_data
;
; INPUTS: MY_DATA - an array of numerical values. Can be multi-D, but
;         can't be complex. 
;       
; KEYWORD PARAMETERS: Everything you can pass to IDL's HISTOGRAM
;                     function can be passed to HIST. Also, any
;                     keyword accepted by the PLOT procedure will be
;                     passed thru as well. In addition to that, HIST
;                     has these keywords:
;
;                     XBINS - a named variable in which the centers of
;                             the histogram bins can be returned to
;                             the caller.
;
;                     HIST - a named variable in which the histogram
;                            itself can be returned. 
;
;                     SIGMA - Error bar for the bins...sqrt(hist)
;
;                     NO_PLOT - if set, the histogram is not
;                               plotted. By default, a plot is made. 
;
;                     NO_ERROR - if set, prevents error bars from
;                                being drawn on the plot.
;
;                     QUIET - suppresses some warnings
; 
;                    
; SIDE EFFECTS: The amount of redundant coding when looking at
;               histograms is drastically reduced. 
;
; EXAMPLE: To see how SOME_DATA are distributed, using a text-only
;          terminal at 300 Baud, do this: 
;
;           hist, some_data, /no_plot, xbins = bins, hist = hist 
;           for i=0,n_elements(bins)-1 do print,bins[i],hist[i]
;
; MODIFICATION HISTORY: written about 10 years late, by Bill Peria. 
;                       2016/02/12 Ripped of for printing purposes by Spencer Hatch
;-


PRO PRINT_HIST, data, BINSIZE =  binsize, INPUT = input,  MAX = max, $
                MIN = min, NAN = nan, OMAX = omax, OMIN = omin, $
                REVERSE_INDICES = reverse_indices, XBINS = bins, $
                _EXTRA = extra, HISTOGRAM = dist, $
                SIGMA = sigma, NO_ERROR = no_error, FORCE_BINSIZE = force, $
                NORMALIZE = normalize, QUIET = quiet, $
                PRINT_EMPTY_BINS=print_empty_bins, $
                UNIQS=uniqs

  quiet                                                = keyword_set(quiet)

  if not defined(data) then begin
     if not quiet then message,'Input data are not defined!',/continue
     return
  endif

  force                                                = defined(binsize) or keyword_set(force_binsize)

  sd                                                   = size(data)
  type                                                 = (sd)[sd[0]+1]

  if not defined(min) then min                         = min(data, /nan) 
  if not defined(max) then max                         = max(data, /nan)

  ok                                                   = where(finite(data) and (data ge min) and (data le max), nok)
  if nok eq 0 then begin
     message,' No finite data...',/continue
     return
  endif

  if not defined(binsize) then begin
     binsize                                           = (make_array(type=type,1,value=1))
     ord                                               = sort(data[ok])
     binsize[*]                                        = (data[ok[ord[nok*0.75]]]-data[ok[ord[nok*.25]]])/5.
     binsize                                           = binsize[0]
     if binsize eq 0 then begin
        five                                           = (make_array(type=type,1,value=5))
        binsize                                        = (total([min(data,/nan),max(data,/nan)])/five)[0]
     endif
  endif else begin
     binsize                                           = (make_array(type=type,1,value=binsize))[0]
  endelse



  nbins_guess                                          = (max-min)/binsize
  if not force then begin
     repeat begin
        nbins_guess                                    = (max-min)/binsize
        if nbins_guess gt nok then binsize             = binsize *2. 
     endrep until nbins_guess lt nok
  endif else begin
     if nbins_guess gt nok then begin
        if not quiet then message,'Roughly '+ $
                                  str(nbins_guess,form ="(I)")+' bins, for ' + $
                                  ''+str(nok)+' points...', /continue
     endif
  endelse

  si                                                   = size(input)
  if  si[si[0]+1] eq 1 then begin
     dist                                              = histogram(data[ok], INPUT = input, BINSIZE =  binsize,  $
                                                                   MAX=max, MIN = min, NAN = nan, OMAX = omax, $
                                                                   OMIN=omin, REVERSE_INDICES = reverse_indices)
  endif else begin
     dist                                              = histogram(data[ok], BINSIZE =  binsize,  MAX = max, $
                                                                   MIN=min, NAN = nan, OMAX = omax, OMIN = omin, $
                                                                   REVERSE_INDICES=reverse_indices)
  endelse
  sigma                                                = sqrt(dist)

;
; since not all data are passed thru to HISTOGRAM, I must now fix the
; reverse index list so that it corresponds to input data. (HISTOGRAM
; should be able to do this, but it stuffs up when there are NaN's in
; the input. This is a bug and a half, if you ask me. 
;
  nh                                                   = n_elements(dist)
  i_ind                                                = reverse_indices[0L:nh] ; this is the "range" part of the
                                ; reverse index list. It has one more
                                ; element than the histogram has
                                ; bins. 
  o_ind                                                = reverse_indices[nh+1L:*] ; there are the indices of the
                                ; elements from the OK array, referred
                                ; to by each bin's range indices
                                ; (I_IND). 
  o_ind                                                = ok[o_ind]             ; now o_ind refers properly to the input data. 
  reverse_indices                                      = [i_ind, o_ind]
  undefine, i_ind, o_ind        ; clean up!


  if keyword_set(normalize) then begin
     denom                                             = total(dist)
     sigma                                             = dist/denom * sqrt(1./denom + 1./dist)
     dist                                              = dist / denom
  endif


  nbins                                                = long((omax - omin)/binsize) + 1L
  if type eq 1 then itype                              = 3 else itype = type ; prevent byte
                                ; rollover in bins 

  indices                                              = make_array(nbins, type = itype,/index)

  one                                                  = (make_array(1, type = type, value=1))[0]
  two                                                  = one + one

  bin_lower_bounds                                     = indices*binsize + omin
  bin_upper_bounds                                     = (indices + one)*binsize + omin

  bins                                                 = (bin_upper_bounds + bin_lower_bounds)/two
;
; why is the following necessary? Why does IDL stick on an empty bin?
; ARGH! 
;
  if max(bins, /nan) gt omax then begin
     not_too_big                                       = where(bins le omax, nntb)
     if nntb eq 0 then begin
        message,'Unable to define bins properly by default...you will ' + $
                'need to set them correctly yourself.',/continue
        return
     endif
     dist                                              = dist[not_too_big]
     bins                                              = bins[not_too_big]
     sigma                                             = sigma[not_too_big]
  endif    

  PRINT,FORMAT='("Bin",T10,"N")'
  fmtString                                            = '(G10.2,T10,I0)'

  ;;PRINT ALL DAT!!!
  CASE 1 OF
     KEYWORD_SET(print_empty_bins): BEGIN
        FOR i=0,N_ELEMENTS(bins)-1 DO PRINT,bins[i],dist[i]
     END
     KEYWORD_SET(uniqs): BEGIN
        uniq_i = UNIQ(data,SORT(data))
        nUniq = N_ELEMENTS(uniq_i)
        IF nUniq GT 5000 THEN STOP
        FOR k=0,nUniq-1 DO $
           PRINT, $
           data[uniq_i[k]], $
           N_ELEMENTS(WHERE(ABS(data - data[uniq_i[k]]) LT 1e-4,/NULL))
        
     END
     ELSE: BEGIN
        FOR i=0,N_ELEMENTS(bins)-1 DO BEGIN
           IF dist[i] NE 0 THEN PRINT,bins[i],dist[i]
        ENDFOR
     END
  ENDCASE

  RETURN
END


