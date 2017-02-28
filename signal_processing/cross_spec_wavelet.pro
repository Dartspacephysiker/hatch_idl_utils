; cross_spec.pro ver 1.1
;+    
; NAME: CROSS_SPEC_WAVELET
;
; PURPOSE: Routine to calculate the cross-spectra of two input time
;          series sig1 and sig2. Program returns coherence and phase
;          delay vs. frequency for the two input signals.
;
; CALLING SEQUENCE: cross_spec, sig1, sig2, n_ave, npts, sample, coh,
;                   frequency, overlap=overlap
;
; INPUTS:
;         sig1  - A one dimensional array containing time series
;                 waveform amplitudes.
;
;         sig2  - A second time series signal similar to sig1.
;
;         N_AVE - The number of segments to divide up the time series
;                 data in sig1 and sig2 for averaging.
;         
;         SAMPLE -The sample time for each point in the time
;                       series data in sig1 and sig2.
;
; OUTPUTS -
;
;         COH   - The resulting coherence data for sig1 and sig2. This
;                 represents gamma^2 in the popular literature.
;
;         PHASE - The resulting phase delay between the two input
;                 signals sig1 and sig2.
;
;         FREQ  - The frequency axis for the coherence and phase data.
;
; KEYWORDS:
;
;         OVERLAP - Set this keyword to slide the cross-spectral
;                   interval by one-half of an interval instead of
;                   averaging together each separate interval. For
;                   most data types this will yield a higher number of
;                   averages and less error per data point than straight
;                   sequential averaging. Note that N_AVE
;                   specifies how many sequential averages are taken
;                   without overlap. Thus N_AVE=4 with /OVERLAP yields
;                   a total of 7 averages, etc. 
;
;
; EXAMPLE:   You have two time series signals sig1 and sig2 that you
;            want to derive a cross-spectra from. Each of these time
;            series data must be of the same length and should be a
;            multiple of 2^n, say 8192 points. To get decent
;            statistics at least 4-8 sequential or sliding averages
;            should be used -- so pick n_ave = 8, npts=1024, and
;            sample = the sample time (in seconds) for the time series
;            data. Alternatively, to get a higher frequency resolution
;            set n_ave=4, npts=2048, and set /overlap. This yields 4+3
;            = 7 sliding averages for the data.
;
;    A note about errors:
;            For n sequential (non-sliding) averages FFT's have a
;            statistical error of 1/n^0.5. For the same time interval,
;            if a sliding average is used of 1/2 a window overlap, n
;            goes up by a factor of 2 but the errors are no longer
;            completely independent -- so the new error per point is
;            1/(0.81*N)^0.5 -- but N=2n so there is less error per point
;            than in the non-sliding average case. For more
;            information about errors and correlation analyses in
;            general, consult Bendat & Piersol, "Engineering
;            Applications of Correlation and Spectral Analysis", 1993.
;
;    A note about the data:
;            Results may not be valid if the number of points per FFT
;            (npts) is not 2^j where j = any integer.
;            This routine assumes that the input data is continuous and
;            that there are no data gaps or bad time points.
;
;
; REVISION HISTORY
;  ver 1.0 04/03/97 G.T.Delory
;  ver 1.1 04/11/97 G.T.Delory
;
;-

pro cross_spec_wavelet,sig1, $
                       sig2, $
                       coh, $
                       phase, $
                       freq, $
                       TIMES=times, $
                       N_AVE=n_ave, $
                       NPTS=npts, $
                       SAMPLE=sample, $
                       OVERLAP=overlap, $
                       FAMILY=family, $
                       START_SCALE=start_scale, $
                       ORDER=order, $
                       DSCALE=dScale, $
                       NSCALE=nScale, $
                       PAD=pad, $
                       PHASE_CORRECT=phase_correct, $
                       DOUBLE=double

  IF ~KEYWORD_SET(family) THEN BEGIN
     family = 'Morlet'
  ENDIF

  IF ~KEYWORD_SET(order) THEN BEGIN
     order  = 6
  ENDIF

  IF KEYWORD_SET(start_frequency) OR ~KEYWORD_SET(start_scale) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(start_frequency): BEGIN
           
        END
        ELSE: BEGIN
           start_scale = 2
        END
     ENDCASE
  ENDIF

  IF ~KEYWORD_SET(dScale) THEN BEGIN
     dScale = 0.25D
  ENDIF

  IF ~KEYWORD_SET(nScale) OR (KEYWORD_SET(dScale) AND KEYWORD_SET(start_scale)) THEN BEGIN
     nScale = FLOOR((ALOG2(nPts/start_scale))/dScale+1)
  ENDIF

; Initialize arrays
  CASE 1 OF
     KEYWORD_SET(double): BEGIN
        corr = DCOMPLEXARR(1,npts,nScale)
        norm = DBLARR(2,npts,nScale)
        coh  = DBLARR(1,npts,nScale)

        IF KEYWORD_SET(phase_correct) THEN BEGIN
           phase_correct = DOUBLE(phase_correct)
        ENDIF
     END
     ELSE: BEGIN
        corr = COMPLEXARR(1,npts,nScale)
        norm = FLTARR(2,npts,nScale)
        coh  = FLTARR(1,npts,nScale)
     END
  ENDCASE

; Basic error checking and setting defaults for unspecified keywords.

if n_elements(sig1) NE n_elements(sig2) then begin
    print,'CROSS_SPEC: Input signals must have the same number of ' + $
      'elements'
    sig1 = REPLICATE(!values.f_nan,256)
    sig2 = replicate(!values.f_nan,256)
endif

if not (defined(sig1) or defined(sig2)) then begin
    print,'No data provided for one or both of the input channels -- ' + $
      'check calling sequence and try again'
    sig1 = replicate(!values.f_nan,256)
    sig2 = replicate(!values.f_nan,256)
endif

if not (defined(npts) AND defined(n_ave)) then begin
    print,'CROSS_SPEC: Number of points per average interval ' + $
      'unspecified.'
    npts = n_elements(sig1)/n_ave
    print,'Assuming '+strcompress(string(npts), /remove_all) + ' ' + $
      'points per interval.'
endif

if not defined(n_ave) then begin
    n_ave=4
    npts = n_elements(sig1)/n_ave
    print,'CROSS_SPEC: Number of averages undefined; assuming n_ave = 4, '
    print,'There are '+strcompress(string(npts), /remove_all)+' ' + $
      'points per averaging interval' 
endif

; If overlap keyword is set, slide half of a segment. If not, slide a
; complete segment.
if keyword_set(overlap) then begin
    n_loop=2*n_ave-1
    seg_step=npts/2
endif else begin
    seg_step=npts
    n_loop = n_ave
endelse

; Initialize indexes for averaging windows.
start=long(0)
stop=long(npts-1)

; Averaging loop - slide window through the data.
for i = long(0),long(n_loop-1) do begin
  dt      = MEDIAN(sample)
  wavsig1 = WV_CWT(sig1[start:stop],family,order, $
                   SCALE=scales1, $
                   START_SCALE=start_scale, $
                   DSCALE=dScale, $
                   NSCALE=nScale, $
                   PAD=pad)
  wavsig2 = WV_CWT(sig2[start:stop],family,order, $
                   SCALE=scales2, $
                   START_SCALE=start_scale, $
                   DSCALE=dScale, $
                   NSCALE=nScale, $
                   PAD=pad)
  tScales1 = dt*scales1
  fScales1 = 1d0/tScales1
  tScales2 = dt*scales2
  fScales2 = 1d0/tScales2
  corr = corr+(wavsig1*CONJ(wavsig2))/n_ave
  norm(0,*,*) = norm(0,*,*)+(wavsig1*CONJ(wavsig1))/n_ave
  norm(1,*,*) = norm(1,*,*)+(wavsig2*CONJ(wavsig2))/n_ave
  ;; corr = corr+(wavsig1*conj(wavsig2))/n_ave
  ;; norm(0,*,*) = norm(0,*,*)+(ABS(wavsig1))^2.D/n_ave
  ;; norm(1,*,*) = norm(1,*,*)+(ABS(wavsig2))^2.D/n_ave ;;No! don't use abs!!!
  stop = stop+seg_step
  start = start+seg_step 
endfor

WAVE_COHERENCY,wavsig1,times,scales1,wavsig2,times,scales2, $
               WAVE_COHER=coh, $
               WAVE_PHASE=phase, $
               /NOSMOOTH, $
               GLOBAL_PHASE=global_phase, $
               GLOBAL_COHER=global_coh


; Form cross-spectral function.
cross_spec = corr/((norm(0,*,*)*norm(1,*,*))^0.5)

y = IMAGINARY(cross_spec)       ;Extract imaginary part of coherence
;; CASE 1 OF
;;    KEYWORD_SET(double): BEGIN
;;       x = DOUBLE(cross_spec)    ;Extract real part of coherence
;;    END
;;    ELSE: BEGIN
;;       x = FLOAT(cross_spec)     ;Extract real part of coherence
;;    END
;; ENDCASE
x = REAL_PART(cross_spec)

; Determine which quadrant the phase angle will be. Convention is
; 0-180, wrap to -180, back to 0 degrees.
quad_1 = where((x GE 0) AND (y GE 0))
quad_2 = where((x LT 0) AND (y GT 0))
quad_3 = where((x LT 0) AND (y LT 0))
quad_4 = where((x GT 0) AND (y LT 0))

; Determine phase angle.
phase = (atan(y/x))

if (quad_2(0) NE -1) then phase(quad_2) = phase(quad_2) + !pi
if (quad_3(0) NE -1) then phase(quad_3) = phase(quad_3) - !pi

;;Correct phases
;;Según Chaston et al. [2006]
IF KEYWORD_SET(phase_correct) THEN BEGIN 

   ;; yNew              = ABS(cross_spec)*SIN(phase)
   ;; xNew              = ABS(cross_spec)*COS(phase)
   ;; cross_specNew     = COMPLEX(x,y)
   ;; this              = WHERE(ABS(cross_specNew -cross_spec) GT 0.0001D*MIN(ABS(cross_spec)),nThis)
   ;; PRINT,MAX(ABS(cross_specNew -cross_spec))/MIN(ABS(cross_spec)) ;Biggest diff over smallest magnitude


   corr              = WHERE(ABS(phase) GT phase_correct,nCorr)
   IF nCorr GT 0 THEN BEGIN
      phase[corr]    = 2.*ABS(phase[corr])/phase[corr]*phase_correct-phase[corr]
   ENDIF

   yNew              = ABS(cross_spec)*SIN(phase)
   xNew              = ABS(cross_spec)*COS(phase)

   ;; cross_specNew  = COMPLEX(xNew,yNew)
   cross_spec        = COMPLEX(xNew,yNew)

ENDIF

;Ignore mirrored part of phase angle data.
;; phase = phase(0:npts/2)

; Derive coherence quantity.
;; CASE 1 OF
;;    KEYWORD_SET(double): BEGIN
;;       coh = DOUBLE(cross_spec*CONJ(cross_spec))
;;    END
;;    ELSE: BEGIN
;;       coh = FLOAT(cross_spec*CONJ(cross_spec))
;;    END
;; ENDCASE
;; this = WHERE(ABS(cross_spec*CONJ(cross_spec)-ABS(cross_spec)) GT 1D-4*MIN(ABS(cross_spec)),nThis)
;; coh = ABS(cross_spec) ;;No! You don't want to kill the complexness
coh = cross_spec*CONJ(cross_spec)

; Take first half of coherence function and ignore mirrored part.
;; coh = coh(0,[0:npts/2],*)


;Construct frequency axis.
;; freq = findgen(npts/2+1)/(npts*sample)
freq = MAKE_ARRAY(nPts,VALUE=1.,/FLOAT) # fscales1
coh  = REFORM(coh)
phase = REFORM(phase)


end
