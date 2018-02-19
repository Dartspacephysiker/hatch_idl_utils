;2018/02/19
;This one lets the shift in energy be linear, as it ought to be for a distribution dropped through a potential structure.
;The referee for the soon-to-be-published (with any luck) kappa GRL pointed this out. Silly we!
;
; Ripped from KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC
;X          = vector of energies in eV for which f(energy) is to be calcked
;P          = vector of function params:
;F          = vector of number flux/energy measurements---units are 1/(cm^2-s-sr-eV-keV)
; A[0]: dPhi,      Potential drop (V)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
; A[4]: Pitch angle (NOT IMPLEMENTED)

; This function returns s^3/cm^3-km^3
;
; If in DFSTD units, the max val should be of order 10^-16 or less (or thereabouts).
; Why? look: n = 1.0 cm^-3 = 10^6 m^-3.   T = 500 eV. m = 5.11 keV/c^2. c = 3.0e8
; The frontmost term is n (m / 2 / !DPI / T)^(3.D/2.D)
; code: n = 1D6 & T = 500 & c = 3.0D8 & m = 5.11D5 / c^2 & maxTerm = n * (m / 2.D / !DPI / T)^(3.D/2.D) & PRINT,maxTerm
;
;SAMPLE from Orbit 1773, 09:27:01.89
;; energies=[815.4, 940.8, 1129., 1380., 1631., 1882., 2258., 2760., 3261., 3763.,4516., 5519., 6523., 7526., 9032., 1.104D4, 1.305D4,1.505D4,2.208D4, 3.011D4]
;; flux=[7.113D5, 7.245D5, 4.415D5, 1.694D5, 6.177D4, 2.504D4, 1.032D4,3854., 2237., 1246., 428.6, 417.9, 193.8, 108.6, 32.97, 20.04,5.744,4.978, 6.748, 2.489]
;; P=[energies[1],222,3.9,0.6,0.D]
;; modelFluxOverE=KAPPA_FLUX__LINEAR_SHIFT_IN_ENERGY__JE_OVER_E(energies,P)
;;plot1=PLOT((energies[2:-1]-energies[1])/1000.D,flux[2:-1]/energies[2:-1],XTITLE="Energy-$\Delta\Phi$ (keV)",YTITLE="J(E)/E (cm!U2!N-s-sr-eV!U2!N)",XLOG=1,YLOG=1,LINESTYLE='',SYMBOL='+')
;;plot2=PLOT((energies[2:-1]-energies[1])/1000.D,modelFluxOverE[2:-1],/OVERPLOT)
FUNCTION KAPPA_FLUX__LINEAR_SHIFT_IN_ENERGY__JE_OVER_E,X,P,DP, $
   NOTELECTRONS=notElectrons, $
   TAKE_STOCK_OF_RB=take_stock_of_RB, $
   TAKE_STOCK__RB=RB
   

  COMPILE_OPT IDL2,STRICTARRSUBS

  energy                 = X

  IF N_ELEMENTS(P) LT 4 THEN BEGIN
     PRINT,"Must have all four estimates for kappa dist! ( dPhi, T, kappa, n[, pitchAngle, m] )"
     PRINT,"Returning..."
     ;; RETURN,-1
  ENDIF

  IF KEYWORD_SET(notElectrons) THEN BEGIN
     PRINT,"NEED TO DERIVE CONSTANT FOR NOT-ELECTRONS."
     PRINT,"Could use mathematica journal 'journal__20180216__salvage_kappa_paper.nb'"
     STOP
  ENDIF

  dPhi                   = DOUBLE(P[0])
  T                      = DOUBLE(P[1])
  kappa                  = DOUBLE(P[2])
  n                      = DOUBLE(P[3])
  pitchAngle             = DOUBLE(P[4])*!DPI / 180.D

  eMDPhi                 = DOUBLE(TEMPORARY(energy))-TEMPORARY(dPhi)

  kappaS = DOUBLE(kappa)
  IF kappa LE 1.5 THEN BEGIN
     kappaS = 1.500001D
     ;; PRINT,"Kappa must be GE 1.5D!"
     ;; PRINT,"Returning..."
     ;; RETURN,-1
  ENDIF

  ;;Still fo' real
  IF kappa EQ 2.0 THEN kappaS = 2.00001D 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;;The whole thing is, as you see below, Finv*FK1*FK2*FK3

  ;;First chunk
  Finv            = n / (DOUBLE((!DPI * T * (kappaS - 1.5D ) )))^(1.5D) * 29675347.D ; 3.D8 / SQRT(102.2D)
  ;;The const at the end is (speedOfLight)/(consts arising from specifying n in cm^-3, mass=511 keV/c^2

  ;;Second chunk
  FK1             = KAPPA_GAMMARAT(kappa)

  ;;Third chunk, in parts that become useful later for PDs
                    
  fk2_innard      = 1.D + eMDPhi / ( ( kappaS - 1.5D ) * T )
  FK2             = ( fk2_innard ) ^ ( -1.D - kappa )

  ;;Fini
  F               = TEMPORARY(Finv)*TEMPORARY(FK1)*TEMPORARY(FK2)

  ;; Assume that this bad boy has been coming down the ol' field line, mirroring and whatnot with the first invariant conserved
  ;; IF KEYWORD_SET(take_stock_of_RB) THEN BEGIN

  ;;    killBelowVal = energy * ( 1.D - DOUBLE(RB) * (SIN(pitchAngle))^2) - E_b

  ;;    toKill       = WHERE(

  ;; ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;If the procedure is called with four parameters, calculate the
  ;;partial derivatives.
  IF N_PARAMS() GT 2 THEN BEGIN

     PRINT,"None of this has been updated after ripping from KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC"
     STOP

     requested              = dp
     dp                     = MAKE_ARRAY(N_ELEMENTS(x),N_ELEMENTS(p),VALUE=X[0]*0)
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 1: PDs wrt to dPhi

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 2: PDs wrt to T
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 3: PDs wrt to kappa--The worst of all, and the most important

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 4: PDs wrt to n

     
  ENDIF

  RETURN,F

END


