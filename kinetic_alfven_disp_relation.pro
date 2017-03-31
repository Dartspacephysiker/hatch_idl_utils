;2017/03/31
FUNCTION KINETIC_ALFVEN_DISP_RELATION, $
   L_PERP=l_perp, $
   L_PAR=l_par, $
   N=n, $
   OMEGA_=omega, $
   OMEGAP=omegaP, $
   V_ALFVEN=v_Alfven, $
   ALTITUDE=altitude, $
   MLT=mlt, $
   ION_GYROR=ion_gyror, $
   E_INERT_LEN=e_inert_len

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; omegaP      = 0.D
  
  c_light      = 299792458.D


  solve_l_perp = 0
  solve_l_par  = 0
  solve_dens   = 0
  

  have_l_perp = N_ELEMENTS(l_perp) GT 0
  have_l_par  = N_ELEMENTS(l_par ) GT 0
  have_dens   = N_ELEMENTS(n     ) GT 0

  CASE 1 OF
     (have_dens AND have_l_par AND have_l_perp): BEGIN
        PRINT,"Why are you here, then?"
        RETURN,-1
     END
     (have_dens AND have_l_par): BEGIN
        solve_l_perp = 1

        k_par        = 2.D*!PI/l_par ;rad/m

     END
     (have_dens AND have_l_perp): BEGIN
        solve_l_par  = 1

        k_perp       = 2.D*!PI/l_perp ;rad/m

     END
     (have_l_par AND have_l_perp): BEGIN

        solve_dens   = 1

        k_par        = 2.D*!PI/l_par ;rad/m
        k_perp       = 2.D*!PI/l_perp ;rad/m

     END
     ;; N_ELEMENTS(l_perp) EQ 0: BEGIN
     ;;    solve_l_perp = 1

     ;; END
     ;; N_ELEMENTS(l_par) EQ 0: BEGIN
     ;;    solve_l_par = 1
     ;; END     
  ENDCASE

  ;;Get plasma frequency
  IF ~solve_dens THEN BEGIN
     IF N_ELEMENTS(omegaP) EQ 0 THEN BEGIN
        
        IF N_ELEMENTS(n) EQ 0 THEN BEGIN

           STOP

        ENDIF
        
        omegaP = 5.64D4 * SQRT(n)

     ENDIF

     e_inert_len = c_light / omegaP

  ENDIF

  ;;Get Alfvén speed
  IF N_ELEMENTS(v_Alfven) EQ 0 THEN BEGIN 

     IF N_ELEMENTS(altitude) EQ 0 THEN BEGIN
        STOP
     ENDIF

     IF N_ELEMENTS(mlt) EQ 0 THEN BEGIN
        STOP
     ENDIF

     v_Alfven = ALFVEN_SPEED_MLT(altitude,mlt)

  ENDIF


  IF N_ELEMENTS(ion_gyror) EQ 0 THEN BEGIN
     ion_gyror = 0.D
  ENDIF

  FOR k=0,N_ELEMENTS(v_Alfven)-1 DO BEGIN

     PRINT,FORMAT='(A0,T20,": ",F0.3," (",A0,")")',"v_Alfven",v_Alfven[0],"km/s"
     PRINT,FORMAT='(A0,T20,": ",F0.3," (",A0,")")',"rho_ion",ion_gyror[0]/1000.D,"km"


  ENDFOR
  

  ;;printers
  IF ~KEYWORD_SET(solve_dens) THEN BEGIN
     PRINT,FORMAT='(A0,T20,": ",F0.3," (",A0,")")',"lambda_e",e_inert_len[0]/1000.D,"km"
     PRINT,FORMAT='(A0,T20,": ",F0.3," (",A0,")")',"omega_pl",omegaP[0],"rad/s"
     PRINT,FORMAT='(A0,T20,": ",F0.3," (",A0,")")',"density ",omegaP[0],"cm^-3"
  ENDIF

  IF ~KEYWORD_SET(solve_par) THEN BEGIN
     PRINT,FORMAT='(A0,T20,": ",F0.3," (",A0,")")',"lambda_par",l_par/1000.D,"km"
     PRINT,FORMAT='(A0,T20,": ",G0.3," (",A0,")")',"k_par"     ,k_par*1000.,"rad/km"
  ENDIF

  IF ~KEYWORD_SET(solve_perp) THEN BEGIN
     PRINT,FORMAT='(A0,T20,": ",F0.3," (",A0,")")',"lambda_perp",l_perp/1000.D,"km"
     PRINT,FORMAT='(A0,T20,": ",G0.3," (",A0,")")',"k_perp"     ,k_perp*1000.,"rad/km"
  ENDIF

  CASE 1 OF
     solve_l_par: BEGIN

        sqrtNumerator = 1.D + (kPerp*e_inert_len)^2.D
        sqrtDenom     = 1.D + (kPerp*ion_gyror)^2.D

        kPar          = omegaP / (v_Alfven * 1000.D) * SQRT(sqrtNumerator/sqrtDenom)

        RETURN,kPar

     END
     solve_l_perp: BEGIN

        l_perp        = 2.D * !PI * e_inert_len / SQRT( ( k_par * ( v_Alfven * 1000.D ) / omega )^2.D - 1 )
        k_perp        = SQRT( ( k_par * ( v_Alfven * 1000.D ) / omega )^2.D - 1.D) / e_inert_len

        PRINT,"****SOLN****"
        PRINT,FORMAT='(A0,T20,": ",F0.3," (",A0,")")',"lambda_perp",l_perp/1000.D,"km"
        PRINT,FORMAT='(A0,T20,": ",F0.3," (",A0,")")',"k_perp"     ,k_perp*1000.D,"rad/km"


        RETURN,l_perp

     END
     solve_dens: BEGIN

        ;; l_perp        = 2.D * !PI * e_inert_len / SQRT( ( k_par * ( v_Alfven * 1000.D ) / omega )^2.D - 1 )
        e_inert_len   = SQRT( ( k_par * ( v_Alfven * 1000.D ) / omega )^2.D - 1.D) / k_perp

        omegaP = c_light / e_inert_len
        n      = (omegaP / 5.64D4)^2.D


        PRINT,"****SOLN****"
        PRINT,FORMAT='(A0,T20,": ",G0.4," (",A0,")")',"lambda_e",e_inert_len[0]/1000.D,"km"
        PRINT,FORMAT='(A0,T20,": ",G0.4," (",A0,")")',"f_plas",omegaP[0]/(2.D * !PI)/1D6,"MHz"
        PRINT,FORMAT='(A0,T20,": ",F0.3," (",A0,")")',"density ",n,"cm^-3"

     END
  ENDCASE


END
