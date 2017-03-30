;2017/03/30
PRO GET_FA_FAC_VECTORS,t1,t2, $
                       ORBSTRUCT=orbStruct, $
                       TIME_ARRAY=time_array, $
                       FAC=fac, $
                       VEL_FAC=facv, $
                       DESPUN_SAXIS_SPLANE=spin

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF SIZE(orbStruct,/TYPE) NE 8 THEN BEGIN
     GET_FA_ORBIT,t1,t2,TIME_ARRAY=time_array, $
                  /NO_STORE, $
                  STRUC=orbStruct, $
                  /ALL, $
                  /DEFINITIVE
  ENDIF
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;FAC  
  b_unit_GEI           = NORMN3(orbStruct.B_model)
  r_unit_GEI           = NORMN3(orbStruct.fa_pos)

  z_unit_GEI           = MAKE_ARRAY(N_ELEMENTS(orbStruct.time),VALUE=1.0D,/DOUBLE) # TRANSPOSE([0.,0.,1.])

  eGEI_unit_GEI        = NORMN3(CROSSN3(z_unit_GEI,r_unit_GEI))             ;;; GEI East unit vector
  nGEI_unit_GEI        = NORMN3(CROSSN3(r_unit_GEI,eGEI_unit_GEI))          ;;; GEI North unit vector 
  eMAG_unit_GEI        = NORMN3(CROSSN3(b_unit_GEI,r_unit_GEI))
  nMAG_unit_GEI        = CROSSN3(eMAG_unit_GEI,b_unit_GEI)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Spin-axis/plane vectors

  ;;Get attitude data for making spin-axis unit vector
  GET_FA_ATTITUDE,orbStruct.time,/TIME_ARRAY,STRUCT=att

  spinA_unit_GEI       = [[SIN(att.angles[*].spin_ra*!DTOR)], $
                          [COS(att.angles[*].spin_ra*!DTOR)], $
                          [SIN(att.angles[*].spin_dec*!DTOR)]]

  unit_spinPlaneB      = CROSSN3(b_unit_GEI,spinA_unit_GEI)

  spinP_nMAG_comp_GEI  = TOTAL(nMAG_unit_GEI*spinA_unit_GEI,2)
  spinP_eMAG_comp_GEI  = TOTAL(eMAG_unit_GEI*spinA_unit_GEI,2)

  norm_spPl            = SQRT(spinP_nMAG_comp_GEI^2+spinP_eMAG_comp_GEI^2)
  spinP_nMAG_comp_GEI /= norm_spPl
  spinP_eMAG_comp_GEI /= norm_spPl

  ;;See how aligned the spin axis is with geographic/geomagnetic east?
  alignedness_eGEO     = TOTAL(eGEI_unit_GEI*spinA_unit_GEI,2)
  alignedness_eMAG     = TOTAL(eMAG_unit_GEI*spinA_unit_GEI,2)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;FAC-V system
  unitQ                = NORMN3(CROSSN3(orbStruct.fa_vel,orbStruct.B_model))             ;;; y-axis 
  unitv_fperp          = NORMN3(CROSSN3(orbStruct.B_model,unitQ))                  ;;; x-axis

  facv                 = {b        : b_unit_GEI , $
                          c        : unitQ      , $ ;cross-track component
                          a        : unitv_fperp, $ ;along-track component
                          coordSys : 'GEI'      }

  fac                  = {b        : b_unit_GEI   , $
                          e        : eMAG_unit_GEI, $
                          n        : nMAG_unit_GEI, $
                          coordSys : 'GEI'        }

  spin                 = {plane    : {nMag : spinP_nMAG_comp_GEI, $
                                      eMag : spinP_eMAG_comp_GEI}, $
                          axis     : spinA_unit_GEI              , $
                          coordSys : 'GEI'                       }
END
