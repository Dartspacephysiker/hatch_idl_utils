;2016/03/19 From the IDL docs, X is "An Nterms by Npoints array of independent variable data, where Nterms is the number of
;coefficients (independent variables) and Npoints is the number of samples."
; You can form X by doing X = [TRANSPOSE[independent_vec1],TRANSPOSE[independent_vec2],...]
FUNCTION MAKE_MULTIPLE_LINREGRESSION_STRUCT,X,y,Y_ERRORS=measures

  fitCoeffs       = REGRESS(X, y, $
                            CHISQ=chisq, $
                            CONST=fitConst, $
                            CORRELATION=correlationVec, $
                            /DOUBLE, $
                            FTEST=fVal_GOF_test, $
                            MCORRELATION=mCorrCoeff, $
                            MEASURE_ERRORS=measures, $
                            SIGMA=sigma, $
                            STATUS=status, $
                            YFIT=yfit)
  
  linFitStr       = { $
                    fitCoeffs:fitCoeffs, $
                    fitConst:fitConst, $  ; the constant of the fitted line
                    yFit:TRANSPOSE(yfit), $
                    mCorrCoeff:mCorrCoeff, $
                    correlationVec:correlationVec, $
                    measure_errors:KEYWORD_SET(measures) ? measures : MAKE_ARRAY(N_ELEMENTS(y),/DOUBLE), $
                    fVal_GOF_test:fVal_GOF_test, $
                    chiSq:chiSq, $
                    sigma:sigma, $
                    status:status, $
                    double:1 $
                    } 

  RETURN,linFitStr

END