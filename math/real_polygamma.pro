;2016/05/10
;From Wikipedia (https://en.wikipedia.org/wiki/Polygamma_function, 2016/05/10):
;"In mathematics, the polygamma function of order m is a meromorphic function on \C and defined as the (m+1)-th derivative of
;the logarithm of the gamma function."

;I'm using complex step differentiation (http://blogs.mathworks.com/cleve/2013/10/14/complex-step-differentiation/), a
;beautifully simple method for getting the derivative of a real-valued, infinitely differentiable function that extends smoothly into
;the complex plane.
;It's based on this idea: F(x_0 + ih) = F(x_0) + ih*F'(x_0) - h^2*F''(x_0)/2! ih^3*F'''(x_0)/3! + ...
;                     ==> F'(x_0) = Im( F(x_0 + ih) ) / h+O(h^2)
;
;  From the blog post: "Simply evaluating the function F at the imaginary argument x0+ih, and dividing by h, gives an
;approximation to the value of the derivative, F'(x_0), that is accurate to order O(h^2). We might as well choose
;h=10−8. Then the error in the approximation is about the same size as the roundoff error involved in storing a double
;precision floating point value of F'(x_0)" (http://blogs.mathworks.com/cleve/2013/10/14/complex-step-differentiation/).
;  There are also several good papers on the topic, including "On the generalization of the Complex Step Method" by Abreu, Stich and
;Morales [2013].
;Based on http://oeis.org/A030169, the minimum of the gamma function is GAMMA(x_0) = 0, which occurs for x_0 = 1.46163214496...
FUNCTION REAL_POLYGAMMA,x,polygamma_index, $
                        STEPSIZE=h

  COMPILE_OPT idl2

  IF ~KEYWORD_SET(h) THEN h  = DOUBLE(1e-8)

  ;;Check data type; only valid for real-valued variables
  good_data_types            = [2,3,4,5,12,13,14,15]
  dType                      = SIZE(x,/TYPE)
  CASE (WHERE(dType EQ good_data_types))[0] OF
     -1: BEGIN
        PRINT,"You've provided a data type I can't deal with! No friend of mine..."
        PRINT,FORMAT='("Data type: ",I0)',dType
        badData              = 1
     END
     ELSE: BEGIN
        badData              = 0
     END
  ENDCASE

  ;;x valid?
  ;; IF N_ELEMENTS(x LE 1) THEN BEGIN
  ;;    PRINT,"Can't compute derivative from one element!"
  ;;    badData                 = 1
  ;; ENDIF

  IF (WHERE(x LE 0))[0] NE -1 THEN BEGIN
     PRINT,"Can't compute real polygamma function for negative values!"
     badData                 = 1
  ENDIF

  ;;Drop it if bad!
  IF badData THEN BEGIN
     RETURN,-1
  ENDIF ELSE BEGIN
     
     ;;Compute derivative based on polygamma index
     IF N_ELEMENTS(polygamma_index) EQ 0 THEN polygamma_index = 0
     CASE polygamma_index OF
        0: BEGIN
           ;; RETURN,REAL_DIGAMMA(x)
           RETURN,IMAGINARY(LNGAMMA(DCOMPLEX(x,h)/h))
        END
        1: BEGIN
           RETURN,( (IMAGINARY(LNGAMMA(DCOMPLEX(x+h,h))) + IMAGINARY(LNGAMMA(DCOMPLEX(x-h,(-h)))))*(0.5D/h^2))
        END
        ELSE: BEGIN
           PRINT,"Illegal polygamma index! Can only have polygamma_index = {0,1} right now..."
           badData           = 1
        ENDELSE
     ENDCASE
  ENDELSE

END