;2016/05/10
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
FUNCTION REAL_DIGAMMA,x


  divVal        = DOUBLE(1e-6)

  gamma_prime   = IMAGINARY(GAMMA(DCOMPLEX(x,divVal))/divVal)
  real_digamma  = gamma_prime / GAMMA(x)

  RETURN,real_digamma

END