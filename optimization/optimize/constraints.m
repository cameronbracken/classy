function [c, ceq] = constraints(x)
  % This function evaluates the constraints 
  
  n=10;
  h=zack(-x,n);
  dem=6.6;
  minh=.7;      %h*, the minimum head value
  
  
  %the first part is the nonlinear min head constraint
  %the second part is the nonnegativity pumping constraint
  c = [ (minh-h)', -x ] ; 
  
  % The demand constraint rearranged to make leq
  % Not really a nonlinear constraint but easier to put here
  ceq = dem-sum(x); %dem-sum(x);