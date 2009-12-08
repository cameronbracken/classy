function [c, ceq] = constraints(x)
  % This function evaluates the constraints 
  
  q = x;

  [n,rows,dem,minh,maxq,xl,xr]=getPars();
  
  h=zack_wrapper(-q,n,rows,xl,xr);
  
  %the first part is the nonlinear min head constraint
  %the second part is the nonnegativity pumping constraint
  %the third part is the max pumping constraint
  c = [ (minh-h), -q, (q-maxq) ] ; 
  
  % The demand constraint rearranged to make leq
  % Not really a nonlinear constraint but easier to put here
  ceq = dem-sum(q); %dem-sum(x);