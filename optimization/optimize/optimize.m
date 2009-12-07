%this mfile sets up the runs the optimiztion
%           max f = sum(h_i)     maximize gw head
%   subject to:
%           sum(Q_i) = D         pumping must satisfy demand
%           h_i >= h*            head levels must not drop below standard
%           Q_i >= 0             no injection 
%the three auxilary files are:
%   objfun.m -      z = objfun(Q) 
%                   evaluates the objective function given the Q_i's (by
%                   calling zack)
%   zack.m -        h=zack(Q)
%                   solves the diffusion equation, takes pumping rates, gives
%                   back heads
%   constraints.m - evaluates the nonlinear constraints (calls zack)
%


    %set the optimization options 
options = optimset('LargeScale','off','Algorithm', 'active-set');
x0=[1 0 0 0 1 0 0 0 0 1];       %the initial guess

delete('op.out')
diary('op.out')

    %computer: optimize!
[x,fval,exitflag,output] = fmincon(@objfun,x0,[],[],[],[],[],[],@constraints,options);

%on output:
%   x           is the value of decision variables (pumping rates)
%   fval        is the objective function value
%   exitflag    is 1 if optimal value was found 0 if not 
%   output      is an object containing some of the statisics of the
%               optimization

if(exitflag)
    'All that stuff up there means OPTIMAL SOLUTION FOUND!!!!'
    'The optimal pumping rates:'
    q=x;
    q'
    % Put the pumping rates back in the model to find the heads
    h=zack(-x,10);
    'The head levels at optimallity:'
    h
    'output saved to op.out'
end
diary off