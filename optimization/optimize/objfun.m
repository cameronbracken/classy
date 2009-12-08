function f = objfun(x)
    % This function evaluates the objective function given the Q_i's 
    % (by calling zack_wrapper)
    
    [n,rows,dem,minh,maxq,xl,xr]=getPars();
    
    %x is the pumping rates, must be negative for the model
    h=zack_wrapper(-x,n,rows,xl,xr);
    
    %fmincon minimizes by default so f must be negative to maximize
    f=-sum(h);