function f = objfun(x)
    %this function evaluates the objective function

    
    n=10;    %number of nodes
    
    %x is the pumping rates, must be negative for the model
    h=zack(-x,n);
    
    %fmincon minimizes by default so f must be negative to maximize
    f=-sum(h);