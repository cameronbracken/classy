%this mfile sets up the runs the optimiztion
%           max f = sum(h_i)     maximize gw head
%   subject to:
%           sum(Q_i) = D         pumping must satisfy demand
%           h_i >= h*            head levels must not drop below standard
%           Q_i >= 0             no injection 
%           Q_i <= Q*            max pumping rate
%the three auxilary files are:
%   objfun.m, zack_wrapper.m, constraints.m, setPars.m, getPars.m 

    %get parameter set
clear global *
clear *
global BC Q_LIMITING
    % 1 = constant boundaries, 
    % 2 = linear decrease on both sides, 
    % 3 = linear decrease one side log increase other
BC = 1;
    % 0 = single run, plot head and pumping rates
    % 1 = multiple runs and plot the optimal solution as a function of demand
multiple = 0;
    % 0 = qmax not limiting
    % 1 = qmax limiting
Q_LIMITING = 1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% No more parameters below here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %set the optimization options 
options = optimset('Algorithm', 'active-set','MaxFunEvals',10000,...
    'Display','off');
        
[n,rows,dem,minh,maxq,xl,xr]=getPars();

prefix = 'figs/';
switch BC
    case 1
        bounds = 'constant-bounds';
    case 2
        bounds = 'linear-bounds';
    case 3
        bounds = 'log-bounds';
end
if(maxq < 100), suffix = '-minq.eps'; else suffix = '.eps'; end
fprintf('maxq %f\n',maxq)

    %the initial guess
x0 = [1,repmat(0,[1,n-2]),1];
x0 = repmat(x0,[1,rows]);

delete('op.out')
diary('op.out')

if(~multiple)
        %computer: optimize!
    [q,fval,exitflag,output] = fmincon(@objfun,x0,[],[],[],[],[],[],...
        @constraints,options);

    %on output:
    %   q           is the value of decision variables (pumping rates)
    %   fval        is the objective function value
    %   exitflag    is 1 if optimal value was found 0 if not 
    %   output      is an object containing some of the statisics of the
    %               optimization

    if(exitflag && ~multiple)
        disp('OPTIMAL SOLUTION FOUND!!!!')
        disp('The optimal pumping rates:')
        disp(q)
        disp(output.message)
        % Put the pumping rates back in the model to find the heads
        h=zack_wrapper(-q,n,rows,xl,xr);
        %disp('The head levels at optimality:')
        hopt = reshape(h,n,rows);
        m = colormap(jet(rows));
        for i=1:rows
            plot(linspace(0,1,n+2),[xl(i),hopt(:,i)',xr(i)],...
                'Color',m(i,:),'LineWidth',2)
            hold on
        end
        legend('Array 1','Array 2','Array 3','Array 4')
        hold off
        
        figure()
        qopt = reshape(q,n,rows);
        m = colormap(jet(rows));
        for i=1:rows
            plot(linspace(0,1,n+2),[0,qopt(:,i)',0],...
                'Color',m(i,:),'LineWidth',2)
            hold on
        end
        legend('Array 1','Array 2','Array 3','Array 4')
        hold off
        
        name1 = sprintf('%s%s%s%s',prefix,'head-',bounds,suffix);
        name2 = sprintf('%s%s%s%s',prefix,'pumping-',bounds,suffix);
        print('-r600','-f1','-depsc',name1)
        print('-r600','-f2','-depsc',name2)
        fprintf('Printed %s\n',name1)
        fprintf('Printed %s\n',name2)
    end
else

    %do multiple runs changing the demand each time 
    if(Q_LIMITING)
        switch BC
            case 1
                dem_mult = [linspace(0,23.2,50)];
            case 2
                dem_mult = [linspace(0,12.4,50)];
            case 3
                dem_mult = [linspace(0,21.5,50)];
        end
    else
        switch BC
            case 1
                dem_mult = linspace(5,26.4,50);
            case 2
                dem_mult = linspace(5,13.2,50);
            case 3
                dem_mult = linspace(5,27.3,50);
        end
    end
    fval_mult = zeros(length(dem_mult),1);
    
    for i=1:length(dem_mult)
        
        [n,rows,dem,minh,maxq,xl,xr] = getPars();
        setPars(n,rows,dem_mult(i),minh,maxq,xl,xr);
        
        [q,fval_mult(i),exitflag,output] = fmincon(@objfun,x0,...
            [],[],[],[],[],[],@constraints,options);
        
        fprintf('%02d Demand=%f Objfun=%f\n',i,dem_mult(i),-fval_mult(i))
        if(~exitflag)
            plot(dem_mult(1:i),fval_mult(1:i))
            disp('Optimization failed')
        end
    end
    fval_mult = - fval_mult;
    plot(dem_mult,fval_mult)
    xlabel('Demand (Volume/Time)')
    ylabel('Objective function at optimality')
    name = sprintf('%s%s%s%s',prefix,'zvd-',bounds,suffix);
    print('-r600','-depsc',name)
    fprintf('Printed %s\n',name)
    save(sprintf('%s%s',name,'.mat'),'dem_mult','fval_mult')
    
end

disp('output saved to op.out')
diary off