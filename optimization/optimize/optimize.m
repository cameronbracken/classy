%this mfile sets up the runs the optimiztion
%           max f = sum(h_i)     maximize gw head
%   subject to:
%           sum(Q_i) = D         pumping must satisfy demand
%           h_i >= h*            head levels must not drop below standard
%           Q_i >= 0             no injection 
%the three auxilary files are:
%   objfun.m, zack_wrapper.m, constraints.m 

    %set the optimization options 
options = optimset('Algorithm', 'interior-point','MaxFunEvals',10000,...
    'Display','off');

    %get parameter set
clearvars -global *
global setting
    % 1=constant boundaries, 
    %2=linear decrease on both sides, 
    %3 = linear decrease one side log increase other
setting = 3;
    %do multiple runs and plot the optimal solution as a function of demand
multiple = 1;
        
[n,rows,dem,minh,maxq,xl,xr]=getPars();

prefix = 'figs/';
switch setting
    case 1
if(setting == 3)
    bounds = 'log-bounds';
elseif(setting == 2)
    bounds = 'log-bounds';
elseif(setting == 1)
    dem_mult = [linspace(0,8,20),8.1:.1:8.5];
    name = 'figs/zvd-const-bounds-minq.eps';
end
if(dem < 100), suffix = '-minq'; end

x0 = [1,repmat(0,[1,n-2]),1];       %the initial guess
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
        % Put the pumping rates back in the model to find the heads
        h=zack_wrapper(-q,n,rows,xl,xr);
        disp('The head levels at optimality:')
        hopt = reshape(h,n,rows);
        disp(hopt)
        m = colormap(jet(4));
        for i=1:rows
            plot(linspace(0,1,n+2),[xl(i),hopt(:,i)',xr(i)],...
                'Color',m(i,:),'LineWidth',2)
            hold on
        end
        legend('Array 1','Array 2','Array 3','Array 4')
        hold off
        figure()
        image(reshape(q,n,rows)','CDataMapping','scaled')
        axis off
        colorbar
        
        if(setting == 3 && dem < 100)
            name1 = 'figs/head-log-bounds-minq.eps';
            name2 = 'figs/head-log-bounds-minq.eps';
        elseif(setting == 2 && dem < 100)
            name1 = 'figs/head-log-bounds-minq.eps';
            name2 = 'figs/head-log-bounds-minq.eps';
        elseif(setting == 1 && dem < 100)
            dem_mult = [linspace(0,8,20),8.1:.1:8.5];
            name = 'figs/zvd-const-bounds-minq.eps';
        end
    end
else

    %do multiple runs changing the demand each time 
    if(setting == 3 && dem < 100)
        dem_mult = [linspace(0,8,20),8.1:.1:8.5];
        name = 'figs/zvd-log-bounds-minq.eps';
    elseif(setting == 2 && dem < 100)
        dem_mult = [linspace(0,4,20),4.1:.1:4.5];
        name = 'figs/zvd-linear-bounds-minq.eps';
    elseif(setting == 1 && dem < 100)
        dem_mult = [linspace(0,8,20),8.1:.1:8.5];
        name = 'figs/zvd-const-bounds-minq.eps';
    else
        dem_mult = linspace(5,25);
        name = 'figs/zvd.eps';
    end
    fval_mult = zeros(length(dem_mult),1);
    
    for i=1:length(dem_mult)
        
        [n,rows,dem,minh,maxq,xl,xr] = getPars();
        setPars(n,rows,dem_mult(i),minh,maxq,xl,xr);
        
        [q,fval_mult(i),exitflag,output] = fmincon(@objfun,x0,...
            [],[],[],[],[],[],@constraints,options);
        
        fprintf('%d Demand=%f Objfun=%f\n',i,dem_mult(i),-fval_mult(i))
        if(~exitflag)
            plot(dem_mult(1:i),fval_mult(1:i))
            error 'Optimization failed'
        end
    end
    
    plot(dem_mult,-fval_mult)
    xlabel('Demand (Volume/Time)')
    ylabel('Objective function at optimality')
    print('-r600','-depsc',name)
    
end

disp('output saved to op.out')
diary off