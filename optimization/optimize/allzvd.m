x = {'figs/zvd-constant-bounds-minq.eps.mat' 
'figs/zvd-linear-bounds.eps.mat'
'figs/zvd-constant-bounds.eps.mat'
'figs/zvd-log-bounds-minq.eps.mat'
'figs/zvd-linear-bounds-minq.eps.mat'	
'figs/zvd-log-bounds.eps.mat'};

dem = zeros(6,50);
fval = dem;
for i=1:length(x)
    load(char(x(i)),'dem_mult','fval_mult')
    dem(i,:) = dem_mult;
    fval(i,:) = fval_mult;
end
plot(dem(1,:),fval(1,:),...
dem(2,:),fval(2,:),...
dem(3,:),fval(3,:),...
dem(4,:),fval(4,:),...
dem(5,:),fval(5,:),...
dem(6,:),fval(6,:))
legend('1(a)','1(b)','1(c)','2(a)','2(b)','3(b)')
xlabel('Demand')
ylabel('Optimal objective function value')

name = 'figs/allzvd.eps';
fprintf('Printed %s\n',name)
print('-r600','-depsc',name)