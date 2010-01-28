D = 1;
M = 1;
x = linspace(0,5);

figure()
plotinflection([0.95 1 1.05],x,D,M)
print('-r600','-depsc','inflection1.eps')

figure()
plotinflection([1.95 2 2.05],x,D,M)
print('-r600','-depsc','inflection2.eps')

figure()
plotinflection([2.95 3 3.05],x,D,M)
print('-r600','-depsc','inflection3.eps')

%%
x = linspace(0,5);
t = x;
xstar = x./sqrt(D*t);
Cstar = exp(-xstar.^2);
plot(Cstar,xstar)
xlabel('x^*')
ylabel('C^*')
title('Dimensionless Concentration Profile')
print('-r600','-depsc','inflection4.eps')