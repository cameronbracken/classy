x = linspace(0,1);
y=linspace(-1,1);
[X Y] = meshgrid(x,y);

Mdot = 1;
D = 1;
u = 1;

C = Mdot./sqrt(4*pi*D*X*u).*exp(-Y.^2*u./(4*D*X));

contourf(X,Y,C)
xlabel('X')
ylabel('Y')
%title('2D Point Source Concentration Profile')
colorbar
caxis([0 3])

print('-r600','-depsc','plume2d-contour.eps')

surf(X,Y,C)
view(72,42)
xlabel('X')
ylabel('Y')
zlabel('Concentration')
%title('2D Point Source Concentration Profile')
colorbar
caxis([0 3])

print('-r600','-depsc','plume2d-surf.eps')