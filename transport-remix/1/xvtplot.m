D = 0.01;
M = 1;
t = linspace(.001,8);
C0 = 1;
x = sqrt(-4*D*t.*log(C0*sqrt(4*pi*D*t)/M));
plot(t,x)
title('Position at which Concentration is 1')
xlabel('t [time]')
ylabel('x_0 [length]')
print('-r600','-f1','-depsc','xvt.eps')