x = linspace(-5,5);
y = linspace(0,5);
M = 1;
D = 1;
t = [0.1,.5,1,10];

x0 = 1;
y0 = 1;
%x = x0;

for i = 1:length(t)
    Cx(:,i) = twoDC(1,y,t(i),M,D,x0,y0) + twoDC(1,y,t(i),M,D,x0,-y0);
end
                   
plot(y,Cx(:,1),'k',y,Cx(:,2),'k--',y,Cx(:,3),'k:',y,Cx(:,4),'k-.')
legend(['t = ',num2str(t(1))],['t = ',num2str(t(2))],...
       ['t = ',num2str(t(3))],['t = ',num2str(t(4))])