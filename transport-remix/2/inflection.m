D = 1;
M = 1;
t = [0.95 1 1.05];
x = linspace(0,3);

stya = ['k' 'k' 'k'];
styb = ['-' '-' '-'];
styc = ['.' ' ' '-'];

hold on
for i=1:3
    
    C = M/sqrt(4*pi*D*t(i))*exp(-x.^2/(4*D*t(i)));
    plot(x,C,[stya(i) styb(i) styc(i)])
    
end

xlabel('x [length]')
ylabel('C [concentration]')
legend('t = 0.95', 't = 1', 't = 1.05')

for i=1:3
    
    vline(sqrt(2*D*t(i)),[stya(i) styb(i) styc(i)])%,['Inflection point, t = ',num2str(t(i))])
    
end

print('-r600','-f1','-depsc','inflection1.eps')