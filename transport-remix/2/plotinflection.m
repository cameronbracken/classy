function plotinflection(t,x,D,M)
    
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
    legend(['t = ',num2str(t(1))], ['t = ',num2str(t(2))], ['t = ',num2str(t(3))])

    for i=1:3
    
        vline(sqrt(2*D*t(i)),[stya(i) styb(i) styc(i)])%,['Inflection point, t = ',num2str(t(i))])
    
    end
    hold off
end