movie = avifile('concentration.avi', 'compression', 'None'); % open movie

x = linspace(-100,100,1000);							  % range of x values for plot data
t = [linspace(0.01,.49,25),linspace(.5,10,25)];
M = 1;												  % Initial mass of polutant
D = 0.01;											  % Diffusion coefficient
nframes = length(t);								% number of frames in the movie
Cul = 15;

for k = 1:nframes
	C = M/sqrt(4*pi*D*t(k))*exp(-x.^2*4*D*t(k));	% Concentration profile
	
	plot(x,C,'k');			  % plot data
	axis([min(x) max(x) 0 Cul]);		 % set axis limits
	xlabel('Distance along tube [Length]','FontSize',12);
	ylabel('Concentration [Mass/Length]','FontSize',12);
	title('Concentration profile over time','FontSize',14) ;
	
	% prints slope value in upper left corner of movie frame:
	text(.9*min(x),.9*Cul,['time = ' num2str(t(k), '%4.2f')], ...
		  'Margin', 5, 'EdgeColor', 'k');  
	
	frame = getframe(gcf);			% get movie frame
	movie = addframe(movie, frame);	% add frame to movie
end

close(gcf);					  % close current figure
movie = close(movie);			% close movie