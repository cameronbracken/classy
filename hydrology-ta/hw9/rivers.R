rivers <- read.csv('rivers.csv')

	#Pull out the largest, smalles and middle rivers
largest <- rivers[rivers$Specific.Runoff >= .98,]
smallest <- rivers[rivers$Specific.Runoff < 0.15,]
middle <- rivers[rivers$Specific.Runoff > 0.15 & rivers$Specific.Runoff < .98,]

	# Plot the Specific runoff
with(rivers, 
	plot(Drainage.Area,Specific.Runoff,log='xy',xlim=c(1,1000),ylim=c(.01,10),
		main='Specific Runoff'))
	#Add the text for the largest and smallest rivers
with(largest, text(Drainage.Area, Specific.Runoff, Number,pos=4))
with(smallest, text(Drainage.Area, Specific.Runoff, Number,pos=4))

	# Plot all the rivers and a best fit scaling relationship 
with(rivers, 
	plot(Drainage.Area,Mean.Runoff,log='xy',xlim=c(1,1000),ylim=c(1,10000),
		main='All Rivers'))

	# Best fit scaling relationship 
all.fit <- lm(log10(Mean.Runoff)~log10(Drainage.Area),data=rivers)
abline(all.fit)

	#add the numbers
with(largest, text(Drainage.Area, Mean.Runoff, Number,pos=4))
with(smallest, text(Drainage.Area, Mean.Runoff, Number,pos=4))

	# Plot the Mean runoff for the middle rivers
with(middle, 
	plot(Drainage.Area,Mean.Runoff,log='xy',xlim=c(1,1000),ylim=c(1,10000),
		main='Middle Rivers'))
	
middle.fit <- lm(log10(Mean.Runoff)~log10(Drainage.Area),data=middle)
abline(middle.fit)

	#Plot the largest rivers and a best fit scaling relationship
with(largest, 
	plot(Drainage.Area,Mean.Runoff,log='xy',xlim=c(1,1000),ylim=c(1,10000),
	main='Largest Rivers'))
with(largest, text(Drainage.Area, Mean.Runoff, Number,pos=4))

largest.fit <- lm(log10(Mean.Runoff)~log10(Drainage.Area),data=largest)
abline(largest.fit)