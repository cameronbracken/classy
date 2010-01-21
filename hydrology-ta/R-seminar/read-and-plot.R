d = read.table("http://www.humboldt.edu/%7Ecwb12/R/thermocouple.txt", header = T)

d$Temp = 0.9662 * d$Temp - 0.9662


fit = lm(Temp ~ Voltage, data = d)

plot(d$Vexpected, d$Temp, 
		col="red", 
		pch=25, 
		ylab = "Temperture [deg C]" ,
		xlab = "Voltage [mV]")
		
points(d$Voltage, d$Temp)

abline(fit)
legtxt = c("Measured Voltage","Expected Voltage","Linear Data Fit")

legend("topleft", legend = legtxt, 
		col = c("black","red"), 
		pch=c(1,25,-1), 
		lty=c("blank","blank","solid"))
		
summary(fit)