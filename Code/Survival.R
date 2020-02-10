#############


#survival rate
#1.calculate the survival rate of a month. It was treated as 30 days a month

##yuxins orginal function
survival <- function(m, TC){  #m : mass, the unit is gram
  Temp = TC -5+273.15                                        #OM: -5? 
  E = 0.45                                                   #OM:activation energy in fish?(ref-savage)this may change per fish/lifestage? check how this effects it?
  k = 8.61*(10^(-5))                                         #OM:boltzmann constant 8.617333262145×10−5 in eV⋅K−1
  V <- E/ (k*Temp)                                           #OM: =exp
  Z <- (m ^(-0.23))*exp(19)/exp(V) # year survival rate      #OM: e(19), from intercept in figure in savage aper, eqn of line is -0.23 +19 i think (not used biotraits data)
  Za <- (1-Z)^(1/12)  # /12 to calculate month survival rate   #not using data, only using paper
  print(Z)
  return(Za)                                                 #OM: returns mortality rate per month
}

#have a look:
survival(100, 15) #mass in g, temp in degrees celsius

plot(survival(0:1000, 0))
plot(survival(0:1000, 20))
plot(survival(0:10, 20))
plot(survival(0:10, 10))




###############################################################################################
#livis
#1. Survival rate for a month
survivaln <- function(m, TC, z0=19, E=0.45){  #m: mass (g)?, TC: temp degrees celsius, z0: normalisation constant, E: activation energy
  Temp = TC+273.15    #to kelvin                                    
  k = 8.62*(10^(-5))  #boltzmann constant                                     
  Z <- exp(z0) * (m^-0.23) * exp(-(E/(k*Temp)))
  #print(paste("e^z0 =", exp(z0)))
  #print(paste("m^-0.23 =", (m^-0.23)))
  #print(paste("exp(-(E/(k*Temp))) =", exp(-(E/(k*Temp))))) 
  print(Z)
  Za <- (exp(-Z/12)) # /12 the initial rate- to calculate month survival rate, then e it to get probability of survival
  return(Za)    #returns mortality rate per month
}



survivaln(200, 0)

#plots
plot(survivaln(0:10, 20))
plot(survivaln(0:2000, 0))
plot(survivaln(0:2000, 5))
plot(survivaln(0:2000, 10))
plot(survivaln(0:2000, 15))
plot(survivaln(0:2000, 20))

require(ggplot2)
library(RColorBrewer)
brewer.pal(5, "Reds")

qplot(c(0, 2000), geom = "blank")+stat_function(fun=survivaln, args = (TC=0), color = "#FEE5D9")+stat_function(fun=survivaln, args = (TC=5), colour = "#FCAE91")+
  stat_function(fun=survivaln, args = (TC=10), colour = "#FB6A4A")+stat_function(fun=survivaln, args = (TC=15), colour ="#DE2D26")+
  stat_function(fun=survivaln, args = (TC=20), colour = "#A50F15")

#plot like yuxins
set <- seq(0,10000, 0.1)

part1 <- matrix(0, nrow = length(set), ncol = 3)
part1[,1] <- rep(-5, length(set)) 
part1[,2] <- set 
part1[,3] <- survivaln(set,-5) 

part2 <- matrix(0, nrow = length(set), ncol = 3)
part2[,1] <- rep(0, length(set)) 
part2[,2] <- set 
part2[,3] <- survivaln(set,0) 

part3 <- matrix(0, nrow = length(set), ncol = 3)
part3[,1] <- rep(5, length(set))
part3[,2] <- set 
part3[,3] <- survivaln(set,5)

part4 <- matrix(0, nrow = length(set), ncol = 3)
part4[,1] <- rep(10, length(set))
part4[,2] <- set 
part4[,3] <- survivaln(set,10)

part5 <- matrix(0, nrow = length(set), ncol = 3)
part5[,1] <- rep(15, length(set)) 
part5[,2] <- set 
part5[,3] <- survivaln(set,15)

part6 <- matrix(0, nrow = length(set), ncol = 3)
part6[,1] <- rep(20, length(set))
part6[,2] <- set 
part6[,3] <- survivaln(set,20)

whole <- rbind(part1, part2, part3, part4,part5, part6)
colnames(whole) <-  c("Temperature","Mass","Survival")
whole <- as.data.frame(whole)


ggplot(whole, aes(x= Mass, y= Survival, group = Temperature, colour = Temperature))+
  geom_line(cex = 0.6)+
  scale_x_log10()+
  ylim(0.50, 1.0)+
  xlab("log(Mass)")+
  ylab("Monthly survival")+
  scale_color_gradient(low = "deepskyblue", high = "tomato")+
  theme_classic()


  #geom_vline(xintercept = 1.5, linetype="solid", color = "black", size=0.5) +
  #geom_vline(xintercept = 3, linetype="solid", color = "black", size=0.5) +
  geom_vline(xintercept = 16, linetype="solid", color = "black", size=0.5) +
  #geom_vline(xintercept = 70, linetype="solid", color = "black", size=0.5)+
  geom_vline(xintercept = 200, linetype="solid", color = "black", size=0.5) +
  geom_vline(xintercept = 1500, linetype="solid", color = "black", size=0.5) +
  
  #annotate(geom="text", x=0.5, y=0, label="egg",color="black", size = 3) +
  #annotate(geom="text", x=2, y=0, label="alevin",color="black", size = 3) +
  annotate(geom="text", x=4, y=0, label="fry",color="black", size = 3) +
  annotate(geom="text", x=50, y=0, label="parr",color="black", size = 3) +
  annotate(geom="text", x=500, y=0, label="smolt",color="black", size = 3)+
  #annotate(geom="text", x=600, y=0, label="marine salmon",color="black", size = 3)+
  annotate(geom="text", x=5000, y=0, label="adult",color="black", size = 3)+
  xlab("Mass (g)")

#biotratits data
read.csv("Documents/PhD/Miniproject/Data/Biotraits_embryomortality.csv")


########################################################################################################################################################
#sensitivity analysis of parameters in survival function 
library(multisensi)

#Model implementation The R function survival is created to run the model for given values
#of m, TC and for a vector Ea of output activation energies? (The output of verhulst is the vector of population
#sizes at the times in t)

#function simplified#
survival <- function(m, TC, E){  #m : mass, the unit is gram
  output <- (1-((m ^(-0.23))*exp(19)/exp(E/ ((8.61*(10^(-5)))*(TC+273.15) )))^(1/12))  # /12 to calculate month survival rate
 return(output)                                                 #OM: returns mortality rate per month
}

#Since the methods implemented in multisensi require to run the dynamic population model repeatedly, another function called verhulst2 is created. It takes a dataframe of input combinations as
#its first argument and the time steps of interest as its second argument..

ea= seq(from = 0.4, to = 1.5 , by = .1 )
survival2<- function(X, E=ea){
  out <- matrix(nrow = nrow(X), ncol = length(E), NA)
  for (i in 1:nrow(X)) {
    out[i, ] <- survivaln(X$m[i], X$TC[i], E )
  }
  out <- as.data.frame(out)
  names(out) <- paste("E", E, sep = "")
  return(out)
}


#Now we want to perform sensitivity analyses on the population sizes with respect to the three
#uncertain parameters: K, Y0, and a. This can be done in different ways by using the main
#function of the package, which is called multisensi like the package itself.
#A first and obvious option is to perform separate sensitivity analyses at t = 5, 10, ..., 100.
#To do this, multisensi must be used with the argument reduction=NULL :


surv.seq<- multisensi(model = survival2, reduction = NULL, center = FALSE,
                      design.args = list(m = c(100, 200, 300,400), TC= c(0,10,20,30)))

#The result surv.seq is an object of class dynsi, which has specific print, summary, and plot methods. 
#The print and summary functions give the sensitivity indices :
print(surv.seq, digits = 2)

# color palettes: rainbow, heat.colors, terrain.colors, topo.colors, cm.colors
plot(surv.seq, normalized = TRUE, color = heat.colors, gsi.plot = FALSE)
title(xlab = "Ea")
plot(surv.seq, normalized = FALSE, color = heat.colors, gsi.plot = FALSE)
title(xlab = "Ea")



#mass?

survival <- function(TC, E, m){  #m : mass, the unit is gram
  output <- (1-((m ^(-0.23))*exp(19)/exp(E/ ((8.61*(10^(-5)))*(TC+273.15) )))^(1/12))  # /12 to calculate month survival rate
  return(output)                                                 #OM: returns mortality rate per month
}


mass= seq(from = 0, to = 30000 , by = 10 )
survival2<- function(X, m=mass){
  out <- matrix(nrow = nrow(X), ncol = length(m), NA)
  for (i in 1:nrow(X)) {
    out[i, ] <- survivaln(X$TC[i], X$E[i], m )
  }
  out <- as.data.frame(out)
  names(out) <- paste("m", m, sep = "")
  return(out)
}


surv.seq<- multisensi(model = survival2, reduction = NULL, center = FALSE,
                      design.args = list( TC= c(0,10,20,30), E = c(1.4,1.6,1.8,2.0)))

#The result surv.seq is an object of class dynsi, which has specific print, summary, and plot methods. 
#The print and summary functions give the sensitivity indices :
print(surv.seq, digits = 2)

# color palettes: rainbow, heat.colors, terrain.colors, topo.colors, cm.colors
plot(surv.seq, normalized = TRUE, color = heat.colors, gsi.plot = FALSE)
title(xlab = "Ea")
plot(surv.seq, normalized = FALSE, color = heat.colors, gsi.plot = FALSE)
title(xlab = "Ea")



#########################################################################################################################################################
#work out better eqn before senisity analysis to look at evs etc