###################Fecundity eqn########################

#length weight relationship
#read in data from De Eyto, E. et al, 2015. The fecundity of wild Irish Atlantic salmon Salmo salar L. and its application for stock assessment purposes. Fisheries Research, 164, pp.159-169.
lw<-read.csv("Documents/PhD/Miniproject/Data/lengthweight.csv")
#plot
ggplot(lw ,aes(length.m., weight.kg.))+
  geom_point()+
  stat_smooth(method = "lm", formula=y~poly(x,2)) 



# 6. calculate the fecundity.
fecundity <- function(m, TC){
  m <- m/1000                                          #OM: as the relationship is in kg and m is in g
  egg_number <- (m^ 0.976) * exp(7.2924)               #OM: from fecundity eqn Coefficients: (Intercept) 7.2924      log(ConSize)  0.9767  
  egg_number <- round(egg_number)                      #OM: round it to whole number
  return(egg_number)                                   #OM: returns egg number depending on mass and temp
}

#how is it using TC, its not...
#OM: testing 
(1^ 0.976) * exp(7.2924) 
