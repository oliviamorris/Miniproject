#MPM Practice
#lets look at a basic age-structured population

#Convert vital rates to a projection matrix, Porjectio nmatrices are square matrices
#where the number of rows and columns are equal to the number of life stages

#in this case, this means three
#first, specify a blank transition matrix, with 3 rows and 3 columns
tmat<- matrix(0, nrow = 3, ncol = 3)
#name the rows and columns
stagenames<- c("Juveniles", "Subadults", "Adults")
rownames(tmat)<-stagenames
colnames(tmat) <-stagenames
tmat
#the second row first column represents the production of subadults by previous years juveniles
#aka the transition rate from juvenile to subadult
#the value is 0.3
tmat[2,1]<-0.3
tmat
#fill in with data
tmat[,1] <- c(0,0.3,0)          # fill in the entire first column of the transition matrix
tmat[,2] <- c(0,0.4,0.1)        # fill in the entire second column of the transition matrix
tmat[,3] <- c(4,0,0.85)
tmat

#now run a 40 year projection

#first you must specify initial abundances in each stage
initabun<- c(40,0,0) #only 40 juveniles
names(initabun)<- colnames(tmat)
initabun

#run for 40 years using a loop
nyears<-40
allyears<- matrix(0, nrow = nrow(tmat), ncol = nyears+1) #creates an empty matrix of the correct dimensions of the tmat(3) and the subsquent 40 years 
rownames(allyears)<- rownames(tmat) #name rows
colnames(allyears)<- seq(0,nyears) #name columns by year number , from zero to forty
allyears[,1]<- initabun #at ,1 in matrix input initial abundance, i.e input 40 juveniles at 0
allyears #look

for(t in 2:(nyears+1)) { 
  allyears[,t]<- tmat %*% allyears[,t-1] #multiply transition matrix by allyears matrix
}

# %*% multiplies two matrices if they are conformable, if one argument is a vector, it will be promoted to either a row or column matrix to make both arguments conformable

allyears

#plot
plot(1,1, pch = "", ylim = c(0,50), xlim = c(0, nyears+1), xlab = "Years", ylab = "Abundance", xaxt = "n") #create empty plot
cols<-rainbow(3) #three colours

for (s in 1:3){
  points(allyears[s,],col=cols[s],type = "l", lwd=2)
}
#plot all three rows in allyears

axis(1, at = seq(1, nyears+1), labels = seq(0,nyears)) #adds 40 years on x axis
legend("topleft", col=cols, lwd = rep(2,3), legend = rownames(allyears)) #legend
#best way to plot? not sure


#whats limiting about mpm?
#density dependance 