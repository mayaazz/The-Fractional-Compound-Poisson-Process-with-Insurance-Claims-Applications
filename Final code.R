
#Importing the dataset 
data<-read.csv( file.choose(), header = TRUE)
attach(data)
names(data)
str(data)
View(data)
head(data)

#Obtaining X_t

#Converting the data into a data frame 
library(dplyr)
d<-data.frame(data$Date.timeOfAccident,data$UltimateIncurredClaimCost)
head(d)

# Convert Date.timeOfAccident to POSIXct type
d$data.Date.timeOfAccident <- as.POSIXct(d$data.Date.timeOfAccident, format =  "%d/%m/%Y %H:%M")
d$data.Date.timeOfAccident

# Extract the date part from Date.timeOfAccident
d$data.Date.timeOfAccident <- as.Date(d$data.Date.timeOfAccident)
d$data.Date.timeOfAccident

#Group by the date and add their respective costs 
results <- d %>%
  mutate(Date = as.Date(data.Date.timeOfAccident)) %>%
  group_by(data.Date.timeOfAccident) %>%
  summarize(Total_Cost = sum(data.UltimateIncurredClaimCost))

#Print results
print(results)

#Calculate the accumulated sums of the ultimate incurred claim costs
accsum<-cumsum(results$Total_Cost)
head(accsum)
length(accsum)

#Calculate the differences between the accumulated sums 

sequence <- seq(from = 100, to = 6163, by =100) #However, when t=300, 100 must change to 300. Likewise for t=500.
sequence
#Calculate the differences between every t terms
differences <- diff(accsum[sequence])
head(differences)
#Print the sequence of differences
print(differences)

options(max.print =1000 )

#Define the differences of the accumulated sums as a vector
#However, set the first accumulated sum to be the intial value 
diffaccsums<-c(accsum[1],differences)
head(diffaccsums)

#Set t=100, followed by t=300 and finally t=500 
#First and second sample moments 
m1<-mean(diffaccsums,na.rm = TRUE)
m1
m2<-mean(diffaccsums^2,na.rm = TRUE)
m2

#Note: Reload the dataset, otherwise the results will be incorrect
#Importing the dataset 
data<-read.csv( file.choose(), header = TRUE)
attach(data)
names(data)
str(data)
View(data)
head(data)

#Obtaining Y_i

#Define s to be an empty vector to store the aggregated incurred claims 
s <-mat.or.vec(length(Distances),1)

#Computing the aggregated incurred claims at the same time stamp, since no two claims may arrive at the same point
for (i in 1:length(Distances)) { 
  #Counting the frequency of each Date\time variable
  count<- sum(Date.timeOfAccident[i]==Date.timeOfAccident)
  #If the same Date\time variable occurs twice, then print the sum of their respective claim costs 
  if (count==2 && Date.timeOfAccident[i]==Date.timeOfAccident[i+1]) {
    s[i]<-print(UltimateIncurredClaimCost[i]+UltimateIncurredClaimCost[i+1])
  }
  #If the same Date\time variable occurs more than twice, then print the sum of their respective claim costs 
  if (count>2 && Date.timeOfAccident[i]==Date.timeOfAccident[i+count-1]) {
    s[i]<-print(sum(UltimateIncurredClaimCost[i:(i+count-1)]))
  }
  #If the Date\time variable occurs once, then print its respective claim cost 
  if (count==1) {
    s[i]<-print(UltimateIncurredClaimCost[i])
  } 
  
}

#Removing the zeros in the vector
newUltimateclaimcost<-s[s!=0]
length(newUltimateclaimcost)

#Removing repetition in the occurrence rate
Distances<-unique(Distances)
head(Distances)
length(Distances)


#Removing repetition in the Date.timeOfAccident
Date.timeOfAccident<-unique(Date.timeOfAccident)
head(Date.timeOfAccident)
length(Date.timeOfAccident)

#Converting the data into a data frame 
library(dplyr)
data<-data.frame(Date.timeOfAccident,newUltimateclaimcost, Distances)
head(data)

#Parametric estimation
#Fit a distribution to the aggregated incurred claim costs-Y_i

#Install and load the required packages 
library(MASS)
library(ParetoPosStable)

#Fit a Pareto distribution
fit2<-pareto.fit(newUltimateclaimcost,estim.method="MLE")
fit2
fitdistr(newUltimateclaimcost,"pareto")

#The parameters of the Pareto distribution
shape<- fit2$estimate[1]
scale<- fit2$estimate[2]

#However, since the shape parameter is less than one, a finite mean does not exist.
#So the Pareto distribution is no longer a suitable distribution for the jumps of the claims


#Fit a log-normal distribution to the aggregated incurred claim costs
fit3<-fitdistr(newUltimateclaimcost, "log-normal")

#The parameters of the log-normal distribution
mu <- fit3$estimate["meanlog"]
sigma<- fit3$estimate["sdlog"]

print(mu)
print(sigma)

#Estimate the first and second order moments for the aggregated incurred claim costs
#w_1 and w_2 for the log-normal distribution
w_1.3=exp(mu+((1/2)*(sigma^2)))
w_2.3=exp(2*mu+(2*(sigma^2)))

print(w_1.3)
print(w_2.3)

#Define the equation
equation <- function(a, v) {
  a * beta(a, a) - v
}

#Define the function
root_function <- function(a, v) {
  equation(a, v)
}

#Let ahat denote the estimator for the parameter alpha
#Let uhat denote the estimator for the parameter u

#Estimating ahat*B(ahat,ahat) in equation 4.4.8
v=(m2/((m1)^2))-(w_2.3/(w_1.3*m1))
print(v)

#Find the root for the estimator ahat, using the uniroot function
root1 <- uniroot(root_function, c(0.0000000001, 2), v = v)

#Extract the root
root_value1 <- root1$root
ahat2<-root_value1

#Print the result
cat("Root:", root_value1, "\n")


t=100  #The unit of time is first set to be 100, followed by t=300 and finally t=500

#Estimator for uhat in equation 4.4.7
uhat2=(m1*gamma(ahat2+1))/(w_1.3*(t^ahat2))
uhat2

#Fit a Weibull distribution on the aggregated incurred claims
fit<-fitdistr(newUltimateclaimcost, "weibull")

#The parameters of the Weibull distribution
shape <- fit$estimate["shape"]
scale <- fit$estimate["scale"]

#Print the estimated parameters
print(shape)
print(scale)

#The first and second-order moments for the Weibull distribution
w_1.1=scale*gamma(1+(1/shape))
w_2.1=(scale^2)*gamma(1+(2/shape))
print(w_1.1)
print(w_2.1)

#Estimator for ahat in equation 4.4.8
v=(m2/(m1^2))-(w_2.1/(w_1.1*m1)) #Estimate for ahat*B(ahat,ahat)
print(v)

#Find a root for ahat using the uniroot function
root2 <- uniroot(root_function, c(0.01, 2), v = v)

#Extract the root
root_value2 <- root2$root
ahat3<-root_value2

#Print the result
cat("Root:", root_value2, "\n")

#First define the unit of time to be t=100, then t=300 and finally t=500
t=100
#Estimator for uhat in equation 4.4.7
uhat3=(m1*gamma(ahat3+1))/(w_1.1*(t^ahat3))
uhat3


#Calculate the log-likelihood of the Weibull distribution 
log_likelihoodweibull <- sum(dweibull(newUltimateclaimcost, shape = shape, scale = scale, log = TRUE))
print(log_likelihoodweibull)
#Calculate the log-likelihood of the log-normal distribution
log_likelihoodlognormal<-sum(dlnorm(newUltimateclaimcost,meanlog = mu,sdlog = sigma, log = TRUE))
print(log_likelihoodlognormal)


#The log-normal distribution obtained the highest log-likelihood. 
#Therefore, it is the most suitable distribution for the jumps of the claims

#Non-parametric estimation 
install.packages("stats")
library(stats)

#Kernel density estimation, with an optimal bandwidth and a Gaussian kernel function is utilised 

#Perform kernel density estimation on the aggregated incurred claim costs
density<- density(newUltimateclaimcost,
                  bw = "nrd0",   # Bandwidth selection method-Silverman's rule-of-thumb
                  adjust = 1,     # Bandwidth adjustment
                  kernel = "gaussian",  # Kernel function
                  weights = NULL,  # Weights for observations
                  window = kernel, # Window function
                  width = NULL,    # Bandwidth specification
                  give.Rkern = FALSE,  # Whether to give the R kernel
                  subdensity = FALSE,  # Whether to compute sub-densities
                  warnWbw = FALSE,     # Whether to warn about bandwidth
                  n = length(newUltimateclaimcost),    # Number of grid points
                  from =min(newUltimateclaimcost),  # Lower limit of range
                  to = max(newUltimateclaimcost),    # Upper limit of range
                  cut = 3,    # Number of bandwidths to try
                  ext = 4,    # Number of extension bandwidths
                  old.coords = FALSE,  # Use old coordinate generation
                  na.rm = FALSE)       # Whether to remove NAs in data



#The points at which the kernel density estimate is evaluated: y
head(density$x)
#The kernel density estimates for the points in x: hat(f)(y)
head(density$y)

#Plot the estimated density function
plot(density, main = "The Probability Density Function",xlim=c(0,10000),ylim=c(0,0.00015),xlab="x",ylab="Density")

#Calculate the first and second order moments for the jumps of the claims, w_1 and w_2, through the rectangular rule 
kdm<-data.frame(density$x,density$y)
head(kdm)

# Create a vector to store the results
rec <- numeric(length(density$x))  

#Compute the rectangular rule approximation
for (i in 1:(length(density$x))) {
  h <- density$x[2] - density$x[1]  # Width of each rectangle (constant increment throughout)
  rec[i] <- density$x[i]*density$y[i]*h #Area of each rectangle
}


#Compute the summation of the points
integral <- sum(rec)

#Print the first order moments-w_1
print(integral)
w_1<-integral 

#Compute the second order moments through the rectangular rule
#Create a vector to store the results
rec2 <- numeric(length(density$x))  

#Compute the rectangular rule approximation
for (i in 1:(length(density$x))) {
  h<- density$x[2] - density$x[1]  # Width of each rectangle (increment)
  rec2[i] <- ((density$x[i])^2)*density$y[i] * h           # Area of each rectangle
}

#Compute the sum of the points
integral2 <- sum(rec2)

#Print the second order moments-w_2
print(integral2)
w_2<-integral2

#Print the results
cat("First-order moment (mean):", w_1, "\n")
cat("Second-order moment:", w_2, "\n")

#Place the values for w_1, w_2, m1 and m2 into the estimators derived in chapter 4 section 4

#Estimator for ahat in equation 4.4.8
v=(m2/(m1^2))-(w_2/(w_1*m1)) #Estimate for ahat*B(ahat,ahat)
print(v)

#Define the equation
equation <- function(a, v) {
  a * beta(a, a) - v
}

#Define the function
root_function <- function(a, v) {
  equation(a, v)
}

#Find the root for ahat using the uniroot function 
root1 <- uniroot(root_function, c(0.0001, 2), v = v)

#Extract the root
root_value1 <- root1$root
ahat<-root_value1
ahat

t=100 #The unit of time is first set to be t=100, followed by t=300 and finally t=500

#Estimator for uhat in equation 4.4.7
uhat=(m1*gamma(ahat+1))/(w_1*(t^ahat))
uhat

#Plotting the log-normal curve, the Weibull curve and the empirical curve on the same axis
library(EnvStats)
library(stats)

x <- seq(min(newUltimateclaimcost), max(newUltimateclaimcost), length.out = 27549)
#Log-normal data
lognormal<- dlnorm(x, mu,sigma )
#Weibull data 
weibull <- dweibull(x, shape, scale)


#Non-parametric distribution
plot(density, main = "The Distributions for the Jumps of the Claims",xlim=c(0,10000),ylim=c(0,0.00020),xlab="x",ylab="y")

lines(x,lognormal,,type = "l", col = "blue", lwd = 2)

#Add the Weibull distribution to the plot
lines(x,weibull, col = "red", lwd = 2)

#Add legend
legend("topright", legend = c("Empirical Distribution","Log-Normal Distribution", "Weibull Distribution"), col = c("black","blue", "red"), lwd = 2)




