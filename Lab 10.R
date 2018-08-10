########################################################
#Lab 10 Multiple Linear Regression Continued. Non Parametric Tests
########################################################
library(ggplot2)

########################################################
#Part 1 Multiple Linear Regression Continued - Interaction Terms, Predictions
#########################################################

CarData = mtcars[1:7] #Use only first 7 columns from the table 
 
CarsLM1 = lm(mpg ~ cyl + disp + hp + drat + wt + qsec, data = CarData)

summary(CarsLM1)

#p-value < 0.0001. R sauqre = 0.85

windows()
pairs(CarData[2:7])
#we can see correlation between several variables

cor(CarData[2:7])

#cyl and disp highly correlated. hp and cyl also. Disp is highly correlated with all variables. So we might 
#decide to take it away

#we consider creating a new model without that variable. hp could also be removed

CarsLM2 = lm(mpg ~ cyl + hp + drat + wt + qsec, data = CarData)
summary(CarsLM2)
#p-value <0.001 R square 0.85

AIC(CarsLM1) #159
AIC(CarsLM2) #158.5

#we create a third model

CarsLM3 = lm(mpg ~ cyl + drat + wt + qsec, data = CarData)
summary(CarsLM3)
#p-value < 0.0001. R squared 0.84

AIC(CarsLM3) #157.7

# we will choose the third model

CarsLM4 = lm(mpg ~ cyl + drat + wt + qsec + cyl*wt, data=CarData)

interaction.plot(CarData$wt, CarData$cyl, CarData$mpg)

#the changing effect variable, the interaction variable, dependent

summary(CarsLM4)


MPG = predict(CarsLM4, CarDataNew) # we are now uding the new table (withouhg mpg info) to estimate the mpg
View(MPG)                           #this is for applying the model that we created.

CarTable = data.frame(CarDataNew, MPG)

View(CarTable)


###############################################################
#Non-Parametric Tests
###############################################################

?warpbreaks

View(warpbreaks)

WoolA = subset(warpbreaks, wool == "A")
WoolB = subset(warpbreaks, wool == "B")


#Mann Whitney U Test (because is not "paired" data)

WoolManWhitney = wilcox.test(WoolA$breaks, WoolB$breaks)
  #H0 = the medians are NOT significantly different
  #H1 = the medians ARE significantly different for wool A and B

  #Median breaks are not significantly different between wool A and wool B (W=431, p=0.25, alpha=0.05)
  
WoolKW = kruskal.test(breaks ~ tension, data=warpbreaks) #here more than 2 variables (3), we use ANOVA for median

#H0 = the median breaks are NOT significantly different for differinf levels of tension
#H1 = at least one level of tension has a significantly different median breaks

#KW chi-squared = 10.8, df = 2 (3 variables - 1), p value = 0.0045. 
#at least one level of tension has significantly different meadian breaks (X^2(2) = 10.809, p=0.004, alpha = 0.05)

#PostHoc Test for Kruskal Wallis is the Dunne's test (as Tukey HSD was for ANOVA)


############################################
#Part 3 User Defined Functions
############################################

ExampleFunction = function(a,b,c) {
  Math = a^2 + a*b +b/c #you give it a name for then asking R to return that. The name could be anything
  return(Math)}

ExampleFunction(10,23,2) #I am giving the parameters here... result = 341.5

#a) n=PV/RT

IGFunction = function(i,j,k,l){
  Moles = (i*j) / (k*l)
  return(Moles)
}

IGFunction(2.6, 3.3, 0.0821, 317) #=0.329 Moles
  
  #i=P, j=V, k=R, l=T 

IGMoles = IGFunction(IdealGas$P, IdealGas$V, IdealGas$R, IdealGas$T) #calculates for the whole table IdealGas

NewIdealGas = data.frame(IdealGas, IGMoles) #creates a new data frame adding the column for the amount of Moles
View(NewIdealGas)



####################################################
#High Resolution Figures
####################################################

tiff(filename="ShiniHighRes.tiff", width=6, height=4, units="in", res=200) #add better resolution

ggplot(mtcars, aes(disp,mpg))+
  geom_point()
  dev.off()

  



