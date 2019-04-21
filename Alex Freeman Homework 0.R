#Question 1

vector1 <- c(1, 2, 3, 4, 5)
"Mindy" <-c(12)
matrix1 <- matrix(1:6, nrow=2, byrow=TRUE)
matrix2 <- matrix(1:6, nrow=2)
matrix3 <- matrix(1, nrow=10, ncol=10)
vector2 <- c("THIS", "IS", "A", "VECTOR")
sum <- function (a, b, c) {a+b+c}
comparison <- function (q) {if (q<=10){print("Yes")}else{print("No")}}
g <- rnorm(1000, 10, 1)
y <- rnorm(1000, 5, 0.5)
x = NULL 
for(i in 1:1000){
  x[i] <- mean(sample(x = g, size = 10, replace = TRUE))
}
x
regression <- lm(y ~ x)
summary(regression)
#these results show that there is a  small positive correlation 
#betweeen x and y, indicated by the positive slope coefficient 0.04638. 

#Question 2
library(readr)
pums_chicago <- read.csv("Desktop/pums_chicago.csv")

dimension <- dim(pums_chicago)
#there are 204 variables in the pums_chicago dataset

meanincome <- mean(pums_chicago$PINCP[!is.na(pums_chicago$PINCP)])
#the mean annual income is $38247.62

PINCP_LOG <- log(!is.na(pums_chicago$PINCP))

pums_chicago$PINCP_LOG <- log(pums_chicago$PINCP)
#NaN values were produced. This refers to a division of 0/0 
#where the result is not a number 

GRAD.DUMMY <- ifelse(pums_chicago$SCHL>12, "grad", "no grad")

pums_chicago$SERIALNO <- NULL 

write.csv(pums_chicago, file = "pums_chicago.csv")

under16 <- pums_chicago[!is.na(pums_chicago$ESR), ]
employed <- pums_chicago[pums_chicago$ESR == 1 | pums_chicago$ESR ==2, ]
unemployed <- pums_chicago[pums_chicago$ESR == 3, ]
armedforces <- pums_chicago[pums_chicago$ESR == 4 | pums_chicago$ESR == 5, ]
notarmedforces <- pums_chicago[pums_chicago$ESR == 6, ]

employed_af <- rbind(employed, armedforces)

data.frame(employed_af$AGEP, employed_af$RAC1P, employed_af$PINCP_LOG)

meantraveltime <- mean(pums_chicago$JWMNP[!is.na(pums_chicago$JWMNP)])
#The mean travel time is 34.84 minutes
mediantraveltime <- median(pums_chicago$JWMNP[!is.na(pums_chicago$JWMNP)])
#The median travel time is 30 minutes
percentile <- quantile(pums_chicago$JWMNP[!is.na(pums_chicago$JWMNP)], 0.80)
#The 80th percentile would be 45 minutes of travel time 

realnumbers <- pums_chicago[!is.na(pums_chicago$JWMNP) & !is.na(pums_chicago$WAGP), ]
correlation <- cor(realnumbers$JWMNP, realnumbers$WAGP)
#The correlation coefficient is -0.042, showing a negative correlation between
#travel time to work and annual wages 

scatterplot <- plot(pums_chicago$AGEP, pums_chicago$PINCP_LOG)

crosstab <- table(pums_chicago$ESR, pums_chicago$RAC1P)

regression2 <- lm(pums_chicago$WAGP ~ pums_chicago$WKHP)

errors <- residuals(regression2)
estimatedwage <- fitted.values(regression2)
comparisonplot <- plot(errors, estimatedwage)
#This plot shows that there is heteroskedasticity in the data. As the estimated
#wage increases, the number of errors present decreases in turn

data(mtcars)

carspeed <- lm(mtcars$mpg~mtcars$wt)

automatic <- mtcars[mtcars$am == 0, ]
manual <- mtcars[mtcars$am == 1, ]
automatic_regression <- lm(automatic$mpg ~ automatic$wt)
manual_regression <- lm(manual$mpg ~ manual$wt)

log_horsepower <- log(!is.na(mtcars$hp))
horsepower_regression <- lm(mtcars$mpg ~ log_horsepower)

install.packages("ggplot2")
library("ggplot2")

ggplot2 <- plot(mtcars$wt, mtcars$mpg)

color = ifelse(mtcars$am == 1, "blue" , "yellow")

shape = mtcars$gear

labs (x = "Weight", y = "Miles per Gallon")

theme_bw()

