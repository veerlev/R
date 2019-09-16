library(dslabs)
data(heights)
names(heights)

#Use the unique and length functions to determine how many unique heights were reported
(length(unique(x)))

#Use the table function to compute the frequencies of each unique height value. Because we are using the resulting frequency table in a later exercise we want you to save the results into an object and call it tab.
x <- heights$height
(tab <- table(x))

#In the previous exercise we computed the variable tab which reports the number of times each unique value appears. For values reported only once tab will be 1. Use logicals and the function sum to count the number of times this happens.
sum(tab == 1)

#Define a variable male that contains the male heights.
#Define a variable female that contains the female heights.

male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
#Report the length of each variable.
(length(male))
(length(female))

male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
quantile(heights$height, seq(.01, 0.99, 0.01))
#Create two five row vectors showing the 10th, 30th, 50th, 70th, and 90th percentiles for the heights of each sex called these vectors female_percentiles and male_percentiles.
female_percentiles <- quantile(female, seq(0.1, 0.9, 0.2))
male_percentiles <- quantile(male, seq(0.1, 0.9, 0.2))
#Then create a data frame called df with these two vectors as columns. The column names should be female and male and should appear in that order. As an example consider that if you want a data frame to have column names names and grades, in that order, you do it like this:
df <- data.frame(Female = female_percentiles, Male = male_percentiles)

#Take a look at the df by printing it. This will provide some information on how male and female heights differ.
print(df)

# define x as vector of male heights
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum(x - average)^2)/length(x)
# built-in mean and sd functions - note that the audio and printed values disagree
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)
# calculate standard units
z <- scale(x)
# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)

x <- heights$height[heights$sex == "Male"]
mean(x > 69 & x <=72)

#Use the normal approximation to estimate the proportion the proportion of the data that is between 69 and 72 inches.
x <- heights$height[heights$sex=="Male"]
avg <- mean(x)
stdev <- sd(x)
#Given a normal distribution with a mean mu and standard deviation sigma, you can calculate the proportion of observations less than or equal to a certain value with pnorm(value, mu, sigma).
(pnorm(72, avg, stdev) - pnorm(69, avg, stdev))

x <- heights$height[heights$sex == "Male"]
exact <- mean(x > 79 & x <= 81)

#Use normal approximation to estimate the proportion of heights between 79 and 81 inches and save it in an object called approx.
avg <- mean(x)
stdev <- sd(x)

approx <- pnorm(81, avg, stdev) - pnorm(79, avg, stdev)
#Report how many times bigger the actual proportion is compared to the approximation.
(exact/approx)

# use pnorm to calculate the proportion over 7 feet (7*12 inches)
#Using the normal approximation, estimate the proportion of adult men that are taller than 7 feet, referred to as seven footers. Remember that 1 foot equals 12 inches.
#Use the pnorm function. Note that pnorm finds the proportion less than or equal to a given value, but you are asked to find the proportion greater than that value.
#Print out your estimate; don't store it in an object.
avg <- 69
stdev <- 3

 1 - pnorm(7 * 12, avg, stdev)

#Use your answer to the previous exercise to estimate the proportion of men that are seven feet tall or taller in the world and store that value as p.
#Then multiply this value by 1 billion (10^9) round the number of 18-40 year old men who are seven feet tall or taller to the nearest integer with round. (Do not store this value in an object.)
avg <- 69
stdev <- 3

 (round((1 - pnorm(7 * 12, avg, stdev)) * 10**9))

#There are about 10 National Basketball Association (NBA) players that are 7 feet tall or higher.

#Use your answer to exercise 4 to estimate the proportion of men that are seven feet tall or taller in the world and store that value as p.
#Use your answer to the previous exercise (exercise 5) to round the number of 18-40 year old men who are seven feet tall or taller to the nearest integer and store that value as N.
#Then calculate the proportion of the world's 18 to 40 year old seven footers that are in the NBA. (Do not store this value in an object.)
avg <- 69
stdev <- 3

N<- 1 - pnorm(7 * 12, avg, stdev)
p <- round((1 - pnorm(7 * 12, avg, stdev)) * 10**9)

 (10 / N)

## Repeat the calculations performed in the previous question for Lebron James' height: 6 feet 8 inches. There are about 150 players, instead of 10, that are at least that tall in the NBA.
p <- 1 - pnorm(6*12 + 8, 69, 3)
N <- round(p * 10^9)
150/N


# define x and z
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)
# proportion of data below 69.5
mean(x <= 69.5)
# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))
# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p) 
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)
