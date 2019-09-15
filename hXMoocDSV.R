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

