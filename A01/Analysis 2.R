# Subject Age, Model Fit and Vehicle Year:

mydata$subject.age.log <- log(mydata$subject.age)

# Creating a subset of SanAntonio:

SanAntonio <- subset(mydata, city=="sa")
SanAntonio

# 2a)
# We want to calculate the sample mean, sample median and sample std
# sample skewness and sample kurtosis for each of subject.age.log and
# subject.age.log for San Antonio. Display your results in a table:

# Mean from the mean() function
age.mean <- mean(SanAntonio$subject.age)
age.log.mean <- mean(SanAntonio$subject.age.log)

# Median from the summary
summary(SanAntonio$subject.age)
summary(SanAntonio$subject.age.log)

# sample standard deviation
age.sd <- sd(SanAntonio$subject.age)
age.log.sd <- sd(SanAntonio$subject.age.log)

# Defining Skewness and Kurtosis Functions:
# Skewness:
skewness <- function(x) {
  (sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)
}

#Kurtosis:
kurtosis <- function(x) {
  (sum((x - mean(x))^4)/length(x))/(sum((x - mean(x))^2)/length(x))^2
}
# Saving Variables
age.skewness <- skewness(SanAntonio$subject.age)
age.log.skewness <- skewness(SanAntonio$subject.age.log)

# age.kurtosis
# age.log.kurtosis

age.kurtosis <- kurtosis(SanAntonio$subject.age)
age.log.kurtosis <- kurtosis(SanAntonio$subject.age.log)

# age.kurtosis
# age.log.kurtosis


# At this point, can I stop with the variables, or is there a better way
# to present this information in a table? Not sure if we've been taught how to
# take variables and present them in a dataframe/table

# 2b) Generate a relative frequency histogram with a suitable 
# superimposed Gaussain Probability Density Function curve for each of
# subject.age and subject.age.log for San Antonio.

# First, we need to download the library to run a relative frequency histogram
# and gain access to truehist. I'll also use the command to allow 2 images in my
# plotting window:
library(MASS)
par(mfrow = c(1,2))


truehist(SanAntonio$subject.age, xlab = "Age of Subjects",
         ylab = "Relative Frequency", main = "Relative Frequency Histogram of Subject's Ages",
         col = "dodgerblue3")

truehist(SanAntonio$subject.age.log, xlab = "log Age of Subjects",
         ylab = "Relative Frequency", main = "Relative Frequency Histogram of the Log of Subject's Ages",
         col = "dodgerblue3")

# Adding a Gaussain Probability Density Function 
# Copying and pasting the histograms above and adding:

truehist(SanAntonio$subject.age, xlab = "Age of Subjects",
         ylab = "Relative Frequency", main = "Relative Frequency Histogram of Subject's Ages",
         col = "dodgerblue3")
curve(dnorm(x, age.mean, age.sd), col="red", add=TRUE, lwd=1.5)

truehist(SanAntonio$subject.age.log, xlab = "log Age of Subjects",
         ylab = "Relative Frequency", main = "Relative Frequency Histogram of the Log of Subject's Ages",
         col = "dodgerblue3")
curve(dnorm(x, age.log.mean, age.log.sd), col='red', add=TRUE, lwd=1.5)

# I'd like to note here that this choice of bins, though default, looks pretty
# good for the shape of the graph. Though it may help to trial an error
# some other bin sizes and see if something else is better.


# 2c) 
# plot an e.c.d.f for this dataset:
plot(ecdf(SanAntonio$subject.age), xlab = "Subject's Ages", 
          main="e.c.d.f of Subject Ages",
          las=1, lwd=2, pch=NA)
curve(pnorm(x, age.mean, age.sd), col="red", add=TRUE, lwd=1.5, lty=2)

plot(ecdf(SanAntonio$subject.age.log), xlab="Log of Subject's Ages", 
     main="e.c.d.f of log of Subject Ages",
     las=1, lwd=2, pch=NA)

curve(pnorm(x, age.log.mean, age.log.sd), col="red", add=TRUE, lwd=1.5, lty=2)


# 2d)
# How well does your data fit a Gaussian Model?
# from the original dataset, we can find the mean, median, skewness and kurtosis
age.kurtosis
age.skewness

age.log.kurtosis
age.log.skewness

# the kurtosis and skewness are practically there (close to 3 and 0), so I'd say that 
# it is close enough to reasonable assume a gaussian distribution.
# Also, it's worth noting that the log function of the age has a better kurtosis
# and skewness for a gaussian distribution
# I'm not too sure what is meant by compare at least 3 variates; I thought the age
# was the variate here.


# 2e)
# Generate a scatterplot whose x-axis corresponds to subject.age
# and y-axis corresponds to vehicle.year for San Antonio

plot(SanAntonio$subject.age, SanAntonio$vehicle.year, xlab="age", ylab="vehicle age")

# 2f)
# Calculate the correlation between the 2 variates:
cor(SanAntonio$subject.age, SanAntonio$vehicle.year)
# from this we see that the correlation is 0.038. The correlation coefficient
# goes from -1 to 1, 0 indicating no correlation. Because the coefficient is so close
# to 0, I think it makes sense to assume there is a weak correlation, almost no correlation

