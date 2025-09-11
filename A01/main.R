# Introductory Material // Loading Dataset
mydata <- read.csv("stat231f25dataset.csv")
dim(mydata)
colnames(mydata)
mydata[1:5, 13:14]
mydata$vehicle.make[1:5]


# Checking NA values
sum(is.na(mydata))