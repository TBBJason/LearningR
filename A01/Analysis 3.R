#3a)
# favourite animal (group) - I think snow leopards are pre cool
# favourite animal (specific) - my dog Izzy. She's a poodle.

#3b)
num=sum(SanAntonio$vehicle.make=="ford")
denom = sum(table(SanAntonio$vehicle.make))
percent = 100*num/denom
percent
# around 15.3%

#3c)
summary(SanAntonio$vehicle.year)
# the range is from the max = 2019, subtracted by the min = 1986
2019 - 1986
# 33

#3d)
NewOrleans <- subset(mydata, city=="no")
table(SanAntonio$vehicle.colour)
table(NewOrleans$vehicle.colour)
# the most in both is "other"

#3e)
# this category doesn't make the most sense as a median value...
# I guess you could organize each car by position in time and then
# find the median, but that data doesn't feel too useful anyways.
