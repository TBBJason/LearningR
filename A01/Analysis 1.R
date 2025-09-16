# 1a) 
# Generate a table that shows the number AND
# percentage of stops for each category

### Ask about the best way to sort and send "other" to the end
### I've googled you can manually set an order, but is there a way to just 
### ensure that 'other' goes to the end without altering the other categories?

# Categories: subject.race or vehicle.make

# sorting by cities
cities = mydata$city

# Saving tables for wanted data
vehicle_make <- mydata$vehicle.make
subject_race <- mydata$subject.race

# Taking into account the city they're recorded in
vehicle_make_by_city <- table(vehicle_make, cities)
# Number of the vehicles per city by stop
vehicle_make_by_city
# Find the percent
prop.table(vehicle_make_by_city, margin = 2) * 100

# Applying the same to race:
subject_race_by_city <- table(subject_race, cities)
subject_race_by_city
prop.table(subject_race_by_city, margin=2) * 100
# We want to find the number and the percent for each
# We'll use the table() function to create the frequency
# tables for each of the columns
table_vehicle <- table(vehicle_make) 
table_vehicle

table_race <- table(subject_race)
table_race

# To find the percent, we'll take the number of 
# frequencies and divide by the total:
# Given the dataset, this should be same total for both
total <- sum(table_vehicle)

percent_vehicle <- (100 * (table_vehicle) / total)
percent_vehicle

percent_race <- (100* (table_race) / total)
percent_race

#1b)
# Generate a barplot for each city for the number
# of total stops within each category

# First, let's separate our data for specific cities
cities <- mydata$city

# Looking for a table to create a grouped barplot
vehicle_stops <- table(cities, vehicle_make)
race_stops <- table(cities, subject_race)

# Viewing our data
vehicle_stops
race_stops

# Looks good, moving on to creating a grouped barchart
# Choosing 420 as an arbitrary ylimit.

vehicle_bar_plot <- barplot(vehicle_stops, beside=T, xlab="Vehicle Makes", 
                            ylab = "Stop Frequencies", las = 1, 
                            main = "stops from vehicle.make and cities",
                            col = c("forestgreen", "dodgerblue3"))
legend("topright", legend=c("NO", "SA"), fill = c("forestgreen", "dodgerblue3"),
       density=50, angle=c(45, 135))


# race_bar_plot <- barplot(race_stops, beside=T, col=c("forestgreen", "dodgerblue3"))

# 1c)
# is the distribution of your chosen variate similar
# or different for the cities? 

# for the vehicle makes, it looks more even, numbers vary my a maximum of 6%
# per category. Do to the symmetry of the data, one could reasonable 
# extrapolate that the make of vehicle doesn't have a substantial impact
# on whether or not its stopped

# race is another story
