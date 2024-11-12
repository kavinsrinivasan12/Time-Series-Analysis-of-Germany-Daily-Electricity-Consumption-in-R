# Creating a Data Framefrom our dataset
data <- read.csv("C:\\Users\\kavin\\Downloads\\opsd_germany_daily.txt", 
                 header=T, row.names = "Date")
data

# Looking at part of data frame using head() or tail()
head(data)
tail(data)

# View data in Tabular Format
View(data)

# Retrieve the dimension of object
dim(data)

# Check data type of each column in data frame
str(data)

# Looking at date column ( will not show data as its index)
head(data$Date)

# Looking at row names (indexes)
row.names(data)

# Accessing a specific row
data['2006-01-01',]

data['2017-08-10',]

# Accessing multiple rows
data[c('2006-01-01', '2006-01-04'),]

# Summary 
summary(data)

# Without parsing date column
data_2 <- read.csv("C:\\Users\\kavin\\Downloads\\opsd_germany_daily.txt", header=T)
data_2

# Look at Date column
str(data_2$Date)

# Convert into date format
x <- as.Date(data_2$Date)
head(x)
class(x)
str(x)

# Create year, month, day columns
year <- as.numeric(format(x,'%Y'))
head(year)

month <- as.numeric(format(x,'%m'))
month

day <- as.numeric(format(x, '%d'))
day

# Add columns to the existing data frame
data_2 <- cbind(data_2,year,month,day)
head(data_2)

# First 3 rows
data[1:3,]

head(sample(data_2,8))

" Let's create a Line Plot of the full time series of Germany's
  daily electricity consumption, using the data frame's plot() method.
"

# Using PLOT()

plot(data_2[,2], xlab="Year", ylab="Consumption", type="l", lwd=2, col='green',
     xlim=c(2006,2018))
plot(data_2[,2], xlab="Year", ylab="Consumption", type="l", lwd=2, col='violet',
     xlim=c(2006,2018), ylim=c(900,2000), main="Consumption Graph")

# Taking log values of consumption and take differences of logs
plot(10*diff(log(data_2[,2])), xlab="Year", ylab="Consumption",
     type="l", lwd=2, ylim=c(-5,5), main="Consumption Graph", col='orange')

# Using ggplot()
install.packages("ggplot2")
library(ggplot2)

ggplot(data=data_2, aes(x=year, y=Consumption, group=1)) + geom_line(linetype='dashed') + geom_point()

ggplot(data=data_2, mapping=aes(x=year, y=Consumption, col='red')) + geom_point()

"   We can see that the plot() method has chossen pretty good tick locations
    (every two years) and labels (the years) for the x-ais, which is helpful,
    However, with so many data points, the line plot is crowded and hard to read.
    Thus we can go with plot()
"

# Plot the data considering the solar and wind time series too..

# Wind Column
min(data_2[,3], na.rm=T)
max(data_2[,3], na.rm=TRUE)

# Consumption Column
min(data_2[,2], na.rm=TRUE)
max(data_2[,2], na.rm=T)

# Solar
min(data_2[,4], na.rm=T)
max(data_2[,4], na.rm=TRUE)

# Wind + Solar
min(data_2[,5], na.rm=T)
max(data_2[,5], na.rm=TRUE)

# OR
plot1 <- plot(data_2[,2], xlab="Year", ylab="Daily Totals (Gwh)", type="l",
              lwd=2, main="Consumption", col="orange",
              ylim=c(840,1750))

plot2 <- plot(data_2[,4], xlab="Year", ylab=" Daily Totals (Gwh)", type="l",
              main="Solar", ylim=c(0,500), col="blue")

plot3 <- plot(data_2[,3], xlab="Year", ylab="Daily Totals (Gwh)",
              type="l", lwd=2, main="Wind", ylim=c(0,900), col="red")

# Lets plot time series in a single year to investigate further
str(data_2)

x <- as.Date(data_2$Date)
head(x)
class(x)
str(x)

# To convert date column into date format
mod_date <- as.Date(x, format="%m/%d/%Y")
mod_date

str(mod_date)
data_3 <- cbind(mod_date, data_2)
head(data_3)
str(data_3)

# Year : 2017
data_4 <- subset(data_3, subset=data_3$mod_date >= "2017-01-01" & data_3$mod_date <= "2017-12-31")
head(data_4)

plot4 <- plot(data_4[,1], data_4[,3], xlab="Year",
              ylab="Daily Totals (Gwh)", type="l",
              lwd=2, main="Consumption", col="orange")

# Zooming in further
data_4 = subset(data_3, subset=data_3$mod_date >= "2017-01-01" & data_3$mod_date <= "2017-02-28")
head(data_4)

xmin <- min(data_4[,1], na.rm=TRUE)
xmax <- max(data_4[,1], na.rm=TRUE)

xmin
xmax

ymin <- min(data_4[,3], na.rm=T)
ymax <- max(data_4[,3], na.rm=TRUE)

ymin
ymax

plot4 <- plot(data_4[,1], data_4[,3], xlab="Year", ylab="Daily Totals (Gwh)",
              lwd=2, main="Consumption", col="orange",
              type="l", xlim=c(xmin,xmax), ylim = c(ymin,ymax))
grid()

# Add solid Horizontal lines 
abline(h=c(1300, 1500, 1600))

# Add dashed blue vertical lines 
abline(v=seq(xmin, xmax,7), lty=2, col="blue")

# Box Plot
boxplot(data_3$Consumption)
boxplot(data_3$Solar)
boxplot(data_3$Wind)

# Box plot is visual display of 5 number summary
quantile(data_3$Consumption, main="Consumption", ylab="Consumption")

boxplot(data_3$Consumption, main="Consumption", ylab="Consumption",
        ylim=c(600,1800))

# Yearly
boxplot(data_3$Consumption ~ data_3$year,
        main="Consumption", ylab="Consumption",
        xlab="years",ylim=c(600,1800))

boxplot(data_3$Consumption ~ data_3$year, main="Consumption",
        ylab="Consumption", xlab="Years",
        ylim=c(600,1800), las=1)

# Monthly
boxplot(data_3$Consumption ~ data_3$month, main="Consumption",
        ylab="Consumption", xlab="month",
        ylim=c(600,1800), las=1)

# Multiple Plots
par(mfrow=c(3,1))

boxplot(data_3$Consumption ~ data_3$month, main="Consumption",
        ylab="Consumption", xlab="Month",
        ylim=c(600,1800), las=1, col="red")

boxplot(data_3$Wind ~ data_3$month, main="Wind",
        ylab="Wind", xlab="Month", ylim=c(0,900),
        las=1, col="blue")
boxplot(data_3$Solar ~ data_3$month, main="Solar",
        ylab="Solar", xlab="Month", 
        ylim=c(0,200), las=1, col="green")

# Days 
par(mfrow=c(1,1))

boxplot(data_3$Consumption ~ data_3$day, main="Consumption",
        ylab="Consumption", xlab="Days",
        ylim=c(600,1800), las=1, col="green")

data_3
library(dplyr)

summary(data_3)
colSums(!is.na(data_3))
sum(is.na(data_3$Consumption))
sum(is.na(data_3$Wind))
sum(is.na(data$Solar))
sum(is.na(data$Wind.Solar))