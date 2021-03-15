
source('http://bit.ly/CEU-R-shoes')

ls()

plot(students$shoe,students$math)

summary(students)

describe(students)

library(corrplot)
library(RColorBrewer)
library(GGally)

ggpairs(students)

students$math

r1 <- residuals(lm(math ~ x , data = students))
r2 <- residuals(lm(shoe ~ x , data = students))

cor(r1,r2)

library(psych)

rm(list = ls())

sessionInfo()

# Always download things first instead of sourcing them from the internet. 
readLines('http://bit.ly/CEU-R-shoes')


# Second_Exercise ---------------------------------------------------------

# Removed once you finish your session 
t <- tempfile()

download.file('https://bit.ly/hun-cities-distance', t, mode = 'wb')

file.info(t)

library(readxl)
cities <- read_excel(t)

str(cities)

cities <- cities[, -1]
cities <- cities[-nrow(cities),]

str(cities)

## MDS

mds <- cmdscale(as.dist(cities))

plot(mds)
text(mds[,1], mds[,2], names(cities))

# Convert to data frame and create a variable with the city names  
mds <- as.data.frame(mds)
mds$city <- rownames(mds)

ggplot(mds, aes(V1, V2, label = city)) + geom_text() + theme_void()

rm(list = ls())


# Pratical_Exercise -------------------------------------------------------

df <- cmdscale(as.dist(eurodist))

df <- as.data.frame(df)

df$city <- rownames(df)

ggplot(df, aes(V1, V2, label = city)) + geom_text() + theme_void()



# Cars_Example ------------------------------------------------------------

rm(list = ls())

str(mtcars)

mds <-as.data.frame(cmdscale(dist(scale(mtcars))))
mds$car <- rownames(mds)

ggplot(mds, aes(V1, V2, label = car)) + geom_text() + theme_void()

library(ggrepel)
ggplot(mds, aes(V1, V2, label = car)) + geom_text_repel() + theme_void()


# UB_Admissions -----------------------------------------------------------

plot(UCBAdmissions)


berkeley <- as.data.frame(UCBAdmissions)

p <- ggplot(berkeley, aes(Gender, Freq, fill = Admit)) +
  geom_col(position = 'fill') 

p + facet_wrap(~Dept) + scale_fill_manual(values = c(
  'Admitted' = 'darkgreen',
  'Rejected' = 'red'
))

p + facet_wrap(~Dept) + scale_fill_brewer(palette = 'Dark2')


# iris --------------------------------------------------------------------

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point()
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
  geom_smooth(method = 'lm')
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(color = Species), method = 'lm')
geom_smooth(method = 'lm')
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(color = Species), method = 'lm') +
  geom_smooth(method = 'lm', se = FALSE, color = 'black')                                                 


#MDS

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(color = Species))

mds <- as.data.frame(cmdscale(dist(iris)))

ggplot(mds, aes(V1, V2, label = iris$Species)) + geom_point() + geom_smooth()


# Data_Table --------------------------------------------------------------

rm(list = ls())

library(dplyr) # summary statistics 
library(data.table) # dt[i, j, by  = ...]


bookings1 <- fread('http://bit.ly/CEU-R-hotels-2018-features')
bookings2 <- fread('http://bit.ly/CEU-R-hotels-2018-prices')

str(bookings)

# dt[i]

#Returns the i rows
bookings[1]
bookings[1:5]
bookings[price < 100]
bookings[price < 100 & offer == 0 & nnights == 4]
#chaining 
bookings[price < 100 & offer == 0 & nnights == 4][1:5]

# dt[i, j ]

bookings[, .N ]
bookings[ price < 100 , .N ]

bookings[ price < 100 , mean(price)]
bookings[ price < 100 , ][, mean(price)]
bookings[ price < 100 , hist(price)]

# Avg price on weekdays
bookings[ weekend == 0 , mean(price)]
# Avg price on weekends
bookings[ weekend == 1 , mean(price)]
# Avg price by  weekdays]
bookings[ , mean(price), by = weekend]

summary(bookings$nnights)

x <- bookings[ , list(price = mean(price)), 
          by = list(weekend, nnights)][order(price)]

bookings[, list( price = mean(price), min = min(price), max = max(price)), by = list(weekend, nnights)][order(price)]

bookings[ , list(price = mean(price)), 
          by = list(stars)][order(price)]

bookingsfinal <- merge(bookings1, bookings2, by = 'hotel_id')

merge(bookings1, bookings2)[, mean(price), by = stars]

bookingsfinal$price_per_night <- bookingsfinal$price / bookingsfinal$nnights

bookingsfinal[ , price_per_night := price/ nnights ]

hotels <-  merge(
  bookings1,
  bookingsfinal[, list( bookings = .N, price_per_night = mean(price_per_night)), by = hotel_id],
  by = 'hotel_id'
)

## compute avg price

hotels[ , mean(price_per_night), by = stars][order(stars)]

hotels[!is.na(stars) , mean(price_per_night), by = stars][order(stars)] # Less computational power
hotels[ , mean(price_per_night), by = stars][order(stars)][!is.na(stars)] # More computational power
hotels[ , mean(price_per_night, na.rm = TRUE), by = stars][order(stars)][!is.na(stars)]

# comparison using weighted means
hotels[!is.na(stars), weighted.mean(price_per_night , bookings), by = stars][order(stars)] 

hotels[!is.na(rating) , list( mean_ratings = mean(rating)) , by = country][order(mean_ratings)]
hotels[!is.na(rating) , list( mean_ratings = weighted.mean(rating, rating_count)) , by = country][order(mean_ratings)]

hotels[!is.na(rating) , list( mean_ratings = mean(rating)), by = country][mean_ratings > mean(mean_ratings)]


# Add new columns to categorize hotels into 3 buckets of price

hotels[ , pricecat := cut(price_per_night, c(0,100,250, Inf),
                          labels = c('cheap', 'avg', 'expensive'))]

hotels[ , .N, by = pricecat][order(pricecat)]

quantile(hotels$price_per_night, probs = seq(0,1,0.33))
str(quantile(hotels$price_per_night,probs = seq(0,1,0.33)))

q <- quantile(hotels$price_per_night, probs = seq(0,1,0.33))

q[2]

hotels[ , pricecat := cut(price_per_night, c(0,q[2],q[3], Inf),
                          labels = c('cheap', 'avg', 'expensive'))]

hotels[ , .N, by = pricecat][order(pricecat)]


hotels[ , pricecat := cut(price_per_night, c(0, q[2], q[3], Inf),
                          labels = c('cheap', 'avg', 'expensive'))]


#Quartiles by country

hotels[ , q1:= quantile(price_per_night, probs = 0.33), by = country ]
hotels[ , q2:= quantile(price_per_night, probs = 0.66), by = country ]

hotels[ q1 != q2 , pricecat := cut(price_per_night, c(0, unique(q1), unique(q2), Inf), 
                          labels = c('cheap', 'avg', 'expensive')),
        by = country]

hotels[ , .N, by = pricecat][order(pricecat)]


##

anscombe

plot(anscombe$x1,anscombe$y1)
lm(anscombe$x1 ~ anscombe$y1)
abline(lm(anscombe$x1 ~ anscombe$y1))

df <- rbindlist(lapply(1:4, function(i) anscombe[, c(i, i +4 )]), fill = TRUE)

