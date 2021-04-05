library(data.table)
library(skimr)

df <- fread('http://bit.ly/CEU-R-numbers-set')

summary(df)
skim(df)
plot(df$x, df$y)

summary(df, by = x)

library(ggplot2)
library(hexbin)
ggplot(df, aes(x , y)) + geom_point(alpha = 0.1)
ggplot(df, aes(x , y)) + geom_hex()

ggplot(df, aes(factor(x) , y)) + geom_boxplot()
ggplot(df, aes(factor(x) , y)) + geom_violin()
ggplot(df, aes(factor(x) , y)) + geom_violin() + geom_jitter()
ggplot(df, aes(factor(x) , y)) + geom_violin() + geom_jitter(height = 0 , width = 0.1)
ggplot(df, aes(factor(x) , y)) + geom_violin() + geom_jitter()

ggplot(df, aes(y , fill = factor(x))) + geom_density(alpha = 0.5)

ggplot(df , aes(y)) + geom_density() + facet_wrap(~x)
ggplot(df , aes(y)) + geom_histogram() + facet_wrap(~x)

dfa <- melt(df[ , list(mean = mean(y), sd = sd(y)), by = x], id = 'x')
ggplot(dfa, aes(x , variable, fill = value)) + geom_tile()      


# Clustering --------------------------------------------------------------

dm <- dist(iris[, 1:4])
hc <- hclust(dm)
plot(hc)
rect.hclust(hc, k = 3)

for (i in 2:8){
  plot(hc)
  rect.hclust(hc, k = i)
  Sys.sleep(1)
}


# Animation_GIF -----------------------------------------------------------

#install.packages('animation')
library(animation)
ani.options(interval = 1) # interval equals to the sys.sleep

saveGIF(
  for (i in 2:8){
    plot(hc)
    rect.hclust(hc, k = i)
  }  
)


library(dendextend)

d <- as.dendrogram(hc)
d <- color_branches(d, k = 3)
plot(d)

ggplot(d)
ggplot(d , horiz = TRUE)
ggplot(d , horiz = TRUE, labels = TRUE)
ggplot(d , labels = FALSE)

ggplot(d , labels = FALSE) + 
  scale_y_reverse( expand = c(0.2,0)) + 
  coord_polar(theta = "x")

## https://talgalili.github.io/dendextend/articles/dendextend.html

saveGIF(
  for (i in 2:8){
    d <- as.dendrogram(hc)
    d <- color_branches(d, k = i)
    # whenever you are running ggplot within a loop, it's important to always call print on it
    print(ggplot(d))
  }  
)


# cluster members
clusters <-  cutree(hc, 3)

library(NbClust)

NbClust(iris[, 1:4], method = 'complete', index = 'all')


ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = clusters)) + geom_point()


# add linear model
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters))) + 
         geom_point(size = 3) + geom_smooth(method = 'lm')

ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters) , linetype = Species)) + 
  geom_point(size = 3) + geom_smooth(method = 'lm') 

## + extra dimension: time

# this explain what`s going on with the animation package

#saveGIF(
  #for (species in unique(iris$Species)){
    #print(ggplot(subset(iris, species == species), aes(...) + geom_...
  #}  
#)

# install.packages("gganimate")
# install.packages("transformr")

library(gganimate)
library(transformr)

ggplot(iris, aes(Sepal.Length, Sepal.Width, colors = factor(clusters)))+
  geom_point(size = 3) + 
  geom_smooth(method = 'lm') +
  transition_states(Species) +
  labs(title = '{closest_state}')

ggplot(iris, aes(Sepal.Length, Sepal.Width, colors = factor(clusters)))+
  geom_point(size = 3) + 
  geom_smooth(method = 'lm') +
  transition_states(Species) +
  labs(title = '{closest_state}' , 
       subtitle = 'Number of flowers: {nrow(subset(iris, Species == closest_state))}')

library(glue)

glue('2 + 2 = {2+2}')
sprintf('2 + 2 = %s' , 2 + 2)


# Hotels_Dataset ----------------------------------------------------------


# Data Table cheat sheet

# https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')
bookings[, price_per_night := price / nnights]
hotels <- merge(
  features,
  bookings[, .(bookings = .N, price_per_night = mean(price_per_night)), by = hotel_id],
  by = 'hotel_id')


hotels[, lapply(.SD, mean, na.rm = TRUE), by = city, 
       .SDcols = c('price_per_night', 'rating', 'stars')]

names(which(sapply(hotels, is.numeric)))

hotels[, lapply(.SD, mean, na.rm = TRUE), by = city, 
       .SDcols = names(which(sapply(hotels, is.numeric)))]

str(hotels)

hotels[, c('price_per_night_huf', 'price_per_night_usd') := list(
  price_per_night * 360,
  price_per_night * 1.18
)]
hotels[, `:=` ( # dplyr::mutate
  price_per_night_huf = price_per_night * 360,
  price_per_night_usd = price_per_night * 1.18
)]
hotels[, list(
  min_price = min(price_per_night),
  avg_price = mean(price_per_night),
  med_price = median(price_per_night),
  max_price = max(price_per_night)
), by = city]
# slow and crappy and still dealing with NAs is hard
# write own function
mystats <- function(x) list(
  min = min(x, na.rm = T),
  avg = mean(x, na.rm = T),
  max = max(x, na.rm = T)
)
mystats(hotels$price_per_night)

hotels[ , as.list(unlist(lapply(.SD, mystats))), .SDcols = c('price_per_night')]

hotels[ , as.list(unlist(lapply(.SD, mystats))), .SDcols = c('price_per_night' , 'rating' , 'stars'),
        by = city]

#install.packages("datasauRus")
library(datasauRus)

df <- rbindlist(lapply(1:13, function(i) 
  cbind(dataset = sub('_x', '', names(datasaurus_dozen_wide)[(2*i)-1]), 
        setnames(datasaurus_dozen_wide[, c((2*i)-1, 2*i)],
                 c('x', 'y')))))

ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~dataset) + geom_smooth(method = 'lm')

ggplot(datasaurus_dozen, aes(x,y)) + geom_point() + facet_wrap(~dataset)
ggplot(datasaurus_dozen, aes(x,y)) + geom_point() + transition_states(dataset)

ggplot(datasaurus_dozen, aes(x,y)) + geom_point() + 
  geom_smooth(method = 'lm' , se = TRUE)
  transition_states(dataset) + 
  labs(title = '{closest_state}')
  
  subtitle <- function(df, round = 2) {
    paste0(
      'mean(x)=', round(mean(df$x), round), ', sd(x)=', round(sd(df$x), round), '\n',
      'mean(y)=', round(mean(df$y), round), ', sd(y)=', round(sd(df$y), round), '\n',
      'cor(x, y)=', round(cor(df$x, df$y), round)
    )
  }
  
  ggplot(datasaurus_dozen, aes(x, y)) + 
    geom_point() + 
    geom_smooth(method = 'lm', se = TRUE) +
    transition_states(dataset) +
    labs(title = '{closest_state}',
         subtitle = '{subtitle(subset(datasaurus_dozen, dataset == closest_state))}')
  

# Interactivity -----------------------------------------------------------

p <- ggplot(mtcars , aes(wt , hp, color = factor(am))) + geom_point()
rm(p)  
p <- last_plot()  

install.packages("ggiraph")
library(ggiraph)

girafe(ggobj = p)

p <- ggplot(mtcars , aes(wt , hp, color = factor(am), 
                         tooltip = rownames(mtcars))) + 
     geom_point_interactive()

mtcars$tooltips <- paste(rownames(mtcars), 'with' , mtcars$gear , 'gears')

p <- ggplot(mtcars , aes(wt , hp, color = factor(am), 
                         tooltip = tooltips, 
                         data_id = factor(gear))) + 
      geom_point_interactive()

# CSS
# JS

girafe( ggobj = p , options = list(opts_hover(css = 'fill:black;')))

# TODO htmlwidgets

library(ggthemes)

p <- ggplot(mtcars , aes(wt , hp, color = factor(am))) + geom_point()

p + theme_bw()
p + theme_minimal()
p + theme_void()

ggthemes::theme_excel()

p + theme_economist() + scale_color_economist()
p + theme_stata() + scale_color_stata()

?theme

theme_custom <- function(){
  
  theme(
    
    axis.text = element_text(family = 'Times New Roman', color = 'orange'),
    panel.background = element_rect(
      fill = 'orange' , 
      color = 'white'
    ),
    legend.position = 'top'
  )
}

theme_custom()
theme_top_legend <- function() theme(legend.position = 'top')

p + theme_custom()
p + theme_custom() + theme_bw()

# Can't download that package
devtools::install_github('Mikata-Project/ggthemr')

library(jpeg)

img <- readJPEG('D:/CEU/Data_Viz_R/Data_Viz_R_Yuri/Second_Class/JPEG.jpg')

str(img)

dim(img)

h <- dim(img)[1]
w <- dim(img)[2]

img2d <-  matrix(img, h * w)

str(img2d)
library(stats)
pca <- prcomp(img2d)
pca$rotation

image(matrix(pca$x[,1], h))
image(matrix(pca$x[,2], h))

extractColors <- function(x) rgb(x[1], x[2], x[3])

colors <- apply(abs(pca$rotation), 2, extractColors)
colors

pie(pca$sdev , col = colors , labels = colors)
