# Computational Movement Analysis - Excercises Week 5
# Tasks and Inputs

# Annika Hirsch

############### Task 1 #################

# Function which calculates the BMI
bmi <- function(weight, height, height_unit = "m"){
  if (height_unit == "m"){
    weight / (height)^2
  } else if (height_unit == "cm"){
    weight / (height * 0.01)^2
  } else {
    print("Error: Unit not known. Change your height to be either in m or cm and state the height unit accordingly. The default unit is m.")
  }
}
bmi(3, 8, "whatever")

# Function which converts degrees Celcius to Farenheight 
celcius_to_fahrenheit <- function(celcius){
  (celcius * (9/5)) + 32
}

# Function which calculates the distance between two sets of coordinates 
euclidean_dist <- function(coord1, coord2){
  ((coord2[1] - coord1[1])^2 + (coord2[2]- coord1[2])^2)^(1/2)
}
euclidean_dist(c(1,2), c(3,4))

############# Task 2 ###############
wildschwein_BE_2056 <- read.csv("wildschwein_BE_2056.csv")
unique(wildschwein_BE_2056$TierName)

wildschwein <- subset(wildschwein_BE_2056, 
                      wildschwein_BE_2056$TierName != "Ruth" & 
                        wildschwein_BE_2056$DatetimeUTC >= "2015-04-01" &
                        wildschwein_BE_2056$DatetimeUTC <= "2015-04-15"
                      )

############# Task 3 ###############
library(lubridate)
wildschwein$DatetimeRound <- 
  round_date(ymd_hms(wildschwein$DatetimeUTC), unit = "15 minutes")

############# Task 4 ###############
sabi <- wildschwein[wildschwein$TierName == "Sabi", ]
rosa <- wildschwein[wildschwein$TierName == "Rosa", ]

# rename col names to avoid suffix .x or .y
sabi <- sabi %>% 
  rename("E.sabi" = "E",
         "N.sabi" = "N")
rosa <- rosa %>% 
  rename("E.rosa" = "E",
         "N.rosa" = "N")

library(dplyr)
joined <- inner_join(sabi, rosa, by = "DatetimeRound")

# remove unnecessary columns
joined[1:4] <- list(NULL)
joined[4:7] <- list(NULL)

# Distance column
#joined$distance <- euclidean_dist(c(joined$E.sabi, joined$N.sabi), 
#                                      c(joined$E.rosa, joined$N.rosa))
# does not work correctly... stores the same distance for all rows...

# Rewriting the function to see if that's the culprit...
euclid_dist <- function(x1, y1, x2, y2){
  ((x2-x1)^2 + (y2-y1)^2)^(1/2)
}
joined$distance <- euclid_dist(joined$E.sabi, joined$N.sabi, 
                               joined$E.rosa, joined$N.rosa)
# Does work now. I suspect the c() does cause the problem, but since 
# there is an easy workaround in this case, I won't try to understand
# why it does not work.

# Meet col: distance < 100m
joined$meet <- joined$distance <= 100


############ Task 5 ##############
library(ggplot2)

ggplot(data = joined) + 
  geom_point(aes(E.sabi, N.sabi), color = "turquoise", alpha = 0.5) +
  geom_point(aes(E.rosa, N.rosa), color = "pink", alpha = 0.5) +
  geom_point(data = joined[joined$meet == TRUE,], 
             aes(E.rosa, N.rosa), 
             color = "red", alpha = 0.6) +
  geom_point(data = joined[joined$meet == TRUE,], 
             aes(E.sabi, N.sabi), 
             color = "blue", alpha = 0.6) 

# Plot is not really finetuned and everything.
# But does its job of visualizing the data

############ Task 6 ##############
library(plotly)

plot_ly(joined, 
        x = ~E.sabi, y = ~N.sabi, z = ~DatetimeRound, 
        type = 'scatter3d', mode = 'lines',
        opacity = 1, 
        line = list(width = 6, color = "lightblue", reverscale = FALSE))

# While I don't want to waste time, creating 3D plots is new for me, and
# now I want to find out how I can make nice ones...
# So let's waste some time 

# So far I've found out, that I can have multiple lines with the split command
cube <- plot_ly(data = wildschwein, 
                x = ~E, y = ~N, z = ~DatetimeRound, 
                split = ~TierName, type = 'scatter3d', mode = 'lines', 
                line = list(width = 4))
# Works. Nice.

# Next point: How to add points?
# So apparently I can add stuff with add_trace()
cube %>% add_trace(data = joined[joined$meet == TRUE,], 
                   x = ~E.sabi, y = ~N.sabi, z = ~DatetimeRound, 
                   type = 'scatter3d')
# Seems to have a problem because I use different dataframes. 
# Understandable... but annoying

meet <- joined[joined$meet == TRUE,]
cube %>% add_trace(x = meet$E.sabi, y = meet$N.sabi, 
                   z = meet$DatetimeRound, 
                   type = 'scatter3d')
# Does still not work

# Let's try this differently and add everything with add_trace
cube <- plot_ly() %>%
  add_trace(data = wildschwein, 
            x = ~E, y = ~N, z = ~DatetimeRound, 
            split = ~TierName, type = 'scatter3d', mode = 'lines', 
            line = list(width = 4)) %>%
  add_trace(data = joined[joined$meet == TRUE,], 
            x = ~E.sabi, y = ~N.sabi, z = ~DatetimeRound, 
            type = 'scatter3d')
# Does nothing


# There are working cubes... 
# I just could not figure out how to add points. 
# I will return to this


