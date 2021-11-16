#****************************************************************************
# AR/ vR 
#
# Data Visualization - ACT Road Crash Data  
#
#****************************************************************************

#clean Console  as command (CTRL + L)
cat("\014") 

#clean all global variables
rm(list = ls())

#****************************************************************************
if (!require(tidyverse)) install.packages('tidyverse'); library (tidyverse)
if (!require(naniar)) install.packages('naniar'); library (naniar)
if (!require(ggthemes)) install.packages('ggthemes'); library (ggthemes)
if (!require(gridExtra)) install.packages('gridExtra'); library (gridExtra)
if (!require(corrplot)) install.packages('corrplot'); library (corrplot)
if (!require(shiny)) install.packages('shiny'); library (shiny)
if (!require(shiny)) install.packages('shiny'); library (shiny)
if (!require(leaflet)) install.packages('leaflet'); library (leaflet)
if (!require(leaflet.extras)) install.packages('leaflet.extras'); library (leaflet.extras)
library(lubridate)
library(plotly)
library(hrbrthemes)
library(scales)
library(mapdeck)

# Read data

act_crash <- read.csv('ACT_Road_Crash_Data.csv')

# Suburbs plot

# Top 10 Suburbs # Possible filters (Years)
act_suburbs <- act_crash %>% count(SUBURB_LOCATION) %>% arrange(desc(n))

munsell::hue_slice("5P")+  # generate a ggplot with hue_slice()
  annotate(                 # add arrows for annotation 
    geom = "segment", 
    x = c(7, 7), 
    y = c(1, 10), 
    xend = c(7, 7), 
    yend = c(2, 9), 
    arrow = arrow(length = unit(2, "mm"))
  ) 

head(act_suburbs,10) %>% ggplot(aes(x = reorder(SUBURB_LOCATION, n), y =n, label=n))+
  geom_bar(stat = 'identity', fill='#5b2c60')+
  theme_minimal()+
  geom_text(size=4,hjust=1.5, color='white', fontface='bold')+
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 1, face = 'bold'),
        axis.text.y = element_text(size = 8),
        axis.line.x = element_line(color='black', size=0.2),
        legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
  )+
  coord_flip()+
  scale_fill_gradient(
    low = munsell::mnsl("5P 2/12"), 
    high = munsell::mnsl("5P 7/12")
  )+
  labs(x = 'Suburbs',
       y= 'Number of Crashes',
       title = 'Top 10 Suburbs - ACT Crashes'
  )


# Line charts

# Extract hour
act_crash$CRASH_TIME <- strptime(act_crash$CRASH_TIME, format = "%H:%M") %>%  hour()

# Convert string to datetime
act_crash$CRASH_DATE <- parse_date_time(act_crash$CRASH_DATE, orders = c('dmy'))

# Extract year
act_crash$Year <- strptime(act_crash$CRASH_DATE, format = "%Y-%m-%d") %>%  year()

act_crash_year <- setNames(data.frame(table(act_crash$Year)),c("Date","Count"))

# Crashes per year - Possible filters (Crash severity, Road condition, )
act_crash_year %>% ggplot(aes(x=Date, y=Count))+
  geom_line(color='#a34198',group=1, size=2)+
  geom_point(fill='#5b2c60')+
  theme_fivethirtyeight()+
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 1, face = 'bold'),
        axis.text.y = element_text(size = 8),
        axis.line.x = element_line(color='black', size=0.2),
        axis.line.y = element_line(color='black', size=0.2),
        legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
  )+
  scale_fill_viridis_d()+
  scale_y_continuous(labels = scales::comma)+
  labs(x = 'Suburbs',
       y= 'Number of Crashes',
       title = 'Years - ACT Crashes'
  )

# Monthly crashes
act_crash$month <- strptime(act_crash$CRASH_DATE, format = "%Y-%m-%d") %>%  month()

act_crash_month <- setNames(data.frame(table(act_crash$month)),c("Date","Count"))

act_crash_month %>% ggplot(aes(x=Date, y=Count))+
  geom_line(color='#a34198',group=1, size=1)+
  geom_point(fill='#5b2c60')+
  theme_minimal()+
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 1, face = 'bold'),
        axis.text.y = element_text(size = 8),
        legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color='black', size=0.2),
        axis.line.y = element_line(color='black', size=0.2),
  )+
  scale_fill_viridis_d()+
  labs(x = 'Suburbs',
       y= 'Number of Crashes',
       title = 'Montly - ACT Crashes'
  )

# Crashes day of the week
act_crash$week_day <- strptime(act_crash$CRASH_DATE, format = "%Y-%m-%d") %>%  weekdays()

act_crash_weekday <- setNames(data.frame(table(act_crash$week_day)),c("Date","Count"))

act_crash_weekday %>% ggplot(aes(x = Date, y=Count))+
  geom_line(color='#a34198',group=1, size=1)+
  geom_point(fill='#5b2c60')+
  theme_minimal()+
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 1, face = 'bold'),
        axis.text.y = element_text(size = 8),
        axis.line.x = element_line(color='black', size=1),
        legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(color='black', size=0.2)
  )+
  scale_fill_viridis_d()+
  labs(x = 'Suburbs',
       y= 'Number of Crashes',
       title = 'Day of the week - ACT Crashes'
  )
  

  
  
# Crashes - Time (Year, Crash severity)

act_crash_hour <- setNames(data.frame(table(act_crash$CRASH_TIME)),c("Time","Count"))

act_crash_hour %>% ggplot(aes(x=Time, y=Count))+
  geom_line(color='#a34198',group=1, size=1)+
  geom_point(fill='#5b2c60')+
  theme_minimal()+
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 1, face = 'bold'),
        axis.text.y = element_text(size = 8),
        legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color='black', size=0.2),
        axis.line.y = element_line(color='black', size=0.2),
  )+
  scale_fill_viridis_d()+
  labs(x = 'Hour',
       y= 'Number of Crashes',
       title = 'Hourly - ACT Crashes'
  )


# Heat map 3D - Filters (Years, Crash severity)
crash_data = na.omit(act_crash)
ms = mapdeck_style("dark")
token = 'pk.eyJ1IjoianVhbmd1YXJpbm8iLCJhIjoiY2t1eGkwbXd5MXlrbjJ3bnlqZmhuY2NjYSJ9.UGZHYXMuS4HM7KMvK9_MSQ'

mapdeck(
  style = mapdeck_style("dark"), 
  pitch = 30,
  zoom = 9, 
  token=token,
  location = c(149.12, -35.28)) %>%
  add_hexagon(
    data = act_crash
    , lat = "LATITUDE"
    , lon = "LONGITUDE"
    , layer_id = "hex_layer"
    , elevation_scale = 15
    , radius = 500
    , colour_range = colourvalues::colour_values(1:6, palette = colourvalues::get_palette("ylorrd"))
    , legend = TRUE
    , auto_highlight = TRUE
    , update_view = F
  )


# Possible tables to plot
road_condition <- act_crash %>% group_by(ROAD_CONDITION) %>% summarise(count = n())

road_condition %>% 
  ggplot(aes(x = reorder(ROAD_CONDITION, count), y = count))+
  geom_bar(stat = 'identity',fill='#5b2c60')+
  theme_minimal()+
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 1, face = 'bold'),
        axis.text.y = element_text(size = 8),
        axis.line.x = element_line(color='black', size=0.2),
        legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
  )+
  coord_flip()+
  scale_fill_viridis_d()+
  labs(x = 'Condition of the road',
       y= 'Number of Crashes',
       title = 'Road condition - ACT Crashes'
  )

light_condition <- act_crash %>% group_by(LIGHTING_CONDITION) %>% summarise(count = n())

light_condition %>% 
  ggplot(aes(x = reorder(LIGHTING_CONDITION, count), y = count))+
  geom_bar(stat = 'identity',fill='#5b2c60')+
  theme_minimal()+
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 1, face = 'bold'),
        axis.text.y = element_text(size = 8),
        axis.line.x = element_line(color='black', size=0.2),
        legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
  )+
  coord_flip()+
  scale_fill_viridis_d()+
  labs(x = 'Condition of the road',
       y= 'Number of Crashes',
       title = 'Lighting condition - ACT Crashes'
  )



Weather_condition <- act_crash %>% group_by(WEATHER_CONDITION) %>% summarise(count = n())

weather_condition %>% 
  ggplot(aes(x = reorder(WEATHER_CONDITION, count), y = count))+
  geom_bar(stat = 'identity',fill='#5b2c60')+
  theme_minimal()+
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 1, face = 'bold'),
        axis.text.y = element_text(size = 8),
        axis.line.x = element_line(color='black', size=0.2),
        legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
  )+
  coord_flip()+
  scale_fill_viridis_d()+
  labs(x = 'Condition of the road',
       y= 'Number of Crashes',
       title = 'Weather condition - ACT Crashes'
  )







