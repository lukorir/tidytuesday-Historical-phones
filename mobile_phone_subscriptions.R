# Libraries
library(tidyverse)
library(magick)

setwd("G:/My Drive/MoreMilk/tidytuesday/Historical phones/tidytuesday-Historical-phones")
# Data
# mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
# write.csv(mobile, "mobile.csv")
mobile <- read.csv("mobile.csv")
mob_dat <- mobile %>% filter(entity == "Libya" | entity == "Kenya" | entity == "Seychelles" , !is.na(mobile_subs), year>= 1995) 

cbp2 <- c("red", "green", "blue")

p <-   ggplot(mob_dat, aes(x=year, y=mobile_subs, col = entity, group = entity)) +
  geom_line(aes(col=entity), size = .8) +
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = "none") +
  xlab("Year")+
  labs(title = "Mobile phone subscriptions in Libya, Seychelles and Kenya",
       subtitle = "Libya attained an all time high of any African country in 2010,\n\nbefore declining. Seychelles was leading in 2017",
       caption = "Data: OurWorldInData.org | plot@lukorir") +
  ylab("Mobile phone subscriptions per 100 people") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 180)) +
  scale_x_continuous(breaks = seq(1995, 2017, by = 2)) +
  theme(plot.subtitle = element_text(lineheight=.5)) +
  theme(plot.background = element_rect(fill = '#fffbf5')) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

p <- p + scale_colour_manual(values=cbp2) +
  annotate(geom = "text", x = 2015,  y = 175,  label = "Seychelles", size= 4, fontface = "italic") +
  annotate(geom = "text", x = 2015,  y = 121,  label = "Libya", size= 4, fontface = "italic") +
  annotate(geom = "text", x = 2016,  y = 89,  label = "Kenya", size= 4, fontface = "italic") 

ggsave("lby_syc_ken.png", p, width = 6, height = 4)
