library(dplyr)
library(ggplot2)
library(countrycode)
library(ggflags)
library(cowplot)
library(ggthemes)
library(readxl)

setwd("C:/Users/milen/Desktop/R")
data2 <- read.csv("faostat22.csv")
names(data2)

## parte1/Wheat

data2$iso2 <- countrycode(data2$Area, "country.name", "iso2c")
data2$code <- tolower(countrycode(data2$Area, origin = 'country.name', destination = 'iso2c'))
data2$continent <- tolower(countrycode(data2$Area, origin = 'country.name', destination = 'continent'))

names(data2)
data2$Value <- as.numeric(data2$Value)

data2$Area[data2$Area == "United States of America"] <- "USA"
data2$Area[data2$Area == "China, mainland"] <- "China"
data2$Area[data2$Area == "Russian Federation"] <- "Russia"

data2$continent[data2$continent == "asia"] <- "Asia"
data2$continent[data2$continent == "americas"] <- "Americas"
data2$continent[data2$continent == "europe"] <- "Europe"
data2$continent[data2$continent == "oceania"] <- "Oceania"


my_palette <- c("Americas" = "#E15554", "Asia" = "#3BB273", "Europe" = "#7768AE", "Oceania" = "#4D9DE0")


p <- ggplot(data2, aes(x=reorder(Area, Value),y = Value/10^6, fill = continent))+
geom_bar(stat = "identity")+
geom_flag(y = -5, aes(country = code), size = 8)+
labs(fill = "Continente",
     title = "Commodities:Top 10 Produtores de Trigo", 
     subtitle = "Fonte:FAO - Statistics, 2020", 
     y = 'Produ??o em (milh?es de toneladas)', 
     x = '')+
theme_economist(horizontal = FALSE) +
theme(legend.position = c(.9,.6),
      axis.text.x = element_text(vjust = 3)) +
coord_flip()+
expand_limits(y = 0) +
scale_fill_manual(values = my_palette)

p


## parte2/rice

data3 <- read.csv("faostat_arroz.csv")
names(data3)

data3$iso2 <- countrycode(data3$Area, "country.name", "iso2c")
data3$code <- tolower(countrycode(data3$Area, origin = 'country.name', destination = 'iso2c'))
data3$continent <- tolower(countrycode(data3$Area, origin = 'country.name', destination = 'continent'))

names(data3)
data3$Value <- as.numeric(data3$Value)

data3$Area[data3$Area == "China, mainland"] <- "China"
data3$Area[data3$Area == "Viet Nam"] <- "Vietnam"

data3$continent[data3$continent == "asia"] <- "Asia"
data3$continent[data3$continent == "americas"] <- "Americas"
data3$continent[data3$continent == "europe"] <- "Europe"
data3$continent[data3$continent == "oceania"] <- "Oceania"



my_palette <- c("Americas" = "#E15554", "Asia" = "#3BB273", "Europe" = "#7768AE", "Oceania" = "#4D9DE0")


p3 <- ggplot(data3, aes(x=reorder(Area, Value),y = Value/10^6, fill = continent))+
  geom_bar(stat = "identity")+
  geom_flag(y = -5, aes(country = code), size = 8)+
  labs(fill = "Continente",
       title = "Commodities:Top 10 Produtores de Arroz", 
       subtitle = "Fonte:FAO - Statistics, 2020", 
       y = 'Produ??o em (milh?es de toneladas)', 
       x = '')+
  theme_economist(horizontal = FALSE) +
  theme(legend.position = c(.9,.6),
        axis.text.x = element_text(vjust = 3)) +
  coord_flip()+
  expand_limits(y = 0) +
  scale_fill_manual(values = my_palette)

p3

## parte3/Soy

data4 <- read.csv("faostat_soja.csv")
names(data4)

data4$iso2 <- countrycode(data4$Area, "country.name", "iso2c")
data4$code <- tolower(countrycode(data4$Area, origin = 'country.name', destination = 'iso2c'))
data4$continent <- tolower(countrycode(data4$Area, origin = 'country.name', destination = 'continent'))

names(data4)
data4$Value <- as.numeric(data4$Value)

data4$Area[data4$Area == "United States of America"] <- "USA"
data4$Area[data4$Area == "China, mainland"] <- "China"
data4$Area[data4$Area == "Russian Federation"] <- "Russia"
data4$Area[data4$Area == "Bolivia (Plurinational State of)"] <- "Bolivia"

data4$continent[data4$continent == "asia"] <- "Asia"
data4$continent[data4$continent == "americas"] <- "Americas"
data4$continent[data4$continent == "europe"] <- "Europe"
data4$continent[data4$continent == "oceania"] <- "Oceania"



my_palette <- c("Americas" = "#E15554", "Asia" = "#3BB273", "Europe" = "#7768AE", "Oceania" = "#4D9DE0")


p4 <- ggplot(data4, aes(x=reorder(Area, Value),y = Value/10^6, fill = continent))+
  geom_bar(stat = "identity")+
  geom_flag(y = -5, aes(country = code), size = 8)+
  labs(fill = "Continente",
       title = "Commodities:Top 10 Produtores de Soja", 
       subtitle = "Fonte:FAO - Statistics, 2020", 
       y = 'Produ??o em (milh?es de toneladas)', 
       x = '')+
  theme_economist(horizontal = FALSE) +
  theme(legend.position = c(.9,.6),
        axis.text.x = element_text(vjust = 3)) +
  coord_flip()+
  expand_limits(y = 0) +
  scale_fill_manual(values = my_palette)


p4


#parte4/ coffee

data5 <- read.csv("faostat_cafe.csv")
names(data5)

data5$iso2 <- countrycode(data5$Area, "country.name", "iso2c")
data5$code <- tolower(countrycode(data5$Area, origin = 'country.name', destination = 'iso2c'))
data5$continent <- tolower(countrycode(data5$Area, origin = 'country.name', destination = 'continent'))

names(data5)
data5$Value <- as.numeric(data5$Value)

data5$Area[data5$Area == "Viet Nam"] <- "Vietnam"

data5$continent[data5$continent == "asia"] <- "Asia"
data5$continent[data5$continent == "americas"] <- "Americas"
data5$continent[data5$continent == "europe"] <- "Europe"
data5$continent[data5$continent == "oceania"] <- "Oceania"
data5$continent[data5$continent == "africa"] <- "Africa"


my_palette <- c("Americas" = "#E15554", "Asia" = "#3BB273", "Europe" = "#7768AE", "Oceania" = "#4D9DE0", "Africa" = "#F0E442")


p5 <- ggplot(data5, aes(x=reorder(Area, Value),y = Value/10^6, fill = continent))+
  geom_bar(stat = "identity")+
  geom_flag(y=-0.1, aes(country = code), size = 8)+
  labs(fill = "Continente",
       title = "Commodities:Top 10 Produtores de Caf?", 
       subtitle = "Fonte:FAO - Statistics, 2020", 
       y = 'Produ??o em (milh?es de toneladas)', 
       x = '')+
  theme_economist(horizontal = FALSE) +
  theme(legend.position = c(.9,.6),
        axis.text.x = element_text(vjust = 3)) +
  coord_flip()+
  expand_limits(y = 0) +
  scale_fill_manual(values = my_palette)


p5

##part5/Oranges


data6 <- read.csv("faostat_laranjas.csv")
names(data6)

data6$iso2 <- countrycode(data6$Area, "country.name", "iso2c")
data6$code <- tolower(countrycode(data6$Area, origin = 'country.name', destination = 'iso2c'))
data6$continent <- tolower(countrycode(data6$Area, origin = 'country.name', destination = 'continent'))

names(data6)
data6$Value <- as.numeric(data6$Value)

data6$Area[data6$Area == "United States of America"] <- "USA"
data6$Area[data6$Area == "China, mainland"] <- "China"
data6$Area[data6$Area == "Iran (Islamic Republic of)"] <- "Iran"

data6$continent[data6$continent == "asia"] <- "Asia"
data6$continent[data6$continent == "americas"] <- "Americas"
data6$continent[data6$continent == "europe"] <- "Europe"
data6$continent[data6$continent == "oceania"] <- "Oceania"
data6$continent[data6$continent == "africa"] <- "Africa"



my_palette <- c("Americas" = "#E15554", "Asia" = "#3BB273", "Europe" = "#7768AE", "Oceania" = "#4D9DE0", "Africa" = "#F0E442")

p6 <- ggplot(data6, aes(x=reorder(Area, Value),y = Value/10^6, fill = continent))+
  geom_bar(stat = "identity")+
  geom_flag(y = -0.5, aes(country = code), size = 8)+
  labs(fill = "Continente",
       title = "Commodities:Top 10 Produtores de Laranja", 
       subtitle = "Fonte:FAO - Statistics, 2020", 
       y = 'Produ??o em (milh?es de toneladas)', 
       x = '')+
  theme_economist(horizontal = FALSE) +
  theme(legend.position = c(.9,.6),
        axis.text.x = element_text(vjust = 3)) +
  coord_flip()+
  expand_limits(y = 0) +
  scale_fill_manual(values = my_palette)


p
