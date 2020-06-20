library(readr)
library(tidyverse)
library(dplyr)
library(lmtest)
library(stats)
library(sandwich)

set.seed(10)


data <- read.csv(file = "/Users/Olesya/Downloads/datasets_14446_19502_country_profile_variables.csv", header = TRUE)
view(data)

#Задание 1 и Задание 2
#Исследовательский вопрос - "Я хочу изучить, как связаны темпы экономического роста и 
#(а) - расходы на здравоохранение (должен быть положительный знак), расходы на образвоание (должен быть положительный знак), уровень безработицы (должен быть отрицательный знак)
#(б) - большое количество детей (создадим дамми-переменную, где "1" - детей больше 2) (должен быть отрицитальный знак)
#(в) - логарифм доли городского населения (должен быть положительный знак)
#Для этого в качестве зависимой переменной я беру логарифм ВВП.

names(data)[33] <- "health_exp"
names(data)[25] <- "urban"
names(data)[7] <- "gdp"
names(data)[27] <- "children"
names(data)[16] <- "unemp"
names(data)[35] <- "educ_exp"
#создадим дамми-переменную, про которую говорили выше
d_children <- NULL
d_children[data$children < 2] = 0
d_children[data$children > 2] = 1
data <- data.frame(d_children, data)

#Задание 5(а)

lngpd <- log(data$gdp)
lnurban <- log(data$urban)
reg <- lm(data$lngdp ~ data$health_exp + data$lnurban + data$unemp + data$d_children + data$educ_exp, data = data)



