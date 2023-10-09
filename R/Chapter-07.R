library(here)
library('tidyverse')
library(dplyr)

heroes <- read.csv(here::here('data/raw', 'heroes_information.csv'),  na.strings = c("NA", "-", "-99"))

str(heroes)

dc <- heroes[heroes$Publisher == 'DC Comics',]
write.csv(dc, 'output/ds_heroes_information.csv', row.names = FALSE)

number <- 1
if(number > 0) {
  'Positive'
} else if (number < 0){
  'Negative'
} else {
  'Ноль'
}

# New Section -------------------------------------------------------------

cumsum(1:10)
numbers <- -2:2
numbers_descriptions <- character(length(numbers)) #создаем строковый вектор с такой же длиной, как и исходный вектор
for (i in 1:length(numbers)) {
  if (numbers[i] > 0) {
    numbers_descriptions[i] <- "Положительное число"
  } else if (numbers[i] < 0) {
    numbers_descriptions[i] <- "Отрицательное число"
  } else {
    numbers_descriptions[i] <- "Ноль"
  }
}
numbers_descriptions


# New section -------------------------------------------------------------

ifelse(numbers > 0, 'Positive', 'Negative')
dplyr::if_else(numbers > 0, 'Positive', 'Negative or Null')
dplyr::case_when(
  numbers < 0 ~ 'Positive',
  numbers > 0 ~ 'Negative',
  numbers == 0 ~ 'Null')
)