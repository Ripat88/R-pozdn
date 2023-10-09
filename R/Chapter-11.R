
#  11  Продвинутый tidyverse ----------------------------------------------


# 11.1 Объединение нескольких датафреймов ---------------------------------


# 11.1.1 Соединение структурно схожих датафреймов: bind_rows(), bi --------

library('tidyverse')
heroes_t2 <- read_csv('data/raw/heroes_information.csv', na = c('-', '-99'))
dc <- heroes_t2 %>% 
  filter(Publisher == 'DC Comics') %>% 
  group_by(Gender) %>% 
  summarise(weight_mean = mean(Weight, na.rm = TRUE))
dc

marvel <- heroes_t2 %>% 
  filter(Publisher == 'Marvel Comics') %>% 
  group_by(Gender) %>% 
  summarise(weight_mean = mean(Weight, na.rm = TRUE))
marvel

other_publisher <- heroes_t2 %>% 
  filter(!(Publisher %in% c('DC Comics', 'Marvel Comics'))) %>% 
  group_by(Gender) %>% 
  summarise(weight_mean = mean(Weight, na.rm = TRUE))
other_publisher

bind_rows(dc, marvel)
bind_cols(dc, marvel)

heroes_list_of_df <- list(DC = dc,
                          Marvel = marvel,
                          Other = other_publisher)
bind_cols

bind_rows(heroes_list_of_df, .id = 'Publisher')


# 11.1.2 Реляционные данные: *_join() -------------------------------------


# 11.2 Tidy data: широкий и длинный форматы данных ------------------------

new_diet <- tibble(
  student = c("Маша", "Рома", "Антонина"),
  before_r_course = c(70, 80, 86),
  after_r_course = c(63, 74, 71)
)
new_diet

new_diet %>% 
  pivot_longer(cols = before_r_course:after_r_course,
               names_to = 'measurement_time',
               values_to = 'weight_kg')
new_diet %>% 
  pivot_longer(cols = before_r_course:after_r_course,
               names_to = 'measurement_time',
               values_to = 'weight_kg') %>% 
  pivot_wider(names_from = 'measurement_time',
              values_from = 'weight_kg')


# 11.3 Трансформация нескольких колонок: dplyr::across() ------------------

heroes_t2 %>%
  group_by(Gender) %>%
  summarise(height = mean(Height, na.rm = TRUE),
            weight = mean(Weight, na.rm = TRUE))

heroes_t2 %>%
  group_by(Gender) %>%
  summarise(across(c(Height, Weight), mean, na.rm = TRUE))

heroes_t2 %>% 
  drop_na(Height, Weight) %>% 
  group_by(Gender) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

heroes_t2 %>% 
  group_by(Gender) %>% 
  summarise(across(where(is.character),
                   function(x) mean(nchar(x), na.rm = TRUE)))

heroes_t2 %>%
  group_by(Gender) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            across(where(is.character), 
                   function(x) mean(nchar(x), na.rm = TRUE)))         
heroes_t2 %>%
  group_by(Gender) %>%
  summarise(across(c(Height, Weight), 
                   list(minimum = min,
                        average = mean,
                        maximum = max), 
                   na.rm = TRUE))
heroes_t21 <- heroes_t2 %>%
  group_by(Gender) %>%
  summarise(across(c(Height, Weight),
                   list(min = function(x) min(x, na.rm = TRUE),
                        mean = function(x) mean(x, na.rm = TRUE),
                        max = function(x) max(x, na.rm = TRUE),
                        na_n = function(x, ...) sum(is.na(x)))
  )
  )

heroes_t2 %>%
  mutate(across(where(is.character), as.factor))
heroes_t2 %>%
  count(across(where(function(x) n_distinct(x) <= 6)))       

# 11.4 Функциональное программирование: purrr -----------------------------

lapply(heroes_t2, class)
map(heroes_t2, class)
heroes_t2 %>% 
  map(class)

heroes_t2 %>% 
  map_chr(class)

heroes_t2 %>% 
  map_df(class)

heroes_t2 %>%
  map_int(function(x) sum(is.na(x)))

heroes_t2 %>%
  map_int(~sum(is.na(.)))


# 11.5 Колонки-списки и нестинг: nest() -----------------------------------

heroes_t2 %>%
  nest(!Gender)

heroes_t2 %>%
  group_by(Gender) %>%
  nest()

heroes_t2 %>%
  group_by(Gender) %>%
  nest() %>%
  mutate(dim = map(data, dim))

heroes_t2 %>%
  group_by(Gender) %>%
  nest() %>%
  mutate(dim = map(data, dim)) %>%
  unnest(dim)

heroes_t2 %>%
  group_by(Gender) %>%
  nest() %>%
  mutate(dim = map(data, dim)) %>%
  unnest_wider(dim, names_sep = "_") 

films <- tribble(
  ~film, ~genres,
  "Ирония Судьбы", "comedy, drama",
  "Большой Лебовски", "comedy, criminal",
  "Аватар", "fantasy, drama"
)

films

films %>%
  mutate(genres = strsplit(genres, ", "))

films %>%
  mutate(genres = strsplit(genres, ", ")) %>%
  unnest()

films %>%
  mutate(genres = strsplit(genres, ", ")) %>%
  unnest() %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = "genres",
              values_from = "value", values_fill = FALSE)

films %>%
  separate_rows(genres, sep = ", ") %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = "genres",
              values_from = "value", values_fill = FALSE)
