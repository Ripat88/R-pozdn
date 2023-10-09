library(tidyverse)

heroes_t <- read_csv('data/raw/heroes_information.csv', na = c('-', '-99'))
class(heroes_t)
heroes_t
heroes_tdf <- as.data.frame(heroes_t)
class(heroes_tdf)
as_tibble(heroes_tdf)

tibble(
  a = 1:3,
  b = letters[1:3]
)


# magrittr::%>% -----------------------------------------------------------

sum(sqrt(abs(sin(1:22))))

1:22 %>% 
  sin() %>% 
  abs() %>% 
  sqrt() %>% 
  sum()


# Главные пакеты tidyverse: dplyr и tidyr ---------------------------------


# 10.6 Работа с колонками тиббла ------------------------------------------


# 10.6.1 Выбор колонок: dplyr::select() -----------------------------------

heroes_t %>% 
  select(1,5)

heroes_t %>% 
  select(name, Race, Publisher, 'Hair color')

heroes_some_cols <- heroes_t %>% 
  select(name, Race, Publisher, 'Hair color')
heroes_some_cols

# 10.6.2 Мини-язык tidyselect для выбора колонок --------------------------

heroes_t %>% 
  select(name:Publisher)

heroes_t %>% 
  select(name: `Eye color`, Publisher:Weight)

heroes_t %>% 
  select(!...1)

heroes_t %>% 
  select(!(Gender:Height))

# Вспомогательная функция last_col() позволит обратиться к последн --------


heroes_t %>% 
  select(name:last_col())

# А функция everything() позволяет выбрать все колонки. -------------------


heroes_t %>% 
  select(everything())

# перестановка колонок ----------------------------------------------------


heroes_t %>% 
  select(name, Publisher, everything())

# выбрать все колонки, заканчивающиеся одинаковым суффиксом: --------------


heroes_t %>% 
  select(ends_with('color'))


#  с помощью функции starts_with() можно найти колонки с одинаковы --------

heroes_t %>% 
  select(starts_with('Eye') & ends_with('color'))

#  с помощью функции starts_with() можно найти колонки с одинаковы --------

heroes_t %>%
  select(contains("eight"))

heroes_t %>%
  select(where(is.numeric))

# выбор колонок без NA ----------------------------------------------------


heroes_t %>% 
  select(where(function(x) !any(is.na(x))))

# переименование колонок --------------------------------------------------

heroes_t %>% 
  select(id = ...1)

# массовое переименование колонок -----------------------------------------

heroes_t %>% 
  rename_with(make.names)

# перестановка колонок ----------------------------------------------------

heroes_t %>% 
  relocate(Publisher)

heroes_t %>% 
  relocate(Publisher, .after = name)

heroes_t %>%
  relocate(Publisher, where(is.numeric), .after = name)

heroes_t %>% 
  select(Height) %>% 
  pull() %>% 
  head()

heroes_t %>% 
  select(Height) %>% 
  pull()

heroes_t %>%
  pull(Height) %>%
  head()

heroes_t %>% 
  pull(Height, name) %>% 
  head()


# 10.7 Работа со строками тиббла ------------------------------------------


# 10.7.1 Выбор строк по номеру: dplyr::slice() ----------------------------

heroes_t %>% 
  slice(1:3)

# 10.7.2 Выбор строк по условию: dplyr::filter() --------------------------

heroes_t %>% 
  filter(Publisher == 'DC Comics')


# 10.7.3 Семейство функций slice() ----------------------------------------
heroes_t %>% 
  slice_max(Weight, n = 3)

heroes_t %>% 
  slice_min(Weight, n = 3)

heroes_t %>% 
  slice_sample(n = 3)

heroes_t %>% 
  slice_sample(prop = .01)

heroes_t %>% 
  slice_sample(prop = 1)

# 10.7.4 Удаление строчек с NA: tidyr::drop_na() --------------------------

heroes_t %>% 
  drop_na()

heroes_t %>% 
  drop_na(Weight)


# 10.7.5 Сортировка строк: dplyr::arrange() -------------------------------

heroes_t %>% 
  arrange(Weight)

heroes_t %>% 
  arrange(desc(Weight))

heroes_t %>% 
  arrange(Gender, desc(Weight))

# 10.8 Создание колонок: dplyr::mutate() и dplyr::transmute() -------------

heroes_t %>% 
  mutate(imt = Weight/(Height/100)^2) %>% 
  select(name, imt) %>% 
  arrange(desc(imt))

heroes_t %>% 
  transmute(imt = Weight/(Height/100)^2)

heroes_t %>% 
  transmute(name, weight_mean = mean(Weight, na.rm = TRUE))

heroes_t %>% 
  mutate(one_and_two = rep(1:2, length.out = nrow(.)))


# 10.9 Агрегация данных в тиббле ------------------------------------------



# 10.9.1 Подытоживание: summarise() ---------------------------------------
heroes_t %>%
  mutate(imt = Weight/(Height/100)^2) %>%
  summarise(min(imt, na.rm = TRUE),
            max(imt, na.rm = TRUE))

heroes_t %>% 
  mutate(imt = Weight/(Height/100)^2) %>% 
  arrange(imt) %>% 
  summarise(first = first(imt),
            tenth = nth(imt, 10),
            last= last(imt))

heroes_t %>%
  mutate(imt = Weight/(Height/100)^2) %>%
  summarise(imt_range = range(imt, na.rm = TRUE)) #функция range() возвращает вектор из двух значений: минимальное и максимальное


# 10.9.2 Группировка: group_by() ------------------------------------------

heroes_t %>% 
  group_by((Gender))

heroes_t %>% 
  mutate(imt = Weight/(Height/100)^2) %>% 
  group_by(Gender) %>% 
  summarise(min(imt, na.rm = TRUE),
            max(imt, na.rm = TRUE))

# 10.9.3 Подсчет строк: dplyr::n(), dplyr::count() ------------------------

heroes_t %>% 
  group_by(Gender) %>% 
  summarise(n = n())

heroes_t %>% 
  group_by(Race) %>% 
  filter(n() > 10) %>% 
  select(name, Race)

heroes_t %>% 
  group_by(Race) %>% 
  filter(n() ==1) %>% 
  select(name, Race)

# таблица частот ----------------------------------------------------------

heroes_t %>% 
  count(Gender)

heroes_t %>% 
  count(Gender, sort = TRUE)


# 10.9.4 Уникальные значения: dplyr::distinct() ---------------------------

heroes_t %>% 
  distinct(Gender)

heroes_t %>% 
  distinct(Gender, Race)

# 10.9.5 Создание колонок с группировкой ----------------------------------

heroes_t %>% 
  group_by(Race) %>% 
  mutate(Race_n = n()) %>% 
  select(Race, name, Gender, Race_n)
