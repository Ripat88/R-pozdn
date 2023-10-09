library(tidyverse)
library(readxl)
library(fpp3)
library(psych)
library(writexl)
# Загрузка данных ---------------------------------------------------------


sales <- read_excel('data/raw/Пермь.xlsx',
                    na = c("-", "-99"))

# Считаем руб. на кг ------------------------------------------------------

sales %>% 
  mutate(rub_kg = value / wt)-> sales

# Статистика по клиентам --------------------------------------------------

customer_stat_wt <- sales %>% 
  group_by(customer) %>% 
  summarise(describe(wt))
write_xlsx(customer_stat_wt, 'output/customer_stat_wt.xlsx')

customer_stat_value <- sales %>% 
  group_by(customer) %>% 
  summarise(describe(value))
write_xlsx(customer_stat_value, 'output/customer_stat_value.xlsx')

customer_stat_rub_kg <- sales %>% 
  group_by(customer) %>% 
  summarise(describe(rub_kg))
write_xlsx(customer_stat_value, 'output/customer_stat_value.xlsx')

customer_stat_wt_skimr <- sales %>% 
  group_by(customer) %>% 
  skimr::skim(wt)
write_xlsx(customer_stat_wt_skimr, 'output/customer_stat_wt_skimr.xlsx')

customer_stat_value_skimr <- sales %>% 
  group_by(customer) %>% 
  skimr::skim(value)
write_xlsx(customer_stat_value_skimr, 'output/customer_stat_value_skimr.xlsx')

customer_stat_rub_kg_skimr <- sales %>% 
  group_by(customer) %>% 
  skimr::skim(rub_kg)
write_xlsx(customer_stat_rub_kg_skimr, 'output/customer_stat_rub_kg_skimr.xlsx')

# Статистика по группам рыб (родитель) ------------------------------------

parent_stat_wt <- sales %>% 
  group_by(parent) %>% 
  summarise(describe(wt))
write_xlsx(parent_stat_wt, 'output/parent_stat_wt.xlsx')

parent_stat_value <- sales %>% 
  group_by(parent) %>% 
  summarise(describe(value))
write_xlsx(parent_stat_value, 'output/parent_stat_value.xlsx')

parent_stat_rub_kg <- sales %>% 
  group_by(parent) %>% 
  summarise(describe(rub_kg))
write_xlsx(parent_stat_rub_kg, 'output/parent_stat_rub_kg.xlsx')

parent_stat_wt_skimr <- sales %>% 
  group_by(parent) %>% 
  skimr::skim(wt)
write_xlsx(parent_stat_wt_skimr, 'output/parent_stat_wt_skimr.xlsx')

parent_stat_value_skimr <- sales %>% 
  group_by(parent) %>% 
  skimr::skim(value)
write_xlsx(parent_stat_value_skimr, 'output/parent_stat_value_skimr.xlsx')

parent_stat_value_rub_kg <- sales %>% 
  group_by(parent) %>% 
  skimr::skim(rub_kg)
write_xlsx(parent_stat_value_rub_kg, 'output/parent_stat_value_rub_kg.xlsx')


# Статистика по видам рыб (программа) -------------------------------------

programm_stat_wt <- sales %>% 
  group_by(programm) %>% 
  summarise(describe(wt))
write_xlsx(programm_stat_wt, 'output/programm_stat_wt.xlsx')

programm_stat_value <- sales %>% 
  group_by(programm) %>% 
  summarise(describe(value))
write_xlsx(programm_stat_value, 'output/programm_stat_value.xlsx')

programm_stat_rub_kg <- sales %>% 
  group_by(programm) %>% 
  summarise(describe(rub_kg))
write_xlsx(programm_stat_rub_kg , 'programm_stat_rub_kg .xlsx')

programm_stat_wt_skimr <- sales %>% 
  group_by(programm) %>% 
  skimr::skim(wt)
write_xlsx(programm_stat_wt_skimr, 'output/programm_stat_wt_skimr.xlsx')

programm_stat_value_skimr <- sales %>% 
  group_by(programm) %>% 
  skimr::skim(value)
write_xlsx(programm_stat_value_skimr, 'output/programm_stat_value_skimr.xlsx')

programm_stat_value_rub_kg <- sales %>% 
  group_by(programm) %>% 
  skimr::skim(rub_kg)
write_xlsx(programm_stat_value_rub_kg, 'output/programm_stat_value_rub_kg.xlsx')


# Статистика по рыбам (номенклатура) --------------------------------------

sku_stat_wt <- sales %>% 
  group_by(sku) %>% 
  summarise(describe(wt))

sku_stat_value <- sales %>% 
  group_by(sku) %>% 
  summarise(describe(value))

sku_stat_rub_kg <- sales %>% 
  group_by(sku) %>% 
  summarise(describe(rub_kg))

sku_stat_wt_skimr <- sales %>% 
  group_by(sku) %>% 
  skimr::skim(wt)

sku_stat_value_skimr <- sales %>% 
  group_by(sku) %>% 
  skimr::skim(value)

sku_stat_value_rub_kg <- sales %>% 
  filter(sku != "Филе форели атл. охлажденное Трим В SUP 1-1,2 Россия") %>% 
  group_by(sku) %>% 
  skimr::skim(rub_kg)


# Статистика по менеджерам ------------------------------------------------

manager_stat_wt <- sales %>% 
  group_by(manager) %>% 
  summarise(describe(wt))

manager_stat_value <- sales %>% 
  group_by(manager) %>% 
  summarise(describe(value))

manager_stat_rub_kg <- sales %>% 
  group_by(manager) %>% 
  summarise(describe(rub_kg))

manager_stat_wt_skimr <- sales %>% 
  group_by(manager) %>% 
  skimr::skim(wt)

manager_stat_value_skimr <- sales %>% 
  group_by(manager) %>% 
  skimr::skim(value)

manager_stat_value_rub_kg <- sales %>% 
  group_by(manager) %>% 
  skimr::skim(rub_kg)


# вывод данных ------------------------------------------------------------

var_list <- list(customer_stat_wt,
             customer_stat_value,
             customer_stat_rub_kg,
             customer_stat_wt_skimr,
             customer_stat_value_skimr,
             customer_stat_rub_kg_skimr,
             parent_stat_wt,
             parent_stat_value,
             parent_stat_rub_kg,
             parent_stat_wt_skimr,
             parent_stat_value_skimr,
             parent_stat_value_rub_kg,
             programm_stat_wt,
             programm_stat_value,
             programm_stat_rub_kg,
             programm_stat_wt_skimr,
             programm_stat_value_skimr,
             programm_stat_value_rub_kg,
             sku_stat_wt,
             sku_stat_value,
             sku_stat_rub_kg,
             sku_stat_wt_skimr,
             sku_stat_value_skimr,
             sku_stat_value_rub_kg,
             manager_stat_wt,
             manager_stat_value,
             manager_stat_rub_kg,
             manager_stat_wt_skimr,
             manager_stat_value_skimr,
             manager_stat_value_rub_kg
)



# Визуализация ------------------------------------------------------------

plot_s <- plot(sales$wt, sales$value)
plot_2 <- hist(sales$wt, breaks=10)
plot_3 <- hist(sales$value, breaks=30)
plot_4 <- hist(sales$rub_kg, breaks=30)
plot_5 <- hist(sales$rub_kg, breaks="FD")
plot_6 <- hist(sales$rub_kg, breaks="Scott")
plot_7 <- boxplot(rub_kg~manager, sales)
plot_8 <- boxplot(wt~parent, sales)


# ggplot ------------------------------------------------------------------

almost_empty_ggplot <- ggplot(data=sales)+
  geom_bar(aes(x=manager, fill = manager))
almost_empty_ggplot

almost_empty_ggplot2 <- ggplot(data=sales)+
  geom_bar(aes(x='', fill = manager))
almost_empty_ggplot2

almost_empty_ggplot3 <- ggplot(data=sales)+
  geom_bar(aes(x='', fill = manager), width =.2)
almost_empty_ggplot3

# пайчарт -----------------------------------------------------------------


almost_empty_ggplot4 <- ggplot(data=sales)+
  geom_bar(aes(x='', fill = manager))+
  coord_polar(theta='y')
almost_empty_ggplot4

almost_empty_ggplot5 <- ggplot(data=sales)+
  geom_bar(aes(x='', fill = manager))+
  coord_polar(theta='y')+
  theme_void()+
  labs(title = 'sales')
almost_empty_ggplot5
