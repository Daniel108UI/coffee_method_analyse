library(car)
library(psych)
library(dplyr)

df <- read.csv("C:/Users/Daniel/Обучение/Аналитика/coffee_datasets/Coffee_Qlty.csv", sep=',')

method <- df$Processing.Method[df$Processing.Method != ""]

species <- df$Species[df$Processing.Method != ""]
continent <- df$Continent.of.Origin[df$Processing.Method != ""]
country <- df$Country.of.Origin[df$Processing.Method != ""]
harvest <- df$Harvest.Year[df$Processing.Method != ""]

print('Данные очищены')

method_count <- table(method)
method_percent <- round(prop.table(method_count) * 100, 1)
print('Информация о методах обработки кофейного зерна в датафрейме:')
print(method_percent)


species_count <- table(species)
species_percent <- round(prop.table(species_count) * 100, 1)
print('Информация о сортах кофейного зерна в датафрейме:')
print(species_percent)

harvest_count <- table(harvest)
harvest_percent <- round(prop.table(harvest_count) * 100, 1)
print('Информация о годе урожая кофейного зерна в датафрейме:')
print(harvest_percent)

country_count <- table(country)
country_percent <- round(prop.table(country_count) * 100, 1)
print('Информация о стране происхождения кофейного зерна в датафрейме:')
print(country_percent)


flavor <- df$Flavor[df$Processing.Method != ""]
print('Информация об оценки вкуса кофейного зерна в этом датафрейме:')
describe(flavor)

print('Проведение статистического теста для оценки зависимости вкуса зерна от метода обработки')
min_m <- min(table(method))
m_balance <- df %>%
  group_by(Processing.Method) %>%
  slice_sample(n = min_m)
table(m_balance$Processing.Method)

lt_balance <- leveneTest(Flavor ~ Processing.Method, data = m_balance)
lt_balance

fit_balanced <- aov(Flavor ~ Processing.Method, data = m_balance)
summary(fit_balanced)

TukeyHSD(fit_balanced)



