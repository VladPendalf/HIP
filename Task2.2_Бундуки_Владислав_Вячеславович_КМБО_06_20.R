library("lmtest")
library("GGally")
library("car")

data = swiss
help(swiss)

#Вариант 2 Бундуки Владислав КМБО-06-20

#Объясняемая переменная Agriculture , регрессоры: Fertility, Education, Catholic
model = lm(Agriculture ~ Fertility + Education  + Catholic, data)
model
summary(model)
#Agriculture = 117.2729 + (-0.78)*Fertility + (-2.01)*Edu + (0.26)Cat
#R^2 =  0.582

# 43 наблюдений и оценивалось 4 коэффициента: 43 - 4 = 39 степени свободы;

#Оценим доверительные интервалы для Agriculture = 117.2729 + (-0.78)*Fertility + (-2.01)*Edu + (0.26)Cat:

# a) Доверительный интервал для Fertility
Std_Err = 0.2748
# Критерий Стьюдента: 95%, 39 степени свободы
t_critical = qt(0.975, df = 39) # ~2.023
model$coefficients[2] - t_critical * Std_Err
model$coefficients[2] + t_critical * Std_Err

# Доверительный интервал для Fertility = [model$coefficients[2] - t_critical * Std_Err , model$coefficients[2] + t_critical * Std_Err]
# [-0.78 - 2.023 * 0.2748 ,  -0.78 + 2.023 * 0.2748]
# [-1.34 ,  -0.23]  -> коэффициент не может быть равным 0


# Проверка:
confint(model, level = 0.95)
#                2.5 %      97.5 %
#(Intercept) 75.233631 159.3121545
#Fertility   -1.338392  -0.2299865
#Education   -2.661649  -1.3704634
#Catholic     0.130171   0.3814702

# б) Доверительный интервал для Education
Std_Err = 0.3201
# Критерий Стьюдента: 95%, 39 степени свободы
model$coefficients[3] - t_critical * Std_Err
model$coefficients[3] + t_critical * Std_Err

# Доверительный интервал для Education = [model$coefficients[3] - t_critical * Std_Err , model$coefficients[3] + t_critical * Std_Err]
# [-2.01 - 2.023 * 0.3201 ,  -2.01 + 2.023 * 0.3201]
# [-2.66 ,  -1.37] -> коэффициент не может быть равным 0


# в) Доверительный интервал для Catholic
Std_Err = 0.0623
# Критерий Стьюдента: 95%, 39 степени свободы
model$coefficients[4] - t_critical * Std_Err
model$coefficients[4] + t_critical * Std_Err

# Доверительный интервал для Education = [model$coefficients[4] - t_critical * Std_Err , model$coefficients[4] + t_critical * Std_Err]
# [0.26 - 2.023 * 0.0623 ,  0.26 + 2.023 * 0.0623]
# [0.13 ,  0.38] -> коэффициент не может быть равным 0


# г) доверительный интервал для свободного коэф:
Std_Err = 20.85
# Критерий Стьюдента: 95%, 39 степени свободы
model$coefficients[1] - t_critical * Std_Err
model$coefficients[1] + t_critical * Std_Err

# Доверительный интервал для Intercept = [model$coefficients[1] - t_critical * Std_Err , model$coefficients[1] + t_critical * Std_Err]
# [117.2729 - 2.023 * 20.85 ,  117.2729 + 2.023 * 20.85]
# [75.1 ,  159.45] -> коэффициент не может быть равным 0

# Вывод: Поскольку 0 не попадает в доверительный интервал регрессоров => все регрессоры связаны с объясняемой переменной.

#Построим доверительный интервал для прогноза (Объясняемая переменная: Agriculture, регрессоры: Fertility , Education , Catholic )
model = lm(Agriculture ~ Fertility + Education  + Catholic, data)
model
summary(model)
#Agriculture = 117.2729 + (-0.78)*Fertility + (-2.01)*Edu + (0.26)Cat

new.data = data.frame(Fertility = 15, Education = 15, Catholic = 30)

predict(model, new.data, interval = "confidence")
# fit      lwr      upr
# 82.94383 54.39527 111.4924

# Вывод: Прогноз модели оценивается как 82.94383;
# Доверительный интервал для свободного коэффициентa = [54.39527,111.4924]