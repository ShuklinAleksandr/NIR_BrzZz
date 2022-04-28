library("lmtest")
library("GGally")
library("car")

data=attitude

summary(data)
ggpairs(data)


model1=lm(rating~complaints+privileges+learning+raises+critical+advance,data)
model1
vif(model1)
summary(model1) #R^2=0.6831, видим, что rating зависит хорошо только от complaints
#посмотрим все возможные вариианты с этим регрессором.

model2=lm(rating~complaints+privileges,data)
model2
summary(model2)  #R^2=0.6831, не изменился.

model3=lm(rating~complaints+learning,data)
model3
summary(model3)  #R^2=0.708, чуть повысился

model4=lm(rating~complaints+raises,data)
model4
summary(model4)  #R^2=0.6839, повысился незначительно

model5=lm(rating~complaints+critical,data)
model5
summary(model5)  #R^2=0.6813, уменьшился

model6=lm(rating~complaints+advance,data)
model6
summary(model6)  #R^2=0.6823, уменьшился

model7=lm(rating~complaints+learning+raises,data)
model7
summary(model7)  #R^2=0.7083. Максимально хорошая модель, хоть при добавлении
# learning и raises R^2 увеличивается меньше, чем на 5%.

#коэфиценты
coef(model7)

confint(model7, level=0.9)

#ошибка se = sqrt (элемент ковариационной матрицы на главной диагонали или есть в таблице summary модели)
#se = 0.13637
se=0.13637

#критерий Стьюдента: 95%, 26 степени свободы
t_critical=qt(0.975, df = 26)  #всего 30, используем 4, значит 30-4
model7$coefficients[2] - t_critical * se
model7$coefficients[2] + t_critical * se
#проверка
confint(model7, level = 0.95)
#прогноз с доверительным интервалом
#я взял median (средние) значения из таблицы, поэтому у прогноза получился маленький интервал
new.data = data.frame(complaints = 65, learning = 57, raises = 64)
predict(model7, new.data, interval = "confidence")
# fit      lwr      upr
# 63.74566 61.08575 66.40557

#возьму для проверки близкие значения и предскажу результат
new.data = data.frame(complaints = 60, learning = 60, raises = 70)
predict(model7, new.data)
#прозноз 60.96845

#можно попробовать более сложную зависимость
model8=lm(rating~complaints+I(learning^2)+raises,data)
model8
summary(model8) #R^2=0.7134

model9=lm(rating~complaints+I(learning^2)+I(raises^2),data)
model9
summary(model9) #R^2=0.7136

#проверю, изменятся ли данные с более лучшей моделью
t_critical=qt(0.975, df = 26)  #всего 30, используем 4, значит 30-4
se2=0.1324543
model7$coefficients[2] - t_critical * se2  #0.3812245 
model7$coefficients[2] + t_critical * se2  #0.9257519
#1ый раз я ошибся и взял на 3 степени свободы меньше, но сейчас я посчитал
#на 4 степени свободы меньше. Что странно, интервал изменился но остальные
#значения не изменились.
#я взял median (средние) значения из таблицы, поэтому у прогноза получился маленький интервал
new.data = data.frame(complaints = 65, learning = 57, raises = 64)
predict(model9, new.data, interval = "confidence")
# fit      lwr      upr
# 63.51763 61.90358 66.13168
#видно, что наш интервал заметно сузился, по сравнения с model7

#возьму для проверки близкие значения и предскажу результат
new.data = data.frame(complaints = 60, learning = 60, raises = 70)
predict(model9, new.data)
#прозноз 60.79247

