library("lmtest")
library("GGally")
library("car")

data=attitude

summary(data)
ggpairs(data)
#1 ПУНКТ
#проверим отсутствие зависимости между регрессорами перед построением модели
test_model1 = lm(raises~critical, data)
test_model2 = lm(raises~advance, data)
test_model3 = lm(critical~advance, data)
test_model4 = lm(raises~advance+critical,data)
test_model5 = lm(critical~raises+advance,data)
test_model6 = lm(advance~raises+critical,data)
summary(test_model1)    #R^2=0.142 зависимость между регрессорами raises 
# и critical маленькая
summary(test_model2)  #R^2=0.3297 зависимость между регрессорами raises
# и advance маленькая
summary(test_model3)  #R^2=0.08028 зависимость между регрессорами critical
# и advance маленькая
summary(test_model4) #R^2=0.3796 зависимость маленькая 3 регрессоров вместе
summary(test_model5) #R^2=0.1487
summary(test_model6) #R^2=0.3349

#2 ПУНКТ
modelee1 = lm(rating~raises+critical+advance, data) 
modelee1
summary(modelee1) #R^2=0.4013/ p-value=0.003537
vif(modelee1)

modelee1_2 = lm(rating~raises+advance, data)
modelee1_2
summary(modelee1_2)
vif(modelee1_2)
#R^2=0.3986, упал незначильно, значит можем убрать эту переменную
#модель плохая, так как нет даже половины от 0.8
#3 ПУНКТ

data = attitude
data = as.data.frame(data)
data["raises_log"] = log(data["raises"])
data["critical_log"] = log(data["critical"])
data["advance_log"] = log(data["advance"])

data

#4)
modelee4_1 = lm(rating~raises+critical+advance+I(raises^2), data) 
modelee4_1
summary(modelee4_1) #R^2=0.4172
vif(modelee4_1)

modelee4_2 = lm(rating~raises+critical+advance+I(critical^2), data) 
modelee4_2
summary(modelee4_2) #R^2=0.4722
vif(modelee4_2)

modelee4_3 = lm(rating~raises+critical+advance+I(advance^2), data) 
modelee4_3
summary(modelee4_3) #R^2=0.4004
vif(modelee4_3)

modelee4_4 = lm(rating~raises+critical+advance+I(raises*critical), data) 
modelee4_4
summary(modelee4_4) #R^2=0.434
vif(modelee4_4)

modelee4_5 = lm(rating~raises+critical+advance+I(raises*advance), data) 
modelee4_5
summary(modelee4_5) #R^2=0.4073
vif(modelee4_5)

modelee4_6 = lm(rating~raises+critical+advance+I(critical*advance), data) 
modelee4_6
summary(modelee4_6) #R^2=0.4011
vif(modelee4_6)

modelee4_7 = lm(rating~raises+critical+advance+I(raises*critical*advance), data) 
modelee4_7
summary(modelee4_7) #R^2=0.4263
vif(modelee4_7)

modelee4_8 = lm(rating~raises+critical+advance+raises_log, data) 
modelee4_8
summary(modelee4_8) #R^2=0.4127
vif(modelee4_8)

modelee4_9 = lm(rating~raises+critical+advance+critical_log, data) 
modelee4_9
summary(modelee4_9) #R^2=0.4676
vif(modelee4_9)

modelee4_10 = lm(rating~raises+critical+advance+advance_log, data) 
modelee4_10
summary(modelee4_10) #R^2=0.4001
vif(modelee4_10)

#Мы можем сделать вывод, что model4_2 является самой качественной. 