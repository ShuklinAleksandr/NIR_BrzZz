library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library("car")
library("sandwich")

data2 <- rlms_read("/Users/user/Desktop/Julia_C++/NIR/r20i_os26c.sav")
glimpse(data2)
data2 = select(data2,p_marst,status,p_educ,ph5,pj6.2,pj13.2,p_age,pj1.1.2)
data2 = na.omit(data2)

#зарплата c элементами нормализации
sal1 = as.character(data2$pj13.2)
sal2 = lapply(sal1, as.integer)
sal = as.numeric(unlist(sal2))
mean(sal)
data2["salary"] = (sal - mean(sal)) / sqrt(var(sal))
data2["salary"]

#возраст c элементами нормализации
age1 = as.character(data2$p_age)
age2 = lapply(age1, as.integer)
age3 = as.numeric(unlist(age2))
data2["age"]= (age3 - mean(age3)) / sqrt(var(age3))
data2["age"]

#продолжительность рабочей недели с элементами нормализации
dur1 = as.character(data2$pj6.2)
dur2 = lapply(dur1, as.integer)
dur3 = as.numeric(unlist(dur2))
data2["dur"] = (dur3 - mean(dur3)) / sqrt(var(dur3))

#удовлетворенность
data2["sat"]=data2$pj1.1.2
data2["sat"] = lapply(data2["sat"], as.character)
data2["satisfy"] = 0
data2$satisfy[which(data2$sat=='1')] <- 1
data2$satisfy[which(data2$sat=='2')] <- 1
data2$satisfy = as.numeric(data2$satisfy)

#Пол
data2["sex"]=data2$ph5
data2["sex"]=lapply(data2["sex"],as.character)
data2$sex[which(data2$sex!='1')] <- 0
data2$sex[which(data2$sex=='1')] <- 1
data2$sex = as.numeric(data2$sex)

#Семейное положение
data2["wed"]= data2$p_marst
data2["wed"] = lapply(data2["wed"], as.character)
data2$wed1 = 0
data2$wed1[which(data2$wed=='1')] <- 1
data2$wed1[which(data2$wed=='3')] <- 1
data2$wed1 = as.numeric(data2$wed1)

data2["wed2"] = lapply(data2["wed"], as.character)
data2$wed2 = 0
data2$wed2[which(data2$wed=='2')] <- 1
data2$wed2 = as.numeric(data2$wed2)

data2["wed3"]=data2$p_marst
data2$wed3 = 0
data2$wed3[which(data2$wed=='4')] <- 1
data2$wed3 = as.numeric(data2$wed3)

data2["wed4"]=data2$p_marst
data2$wed4 = 0
data2$wed4[which(data2$wed=='5')] <- 1
data2$wed4 = as.numeric(data2$wed4)

#Горожанин или нет
data2["status1"]=data2$status
data2["status1"] = lapply(data2["status1"], as.character)
data2["status2"] = 0
data2$status2[which(data2$status1=='1')] <- 1
data2$status2[which(data2$status1=='2')] <- 1
data2$status2 = as.numeric(data2$status2)

#Имеет ли диплом о высшем образовании
data2["higher_educ"]=data2$p_educ
data2["higher_educ"]=lapply(data2["higher_educ"],as.character)
data2$higher_educ[which(data2$higher_educ=='21')] <- 21
data2$higher_educ[which(data2$higher_educ=='22')] <- 21
data2$higher_educ[which(data2$higher_educ=='23')] <- 21
data2$higher_educ = as.numeric(data2$higher_educ)

data2 = na.omit(data2)
#учет инфляции: +3.4% к показателям зар.платы 2017 года, +2.52% к показателям 2016 года
data2["salary"]
data2["salary"] = data2["salary"]*1.0252*1.034

data3 = select(data2, salary, age, sex, higher_educ, status2, dur, wed1,wed3, satisfy)

#среднее, медиана и мода для каждого параметра
uniqv <- unique(data2$salary)
u1 = uniqv[which.max(tabulate(match(data2$salary, uniqv)))]
table_salary = data.frame(Value = "salary", Mean = mean(data2$salary,na.rm = TRUE), Median = median(data2$salary,na.rm = TRUE), Mode = u1)
table_salary

uniqv <- unique(data2$age)
u1 = uniqv[which.max(tabulate(match(data2$age, uniqv)))]
table_age = data.frame(Value = "age", Mean = mean(data2$age,na.rm = TRUE), Median = median(data2$age,na.rm = TRUE), Mode = u1)
table_age

mean(data2$sex)

mean(data2$status2)

uniqv <- unique(data2$dur)
u1 = uniqv[which.max(tabulate(match(data2$dur, uniqv)))]
table_dur = data.frame(Value = "dur", Mean = mean(data2$dur,na.rm = TRUE), Median = median(data2$dur,na.rm = TRUE), Mode = u1)
table_dur

mean(data2$wed1)
mean(data2$wed2)
mean(data2$wed3)
mean(data2$wed4)
mean(data2$satisfy)
mean(data2$higher_educ)

model1 = lm(data = data2, salary~age + sex + higher_educ + status2 + dur + wed1 + wed2 + wed3)
summary(model1)#Multiple R-squared: 0.1648,	Adjusted R-squared:  0.1637
vif(model1)

#Переменные wed1 и wed2 имеют слишком большой vif, и R-squared почти не изменился, значит уберем
model2 = lm(data = data2, salary~age + sex + higher_educ + status2 + dur  + wed3)
summary(model2)#Multiple R-squared: 0.1627,	Adjusted R-squared:  0.1619
vif(model2)
waldtest(model1,model2)

model3 = lm(data = data2, salary~log(age) + sex + higher_educ + status2 + dur + wed3 + I(dur^2))
summary(model3)#Multiple R-squared: 0.1683,	Adjusted R-squared:  0.1663
vif(model3)

model4 = lm(data = data2, salary~log(age) + sex + higher_educ + status2 + dur + wed3 + I(dur^2) + I(dur*age))
summary(model4)#Multiple R-squared: 0.1687,	Adjusted R-squared:  0.1664
vif(model4)

#в целом, можно оценивать не переменную, а функцию от неё
model5 = lm(data = data2, log(salary)~log(age) + sex + higher_educ + status2 + dur + wed3 + I(dur^2) + I(dur*age))
summary(model5)#Multiple R-squared: 0.05306,	Adjusted R-squared:  0.04489
vif(model5)
#Но это была плохая идея

#выделим подмножество мужчин, не вступавшие в брак, без высшего образования
data3 = subset(data2, sex == 1)
data4= subset(data3, wed3 == 1)
data5= subset(data4, higher_educ != 21)
data5

#выделим мужчин сотоящие в браке. являющиеся городскими жителями
data6 = subset(data2, sex == 1)
data7 = subset(data6, wed1 == 1)
data8 = subset(data7, status2 == 1)
data8

#Сравню среднюю зарплата моих моделей
#1 0.04262451
mean(data5$salary)
#2 0.2652047
mean(data8$salary)
 
#Очевидно, что 1<2

#строим модель для подмножества 1
model6 = lm(data = data5, salary~age + sex + higher_educ + status2 + dur + wed1 + wed2 + wed3)
summary(model6)#Multiple R-squared: 0.1362 ,	Adjusted R-squared:  0.0889
vif(model6)

model6_2 = lm(data = data5, salary~log(age) + sex + higher_educ + status2 + dur + wed1 + wed2 + wed3 + I(dur^2))
summary(model6_2)#Multiple R-squared: 0.3856,	Adjusted R-squared:  0.2925
vif(model6_2)

#строим модель для подмножества 2
model7 = lm(data = data8, salary~age + sex + higher_educ + status2 + dur + wed1 + wed2 + wed3)
summary(model7)#Multiple R-squared:  0.08563,	Adjusted R-squared:  0.08081
vif(model7)

model7_2 = lm(data = data8, salary~log(age) + sex + higher_educ + status2 + dur + wed1 + wed2 + wed3 + I(dur^2))
summary(model7_2)#Multiple R-squared: 0.1828,	Adjusted R-squared:  0.1564
vif(model7_2)


#строим модель на всех данных
model8 = lm(data = data2, salary~age + sex + higher_educ + status2 + dur + wed1 + wed2 + wed3)
summary(model8)#Multiple R-squared: 0.1648,	Adjusted R-squared: 0.1637
vif(model8)

model8_2 = lm(data = data2, salary~log(age) + sex + higher_educ + status2 + dur + wed1 + wed2 + wed3 + I(dur^2))
summary(model8_2)#Multiple R-squared: 0.1704,	Adjusted R-squared:  0.1678
vif(model8_2)




