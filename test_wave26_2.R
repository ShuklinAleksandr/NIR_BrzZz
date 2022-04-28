#install.packages("devtools")
#devtools::install_github("bdemeshev/rlms")

library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library("car")
library("sandwich")


data <- rlms_read("C:\\Rlib\\r26i_os26b.sav")
glimpse(data)
data2 = select(data, vj13.2, v_age, vh5, v_educ, status, vj6.2, v_marst, vj1.1.2, vj23, vj24, vj32.1)

#исключаем строки с отсутствующими значениями NA
data2 = na.omit(data2)

#зарплата c элементами нормализации
data2$vj13.2
sal = as.numeric(data2$vj13.2)
sal1 = as.character(data2$vj13.2)
sal2 = lapply(sal1, as.integer)
sal = as.numeric(unlist(sal2))
mean(sal)
data2["salary"] = (sal - mean(sal)) / sqrt(var(sal))
data2["salary"]

#возраст c элементами нормализации
age1 = as.character(data2$v_age)
age2 = lapply(age1, as.integer)
age3 = as.numeric(unlist(age2))
data2["age"]= (age3 - mean(age3)) / sqrt(var(age3))
data2["age"]

#пол
data2["sex"]=data2$vh5
#data2["sex"] = lapply(data2$vh5, as.character)
data2$sex[which(data2$sex!='1')] <- 0
data2$sex[which(data2$sex=='1')] <- 1
data2$sex = as.numeric(data2$sex)

#образование
data2["h_educ"] = data2$v_educ
#data2["h_educ"] = lapply(data2$v_educ, as.character)
data2["higher_educ"] = data2$v_educ
data2["higher_educ"] = 0
data2$higher_educ[which(data2$h_educ=='21')] <- 1
data2$higher_educ[which(data2$h_educ=='22')] <- 1
data2$higher_educ[which(data2$h_educ=='23')] <- 1

#населенный пункт
data2["status1"]=data2$status
#data2["status1"] = lapply(data2$status, as.character)
data2["status2"] = 0
data2$status2[which(data2$status1=='1')] <- 1
data2$status2[which(data2$status1=='2')] <- 1
data2$status2 = as.numeric(data2$status2)

#продолжительность рабочей недели
dur1 = as.character(data2$vj6.2)
dur2 = lapply(dur1, as.integer)
dur3 = as.numeric(unlist(dur2))
data2["dur"] = (dur3 - mean(dur3)) / sqrt(var(dur3))

#семейное положение
data2["wed"]= data2$v_marst
#data2["wed"] = lapply(data2$v_marst, as.character)
data2$wed1 = 0
data2$wed1[which(data2$wed=='1')] <- 1
data2$wed1[which(data2$wed=='3')] <- 1
data2$wed1 = as.numeric(data2$wed1)

data2["wed2"] = lapply(data2["wed"], as.character)
data2$wed2 = 0
data2$wed2[which(data2$wed=='2')] <- 1
data2$wed2 = as.numeric(data2$wed2)

data2["wed3"]=data2$v_marst
data2$wed3 = 0
data2$wed3[which(data2$wed=='4')] <- 1
data2$wed3 = as.numeric(data2$wed3)

data2["wed4"]=data2$v_marst
data2$wed4 = 0
data2$wed4[which(data2$wed=='5')] <- 1
data2$wed4 = as.numeric(data2$wed4)

#удовлетворенность
data2["sat"]=data2$vj1.1.2
data2["sat"] = lapply(data2["sat"], as.character)
data2["satisfy"] = 0
data2$satisfy[which(data2$sat=='1')] <- 1
data2$satisfy[which(data2$sat=='2')] <- 1
data2$satisfy = as.numeric(data2$satisfy)

#государство - совладелец предприятия
data2["state"] = data2$vj23
data2["state"] = lapply(data2["state"], as.character)
data2["state_owner"] = 0
data2$state_owner[which(data2$state=='1')] <- 1
data2$state_owner = as.numeric(data2$state_owner)

#другое государство - совладелец предприятия
data2["foreign"] = data2$vj24
data2["foreign"] = lapply(data2["foreign"], as.character)
data2["foreign_owner"] = 0
data2$foreign_owner[which(data2$foreign=='1')] <- 1
data2$foreign_owner = as.numeric(data2$foreign_owner)

#есть приработок, вторая, n-я работа
data2["sj"] = data2$vj32.1
data2["sj"] = lapply(data2["sj"], as.character)
data2["second_job"] = 0
data2$second_job[which(data2$sj=='1')] <- 1
data2$second_job = as.numeric(data2$second_job)

data2 = na.omit(data2)

#учет инфляции: +3.4% к показателям зар.платы 2017 года, +2.52% к показателям 2016 года
data2["salary"]
data2["salary"] = data2["salary"]*1.034

data3 = select(data2, salary, age, sex, higher_educ, status2, dur, wed1,wed2,wed3,wed4, satisfy, second_job, foreign_owner, state_owner)

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

mean(data3$wed1)
mean(data3$wed2)
mean(data3$wed3)
mean(data3$wed4)
mean(data2$satisfy)
mean(data3$higher_educ)
mean(data3$state_owner)
mean(data3$foreign_owner)
mean(data3$second_job)

#графики парных зависимостей
ggpairs(data3)

#уровни факторных переменных
levels(data2$v_educ)
qplot(data = data3, salary)


#построение зависимостей
model1 = lm(data = data3, salary~age + sex + higher_educ + status2 + dur + wed1 + wed2 + wed3)
summary(model1)
vif(model1)
#переменные на пол плохие - у них большой vif, в модели они не значимые

model2 = lm(data = data3, salary~age + sex + higher_educ + status2 + dur)
summary(model2)
vif(model2)
waldtest(model2, model1)
#но модель без них работает хуже

#r^2 = 0.1535

bptest(model1)
gqtest(model1, order.by = ~salary, data = data3, fraction = 0.2)
#гетероскедастичность подтверждается и по тесту Бройша-Пагана (Уайта), и по Голдфельду-Квандту

#оценка коэффициентов и их ошибки. Доверительные интервалы: коэффициент +- 1.96*ошибка
conftable = coeftest(model1, vcov.=vcovHC(model1))
conftable[,1]
conftable[,2]

#можем ввести нелинейные регрессоры
model2 = lm(data = data3, salary~age + sex + higher_educ + status2 + I(dur^2) + dur + wed1 + wed2 + wed3+satisfy+state_owner + foreign_owner)
summary(model2)
vif(model2)

#можем оценить степень от salary
model3 = lm(data = data3, salary^0.5~age + sex + higher_educ + status2 + I(dur^2) + dur + wed1 + wed2 + wed3+satisfy+state_owner + foreign_owner)
summary(model3)
vif(model3)


#квантили по зарплате
data0 = select(data, vj13.2)
salq = as.numeric(data0$vj13.2)
salq1 = as.character(data0$vj13.2)
salq2 = lapply(salq1, as.integer)
salq3 = as.numeric(unlist(salq2))*1.034
quantile(salq3, c(0.1, 0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8, 0.9, 0.95), na.rm = TRUE)
