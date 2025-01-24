library("lmtest")
library("GGally")
library("car")

data=attitude

summary(data)
ggpairs(data)


model1=lm(rating~complaints+privileges+learning+raises+critical+advance,data)
model1
vif(model1)
summary(model1) #R^2=0.6831, �����, ��� rating ������� ������ ������ �� complaints
#��������� ��� ��������� ��������� � ���� �����������.

model2=lm(rating~complaints+privileges,data)
model2
summary(model2)  #R^2=0.6831, �� ���������.

model3=lm(rating~complaints+learning,data)
model3
summary(model3)  #R^2=0.708, ���� ���������

model4=lm(rating~complaints+raises,data)
model4
summary(model4)  #R^2=0.6839, ��������� �������������

model5=lm(rating~complaints+critical,data)
model5
summary(model5)  #R^2=0.6813, ����������

model6=lm(rating~complaints+advance,data)
model6
summary(model6)  #R^2=0.6823, ����������

model7=lm(rating~complaints+learning+raises,data)
model7
summary(model7)  #R^2=0.7083. ����������� ������� ������, ���� ��� ����������
# learning � raises R^2 ������������� ������, ��� �� 5%.

#����������
coef(model7)

confint(model7, level=0.9)

#������ se = sqrt (������� �������������� ������� �� ������� ��������� ��� ���� � ������� summary ������)
#se = 0.13637
se=0.13637

#�������� ���������: 95%, 26 ������� �������
t_critical=qt(0.975, df = 26)  #����� 30, ���������� 4, ������ 30-4
model7$coefficients[2] - t_critical * se
model7$coefficients[2] + t_critical * se
#��������
confint(model7, level = 0.95)
#������� � ������������� ����������
#� ���� median (�������) �������� �� �������, ������� � �������� ��������� ��������� ��������
new.data = data.frame(complaints = 65, learning = 57, raises = 64)
predict(model7, new.data, interval = "confidence")
# fit      lwr      upr
# 63.74566 61.08575 66.40557

#������ ��� �������� ������� �������� � ��������� ���������
new.data = data.frame(complaints = 60, learning = 60, raises = 70)
predict(model7, new.data)
#������� 60.96845

#����� ����������� ����� ������� �����������
model8=lm(rating~complaints+I(learning^2)+raises,data)
model8
summary(model8) #R^2=0.7134

model9=lm(rating~complaints+I(learning^2)+I(raises^2),data)
model9
summary(model9) #R^2=0.7136

#�������, ��������� �� ������ � ����� ������ �������
t_critical=qt(0.975, df = 26)  #����� 30, ���������� 4, ������ 30-4
se2=0.1324543
model7$coefficients[2] - t_critical * se2  #0.3812245 
model7$coefficients[2] + t_critical * se2  #0.9257519
#1�� ��� � ������ � ���� �� 3 ������� ������� ������, �� ������ � ��������
#�� 4 ������� ������� ������. ��� �������, �������� ��������� �� ���������
#�������� �� ����������.
#� ���� median (�������) �������� �� �������, ������� � �������� ��������� ��������� ��������
new.data = data.frame(complaints = 65, learning = 57, raises = 64)
predict(model9, new.data, interval = "confidence")
# fit      lwr      upr
# 63.51763 61.90358 66.13168
#�����, ��� ��� �������� ������� �������, �� ��������� � model7

#������ ��� �������� ������� �������� � ��������� ���������
new.data = data.frame(complaints = 60, learning = 60, raises = 70)
predict(model9, new.data)
#������� 60.79247

