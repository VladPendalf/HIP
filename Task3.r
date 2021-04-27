install.packages("devtools")
devtools::install_github("bdemeshev/rlms")

library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library("car")
library("sandwich")


data <- rlms_read("C:\\Users\\vladb\\Downloads\\r22i_os26c.sav")
glimpse(data)
data2 = select(data, rj13.2, r_age, rh5, r_educ, status, rj6.2, r_marst)

#������� ������ � NA
data2 = na.omit(data2)
glimpse(data2)

#�������� c ���������� ������������
data2$rj13.2
sal = as.numeric(data2$rj13.2)
sal1 = as.character(data2$rj13.2)
sal2 = lapply(sal1, as.integer)
sal = as.numeric(unlist(sal2))
mean(sal)
data2["salary"] = (sal - mean(sal)) / sqrt(var(sal))
data2["salary"]

#������� c ���������� ������������
age1 = as.character(data2$r_age)
age2 = lapply(age1, as.integer)
age3 = as.numeric(unlist(age2))
data2["age"]= (age3 - mean(age3)) / sqrt(var(age3))
data2["age"]

#���
#data2["sex"]=data2$rh5
#data2["sex"] = lapply(data2$rh5, as.character)
data2$sex = as.numeric(data2$rh5)
data2$sex[which(data2$sex!='1')] <- 0 #�������
data2$sex[which(data2$sex=='1')] <- 1 #�������


#�����������
#data2["r_educ"] = data2$r_educ
#data2["r_educ"] = lapply(data2$r_educ, as.character)
#data2["higher_educ"] = data2$r_educ
data2$r_educ = as.numeric(data2$r_educ)
data2$r_educ[which(data2$r_educ=='21')] <- 1 #���� ������ � ������ �����������
data2$r_educ[which(data2$r_educ=='22')] <- 1 #����������� � �.�. ��� �������
data2$r_educ[which(data2$r_educ=='23')] <- 1 #����������� � �.�. � ��������

#���������� �����
#data2["status1"]=data2$status
#data2["status1"] = lapply(data2$status, as.character)
data2$status1 = as.numeric(data2$status)
data2$status1[which(data2$status1=='1')] <- 1 #��������� �����
data2$status1[which(data2$status1=='2')] <- 1 #�����


#����������������� ������� ������
dur1 = as.character(data2$rj6.2)
dur2 = lapply(dur1, as.integer)
dur3 = as.numeric(unlist(dur2))
data2["dur"] = (dur3 - mean(dur3)) / sqrt(var(dur3))

#�������� ���������
#data2["wed"]= data2$r_marst
#data2["wed"] = lapply(data2$r_marst, as.character)
data2$wed = as.numeric(data2$r_marst)
data2$wed[which(data2$wed=='1')] <- 1 #������� �� ���� � �����
data2$wed[which(data2$wed=='2')] <- 1 # ������� � ������������������ �����


#data2["wed2"] = lapply(data2["wed"], as.character)
data2$wed2 = as.numeric(data2$r_marst)
data2$wed2[which(data2$wed=='4')] <- 1 # ���������
data2$wed2[which(data2$wed=='5')] <- 1 # B�����/�����

#data2$wed2 = as.numeric(data2$wed2)

#data2["wed3"] = lapply(data2["wed"], as.character)
data2$wed3 = as.numeric(data2$r_marst)
data2$wed3[which(data2$wed=='3')] <- 1 # ������ ������, �� �� ����������������
data2$wed3[which(data2$wed=='6')] <- 1 # ���������� ����������������, �� ������ �� ���������

data3 = select(data2, salary, age, sex, r_educ, status1, dur, wed, wed2, wed3)

#���������� ������������ ��� ������ 
model = lm(data = data3, salary~age + sex + r_educ + status1 + dur + wed + wed2 + wed3)
summary(model)
vif(model)
#R^2 = 0.1339 - ����� ������
#p-�������������� � ���� �������� (***), ����� wed2 (**)

# 2. ������������������� � ��������� ������������ ����������: ����������� �������� � ������� (���� �� �� 0.1 �� 2 � ����� 0.1).
model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(dur^0.1) + I(age^0.1))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.1691
#� - ���������
#vif <= 7

model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(dur^0.2) + I(age^0.2))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.1695
#� - ���������
#vif <= 7

model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(dur^0.3) + I(age^0.3))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.17
#� - ���������
#vif <= 10 ~ ���� �������� �����������

model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(age^0.4) + I(dur^0.4))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.1704
#� - ���������
#vif <= 13 ~ ���� �������� �����������

model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(dur^0.4))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.1413
#� - ���������
#vif <= 14 ~ ���� �������� �����������

model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(age^0.4))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.1595
#� - ���������
#vif <= 11 ~ ���� �������� �����������

model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(dur^0.4))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.1413
#� - ���������
#vif <= 14 ~ ���� �������� �����������

model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(age^2) + I(dur^2))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.1488
#� - ���������
#vif <= 3 

model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(dur^2))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.1345
#� - ���������
#vif <= 3

model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(age^2))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.1484
#� - ����-���� ����, ��� � �������� ������
#vif <= 3

model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(log(age)) + I(log(dur)))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.1687
#� - ���������
#vif <= 7

model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(log(dur)))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.1407
#� - ����, ��� � ��������� ������, �� �����, ��� � ��������� ������� �� ��������� � �����������
#vif <= 5

model_test_1 = lm(data = data3, salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(log(age)))
summary(model_test_1)
vif(model_test_1)
#R^2 = 0.1596
#� - ���������
#vif <= 7

#����: ������ ������ ��� salary~ age + sex + r_educ + status1 + dur + wed + wed2 + wed3 + I(log(dur))
#�������� ������ �� �������� ����������� ����������
modele_1 = lm(dur~I(log(dur)), data3)
modele_1
summary(modele_1) # R^2 =  0.8063 < 0.8, ������ ��� �������� ����������� � ����� ������������ � ����� ������

modele_2 = lm(age~I(dur^2), data3)
modele_2
summary(modele_2) # R^2 =  3.001e-05 < 0.1 , ������ ��� �������� ����������� � ����� ������������ � ����� ������

#�����: ����� ������� �� �������� ������� �������� �������� ��� ������� �����������, ���������� � ��������� ������ , ����������������, 
# �� ���������� � ����� ��� ������ ��� ���������� ������������������, �� �� ����������� ������.

#(Intercept)      age          sex       r_educ      status1          dur          wed         wed2         wed3    I(log(dur))  
#0.009378    -0.100804     0.452144    -0.030679    -0.154806     0.068537    -0.086875     0.143205     0.152724     0.056648    

#���� ������������: "������� �� �������"
data4 = subset(data3, sex == 0)
data4

data5 = subset(data4, wed2 == 1)
data5

# ���� ������������: "�������, ������� � ������, �����������"
data6 = subset(data3, sex == 0)
data6

data7 = subset(data6, status1 == 1)
data7

data8 = subset(data7, wed2 == 1)
data8

model_subset = lm(data = data5, salary~age + sex + r_educ + status1 + dur + wed + wed2 + wed3)
summary(model_subset)
# R^2 = 0.08617
# ��������� �������� �������� ������� , ��� ������� �����������, ���������� �� � ������, � ����������������

model_subset = lm(data = data8,salary~age + sex + r_educ + status1 + dur + wed + wed2 + wed3)
summary(model_subset)
# R^2 = 0.06726
# ��������� �������� �������� �������, ��� ������� ����������� � ����������������

