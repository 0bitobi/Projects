# �������� ������ ������:
city <- c("City1", "City1", "City2", "City2", "City3", "City3")
��� <- c("Male", "Female", "Male", "Female", "Male", "Female")
number <- c(12450, 10345, 5670, 5800, 25129, 26000)
CITY <- data.frame(City = city, ��� = ���, Number = number) 


# �������� ����������� ������� CITY � �� ��������� ������������:
CITY
CITY$���
CITY[, 2]
CITY["���"]
CITY$Number[1:3] 

# ��������� 4-� ������� �� ������� Number:
CITY$Number[4]

# ��������� �������� 1-3 �� ������� Number:
CITY$Number[1:3]

# ��������� ��� �������� �����������, ����������� 10000
CITY$Number[CITY$Number > 10000]

# ��������� ��� �������� ����������� �������� ���������:
CITY$Number[CITY$��� == "Female"]

# ��������� �� �� �������, �� � �������������� []:
CITY[4, 3]
CITY[1:3, 3]
CITY[CITY$Number > 10000, 3]
CITY[CITY$��� == "Male", 3]

# �������� ��������� �������:
str(CITY)

# �������� ���� ����������, �������� � �������:
names(CITY)

# �������� ������ 3 ����� �������:
head(CITY, n = 3)

# �������� ��������� 3 ����� �������:
tail(CITY, n = 3)

# ���������� ������:
DF <- data.frame(X1 = c(1, 15, 1, 3), X2 = c(1, 0, 7, 0), X3 = c(1, 0, 1, 2),
                 X4 = c(7, 4, 41, 0), X5 = c(1, 0, 5, 3))
row.names(DF) <- c("A","B","C","D")

#  DF1 - �������, ������� ������� ������������� �� �������� ����� ��������:
DF1 <-  DF[ , rev(order(colSums(DF)))]

# DF2 - �������, ������ ������� ������������� � ����������
# ������� �� 1 �������, ����� � ���������� �� �������:
DF2 <- DF[order(DF$X1, -DF$X2), ]

# ����������� ������:

merge(DF1, DF2, all = TRUE)
