titanic = read.csv("titanic_clean.csv",header = TRUE,sep=",")
str(titanic)
titanic$pclass = as.factor(titanic$pclass)
titanic$name = as.character(titanic$name)
titanic$ticket =as.character(titanic$ticket)
titanic$cabin = as.character(titanic$cabin)
titanic$survived = factor(titanic$survived,levels=c(0,1))


str(titanic)
# summary
selected_data = titanic %>% select(age, body, fare, has_cabin_number, parch, sibsp)
descr(selected_data, stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "mad", "iqr", "cv", "skewness","kurtosis"), headings = FALSE)


descr(selected_data, stats = c("common"), headings = FALSE)


##단변량 분석

library(gmodels)

#1. 생존자 수
survive = titanic %>% group_by(survived) %>%summarise(count = n())
survive

CrossTable(titanic$survived)

ggplot(data= survive, aes(x=survived,y=count,fill= survived))+geom_bar(stat = "identity")



#2. Pclass별 탑승객 수
class = titanic %>% group_by(pclass) %>% summarise(count = n()) 
class
CrossTable(titanic$pclass)
ggplot(data= class, aes(x=pclass,y=count,fill= pclass))+geom_col()

#3. 성별 탑승객 수
gender = titanic %>% group_by(sex) %>% summarise(count = n()) 
gender
CrossTable(titanic$sex)
ggplot(data= gender, aes(x=sex,y=count,fill= sex))+geom_col()


#4. 동반 탑승객 수 (형제/배우자 )
sibsp = titanic %>% filter(sibsp >0) %>% group_by(sibsp) %>% summarise(count = n()) 


sibsp$sibsp <- factor(sibsp$sibsp, levels = c("0", "1", "2", "3", "4", "5", "8"))

CrossTable(titanic$sibsp)

ggplot(data = sibsp, aes(x = sibsp, y = count, fill = sibsp)) +
  geom_bar(stat = "identity") 



#5. 동반 탑승객 수 (부모/자식)
parch = titanic %>% group_by(parch) %>% summarise(count = n()) 

parch$parch <- factor(parch$parch, levels = c("0", "1", "2", "3", "4", "5", "6"))
parch <- parch %>% filter(!is.na(parch))

CrossTable(titanic$parch)

ggplot(data = parch, aes(x = parch, y = count, fill = parch)) +
  geom_bar(stat="identity") 

#6. 중간경유지 탑승객수
embark = titanic %>% group_by(embarked) %>% summarise(count = n()) 
embark
CrossTable(titanic$embarked)
ggplot(data = embark, aes(x = reorder(embarked,-count), y = count, fill = embarked)) +
  geom_bar(stat="identity") +scale_x_discrete(name = "Embarked")   


#7. 요금 


ggplot(titanic, aes(x = fare)) + 
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "lightblue", color = NA) +
  geom_density(alpha = .05, fill = "lightblue",color= "blue") +
  scale_y_continuous(breaks = seq(0, max(titanic$fare, na.rm = TRUE), by = 0.005)) +
  labs(title = "", x = "Fare", y = "") +
  theme_minimal()

CrossTable(titanic$fare)

#8. 나이


ggplot(titanic, aes(x = age)) + 
  geom_histogram(aes(y = ..density..), binwidth = 4, fill = "lightblue", color = NA) +
  geom_density(alpha = 0.2, fill = "lightblue",color= "blue") +
  scale_y_continuous(breaks = seq(0, max(titanic$age, na.rm = TRUE), by = 0.005)) +
  labs(title = "", x = "Age", y = "") +
  theme_minimal()

CrossTable(titanic$age)



##다변량 분석
# 1. 등석별 생존자 비율
titanic$survived =as.numeric(titanic$survived)

class_survive = titanic %>% filter(!is.na(survived)) %>% group_by(pclass) %>% summarise(survivedrate = mean(survived)-1)
class_survive


CrossTable(titanic$pclass, titanic$survived)

ggplot(class_survive, aes(x = factor(pclass), y = survivedrate)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(x = "Pclass", y = "", title = "") +
  theme_minimal() +coord_flip() + scale_y_continuous(limits = c(0, 1.0),breaks = seq(0,1.0,by= 0.2)) 


#2. 성별 생존자 비율
sex_survive = titanic %>% filter(!is.na(survived)) %>% group_by(sex)%>% summarise(survivedrate = mean(survived)-1)
sex_survive
CrossTable(titanic$sex, titanic$survived)
ggplot(sex_survive, aes(x = sex, y = survivedrate)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(x = "Sex", y = "", title = "") +
  theme_minimal() +coord_flip() + scale_y_continuous(limits = c(0, 1.0),breaks = seq(0,1.0,by= 0.2)) 


#3. 동반탑승객수 별 생존자 (형제/배우자)
sibsp_survive = titanic %>% filter(!is.na(survived)) %>% group_by(sibsp) %>% summarise(survivedrate = mean(survived)-1)

sibsp_survive
CrossTable(titanic$sibsp, titanic$survived)

ggplot(sibsp_survive, aes(x = sibsp, y = survivedrate)) +
  geom_bar(stat = "identity", fill = "blue") + 
  labs(x = "Sibsp", y = "", title = "") +
  theme_minimal() +coord_flip() + scale_y_continuous(limits = c(0, 1.0),breaks = seq(0,1.0,by= 0.2)) +scale_x_continuous(limits = c(-1, 8),breaks = seq(0,8,by= 1)) 

#4. 동반탑승객수 별 생존자 (부모/자식)
parch_survive = titanic %>% filter(!is.na(survived)) %>% group_by(parch) %>% summarise(survivedrate = mean(survived)-1)


CrossTable(titanic$parch, titanic$survived)

ggplot(parch_survive, aes(x = parch, y = survivedrate)) +
  geom_bar(stat = "identity", fill = "blue") + 
  labs(x = "Parch", y = "", title = "") +
  theme_minimal() +coord_flip() + scale_y_continuous(limits = c(0, 1.0),breaks = seq(0,1.0,by= 0.2))+scale_x_continuous(limits = c(-1, 8),breaks = seq(0,8,by= 1)) 


#5. 중간경유지 탑승별 생존자 비율
embark_survive = titanic %>% filter(!is.na(survived)) %>% group_by(embarked) %>% summarise(survivedrate = mean(survived)-1)

embark_survive
CrossTable(titanic$embarked, titanic$survived)

ggplot(embark_survive, aes(x = embarked, y = survivedrate)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(x = "Embarked", y = "", title = "") +
  theme_minimal() +coord_flip() + scale_y_continuous(limits = c(0, 1.0),breaks = seq(0,1.0,by= 0.2))


#6.나이별 생존자 분포
age_survive= boxplot(age~survived, data= titanic, col= c("steelblue","darkgreen"))
CrossTable(titanic$age, titanic$survived)
age_survive


#7.요금별 생존자 분포
fare_survive= boxplot(fare~survived, data= titanic, col= c("steelblue","darkgreen"))








