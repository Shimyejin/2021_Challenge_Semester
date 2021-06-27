setwd('C:\\Users\\User\\Desktop\\성대\\도전학기 2021\\R')

#일반적인 데이터 불러오기
#bike_data<-read.csv('bike_usage_0.csv')
#population은 인코딩 문제 해결해줘야함
#library(data.table)
#population <- fread('population_by_Gu.txt',encoding='UTF-8')
#weather <- read.csv("weather.csv")

#여기서 tidyverse로 파일들 불러오기
library(tidyverse)
bike_data<-read_csv('bike_usage_0.csv',locale=locale(encoding='EUC-KR'))
problems(bike_data) #18개의 문제 발견

population <- read_delim('population_by_Gu.txt',delim='\t')
population

weather <- read.csv("weather.csv")

head(bike_data,10)

unique(bike_data$Membership_type)
table(bike_data$Membership_type)

#데이터 전처리
colSums(is.na(bike_data))
bike_data$Gender[is.na(bike_data$Gender)]<-'U'

table(bike_data$Gender)
bike_data$Gender[bike_data$Gender=='f']<-'F'
bike_data$Gender[bike_data$Gender=='m']<-'M'

bike_data <- bike_data %>% filter(!is.na(Momentum))

#Tukey(이상값확인하고 처리하기)
tukey_outliers<-function(x,k=1.5,na.rm=T){
  q<-quantile(x,probs=c(0.25,0.75),na.rm=na.rm)
  iqr<-diff(q)
  (q[1]-k*iqr<=x)&(x<=q[2]+k*iqr)
}

tukey_outliers(bike_data$Distance) #False = 이상값
bike_data$outlier<-!tukey_outliers(bike_data$Distance) #따라서 반대로 적용
sum(bike_data$outlier)

#데이터 하나로 합치기
stations = read_csv('stations.csv')
bike_data2 <- inner_join(bike_data, stations,by=c('Station_no_out'='ID'))
bike_data2

#시각화
library(ggplot2)
data<-bike_data2 %>% group_by(Gender) %>% summarise(Counts=n()) %>% 
  mutate(Percentage=Counts/sum(Counts))

ggplot(data,aes(x='',y=Percentage,fill=Gender))+geom_bar(width=1,stat='identity')+coord_polar('y')

ggplot(data,aes(x=Gender,y=Percentage,fill=Gender))+geom_bar(stat='identity')

ggplot(bike_data2,aes(x=Distance))+geom_histogram(binwidth = 2000,fill='skyblue',color='black')

ggplot(bike_data2,aes(x=Distance))+geom_boxplot()
ggplot(bike_data2,aes(x=Gender,y=Distance))+geom_boxplot()

under_5000<-bike_data[bike_data2$Distance<5000,]
ggplot(under_5000,aes(x=Gender,y=Distance))+geom_boxplot(aes(fill=Gender))

data<-weather %>% group_by(time) %>% summarize(mean_temp=mean(temp))
data
ggplot(data,aes(x=time,y=mean_temp))+geom_line(color='red')+geom_point()

#데이터전처리
bike_data2[bike_data2$Distance==0,"Duration"]

bike_data2[bike_data2$Distance==0,] %>% summarize(mean=mean(Duration))
bike_data2[bike_data2$Distance==0,] %>% summarize(max=max(Duration))
mean(bike_data2$Duration[bike_data2$Distance==0])

bike_data2<-bike_data2[bike_data2$Distance!=0,]

spread(summarise(group_by(bike_data2,Age_Group,Membership_type),sum=sum(Distance)),Membership_type,sum)
pivot_table<-bike_data2 %>% 
  group_by(Age_Group,Membership_type) %>% 
  summarise(sum=sum(Distance)) %>% 
  spread(Membership_type,sum)
pivot_table

unpivot<-pivot_table %>% gather(Membership_type,value,-Age_Group)
unpivot

unpivot %>% spread(Membership_type,value)

##정규분포

#[histogram]
#random_sample<-data.frame(Num=rnorm(1000,mean=10,sd=2))
#ggplot(random_sample,aes(x=Num))+geom_histogram(bins=100)

#[QQplot]
#rd_sample<-rnorm(1000,mean=10,sd=2)
#qqnorm(rd_sample)
#qqline(rd_sample,col=2)}

##가설검정 & t-검정
unique(bike_data2$Gu)
var.test(bike_data2$Distance[bike_data2$Gu=='마포구'],
         bike_data2$Distance[bike_data2$Gu=='영등포구'])

mean(bike_data2$Distance[bike_data2$Gu=='마포구'])
mean(bike_data2$Distance[bike_data2$Gu=='영등포구'])

#data<-bike_data2[bike_data2$Gu=='마포구' | bike_data2$Gu=='영등포구']
#t.test(Distance ~ Gu,data=data,var.equal=True)

##분산분석
bartlett.test(bike_data2$Distance,bike_data2$Gu)
summary(aov(Distance ~ Gu,data=bike_data2))
boxplot(formula=Distance~Gu,data=bike_data2,outline=F)

#Tukey HSD
group_aov<-aov(Distance~Gu,data=bike_data2)
TukeyHSD(group_aov)

chi_table<-table(bike_data2$Age_Group,bike_data2$Membership_type)
chi_table
chisq.test(chi_table)

##상관분석
count_by_Gu <- bike_data2 %>% group_by(Gu) %>% summarise(count=n())
count_by_Gu
by_Gu <- inner_join(count_by_Gu,population,by='Gu')
by_Gu
#산점도
plot(by_Gu$count,by_Gu$Population,cex=2,lwd=3)
#상관계수
cor(by_Gu$count,by_Gu$Population)
cor.test(by_Gu$count,by_Gu$Population) #피어슨

##회귀분석
str(weather)
str(bike_data2)
weather2<-aggregate(cbind(temp,cum_precipitation,humidity,insolation,sunshine,wind,wind_direction,sea_lvl_pressure,pressure)~date+time,data = weather,mean)
str(weather2)
bike_data3<-aggregate(Distance~Date_out+Time_out,data=bike_data2,length)
str(bike_data3)
names(bike_data3)
names(bike_data3)[3]<-'Count'
names(bike_data3)
bike_data3$Date<-as.character(bike_data3$Date_out)
bike_weather<-inner_join(bike_data3,weather2,by=c('Date'='date','Time_out'='time'))
str(bike_weather)
summary(lm(Count~temp,data=bike_weather))

##다준회귀분석_범주형 변수
library(caret)
train_index<-createDataPartition(bike_weather$Time_out,p=0.7,list = F)
training<-bike_weather[train_index,]
testing<-bike_weather[-train_index,]

str(training)
str(testing)

fitted<-lm(Count~cum_precipitation+humidity+temp+wind,data = training)
summary(fitted)

pred <- predict(fitted,testing)
library(MLmetrics)

MSE(pred,testing$Count)
RMSE(pred,testing$Count)
MAE(pred,testing$Count)
MAPE(pred,testing$Count)

bike_weather$Rain_YN='N'
bike_weather[bike_weather$cum_precipitation>0,'Rain_YN']<-'Y'
str(bike_weather)

dummy<-dummyVars("~.",data=bike_weather)
newdata<-data.frame(predict(dummy,newdata=bike_weather))
str(newdata)
str(bike_weather)

train_index<-createDataPartition(newdata$Time_out,p=.7,list = F)
training<-newdata[train_index,]
testing<-newdata[-train_index,]

fitted<-lm(Count~humidity+temp+wind+Rain_YNN+Rain_YNY,data=training)
summary(fitted)
pred<-predict(fitted,testing)
MSE(pred,testing$Count)
RMSE(pred,testing$Count)
MAE(pred,testing$Count)
MAPE(pred,testing$Count)

##로지스틱 회귀분석
bike_weather$over_500<-1
bike_weather[bike_weather$Count<500,'over_500']<-0
bike_weather

train_index<-createDataPartition(bike_weather$Time_out,p=0.7,list=F)
training<-bike_weather[train_index,]
testing<-bike_weather[-train_index,]

logreg<-glm(over_500 ~ cum_precipitation+humidity+temp+wind,data=training,family = binomial(link='logit'))
summary(logreg)

pred$prediction<-predict(logreg,newdata=testing,type='response')
pred<-ifelse(pred$prediction<0.5,0,1)

Recall(pred,testing$over_500,positive = '1')
Accuracy(pred,testing$over_500)
Precision(testing$over_500,pred,positive=1)
F1_Score(testing$over_500,pred,positive = 1)

##의사결정나무
library(tree)
set.seed(1234)
bike_weather$over_500<-as.factor(bike_weather$over_500)
levels(bike_weather$over_500)<-c('No','Yes')

train_index<-createDataPartition(bike_weather$Time_out,p=.7,list=F)
training<-bike_weather[train_index,]
testing<-bike_weather[-train_index,]

treemod<-tree(over_500~cum_precipitation+humidity+temp+wind,data=training)
plot(treemod)
text(treemod)

install.packages('e1071')
treepred<-predict(treemod,testing,type='class')
confusionMatrix(treepred,testing$over_500, positive = 'Yes')

##군집분석: K means clustering
weather$scaled_humidity<-scale(weather$humidity)
str(weather)
hist(weather$scaled_humidity,breaks=100)
summary(weather$scaled_humidity)

n_bike<-aggregate(Distance~Gu+Time_out,data=bike_data2,length)
str(n_bike)
n_bike2<-spread(n_bike,key=Time_out,value=Distance)
n_bike2

set.seed(1234)
km2<-kmeans(n_bike2[,2:25],centers=2)
km2
km3<-kmeans(n_bike2[,2:25],centers=3)
km4<-kmeans(n_bike2[,2:25],centers=4)
cl_result<-cbind(n_bike2$Gu,km2$cluster,km3$cluster,km4$cluster)
cl_result

cl_ss<-cbind(n_bike2$Gu,km2$tot.withinss/km2$betweenss,km3$tot.withinss/km3$betweenss,km4$tot.withinss/km4$betweenss)
tibble(cl_ss)
