rm(list=ls())
library(ggplot2)
library(dplyr)
library(data.table)
library(caret)
library(mice)
library(randomForest)
library(xgboost)
set.seed(0)

#데이터 불러오기
setwd('C:/Users/vz050/OneDrive/바탕 화면/모델링2_프젝')
data<-fread("국민건강보험공단_건강검진정보_20211229.csv")

#데이터 개요 확인
str(data) #100만 행, 31 변수
head(data)

#결측치 확인
colSums(is.na(data)) #여러 변수에서 결측치 상당수 존재

#의미 없는 변수 제거
colnames(data)
data<-data[,-c("기준년도","가입자 일련번호","시도코드","데이터 공개일자")]
colnames(data)%>%length() #27개의 변수로 줄어듬

#여러 변수에서 동시에 결측인 행 33개 제거
data[which(duplicated(data)),]%>%head()
data[which(duplicated(data)),]%>%nrow() #33개 행
data2<-data[-which(duplicated(data)),]

colSums(is.na(data2))
colnames(data2)<-c('성별', '연령대', '신장', '체중', '허리둘레', 
     '시력(좌)', '시력(우)', '청력(좌)', '청력(우)',
     '수축기', '이완기', '공복혈당', 
     '총콜레스테롤', '트리글리세라이드', 'hdl', 'ldl', 
     '혈색소', '요단백', '혈청크레아티닌', 
     'ast', 'alt', 'gpt', 
     '흡연', '음주여부',
     "구강검진 수검여부","충치" ,"치석")

#흡연,총콜레스테롤,트리글리세라이드,hdl에서 
#NA인 행들 제거
data3<-data2[ -which(is.na(data2$총콜레스테롤)|
               is.na(data2$트리글리세라이드)|
               is.na(data2$hdl)|
               is.na(data2$흡연)),  ]
dim(data3)

#충치, 치석, 구강검진 수검여부
colnames(data3)
data4<-data3[,-c("구강검진 수검여부","충치" ,"치석")]

df<-data4
colnames(df)

#흡연 재범주화
df[which(df$흡연==2),"흡연"] = 2
df[which(df$흡연==3),"흡연"] = 2
table(df$흡연)

#연령대
df[which(df$연령대==9),"연령대"] = 40
df[which(df$연령대==10),"연령대"] = 45
df[which(df$연령대==11),"연령대"] = 50
df[which(df$연령대==12),"연령대"] = 55
df[which(df$연령대==13),"연령대"] = 60
df[which(df$연령대==14),"연령대"] = 65
df[which(df$연령대==15),"연령대"] = 70
df[which(df$연령대==16),"연령대"] = 75
df[which(df$연령대==17),"연령대"] = 80
df[which(df$연령대==18),"연령대"] = 85
table(df$연령대)

#요단백
df[ which( df$요단백==2|df$요단백==3|df$요단백==4|
            df$요단백==5|df$요단백==6), "요단백"] = 2
table(df$요단백)

#factor로 변환
df$성별<-as.factor(df$성별)
df$연령대<-as.factor(df$연령대)
df$'청력(좌)'<-as.factor(df$'청력(좌)')
df$'청력(우)'<-as.factor(df$'청력(우)')
df$요단백<-as.factor(df$요단백)
df$흡연<-as.factor(df$흡연)
df$음주여부<-as.factor(df$음주여부)

str(df)

#허리둘레 : 600,999 값
df[which(df$허리둘레==999),] #1개 확인
df[which(df$허리둘레==999),"허리둘레"] = NA #na처리
df[which(df$허리둘레 >600),] #1개 확인
df[which(df$허리둘레 >600),"허리둘레"] = NA #na처리

#시력 : 9.9값
df[which(df$'시력(좌)'==9.9|df$'시력(우)'==9.9),]%>%nrow() #2976개 확인
df[which(df$'시력(좌)'==9.9),'시력(좌)']=NA
df[which(df$'시력(우)'==9.9),'시력(우)']=NA

#혈압 변수 이상치 제거 : 0.1percentile
df <- df[ -which(  (df$수축기>=188) & (df$수축기>=117) ),]

#트리글리세라이드가 800보다 큰 행 제거
df <- df[ -which(  (df$트리글리세라이드>=800)  ),]

#트리글리세라이드가 400미만이면서 ldl이 결측인행은 
#프리드와일드 공식으로 대체
#트리글리세라이드가 400이상 800미만이면서 ldl이 결측인행은 
#샘슨공식으로 대체
df[ which(  (df$트리글리세라이드<400)&is.na(df$ldl)  ),]%>%nrow()
dim(df)



df$ldl2<-df$총콜레스테롤 - df$'hdl'-df$'트리글리세라이드'/5
df$ldl3<-round( df$'트리글리세라이드'/0.948 - df$'hdl'/0.971 
                - ( df$'트리글리세라이드'/8.56 + (( df$'트리글리세라이드'*(df$'총콜레스테롤'-df$'hdl')/2140) - ( (df$'트리글리세라이드')^2)/16100) ) - 9.44
                , 2 )

df$ldl%>%is.na()%>%sum() #6895
idx<-which(  df$트리글리세라이드<400&is.na(df$ldl)  )
for (i in 1:nrow(df)){
  if (  df$트리글리세라이드[i]<400&is.na(df$ldl[i]) ){
    df$ldl[i]<-df$ldl2[i]
  }
}
df$ldl%>%is.na()%>%sum() #6588

idx2<-which(  (df$트리글리세라이드>=400)&(df$트리글리세라이드<800)&(is.na(df$ldl))  )
for (i in 1:nrow(df)){
  if (  (df$트리글리세라이드[i]>=400)&(df$트리글리세라이드[i]<800)&(is.na(df$ldl[i])) ){
    df$ldl[i]<-df$ldl3[i]
  }
}
df$ldl%>%is.na()%>%sum() #0
#0 NA모두가 채워졌음
#음수는 말이 안되므로 처리
df[which(df$ldl<0),]%>%nrow() #16
df<-df[-which(df$ldl<0),]

#호르몬 수치들의 1percentile 기준으로 이상치 처리
df[ -which(  (df$총콜레스테롤>=303) & (df$hdl>=99) & (df$ldl>=352)),]%>%nrow()
df <- df[ -which(  (df$총콜레스테롤>=303) & (df$hdl>=99) & (df$ldl>=352)),]

#공복혈당 수치의 1percentile 기준으로 이상치 처리
df[ -which(  (df$공복혈당>=207) ),]%>%nrow()
df <- df[ -which(  (df$공복혈당>=207) ),]

#혈청크레아티닌 도메인 정보로 2.1보다 큰 값 이상치 처리
df<-df[ -which(  (df$혈청크레아티닌>=2.1) ),]

#gpt가 999인 행 제거
df[which(df$gpt==999),"gpt"]<-NA

#ast alt gpt 도메인 근거해서 200값으로 자르기
which((df$ast>=200)|(df$alt>=200)|(df$gpt>=200))%>%length() #5411
df<-df[-which((df$ast>=200)|(df$alt>=200)|(df$gpt>=200)),]

##########################################################
##결측치처리
##범주형 최빈값, 연속형 mice
df2<-df[,-c("시력(좌)",'시력(우)','청력(좌)','청력(우)','ldl2','ldl3')]
df2%>%is.na()%>%colSums()

#최빈값코드
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df2$요단백 <- ifelse(is.na(df2$요단백), getmode(df2$요단백), df2$요단백)
df2$음주여부 <- ifelse(is.na(df2$음주여부), getmode(df2$음주여부), df2$음주여부)

df2$요단백<-as.factor(df2$요단백)
df2$음주여부<-as.factor(df2$음주여부)

##수치형 변수 mice알고리즘
str(df2)
df2$흡연<-as.numeric(df2$흡연)
dmy <- dummyVars(~., data = df2)
df3 <- data.frame(predict(dmy, newdata = df2))


df4 <- mice(data=df3%>%select(-"흡연"), #데이터프레임아님
                m=3,
                method="pmm",
                seed=0,
                maxit=5)
complete(data=df4,action=1)%>%is.na()%>%sum() #0
df5 <- complete(data=df4,action=1)
df5$흡연<-df3$흡연
###########################################################
##파생변수 생성
#bmi
df5$bmi<-round( df5$체중/((df5$신장/100)^2), 1 )
#복부비만
df5<-df5%>%mutate(복부비만= 
                ifelse( (df5$'성별.1'==1|df5$허리둘레>=85) | (df5$'성별.1'==1|df5$허리둘레>=85),1,0))
table(df5$복부비만)

#혈당수준
df5<-df5%>%mutate(혈당수준=ifelse(df5$공복혈당<70,1,
                         ifelse(df5$공복혈당<100,2,
                                ifelse(df5$공복혈당<125,3,4))))
table(df5$혈당수준)

#혈색소_cat:혈색소 수준
df5<-df5%>%mutate(혈색소_cat= 
                    ifelse( (df5$'성별.1'==1 & df5$혈색소>=17.5) ,'높음',
                            ifelse( (df5$'성별.1'==1 & df5$혈색소>=13.5) ,'정상',
                                    ifelse( (df5$'성별.1'==1 & df5$혈색소>=0) ,'낮음', 
                                            ifelse( (df5$'성별.1'==0 & df5$혈색소>=15.5) ,'높음',
                                                    ifelse( (df5$'성별.1'==0 & df5$혈색소>=12.5) ,'정상','낮음' 
                                                            ))))))
table(df5$혈색소_cat)

df5$복부비만<-as.factor(df5$복부비만)
df5$혈당수준<-as.factor(df5$혈당수준)
df5$혈색소_cat<-as.factor(df5$혈색소_cat)

str(df5)
dmy2 <- dummyVars(~., data = df5)
df6 <- data.frame(predict(dmy2, newdata = df5))
df6$흡연<-as.factor(df6$흡연)
str(df6)
###########################################################
#랜포,xgboost
#변수 선택 및 모델링
df7<-df6 %>% select(-c("신장","체중","허리둘레","ldl","혈색소",'ast'))
str(df7)
##사용하는 변수 종류들##
#'수축기', '이완기', '공복혈당', 
#'총콜레스테롤', '트리글리세라이드', 'hdl', 
#'혈청크레아티닌','alt', 'gpt', 
#'성별', '연령대', '요단백','음주여부',
#'bmi', '복부비만', '혈당수준', '혈색소_cat'

#데이터 분리
set.seed(0)
sub<-sample( nrow(df7),nrow(df7)*0.8)
nrow(df7) #389954
length(sub) #311963



trainvalid<-df7[sub,]
test<-df7[-sub,]
sub2<-sample( nrow(trainvalid),nrow(trainvalid)*0.75)
train<-trainvalid[sub2,]
valid<-trainvalid[-sub2,]
dim(trainvalid)
dim(train)
dim(valid)
dim(test)



#랜덤포레스트 적합
rf.tree=randomForest(흡연~.,data=train,ntree=50)
rf.tree
#성능확인
pred_rf<-predict(rf.tree,valid)
confusionMatrix(table(pred_rf,valid$흡연))
#변수중요도 확인
varImpPlot(rf.tree)

###########################################################
#Xgboost 적합
set.seed(0)
X_train<-data.matrix(train%>%select(-"흡연"))
Y_train<-data.matrix(train%>%select("흡연"))

xgb_train<-xgb.DMatrix(data=X_train,label=Y_train)
list <- list(train=xgb_train)
xgb_fit <- xgb.train(data = xgb_train, 
                     eta=0.05, 
                     max_depth=5, subsample=0.8,colsample_bytree=0.8,
                     nrounds= 50,  # xgb_cv 결과, Best iteration
                     objective= "multi:softprob",  
                     eval_metric= "mlogloss", num_class=3,             
                     watchlist=list,
                     print_every_n = 10
)
X_valid<-data.matrix(valid%>%select(-"흡연"))
Y_valid<-data.matrix(valid%>%select("흡연"))
dim(X_train)
dim(Y_train)
dim(X_valid)
dim(Y_valid)


xgb.pred = predict(xgb_fit,X_valid,reshape=T)
xgb.pred = as.data.frame(xgb.pred)

xgb_pred<-predict(xgb_fit,X_valid)
xgb_pred<-ifelse(xgb_pred >= 0.5, 1, 0)

confusionMatrix(table(xgb_pred,Y_valid))

#변수중요도
xgb.importance(colnames(xgb_train), model = xgb_fit) %>% 
  xgb.plot.importance(top_n = 8)

