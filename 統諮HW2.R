library(dplyr)
library(ggplot2)
library(rgdal)
library(sf)
library(RColorBrewer)
library(ggrepel)
library(showtext)
library(maptools)
library(mapproj)
library(knitr)
library(Rmisc)
library(lmtest)


Taiwan<-st_read("~/Desktop/mapdata201907311023/VILLAGE_MOI_1080726.shp",layer = "VILLAGE_MOI_1080726")
data<-read.csv("~/Desktop/統計諮詢/SM and CJ Dengue.csv",head=T)
data.1<-read.csv("~/Desktop/統計諮詢/mosqSM01.csv",head=T)
data.1$date<-as.Date(data.1$date)

SMdata<-filter(data,data$居住鄉鎮=="三民區")
count<-rep(NA,length(unique(SMdata$居住村里)))
male<-rep(NA,length(unique(SMdata$居住村里)))
female<-rep(NA,length(unique(SMdata$居住村里)))
village<-rep(NA,length(unique(SMdata$居住村里)))
for(i in 1:length(unique(SMdata$居住村里))){
  list<-subset(SMdata,居住村里==unique(SMdata$居住村里)[i],select="性別")
  count[i]<-nrow(list)
  village[i]<-unique(SMdata$居住村里)[i]
  male[i]<-length(which(list=="男"))
  female[i]<-length(which(list=="女"))
  total<-data.frame(village,count,male,female)
}
colnames(total)<-c("VILLNAME","總人數","男性人數","女性人數")

#長條圖
p<-ggplot(data = total[-4,],aes(x = reorder(VILLNAME,總人數),y = 總人數))
p<-p+geom_bar(stat="identity",fill = "blue") + coord_flip()
p<-p+ xlab("village")+ylab("Confirmed numbers")+theme(text=element_text(family="黑體-繁 中黑"))
p<-p+theme(text = element_text(size=7.5))
##p

#分佈圖
Kao<-Taiwan[Taiwan$COUNTYNAME=="高雄市",]
SM<-Kao[Kao$TOWNNAME=="三民區",]
total$VILLNAME<-as.character(total$VILLNAME)
my.SM.data<-SM[c("VILLNAME","geometry")]
my.SM.data$VILLNAME<-as.character(my.SM.data$VILLNAME)

my.new.SM.data<-merge(my.SM.data,total,by="VILLNAME")
map<-ggplot(data = my.new.SM.data)+geom_sf(aes(fill = 總人數))+
     scale_fill_gradient(low="lightgreen",high="darkgreen",name="總人數")+
     labs(title="高雄市三民區各里通報人數分布圖",fill="人數")+
     theme(text=element_text(family="黑體-繁 中黑"))
##map

femalemap<-ggplot(data = my.new.SM.data)+geom_sf(aes(fill = 女性人數))+
           scale_fill_gradient(low="coral1",high="darkred",name="女性人數")+
           labs(title="高雄市三民區各里通報女性人數分布圖",fill="人數")+
           theme(text=element_text(family="黑體-繁 中黑"))

malemap<-ggplot(data = my.new.SM.data)+geom_sf(aes(fill = 男性人數))+
         scale_fill_gradient(low="lightskyblue",high="darkblue",name="男性人數")+
         labs(title="高雄市三民區各里通報男性人數分布圖",fill="人數")+
         theme(text=element_text(family="黑體-繁 中黑"))

multiplot(malemap,femalemap,cols=1)


SMdata$確定病名<-gsub("登革熱","1",SMdata$確定病名)
SMdata$確定病名<-gsub("","0",SMdata$確定病名)
SMdata$確定病名<-gsub("010","1",SMdata$確定病名)


season<-rep(NA,nrow(SMdata)) 

for(i in 1:nrow(SMdata))
{
  if(as.numeric(substr(SMdata$發病年月[i],nchar(SMdata$發病年月[i])-1,nchar(SMdata$發病年月[i])))>=3&&as.numeric(substr(SMdata$發病年月[i],nchar(SMdata$發病年月[i])-1,nchar(SMdata$發病年月[i])))<=5){
    season[i]<-"spring"
  }else if(as.numeric(substr(SMdata$發病年月[i],nchar(SMdata$發病年月[i])-1,nchar(SMdata$發病年月[i])))>=6&&as.numeric(substr(SMdata$發病年月[i],nchar(SMdata$發病年月[i])-1,nchar(SMdata$發病年月[i])))<=8){
    season[i]<-"summer"
  }else if(as.numeric(substr(SMdata$發病年月[i],nchar(SMdata$發病年月[i])-1,nchar(SMdata$發病年月[i])))>=9&&as.numeric(substr(SMdata$發病年月[i],nchar(SMdata$發病年月[i])-1,nchar(SMdata$發病年月[i])))<=11){
    season[i]<-"autumn"
  }else{
    season[i]<-"winter"
  }
}
SMdata$season<-season

SMdata$year<-SMdata$發病日期.民國.yyy.+1911
SMdata$age<-SMdata$發病年齡.足歲.
SMdata$確定病名<-as.numeric(SMdata$確定病名)
SMdata$gender<-SMdata$性別

SMdata<-SMdata[-c(which(SMdata$居住村里=="三民區")),]

a<-glm(確定病名~age+year+factor(season)+factor(gender),data=SMdata,family=binomial(link="logit"))
summary(a)

#null<-glm(確定病名 ~ 1,data=SMdata[,c(24,35:38)],family=binomial(link="logit"))
#full<-glm(確定病名 ~ age+year+factor(season)+factor(gender),data=SMdata[,c(24,35:38)],family=binomial(link="logit"))
#backward.glm<-step(full, scope = list(lower=null,upper=full), direction="backward",test="F")
#forward.glm<-step(null, scope = list(lower=null,upper=full), direction="forward",test="F")
#stepwise.glm<-step(full,direction="both",test="F")   
##根據結果可以知道年齡、性別、季節及年份都有相當的顯著。



##以週為標準
SMdata_new<-filter(SMdata,SMdata$year%in%c("2005","2006","2007","2008","2009","2010","2011","2012"))
SMdata_new$值<-as.numeric(SMdata_new$值) 
SMdata_new$值.1<-as.numeric(SMdata_new$值.1) 

SMorder<-SMdata_new[order(SMdata_new$值.1, SMdata_new$year, SMdata_new$周ISO.8601),]
mosorder<-data.1[order(data.1$LiNo, data.1$year, data.1$ISO),]

num = rep(0,nrow(mosorder))
for (i in (1:nrow(mosorder))) {
  for (j in (1:nrow(SMorder))) {
    if(SMorder$值.1[j] == mosorder$LiNo[i]){
      if(SMorder$year[j] == mosorder$year[i]){
        if(SMorder$周ISO.8601[j] <= mosorder$ISO[i] &&SMorder$周ISO.8601[j]>mosorder$ISO[i-1]){
          num[i] = num[i]+1
        }else{num[i] =  num[i]+0}
      }else{num[i] =  num[i]+0}
    }else{num[i] =  num[i]+0}
  }
}
mosorder$count<-num
rownames(mosorder)<-c(1:nrow(mosorder))

season1<-rep(NA,nrow(mosorder)) 

for(i in 1:nrow(mosorder))
{
  if(as.numeric(substr(mosorder$date[i],6,7))>=3&&as.numeric(substr(mosorder$date[i],6,7))<=5){
    season1[i]<-"spring"
  }else if(as.numeric(substr(mosorder$date[i],6,7))>=6&&as.numeric(substr(mosorder$date[i],6,7))<=8){
    season1[i]<-"summer"
  }else if(as.numeric(substr(mosorder$date[i],6,7))>=9&&as.numeric(substr(mosorder$date[i],6,7))<=11){
    season1[i]<-"autumn"
  }else{
    season1[i]<-"winter"
  }
}
mosorder$season<-season1


model<-lm(data=mosorder,count~Breteau_index+Aeg_aed_index+Res_index+Contain_index+Larvae_index+year+factor(season))
summary(model)

#pairs(data=mosorder,count~Breteau_index+Aeg_aed_index+Res_index+Contain_index+Larvae_index+year+factor(season))
#cor(mosorder[,c(6:10,13)])

null<-lm(count ~ 1, data = mosorder[,c(1,6:10,13,14)])  
full<-lm(count~ ., data = mosorder[,c(1,6:10,13,14)])
backward.lm<-step(full, scope = list(lower=null,upper=full), direction="backward",test="F")
forward.lm<-step(null, scope = list(lower=null,upper=full), direction="forward",test="F")
stepwise<-step(full,direction="both",test="F")   


model.1<-lm(data=mosorder,count ~ year + Contain_index + factor(season))
summary(model.1)
##根據結果可以知道容器指數、季節及年份都有相當的顯著。


##以年為標準
SMorder$值<-as.numeric(SMorder$值)
a<-matrix(0,8,86)
for(i in c(2005:2012)){
  for( j in c(1:86)){
    a[i-2004,j]<-nrow(subset(SMorder,year==paste(i)&值.1==paste(j),select="值"))
  }}

average1<-matrix(0,8,86)
for(i in c(2005:2012)){
  for( j in c(1:86)){
    data<-subset(mosorder,year==paste(i)&LiNo==paste(j),select="Breteau_index")
average1[i-2004,j]<-sum(data)/nrow(data)}}

average2<-matrix(0,8,86)
for(i in c(2005:2012)){
  for( j in c(1:86)){
    data<-subset(mosorder,year==paste(i)&LiNo==paste(j),select="Aeg_aed_index")
    average2[i-2004,j]<-sum(data)/nrow(data)}}

average3<-matrix(0,8,86)
for(i in c(2005:2012)){
  for( j in c(1:86)){
    data<-subset(mosorder,year==paste(i)&LiNo==paste(j),select="Res_index")
    average3[i-2004,j]<-sum(data)/nrow(data)}}

average4<-matrix(0,8,86)
for(i in c(2005:2012)){
  for( j in c(1:86)){
    data<-subset(mosorder,year==paste(i)&LiNo==paste(j),select="Contain_index")
    average4[i-2004,j]<-sum(data)/nrow(data)}}

average5<-matrix(0,8,86)
for(i in c(2005:2012)){
  for( j in c(1:86)){
    data<-subset(mosorder,year==paste(i)&LiNo==paste(j),select="Larvae_index")
    average5[i-2004,j]<-sum(data)/nrow(data)}}


v1 = c()
for (i in 1:8){v1 = c(v1,average1[i,])}

v2 = c()
for (i in 1:8){v2 = c(v2,average2[i,])}

v3 = c()
for (i in 1:8){v3 = c(v3,average3[i,])}

v4 = c()
for (i in 1:8){v4 = c(v4,average4[i,])}

v5 = c()
for (i in 1:8){v5 = c(v5,average5[i,])}

v6 = c()
for (i in 1:8){v6 = c(v6,a[i,])}

newdata<-data.frame(year=c(rep(2005,86),rep(2006,86),rep(2007,86),rep(2008,86),rep(2009,86),rep(2010,86),rep(2011,86),rep(2012,86)),
                    Li=c(1:86),
                    cbind(v1,v2,v3,v4,v5,v6))
colnames(newdata)[c(3:8)]<-c("Breteau_index","Aeg_aed_index","Res_index","Contain_index","Larvae_index","count")

m<-lm(data=newdata,count~Breteau_index+Aeg_aed_index+Res_index+Contain_index+Larvae_index+year)
summary(m)
anova(m)

vif(m)
null.1<-lm(count ~ 1, data = newdata)  
full.1<-lm(count~ ., data = newdata)
backward.lm<-step(full.1, scope = list(lower=null.1,upper=full.1), direction="backward",test="F")
forward.lm<-step(null.1, scope = list(lower=null.1,upper=full.1), direction="forward",test="F")
stepwise<-step(full.1,direction="both",test="F")   
m.1<-lm(data=newdata,count~Res_index+Contain_index+year)
summary(m.1)


SMorder$居住村里<-gsub("港北里","博愛里",SMorder$居住村里)
li<-c()
for(i in c(1:86)){
li[i]<-SMorder$居住村里[which(SMorder$值.1==i)[1]]
}

newdata.1<-cbind(newdata,rep(li,8))
colnames(newdata.1)[9]<-"VILLNAME"

new.SM.data<-merge(my.SM.data,newdata.1,by="VILLNAME")



p1<-ggplot(data=filter(new.SM.data,new.SM.data$year=="2005"))+geom_sf(aes(fill=count))+
    scale_fill_gradient(low="lightgreen",high="darkgreen",name="總人數")+
    labs(title="2005年高雄市三民區各里通報人數")+
    theme(text=element_text(family="黑體-繁 中黑"))


p2<-ggplot(data=filter(new.SM.data,new.SM.data$year=="2006"))+geom_sf(aes(fill=count))+
    scale_fill_gradient(low="lightgreen",high="darkgreen",name="總人數")+
    labs(title="2006年高雄市三民區各里通報人數")+
    theme(text=element_text(family="黑體-繁 中黑"))


p3<-ggplot(data=filter(new.SM.data,new.SM.data$year=="2007"))+geom_sf(aes(fill=count))+
    scale_fill_gradient(low="lightgreen",high="darkgreen",name="總人數")+
    labs(title="2007年高雄市三民區各里通報人數")+
    theme(text=element_text(family="黑體-繁 中黑"))


p4<-ggplot(data=filter(new.SM.data,new.SM.data$year=="2008"))+geom_sf(aes(fill=count))+
    scale_fill_gradient(low="lightgreen",high="darkgreen",name="總人數")+
    labs(title="2008年高雄市三民區各里通報人數")+
    theme(text=element_text(family="黑體-繁 中黑"))


p5<-ggplot(data=filter(new.SM.data,new.SM.data$year=="2009"))+geom_sf(aes(fill=count))+
    scale_fill_gradient(low="lightgreen",high="darkgreen",name="總人數")+
    labs(title="2009年高雄市三民區各里通報人數")+
    theme(text=element_text(family="黑體-繁 中黑"))


p6<-ggplot(data=filter(new.SM.data,new.SM.data$year=="2010"))+geom_sf(aes(fill=count))+
    scale_fill_gradient(low="lightgreen",high="darkgreen",name="總人數")+
    labs(title="2010年高雄市三民區各里通報人數")+
    theme(text=element_text(family="黑體-繁 中黑"))


p7<-ggplot(data=filter(new.SM.data,new.SM.data$year=="2011"))+geom_sf(aes(fill=count))+
  scale_fill_gradient(low="lightgreen",high="darkgreen",name="總人數")+
  labs(title="2011年高雄市三民區各里通報人數")+
  theme(text=element_text(family="黑體-繁 中黑"))


p8<-ggplot(data=filter(new.SM.data,new.SM.data$year=="2012"))+geom_sf(aes(fill=count))+
    scale_fill_gradient(low="lightgreen",high="darkgreen",name="總人數")+
    labs(title="2012年高雄市三民區各里通報人數")+
    theme(text=element_text(family="黑體-繁 中黑"))


multiplot(p1,p5,p2,p6,p3,p7,p4,p8,cols=4)


tol<-c()
for(i in 2005:2012){
   tol[i-2004]<-sum(filter(new.SM.data,new.SM.data$year==paste(i))$count)}

s<-data.frame("年份"=c("2005","2006","2007","2008","2009","2010","2011","2012"),
              "累積人數"=tol)

ggplot()+geom_bar(data=s,aes(x=年份,y=累積人數),stat="identity",width=0.5,fill="blue")+
         theme(text=element_text(family="黑體-繁 中黑"))+
         labs(title=("三民區各年通報累積人數"))+
         geom_text(data=s,aes(x=年份,y=累積人數,label=累積人數,vjust =-1))


age_stage<-c()
for(i in c(1:nrow(SMdata))){
  if(SMdata$age[i]<=18){
    age_stage[i]<-1
  }else if(SMdata$age[i]>18&&SMdata$age[i]<=65){
    age_stage[i]<-2
  }else{
    age_stage[i]<-3
  }
}
SMdata$age_stage<-age_stage

SMdata1<-filter(SMdata,SMdata$確定病名%in%c("1"))
SMdata0<-filter(SMdata,SMdata$確定病名%in%c("0"))
table(SMdata1$age_stage)
table(SMdata$age_stage)

w<-data.frame("人數"=c(166,983,188,361,1821,325),
              "是否確診"=c("確診","確診","確診","通報","通報","通報"),
              "年齡"=c("0~18歲","19~65歲","66歲以上"))
w$是否確診<-factor(w$是否確,levels=c("通報","確診"))


ggplot()+geom_bar(data=w,aes(x=年齡,y=人數,fill=是否確診),stat="identity",width=0.5,position="dodge")+
         theme(text=element_text(family="黑體-繁 中黑"))+
         labs(fill=" ")
         

cor.test(newdata$Res_index,newdata$Contain_index,alternative=c("two.sided"))

