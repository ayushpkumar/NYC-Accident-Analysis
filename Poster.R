
#NYPD Vehicle Collisions
#Ayush



file<- file.choose()

data<- read.csv(file, header = T, stringsAsFactors = F)
View(data)

library(lubridate)



d<- data[complete.cases(data),]
d$year<- substring(d$DATE,1,4)
d$DATE<- substring(d$DATE,1,10)
d<- d[,-13:-18]
View(d)

str(d)
killing<- d[d$NUMBER.OF.PERSONS.KILLED>0,]
str(killing)
accidents.group.borrough<- tapply(d$NUMBER.OF.PERSONS.INJURED, list(d$year), sum)
injuriesborrough<- tapply(d$NUMBER.OF.PERSONS.INJURED, list(d$BOROUGH), sum)
killedGroup<- tapply(d$NUMBER.OF.PERSONS.KILLED, list(d$year,d$BOROUGH), sum)

#0 Accidents per year
d$year<-as.numeric(d$year)
table(d$year)
hist(d$year, main='Accidents per year (NYC)', xlab = 'Year', border = 1,labels = d$year,breaks = 7,ylim = c(0,250000))

#scatter plot
plot(d$year,d$NUMBER.OF.PERSONS.INJURED)

#1
barplot(killedGroup,beside = T, main = 'Fatal Accidents per Borough throughout the Years in NYC', ylab = 'No of Person Killed', xlab = 'Borough', col = c("red2","red4","cyan","cyan3","yellow","yellow4","grey","grey3"))
legend(x=40,y=50,legend = rownames(killedGroup), fill = c("red2","red4","cyan","cyan3","yellow","yellow4","grey","grey3"), cex = 0.6)

library(ggplot2)


#2
options(scipen = 99)
barplot(injuriesborrough, main = 'Injuries per Borough in NYC',horiz = F,plot = T , ylab = 'No of Person Injured', xlab = 'Boroughs', ylim = c(0,100000), col = c("red3","cyan3","yellow","green3","orange2"))


#horiz
IB<- data.frame(injuriesborrough)
IB$BUROUGH<- row.names(IB)
rownames(IB)<-c()
ggplot(data=IB, aes(x=BUROUGH,y=injuriesborrough)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Injuries per Burrough")


#4
#injuriesborrough<- data.frame(injuriesborrough)
library(plotrix)
pie3D(injuriesborrough,labels = row.names(injuriesborrough),explode=0.1,main = 'Injuries per Borough in NYC',col = c("red3","cyan3","yellow","green3","orange2"))


#3
plot(accidents.group.borrough, type = 'l', ylab='Injuries',xlab='Year',main = "Injuries through last 8 years",lwd=3,col='blue3')
points(accidents.group.borrough, pch=18, col='red3',lwd=2)

str(vehicles)
vehicles<-data.frame(table(d$VEHICLE.TYPE.CODE.3))
accidents5veh<- d[(d$VEHICLE.TYPE.CODE.2!="" &&d$VEHICLE.TYPE.CODE.1!=""),]
View(vehicles)


##################################################
# early morning (12-6) , morning (6-12), afternoon(12-5), evening(5-8), night(8-12)
library(plotrix)

g<- rep("Early Morning", nrow(d))
g[which((d$TIME>=6:00) & (d$TIME < 12:00))] <- "Morning"
g[which((d$TIME>=12:00) & (d$TIME < 17:00))] <- "Afternoon"
g[which((d$TIME>=17:00) & (d$TIME < 20:00))] <- "Evening"
g[which((d$TIME>=20:00) & (d$TIME < 24:00))] <- "Night"
pie(table(g))
View(g)
gt<-table(g)
Category.pct <- gt/sum(gt)
pie3D(table(g),labels = row.names(table(g)),explode=0.1,main = 'Time of Accidents in the city',col = c("red3","cyan3","yellow","green3","orange2"))
dataC<- data.frame(d,g)
dataC<- dataC[,-25]
dataC<- dataC[,-7]
View(InjuredGroupC)


d$g<- rep("Early Morning", nrow(d))
d$g[which((d$TIME>=6:00) & (d$TIME < 12:00))] <- "Morning"
d$g[which((d$TIME>=12:00) & (d$TIME < 17:00))] <- "Afternoon"
d$g[which((d$TIME>=17:00) & (d$TIME < 20:00))] <- "Evening"
d$g[which((d$TIME>=20:00) & (d$TIME < 24:00))] <- "Night"

injuriesHeat<- tapply(d$NUMBER.OF.PERSONS.INJURED, list(d$BOROUGH,d$g), sum)


library(RColorBrewer)
colors <- colorRampPalette(c("blue", "green", "yellow", "red"))
class(injuriesHeat)

p <- ggplot(d, aes(BOROUGH, g)) +
  geom_tile(aes(fill = NUMBER.OF.PERSONS.KILLED), colour = "black") +
  scale_fill_gradientn(colours = rainbow(6, alpha = 1)) +
  xlab("Burrough") + ylab("Time") + labs(fill = "Number of Fatal Injuries")

p



heatmap(as.matrix(injuriesHeat))
library(stringr)
library(lubridate)
library(dplyr)
#KaggleCode

d <- d %>% mutate(DATE_YYYYMMDD = str_extract(d$DATE, "\\d{4}-\\d{2}-\\d{2}"))

d <- d %>% mutate(DATE_YEAR = year(DATE_YYYYMMDD), DATE_MONTH = month(DATE_YYYYMMDD), DATE_DAY = day(DATE_YYYYMMDD))

d %>% filter(DATE_YEAR %in% c('2013','2014','2015','2016','2017','2018')) %>% filter(BOROUGH != "") %>% ggplot() + geom_histogram(aes(BOROUGH), stat="count", color = 'black') + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) + facet_wrap(~ DATE_YEAR, ncol=3) + ggtitle('Motor vehicle collisions in the boroughs of NYC')  + theme(plot.title = element_text(hjust=0.5))


#dev.off()
#Violen

dataSetViolen<- as.data.frame(as.matrix(tapply(d$NUMBER.OF.PERSONS.INJURED, list(d$year), sum)))
dataSetViolen$Year<- row.names(dataSetViolen)
rownames(dataSetViolen)<- c()
str(dataSetViolen)
dataSetViolen %>% 
  ggplot()+
  geom_violin(aes(x=dataSetViolen$Year, y=dataSetViolen$V1, group=Year, fill=dataSetViolen$Year))+scale_fill_gradient(low= "yellow" , high= "green",)


#wordcloud

myPalFun<- colorRampPalette(c("gold","red","green"))
par(mar=c(0,0,0,0), bg='black')

InjuredGroupC<- data.frame(tapply(dataC$NUMBER.OF.PERSONS.KILLED, list(dataC$CONTRIBUTING.FACTOR.VEHICLE.1), sum))

row.names.remove <- c("Unspecified")
#InjuredGroupC[!(row.names(DF) %in% row.names.remove), ]

#InjuredGroupC<- InjuredGroupC[-57,]



str(InjuredGroupC)
colnames(InjuredGroupC)<- c('Freq')
InjuredGroupC1<-InjuredGroupC
InjuredGroupC1$word<- row.names(InjuredGroupC)

library(wordcloud)
png("nyc.png", width=1280,height=800)
wordcloud(row.names(InjuredGroupC),InjuredGroupC$Freq, scale = c(6.55,.5)
          , min.freq = 1, max.words = Inf,
          random.order= F, random.color= F,
          ordered.colors=TRUE, rot.per =.3
         ,colors=myPalFun(length(row.names(InjuredGroupC))))
dev.off()


rownames(InjuredGroupC1)<- c()
InjuredGroupC1$freq<-InjuredGroupC1$Freq
InjuredGroupC1<- InjuredGroupC1[,-1]
View(InjuredGroupC1)

library(wordcloud2)
fig<- png('nyc.png')
wordcloud2(InjuredGroupC1, figPath = "nyc.png", size = 100, color = "skyblue", backgroundColor="black")

killedGroupC<- data.frame(tapply(dataC$NUMBER.OF.PERSONS.KILLED, list(dataC$BOROUGH,dataC$g), sum))


barplot(killedGroupC)



# remove.packages("ggmap")
# remove.packages('tibble')
# 

# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup", force =T)
# library(ggmap)
# library(devtools)
# register_google(key = "AIzaSyCL4ySKljdhUH9fYC_V7yqVLpUHX429mBc")
# ggmap(get_googlemap())
# get_map('new york city', source = 'stamen')
# get_map(c(-97.14667, 31.5493), source = 'stamen')
# location<- geocode(source = 'dsk',"nyc, new york,ny")

# 
# map_nyc<- get_map('New York City', zoom = 12, maptype = 'satellite', api_key = 'AIzaSyCL4ySKljdhUH9fYC_V7yqVLpUHX429mBc')
# 
# nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 11, key='AIzaSyCL4ySKljdhUH9fYC_V7yqVLpUHX429mBc')
# 
# map_sf <- get_map('New York City', zoom = 12, maptype = 'satellite',  api_key = 'AIzaSyCL4ySKljdhUH9fYC_V7yqVLpUHX429mBc')
# 
# ggmap(map_nyc)

library(ggplot2)
p=ggplot(d,aes(y=frequency(NUMBER.OF.PERSONS.INJURED),x=year,size=NUMBER.OF.PERSONS.INJURED,color=NUMBER.OF.PERSONS.KILLED))
p+geom_point()+scale_size_discrete(range = c(4,6))


