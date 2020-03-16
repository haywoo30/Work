
airline <- readRDS("SmallAirlineData.rds")
#1a
mean(airline$ArrDelay)
#1b
mean(airline$ArrDelay[airline$Year == 2002])
mean(airline$ArrDelay[airline$Year == 2003])
mean(airline$ArrDelay[airline$Year == 2004])
#1c
mean(airline$Distance)
#1d
mean(airline$distance[airline$year == 2002])
mean(airline$distance[airline$year == 2003])
mean(airline$distance[airline$year == 2004])

#2a
sum(is.na(airline$Deptime))
sum(is.na(airline$Deptime)) / length(airline$Deptime)
#2b
60 * 24 #Possible departure times
length(unique(airline$Deptime))
#2c
length(unique(airline$DepTime))
#Yes, there are more times in the data set than possible minutes within 24 hours.
#2d
airline_valid <- airline$DepTime[airline$DepTime < 2400 & airline$DepTime > 0]
length(airline_valid)

#3a
for (i in seq(0,23,1)){
  cat(length(airline_valid$DepTime[airline$DepTime > 100*i & airline$DepTime < 100*i+101]))
}
#3b
index = cut(airline$ArrTime,breaks=c(0,600,1200,1800,2400),labels=c("Early","Morning","Afternoon","Evening"))
tapply(airline$ArrTime,INDEX= index,FUN=mean)
#Lowest is Early Morning

#3c
tapply(airline$ArrTime,INDEX= index,FUN=sd)
#3d
sd(airline$ArrTime[airline$ArrTime >= 0 & airline$ArrTime < 600],na.rm=TRUE)
sd(airline$ArrTime[airline$ArrTime > 600 & airline$ArrTime < 1200],na.rm=TRUE) 
sd(airline$ArrTime[airline$ArrTime > 1200 & airline$ArrTime < 1800 ],na.rm=TRUE) 
sd(airline$ArrTime[airline$ArrTime > 1800 & airlineArrTime < 2400],na.rm=TRUE)


#4a
tail(names(sort(table(airline$Origin))), 10)
#4b
tail(names(sort(table(airline$Dest))), 10)

#5a
max = 0
for (i in sort(unique(airline$DayOfWeek))){
  if (mean(airline$ArrDelay[airline$DayOfWeek == i],na.rm=TRUE) > max){
    max = i
  }
}
print(max)
#5b
for (i in sort(unique(airline$DayOfWeek))){
  print(length(airline$DayOfWeek[airline$DayOfWeek == i])/length(airline$DayOfWeek))
}
#5c
tapply(airline$DepTime,INDEX = index,FUN=length)[1]/length(airline$DepTime)
tapply(airline$DepTime,INDEX = index,FUN=length)[2]/length(airline$DepTime)
tapply(airline$DepTime,INDEX = index,FUN=length)[3]/length(airline$DepTime)
tapply(airline$DepTime,INDEX = index,FUN=length)[4]/length(airline$DepTime)
#5d
mins = c()
for (i in sort(unique(airline$DayOfWeek))){
  a = mean(airline$ArrDelay[airline$DayOfWeek == i & airline$DepTime >= 0 & airline$DepTime <= 600],na.rm=TRUE)
  b = mean(airline$ArrDelay[airline$DayOfWeek == i & airline$DepTime > 600 & airline$DepTime <= 1200],na.rm=TRUE)
  c = mean(airline$ArrDelay[airline$DayOfWeek == i & airline$DepTime > 1200 & airline$DepTime <= 1800],na.rm=TRUE)
  d = mean(airline$ArrDelay[airline$DayOfWeek == i & airline$DepTime > 1800 & airline$DepTime < 2400],na.rm=TRUE)
  
  cat("\nEarly morning", i, ":", a)
  cat("\nMorning", i, ":", b)
  cat("\nAfternoon", i, ":", c)
  cat("\nEvening", i, ":", d)
  
  mins <- c(mins, c(a,b,c,d))
}
min(mins)
match(min(mins),mins)
mins[match(min(mins),mins)]

#6a
airline_cancelled <- subset(airline,airline$Cancelled > 0)
table(airline_cancelled$Year,airline_cancelled$Month)
#There is a significant difference between 2001 and 2002, most likely due to 9/11. 

