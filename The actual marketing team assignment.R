#The ivestment company

mark.csv<-read.csv(file.choose())

str(mark.csv)
summary(mark.csv$FeeType)
# According to the summary on FeeType there're only 2 Feetypes with 28 observations total (out of 3198). Decision to omit.

mark.csv$FeeType<-NULL

#Let's look at CLient ID now.

summary(mark.csv$Client.ID)
hist(mark.csv$Client.ID)
#Treating Client ID as Iinteger is not very informative, converting to string
plot(as.factor(mark.csv$Client.ID))
#Considering that some clients have more transactions than others let's aggregate data by book value
mark.OutstandingSharesbyClient<-aggregate(mark.csv$Book.Value,list(mark.csv$Client.ID),FUN=sum)
plot(mark.OutstandingSharesbyClient)
#Concluding that ClientId is clean.

#LEt's look at Age
hist(mark.csv$Age)
# We see a large portion of Age numbers being 0 ( or unknown), let's investigate.

countofAge0<-length(mark.csv$Age[mark.csv$Age==0])
countAgeTotal<-length(mark.csv$Age)
countAgeTotal
percentof0s<-countofAge0/countAgeTotal
paste(percentof0s*100,"%")


#Figured out that 0 might be a corporation
# 13% of all observations have the age of 0
# A few options to treat it - one is to remove them, another one is to take an average age,
# another one is to use FIML methodology. Suggestion to use FIML method to restore missing data, as lost information volume
# is small and potential to restore it with high degree of quality is big. 

mark.fiml.csv<-mark.csv[mark.csv$Age>0,]
mark.regression<-lm(mark.fiml.csv$Age~mark.fiml.csv$Client.ID+mark.fiml.csv$Advisor+mark.fiml.csv$Dealer)
summary(mark.regression)

# Let's look at Geo.CODE.Province

summary(mark.csv$GEO.CODE.Province.)
hist(mark.csv$GEO.CODE.Province.)

countinProvince1<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==1])
countProvinceTotal<-length(mark.csv$GEO.CODE.Province.)

percentofRegion1<-countinProvince1*100/countProvinceTotal
percentofRegion1

#Over half of the observations are concentrated in Ontario. We are biased towards Ontario.. 
# Can we be sure that we can use results for any province? Let's check 
countinProvince2<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==2])
percentofRegion2<-countinProvince2*100/countProvinceTotal

countinProvince3<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==3])
percentofRegion3<-countinProvince3*100/countProvinceTotal

countinProvince4<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==4])
percentofRegion4<-countinProvince4*100/countProvinceTotal

countinProvince5<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==5])
percentofRegion5<-countinProvince5*100/countProvinceTotal

countinProvince6<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==6])
percentofRegion6<-countinProvince6*100/countProvinceTotal


countinProvince7<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==7])
percentofRegion7<-countinProvince7*100/countProvinceTotal

countinProvince8<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==8])
percentofRegion8<-countinProvince8*100/countProvinceTotal

countinProvince9<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==9])
percentofRegion9<-countinProvince9*100/countProvinceTotal


countinProvince10<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==10])
percentofRegion10<-countinProvince10*100/countProvinceTotal

countinProvince11<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==11])
percentofRegion11<-countinProvince11*100/countProvinceTotal

countinProvince12<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==12])
percentofRegion12<-countinProvince12*100/countProvinceTotal

countinProvince13<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==13])
percentofRegion13<-countinProvince13*100/countProvinceTotal



percentofRegion1
percentofRegion2
percentofRegion3
percentofRegion4
percentofRegion5
percentofRegion6
percentofRegion7
percentofRegion8
percentofRegion9
percentofRegion10
percentofRegion11
percentofRegion12
percentofRegion13

#Ontario, Alberta, Quebec and BC have a ok volume of transactions. VOlume is still significant enough to use all geo data.

#What is left?

countinProvinceOther<-length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.>13])
percentofRegionOther<-countinProvinceOther*100/countProvinceTotal

percentofRegionOther

# As the "Other" regions are outside of Canada and with unknown precise location, also having only 1.6% of total observation 
# it is reasonable to omit this data from analysis.

mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.>13]<-""

# Let's look at Gender

summary(mark.csv$Acm.Shm.Sex)
plot(mark.csv$Acm.Shm.Sex)

countUnknown<-length(mark.csv$Acm.Shm.Sex[mark.csv$Acm.Shm.Sex==""])
countUnknown/countProvinceTotal

#78.8% of the Gender dataset is unknown this proportion is too high to use FIML algorithm. 
#Initial suggestion is to remove the variable, though we should first run corerlation analysis
#for correlation analysis we need to re-code gender as 0 for unknown, 1 for female, 2 for male.
mark.csv$GenderRecoded<-mark.csv$Acm.Shm.Sex
mark.csv$GenderRecoded<-as.character(mark.csv$GenderRecoded)


mark.csv$GenderRecoded[mark.csv$GenderRecoded=="F"]<-"1"
mark.csv$GenderRecoded[mark.csv$GenderRecoded=="M"]<-"2"
mark.csv$GenderRecoded[mark.csv$GenderRecoded==""]<-"0"

mark.csv$GenderRecoded<-as.numeric(mark.csv$GenderRecoded)

summary(mark.csv$GenderRecoded)
str(mark.csv)

library(corrplot)
correlat<-cor(mark.csv[,c(1,2,5,6,7,8,11,13,14,15,16)])
corrplot(correlat, method="number")

#There're no clear correaltions except the ones expected. Gender doesn't seem to be influencing any other variables.
#Just to make sure that there're no non-linear relationships we plot Gender VS Age and Book Value 

plot(mark.csv$GenderRecoded,mark.csv$Age)
plot(mark.csv$GenderRecoded,mark.csv$Minor)
plot(mark.csv$GenderRecoded,mark.csv$Book.Value)

#Conclusion - geneder is not a valuable variable, at least considering the dataset provided, and can be omitted alltogether.

mark.csv$Acm.Shm.Sex<-NULL
mark.csv$GenderRecoded<-NULL

str(mark.csv)

# Let's look at Dealer

summary(mark.csv$Dealer)
#Observation - dealers do not have a uniform 4 digit format, min. is 45) 
hist(mark.csv$Dealer)
#Dealer numbers are split by numbers between 45 and 1000 and 2000+. Strange and require investigation.
#Dealer numbers should be full not substituted -working with the new dataset (Naida provided)

# Let's look at Advisor

summary(mark.csv$Advisor)
hist(mark.csv$Advisor)

# Let's look at Minor

summary(mark.csv$Minor)
hist(mark.csv$Minor)
head(mark.csv$Minor[mark.csv$Minor==100])
mark.csv$Minor[mark.csv$Minor==100]<-1

#One instance of 100, we established that it's an entry error and it should be 1. Corrected

# Let's look at Fund

summary(mark.csv[,7])
plot(mark.csv$Fund..)
hist(mark.csv$Fund..)

#One fund is 13,000 - clear entry error. It's only one instance. Nadia will check.
#There's a gap between fund #s 5000 to 6000. Will require additional investigation.

# Let's look at Type of Series

summary(mark.csv$Type.of.series.for.each.fund)
#We're heavily biased toward funds O, Y, J, A


# Let's look at Load

summary(mark.csv$Load)
labels(c("LL3","RCS","SCS"))
plot(mark.csv$Load)

# the largest amount of observations are in SCS load bucket

# Let's look at Portfoli Xref

summary(mark.csv$Portfolio.Xref.Fund.Id)
plot(as.factor(mark.csv$Portfolio.Xref.Fund.Id))

# number of events for fund IDs at the end of the spectrum is supiciouosly low. It is possible that it has something to do with new accounts/funds.
# No concern.

# Let's look at Fund.Name

summary(mark.csv$Fund.Name)
plot(mark.csv$Fund.Name)
#No concern, all in order

#Let's look at Shares.Owned

summary(mark.csv$Shares.Owned)
length(mark.csv$Shares.Owned[mark.csv$Shares.Owned<2])
length(mark.csv$Shares.Owned[mark.csv$Shares.Owned<100])

hist(mark.csv$Shares.Owned,breaks=200)
plot(aggregate(mark.csv$Shares.Owned,list(as.factor(mark.csv$Client.ID)),FUN=sum))


#167 observations have very low share volume ( below 100). Decision is to keep them in the data set as they might be indicators of consumer behaviour.

#everything seems to be fine with shares owned


# Let's look at Book.Value

summary(mark.csv$Book.Value)

#to confirm that small Book value is with small share volume

plot(mark.csv$Book.Value,mark.csv$Shares.Owned)

#No misused data - all makes sense.

# Let's look at Mkt.Value

summary(mark.csv$Mkt.Value)
plot(mark.csv$Mkt.Value,mark.csv$Shares.Owned)
#No misused data - all makes sense.


#After all other transformations are done we need to omit al nulls


#test ho many should be removed
str(mark.csv$GEO.CODE.Province.)
mark.csv$GEO.CODE.Province.<-as.numeric(mark.csv$GEO.CODE.Province.)
length(mark.csv$GEO.CODE.Province.[mark.csv$GEO.CODE.Province.==""])

mark.csv<-na.omit(mark.csv)

#Data cleanup is done!!!

mark.regression<-lm(mark.csv$Age~mark.csv$GEO.CODE.Province.+mark.csv$Dealer+mark.csv$Advisor+mark.csv$Minor+mark.csv$Fund..+mark.csv$Load+mark.csv$Portfolio.Xref.Fund.Id)
summary(mark.regression)

#Taking delaer out
mark.regression<-lm(mark.csv$Age~mark.csv$GEO.CODE.Province.+mark.csv$Advisor+mark.csv$Minor+mark.csv$Fund..+mark.csv$Load+mark.csv$Portfolio.Xref.Fund.Id)
summary(mark.regression)

#Converting data into appropriate format (WRONG)
str(mark.csv)
mark.csv$Age<-as.factor(mark.csv$Age)
mark.csv$GEO.CODE.Province.<-as.factor(mark.csv$GEO.CODE.Province.)
mark.csv$Dealer<-as.factor(mark.csv$Dealer)
mark.csv$Advisor<-as.factor(mark.csv$Advisor)
mark.csv$Minor<-as.factor(mark.csv$Minor)
mark.csv$Fund..<-as.factor(mark.csv$Fund..)

#to avoid double input remove fund name
mark.cluster<-mark.csv
mark.cluster$Fund.Name<-NULL
mark.cluster$Portfolio.Xref.Fund.Id<-NULL

str(mark.cluster)


#Converting data into appropriate format (RIGHT)
str(mark.cluster)
mark.cluster$Client.ID<-NULL

str(mark.cluster)


#As correlation is above .95% for Shares owned, book value and market value
mark.cluster$Shares.Owned<-NULL
mark.cluster$Mkt.Value<-NULL

#clustering analysis

#check for NAs

nafinder<-(is.na(mark.cluster))
summary(nafinder)

# Ward Hierarchical Clustering
d <- dist(mark.cluster[,c(1,2,3,4,5,6,9)],method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters# Ward Hierarchical Clustering with Bootstrapped p values

#Bootstraping

library(pvclust)
fit <- pvclust(mark.cluster[,c(1,2,3,4,5,6,9)], method.hclust="ward.D", method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

# Model Based Clustering
library(mclust)
fit2 <- Mclust(mark.cluster[,c(1,2,3,4,5,6,9)])
plot(fit2) # plot results 
summary(fit2) # display the best model

str(fit2)

mark.cluster$Segment<-fit2$classification
head(mark.cluster)

str(mark.cluster)
mark.cluster$Age<-as.numeric(mark.cluster$Age)
mark.cluster$GEO.CODE.Province.<-as.numeric(mark.cluster$GEO.CODE.Province.)
mark.cluster$Dealer<-as.numeric(mark.cluster$Dealer)
mark.cluster$Advisor<-as.numeric(mark.cluster$Advisor)
mark.cluster$Minor<-as.numeric(mark.cluster$Minor)
mark.cluster$Fund..<-as.numeric(mark.cluster$Fund..)

#summary of segments
aggregate(mark.cluster,by=list(mark.cluster$Segment),mean)
suspectSegment4<-mark.cluster[mark.cluster$Segment==4,]

SuspectSegments<-mark.cluster
SuspectSegments$Advisor<-mark.csv$Advisor

suspectSegment4<-SuspectSegments[SuspectSegments$Segment==4,]
suspectSegment4

plot(as.factor(SuspectSegments$Segment))

segmentTotals<-(aggregate(SuspectSegments$Book.Value,list(SuspectSegments$Segment), FUN=sum))
percentOfSegment2<-segmentTotals[2]/sum(segmentTotals)
percentOfSegment2

suspectSegment2<-SuspectSegments[SuspectSegments$Segment==2,]
suspectSegment2
