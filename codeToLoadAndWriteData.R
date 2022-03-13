OLD <- read.table(file="CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.tsv",sep ="\t",quote="",header=TRUE)
dim(OLD)
a <- OLD[1:100000,]
b <- OLD[100001:200000,]
c <- OLD[200001:300000,]
d <- OLD[300001:400000,]
e <- OLD[400001:500000,]
f <- OLD[500001:600000,]
g <- OLD[600001:700000,]
h <- OLD[700001:800000,]
i <- OLD[800001:900000,]
j <- OLD[900001:1000000,]
k <- OLD[1000001:1088958,]

write.csv(a,"1.csv",row.names=FALSE)
write.csv(b,"2.csv",row.names=FALSE)
write.csv(c,"3.csv",row.names=FALSE)
write.csv(d,"4.csv",row.names=FALSE)
write.csv(e,"5.csv",row.names=FALSE)
write.csv(f,"6.csv",row.names=FALSE)
write.csv(g,"7.csv",row.names=FALSE)
write.csv(h,"8.csv",row.names=FALSE)
write.csv(i,"9.csv",row.names=FALSE)
write.csv(j,"10.csv",row.names=FALSE)
write.csv(k,"11.csv",row.names=FALSE)

stationInfo <- read.table(file="CTA_-_System_Information_-_List_of__L__Stops.tsv",sep ="\t",quote="",header=TRUE)

l <- stationInfo[1:140,]
m <- stationInfo[141:280,]
n <- stationInfo[281:300,]

write.csv(l,"12.csv",row.names=FALSE)
write.csv(m,"13.csv",row.names=FALSE)
write.csv(n,"14.csv",row.names=FALSE)
