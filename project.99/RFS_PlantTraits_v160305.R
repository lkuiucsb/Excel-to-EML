#An example of a R code came with dataset 
##################################################################################

data_indi<-read.csv("plant_all.csv") %>%
	filter(density=="individual")
indi1<-data_indi[2:23,c(7,19)]


indi2$bins <- cut(indi1$bucket.location, breaks=c(12.5,15,17.5,20,22.5,25), labels=c("1","2","3","4","5"))
indi3<-indi2[with(indi2, order(bins,status)), ]
indi_size<-c(1,0.8,0.5,0.4,0.3)
indi_dl<-c(13.75,16.25,18.75,21.25,23.75)

par(mar=c(4,0.1,0.1,0.1))
plot(1, type="n", xlim=c(13.5,26.5),ylim=c(6.5,0.5),las=1,xlab="Downstream distance (m)", ylab="", yaxt='n')


points(x=indi_dl,y=rep(1,5),type="p",cex=indi_size*2)
points(x=indi_dl,y=rep(1,5),type="p",pch=3)

points(x=patch_dl$location,y=patch_dl$run-4,type="p",cex=patch_dl$count*2)
points(x=patch_rm$location_m,y=patch_rm$run-4,type="p",pch=3)

text(25.5,1,"individual mix")
text(24.5,2,"short sparse CW")
text(24.5,3,"tall sparse CW")
text(24.5,4,"short sparse TM")
text(24.5,5,"tall sparse TM")
text(24.5,6,"short dense CW")

