setwd("~/Dropbox/Kui_VegemorfPhD_shared/KuiEtAl_RFS_manuscript/Rcode_JCS")
##################################################################################
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{	usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y,use="pairwise.complete.obs"))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    test <- cor.test(x,y)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
    text(0.5, 0.5, txt, cex = max(cex * r,1.25))
    text(.8, .8, Signif, cex=cex, col=2)
}

##################################################################################
#Read in project data on plant structure; N = 90 plants used to study morphology
#Calculate frontal area totals for roots and shoots, as well as crown densityt positions on the plants

fronarea <- read.csv("plant_structure.csv")
#aboveground part
FA_shoot<-fronarea[58:176]
MaxFA_shoot<-0
LocMaxFA_shoot<-0
TotHeight_shoot<-0
for (i in 1:90){
     MaxFA_shoot[i]<-FA_shoot[i,1]
     LocMaxFA_shoot[i]<-15
     TotHeight_shoot[i]<-15
     for (j in 2:119){
         if (FA_shoot[i,j]>=MaxFA_shoot[i]) {
            MaxFA_shoot[i]<-FA_shoot[i,j]
            LocMaxFA_shoot[i]<-j*10-5
            }
         if (FA_shoot[i,j]>0) {
             TotHeight_shoot[i]<-j*10-5
          }
         }
    }

#belowground part
FA_root<-fronarea[25:57]
MaxFA_root<-0
LocMaxFA_root<-0
TotDepth_root<-0
for (i in 1:90){
     MaxFA_root[i]<-FA_root[i,1]
     LocMaxFA_root[i]<--325
     TotDepth_root[i]<-325
     for (j in 2:33){
         if (FA_root[i,j]>=MaxFA_root[i]) {
            MaxFA_root[i]<-FA_root[i,j]
            LocMaxFA_root[i]<--325+(j-1)*10
            }
         if (FA_root[i,j]==0) {
             TotDepth_root[i]<-325-j*10
          }
         }
    }

#species<-as.factor(ifelse(fronarea$species==1,"cottonwood","tamarisk"))
root_weight<-rowSums(fronarea[,16:20],na.rm=T)

newfron<-data.frame(cbind(
	"Species"= fronarea$species,
	"Height"=fronarea$hei_cm,
	"Diameter"=fronarea$dia_mm, 
	"Shoot_weight"=fronarea$aboveground_g,
	"Bend_force"=fronarea$bend_fce_g,
	"MaxFA_shoot"=MaxFA_shoot,
#	"LocMaxFA_shoot"=LocMaxFA_shoot,
	"RelLocMaxFA_shoot"=LocMaxFA_shoot/TotHeight_shoot,
	"Root_length"=fronarea$root_len, 
	"Root_weight"=root_weight,
	"MaxFA_root"=MaxFA_root,
#	"LocMaxFA_root"=LocMaxFA_root,
	"RelLocMaxFA_root"=LocMaxFA_root/TotDepth_root
	))

#############################################################################
# LDA caluclation
library(MASS)
fit <- lda(Species ~ Height + Diameter + Shoot_weight + Bend_force + MaxFA_shoot + RelLocMaxFA_shoot + Root_length + Root_weight + MaxFA_root + RelLocMaxFA_root, data=newfron)
LD1<-predict(fit)$x
newfron$LDA<-as.numeric(LD1)

#fit <- lda(Species ~ Height + Diameter + Shoot_weight + Bend_force + MaxFA_shoot + LocMaxFA_shoot + RelLocMaxFA_shoot + Root_length + Root_weight + MaxFA_root + LocMaxFA_root + RelLocMaxFA_root, data=newfron)

#############################################################################
write.csv(newfron, row.names=F, file = "planttraits_reorganize.csv")
#############################################################################
#Preliminary data plots

isShort <- newfron$Height<=40
isTall <- newfron$Height>40
sum(isShort)
sum(isTall)

pairs(newfron,upper.panel=panel.smooth, lower.panel=panel.cor); #round(cor(data2,method='pearson',use="pairwise.complete.obs"),2)
mtext(side=3,line=2.5,"Correlation matrix of plant morphology traits (N=90 plants)")

boxplot(newfron$Height~newfron$Species,names=c("Cottonwood","Tamarisk"),ylab="Shoot height (cm)")

par(mfrow=c(2,2))
boxplot(LocMaxFA_shoot~Species, data=newfron[isShort,], ylim=c(0,800), las=1, names=c("Cottonwood","Tamarisk"),ylab="Position of maximum crown frontal area (mm)")
boxplot(LocMaxFA_shoot~Species, data=newfron[isTall,], ylim=c(0,800), las=1, names=c("Cottonwood","Tamarisk"),ylab="Position of maximum crown frontal area (mm)")
boxplot(RelLocMaxFA_shoot~Species, data=newfron[isShort,], ylim=c(0,1), las=1,  names=c("Cottonwood","Tamarisk"),ylab="Relative position of maximum crown frontal area")
boxplot(RelLocMaxFA_shoot~Species, data=newfron[isTall,], ylim=c(0,1), las=1,  names=c("Cottonwood","Tamarisk"),ylab="Relative position of maximum crown frontal area")

boxplot(newfron$RelLocMaxFA_shoot~newfron$Species,names=c("Cottonwood","Tamarisk"),ylab="Relative position of maximum crown frontal area")
boxplot(newfron$RelLocMaxFA_root~newfron$Species,names=c("Cottonwood","Tamarisk"),ylab="Relative position of maximum root frontal area")
#############################################################################
#############################################################################
#Calculate means and sd for all trait variables
names(newfron)
 [1] "Species"           "Height"            "Diameter"          "Shoot_weight"     
 [5] "Bend_force"        "MaxFA_shoot"       "LocMaxFA_shoot"    "RelLocMaxFA_shoot"
 [9] "Root_length"       "Root_weight"       "MaxFA_root"        "LocMaxFA_root"    
[13] "RelLocMaxFA_root"  "LDA" 
#t(aggregate(newfron[,-1],by=list(newfron[,1]), mean))		#'aggregate' can calculate functions on multiple dataframe columns

means<-t(apply(newfron[,2:11], 2, function(x) tapply(x, newfron[,1], mean)))
sd<-t(apply(newfron[,2:11], 2, function(x) tapply(x, newfron[,1], sd)))
traits_moments<-data.frame(trait=names(newfron[,2:11]),cw_avg=means[,1],cw_sd=sd[,1], tx_avg=means[,2],tx_sd=sd[,2])

#############################################################################
#Calculate zscores for all trait data (plotted in Fig 2); select median and IQR for plotting
zscores <- data.frame(scale(newfron[,2:11], center=T, scale=T))
temp<-t(aggregate(zscores,by=list(newfron[,1]), quantile))
q_z<-data.frame(cw=temp[-1,1], tm=temp[-1,2])
q25<-q_z[seq(2,50,by=5),]
q50<-q_z[seq(3,50,by=5),]
q75<-q_z[seq(4,50,by=5),]
name<-as.vector(names(newfron[,2:11]))

traits<-data.frame(name, q25_cw=q25[,1], q25_tm=q25[,2],q50_cw=q50[,1], q50_tm=q50[,2], q75_cw=q75[,1], q75_tm=q75[,2])

#means_z<-t(apply(zscores, 2, function(x) tapply(x, newfron[,1], mean)))
#sd_z<-t(apply(zscores, 2, function(x) tapply(x, newfron[,1], sd)))

#############################################################################
#Plot composite Figure 2 for manuscript
Fig2A<-function()
{	par(mar=c(4.25,6,0.5,0.5))
	plot(1, type="n", xlim=c(-1.5,1.5),ylim=c(10.5,-0.5),las=1,xlab="Standardized mean", ylab="", yaxt='n')
	segments(x0=traits$q25_cw, y0=(c(1:10)-0.1), x1 = traits$q75_cw, y1 = (c(1:10)-0.1),lwd=2)
	points(x=traits$q50_cw,y=(c(1:10)-0.1),type="p",pch=16,cex=1.5)

	segments(x0=traits$q25_tm, y0=(c(1:10)+0.1), x1 = traits$q75_tm, y1 = (c(1:10)+0.1), lty=2, lwd=2,col="black")
	points(x=traits$q50_tm,y=(c(1:10)+0.1),type="p",pch=16,col='white',cex=1.5)
	points(x=traits$q50_tm,y=(c(1:10)+0.1),type="p",pch=1,cex=1.5)

	abline(h=0.3,lty=3)
	abline(h=6.3,lty=3)
	axis(2, at=c(1:10),labels=c(expression("H"[Shoot]), expression("D"[Shoot]), expression("W"[Shoot]), "BF", expression("MFAD"[Shoot]),expression("LMFAD"[Shoot]), expression("L"[Root]),expression("W"[Root]), expression("MFAD"[Root]), expression("LMFAD"[Root])), las=1, cex.axis=1, tck=-.02)
	text(-0.3,0.5,"Aboveground variables",cex=1)
	text(-0.3,6.5,"Belowground variables",cex=1)
	legend('topright',legend=c("Cottonwood", "Tamarisk"),cex=1,lty=c(1,2),bty="n",lw=2)
	legend('topleft', "(A)", cex=1.25, bty='n')
	#text(-0.95,0.5,"A")
}

##############################
Fig2B<-function()
{	par(mar=c(4.25,4.5,0.5,0.5))

 #Calculate tamarisk mean frontal area at different heights. 
	fronden.t<-subset(fronarea,species=="tamarisk")
	location.t<-0
	uci.t<-0
	lci.t<-0
	meanden.t<-0
	library(plotrix)
	for (i in 1:152){
	   location.t[i]<--33.5+1*i
	   meanden.t[i]<-colMeans(fronden.t[24+i])
	   uci.t[i]<-colMeans(fronden.t[24+i])+std.error(fronden.t[24+i])
	   lci.t[i]<-colMeans(fronden.t[24+i])-std.error(fronden.t[24+i])
	    }

 #Calculate cottonwood mean frontal area at different heights. 
	fronden.c<-subset(fronarea,species=="cottonwood")
	meanden.c<-0
	location.c<-0
	uci.c<-0
	lci.c<-0
	for (i in 1:152){
	   location.c[i]<--33.5+1*i
	   meanden.c[i]<-colMeans(fronden.c[24+i])
	   uci.c[i]<-colMeans(fronden.c[24+i])+std.error(fronden.c[24+i])
	   lci.c[i]<-colMeans(fronden.c[24+i])-std.error(fronden.c[24+i])
	    }
 
 #Plot frontal area density (mean and SE) at different heights. 
	plot(1,xlim=c(0,20),ylim=c(-30,120),type="n", las=1, axes=T, xlab=expression(paste("Frontal area density (",cm^2," ",cm^-1,")")), ylab="Vertical position on plant (cm)", cex=1)
		lines(uci.t,location.t,col="grey60",lty=2)
		lines(lci.t,location.t,col="grey60",lty=2)
		lines(meanden.t,location.t,col="black",lty=2, lw=2)

		lines(uci.c,location.c,col="grey60",lty=1)
		lines(lci.c,location.c,col="grey60",lty=1)
		lines(meanden.c,location.c,col="black",lty=1,lw=2)

		abline(h=0, lty=2)
		legend('topleft', "(B)", cex=1.25, bty='n')
		legend('topright',legend=c("Cottonwood", "Tamarisk"),cex=1,lty=c(1,2),bty="n",lw=2)
		text(10,50,"Cottonwood \n\ Max FAD at 75% \n\ of stem height",cex=1)
		text(15,20,"Tamarisk \n\ Max FAD at 28% \n\ of stem height",cex=1)
}
##############################
Fig2C<-function()
{	par(mar=c(4.25,4.5,0.5,1))
	cw<-newfron$Species==1
	tm<-newfron$Species==2		
	plot(100*RelLocMaxFA_shoot~LDA, data=newfron[cw,], pch=16, xlim=c(-4.5,4.5), ylim=c(0,115), ylab=expression("LMFAD"[Shoot]~"( % )"), xlab="LDA ordination", las=1)
	points(100*RelLocMaxFA_shoot~LDA,data=newfron[tm,],pch=1)
	lm3<-lm(100*RelLocMaxFA_shoot~LDA,newfron); print(summary(lm3))
	abline(lm3,lty=2)
	legend(-6,120, "(C)", cex=1.25, bty='n')
	legend('topright', legend=c("p < 0.05", expression(paste(R^2, " = 0.83"))), cex=1, bty='n')
	legend('bottomleft',legend=c("Cottonwood","Tamarisk"), pch=c(16,1), bty="n")
}

##############################
Fig2D<-function()
{	par(mar=c(4.25,4.5,0.5,1))
	cw<-newfron$Species==1
	tm<-newfron$Species==2	
	plot(MaxFA_shoot~ LDA, data=newfron[cw,],pch=16, xlim=c(-4.5,4.5), ylim=c(0,30), 
	     ylab=expression(paste("MFAD"[Shoot]~"(",cm^2," ", cm^-1,")")), xlab="LDA ordination", las=1)
	points(MaxFA_shoot~ LDA,data=newfron[tm,],pch=1)
	lm2<-lm(MaxFA_shoot~ LDA,newfron); print(summary(lm2))
	abline(lm2,lty=2)
	legend(-6,32, "(D)", cex=1.25, bty='n')
	legend('bottomright', legend=c("p < 0.05", expression(paste(R^2, " = 0.34"))), cex=1, bty='n')
}

##############################
Fig2E<-function()
{	par(mar=c(4.25,4.5,0.5,1))
	cw<-newfron$Species==1
	tm<-newfron$Species==2
plot(Diameter~LDA,data=newfron[cw,],pch=16,xlim=c(-4.5,4.5), ylim=c(0,13), ylab=expression("D"[Shoot]~"( mm )"),xlab="LDA ordination", las=1)
	points(Diameter~LDA,data=newfron[tm,],pch=1)
	lm1<-lm(Diameter~LDA,newfron); print(summary(lm1))
	abline(lm1,lty=2)
	legend(-6, 13, "(E)", cex=1.25, bty='n')
	legend('bottomright', legend=c("p < 0.05", expression(paste(R^2, " = 0.29"))), cex=1, bty='n')
}

##############################
#Plot all five panels
layout(matrix(c(1,1,1,2,2,2,3,4,5),3,3, byrow=F))
Fig2A()
Fig2B()
Fig2C()
Fig2D()
Fig2E()
#############################################################################
#############################################################################
#Appendix figures
par(mfcol=c(3,1), oma=c(2,1,1,1), mar=c(1, 4.5, 0, 0.5))

plot(Shoot_weight~LDA,data=cw,pch=16,xlim=c(-4.5,4.5), ylim=c(0,20), ylab=expression("W"[Shoot]~"( g )"),xaxt='n', las=1)
points(Shoot_weight ~LDA,data=tm,pch=1)
lm<-lm(Shoot_weight ~LDA,newfron)
summary(lm)
abline(lm,lty=2)
text(3.5,2,"p < 0.05")
text(3.5,0.5,expression(paste(R^2, " = 0.12")))
legend('topleft', "A", cex=1.5, bty='n')

plot(MaxFA_root~LDA,data=cw,pch=16,xlim=c(-4.5,4.5), ylim=c(0,35), ylab=expression(paste("MFA"[Root]~"(",cm^2,")")),xaxt='n', las=1)
points(MaxFA_root~LDA,data=tm,pch=1)
lm<-lm(MaxFA_root~LDA,newfron)
summary(lm)
abline(lm,lty=2)
text(3.5,4,"p < 0.05")
text(3.5,2,expression(paste(R^2, " = 0.03")))
legend('topleft', "B", cex=1.5, bty='n')

plot(Root_weight~LDA,data=cw,pch=16,xlim=c(-4.5,4.5), ylim=c(0,12), ylab=expression("W"[Shoot]~"( g )"),xlab="LDA ordination", las=1)
points(Root_weight ~LDA,data=tm,pch=1)
lm<-lm(Root_weight ~LDA,newfron)
summary(lm)
abline(lm,lty=2)
text(3.5,1.5,"p < 0.05")
text(3.5,0.5,expression(paste(R^2, " = 0.34")))
legend('topleft', "C", cex=1.5, bty='n')

#############################################################################
#STOPPED HERE; THIS PART IS UNFINISHED. HOW DO WE GET TO THE OUTPUT "plant_traits.csv"?
#rescale.cw <- as.data.frame(scale(cw[2:14]))
#rescale.tm<- as.data.frame(scale(tm[2:14]))
#Can't perform centering on each species separately; need to center them together within each factor first, then plot CW and TX separately. Need to do this for each factor.
trait_zscores <- data.frame(scale(newfron[,2:13], center=T, scale=T))

test<-function(i)
	{	vec<-tapply(trait_zscores[,i],newfron$Species,mean, na.rm=T)
		mydf<-data.frame(trait=names(trait_zscores)[i],CW=vec[1],TX=vec[2])
		mydf
		}
for(i in 1:length(names(rescale.traits)))
{ print(test(i))}

sapply(rescale.cw,mean)
quantile(rescale.cw[,1])
c(1,2,3,5,6,8,9,10,11,12,13)

#calculation above saved to "plant_traits". 
#############################################################################
#I don't know where this dataframe came from:
traits <- read.csv("plant_traits.csv")

par(mfrow=c(1,1),mar=c(4, 6, 1, 1) )
par(las=1)
plot(1, type="n", xlim=c(-1,1),ylim=c(10.5,0.5),las=1,xlab="Standardized mean", ylab="", yaxt='n')
points(x=traits$med.cw,y=(c(1:10)+0.1),type="p",pch=16,cex=1.5)
segments(x0=traits$low.cw, y0=(c(1:10)+0.1), x1 = traits$high.cw, y1 = (c(1:10)+0.1),lwd=2)
points(x=traits$med.tm,y=(c(1:10)-0.1),type="p",pch=1,cex=1.5)
segments(x0=traits$low.tm, y0=(c(1:10)-0.1), x1 = traits$high.tm, y1 = (c(1:10)-0.1),lwd=1,col="gray")
abline(h=6.3,lty=2)
axis(2, at=c(1:10),labels=c(expression("H"[Shoot]),expression("MFA"[Shoot]),expression("LMFA"[Shoot]), expression("W"[Shoot]), expression("D"[Shoot]), "BF", expression("MFA"[Root]), expression("LMFA"[Root]),
expression("L"[Root]),expression("W"[Root])), las=1, cex.axis=1, tck=-.02)
text(0.45,0.5,"Aboveground variables",cex=0.8)
text(0.45,6.5,"Belowground variables",cex=0.8)
text(-0.95,0.5,"A")


cw.or<-rescale.cw[,5]*-3.68
tm.or<-rescale.tm[,5]*-3.68
plot(cw[,6]~cw.or,ylab="",type="p",pch=16)
plot(tm[,6]~tm.or,ylab="",type="p",pch=1)



cw<-droplevels(subset(newfron,Species=="cottonwood"))
tm<-droplevels(subset(newfron,Species=="tamarisk"))
tapply(newfron$RelLocMaxFA_shoot,newfron$Species,mean)
tapply(newfron$RelLocMaxFA_shoot,newfron$Species,sd)

tapply(newfron$RelLocMaxFA_root,newfron$Species,mean)
tapply(newfron$RelLocMaxFA_root,newfron$Species,sd)
summary(lm(RelLocMaxFA_shoot~Height*Species,data=newfron))
summary(lm(RelLocMaxFA_root~Height+Species,data=newfron))

 [1] "Species"           "Height"            "Diameter"          "Shoot_weight"     
 [5] "Bend_force"        "MaxFA_shoot"       "LocMaxFA_shoot"    "RelLocMaxFA_shoot"
 [9] "Root_length"       "Root_weight"       "MaxFA_root"        "LocMaxFA_root"    
[13] "RelLocMaxFA_root"

#LD1<-predict(fit)$x		#This was already defined above
#Test correlations of LD1 with all component variables
	cor.test(newfron$Height,newfron$LDA,method = "pearson")
	cor.test(newfron$Diameter,newfron$LDA,method = "pearson")
	cor.test(newfron$Shoot_weight,newfron$LDA,method = "pearson")
	cor.test(newfron$Bend_force,newfron$LDA,method = "pearson")
	cor.test(newfron$MaxFA_shoot,newfron$LDA,method = "pearson")
	cor.test(newfron$RelLocMaxFA_shoot,newfron$LDA,method = "pearson")
	cor.test(newfron$Root_length,newfron$LDA,method = "pearson")
	cor.test(newfron$Root_weight,newfron$LDA,method = "pearson")
	cor.test(newfron$MaxFA_root,newfron$LDA,method = "pearson")
	cor.test(newfron$RelLocMaxFA_root,newfron$LDA,method = "pearson")


z<-lm(LD1~height + MaxFA_shoot+RelLocMaxFA_shoot+shootwt+ root+ MaxFA_root++RelLocMaxFA_root+rootwt+diameter+bend_force,data=newfron)

plot(fit)
library(klaR)
partimat(Species~MaxFA_shoot+ shootwt+RelLocMaxFA_shoot+ root+ MaxFA_root+RelLocMaxFA_root+rootwt+diameter,data=newfron,method="lda",plot.matrix = TRUE)


t.test(pos.cw$LocMaxFA_shoot,pos.tm$LocMaxFA_shoot)
#t = 8.4407, df = 66.841, p-value = 3.933e-12, average: 359.2553   99.4186 
t.test(pos.cw$MaxFA_shoot,pos.tm$MaxFA_shoot)
#t = -5.0359, df = 61.414, p-value = 4.446e-06, average:  6.675617 13.595686 
t.test(pos.cw$RelLocMaxFA_shoot,pos.tm$RelLocMaxFA_shoot)
#t = 10.7901, df = 84.247, p-value < 2.2e-16, average: 0.7470189 0.2802768 

t.test(pos.cw$LocMaxFA_root,pos.tm$LocMaxFA_root)
#t = 0.3731, df = 87.368, p-value = 0.71
t.test(pos.cw$MaxFA_root,pos.tm$MaxFA_root)
#t = -1.6228, df = 81.267, p-value = 0.1085 
t.test(pos.cw$RelLocMaxFA_root,pos.tm$RelLocMaxFA_root)
#t = 0.3137, df = 86.329, p-value = 0.7545

#I have done ks.test for those relationship and the results look similar


lmfab<-lm(MaxFA_shoot~Species*height,data=newfron)
lmfbe<-lm(MaxFA_root~Species*height,data=newfron)
lmrt<-lm(root~Species*height,data=newfron)
lmpsab<-lm(LocMaxFA_shoot~Species+height,data=newfron)
lmpsbe<-lm(LocMaxFA_root~Species+height,data=newfron)

tolft<-fronarea$total.frontal.area
lmtolft<-lm(tolft~Species+hei_cm,data=fronarea)
lmleaf<-lm(leaf_num~hei_cm,data=fronarea)
lmdia<-lm(dia_mm~Species+hei_cm,data=fronarea)
lmbend<-lm(fronarea$bend_fce_g~fronarea$Species+fronarea$hei_cm)
rootwt<-rowSums(fronarea[,16:20],na.rm=T)
fronarea$rootwt<-rootwt
lmrtwt<-lm(rootwt~Species*hei_cm,data=fronarea)
lmabwt<-lm(aboveground_g~Species*hei_cm,data=fronarea)


#calculated mean and SD
mean(fron.c$total.frontal.area)
sd(fron.c$total.frontal.area)
mean(fron.t$total.frontal.area)
sd(fron.t$total.frontal.area)

newfron$Species<-as.factor(newfron$Species)

tapply(newfron$Height,newfron$Species,mean)
tapply(newfron$Height,newfron$Species,sd)

tapply(newfron$Diameter,newfron$Species,mean)
tapply(newfron$Diameter,newfron$Species,sd)

tapply(fronarea$aboveground_g,fronarea$Species,mean)
tapply(fronarea$aboveground_g,fronarea$Species,sd)

tapply(newfron$bend_force,newfron$Species,mean)
tapply(newfron$bend_force,newfron$Species,sd)

tapply(newfron$root,newfron$Species,mean)
tapply(newfron$root,newfron$Species,sd)

tapply(newfron$MaxFA_shoot,newfron$Species,mean)
tapply(newfron$MaxFA_shoot,newfron$Species,sd)

tapply(newfron$LocMaxFA_shoot,newfron$Species,mean)
tapply(newfron$LocMaxFA_shoot,newfron$Species,sd)

tapply(newfron$MaxFA_root,newfron$Species,mean)
tapply(newfron$MaxFA_root,newfron$Species,sd)

tapply(newfron$LocMaxFA_root,newfron$Species,mean)
tapply(newfron$LocMaxFA_root,newfron$Species,sd)

tapply(newfron$leaf,newfron$Species,mean)
tapply(newfron$leaf,newfron$Species,sd)

tapply(fronarea$rootwt,fronarea$Species,mean)
tapply(fronarea$rootwt,fronarea$Species,sd)


#plot plant structure
fronarea <- read.csv("plant_structure.csv")
spec<-fronarea$species
hei<-fronarea$hei_cm
befron<-rowSums(fronarea[,25:57])
abfron<-rowSums(fronarea[,58:176])

log.ab<-log(abfron)
log.be<-log(befron)
log.h<-log(hei)

lm.ab<-lm(log.ab~log.h*spec)
lm.be<-lm(log.be~log.h*spec)
plot(resid(lm.ab)~log.h,xlab="Height",ylab="residual of aboveground")
plot(resid(lm.be)~log.h,xlab="Height",ylab="residual of belowground")


summary(lm.ab)
summary(lm.be)




#plot frontal area (above and below ground)
fron.ab<-rowSums(fronarea[,58:176])
hei<-fronarea$hei_cm
fron.be<-rowSums(fronarea[,25:57])
layout(matrix(1:2, ncol = 1), widths = 1, respect = FALSE)
par(las=1)
par(mar=c(0, 5, 2,2) )
plot(fron.ab~hei,log="xy",pch=c(16,17)[spec],xlim=c(10,100),col=c("black","gray")[spec],xlab="", xaxt="n",ylab= expression(paste("Aboveground frontal area ( ",cm^2,")")))
curve(exp(-2.12)*x^1.74,log="xy",from=10, to=150,add=T,col="black",lty=2)
curve(exp(1.44)*x^1.03,log="xy",from=10, to=150,add=T,col="gray",lty=1)
legend(30,20,legend=c(expression("FA"[CW]~"="~paste("0.12*height"^"1.74")),
expression("FA"[TM]~"="~paste("4.22*height"^"1.03"))),cex=1, bty="n",col=c("black","gray"))
legend(10,900,legend=c("Cottonwood", "Tamarisk"),lty=c(2,1),col=c("black","gray"),pch=c(16,17),bty="n")
text(10,900,"A")

par(mar=c(4, 5, 0, 2))
plot(fron.be~hei,log="xy",pch=c(16,17)[spec],col=c("black","gray")[spec], xlim=c(10,100),xlab="Plant height (cm)",
ylab= expression(paste("Belowground frontal area ( ",cm^2,")")))
curve(exp(-1.24)*x^1.66,log="xy",from=10, to=100,add=T,col="black",lty=2)
curve(exp(3.83)*x^0.40,log="xy",from=10, to=100,add=T,col="gray",lty=1)
legend(30,20,legend=c(expression("FA"[CW]~"="~paste("0.29*heigh"^"1.66")),
expression("FA"[TM]~"="~paste("46*height"^"0.40"))),cex=1, bty="n",col=c("black","gray"))
text(10,600,"B")


#check the ratio between biomass and frontal area. conclusion: negative relationship
fronarea <- read.csv("plant_structure.csv")
fron.c<-subset(fronarea,Species=="cottonwood")
fron.t<-subset(fronarea,Species=="tamarisk")
ab.c=rowSums(fron.c[,25:57])
ab.t=rowSums(fron.t[,25:57])
be.c=rowSums(fron.c[,58:150])
be.t=rowSums(fron.t[,58:150])
fronratio.c<-ab.c/be.c
fronratio.t<-ab.t/be.t
mas.c.be<-rowSums(fron.c[,16:20])
mas.t.be<-rowSums(fron.t[,16:20])
massratio.c<-fron.c$aboveground_g/mas.c.be
massratio.t<-fron.t$aboveground_g/mas.t.be
summary(lm(massratio.c~fronratio.c))
summary(lm(massratio.t~fronratio.t))

library(psych)

pairs.panels(~newfron$Species+newfron$height+newfron$root+tolft+fronarea$aboveground_g+newfron$MaxFA_shoot+
newfron$LocMaxFA_shoot+rootwt+newfron$MaxFA_root+newfron$LocMaxFA_root+newfron$leaf+newfron$diameter+
newfron$bend_force,labels=c("Species","height","root length","total FA",
"aboveground mass","max aboveground","position of max ab",
"root weight","max belowground","position of max be","leaf","diameter","bending force"),gap=0)


plot(MaxFA_shoot~height,data=subset(newfron, Species==1))
plot(MaxFA_root~height,data=newfron)
plot(root~height,data=newfron)
plot(LocMaxFA_shoot~height,data=newfron)
plot(LocMaxFA_root~height,data=newfron)

plot(tolft~hei_cm,data=fronarea)
plot(leaf_num~hei_cm,data=fronarea)
plot(dia_mm~hei_cm,data=fronarea)
plot(bend_fce_g~hei_cm,data=fronarea)
plot(rootwt~hei_cm,data=fronarea)
plot(aboveground_g~hei_cm,data=subset(fronarea, Species=="cottonwood"))

#plot bending force
plot(bend_fce_g~hei_cm,data=fronarea,col=c("dark green","red")[Species],pch=c(16,17)[Species])

legend(3,1520,legend=c(expression("Cottonwood: FA"[CW]~"="~paste(e^"(3.78+0.039 * height)")),
expression("Tamarisk:FA"[TM]~"="~paste(e^"(5.59+0.016 * height)"))),cex=1, lty=c(1,2),pch=c(16,17),bty="n",col=c("dark green","red"))


#plant structure with a few other growth related parameters
struc<-read.csv("plant_structure.csv")
rootwt<-struc$root_wt_5_g+struc$root_wt_10_g+struc$root_wt_15_g+struc$root_wt_20_g+struc$root_wt_25_g
prt25<-struc$root_wt_25_g/rootwt
prt5<-struc$root_wt_5_g/rootwt
prt10<-struc$root_wt_10_g/rootwt
prt15<-struc$root_wt_15_g/rootwt
prt20<-struc$root_wt_20_g/rootwt
mean(rootwt,na.rm=T)
sd(rootwt,na.rm=T)
mean(struc$aboveground_g-rootwt,na.rm=T)
sd(struc$aboveground_g)

# pulling force
force<- read.csv("plant_root_pulling_force.csv")
max(force$force_g)
min(force$force_g)
tapply(force$force_g,force$description,mean)
tapply(force$force_g,force$description,sd)
summary(lm(force$force_g~force$description))

force_s<- read.csv("plant_stem_pulling_force.csv")
summary(lm(force_s$pulling.force_kg~force_s$Species+force_s$height_cm))
tapply(force_s$pulling.force_kg,force_s$Species,mean)
tapply(force_s$pulling.force_kg,force_s$Species,sd)


#individual run, the scour depth is the maximum scour depth instead of the scour depth at the dislodged time
indi<- read.csv("plant_individualrun.csv")
summary(lm(indi$hei_cm~indi$Species))
indi_tm<-subset(indi,Species=="tamarisk")
indi_cw<-subset(indi,Species=="cottonwood")
mean(indi_tm$hei_cm)
sd(indi_tm$hei_cm)
mean(indi_cw$hei_cm)
sd(indi_cw$hei_cm)

#plot relationship between the scour rate and scour depth vs others among individuals
indi_org<- read.csv("plant_individualrun.csv")
indi<-indi_org[2:23,]

scor<-indi$total.scour
scra<-indi$scour_rate
spec<-indi$Species
heig<-indi$hei_cm
stat<-ifelse(indi$status_deg=="dl","Dislodged ones","Non-disolodged ones")
local2<-indi$bucket.location

#calculate some parameters
max(heig)
min(heig)

indi_dl<-subset(indi,status=="y")
loca<-indi_dl$bucket.location
time<-indi_dl$time_elapse
spec<-indi_dl$Species
plot(time~loca,pch=c(16,17)[spec],col=c("dark green","red")[spec], xlab="location in the flume (m)",ylab="dislodgement time (min)")
legend(13.5,250,c("dislodged cw","dislodged tm"), pch=c(16,17),col=c("dark green","red"), box.col="white")

plot(scor~local2,pch=c(16,17)[spec],col=c("dark green","red")[spec], xlab="location in the flume (m)",ylab="Scour depth (mm)")
legend(22,130,c("cottonwood","tamarisk"), pch=c(16,17),col=c("dark green","red"), box="n")


summary(aov(scor~stat+spec))
summary(aov(scra~stat+spec))


par(las=1)
boxplot(scor~stat*spec,ylim=c(100,250),ylab="Maximum scour depth (mm)", names=c("Dislodged \n Cottonwood","Non-dislodged \n Cottonwood","Dislodged \n Tamarisk","Non-dislodged \n Tamarisk"))
text(2.5,230,"ANOVA p are not significant for either species or status")

boxplot(scra~stat*spec,ylim=c(0,6), ylab="Scour rate",names=c("Dislodged \n Cottonwood","Non-dislodged \n Cottonwood","Dislodged \n Tamarisk","Non-dislodged \n Tamarisk"))
text(2.5,5,"ANOVA p are not significant for either species or status")
text(3,4,"One extra point for non-dislodged tamarisk at scour rate of 15", cex=0.8)

#preliminary plots for plants and scour depth or scour rate
cwun<-subset(indi,Species=="cottonwood" &status=="n")
cwdl<-subset(indi,Species=="cottonwood" &status=="y")
tmun<-subset(indi,Species=="tamarisk" &status=="n")
tmdl<-subset(indi,Species=="tamarisk" &status=="y")

#height
plot(total.scour~hei_cm,data=cwun,pch=1, xlab="Plant height (cm)",ylab="Maximum scour depth(mm)",ylim=c(100,220),xlim=c(0,100),col="dark green")
points(total.scour~hei_cm,data=cwdl,pch=16,col="dark green")
points(total.scour~hei_cm,data=tmun,pch=2,col="red")
points(total.scour~hei_cm,data=tmdl,pch=17,col="red")

legend(70,120,c("non-dislodged cw","dislodged cw","non-dislodged tm","dislodged tm"), pch=c(1,16,2,17),col=c("dark green","dark green","red","red"), box.col="white")

plot(scour_rate~hei_cm,data=cwun,pch=1, xlab="Plant height (cm)",ylab="Scour rate",ylim=c(0,15),xlim=c(0,100),col="dark green")
points(scour_rate~hei_cm,data=cwdl,pch=16,col="dark green")
points(scour_rate~hei_cm,data=tmun,pch=2,col="red")
points(scour_rate~hei_cm,data=tmdl,pch=17,col="red")

legend(70,10,c("non-dislodged cw","dislodged cw","non-dislodged tm","dislodged tm"), pch=c(1,16,2,17),col=c("dark green","dark green","red","red"), box.col="white")

#diameter
plot(total.scour~dia_mm,data=cwun,pch=1, xlab="Stem diameter (mm)",ylab="Maximum scour depth(mm)",ylim=c(100,220),xlim=c(0,15),col="dark green")
points(total.scour~dia_mm,data=cwdl,pch=16,col="dark green")
points(total.scour~dia_mm,data=tmun,pch=2,col="red")
points(total.scour~dia_mm,data=tmdl,pch=17,col="red")

legend(10,130,c("non-dislodged cw","dislodged cw","non-dislodged tm","dislodged tm"), pch=c(1,16,2,17),col=c("dark green","dark green","red","red"), box.col="white")

plot(scour_rate~dia_mm,data=cwun,pch=1, xlab="Stem diameter (cm)",ylab="Scour rate",ylim=c(0,15),xlim=c(0,15),col="dark green")
points(scour_rate~dia_mm,data=cwdl,pch=16,col="dark green")
points(scour_rate~dia_mm,data=tmun,pch=2,col="red")
points(scour_rate~dia_mm,data=tmdl,pch=17,col="red")

legend(10,10,c("non-dislodged cw","dislodged cw","non-dislodged tm","dislodged tm"), pch=c(1,16,2,17),col=c("dark green","dark green","red","red"), box.col="white")

#FA
plot(total.scour~leaf_FA,data=cwun,pch=1, xlab="Aboveground FA (cm2)",ylab="Maximum scour depth(mm)",ylim=c(100,220),xlim=c(0,1000),col="dark green")
points(total.scour~leaf_FA,data=cwdl,pch=16,col="dark green")
points(total.scour~leaf_FA,data=tmun,pch=2,col="red")
points(total.scour~leaf_FA,data=tmdl,pch=17,col="red")

legend(700,140,c("non-dislodged cw","dislodged cw","non-dislodged tm","dislodged tm"), pch=c(1,16,2,17),col=c("dark green","dark green","red","red"), box.col="white")

plot(scour_rate~leaf_FA,data=cwun,pch=1, xlab="Aboveground FA (cm2)",ylab="Scour rate",ylim=c(0,15),xlim=c(0,1000),col="dark green")
points(scour_rate~leaf_FA,data=cwdl,pch=16,col="dark green")
points(scour_rate~leaf_FA,data=tmun,pch=2,col="red")
points(scour_rate~leaf_FA,data=tmdl,pch=17,col="red")

legend(700,10,c("non-dislodged cw","dislodged cw","non-dislodged tm","dislodged tm"), pch=c(1,16,2,17),col=c("dark green","dark green","red","red"), box.col="white")




#dislodgement equation for individual plants  #logistic model for 22 plants
indi_org<- read.csv("plant_individualrun.csv")
indi<-droplevels(indi_org[2:23,])
disl<-as.factor(ifelse(indi$status=="y","1","0"))

spec<-indi$Species
heig<-indi$hei_cm
root<-indi$root_len
scor<-indi$total.scour
fron<-indi$leaf_FA
ratio<-(scor/10)/root
shea<-indi$shear.stress

library(bbmle)
library(MASS)

step <- stepAIC(glm1, direction="both")

glm1=glm(disl~spec*heig,family="binomial")
glm2=glm(disl~spec*ratio,family="binomial")
glm3=glm(disl~spec*shea,family="binomial")

glm4=glm(disl~spec+heig,family="binomial")
glm5=glm(disl~spec+ratio,family="binomial")
glm6=glm(disl~spec+shea,family="binomial")

glm7=glm(disl~spec,family="binomial")
glm8=glm(disl~heig,family="binomial")
glm9=glm(disl~ratio,family="binomial")
glm10=glm(disl~shea,family="binomial")
glm11=glm(disl~1,family="binomial")

AICtab(glm1,glm2,glm3,glm4,glm5,glm6,glm7,glm8,glm9,glm10,glm11,weights=T,delta=T,base=T,sort=T)

      AIC  dAIC df weight
glm10 26.0  0.0 2  0.5682
glm6  27.9  1.9 3  0.2179
glm3  29.8  3.8 4  0.0848
glm11 30.8  4.8 1  0.0506
glm9  32.7  6.7 2  0.0200
glm8  32.7  6.7 2  0.0195
glm7  32.8  6.8 2  0.0186
glm5  34.7  8.6 3  0.0075
glm4  34.7  8.7 3  0.0072
glm1  36.6 10.6 4  0.0028
glm2  36.6 10.6 4  0.0028


##glm without shear stress

indi_org<- read.csv("plant_individualrun.csv")
indi<-droplevels(indi_org[2:23,])
disl<-as.factor(ifelse(indi$status=="y","1","0"))
spec<-indi$Species
heig<-indi$hei_cm
root<-indi$root_len
scor<-indi$total.scour
fron<-indi$leaf_FA
ratio<-(scor/10)/root


library(bbmle)
library(MASS)

glm1=glm(disl~spec*fron+ratio,family="binomial")
glm2=glm(disl~spec+fron+ratio,family="binomial")

glm3=glm(disl~spec+fron,family="binomial")
glm4=glm(disl~spec+ratio,family="binomial")
glm5=glm(disl~fron+ratio,family="binomial")

glm6=glm(disl~spec,family="binomial")
glm7=glm(disl~fron,family="binomial")
glm8=glm(disl~ratio,family="binomial")

glm9=glm(disl~spec*heig+ratio,family="binomial")
glm10=glm(disl~spec+heig+ratio,family="binomial")

glm11=glm(disl~spec+heig,family="binomial")
glm12=glm(disl~heig+ratio,family="binomial")
glm13=glm(disl~heig,family="binomial")

glm14=glm(disl~1,family="binomial")

AICtab(glm1,glm2,glm3,glm4,glm5,glm6,glm7,glm8,glm9,glm10,glm11,glm12,glm13,
glm14,weights=T,delta=T,base=T,sort=T)


exp(coef(glm1))

#scour depth and shear stress
dislodge<-read.csv("flood_repeatindividual.csv")
summary(lm(dislodge$shear.stress~dislodge$scour_depth))

#plot shear.stree and score depth

disl_no<-subset(dislodge,status=="N")
disl_y<-subset(dislodge,status=="Y")
ratio_no <- (disl_no$scour_depth/10)/disl_no$root_len
ratio_y <-(disl_y$scour_depth/10)/disl_y$root_len

par(mar=c(4,5, 1, 1) )
plot(shear.stress~ratio_no,data=disl_no, pch=c(1,2)[Species],
      xlim=c(0,1),ylim=c(0,30),xlab="Scour depth /root length",
     ylab=expression(paste("Shear stress ( N/ ",m^2,")")))
points(shear.stress~ratio_y,data=disl_y, pch=c(16,17)[Species],cex=2)

legend("topright",c("cottonwood","tamarisk"), pch=c(1,2),bty="n",cex=1.2)

summary(lm(shear.stress~ratio_y,data=disl_y))

#frontal area ~ scour ratio

data_indi<-read.csv("plant_individualrun.csv")
indi<-droplevels(data_indi[2:23,c("run","Species","status","leaf_FA","root_len","total.scour")])
indi$ratio<-(indi$total.scour/10)/indi$root_len
indi_n<-subset(indi,status=="n")
indi_y<-subset(indi,status=="y")

plot(leaf_FA~ratio,data=indi_y, pch=c(16,17)[Species],
      xlim=c(0,1),ylim=c(0,1000),xlab="Scour depth /root length",
     ylab=expression(paste("Aboverground FA ( ",m^2,")")),cex=1.2)
points(leaf_FA~ratio,data=indi_n,pch=c(1,2)[Species],cex=1.2)
legend("topright",c("uprooted cottonwood","uprooted tamarisk"), pch=c(16,17),bty="n",cex=1.2)



summary(lm(leaf_FA~ratio_all,data=indi))

#re_organizing hydro data for patches

patch<-read.csv("plant_patch_query.csv")
barebed<-read.csv("bare_bed.csv")
powerpatch<-read.csv("streampower_patch.csv")

#run3
    hydro3<-read.csv("clearing_run3.csv")
    patch_sub3<-subset(patch,run==3)
    elapse3<-patch_sub3[,16]+0.1
    elapse3[is.na(elapse3)]=-1
    diff3<-NA
    diff3[nrow(patch_sub3)]<-NA
    sp3<-NA
    sp3[nrow(patch_sub3)]<-NA
    for (i in 1:nrow(patch_sub3)) {
        time3<-0
        location3<-0
        elev3<-0
        location32<-0
        org_elev3<-0
        time32<-0
     if (elapse3[i]>0){ 
            time3<-which(abs(hydro3[1,]-elapse3[i])==min(abs(hydro3[1,]-elapse3[i]),na.rm=T))
            location3<-which(abs(hydro3[,time3-1]-patch_sub3[i,8]*1000)==min(abs(hydro3[,time3-1]-patch_sub3[i,8]*1000),na.rm=T))
            elev3<-hydro3[location3,time3]
            location32<-which(abs(barebed[,1]-patch_sub3[i,8]*1000)==min(abs(barebed[,1]-patch_sub3[i,8]*1000),na.rm=T))
            orig_elev3<-barebed[location32,2]
            diff3[i]<-orig_elev3-elev3
            
            time32<-which(abs(powerpatch[,1]-elapse3[i])==min(abs(powerpatch[,1]-elapse3[i]),na.rm=T))
            sp3[i]<-powerpatch[time32,2]
        }
      else {
        location3<-which(abs(hydro3[,49]-patch_sub3[i,8]*1000)==min(abs(hydro3[,49]-patch_sub3[i,8]*1000),na.rm=T))
        elev3<-min(hydro3[location3,50:80],na.rm=T)
        location32<-which(abs(barebed[,1]-patch_sub3[i,8]*1000)==min(abs(barebed[,1]-patch_sub3[i,8]*1000),na.rm=T))
        orig_elev3<-barebed[location32,2]
        diff3[i]<-orig_elev3-elev3
 
        
        time32<-hydro3[1,which(hydro3[location3,]==elev3)]
        sp3[i]<-powerpatch[which(powerpatch[,1]==time32,arr.ind=T),2]
      }
    }
     



#run4
    hydro4<-read.csv("clearing_run4.csv")
    patch_sub4<-subset(patch,run==4)
    elapse4<-patch_sub4[,16]+0.1
    elapse4[is.na(elapse4)]=-1
    diff4<-NA
    diff4[nrow(patch_sub4)]<-NA
    sp4<-NA
    sp4[nrow(patch_sub4)]<-NA
    for (i in 1:nrow(patch_sub4)) {
        time4<-0
        location4<-0
        elev4<-0
        location42<-0
        org_elev4<-0 
        time42<-0
     if (elapse4[i]>0){ 
            time4<-which(abs(hydro4[1,]-elapse4[i])==min(abs(hydro4[1,]-elapse4[i]),na.rm=T))
            location4<-which(abs(hydro4[,time4-1]-patch_sub4[i,8]*1000)==min(abs(hydro4[,time4-1]-patch_sub4[i,8]*1000),na.rm=T))
            elev4<-hydro4[location4,time4]
            location42<-which(abs(barebed[,1]-patch_sub4[i,8]*1000)==min(abs(barebed[,1]-patch_sub4[i,8]*1000),na.rm=T))
            orig_elev4<-barebed[location42,2]
            diff4[i]<-orig_elev4-elev4

            time42<-which(abs(powerpatch[,3]-elapse4[i])==min(abs(powerpatch[,3]-elapse4[i]),na.rm=T))
            sp4[i]<-powerpatch[time42,4]

        }
      else {
        location4<-which(abs(hydro4[,39]-patch_sub4[i,8]*1000)==min(abs(hydro4[,39]-patch_sub4[i,8]*1000),na.rm=T))
        elev4<-min(hydro4[location4,40:74],na.rm=T)
        location42<-which(abs(barebed[,1]-patch_sub4[i,8]*1000)==min(abs(barebed[,1]-patch_sub4[i,8]*1000),na.rm=T))
        orig_elev4<-barebed[location42,2]
        diff4[i]<-orig_elev4-elev4

        time42<-hydro4[1,which(hydro4[location4,]==elev4)]
        sp4[i]<-powerpatch[which(powerpatch[,3]==time42,arr.ind=T),4]
      }

     }

#run5
    hydro5<-read.csv("clearing_run5.csv")
    patch_sub5<-subset(patch,run==5)
    elapse5<-patch_sub5[,16]+0.1
    elapse5[is.na(elapse5)]=-1
    diff5<-NA
    diff5[nrow(patch_sub5)]<-NA
    sp5<-NA
    sp5[nrow(patch_sub5)]<-NA
    for (i in 1:nrow(patch_sub5)) {
        time5<-0
        location5<-0
        elev5<-0
        location52<-0
        org_elev5<-0 
         time52<-0
      if (elapse5[i]>0){ 
            time5<-which(abs(hydro5[1,]-elapse5[i])==min(abs(hydro5[1,]-elapse5[i]),na.rm=T))
            location5<-which(abs(hydro5[,time5-1]-patch_sub5[i,8]*1000)==min(abs(hydro5[,time5-1]-patch_sub5[i,8]*1000),na.rm=T))
            elev5<-hydro5[location5,time5]
            location52<-which(abs(barebed[,1]-patch_sub5[i,8]*1000)==min(abs(barebed[,1]-patch_sub5[i,8]*1000),na.rm=T))
            orig_elev5<-barebed[location52,2]
            diff5[i]<-orig_elev5-elev5

            time52<-which(abs(powerpatch[,5]-elapse5[i])==min(abs(powerpatch[,5]-elapse5[i]),na.rm=T))
            sp5[i]<-powerpatch[time52,6]
        }
      else {
        location5<-which(abs(hydro5[,1]-patch_sub5[i,8]*1000)==min(abs(hydro5[,1]-patch_sub5[i,8]*1000),na.rm=T))
        elev5<-min(hydro5[location5,2:66],na.rm=T)
        location52<-which(abs(barebed[,1]-patch_sub5[i,8]*1000)==min(abs(barebed[,1]-patch_sub5[i,8]*1000),na.rm=T))
        orig_elev5<-barebed[location52,2]
        diff5[i]<-orig_elev5-elev5

        time52<-hydro5[1,which(hydro5[location5,]==elev5)]
        sp5[i]<-powerpatch[which(powerpatch[,5]==time52,arr.ind=T),6]
      }
     }

#run6
    hydro6<-read.csv("clearing_run6.csv")
    patch_sub6<-subset(patch,run==6)
    elapse6<-patch_sub6[,16]+0.1
    elapse6[is.na(elapse6)]=-1
    diff6<-NA
    diff6[nrow(patch_sub6)]<-NA
    sp6<-NA
    sp6[nrow(patch_sub6)]<-NA
    for (i in 1:nrow(patch_sub6)) {
        time6<-0
        location6<-0
        elev6<-0
        location62<-0
        org_elev6<-0 
        time62<-0
      if (elapse6[i]>0){ 
            time6<-which(abs(hydro6[1,]-elapse6[i])==min(abs(hydro6[1,]-elapse6[i]),na.rm=T))
            location6<-which(abs(hydro6[,time6-1]-patch_sub6[i,8]*1000)==min(abs(hydro6[,time6-1]-patch_sub6[i,8]*1000),na.rm=T))
            elev6<-hydro6[location6,time6]
            location62<-which(abs(barebed[,1]-patch_sub6[i,8]*1000)==min(abs(barebed[,1]-patch_sub6[i,8]*1000),na.rm=T))
            orig_elev6<-barebed[location62,2]
            diff6[i]<-orig_elev6-elev6
            time62<-which(abs(powerpatch[,7]-elapse6[i])==min(abs(powerpatch[,7]-elapse6[i]),na.rm=T))
            sp6[i]<-powerpatch[time62,8]
        }
      else {
        location6<-which(abs(hydro6[,1]-patch_sub6[i,8]*1000)==min(abs(hydro6[,1]-patch_sub6[i,8]*1000),na.rm=T))
        elev6<-min(hydro6[location6,2:62],na.rm=T)
        location62<-which(abs(barebed[,1]-patch_sub6[i,8]*1000)==min(abs(barebed[,1]-patch_sub6[i,8]*1000),na.rm=T))
        orig_elev6<-barebed[location62,2]
        diff6[i]<-orig_elev6-elev6

        time62<-hydro6[1,which(hydro6[location6,]==elev6)]
        sp6[i]<-powerpatch[which(powerpatch[,7]==time62,arr.ind=T),8]
      }    
     }

#run8
    hydro8<-read.csv("clearing_run8.csv")
    patch_sub8<-subset(patch,run==8)
    elapse8<-patch_sub8[,16]+0.1
    elapse8[is.na(elapse8)]=-1
    diff8<-NA
    diff8[nrow(patch_sub8)]<-NA
    sp8<-NA
    sp8[nrow(patch_sub8)]<-NA
    for (i in 1:nrow(patch_sub8)) {
        time8<-0
        location8<-0
        elev8<-0
        location82<-0
        org_elev8<-0 
        time82<-0
      if (elapse8[i]>0){ 
            time8<-which(abs(hydro8[1,]-elapse8[i])==min(abs(hydro8[1,]-elapse8[i]),na.rm=T))
            location8<-which(abs(hydro8[,time8-1]-patch_sub8[i,8]*1000)==min(abs(hydro8[,time8-1]-patch_sub8[i,8]*1000),na.rm=T))
            elev8<-hydro8[location8,time8]
            location82<-which(abs(barebed[,1]-patch_sub8[i,8]*1000)==min(abs(barebed[,1]-patch_sub8[i,8]*1000),na.rm=T))
            orig_elev8<-barebed[location82,2]
            diff8[i]<-orig_elev8-elev8
  
            time82<-which(abs(powerpatch[,9]-elapse8[i])==min(abs(powerpatch[,9]-elapse8[i]),na.rm=T))
            sp8[i]<-powerpatch[time82,10]
          }
      else {
        location8<-which(abs(hydro8[,41]-patch_sub8[i,8]*1000)==min(abs(hydro8[,41]-patch_sub8[i,8]*1000),na.rm=T))
        elev8<-min(hydro8[location8,42:50],na.rm=T)
        location82<-which(abs(barebed[,1]-patch_sub8[i,8]*1000)==min(abs(barebed[,1]-patch_sub8[i,8]*1000),na.rm=T))
        orig_elev8<-barebed[location82,2]
        diff8[i]<-orig_elev8-elev8

        time82<-hydro8[1,which(hydro8[location8,]==elev8)]
        sp8[i]<-powerpatch[which(powerpatch[,9]==time82,arr.ind=T),10]
      } 
     }

diff<-append(diff3,c(diff4,diff5,diff6,diff8))
streampower<-append(sp3,c(sp4,sp5,sp6,sp8))

#all data above has been saved to "plant_patch query_sed.csv". 

library(combinat)
combn(c("heig+","spec+","dens+","rsratio+","shear+","spec*heig+", "spec*rsratio+","spec*shear+"),8,simplify=F)
combn(c("heig+","spec+","dens+","rsratio+","shear+","spec*heig+", "spec*rsratio+","spec*shear+"),7,simplify=F)
combn(c("heig+","spec+","dens+","rsratio+","shear+","spec*heig+", "spec*rsratio+","spec*shear+"),6,simplify=F)
combn(c("heig+","spec+","dens+","rsratio+","shear+","spec*heig+", "spec*rsratio+","spec*shear+"),5,simplify=F)
combn(c("heig+","spec+","dens+","rsratio+","shear+","spec*heig+", "spec*rsratio+","spec*shear+"),4,simplify=F)
combn(c("heig+","spec+","dens+","rsratio+","shear+","spec*heig+", "spec*rsratio+","spec*shear+"),3,simplify=F)
combn(c("heig+","spec+","dens+","rsratio+","shear+","spec*heig+", "spec*rsratio+","spec*shear+"),2,simplify=F)
combn(c("heig+","spec+","dens+","rsratio+","shear+","spec*heig+", "spec*rsratio+","spec*shear+"),1,simplify=F)




#glm for all with shear stress
patch2<-read.csv("plant_all.csv")
disl<-as.factor(ifelse(patch2$status_def=="dl","1","0"))
run<-patch2$run
spec<-patch2$Species
heig<-patch2$hei_cm
root<-patch2$root.length
scor<-patch2$sed_diff
dens<-patch2$density
rsratio<-scor/root
shear<-patch2$shear_stress
shdum<-patch2$sheardummy 

library(bbmle)
library(lme4)

glm0=glmer(disl~spec+heig+dens+rsratio+shear+spec*heig+spec*rsratio+spec*shear+(1|run),family="binomial",control = glmerControl(optimizer = "bobyqa"))

glm1=glmer(disl~heig+spec+dens+rsratio+shear+spec*heig+spec*rsratio+(1|run),family="binomial",control = glmerControl(optimizer = "bobyqa"))
glm2=glmer(disl~heig+spec+dens+rsratio+shear+spec*heig+spec*shear+(1|run),family="binomial")
glm3=glmer(disl~heig+spec+dens+rsratio+shear+spec*rsratio+spec*shear+(1|run),family="binomial",control = glmerControl(optimizer = "bobyqa"))
glm4=glmer(disl~heig+spec+rsratio+shear+spec*heig+spec*rsratio+spec*shear+(1|run),family="binomial")
glm5=glmer(disl~heig+spec+dens+rsratio+shear+spec*heig+(1|run),family="binomial")
glm6=glmer(disl~heig+spec+dens+rsratio+shear+spec*rsratio+(1|run),family="binomial")
glm7=glmer(disl~heig+spec+dens+rsratio+shear+spec*shear+(1|run),family="binomial")
glm8=glmer(disl~heig+spec+dens+rsratio+spec*heig+spec*rsratio+(1|run),family="binomial",control = glmerControl(optimizer = "bobyqa"))
glm9=glmer(disl~heig+spec+dens+shear+spec*heig+spec*shear+(1|run),family="binomial")
glm10=glmer(disl~heig+spec+rsratio+shear+spec*heig+spec*rsratio+(1|run),family="binomial")
glm11=glmer(disl~heig+spec+rsratio+shear+spec*heig+spec*shear+(1|run),family="binomial")
glm12=glmer(disl~heig+spec+rsratio+shear+spec*rsratio+spec*shear+(1|run),family="binomial")
glm13=glmer(disl~spec+dens+rsratio+shear+spec*rsratio+spec*shear+(1|run),family="binomial")


glm14=glmer(disl~heig+spec+dens+rsratio+shear+(1|run),family="binomial")
glm15=glmer(disl~heig+spec+dens+rsratio+spec*heig+(1|run),family="binomial")
glm16=glmer(disl~heig+spec+dens+rsratio+spec*rsratio+(1|run),family="binomial")
glm17=glmer(disl~heig+spec+dens+shear+spec*heig+(1|run),family="binomial")
glm18=glmer(disl~heig+spec+dens+shear+spec*shear+(1|run),family="binomial")
glm19=glmer(disl~heig+spec+rsratio+shear+spec*heig+(1|run),family="binomial")
glm20=glmer(disl~heig+spec+rsratio+shear+spec*rsratio+(1|run),family="binomial")
glm21=glmer(disl~heig+spec+rsratio+shear+spec*shear+(1|run),family="binomial")
glm22=glmer(disl~heig+spec+rsratio+spec*heig+spec*rsratio+(1|run),family="binomial")
glm23=glmer(disl~heig+spec+shear+spec*heig+spec*shear+(1|run),family="binomial")
glm24=glmer(disl~spec+dens+rsratio+shear+spec*rsratio+(1|run),family="binomial")
glm25=glmer(disl~spec+dens+rsratio+shear+spec*shear+(1|run),family="binomial")
glm26=glmer(disl~spec+rsratio+shear+spec*rsratio+spec*shear+(1|run),family="binomial")

glm27=glmer(disl~heig+spec+dens+rsratio+(1|run),family="binomial")
glm28=glmer(disl~heig+spec+dens+shear+(1|run),family="binomial")
glm29=glmer(disl~heig+spec+dens+spec*heig+(1|run),family="binomial")
glm30=glmer(disl~heig+spec+rsratio+shear+(1|run),family="binomial")
glm31=glmer(disl~heig+spec+rsratio+spec*heig+(1|run),family="binomial")
glm32=glmer(disl~heig+spec+rsratio+spec*rsratio+(1|run),family="binomial")
glm33=glmer(disl~heig+spec+shear+spec*heig+(1|run),family="binomial")
glm34=glmer(disl~heig+spec+shear+spec*shear+(1|run),family="binomial")
glm35=glmer(disl~heig+dens+rsratio+shear+(1|run),family="binomial")
glm36=glmer(disl~spec+dens+rsratio+shear+(1|run),family="binomial")
glm37=glmer(disl~spec+dens+rsratio+spec*rsratio+(1|run),family="binomial")
glm38=glmer(disl~spec+dens+shear+spec*shear+(1|run),family="binomial")
glm39=glmer(disl~spec+rsratio+shear+spec*rsratio+(1|run),family="binomial")
glm40=glmer(disl~spec+rsratio+shear+spec*shear+(1|run),family="binomial")

glm41=glmer(disl~heig+spec+dens+(1|run),family="binomial")
glm42=glmer(disl~heig+spec+rsratio+(1|run),family="binomial")
glm43=glmer(disl~heig+spec+shear+(1|run),family="binomial")
glm44=glmer(disl~heig+spec+spec*heig+(1|run),family="binomial")
glm45=glmer(disl~heig+dens+rsratio+(1|run),family="binomial")
glm46=glmer(disl~heig+dens+shear+(1|run),family="binomial")
glm47=glmer(disl~heig+rsratio+shear+(1|run),family="binomial")
glm48=glmer(disl~spec+dens+rsratio+(1|run),family="binomial")
glm49=glmer(disl~spec+dens+shear+(1|run),family="binomial")
glm50=glmer(disl~spec+rsratio+shear+(1|run),family="binomial")
glm51=glmer(disl~spec+rsratio+spec*rsratio+(1|run),family="binomial")
glm52=glmer(disl~spec+shear+spec*shear+(1|run),family="binomial")
glm53=glmer(disl~dens+rsratio+shear+(1|run),family="binomial")

glm54=glmer(disl~heig+spec+(1|run),family="binomial")
glm55=glmer(disl~heig+dens+(1|run),family="binomial")
glm56=glmer(disl~heig+rsratio+(1|run),family="binomial")
glm57=glmer(disl~heig+shear+(1|run),family="binomial")
glm58=glmer(disl~spec+dens+(1|run),family="binomial")
glm59=glmer(disl~spec+rsratio+(1|run),family="binomial")
glm60=glmer(disl~spec+shear+(1|run),family="binomial")
glm61=glmer(disl~dens+rsratio+(1|run),family="binomial")
glm62=glmer(disl~dens+shear+(1|run),family="binomial")
glm63=glmer(disl~rsratio+shear+(1|run),family="binomial")

glm64=glmer(disl~heig+(1|run),family="binomial")
glm65=glmer(disl~spec+(1|run),family="binomial")
glm66=glmer(disl~dens+(1|run),family="binomial")
glm67=glmer(disl~rsratio+(1|run),family="binomial")
glm68=glmer(disl~shear+(1|run),family="binomial")
glm69=glmer(disl~1+(1|run),family="binomial")


AICtab(glm0,glm1,glm2,glm3,glm4,glm5,glm6,glm7,glm8,glm9,glm10,glm11,glm12,glm13,
glm14,glm15,glm16,glm17,glm18,glm19,glm20,glm21,glm22,glm23,glm24,glm25,glm26,glm27,glm28,glm29,glm30,
glm31,glm32,glm33,glm34,glm35,glm36,glm37,glm38,glm39,glm40,glm41,glm42,glm43,glm44,glm45,glm46,glm47,glm48,
glm49,glm50,glm51,glm52,glm53,glm54,glm55,glm56,glm57,glm58,glm59,glm60,glm61,glm62,glm63,glm64,glm65,glm66,
glm67,glm68,glm69,weights=T,delta=T,base=T,sort=T)

       AIC   dAIC  df weight
glm39 406.8   0.0 6  0.2282
glm26 407.1   0.2 7  0.2039
glm20 408.8   2.0 7  0.0840
glm24 409.0   2.2 8  0.0758
glm12 409.0   2.2 8  0.0756
glm10 409.4   2.6 8  0.0628
glm13 409.5   2.7 9  0.0590
glm4  410.0   3.1 9  0.0474
glm6  411.0   4.2 9  0.0285
glm1  411.5   4.7 10 0.0215
glm3  411.5   4.7 10 0.0213
glm51 412.0   5.1 5  0.0170
glm0  412.4   5.5 11 0.0141
glm37 413.0   6.1 7  0.0107
glm68 413.1   6.2 3  0.0101
glm32 413.9   7.1 6  0.0067
glm63 414.4   7.6 4  0.0051
glm22 414.5   7.6 7  0.0050
glm16 414.6   7.7 8  0.0047
glm60 414.8   8.0 4  0.0043
glm57 415.0   8.2 4  0.0038
glm8  415.1   8.2 9  0.0037
glm52 415.5   8.6 5  0.0031
glm62 415.5   8.7 5  0.0030
glm50 416.2   9.4 5  0.0021
glm47 416.4   9.6 5  0.0019
glm40 416.6   9.8 6  0.0017
glm43 416.8   9.9 5  0.0016
glm49 416.9  10.0 6  0.0015
glm53 416.9  10.1 6  0.0015
glm46 417.4  10.5 6  0.0012
glm34 417.4  10.6 6  0.0011
glm38 417.7  10.9 7  <0.001
glm30 418.2  11.4 6  <0.001
glm36 418.3  11.5 7  <0.001
glm33 418.6  11.8 6  <0.001
glm21 418.6  11.8 7  <0.001
glm28 418.7  11.9 7  <0.001
glm35 418.8  12.0 7  <0.001
glm25 419.0  12.2 8  <0.001
glm23 419.4  12.6 7  <0.001
glm18 419.7  12.8 8  <0.001
glm19 420.0  13.2 7  <0.001
glm14 420.3  13.4 8  <0.001
glm11 420.5  13.7 8  <0.001
glm17 420.5  13.7 8  <0.001
glm7  421.0  14.1 9  <0.001
glm9  421.6  14.7 9  <0.001
glm69 421.9  15.0 2  <0.001
glm5  422.0  15.2 9  <0.001
glm67 422.3  15.5 3  <0.001
glm2  422.8  16.0 10 <0.001
glm66 422.9  16.0 4  <0.001
glm64 423.4  16.5 3  <0.001
glm61 423.4  16.6 5  <0.001
glm65 423.6  16.8 3  <0.001
glm58 423.8  17.0 5  <0.001
glm55 423.9  17.1 5  <0.001
glm56 424.2  17.3 4  <0.001
glm59 424.2  17.3 4  <0.001
glm48 424.5  17.7 6  <0.001
glm41 424.8  18.0 6  <0.001
glm45 424.8  18.0 6  <0.001
glm54 425.2  18.4 4  <0.001
glm27 425.9  19.1 7  <0.001
glm42 426.0  19.2 5  <0.001
glm29 426.7  19.9 7  <0.001
glm44 427.1  20.3 5  <0.001
glm15 427.8  20.9 8  <0.001
glm31 427.9  21.1 6  <0.001


#glm for all with shear stress,,,no mixed effect
patch2<-read.csv("plant_all.csv")
disl<-as.factor(ifelse(patch2$status_def=="dl","1","0"))
run<-patch2$run
spec<-patch2$Species
heig<-patch2$hei_cm
root<-patch2$root.length
scor<-patch2$sed_diff
dens<-patch2$density
rsratio<-scor/root
shear<-patch2$shear_stress
shdum<-patch2$sheardummy 

library(bbmle)
library(lme4)

glm0=glm(disl~spec+heig+dens+rsratio+shear+spec*heig+spec*rsratio+spec*shear,family="binomial")

glm1=glm(disl~heig+spec+dens+rsratio+shear+spec*heig+spec*rsratio,family="binomial")
glm2=glm(disl~heig+spec+dens+rsratio+shear+spec*heig+spec*shear,family="binomial")
glm3=glm(disl~heig+spec+dens+rsratio+shear+spec*rsratio+spec*shear,family="binomial")
glm4=glm(disl~heig+spec+rsratio+shear+spec*heig+spec*rsratio+spec*shear,family="binomial")
glm5=glm(disl~heig+spec+dens+rsratio+shear+spec*heig,family="binomial")
glm6=glm(disl~heig+spec+dens+rsratio+shear+spec*rsratio,family="binomial")
glm7=glm(disl~heig+spec+dens+rsratio+shear+spec*shear,family="binomial")
glm8=glm(disl~heig+spec+dens+rsratio+spec*heig+spec*rsratio,family="binomial")
glm9=glm(disl~heig+spec+dens+shear+spec*heig+spec*shear,family="binomial")
glm10=glm(disl~heig+spec+rsratio+shear+spec*heig+spec*rsratio,family="binomial")
glm11=glm(disl~heig+spec+rsratio+shear+spec*heig+spec*shear,family="binomial")
glm12=glm(disl~heig+spec+rsratio+shear+spec*rsratio+spec*shear,family="binomial")
glm13=glm(disl~spec+dens+rsratio+shear+spec*rsratio+spec*shear,family="binomial")


glm14=glm(disl~heig+spec+dens+rsratio+shear,family="binomial")
glm15=glm(disl~heig+spec+dens+rsratio+spec*heig,family="binomial")
glm16=glm(disl~heig+spec+dens+rsratio+spec*rsratio,family="binomial")
glm17=glm(disl~heig+spec+dens+shear+spec*heig,family="binomial")
glm18=glm(disl~heig+spec+dens+shear+spec*shear,family="binomial")
glm19=glm(disl~heig+spec+rsratio+shear+spec*heig,family="binomial")
glm20=glm(disl~heig+spec+rsratio+shear+spec*rsratio,family="binomial")
glm21=glm(disl~heig+spec+rsratio+shear+spec*shear,family="binomial")
glm22=glm(disl~heig+spec+rsratio+spec*heig+spec*rsratio,family="binomial")
glm23=glm(disl~heig+spec+shear+spec*heig+spec*shear,family="binomial")
glm24=glm(disl~spec+dens+rsratio+shear+spec*rsratio,family="binomial")
glm25=glm(disl~spec+dens+rsratio+shear+spec*shear,family="binomial")
glm26=glm(disl~spec+rsratio+shear+spec*rsratio+spec*shear,family="binomial")

glm27=glm(disl~heig+spec+dens+rsratio,family="binomial")
glm28=glm(disl~heig+spec+dens+shear,family="binomial")
glm29=glm(disl~heig+spec+dens+spec*heig,family="binomial")
glm30=glm(disl~heig+spec+rsratio+shear,family="binomial")
glm31=glm(disl~heig+spec+rsratio+spec*heig,family="binomial")
glm32=glm(disl~heig+spec+rsratio+spec*rsratio,family="binomial")
glm33=glm(disl~heig+spec+shear+spec*heig,family="binomial")
glm34=glm(disl~heig+spec+shear+spec*shear,family="binomial")
glm35=glm(disl~heig+dens+rsratio+shear,family="binomial")
glm36=glm(disl~spec+dens+rsratio+shear,family="binomial")
glm37=glm(disl~spec+dens+rsratio+spec*rsratio,family="binomial")
glm38=glm(disl~spec+dens+shear+spec*shear,family="binomial")
glm39=glm(disl~spec+rsratio+shear+spec*rsratio,family="binomial")
glm40=glm(disl~spec+rsratio+shear+spec*shear,family="binomial")

glm41=glm(disl~heig+spec+dens,family="binomial")
glm42=glm(disl~heig+spec+rsratio,family="binomial")
glm43=glm(disl~heig+spec+shear,family="binomial")
glm44=glm(disl~heig+spec+spec*heig,family="binomial")
glm45=glm(disl~heig+dens+rsratio,family="binomial")
glm46=glm(disl~heig+dens+shear,family="binomial")
glm47=glm(disl~heig+rsratio+shear,family="binomial")
glm48=glm(disl~spec+dens+rsratio,family="binomial")
glm49=glm(disl~spec+dens+shear,family="binomial")
glm50=glm(disl~spec+rsratio+shear,family="binomial")
glm51=glm(disl~spec+rsratio+spec*rsratio,family="binomial")
glm52=glm(disl~spec+shear+spec*shear,family="binomial")
glm53=glm(disl~dens+rsratio+shear,family="binomial")

glm54=glm(disl~heig+spec,family="binomial")
glm55=glm(disl~heig+dens,family="binomial")
glm56=glm(disl~heig+rsratio,family="binomial")
glm57=glm(disl~heig+shear,family="binomial")
glm58=glm(disl~spec+dens,family="binomial")
glm59=glm(disl~spec+rsratio,family="binomial")
glm60=glm(disl~spec+shear,family="binomial")
glm61=glm(disl~dens+rsratio,family="binomial")
glm62=glm(disl~dens+shear,family="binomial")
glm63=glm(disl~rsratio+shear,family="binomial")

glm64=glm(disl~heig,family="binomial")
glm65=glm(disl~spec,family="binomial")
glm66=glm(disl~dens,family="binomial")
glm67=glm(disl~rsratio,family="binomial")
glm68=glm(disl~shear,family="binomial")
glm69=glm(disl~1,family="binomial")


AICtab(glm0,glm1,glm2,glm3,glm4,glm5,glm6,glm7,glm8,glm9,glm10,glm11,glm12,glm13,
glm14,glm15,glm16,glm17,glm18,glm19,glm20,glm21,glm22,glm23,glm24,glm25,glm26,glm27,glm28,glm29,glm30,
glm31,glm32,glm33,glm34,glm35,glm36,glm37,glm38,glm39,glm40,glm41,glm42,glm43,glm44,glm45,glm46,glm47,glm48,
glm49,glm50,glm51,glm52,glm53,glm54,glm55,glm56,glm57,glm58,glm59,glm60,glm61,glm62,glm63,glm64,glm65,glm66,
glm67,glm68,glm69,weights=T,delta=T,base=T,sort=T)







#mixed linear model with dummy shear stress
patch2<-read.csv("plant_all.csv")
disl<-as.factor(ifelse(patch2$status_def=="dl","1","0"))
run<-patch2$run
spec<-patch2$Species
heig<-patch2$hei_cm
root<-patch2$root.length
scor<-patch2$sed_diff
dens<-patch2$density
rsratio<-scor/root
shear<-patch2$shear_stress
shdum<-patch2$sheardummy 

library(bbmle)
library(lme4)

glm0=glmer(disl~spec+heig+dens+rsratio+shear:shdum+spec*heig+spec*rsratio+spec:shear:shdum+(1|run),family="binomial",control = glmerControl(optimizer = "bobyqa"))

glm1=glmer(disl~heig+spec+dens+rsratio+shear:shdum+spec*heig+spec*rsratio+(1|run),family="binomial")
glm2=glmer(disl~heig+spec+dens+rsratio+shear:shdum+spec*heig+spec:shear:shdum+(1|run),family="binomial",control = glmerControl(optimizer = "bobyqa"))
glm3=glmer(disl~heig+spec+dens+rsratio+shear:shdum+spec*rsratio+spec:shear:shdum+(1|run),family="binomial",control = glmerControl(optimizer = "bobyqa"))
glm4=glmer(disl~heig+spec+rsratio+shear:shdum+spec*heig+spec*rsratio+spec:shear:shdum+(1|run),family="binomial")
glm5=glmer(disl~heig+spec+dens+rsratio+shear:shdum+spec*heig+(1|run),family="binomial")
glm6=glmer(disl~heig+spec+dens+rsratio+shear:shdum+spec*rsratio+(1|run),family="binomial")
glm7=glmer(disl~heig+spec+dens+rsratio+shear:shdum+spec:shear:shdum+(1|run),family="binomial")
glm8=glmer(disl~heig+spec+dens+rsratio+spec*heig+spec*rsratio+(1|run),family="binomial",control = glmerControl(optimizer = "bobyqa"))
glm9=glmer(disl~heig+spec+dens+shear:shdum+spec*heig+spec:shear:shdum+(1|run),family="binomial")
glm10=glmer(disl~heig+spec+rsratio+shear:shdum+spec*heig+spec*rsratio+(1|run),family="binomial")
glm11=glmer(disl~heig+spec+rsratio+shear:shdum+spec*heig+spec:shear:shdum+(1|run),family="binomial")
glm12=glmer(disl~heig+spec+rsratio+shear:shdum+spec*rsratio+spec:shear:shdum+(1|run),family="binomial")
glm13=glmer(disl~spec+dens+rsratio+shear:shdum+spec*rsratio+spec:shear:shdum+(1|run),family="binomial")


glm14=glmer(disl~heig+spec+dens+rsratio+shear:shdum+(1|run),family="binomial")
glm15=glmer(disl~heig+spec+dens+rsratio+spec*heig+(1|run),family="binomial")
glm16=glmer(disl~heig+spec+dens+rsratio+spec*rsratio+(1|run),family="binomial")
glm17=glmer(disl~heig+spec+dens+shear:shdum+spec*heig+(1|run),family="binomial")
glm18=glmer(disl~heig+spec+dens+shear:shdum+spec:shear:shdum+(1|run),family="binomial")
glm19=glmer(disl~heig+spec+rsratio+shear:shdum+spec*heig+(1|run),family="binomial")
glm20=glmer(disl~heig+spec+rsratio+shear:shdum+spec*rsratio+(1|run),family="binomial")
glm21=glmer(disl~heig+spec+rsratio+shear:shdum+spec:shear:shdum+(1|run),family="binomial")
glm22=glmer(disl~heig+spec+rsratio+spec*heig+spec*rsratio+(1|run),family="binomial")
glm23=glmer(disl~heig+spec+shear:shdum+spec*heig+spec:shear:shdum+(1|run),family="binomial")
glm24=glmer(disl~spec+dens+rsratio+shear:shdum+spec*rsratio+(1|run),family="binomial")
glm25=glmer(disl~spec+dens+rsratio+shear:shdum+spec:shear:shdum+(1|run),family="binomial")
glm26=glmer(disl~spec+rsratio+shear:shdum+spec*rsratio+spec:shear:shdum+(1|run),family="binomial")

glm27=glmer(disl~heig+spec+dens+rsratio+(1|run),family="binomial")
glm28=glmer(disl~heig+spec+dens+shear:shdum+(1|run),family="binomial")
glm29=glmer(disl~heig+spec+dens+spec*heig+(1|run),family="binomial")
glm30=glmer(disl~heig+spec+rsratio+shear:shdum+(1|run),family="binomial")
glm31=glmer(disl~heig+spec+rsratio+spec*heig+(1|run),family="binomial")
glm32=glmer(disl~heig+spec+rsratio+spec*rsratio+(1|run),family="binomial")
glm33=glmer(disl~heig+spec+shear:shdum+spec*heig+(1|run),family="binomial")
glm34=glmer(disl~heig+spec+shear:shdum+spec:shear:shdum+(1|run),family="binomial")
glm35=glmer(disl~heig+dens+rsratio+shear:shdum+(1|run),family="binomial")
glm36=glmer(disl~spec+dens+rsratio+shear:shdum+(1|run),family="binomial")
glm37=glmer(disl~spec+dens+rsratio+spec*rsratio+(1|run),family="binomial")
glm38=glmer(disl~spec+dens+shear:shdum+spec:shear:shdum+(1|run),family="binomial")
glm39=glmer(disl~spec+rsratio+shear:shdum+spec*rsratio+(1|run),family="binomial")
glm40=glmer(disl~spec+rsratio+shear:shdum+spec:shear:shdum+(1|run),family="binomial")

glm41=glmer(disl~heig+spec+dens+(1|run),family="binomial")
glm42=glmer(disl~heig+spec+rsratio+(1|run),family="binomial")
glm43=glmer(disl~heig+spec+shear:shdum+(1|run),family="binomial")
glm44=glmer(disl~heig+spec+spec*heig+(1|run),family="binomial")
glm45=glmer(disl~heig+dens+rsratio+(1|run),family="binomial")
glm46=glmer(disl~heig+dens+shear:shdum+(1|run),family="binomial")
glm47=glmer(disl~heig+rsratio+shear:shdum+(1|run),family="binomial")
glm48=glmer(disl~spec+dens+rsratio+(1|run),family="binomial")
glm49=glmer(disl~spec+dens+shear:shdum+(1|run),family="binomial")
glm50=glmer(disl~spec+rsratio+shear:shdum+(1|run),family="binomial")
glm51=glmer(disl~spec+rsratio+spec*rsratio+(1|run),family="binomial")
glm52=glmer(disl~spec+shear:shdum+spec:shear:shdum+(1|run),family="binomial")
glm53=glmer(disl~dens+rsratio+shear:shdum+(1|run),family="binomial")

glm54=glmer(disl~heig+spec+(1|run),family="binomial")
glm55=glmer(disl~heig+dens+(1|run),family="binomial")
glm56=glmer(disl~heig+rsratio+(1|run),family="binomial")
glm57=glmer(disl~heig+shear:shdum+(1|run),family="binomial")
glm58=glmer(disl~spec+dens+(1|run),family="binomial")
glm59=glmer(disl~spec+rsratio+(1|run),family="binomial")
glm60=glmer(disl~spec+shear:shdum+(1|run),family="binomial")
glm61=glmer(disl~dens+rsratio+(1|run),family="binomial")
glm62=glmer(disl~dens+shear:shdum+(1|run),family="binomial",control = glmerControl(optimizer = "bobyqa"))
glm63=glmer(disl~rsratio+shear:shdum+(1|run),family="binomial")

glm64=glmer(disl~heig+(1|run),family="binomial")
glm65=glmer(disl~spec+(1|run),family="binomial")
glm66=glmer(disl~dens+(1|run),family="binomial")
glm67=glmer(disl~rsratio+(1|run),family="binomial")
glm68=glmer(disl~shear:shdum+(1|run),family="binomial")
glm69=glmer(disl~1+(1|run),family="binomial")

AICtab(glm0,glm1,glm2,glm3,glm4,glm5,glm6,glm7,glm8,glm9,glm10,glm11,glm12,glm13,
glm14,glm15,glm16,glm17,glm18,glm19,glm20,glm21,glm22,glm23,glm24,glm25,glm26,glm27,glm28,glm29,glm30,
glm31,glm32,glm33,glm34,glm35,glm36,glm37,glm38,glm39,glm40,glm41,glm42,glm43,glm44,glm45,glm46,glm47,glm48,
glm49,glm50,glm51,glm52,glm53,glm54,glm55,glm56,glm57,glm58,glm59,glm60,glm61,glm62,glm63,glm64,glm65,glm66,
glm67,glm68,glm69,weights=T,delta=T,base=T,sort=T)

      AIC   dAIC  df weight
glm39 403.3   0.0 6  0.3289
glm26 404.5   1.2 7  0.1798
glm20 405.2   1.9 7  0.1243
glm10 406.2   2.9 8  0.0760
glm12 406.4   3.2 8  0.0678
glm24 406.7   3.4 8  0.0600
glm4  407.2   3.9 9  0.0462
glm13 407.6   4.3 9  0.0387
glm6  408.7   5.4 9  0.0223
glm3  409.5   6.3 10 0.0144
glm1  409.8   6.5 10 0.0125
glm0  410.5   7.2 11 0.0089
glm51 412.0   8.7 5  0.0042
glm37 413.0   9.7 7  0.0026
glm32 413.9  10.6 6  0.0016
glm68 414.0  10.7 3  0.0015
glm22 414.5  11.2 7  0.0012
glm16 414.6  11.3 8  0.0012
glm63 414.7  11.4 4  0.0011
glm8  415.1  11.8 9  <0.001
glm60 415.4  12.1 4  <0.001
glm57 415.7  12.4 4  <0.001
glm50 416.2  12.9 5  <0.001
glm47 416.6  13.3 5  <0.001
glm52 416.9  13.6 5  <0.001
glm43 417.1  13.8 5  <0.001
glm62 417.4  14.1 5  <0.001
glm40 417.7  14.4 6  <0.001
glm53 418.1  14.8 6  <0.001
glm30 418.1  14.8 6  <0.001
glm49 418.6  15.3 6  <0.001
glm34 418.6  15.3 6  <0.001
glm33 419.1  15.8 6  <0.001
glm46 419.2  15.9 6  <0.001
glm36 419.3  16.1 7  <0.001
glm21 419.6  16.3 7  <0.001
glm38 419.6  16.3 7  <0.001
glm19 420.0  16.7 7  <0.001
glm35 420.0  16.8 7  <0.001
glm28 420.3  17.0 7  <0.001
glm25 420.4  17.1 8  <0.001
glm23 420.5  17.2 7  <0.001
glm14 421.3  18.0 8  <0.001
glm18 421.3  18.0 8  <0.001
glm11 421.5  18.2 8  <0.001
glm69 421.9  18.6 2  <0.001
glm7  422.3  19.0 9  <0.001
glm17 422.3  19.0 8  <0.001
glm67 422.3  19.1 3  <0.001
glm66 422.9  19.6 4  <0.001
glm5  423.2  20.0 9  <0.001
glm9  423.3  20.0 9  <0.001
glm64 423.4  20.1 3  <0.001
glm61 423.4  20.1 5  <0.001
glm65 423.6  20.3 3  <0.001
glm58 423.8  20.6 5  <0.001
glm55 423.9  20.6 5  <0.001
glm56 424.2  20.9 4  <0.001
glm59 424.2  20.9 4  <0.001
glm2  424.2  20.9 10 <0.001
glm48 424.5  21.3 6  <0.001
glm41 424.8  21.5 6  <0.001
glm45 424.8  21.6 6  <0.001
glm54 425.2  21.9 4  <0.001
glm27 425.9  22.6 7  <0.001
glm42 426.0  22.8 5  <0.001
glm29 426.7  23.5 7  <0.001
glm44 427.1  23.9 5  <0.001
glm15 427.8  24.5 8  <0.001
glm31 427.9  24.6 6  <0.001

#mixed linear model with no scour depth and ratio interception
patch2<-read.csv("plant_all.csv")
disl<-as.factor(ifelse(patch2$status_def=="dl","1","0"))
run<-patch2$run
spec<-patch2$Species
heig<-patch2$hei_cm
root<-patch2$root.length
scor<-patch2$sed_diff
dens<-patch2$density
rsratio<-scor/root
shear<-patch2$shear_stress
shdum<-patch2$sheardummy 

library(bbmle)
library(lme4)

glm1=glmer(disl~heig+spec+dens+rsratio+shear:shdum+spec*heig+(1|run),family="binomial")
glm2=glmer(disl~heig+spec+dens+rsratio+shear:shdum+(1|run),family="binomial")
glm3=glmer(disl~heig+spec+dens+rsratio+spec*heig+(1|run),family="binomial")
glm4=glmer(disl~heig+spec+dens+shear:shdum+spec*heig+(1|run),family="binomial")
glm5=glmer(disl~heig+spec+rsratio+shear:shdum+spec*heig+(1|run),family="binomial")

glm6=glmer(disl~heig+spec+dens+rsratio+(1|run),family="binomial")
glm7=glmer(disl~heig+spec+dens+shear:shdum+(1|run),family="binomial")
glm8=glmer(disl~heig+spec+dens+spec*heig+(1|run),family="binomial")
glm9=glmer(disl~heig+spec+rsratio+shear:shdum+(1|run),family="binomial")
glm10=glmer(disl~heig+spec+rsratio+spec*heig+(1|run),family="binomial")
glm11=glmer(disl~heig+spec+shear:shdum+spec*heig+(1|run),family="binomial")
glm12=glmer(disl~heig+dens+rsratio+shear:shdum+(1|run),family="binomial")
glm13=glmer(disl~spec+dens+rsratio+shear:shdum+(1|run),family="binomial")

glm14=glmer(disl~heig+spec+dens+(1|run),family="binomial")
glm15=glmer(disl~heig+spec+rsratio+(1|run),family="binomial")
glm16=glmer(disl~heig+spec+shear:shdum+(1|run),family="binomial")
glm17=glmer(disl~heig+spec+spec*heig+(1|run),family="binomial")
glm18=glmer(disl~heig+dens+rsratio+(1|run),family="binomial")
glm19=glmer(disl~heig+dens+shear:shdum+(1|run),family="binomial")
glm20=glmer(disl~heig+rsratio+shear:shdum+(1|run),family="binomial")
glm21=glmer(disl~spec+dens+rsratio+(1|run),family="binomial")
glm22=glmer(disl~spec+dens+shear:shdum+(1|run),family="binomial")
glm23=glmer(disl~spec+rsratio+shear:shdum+(1|run),family="binomial")
glm24=glmer(disl~dens+rsratio+shear:shdum+(1|run),family="binomial")

glm25=glmer(disl~heig+spec+(1|run),family="binomial")
glm26=glmer(disl~heig+dens+(1|run),family="binomial")
glm27=glmer(disl~heig+rsratio+(1|run),family="binomial")
glm28=glmer(disl~heig+shear:shdum+(1|run),family="binomial")
glm29=glmer(disl~spec+dens+(1|run),family="binomial")
glm30=glmer(disl~spec+rsratio+(1|run),family="binomial")
glm31=glmer(disl~spec+shear:shdum+(1|run),family="binomial")
glm32=glmer(disl~dens+rsratio+(1|run),family="binomial")
glm33=glmer(disl~dens+shear:shdum+(1|run),family="binomial")
glm34=glmer(disl~rsratio+shear:shdum+(1|run),family="binomial")

glm35=glmer(disl~heig+(1|run),family="binomial")
glm36=glmer(disl~spec+(1|run),family="binomial")
glm37=glmer(disl~dens+(1|run),family="binomial")
glm38=glmer(disl~rsratio+(1|run),family="binomial")
glm39=glmer(disl~shear:shdum+(1|run),family="binomial")
glm40=glmer(disl~1+(1|run),family="binomial")

AICtab(glm0,glm1,glm2,glm3,glm4,glm5,glm6,glm7,glm8,glm9,glm10,glm11,glm12,glm13,
glm14,glm15,glm16,glm17,glm18,glm19,glm20,glm21,glm22,glm23,glm24,glm25,glm26,glm27,glm28,glm29,glm30,
glm31,glm32,glm33,glm34,glm35,glm36,glm37,glm38,glm39,glm40,weights=T,delta=T,base=T,sort=T)

      AIC   dAIC  df weight
glm0  410.5   0.0 11 0.5606
glm39 414.0   3.5 3  0.0972
glm34 414.7   4.2 4  0.0675
glm31 415.4   4.9 4  0.0485
glm28 415.7   5.2 4  0.0414
glm23 416.2   5.7 5  0.0323
glm20 416.6   6.1 5  0.0261
glm16 417.1   6.6 5  0.0206
glm33 417.4   6.9 5  0.0179
glm24 418.1   7.6 6  0.0126
glm9  418.1   7.6 6  0.0125
glm22 418.6   8.1 6  0.0100
glm11 419.1   8.6 6  0.0076
glm19 419.2   8.7 6  0.0072
glm13 419.3   8.8 7  0.0067
glm5  420.0   9.5 7  0.0048
glm12 420.0   9.6 7  0.0047
glm7  420.3   9.8 7  0.0042
glm2  421.3  10.8 8  0.0026
glm40 421.9  11.4 2  0.0019
glm4  422.3  11.8 8  0.0015
glm38 422.3  11.8 3  0.0015
glm37 422.9  12.4 4  0.0011
glm1  423.2  12.7 9  <0.001
glm35 423.4  12.9 3  <0.001
glm32 423.4  12.9 5  <0.001
glm36 423.6  13.1 3  <0.001
glm29 423.8  13.3 5  <0.001
glm26 423.9  13.4 5  <0.001
glm27 424.2  13.7 4  <0.001
glm30 424.2  13.7 4  <0.001
glm21 424.5  14.0 6  <0.001
glm14 424.8  14.3 6  <0.001
glm18 424.8  14.4 6  <0.001
glm25 425.2  14.7 4  <0.001
glm6  425.9  15.4 7  <0.001
glm15 426.0  15.5 5  <0.001
glm8  426.7  16.2 7  <0.001
glm17 427.1  16.6 5  <0.001
glm3  427.8  17.3 8  <0.001
glm10 427.9  17.4 6  <0.001

> summary(glm0)
Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: disl ~ spec + heig + dens + rsratio + shear:shdum + spec * heig +  
    spec * rsratio + spec:shear:shdum + (1 | run)
Control: glmerControl(optimizer = "bobyqa")

     AIC      BIC   logLik deviance df.resid 
   410.5    453.5   -194.2    388.5      359 

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.1570 -0.6434 -0.2142  0.8003 10.9722 

Random effects:
 Groups Name        Variance Std.Dev.
 run    (Intercept) 1.26     1.123   
Number of obs: 370, groups:  run, 10

Fixed effects:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)              -2.84567    1.79142  -1.589 0.112173    
spectamarisk              1.90809    1.72005   1.109 0.267291    
heig                      0.01090    0.01894   0.575 0.564958    
densb                     1.11285    1.51823   0.733 0.463564    
densc                     0.31461    1.87145   0.168 0.866495    
rsratio                   2.46504    0.95187   2.590 0.009606 ** 
shear:shdum               0.18133    0.11592   1.564 0.117756    
spectamarisk:heig        -0.02710    0.02642  -1.026 0.305100    
spectamarisk:rsratio     -6.60066    1.84668  -3.574 0.000351 ***
spectamarisk:shear:shdum  0.15370    0.12907   1.191 0.233748    
---
Signif. codes:  0 ??***????????o??????????? 0.001 ??**????????o??????????? 0.01 ??*????????o??????????? 0.05 ??.????????o??????????? 0.1 ?? ????????o??????????? 1

Correlation of Fixed Effects:
            (Intr) spctmr heig   densb  densc  rsrati shr:sh spctmrsk:h
spectamarsk -0.401                                                     
heig        -0.559  0.499                                              
densb       -0.728 -0.110  0.087                                       
densc       -0.714  0.165  0.176  0.660                                
rsratio     -0.399  0.366  0.291  0.056  0.115                         
shear:shdum -0.625  0.103  0.044  0.627  0.580  0.025                  
spctmrsk:hg  0.290 -0.682 -0.681  0.057 -0.033 -0.200  0.079           
spctmrsk:rs  0.220 -0.474 -0.172 -0.015 -0.066 -0.514 -0.032  0.210    
spctmrsk:s:  0.048 -0.333  0.031  0.147 -0.060  0.022 -0.384 -0.094    
            spctmrsk:r
spectamarsk           
heig                  
densb                 
densc                 
rsratio               
shear:shdum           
spctmrsk:hg           
spctmrsk:rs           
spctmrsk:s: -0.085    
> summary(glm39)
Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: disl ~ shear:shdum + (1 | run)

     AIC      BIC   logLik deviance df.resid 
   414.0    425.7   -204.0    408.0      367 

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.2295 -0.7744 -0.1864  0.7418  5.3640 

Random effects:
 Groups Name        Variance Std.Dev.
 run    (Intercept) 1.612    1.27    
Number of obs: 370, groups:  run, 10

Fixed effects:
            Estimate Std. Error z value Pr(>|z|)   
(Intercept) -1.12764    0.55261  -2.041  0.04129 * 
shear:shdum  0.19037    0.07024   2.710  0.00672 **
---
Signif. codes:  0 ??***????????o??????????? 0.001 ??**????????o??????????? 0.01 ??*????????o??????????? 0.05 ??.????????o??????????? 0.1 ?? ????????o??????????? 1

Correlation of Fixed Effects:
            (Intr)
shear:shdum -0.502
> 

#glmer for all plants without shear stress and only scour depth (no ratio)
patch2<-read.csv("plant_all.csv")
disl<-as.factor(ifelse(patch2$status_def=="dl","1","0"))
run<-patch2$run
spec<-patch2$Species
heig<-patch2$hei_cm
root<-patch2$root.length
scor<-patch2$sed_diff
dens<-patch2$density


library(bbmle)
library(lme4)

glm1=glmer(disl~heig+spec+dens+scor+spec*heig+spec*scor+(1|run),family="binomial")

glm2=glmer(disl~heig+spec+dens+scor+spec*heig+(1|run),family="binomial")
glm3=glmer(disl~heig+spec+dens+scor+spec*scor+(1|run),family="binomial")
glm4=glmer(disl~heig+spec+scor+spec*heig+spec*scor+(1|run),family="binomial")

glm5=glmer(disl~heig+spec+dens+scor+(1|run),family="binomial")
glm6=glmer(disl~heig+spec+dens+spec*heig+(1|run),family="binomial")
glm7=glmer(disl~heig+spec+scor+spec*heig+(1|run),family="binomial")
glm8=glmer(disl~heig+spec+scor+spec*scor+(1|run),family="binomial")
glm9=glmer(disl~spec+dens+scor+spec*scor+(1|run),family="binomial")

glm10=glmer(disl~heig+spec+dens+(1|run),family="binomial")
glm11=glmer(disl~heig+spec+scor+(1|run),family="binomial")
glm12=glmer(disl~heig+spec+spec*heig+(1|run),family="binomial")
glm13=glmer(disl~heig+dens+scor+(1|run),family="binomial")
glm14=glmer(disl~spec+dens+scor+(1|run),family="binomial")
glm15=glmer(disl~spec+scor+spec*scor+(1|run),family="binomial")

glm16=glmer(disl~heig+spec+(1|run),family="binomial")
glm17=glmer(disl~heig+dens+(1|run),family="binomial")
glm18=glmer(disl~heig+scor+(1|run),family="binomial")
glm19=glmer(disl~spec+dens+(1|run),family="binomial")
glm20=glmer(disl~spec+scor+(1|run),family="binomial")
glm21=glmer(disl~dens+scor+(1|run),family="binomial")

glm22=glmer(disl~heig+(1|run),family="binomial")
glm23=glmer(disl~spec+(1|run),family="binomial")
glm24=glmer(disl~dens+(1|run),family="binomial")
glm25=glmer(disl~scor+(1|run),family="binomial")
glm26=glmer(disl~1+(1|run),family="binomial")



AICtab(glm1,glm2,glm3,glm4,glm5,glm6,glm7,glm8,glm9,glm10,glm11,glm12,glm13,
glm14,glm15,glm16,glm17,glm18,glm19,glm20,glm21,glm22,glm23,glm24,glm25,glm26,weights=T,delta=T,base=T,sort=T)


      AIC   dAIC  df weight
glm9  386.7   0.0 7  0.329 
glm15 386.9   0.2 5  0.293 
glm3  388.5   1.9 8  0.129 
glm8  388.7   2.0 6  0.120 
glm4  389.9   3.3 7  0.065 
glm1  389.9   3.3 9  0.064 
glm21 410.6  24.0 5  <0.001
glm25 411.1  24.5 3  <0.001
glm14 411.4  24.8 6  <0.001
glm13 412.5  25.9 6  <0.001
glm20 412.9  26.3 4  <0.001
glm18 413.1  26.5 4  <0.001
glm5  413.3  26.7 7  <0.001
glm11 414.9  28.3 5  <0.001
glm2  415.3  28.6 8  <0.001
glm7  416.9  30.2 6  <0.001
glm26 421.9  35.2 2  <0.001
glm24 422.9  36.2 4  <0.001
glm22 423.4  36.7 3  <0.001
glm23 423.6  36.9 3  <0.001
glm19 423.8  37.2 5  <0.001
glm17 423.9  37.2 5  <0.001
glm10 424.8  38.1 6  <0.001
glm16 425.2  38.5 4  <0.001
glm6  426.7  40.1 7  <0.001
glm12 427.1  40.5 5  <0.001

Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
 Family: binomial  ( logit )
Formula: disl ~ spec + dens + scor + spec * rsratio + (1 | run)

     AIC      BIC   logLik deviance df.resid 
   386.7    414.0   -186.3    372.7      363 

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-1.9697 -0.5198 -0.1644  0.7149 14.6912 

Random effects:
 Groups Name        Variance Std.Dev.
 run    (Intercept) 5.121    2.263   
Number of obs: 370, groups:  run, 10

Fixed effects:
                      Estimate Std. Error z value Pr(>|z|)    
(Intercept)           1.316728   1.447274   0.910  0.36293    
spectamarisk          6.690880   2.180032   3.069  0.00215 ** 
densb                -3.718208   1.705246  -2.180  0.02922 *  
densc                -2.603724   2.627208  -0.991  0.32166    
scor              -0.005811   0.040506  -0.143  0.88592    
spectamarisk:scor -0.461724   0.109637  -4.211 2.54e-05 ***
---
Signif. codes:  0 ??***?????o????? 0.001 ??**?????o????? 0.01 ??*?????o????? 0.05 ??.?????o????? 0.1 ?? ?????o????? 1

Correlation of Fixed Effects:
            (Intr) spctmr densb  densc  scor
spectamarsk -0.357                            
densb       -0.547 -0.226                     
densc       -0.469  0.141  0.294              
scor        -0.479  0.328  0.042  0.092       
spctmrsk:rs  0.201 -0.869  0.256 -0.045 -0.383


#no mixed effect and no shear stress
glm1=glm(disl~heig+spec+dens+scor+spec*heig+spec*scor,family="binomial")

glm2=glm(disl~heig+spec+dens+scor+spec*heig,family="binomial")
glm3=glm(disl~heig+spec+dens+scor+spec*scor,family="binomial")
glm4=glm(disl~heig+spec+scor+spec*heig+spec*scor,family="binomial")

glm5=glm(disl~heig+spec+dens+scor,family="binomial")
glm6=glm(disl~heig+spec+dens+spec*heig,family="binomial")
glm7=glm(disl~heig+spec+scor+spec*heig,family="binomial")
glm8=glm(disl~heig+spec+scor+spec*scor,family="binomial")
glm9=glm(disl~spec+dens+scor+spec*scor,family="binomial")

glm10=glm(disl~heig+spec+dens,family="binomial")
glm11=glm(disl~heig+spec+scor,family="binomial")
glm12=glm(disl~heig+spec+spec*heig,family="binomial")
glm13=glm(disl~heig+dens+scor,family="binomial")
glm14=glm(disl~spec+dens+scor,family="binomial")
glm15=glm(disl~spec+scor+spec*scor,family="binomial")

glm16=glm(disl~heig+spec,family="binomial")
glm17=glm(disl~heig+dens,family="binomial")
glm18=glm(disl~heig+scor,family="binomial")
glm19=glm(disl~spec+dens,family="binomial")
glm20=glm(disl~spec+scor,family="binomial")
glm21=glm(disl~dens+scor,family="binomial")

glm22=glm(disl~heig,family="binomial")
glm23=glm(disl~spec,family="binomial")
glm24=glm(disl~dens,family="binomial")
glm25=glm(disl~scor,family="binomial")
glm26=glm(disl~1,family="binomial")

Call:
glm(formula = disl ~ heig + spec + dens + scor + spec * scor, 
    family = "binomial")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3554  -0.8827  -0.5995   0.9928   2.4051  

Coefficients:
                   Estimate Std. Error z value Pr(>|z|)    
(Intercept)        3.678861   0.963297   3.819 0.000134 ***
heig              -0.036643   0.007381  -4.965 6.88e-07 ***
spectamarisk       0.033240   0.715494   0.046 0.962945    
densb             -2.231531   0.650453  -3.431 0.000602 ***
densc             -3.849507   0.757262  -5.083 3.71e-07 ***
scor              -0.005865   0.040348  -0.145 0.884436    
spectamarisk:scor -0.140769   0.062886  -2.238 0.025189 *  
---
Signif. codes:  0 ??***?????o????? 0.001 ??**?????o????? 0.01 ??*?????o????? 0.05 ??.?????o????? 0.1 ?? ?????o????? 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 486.66  on 369  degrees of freedom
Residual deviance: 418.71  on 363  degrees of freedom
AIC: 432.71

Number of Fisher Scoring iterations: 4





#####################################################
#glmer with scour ratio but no shear stress, root length was added in the model
patch3<-read.csv("plant_all.csv")
disl<-as.factor(ifelse(patch3$status_def=="dl","1","0"))
run<-patch3$run
spec<-patch3$Species
heig<-patch3$hei_cm
root<-patch3$root.length
scor<-patch3$sed_diff
dens<-patch3$density2

library(bbmle)
library(lme4)

glm1=glmer(disl~heig+spec+dens+scor+spec*heig+spec*scor+(1|run),family="binomial")
glm2=glmer(disl~spec+dens+scor+root+spec*scor+spec*root+(1|run),family="binomial")

glm3=glmer(disl~heig+spec+dens+scor+spec*heig+(1|run),family="binomial")
glm4=glmer(disl~heig+spec+dens+scor+spec*scor+(1|run),family="binomial")
glm5=glmer(disl~heig+spec+scor+spec*heig+spec*scor+(1|run),family="binomial")
glm6=glmer(disl~spec+dens+scor+root+spec*scor+(1|run),family="binomial")
glm7=glmer(disl~spec+dens+scor+root+spec*root+(1|run),family="binomial")
glm8=glmer(disl~spec+scor+root+spec*scor+spec*root+(1|run),family="binomial")

glm9=glmer(disl~heig+spec+dens+scor+(1|run),family="binomial")
glm10=glmer(disl~heig+spec+dens+spec*heig+(1|run),family="binomial")
glm11=glmer(disl~heig+spec+scor+spec*heig+(1|run),family="binomial")
glm12=glmer(disl~heig+spec+scor+spec*scor+(1|run),family="binomial")
glm13=glmer(disl~spec+dens+scor+root+(1|run),family="binomial")
glm14=glmer(disl~spec+dens+scor+spec*scor+(1|run),family="binomial")
glm15=glmer(disl~spec+dens+root+spec*root+(1|run),family="binomial")
glm16=glmer(disl~spec+scor+root+spec*scor+(1|run),family="binomial")
glm17=glmer(disl~spec+scor+root+spec*root+(1|run),family="binomial")

glm18=glmer(disl~heig+spec+dens+(1|run),family="binomial")
glm19=glmer(disl~heig+spec+scor+(1|run),family="binomial")
glm20=glmer(disl~heig+spec+spec*heig+(1|run),family="binomial")
glm21=glmer(disl~heig+dens+scor+(1|run),family="binomial")
glm22=glmer(disl~spec+dens+scor+(1|run),family="binomial")
glm23=glmer(disl~spec+dens+root+(1|run),family="binomial")
glm24=glmer(disl~spec+scor+root+(1|run),family="binomial")
glm25=glmer(disl~spec+scor+spec*scor+(1|run),family="binomial")
glm26=glmer(disl~spec+root+spec*root+(1|run),family="binomial")
glm27=glmer(disl~dens+scor+root+(1|run),family="binomial")

glm28=glmer(disl~heig+spec+(1|run),family="binomial")
glm29=glmer(disl~heig+dens+(1|run),family="binomial")
glm30=glmer(disl~heig+scor+(1|run),family="binomial")
glm31=glmer(disl~spec+dens+(1|run),family="binomial")
glm32=glmer(disl~spec+scor+(1|run),family="binomial")
glm33=glmer(disl~spec+root+(1|run),family="binomial")
glm34=glmer(disl~dens+scor+(1|run),family="binomial")
glm35=glmer(disl~dens+root+(1|run),family="binomial")
glm36=glmer(disl~scor+root+(1|run),family="binomial")

glm37=glmer(disl~heig+(1|run),family="binomial")
glm38=glmer(disl~spec+(1|run),family="binomial")
glm39=glmer(disl~dens+(1|run),family="binomial")
glm40=glmer(disl~scor+(1|run),family="binomial")
glm41=glmer(disl~root+(1|run),family="binomial")
glm42=glmer(disl~1+(1|run),family="binomial")

AICtab(glm1,glm2,glm3,glm4,glm5,glm6,glm7,glm8,glm9,glm10,glm11,glm12,glm13,
glm14,glm15,glm16,glm17,glm18,glm19,glm20,glm21,glm22,glm23,glm24,glm25,glm26,glm27,glm28,glm29,glm30,
glm31,glm32,glm33,glm34,glm35,glm36,glm37,glm38,glm39,glm40,glm41,glm42,weights=T,delta=T,base=T,sort=T)


 Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: disl ~ spec + dens + scor + root + spec * scor + (1 | run)

     AIC      BIC   logLik deviance df.resid 
   382.0    413.3   -183.0    366.0      362 

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.0679 -0.6034 -0.1541  0.5813 10.0943 

Random effects:
 Groups Name        Variance Std.Dev.
 run    (Intercept) 4.7      2.168   
Number of obs: 370, groups:  run, 10

Fixed effects:
                   Estimate Std. Error z value Pr(>|z|)    
(Intercept)        2.349358   1.498592   1.568  0.11695    
spectamarisk       6.535005   2.204315   2.965  0.00303 ** 
densb             -3.778505   1.650604  -2.289  0.02207 *  
densc             -2.653298   2.538667  -1.045  0.29595    
scor               0.009615   0.041711   0.231  0.81769    
root              -0.034331   0.013404  -2.561  0.01043 *  
spectamarisk:scor -0.432986   0.109200  -3.965 7.34e-05 ***
---
Signif. codes:  0 ??***?????o????? 0.001 ??**?????o????? 0.01 ??*?????o????? 0.05 ??.?????o????? 0.1 ?? ?????o????? 1

Correlation of Fixed Effects:
            (Intr) spctmr densb  densc  scor   root  
spectamarsk -0.364                                   
densb       -0.529 -0.224                            
densc       -0.463  0.157  0.298                     
scor        -0.433  0.337  0.037  0.097              
root        -0.270 -0.028  0.043  0.010 -0.145       
spctmrsk:sc  0.222 -0.865  0.256 -0.053 -0.384 -0.039


##glm function (no mixed effects)

glm1=glm(disl~heig+spec+dens+scor+spec*heig+spec*scor,family="binomial")
glm2=glm(disl~spec+dens+scor+root+spec*scor+spec*root,family="binomial")

glm3=glm(disl~heig+spec+dens+scor+spec*heig,family="binomial")
glm4=glm(disl~heig+spec+dens+scor+spec*scor,family="binomial")
glm5=glm(disl~heig+spec+scor+spec*heig+spec*scor,family="binomial")
glm6=glm(disl~spec+dens+scor+root+spec*scor,family="binomial")
glm7=glm(disl~spec+dens+scor+root+spec*root,family="binomial")
glm8=glm(disl~spec+scor+root+spec*scor+spec*root,family="binomial")

glm9=glm(disl~heig+spec+dens+scor,family="binomial")
glm10=glm(disl~heig+spec+dens+spec*heig,family="binomial")
glm11=glm(disl~heig+spec+scor+spec*heig,family="binomial")
glm12=glm(disl~heig+spec+scor+spec*scor,family="binomial")
glm13=glm(disl~spec+dens+scor+root,family="binomial")
glm14=glm(disl~spec+dens+scor+spec*scor,family="binomial")
glm15=glm(disl~spec+dens+root+spec*root,family="binomial")
glm16=glm(disl~spec+scor+root+spec*scor,family="binomial")
glm17=glm(disl~spec+scor+root+spec*root,family="binomial")

glm18=glm(disl~heig+spec+dens,family="binomial")
glm19=glm(disl~heig+spec+scor,family="binomial")
glm20=glm(disl~heig+spec+spec*heig,family="binomial")
glm21=glm(disl~heig+dens+scor,family="binomial")
glm22=glm(disl~spec+dens+scor,family="binomial")
glm23=glm(disl~spec+dens+root,family="binomial")
glm24=glm(disl~spec+scor+root,family="binomial")
glm25=glm(disl~spec+scor+spec*scor,family="binomial")
glm26=glm(disl~spec+root+spec*root,family="binomial")
glm27=glm(disl~dens+scor+root,family="binomial")

glm28=glm(disl~heig+spec,family="binomial")
glm29=glm(disl~heig+dens,family="binomial")
glm30=glm(disl~heig+scor,family="binomial")
glm31=glm(disl~spec+dens,family="binomial")
glm32=glm(disl~spec+scor,family="binomial")
glm33=glm(disl~spec+root,family="binomial")
glm34=glm(disl~dens+scor,family="binomial")
glm35=glm(disl~dens+root,family="binomial")
glm36=glm(disl~scor+root,family="binomial")

glm37=glm(disl~heig,family="binomial")
glm38=glm(disl~spec,family="binomial")
glm39=glm(disl~dens,family="binomial")
glm40=glm(disl~scor,family="binomial")
glm41=glm(disl~root,family="binomial")
glm42=glm(disl~1,family="binomial")

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)        3.89705    0.97266   4.007 6.16e-05 ***
spectamarisk       0.18842    0.72329   0.261 0.794472    
densb             -2.26528    0.64244  -3.526 0.000422 ***
densc             -3.47603    0.73549  -4.726 2.29e-06 ***
scor               0.01410    0.04051   0.348 0.727846    
root              -0.05688    0.01075  -5.292 1.21e-07 ***
spectamarisk:scor -0.12302    0.06343  -1.939 0.052454 .  
---
Signif. codes:  0 ??***?????o????? 0.001 ??**?????o????? 0.01 ??*?????o????? 0.05 ??.?????o????? 0.1 ?? ?????o????? 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 486.66  on 369  degrees of freedom
Residual deviance: 414.91  on 363  degrees of freedom
AIC: 428.91

Number of Fisher Scoring iterations: 4


      AIC   dAIC  df weight
glm6  428.9   0.0 7  0.3290
glm23 430.0   1.1 5  0.1859
glm2  430.7   1.8 8  0.1312
glm13 430.8   1.9 6  0.1269
glm15 432.0   3.1 6  0.0684
glm4  432.7   3.8 7  0.0492
glm7  432.8   3.9 7  0.0467




#plot conditional plot for scour/root ratio for patches
patch_sed<-read.csv("plant_patch_query_sed.csv")

patch2<-subset(patch_sed,!status_equ=="dl")
disl<-as.factor(ifelse(patch2$status_def=="dl","1","0"))
spec<-patch2$species
heig<-patch2$hei_cm
root<-patch2$root.length
scor<-patch2$sed_diff
dens<-patch2$density
srratio<-scor/root
massratio<-patch2$above_g/patch2$below_g
stpw<-patch2$stream_power

print(srratio)

glm1=glm(disl~srratio+dens+spec+heig,family="binomial")
step <- stepAIC(glm1, direction="both")
summary(glm1)

layout(matrix(1:2,ncol=2), widths = 1, respect = FALSE)
par(las=1)
par(mar=c(4,4, 1, 0) )
root.cw <- cdplot(disl~srratio,subset=(patch2$species=="cottonwood"),plot=F)
root.tm <- cdplot(disl~srratio,subset=(patch2$species=="tamarisk"),plot=F)
plot(1, xlim=c(0,2),ylim=c(0,1),type="n", axes=T, xlab="Scour depth / root length ratio", ylab="Probability density of plant dislodgement",cex.lab=1)
lines(0:2, 1-root.cw[[1]](0:2), col = "Dark Green",lty=2,lwd=2)
lines(0:2, 1-root.tm[[1]](0:2), col = "red",lty=1,lwd=2)
legend(1,0.5,legend=c("Cottonwood", "Tamarisk"),lty=c(2,1),lwd=2,bty="n",cex=1,col=c("dark green","red"))
text(0,1,"A",cex=1.2)

par(mar=c(4,0, 1, 1) )
hei.cw.b <- cdplot(disl~heig,subset=(patch2$species=="cottonwood"),plot=F)
hei.tm.b <- cdplot(disl~heig,subset=(patch2$species=="tamarisk"),plot=F)
plot(1, xlim=c(0,100),ylim=c(0,1),type="n",yaxt='n', ylab="", xlab="Plant height (cm)", cex.lab=1)
lines(5:100, 1-hei.cw.b[[1]](5:100), col = "dark green",lty=2,lwd=2)
lines(5:100, 1-hei.tm.b[[1]](5:100), col = "red",lty=1,lwd=2)
text(0,1,"B",cex=1.2)


#plant patch query

patch<-read.csv("plant_patch_query_sed.csv")
table(patch$status_equ)
table(patch$status_def)
table(patch$species)
123/(357-4)
357+114
33/357
210+114
br <-subset(patch,status_equ=="br")
disdef <-subset(patch,status_def=="dl")
table(disdef$run)

run3<-subset(patch,run==3)
run4<-subset(patch,run==4)
run5<-subset(patch,run==5)
run6<-subset(patch,run==6)
run8<-subset(patch,run==8)

t.test(run3$hei_cm,run5$hei_cm)
t.test(run4$hei_cm,run8$hei_cm)
t.test(run3$hei_cm,run4$hei_cm)
t.test(run5$hei_cm,run8$hei_cm)
53-22
50-24
114+60
12/174
summary(lm(time_elapse~species, data=disdef))

mean(run6$hei_cm)
sd(run6$hei_cm)
run6nodot<-read.csv("plant_run6nodot_structure.csv")
b<-append(run6$hei_cm,run6nodot$height_cm)
mean(b)
sd(b)

#plot plant dislodgement location
patch<-read.csv("plant_patch_query_sed.csv")
table(patch$run)
patch2<-patch[,c("location_m","status_def")]
nodot<-read.csv("plant_run6nodot_query.csv")
nodot2<-nodot[,c("location_m","status_def")]
patch3<-rbind(patch2,nodot2)
run<-rep(c(6,7,8,10,9,10),c(65,79,76,71,57,111))
patch4<-cbind(patch3,run)
patch_dl1<-subset(patch4,status_def=="dl")
patch_rm<-subset(patch4,!status_def=="dl")
patch_dl1<-patch_dl1[with(patch_dl1, order(run)), ]

patch_dl2<-cbind(patch_dl1,"count"=rep(c(0.25,0.05),c(110,51)))
patch_dl<-aggregate(patch_dl2$count, by=list(patch_dl2$run,patch_dl2$location_m),FUN=sum)
colnames(patch_dl)<-c("run","location","count")

data_indi<-read.csv("plant_individualrun.csv")
indi1<-data_indi[2:23,c(7,19)]
#run<-rep(c(1,2,3,4,5),c(4,4,4,5,5))
#indi2<-cbind(indi1,run)
#indi_dl<-subset(indi2,status=="y")
#indi_rm<-subset(indi2,status=="n")

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


##plot boxplot

patch_sed<-read.csv("plant_patch_query_sed.csv")
patch5<-subset(patch_sed,!status_equ=="dl")
patch5<-droplevels(subset(patch5,status_def=="dl"))
patch6<-subset(patch5,density=="a")
patch7<-subset(patch5,density=="b")
spa_ratio<-patch6$sed_diff/patch6$root.length
den_ratio<-patch7$sed_diff/patch7$root.length

data_indi<-read.csv("plant_individualrun.csv")
indi5<-data_indi[2:23,c("run","species","status","root_len","total.scour")]
indi5<- droplevels(subset(indi5,status=="y"))
indi_ratio<-(indi5$total.scour/10)/indi5$root_len

par(mar=c(8,4, 1, 1) )
plot(1,type="n", xlim=c(0.5,7.5),ylim=c(0,1.1),xaxt='n',ylab='Scour depth / root length',xlab='',las=1)
boxplot(indi_ratio~species,data=indi5,yaxt='n',las=2, add=T)
boxplot(spa_ratio~size*species,data=patch6,yaxt='n',las=2, add=T, at=3:6)
boxplot(den_ratio,data=patch7,yaxt='n',xaxt='n', add=T, at=7)

axis(1, at=7,labels=c("small.cottonwood"), las=2, cex.axis=1, tck=-.02)
abline(v=2.5,lty=2)
abline(v=6.5,lty=2)
text(1,1,"Individual")
text(4,1,"Sparse patch")
text(7.2,1,"Dense patch")

#plot shear stress
stress<-read.csv("RFS_shearstress.csv")

par(mar=c(4, 5, 1, 1) )
boxplot(shear.stress~name,data=stress, xlim=c(0.5,6.5),ylim=c(-5,32),xaxt='n',
       ylab=expression(paste("Shear stress ( N/ ",m^2,")")),xlab='',las=1)
abline(v=1.5,lty=2)
abline(v=5.5,lty=2)
axis(1, at=c(1:6),labels=c("Mixed species","Short.CW","Tall.CW","Short.TM","Tall.TM","Short.CW"), las=1, cex.axis=1, tck=-.02)

text(1,32,"Individual")
text(3.5,32,"Sparse patch")
text(6.2,32,"Dense patch")

pairwise.t.test(stress$shear.stress, stress$name, p.adjust="bonferroni", pool.sd = T)

#pair plots

patch_sed<-read.csv("plant_patch_query_sed.csv")
patch10<-subset(patch_sed,!status_equ=="dl")
patch10$stat<-as.factor(ifelse(patch10$status_def=="dl","Uprooted","Intact"))
patch10$ratio<-patch10$sed_diff/patch10$root.length
patch11<-subset(patch10,density=="a")
patch12<-subset(patch10,density=="b")

data_indi<-read.csv("plant_individualrun.csv")
indi10<-droplevels(data_indi[2:23,])
indi10$ratio<-(indi10$total.scour/10)/indi10$root_len

library(psych)

pairs.panels(~species+log(leaf_FA)+log(ratio)+log(shear.stress)+log(hei_cm)+log(root_len)+log(dia_mm)+log(root_wt_g)+log(above_wt_g)+status,data=indi10,
labels=c("species","FA","scour/root","shear \nstress","height","root length","diameter","root mass","leaf mass","status"),gap=1,cex.labels=1)

pairs.panels(~species+log(hei_cm)+density+log(ratio)+log(dia_mm)+log(root.length)+log(above_g)+log(below_g)+stat,data=patch10,
labels=c("species","heigh","density","scour/root","diameter","root length","leaf mass","root mass","status"),gap=0,cex.labels=1.2)

pairs.panels(~species+log(hei_cm)+log(ratio)+log(dia_mm)+log(root.length)+log(above_g)+log(below_g)+stat,data=patch11,
labels=c("species","heigh","scour/root","diameter","root length","leaf mass","root mass","status"),gap=0,cex.labels=1.2)

pairs.panels(~log(hei_cm)+log(ratio)+log(dia_mm)+log(root.length)+log(above_g)+log(below_g)+stat,data=patch12,
labels=c("heigh","scour/root","diameter","root length","leaf mass","root mass","status"),gap=0,cex.labels=1.2)

# calculate last synthesis graph
indi_org<- read.csv("plant_individualrun.csv")
indi<-droplevels(indi_org[2:23,])
indi$con<-1
indi1<-subset(indi,hei_cm<20)
tapply(indi1$con,list(indi1$species,indi1$status_deg),sum)

patch_sed<-read.csv("plant_patch_query_sed.csv")
patch9<-subset(patch_sed,!status_equ=="dl")
patch9$con<-1
patch10<-subset(patch9,hei_cm<20)
patch11<-subset(patch9,hei_cm>20)
tapply(patch10$con,list(patch10$species,patch10$status_def),sum)
tapply(patch11$con,list(patch11$species,patch11$status_def),sum)

patch12<-read.csv("plant_run6nodot_query.csv")
patch12$con<-1
patch13<-subset(patch12,height_cm<20)
patch14<-subset(patch12,height_cm>20)
tapply(patch13$con,list(patch13$species,patch13$status_def),sum)
tapply(patch14$con,list(patch14$species,patch14$status_def),sum)

data<-read.csv("synthesis_plantdata.csv")
data_d<-subset(data[1:4,])
data_b<-subset(data[5:8,])
par(mar=c(2,4, 1, 1) )

layout(matrix(1:2, ncol = 1), widths = 1, respect = FALSE)

barplot(data_d$percentage,y=c(0,0.8), ylab="Percentage of dislogement",
   xlab='',las=1,names.arg=c("Short.CW","Short.TM","Tall.CW","Tall.TM"))
abline(v=2.5,lty=2)

barplot(data_b$percentage,y=c(0,0.8), ylab="Percentage of burial",
   xlab='',las=1,names.arg=c("Short.CW","Short.TM","Tall.CW","Tall.TM"))
abline(v=2.5,lty=2)

