######################################################################################
# Create a case-control study for HEI in men for colorrectal cancer
# for use in spring Stat 689 in Cox regression
######################################################################################
#
#
# Clear the workspace
rm(list = ls())
######################################################################################
# Set the working directory
######################################################################################
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\HEI_AARP")
######################################################################################
# Get the libraries
######################################################################################
library(mgcv)
library(HRW)
######################################################################################
# Read in the data 
######################################################################################
zz = read.csv(file="Stat689_alldata_nottobeshown.csv",header=TRUE,sep=",")
######################################################################################
# Check out a gender effect
######################################################################################
colorectal = zz$colorectal_cancer
Rlindata = gam(personyrs_cancer ~ SEX + hei_f_whl + hei_f_total
               + hei_g_whl + hei_g_total 
               + hei_v_total + hei_v_dol + hei_meatbeans + hei_oils
               + hei_sodium   + hei_sfat + hei_d_total+ + hei_sofaas
               + agegrp2 + agegrp3 + agegrp4 + agegrp5
               + race2 + race3 + race9
               + bmicat2 + bmicat3 + bmicat9
               + educat2 + educat3 + educat9
               + act2 + act3 + act4 + act5 + act9,
               weights=colorectal,family=cox.ph,zz)
summary(Rlindata)
######################################################################################
# Work only with men
######################################################################################
thesex = 0
thedata = zz[zz$SEX==thesex,]
colorectal = thedata$colorectal_cancer
sum(colorectal)
unique(colorectal)

######################################################################################
# Check out the individual component scores
######################################################################################
colorectal = thedata$colorectal_cancer
Rlindata = gam(personyrs_cancer ~ hei_f_whl + hei_f_total
               + hei_g_whl + hei_g_total 
               + hei_v_total + hei_v_dol + hei_meatbeans + hei_oils
               + hei_sodium   + hei_sfat + hei_d_total+ + hei_sofaas
               + agegrp2 + agegrp3 + agegrp4 + agegrp5
               + race2 + race3 + race9
               + bmicat2 + bmicat3 + bmicat9
               + educat2 + educat3 + educat9
               + act2 + act3 + act4 + act5 + act9,
               weights=colorectal,family=cox.ph,thedata)



thedata$Fruit   = thedata$hei_f_whl  + thedata$hei_f_total
thedata$Grains  = thedata$hei_g_total  + thedata$hei_g_whl
thedata$Veggies = thedata$hei_v_dol + thedata$hei_v_total
######################################################################################
# Now try the HEI total score
# Do a histogram and I box plot
######################################################################################
thedata$hei_total = 100 * thedata$hei_total
pdf("Stat689_Analysis_Histogram.pdf")
hist(thedata$hei_total,main="HEI Total Score",xlab="Score")
dev.off()
pdf("Stat689_Analysis_Boxplot.pdf")
boxplot(thedata$hei_total,main="HEI Total Score")
dev.off()
######################################################################################
# Run a spline fit
######################################################################################
splineHEI_fit = gam(personyrs_cancer ~ s(hei_total)
               + agegrp2 + agegrp3 + agegrp4 + agegrp5
               + race2 + race3 + race9
               + bmicat2 + bmicat3 + bmicat9
               + educat2 + educat3 + educat9
               + act2 + act3 + act4 + act5 + act9,
               weights=colorectal,family=cox.ph,thedata)
summary(splineHEI_fit)
pdf("Stat689_Analysis_FunctionPlot.pdf")
plot(splineHEI_fit,select = 1,shade = TRUE, 
     cex.lab = 1.3,xlab = "HEI Total Score",
     ylab = "Log of Relative Risk: f(HEI Total Score)", shade.col = "palegreen",
     rug = FALSE)
dev.off()
######################################################################################
# Compare the spline fit with the linear fit
######################################################################################
linearHEI_fit = gam(personyrs_cancer ~ hei_total
                + agegrp2 + agegrp3 + agegrp4 + agegrp5
                + race2 + race3 + race9
                + bmicat2 + bmicat3 + bmicat9
                + educat2 + educat3 + educat9
                + act2 + act3 + act4 + act5 + act9,
                weights=colorectal,family=cox.ph,thedata)
summary(linearHEI_fit)
pp = anova(splineHEI_fit,linearHEI_fit,test="Chisq")
cat('The p-value for the spline = ',pp$'Pr(>Chi)'[2],"\n")
######################################################################################
# In the linear fit, compute the relative risk for being at 30 to that
# of being at 80
######################################################################################
RR30_to_80 = exp(linearHEI_fit$coefficients[1] * (30 - 80))
RR30_to_80
