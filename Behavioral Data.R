#----------------------------------------------------------------------------------#
# LOAD XLSX file generated for each patient, select the sessions of interest and do stats
#----------------------------------------------------------------------------------#

# Select patient and sessions:

subject='006'

# load libraries
library(xlsx)
library(plyr)
library(ggplot2)

#read data:

root2read <- paste0("~/Desktop/DBSTRD", subject, "/Final Datasets/New Final Datasets/")

data<-read.xlsx(file=paste(root2read,"DBSTRD", subject, "_all_trials.xlsx",sep=""),sheetIndex=1,header=TRUE)

# check the first few lines of data to see what you have

head(data)

dataPP<-data[data$Session.Name=="Pre" | data$Session.Name=="Post",]

summary(dataPP)

# convert some variables to be "factor" and create a new variable based on intensity

dataPP$Valence=as.factor(dataPP$Valence)
dataPP$Session.Name<-as.factor(dataPP$Session.Name)
dataPP$IntensityClass<-factor(dataPP$Intensity, levels=c("0","10","30","50","100"),labels=c("neutral","subtle","subtle","overt","overt"))

# not great plotting, but simple:

with(dataPP, interaction.plot(Intensity, Valence , Observed.Rating))
with(dataPP, interaction.plot(IntensityClass, Valence , Observed.Rating))

#---------------------------------------------------------------------------------------------
# summarize data first:

dataPP$depvar=dataPP$Observed.Rating
res <- ddply(dataPP, c("Valence","Intensity","IntensityClass","Session.Name"), summarise,
               N    = sum(!is.na(depvar)),
               mObserved = mean(depvar, na.rm=TRUE),
               sdObserved   = sd(depvar, na.rm=TRUE),
               seObserved   = sdObserved / sqrt(N)
)

dataPP$depvar=dataPP$Bias
res2 <- ddply(dataPP, c("Valence","Intensity","IntensityClass","Session.Name"), summarise,
               N    = sum(!is.na(depvar)),
               mBias = mean(depvar, na.rm=TRUE),
               sdBias   = sd(depvar, na.rm=TRUE),
               seBias   = sdBias / sqrt(N)
)

dataPP$depvar=dataPP$Expected
dataPP_tmp<-subset(dataPP, Session.Name=="Pre" & Run==1)
resMTurk <- ddply(dataPP_tmp, c("Valence","Intensity","IntensityClass"), summarise,
               N    = sum(!is.na(depvar)),
               mObserved = mean(depvar, na.rm=TRUE),
               sdObserved   = sd(depvar, na.rm=TRUE),
               seObserved   = sdObserved / sqrt(N)
)


resDBS<-merge(res, res2)

resMTurk$Session.Name="NormativeSample"
resRating<-rbind(res,resMTurk)

#---------------------------------------------------------------------------------------------
# the fucntion ggplot allows you to create beautiful plots, but it is more complex to use
# you need to create new "variables" that account for combinations of variables: 
# eg. combining valence with the Pre-Post session information, will give you Pre Positive Stims, Post Positive, Pre Negative Stims, Post Negative, 

resDBS$ValencebyDBS<-interaction(resDBS$Valence, resDBS$Session.Name)
resRating$ValencebyDBS<-interaction(resRating$Valence, resRating$Session.Name)

pd <- position_dodge(0.1) # move them .05 to the left and right

# BIAS plot
ggplot(resDBS, aes(x=Intensity, y= mBias, group= ValencebyDBS,colour= ValencebyDBS)) + 
  geom_hline(yintercept=0,linetype="dashed") +
  geom_errorbar(aes(ymin= mBias-seBias, ymax= mBias+seBias), width=3, size=1, position=pd) +
  geom_line(position=pd,size=3) +
  geom_point(position=pd,size=5)+
  scale_color_manual(values=c("blue","orange", "#33C3FF","#FFD633")) + #NegPoststim, PosPoststim, NegPre, PosPre
  theme_classic() + scale_y_continuous(limits=c(-.3, .3)) 

# RATING plot 
ggplot(resDBS, aes(x=Intensity, y= mObserved, group= ValencebyDBS,colour= ValencebyDBS)) + 
  geom_hline(yintercept=0.5,linetype="dashed") +
  geom_errorbar(aes(ymin= mObserved-seObserved, ymax= mObserved +seObserved), width=3, size=1, position=pd) +
  geom_line(position=pd,size=3) +
  geom_point(position=pd,size=5)+
  scale_color_manual(values=c("blue","orange", "#33C3FF","#FFD633")) + #NegPoststim, PosPoststim, NegPre, PosPre
  theme_classic() + scale_y_continuous(limits=c(0, 3)) 


# RATING plot with normaitve sample overlaid
ggplot(resRating, aes(x=Intensity, y= mObserved, group= ValencebyDBS,colour= ValencebyDBS)) + 
  geom_hline(yintercept=0.5,linetype="dashed") +
  geom_errorbar(aes(ymin= mObserved-seObserved, ymax= mObserved +seObserved), width=3, size=1, position=pd) +
  geom_line(position=pd,size=3) +
  geom_point(position=pd,size=5)+
  scale_color_manual(values=c("blue","orange", "#33C3FF","#FFD633", "gray","black")) + #NegPoststim, PosPoststim, NegPre, PosPre, NegNorm, PosNorm
  theme_classic() + scale_y_continuous(limits=c(0, 1)) 

#---------------------------------------------------------------------------------------------

dataPP_sub<-subset(dataPP,Intensity>0)
dataPP_sub$ValencebyDBS<-interaction(dataPP_sub$Valence, dataPP_sub$Session.Name)
dataPP_sub$ValencebyDBS<-factor(dataPP_sub$ValencebyDBS,levels=c("Neg.Pre", "Pos.Pre", "Neg.Post", "Pos.Post" ))
dataPP_sub$depvar=dataPP_sub$Bias
resAVG <- ddply(dataPP_sub, c("Valence","Session.Name","ValencebyDBS"), summarise,
                N  = sum(!is.na(depvar)),
                mBias = mean(depvar, na.rm=TRUE),
                sdBias  = sd(depvar, na.rm=TRUE),
                seBias  = sdBias / sqrt(N)
)
ggplot(resAVG, aes(x = ValencebyDBS, y = mBias, fill = Valence)) +
  geom_bar(stat = "summary", fun = "mean") +
  scale_fill_manual(values = c("blue", "orange", "blue", "orange")) +
  geom_point(data = dataPP_sub, aes(x = ValencebyDBS, y = Bias, color = Intensity),
             position = position_jitter(width = 0.1, height = 0)) +
  scale_color_gradient(low = "lightgray", high = "darkgray") +
  geom_errorbar(aes(ymin = mBias - seBias, ymax = mBias + seBias), width = 0.4) +
  xlab("") + ylab("Bias") +
  theme_classic() +
  scale_y_continuous(limits = c(-0.15, 1)) +
  guides(fill = guide_legend(title = "Valence"))

#---------------------------------------------------------------------------------------------
#one way anova
model <- aov( Bias ~ ValencebyDBS, data = dataPP_sub)
summary(model)

#n-way anova
model <- aov( Bias ~ Session.Name * Valence, data = dataPP_sub)
summary(model)
