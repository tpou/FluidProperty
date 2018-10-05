# Define library
library(reshape2)
library(dplyr)
library(ggplot2)
# Load data from ..data/

fluiddata = read.csv('../data/FluidData.csv')
labdata = read.csv('../data/LabData.csv')

# Explore fluidata fist
str(fluiddata)
# There are 30 attributes,each has 264 records:
#   Factor variables are: $Block: 3 levels, $Area: 6 level, $Wellname: 20 level
#                         $Horozon: 6 levels, $SandName: 192 levels, $DataSoucre: 4 levels
# Numerical variables are: $X, $Y, $TopSand, $BaseSand, $Netpay, $Porosity, $Sw
#                         $X1.Bgi, $P, $T, $C1-C8.

# Get descriptive statistics:
summary(fluiddata)
m_fluiddata = melt(fluiddata,id="Id")
cm_fluiddata = m_fluiddata[m_fluiddata$variable %in% c("Block","Area","Horizon","DataSource"),]
nm_fluiddata = m_fluiddata[m_fluiddata$variable %in% c("TopSand","Netpay","Porosity","Sw","Egi","P","T","C1",
                                                   "C2","C3","iC4","nC4","iC5","nC5","C6","C7","C8.",
                                                   "CO2","N2","GHV"),]
nm_fluiddata$value = as.numeric(as.character(nm_fluiddata$value))

ggplot(data=cm_fluiddata,aes(x=value)) + geom_histogram(stat="count",aes(fill = variable)) +
  facet_wrap(~variable,scales = "free")

ggplot(nm_fluiddata,aes(x=value)) + geom_histogram(aes(fill = variable))+
facet_wrap(~variable,scales = "free")

#Boxplot
m2_fluiddata = melt(fluiddata,id=c("Id","Block","Area","WellName","Horizon","DataSource"))
nm2_fluiddata = m2_fluiddata[m2_fluiddata$variable %in% c("Netpay","Porosity","Sw","C1","iC5","CO2"),]
nm2_fluiddata$value = as.numeric(as.character(nm2_fluiddata$value))

ggplot(nm2_fluiddata,aes(y=value)) + geom_boxplot(col="deepskyblue")+facet_wrap(~variable,scale="free")

#Facet CO2:
CO2_nm2_fluiddata = nm2_fluiddata[nm2_fluiddata$variable %in% c("CO2"),]

ggplot(CO2_nm2_fluiddata,aes(x=variable,y=value)) + geom_boxplot(mapping=aes(col=Area))+facet_wrap(~Horizon,scales="free")

g=ggplot(CO2_nm2_fluiddata,aes(x=Horizon,y=value)) + geom_point(aes(shape=DataSource,col=Horizon),size=1.5)+
geom_text(aes(label=ifelse(((value>25)&(Horizon %in% c("F","H"))|((value>20) & (Horizon=="I") & (Area=="KL"))),as.character(WellName),'')),hjust=1.1,col='red')+
  facet_wrap(~Area)
g+geom_boxplot(aes(color=Horizon),outlier.color=NA)+ggtitle("CO2 Boxplot - Outlier identification")+theme(plot.title=element_text(hjust=0.5)) 

#Facet netpay
Net_nm2_fluiddata = nm2_fluiddata[nm2_fluiddata$variable %in% c("Netpay"),]

ggplot(Net_nm2_fluiddata,aes(x=variable,y=value)) + geom_boxplot(mapping=aes(col=Area))+facet_wrap(~Horizon,scales="free")

g=ggplot(Net_nm2_fluiddata,aes(x=Horizon,y=value)) + geom_point(aes(shape=DataSource,col=Horizon),size=1.5)+
  geom_text(aes(label=ifelse(((value>25)&(Horizon %in% c("F","H"))|((value>20) & (Horizon=="I") & (Area=="KL"))),as.character(WellName),'')),hjust=1.1,col='red')+
  facet_wrap(~Area)
g+geom_boxplot(aes(color=Horizon),outlier.color=NA)+ggtitle("CO2 Boxplot - Outlier identification")+theme(plot.title=element_text(hjust=0.5)) 
