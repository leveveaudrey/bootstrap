####### Comparison by boostrap #######

# data load
library(ggplot2)
rm(list=ls())

# Parameter to modify following analysis

Data=read.csv("data_test_expression.csv",dec=".",h=T,sep=";")
nb_column_group=11 ## in which column you distinguish group
Name_control_group="Endosperm" ## Name of control group in column of group
nb_column_value_studied=12 ## in which column is the value compared
Name_compared_group="PEG Endosperm" ## Name of group compared to control group 
                                    ## in column of group
size_group_compared=29 ## Size of group compared to control group
Name_analysis="Test" ## Name of analysis used for output file
nb_replicat = 1000 ## number of replicats for bootstrap
treshold_min=0.025 ## value of distribution for significant low value
treshold_max=0.975 ## value of distribution for significant high value


###### NOTHING TO CHANGE AFTER!!! #####

Name_value=names(Data[nb_column_value_studied])
data=cbind(Data[,nb_column_group],Data[,nb_column_value_studied])
data=as.data.frame(data)
colnames(data)=c("Cluster","Value")

# determination of real value obtain
tested_group=subset(data,data$Cluster==Name_compared_group)
tested_group_val=tested_group$Value
real_mean=mean(as.numeric(tested_group$Value))
real_median=median(as.numeric(tested_group$Value))

# determination of mean values expected based on control

control=subset(data,data$Cluster==Name_control_group)
value_temp=control$Value
mean_control=mean(as.numeric(value_temp))
median_control=median(as.numeric(value_temp))

table_temp_mean=NULL
table_temp_median=NULL
for (i in 1:nb_replicat)
{ x=sample(value_temp, size_group_compared)
mean_gene=mean(as.numeric(x))
median_gene=median(as.numeric(x))
table_temp_mean=rbind(table_temp_mean,mean_gene)
table_temp_median=rbind(table_temp_median,median_gene)
}
table_temp_median=as.data.frame(table_temp_median)
table_temp_mean=as.data.frame(table_temp_mean)

temp_mean=sort(table_temp_mean$V1)
val_min_mean=temp_mean[length(temp_mean)*treshold_min]
val_max_mean=temp_mean[length(temp_mean)*treshold_max]
temp_median=sort(table_temp_median$V1)
val_min_median=temp_median[length(temp_median)*treshold_min]
val_max_median=temp_median[length(temp_median)*treshold_max]

## plot

svg(paste(Name_analysis,"_",Name_value,"_",Name_compared_group,"_vs",
          Name_control_group,"mean.svg",sep=""), width=5, height = 8)

ggplot(table_temp_mean, aes(x=V1)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="grey") +
  geom_vline(aes(xintercept=val_min_mean),
             color="black", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=val_max_mean),
             color="black", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=real_mean),
             color="red", linetype="dashed", size=1)+
  xlab(Name_value)+
  ylab(paste("Density of mean in ",nb_replicat," random samples 
             \nof ",size_group_compared," ",Name_control_group, sep=""))+
  theme(legend.position = "none",axis.title.x =element_text(size=0,colour="black") ,axis.title.y = element_text(size=0,colour = "black"),axis.text.x = element_text(size=15, colour = "black"),axis.text.y = element_text(size=15,colour="black"))+
  theme_classic()

dev.off()

svg(paste(Name_analysis,"_",Name_value,"_",Name_compared_group,"_vs",
          Name_control_group,"median.svg",sep=""), width=5, height = 8)

ggplot(table_temp_median, aes(x=V1)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="grey") +
  geom_vline(aes(xintercept=val_min_median),
             color="black", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=val_max_median),
             color="black", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=real_median),
             color="red", linetype="dashed", size=1)+
  xlab(Name_value)+
  ylab(paste("Density of median in ",nb_replicat," random samples 
             \nof ",size_group_compared," ",Name_control_group, sep=""))+
  theme(legend.position = "none",axis.title.x =element_text(size=0,colour="black") ,axis.title.y = element_text(size=0,colour = "black"),axis.text.x = element_text(size=15, colour = "black"),axis.text.y = element_text(size=15,colour="black"))+
  theme_classic()

dev.off()


# Comparison with real value and summary

pval_mean=subset(table_temp_mean,table_temp_mean$V1<real_mean)
pval_mean=length(pval_mean$V1)/nb_replicat
if(pval_mean>0.5)
{
  pval_mean=1-pval_mean
}
pval_median=subset(table_temp_median,table_temp_median$V1<real_median)
pval_median=length(pval_median$V1)/nb_replicat
if(pval_median>0.5)
{
  pval_median=1-pval_median
}

summary=cbind(paste("Mean ",Name_value,sep=""), real_mean,mean_control,
              val_min_mean,val_max_mean,pval_mean)
x=cbind(paste("Median ",Name_value,sep=""), real_median,median_control,
              val_min_median,val_max_median,pval_median)
summary=rbind(summary,x)
summary=as.data.frame(summary)
colnames(summary)=c("Value","Tested", "Control","val_min","val_max","pval")
write.csv(summary,paste(Name_analysis,"_",Name_value,"_",
                        Name_compared_group,"_vs",
                        Name_control_group,"_all.csv",sep=""))


## plot

svg(paste(Name_analysis,"_",Name_value,"_",
          Name_compared_group,"_vs",
          Name_control_group,"_point_mean_plot.svg",sep=""),
    width=8, height = 8)

ggplot(data, aes(x=Cluster, y=as.numeric(Value))) +
  geom_jitter(color="grey", size=0.5)+
  geom_hline(aes(yintercept=val_min_mean),
             color="black", linetype="dashed", size=1)+
  geom_hline(aes(yintercept=val_max_mean),
             color="black", linetype="dashed", size=1)+
  stat_summary(fun.y = mean,geom="point",size=2,color="red")+
  ylab(Name_value)+
  theme_classic()+
  theme(legend.position = "none",axis.title.x =element_text(colour="black") ,axis.title.y = element_text(size=25,colour = "black"),axis.text.x = element_text(size=15, colour = "black"),axis.text.y = element_text(size=15,colour="black"))

dev.off()

svg(paste(Name_analysis,"_",Name_value,"_",
          Name_compared_group,"_vs",
          Name_control_group,"_point_median_plot.svg",sep=""),
    width=8, height = 8)

ggplot(data, aes(x=Cluster, y=as.numeric(Value))) +
  geom_jitter(color="grey", size=0.5)+
  geom_hline(aes(yintercept=val_min_median),
             color="black", linetype="dashed", size=1)+
  geom_hline(aes(yintercept=val_max_median),
             color="black", linetype="dashed", size=1)+
  stat_summary(fun.y = median,geom="point",size=2,color="red")+
  ylab(Name_value)+
  theme_classic()+
  theme(legend.position = "none",axis.title.x =element_text(colour="black") ,axis.title.y = element_text(size=25,colour = "black"),axis.text.x = element_text(size=15, colour = "black"),axis.text.y = element_text(size=15,colour="black"))


dev.off()
