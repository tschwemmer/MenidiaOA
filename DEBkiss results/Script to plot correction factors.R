#plotting the correction factors for Ch 4

library(ggplot2)
library(grid)
library(gridExtra)

my_equation1<-function(x){(1*(x-2.044))/(1+(1*(x-2.044)))}
my_equation2<-function(x){(3*(x-2.044))/(1+(3*(x-2.044)))}
my_equation3<-function(x){(10*(x-2.044))/(1+(10*(x-2.044)))}

cplot<-ggplot(data.frame(x=c(0,10)),aes(x=x))+
  stat_function(fun=my_equation1,xlim=c(2.044,10))+
  stat_function(fun=my_equation2,xlim=c(2.044,10))+
  stat_function(fun=my_equation3,xlim=c(2.044,10))+
  coord_cartesian(xlim=c(0,10),ylim=c(0,1))+
  annotation_custom(grobTree(textGrob(expression(paste(italic("Z"),"=1")),x=0.44,y=0.78,hjust=0,gp=gpar(col="black",fontsize=9))))+
  annotation_custom(grobTree(textGrob(expression(paste(italic("Z"),"=3")),x=0.33,y=0.88,hjust=0,gp=gpar(col="black",fontsize=9))))+
  annotation_custom(grobTree(textGrob(expression(paste(italic("Z"),"=10")),x=0.21,y=0.93,hjust=0,gp=gpar(col="black",fontsize=9))))+
  labs(x=expression(paste("Dissolved Oxygen (mg L"^-1,")")),y=expression(paste("Correction Factor ",italic("c"))))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(plot.margin=unit(c(0.2,0.2,0.2,0.2),"inches"))
print(cplot)

my_equation4<-function(x){1/(x+0.0001)}

c1plot<-ggplot(data.frame(x=c(0,1)),aes(x=x))+
  stat_function(fun=my_equation4)+
  coord_cartesian(ylim=c(0,12))+
  labs(x=expression(paste("Correction Factor ",italic("c"))),y=expression(paste("Correction Factor ",italic("c"["1"]))))+
  theme(axis.title.y=element_text(angle=90))+
  geom_segment(aes(x=0,y=10,xend=0.1,yend=10))+
  theme_classic()
print(c1plot)

grid.arrange(cplot,c1plot,ncol=1)

#calculating values of yVA, mu_emb, and mu_lar with K=1.315
my_equation5<-function(x){1-exp(-1.315*(x-2.044))}
#yVA
c7.7<-my_equation5(7.7)*.3646
c4.2<-my_equation5(4.2)*.3646
c3.1<-my_equation5(3.1)*.3646
c2.7<-my_equation5(2.7)*.3646

#mu_emb
c1.7.7<-my_equation4(c7.7)*.06393
c1.4.2<-my_equation4(c4.2)*.06393
c1.3.1<-my_equation4(c3.1)*.06393
c1.2.7<-my_equation4(c2.7)*.06393

#mu_lar
c2.7.7<-my_equation4(c7.7)*.0294
c2.4.2<-my_equation4(c4.2)*.0294
c2.3.1<-my_equation4(c3.1)*.0294
c2.2.7<-my_equation4(c2.7)*.0294

#Lower limit of 95% CI
my_equation5<-function(x){1-exp(-1.196*(x-2.044))}
c7.7<-my_equation5(7.7)*.3646
c4.2<-my_equation5(4.2)*.3646
c3.1<-my_equation5(3.1)*.3646
c2.7<-my_equation5(2.7)*.3646

c1.7.7<-my_equation4(c7.7)*.06393
c1.4.2<-my_equation4(c4.2)*.06393
c1.3.1<-my_equation4(c3.1)*.06393
c1.2.7<-my_equation4(c2.7)*.06393

c2.7.7<-my_equation4(c7.7)*.0294
c2.4.2<-my_equation4(c4.2)*.0294
c2.3.1<-my_equation4(c3.1)*.0294
c2.2.7<-my_equation4(c2.7)*.0294

#Upper limit of 95% CI
my_equation5<-function(x){1-exp(-1.756*(x-2.044))}
c7.7<-my_equation5(7.7)*.3646
c4.2<-my_equation5(4.2)*.3646
c3.1<-my_equation5(3.1)*.3646
c2.7<-my_equation5(2.7)*.3646

c1.7.7<-my_equation4(c7.7)*.06393
c1.4.2<-my_equation4(c4.2)*.06393
c1.3.1<-my_equation4(c3.1)*.06393
c1.2.7<-my_equation4(c2.7)*.06393

c2.7.7<-my_equation4(c7.7)*.0294
c2.4.2<-my_equation4(c4.2)*.0294
c2.3.1<-my_equation4(c3.1)*.0294
c2.2.7<-my_equation4(c2.7)*.0294
