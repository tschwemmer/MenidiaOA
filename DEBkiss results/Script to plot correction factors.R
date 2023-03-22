#plotting the correction factors for Ch 4

library(ggplot2)
library(grid)

my_equation1<-function(x){1-exp(-0.3*(x-2.044))}
my_equation2<-function(x){1-exp(-0.8*(x-2.044))}
my_equation3<-function(x){1-exp(-1.8*(x-2.044))}

cplot<-ggplot(data.frame(x=c(0,10)),aes(x=x))+
  stat_function(fun=my_equation1)+
  stat_function(fun=my_equation2)+
  stat_function(fun=my_equation3)+
  coord_cartesian(ylim=c(0,1))+
  annotation_custom(grobTree(textGrob("K=0.3",x=0.49,y=0.49,hjust=0,gp=gpar(col="black",fontsize=11))))+
  annotation_custom(grobTree(textGrob("K=0.8",x=0.4,y=0.68,hjust=0,gp=gpar(col="black",fontsize=11))))+
  annotation_custom(grobTree(textGrob("K=1.8",x=0.26,y=0.91,hjust=0,gp=gpar(col="black",fontsize=11))))+
  labs(x=expression(paste("Dissolved Oxygen (mg L"^-1,")")),y=expression(paste("Correction Factor ",italic("c"))))+
  theme_classic()
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


my_equation5<-function(x){1-exp(-1.326*(x-2.044))}
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
