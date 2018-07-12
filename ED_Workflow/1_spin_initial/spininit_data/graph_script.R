# ----------------- #
# Graphing the Data #
# ----------------- #

library(ggplot2)

plot <- ggplot(agb.data,aes(x=days,y=agb,color=pft))+geom_line()+xlab("Days since 01-01-1801")+ylab("Above Ground Biomass, kg C m-2")+theme(panel.background=element_rect(fill="white"))+theme_bw()+theme(panel.grid=element_blank())+scale_color_manual(values=c("#E69F00","#009E73"))
plot 