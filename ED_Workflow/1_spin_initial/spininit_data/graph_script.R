# ----------------- #
# Graphing the Data #
# ----------------- #

library(ggplot2)

if(!dir.exists("./graphs/")) dir.create("./graphs/", recursive = T) #This should create a folder for the output to go into. 
path.data <- ("./tables/tables/")
all.data <- dir(path.data)

for(RUNID in all.data){
  print(RUNID)
  agb.data <- read.csv(file.path(path.data,all.data[RUNID])) #Should build a filepath where I can read each .csv in the list. But it's not working, and I don't know why. It just keeps telling me that there's no such file or directory.
  jpeg("../graphs",RUNID,"_graph")
  ggplot(agb.data,aes(x=days,y=agb,color=pft))+geom_line()+xlab("Days since 01-01-1801")+ylab("Above Ground Biomass, kg C m-2")+theme(panel.background=element_rect(fill="white"))+theme_bw()+theme(panel.grid=element_blank())+scale_color_manual(values=c("#E69F00","#009E73"))
  dev.off()
}
