# Read in information from Dr. John Tipton (Arkansas) about how model members compare to empirical climate
mcmc.out <- read.csv("../data_raw/met/climate-filitering-weights/ensemble-weights-monte-carlo-mean-glsp.csv")
dim(mcmc.out)
summary(mcmc.out)

ens.agg <- aggregate(mcmc.out$weights, by=list(mcmc.out$climate_model), FUN=mean)
names(ens.agg) <- c("climate_model", "weight.mean")
summary(ens.agg)

# Looking at weights just for the spinup period
ens.agg.spinup <- aggregate(mcmc.out[mcmc.out$year<=1830, "weights"], by=list(mcmc.out[mcmc.out$year<=1830, "climate_model"]), FUN=mean)
names(ens.agg.spinup) <- c("climate_model", "weight.mean")
summary(ens.agg.spinup)

best.overall <- paste(ens.agg[ens.agg$weight.mean==max(ens.agg$weight.mean),"climate_model"])
best.spinup <-  paste(ens.agg.spinup[ens.agg.spinup$weight.mean==max(ens.agg.spinup$weight.mean),"climate_model"])
best.overall
best.spinup

ens.agg[ens.agg$climate_model %in% c(best.overall, best.spinup),]
ens.agg.spinup[ens.agg.spinup$climate_model %in% c(best.overall, best.spinup),]

# Will add graphs of PDSI during the time
PDSI.GLSP <- read.csv("../data_raw/met/month/PDSI_AllMembers.csv") #Reading in the CSV, and called it PDSI (for the name of the drought index) .GLSP (for Glacial Lakes state park).
class(PDSI.GLSP)
ncol(PDSI.GLSP) #This should tell me the number of columns. I want to add a new column, so that's how this will work. There should be 201 columns. 
# Each column is a different weather. 

# Add columns that describe the year, month, and day. Row 1=January 1 1800. 
pdsi.month <- rep(1:12,length.out=nrow(PDSI.GLSP))
pdsi.year <- rep(1800:(1800-1+nrow(PDSI.GLSP)/12), length.out=nrow(PDSI.GLSP), each=12)

# Subset to just JJA
pdsi.sims <- data.frame(year=pdsi.year, month=pdsi.month, 
                        sim=c(rep(best.overall, length(pdsi.month)), rep(best.spinup, length(pdsi.month))),
                        PDSI=c(PDSI.GLSP[,which(tolower(names(PDSI.GLSP))==tolower(best.overall))],
                               PDSI.GLSP[,which(tolower(names(PDSI.GLSP))==tolower(best.spinup))]))
pdsi.sims <- pdsi.sims[pdsi.sims$month %in% 6:8,]
summary(pdsi.sims)

pdsi.sims <- aggregate(pdsi.sims$PDSI, by=pdsi.sims[,c("sim", "year")], FUN=mean)
names(pdsi.sims)[which(names(pdsi.sims)=="x")] <- "pdsi"
for(ENS in unique(pdsi.sims$sim)){
  pdsi.sims[pdsi.sims$sim==ENS, "pdsi.10"]  <- zoo::rollapply(pdsi.sims[pdsi.sims$sim==ENS, "pdsi"],  10, FUN=mean, fill=NA)
  pdsi.sims[pdsi.sims$sim==ENS, "pdsi.30"]  <- zoo::rollapply(pdsi.sims[pdsi.sims$sim==ENS, "pdsi"],  30, FUN=mean, fill=NA)
  
}
summary(pdsi.sims)

yrs.mid <- seq(min(pdsi.sims$year), max(pdsi.sims$year), 0.5)
pdsi.yrs <- data.frame(year=yrs.mid, sim=rep(unique(pdsi.sims$sim), each=length(yrs.mid)))
pdsi.sims <- merge(pdsi.sims, pdsi.yrs, all=T)
summary(pdsi.sims)
for(ENS in unique(pdsi.sims$sim)){
  for(YR in unique(pdsi.sims$year)){

    pdsi.sims[pdsi.sims$sim==ENS & pdsi.sims$year==YR,c("pdsi", "pdsi.10", "pdsi.30")] <- apply(pdsi.sims[pdsi.sims$sim==ENS & pdsi.sims$year>=YR & pdsi.sims$year<=YR+1,c("pdsi", "pdsi.10", "pdsi.30")], 2, mean, na.rm=T)
  }
}
summary(pdsi.sims)


pdsi.sims$pdsi.pos <- ifelse(pdsi.sims$pdsi>=0, pdsi.sims$pdsi, 0)
pdsi.sims$pdsi.neg <- ifelse(pdsi.sims$pdsi<0, pdsi.sims$pdsi, 0)
pdsi.sims$pdsi.10.pos <- ifelse(pdsi.sims$pdsi.10>=0, pdsi.sims$pdsi.10, 0)
pdsi.sims$pdsi.10.neg <- ifelse(pdsi.sims$pdsi.10< 0, pdsi.sims$pdsi.10, 0)
pdsi.sims$pdsi.30.pos <- ifelse(pdsi.sims$pdsi.30>=0, pdsi.sims$pdsi.30, 0)
pdsi.sims$pdsi.30.neg <- ifelse(pdsi.sims$pdsi.30< 0, pdsi.sims$pdsi.30, 0)
summary(pdsi.sims)


spinup <- lm(pdsi ~ year, data=pdsi.sims[pdsi.sims$sim==best.spinup,])
overall <- lm(pdsi ~ year, data=pdsi.sims[pdsi.sims$sim==best.overall,])
summary(spinup)
summary(overall)

# Path for where to save images
path.google <- "/Volumes/GoogleDrive/My Drive/URF 2018 Butkiewicz/Met_Project_Month/"

library(ggplot2)
ggplot(data=pdsi.sims) +
  facet_grid(sim~.) +
  geom_line(aes(x=year, y=pdsi)) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() 

png(file.path(path.google, "PDSI_Best_01yr.png"), height=4, width=6, unit="in", res=320)
ggplot(data=pdsi.sims) +
  ggtitle("Annual PDSI") +
  facet_grid(sim~.) +
  geom_area(aes(x=year, y=pdsi.pos, fill="wet")) +
  geom_area(aes(x=year, y=pdsi.neg, fill="dry")) +
  geom_line(aes(x=year, y=pdsi), size=0.25, alpha=0.8) +
  geom_vline(xintercept=1830, linetype="dashed") +
  scale_fill_manual(values=c("indianred2", "dodgerblue2")) +
  scale_y_continuous(name="Drought (PDSI)") +
  scale_x_continuous(name="Year", expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="bottom")
dev.off()

png(file.path(path.google, "PDSI_Best_10yr.png"), height=4, width=6, unit="in", res=320)
ggplot(data=pdsi.sims) +
  ggtitle("10-year running mean") +
  facet_grid(sim~.) +
  geom_area(aes(x=year, y=pdsi.10.pos, fill="wet")) +
  geom_area(aes(x=year, y=pdsi.10.neg, fill="dry")) +
  geom_line(aes(x=year, y=pdsi.10), size=0.25, alpha=0.8) +
  geom_vline(xintercept=1830, linetype="dashed") +
  scale_fill_manual(values=c("indianred2", "dodgerblue2")) +
  scale_y_continuous(name="Drought (PDSI)") +
  scale_x_continuous(name="Year", expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="bottom")
dev.off()

png(file.path(path.google, "PDSI_Best_30yr.png"), height=4, width=6, unit="in", res=320)
ggplot(data=pdsi.sims) +
  ggtitle("30-year running mean") +
  facet_grid(sim~.) +
  geom_area(aes(x=year, y=pdsi.30.pos, fill="wet")) +
  geom_area(aes(x=year, y=pdsi.30.neg, fill="dry")) +
  geom_line(aes(x=year, y=pdsi.30), size=0.25, alpha=0.8) +
  geom_vline(xintercept=1830, linetype="dashed") +
  scale_fill_manual(values=c("indianred2", "dodgerblue2")) +
  scale_y_continuous(name="Drought (PDSI)") +
  scale_x_continuous(name="Year", expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="bottom")
dev.off()
