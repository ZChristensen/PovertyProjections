# library(readr)
# growth_rates <- read_csv("~/GHA 2017/growth rates.csv")
# growth_rates$gr.rt.20 = growth_rates$gr.rt.20 / 100
# growth_rates$gr.rt.10 = as.numeric(growth_rates$gr.rt.10) / 100
library(readr)
library(WDI)
library(data.table)
library(varhandle)
library(readxl)
GDPgrowth = WDI(country="all", indicator="NY.GDP.MKTP.KD.ZG", start=1996, end=2016, extra=TRUE)
GDPgrowth$iso3c=unfactor(GDPgrowth$iso3c)
GDPgrowth$iso3c[which(GDPgrowth$country=="Kosovo")]="XKX"
growthrate20 = data.table(GDPgrowth)[,.(
  mean.20.year.gdpgrowth = mean(NY.GDP.MKTP.KD.ZG, na.rm=TRUE)/100
), by=.(iso3c) ]
growthrate10 = data.table(subset(GDPgrowth,year>=2006))[,.(
  mean.10.year.gdpgrowth = mean(NY.GDP.MKTP.KD.ZG, na.rm=TRUE)/100
), by=.(iso3c) ]
  
#PCN
pcn_2013 <- read_csv("~/Poverty data/years/pcn.2013.csv")
metadata <- read_excel("~/Poverty data/Country metadata.xlsx")
names(metadata)[8]="iso3c"
names(metadata)[12]="code2"
setnames(pcn_2013, "country", "PovcalNet name")
pcniso =merge(metadata,pcn_2013, by="PovcalNet name", all=TRUE)
pcniso$iso3c[which(pcniso$iso3c=="KSV")]="XKX"

merge.dat = merge(growthrate20,pcniso, by="iso3c")
merge.dat = merge(growthrate10,merge.dat, by="iso3c")
merge.dat$plproj.20 = merge.dat$pl*(1+merge.dat$mean.20.year.gdpgrowth)^17
merge.dat$plproj.10 = merge.dat$pl*(1+merge.dat$mean.10.year.gdpgrowth)^17
merge.dat$poor20 = merge.dat$plproj.20 <1.90
merge.dat$poor10 = merge.dat$plproj.10 <1.90

firstto190= function(pl.vector){
  minimum.value = 100000000
  minimum.index = 0
  for(i in 1:length(pl.vector)){
    x <- pl.vector[i]
    if(!is.na(x)) {
      diff = abs(1.90-x)
      if(diff<minimum.value){
        minimum.value = diff
        minimum.index = i
      }
    } 
 
 
  }
  return(pl.vector[minimum.index])
}

lines190 = data.table(subset(merge.dat,!is.na(plproj.20)))[,.(equiv190=firstto190(plproj.20)),by=.(iso3c)]


merge.190 = merge(merge.dat,lines190, by="iso3c")
merge.190 = subset(merge.190,plproj.20==equiv190)
#note the poor pops here are based on 2013 populations and need to be multiplied by 2030 projections
merge.190$hc203020 = merge.190$poorpop/merge.190$pop
keep = c("iso3c", "plproj.20", "poorpop", "hc203020")
merge.190 = data.frame(merge.190)[,keep]

#10 year
lines19010 = data.table(subset(merge.dat,!is.na(plproj.10)))[,.(equiv190=firstto190(plproj.10)),by=.(iso3c)]

merge.19010 = merge(merge.dat,lines19010, by="iso3c")
merge.19010 = subset(merge.19010,plproj.10==equiv190)
merge.19010$hc203010 = merge.19010$poorpop/merge.19010$pop
keep = c("iso3c", "plproj.10", "hc203010")
merge.19010 = data.frame(merge.19010)[,keep]


merge.190$diff = merge.190$hc203010-merge.190$hc203020
population2030 <- read_excel("~/GHA 2017/TotalPopSex-20170404102034.xls")
setnames(population2030, "2030.000000", "pop2030")
#no population projections available for Kosovo but 2030HC20 is 0.0002 and 2030HC10 is 0.0020, so ommission is minimal.
merge.190=merge(merge.190,population2030)

merge.190$poorpop203010 = merge.190$hc203010 * merge.190$pop2030
merge.190$poorpop203020 = merge.190$hc203020 * merge.190$pop2030

write.csv(merge.190,"~/GHA 2017/povertyforecasts20170509.csv", row.names=FALSE, na="")

#Venn
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(varhandle)
library(venneuler)
vd = venneuler(c(A=314, B=87, C=402, "A&B"= 65, "A&C"=314, "B&C"=87, "A&B&C"=65))
plot(vd)
library(VennDiagram)
vd2 = draw.triple.venn(area1=314, area2=87, area3=402, n12=65, n23=87, n13=314, n123=65, scaled = TRUE, euler.d = TRUE)
