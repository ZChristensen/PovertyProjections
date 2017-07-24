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
GDPgrowth$iso3c[which(GDPgrowth$country=="Cabo Verde")]="CPV"
growthrate20 = data.table(GDPgrowth)[,.(
  mean.20.year.gdpgrowth = mean(NY.GDP.MKTP.KD.ZG, na.rm=TRUE)/100
), by=.(iso3c) ]
growthrate10 = data.table(subset(GDPgrowth,year>=2006))[,.(
  mean.10.year.gdpgrowth = mean(NY.GDP.MKTP.KD.ZG, na.rm=TRUE)/100
), by=.(iso3c) ]

mediangrowth <- read_csv("~/Poverty projections/mediangrowthrates.csv")
allgrowthrates=merge(mediangrowth,growthrate20)
allgrowthrates=merge(allgrowthrates,growthrate10)

#PCN
pcn_2013 <- read_csv("~/Poverty data/years/pcn.2013.csv")
metadata <- read_excel("~/Poverty data/Country metadata.xlsx")
names(metadata)[8]="iso3c"
names(metadata)[12]="code2"
setnames(pcn_2013, "country", "PovcalNet name")
pcniso =merge(metadata,pcn_2013, by="PovcalNet name", all=TRUE)
pcniso$iso3c[which(pcniso$iso3c=="KSV")]="XKX"
pcniso$iso3c[which(pcniso$iso3c=="TMP")]="TLS"
pcniso$iso3c[which(pcniso$iso3c=="ROM")]="ROU"

#merge.dat = merge(growthrate20,pcniso, by="iso3c")
#merge.dat = merge(growthrate10,merge.dat, by="iso3c")
merge.dat=merge(allgrowthrates,pcniso, by="iso3c")
merge.dat$plproj.20 = merge.dat$pl*(1+merge.dat$mean.20.year.gdpgrowth)^17
merge.dat$plproj.10 = merge.dat$pl*(1+merge.dat$mean.10.year.gdpgrowth)^17
merge.dat$plproj.20med = merge.dat$pl * (1+merge.dat$)
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
medianincomes2013 = subset.data.frame(pcniso, hc>=49 & hc<=51)
medianincomes2013 = data.table(medianincomes2013)[,.(
  medianincome = median(pl, na.rm=TRUE)
), by=.(iso3c)]
median.tab$year = 2013


merge.190 = merge(merge.dat,lines190, by="iso3c")
merge.190 = subset(merge.190,plproj.20==equiv190)
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
merge.190 = merge(merge.190,merge.19010)

merge.190$diff = merge.190$hc203010-merge.190$hc203020
population2030 <- read_excel("~/GHA 2017/TotalPopSex-20170404102034.xls")
setnames(population2030, "2030.000000", "pop2030")
population2030$iso3c[which(population2030$iso3c=="TMP")]="TLS"
population2030$iso3c[which(population2030$iso3c=="ROM")]="ROU"
#no population projections available for Kosovo but 2030HC20 is 0.0002 and 2030HC10 is 0.0020, so ommission is minimal.
merge.190=merge(merge.190,population2030)

merge.190$poorpop203010 = merge.190$hc203010 * merge.190$pop2030
merge.190$poorpop203020 = merge.190$hc203020 * merge.190$pop2030
vulnerable=c(
  "AF",
  "BD",
  "BZ",
  "KH",
  "DJ",
  "GT",
  "HT",
  "HN",
  "IN",
  "ID",
  "IR",
  "IQ",
  "KE",
  "KP",
  "LA",
  "MG",
  "MR",
  "MA",
  "MZ",
  "MM",
  "NP",
  "NI",
  "PK",
  "PG",
  "SB",
  "SO",
  "SD",
  "SY",
  "TJ",
  "ZW"
)
merge.190$vulnerable=merge.190$di_id %in% vulnerable
#OECD States of Fragility 2016
fragile=c(
  "SO",
  "SS",
  "CF",
  "CD",
  "YE",
  "SD",
  "ER",
  "AF",
  "TD",
  "ET",
  "BI",
  "HT",
  "SY",
  "IQ",
  "ML",
  "GN",
  "NE",
  "PS",
  "MM",
  "KE",
  "ZW",
  "NG",
  "MZ",
  "UG",
  "GW",
  "LR",
  "PK",
  "MR",
  "GM",
  "GT",
  "KP",
  "AO",
  "SL",
  "HN",
  "TZ",
  "VE",
  "PG",
  "MG",
  "CG",
  "BD",
  "BF",
  "CM",
  "CI",
  "ZM",
  "RW",
  "SZ",
  "EG",
  "LA",
  "LS",
  "TL",
  "KM",
  "SB",
  "MW",
  "KH",
  "LY",
  "TJ"
)
merge.190$fragile=merge.190$di_id %in% fragile
#INFORM High risk
risk=c(
  "AF",
  "BD",
  "BF",
  "BI",
  "CM",
  "CF",
  "TD",
  "CO",
  "CG",
  "CD",
  "CI",
  "DJ",
  "SV",
  "ER",
  "ET",
  "GT",
  "GN",
  "HT",
  "IN",
  "IR",
  "IQ",
  "KE",
  "KP",
  "LB",
  "LR",
  "LY",
  "MG",
  "ML",
  "MR",
  "MZ",
  "MM",
  "NP",
  "NE",
  "NG",
  "PK",
  "PG",
  "RW",
  "SN",
  "SL",
  "SB",
  "SO",
  "SS",
  "SD",
  "SY",
  "TZ",
  "TR",
  "UG",
  "UA",
  "YE"
)
merge.190$highrisk=merge.190$di_id %in% risk


write.csv(merge.190,"~/GHA 2017/povertyforecasts20170509.csv", row.names=FALSE, na="")
#####
####Median income growth

medianincomes2013 = subset.data.frame(pcniso, hc>=49 & hc<=51)
medianincomes2013 = data.table(medianincomes2013)[,.(
  medianincome = median(pl, na.rm=TRUE)
), by=.(iso3c)]
medianincomes2013$year = 2013



pcn_2012 <- read_csv("~/Poverty data/years/pcn.2012.csv")
metadata <- read_excel("~/Poverty data/Country metadata.xlsx")
names(metadata)[8]="iso3c"
names(metadata)[12]="code2"
setnames(pcn_2012, "country", "PovcalNet name")
pcniso =merge(metadata,pcn_2012, by="PovcalNet name", all=TRUE)
pcniso$iso3c[which(pcniso$iso3c=="KSV")]="XKX"
pcniso$iso3c[which(pcniso$iso3c=="TMP")]="TLS"
pcniso$iso3c[which(pcniso$iso3c=="ROM")]="ROU"

medianincomes2012 = subset.data.frame(pcniso, hc>=49 & hc<=51)
medianincomes2012 = data.table(medianincomes2012)[,.(
  medianincome = median(pl, na.rm=TRUE)
), by=.(iso3c)]
medianincomes2012$year = 2012

pcn_2011 <- read_csv("~/Poverty data/years/pcn.2011.csv")
metadata <- read_excel("~/Poverty data/Country metadata.xlsx")
names(metadata)[8]="iso3c"
names(metadata)[12]="code2"
setnames(pcn_2011, "country", "PovcalNet name")
pcniso =merge(metadata,pcn_2011, by="PovcalNet name", all=TRUE)
pcniso$iso3c[which(pcniso$iso3c=="KSV")]="XKX"
pcniso$iso3c[which(pcniso$iso3c=="TMP")]="TLS"
pcniso$iso3c[which(pcniso$iso3c=="ROM")]="ROU"

medianincomes2011 = subset.data.frame(pcniso, hc>=49 & hc<=51)
medianincomes2011 = data.table(medianincomes2011)[,.(
  medianincome = median(pl, na.rm=TRUE)
), by=.(iso3c)]
medianincomes2011$year = 2011

pcn_2010 <- read_csv("~/Poverty data/years/pcn.2010.csv")
metadata <- read_excel("~/Poverty data/Country metadata.xlsx")
names(metadata)[8]="iso3c"
names(metadata)[12]="code2"
setnames(pcn_2010, "country", "PovcalNet name")
pcniso =merge(metadata,pcn_2010, by="PovcalNet name", all=TRUE)
pcniso$iso3c[which(pcniso$iso3c=="KSV")]="XKX"
pcniso$iso3c[which(pcniso$iso3c=="TMP")]="TLS"
pcniso$iso3c[which(pcniso$iso3c=="ROM")]="ROU"

medianincomes2010 = subset.data.frame(pcniso, hc>=49 & hc<=51)
medianincomes2010 = data.table(medianincomes2010)[,.(
  medianincome = median(pl, na.rm=TRUE)
), by=.(iso3c)]
medianincomes2010$year = 2010

pcn_2008 <- read_csv("~/Poverty data/years/pcn.2008.csv")
metadata <- read_excel("~/Poverty data/Country metadata.xlsx")
names(metadata)[8]="iso3c"
names(metadata)[12]="code2"
setnames(pcn_2008, "country", "PovcalNet name")
pcniso =merge(metadata,pcn_2008, by="PovcalNet name", all=TRUE)
pcniso$iso3c[which(pcniso$iso3c=="KSV")]="XKX"
pcniso$iso3c[which(pcniso$iso3c=="TMP")]="TLS"
pcniso$iso3c[which(pcniso$iso3c=="ROM")]="ROU"

medianincomes2008 = subset.data.frame(pcniso, hc>=49 & hc<=51)
medianincomes2008 = data.table(medianincomes2008)[,.(
  medianincome = median(pl, na.rm=TRUE)
), by=.(iso3c)]
medianincomes2008$year = 2008

pcn_2005 <- read_csv("~/Poverty data/years/pcn.2005.csv")
metadata <- read_excel("~/Poverty data/Country metadata.xlsx")
names(metadata)[8]="iso3c"
names(metadata)[12]="code2"
setnames(pcn_2005, "country", "PovcalNet name")
pcniso =merge(metadata,pcn_2005, by="PovcalNet name", all=TRUE)
pcniso$iso3c[which(pcniso$iso3c=="KSV")]="XKX"
pcniso$iso3c[which(pcniso$iso3c=="TMP")]="TLS"
pcniso$iso3c[which(pcniso$iso3c=="ROM")]="ROU"

medianincomes2005 = subset.data.frame(pcniso, hc>=49 & hc<=51)
medianincomes2005 = data.table(medianincomes2005)[,.(
  medianincome = median(pl, na.rm=TRUE)
), by=.(iso3c)]
medianincomes2005$year = 2005

pcn_2002 <- read_csv("~/Poverty data/years/pcn.2002.csv")
metadata <- read_excel("~/Poverty data/Country metadata.xlsx")
names(metadata)[8]="iso3c"
names(metadata)[12]="code2"
setnames(pcn_2002, "country", "PovcalNet name")
pcniso =merge(metadata,pcn_2002, by="PovcalNet name", all=TRUE)
pcniso$iso3c[which(pcniso$iso3c=="KSV")]="XKX"
pcniso$iso3c[which(pcniso$iso3c=="TMP")]="TLS"
pcniso$iso3c[which(pcniso$iso3c=="ROM")]="ROU"

medianincomes2002 = subset.data.frame(pcniso, hc>=49 & hc<=51)
medianincomes2002 = data.table(medianincomes2002)[,.(
  medianincome = median(pl, na.rm=TRUE)
), by=.(iso3c)]
medianincomes2002$year = 2002

pcn_1999 <- read_csv("~/Poverty data/years/pcn.1999.csv")
metadata <- read_excel("~/Poverty data/Country metadata.xlsx")
names(metadata)[8]="iso3c"
names(metadata)[12]="code2"
setnames(pcn_1999, "country", "PovcalNet name")
pcniso =merge(metadata,pcn_1999, by="PovcalNet name", all=TRUE)
pcniso$iso3c[which(pcniso$iso3c=="KSV")]="XKX"
pcniso$iso3c[which(pcniso$iso3c=="TMP")]="TLS"
pcniso$iso3c[which(pcniso$iso3c=="ROM")]="ROU"

medianincomes1999 = subset.data.frame(pcniso, hc>=49 & hc<=51)
medianincomes1999 = data.table(medianincomes1999)[,.(
  medianincome = median(pl, na.rm=TRUE)
), by=.(iso3c)]
medianincomes1999$year = 1999

pcn_1996 <- read_csv("~/Poverty data/years/pcn.1996.csv")
metadata <- read_excel("~/Poverty data/Country metadata.xlsx")
names(metadata)[8]="iso3c"
names(metadata)[12]="code2"
setnames(pcn_1996, "country", "PovcalNet name")
pcniso =merge(metadata,pcn_1996, by="PovcalNet name", all=TRUE)
pcniso$iso3c[which(pcniso$iso3c=="KSV")]="XKX"
pcniso$iso3c[which(pcniso$iso3c=="TMP")]="TLS"
pcniso$iso3c[which(pcniso$iso3c=="ROM")]="ROU"

medianincomes1996 = subset.data.frame(pcniso, hc>=49 & hc<=51)
medianincomes1996 = data.table(medianincomes1996)[,.(
  medianincome = median(pl, na.rm=TRUE)
), by=.(iso3c)]
medianincomes1996$year = 1996

pcn_1993 <- read_csv("~/Poverty data/years/pcn.1993.csv")
metadata <- read_excel("~/Poverty data/Country metadata.xlsx")
names(metadata)[8]="iso3c"
names(metadata)[12]="code2"
setnames(pcn_1993, "country", "PovcalNet name")
pcniso =merge(metadata,pcn_1993, by="PovcalNet name", all=TRUE)
pcniso$iso3c[which(pcniso$iso3c=="KSV")]="XKX"
pcniso$iso3c[which(pcniso$iso3c=="TMP")]="TLS"
pcniso$iso3c[which(pcniso$iso3c=="ROM")]="ROU"

medianincomes1993 = subset.data.frame(pcniso, hc>=49 & hc<=51)
medianincomes1993 = data.table(medianincomes1993)[,.(
  medianincome = median(pl, na.rm=TRUE)
), by=.(iso3c)]
medianincomes1993$year = 1993

pcn_1990 <- read_csv("~/Poverty data/years/pcn.1990.csv")
metadata <- read_excel("~/Poverty data/Country metadata.xlsx")
names(metadata)[8]="iso3c"
names(metadata)[12]="code2"
setnames(pcn_1990, "country", "PovcalNet name")
pcniso =merge(metadata,pcn_1990, by="PovcalNet name", all=TRUE)
pcniso$iso3c[which(pcniso$iso3c=="KSV")]="XKX"
pcniso$iso3c[which(pcniso$iso3c=="TMP")]="TLS"
pcniso$iso3c[which(pcniso$iso3c=="ROM")]="ROU"

medianincomes1990 = subset.data.frame(pcniso, hc>=49 & hc<=51)
medianincomes1990 = data.table(medianincomes1990)[,.(
  medianincome = median(pl, na.rm=TRUE)
), by=.(iso3c)]
medianincomes1990$year = 1990



medians=merge(medianincomes1990,medianincomes1993,by="iso3c")
setnames(medians,"medianincome.x","1990")
setnames(medians,"medianincome.y","1993")
medians=merge(medians,medianincomes1996,by="iso3c")
medians=merge(medians,medianincomes1999,by="iso3c")
setnames(medians,"medianincome.x","1996")
setnames(medians,"medianincome.y","1999")
medians$year.x = NULL
medians$year.y = NULL
medians$year.x = NULL
medians$year.y = NULL
medians=merge(medians,medianincomes2002,by="iso3c")
medians=merge(medians,medianincomes2005,by="iso3c")
setnames(medians,"medianincome.x","2002")
setnames(medians,"medianincome.y","2005")
medians=merge(medians,medianincomes2008,by="iso3c")
medians=merge(medians,medianincomes2010,by="iso3c")
setnames(medians,"medianincome.x","2008")
setnames(medians,"medianincome.y","2010")
medians$year.x = NULL
medians$year.y = NULL
medians$year.x = NULL
medians$year.y = NULL
medians=merge(medians,medianincomes2011,by="iso3c")
medians=merge(medians,medianincomes2012,by="iso3c")
setnames(medians,"medianincome.x","2011")
setnames(medians,"medianincome.y","2012")
medians=merge(medians,medianincomes2013,by="iso3c")
setnames(medians,"medianincome","2013")
medians$year.x = NULL
medians$year.y = NULL
medians$year = NULL

write.csv(medians,"~/Poverty projections/medianincomes.csv", row.names=FALSE, na="")


