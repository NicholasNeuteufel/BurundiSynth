# Read data

setwd("C:/Users/nneuteuf/Dropbox/Senior Spring/IPE/Term Paper")
data <- read.csv(file="dotsSimple.csv", h=T)

library(dplyr)

# Rename the columns

newData <- rename(data, Exports=Goods..Value.of.Exports..Free.on.board..FOB...US.Dollars..TXG_FOB_USD., Country.Name=ï..Country.Name)

# Reduce the Exports columns
newData$Exports <- newData$Exports/1000000

# Years only

df1 <- newData[ grepl( "\\d\\d\\d\\d$" , newData$Time.Period ), ]

# Limit time period

df2 <- subset(df1, Time.Period=="1997" | Time.Period=="1998" | Time.Period=="1999" | Time.Period=="2000" | Time.Period=="2001" | Time.Period=="2002" | Time.Period=="2003" | Time.Period=="2004" | Time.Period=="2005")
df2$Time.Period <- as.numeric(as.character(df2$Time.Period))

# Select only the relevant columns

df3 <- select(df2, -c(Status, Status.1, X) )

# Replace names

df3$Country.Name <- as.character(df3$Country.Name)

df3$Country.Name[df3$Country.Name=="Congo, Democratic Republic of"] <- "Congo, Dem. Rep."
df3$Country.Name[df3$Country.Name=="Congo, Republic of"] <- "Congo, Rep."
df3$Country.Name[df3$Country.Name=="Cape Verde"] <- "Cabo Verde"

# WDI

library(WDI)

ind <- c(
  "NY.GDP.MKTP.KD.ZG", # Real GDP growth
  "PA.NUS.PPP", # PPP conversion factor
  "BX.KLT.DINV.WD.GD.ZS", # Net FDI (% of GDP)
  "NY.GDP.PCAP.CD", # Per capita GDP, real 2005
  "NY.GDP.TOTL.RT.ZS", # Total natural resource rents
  "NY.GDP.PETR.RT.ZS", # Oil rents 
  "AG.PRD.CREL.MT", # Agricultural cereal production (metric tons)
  "DC.DAC.TOTL.CD", # Net ODA, total--current dollars
  "NE.TRD.GNFS.ZS" # Urbanization
)

worldBank <- WDI(country="all", ind = ind, start=1995, end=2009)
df4 <- merge(x=df3, y=worldBank, by.x=c("Country.Name", "Time.Period"), by.y=c("country", "year") )

# Reform the columns

df5 <- select(df4, -c(iso2c) )
df6 <- rename(df5, Imports=Goods..Value.of.Imports..Free.on.board..FOB...US.Dollars..TMG_FOB_USD.)

# Impute missing natural resource and petroleum rents as 0

df6$NY.GDP.TOTL.RT.ZS[is.na(df6$NY.GDP.TOTL.RT.ZS)] <- 0
df6$NY.GDP.PETR.RT.ZS[is.na(df6$NY.GDP.PETR.RT.ZS)] <- 0

# Function for only complete cases of some columns 

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# I need GDP, growth, per capita GDP, PPP, net FDI, nat resource rents, petroleum rents, ag production, development assistance, inflation, population, and urbanization

df7 <- completeFun(df6, c(
  "NY.GDP.MKTP.KD.ZG", # GDP growth
  "NY.GDP.PCAP.CD", # per capita GDP
  "PA.NUS.PPP", # PPP conversion factor
  "BX.KLT.DINV.WD.GD.ZS", # Net FDI
  "NY.GDP.TOTL.RT.ZS", # Nat resource rents
  "NY.GDP.PETR.RT.ZS", # Oil rents
  "AG.PRD.CREL.MT", # Ag production
  "DC.DAC.TOTL.CD", # Development assistance
  "NE.TRD.GNFS.ZS" # Trade % of GDP
)
)

# Let's balance this panel set

balance <- df7
balance$Country.Name <- as.factor(balance$Country.Name)

# US-specific data

balanceUS <- subset(balance, Counterpart.Country.Name=="United States")
balanceUK <- subset(balance, Counterpart.Country.Name=="United Kingdom")
balanceGer <- subset(balance, Counterpart.Country.Name=="Germany")

# Option A: Include Gabon to foresake industry and service variables

balanceUSfull <- subset(balanceUS, Country.Name=="Benin" |Country.Name=="Central African Republic" |Country.Name=="Cote d'Ivoire" |Country.Name=="Malawi" |Country.Name=="Nigeria" |Country.Name=="Tanzania" |Country.Name=="Burkina Faso" |Country.Name=="Chad" |Country.Name=="Ethiopia" |Country.Name=="Guinea-Bissau" |Country.Name=="Mali" |Country.Name=="Togo" |Country.Name=="Burundi" |Country.Name=="Gabon" |Country.Name=="Kenya" |Country.Name=="Mauritius" |Country.Name=="Senegal" |Country.Name=="Uganda" |Country.Name=="Cabo Verde" |Country.Name=="Congo, Dem. Rep" |Country.Name=="Mozambique" | Country.Name=="Sierra Leone" |  Country.Name=="Zambia" | Country.Name=="Cameroon" | Country.Name=="Ghana" | Country.Name=="Madagascar" |  Country.Name=="Niger")
balanceUKfull <- subset(balanceUK, Country.Name=="Benin" |Country.Name=="Central African Republic" |Country.Name=="Cote d'Ivoire" |Country.Name=="Malawi" |Country.Name=="Nigeria" |Country.Name=="Tanzania" |Country.Name=="Burkina Faso" |Country.Name=="Chad" |Country.Name=="Ethiopia" |Country.Name=="Guinea-Bissau" |Country.Name=="Mali" |Country.Name=="Togo" |Country.Name=="Burundi" |Country.Name=="Gabon" |Country.Name=="Kenya" |Country.Name=="Mauritius" |Country.Name=="Senegal" |Country.Name=="Uganda" |Country.Name=="Cabo Verde" |Country.Name=="Congo, Dem. Rep" |Country.Name=="Mozambique" | Country.Name=="Sierra Leone" |  Country.Name=="Zambia" | Country.Name=="Cameroon" | Country.Name=="Ghana" | Country.Name=="Madagascar" |  Country.Name=="Niger")
balanceGerfull <- subset(balanceGer, Country.Name=="Benin" |Country.Name=="Central African Republic" |Country.Name=="Cote d'Ivoire" |Country.Name=="Malawi" |Country.Name=="Nigeria" |Country.Name=="Tanzania" |Country.Name=="Burkina Faso" |Country.Name=="Chad" |Country.Name=="Ethiopia" |Country.Name=="Guinea-Bissau" |Country.Name=="Mali" |Country.Name=="Togo" |Country.Name=="Burundi" |Country.Name=="Gabon" |Country.Name=="Kenya" |Country.Name=="Mauritius" |Country.Name=="Senegal" |Country.Name=="Uganda" |Country.Name=="Cabo Verde" |Country.Name=="Congo, Dem. Rep" |Country.Name=="Mozambique" | Country.Name=="Sierra Leone" |  Country.Name=="Zambia" | Country.Name=="Cameroon" | Country.Name=="Ghana" | Country.Name=="Madagascar" |  Country.Name=="Niger")

# Synth method

library(Synth)

# Analysis

# Change Country.Name  to character vector

balanceUSfull$Country.Name <- as.character(balanceUSfull$Country.Name)
balanceUKfull$Country.Name <- as.character(balanceUKfull$Country.Name)
balanceGerfull$Country.Name <- as.character(balanceGerfull$Country.Name)

# Do Synth

prepped <- dataprep(
  foo = balanceUSfull, # Change this data set!
  predictors = c(
    "NY.GDP.MKTP.KD.ZG",
    "PA.NUS.PPP",
    "BX.KLT.DINV.WD.GD.ZS",
    "NY.GDP.PCAP.CD",
    "NY.GDP.TOTL.RT.ZS",
    "NY.GDP.PETR.RT.ZS",
    "NE.TRD.GNFS.ZS", # Trade % of GDP
    "AG.PRD.CREL.MT"
    #"SP.URB.TOTL.IN.ZS"
  ),
  predictors.op = "mean",
  dependent = "Exports", 
  unit.variable = "Country.Code",
  time.variable = "Time.Period",
  #special.predictors = list (
  #  ),
  treatment.identifier = "Burundi",
  controls.identifier = c("Benin", 
                          "Burkina Faso",
                          "Cabo Verde",
                          "Cameroon",
                          "Central African Republic",
                          "Cote d'Ivoire",
                          #"Ethiopia",
                          "Gabon",
                          "Ghana",
                          "Guinea-Bissau",
                          "Kenya",
                          "Madagascar",
                          "Malawi",
                          "Mali",
                          "Mauritius",
                          "Mozambique",
                          "Niger",
                          "Nigeria",
                          "Senegal",
                          "Sierra Leone",
                          "Tanzania",
                          "Togo",
                          "Uganda",
                          "Zambia"
  ),
  time.predictors.prior = c(1998:2000),
  time.optimize.ssr = c(1998:2000),
  unit.names.variable = "Country.Name",
  time.plot = 1998:2003
)

# Run actual syth method

synth.out <- synth(prepped)

# Unit weights

round(synth.out$solution.w, 2)

# Predictor weights

round(synth.out$solution.v, 2)

# Table 

synth.tables <- synth.tab(
  dataprep.res = prepped,
  synth.res = synth.out
)

synth.tables$tab.pred

# Graphing

path.plot(synth.res = synth.out,
          dataprep.res = prepped,
          Ylab = c("Burundi's exports to the U.S., Current USD (millions)"),
          Xlab = c("Year"),
          Ylim = c(0,3),
          Legend = c("Real","Synthetic"),
          Legend.position = "topleft"
)

# Gaps

gaps.plot(synth.res = synth.out,
          dataprep.res = prepped,
          Ylab = c("Gap in exports to the U.S., Current USD (millions)"),
          Xlab = c("Year"),
          Ylim = c(-2.5,2.5),
)

# UK 

prepped1 <- dataprep(
  foo = balanceUKfull, # Change this data set!
  predictors = c(
    "NY.GDP.MKTP.KD.ZG",
    "PA.NUS.PPP",
    "BX.KLT.DINV.WD.GD.ZS",
    "NY.GDP.PCAP.CD",
    "NY.GDP.TOTL.RT.ZS",
    "NY.GDP.PETR.RT.ZS",
    "NE.TRD.GNFS.ZS", # Trade % of GDP
    "AG.PRD.CREL.MT"
    #"SP.URB.TOTL.IN.ZS"
  ),
  predictors.op = "mean",
  dependent = "Exports", 
  unit.variable = "Country.Code",
  time.variable = "Time.Period",
  #special.predictors = list (
  #  ),
  treatment.identifier = "Burundi",
  controls.identifier = c("Benin", 
                          "Burkina Faso",
                          "Cabo Verde",
                          "Cameroon",
                          "Central African Republic",
                          "Cote d'Ivoire",
                          #"Ethiopia",
                          "Gabon",
                          "Ghana",
                          "Guinea-Bissau",
                          "Kenya",
                          "Madagascar",
                          "Malawi",
                          "Mali",
                          "Mauritius",
                          "Mozambique",
                          "Niger",
                          "Nigeria",
                          "Senegal",
                          "Sierra Leone",
                          "Tanzania",
                          "Togo",
                          "Uganda",
                          "Zambia"
  ),
  time.predictors.prior = c(1998:2000),
  time.optimize.ssr = c(1998:2000),
  unit.names.variable = "Country.Name",
  time.plot = 1998:2003
)

# Run actual syth method

synth.out1 <- synth(prepped1)

# Unit weights

round(synth.out1$solution.w, 2)

# Predictor weights

round(synth.out1$solution.v, 2)

# Table 

synth.tables1 <- synth.tab(
  dataprep.res = prepped1,
  synth.res = synth.out1
)

synth.tables1$tab.pred

# Graphing

path.plot(synth.res = synth.out1,
          dataprep.res = prepped1,
          Ylab = c("Burundi's exports to the U.K., Current USD (millions)"),
          Xlab = c("Year"),
          Ylim = c(0,30),
          Legend = c("Real","Synthetic"),
          Legend.position = "topleft"
)

# Gaps

gaps.plot(synth.res = synth.out1,
          dataprep.res = prepped1,
          Ylab = c("Gap in exports to the U.K., Current USD (millions)"),
          Xlab = c("Year"),
          Ylim = c(-10,10),
)


# Placebo -- Togo

prepped2 <- dataprep(
  foo = balanceUSfull, # Change this data set!
  predictors = c(
    "NY.GDP.MKTP.KD.ZG",
    "PA.NUS.PPP",
    "BX.KLT.DINV.WD.GD.ZS",
    "NY.GDP.PCAP.CD",
    "NY.GDP.TOTL.RT.ZS",
    "NY.GDP.PETR.RT.ZS",
    "NE.TRD.GNFS.ZS", # Trade % of GDP
    "AG.PRD.CREL.MT"
    #"SP.URB.TOTL.IN.ZS"
  ),
  predictors.op = "mean",
  dependent = "Exports", 
  unit.variable = "Country.Code",
  time.variable = "Time.Period",
  #special.predictors = list (
  #  ),
  treatment.identifier = "Togo",
  controls.identifier = c("Benin", 
                          "Burkina Faso",
                          "Burundi",
                          "Cabo Verde",
                          "Cameroon",
                          "Central African Republic",
                          "Cote d'Ivoire",
                          #"Ethiopia",
                          "Gabon",
                          "Ghana",
                          "Guinea-Bissau",
                          "Kenya",
                          "Madagascar",
                          "Malawi",
                          "Mali",
                          "Mauritius",
                          "Mozambique",
                          "Niger",
                          "Nigeria",
                          "Senegal",
                          "Sierra Leone",
                          "Tanzania",
                          #"Togo",
                          "Uganda",
                          "Zambia"
  ),
  time.predictors.prior = c(1998:2000),
  time.optimize.ssr = c(1998:2000),
  unit.names.variable = "Country.Name",
  time.plot = 1998:2003
)

# Run actual syth method

synth.out2 <- synth(prepped2)

# Unit weights

round(synth.out2$solution.w, 2)

# Predictor weights

round(synth.out2$solution.v, 2)

# Table 

synth.tables1 <- synth.tab(
  dataprep.res = prepped2,
  synth.res = synth.out2
)

synth.tables2$tab.pred

# Graphing

path.plot(synth.res = synth.out2,
          dataprep.res = prepped2,
          Ylab = c("Togo's exports to the U.S., Current USD (millions)"),
          Xlab = c("Year"),
          Ylim = c(0,8),
          Legend = c("Real","Synthetic"),
          Legend.position = "topleft"
)

# Placebo -- Togo

prepped2 <- dataprep(
  foo = balanceUKfull, # Change this data set!
  predictors = c(
    "NY.GDP.MKTP.KD.ZG",
    "PA.NUS.PPP",
    "BX.KLT.DINV.WD.GD.ZS",
    "NY.GDP.PCAP.CD",
    "NY.GDP.TOTL.RT.ZS",
    "NY.GDP.PETR.RT.ZS",
    "NE.TRD.GNFS.ZS", # Trade % of GDP
    "AG.PRD.CREL.MT"
    #"SP.URB.TOTL.IN.ZS"
  ),
  predictors.op = "mean",
  dependent = "Exports", 
  unit.variable = "Country.Code",
  time.variable = "Time.Period",
  #special.predictors = list (
  #  ),
  treatment.identifier = "Togo",
  controls.identifier = c("Benin", 
                          "Burkina Faso",
                          "Burundi",
                          "Cabo Verde",
                          "Cameroon",
                          "Central African Republic",
                          "Cote d'Ivoire",
                          #"Ethiopia",
                          "Gabon",
                          "Ghana",
                          "Guinea-Bissau",
                          "Kenya",
                          "Madagascar",
                          "Malawi",
                          "Mali",
                          "Mauritius",
                          "Mozambique",
                          "Niger",
                          "Nigeria",
                          "Senegal",
                          "Sierra Leone",
                          "Tanzania",
                          #"Togo",
                          "Uganda",
                          "Zambia"
  ),
  time.predictors.prior = c(1998:2000),
  time.optimize.ssr = c(1998:2000),
  unit.names.variable = "Country.Name",
  time.plot = 1998:2003
)

# Run actual syth method

synth.out2 <- synth(prepped2)

# Unit weights

round(synth.out2$solution.w, 2)

# Predictor weights

round(synth.out2$solution.v, 2)

# Table 

synth.tables1 <- synth.tab(
  dataprep.res = prepped2,
  synth.res = synth.out2
)

synth.tables2$tab.pred

# Graphing

path.plot(synth.res = synth.out2,
          dataprep.res = prepped2,
          Ylab = c("Togo's exports to the U.K., Current USD (millions)"),
          Xlab = c("Year"),
          Ylim = c(0,8),
          Legend = c("Real","Synthetic"),
          Legend.position = "topleft"
)

# Placebo -- Gabon

prepped3 <- dataprep(
  foo = balanceUKfull, # Change this data set!
  predictors = c(
    "NY.GDP.MKTP.KD.ZG",
    "PA.NUS.PPP",
    "BX.KLT.DINV.WD.GD.ZS",
    "NY.GDP.PCAP.CD",
    "NY.GDP.TOTL.RT.ZS",
    "NY.GDP.PETR.RT.ZS",
    "NE.TRD.GNFS.ZS", # Trade % of GDP
    "AG.PRD.CREL.MT"
    #"SP.URB.TOTL.IN.ZS"
  ),
  predictors.op = "mean",
  dependent = "Exports", 
  unit.variable = "Country.Code",
  time.variable = "Time.Period",
  #special.predictors = list (
  #  ),
  treatment.identifier = "Gabon",
  controls.identifier = c("Benin", 
                          "Burkina Faso",
                          "Burundi",
                          "Cabo Verde",
                          "Cameroon",
                          "Central African Republic",
                          "Cote d'Ivoire",
                          #"Ethiopia",
                          #"Gabon",
                          "Ghana",
                          "Guinea-Bissau",
                          "Kenya",
                          "Madagascar",
                          "Malawi",
                          "Mali",
                          "Mauritius",
                          "Mozambique",
                          "Niger",
                          "Nigeria",
                          "Senegal",
                          "Sierra Leone",
                          "Tanzania",
                          "Togo",
                          "Uganda",
                          "Zambia"
  ),
  time.predictors.prior = c(1998:2000),
  time.optimize.ssr = c(1998:2000),
  unit.names.variable = "Country.Name",
  time.plot = 1998:2003
)

# Run actual syth method

synth.out3 <- synth(prepped3)

# Unit weights

round(synth.out3$solution.w, 2)

# Predictor weights

round(synth.out3$solution.v, 2)

# Table 

synth.tables3 <- synth.tab(
  dataprep.res = prepped3,
  synth.res = synth.out3
)

synth.tables3$tab.pred

# Graphing

path.plot(synth.res = synth.out3,
          dataprep.res = prepped3,
          Ylab = c("Gabon's exports to the U.K., Current USD (millions)"),
          Xlab = c("Year"),
          Ylim = c(0,40),
          Legend = c("Real","Synthetic"),
          Legend.position = "topleft"
)