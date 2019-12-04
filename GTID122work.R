#Begin work in downloads directory
setwd("~/Downloads")

#Load the dataframe
ProjectData <- read.csv("YearlyFile121.csv")

#Hide tourism employment column from data frame
ProjectData$Tourism_Employed <- NULL

#Remove commas from numbers in dataframe
ProjectData$Pop <- as.numeric(gsub(",","",ProjectData$Pop))
ProjectData$GDP_Millions_Euro <- as.numeric(gsub(",","",ProjectData$GDP_Millions_Euro))
ProjectData$Total_Household_Income <- as.numeric(gsub(",","",ProjectData$Total_Household_Income))
ProjectData$Tourism_NightStays <- as.numeric(gsub(",","",ProjectData$Tourism_NightStays))
ProjectData$Flight_Arrivals <- as.numeric(gsub(",","",ProjectData$Flight_Arrivals))

#Generate basic summary statistics of the data and save as a file
sink("ProjectStatsGTID.txt")
print(summary(ProjectData))
sink()

#Load packages necessary to run IV regression
library("AER")

#Run an iv regression using the years before or after a budget airline as an instrument on the flight arrivals
firstivreg122 <- ivreg(GDP_Millions_Euro~Flight_Arrivals+Tourism_NightStays+Pop+Total_Household_Income+Econ_Freedom|Year_PostBudget+Tourism_NightStays+Pop+Total_Household_Income+Econ_Freedom, data=ProjectData)
print(summary(firstivreg122))
#Compare this to the regression without instrumentation
firstregress122 <- lm(GDP_Millions_Euro ~ Flight_Arrivals + Pop + Tourism_NightStays+Total_Household_Income+Econ_Freedom, data=ProjectData)
print(summary(firstregress122))
#Notice the drastic change in the effect that flight arrivals variable has on GDP. Even a switch in signs!
#Looking into the first stage that occurred in this 2SLS regression
thirdregress122 <- lm(Flight_Arrivals ~ Year_PostBudget + Pop + Tourism_NightStays + GDP_Millions_Euro + Econ_Freedom + Total_Household_Income, data=ProjectData)
print(summary(thirdregress122))

#What is the effect that the addition of a budget airline has on local economies
secondregress122 <- lm(GDP_Millions_Euro ~ Year_PostBudget + Flight_Arrivals + Pop + Tourism_NightStays+Total_Household_Income+Econ_Freedom, data=ProjectData)
print(summary(secondregress122))

#We are going to graph the two linear regressions to test for heteroskedasticity
par(mfrow=c(2,2))
plot(firstregress122)
plot(secondregress122)
#These graphs do not give a clear representation, the Q-Q plots are both heavy-tailed
lmtest::bptest(firstregress122)
lmtest::bptest(secondregress122)
car::ncvTest(firstregress122)
car::ncvTest(secondregress122)

#These show that there is most likely heteroskedasticity present. Maybe we need to log transform the data to reduce this potential.
#Load packages necessary to log transform data easily
library("tidyverse")
ProjectData <- ProjectData %>% 
  mutate(GDP_log = log(1 + ProjectData$GDP_Millions_Euro))
ProjectData <- ProjectData %>% 
  mutate(Pop_log = log(1 + ProjectData$Pop))
ProjectData <- ProjectData %>% 
  mutate(Wages_log = log(1 + ProjectData$Total_Household_Income))
ProjectData <- ProjectData %>% 
  mutate(TourNights_log = log(1 + ProjectData$Tourism_NightStays))
ProjectData <- ProjectData %>% 
  mutate(Arrivals_log = log(1 + ProjectData$Flight_Arrivals))

#Now let's try and rerun the regressions from earlier
firstivreg123 <- ivreg(GDP_log ~ Arrivals_log + TourNights_log + Pop_log + Wages_log + Econ_Freedom|Year_PostBudget + TourNights_log + Pop_log + Wages_log + Econ_Freedom, data=ProjectData)
print(summary(firstivreg123))
#Compare this to the regression without instrumentation 
firstregress123 <- lm(GDP_log ~ Arrivals_log + TourNights_log + Wages_log + Econ_Freedom, data=ProjectData)
print(summary(firstregress123))
#Notice the change in the effect that flight arrivals variable has on GDP is much smaller. Instrumenting increased coefficient effect. Much more like what we'd expect
#Let's now take a look into the first stage regression that ocurred in the IV
fourthregress123 <- lm(Arrivals_log ~ Year_PostBudget + TourNights_log + Pop_log + Wages_log + Econ_Freedom + GDP_log, data = ProjectData)
#A look at how arrivals are affected by other variables without dummy. Pop removed for potential colinearity. Tourism nights for same
fifthregress123 <- lm(Arrivals_log ~ Wages_log + Econ_Freedom + GDP_log, data = ProjectData)
print(summary(fifthregress123))
#Now let's add back the dummy and see its affect
sixthregress123 <- lm(Arrivals_log ~ Year_PostBudget + Wages_log + Econ_Freedom + GDP_log, data = ProjectData)
print(summary(sixthregress123))
#Let's now take away all of the control variables and just test for the affect of budget airline on log arrivals
seventhregress123 <- lm(Arrivals_log ~ Year_PostBudget, data = ProjectData)
print(summary(seventhregress123))


#Now let's see what the effect that the addition of a budget airline has on local economies' GDP in log growth.
secondregress123 <- lm(GDP_log ~ Year_PostBudget + Arrivals_log + TourNights_log + Wages_log + Econ_Freedom, data=ProjectData)
print(summary(secondregress123))

#Let's run a regresssion testing for effect of tourist nights on GDP
eighthregress123 <- lm(GDP_log ~ TourNights_log + Wages_log + Econ_Freedom, data=ProjectData)
print(summary(eighthregress123))

#Let's re-check for heteroskedasticity with transformed regressions in their graphs
par(mfrow=c(2,2))
plot(firstregress123)
plot(secondregress123)
#The graphs look somewhat better. What about if we check using statistical tests?
lmtest::bptest(firstregress123)
lmtest::bptest(secondregress123)
car::ncvTest(firstregress123)
car::ncvTest(secondregress123)
#These still show some heteroskedasticity, but a lot less.

#Now let's look at how adding a budget airline affects log change of total earned wages in the region.
thirdregress123 <- lm(Wages_log ~ Year_PostBudget + Arrivals_log + Pop_log + TourNights_log + GDP_log + Econ_Freedom, data=ProjectData)
print(summary(thirdregress123))

#Let's make some graphs
library(cowplot)
library(reshape2)
ggplot(data=ProjectData)+
  geom_point(aes(x=Flight_Arrivals, y=GDP_Millions_Euro),color="maroon")+theme_bw()+
  xlab("Regional Flight Arrivals")+ylab("GDP (millions of Euros)")
ggsave("ArrivalsvsGDP.png")

ggplot(data=ProjectData)+
  geom_boxplot(aes(x=Country, y=Flight_Arrivals), color="maroon")+theme_bw()
ggsave("ArrivalsbyCountry.png")

AustriaShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("AT12" , "AT32")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Austria")

BulgariaShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("BG34")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Bulgaria")

CypressShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("CY00")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Cyprus")

CzechShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("CZ01")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Czechia")

plot_grid(AustriaShock, BulgariaShock, CypressShock, CzechShock, nrow=2)
ggsave("ATBGCYCZ.png")

GermanyShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("DE12" , "DE25", "DE92")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Germany")

DenmarkShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("DK03")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Denmark")

GreeceShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("EL30" , "EL52")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Greece")

SpainShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("ES11" , "ES21", "ES53", "ES61", "ES70")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Spain")

plot_grid(GermanyShock, DenmarkShock, GreeceShock, SpainShock, nrow=2)
ggsave("DEDKELES.png")

FinlandShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("FI1B")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Finland")

FranceShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("FRL0" , "FRK2", "FRJ2", "FRI1", "FRG0")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in France")

HungaryShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("HU32")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Hungary")

ItalyShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("ITG2" , "ITG1", "ITF4", "ITF1")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Italy")

plot_grid(FinlandShock, FranceShock, HungaryShock, ItalyShock, nrow=2)
ggsave("FIFRHUIT.png")

LithuaniaShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("LT01" , "LT02")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Lithuania")

LatviaShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("LV00")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Latvia")

MaltaShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("MT00")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Malta")

HollandShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("NL33" , "NL41", "NL42")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in The Netherlands")

plot_grid(LithuaniaShock, LatviaShock, MaltaShock, HollandShock, nrow=2)
ggsave("LTLVMTNL.png")

PolandShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("PL41" , "PL51")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Poland")

PortugalShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("PT11" , "PT15", "PT17")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Portugal")

RomaniaShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("RO11" , "RO21", "RO41", "RO42")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Romania")

SwedenShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("SE23")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Sweden")

plot_grid(PolandShock, PortugalShock, RomaniaShock, SwedenShock, nrow=2)
ggsave("PLPTROSE.png")

SlovakiaShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("SK01" , "SK04")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in Slovakia")

UKShock <- ggplot(subset(ProjectData,NUTS2_Code %in% c("UKE3" , "UKK2", "UKM5")), aes(x=Year_PostBudget, y=Flight_Arrivals))+
  stat_summary(geom="bar", fun.y="mean")+
  stat_summary(geom="errorbar", fun.data = "mean_se")+
  theme_bw()+xlab("Budget Shock in The United Kingdom")

plot_grid(SlovakiaShock, UKShock, nrow=1)
ggsave("SKUK.png")
