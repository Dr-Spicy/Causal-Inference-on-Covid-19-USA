library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)



states = read.csv("C:/Users/Han/Desktop/Box Sync/CS 520 Causual Inference/UMD Data/State.csv")
states = as.data.table(states)
states$Week <- floor_date(mdy(states$date), "week")

# remove the state acrynom 
AL = states[STFIPS == 1][1:476,-2]
AK = states[STFIPS == 2][1:476,-2]
AZ = states[STFIPS == 4][1:476,-2]
AR = states[STFIPS == 5][1:476,-2]
CA = states[STFIPS == 6][1:476,-2]
CO = states[STFIPS == 8][1:476,-2]
CT = states[STFIPS == 9][1:476,-2]
DE = states[STFIPS == 10][1:476,-2]
FL = states[STFIPS == 12][1:476,-2]
GA = states[STFIPS == 13][1:476,-2]
HI = states[STFIPS == 15][1:476,-2]
ID = states[STFIPS == 16][1:476,-2]
IL = states[STFIPS == 17][1:476,-2]
IN = states[STFIPS == 18][1:476,-2]
IA = states[STFIPS == 19][1:476,-2]
KS = states[STFIPS == 20][1:476,-2]
KY = states[STFIPS == 21][1:476,-2]
LA = states[STFIPS == 22][1:476,-2]
ME = states[STFIPS == 23][1:476,-2]
MD = states[STFIPS == 24][1:476,-2]
MA = states[STFIPS == 25][1:476,-2]
MI = states[STFIPS == 26][1:476,-2]
MN = states[STFIPS == 27][1:476,-2]
MS = states[STFIPS == 28][1:476,-2]
MO = states[STFIPS == 29][1:476,-2]
MT = states[STFIPS == 30][1:476,-2]
NE = states[STFIPS == 31][1:476,-2]
NV = states[STFIPS == 32][1:476,-2]
NH = states[STFIPS == 33][1:476,-2]
NJ = states[STFIPS == 34][1:476,-2]
NM = states[STFIPS == 35][1:476,-2]
NY = states[STFIPS == 36][1:476,-2]
NC = states[STFIPS == 37][1:476,-2]
ND = states[STFIPS == 38][1:476,-2]
OH = states[STFIPS == 39][1:476,-2]
OK = states[STFIPS == 40][1:476,-2]
OR = states[STFIPS == 41][1:476,-2]
PA = states[STFIPS == 42][1:476,-2]
RI = states[STFIPS == 44][1:476,-2]
SC = states[STFIPS == 45][1:476,-2]
SD = states[STFIPS == 46][1:476,-2]
TN = states[STFIPS == 47][1:476,-2]
TX = states[STFIPS == 48][1:476,-2]
UT = states[STFIPS == 49][1:476,-2]
VT = states[STFIPS == 50][1:476,-2]
VA = states[STFIPS == 51][1:476,-2]
WA = states[STFIPS == 53][1:476,-2]
WV = states[STFIPS == 54][1:476,-2]
WI = states[STFIPS == 55][1:476,-2]
WY = states[STFIPS == 56][1:476,-2]

# Daily new COVID cases of US
New.COVID.cases <- AL$New.COVID.cases+ AK $New.COVID.cases + AZ$New.COVID.cases + AR$New.COVID.cases +
    CA$New.COVID.cases + CO$New.COVID.cases + CT$New.COVID.cases + DE$New.COVID.cases + FL$New.COVID.cases +
    GA$New.COVID.cases + HI$New.COVID.cases + ID$New.COVID.cases + IL$New.COVID.cases + IN$New.COVID.cases +
    IA$New.COVID.cases + KS$New.COVID.cases + KY$New.COVID.cases + LA$New.COVID.cases +
    ME$New.COVID.cases + MD$New.COVID.cases + MA$New.COVID.cases + MI$New.COVID.cases +
    MN$New.COVID.cases + MS$New.COVID.cases + MO$New.COVID.cases + MT$New.COVID.cases + NE$New.COVID.cases + 
    NV$New.COVID.cases + NH$New.COVID.cases + NJ$New.COVID.cases + NM$New.COVID.cases + NY$New.COVID.cases +
    NC$New.COVID.cases + ND$New.COVID.cases + OH$New.COVID.cases + OK$New.COVID.cases + OR$New.COVID.cases + 
    PA$New.COVID.cases + RI$New.COVID.cases + SC$New.COVID.cases + SD$New.COVID.cases + TN$New.COVID.cases + 
    TX$New.COVID.cases + UT$New.COVID.cases + VT$New.COVID.cases + VA$New.COVID.cases + WA$New.COVID.cases + 
    WV$New.COVID.cases + WI$New.COVID.cases + WY$New.COVID.cases
New.COVID.cases.US <- cbind(states$date[1:476], New.COVID.cases)

# Get the 7-day moving average of new cases
SevenDayAveg.new.cases <- as.integer(rollmean(c(rep(0,3), New.COVID.cases, rep(0,3)), 7))
SevenDayAveg.new.cases.US <- cbind(states$date[1:476], SevenDayAveg.new.cases)

# limit the time range from March 21st 2020 (Sunday, 82nd row) to Mar 20th 2021 (Sunday, 445th row)
SevenDayAveg.new.cases.US.window <- cbind(states$date[82:445], New.COVID.cases[82:445], SevenDayAveg.new.cases[82:445])
colnames(SevenDayAveg.new.cases.US.window) = c('Date','Daily', '7-Day')

write.csv(SevenDayAveg.new.cases.US.window,"C:/Users/Han/Desktop/Box Sync/CS 520 Causual Inference/UMD Data/7 day aveg new cases (2020-03-22 to 2021-03-20).csv")
