

#states = read.csv("~/Desktop/Box Sync/CS 520 Causual Inference/UMD Data/State.csv")

states = read.csv("UMD Data/State.csv")
states = as.data.table(states)
states$date = lubridate::mdy(states$date)
states$Week <- lubridate::floor_date(states$date, "week", week_start = 1)

#### limit the time range from March 23nd 2020 (Monday, 82rd row) to Mar 21nd 2021 (Sunday, 445th row) ####
# remove the state acrynom 
AL = states[STFIPS == 1][83:446,]
AK = states[STFIPS == 2][83:446,]
AZ = states[STFIPS == 4][83:446,]
AR = states[STFIPS == 5][83:446,]
CA = states[STFIPS == 6][83:446,]
CO = states[STFIPS == 8][83:446,]
CT = states[STFIPS == 9][83:446,]
DE = states[STFIPS == 10][83:446,]
FL = states[STFIPS == 12][83:446,]
GA = states[STFIPS == 13][83:446,]
HI = states[STFIPS == 15][83:446,]
ID = states[STFIPS == 16][83:446,]
IL = states[STFIPS == 17][83:446,]
IN = states[STFIPS == 18][83:446,]
IA = states[STFIPS == 19][83:446,]
KS = states[STFIPS == 20][83:446,]
KY = states[STFIPS == 21][83:446,]
LA = states[STFIPS == 22][83:446,]
ME = states[STFIPS == 23][83:446,]
MD = states[STFIPS == 24][83:446,]
MA = states[STFIPS == 25][83:446,]
MI = states[STFIPS == 26][83:446,]
MN = states[STFIPS == 27][83:446,]
MS = states[STFIPS == 28][83:446,]
MO = states[STFIPS == 29][83:446,]
MT = states[STFIPS == 30][83:446,]
NE = states[STFIPS == 31][83:446,]
NV = states[STFIPS == 32][83:446,]
NH = states[STFIPS == 33][83:446,]
NJ = states[STFIPS == 34][83:446,]
NM = states[STFIPS == 35][83:446,]
NY = states[STFIPS == 36][83:446,]
NC = states[STFIPS == 37][83:446,]
ND = states[STFIPS == 38][83:446,]
OH = states[STFIPS == 39][83:446,]
OK = states[STFIPS == 40][83:446,]
OR = states[STFIPS == 41][83:446,]
PA = states[STFIPS == 42][83:446,]
RI = states[STFIPS == 44][83:446,]
SC = states[STFIPS == 45][83:446,]
SD = states[STFIPS == 46][83:446,]
TN = states[STFIPS == 47][83:446,]
TX = states[STFIPS == 48][83:446,]
UT = states[STFIPS == 49][83:446,]
VT = states[STFIPS == 50][83:446,]
VA = states[STFIPS == 51][83:446,]
WA = states[STFIPS == 53][83:446,]
WV = states[STFIPS == 54][83:446,]
WI = states[STFIPS == 55][83:446,]
WY = states[STFIPS == 56][83:446,]


if(response == 'New.case.1000.people'){

#### conver to weekly resolution ####
AL <- AL %>%
    group_by(Week) %>%
    summarise(State = 'AL', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
AK <- AK %>% 
    group_by(Week) %>%
    summarise(State = 'AK', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
AZ <- AZ %>%
    group_by(Week) %>%
    summarise(State = 'AZ', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
AR <- AR %>%
    group_by(Week) %>%
    summarise(State = 'AR', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
CA <- CA %>%
    group_by(Week) %>%
    summarise(State = 'CA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
CO <- CO %>%
    group_by(Week) %>%
    summarise(State = 'CO', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
CT <- CT %>%
    group_by(Week) %>%
    summarise(State = 'CT', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
DE <- DE %>%
    group_by(Week) %>%
    summarise(State = 'DE', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
FL <- FL %>%
    group_by(Week) %>%
    summarise(State = 'FL', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
GA <- GA %>%
    group_by(Week) %>%
    summarise(State = 'GA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
HI <- HI %>%
    group_by(Week) %>%
    summarise(State = 'HI', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
ID <- ID %>%
    group_by(Week) %>%
    summarise(State = 'ID', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
IL <- IL %>%
    group_by(Week) %>%
    summarise(State = 'IL', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
IN <- IN %>%
    group_by(Week) %>%
    summarise(State = 'IN', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
IA <- IA %>%
    group_by(Week) %>%
    summarise(State = 'IA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
KS <- KS %>%
    group_by(Week) %>%
    summarise(State = 'KS', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
KY <- KY %>%
    group_by(Week) %>%
    summarise(State = 'KY', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
LA <- LA %>%
    group_by(Week) %>%
    summarise(State = 'LA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
ME <- ME %>%
    group_by(Week) %>%
    summarise(State = 'ME', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
MD <- MD %>%
    group_by(Week) %>%
    summarise(State = 'MD', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
MA <- MA %>%
    group_by(Week) %>%
    summarise(State = 'MA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
MI <- MI %>%
    group_by(Week) %>%
    summarise(State = 'MI', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
MN <- MN %>%
    group_by(Week) %>%
    summarise(State = 'MN', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
MS <- MS %>%
    group_by(Week) %>%
    summarise(State = 'MS', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
MO <- MO %>%
    group_by(Week) %>%
    summarise(State = 'MO', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
MT <- MT %>%
    group_by(Week) %>%
    summarise(State = 'MT', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
NE <- NE %>%
    group_by(Week) %>%
    summarise(State = 'NE', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
NV <- NV %>%
    group_by(Week) %>%
    summarise(State = 'NV', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
NH <- NH %>%
    group_by(Week) %>%
    summarise(State = 'NH', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
NJ <- NJ %>%
    group_by(Week) %>%
    summarise(State = 'NJ', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
NM <- NM %>%
    group_by(Week) %>%
    summarise(State = 'NM', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
NY <- NY %>%
    group_by(Week) %>%
    summarise(State = 'NY', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
NC <- NC %>%
    group_by(Week) %>%
    summarise(State = 'NC', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
ND <- ND %>%
    group_by(Week) %>%
    summarise(State = 'ND', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
OH <- OH %>%
    group_by(Week) %>%
    summarise(State = 'OH', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
OK <- OK %>%
    group_by(Week) %>%
    summarise(State = 'OK', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
OR <- OR %>%
    group_by(Week) %>%
    summarise(State = 'OK', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
PA <- PA %>%
    group_by(Week) %>%
    summarise(State = 'PA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
RI <- RI %>%
    group_by(Week) %>%
    summarise(State = 'RI', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
SC <- SC %>%
    group_by(Week) %>%
    summarise(State = 'SC', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
SD <- SD %>%
    group_by(Week) %>%
    summarise(State = 'SD', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
TN <- TN %>%
    group_by(Week) %>%
    summarise(State = 'TN', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
TX <- TX %>%
    group_by(Week) %>%
    summarise(State = 'TX', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
UT <- UT %>%
    group_by(Week) %>%
    summarise(State = 'UT', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
VT <- VT %>%
    group_by(Week) %>%
    summarise(State = 'VT', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
VA <- VA %>%
    group_by(Week) %>%
    summarise(State = 'VA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
WA <- WA %>%
    group_by(Week) %>%
    summarise(State = 'WA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
WV <- WV %>%
    group_by(Week) %>%
    summarise(State = 'WV', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
WI <- WI %>%
    group_by(Week) %>%
    summarise(State = 'WI', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
WY <- WY %>%
    group_by(Week) %>%
    summarise(State = 'WY', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))




#### extract the corresponding resonse and variables for ANM bivariate causal discovery into 4 partitions each with 13 weeks ####
# New cases ~ # of contact tracing workers + pop density + % of male  + % of senior ppl + % of black + % of latino

AL.ANM.1 <- AL[1:13,c(3, 29,16, 17, 18,19,20,21)]; AL.ANM.2 <- AL[1:13+13,c(3, 29,16, 17, 18,19,20,21)];AL.ANM.3 <- AL[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];AL.ANM.4 <- AL[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
AK.ANM.1 <- AK[1:13,c(3, 29,16, 17, 18,19,20,21)]; AK.ANM.2 <- AK[1:13+13,c(3, 29,16, 17, 18,19,20,21)];AK.ANM.3 <- AK[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];AK.ANM.4 <- AK[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
AZ.ANM.1 <- AZ[1:13,c(3, 29,16, 17, 18,19,20,21)]; AZ.ANM.2 <- AZ[1:13+13,c(3, 29,16, 17, 18,19,20,21)];AZ.ANM.3 <- AZ[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];AZ.ANM.4 <- AZ[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
AR.ANM.1 <- AR[1:13,c(3, 29,16, 17, 18,19,20,21)]; AR.ANM.2 <- AR[1:13+13,c(3, 29,16, 17, 18,19,20,21)];AR.ANM.3 <- AR[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];AR.ANM.4 <- AR[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
CA.ANM.1 <- CA[1:13,c(3, 29,16, 17, 18,19,20,21)]; CA.ANM.2 <- CA[1:13+13,c(3, 29,16, 17, 18,19,20,21)];CA.ANM.3 <- CA[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];CA.ANM.4 <- CA[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
CO.ANM.1 <- CO[1:13,c(3, 29,16, 17, 18,19,20,21)]; CO.ANM.2 <- CO[1:13+13,c(3, 29,16, 17, 18,19,20,21)];CO.ANM.3 <- CO[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];CO.ANM.4 <- CO[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
CT.ANM.1 <- CT[1:13,c(3, 29,16, 17, 18,19,20,21)]; CT.ANM.2 <- CT[1:13+13,c(3, 29,16, 17, 18,19,20,21)];CT.ANM.3 <- CT[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];CT.ANM.4 <- CT[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
DE.ANM.1 <- DE[1:13,c(3, 29,16, 17, 18,19,20,21)]; DE.ANM.2 <- DE[1:13+13,c(3, 29,16, 17, 18,19,20,21)];DE.ANM.3 <- DE[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];DE.ANM.4 <- DE[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
FL.ANM.1 <- FL[1:13,c(3, 29,16, 17, 18,19,20,21)]; FL.ANM.2 <- FL[1:13+13,c(3, 29,16, 17, 18,19,20,21)];FL.ANM.3 <- FL[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];FL.ANM.4 <- FL[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
GA.ANM.1 <- GA[1:13,c(3, 29,16, 17, 18,19,20,21)]; GA.ANM.2 <- GA[1:13+13,c(3, 29,16, 17, 18,19,20,21)];GA.ANM.3 <- GA[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];GA.ANM.4 <- GA[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
HI.ANM.1 <- HI[1:13,c(3, 29,16, 17, 18,19,20,21)]; HI.ANM.2 <- HI[1:13+13,c(3, 29,16, 17, 18,19,20,21)];HI.ANM.3 <- HI[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];HI.ANM.4 <- HI[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
ID.ANM.1 <- ID[1:13,c(3, 29,16, 17, 18,19,20,21)]; ID.ANM.2 <- ID[1:13+13,c(3, 29,16, 17, 18,19,20,21)];ID.ANM.3 <- ID[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];ID.ANM.4 <- ID[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
IL.ANM.1 <- IL[1:13,c(3, 29,16, 17, 18,19,20,21)]; IL.ANM.2 <- IL[1:13+13,c(3, 29,16, 17, 18,19,20,21)];IL.ANM.3 <- IL[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];IL.ANM.4 <- IL[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
IN.ANM.1 <- IN[1:13,c(3, 29,16, 17, 18,19,20,21)]; IN.ANM.2 <- IN[1:13+13,c(3, 29,16, 17, 18,19,20,21)];IN.ANM.3 <- IN[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];IN.ANM.4 <- IN[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
IA.ANM.1 <- IA[1:13,c(3, 29,16, 17, 18,19,20,21)]; IA.ANM.2 <- IA[1:13+13,c(3, 29,16, 17, 18,19,20,21)];IA.ANM.3 <- IA[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];IA.ANM.4 <- IA[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
KS.ANM.1 <- KS[1:13,c(3, 29,16, 17, 18,19,20,21)]; KS.ANM.2 <- KS[1:13+13,c(3, 29,16, 17, 18,19,20,21)];KS.ANM.3 <- KS[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];KS.ANM.4 <- KS[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
KY.ANM.1 <- KY[1:13,c(3, 29,16, 17, 18,19,20,21)]; KY.ANM.2 <- KY[1:13+13,c(3, 29,16, 17, 18,19,20,21)];KY.ANM.3 <- KY[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];KY.ANM.4 <- KY[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
LA.ANM.1 <- LA[1:13,c(3, 29,16, 17, 18,19,20,21)]; LA.ANM.2 <- LA[1:13+13,c(3, 29,16, 17, 18,19,20,21)];LA.ANM.3 <- LA[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];LA.ANM.4 <- LA[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
ME.ANM.1 <- ME[1:13,c(3, 29,16, 17, 18,19,20,21)]; ME.ANM.2 <- ME[1:13+13,c(3, 29,16, 17, 18,19,20,21)];ME.ANM.3 <- ME[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];ME.ANM.4 <- ME[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
MD.ANM.1 <- MD[1:13,c(3, 29,16, 17, 18,19,20,21)]; MD.ANM.2 <- MD[1:13+13,c(3, 29,16, 17, 18,19,20,21)];MD.ANM.3 <- MD[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];MD.ANM.4 <- MD[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
MA.ANM.1 <- MA[1:13,c(3, 29,16, 17, 18,19,20,21)]; MA.ANM.2 <- MA[1:13+13,c(3, 29,16, 17, 18,19,20,21)];MA.ANM.3 <- MA[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];MA.ANM.4 <- MA[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
MI.ANM.1 <- MI[1:13,c(3, 29,16, 17, 18,19,20,21)]; MI.ANM.2 <- MI[1:13+13,c(3, 29,16, 17, 18,19,20,21)];MI.ANM.3 <- MI[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];MI.ANM.4 <- MI[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
MN.ANM.1 <- MN[1:13,c(3, 29,16, 17, 18,19,20,21)]; MN.ANM.2 <- MN[1:13+13,c(3, 29,16, 17, 18,19,20,21)];MN.ANM.3 <- MN[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];MN.ANM.4 <- MN[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
MS.ANM.1 <- MS[1:13,c(3, 29,16, 17, 18,19,20,21)]; MS.ANM.2 <- MS[1:13+13,c(3, 29,16, 17, 18,19,20,21)];MS.ANM.3 <- MS[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];MS.ANM.4 <- MS[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
MO.ANM.1 <- MO[1:13,c(3, 29,16, 17, 18,19,20,21)]; MO.ANM.2 <- MO[1:13+13,c(3, 29,16, 17, 18,19,20,21)];MO.ANM.3 <- MO[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];MO.ANM.4 <- MO[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
MT.ANM.1 <- MT[1:13,c(3, 29,16, 17, 18,19,20,21)]; MT.ANM.2 <- MT[1:13+13,c(3, 29,16, 17, 18,19,20,21)];MT.ANM.3 <- MT[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];MT.ANM.4 <- MT[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
NE.ANM.1 <- NE[1:13,c(3, 29,16, 17, 18,19,20,21)]; NE.ANM.2 <- NE[1:13+13,c(3, 29,16, 17, 18,19,20,21)];NE.ANM.3 <- NE[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];NE.ANM.4 <- NE[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
NV.ANM.1 <- NV[1:13,c(3, 29,16, 17, 18,19,20,21)]; NV.ANM.2 <- NV[1:13+13,c(3, 29,16, 17, 18,19,20,21)];NV.ANM.3 <- NV[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];NV.ANM.4 <- NV[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
NH.ANM.1 <- NH[1:13,c(3, 29,16, 17, 18,19,20,21)]; NH.ANM.2 <- NH[1:13+13,c(3, 29,16, 17, 18,19,20,21)];NH.ANM.3 <- NH[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];NH.ANM.4 <- NH[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
NJ.ANM.1 <- NJ[1:13,c(3, 29,16, 17, 18,19,20,21)]; NJ.ANM.2 <- NJ[1:13+13,c(3, 29,16, 17, 18,19,20,21)];NJ.ANM.3 <- NJ[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];NJ.ANM.4 <- NJ[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
NM.ANM.1 <- NM[1:13,c(3, 29,16, 17, 18,19,20,21)]; NM.ANM.2 <- NM[1:13+13,c(3, 29,16, 17, 18,19,20,21)];NM.ANM.3 <- NM[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];NM.ANM.4 <- NM[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
NY.ANM.1 <- NY[1:13,c(3, 29,16, 17, 18,19,20,21)]; NY.ANM.2 <- NY[1:13+13,c(3, 29,16, 17, 18,19,20,21)];NY.ANM.3 <- NY[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];NY.ANM.4 <- NY[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
NC.ANM.1 <- NC[1:13,c(3, 29,16, 17, 18,19,20,21)]; NC.ANM.2 <- NC[1:13+13,c(3, 29,16, 17, 18,19,20,21)];NC.ANM.3 <- NC[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];NC.ANM.4 <- NC[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
ND.ANM.1 <- ND[1:13,c(3, 29,16, 17, 18,19,20,21)]; ND.ANM.2 <- ND[1:13+13,c(3, 29,16, 17, 18,19,20,21)];ND.ANM.3 <- ND[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];ND.ANM.4 <- ND[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
OH.ANM.1 <- OH[1:13,c(3, 29,16, 17, 18,19,20,21)]; OH.ANM.2 <- OH[1:13+13,c(3, 29,16, 17, 18,19,20,21)];OH.ANM.3 <- OH[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];OH.ANM.4 <- OH[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
OK.ANM.1 <- OK[1:13,c(3, 29,16, 17, 18,19,20,21)]; OK.ANM.2 <- OK[1:13+13,c(3, 29,16, 17, 18,19,20,21)];OK.ANM.3 <- OK[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];OK.ANM.4 <- OK[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
OR.ANM.1 <- OR[1:13,c(3, 29,16, 17, 18,19,20,21)]; OR.ANM.2 <- OR[1:13+13,c(3, 29,16, 17, 18,19,20,21)];OR.ANM.3 <- OR[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];OR.ANM.4 <- OR[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
PA.ANM.1 <- PA[1:13,c(3, 29,16, 17, 18,19,20,21)]; PA.ANM.2 <- PA[1:13+13,c(3, 29,16, 17, 18,19,20,21)];PA.ANM.3 <- PA[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];PA.ANM.4 <- PA[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
RI.ANM.1 <- RI[1:13,c(3, 29,16, 17, 18,19,20,21)]; RI.ANM.2 <- RI[1:13+13,c(3, 29,16, 17, 18,19,20,21)];RI.ANM.3 <- RI[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];RI.ANM.4 <- RI[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
SC.ANM.1 <- SC[1:13,c(3, 29,16, 17, 18,19,20,21)]; SC.ANM.2 <- SC[1:13+13,c(3, 29,16, 17, 18,19,20,21)];SC.ANM.3 <- SC[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];SC.ANM.4 <- SC[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
SD.ANM.1 <- SD[1:13,c(3, 29,16, 17, 18,19,20,21)]; SD.ANM.2 <- SD[1:13+13,c(3, 29,16, 17, 18,19,20,21)];SD.ANM.3 <- SD[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];SD.ANM.4 <- SD[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
TN.ANM.1 <- TN[1:13,c(3, 29,16, 17, 18,19,20,21)]; TN.ANM.2 <- TN[1:13+13,c(3, 29,16, 17, 18,19,20,21)];TN.ANM.3 <- TN[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];TN.ANM.4 <- TN[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
TX.ANM.1 <- TX[1:13,c(3, 29,16, 17, 18,19,20,21)]; TX.ANM.2 <- TX[1:13+13,c(3, 29,16, 17, 18,19,20,21)];TX.ANM.3 <- TX[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];TX.ANM.4 <- TX[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
UT.ANM.1 <- UT[1:13,c(3, 29,16, 17, 18,19,20,21)]; UT.ANM.2 <- UT[1:13+13,c(3, 29,16, 17, 18,19,20,21)];UT.ANM.3 <- UT[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];UT.ANM.4 <- UT[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
VT.ANM.1 <- VT[1:13,c(3, 29,16, 17, 18,19,20,21)]; VT.ANM.2 <- VT[1:13+13,c(3, 29,16, 17, 18,19,20,21)];VT.ANM.3 <- VT[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];VT.ANM.4 <- VT[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
VA.ANM.1 <- VA[1:13,c(3, 29,16, 17, 18,19,20,21)]; VA.ANM.2 <- VA[1:13+13,c(3, 29,16, 17, 18,19,20,21)];VA.ANM.3 <- VA[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];VA.ANM.4 <- VA[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
WA.ANM.1 <- WA[1:13,c(3, 29,16, 17, 18,19,20,21)]; WA.ANM.2 <- WA[1:13+13,c(3, 29,16, 17, 18,19,20,21)];WA.ANM.3 <- WA[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];WA.ANM.4 <- WA[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
WV.ANM.1 <- WV[1:13,c(3, 29,16, 17, 18,19,20,21)]; WV.ANM.2 <- WV[1:13+13,c(3, 29,16, 17, 18,19,20,21)];WV.ANM.3 <- WV[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];WV.ANM.4 <- WV[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
WI.ANM.1 <- WI[1:13,c(3, 29,16, 17, 18,19,20,21)]; WI.ANM.2 <- WI[1:13+13,c(3, 29,16, 17, 18,19,20,21)];WI.ANM.3 <- WI[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];WI.ANM.4 <- WI[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];
WY.ANM.1 <- WY[1:13,c(3, 29,16, 17, 18,19,20,21)]; WY.ANM.2 <- WY[1:13+13,c(3, 29,16, 17, 18,19,20,21)];WY.ANM.3 <- WY[1:13+13*2,c(3, 29,16, 17, 18,19,20,21)];WY.ANM.4 <- WY[1:13+13*3,c(3, 29,16, 17, 18,19,20,21)];


AL.ANM.1 <- apply(AL.ANM.1, 2, mean); AL.ANM.2 <- apply(AL.ANM.2, 2, mean); AL.ANM.3 <- apply(AL.ANM.3, 2, mean); AL.ANM.4 <- apply(AL.ANM.4, 2, mean); 
AK.ANM.1 <- apply(AK.ANM.1, 2, mean); AK.ANM.2 <- apply(AK.ANM.2, 2, mean); AK.ANM.3 <- apply(AK.ANM.3, 2, mean); AK.ANM.4 <- apply(AK.ANM.4, 2, mean); 
AZ.ANM.1 <- apply(AZ.ANM.1, 2, mean); AZ.ANM.2 <- apply(AZ.ANM.2, 2, mean); AZ.ANM.3 <- apply(AZ.ANM.3, 2, mean); AZ.ANM.4 <- apply(AZ.ANM.4, 2, mean); 
AR.ANM.1 <- apply(AR.ANM.1, 2, mean); AR.ANM.2 <- apply(AR.ANM.2, 2, mean); AR.ANM.3 <- apply(AR.ANM.3, 2, mean); AR.ANM.4 <- apply(AR.ANM.4, 2, mean); 
CA.ANM.1 <- apply(CA.ANM.1, 2, mean); CA.ANM.2 <- apply(CA.ANM.2, 2, mean); CA.ANM.3 <- apply(CA.ANM.3, 2, mean); CA.ANM.4 <- apply(CA.ANM.4, 2, mean); 
CO.ANM.1 <- apply(CO.ANM.1, 2, mean); CO.ANM.2 <- apply(CO.ANM.2, 2, mean); CO.ANM.3 <- apply(CO.ANM.3, 2, mean); CO.ANM.4 <- apply(CO.ANM.4, 2, mean); 
CT.ANM.1 <- apply(CT.ANM.1, 2, mean); CT.ANM.2 <- apply(CT.ANM.2, 2, mean); CT.ANM.3 <- apply(CT.ANM.3, 2, mean); CT.ANM.4 <- apply(CT.ANM.4, 2, mean); 
DE.ANM.1 <- apply(DE.ANM.1, 2, mean); DE.ANM.2 <- apply(DE.ANM.2, 2, mean); DE.ANM.3 <- apply(DE.ANM.3, 2, mean); DE.ANM.4 <- apply(DE.ANM.4, 2, mean); 
FL.ANM.1 <- apply(FL.ANM.1, 2, mean); FL.ANM.2 <- apply(FL.ANM.2, 2, mean); FL.ANM.3 <- apply(FL.ANM.3, 2, mean); FL.ANM.4 <- apply(FL.ANM.4, 2, mean); 
GA.ANM.1 <- apply(GA.ANM.1, 2, mean); GA.ANM.2 <- apply(GA.ANM.2, 2, mean); GA.ANM.3 <- apply(GA.ANM.3, 2, mean); GA.ANM.4 <- apply(GA.ANM.4, 2, mean); 
HI.ANM.1 <- apply(HI.ANM.1, 2, mean); HI.ANM.2 <- apply(HI.ANM.2, 2, mean); HI.ANM.3 <- apply(HI.ANM.3, 2, mean); HI.ANM.4 <- apply(HI.ANM.4, 2, mean); 
ID.ANM.1 <- apply(ID.ANM.1, 2, mean); ID.ANM.2 <- apply(ID.ANM.2, 2, mean); ID.ANM.3 <- apply(ID.ANM.3, 2, mean); ID.ANM.4 <- apply(ID.ANM.4, 2, mean); 
IL.ANM.1 <- apply(IL.ANM.1, 2, mean); IL.ANM.2 <- apply(IL.ANM.2, 2, mean); IL.ANM.3 <- apply(IL.ANM.3, 2, mean); IL.ANM.4 <- apply(IL.ANM.4, 2, mean); 
IN.ANM.1 <- apply(IN.ANM.1, 2, mean); IN.ANM.2 <- apply(IN.ANM.2, 2, mean); IN.ANM.3 <- apply(IN.ANM.3, 2, mean); IN.ANM.4 <- apply(IN.ANM.4, 2, mean); 
IA.ANM.1 <- apply(IA.ANM.1, 2, mean); IA.ANM.2 <- apply(IA.ANM.2, 2, mean); IA.ANM.3 <- apply(IA.ANM.3, 2, mean); IA.ANM.4 <- apply(IA.ANM.4, 2, mean); 
KS.ANM.1 <- apply(KS.ANM.1, 2, mean); KS.ANM.2 <- apply(KS.ANM.2, 2, mean); KS.ANM.3 <- apply(KS.ANM.3, 2, mean); KS.ANM.4 <- apply(KS.ANM.4, 2, mean); 
KY.ANM.1 <- apply(KY.ANM.1, 2, mean); KY.ANM.2 <- apply(KY.ANM.2, 2, mean); KY.ANM.3 <- apply(KY.ANM.3, 2, mean); KY.ANM.4 <- apply(KY.ANM.4, 2, mean); 
LA.ANM.1 <- apply(LA.ANM.1, 2, mean); LA.ANM.2 <- apply(LA.ANM.2, 2, mean); LA.ANM.3 <- apply(LA.ANM.3, 2, mean); LA.ANM.4 <- apply(LA.ANM.4, 2, mean); 
ME.ANM.1 <- apply(ME.ANM.1, 2, mean); ME.ANM.2 <- apply(ME.ANM.2, 2, mean); ME.ANM.3 <- apply(ME.ANM.3, 2, mean); ME.ANM.4 <- apply(ME.ANM.4, 2, mean); 
MD.ANM.1 <- apply(MD.ANM.1, 2, mean); MD.ANM.2 <- apply(MD.ANM.2, 2, mean); MD.ANM.3 <- apply(MD.ANM.3, 2, mean); MD.ANM.4 <- apply(MD.ANM.4, 2, mean); 
MA.ANM.1 <- apply(MA.ANM.1, 2, mean); MA.ANM.2 <- apply(MA.ANM.2, 2, mean); MA.ANM.3 <- apply(MA.ANM.3, 2, mean); MA.ANM.4 <- apply(MA.ANM.4, 2, mean); 
MI.ANM.1 <- apply(MI.ANM.1, 2, mean); MI.ANM.2 <- apply(MI.ANM.2, 2, mean); MI.ANM.3 <- apply(MI.ANM.3, 2, mean); MI.ANM.4 <- apply(MI.ANM.4, 2, mean); 
MN.ANM.1 <- apply(MN.ANM.1, 2, mean); MN.ANM.2 <- apply(MN.ANM.2, 2, mean); MN.ANM.3 <- apply(MN.ANM.3, 2, mean); MN.ANM.4 <- apply(MN.ANM.4, 2, mean); 
MS.ANM.1 <- apply(MS.ANM.1, 2, mean); MS.ANM.2 <- apply(MS.ANM.2, 2, mean); MS.ANM.3 <- apply(MS.ANM.3, 2, mean); MS.ANM.4 <- apply(MS.ANM.4, 2, mean); 
MO.ANM.1 <- apply(MO.ANM.1, 2, mean); MO.ANM.2 <- apply(MO.ANM.2, 2, mean); MO.ANM.3 <- apply(MO.ANM.3, 2, mean); MO.ANM.4 <- apply(MO.ANM.4, 2, mean); 
MT.ANM.1 <- apply(MT.ANM.1, 2, mean); MT.ANM.2 <- apply(MT.ANM.2, 2, mean); MT.ANM.3 <- apply(MT.ANM.3, 2, mean); MT.ANM.4 <- apply(MT.ANM.4, 2, mean); 
NE.ANM.1 <- apply(NE.ANM.1, 2, mean); NE.ANM.2 <- apply(NE.ANM.2, 2, mean); NE.ANM.3 <- apply(NE.ANM.3, 2, mean); NE.ANM.4 <- apply(NE.ANM.4, 2, mean); 
NV.ANM.1 <- apply(NV.ANM.1, 2, mean); NV.ANM.2 <- apply(NV.ANM.2, 2, mean); NV.ANM.3 <- apply(NV.ANM.3, 2, mean); NV.ANM.4 <- apply(NV.ANM.4, 2, mean); 
NH.ANM.1 <- apply(NH.ANM.1, 2, mean); NH.ANM.2 <- apply(NH.ANM.2, 2, mean); NH.ANM.3 <- apply(NH.ANM.3, 2, mean); NH.ANM.4 <- apply(NH.ANM.4, 2, mean); 
NJ.ANM.1 <- apply(NJ.ANM.1, 2, mean); NJ.ANM.2 <- apply(NJ.ANM.2, 2, mean); NJ.ANM.3 <- apply(NJ.ANM.3, 2, mean); NJ.ANM.4 <- apply(NJ.ANM.4, 2, mean); 
NM.ANM.1 <- apply(NM.ANM.1, 2, mean); NM.ANM.2 <- apply(NM.ANM.2, 2, mean); NM.ANM.3 <- apply(NM.ANM.3, 2, mean); NM.ANM.4 <- apply(NM.ANM.4, 2, mean); 
NY.ANM.1 <- apply(NY.ANM.1, 2, mean); NY.ANM.2 <- apply(NY.ANM.2, 2, mean); NY.ANM.3 <- apply(NY.ANM.3, 2, mean); NY.ANM.4 <- apply(NY.ANM.4, 2, mean); 
NC.ANM.1 <- apply(NC.ANM.1, 2, mean); NC.ANM.2 <- apply(NC.ANM.2, 2, mean); NC.ANM.3 <- apply(NC.ANM.3, 2, mean); NC.ANM.4 <- apply(NC.ANM.4, 2, mean); 
ND.ANM.1 <- apply(ND.ANM.1, 2, mean); ND.ANM.2 <- apply(ND.ANM.2, 2, mean); ND.ANM.3 <- apply(ND.ANM.3, 2, mean); ND.ANM.4 <- apply(ND.ANM.4, 2, mean); 
OH.ANM.1 <- apply(OH.ANM.1, 2, mean); OH.ANM.2 <- apply(OH.ANM.2, 2, mean); OH.ANM.3 <- apply(OH.ANM.3, 2, mean); OH.ANM.4 <- apply(OH.ANM.4, 2, mean); 
OK.ANM.1 <- apply(OK.ANM.1, 2, mean); OK.ANM.2 <- apply(OK.ANM.2, 2, mean); OK.ANM.3 <- apply(OK.ANM.3, 2, mean); OK.ANM.4 <- apply(OK.ANM.4, 2, mean); 
OR.ANM.1 <- apply(OR.ANM.1, 2, mean); OR.ANM.2 <- apply(OR.ANM.2, 2, mean); OR.ANM.3 <- apply(OR.ANM.3, 2, mean); OR.ANM.4 <- apply(OR.ANM.4, 2, mean); 
PA.ANM.1 <- apply(PA.ANM.1, 2, mean); PA.ANM.2 <- apply(PA.ANM.2, 2, mean); PA.ANM.3 <- apply(PA.ANM.3, 2, mean); PA.ANM.4 <- apply(PA.ANM.4, 2, mean); 
RI.ANM.1 <- apply(RI.ANM.1, 2, mean); RI.ANM.2 <- apply(RI.ANM.2, 2, mean); RI.ANM.3 <- apply(RI.ANM.3, 2, mean); RI.ANM.4 <- apply(RI.ANM.4, 2, mean); 
SC.ANM.1 <- apply(SC.ANM.1, 2, mean); SC.ANM.2 <- apply(SC.ANM.2, 2, mean); SC.ANM.3 <- apply(SC.ANM.3, 2, mean); SC.ANM.4 <- apply(SC.ANM.4, 2, mean); 
SD.ANM.1 <- apply(SD.ANM.1, 2, mean); SD.ANM.2 <- apply(SD.ANM.2, 2, mean); SD.ANM.3 <- apply(SD.ANM.3, 2, mean); SD.ANM.4 <- apply(SD.ANM.4, 2, mean); 
TN.ANM.1 <- apply(TN.ANM.1, 2, mean); TN.ANM.2 <- apply(TN.ANM.2, 2, mean); TN.ANM.3 <- apply(TN.ANM.3, 2, mean); TN.ANM.4 <- apply(TN.ANM.4, 2, mean); 
TX.ANM.1 <- apply(TX.ANM.1, 2, mean); TX.ANM.2 <- apply(TX.ANM.2, 2, mean); TX.ANM.3 <- apply(TX.ANM.3, 2, mean); TX.ANM.4 <- apply(TX.ANM.4, 2, mean); 
UT.ANM.1 <- apply(UT.ANM.1, 2, mean); UT.ANM.2 <- apply(UT.ANM.2, 2, mean); UT.ANM.3 <- apply(UT.ANM.3, 2, mean); UT.ANM.4 <- apply(UT.ANM.4, 2, mean); 
VT.ANM.1 <- apply(VT.ANM.1, 2, mean); VT.ANM.2 <- apply(VT.ANM.2, 2, mean); VT.ANM.3 <- apply(VT.ANM.3, 2, mean); VT.ANM.4 <- apply(VT.ANM.4, 2, mean); 
VA.ANM.1 <- apply(VA.ANM.1, 2, mean); VA.ANM.2 <- apply(VA.ANM.2, 2, mean); VA.ANM.3 <- apply(VA.ANM.3, 2, mean); VA.ANM.4 <- apply(VA.ANM.4, 2, mean); 
WA.ANM.1 <- apply(WA.ANM.1, 2, mean); WA.ANM.2 <- apply(WA.ANM.2, 2, mean); WA.ANM.3 <- apply(WA.ANM.3, 2, mean); WA.ANM.4 <- apply(WA.ANM.4, 2, mean); 
WV.ANM.1 <- apply(WV.ANM.1, 2, mean); WV.ANM.2 <- apply(WV.ANM.2, 2, mean); WV.ANM.3 <- apply(WV.ANM.3, 2, mean); WV.ANM.4 <- apply(WV.ANM.4, 2, mean); 
WI.ANM.1 <- apply(WI.ANM.1, 2, mean); WI.ANM.2 <- apply(WI.ANM.2, 2, mean); WI.ANM.3 <- apply(WI.ANM.3, 2, mean); WI.ANM.4 <- apply(WI.ANM.4, 2, mean); 
WY.ANM.1 <- apply(WY.ANM.1, 2, mean); WY.ANM.2 <- apply(WY.ANM.2, 2, mean); WY.ANM.3 <- apply(WY.ANM.3, 2, mean); WY.ANM.4 <- apply(WY.ANM.4, 2, mean); 





###





#### Combine the states into 4 stages ####
ANM1 = rbind(
    AL.ANM.1, AK.ANM.1, AZ.ANM.1, AR.ANM.1, CA.ANM.1,
    CO.ANM.1, CT.ANM.1, DE.ANM.1, FL.ANM.1, GA.ANM.1,
    HI.ANM.1, ID.ANM.1, IL.ANM.1, IN.ANM.1, IA.ANM.1,
    KS.ANM.1, KY.ANM.1, LA.ANM.1, ME.ANM.1, MD.ANM.1, 
    MA.ANM.1, MI.ANM.1, MN.ANM.1, MS.ANM.1, MO.ANM.1, 
    MT.ANM.1, NE.ANM.1, NV.ANM.1, NH.ANM.1, NJ.ANM.1, 
    NM.ANM.1, NY.ANM.1, NC.ANM.1, ND.ANM.1, OH.ANM.1, 
    OK.ANM.1, OR.ANM.1, PA.ANM.1, RI.ANM.1, SC.ANM.1, 
    SD.ANM.1, TN.ANM.1, TX.ANM.1, UT.ANM.1, VT.ANM.1, 
    VA.ANM.1, WA.ANM.1, WV.ANM.1, WI.ANM.1, WY.ANM.1
)

ANM2 = rbind(
    AL.ANM.2, AK.ANM.2, AZ.ANM.2, AR.ANM.2, CA.ANM.2,
    CO.ANM.2, CT.ANM.2, DE.ANM.2, FL.ANM.2, GA.ANM.2,
    HI.ANM.2, ID.ANM.2, IL.ANM.2, IN.ANM.2, IA.ANM.2,
    KS.ANM.2, KY.ANM.2, LA.ANM.2, ME.ANM.2, MD.ANM.2, 
    MA.ANM.2, MI.ANM.2, MN.ANM.2, MS.ANM.2, MO.ANM.2, 
    MT.ANM.2, NE.ANM.2, NV.ANM.2, NH.ANM.2, NJ.ANM.2, 
    NM.ANM.2, NY.ANM.2, NC.ANM.2, ND.ANM.2, OH.ANM.2, 
    OK.ANM.2, OR.ANM.2, PA.ANM.2, RI.ANM.2, SC.ANM.2, 
    SD.ANM.2, TN.ANM.2, TX.ANM.2, UT.ANM.2, VT.ANM.2, 
    VA.ANM.2, WA.ANM.2, WV.ANM.2, WI.ANM.2, WY.ANM.2
)

ANM3 = rbind(
    AL.ANM.3, AK.ANM.3, AZ.ANM.3, AR.ANM.3, CA.ANM.3,
    CO.ANM.3, CT.ANM.3, DE.ANM.3, FL.ANM.3, GA.ANM.3,
    HI.ANM.3, ID.ANM.3, IL.ANM.3, IN.ANM.3, IA.ANM.3,
    KS.ANM.3, KY.ANM.3, LA.ANM.3, ME.ANM.3, MD.ANM.3, 
    MA.ANM.3, MI.ANM.3, MN.ANM.3, MS.ANM.3, MO.ANM.3, 
    MT.ANM.3, NE.ANM.3, NV.ANM.3, NH.ANM.3, NJ.ANM.3, 
    NM.ANM.3, NY.ANM.3, NC.ANM.3, ND.ANM.3, OH.ANM.3, 
    OK.ANM.3, OR.ANM.3, PA.ANM.3, RI.ANM.3, SC.ANM.3, 
    SD.ANM.3, TN.ANM.3, TX.ANM.3, UT.ANM.3, VT.ANM.3, 
    VA.ANM.3, WA.ANM.3, WV.ANM.3, WI.ANM.3, WY.ANM.3
)

ANM4 = rbind(
    AL.ANM.4, AK.ANM.4, AZ.ANM.4, AR.ANM.4, CA.ANM.4,
    CO.ANM.4, CT.ANM.4, DE.ANM.4, FL.ANM.4, GA.ANM.4,
    HI.ANM.4, ID.ANM.4, IL.ANM.4, IN.ANM.4, IA.ANM.4,
    KS.ANM.4, KY.ANM.4, LA.ANM.4, ME.ANM.4, MD.ANM.4, 
    MA.ANM.4, MI.ANM.4, MN.ANM.4, MS.ANM.4, MO.ANM.4, 
    MT.ANM.4, NE.ANM.4, NV.ANM.4, NH.ANM.4, NJ.ANM.4, 
    NM.ANM.4, NY.ANM.4, NC.ANM.4, ND.ANM.4, OH.ANM.4, 
    OK.ANM.4, OR.ANM.4, PA.ANM.4, RI.ANM.4, SC.ANM.4, 
    SD.ANM.4, TN.ANM.4, TX.ANM.4, UT.ANM.4, VT.ANM.4, 
    VA.ANM.4, WA.ANM.4, WV.ANM.4, WI.ANM.4, WY.ANM.4
)

} else if (response =='New.case') {
    #### conver to weekly resolution ####
    AL <- AL %>%
        group_by(Week) %>%
        summarise(State = 'AL', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    AK <- AK %>% 
        group_by(Week) %>%
        summarise(State = 'AK', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    AZ <- AZ %>%
        group_by(Week) %>%
        summarise(State = 'AZ', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    AR <- AR %>%
        group_by(Week) %>%
        summarise(State = 'AR', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    CA <- CA %>%
        group_by(Week) %>%
        summarise(State = 'CA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    CO <- CO %>%
        group_by(Week) %>%
        summarise(State = 'CO', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    CT <- CT %>%
        group_by(Week) %>%
        summarise(State = 'CT', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    DE <- DE %>%
        group_by(Week) %>%
        summarise(State = 'DE', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    FL <- FL %>%
        group_by(Week) %>%
        summarise(State = 'FL', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    GA <- GA %>%
        group_by(Week) %>%
        summarise(State = 'GA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    HI <- HI %>%
        group_by(Week) %>%
        summarise(State = 'HI', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    ID <- ID %>%
        group_by(Week) %>%
        summarise(State = 'ID', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    IL <- IL %>%
        group_by(Week) %>%
        summarise(State = 'IL', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    IN <- IN %>%
        group_by(Week) %>%
        summarise(State = 'IN', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    IA <- IA %>%
        group_by(Week) %>%
        summarise(State = 'IA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    KS <- KS %>%
        group_by(Week) %>%
        summarise(State = 'KS', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    KY <- KY %>%
        group_by(Week) %>%
        summarise(State = 'KY', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    LA <- LA %>%
        group_by(Week) %>%
        summarise(State = 'LA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    ME <- ME %>%
        group_by(Week) %>%
        summarise(State = 'ME', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    MD <- MD %>%
        group_by(Week) %>%
        summarise(State = 'MD', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    MA <- MA %>%
        group_by(Week) %>%
        summarise(State = 'MA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    MI <- MI %>%
        group_by(Week) %>%
        summarise(State = 'MI', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    MN <- MN %>%
        group_by(Week) %>%
        summarise(State = 'MN', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    MS <- MS %>%
        group_by(Week) %>%
        summarise(State = 'MS', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    MO <- MO %>%
        group_by(Week) %>%
        summarise(State = 'MO', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    MT <- MT %>%
        group_by(Week) %>%
        summarise(State = 'MT', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    NE <- NE %>%
        group_by(Week) %>%
        summarise(State = 'NE', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    NV <- NV %>%
        group_by(Week) %>%
        summarise(State = 'NV', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    NH <- NH %>%
        group_by(Week) %>%
        summarise(State = 'NH', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    NJ <- NJ %>%
        group_by(Week) %>%
        summarise(State = 'NJ', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    NM <- NM %>%
        group_by(Week) %>%
        summarise(State = 'NM', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    NY <- NY %>%
        group_by(Week) %>%
        summarise(State = 'NY', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    NC <- NC %>%
        group_by(Week) %>%
        summarise(State = 'NC', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    ND <- ND %>%
        group_by(Week) %>%
        summarise(State = 'ND', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    OH <- OH %>%
        group_by(Week) %>%
        summarise(State = 'OH', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    OK <- OK %>%
        group_by(Week) %>%
        summarise(State = 'OK', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    OR <- OR %>%
        group_by(Week) %>%
        summarise(State = 'OK', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    PA <- PA %>%
        group_by(Week) %>%
        summarise(State = 'PA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    RI <- RI %>%
        group_by(Week) %>%
        summarise(State = 'RI', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    SC <- SC %>%
        group_by(Week) %>%
        summarise(State = 'SC', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    SD <- SD %>%
        group_by(Week) %>%
        summarise(State = 'SD', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    TN <- TN %>%
        group_by(Week) %>%
        summarise(State = 'TN', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    TX <- TX %>%
        group_by(Week) %>%
        summarise(State = 'TX', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    UT <- UT %>%
        group_by(Week) %>%
        summarise(State = 'UT', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    VT <- VT %>%
        group_by(Week) %>%
        summarise(State = 'VT', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    VA <- VA %>%
        group_by(Week) %>%
        summarise(State = 'VA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    WA <- WA %>%
        group_by(Week) %>%
        summarise(State = 'WA', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    WV <- WV %>%
        group_by(Week) %>%
        summarise(State = 'WV', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    WI <- WI %>%
        group_by(Week) %>%
        summarise(State = 'WI', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    WY <- WY %>%
        group_by(Week) %>%
        summarise(State = 'WY', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
    
    
    
    
    #### extract the corresponding resonse and variables for ANM bivariate causal discovery into 4 partitions each with 13 weeks ####
    # New cases ~ # of contact tracing workers + pop density + % of male  + % of senior ppl + % of black + % of latino
    
    AL.ANM.1 <- AL[1:13,c(3, 12,16, 17, 18,19,20,21)]; AL.ANM.2 <- AL[1:13+13,c(3, 12,16, 17, 18,19,20,21)];AL.ANM.3 <- AL[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];AL.ANM.4 <- AL[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    AK.ANM.1 <- AK[1:13,c(3, 12,16, 17, 18,19,20,21)]; AK.ANM.2 <- AK[1:13+13,c(3, 12,16, 17, 18,19,20,21)];AK.ANM.3 <- AK[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];AK.ANM.4 <- AK[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    AZ.ANM.1 <- AZ[1:13,c(3, 12,16, 17, 18,19,20,21)]; AZ.ANM.2 <- AZ[1:13+13,c(3, 12,16, 17, 18,19,20,21)];AZ.ANM.3 <- AZ[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];AZ.ANM.4 <- AZ[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    AR.ANM.1 <- AR[1:13,c(3, 12,16, 17, 18,19,20,21)]; AR.ANM.2 <- AR[1:13+13,c(3, 12,16, 17, 18,19,20,21)];AR.ANM.3 <- AR[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];AR.ANM.4 <- AR[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    CA.ANM.1 <- CA[1:13,c(3, 12,16, 17, 18,19,20,21)]; CA.ANM.2 <- CA[1:13+13,c(3, 12,16, 17, 18,19,20,21)];CA.ANM.3 <- CA[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];CA.ANM.4 <- CA[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    CO.ANM.1 <- CO[1:13,c(3, 12,16, 17, 18,19,20,21)]; CO.ANM.2 <- CO[1:13+13,c(3, 12,16, 17, 18,19,20,21)];CO.ANM.3 <- CO[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];CO.ANM.4 <- CO[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    CT.ANM.1 <- CT[1:13,c(3, 12,16, 17, 18,19,20,21)]; CT.ANM.2 <- CT[1:13+13,c(3, 12,16, 17, 18,19,20,21)];CT.ANM.3 <- CT[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];CT.ANM.4 <- CT[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    DE.ANM.1 <- DE[1:13,c(3, 12,16, 17, 18,19,20,21)]; DE.ANM.2 <- DE[1:13+13,c(3, 12,16, 17, 18,19,20,21)];DE.ANM.3 <- DE[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];DE.ANM.4 <- DE[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    FL.ANM.1 <- FL[1:13,c(3, 12,16, 17, 18,19,20,21)]; FL.ANM.2 <- FL[1:13+13,c(3, 12,16, 17, 18,19,20,21)];FL.ANM.3 <- FL[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];FL.ANM.4 <- FL[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    GA.ANM.1 <- GA[1:13,c(3, 12,16, 17, 18,19,20,21)]; GA.ANM.2 <- GA[1:13+13,c(3, 12,16, 17, 18,19,20,21)];GA.ANM.3 <- GA[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];GA.ANM.4 <- GA[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    HI.ANM.1 <- HI[1:13,c(3, 12,16, 17, 18,19,20,21)]; HI.ANM.2 <- HI[1:13+13,c(3, 12,16, 17, 18,19,20,21)];HI.ANM.3 <- HI[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];HI.ANM.4 <- HI[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    ID.ANM.1 <- ID[1:13,c(3, 12,16, 17, 18,19,20,21)]; ID.ANM.2 <- ID[1:13+13,c(3, 12,16, 17, 18,19,20,21)];ID.ANM.3 <- ID[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];ID.ANM.4 <- ID[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    IL.ANM.1 <- IL[1:13,c(3, 12,16, 17, 18,19,20,21)]; IL.ANM.2 <- IL[1:13+13,c(3, 12,16, 17, 18,19,20,21)];IL.ANM.3 <- IL[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];IL.ANM.4 <- IL[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    IN.ANM.1 <- IN[1:13,c(3, 12,16, 17, 18,19,20,21)]; IN.ANM.2 <- IN[1:13+13,c(3, 12,16, 17, 18,19,20,21)];IN.ANM.3 <- IN[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];IN.ANM.4 <- IN[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    IA.ANM.1 <- IA[1:13,c(3, 12,16, 17, 18,19,20,21)]; IA.ANM.2 <- IA[1:13+13,c(3, 12,16, 17, 18,19,20,21)];IA.ANM.3 <- IA[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];IA.ANM.4 <- IA[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    KS.ANM.1 <- KS[1:13,c(3, 12,16, 17, 18,19,20,21)]; KS.ANM.2 <- KS[1:13+13,c(3, 12,16, 17, 18,19,20,21)];KS.ANM.3 <- KS[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];KS.ANM.4 <- KS[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    KY.ANM.1 <- KY[1:13,c(3, 12,16, 17, 18,19,20,21)]; KY.ANM.2 <- KY[1:13+13,c(3, 12,16, 17, 18,19,20,21)];KY.ANM.3 <- KY[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];KY.ANM.4 <- KY[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    LA.ANM.1 <- LA[1:13,c(3, 12,16, 17, 18,19,20,21)]; LA.ANM.2 <- LA[1:13+13,c(3, 12,16, 17, 18,19,20,21)];LA.ANM.3 <- LA[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];LA.ANM.4 <- LA[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    ME.ANM.1 <- ME[1:13,c(3, 12,16, 17, 18,19,20,21)]; ME.ANM.2 <- ME[1:13+13,c(3, 12,16, 17, 18,19,20,21)];ME.ANM.3 <- ME[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];ME.ANM.4 <- ME[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    MD.ANM.1 <- MD[1:13,c(3, 12,16, 17, 18,19,20,21)]; MD.ANM.2 <- MD[1:13+13,c(3, 12,16, 17, 18,19,20,21)];MD.ANM.3 <- MD[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];MD.ANM.4 <- MD[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    MA.ANM.1 <- MA[1:13,c(3, 12,16, 17, 18,19,20,21)]; MA.ANM.2 <- MA[1:13+13,c(3, 12,16, 17, 18,19,20,21)];MA.ANM.3 <- MA[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];MA.ANM.4 <- MA[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    MI.ANM.1 <- MI[1:13,c(3, 12,16, 17, 18,19,20,21)]; MI.ANM.2 <- MI[1:13+13,c(3, 12,16, 17, 18,19,20,21)];MI.ANM.3 <- MI[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];MI.ANM.4 <- MI[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    MN.ANM.1 <- MN[1:13,c(3, 12,16, 17, 18,19,20,21)]; MN.ANM.2 <- MN[1:13+13,c(3, 12,16, 17, 18,19,20,21)];MN.ANM.3 <- MN[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];MN.ANM.4 <- MN[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    MS.ANM.1 <- MS[1:13,c(3, 12,16, 17, 18,19,20,21)]; MS.ANM.2 <- MS[1:13+13,c(3, 12,16, 17, 18,19,20,21)];MS.ANM.3 <- MS[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];MS.ANM.4 <- MS[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    MO.ANM.1 <- MO[1:13,c(3, 12,16, 17, 18,19,20,21)]; MO.ANM.2 <- MO[1:13+13,c(3, 12,16, 17, 18,19,20,21)];MO.ANM.3 <- MO[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];MO.ANM.4 <- MO[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    MT.ANM.1 <- MT[1:13,c(3, 12,16, 17, 18,19,20,21)]; MT.ANM.2 <- MT[1:13+13,c(3, 12,16, 17, 18,19,20,21)];MT.ANM.3 <- MT[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];MT.ANM.4 <- MT[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    NE.ANM.1 <- NE[1:13,c(3, 12,16, 17, 18,19,20,21)]; NE.ANM.2 <- NE[1:13+13,c(3, 12,16, 17, 18,19,20,21)];NE.ANM.3 <- NE[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];NE.ANM.4 <- NE[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    NV.ANM.1 <- NV[1:13,c(3, 12,16, 17, 18,19,20,21)]; NV.ANM.2 <- NV[1:13+13,c(3, 12,16, 17, 18,19,20,21)];NV.ANM.3 <- NV[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];NV.ANM.4 <- NV[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    NH.ANM.1 <- NH[1:13,c(3, 12,16, 17, 18,19,20,21)]; NH.ANM.2 <- NH[1:13+13,c(3, 12,16, 17, 18,19,20,21)];NH.ANM.3 <- NH[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];NH.ANM.4 <- NH[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    NJ.ANM.1 <- NJ[1:13,c(3, 12,16, 17, 18,19,20,21)]; NJ.ANM.2 <- NJ[1:13+13,c(3, 12,16, 17, 18,19,20,21)];NJ.ANM.3 <- NJ[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];NJ.ANM.4 <- NJ[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    NM.ANM.1 <- NM[1:13,c(3, 12,16, 17, 18,19,20,21)]; NM.ANM.2 <- NM[1:13+13,c(3, 12,16, 17, 18,19,20,21)];NM.ANM.3 <- NM[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];NM.ANM.4 <- NM[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    NY.ANM.1 <- NY[1:13,c(3, 12,16, 17, 18,19,20,21)]; NY.ANM.2 <- NY[1:13+13,c(3, 12,16, 17, 18,19,20,21)];NY.ANM.3 <- NY[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];NY.ANM.4 <- NY[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    NC.ANM.1 <- NC[1:13,c(3, 12,16, 17, 18,19,20,21)]; NC.ANM.2 <- NC[1:13+13,c(3, 12,16, 17, 18,19,20,21)];NC.ANM.3 <- NC[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];NC.ANM.4 <- NC[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    ND.ANM.1 <- ND[1:13,c(3, 12,16, 17, 18,19,20,21)]; ND.ANM.2 <- ND[1:13+13,c(3, 12,16, 17, 18,19,20,21)];ND.ANM.3 <- ND[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];ND.ANM.4 <- ND[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    OH.ANM.1 <- OH[1:13,c(3, 12,16, 17, 18,19,20,21)]; OH.ANM.2 <- OH[1:13+13,c(3, 12,16, 17, 18,19,20,21)];OH.ANM.3 <- OH[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];OH.ANM.4 <- OH[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    OK.ANM.1 <- OK[1:13,c(3, 12,16, 17, 18,19,20,21)]; OK.ANM.2 <- OK[1:13+13,c(3, 12,16, 17, 18,19,20,21)];OK.ANM.3 <- OK[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];OK.ANM.4 <- OK[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    OR.ANM.1 <- OR[1:13,c(3, 12,16, 17, 18,19,20,21)]; OR.ANM.2 <- OR[1:13+13,c(3, 12,16, 17, 18,19,20,21)];OR.ANM.3 <- OR[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];OR.ANM.4 <- OR[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    PA.ANM.1 <- PA[1:13,c(3, 12,16, 17, 18,19,20,21)]; PA.ANM.2 <- PA[1:13+13,c(3, 12,16, 17, 18,19,20,21)];PA.ANM.3 <- PA[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];PA.ANM.4 <- PA[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    RI.ANM.1 <- RI[1:13,c(3, 12,16, 17, 18,19,20,21)]; RI.ANM.2 <- RI[1:13+13,c(3, 12,16, 17, 18,19,20,21)];RI.ANM.3 <- RI[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];RI.ANM.4 <- RI[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    SC.ANM.1 <- SC[1:13,c(3, 12,16, 17, 18,19,20,21)]; SC.ANM.2 <- SC[1:13+13,c(3, 12,16, 17, 18,19,20,21)];SC.ANM.3 <- SC[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];SC.ANM.4 <- SC[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    SD.ANM.1 <- SD[1:13,c(3, 12,16, 17, 18,19,20,21)]; SD.ANM.2 <- SD[1:13+13,c(3, 12,16, 17, 18,19,20,21)];SD.ANM.3 <- SD[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];SD.ANM.4 <- SD[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    TN.ANM.1 <- TN[1:13,c(3, 12,16, 17, 18,19,20,21)]; TN.ANM.2 <- TN[1:13+13,c(3, 12,16, 17, 18,19,20,21)];TN.ANM.3 <- TN[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];TN.ANM.4 <- TN[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    TX.ANM.1 <- TX[1:13,c(3, 12,16, 17, 18,19,20,21)]; TX.ANM.2 <- TX[1:13+13,c(3, 12,16, 17, 18,19,20,21)];TX.ANM.3 <- TX[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];TX.ANM.4 <- TX[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    UT.ANM.1 <- UT[1:13,c(3, 12,16, 17, 18,19,20,21)]; UT.ANM.2 <- UT[1:13+13,c(3, 12,16, 17, 18,19,20,21)];UT.ANM.3 <- UT[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];UT.ANM.4 <- UT[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    VT.ANM.1 <- VT[1:13,c(3, 12,16, 17, 18,19,20,21)]; VT.ANM.2 <- VT[1:13+13,c(3, 12,16, 17, 18,19,20,21)];VT.ANM.3 <- VT[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];VT.ANM.4 <- VT[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    VA.ANM.1 <- VA[1:13,c(3, 12,16, 17, 18,19,20,21)]; VA.ANM.2 <- VA[1:13+13,c(3, 12,16, 17, 18,19,20,21)];VA.ANM.3 <- VA[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];VA.ANM.4 <- VA[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    WA.ANM.1 <- WA[1:13,c(3, 12,16, 17, 18,19,20,21)]; WA.ANM.2 <- WA[1:13+13,c(3, 12,16, 17, 18,19,20,21)];WA.ANM.3 <- WA[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];WA.ANM.4 <- WA[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    WV.ANM.1 <- WV[1:13,c(3, 12,16, 17, 18,19,20,21)]; WV.ANM.2 <- WV[1:13+13,c(3, 12,16, 17, 18,19,20,21)];WV.ANM.3 <- WV[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];WV.ANM.4 <- WV[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    WI.ANM.1 <- WI[1:13,c(3, 12,16, 17, 18,19,20,21)]; WI.ANM.2 <- WI[1:13+13,c(3, 12,16, 17, 18,19,20,21)];WI.ANM.3 <- WI[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];WI.ANM.4 <- WI[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    WY.ANM.1 <- WY[1:13,c(3, 12,16, 17, 18,19,20,21)]; WY.ANM.2 <- WY[1:13+13,c(3, 12,16, 17, 18,19,20,21)];WY.ANM.3 <- WY[1:13+13*2,c(3, 12,16, 17, 18,19,20,21)];WY.ANM.4 <- WY[1:13+13*3,c(3, 12,16, 17, 18,19,20,21)];
    
    
    AL.ANM.1 <- apply(AL.ANM.1, 2, mean); AL.ANM.2 <- apply(AL.ANM.2, 2, mean); AL.ANM.3 <- apply(AL.ANM.3, 2, mean); AL.ANM.4 <- apply(AL.ANM.4, 2, mean); 
    AK.ANM.1 <- apply(AK.ANM.1, 2, mean); AK.ANM.2 <- apply(AK.ANM.2, 2, mean); AK.ANM.3 <- apply(AK.ANM.3, 2, mean); AK.ANM.4 <- apply(AK.ANM.4, 2, mean); 
    AZ.ANM.1 <- apply(AZ.ANM.1, 2, mean); AZ.ANM.2 <- apply(AZ.ANM.2, 2, mean); AZ.ANM.3 <- apply(AZ.ANM.3, 2, mean); AZ.ANM.4 <- apply(AZ.ANM.4, 2, mean); 
    AR.ANM.1 <- apply(AR.ANM.1, 2, mean); AR.ANM.2 <- apply(AR.ANM.2, 2, mean); AR.ANM.3 <- apply(AR.ANM.3, 2, mean); AR.ANM.4 <- apply(AR.ANM.4, 2, mean); 
    CA.ANM.1 <- apply(CA.ANM.1, 2, mean); CA.ANM.2 <- apply(CA.ANM.2, 2, mean); CA.ANM.3 <- apply(CA.ANM.3, 2, mean); CA.ANM.4 <- apply(CA.ANM.4, 2, mean); 
    CO.ANM.1 <- apply(CO.ANM.1, 2, mean); CO.ANM.2 <- apply(CO.ANM.2, 2, mean); CO.ANM.3 <- apply(CO.ANM.3, 2, mean); CO.ANM.4 <- apply(CO.ANM.4, 2, mean); 
    CT.ANM.1 <- apply(CT.ANM.1, 2, mean); CT.ANM.2 <- apply(CT.ANM.2, 2, mean); CT.ANM.3 <- apply(CT.ANM.3, 2, mean); CT.ANM.4 <- apply(CT.ANM.4, 2, mean); 
    DE.ANM.1 <- apply(DE.ANM.1, 2, mean); DE.ANM.2 <- apply(DE.ANM.2, 2, mean); DE.ANM.3 <- apply(DE.ANM.3, 2, mean); DE.ANM.4 <- apply(DE.ANM.4, 2, mean); 
    FL.ANM.1 <- apply(FL.ANM.1, 2, mean); FL.ANM.2 <- apply(FL.ANM.2, 2, mean); FL.ANM.3 <- apply(FL.ANM.3, 2, mean); FL.ANM.4 <- apply(FL.ANM.4, 2, mean); 
    GA.ANM.1 <- apply(GA.ANM.1, 2, mean); GA.ANM.2 <- apply(GA.ANM.2, 2, mean); GA.ANM.3 <- apply(GA.ANM.3, 2, mean); GA.ANM.4 <- apply(GA.ANM.4, 2, mean); 
    HI.ANM.1 <- apply(HI.ANM.1, 2, mean); HI.ANM.2 <- apply(HI.ANM.2, 2, mean); HI.ANM.3 <- apply(HI.ANM.3, 2, mean); HI.ANM.4 <- apply(HI.ANM.4, 2, mean); 
    ID.ANM.1 <- apply(ID.ANM.1, 2, mean); ID.ANM.2 <- apply(ID.ANM.2, 2, mean); ID.ANM.3 <- apply(ID.ANM.3, 2, mean); ID.ANM.4 <- apply(ID.ANM.4, 2, mean); 
    IL.ANM.1 <- apply(IL.ANM.1, 2, mean); IL.ANM.2 <- apply(IL.ANM.2, 2, mean); IL.ANM.3 <- apply(IL.ANM.3, 2, mean); IL.ANM.4 <- apply(IL.ANM.4, 2, mean); 
    IN.ANM.1 <- apply(IN.ANM.1, 2, mean); IN.ANM.2 <- apply(IN.ANM.2, 2, mean); IN.ANM.3 <- apply(IN.ANM.3, 2, mean); IN.ANM.4 <- apply(IN.ANM.4, 2, mean); 
    IA.ANM.1 <- apply(IA.ANM.1, 2, mean); IA.ANM.2 <- apply(IA.ANM.2, 2, mean); IA.ANM.3 <- apply(IA.ANM.3, 2, mean); IA.ANM.4 <- apply(IA.ANM.4, 2, mean); 
    KS.ANM.1 <- apply(KS.ANM.1, 2, mean); KS.ANM.2 <- apply(KS.ANM.2, 2, mean); KS.ANM.3 <- apply(KS.ANM.3, 2, mean); KS.ANM.4 <- apply(KS.ANM.4, 2, mean); 
    KY.ANM.1 <- apply(KY.ANM.1, 2, mean); KY.ANM.2 <- apply(KY.ANM.2, 2, mean); KY.ANM.3 <- apply(KY.ANM.3, 2, mean); KY.ANM.4 <- apply(KY.ANM.4, 2, mean); 
    LA.ANM.1 <- apply(LA.ANM.1, 2, mean); LA.ANM.2 <- apply(LA.ANM.2, 2, mean); LA.ANM.3 <- apply(LA.ANM.3, 2, mean); LA.ANM.4 <- apply(LA.ANM.4, 2, mean); 
    ME.ANM.1 <- apply(ME.ANM.1, 2, mean); ME.ANM.2 <- apply(ME.ANM.2, 2, mean); ME.ANM.3 <- apply(ME.ANM.3, 2, mean); ME.ANM.4 <- apply(ME.ANM.4, 2, mean); 
    MD.ANM.1 <- apply(MD.ANM.1, 2, mean); MD.ANM.2 <- apply(MD.ANM.2, 2, mean); MD.ANM.3 <- apply(MD.ANM.3, 2, mean); MD.ANM.4 <- apply(MD.ANM.4, 2, mean); 
    MA.ANM.1 <- apply(MA.ANM.1, 2, mean); MA.ANM.2 <- apply(MA.ANM.2, 2, mean); MA.ANM.3 <- apply(MA.ANM.3, 2, mean); MA.ANM.4 <- apply(MA.ANM.4, 2, mean); 
    MI.ANM.1 <- apply(MI.ANM.1, 2, mean); MI.ANM.2 <- apply(MI.ANM.2, 2, mean); MI.ANM.3 <- apply(MI.ANM.3, 2, mean); MI.ANM.4 <- apply(MI.ANM.4, 2, mean); 
    MN.ANM.1 <- apply(MN.ANM.1, 2, mean); MN.ANM.2 <- apply(MN.ANM.2, 2, mean); MN.ANM.3 <- apply(MN.ANM.3, 2, mean); MN.ANM.4 <- apply(MN.ANM.4, 2, mean); 
    MS.ANM.1 <- apply(MS.ANM.1, 2, mean); MS.ANM.2 <- apply(MS.ANM.2, 2, mean); MS.ANM.3 <- apply(MS.ANM.3, 2, mean); MS.ANM.4 <- apply(MS.ANM.4, 2, mean); 
    MO.ANM.1 <- apply(MO.ANM.1, 2, mean); MO.ANM.2 <- apply(MO.ANM.2, 2, mean); MO.ANM.3 <- apply(MO.ANM.3, 2, mean); MO.ANM.4 <- apply(MO.ANM.4, 2, mean); 
    MT.ANM.1 <- apply(MT.ANM.1, 2, mean); MT.ANM.2 <- apply(MT.ANM.2, 2, mean); MT.ANM.3 <- apply(MT.ANM.3, 2, mean); MT.ANM.4 <- apply(MT.ANM.4, 2, mean); 
    NE.ANM.1 <- apply(NE.ANM.1, 2, mean); NE.ANM.2 <- apply(NE.ANM.2, 2, mean); NE.ANM.3 <- apply(NE.ANM.3, 2, mean); NE.ANM.4 <- apply(NE.ANM.4, 2, mean); 
    NV.ANM.1 <- apply(NV.ANM.1, 2, mean); NV.ANM.2 <- apply(NV.ANM.2, 2, mean); NV.ANM.3 <- apply(NV.ANM.3, 2, mean); NV.ANM.4 <- apply(NV.ANM.4, 2, mean); 
    NH.ANM.1 <- apply(NH.ANM.1, 2, mean); NH.ANM.2 <- apply(NH.ANM.2, 2, mean); NH.ANM.3 <- apply(NH.ANM.3, 2, mean); NH.ANM.4 <- apply(NH.ANM.4, 2, mean); 
    NJ.ANM.1 <- apply(NJ.ANM.1, 2, mean); NJ.ANM.2 <- apply(NJ.ANM.2, 2, mean); NJ.ANM.3 <- apply(NJ.ANM.3, 2, mean); NJ.ANM.4 <- apply(NJ.ANM.4, 2, mean); 
    NM.ANM.1 <- apply(NM.ANM.1, 2, mean); NM.ANM.2 <- apply(NM.ANM.2, 2, mean); NM.ANM.3 <- apply(NM.ANM.3, 2, mean); NM.ANM.4 <- apply(NM.ANM.4, 2, mean); 
    NY.ANM.1 <- apply(NY.ANM.1, 2, mean); NY.ANM.2 <- apply(NY.ANM.2, 2, mean); NY.ANM.3 <- apply(NY.ANM.3, 2, mean); NY.ANM.4 <- apply(NY.ANM.4, 2, mean); 
    NC.ANM.1 <- apply(NC.ANM.1, 2, mean); NC.ANM.2 <- apply(NC.ANM.2, 2, mean); NC.ANM.3 <- apply(NC.ANM.3, 2, mean); NC.ANM.4 <- apply(NC.ANM.4, 2, mean); 
    ND.ANM.1 <- apply(ND.ANM.1, 2, mean); ND.ANM.2 <- apply(ND.ANM.2, 2, mean); ND.ANM.3 <- apply(ND.ANM.3, 2, mean); ND.ANM.4 <- apply(ND.ANM.4, 2, mean); 
    OH.ANM.1 <- apply(OH.ANM.1, 2, mean); OH.ANM.2 <- apply(OH.ANM.2, 2, mean); OH.ANM.3 <- apply(OH.ANM.3, 2, mean); OH.ANM.4 <- apply(OH.ANM.4, 2, mean); 
    OK.ANM.1 <- apply(OK.ANM.1, 2, mean); OK.ANM.2 <- apply(OK.ANM.2, 2, mean); OK.ANM.3 <- apply(OK.ANM.3, 2, mean); OK.ANM.4 <- apply(OK.ANM.4, 2, mean); 
    OR.ANM.1 <- apply(OR.ANM.1, 2, mean); OR.ANM.2 <- apply(OR.ANM.2, 2, mean); OR.ANM.3 <- apply(OR.ANM.3, 2, mean); OR.ANM.4 <- apply(OR.ANM.4, 2, mean); 
    PA.ANM.1 <- apply(PA.ANM.1, 2, mean); PA.ANM.2 <- apply(PA.ANM.2, 2, mean); PA.ANM.3 <- apply(PA.ANM.3, 2, mean); PA.ANM.4 <- apply(PA.ANM.4, 2, mean); 
    RI.ANM.1 <- apply(RI.ANM.1, 2, mean); RI.ANM.2 <- apply(RI.ANM.2, 2, mean); RI.ANM.3 <- apply(RI.ANM.3, 2, mean); RI.ANM.4 <- apply(RI.ANM.4, 2, mean); 
    SC.ANM.1 <- apply(SC.ANM.1, 2, mean); SC.ANM.2 <- apply(SC.ANM.2, 2, mean); SC.ANM.3 <- apply(SC.ANM.3, 2, mean); SC.ANM.4 <- apply(SC.ANM.4, 2, mean); 
    SD.ANM.1 <- apply(SD.ANM.1, 2, mean); SD.ANM.2 <- apply(SD.ANM.2, 2, mean); SD.ANM.3 <- apply(SD.ANM.3, 2, mean); SD.ANM.4 <- apply(SD.ANM.4, 2, mean); 
    TN.ANM.1 <- apply(TN.ANM.1, 2, mean); TN.ANM.2 <- apply(TN.ANM.2, 2, mean); TN.ANM.3 <- apply(TN.ANM.3, 2, mean); TN.ANM.4 <- apply(TN.ANM.4, 2, mean); 
    TX.ANM.1 <- apply(TX.ANM.1, 2, mean); TX.ANM.2 <- apply(TX.ANM.2, 2, mean); TX.ANM.3 <- apply(TX.ANM.3, 2, mean); TX.ANM.4 <- apply(TX.ANM.4, 2, mean); 
    UT.ANM.1 <- apply(UT.ANM.1, 2, mean); UT.ANM.2 <- apply(UT.ANM.2, 2, mean); UT.ANM.3 <- apply(UT.ANM.3, 2, mean); UT.ANM.4 <- apply(UT.ANM.4, 2, mean); 
    VT.ANM.1 <- apply(VT.ANM.1, 2, mean); VT.ANM.2 <- apply(VT.ANM.2, 2, mean); VT.ANM.3 <- apply(VT.ANM.3, 2, mean); VT.ANM.4 <- apply(VT.ANM.4, 2, mean); 
    VA.ANM.1 <- apply(VA.ANM.1, 2, mean); VA.ANM.2 <- apply(VA.ANM.2, 2, mean); VA.ANM.3 <- apply(VA.ANM.3, 2, mean); VA.ANM.4 <- apply(VA.ANM.4, 2, mean); 
    WA.ANM.1 <- apply(WA.ANM.1, 2, mean); WA.ANM.2 <- apply(WA.ANM.2, 2, mean); WA.ANM.3 <- apply(WA.ANM.3, 2, mean); WA.ANM.4 <- apply(WA.ANM.4, 2, mean); 
    WV.ANM.1 <- apply(WV.ANM.1, 2, mean); WV.ANM.2 <- apply(WV.ANM.2, 2, mean); WV.ANM.3 <- apply(WV.ANM.3, 2, mean); WV.ANM.4 <- apply(WV.ANM.4, 2, mean); 
    WI.ANM.1 <- apply(WI.ANM.1, 2, mean); WI.ANM.2 <- apply(WI.ANM.2, 2, mean); WI.ANM.3 <- apply(WI.ANM.3, 2, mean); WI.ANM.4 <- apply(WI.ANM.4, 2, mean); 
    WY.ANM.1 <- apply(WY.ANM.1, 2, mean); WY.ANM.2 <- apply(WY.ANM.2, 2, mean); WY.ANM.3 <- apply(WY.ANM.3, 2, mean); WY.ANM.4 <- apply(WY.ANM.4, 2, mean); 
    
    
    
    
    
    ###
    
    
    
    
    
    #### Combine the states into 4 stages ####
    ANM1 = rbind(
        AL.ANM.1, AK.ANM.1, AZ.ANM.1, AR.ANM.1, CA.ANM.1,
        CO.ANM.1, CT.ANM.1, DE.ANM.1, FL.ANM.1, GA.ANM.1,
        HI.ANM.1, ID.ANM.1, IL.ANM.1, IN.ANM.1, IA.ANM.1,
        KS.ANM.1, KY.ANM.1, LA.ANM.1, ME.ANM.1, MD.ANM.1, 
        MA.ANM.1, MI.ANM.1, MN.ANM.1, MS.ANM.1, MO.ANM.1, 
        MT.ANM.1, NE.ANM.1, NV.ANM.1, NH.ANM.1, NJ.ANM.1, 
        NM.ANM.1, NY.ANM.1, NC.ANM.1, ND.ANM.1, OH.ANM.1, 
        OK.ANM.1, OR.ANM.1, PA.ANM.1, RI.ANM.1, SC.ANM.1, 
        SD.ANM.1, TN.ANM.1, TX.ANM.1, UT.ANM.1, VT.ANM.1, 
        VA.ANM.1, WA.ANM.1, WV.ANM.1, WI.ANM.1, WY.ANM.1
    )
    
    ANM2 = rbind(
        AL.ANM.2, AK.ANM.2, AZ.ANM.2, AR.ANM.2, CA.ANM.2,
        CO.ANM.2, CT.ANM.2, DE.ANM.2, FL.ANM.2, GA.ANM.2,
        HI.ANM.2, ID.ANM.2, IL.ANM.2, IN.ANM.2, IA.ANM.2,
        KS.ANM.2, KY.ANM.2, LA.ANM.2, ME.ANM.2, MD.ANM.2, 
        MA.ANM.2, MI.ANM.2, MN.ANM.2, MS.ANM.2, MO.ANM.2, 
        MT.ANM.2, NE.ANM.2, NV.ANM.2, NH.ANM.2, NJ.ANM.2, 
        NM.ANM.2, NY.ANM.2, NC.ANM.2, ND.ANM.2, OH.ANM.2, 
        OK.ANM.2, OR.ANM.2, PA.ANM.2, RI.ANM.2, SC.ANM.2, 
        SD.ANM.2, TN.ANM.2, TX.ANM.2, UT.ANM.2, VT.ANM.2, 
        VA.ANM.2, WA.ANM.2, WV.ANM.2, WI.ANM.2, WY.ANM.2
    )
    
    ANM3 = rbind(
        AL.ANM.3, AK.ANM.3, AZ.ANM.3, AR.ANM.3, CA.ANM.3,
        CO.ANM.3, CT.ANM.3, DE.ANM.3, FL.ANM.3, GA.ANM.3,
        HI.ANM.3, ID.ANM.3, IL.ANM.3, IN.ANM.3, IA.ANM.3,
        KS.ANM.3, KY.ANM.3, LA.ANM.3, ME.ANM.3, MD.ANM.3, 
        MA.ANM.3, MI.ANM.3, MN.ANM.3, MS.ANM.3, MO.ANM.3, 
        MT.ANM.3, NE.ANM.3, NV.ANM.3, NH.ANM.3, NJ.ANM.3, 
        NM.ANM.3, NY.ANM.3, NC.ANM.3, ND.ANM.3, OH.ANM.3, 
        OK.ANM.3, OR.ANM.3, PA.ANM.3, RI.ANM.3, SC.ANM.3, 
        SD.ANM.3, TN.ANM.3, TX.ANM.3, UT.ANM.3, VT.ANM.3, 
        VA.ANM.3, WA.ANM.3, WV.ANM.3, WI.ANM.3, WY.ANM.3
    )
    
    ANM4 = rbind(
        AL.ANM.4, AK.ANM.4, AZ.ANM.4, AR.ANM.4, CA.ANM.4,
        CO.ANM.4, CT.ANM.4, DE.ANM.4, FL.ANM.4, GA.ANM.4,
        HI.ANM.4, ID.ANM.4, IL.ANM.4, IN.ANM.4, IA.ANM.4,
        KS.ANM.4, KY.ANM.4, LA.ANM.4, ME.ANM.4, MD.ANM.4, 
        MA.ANM.4, MI.ANM.4, MN.ANM.4, MS.ANM.4, MO.ANM.4, 
        MT.ANM.4, NE.ANM.4, NV.ANM.4, NH.ANM.4, NJ.ANM.4, 
        NM.ANM.4, NY.ANM.4, NC.ANM.4, ND.ANM.4, OH.ANM.4, 
        OK.ANM.4, OR.ANM.4, PA.ANM.4, RI.ANM.4, SC.ANM.4, 
        SD.ANM.4, TN.ANM.4, TX.ANM.4, UT.ANM.4, VT.ANM.4, 
        VA.ANM.4, WA.ANM.4, WV.ANM.4, WI.ANM.4, WY.ANM.4
    )
}
