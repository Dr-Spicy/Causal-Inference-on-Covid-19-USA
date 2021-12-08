#### Generate the pairwise temporal datasets ####

states = read.csv("Data/UMD Data/State.csv")
states = as.data.table(states)
states$date = lubridate::mdy(states$date)
states$Week <- lubridate::floor_date(states$date, "week", week_start = 1)

national = read.csv("Data/UMD Data/National.csv")
national = as.data.table(national)
national$date = lubridate::mdy(national$date)
national$Week <- lubridate::floor_date(national$date, "week", week_start = 1)

Google.20 = read.csv("Data/Region_Mobility_Report_CSVs/2020_US_Region_Mobility_Report.csv")
Google.21 = read.csv("Data/Region_Mobility_Report_CSVs/2021_US_Region_Mobility_Report.csv")
Google.mob = rbind(Google.20, Google.21)
Google.mob <- Google.mob[,c(9,3,10:15)]
Google.mob = as.data.table(Google.mob)
Google.mob$date = ymd(Google.mob$date)
Google.mob$Week <- floor_date(Google.mob$date, "week", week_start = 1)

# Google.mob.son = Google.mob[date>= "2020-03-23" & date<= "2021-04-18"][,-1]
Google.mob = Google.mob[date>= "2020-03-23" & date<= "2021-03-21"][,-1]


AL.g = Google.mob[sub_region_1 == 'Alabama'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>% 
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")

AK.g = Google.mob[sub_region_1 == 'Alaska'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
AZ.g = Google.mob[sub_region_1 == 'Arizona'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
AR.g = Google.mob[sub_region_1 == 'Arkansas'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
CA.g = Google.mob[sub_region_1 == 'California'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
CO.g = Google.mob[sub_region_1 == 'Colorado'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
CT.g = Google.mob[sub_region_1 == 'Connecticut'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
DE.g = Google.mob[sub_region_1 == 'Delaware'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
FL.g = Google.mob[sub_region_1 == 'Florida'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
GA.g = Google.mob[sub_region_1 == 'Georgia'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
HI.g = Google.mob[sub_region_1 == 'Hawaii'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
ID.g = Google.mob[sub_region_1 == 'Idaho'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
IL.g = Google.mob[sub_region_1 == 'Illinois'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
IN.g = Google.mob[sub_region_1 == 'Indiana'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
IA.g = Google.mob[sub_region_1 == 'Iowa'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
KS.g = Google.mob[sub_region_1 == 'Kansas'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
KY.g = Google.mob[sub_region_1 == 'Kentucky'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
LA.g = Google.mob[sub_region_1 == 'Louisiana'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
ME.g = Google.mob[sub_region_1 == 'Maine'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
MD.g = Google.mob[sub_region_1 == 'Maryland'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
MA.g = Google.mob[sub_region_1 == 'Massachusetts'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
MI.g = Google.mob[sub_region_1 == 'Michigan'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
MN.g = Google.mob[sub_region_1 == 'Minnesota'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
MS.g = Google.mob[sub_region_1 == 'Mississippi'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
MO.g = Google.mob[sub_region_1 == 'Missouri'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
MT.g = Google.mob[sub_region_1 == 'Montana'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
NE.g = Google.mob[sub_region_1 == 'Nebraska'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
NV.g = Google.mob[sub_region_1 == 'Nevada'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
NH.g = Google.mob[sub_region_1 == 'New Hampshire'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
NJ.g = Google.mob[sub_region_1 == 'New Jersey'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
NM.g = Google.mob[sub_region_1 == 'New Mexico'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
NY.g = Google.mob[sub_region_1 == 'New York'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
NC.g = Google.mob[sub_region_1 == 'North Carolina'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
ND.g = Google.mob[sub_region_1 == 'North Dakota'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
OH.g = Google.mob[sub_region_1 == 'Ohio'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
OK.g = Google.mob[sub_region_1 == 'Oklahoma'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
OR.g = Google.mob[sub_region_1 == 'Oregon'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
PA.g = Google.mob[sub_region_1 == 'Pennsylvania'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
RI.g = Google.mob[sub_region_1 == 'Rhode Island'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
SC.g = Google.mob[sub_region_1 == 'South Carolina'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
SD.g = Google.mob[sub_region_1 == 'South Dakota'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
TN.g = Google.mob[sub_region_1 == 'Tennessee'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
TX.g = Google.mob[sub_region_1 == 'Texas'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
UT.g = Google.mob[sub_region_1 == 'Utah'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
VT.g = Google.mob[sub_region_1 == 'Vermont'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
VA.g = Google.mob[sub_region_1 == 'Virginia'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
WA.g = Google.mob[sub_region_1 == 'Washington'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
WV.g = Google.mob[sub_region_1 == 'West Virginia'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
WI.g = Google.mob[sub_region_1 == 'Wisconsin'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
WY.g = Google.mob[sub_region_1 == 'Wyoming'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")
USA.g = Google.mob[sub_region_1 == ''] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
    replace(is.na(.),0) %>%
    summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")

# TX.g.son = Google.mob.son[sub_region_1 == 'Texas'] %>% group_by(Week) %>% dplyr::select(-sub_region_1) %>%
#     replace(is.na(.),0) %>%
#     summarise_each(funs(mean)) %>% remove_rownames %>% column_to_rownames(var = "Week")

#### limit the time range from March 23nd 2020 (Monday, 83rd row) to Mar 21nd 2021 (Sunday, 446th row) ####
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
USA = national[83:446,]

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

USA <- USA %>%
    group_by(Week) %>%
    summarise(Name = "USA", across(c(Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))

# ## Get the Goole part of data
# TX.son = states[STFIPS == 48][83:474,]
# TX.son <- TX.son %>%
#     group_by(Week) %>%
#     summarise(State = 'TX', across(c(STFIPS, Social.distancing.index:X..change.in.consumption, Transit.mode.share:COVID.death.rate), mean))
# 



## Get the temporal part from UMD data

AL <- AL %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
AK <- AK %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
AZ <- AZ %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
AR <- AR %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
CA <- CA %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
CO <- CO %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
CT <- CT %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
DE <- DE %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
FL <- FL %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
GA <- GA %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
HI <- HI %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
ID <- ID %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
IL <- IL %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
IN <- IN %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
IA <- IA %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
KS <- KS %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
KY <- KY %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
LA <- LA %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
ME <- ME %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
MD <- MD %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
MA <- MA %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
MI <- MI %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
MN <- MN %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
MS <- MS %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
MO <- MO %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
MT <- MT %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
NE <- NE %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
NV <- NV %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
NH <- NH %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
NJ <- NJ %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
NM <- NM %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
NY <- NY %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
NC <- NC %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
ND <- ND %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
OH <- OH %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
OK <- OK %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
OR <- OR %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
PA <- PA %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
RI <- RI %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
SC <- SC %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
SD <- SD %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
TN <- TN %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
TX <- TX %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
UT <- UT %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
VT <- VT %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
VA <- VA %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
WA <- WA %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
WI <- WI %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
WV <- WV %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
WY <- WY %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )
USA <- USA %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )

# TX.son <- TX.son %>% remove_rownames %>% column_to_rownames(var = "Week") %>% dplyr::select(New.COVID.cases, New.cases.1000.people, Active.cases.1000.people, Work.trips.person, Non.work.trips.person , Miles.person, X..change.in.consumption, Social.distancing.index, Unemployment.rate, X..working.from.home, Tests.done.1000.people, Imported.COVID.cases, )

## Bind the 2 parts together
if (response == "new.case"){
    
    
    AL = bind_cols(AL, AL.g) %>% dplyr::select(-New.cases.1000.people)
    AK = bind_cols(AK, AK.g)%>% dplyr::select(-New.cases.1000.people)
    AZ = bind_cols(AZ, AZ.g)%>% dplyr::select(-New.cases.1000.people)
    AR = bind_cols(AR, AR.g)%>% dplyr::select(-New.cases.1000.people)
    CA = bind_cols(CA, CA.g)%>% dplyr::select(-New.cases.1000.people)
    CO = bind_cols(CO, CO.g)%>% dplyr::select(-New.cases.1000.people)
    CT = bind_cols(CT, CT.g)%>% dplyr::select(-New.cases.1000.people)
    DE = bind_cols(DE, DE.g)%>% dplyr::select(-New.cases.1000.people)
    FL = bind_cols(FL, FL.g)%>% dplyr::select(-New.cases.1000.people)
    GA = bind_cols(GA, GA.g)%>% dplyr::select(-New.cases.1000.people)
    HI = bind_cols(HI, HI.g)%>% dplyr::select(-New.cases.1000.people)
    ID = bind_cols(ID, ID.g)%>% dplyr::select(-New.cases.1000.people)
    IL = bind_cols(IL, IL.g)%>% dplyr::select(-New.cases.1000.people)
    IN = bind_cols(IN, IN.g)%>% dplyr::select(-New.cases.1000.people)
    IA = bind_cols(IA, IA.g)%>% dplyr::select(-New.cases.1000.people)
    KS = bind_cols(KS, KS.g)%>% dplyr::select(-New.cases.1000.people)
    KY = bind_cols(KY, KY.g)%>% dplyr::select(-New.cases.1000.people)
    LA = bind_cols(LA, LA.g)%>% dplyr::select(-New.cases.1000.people)
    ME = bind_cols(ME, ME.g)%>% dplyr::select(-New.cases.1000.people)
    MD = bind_cols(MD, MD.g)%>% dplyr::select(-New.cases.1000.people)
    MA = bind_cols(MA, MA.g)%>% dplyr::select(-New.cases.1000.people)
    MI = bind_cols(MI, MI.g)%>% dplyr::select(-New.cases.1000.people)
    MN = bind_cols(MN, MN.g)%>% dplyr::select(-New.cases.1000.people)
    MS = bind_cols(MS, MS.g)%>% dplyr::select(-New.cases.1000.people)
    MO = bind_cols(MO, MO.g)%>% dplyr::select(-New.cases.1000.people)
    MT = bind_cols(MT, MT.g)%>% dplyr::select(-New.cases.1000.people)
    NE = bind_cols(NE, NE.g)%>% dplyr::select(-New.cases.1000.people)
    NV = bind_cols(NV, NV.g)%>% dplyr::select(-New.cases.1000.people)
    NH = bind_cols(NH, NH.g)%>% dplyr::select(-New.cases.1000.people)
    NJ = bind_cols(NJ, NJ.g)%>% dplyr::select(-New.cases.1000.people)
    NM = bind_cols(NM, NM.g)%>% dplyr::select(-New.cases.1000.people)
    NY = bind_cols(NY, NY.g)%>% dplyr::select(-New.cases.1000.people)
    NC = bind_cols(NC, NC.g)%>% dplyr::select(-New.cases.1000.people)
    ND = bind_cols(ND, ND.g)%>% dplyr::select(-New.cases.1000.people)
    OH = bind_cols(OH, OH.g)%>% dplyr::select(-New.cases.1000.people)
    OK = bind_cols(OK, OK.g)%>% dplyr::select(-New.cases.1000.people)
    OR = bind_cols(OR, OR.g)%>% dplyr::select(-New.cases.1000.people)
    PA = bind_cols(PA, PA.g)%>% dplyr::select(-New.cases.1000.people)
    RI = bind_cols(RI, RI.g)%>% dplyr::select(-New.cases.1000.people)
    SC = bind_cols(SC, SC.g)%>% dplyr::select(-New.cases.1000.people)
    SD = bind_cols(SD, SD.g)%>% dplyr::select(-New.cases.1000.people)
    TN = bind_cols(TN, TN.g)%>% dplyr::select(-New.cases.1000.people)
    TX = bind_cols(TX, TX.g)%>% dplyr::select(-New.cases.1000.people)
    UT = bind_cols(UT, UT.g)%>% dplyr::select(-New.cases.1000.people)
    VT = bind_cols(VT, VT.g)%>% dplyr::select(-New.cases.1000.people)
    VA = bind_cols(VA, VA.g)%>% dplyr::select(-New.cases.1000.people)
    WA = bind_cols(WA, WA.g)%>% dplyr::select(-New.cases.1000.people)
    WV = bind_cols(WV, WV.g)%>% dplyr::select(-New.cases.1000.people)
    WI = bind_cols(WI, WI.g)%>% dplyr::select(-New.cases.1000.people)
    WY = bind_cols(WY, WY.g)%>% dplyr::select(-New.cases.1000.people)
    USA = bind_cols(USA, USA.g)%>% dplyr::select(-New.cases.1000.people)
    # TX.son = bind_cols(TX.son, TX.g.son)   
} else if (response == "cumsum.new.cases"){
    
    AL = bind_cols(AL, AL.g) %>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    AK = bind_cols(AK, AK.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    AZ = bind_cols(AZ, AZ.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    AR = bind_cols(AR, AR.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    CA = bind_cols(CA, CA.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    CO = bind_cols(CO, CO.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    CT = bind_cols(CT, CT.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    DE = bind_cols(DE, DE.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    FL = bind_cols(FL, FL.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    GA = bind_cols(GA, GA.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    HI = bind_cols(HI, HI.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    ID = bind_cols(ID, ID.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    IL = bind_cols(IL, IL.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    IN = bind_cols(IN, IN.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    IA = bind_cols(IA, IA.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    KS = bind_cols(KS, KS.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    KY = bind_cols(KY, KY.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    LA = bind_cols(LA, LA.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    ME = bind_cols(ME, ME.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    MD = bind_cols(MD, MD.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    MA = bind_cols(MA, MA.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    MI = bind_cols(MI, MI.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    MN = bind_cols(MN, MN.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    MS = bind_cols(MS, MS.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    MO = bind_cols(MO, MO.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    MT = bind_cols(MT, MT.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    NE = bind_cols(NE, NE.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    NV = bind_cols(NV, NV.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    NH = bind_cols(NH, NH.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    NJ = bind_cols(NJ, NJ.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    NM = bind_cols(NM, NM.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    NY = bind_cols(NY, NY.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    NC = bind_cols(NC, NC.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    ND = bind_cols(ND, ND.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    OH = bind_cols(OH, OH.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    OK = bind_cols(OK, OK.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    OR = bind_cols(OR, OR.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    PA = bind_cols(PA, PA.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    RI = bind_cols(RI, RI.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    SC = bind_cols(SC, SC.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    SD = bind_cols(SD, SD.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    TN = bind_cols(TN, TN.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    TX = bind_cols(TX, TX.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    UT = bind_cols(UT, UT.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    VT = bind_cols(VT, VT.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    VA = bind_cols(VA, VA.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    WA = bind_cols(WA, WA.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    WV = bind_cols(WV, WV.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    WI = bind_cols(WI, WI.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    WY = bind_cols(WY, WY.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    USA = bind_cols(USA, USA.g)%>% mutate(New.COVID.cases = cumsum(New.COVID.cases))%>% dplyr::select(-New.cases.1000.people)
    } else if (response == "New.case.1000.people"){
    
    AL = bind_cols(AL, AL.g) %>% dplyr::select(-New.COVID.cases)
    AK = bind_cols(AK, AK.g)%>% dplyr::select(-New.COVID.cases)
    AZ = bind_cols(AZ, AZ.g)%>% dplyr::select(-New.COVID.cases)
    AR = bind_cols(AR, AR.g)%>% dplyr::select(-New.COVID.cases)
    CA = bind_cols(CA, CA.g)%>% dplyr::select(-New.COVID.cases)
    CO = bind_cols(CO, CO.g)%>% dplyr::select(-New.COVID.cases)
    CT = bind_cols(CT, CT.g)%>% dplyr::select(-New.COVID.cases)
    DE = bind_cols(DE, DE.g)%>% dplyr::select(-New.COVID.cases)
    FL = bind_cols(FL, FL.g)%>% dplyr::select(-New.COVID.cases)
    GA = bind_cols(GA, GA.g)%>% dplyr::select(-New.COVID.cases)
    HI = bind_cols(HI, HI.g)%>% dplyr::select(-New.COVID.cases)
    ID = bind_cols(ID, ID.g)%>% dplyr::select(-New.COVID.cases)
    IL = bind_cols(IL, IL.g)%>% dplyr::select(-New.COVID.cases)
    IN = bind_cols(IN, IN.g)%>% dplyr::select(-New.COVID.cases)
    IA = bind_cols(IA, IA.g)%>% dplyr::select(-New.COVID.cases)
    KS = bind_cols(KS, KS.g)%>% dplyr::select(-New.COVID.cases)
    KY = bind_cols(KY, KY.g)%>% dplyr::select(-New.COVID.cases)
    LA = bind_cols(LA, LA.g)%>% dplyr::select(-New.COVID.cases)
    ME = bind_cols(ME, ME.g)%>% dplyr::select(-New.COVID.cases)
    MD = bind_cols(MD, MD.g)%>% dplyr::select(-New.COVID.cases)
    MA = bind_cols(MA, MA.g)%>% dplyr::select(-New.COVID.cases)
    MI = bind_cols(MI, MI.g)%>% dplyr::select(-New.COVID.cases)
    MN = bind_cols(MN, MN.g)%>% dplyr::select(-New.COVID.cases)
    MS = bind_cols(MS, MS.g)%>% dplyr::select(-New.COVID.cases)
    MO = bind_cols(MO, MO.g)%>% dplyr::select(-New.COVID.cases)
    MT = bind_cols(MT, MT.g)%>% dplyr::select(-New.COVID.cases)
    NE = bind_cols(NE, NE.g)%>% dplyr::select(-New.COVID.cases)
    NV = bind_cols(NV, NV.g)%>% dplyr::select(-New.COVID.cases)
    NH = bind_cols(NH, NH.g)%>% dplyr::select(-New.COVID.cases)
    NJ = bind_cols(NJ, NJ.g)%>% dplyr::select(-New.COVID.cases)
    NM = bind_cols(NM, NM.g)%>% dplyr::select(-New.COVID.cases)
    NY = bind_cols(NY, NY.g)%>% dplyr::select(-New.COVID.cases)
    NC = bind_cols(NC, NC.g)%>% dplyr::select(-New.COVID.cases)
    ND = bind_cols(ND, ND.g)%>% dplyr::select(-New.COVID.cases)
    OH = bind_cols(OH, OH.g)%>% dplyr::select(-New.COVID.cases)
    OK = bind_cols(OK, OK.g)%>% dplyr::select(-New.COVID.cases)
    OR = bind_cols(OR, OR.g)%>% dplyr::select(-New.COVID.cases)
    PA = bind_cols(PA, PA.g)%>% dplyr::select(-New.COVID.cases)
    RI = bind_cols(RI, RI.g)%>% dplyr::select(-New.COVID.cases)
    SC = bind_cols(SC, SC.g)%>% dplyr::select(-New.COVID.cases)
    SD = bind_cols(SD, SD.g)%>% dplyr::select(-New.COVID.cases)
    TN = bind_cols(TN, TN.g)%>% dplyr::select(-New.COVID.cases)
    TX = bind_cols(TX, TX.g)%>% dplyr::select(-New.COVID.cases)
    UT = bind_cols(UT, UT.g)%>% dplyr::select(-New.COVID.cases)
    VT = bind_cols(VT, VT.g)%>% dplyr::select(-New.COVID.cases)
    VA = bind_cols(VA, VA.g)%>% dplyr::select(-New.COVID.cases)
    WA = bind_cols(WA, WA.g)%>% dplyr::select(-New.COVID.cases)
    WV = bind_cols(WV, WV.g)%>% dplyr::select(-New.COVID.cases)
    WI = bind_cols(WI, WI.g)%>% dplyr::select(-New.COVID.cases)
    WY = bind_cols(WY, WY.g)%>% dplyr::select(-New.COVID.cases)
    USA = bind_cols(USA, USA.g)%>% dplyr::select(-New.COVID.cases)
    } else if (response == "cumsum.new.cases.1000.people") { 
        
        AL = bind_cols(AL, AL.g) %>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        AK = bind_cols(AK, AK.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        AZ = bind_cols(AZ, AZ.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        AR = bind_cols(AR, AR.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        CA = bind_cols(CA, CA.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        CO = bind_cols(CO, CO.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        CT = bind_cols(CT, CT.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        DE = bind_cols(DE, DE.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        FL = bind_cols(FL, FL.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        GA = bind_cols(GA, GA.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        HI = bind_cols(HI, HI.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        ID = bind_cols(ID, ID.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        IL = bind_cols(IL, IL.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        IN = bind_cols(IN, IN.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        IA = bind_cols(IA, IA.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        KS = bind_cols(KS, KS.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        KY = bind_cols(KY, KY.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        LA = bind_cols(LA, LA.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        ME = bind_cols(ME, ME.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        MD = bind_cols(MD, MD.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        MA = bind_cols(MA, MA.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        MI = bind_cols(MI, MI.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        MN = bind_cols(MN, MN.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        MS = bind_cols(MS, MS.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        MO = bind_cols(MO, MO.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        MT = bind_cols(MT, MT.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        NE = bind_cols(NE, NE.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        NV = bind_cols(NV, NV.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        NH = bind_cols(NH, NH.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        NJ = bind_cols(NJ, NJ.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        NM = bind_cols(NM, NM.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        NY = bind_cols(NY, NY.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        NC = bind_cols(NC, NC.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        ND = bind_cols(ND, ND.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        OH = bind_cols(OH, OH.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        OK = bind_cols(OK, OK.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        OR = bind_cols(OR, OR.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        PA = bind_cols(PA, PA.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        RI = bind_cols(RI, RI.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        SC = bind_cols(SC, SC.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        SD = bind_cols(SD, SD.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        TN = bind_cols(TN, TN.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        TX = bind_cols(TX, TX.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        UT = bind_cols(UT, UT.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        VT = bind_cols(VT, VT.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        VA = bind_cols(VA, VA.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        WA = bind_cols(WA, WA.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        WV = bind_cols(WV, WV.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        WI = bind_cols(WI, WI.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        WY = bind_cols(WY, WY.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        USA = bind_cols(USA, USA.g)%>% mutate(New.cases.1000.people = cumsum(New.cases.1000.people))%>% dplyr::select(-New.COVID.cases)
        
}

temporal = list(USA, AL, AK, AZ, AR, CA, CO, CT, DE, FL, GA, HI, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, 
                MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, 
                WA, WV, WI, WY)


# TX.son = TX.son[,-c(1,8)]
# write.csv(TX.son, file = 'Texas.Son.csv')
