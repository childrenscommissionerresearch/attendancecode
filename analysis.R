#Purpose: Examine the link between previous attendance and current attendance at school or trust level

# Load packages -------------------------------------------------------------
require(dplyr)
require(rlang)

# Import data -------------------------------------------------------------
# Read in attendance register data from most recent term or academic year and previous terms or academic years (depending on what data you have available)
dfmostrecentperiod <- read.csv('insert_filepath_mostrecentperiod.csv')
dfpreviousperiod1 <- read.csv('insert_filepath_previousperiod1.csv')
dfpreviousperiod2 <- read.csv('insert_filepath_previousperiod2.csv')

# Define functions ------------------------------------------------------------
clean_attendance_data <- function(df){
  df_clean <- df %>% 
    #Renaming columns for code. Enter name of current columns on right side of "=".
    rename(UPN = UPN,
           Year = Year,
           Mark = Mark,
           URN = School) %>% 
    #Filtering out codes not counted in possible attendances (X- not expected to attend / Covid related reasons, Z-not on school register
    #Y school closure and other and D registered and attending elsewhere as pupil not absent from required session)
    filter(!Mark %in% c("#","Z","X","Y","D")) %>% 
    #Filtering year groups to 1-11
    filter(as.character(Year) %in% c("1","2","3","4","5","6","7","8","9","10","11")) %>% 
    mutate(Attendance=case_when(Mark %in% c("/","\\","B","D","L","J","P","V","W") ~ "P",
                                Mark %in% c("C","E","H","I","M","R","S","T","Y","Z") ~ "AA",
                                Mark %in% c("G","N","O","U") ~ "UA"),
           absenceflag=ifelse(Attendance %in% c("UA","AA"),1,0), 
           UAflag=ifelse(Attendance=="UA",1,0))
  return(df_clean)
}

pupillevel_mostrecentperiod <- function(dfclean){
  dfpupillevel <- dfclean %>% 
    group_by(UPN,Year) %>% 
    summarise(OAmostrecentperiod=mean(absenceflag,na.rm=T),
              OUAmostrecentperiod=mean(UAflag,na.rm=T)) %>% 
    mutate(PAmostrecentperiod=ifelse(OAmostrecentperiod>=0.1,1,0),#Persistent absence
           PUAmostrecentperiod=ifelse(OUAmostrecentperiod>=0.1,1,0)) #Persistent unauthorised absence
  return(dfpupillevel)
}

pupillevel_previousperiod <- function(dfclean,period){
  dfpupillevel <- dfclean %>% 
    group_by(UPN) %>% 
    summarise(OApreviousperiod=mean(absenceflag,na.rm=T),
              OUApreviousperiod=mean(UAflag,na.rm=T)) %>% 
    mutate(PApreviousperiod=ifelse(OApreviousperiod>=0.1,1,0),#Persistent absence 
           PUApreviousperiod=ifelse(OUApreviousperiod>=0.1,1,0)) #Persistent unauthorised absence
  names(dfpupillevel) <- c("UPN",paste0("OApreviousperiod",period),
                           paste0("OUApreviousperiod",period),
                           paste0("PApreviousperiod",period),
                           paste0("PUApreviousperiod",period))
  return(dfpupillevel)
}

calc_absence_by_previousabsencetype_year <- function(previousabsencevariable){
  summary <- dfpupillevel %>% 
    group_by(Year,{{previousabsencevariable}}) %>% 
    summarise(share_pa=round(mean(PAmostrecentperiod,na.rm=T)*100,0),
              share_pua=round(mean(PUAmostrecentperiod,na.rm=T)*100,0),
              numberofpupils=n()) %>% 
    filter(!is.na({{previousabsencevariable}}))
  return(summary)
}

# Clean data ------------------------------------------------------------
# add previous periods as necessary
dfpupillevelmostrecentperiod <- pupillevel_mostrecentperiod(clean_attendance_data(dfmostrecentperiod))
dfpupillevelpreviousperiod1 <- pupillevel_previousperiod(clean_attendance_data(dfpreviousperiod1),"1")
dfpupillevelpreviousperiod2 <- pupillevel_previousperiod(clean_attendance_data(dfpreviousperiod2),"2")

# Join data ------------------------------------------------------------
dfpupillevel <- dfpupillevelmostrecentperiod %>% 
  #join previous available periods
  left_join(dfpupillevelpreviousperiod1,by="UPN") %>%
  left_join(dfpupillevelpreviousperiod2,by="UPN") %>%
  mutate(previouslyPA=ifelse(PApreviousperiod1==1,"Yes",
                             ifelse(PApreviousperiod2==1,"Yes","No")),
         previouslyPUA=ifelse(PUApreviousperiod1==1,"Yes",
                              ifelse(PUApreviousperiod2==1,"Yes","No")))

# Analyse data --------------------------------------------------------------------------
calc_absence_by_previousabsencetype_year(previouslyPA)
calc_absence_by_previousabsencetype_year(previouslyPUA)
