require(tidyverse)
######### Kachemak Bay Birders Bay --- West Data ##############
# Import data - Available form Kachemak Bay Birders
# west.ketch <- read.csv('West_data.csv', header = T)

a <- filter(west.ketch, grepl('census', Source.Comments))

# Group into date, then filter out non relavent species
west.clean <-
  west.ketch %>%
  filter(grepl('census', Source.Comments)) %>%
  dplyr::select(-Source.Comments, -Code.No) %>%
  group_by(Date, Species, Location) %>% #
  dplyr::summarize(Sum.count = sum(No, na.rm=T), n.sites= n()) %>% # Location1= Location %in% first(Location),Location2 = last(Location)
  filter(!is.na(Sum.count)) %>%
  tidyr::spread(Species, Sum.count ) %>% # spread count column into species counts
#   dplyr::select(n.sites, Date,DUNL, WESA ) %>%
  group_by(Date) %>%
  dplyr::summarize(DUNL = sum(DUNL, na.rm = T), WESA = sum(WESA, na.rm=T), nsites = sum(n.sites, na.rm=T)) %>%
  mutate(Year = format(as.Date(Date), format = "%Y"), # Extract date information
         Month = format(as.Date(Date), format = "%m"),
         Day = format(as.Date(Date), format = "%d"),
         Day.of.Year = as.integer(format(as.Date(Date) , format = "%j")),
         Site = 'Kachemak Bay',
         SiteID = 'KABA') %>%
  filter(Day.of.Year > 110 & Day.of.Year < 150 ) %>%#& Year <= 1994 & Year != 1988) %>%
  arrange(Date) %>%
  mutate_each(funs(as.character))

west.clean$WESA[is.na(west.clean$WESA)] <- 0 # Replace NAs with zeros
west.clean$DUNL[is.na(west.clean$DUNL)] <- 0

multiple.locations <- west.ketch %>% group_by(Date, Location) %>% summarize(n.1 = n()) %>% group_by(Date) %>% summarize(n.2 = length(Location)) %>%
  arrange(desc(n.2)) %>% filter(n.2 > 1) %>% dplyr::select(Date)

# datebyloc <- filter(west.ketch, Date %in% multiple.locations$Date )%>%
#   group_by(Date, Location)%>% summarize() %>% distinct() %>% tidyr::spread(Location, Date)



######### Kachemack Bay --- KB Birders ##############
KB.Birders.ketch <- read.csv('Kachemak_Redone.csv', header = T)
KB2.clean <- 
  KB.Birders.ketch %>%
  rename(WESA = Western.Sandpiper,
         LESA = Least.Sandpiper,
         SESA = Semipalmated.Sandpiper,
         DUNL = Dunlin,
         sm.peeps = LESA.WESA.SESA) %>%
  mutate(Month = ifelse(Month == 'April', '04', '05')) %>%
  mutate(Date = as.Date(paste(Year, '-', Month, "-", Day, sep = ""), format = "%Y-%m-%d"),
         Day.of.Year = as.integer(format(Date, format = "%j"))) %>% 
  group_by(Year, Day.of.Year) %>% 
    mutate(prop.W = WESA / max(1,WESA + LESA + SESA),
         WESA.2 = WESA + sm.peeps ) %>% ungroup %>%  #* prop.W
    mutate(
         Site = 'Kachemak Bay',
         SiteID = 'KABA',
         Year = as.character(Year)) %>%
  rename(WESA.base = WESA,
         WESA = WESA.2) %>%
  dplyr::select(Site, SiteID, Date, Month, Day, Year, Day.of.Year, WESA, DUNL, prop.W) %>%
  mutate_each(funs(as.character))


## Dealingwith2016
require(lubridate)
kbsupp2016 <- read.csv("KABA_Supplimentary_2016_raw_ebird.csv") %>% 
  mutate(Date = mdy(DATE),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date),
         Day.of.Year = yday(Date)) %>% 
  group_by(Date, Year, Month, Day, COMMON.NAME, Day.of.Year, LOCALITY) %>% 
  summarize(COUNT = max(COUNT, na.rm=T)) %>% 
  group_by(Date, Year, Month, Day, COMMON.NAME, Day.of.Year) %>% 
  summarize(COUNT = sum(COUNT)) %>% ungroup %>% 
  spread(key = COMMON.NAME, value = COUNT, fill = 0) %>% filter(Day.of.Year >100) %>% 
  rename(WESA = `Western Sandpiper`,
          LESA = `Least Sandpiper`,
         SESA = `Semipalmated Sandpiper`,
         DUNL = Dunlin,
         sm.peeps = `peep sp.`) %>% 
  mutate(Date = as.character(Date),
         Site = 'Kachemak Bay',
         SiteID = 'KABA',
         Year = Year) %>%
  group_by(Day.of.Year, Year) %>% 
  mutate(prop.W = WESA / max(1,WESA + LESA + SESA),
         WESA.2 = WESA + sm.peeps  ) %>% ungroup %>% #* prop.W
  rename(WESA.base = WESA,
         WESA = WESA.2) %>%
  dplyr::select(Site, SiteID, Date, Month, Day, Year, Day.of.Year, WESA, DUNL, prop.W) %>% 
  mutate_each(funs(as.character))


# KABA - Supplimentary Counts ---------------------------------------------
KB.Birders.ketch.supp <- read.csv('KABA_Supplimentary.csv', header = T, stringsAsFactors = F)
KB2.supp.clean <- 
  KB.Birders.ketch.supp %>%
  rename(WESA = Western.Sandpiper,
         LESA = Least.Sandpiper,
         SESA = Semipalmated.Sandpiper,
         DUNL = Dunlin,
         sm.peeps = LESA.WESA.SESA) %>%
  mutate(Month = ifelse(Month == 'April', '04', '05')) %>%
  mutate(Date = as.Date(paste(Year, '-', Month, "-", Day, sep = ""), format = "%Y-%m-%d"),
         Day.of.Year = as.integer(format(Date, format = "%j")),
         # prop.W = WESA / max(1,WESA + LESA + SESA),
         # WESA.2 = WESA + sm.peeps * prop.W,
         Site = 'Kachemak Bay',
         SiteID = 'KABA',
         Year = as.character(Year)) %>%
  group_by(Day.of.Year, Year) %>% 
  mutate(prop.W = WESA / max(1,WESA + LESA + SESA),
         WESA.2 = WESA + sm.peeps ) %>% ungroup %>% # * prop.W
  rename(WESA.base = WESA,
         WESA = WESA.2) %>%
  dplyr::select(Site, SiteID, Date, Month, Day, Year, Day.of.Year, WESA, DUNL, prop.W) %>%
  mutate_each(funs(as.character)) %>% filter(!(Day.of.Year == 132 & Year == 2014)) %>% 
  bind_rows(kbsupp2016) 

yrs <- unique(KB2.supp.clean$Year)
KB.supp.filtered <- KB2.supp.clean
for (i in yrs){
  days <- KB2.clean$Day.of.Year[which(KB2.clean$Year == i)] %>% unique
  # print(days)
  KB.supp.filtered <- filter(KB.supp.filtered, Year != i | !Day.of.Year %in% days)
  
}


KB2.w.supp <- bind_rows(KB2.clean, KB.supp.filtered)

######### Tofino ##############
# Available through Environment and Climate Change Canada
Tofino <- read.csv('Tofino.csv', header = T)
Tofino.clean <-
  Tofino %>% dplyr::select(Date, DOY, Year, Location, WESA, DUNL) %>%
  group_by(Date, DOY, Year) %>%
  dplyr::summarise_each(funs(mean)) %>%
  dplyr::select(-Location) %>%
  filter(!is.na(DOY)) %>%
  mutate( # Extract date information
    Month = format(as.Date(Date, format = "%d-%m-%Y"), format = "%m"),
    Day = format(as.Date(Date, format = "%d-%m-%Y"), format = "%d"),
    Site = 'Tofino',
    SiteID = 'TOFN') %>%
  rename(Day.of.Year = DOY) %>%
  ungroup() %>%
  mutate_each(funs(as.character)) 
  


######### Roberts Bank ##############
  # Available through Environment and Climate Change Canada
RBBP <- read_tsv('RBBP.csv') # Import

# Remove non-count days and add date variables
RBBP.clean <- 
  RBBP %>% filter(!is.na(count)) %>%
  dplyr::select(DATE, DUNL, WESA, Year, DOY ) %>%
  mutate( # Extract date information
         D = mdy(DATE),
         Month = month(D),#format(as.Date(DATE, format = "%d-%m-%Y"), format = "%m"),
         Day = day(D),#format(as.Date(DATE, format = "%d-%m-%Y"), format = "%d"),
         doy = yday(D),
         Site = 'Roberts Bank',
         SiteID = 'RBBP') %>%
  rename(Day.of.Year = DOY, Date = DATE) %>%
  mutate_each(funs(as.character))

qnt <-Hmisc::wtd.quantile(RBBP.clean$Day.of.Year, weights = as.numeric(RBBP.clean$WESA), probs = c(0.33,0.67))

# R2 <- 
# RBBP.clean %>% mutate(timing = ifelse(Day.of.Year < qnt[1],'early', ifelse(Day.of.Year< qnt[2], "late", 'mid') )) %>% 
#   group_by(timing, Year) %>% 
#   summarize(WESA = mean(WESA, na.rm=T))



######### Kennedy Creek  ##############
# Contact Joseph B. Buchanan for data access
KENN <- read.csv('KENN.csv', header = T, sep = '\t')

KENN.clean <- 
  KENN %>% dplyr::select(-BBPL) %>%
  mutate(Date = as.Date(paste(Year, '-', Month, "-", Day, sep = "")),
         Day.of.Year = as.integer(format(Date, format = "%j")),
         Site = 'Kennedy Creek',
         SiteID = 'KENN') %>%
  mutate_each(funs(as.character))


KENN2 <- read_csv("KENN_2013_2016.csv")
require(lubridate)
KENN2.clean <- 
  KENN2 %>% tidyr::separate(col = Date, into = c("Day", "MonthName"), sep = "-", remove = F) %>% 
    mutate(Month = ifelse(MonthName == "Mar", 3, ifelse(MonthName == "Apr", 4,5 )),
      Date.cln = as_date(paste0(Year, "-", Month,"-",  Day)),
      Day.of.Year = yday(Date.cln),
      Site = 'Kennedy Creek',
      SiteID = 'KENN') %>%
  mutate_each(funs(as.character))
  


######### Copper River Delta  ##############
# Original Data with Mary Ann's species predictions
# CRD <- read.csv('../DataReview/cleaned_data/CRD_Cleaned.txt', header = T, sep = '\t')
# CRD.clean <-
#   CRD %>% dplyr::select(-X) %>%
#   rename(Day.of.Year = Date) %>%
#   mutate(Date = format(as.Date(paste(Year, "-", Day.of.Year, sep = ""), format = "%Y-%j"), format = "%d-%m-%Y"),
#          Month = format(as.Date(paste(Year, "-", Day.of.Year, sep = ""), format = "%Y-%j"), format = "%m"),
#          Day = format(as.Date(paste(Year, "-", Day.of.Year, sep = ""), format = "%Y-%j"), format = "%d"),
#          Site = 'Copper River Delta',
#          SiteID = 'CRD',
#          Year = as.character(Year)) %>%
#   mutate_each(funs(as.character))

########### Running version with Linear interpolation
# Daily counts from linear interpolation described in CRD_species_composition.R
CRD <- read.csv('predicted DUNL and WESA.csv', header = T)
CRD.clean <-
  CRD %>% dplyr::select(Date, Totals,WESA, DUNL,  pred.WESA, pred.DUNL, Year ) %>%
  rename(Day.of.Year = Date) %>% 
  mutate(
        WESA = ifelse(is.na(WESA), pred.WESA, WESA),
        DUNL = ifelse(is.na(DUNL), pred.DUNL, DUNL),
        Date = format(as.Date(paste(Year, "-", Day.of.Year, sep = ""), format = "%Y-%j"), format = "%d-%m-%Y"),
         Month = format(as.Date(paste(Year, "-", Day.of.Year, sep = ""), format = "%Y-%j"), format = "%m"),
         Day = format(as.Date(paste(Year, "-", Day.of.Year, sep = ""), format = "%Y-%j"), format = "%d"),
         Site = 'Copper River Delta',
         SiteID = 'CRD',
         Year = as.character(Year)) %>% select(-pred.WESA, -pred.DUNL) %>% 
  mutate_each(funs(as.character))


############ Join the data together ####################3

spring.counts <-  
  CRD.clean %>% 
  bind_rows(west.clean) %>%
  bind_rows(KB2.clean) %>%
  bind_rows(RBBP.clean) %>%
  bind_rows(Tofino.clean) %>%
  bind_rows(KENN.clean) %>%
  bind_rows(KENN2.clean) %>% 
  mutate(Day.of.Year = as.numeric(Day.of.Year),
         Totals = as.numeric(Totals),
         WESA = as.numeric(WESA),
         DUNL= as.numeric(DUNL),
         Year= as.numeric(Year),
         Month= as.numeric(Month),
         Day= as.numeric(Day)
         ) %>%
    filter(as.numeric(Month) == 4 | Month ==5 )



spring.counts.w.supp <-  
  CRD.clean %>% 
  bind_rows(west.clean) %>%
  bind_rows(KB2.w.supp) %>%
  bind_rows(RBBP.clean) %>%
  bind_rows(Tofino.clean) %>%
  bind_rows(KENN.clean) %>%
  bind_rows(KENN2.clean) %>% 
  mutate(Day.of.Year = as.numeric(Day.of.Year),
         Totals = as.numeric(Totals),
         WESA = as.numeric(WESA),
         DUNL= as.numeric(DUNL),
         Year= as.numeric(Year),
         Month= as.numeric(Month),
         Day= as.numeric(Day)
  ) %>%
  filter(as.numeric(Month) == 4 | Month ==5 )


saveRDS(spring.counts, "./datafiles/spring.counts.rds")
saveRDS(spring.counts.w.supp, "./datafiles/spring.counts.w.supp.rds")