## Data from Hartney Bay surveys conducted by 
## Environment for the Americas, the U.S. Forest Service, and Point Blue Conservation Sciences
## Data available by contacting Point Blue Conservation Sciences

require(tidyverse)
require(lubridate)

suppDat <- read_csv("springdf2.csv") %>% 
  mutate(Date = as_date(paste0(YearCollected,"/", MonthCollected, "/", DayCollected )),
                        Day.of.Year = lubridate::yday(Date))

# Later data collected by Environment for the Americas. Linear interpolation of missing species
# composition done in HART_species_composition.R  
 CopperDat <- filter(suppDat, SamplingUnitId == 147066)  %>%# 147066 is Harney Bay  #SamplingUnitId %in% c(147066, 147067, 147068)) %>%
  mutate(Year = YearCollected, Month = MonthCollected, Day = DayCollected) %>% 
  filter(Day.of.Year >107) %>% 
  group_by(Year, Day, Month, Date, Day.of.Year, SamplingUnitId) %>% 
  summarize(
    n = n(),
    WESA = sum(WESA),
            LESA = sum(LESA),
            WLD = sum(XWLD),
            WL = sum(XWLS),
         DUNL = sum(DUNL)#,
          )%>% mutate(prop_WESA = WESA/ (WESA + DUNL),
            Site = "Hartney Bay", #paste0("Orca  - ", SamplingUnitId), 
                      SiteID = ifelse(SamplingUnitId == 147066, "HART" , SamplingUnitId)) %>% 
           ungroup %>% 
         # Site = 'Hartney Bay', SiteID = "HAR") %>% ungroup %>% 
  # dplyr::select(Date, Day.of.Year, Year, Month, Day, WESA, DUNL, Site, SiteID, SamplingUnitId) %>% 
  mutate(Date = as.character(Date), Year.factor = as.factor(Year),
         DayYr = arm::rescale(Day.of.Year))


### Early Hartney Bay Data
# Available by contacting Mary Anne Bishop  

require(readxl)
require(lubridate)
Hart1991 <- read_xls("../../DataReview/MasterFiles/dhope hartney 1991 high tide data.xls", sheet = "TRANSECT DATA") %>% 
  mutate(Day.of.Year = yday(DATE)) %>% group_by(DATE, Day.of.Year) %>% summarise(WESA = sum(WESA), DUNL = sum(DUNL)) %>% 
  ungroup %>% 
  mutate(Year = year(DATE),
         Month = month(DATE),
         Day = day(DATE)) %>% rename(Date = DATE) %>% filter(!is.na(Year))
  

Hart1992 <- read_xls("../../DataReview/MasterFiles/dhope hartney bay 1992.xls", sheet = "Transect data") %>% 
  mutate(Day.of.Year = yday(Date)) %>% group_by(Date, Day.of.Year, `Spp#`) %>% summarise(Birds = sum(No.)) %>% ungroup %>% 
  spread(key = `Spp#`,  Birds, fill = 0) %>% dplyr::select(Date, Day.of.Year, WESA, DUNL) %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date))%>% filter(!is.na(Year))

Hart1993 <- read_xlsx("../../DataReview/MasterFiles/dhope 1993 hartney.xlsx", sheet = "hartney transects 1993") %>% 
  mutate(Day.of.Year = yday(Date)) %>% group_by(Date, Day.of.Year, `Spp`) %>% summarise(Birds = sum(No)) %>% ungroup %>% 
  spread(key = `Spp`,  Birds, fill = 0) %>% dplyr::select(Date, Day.of.Year, WESA, DUNL) %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date))%>% filter(!is.na(Year))

Hart_early <- bind_rows(Hart1991, Hart1992) %>% bind_rows(Hart1993) %>% mutate(Site = 'Hartney Bay', SiteID = "HART") %>% 
  mutate(Date = as.character(Date))

saveRDS(Hart_early, ".data/Hart_early.rds")

# Hart_all <- bind_rows(Hart_early, CopperDat)