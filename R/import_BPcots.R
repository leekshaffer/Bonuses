require(dplyr)
require(tidyr)
require(readr)
require(googlesheets4) ## used to import contracts and service time information

load(file="int/Crosswalk.Rda")

## name helper functions
rem.acc.pd <- function(string) {
  string <- chartr("áÁéÉëËíÍñóÓúÚüÜ","aAeEeEiInoOuUuU",
                   gsub("[.-]","",string))
  return(string)
}
rem.space <- function(string) {
  return(gsub("[[:blank:]]","",string))
}
rem.acc.pd.suff <- function(string) {
  string <- rem.acc.pd(string)
  string <- gsub(" Jr","",gsub(" II","",gsub(" III","",string)))
  return(string)
}
rem.full <- function(string) {
  return(tolower(rem.space(rem.acc.pd.suff(string))))
}

CU <- Cross_use %>% 
  dplyr::filter(mlb_played_first > 1975) %>%
  mutate(first=rem.full(name_first),
         last=rem.full(name_last))

### Service time and contract information import:
#### Note season is the year that has that salary and starts with that much service time

cols1 <- "ccdd---"
names1 <- c("Player","Position","Service","Salary")
cols2 <- "ccd"
names2 <- c("Player","Position","Service")
Fix_Sal <- NULL
Fix_Svc <- NULL
for (yr in 2010:2024) {
  print(paste0("Starting year ",yr))
  import1 <- range_read(ss="https://docs.google.com/spreadsheets/d/12XSXOQpjDJDCJKsA4xC1e_9FlS11aeioZy_p1nqpclg/edit?gid=1937251654#gid=1937251654",
                        sheet=paste0(as.character(yr),".xls"),
                        col_types=cols1,
                        col_names=names1,
                        skip=2,
                        trim_ws=TRUE) %>%
    mutate(Season=yr,
           original=Player) %>%
    dplyr::filter(!is.na(Service) | !is.na(Salary)) %>% 
    separate_wider_delim(cols="Player", names=c("last","first"), delim=",", 
                         too_many="merge", too_few="align_start") %>% 
    mutate(first=rem.full(first),
           last=rem.full(last))
  Sal_fix <- import1 %>% group_by(last,first,original) %>% filter(n() > 1) %>% ungroup()
  Sal_keep <- import1 %>% group_by(last,first,original) %>% filter(n()==1) %>% ungroup() %>%
    left_join(CU %>% 
                dplyr::select(key_bbref,key_mlbam,last,first,full), 
              by=c("first","last"))
  Sal_fix <- Sal_fix %>% bind_rows(Sal_keep %>% group_by(last,first,original) %>% 
                                     dplyr::filter(n() > 1 | is.na(key_bbref) | key_bbref=="") %>% ungroup() %>%
                                     dplyr::select(last,first,Position,Service,Salary,Season,original) %>%
                                     distinct())
  Sal_keep <- Sal_keep %>% group_by(last,first,original) %>%
    dplyr::filter(n()==1 & !is.na(key_bbref) & key_bbref != "") %>% ungroup()
  save(Sal_keep,
       file=paste0("int_sal_svc/Sal_keep_",yr,".Rda"))
  Fix_Sal <- Fix_Sal %>% bind_rows(Sal_fix)
}

for (yr in 2010:2025) { 
  import2 <- range_read(ss="https://docs.google.com/spreadsheets/d/1m9ap5cOX3j4ZYnmceOZ0oK8GtLg5YGesNsxMZb6GFIs/edit?gid=435379795#gid=435379795",
                        sheet=paste0("MLS thru ",as.character(yr-1),".xls"),
                        col_types=cols2,
                        col_names=names2,
                        skip=1,
                        trim_ws=TRUE) %>%
    mutate(Season=yr, original=Player) %>%
    dplyr::filter(!is.na(Service)) %>% 
    separate_wider_delim(cols="Player", names=c("last","first"), delim=",", 
                         too_many="merge", too_few="align_start") %>% 
    mutate(first=rem.full(first),
           last=rem.full(last))
  Svc_fix <- import2 %>% group_by(last,first,original) %>% filter(n() > 1) %>% ungroup()
  Svc_keep <- import2 %>% group_by(last,first,original) %>% filter(n()==1) %>% ungroup() %>%
    left_join(CU %>% 
                dplyr::select(key_bbref,key_mlbam,last,first,full), 
              by=c("first","last"))
  Svc_fix <- Svc_fix %>% bind_rows(Svc_keep %>% group_by(last,first,original) %>% 
                                     dplyr::filter(n() > 1 | is.na(key_bbref) | key_bbref=="") %>% ungroup() %>%
                                     dplyr::select(last,first,Position,Service,Season,original) %>%
                                     distinct())
  Svc_keep <- Svc_keep %>% group_by(last,first,original) %>%
    dplyr::filter(n()==1 & !is.na(key_bbref) & key_bbref != "") %>% ungroup()
  save(Svc_keep,
       file=paste0("int_sal_svc/Svc_keep_",yr,".Rda"))
  Fix_Svc <- Fix_Svc %>% bind_rows(Svc_fix)
}

write_csv(x=Fix_Sal %>% arrange(original),
          file="int_sal_svc/Sal_fix_full.csv")
write_csv(x=Fix_Svc %>% arrange(original),
          file="int_sal_svc/Svc_fix_full.csv")


#### Manually fix Fix_ spreadsheets and save as _fix_res.csv:

Salaries <- read_csv(file="int_sal_svc/Sal_fix_res.csv") %>% dplyr::select(-c(last,first)) %>%
  left_join(CU %>% dplyr::select(key_bbref,key_mlbam,last,first,full), 
            by="key_bbref") %>% 
  dplyr::select(full,last,first,original,key_bbref,key_mlbam,Season,Position,Service,Salary)
SvcTimes <- read_csv(file="int_sal_svc/Svc_fix_res.csv") %>% dplyr::select(-c(last,first)) %>%
  left_join(CU %>% dplyr::select(key_bbref,key_mlbam,last,first,full), 
            by="key_bbref") %>% 
  dplyr::select(full,last,first,original,key_bbref,key_mlbam,Season,Position,Service)
for (yr in 2010:2024) {
  load(file=paste0("int_sal_svc/Sal_keep_",yr,".Rda"))
  load(file=paste0("int_sal_svc/Svc_keep_",yr,".Rda"))
  Salaries <- Salaries %>% bind_rows(Sal_keep)
  SvcTimes <- SvcTimes %>% bind_rows(Svc_keep)
}

Svc_Sal <- SvcTimes %>% full_join(Salaries, by=c("Season","key_bbref")) %>%
  mutate(full=if_else(is.na(full.x),full.y,full.x),
         last=if_else(is.na(last.x),last.y,last.x),
         first=if_else(is.na(first.x),first.y,first.x),
         original=if_else(is.na(original.x),original.y,original.x),
         key_mlbam=if_else(is.na(key_mlbam.x),key_mlbam.y,key_mlbam.x),
         Position=if_else(is.na(Position.x),Position.y,Position.y),
         Service=if_else(is.na(Service.x),Service.y,
                         if_else(Service.x==0 & Service.y > 0,Service.y,Service.x))) %>%
  dplyr::select(full,last,first,key_bbref,key_mlbam,Season,Position,Service,Salary)


## Arb/Pre-Arb/Super 2 Designations:
Super2 <- read_csv(file="data/SuperTwo.csv")
SSlag <- Svc_Sal %>% select(key_bbref,Season,Service,Salary) %>%
  mutate(Season=Season+1) %>%
  rename(LagService=Service,
         LagSalary=Salary)
Svc_Sal <- Svc_Sal %>% left_join(SSlag, by=c("key_bbref","Season")) %>%
  left_join(Super2 %>% mutate(Season=Season+1)) %>%
  dplyr::mutate(Svc_diffl=if_else(is.na(Service) | is.na(LagService), NA,
                                  if_else(floor(Service)==floor(LagService),
                                          (Service-LagService)*100,
                                          172*(floor(Service)-floor(LagService))+
                                            100*(Service-floor(Service)-
                                                   (LagService-floor(LagService))))),
                Svc_cat=if_else(is.na(Service),"Unkown",
                                if_else(Service >= 3,"3+",
                                        if_else(Service < Threshold,"UnderS2",
                                                if_else(is.na(Svc_diffl),"Possible S2",
                                                        if_else(Svc_diffl >= 86,"SuperTwo","UnderS2"))))))

save(Svc_Sal,
     file="int/Svc_Sal.Rda")
save(Svc_Sal,
     file="int_sal_svc/Svc_Sal.Rda")



## Check that this data captures the bonus players:
for (yr in 22:24) {
  load(file=paste0("Data_",yr,".Rda"))
}

B23Check <- Data_23_Full %>%
  left_join(Svc_Sal %>% dplyr::filter(Season==2023) %>% 
              dplyr::select(key_bbref,Svc_cat),
            by=join_by(BREFID==key_bbref))
B23Check %>% dplyr::select(Name,Bonus,Svc_cat,BREFID) %>% 
  filter(is.na(Svc_cat))
Svc_Sal %>% filter(key_bbref %in% (B23Check %>% 
                                     dplyr::select(Name,Bonus,Svc_cat,BREFID) %>% 
                                     filter(is.na(Svc_cat)) %>% pull(BREFID)))
#### 11 NAs, all of whom appear in salary data set first in 2024
#### All others are in "UnderS2" category

B22Check <- Data_22_Full %>%
  left_join(Svc_Sal %>% dplyr::filter(Season==2022) %>% 
              dplyr::select(key_bbref,Svc_cat),
            by=join_by(BREFID==key_bbref))
B22Check %>% dplyr::select(Name,Bonus,Svc_cat,BREFID) %>% 
  filter(is.na(Svc_cat))
Svc_Sal %>% filter(key_bbref %in% (B22Check %>% 
                                     dplyr::select(Name,Bonus,Svc_cat,BREFID) %>% 
                                     filter(is.na(Svc_cat)) %>% pull(BREFID)))
#### 2 NAs, both of whom appear in salary data set first in 2023
#### All others are in "UnderS2" category

B24Check <- Data_24_Full %>%
  left_join(Svc_Sal %>% dplyr::filter(Season==2024) %>% 
              dplyr::select(key_bbref,Svc_cat),
            by=join_by(BREFID==key_bbref))
B24Check %>% dplyr::select(Name,Bonus,Svc_cat,BREFID) %>% 
  filter(is.na(Svc_cat))
Svc_Sal %>% filter(key_bbref %in% (B23Check %>% 
                                     dplyr::select(Name,Bonus,Svc_cat,BREFID) %>% 
                                     filter(is.na(Svc_cat)) %>% pull(BREFID)))
#### 3 NAs, all of whom appear were not on 2024 opening day roster (I think)
#### All others are in "UnderS2" category




