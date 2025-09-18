require(dplyr)
require(tidyr)
require(stringr)
require(readr)
require(rvest)
require(janitor)

## load Crosswalk data
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

## Function to pull in a year of Spotrac data
### based on code from baseballr function scrape_team_active_payroll
### at https://billpetti.github.io/baseballr/reference/sptrc_team_active_payroll.html
get_spotrac <- function(year) {
  ## Pre-Arb:
  PAU <- paste0("https://www.spotrac.com/mlb/pre-arbitration/_/year/",year)
  PAL <- rvest::read_html(PAU) %>% rvest::html_elements("table")
  PreArb <- PAL[[1]] %>%
    rvest::html_table() %>% 
    janitor::clean_names() %>%
    mutate(status="Pre-Arb",
           year=year,
           value=as.numeric(str_replace_all(value, "[$.,]", ""))) %>%
    rename_with(.fn=~str_extract(.x, "[^_]+"), .cols=starts_with("player")) %>%
    dplyr::select(year,player,status,pos,team,age,yos,value)
  
  ## For any duplicate rows, keep the max yos and value:
  dupes <- PreArb %>% 
    semi_join((PreArb %>% dplyr::select(year,player,pos,age))[duplicated(PreArb %>% 
                                                                                dplyr::select(year,player,pos,age)),],
              by=join_by(year,player,pos,age))
  dupe_pl <- unique(dupes$player)
  dupes_keep <- NULL
  for (pl in dupe_pl) {
    dupes_keep <- dupes_keep %>% 
      bind_rows(dupes %>% filter(player==pl) %>%
                  slice_max(order_by=tibble(value,yos), n=1, with_ties=FALSE))
  }
  
  PreArb <- PreArb %>% anti_join(dupes) %>%
    bind_rows(dupes_keep)
  
  
  ## Arbitration-Eligible:
  AU <- paste0("https://www.spotrac.com/mlb/arbitration/_/year/",year)
  AL <- rvest::read_html(AU) %>% rvest::html_elements("table")
  Arb <- AL[[1]] %>%
    rvest::html_table() %>% 
    janitor::clean_names() %>%
    mutate(status="Arb-Eligible",
           year=year,
           value=as.numeric(str_replace_all(value, "[$.,]", ""))) %>%
    rename_with(.fn=~str_extract(.x, "[^_]+"), .cols=starts_with("player")) %>%
    rename("war_prev"=ends_with("war")) %>%
    dplyr::select(year,player,status,pos,team,age,yos,value,arb_level,type,war_prev)
  
  dupes <- Arb %>% 
    semi_join((Arb %>% dplyr::select(year,player,pos,age))[duplicated(Arb %>% 
                                                                             dplyr::select(year,player,pos,age)),],
              by=join_by(year,player,pos,age))
  
  ## For any duplicate rows, first drop those of type Club or Mutual if there is
  ### another type. Then, if still duplicate, keep max of yos or age
  dupe_pl <- unique(dupes$player)
  dupes_keep <- NULL
  for (pl in dupe_pl) {
    dupe_ind <- dupes %>% filter(player==pl) %>% 
      mutate(keep = !(type %in% c("Club","Mutual","Buried")))
    if (all(!dupe_ind$keep)) {
      dupes_keep <- dupes_keep %>% 
        bind_rows(dupe_ind %>% 
                    slice_max(order_by=tibble(value,yos), 
                              n=1, with_ties=FALSE))
    } else {
      dupes_keep <- dupes_keep %>% 
        bind_rows(dupe_ind %>% 
                    dplyr::filter(keep) %>%
                    dplyr::select(-c("keep")) %>%
                    slice_max(order_by=tibble(value,yos), 
                              n=1, with_ties=FALSE))
    }
  }
  
  Arb <- Arb %>% anti_join(dupes) %>%
    bind_rows(dupes_keep)
  
  return(list(PreArb=PreArb, Arb=Arb))
}

## Function for pre-arbitration extensions:
get_spotrac_ext <- function() {
  ExtL <- rvest::read_html("https://www.spotrac.com/mlb/pre-arbitration/extensions") %>% 
    rvest::html_elements("table")
  PAExt <- ExtL[[1]] %>%
    rvest::html_table() %>% 
    janitor::clean_names() %>%
    mutate(status="Extension",
           year_last=year+length-1) %>%
    mutate(across(.cols=c("value","average","signing_bonus","x2_yr_cash","x3_yr_cash"),
                  .fn=~as.numeric(str_replace_all(.x, "[$.,]","")))) %>%
    rename_with(.fn=~str_extract(.x, "[^_]+"), .cols=starts_with("player")) %>%
    rename(year_sign=year) %>%
    dplyr::select(player,status,pos,team,age_sign,year_sign,year_last,
                  length,type,everything())
  return(PAExt)
}

PreArbDB <- NULL
ArbDB <- NULL
years <- 2021:2025
for (year in years) {
  yrlist <- get_spotrac(year)
  PreArbDB <- rbind(PreArbDB,yrlist$PreArb)
  ArbDB <- rbind(ArbDB,yrlist$Arb)
}
ExtDB <- get_spotrac_ext() %>%
  cross_join(tibble(year=years)) %>%
  dplyr::filter(year >= year_sign & year <= year_last) %>%
  mutate(age_low=year-year_sign+age_sign,
         age_high=year-year_sign+age_sign+1,
         year_cont=year-year_sign+1,
         status=paste0("Extension-",year_cont)) %>%
  rename(pos_hold=team) %>%
  rename(team=pos) %>%
  rename(pos=pos_hold) %>%
  dplyr::select(year,player,status,pos,team,age_low,age_high,year_cont,everything())

CombDB <- PreArbDB %>% 
  bind_rows(ArbDB %>% 
              dplyr::select(all_of(colnames(PreArbDB)))) %>%
  mutate(age_low=age, age_high=age) %>%
  bind_rows(ExtDB %>%
              mutate(yos=NA_real_,
                     value=average) %>%
              dplyr::select(any_of(c(colnames(PreArbDB),"age_low","age_high"))))

save(list=c("PreArbDB","ArbDB","ExtDB","CombDB"),
     file="int/PreFADBs.Rda")

Cross_use_DB <- Cross_use %>% dplyr::filter(birth_year >= 1980) %>%
  dplyr::mutate(pl_simp=rem.full(full),
                age_yr=if_else(birth_month < 7, birth_year,
                               if_else(birth_month == 7 & birth_day == 1, 
                                       birth_year, birth_year+1))) %>%
  cross_join(tibble(year=years)) %>%
  dplyr::mutate(age=year-age_yr) %>%
  dplyr::select(year,pl_simp,age,full,key_person,key_mlbam,key_bbref,
                key_bbref_minors,key_fangraphs,mlb_played_first)

CombDB2 <- CombDB %>% mutate(pl_simp=rem.full(player)) %>% 
  left_join(Cross_use_DB,
            by=join_by(year==year,
                       pl_simp==pl_simp,
                       age_low<=age,
                       age_high>=age)) %>%
  mutate(age=age.y) %>% 
  dplyr::select(-c("age.x","age.y","age_low","age_high"))

dupes <- CombDB2 %>% 
  semi_join((CombDB2 %>% dplyr::select(year,player,status,pos,team,age))[duplicated(CombDB2 %>% dplyr::select(year,player,status,pos,team,age)),],
            by=join_by(year,player,status,pos,team,age)) %>%
  arrange(player,year)

missings <- CombDB2 %>% 
  dplyr::filter((is.na(key_bbref_minors) | key_bbref_minors=="") & 
                  (is.na(key_bbref) | key_bbref=="")) %>%
  arrange(player,year) %>%
  dplyr::select(year,player,status,pos,team,age,yos,value,pl_simp)

write_csv(x=dupes,
          file="int_sal_svc/spotrac_dupes.csv")
write_csv(x=missings,
          file="int_sal_svc/spotrac_missings.csv")

DF <- read_csv(file="int_sal_svc/spotrac_dupes_fix.csv") %>%
  dplyr::filter(use==1) %>%
  dplyr::select(-c("use"))
MF <- read_csv(file="int_sal_svc/spotrac_missings_fix.csv")

MF_full <- MF %>% dplyr::filter(is.na(key_bbref_minors)) %>%
  dplyr::select(-c("key_bbref_minors","age","pl_simp")) %>%
  left_join(Cross_use_DB) %>%
  bind_rows(MF %>% dplyr::filter(is.na(key_bbref)) %>%
              dplyr::select(-c("key_bbref","age","pl_simp")) %>%
              left_join(Cross_use_DB))

FullDB <- CombDB2 %>% 
  anti_join(dupes) %>%
  anti_join(missings) %>%
  bind_rows(DF,MF_full) %>%
  mutate(stat_det=status,
         status=if_else(substr(status,1,9)=="Extension",
                        "Extension",status))

## Improving Extension player info:
load(file="int/Svc_Sal.Rda")
STwo <- read_csv(file="data/SuperTwo.csv") %>%
  mutate(Season=Season+1)
  
PAE_players <- unique(FullDB %>% filter(status=="Extension") %>% 
                        pull(key_bbref))

Service_Cat_Fn <- function(yos,yos_lag,Threshold) {
  if (is.na(yos)) {
    if (is.na(yos_lag)) {
      return("Unknown")
    } else {
      if (yos_lag >= 3) {
        return("Arb+")
      } else if (yos_lag <= Threshold-1) {
        return("PreArb")
      } else {
        return("Possible Pre/Arb")
      }
    }
  } else {
    if (yos >= 3) {
      return("Arb+")
    } else if (yos <= Threshold) {
      return("PreArb")
    } else {
      if (is.na(yos_lag)) {
        return("Possible Pre/Arb")
      } else {
        diffl <- 172*floor(yos)+1000*(yos-floor(yos)) - 
          (172*floor(yos_lag)+1000*(yos_lag-floor(yos_lag)))
        if (diffl >= 86) {
          return("SuperTwo")
        } else {
          return("PreArb")
        }
      }
    }
  }
}

SCFV <- Vectorize(Service_Cat_Fn)

FullDB_dupes <- FullDB[duplicated(FullDB %>% select(year,key_mlbam)),] %>% select(year,key_mlbam)
FullDB_dupeRows <- FullDB %>% semi_join(FullDB_dupes) %>%
  group_by(year,player,pl_simp,full,key_person,key_mlbam,key_bbref,key_bbref_minors,key_fangraphs,mlb_played_first) %>%
  dplyr::summarize(status=if_else("Extension" %in% status,"Extension","Pre-Arb"),
                   yos=max(yos, na.rm=TRUE),
                   value=max(value, na.rm=TRUE)) %>%
  dplyr::mutate(pos="", team="", stat_det="")

FullDB <- FullDB %>% anti_join(FullDB_dupes) %>%
  bind_rows(FullDB_dupeRows)

FDB_e <- FullDB %>% 
  filter(key_bbref %in% PAE_players) %>%
  arrange(player,year) %>%
  left_join(Svc_Sal %>% dplyr::select(Season,key_bbref,key_mlbam,Service),
            by=join_by(year==Season,
                       key_bbref,
                       key_mlbam)) %>%
  left_join(STwo, by=join_by(year==Season)) %>%
  mutate(yos=if_else(is.na(yos),Service,yos)) %>%
  dplyr::select(-Service) %>%
  group_by(player,key_bbref) %>%
  mutate(yos_lag=lag(yos, default=NA_real_)) %>%
  ungroup() %>%
  mutate(Svc_Cat=SCFV(yos,yos_lag,Threshold))

FDB <- FDB_e %>% 
  bind_rows(FullDB %>% dplyr::filter(!(key_bbref %in% PAE_players))) %>%
  mutate(Svc_Cat=if_else(is.na(Svc_Cat),
                         if_else(status=="Arb-Eligible","Arb+",
                                 "PreArb"),Svc_Cat))

save(FDB, file="int/FDB.Rda")

