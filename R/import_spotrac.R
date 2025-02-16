require(dplyr)
require(tidyr)
require(stringr)
require(readr)
require(rvest)

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
                    slice_max(order_by=tibble(value,yos), 
                              n=1, with_ties=FALSE))
    }
  }
  
  Arb <- Arb %>% anti_join(dupes) %>%
    bind_rows(dupes_keep)
  
  return(list(PreArb=PreArb, Arb=Arb))
}

PreArbDB <- NULL
ArbDB <- NULL
years <- 2021:2025
for (year in years) {
  yrlist <- get_spotrac(year)
  PreArbDB <- rbind(PreArbDB,yrlist$PreArb)
  ArbDB <- rbind(ArbDB,yrlist$Arb)
}

CombDB <- PreArbDB %>% 
  bind_rows(ArbDB %>% 
              dplyr::select(all_of(colnames(PreArbDB))))

save(list=c("PreArbDB","ArbDB","CombDB"),
     file="int/PreFADBs.Rda")

Cross_use_DB <- Cross_use %>%
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
            by=c("year","pl_simp","age"))

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
  dplyr::select(-c("key_bbref_minors","age")) %>%
  left_join(Cross_use_DB) %>%
  bind_rows(MF %>% dplyr::filter(is.na(key_bbref)) %>%
              dplyr::select(-c("key_bbref","age")) %>%
              left_join(Cross_use_DB))

FullDB <- CombDB2 %>% 
  anti_join(dupes) %>%
  anti_join(missings) %>%
  bind_rows(DF,MF_full)

save(FullDB, file="int/PreFAFullDB.Rda")
