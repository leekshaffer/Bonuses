library(tidyverse)
library(readxl)
library(baseballr) ## used to map across IDs
library(googlesheets4) ## used to import contracts and service time information

## Import ID-matching info:
Crosswalk <- chadwick_player_lu()
rem.acc.pd.suff <- function(string) {
  string <- chartr("áÁéÉëíÍñóÓúÚ","aAeEeiInoOuU",str_squish(gsub("[.]"," ",string)))
  string <- gsub(" Jr","",gsub(" II","",gsub(" III","",string)))
  string <- gsub("-","",gsub(" ","",string))
  return(string)
}
Cross_use <- Crosswalk %>% filter(mlb_played_first > 1975) %>%
  mutate(first=rem.acc.pd.suff(name_first),
         last=rem.acc.pd.suff(name_last),
         full=str_squish(paste(name_first,name_last,name_suffix,sep=" ")))


for (yr in 22:23) {
  ## Read in bonus data:
  Bonus <- read_excel("data/PreArbBonus.xlsx",
                      sheet = paste0("BonusesAdj",yr)) %>% 
    mutate(BonusW=Bonus-Set_Bonus,
           Share=BonusW/sum(BonusW),
           Season=as.numeric(paste0("20",yr))) %>%
    dplyr::select(-c(Notes,Set_Bonus))
  
  ## Read in stats data:
  BRp <- read_excel("data/BRef.xlsx",
                    sheet = paste0("BRp",yr)) %>%
    dplyr::select(`Player-additional`,IP,RA9,RAA,WAA,WAAadj,RAR,WAR) %>%
    rename(BREFID=`Player-additional`,bWAR.p=WAR,
           RAA.p=RAA,WAA.p=WAA,WAAadj.p=WAAadj,RAR.p=RAR) %>%
    mutate(B.p=if_else(IP >= 10,1,0))
  BRb <- read_excel("data/BRef.xlsx",
                    sheet = paste0("BRb",yr)) %>%
    dplyr::select(`Player-additional`,PA,RAA,WAA,RAR,oWAR,dWAR,oRAR,WAR) %>%
    rename(BREFID=`Player-additional`,bWAR.b=WAR,
           RAA.b=RAA,WAA.b=WAA,RAR.b=RAR) %>%
    mutate(B.b=if_else(PA >= 10,1,0))
  FGp <- read_csv(file="data/FGp.csv") %>% 
    dplyr::filter(Season==as.numeric(paste0("20",yr))) %>%
    dplyr::select(MLBAMID,ERA,xERA,FIP,xFIP,WAR) %>%
    rename(fWAR.p=WAR)
  FGb <- read_csv(file="data/FGb.csv") %>% 
    dplyr::filter(Season==as.numeric(paste0("20",yr))) %>%
    dplyr::select(MLBAMID,OBP,Off,Def,WAR) %>%
    rename(fWAR.b=WAR)
  BPp <- read_excel("data/BP.xlsx",
                    sheet = paste0("BPp",yr)) %>%
    dplyr::select(mlbid,`DRA-`,DRA,cFIP,WHIP,RA9,WARP) %>%
    rename(MLBAMID=mlbid,RA9.bp=RA9,WARP.p=WARP)
  BPb <- read_excel("data/BP.xlsx",
                    sheet = paste0("BPb",yr)) %>%
    dplyr::select(mlbid,`DRC+`,WARP) %>%
    rename(MLBAMID=mlbid,WARP.b=WARP)
  
  ## Combine data:
  Base_info <- Bonus %>% 
    left_join(Cross_use %>% 
                dplyr::select(name_first,name_last,
                              key_mlbam,key_bbref,key_fangraphs) %>%
                rename(MLBAMID=key_mlbam,BREFID=key_bbref,FGID=key_fangraphs),
              by="BREFID")
  Data <- Base_info %>%
    left_join(BRp %>% dplyr::select(BREFID,B.p,IP,RA9,RAA.p,WAA.p,WAAadj.p,RAR.p,bWAR.p),
              by="BREFID") %>%
    left_join(BRb %>% dplyr::select(BREFID,B.b,PA,RAA.b,WAA.b,RAR.b,oWAR,dWAR,oRAR,bWAR.b),
              by="BREFID") %>%
    left_join(FGp %>% dplyr::select(MLBAMID,ERA,xERA,FIP,xFIP,fWAR.p),
              by="MLBAMID") %>%
    left_join(FGb %>% dplyr::select(MLBAMID,OBP,Off,Def,fWAR.b),
              by="MLBAMID") %>%
    left_join(BPp %>% dplyr::select(MLBAMID,`DRA-`,DRA,cFIP,WHIP,RA9.bp,WARP.p),
              by="MLBAMID") %>%
    left_join(BPb %>% dplyr::select(MLBAMID,`DRC+`,WARP.b),
              by="MLBAMID")
  
  ## Change NAs to 0s and compute total WAR values:
  Stat_Names <- colnames(Data)[(dim(Base_info)[2]+1):(dim(Data)[2])]
  NAVals <- as.list(rep(0,((dim(Data)[2]) - (dim(Base_info)[2]))))
  names(NAVals) <- Stat_Names
  Data_full <- Data %>% replace_na(NAVals) %>%
    mutate(bWAR.t=bWAR.b+bWAR.p,
           fWAR.t=fWAR.b+fWAR.p,
           WARP.t=WARP.b+WARP.p)
  
  ## Get subsets with positive stat-based bonus & exact bonus
  assign(x=paste0("Data_",yr,"_Full"),
         value=Data_full)
  savelist <- c(paste0("Data_",yr,"_Full"))
  if (mean(Data_full$Exact) < 1) {
    assign(x=paste0("Data_",yr,"_Ex"),
           value=Data_full %>% dplyr::filter(Exact))
    savelist <- c(savelist, paste0("Data_",yr,"_Ex"))
  }
  if (dim(Data_full %>% filter(Share <= 0))[1] > 0) {
    assign(x=paste0("Data_",yr,"_Pos"),
           value=Data_full %>% dplyr::filter(Share > 0))
    savelist <- c(savelist, paste0("Data_",yr,"_Pos"))
  }
  
  ## Save results:
  save(list=savelist,
       file=paste0("int/Data_",yr,".Rda"))
}


### Service time and contract information import:
#### Note season is the year that has that salary and starts with that much service time

cols1 <- "ccdd---"
names1 <- c("Player","Position","Service","Salary")
cols2 <- "ccd"
names2 <- c("Player","Position","Service")
Sizes <- NULL
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
           dplyr::filter(!is.na(Service) & !is.na(Salary)) %>% 
           separate_wider_delim(cols="Player", names=c("last","first"), delim=",", 
                                too_many="merge", too_few="align_start") %>% 
           mutate(first=rem.acc.pd.suff(first),
                  last=rem.acc.pd.suff(last))
  Sal_fix <- import1 %>% group_by(last,first,original) %>% filter(n() > 1) %>% ungroup()
  Sal_keep <- import1 %>% group_by(last,first,original) %>% filter(n()==1) %>% ungroup() %>%
    left_join(Cross_use %>% dplyr::select(key_bbref,key_mlbam,last,first,full), 
                        by=c("first","last"))
  Sal_fix <- Sal_fix %>% bind_rows(Sal_keep %>% group_by(last,first,original) %>% 
                                     dplyr::filter(n() > 1 | is.na(key_bbref)) %>% ungroup() %>%
                                     dplyr::select(last,first,Position,Service,Salary,Season,original) %>%
                                     distinct())
  Sal_keep <- Sal_keep %>% group_by(last,first,original) %>%
    dplyr::filter(n()==1 & !is.na(key_bbref)) %>% ungroup()
  write_csv(x=Sal_fix %>% arrange(original),
            file=paste0("int_sal_svc/Sal_fix_",yr,".csv"))
  save(Sal_keep,
       file=paste0("int_sal_svc/Sal_keep_",yr,".Rda"))
  
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
    mutate(first=rem.acc.pd.suff(first),
           last=rem.acc.pd.suff(last))
  Svc_fix <- import2 %>% group_by(last,first,original) %>% filter(n() > 1) %>% ungroup()
  Svc_keep <- import2 %>% group_by(last,first,original) %>% filter(n()==1) %>% ungroup() %>%
    left_join(Cross_use %>% dplyr::select(key_bbref,key_mlbam,last,first,full), 
              by=c("first","last"))
  Svc_fix <- Svc_fix %>% bind_rows(Svc_keep %>% group_by(last,first,original) %>% 
                                     dplyr::filter(n() > 1 | is.na(key_bbref)) %>% ungroup() %>%
                                     dplyr::select(last,first,Position,Service,Season,original) %>%
                                     distinct())
  Svc_keep <- Svc_keep %>% group_by(last,first,original) %>%
    dplyr::filter(n()==1 & !is.na(key_bbref)) %>% ungroup()
  write_csv(x=Svc_fix %>% arrange(original),
            file=paste0("int_sal_svc/Svc_fix_",yr,".csv"))
  save(Svc_keep,
       file=paste0("int_sal_svc/Svc_keep_",yr,".csv"))
  
  Sizes <- Sizes %>% bind_rows(tibble(Season=yr,
                                      I1.size=dim(import1)[1],
                                      I1.check=dim(Sal_fix)[1]+dim(Sal_keep)[1],
                                      I1.fix=dim(Sal_fix)[1],
                                      I2.size=dim(import2)[1],
                                      I2.check=dim(Svc_fix)[1]+dim(Svc_keep)[1],
                                      I2.fix=dim(Svc_fix)[1]))
}
