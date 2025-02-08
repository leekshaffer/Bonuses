require(tidyverse)
require(readxl)

## Import crosswalk data, add some fixes:
load(file="int/Crosswalk.Rda")
rem.acc.pd.suff <- function(string) {
  string <- chartr("áÁéÉëËíÍñóÓúÚüÜ","aAeEeEiInoOuUuU",str_squish(gsub("[.]"," ",string)))
  string <- gsub(" Jr","",gsub(" II","",gsub(" III","",string)))
  string <- gsub("-","",gsub(" ","",string))
  return(string)
}
Cross_use[Cross_use$key_bbref=="skenepa01","key_fangraphs"] <- 33677
Cross_use[Cross_use$key_bbref=="schwesp01","key_fangraphs"] <- 31846
Cross_use[Cross_use$key_bbref=="myersto01","key_fangraphs"] <- 22191


for (yr in 22:24) {
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


### Service time and salary import and SuperTwo calculations:
#### Note season is the year that has that salary and starts with that much service time

load(file="int/Svc_Sal.Rda")
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


## See if Bonus folks are caught in Under S2 classification:
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



