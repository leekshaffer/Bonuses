require(dplyr)
require(tidyr)
require(readr)
require(readxl)

yrs <- 22:24

## load crosswalk:
load(file="int/Crosswalk.Rda")
CU <- Cross_use %>% dplyr::select(key_person,
                                  key_mlbam,key_bbref,key_bbref_minors,
                                  key_fangraphs,
                                  full)

FullStats <- NULL

for (yr in 22:24) {
  fullyr <- as.numeric(paste0("20",yr))
  BR <- read_excel("data/BRef.xlsx",
                    sheet = paste0("BRp",yr)) %>%
    dplyr::select(`Player-additional`,IP,RA9,RAA,WAA,WAAadj,RAR,WAR) %>%
    rename(key_bbref=`Player-additional`,bWAR.p=WAR,
           RAA.p=RAA,WAA.p=WAA,WAAadj.p=WAAadj,RAR.p=RAR) %>%
    mutate(B.p=if_else(IP >= 10,1,0)) %>%
    full_join(read_excel("data/BRef.xlsx",
                    sheet = paste0("BRb",yr)) %>%
                dplyr::select(`Player-additional`,PA,RAA,WAA,RAR,
                              oWAR,dWAR,oRAR,WAR) %>%
                rename(key_bbref=`Player-additional`,bWAR.b=WAR,
                       RAA.b=RAA,WAA.b=WAA,RAR.b=RAR) %>%
                mutate(B.b=if_else(PA >= 10,1,0)),
              by=join_by(key_bbref)) %>%
    mutate(year=fullyr)
  
  FG <- read_csv(file="data/FGp.csv") %>% 
    dplyr::filter(Season==as.numeric(paste0("20",yr))) %>%
    dplyr::select(MLBAMID,ERA,xERA,FIP,xFIP,WAR) %>%
    rename(key_mlbam=MLBAMID,
           fWAR.p=WAR) %>%
    mutate(F.p=1) %>%
    full_join(read_csv(file="data/FGb.csv") %>% 
                dplyr::filter(Season==as.numeric(paste0("20",yr))) %>% 
                dplyr::select(MLBAMID,OBP,Off,Def,WAR) %>% 
                rename(key_mlbam=MLBAMID,
                       fWAR.b=WAR) %>%
                mutate(F.b=1),
              by=join_by(key_mlbam)) %>%
    mutate(year=fullyr)
  
  BP <- read_excel("data/BP.xlsx",
                    sheet = paste0("BPp",yr)) %>%
    dplyr::select(mlbid,`DRA-`,DRA,cFIP,WHIP,RA9,WARP) %>%
    rename(key_mlbam=mlbid,RA9.bp=RA9,WARP.p=WARP) %>%
    mutate(P.p=1) %>%
    full_join(read_excel("data/BP.xlsx",
                         sheet = paste0("BPb",yr)) %>%
                dplyr::select(mlbid,`DRC+`,WARP) %>%
                rename(key_mlbam=mlbid,WARP.b=WARP) %>%
                mutate(P.b=1),
              by=join_by(key_mlbam)) %>%
    mutate(year=fullyr)
  
  Stats <- CU %>%
    right_join(FG %>% full_join(BP,
                                by=join_by(key_mlbam, year)),
               by=join_by(key_mlbam)) %>%
    full_join(CU %>% right_join(BR, by=join_by(key_bbref)))
  
  FullStats <- FullStats %>% bind_rows(Stats)
}

FullStats <- FullStats %>%
    replace_na(list(F.p=0, F.b=0,
                    B.p=0, B.b=0,
                    P.p=0, P.b=0,
                    bWAR.b=0, bWAR.p=0,
                    fWAR.b=0, fWAR.p=0,
                    WARP.b=0, WARP.p=0
                    )) %>%
    mutate(Any.p = if_any(c("F.p","B.p","P.p"), ~ . == 1),
           All.p = if_all(c("F.p","B.p","P.p"), ~ . == 1),
           Any.b = if_any(c("F.b","B.b","P.b"), ~ . == 1),
           All.b = if_all(c("F.b","B.b","P.b"), ~ . == 1),
           bWAR.t=bWAR.b+bWAR.p,
           fWAR.t=fWAR.b+fWAR.p,
           WARP.t=WARP.b+WARP.p)

save(FullStats,
     file="int/FullStats.Rda")
