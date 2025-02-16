require(dplyr)
require(tidyr)
require(readr)
require(readxl)

## Import crosswalk data:
load(file="int/Crosswalk.Rda")

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
