require(dplyr)

## Load service time categories:
load(file="int/FDB.Rda")
## Load Stats
load(file="int/FullStats.Rda")

yrs <- 22:24

## Load Bonuses:
for (yr in yrs) {
  load(file=paste0("int/Bonus_",yr,".Rda"))
  assign(x=paste0("Check_",yr),
         value=get(paste0("Bonus_",yr,"_Full")) %>% 
           left_join(FDB %>% 
                       dplyr::select(key_bbref,year,status,yos,value,Svc_Cat),
                     by=join_by(key_bbref, year)))
}

## Status Check for Those with Bonuses:
for (yr in yrs) {
  print(paste0("Season: ",yr))
  print(paste0("Number of NAs: ",dim(get(paste0("Check_",yr)) %>% filter(is.na(status)))[1]))
  print(paste0("All in PreArb Svc_Cat? ",all(unique(get(paste0("Check_",yr)) %>% pull(Svc_Cat))=="PreArb")))
}

## Add Stats to FDB data for non-Arb players:
Comps <- FDB %>% dplyr::filter(!(Svc_Cat %in% c("Arb+","SuperTwo"))) %>%
  left_join(FullStats) %>%
  mutate(WAR.num = 3-(is.na(bWAR.t) + is.na(fWAR.t) + is.na(WARP.t)),
         WAR.sum = if_else(is.na(bWAR.t),0,bWAR.t) + 
           if_else(is.na(fWAR.t),0,fWAR.t) + 
           if_else(is.na(WARP.t),0,WARP.t),
         WAR.avg.t = if_else(WAR.num==0, NA_real_, WAR.sum/WAR.num))

PAComp <- Comps %>% dplyr::filter(Svc_Cat == "PreArb")
## To Do: Set 0 instead of NA for all bonuses. Calculate difference in PAC100 not Meas100
for (yr in yrs) {
  fullyr <- as.numeric(paste0("20",yr))
  
  Bonus <- get(paste0("Bonus_",yr,"_Full")) %>%
    dplyr::filter(Share > 0) %>%
    dplyr::arrange(desc(Share)) %>%
    dplyr::mutate(Bonus_rank=min_rank(desc(BonusW)))
  TotalBW <- sum(Bonus$BonusW)
  # NB <- nrow(Bonus)

  PAC_yr <- PAComp %>% dplyr::filter(year==fullyr) 
  # N <- nrow(PAC_yr)
  # PAC_yr_ranks <- PAC_yr %>% 
  #   dplyr::arrange(desc(bWAR.t)) %>% 
  #   mutate(bWAR.rank=1:N) %>% ## rank with ties?
  #   dplyr::arrange(desc(fWAR.t)) %>%
  #   mutate(fWAR.rank=1:N) %>% ## rank with ties?
  #   dplyr::arrange(desc(WARP.t)) %>%
  #   mutate(WARP.rank=1:N) %>% ## rank with ties?
  #   dplyr::arrange(desc(WAR.avg)) %>%
  #   mutate(WAR.avg.rank=1:N) %>% ## rank with ties?
  #   left_join(Bonus %>% mutate(Bonus.rank=1:NB) %>% ## rank with ties?
  #               dplyr::select(key_bbref,year,Exact,BonusW,Share,Bonus.rank))
  PAC_yr_ranks <- PAC_yr %>%
    dplyr::mutate(bWAR_rank=min_rank(desc(bWAR.t)),
                  fWAR_rank=min_rank(desc(fWAR.t)),
                  WARP_rank=min_rank(desc(WARP.t)),
                  WAR.avg_rank=min_rank(desc(WAR.avg.t))) %>%
    left_join(Bonus %>%
                dplyr::select(key_bbref,year,Exact,BonusW,Share,Bonus_rank), 
              by=join_by(key_bbref, year))
  
  PAC100_any <- PAC_yr_ranks %>%
    dplyr::filter(!is.na(Bonus_rank) | bWAR_rank <= 100 | 
                    fWAR_rank <= 100 | WARP_rank <= 100 | 
                    WAR.avg_rank <= 100)
  
  for (meas in c("bWAR","fWAR","WARP","WAR.avg")) {
    Meas100 <- PAC100_any %>% 
      dplyr::filter(get(paste(meas, "rank", sep="_")) <= 100)
    Sum <- sum(Meas100 %>%
                 dplyr::pull(get(paste(meas, "t", sep="."))))
    Share <- (Meas100 %>%
      dplyr::pull(get(paste(meas, "t", sep="."))))/Sum
    Meas100$Meas_Share <- Share
    Meas100$Meas_BonusW <- Share*TotalBW
    Meas100$Meas_Diff <- Meas100$Meas_BonusW-Meas100$BonusW
    PAC100_any <- PAC100_any %>% left_join(Meas100) %>%
      rename_with(.cols=starts_with("Meas_"), 
                  .fn=~paste(meas, sub("Meas_", "", .x), sep="_"))
  }

  PAP_yr <- Comps %>% dplyr::filter(year==fullyr)
  PAP_yr_ranks <- PAP_yr %>% 
    dplyr::mutate(bWAR_rank=min_rank(desc(bWAR.t)),
                  fWAR_rank=min_rank(desc(fWAR.t)),
                  WARP_rank=min_rank(desc(WARP.t)),
                  WAR.avg_rank=min_rank(desc(WAR.avg.t))) %>%
    left_join(Bonus %>%
                dplyr::select(key_bbref,year,Exact,BonusW,Share,Bonus_rank), 
              by=join_by(key_bbref, year))
  
  PAP100_any <- PAP_yr_ranks %>%
    dplyr::filter(!is.na(Bonus_rank) | bWAR_rank <= 100 | 
                    fWAR_rank <= 100 | WARP_rank <= 100 | 
                    WAR.avg_rank <= 100)
  
  assign(x=paste0("PAC100_any_",yr),
         value=PAC100_any)
  assign(x=paste0("PAP100_any_",yr),
         value=PAP100_any)
  assign(x=paste0("PAC_",yr,"_ranks"),
         value=PAC_yr_ranks)
  assign(x=paste0("PAP_",yr,"_ranks"),
         value=PAP_yr_ranks)
  save(list=c(paste0(c("PAC100_any_","PAP100_any_"),yr),
              paste0("PAC_",yr,"_ranks"),paste0("PAP_",yr,"_ranks")),
       file=paste0("int/Comp100s_",yr,".Rda"))
}
