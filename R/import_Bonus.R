require(dplyr)
require(tidyr)
require(readr)
require(readxl)

## Import crosswalk data:
load(file="int/Crosswalk.Rda")

for (yr in 22:24) {
  ## Read in bonus data:
  Bonus <- Cross_use %>% 
    dplyr::select(key_person,key_mlbam,
                  key_bbref,key_bbref_minors,key_fangraphs,
                  full) %>%
    right_join(read_excel("data/PreArbBonus.xlsx",
                          sheet = paste0("BonusesAdj",yr)) %>% 
                 mutate(BonusW=Bonus-Set_Bonus,
                        Share=BonusW/sum(BonusW),
                        year=as.numeric(paste0("20",yr))) %>%
                 dplyr::select(-c(Notes)),
               by=join_by(key_bbref))
  
  ## Get subsets with positive stat-based bonus & exact bonus
  assign(x=paste0("Bonus_",yr,"_Full"),
         value=Bonus)
  savelist <- c(paste0("Bonus_",yr,"_Full"))
  if (mean(Bonus$Exact) < 1) {
    assign(x=paste0("Bonus_",yr,"_Ex"),
           value=Bonus %>% dplyr::filter(Exact))
    savelist <- c(savelist, paste0("Bonus_",yr,"_Ex"))
  }
  if (dim(Bonus %>% filter(Share <= 0))[1] > 0) {
    assign(x=paste0("Bonus_",yr,"_Pos"),
           value=Bonus %>% dplyr::filter(Share > 0))
    savelist <- c(savelist, paste0("Bonus_",yr,"_Pos"))
  }
  
  ## Save results:
  save(list=savelist,
       file=paste0("int/Bonus_",yr,".Rda"))
}
