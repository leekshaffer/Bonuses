library(dplyr)
library(ggplot2)
library(patchwork)
library(ggsci)
yrs <- 22:24
measures <- c("bWAR","fWAR","WARP","WAR.avg")
meas_names <- c("bWAR", "fWAR", "WARP", "Avg. WAR")
names(meas_names) <- measures


Res_Summ <- NULL
Res_Full <- list()

for (yr in yrs) {
  load(file=paste0("int/Comp100s_",yr,".Rda"))
  tbl <- get(paste("PAC100", "any", yr, sep="_"))
  
  Res_Full[[paste0("20",yr)]] <- tbl %>% 
    dplyr::select(player,status,pos,team,ends_with(".t"),ends_with("rank"),ends_with("BonusW")) %>%
    dplyr::rename_with(~sub("BonusW", "Bonus", .x),
                       ends_with("BonusW")) %>%
    dplyr::rename_with(~sub(".t", "", .x),
                       ends_with(".t")) %>%
    dplyr::rename_with(~sub("_rank", "_Rank", .x),
                       ends_with("_rank"))
  
  if (yr==22) {
    tbl <- tbl %>% dplyr::filter(Exact)
  }
  
  for (meas in measures) {
    tbl_m <- tbl %>% 
      dplyr::filter(BonusW > 0 | get(paste(meas, "BonusW", sep="_")) > 0) %>%
      dplyr::mutate(BonusCat=if_else(BonusW <= 0, "No Bonus",
                                     if_else(get(paste(meas, "BonusW", sep="_")) <= 0, "Not By Metric", "Both")))
    Vec <- tbl_m %>%
      pull(paste(meas, "Diff", sep="_"))
    MaxR <- tbl_m %>% slice_max(order_by=get(paste(meas, "Diff", sep="_")), n=1)
    MinR <- tbl_m %>% slice_min(order_by=get(paste(meas, "Diff", sep="_")), n=1)
    Row <- tibble(Year=as.numeric(paste0("20",yr)),
             Measure=meas,
             `Total Re-Allocated`=sum(abs(Vec))/2,
             `Mean Abs. Difference`=mean(abs(Vec)),
             `Max Increase`=max(Vec),
             `Max Increase Player`=MaxR$player,
             `Max Decrease`=-1*min(Vec),
             `Max Decrease Player`=MinR$player,
             `Players Added`=sum(tbl_m$BonusCat=="Not By Metric", na.rm=TRUE),
             `Players Subtracted`=sum(tbl_m$BonusCat=="No Bonus", na.rm=TRUE)
    )
    if (yr==22) {
      Row$`Players Added` <- NA_real_
    }
    Res_Summ <- Res_Summ %>% bind_rows(Row)
  }
  
  
  # DiffR <- tbl %>% slice_max(order_by=Max_Diff, n=1)
  # print(paste0("Biggest Possible Difference in ", "20", yr, ": ", format(round(DiffR$Max_Diff, digits=0), nsmall=0, big.mark=","),
  #              " (", DiffR$player, ")"))
}

Res_Summ
save(Res_Summ, file="out/Res_Summ.Rda")
Res_Full
save(Res_Full, file="out/Res_Full.Rda")

## Notes:
### In 2023, if not for Adolis Garcia, the max increase would be:
### Triston Casas at 356720.5 (WAR.avg), JP Sears at 352344.5 (bWAR), 
### Matt Brash at 300035.7 (fWAR), Hunter Brown at 597507.3 (WARP)

Create_Fig <- function(yr, meas) {
  load(file=paste0("int/Comp100s_",yr,".Rda"))
  tbl <- get(paste("PAC100", "any", yr, sep="_"))
  if (yr==22) {
    tbl <- tbl %>% dplyr::filter(Exact)
  }
  
  tbl <- tbl %>% 
    dplyr::filter(BonusW > 0 | get(paste(meas, "BonusW", sep="_")) > 0) %>%
    dplyr::mutate(BonusCat=if_else(BonusW <= 0, "No Bonus",
                                   if_else(get(paste(meas, "BonusW", sep="_")) <= 0, "Not By Metric", "Both")),
                  `Player Type`=if_else(Any.p & Any.b, if_else(All.p & !All.b, "Pitcher", 
                                                               if_else(All.b & !All.p, 
                                                                       "Position", 
                                                                       "Both/Indeterminate")),
                                        if_else(Any.p, "Pitcher", "Position")))
  
  lm <- lm(BonusW~0+get(paste(meas, "BonusW", sep="_")), data=tbl)
  coef <- unname(lm$coefficients)
  r2 <- summary(lm)$r.squared
  
  plot <- ggplot(tbl) +
    geom_point(mapping=aes(x=BonusW/1000,
                           y=get(paste(meas, "BonusW", sep="_"))/1000,
                           color=`Player Type`,
                           shape=`Player Type`)) +
                           # shape=BonusCat)) +
    scale_x_continuous(limits=c(0, 1400), breaks=seq(0, 1500, by=300)) + 
    scale_y_continuous(limits=c(0, 1400), breaks=seq(0, 1500, by=300)) + 
    scale_color_nejm() +
    geom_abline(slope=1, intercept=0) +
    theme_bw() +
    labs(x="Reported Bonus ($1000s)",
         y=paste0("Theoretical Bonus Using ", meas_names[meas], " ($1000s)"),
         title=paste0("Bonus Comparison Using ", meas_names[meas], " in 20", yr))
  
  return(list(Plot=plot,
              Coefficient=coef,
              R.Squared=r2))
}

for (yr in yrs) {
  for (meas in measures) {
    assign(x=paste(meas, yr, sep="_"),
           value=Create_Fig(yr, meas))
    get(paste(meas, yr, sep="_"))
    # save(list=paste(meas, yr, sep="_"),
    #      file=paste0("out/plot_", meas, "_", yr, ".Rda"))
    ggsave(filename=paste0("out/plot_",meas,"_",yr,".png"),
           plot=get(paste(meas, yr, sep="_"))$Plot,
           units="in", width=5, height=3, dpi=300)
  }
}

save(list=paste(rep(measures, each = length(yrs)), yrs, sep = "_"),
     file="out/plot_all.Rda")

for (yr in yrs) {
  ggsave(filename=paste0("out/plot_", yr, "_comb.png"),
         plot=get(paste("bWAR", yr, sep="_"))$Plot + labs(title="A) bWAR") + 
           get(paste("fWAR", yr, sep="_"))$Plot + labs(title="B) fWAR") + 
           get(paste("WARP", yr, sep="_"))$Plot + labs(title="C) WARP") +
           guide_area() +
           # plot_annotation(title=paste0("Bonus Comparisons by Measure in 20", yr)) +
           plot_layout(guides="collect", nrow=2, ncol=2, byrow=TRUE),
         units="in", width=6, height=6, dpi=300)
}


