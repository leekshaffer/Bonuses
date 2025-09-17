require(dplyr)
yrs <- 22:24
measures <- c("bWAR","fWAR","WARP","WAR.avg")


for (yr in yrs) {
  load(file=paste0("int/Comp100s_",yr,".Rda"))
  DiffR <- get(paste("PAC100", "any", yr, sep="_")) %>% slice_max(order_by=Max_Diff, n=1)
  print(paste0("Biggest Possible Difference in ", "20", yr, ": ", format(round(DiffR$Max_Diff, digits=0), nsmall=0, big.mark=","),
               " (", DiffR$player, ")"))
  for (meas in measures) {
    Vec <- get(paste("PAC100", "any", yr, sep="_")) %>% pull(paste(meas, "Diff", sep="_"))
    MaxR <- get(paste("PAC100", "any", yr, sep="_")) %>% slice_max(order_by=get(paste(meas, "Diff", sep="_")), n=1)
    MinR <- get(paste("PAC100", "any", yr, sep="_")) %>% slice_min(order_by=get(paste(meas, "Diff", sep="_")), n=1)
    
    print(paste0("Total Abs. Difference in ", "20", yr, " for ", meas, ": ", format(round(sum(abs(Vec))/2, digits=0), nsmall=0, big.mark=",")))
    print(paste0("Mean Abs. Difference in ", "20", yr, " for ", meas, ": ", format(round(mean(abs(Vec)), digits=0), nsmall=0, big.mark=",")))
    print(paste0("Max Pos. Difference in ", "20", yr, " for ", meas, ": ", format(round(max(Vec), digits=0), nsmall=0, big.mark=","),
                 " (", MaxR$player, ")"))
    print(paste0("Max Neg. Difference in ", "20", yr, " for ", meas, ": ", format(round(min(Vec), digits=0), nsmall=0, big.mark=","),
          " (", MinR$player, ")"))
  }
}
