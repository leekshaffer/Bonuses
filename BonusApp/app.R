library(shiny)
library(tidyverse)
library(ggsci)
library(DT)
library(bslib)

load("res/plot_all.Rda")
load("res/Res_Summ.Rda")
load("res/Res_Full.Rda")

Years <- 2022:2024
Measures <- c("bWAR", "fWAR", "WARP", "WAR.avg")
Meas_Names <- c("bWAR", "fWAR", "WARP", "Avg. WAR")
names(Meas_Names) <- Measures
names(Measures) <- Meas_Names

## UI:
ui <- fluidPage(
  tabsetPanel(
    id = "tabset",
    tabPanel("Pre-Arbitration Bonuses", 
             page_sidebar(
               title = "Comparisons of Pre-Arb Bonuses by Value Measure",
               sidebar=sidebar(
                 selectInput("Year", 
                             label = h4("Season"),
                             choices = as.list(Years),
                             selected = 2024),
                 selectInput("Meas",
                             label=h4("Comparison Measure"),
                             choices=as.list(Measures),
                             selected= "bWAR"),
                 h5("Details at GitHub:"),
                 a("https://bit.ly/PreArb-Bonus", href="https://bit.ly/PreArb-Bonus"),
                 h5("Data Sources:"),
                 a("AP News",
                   href="https://apnews.com/sports/baseball-5dc4b61e33649fa5d08e17c3de87cf67"),
                 a("MLB Players Association: CBA",
                   href="https://www.mlbplayers.com/cba"),
                 a("Spotrac", href="https://www.spotrac.com/mlb/pre-arbitration/year/_/year/2024"),
                 a("Baseball Reference",
                   href="https://www.baseball-reference.com/leagues/"),
                 a("FanGraphs",
                   href="https://www.fangraphs.com/leaders/major-league"),
                 a("Baseball Prospectus",
                   href="https://www.baseballprospectus.com/leaderboards/"),
                 a("baseballr R Package",
                   href="https://billpetti.github.io/baseballr/")
               ),
               accordion(
                 open = c("Figure"),
                 accordion_panel("Figure",
                                 plotOutput("Fig1", width="95%")),
                 accordion_panel("Summary Table (All Measures)",
                                 DT::dataTableOutput("Tbl_Summ")),
                 accordion_panel("Full Table",
                                 DT::dataTableOutput("Tbl_Full"))
               )
             )
    )
  )
)

## Server:
server <- function(input, output) {
  
  output$Fig1 <- renderPlot({
    get(paste(input$Meas, substr(input$Year, 3, 4), sep="_"))
  })
  
  output$Tbl_Summ <- DT::renderDataTable({
    tbl <- Res_Summ %>% 
      dplyr::filter(Year==as.numeric(input$Year)) %>%
      dplyr::mutate(`Difference in Players`=pmax(`Players Added`, `Players Subtracted`)) %>%
      dplyr::rename(`Total Amount Re-Allocated`=`Total Re-Allocated`,
                    `Mean Absolute Difference`=`Mean Abs. Difference`,
                    `Maximum Increase`=`Max Increase`,
                    `Max. Inc. Player`=`Max Increase Player`,
                    `Maximum Decrease`=`Max Decrease`,
                    `Max. Dec. Player`=`Max Decrease Player`) %>%
      dplyr::select(Measure, `Total Amount Re-Allocated`, `Mean Absolute Difference`,
                    `Maximum Increase`, `Max. Inc. Player`, `Maximum Decrease`, `Max. Dec. Player`)
    DT::datatable(tbl %>%
                      dplyr::mutate(across(.cols=starts_with(c("Total", "Mean", "Maximum")),
                                           .fns=~paste0("$",format(round(.x, digits=0), nsmall=0, big.mark=",")))),
                    options = list(paging = FALSE,
                                   searching = FALSE))
  })
  
  output$Tbl_Full <- DT::renderDataTable({
    tbl <- Res_Full[[as.character(input$Year)]] %>% 
      dplyr::select(player, status, pos, team, starts_with(input$Meas), Bonus, Bonus_Rank, Max_Bonus, Min_Bonus) %>%
      arrange(desc(get(input$Meas))) %>%
      dplyr::rename(Player=player, Contract=status, Position=pos, Team=team,
                    `Bonus: Reported`=Bonus, `Rank: Reported`=Bonus_Rank, 
                    `Maximum Bonus`=Max_Bonus, `Minimum Bonus`=Min_Bonus) %>%
      dplyr::rename_with(~paste0(sub(paste0(input$Meas,"_"),"", .x),": ",Meas_Names[input$Meas]),
                         starts_with(paste0(input$Meas,"_"))) %>%
      dplyr::select(Player, Contract, Position, Team, starts_with(input$Meas), 
                    ends_with(input$Meas), `Rank: Reported`, `Bonus: Reported`, ends_with("Bonus"))
    DT::datatable(tbl %>%
                    dplyr::mutate(across(.cols=starts_with("Bonus"),
                                         .fns=~paste0("$",format(round(.x, digits=0), nsmall=0, big.mark=","))),
                                  across(.cols=ends_with("Bonus"),
                                         .fns=~paste0("$",format(round(.x, digits=0), nsmall=0, big.mark=","))),
                                  across(.cols=starts_with(input$Meas),
                                         .fns=~round(.x, digits=1))),
                  options = list(lengthMenu=list(c(12, 25, 50, -1),
                                                 c("12", "25", "50", "All")),
                                 pageLength=25,
                                 searching = TRUE))
  })
}

## Run the App:
shinyApp(ui = ui, server = server)