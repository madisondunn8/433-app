packages <- c("shiny", "tidyverse", "ggplot2", "DT")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)



draft_data=read.csv("draft_data.csv")
season_data=read.csv("season_data.csv")
combine_data=read.csv("combine_data.csv")
wins_data = read.csv("Team_Records.csv")
colnames(wins_data)[1] = "Season"
# combine draft data with team success data merging by team and year

draft = draft_data %>% 
  #filter(numberPickOverall <= 14) %>% 
  mutate(teamYear = paste(teamName, ", ", yearDraft, sep=""))

wins_data = wins_data %>% 
  mutate(teamYear = paste(gsub("[^[:alnum:]]", "", word(Team,-1)), ", ", substr(Season, 1, 4), sep=""))

draft_and_wins = merge(draft, wins_data, by.x = "teamYear", by.y = "teamYear")

df1 = wins_data[1,]
five_after_first = list()
#rbind(df1, df2)
index = 0
for (i in draft_and_wins[draft_and_wins$numberPickOverall==1,]$teamYear){
  team = strsplit(i, ', ')[[1]][1]
  year = as.numeric(strsplit(i, ', ')[[1]][2])
  
  df2 = wins_data[1,]
  for (j in 1:5){
    a = wins_data[wins_data$teamYear == paste(team, ", ", toString(year + j), sep=""),]
    
    df1 = rbind(df1, a)
    df2 = rbind(df2, a)
    #print(b)
    ##points(a$W.L., a$yearDraft)
    
  }
  index = index + 1
  five_after_first[[index]] = df2[2:6, ]
  df2 = ""
  
}
five_years_after_first_pick = df1 %>% distinct(teamYear, .keep_all = TRUE)

draft_scores = draft_and_wins %>% 
  filter(yearDraft>=1990) %>% 
  group_by(teamYear) %>% 
  summarize(pick_sum = sum(numberPickOverall), 
            pick_count = n(), 
            best_pick = min(numberPickOverall),
            worst_pick = max(numberPickOverall),
            draft_score = 61*pick_count - pick_sum) %>%
  arrange(desc(draft_score))
 



five_after = list()
index = 0

for (i in draft_scores$teamYear){
  team = strsplit(i, ', ')[[1]][1]
  year = as.numeric(strsplit(i, ', ')[[1]][2])
  
  draft_score = as.numeric(draft_scores[draft_scores$teamYear == i,]$draft_score)
  best_pick = as.numeric(draft_scores[draft_scores$teamYear == i,]$best_pick)
  win_pct = as.double(wins_data[wins_data$teamYear == paste(team, ", ", toString(year), sep=""),]$W.L.)
  win_pct_2 = as.double(wins_data[wins_data$teamYear == paste(team, ", ", toString(year + 2), sep=""),]$W.L.)
  win_pct_3 = as.double(wins_data[wins_data$teamYear == paste(team, ", ", toString(year + 3), sep=""),]$W.L.)
  win_pct_4 = as.double(wins_data[wins_data$teamYear == paste(team, ", ", toString(year + 4), sep=""),]$W.L.)
  win_pct_5 = as.double(wins_data[wins_data$teamYear == paste(team, ", ", toString(year + 5), sep=""),]$W.L.)
  win_pct_6 = as.double(wins_data[wins_data$teamYear == paste(team, ", ", toString(year + 6), sep=""),]$W.L.)
  win_pct_7 = as.double(wins_data[wins_data$teamYear == paste(team, ", ", toString(year + 7), sep=""),]$W.L.)
  
  avg_win_pct_change = ((win_pct_2 - win_pct) + (win_pct_3 - win_pct_2) + (win_pct_4 - win_pct_3) + (win_pct_5 - win_pct_4) + (win_pct_6 - win_pct_5) + (win_pct_7 - win_pct_6)) / 6
  
  win_pct_change_3_years = win_pct_4 - win_pct
  win_pct_change_5_years = win_pct_6 - win_pct
  
  if (length(win_pct_3) == 1){
    index = index + 1
    five_after[[index]] = c(team, year, draft_score, best_pick, win_pct_change_3_years, win_pct_change_5_years, win_pct, win_pct_2, win_pct_3, win_pct_4, win_pct_5, win_pct_6, win_pct_7, avg_win_pct_change)
  }
}

draft_scores_records = as.data.frame(do.call(rbind, five_after))
colnames(draft_scores_records) = c("team", "year", "draft_score", "best_pick", "win_pct_change_3_years", "win_pct_change_5_years", "win_pct_current", "win_pct_2_years_after", "win_pct_3_years_after", "win_pct_4_years_after", "win_pct_5_years_after", "win_pct_6_years_after", "win_pct_7_years_after",  "avg_win_pct_change")
draft_scores_records$draft_score <- as.integer(as.character(draft_scores_records$draft_score))
draft_scores_records$win_pct_current <- as.double(as.character(draft_scores_records$win_pct_current))
draft_scores_records$win_pct_2_years_after <- as.double(as.character(draft_scores_records$win_pct_2_years_after))
draft_scores_records$win_pct_3_years_after <- as.double(as.character(draft_scores_records$win_pct_3_years_after))
draft_scores_records$win_pct_4_years_after <- as.double(as.character(draft_scores_records$win_pct_4_years_after))
draft_scores_records$win_pct_5_years_after <- as.double(as.character(draft_scores_records$win_pct_5_years_after))
draft_scores_records$win_pct_6_years_after <- as.double(as.character(draft_scores_records$win_pct_6_years_after))
draft_scores_records$win_pct_7_years_after <- as.double(as.character(draft_scores_records$win_pct_7_years_after))
draft_scores_records$win_pct_change_3_years <- as.double(as.character(draft_scores_records$win_pct_change_3_years))
draft_scores_records$win_pct_change_5_years <- as.double(as.character(draft_scores_records$win_pct_change_5_years))
draft_scores_records$best_pick <- as.integer(as.character(draft_scores_records$best_pick))
draft_scores_records$year <- as.integer(as.character(draft_scores_records$year))
draft_scores_records$avg_win_pct_change <- as.double(as.character(draft_scores_records$avg_win_pct_change))
#draft_scores_records[, 2:6] <- sapply(draft_scores_records[, 3:6], as.double)
draft_scores_records = draft_scores_records %>% filter(win_pct_2_years_after <= 1.00)
draft_scores_records = draft_scores_records %>% filter(win_pct_3_years_after <= 1.00)
draft_scores_records = draft_scores_records %>% filter(win_pct_4_years_after <= 1.00)
draft_scores_records = draft_scores_records %>% filter(win_pct_5_years_after <= 1.00)
draft_scores_records = draft_scores_records %>% filter(win_pct_6_years_after <= 1.00)
draft_scores_records = draft_scores_records %>% filter(win_pct_7_years_after <= 1.00)
draft_scores_records = draft_scores_records %>% filter(avg_win_pct_change <= 200)


ui <- fluidPage(
  title = "Stat 433 - NBA Draft Statistics",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "draft_and_wins"',
        checkboxGroupInput("show_vars", "Columns in draft_and_wins to show:",
                           names(draft_and_wins), selected = names(draft_and_wins))
      ),
      conditionalPanel(
        'input.dataset === "season_data"',
        helpText("Click the column header to sort a column.")
      ),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("draft_and_wins", DT::dataTableOutput("mytable1")),
        tabPanel("season_data", DT::dataTableOutput("mytable2"))
      )
    )
  )
)

server <- function(input, output) {
  
  # choose columns to display
  draft_and_wins2 = draft_and_wins[sample(nrow(draft_and_wins), 1000), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(draft_and_wins2[, input$show_vars, drop = FALSE])
  })
  
  # sorted columns are colored 
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(season_data, options = list(orderClasses = TRUE))
  })
  
  
}

shinyApp(ui, server)