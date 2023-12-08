#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# 
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(readr)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(rsconnect)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(corrplot)
library(RColorBrewer)
library(plotly)
library(fmsb)

#upload the datasets, already wrangled to fit the app criteria
livePlays <- read_csv('livePlays.csv')
players <- read_csv('players.csv')
player_tackles_byweek <- read_csv('player_tackles_byweek.csv')
player_rankings <- read_csv('player_rankings.csv')
weekly_position_avgs <- read_csv('weekly_position_avgs.csv')
player_tackles_total <- read_csv('player_tackles_total.csv')
displayName_defensiveTeam_color <- read_csv('displayName_defensiveTeam_color.csv')
player_tackles_byposition_byweek <- read_csv('player_tackles_byposition_byweek.csv')
top10_tacklers_bypos_stats <- read_csv('top10_tacklers_bypos_stats.csv')


ui <- dashboardPage(
  skin = "red",
  
  dashboardHeader(title = "Tackling the NFL Defensive Data!", titleWidth = 500),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("house")),
      menuItem("Player Tracker", tabName = "playertracker", icon = icon("chart-line")),
      menuItem("Team Tracker", tabName = "teamtracker", icon = icon("chart-line")),
      menuItem("Leaderboards", tabName = "leaderboard", icon = icon("chart-line")),
      menuItem("Field Look", tabName = "field", icon = icon("map")),
      menuItem("About Me", tabName = "about", icon = icon("face-smile"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Welcome page
      tabItem(tabName = "welcome",
              h1("Kickoff!", 
                 style="text-align:center"),
              h2("This Shiny App is centered around delivering NFL Defensive Player insights!", 
                 style="text-align:center"),
              h4("Explore the player stats around position, team, and stats in the tabs to the left.", 
                 style="text-align:center"),
              h1("- - - - - - - - - - - - - - - - - - - - - -", 
                 style="text-align:center"),
              #upload NFL image in welcome page
              div(img(src='NFLLOGO.png', height="25%", width="25%"), style="text-align: center;")
              
      ),
      
       # player tracker page, includes bar chart with trendline and radar plot
       tabItem(tabName = "playertracker",
               h2("NFL Defensive Stats by Player"), #heading
               # Sidebar
               sidebarLayout(
                 sidebarPanel(
      
                   selectizeInput("name", "Player Name",
                                  choices = player_tackles_byweek$displayName),
                                  # choices = NULL,
                                  # options = list(
                                  # placeholder = "Select a player...",
                                  # onInitialize = I('function() { this.setValue(""); }'),
                                  # create = TRUE) ) ,
                                 
                   div('Welcome to the Player Tracker! Please select a player name from the drop-down menu.
                       This page is meant to provide an individual analysis of a specific player on defense.
                       On the top chart, we have a week-by-week count of tackles for the chosen player, the trendline is
                       showing the top 10 tacklers of the week at their position.',style='text-align:center;'),
                   div(imageOutput('logo'), style="text-align: center;"),
      
                  ),

                # Show plot
                mainPanel(
                
                  plotOutput("playerPlot_TacklesByWeek", height = 450),
                  plotOutput("playerPlot_Radar", height = 450)
                )
              )
      ),

      #team tracker, includes a bar chart for positional counts and a polar plot to show level of rank for each position
      tabItem(tabName = "teamtracker",
              h2("Team Tracker"), #heading
              # Sidebar
              sidebarLayout(
                sidebarPanel(
                  selectizeInput("teamName", "Team Name",
                                 choices = player_rankings$defensiveTeam),
                  div("Welcome to the Team Tracker! Please select a team name from the drop-down menu.
                      This page provides a bar chart with counts of each position, alongside a polar chart with rating relative to position.
                      Defensive scouts or team owners can look at this page to see where they are strong, or where they are weak.
                      Alongside this, they can view other teams for trade opportunities.",style='text-align:center;'),
                  div(imageOutput('logo2'), style="text-align: center;"),

                ),
             mainPanel(

                  plotOutput("teamPosPlot", height = 450),
                  plotOutput("teamPlot", height = 450)

                )
              )
      ),
      #leaderboard to show which weeks and what defensive stat for selection
      tabItem(tabName = "leaderboard",
              h2("Leaderboards (First Nine Weeks)"),
              # Sidebar
              sidebarLayout(
                sidebarPanel(
                  # slider to choose week(s)
                  chooseSliderSkin("Flat"),
                  sliderInput("weeks", "Weeks", value = c(1, 2), min = 1, max = 9),

                  selectizeInput("defstat", "Defensive Stat",
                                 choices = c("Tackles", "Tackles for a Loss",
                                             "Forced Fumbles","Impact Plays", "Assists", "Missed Tackles")),
                  div("Welcome to the Leaderboards! Please select a defensive stat of interest from the drop-down menu.
                      This is where one can find a top 10 set of players for a given stat, as well as filter for specific weeks.",style='text-align:center;'),
                ),
                # Show plot
                mainPanel(

                   plotOutput("leaderPlot", height = 600)
                 )
               )
      
       ),
        #animtion for plays by frame id
       tabItem(tabName = "field",
               h2("Live Action Play View"),
               # Sidebar
               sidebarLayout(
                 sidebarPanel(
                   selectizeInput("field", "Choose a Play",
                                  choices = livePlays$playDescription),
                   # drop down menu for field play
                   div("Welcome to the Field Look, we give a live-action view of a play selected through the drop-down menu.
                       View the plays that mattered most in the game and see who was making a difference by hovering over their circle.",style='text-align:center;'),
                 ),
                 # Show plot
                 mainPanel(
      
                   plotlyOutput("fieldLook", height = 600)
                 )
               )

      ),
      
      
      # About Me
      tabItem(tabName = "about",
              h2("Brian Drewes", 
                 style="text-align:center"),
              h4("I created this app as part of my fellowship at NYC Data Science Academy. Check out my LinkedIn &  
                 read my blog to learn more about this app or some of my other projects.", 
                 style="text-align:center"),
              
              div(img(src='brianpic.jpeg', height="25%", width="25%"), style="text-align: center;"),
              
              uiOutput("tab3"),
              uiOutput("tab1"),
              uiOutput("tab2")
      ) ) #end of tab items
  
) # end of dashboard body

)

server <- function(input, output) {
  

  
  output$playerPlot_TacklesByWeek <- renderPlot({

    counts <- player_tackles_byweek %>% group_by(week_num) %>% summarise(tackles = sum(tot_tackles[displayName==input$name]))

    avg_trendline <- weekly_position_avgs %>% filter(position == players[players$displayName == input$name,]$position[1])

    ggplot(data = counts, mapping = aes(x=week_num,y=tackles)) +
      geom_bar(stat='identity', fill=displayName_defensiveTeam_color[displayName_defensiveTeam_color$displayName == input$name,]$color[1]) +
      geom_line(aes(x = avg_trendline$week_num, y = avg_trendline$Tackles))  +
      scale_x_continuous(counts$week_num, labels = sprintf('Week %s',as.character(counts$week_num)), breaks = counts$week_num) +
      labs(y='Tackles', x='Week #') + theme(axis.title.x = element_blank(), axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))



    })

  output$playerPlot_Radar <- renderPlot({

    appradar_idealplayer = top10_tacklers_bypos_stats %>%
      filter(position == players[players$displayName == input$name,]$position[1]) %>%
      group_by(position) %>%
      summarise(Tackles = mean(tot_tackles), Assists = mean(tot_assists), Tackles_For_Loss = mean(tot_tackles_forloss),
                Forced_Fumbles = mean(tot_ffumbles), Height = mean(Height), Weight = mean(Weight))

    appradar_maxplayer = player_tackles_total %>%
      filter(position == players[players$displayName == input$name,]$position[1]) %>%
      group_by(position) %>%
      summarise(Tackles = max(tot_tackles), Assists = max(tot_assists), Tackles_For_Loss = max(tot_tackles_forloss),
                Forced_Fumbles = max(tot_ffumbles), Height = max(Height), Weight = max(Weight))

    appradar_minplayer = player_tackles_total %>%
      filter(position == players[players$displayName == input$name,]$position[1]) %>%
      group_by(position) %>%
      summarise(Tackles = min(tot_tackles), Assists = min(tot_assists), Tackles_For_Loss = min(tot_tackles_forloss),
                Forced_Fumbles = min(tot_ffumbles), Height = min(Height), Weight = min(Weight))

    inputplayer_radar = player_tackles_total %>%
      filter(displayName == input$name) %>%
      group_by(position) %>%
      summarise(Tackles = min(tot_tackles), Assists = min(tot_assists), Tackles_For_Loss = min(tot_tackles_forloss),
                Forced_Fumbles = min(tot_ffumbles), Height = min(Height), Weight = min(Weight))

    radarplot = rbind(appradar_maxplayer, appradar_minplayer, appradar_idealplayer, inputplayer_radar)
    radarplot = radarplot[-c(1)]



    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    # Custom the radarChart !
    radarchart(radarplot, axistype=1 ,

               #custom polygon
               pcol=colors_border, pfcol=colors_in, plwd=4 , plty=1,

               #custom the grid
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,

               #custom labels
               vlcex=0.8


    )
    legend(x=1, y=1.35,  legend = c(sprintf('Top 10 %s Avgs',players[players$displayName == input$name,]$position[1]),sprintf("%s",input$name)),
           bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3) + theme(axis.text = element_text(face="bold"))


  })


  output$logo <- renderImage({ list(src=sprintf("www/%s",displayName_defensiveTeam_color[displayName_defensiveTeam_color$displayName == input$name,]$logo[1] ), width='50%',height='50%', deleteFile = FALSE) })
  output$logo2 <- renderImage({ list(src=sprintf("www/%s",displayName_defensiveTeam_color[displayName_defensiveTeam_color$defensiveTeam == input$teamName,]$logo[1] ), width='50%',height='50%', deleteFile = FALSE) })


  output$error <- renderText({
    "Choose a selection."
  })



  output$leaderPlot <- renderPlot({
    if (input$defstat == 'Tackles'){
          leaderboard_input = player_tackles_byposition_byweek[c('week_num','displayName', 'tot_tackles')]
          counts <- player_tackles_byposition_byweek[(player_tackles_byposition_byweek$week_num >= input$weeks[1]) &
                                                       (player_tackles_byposition_byweek$week_num <= input$weeks[2]),] %>%
                                                            group_by(displayName) %>%
                                                            summarise(Stat = sum(tot_tackles))}
    else if (input$defstat == "Tackles for a Loss"){
          leaderboard_input = player_tackles_byposition_byweek[c('week_num','displayName', 'tot_tackles_forloss')]
          counts <- player_tackles_byposition_byweek[(player_tackles_byposition_byweek$week_num >= input$weeks[1]) &
                                                       (player_tackles_byposition_byweek$week_num <= input$weeks[2]),] %>%
                                                            group_by(displayName) %>%
                                                            summarise(Stat = sum(tot_tackles_forloss))}
    else if (input$defstat == "Forced Fumbles"){
          leaderboard_input = player_tackles_byposition_byweek[c('week_num','displayName', 'tot_ffumbles')]
          counts <- player_tackles_byposition_byweek[(player_tackles_byposition_byweek$week_num >= input$weeks[1]) &
                                                       (player_tackles_byposition_byweek$week_num <= input$weeks[2]),] %>%
                                                            group_by(displayName) %>%
                                                            summarise(Stat = sum(tot_ffumbles))}
    else if (input$defstat == "Impact Plays"){
          leaderboard_input = player_tackles_byposition_byweek[c('week_num','displayName', 'tot_ips')]
          counts <- player_tackles_byposition_byweek[(player_tackles_byposition_byweek$week_num >= input$weeks[1]) &
                                                       (player_tackles_byposition_byweek$week_num <= input$weeks[2]),] %>%
                                                            group_by(displayName) %>%
                                                            summarise(Stat = sum(tot_ips))}
    else if (input$defstat == "Assists"){
          leaderboard_input = player_tackles_byposition_byweek[c('week_num','displayName', 'tot_assists')]
          counts <- player_tackles_byposition_byweek[(player_tackles_byposition_byweek$week_num >= input$weeks[1]) &
                                                       (player_tackles_byposition_byweek$week_num <= input$weeks[2]),] %>%
                                                            group_by(displayName) %>%
                                                            summarise(Stat = sum(tot_assists))}
    else if (input$defstat == "Missed Tackles"){
          leaderboard_input = player_tackles_byposition_byweek[c('week_num','displayName', 'tot_missed_tackles')]
          counts <- player_tackles_byposition_byweek[(player_tackles_byposition_byweek$week_num >= input$weeks[1]) &
                                                       (player_tackles_byposition_byweek$week_num <= input$weeks[2]),] %>%
                                                            group_by(displayName) %>%
                                                            summarise(Stat = sum(tot_missed_tackles))}



    # counts <- player_tackles_byposition_byweek[(player_tackles_byposition_byweek$week_num >= input$weeks[1]) &
    #                                              (player_tackles_byposition_byweek$week_num <= input$weeks[2]),] %>%
    #                                                     group_by(displayName) %>%
    #                                                       summarise(Stat = sum(tot_tackles))

    counts <- head(arrange(counts,desc(Stat)),10)

    #ggplot(counts, aes(x = reorder(displayName, Stat),y= Stat)) + geom_bar(stat='identity')  + xlab('Player') + ylab(input$defstat) + coord_flip()
    ggplot(counts, aes(x=reorder(displayName, Stat), y=Stat)) +
      geom_segment( aes(x=reorder(displayName, Stat), xend=reorder(displayName, Stat), y=0, yend=Stat), color="skyblue") +
      geom_point( color="blue", size=4, alpha=0.6) +
      theme_light() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(face="bold")
      ) + labs(x='Player', y = sprintf("Total %s",input$defstat)) + theme(axis.title.x = element_text(size=15),axis.title.y = element_text(size=15), axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))




  })

    output$teamPosPlot <- renderPlot({

      ggplot(player_rankings %>% filter(defensiveTeam ==
               player_rankings[player_rankings$defensiveTeam == input$teamName,]$defensiveTeam[1]) %>%
               group_by(position) %>% summarise(n = n()),aes(x=position,y=n))+
        geom_bar(stat='identity', fill=displayName_defensiveTeam_color[displayName_defensiveTeam_color$defensiveTeam == input$teamName,]$color[1]) +
        coord_flip() + ggtitle('Position Counts') +
        theme(axis.title.x = element_blank(),axis.title.y = element_text(size=15),
              axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))






    })

    output$teamPlot <- renderPlot({

      polarchartlive = player_rankings %>% filter(defensiveTeam == player_rankings[player_rankings$defensiveTeam == input$teamName,]$defensiveTeam[1]) %>% group_by(defensiveTeam, position)  %>%  summarise(Position_Rating = mean(rank))
      polarchartlive$id = seq(1,nrow(polarchartlive))




      # Get the name and the y position of each label
      label_datalive <- polarchartlive
      # calculate the ANGLE of the labels
      number_of_bar <- nrow(label_datalive)
      angle <-  90 - 360 * (label_datalive$id-0.5) /number_of_bar
      # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
      # calculate the alignment of labels: right or left
      # If I am on the left part of the plot, my labels have currently an angle < -90
      label_datalive$hjust<-ifelse( angle < -90, 1, 0)
      # flip angle BY to make them readable
      label_datalive$angle<-ifelse(angle < -90, angle+180, angle)
      mid<-mean(polarchartlive$Position_Rating)
      # Make the plot
      ggplot(polarchartlive %>% filter(defensiveTeam == player_rankings[player_rankings$defensiveTeam == input$teamName,]$defensiveTeam[1]), aes(x=as.factor(position), y=Position_Rating/2, fill=Position_Rating)) +
        # Note that id is a factor. If x is numeric, there is some space between the first bar
        # This add the bars with a blue color
        geom_bar(stat="identity") +
        scale_fill_gradient(low="green", high="red") +
        # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
        ylim(-100,120) +
        # Custom the theme: no axis title and no cartesian grid
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
        ) +

        # This makes the coordinate polar instead of cartesian.
        coord_polar(start = 0) +
        geom_text(data=label_datalive, aes(x=id, y=Position_Rating/1.5, label=position, hjust=hjust),
                  color="black", fontface="bold",alpha=0.6, size=15, angle= label_datalive$angle, inherit.aes = FALSE ) +
        theme(legend.key.size = unit(2, 'cm'))


    })


  output$fieldLook <- renderPlotly({

    selected_event = input$field


    vline <- function(x = 0, color = "yellow") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color),
        layer = "below"
      )
    }

    scrimline <- function(x = 0, color = "blue") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color),
        layer = "below"
      )
    }

    selected_gamePlayIds = livePlays[livePlays$playDescription == selected_event,][
                                c('gameId.x','playId.x')]


    selected_play = livePlays %>% filter(gameId.x == selected_gamePlayIds[[1]], playId.x == selected_gamePlayIds[[2]])

    firstdown = ifelse(selected_play$playDirection == 'left',
               (livePlays[livePlays$gameId.x == selected_gamePlayIds$gameId.x & livePlays$playId.x == selected_gamePlayIds$playId.x,]$absoluteYardlineNumber) -
                 (livePlays[livePlays$gameId.x == selected_gamePlayIds$gameId.x & livePlays$playId.x == selected_gamePlayIds$playId.x,]$yardsToGo),
               (livePlays[livePlays$gameId.x == selected_gamePlayIds$gameId.x & livePlays$playId.x == selected_gamePlayIds$playId.x,]$absoluteYardlineNumber) +
                 (livePlays[livePlays$gameId.x == selected_gamePlayIds$gameId.x & livePlays$playId.x == selected_gamePlayIds$playId.x,]$yardsToGo))[1]

    los = livePlays[livePlays$gameId.x == selected_play$gameId.x & livePlays$playId.x == selected_play$playId.x,]$absoluteYardlineNumber[1]

    vertical_lines = vline(55,color='red')

    selected_play %>% plot_ly(

      x = selected_play$x,
      y = selected_play$y,
      color = ~club,
      colors = c("red","#CFFF04","blue"),
      frame = ~frameId,
      text = ~displayName,
      size = 12,
      marker = list(line = list(color = "black", width=2)),
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers') %>%
      config(selected_play, responsive = FALSE) %>%
      layout(title = selected_play$playDescription,
             shapes = list(
               vline(10,color='white'), vline(20,color='white'), vline(30,color='white'),
               vline(40,color='white'), vline(50,color='white'), vline(60,color='white'),
               vline(70,color='white'), vline(80,color='white'), vline(90,color='white'),
               vline(0,color='orange'), vline(100,color='orange'),
               vline(firstdown),
               scrimline(los),#scrim lines and first down
               list(type = "rect", fillcolor = "red", line = list(color = "red"),
                    opacity = 0.6, y0 = 0, y1 = 53.3, x0 = -10, x1 = 0,
                    layer = "below"),
               list(type = "rect", fillcolor = "red", line = list(color = "red"),
                    opacity = 0.6, y0 = 0, y1 = 53.3, x0 = 100, x1 = 110,
                    layer = "below")),
             plot_bgcolor= 'lightgreen',
             xaxis = list(
               range=c(-10,110), showticklabels=FALSE, showgrid = F, zerolinecolor = '#ffff'),
             yaxis = list(
               range=c(0,53.3), showticklabels=FALSE, showgrid = F, zerolinecolor = '#ffff')) %>%
      add_annotations(x= c(10,20,30,40,50,60,70,80,90),
                      y= c(50,50,50,50,50,50,50,50,50),
                      xref = 'x', yref = 'y',
                      text = c("10", "20","30","40","50","40","30","20","10"),
                      showarrow = F )


  })
    
    # About Me
    
    
    linkedin_url = a("LinkedIn", href="https://www.linkedin.com/in/briangdrewes/")
    github_link = a("GitHub", href="https://github.com/briangdrewes")
    blog_link = a("Blog", href="https://nycdatascience.com/blog/author/briandrewes/")
    
    output$tab3 <- renderUI({
      tagList("Read my blog:", blog_link)
    })
    
    output$tab1 <- renderUI({
      tagList("Find me on LinkedIn:", linkedin_url)
    })
    output$tab2 <- renderUI({
      tagList("Check out my GitHub:", github_link)
    })
  
    
    
    

}
# Run the application 
shinyApp(ui = ui, server = server)


