############ Import Libraries ############
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggmap)
library(plotly)
library(shiny)


############################ Subset & Export Data #############################
#dest <- read.delim("dest.txt")
#data <- read.delim("data.txt")
#mergeddata <- left_join(dest, data, by = "srch_destination_id")
#set.seed(12345)
#fulldata <- sample_n(mergeddata, 1500, na.rm = T)
#write.csv(fulldata, "fulldata.csv")
################################################################################

fulldata <- read.csv("fulldata.csv")

######################## Data Cleaning ######################## 
# modify existing variables
fulldata$srch_destination_latitude <- as.numeric(fulldata$srch_destination_latitude)
fulldata$srch_destination_longitude <- as.numeric(fulldata$srch_destination_longitude)
fulldata$hour <- hour(format(fulldata$date_time, format = "%H:%M:%S"))
fulldata$date_time <- as.Date(fulldata$date_time)
fulldata$srch_ci <- as.Date(fulldata$srch_ci)
fulldata$srch_co <- as.Date(fulldata$srch_co)
fulldata$orig_destination_distance <- as.numeric(fulldata$orig_destination_distance)
fulldata$prop_starrating_cat <- as.factor(fulldata$prop_starrating)
fulldata$prop_is_branded[fulldata$prop_is_branded == 1] <- "Yes"
fulldata$prop_is_branded[fulldata$prop_is_branded == 0] <- "No"
fulldata$prop_is_branded <- as.factor(fulldata$prop_is_branded)
fulldata$is_booking[fulldata$is_booking == 1] <- "Booked"
fulldata$is_booking[fulldata$is_booking == 0] <- "Did not book"
fulldata$is_booking <- as.factor(fulldata$is_booking) 


temp_price <- fulldata$hist_price_band
temp_pop <- fulldata$popularity_band
temp_dist <- fulldata$distance_band

dict1 <- c(
  "VL" = "Very Low",
  "L" = "Low",
  "M" = "Medium", 
  "H" = "High", 
  "VH" = "Very High"
)
dict2 <- c(
  "VC" = "Very Close",
  "C" = "Close",
  "M" = "Medium",
  "F" = "Far",
  "VF" = "Very Far"
)

fulldata$hist_price_band <- dict1[temp_price]
fulldata$popularity_band <- dict1[temp_pop]
fulldata$distance_band <- dict2[temp_dist]

fulldata$hist_price_band <- factor(fulldata$hist_price_band, levels = c("Very Low", "Low", "Medium", "High", "Very High"))
fulldata$popularity_band <- factor(fulldata$popularity_band, levels = c("Very Low", "Low", "Medium", "High", "Very High"))
fulldata$distance_band <- factor(fulldata$distance_band, levels = c("Very Far", "Far", "Medium", "Close", "Very Close"))


# create new variables
fulldata$stay_length <- as.numeric(difftime(fulldata$srch_co, fulldata$srch_ci, units = "days"))
fulldata$abroad <- factor(NA, levels = c("Domestic", "International"))
fulldata$abroad[fulldata$user_location_country == fulldata$hotel_country] <- "Domestic"
fulldata$abroad[fulldata$user_location_country != fulldata$hotel_country] <- "International"

fulldata$User_Location <- factor(NA, levels = c(
  "US", "Not US"
))
dict_location <- function(x) {
  if(x == "UNITED STATES OF AMERICA") {
    return("US")
  }
  else {
    return("Not US")
  }
}
fulldata$User_Location <- apply(matrix(fulldata$user_location_country), 1, dict_location)

fulldata$month <- month(as.Date(fulldata$date_time))
dict_season <- function(month){
  if (month >= 3 & month < 6) {
    return("Spring")
  }
  if (month >= 6 & month < 9) {
    return("Summer")
  }
  if (month >= 9 & month < 12) {
    return("Fall")
  }
  return("Winter")
}
fulldata$season <- apply(matrix(fulldata$month), 1, dict_season)

# compute the average of all popularity scores for each column 
fulldata$pop_score <- fulldata %>% 
  select(starts_with("popular_")) %>%
  apply(1, function(x) {as.numeric(sum(x) * -1 - 300)})

# remove individual pop score columns
fulldata <- fulldata[, substr(colnames(fulldata), 1, 8) != "popular_"]
 
# remove missing & irrational data
fulldata$stay_length[fulldata$stay_length < 0] <- NA
fulldata$orig_destination_distance[fulldata$orig_destination_distance < 0] <- NA
fulldata <- fulldata %>% drop_na(stay_length, orig_destination_distance, pop_score)


######################## Shiny App ######################## 

ui <- navbarPage(
  title = "Agenda",
  tabPanel("Intro", 
           h1("Group 3 Project Presentation", align = "center"),
           plotOutput("intro")
           ),
  tabPanel("Plot 1",
           h1("How is the distance/length of travel related to the number of adults/children traveling?"),
           sidebarPanel(radioButtons(
             "plot1_input", 
             "User Cateogry",
             choices = c("all users", "only users who booked hotel", "only users who did not book hotel")
           )),
           mainPanel(plotlyOutput("distance"), plotlyOutput("length"))),
  tabPanel("Plot 2", 
           h1("What are the most popular destinations for US and Non-US users?"),
           sidebarPanel(radioButtons(
             "plot3_input",
             "Users Category", 
             choices = c("all users", "us users", "non-us users")
           )),
           plotOutput("dest", height = 600, width = 1300),
           imageOutput("image")),
  tabPanel("Plot 3",
           sidebarPanel(
             selectInput("question_num", "Research Question",
                         c(Q1_Clicks_Bookings = "q1", Q2_Hotel_Likeability = "q2")),
             width = 15
           ),

           conditionalPanel(
             condition = "input.question_num == 'q1'",
             h1("How do the numbers of clicks and bookings vary for different hotels?"),
             sidebarPanel(
               radioButtons("show_p1", "Show Plot", choices = c("no", "yes")),
               radioButtons("response", "Response Variable", 
                            choices = c("clicks", "bookings")),
               radioButtons("explanatory", "Explanatory Variable", 
                            choices = c("is branded hotel", "hotel star rating",
                                        "hotel price band", "hotel popularity band"))
             )),
           conditionalPanel(
             condition = "input.question_num == 'q2'",
             h1("How do price/distance/popularity influence the likeability of hotels?"),
             sidebarPanel(
               radioButtons("show_p2", "Show Plot", choices = c("no", "yes")),
               radioButtons(
                 "like_response", "Response Variable", 
                 choices = c("length of stay", "hotel star rating", "travel-related facets score")
               ),
               radioButtons(
                 "like_explanatory", "Explanatory Variable",
                 choices = c("popularity band", "price band", "distance band")
               ),
             )),
           mainPanel(plotlyOutput("bookings_clicks")),
           mainPanel(plotlyOutput("likeability"))
           )
######################## Some Extra Plots ######################## 
#  tabPanel("Plot 4", 
#           h1("How do hotel preferences differ for domestic and international travelers?"),
#           sidebarPanel(radioButtons(
#             "plot2_input", 
#             "Preference Variable",
#             choices = c("is branded hotel", "hotel star rating", "hotel price band", "hotel popularity band")
#           )),
#           mainPanel(plotlyOutput("preferences"))
#           ),
#  tabPanel("Plot 5",
#           h1("How does percent booking change across seasons?"),
#           plotlyOutput("season")),
#  tabPanel("Plot 6",
#           h1("How does the number of bookings/clicks vary for different times in the day?"),
#           plotlyOutput("hour")),
)

server <- function(input, output) {
  
  output$intro <- renderPlot({
    df <- data.frame(X = c(5,5,5,5,5), Y = c(1,3,5,7,9), Members = c("Ryan Charter", "David Chen", "Ria Krishna", "Fatim Majumder", "Mia Roselinsky"))
    ggplot(df, aes(x = X, y = Y)) +
      geom_point(color = "white") +
      geom_label(aes(x = X + 5, y = Y, label = Members), show.legend = F, size = 13, nudge_y = 1) + 
      ggtitle("**Members**", ) +
      xlim(5, 15) +
      ylim(1, 11) +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(), 
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 30))
      
  })
  
  output$distance <- renderPlotly({
    if (input$plot1_input == "all users") {
      sub_data = fulldata
    }
    
    if (input$plot1_input == "only users who booked hotel") {
      sub_data = fulldata %>% filter(is_booking == "Booked")
    }
    
    if (input$plot1_input == "only users who did not book hotel") {
      sub_data = fulldata %>% filter(is_booking == "Did not book")
    } 
    
    fit_adult <- lm(orig_destination_distance ~ srch_adults_cnt, data = sub_data)
    fit_children <- lm(orig_destination_distance ~ srch_children_cnt, data = sub_data)
    
    plot_adult <- plot_ly(sub_data, x = ~srch_adults_cnt) %>%
      add_markers(y = ~orig_destination_distance) %>%
      add_lines(x = ~srch_adults_cnt, y = fitted(fit_adult)) %>%
      layout(xaxis = list(title = "Number of Adults"), 
             yaxis = list(title = "User Distance from The Booked Hotel"),
             showlegend = F)
    plot_children <- plot_ly(sub_data, x = ~srch_children_cnt) %>%
      add_markers(y = ~orig_destination_distance) %>%
      add_lines(x = ~srch_children_cnt, y = fitted(fit_children))  %>%
      layout(xaxis = list(title = "Number of Childen"),
             showlegend = F)
    subplot(plot_adult, plot_children, nrows = 1, shareY = T, shareX = T) %>%
      layout(title = "<b> Distance <b>")
    
  })
  
  output$length <- renderPlotly({
    if (input$plot1_input == "all users") {
      sub_data <- fulldata
    }
    
    if (input$plot1_input == "only users who booked hotel") {
      sub_data <- fulldata %>% filter(is_booking == "Booked")
    }
    
    if (input$plot1_input == "only users who did not book hotel") {
      sub_data <- fulldata %>% filter(is_booking == "Did not book")
    } 
    
    fit_adult <- lm(stay_length ~ srch_adults_cnt, data = sub_data)
    fit_children <- lm(stay_length ~ srch_children_cnt, data = sub_data)
    
    plot_adult <- plot_ly(sub_data, x = ~srch_adults_cnt) %>%
      add_markers(y = ~stay_length) %>%
      add_lines(x = ~srch_adults_cnt, y = fitted(fit_adult)) %>%
      add_text(x = 10, y = 10, text = "100") %>%
      layout(xaxis = list(title = "Number of Adults"), 
             yaxis = list(title = "User Intended Length of Stay"), 
             showlegend = F)
    plot_children <- plot_ly(sub_data, x = ~srch_children_cnt) %>%
      add_markers(y = ~stay_length) %>%
      add_lines(x = ~srch_children_cnt, y = fitted(fit_children))  %>%
      layout(xaxis = list(title = "Number of Childen"),
             showlegend = F)
    subplot(plot_adult, plot_children, nrows = 1, shareY = T, shareX = T) %>%
      layout(title = "<b> Length <b>")
  })
  
  output$preferences <- renderPlotly({
    if (input$plot2_input == "is branded hotel") {
      table <- fulldata %>%
        group_by(abroad, prop_is_branded) %>%
        summarize(count = n()) %>%
        mutate(prop = count / sum(count))
      
      plot_ly(table, x = ~abroad, y = ~prop, color = ~prop_is_branded) %>%
        add_bars() %>%
        layout(xaxis = list(title = "Hotel Location"), 
               yaxis = list(title = "Percentage of Users"),
               legend = list(title = list(text = "<b> Whether the Hotel Is Branded <b>")),
               title = list(text = "<b> Is Branded Hotel <b>", x = 0.3))
    }
    else if (input$plot2_input == "hotel star rating") {
      table <- fulldata %>%
        group_by(abroad, prop_starrating_cat) %>%
        summarize(count = n()) %>%
        mutate(prop = count / sum(count))
      
      plot_ly(table, x = ~abroad, y = ~prop, color = ~prop_starrating_cat) %>%
        add_bars() %>%
        layout(xaxis = list(title = "Hotel Location"), 
               yaxis = list(title = "Percentage of Users"),
               legend = list(title = list(text = "<b> Hotel Star Rating <b>")),
               title = list(text = "<b> Hotel Star Rating <b>", x = 0.3))
    }
    else if (input$plot2_input == "hotel price band") {
      table <- fulldata %>%
        group_by(abroad, hist_price_band) %>%
        summarize(count = n()) %>%
        mutate(prop = count / sum(count))
      
      plot_ly(table, x = ~abroad, y = ~prop, color = ~hist_price_band) %>%
        add_bars() %>%
        layout(xaxis = list(title = "Hotel Location"), 
               yaxis = list(title = "Percentage of Users"),
               legend = list(title = list(text = "<b> Hotel Price Ragne <b>")),
               title = list(text = "<b> Hotel Price Band <b>", x = 0.3))
    }
    
    else if (input$plot2_input == "hotel popularity band") {
      table <- fulldata %>%
        group_by(abroad, popularity_band) %>%
        summarize(count = n()) %>%
        mutate(prop = count / sum(count))
      
      plot_ly(table, x = ~abroad, y = ~prop, color = ~popularity_band) %>%
        add_bars() %>%
        layout(xaxis = list(title = "Hotel Location"), 
               yaxis = list(title = "Percentage of Users"),
               legend = list(title = list(text = "<b> Hotel Popularity <b>")),
               title = list(text = "<b> Hotel Popularity Band <b>", x = 0.3))
    }
  })
  
  output$dest <- renderPlot({
    library(ggmap)
    library(ggplot2)
    if (input$plot3_input == "all users") {
      world <- map_data("world")
      
      ggplot() +
        geom_polygon(data = world, aes(x = long, y = lat, group = group),
                     fill = "white", color = "black") +
        geom_point(data = fulldata, aes(x = srch_destination_longitude,
                                        y = srch_destination_latitude,
                                        color = User_Location),
                   alpha = 0.5) +
        scale_color_manual(values = c("red", "blue")) + 
        ggtitle("Popular Destinations of US and Non-US Users") +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 15),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) 
    }
    else {
      if (input$plot3_input == "us users") {
        data <- fulldata %>% filter(User_Location == "US")
        choose_color <- "blue"
      }
      else if (input$plot3_input == "non-us users") {
        data <- fulldata %>% filter(User_Location == "Not US")
        choose_color <- "red"
      }
      
      world <- map_data("world")
      
      ggplot() +
        geom_polygon(data = world, aes(x = long, y = lat, group = group), 
                     fill = "white", color = "black") +
        geom_point(data = data, aes(x = srch_destination_longitude, 
                                    y = srch_destination_latitude), 
                   alpha = 0.5, color = choose_color) +
        ggtitle("Popular Destinations of US and Non-US Users") +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) 
    }
  })
  
  output$season<- renderPlotly({
    season_tab <- fulldata %>%
      group_by(season, is_booking) %>%
      summarize(n = n()) %>%
      mutate(prop = n / sum(n))
    
    plot_ly(season_tab, x = ~season, y = ~prop, color = ~factor(is_booking)) %>%
      add_bars() %>%
      layout(xaxis = list(title = "Season"),
             yaxis = list(title = "Percent of Users"),
             title = "Booking by Season")
  })
  
  output$hour <- renderPlotly({
    bookings_tab <- fulldata %>%
      group_by(hour, is_booking) %>%
      summarize(count = n()) %>%
      filter(is_booking == "Booked")
      
    plot_ly(bookings_tab, x = ~hour, y = ~count, type = "scatter", mode = "line") %>%
      layout(xaxis = list(title = "Hour of Day"),
             yaxis = list(title = "Number of Bookings"), 
             title = "Booking by Hour of Day")
  })
  
  output$bookings_clicks <- renderPlotly({
    if (input$show_p1 == "no") {
      plotly_empty(type = "scatter", mode = "markers", width = 0.1, height = 0.1)
    }
    else {
      if (input$response == "clicks") {
        data <- fulldata %>% filter(is_booking == "Did not book")
        response <- "Clicks"
      }
      else if (input$response == "bookings") {
        data <- fulldata %>% filter(is_booking == "Booked")
        response <- "Bookings"
      }
    
      if (input$explanatory == "is branded hotel") {
        explanatory <- "Brand"
        tab <- data %>%
          group_by(prop_is_branded, is_booking) %>%
          summarize(count = n())
        
        plot_ly(tab, x = ~prop_is_branded, y = ~count, color = ~prop_is_branded) %>%
          add_bars() %>%
          layout(xaxis = list(title = "Is Branded Hotel"),
                 yaxis = list(title = paste("Number of ", response)),
                 title = paste(response, "by", explanatory), 
                 showlegend = F)
      }
      else if (input$explanatory == "hotel star rating") {
        explanatory <- "Star Rating"
        tab <- data %>%
          group_by(prop_starrating_cat, is_booking) %>%
          summarize(count = n())
        
        plot_ly(tab, x = ~prop_starrating_cat, y = ~count, color = ~prop_starrating_cat) %>%
          add_bars() %>%
          layout(xaxis = list(title = "Hotel Star Rating"),
                 yaxis = list(title = paste("Number of ", response)),
                 title = paste(response, "by", explanatory),
                 showlegend = F)
      }
      else if (input$explanatory == "hotel price band") {
        explanatory <- "Price Band"
        tab <- data %>%
          group_by(hist_price_band, is_booking) %>%
          summarize(count = n())
        
        plot_ly(tab, x = ~hist_price_band, y = ~count, color = ~hist_price_band) %>%
          add_bars() %>%
          layout(xaxis = list(title = "Hotel Price Range"),
                 yaxis = list(title = paste("Number of ", response)),
                 title = paste(response, "by", explanatory),
                 showlegend = F)
      }
      else if (input$explanatory == "hotel popularity band") {
        explanatory <- "Popularity Band"
        tab <- data %>%
          group_by(popularity_band, is_booking) %>%
          summarize(count = n())
        
        plot_ly(tab, x = ~popularity_band, y = ~count, color = ~popularity_band) %>%
          add_bars() %>%
          layout(xaxis = list(title = "Hotel Popularity Band"),
                 yaxis = list(title = paste("Number of ", response)),
                 title = paste(response, "by", explanatory), 
                 showlegend = F)
      }
    }
  })
  
  output$likeability <- renderPlotly({
    if (input$show_p2 == "no") {
      plotly_empty(type = "scatter", mode = "markers", width = 0.1, height = 0.1)
    }
    else {
      if (input$like_response == "length of stay") {
        res_name <- "Length of Stay"
        if (input$like_explanatory == "popularity band") {
          exp_name <- "Popularity Band"
          tab <- fulldata %>%
            group_by(popularity_band) %>%
            summarize(avg = mean(stay_length))
          
          plot_ly(tab, x = ~popularity_band, y = ~avg, color = ~popularity_band) %>%
            add_bars() %>%
            layout(xaxis = list(title = exp_name),
                   yaxis = list(title = res_name), 
                   title = paste(res_name, "by", exp_name),
                   showlegend = F)
        }
        else if (input$like_explanatory == "price band") {
          exp_name <- "Price Range"
          tab <- fulldata %>%
            group_by(hist_price_band) %>%
            summarize(avg = mean(stay_length))
          
          plot_ly(tab, x = ~hist_price_band, y = ~avg, color = ~hist_price_band) %>%
            add_bars() %>%
            layout(xaxis = list(title = exp_name),
                   yaxis = list(title = res_name), 
                   title = paste(res_name, "by", exp_name),
                   showlegend = F)
        }
        else if (input$like_explanatory == "distance band") {
          exp_name <- "Distance Band"
          tab <- fulldata %>%
            group_by(distance_band) %>%
            summarize(avg = mean(stay_length))
          
          plot_ly(tab, x = ~distance_band, y = ~avg, color = ~distance_band) %>%
            add_bars() %>%
            layout(xaxis = list(title = exp_name),
                   yaxis = list(title = res_name), 
                   title = paste(res_name, "by", exp_name),
                   showlegend = F)
        }
      }
      else if (input$like_response == "hotel star rating") {
        res_name <- "Hotel Star Rating"
        if (input$like_explanatory == "popularity band") {
          exp_name <- "Popularity Band"
          tab <- fulldata %>%
            group_by(popularity_band) %>%
            summarize(avg = mean(prop_starrating))
          
          plot_ly(tab, x = ~popularity_band, y = ~avg, color = ~popularity_band) %>%
            add_bars() %>%
            layout(xaxis = list(title = exp_name),
                   yaxis = list(title = res_name), 
                   title = paste(res_name, "by", exp_name),
                   showlegend = F)
        }
        else if (input$like_explanatory == "price band") {
          exp_name <- "Price Range"
          tab <- fulldata %>%
            group_by(hist_price_band) %>%
            summarize(avg = mean(prop_starrating))
          
          plot_ly(tab, x = ~hist_price_band, y = ~avg, color = ~hist_price_band) %>%
            add_bars() %>%
            layout(xaxis = list(title = exp_name),
                   yaxis = list(title = res_name), 
                   title = paste(res_name, "by", exp_name),
                   showlegend = F)
        }
        else if (input$like_explanatory == "distance band") {
          exp_name <- "Distance Band"
          tab <- fulldata %>%
            group_by(distance_band) %>%
            summarize(avg = mean(prop_starrating))
          
          plot_ly(tab, x = ~distance_band, y = ~avg, color = ~distance_band) %>%
            add_bars() %>%
            layout(xaxis = list(title = exp_name),
                   yaxis = list(title = res_name), 
                   title = paste(res_name, "by", exp_name),
                   showlegend = F)
        }
      }
      else if (input$like_response == "travel-related facets score") {
        res_name <- "Travel-Related Facets Score"
        if (input$like_explanatory == "popularity band") {
          exp_name <- "Popularity Band"
          tab <- fulldata %>%
            group_by(popularity_band) %>%
            summarize(avg = mean(pop_score))
          
          plot_ly(tab, x = ~popularity_band, y = ~avg, color = ~popularity_band) %>%
            add_bars() %>%
            layout(xaxis = list(title = exp_name),
                   yaxis = list(title = res_name), 
                   title = paste(res_name, "by", exp_name),
                   showlegend = F)
        }
        else if (input$like_explanatory == "price band") {
          exp_name <- "Price Range"
          tab <- fulldata %>%
            group_by(hist_price_band) %>%
            summarize(avg = mean(pop_score))
          
          plot_ly(tab, x = ~hist_price_band, y = ~avg, color = ~hist_price_band) %>%
            add_bars() %>%
            layout(xaxis = list(title = exp_name),
                   yaxis = list(title = res_name), 
                   title = paste(res_name, "by", exp_name),
                   showlegend = F)
        }
        else if (input$like_explanatory == "distance band") {
          exp_name <- "Distance Band"
          tab <- fulldata %>%
            group_by(distance_band) %>%
            summarize(avg = mean(pop_score))
          
          plot_ly(tab, x = ~distance_band, y = ~avg, color = ~distance_band) %>%
            add_bars() %>%
            layout(xaxis = list(title = exp_name),
                   yaxis = list(title = res_name), 
                   title = paste(res_name, "by", exp_name),
                   showlegend = F)
        }
      }
    }
  })
}

shinyApp(ui, server)