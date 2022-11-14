library(shiny)
library(gridExtra)
library(plotly)
library(shinyWidgets)
library(NatParksPalettes)
library(tidyverse)

# Data source
combined_data = readRDS(file="nchs_viz_data.rds")
# Possible breakdowns
subgroup_types = c("All", "Age", "Race", "Region", "Sex")  
# Function to make sure y axis labels don't have decimals
scaleFUN <- function(x) sprintf("%.0f", x)

######
# UI #
######
ui <- fluidPage(
  titlePanel("Impact of heat events on heat-related ER visits"),
  
  # Drop down boxes, make each an equal width
  fluidRow(
    column(width = 4, 
           selectInput("temp_type",
                       h4("Temperature Measurement"),
                       c("Maximum" = "maximum",
                         "Minimum" = "minimum", 
                         "Days Over 32.2\u00B0C (90\u00B0F)" = "n_days_over90"),
                       selected="Maximum",
                       width = "400px")),
    
    column(width = 4,
           selectInput("breakdown",
                       h4("Breakdown Type"),
                       subgroup_types,
                       selected="All",
                       width = "400px")),
    
    column(width = 4, uiOutput("date_range", width = "400px"))
  ),   
  
  # Line plot
  fluidRow(column(width = 12, h3("Temperature and heat-related ER visits over time"), plotlyOutput("linePlot", height="400px"))),
  
  # Bar plot and scatter plot in same row
  fluidRow(
    column(width = 6, h3("Differences in demographic information"), plotlyOutput("barPlot", height="400px")), 
    column(width = 6, h3("Temperature and heat-related ER visits"), plotlyOutput("scatterPlot", height="400px")) 
  )
)

server <- function(input, output) {
  
  output$date_range <- renderUI({
    tagList(setSliderColor("#0166A2", sliderId = 1), #put vectors here to change the colors
            sliderInput("dates", h4("Dates"), 
                        min = min(combined_data$date), 
                        max = max(combined_data$date), 
                        value = c(min(combined_data$date), max(combined_data$date)),
                        timeFormat = "%b %Y"))
  })
  
  # Filter data for outcome displays by date and breakdown
  filtered_df = reactive({
    if(is.null(input$dates)){
      # On initialization, dates is null so don't filter
      combined_data %>% filter(cat == input$breakdown,
                               type == input$temp_type)
    } else
      combined_data %>% filter(date >= input$dates[1], 
                               date <= input$dates[2],
                               cat == input$breakdown,
                               type == input$temp_type) 
  })
  
  # Filter data for temperature graph by date and breakdown
  temperature_df = reactive({
    if(input$breakdown=="Region"){
      # Region will have 4 lines for the data in the 4 regions
      combined_data %>% 
        filter(date >= input$dates[1],
               date <= input$dates[2],
               cat == "Region",
               type == input$temp_type)
    } else if (is.null(input$dates)) {
      # Everything else will have one line summarizing the whole US
      # If dates is null, don't filter on date
      combined_data %>%
        filter(cat == "All",
               type == input$temp_type)
    } else {
      # Everything else will have one line summarizing the whole US
      combined_data %>%
        filter(date >= input$dates[1],
               date <= input$dates[2],
               cat == "All",
               type == input$temp_type)
    }
  })
  
  
  temp_type_label = reactive({case_when(input$temp_type == "minimum" ~ "Min. Temp. (\u00B0C)",
                                        input$temp_type == "maximum" ~ "Max. Temp. (\u00B0C)",
                                        input$temp_type == "n_days_over90" ~ "Days > 32.2\u00B0C (90\u00B0F)")})
  
  color_pal = reactive({
    n_colors = filtered_df() %>% pull(subg) %>% unique() %>% length
    #lacroix_palette("PeachPear", n = n_colors, type = "discrete")
    colors = natparks.pals(name="Yellowstone", n = n_colors, type = "discrete")
    colors[4] = "#264C31"
    colors
  })
  
  # Line plot for temperature and heat-related ER admissions over time
  output$linePlot <- renderPlotly({
    # Plot heat-related ER visits
    if (input$breakdown == "All") { 
      amb_plt = ggplot(filtered_df(), 
                       aes(x = date, y = count, group=subg, color=subg,
                           text = glue::glue("Date: {date}
                                              Cases: {count}
                                              {temp_type_label()}: {round(value, 2)}"))) 
    } else {
      amb_plt = ggplot(filtered_df(), 
                       aes(x = date, y = count, group=subg, color=subg, 
                           text = glue::glue("{input$breakdown}: {subg}
                                              Date: {date}
                                              Cases: {count}
                                              {temp_type_label()}: {round(value, 2)}"))) 
    }
    
    amb_plt = amb_plt +
      geom_line() +
      scale_color_manual(values = color_pal(), name = input$breakdown) +
      theme_minimal() +
      xlab("") + 
      ylab("Number of ER Visits") + 
      theme(legend.position = "top", 
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            plot.margin=unit(c(5.5,5.5,0,5.5),"pt")) 
    
    if (input$breakdown == "Region"){
      temp_plt = ggplot(temperature_df(),
                        aes(x = date, y=value, group=subg, color=subg,
                            text = glue::glue("Region: {subg}\nDate: {date}\nCases: {count}\n{temp_type_label()}: {round(value, 2)}"))) 
    } else if (input$breakdown == "Region") {
      temp_plt = ggplot(temperature_df(),
                        aes(x = date, y=value, group=subg, color=subg,
                            text = glue::glue("Date: {date}\nCases: {count}\n{temp_type_label()}: {round(value, 2)}"))) 
    } else {
      temp_plt = ggplot(temperature_df(),
                        aes(x = date, y=value, group=subg, color=subg,
                            text = glue::glue("Date: {date}\n{temp_type_label()}: {round(value, 2)}"))) 
    }
    temp_plt = temp_plt +
      scale_color_manual(values = color_pal(), name = input$breakdown) + 
      geom_line() +
      xlab("Date") +
      ylab(temp_type_label()) +
      theme_minimal() +
      theme(plot.margin=unit(c(0,5.5,5.5,5.5),"pt")) +
      scale_y_continuous(labels=scaleFUN)
    
    # Stack ambulatory and temperature plots
    subplot(list(style(ggplotly(amb_plt, tooltip = c("text")), showlegend = ifelse(input$breakdown == "All", F, T)), 
                 style(ggplotly(temp_plt, tooltip = c("text")), showlegend = F)),
            nrows = 2, 
            heights = c(0.7, 0.3), 
            shareX = TRUE, 
            titleY = TRUE) %>%
      layout(hovermode = "x unified", 
             xaxis = list(titlefont = list(size = 1)),
             yaxis = list(titlefont = list(size = 1)),
             legend = list(orientation = 'h', 
                           x = 0, y = 100))
  })
  
  # bar plot for comparison of demographics
  output$barPlot <- renderPlotly({
    bar_plt_df = filtered_df() %>%
      group_by(subg) %>% 
      summarize(count = sum(count), rep_pop = sum(rep_pop)) %>% 
      pivot_longer(cols = c(count, rep_pop),
                   names_to = "x_axis",
                   values_to = "num_people") %>% 
      group_by(x_axis) %>% 
      mutate(prop = round((num_people / sum(num_people, na.rm = T)) * 100, 2))
    
    if (input$breakdown == "All") { 
      bar_plt = ggplot(bar_plt_df,
                       aes(x=x_axis, y=num_people, fill=subg, 
                           text = glue::glue("All
                                              Percent: {prop}%")))
    } else {
      bar_plt = ggplot(bar_plt_df,
                       aes(x=x_axis, y=num_people, fill=subg, 
                           text = glue::glue("{input$breakdown}: {subg}
                                  Percent: {prop}%")))
    }
    
    bar_plt = bar_plt +
      geom_bar(position="fill", stat="identity") + 
      scale_x_discrete(name="", labels=c("count" = "Heat Outcomes",
                                         "rep_pop" = "Population")) +
      ylab("Proportion") +
      theme_minimal() + 
      scale_fill_manual(values = color_pal(), name = input$breakdown) + 
      theme(legend.position = ifelse(input$breakdown == "All", "none", "top"))
    
    barplot_plotly = style(ggplotly(bar_plt, tooltip = c("text")), showlegend = ifelse(input$breakdown == "All", F, T))
    
    if (input$breakdown != "All") {
      barplot_plotly = barplot_plotly %>% layout(legend = list(orientation = 'h',  x = 0, y = 100))
    }
    
    barplot_plotly
  })
  
  # scatterplot of temperature vs number of visits
  output$scatterPlot <- renderPlotly({
    
    if (input$breakdown == "All") { 
      scatter_plt = ggplot(filtered_df(), 
                           aes(x = value, y = count, group=subg, color=subg,
                               text = glue::glue("Date: {date}
                                                  {temp_type_label()}: {round(value, 2)}
                                                  Number of Case: {count}"))) 
    } else {
      scatter_plt = ggplot(filtered_df(), 
                           aes(x = value, y = count, group=subg, color=subg,
                               text = glue::glue("{input$breakdown}: {subg}
                                                  Date: {date}
                                                  {temp_type_label()}: {round(value, 2)}
                                                  Number of Case: {count}"))) 
    }
    
    scatter_plt = scatter_plt + 
      geom_point() + 
      theme_minimal() +
      scale_color_manual(values = color_pal(), name = input$breakdown)+
      xlab(temp_type_label()) + 
      ylab("Number of ER Visits")  
    
    scatter_plotly = ggplotly(scatter_plt, tooltip = c("text")) %>% layout(showlegend = ifelse(input$breakdown == "All", F, T))
    
    if (input$breakdown != "All") {
      scatter_plotly = scatter_plotly %>% layout(legend = list(orientation = 'h',  x = 0, y = 100))
    }
    
    scatter_plotly
  })
}

# Run the application 
shinyApp(ui = ui, server = server)