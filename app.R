# library(shiny)
# library(bslib)
# library(ggplot2)
# library(plotly)
# library(dplyr)
# library(data.table)
# library(purrr)
# library(janitor)
# library(stringr)
# library(tidyr)
# library(lubridate)
# library(DT)
# 
# options(shiny.maxRequestSize = 100*1024^2)
# 
# # Define UI ----
# ui <- page_sidebar(
#   
#   # App title ----
#   title = "Multi-Gas File Processor",
#   
#   # Sidebar panel for inputs ----
#   sidebar = sidebar(
#     
#     # Input: Select text files ----
#     fileInput(
#       "file_input",
#       "Choose Text Files",
#       multiple = TRUE,
#       accept = c(
#         "text/plain", 
#         ".txt"
#       )
#     ),
#     
#     # Horizontal line ----
#     tags$hr(),
#     
#     # Input: Toggle for interactive graph
#     checkboxInput("interactive", "Interactive Graph", value = FALSE),
#     
#     # Input: Select Gas for Plotting
#     selectInput("gas_choice", "Select Gas", choices = NULL)
#     
#   ),
#   
#   # Main panel for displaying outputs ----
#   navset_card_underline(
#     
#     nav_panel("Overview Plot", plotOutput("overview_plot_static"), height='100%'),
#     
#     nav_panel("Data Preview", DTOutput("data_preview")),
#     
#     nav_panel("Plot", uiOutput("plot_ui"))
#   )
# )
# 
# # Define Server ----
# server <- function(input, output, session) {
#   
#   # Reactive to process uploaded files
#   processed_data <- reactive({
#     req(input$file_input)
#     
#     withProgress(message = 'Preparing data...', value = 0, {
#       # Load and bind files
#       incProgress(0.2, detail='Reading files')
#       multi_files <- input$file_input$datapath
#       one_data <- multi_files |>
#         purrr::map(~ data.table::fread(.x, fill = TRUE, sep = '\t', encoding = "Latin-1")) %>%
#         bind_rows()
#       
#       incProgress(0.2, detail='Renaming columns')
#       # Rename columns
#       new_colnames <- names(one_data) |> 
#         stringr::str_replace("^_", 'UNDERSCORE_') |> 
#         stringr::str_replace("%", 'percent') |> 
#         janitor::make_clean_names(case = 'none', 
#                                   replace = janitor:::mu_to_u, 
#                                   unique_sep = '_duplicate')
#       
#       colnames(one_data) <- new_colnames
#       
#       incProgress(0.2, detail='Subsetting data')
#       # Select required columns and transform DATE
#       multi_data <- one_data |>
#         dplyr::select(
#           'DATE',
#           'CHANNEL',
#           starts_with(c('N2O', 'CH4', 'O2', 'H2O', 'NH3', 'CO2', "H2"))
#         ) |>
#         mutate(DATE = lubridate::dmy_hms(DATE))
#       
#       # Pivot data to long format
#       multi_long <- multi_data |>
#         pivot_longer(
#           cols = starts_with(c('N2O', 'CH4', 'O2', 'H2O', 'NH3', 'CO2', 'H2')),
#           names_sep = '_',
#           names_to = c('Gas', 'Units', 'Chamber')
#         )
#       
#       incProgress(0.2, detail='Pivot and clean')
#       # Filter data based on chamber/channel
#       multi_long_sub <- multi_long |>
#         mutate(
#           Chamber = str_remove(Chamber, 'V'),
#           Channel_chamber_number = CHANNEL + 1
#         ) |>
#         filter(Chamber == Channel_chamber_number)
#       
#       # Update Gas selection choices dynamically
#       gas_list <- unique(multi_long_sub$Gas)
#       updateSelectInput(session, "gas_choice", choices = gas_list)  
#       incProgress(0.2, detail='Finish')
#     })
#     
#     
#     return(multi_long_sub)
#   })
#   
#   # Output: Data preview table
#   output$data_preview <- renderDT({
#     req(processed_data())
#     
#     display_data <- processed_data() |> 
#       mutate(
#         Gas_Units = paste(Gas, Units, sep = "_"),
#       ) |> 
#       select(-Channel_chamber_number,
#              -Gas,
#              -Units,
#              -CHANNEL) |> 
#       pivot_wider(names_from = Gas_Units, values_from = value) |> 
#       mutate(        
#         DATE = format(DATE, "%d-%m-%Y %H:%M") 
#         )
#     
#     return(display_data)
#     
#   })
#   
#   # Output: Plot (static/interactive based on input)
#   output$plot_ui <- renderUI({
#     if (input$interactive) {
#       plotlyOutput("interactive_plot", height = '100%')
#     } else {
#       plotOutput("static_plot", height = '100%')
#     }
#   })
#   
#   # Static plot
#   output$static_plot <- renderPlot({
#     req(processed_data())
#     gas_plotter(input$gas_choice)
#   },
#   res = 120)
#   
#   # Interactive plot
#   output$interactive_plot <- renderPlotly({
#     req(processed_data())
#     ggplotly(gas_plotter(input$gas_choice), dynamicTicks = FALSE)
#     
#   })
#   
#   # Gas plotting function (based on user-selected gas)
#   gas_plotter <- function(gas_string) {
#     req(processed_data())
#     
#     processed_data() |>
#       filter(Gas == gas_string) |>
#       mutate(Chamber = as.numeric(Chamber) |> as.factor()) |>
#       ggplot(aes(x = DATE, y = value, colour = Chamber)) +
#       geom_point() +
#       geom_line() +
#       scale_color_viridis_d(option = 'H') +
#       theme_bw() +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#       facet_wrap(vars(Chamber), scales = 'free') +
#       labs(title = str_glue('{gas_string} from multi-gas file'))+
#       scale_x_datetime(date_breaks = '6 hour', date_labels = "%d-%m %H-%M") 
#   }
#   
# 
#   # Static Overview Plot
#   output$overview_plot_static <- renderPlot({
#     req(processed_data())
#     plot_overview(processed_data())
#   },
#   res = 100
#   )
# 
#   # Overview plotting function: Summarizes all chambers and gases
#   plot_overview <- function(data) {
#     data |>
#       mutate(Chamber = as.numeric(Chamber) |> as.factor()) |>
#       ggplot(aes(x = DATE, y = value, colour = Gas)) +
#       geom_line(linewidth=1.2) +
#       facet_grid(rows = vars(Gas), cols = vars(Chamber), scales = 'free') +
#       theme_bw() +
#       scale_color_viridis_d(option = 'H') +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#       labs(title = 'Overview of Multi-Gas Data', x = 'Date', y = 'Value')+
#       scale_x_datetime(date_breaks = '1 day', date_labels = "%d-%m") 
#   }
#   
# }
# 
# # Run the app ----
# shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(data.table)
library(purrr)
library(janitor)
library(stringr)
library(tidyr)
library(lubridate)
library(DT)
library(shinyWidgets)

options(shiny.maxRequestSize = 100*1024^2)

# Define UI ----
ui <- page_sidebar(
  
  # App title ----
  title = "Multi-Gas File Processor",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    
    # Input: Select text files ----
    fileInput(
      "file_input",
      "Choose Text Files",
      multiple = TRUE,
      accept = c(
        "text/plain", 
        ".txt"
      )
    ),
    
    # Horizontal line ----
    tags$hr(),
    
    
    # Conditional input elements that are only shown in the Plot tab
    conditionalPanel(
      condition = "input.navset == 'Plot'",  # Update the condition to check the ID of the navset_card_underline
      
      # Input: Toggle for interactive graph
      checkboxInput("interactive", "Interactive Graph", value = FALSE),
      
      # Input: Select Chamber for Plotting
      # selectizeInput("chamber_choice", "Select Chambers", choices = NULL, multiple = TRUE),
      pickerInput(
        inputId = "chamber_choice", 
        label = "Select:", 
        choices = NULL, 
        options = pickerOptions(
          actionsBox = TRUE, 
          size = 10,
          selectedTextFormat = "count > 3"
        ), 
        multiple = TRUE
      ),
      
      # Input: Select Gas for Plotting
      selectInput("gas_choice", "Select Gas", choices = NULL)
    )
  ),
  
  # Main panel for displaying outputs ----
  navset_card_underline(
    id = "navset",  # Give the navset_card_underline an ID so we can reference it in the conditionalPanel
    
    nav_panel("Overview Plot", plotOutput("overview_plot_static", height = '100%')),
    
    nav_panel("Data Preview", DTOutput("data_preview")),
    
    nav_panel("Plot", uiOutput("plot_ui"), value = "Plot")  # The value 'Plot' will be referenced in the conditionalPanel
  )
)

# Define Server ----
server <- function(input, output, session) {
  
  # Reactive to process uploaded files
  processed_data <- reactive({
    req(input$file_input)
    
    withProgress(message = 'Preparing data...', value = 0, {
      # Load and bind files
      incProgress(0.2, detail='Reading files')
      multi_files <- input$file_input$datapath
      one_data <- multi_files |>
        purrr::map(~ data.table::fread(.x, fill = TRUE, sep = '\t', encoding = "Latin-1")) %>%
        bind_rows()
      
      incProgress(0.2, detail='Renaming columns')
      # Rename columns
      new_colnames <- names(one_data) |> 
        stringr::str_replace("^_", 'UNDERSCORE_') |> 
        stringr::str_replace("%", 'percent') |> 
        janitor::make_clean_names(case = 'none', 
                                  replace = janitor:::mu_to_u, 
                                  unique_sep = '_duplicate')
      
      colnames(one_data) <- new_colnames
      
      incProgress(0.2, detail='Subsetting data')
      # Select required columns and transform DATE
      multi_data <- one_data |>
        dplyr::select(
          'DATE',
          'CHANNEL',
          starts_with(c('N2O', 'CH4', 'O2', 'H2O', 'NH3', 'CO2', "H2"))
        ) |>
        mutate(DATE = lubridate::dmy_hms(DATE))
      
      # Pivot data to long format
      multi_long <- multi_data |>
        pivot_longer(
          cols = starts_with(c('N2O', 'CH4', 'O2', 'H2O', 'NH3', 'CO2', 'H2')),
          names_sep = '_',
          names_to = c('Gas', 'Units', 'Chamber')
        )
      
      incProgress(0.2, detail='Pivot and clean')
      # Filter data based on chamber/channel
      multi_long_sub <- multi_long |>
        mutate(
          Chamber = str_remove(Chamber, 'V'),
          Channel_chamber_number = CHANNEL + 1
        ) |>
        filter(Chamber == Channel_chamber_number)
      
      # Update Gas and Chamber selection choices dynamically
      gas_list <- unique(multi_long_sub$Gas)
      chamber_list <- unique(multi_long_sub$Chamber)
      updateSelectInput(session, "gas_choice", choices = gas_list)  
      updatePickerInput(session, "chamber_choice", choices = chamber_list, selected = head(chamber_list,10))
      
      
      incProgress(0.2, detail='Finish')
    })
    
    return(multi_long_sub)
  })
  
  # Output: Data preview table with download buttons
  output$data_preview <- renderDT({
    req(processed_data())
    
    display_data <- processed_data() |> 
      mutate(
        Gas_Units = paste(Gas, Units, sep = "_")
      ) |> 
      select(-Channel_chamber_number,
             -Gas,
             -Units,
             -CHANNEL) |> 
      pivot_wider(names_from = Gas_Units, values_from = value) |> 
      mutate(        
        DATE = format(DATE, "%d-%m-%Y %H:%M") 
      )
    
    # Add Buttons for copy, CSV, Excel, PDF, and print
    datatable(display_data, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      pageLength = 10  # Optional: Set number of rows per page
    ))
  })
  
  
  
  # Output: Plot (static/interactive based on input)
  output$plot_ui <- renderUI({
    if (input$interactive) {
      plotlyOutput("interactive_plot", height = '100%')
    } else {
      plotOutput("static_plot", height = '100%')
    }
  })
  
  # Static plot
  output$static_plot <- renderPlot({
    req(processed_data())
    gas_plotter(input$gas_choice, input$chamber_choice)
  },
  res = 120)
  
  # Interactive plot
  output$interactive_plot <- renderPlotly({
    req(processed_data())
    ggplotly(gas_plotter(input$gas_choice, input$chamber_choice), dynamicTicks = FALSE)
  })
  
  # Gas plotting function (based on user-selected gas and chamber)
  gas_plotter <- function(gas_string, chamber_choices) {
    req(processed_data())
    
    processed_data() |>
      filter(Gas == gas_string, Chamber %in% chamber_choices) |>
      mutate(Chamber = as.numeric(Chamber) |> as.factor()) |>
      ggplot(aes(x = DATE, y = value, colour = Chamber)) +
      geom_point() +
      geom_line() +
      scale_color_viridis_d(option = 'H') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(vars(Chamber), scales = 'free') +
      labs(title = str_glue('{gas_string} from multi-gas file'))+
      scale_x_datetime(date_breaks = '6 hour', date_labels = "%d-%m %H-%M") 
  }
  
  # Static Overview Plot
  output$overview_plot_static <- renderPlot({
    req(processed_data())
    plot_overview(processed_data())
  },
  res = 100
  )
  
  # Overview plotting function: Summarizes all chambers and gases
  plot_overview <- function(data) {
    data |>
      mutate(Chamber = as.numeric(Chamber) |> as.factor()) |>
      ggplot(aes(x = DATE, y = value, colour = Gas)) +
      geom_line(linewidth=1.2) +
      facet_grid(rows = vars(Gas), cols = vars(Chamber), scales = 'free') +
      theme_bw() +
      scale_color_viridis_d(option = 'H') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = 'Overview of Multi-Gas Data', x = 'Date', y = 'Value') +
      scale_x_datetime(date_breaks = '1 day', date_labels = "%d-%m") 
  }
}

# Run the app ----
shinyApp(ui = ui, server = server)



