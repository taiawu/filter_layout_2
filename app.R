#### main app
library(shiny)
library(tidyverse)
library(shinyWidgets)

contract_layout <- function(layout) {
    layout %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(-well, names_to = "variable", values_to = "value") %>%
        unite(picker_val, c(variable, value), sep  = "<LAYOUT_GROUP>", remove = FALSE)
}

expand_layout <- function(layout_cont) {
    layout_cont %>% 
        select(-picker_val) %>%
        pivot_wider(id_cols = well, names_from = variable, values_from = value)
    
}

layout_to_picker_list <- function(layout_cont) {
    
    layout_exp <- expand_layout(layout_cont)
    
    layout_names <- layout_exp %>% 
        as.list() %>% 
        lapply(unique)
    
    layout_vals <- map2(layout_names, names(layout_names), paste, sep = "<LAYOUT_GROUP>")
    
    layout_list <- map2(layout_vals, layout_names, set_names)  %>%
        .[order(sapply(., length))]
    
}

filter_picked <- function(layout, picked_list) {
    
    pick_these <- picked_list %>% flatten() %>% as_vector()
    
    layout_picker <-  layout %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(cols = c(dye, dye_conc), names_to = "group", values_to = "value") %>%
        unite(picker_val, c(value, group), sep  = "<LAYOUT_GROUP>", remove = FALSE)  %>%
        filter(picker_val %in% pick_these) 
    
    layout_picker
}

### start app

    ui <- fluidPage(
        tags$h2("Select / Deselect all"),
        uiOutput("plot_picker"),
        verbatimTextOutput("r1"),
        tags$br(),

        uiOutput("fit_picker"),
        verbatimTextOutput("r2")
    )

    server <- function(input, output, session) {
        
        ### layout munching
        layout_ex <- reactive(tibble(well = c("A1", "A2", "A3", "A4"),
                            dye = c("A001", "A001", "A001", "A004"),
                            dye_conc = c(1, 2, 3, 4)))
        
        layout_cont <- reactive(contract_layout(layout_ex()))
        
        picker_list_raw <-  reactive(layout_to_picker_list(layout_cont()))

        output$plot_picker <- renderUI({

             pickerInput(
                 inputId = "plot_these",
                 choices = picker_list_raw(),
                 multiple = TRUE,
                 options = pickerOptions(
                     actionsBox = TRUE,
                     selectAllText = "Plot all",
                     deselectAllText = "Plot none",
                     title = "Select data to plot",
                     tickIcon = "glyphicon-eye-open",
                     selectedTextFormat= "count",
                     countSelectedText = "Plotting from {0} selections"
                 )
             )
         })
        
        output$fit_picker <- renderUI({
            
            pickerInput(
                inputId = "fit_these",
                choices = picker_list_raw(),
                multiple = TRUE,
                options = pickerOptions(
                    actionsBox = TRUE,
                    selectAllText = "Fit all",
                    deselectAllText = "Fit none",
                    title = "Select data to fit",
                    tickIcon = "glyphicon-eye-open",
                    selectedTextFormat= "count",
                    countSelectedText = "Fitting from {0} selections"
                )
            )
        })
         
         # observeEvent(input$plot_these, {
         # to update/grey out choices no longer available, see 
         #  https://rdrr.io/cran/shinyWidgets/man/updatePickerInput.html
         #     mtcars2 <- mtcars[mtcars$mpg >= input$up, ]
         #     
         #     # Method 1
         #     updatePickerInput(session = session, inputId = "p1",
         #                       choices = rownames(mtcars2))
         #     
         #     # Method 2
         #     disabled_choices <- !rownames(mtcars) %in% rownames(mtcars2) # a boolean vector
         #     
         #     updatePickerInput(
         #         session = session, inputId = "p2",
         #         choices = rownames(mtcars),
         #         choicesOpt = list(
         #             disabled = disabled_choices,
         #             style = ifelse(disabled_choices,
         #                            yes = "color: rgba(119, 119, 119, 0.5);",
         #                            no = "")
         #         )
         #     )
         # }, ignoreInit = TRUE)
        output$r1 <- renderPrint(input$plot_these)
        output$r2 <- renderPrint(input$fit_these)

    }

    shinyApp(ui = ui, server = server)


