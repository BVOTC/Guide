###### Black Planners Literature Guide Shiny App ########

# Load Libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(shinythemes)
library(DT)

Sys.setenv(LANG = "en")

## Load data
#data <- read_xlsx("data/BlackPlannerGuide_Sample.xlsx")

## data <- data %>% 
#  rename("Urban Design" = "A", "Social Injustice" = "B", "Policing" = "C",
#         "Land Use" = "D", "Community" = "E")

#save(data, file = "data/data.Rdata")

load(file = "data/data.Rdata")

### UI ###

ui <- fluidPage(
  theme = shinytheme("slate"),
  includeCSS("www/bootstrap.css"),
  navbarPage("Black Planner's Guide",
             tabPanel("Search Engine",
                      titlePanel(h1("Black Planner's Resource Guide")),
                      br(),
                      hr(),
                      fluidRow(
                        column(6, offset = 3,
                               wellPanel(
                                 checkboxGroupInput(inputId = "keyword",
                                             label = h4("Themes and Keywords"),
                                             choices = c("Urban Design", "Social Injustice", "Policing",
                                                         "Land Use", "Community"),
                                             selected = "Community",
                                             inline = T),
                                 checkboxGroupInput(inputId = "type",
                                                    label = h4("Media Type"),
                                                    choices = unique(data$Media_Type),
                                                    selected = "Book",
                                                    inline = T),
                                 dateInput(inputId = "date",
                                           label = h4("Year"),
                                           format = "yyyy"))),
                      ),
                      fluidRow(
                        column(12,
                               dataTableOutput("table"))
                        )
                      ),
             tabPanel("Mission Statement",
                      titlePanel(h1("Mission Statement")),
                      br(),
                      hr(),
                      fluidRow(
                        column(5, offset = 1,
                               br(),
                               helpText(h3("Why We Created this Guide")),
                               helpText(p(style="text-align: justify;", 
                               "The purpose of this resource guide is two-fold: first,
                        to catalog the contributions of black researchers and practitioners
                        to the field of urban planning and second,
                        to amplify the unique voices and perspectives they bring to a field
                        that has been overwhelmingly dominated by white, cisgender,
                        heterosexual men in North America since its beginnings. 
                        As current students and alumni of urban planning programs,
                        we feel that the best way to channel our frustrations with the lack
                        of diversity present in our classrooms and curriculum is to lead by example
                        and demonstrate how we seek out and learn from Black planners and scholars.
                        This guide is just one part of a larger, nascent effort to organize students
                        and faculty around re-imagining what constitutes urban planning and who contributes
                        to that dialogue."))
                        )),
                      fluidRow(
                        column(5, offset = 6,
                               br(),
                               helpText(h3("Who We Are")),
                               helpText(p(style="text-align: justify;",
                               "One of the most important guiding questions in anti-racism teachings
                               is “Who am I/who are we to do this work?” We owe users of this guide our answer 
                               to that question so they can hold us accountable as we exercise our privileges to
                               fight anti-Black racism in the field of urban planning. First and foremost, we are
                               a group of largely non-Black graduate students and alumni whose lives have benefited
                               from intersections with other forms of privilege, including being male and cisgender
                               as well as having access to family wealth and higher education. Some of us also identify
                               as queer, feminist, and immigrants. We come to this work with a recognition that these
                               identities produce a number of limitations and blindspots, which is why we are calling
                               in everyone with an interest in tackling anti-Blackness within urban planning to collaborate
                               with us and critique our work.")),
                               helpText(p(style="text-align: justify;",
                                           "For additional information on how we inform our allyship, please see Amélie Lamont’s",
                                          tags$a(href = "https://guidetoallyship.com/", "Guide to Allyship.")))
                      )),
                      fluidRow(
                        column(5, offset = 1,
                               br(),
                               helpText(h3("What’s In This Guide")),
                               helpText(p(style="text-align: justify;",
                               "This guide attempts to collect and curate the contributions of Black planners,
                                        scholars, artists, writers, organizers and practitioners from a variety of fields that
                                        are concerned with the process of organizing space and place in the urban environments.
                                        The works listed here represent both traditional planning preoccupations such as housing
                                        policy, transportation planning and urban design, as well as more interdisciplinary works
                                        about urban sociology, cultural history, and Black-centered approaches to community building
                                        and organizing. Users will also find a number of critical approaches and novel methodologies
                                        employed to de-center Whiteness in the analysis of urban issues. Finally, while the majority
                                        of resources are books or journal articles, we have also endeavored to include various media
                                        such as films, podcasts, essays, and online essays. ")))),
                      fluidRow(
                        column(5, offset = 6,
                               br(),
                               helpText(h3("What’s NOT In This Guide")),
                               helpText(p(style="text-align: justify;",
                               "Simply put, this guide does not include non-Black people writing about Black people.
                                        A wealth of important scholarship by non-Black scholars and planners exists on the racist
                                        practices embedded within urban planning, i.e. redlining, gentrification and “urban renewal”.
                                        They have also documented successful urban social movements and advocacy efforts within 
                                        Black communities. While such materials are essential to a comprehensive understanding of 
                                        the historical, social and economic dynamics within cities, this guide is meant to specifically
                                        highlight the ideas and works of Black creators.")))
                      )
             ),
             
             tabPanel("Full Bibliography"
               
             )
             )
  
)

### Server ###
server <- function(input, output) {
  
    output$table <- DT::renderDataTable({
        data <- data %>%
            filter(Community %in% input$keyword |
                   `Urban Design` %in% input$keyword |
                     Policing %in% input$keyword |
                     `Social Injustice` %in% input$keyword |
                     `Land Use` %in% input$keyword,
                   Media_Type %in% input$type) 
        datatable(data, options = list(columnDefs = list(list(visible = FALSE, targets = c(12:16)))))
            })
}

### Run the application ###
shinyApp(ui = ui, server = server)















