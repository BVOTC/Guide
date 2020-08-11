###### Black Voices on the City Shiny App ########

# Load Libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(shinythemes)
library(shinyWidgets)
library(DT)



Sys.setenv(LANG = "en")


######### Import guide data ################################

## Load data
#data <- read_csv("data/Guide_Sources.csv")


## Put data in order
# data <- data %>% 
 # select(1:2, 4, 3, 5:7, 14, 12, 13, 8:11)

# Pivot Longer
#data <- data %>% 
 # pivot_longer(cols = 11:14,
  #            names_to = "Theme Type",
   #           values_to = "Themes") %>% 
#  select(-11)

# Pivot Wider
#data <- data %>% 
#  pivot_wider(names_from = Themes,
#               values_from = Themes) %>% 
#  select(-14)

# Create new Item Format column (remove book redundancy)
#data <- data %>% 
# mutate(item_format_2 = `Item Format`)

#data$item_format_2 <- data$item_format_2 %>% 
# replace(data$item_format_2 == "Book - Entire" | data$item_format_2 == "Book - Chapter", "Book") 

# save(data, file = "data/data.Rdata")

###### Import additional resources data ###############################

data_AR <- read_csv("data/Guide_additional_resources.csv") %>%
  mutate( Name = if_else(is.na(Link),
                         Name,
                         paste0("<a href='",Link,"' target='_blank'>", Name,"</a>"))) %>%
  select(Type, Name, Notes = Description)

###### Webpage layout ################################################

load(file = "data/data.Rdata")


data <- data  %>%   mutate( Title = if_else(is.na(Link),
                                            Title,
                                            paste0("<a href='",Link,"' target='_blank'>", Title,"</a>")))


tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 200px;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 "))  ))

library(rsconnect)

### UI ###

ui <- fluidPage(
  includeCSS("www/bootstrap.css"),
  navbarPage("BVOTC",
             tabPanel("Resource Guide",
                      fluidRow(
                        column(4, offset = 5,
                               img(src='Logo-Dark-Fresno-6.png', width="200", align="center"))),
                      fluidRow(
                        column(8, offset = 2,
                               titlePanel(h1("Black Voices on the City: Resource Guide")))),
                      br(),
                      hr(),
                      fluidRow(
                        column(8, offset = 2,
                               br(),
                               helpText(p(style="text-align: justify;", 
                                          h4("Black Voices on the City is a student-organized database that 
                                          aims to accomplish two things: first, to catalog the contributions
                                          of black researchers and practitioners to the field of urban planning,
                                          and second, to amplify the unique voices and perspectives they bring to
                                          a field that has been overwhelmingly dominated by white, cisgender,
                                          heterosexual men since its beginnings. As current students and alumni 
                                          of urban planning programs, we feel that the best way to channel our 
                                          frustrations with the anti-Black racism left unacknowledged in our 
                                          classrooms and curricula is to lead by example, proactively seeking out
                                          and learning from Black planners and scholars. This guide is just one part
                                          of a larger, nascent effort to organize students and faculty around 
                                          re-imagining what constitutes urban planning and who contributes to that dialogue."))),
                               helpText(p(style="text-align: justify;", 
                                          h4("If you would like to contribute a resource to this database, please use our", 
                                             tags$a(href = "https://forms.gle/EuVgpKqhT4aGCaYFA", "Google Form"),
                                             "or send us an email at",  tags$a(href = "bvotc.guide@gmail.com", "bvotc.guide@gmail.com"))))
                        )),
                      br(),
                      tweaks,
                      fluidRow(
                        column(8, offset = 2,
                               wellPanel(
                                 tags$div(align = 'left', 
                                          class = 'multicol',
                                          h5(radioButtons(inputId = "keyword",
                                                                label = h3("Themes & Keywords"),
                                                                choices = c("Architecture and Urban Design", "Black Perspectives on Planning Practice and Education",
                                                                            "Community Organizing and Citizen Participation",
                                                                            "Crime, Policing, and Surveillance", "Culture, Placemaking, and Black Geographies", 
                                                                            "Development and Gentrification", "Feminist and Queer Urbanism", 
                                                                            "Mapping and GIS", "Municipal Policy and Governance", 
                                                                            "Politics of Land, Property, and Colonialism", 
                                                                            "Public Housing and Cooperatives", "Public Space and Parks", 
                                                                            "Racial and Social Justice", "Segregation and Redlining",
                                                                            "Sustainability, Environment, and Health","Transportation", "Urban History"),
                                                                selected = "Architecture and Urban Design",
                                                                inline = FALSE))),
                                 h5(checkboxGroupInput(inputId = "type",
                                                       label = h3("Media Type"),
                                                       choices = unique(data$item_format_2),
                                                       selected = unique(data$item_format_2),
                                                       inline = T)),
                                 h5(sliderInput (inputId = "years",
                                                 label = h3("Year Released"),
                                                 1850, 2020, 
                                                 value = c(1850, 2020),
                                                 sep = "")),
                                 h5(selectInput(inputId = "location",
                                                 label = h3("Location"),
                                                 choices = c("All", "United States", "Canada", "Global"),
                                                 selected = "All",
                                                width = "25%"),
                                    selectInput(inputId = "language",
                                                label = h3("Language"),
                                                choices = c("All", "English", "French"),
                                                selected = "All",
                                                width = "25%"))
                                 ))
                      ),
                      br(),
                      fluidRow(
                        column(8,offset = 2,
                               dataTableOutput("table"))
                      ),
                      br(),
                      br(),
                      br()
             ),
             tabPanel("About",
                      fluidRow(
                        column(4, offset = 5,
                               img(src='Logo-Dark-Fresno-6.png', width="200", align="center"))),
                      fluidRow(
                        column(8, offset = 2,
                               titlePanel(h1("About")))),
                      br(),
                      hr(),
                      br(),
                      fluidRow(
                        column(8, offset = 2,
                               helpText(h3("Who We Are"), align="center"),
                               helpText(p(style="text-align: justify;",
                                          h4("One of the most important guiding questions in anti-racism teachings
                               is “Who am I/who are we to do this work?” We owe users of this guide our answer 
                               to that question so they can hold us accountable as we exercise our privileges to
                               fight anti-Black racism in the field of urban planning. We are
                               a group of majority non-Black graduate students and alumni whose lives have benefited
                               from intersections with other forms of privilege, such as being male and cisgender
                              or having access to family wealth and higher education. Some of us also identify
                               as queers, feminists, and immigrants. We come to this work with a recognition that these
                               identities produce a number of limitations and blindspots, which is why we are calling
                               on everyone with an interest in tackling anti-Blackness within urban planning to collaborate
                               with us and critique our work."))),
                               helpText(p(style="text-align: justify;",
                                          h4("For additional information on how we inform our allyship, please see Amélie Lamont’s",
                                             tags$a(href = "https://guidetoallyship.com/", "Guide to Allyship.")))),
                               br(),
                               helpText(h3("Acknowledgements"), align="center"),
                               helpText(p(style="text-align: justify;", 
                                          h4("This guide is indebted first and foremost to the scholars, authors and creators
                                          listed here. It is also the result of the collective effort of students, alumni
                                          and others, who have dedicated their time and resources to changing the ways in
                                          which we learn what planning is and who planners are. Finally, we would like to
                                          acknowledge the efforts of individuals and organizations who have also created
                                          resource lists on anti-Blackness and anti-racism in planning and related fields, 
                                          which have helped in the creation of this guide  (see Additional Resources).")))
                        )),
                      br(),
                      fluidRow(
                        column(4, offset = 2,
                               helpText(h3("What’s In This Guide"), align="center"),
                               helpText(p(style="text-align: justify;", 
                                          h4("This guide attempts to collect and curate the contributions of Black planners,
                                          scholars, artists, writers, organizers and practitioners from a variety of fields
                                          that are concerned with the process of organizing space and place in the urban
                                          environment. The works listed here represent both traditional planning preoccupations
                                          such as housing policy, transportation planning and urban design, as well as more
                                          interdisciplinary fields like urban sociology, cultural history, and Black-centered
                                          approaches to community building and organizing. Users will also find a number of
                                          critical approaches and novel methodologies employed to de-center whiteness in the
                                          analysis of urban issues. Finally, while the majority of resources are books or journal
                                          articles, we have also endeavored to include various media such as videos
                                          and online essays.")))
                        ),
                        column(4,
                               helpText(h3("What’s NOT In This Guide"), align="center"),
                               helpText(p(style="text-align: justify;", 
                                          h4("Simply put, this guide does not include non-Black people writing about Black people.
                                          A wealth of important scholarship by non-Black scholars and planners exists on the racist
                                          practices embedded within urban planning, such as redlining, gentrification and “urban renewal.”
                                          They have also documented successful urban social movements and advocacy efforts within Black
                                          communities. While such materials are important to a comprehensive understanding of the historical,
                                          social and economic dynamics within cities, this guide is meant to specifically highlight the ideas
                                          and work of Black creators.")))  )),
                      br(),
                      br(),
                      br() ),
             
             tabPanel("How To Use This Guide",
                      fluidRow(
                        column(4, offset = 5,
                               img(src='Logo-Dark-Fresno-6.png', width="200", align="center"))),
                      fluidRow(
                        column(8, offset = 2,
                               titlePanel(h1("How To Use This Guide")))),
                      br(),
                      hr(),
                      fluidRow(
                        column(8, offset = 2,
                               br(),
                               helpText(p(style="text-align: justify;",
                                          h4("We hope this guide will be of great use to all members of the general
                                          public interested in urban planning and policy issues, but we particularly
                                          hope it provokes a deeper conversation within university-based planning
                                          programs to change the methods of teaching and learning to include Black voices
                                          and previously underrepresented approaches to the built environment."))),
                               br(),
                               helpText(h3("For Faculty...")),
                               helpText(p(style="text-align: justify;",
                                          h4("... this means ensuring that syllabi and class discussions engage with
                                          the writings and practice of Black planners, both historically and today. While 
                                          current curricula may acknowledge historical racism in planning, 
                                          graduates of urban planning programs need exposure to the wealth of recent Black
                                          scholarship that addresses the forms of racism we will encounter in our professional
                                          lives. If you are uncomfortable in presenting the material,  especially if you are white, 
                                          then this is an opportunity to learn and grow with your students, not an excuse to opt out.
                                          At the same time, not all of the materials inlcuded in this guide focus on racial issues, 
                                          and it is essential to not limit the
                                          authority of these writers to issues of race. They are experts and leaders in their
                                          respective fields, including transportation, sustainability, and urban governance,
                                          and should be treated as such in undergraduate and graduate planning courses."))),
                               br(),
                               helpText(h3("For Students...")),
                               helpText(p(style="text-align: justify;",
                                          h4("... this guide is a call to action. You should use it to organize with your peers to
                                          insist that more diverse perspectives from the field are included in your education - not just Black
                                          voices, but also those of women, people of color/visible minorities, Indigenous peoples, LGBTQ folks, individuals
                                          with altered mobility, and immigrant communities. You should also use it as
                                          a resource for personal learning and as source material for your coursework. Every essay, term paper
                                          or thesis is an opportunity to read, learn from, and cite Black sources. It also an opportunity to push
                                          the boundaries of what constitutes “appropriate” source material for academic work. You will find videos,
                                          podcasts, blogs, interviews, and other forms of media that represent a greater breadth of Black thinking
                                          on urban issues than traditional academic journals and publishing houses have produced. Finally,
                                          this guide is intended as a launching point and we encourage you to pay it forward by contributing to this
                                          guide in the future as new resources become available."))),
                               br(),
                               helpText(h3("For the General Public...")),
                               helpText(p(style="text-align: justify;",
                                          h4("... this guide offers an abundance of readings to challenge assumptions about race,
                                          place and meaning in the urban environment. It spans from the history of residential segregation and disparities
                                          in public transportation to mapping Black joy and the achievements of Black architects. While many of the journal
                                          articles and scholarly works are accessible only through institutional (i.e. university) access, please check if
                                          your local public library offers access online or through Interlibrary Loan (ILL) programs. Many of the books are
                                          available through your local bookstores."))),
                               br(),
                               helpText(p(style="text-align: justify;",
                                          h4("Please report any broken links or corrections to", tags$a(href = "bvotc.guide@gmail.com", "bvotc.guide@gmail.com")))),
                               br(),
                               br()
                        ))
                      
             ),
             
             tabPanel("Additional Resources",
                      fluidRow(
                        column(4, offset = 5,
                               img(src='Logo-Dark-Fresno-6.png', width="200", align="center"))),
                      fluidRow(
                        column(8, offset = 2,
                               titlePanel(h1("Additional Resources")))),
                      br(),
                      
                      hr(),
                      #   
                      # fluidRow(
                      #   column(8, offset = 2,
                      #          br(),
                      #          helpText(p(style="text-align: justify;", 
                      #                     h4("Here you can find a very inexhaustive list of Black-led urbanist organisations and media, as well as complementary resource lists."))))),
                      # br(),
                      
                      fluidRow(
                        column(8, offset = 2,
                               wellPanel(
                                 tags$div(align = 'left',
                                          
                                          h5(radioButtons(inputId = "type2",
                                                                label =  h4("Here you can find a very inexhaustive list of Black-led urbanist organisations, media, and complementary resource lists."),
                                                                choices = unique(data_AR$Type),
                                                                inline = FALSE))  ))    ) ),
                      br(),
                      fluidRow(
                        column(8,offset = 2,
                               dataTableOutput("table_AR"))),
                      br(),
                      br()  )            
  ))

### Server ###
server <- function(input, output) {
  
  output$table <- DT::renderDataTable({

      if (input$location == "All" & input$language == "All"){
        data <- data %>%
          filter(`Sustainability, Environment, and Health` %in% input$keyword |
                   `Racial and Social Justice` %in% input$keyword |
                   `Development and Gentrification` %in% input$keyword |
                   `Community Organizing and Citizen Participation` %in% input$keyword |
                   `Culture, Placemaking, and Black Geographies` %in% input$keyword |
                   `Politics of Land, Property, and Colonialism` %in% input$keyword |
                   `Transportation` %in% input$keyword |
                   `Architecture and Urban Design` %in% input$keyword |
                   `Urban History` %in% input$keyword |
                   `Black Perspectives on Planning Practice and Education` %in% input$keyword |
                   `Public Space and Parks` %in% input$keyword |
                   `Municipal Policy and Governance` %in% input$keyword |
                   `Mapping and GIS` %in% input$keyword |
                   `Segregation and Redlining` %in% input$keyword |
                   `Public Housing and Cooperatives` %in% input$keyword |
                   `Feminist and Queer Urbanism` %in% input$keyword |
                   `Crime, Policing, and Surveillance` %in% input$keyword,
                 item_format_2 %in% input$type,
                 Year >= input$years[1] & Year <= input$years[2])}
      else if (input$location != "All" & input$language == "All") {
        data <- data %>%
          filter(`Sustainability, Environment, and Health` %in% input$keyword |
                   `Racial and Social Justice` %in% input$keyword |
                   `Development and Gentrification` %in% input$keyword |
                   `Community Organizing and Citizen Participation` %in% input$keyword |
                   `Culture, Placemaking, and Black Geographies` %in% input$keyword |
                   `Politics of Land, Property, and Colonialism` %in% input$keyword |
                   `Transportation` %in% input$keyword |
                   `Architecture and Urban Design` %in% input$keyword |
                   `Urban History` %in% input$keyword |
                   `Black Perspectives on Planning Practice and Education` %in% input$keyword |
                   `Public Space and Parks` %in% input$keyword |
                   `Municipal Policy and Governance` %in% input$keyword |
                   `Mapping and GIS` %in% input$keyword |
                   `Segregation and Redlining` %in% input$keyword |
                   `Public Housing and Cooperatives` %in% input$keyword |
                   `Feminist and Queer Urbanism` %in% input$keyword |
                   `Crime, Policing, and Surveillance` %in% input$keyword,
                 item_format_2 %in% input$type,
                 Year >= input$years[1] & Year <= input$years[2],
                 Location == input$location) 
      }
    else if (input$location == "All" & input$language != "All") {
      data <- data %>%
        filter(`Sustainability, Environment, and Health` %in% input$keyword |
                 `Racial and Social Justice` %in% input$keyword |
                 `Development and Gentrification` %in% input$keyword |
                 `Community Organizing and Citizen Participation` %in% input$keyword |
                 `Culture, Placemaking, and Black Geographies` %in% input$keyword |
                 `Politics of Land, Property, and Colonialism` %in% input$keyword |
                 `Transportation` %in% input$keyword |
                 `Architecture and Urban Design` %in% input$keyword |
                 `Urban History` %in% input$keyword |
                 `Black Perspectives on Planning Practice and Education` %in% input$keyword |
                 `Public Space and Parks` %in% input$keyword |
                 `Municipal Policy and Governance` %in% input$keyword |
                 `Mapping and GIS` %in% input$keyword |
                 `Segregation and Redlining` %in% input$keyword |
                 `Public Housing and Cooperatives` %in% input$keyword |
                 `Feminist and Queer Urbanism` %in% input$keyword |
                 `Crime, Policing, and Surveillance` %in% input$keyword,
               item_format_2 %in% input$type,
               Year >= input$years[1] & Year <= input$years[2],
               Language == input$language) 
    }
    
    else {
      data <- data %>%
        filter(`Sustainability, Environment, and Health` %in% input$keyword |
                 `Racial and Social Justice` %in% input$keyword |
                 `Development and Gentrification` %in% input$keyword |
                 `Community Organizing and Citizen Participation` %in% input$keyword |
                 `Culture, Placemaking, and Black Geographies` %in% input$keyword |
                 `Politics of Land, Property, and Colonialism` %in% input$keyword |
                 `Transportation` %in% input$keyword |
                 `Architecture and Urban Design` %in% input$keyword |
                 `Urban History` %in% input$keyword |
                 `Black Perspectives on Planning Practice and Education` %in% input$keyword |
                 `Public Space and Parks` %in% input$keyword |
                 `Municipal Policy and Governance` %in% input$keyword |
                 `Mapping and GIS` %in% input$keyword |
                 `Segregation and Redlining` %in% input$keyword |
                 `Public Housing and Cooperatives` %in% input$keyword |
                 `Feminist and Queer Urbanism` %in% input$keyword |
                 `Crime, Policing, and Surveillance` %in% input$keyword,
               item_format_2 %in% input$type,
               Year >= input$years[1] & Year <= input$years[2],
               Language == input$language,
               Location == input$location) 
    }

    
    datatable(data, options = list(columnDefs = list(list(visible = FALSE, targets = c(8:28))), pageLength = 20),
              escape = FALSE #  makes HTML entities in the table not escaped (allows hyperlinks in table)
    )
  })
  
  output$table_AR <- DT::renderDataTable({
    data_AR <- data_AR %>%
      filter(Type %in% input$type2 ) 
    datatable(data_AR,
              
              escape = FALSE,
              rownames= FALSE,
              options = list(pageLength = 20,
                             columnDefs = list(list(visible=FALSE, targets= c(0)))
                             ))
  })
}

### Run the application ###
shinyApp(ui = ui, server = server)

