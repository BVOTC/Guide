###### Black Voices on the City Shiny App ########

# Load Libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(shinydashboard)



Sys.setenv(LANG = "en")


######### Import guide data ################################

## Load data
data <- read_csv("data/Guide_Sources.csv") %>%

  # Pivot Longer
  select(1:14) %>%
pivot_longer(cols = 11:14,
           names_to = "Theme Type",
          values_to = "Themes") %>%
 select(-11) %>%

# Pivot Wider
 pivot_wider(names_from = Themes,
              values_from = Themes) %>%
 select(-14) %>%

  mutate(
    # Create new Item Format column (remove book redundancy)
    
          item_format_2 = `Media Format` %>%
                          replace(`Media Format` == "Book - Entire" |
                                    `Media Format` == "Book - Chapter", 
                                  "Book"),
         
         ##Embed hyperlinks in the item titles
          Title = if_else(is.na(Link),
                Title,
                paste0("<a href='",Link,"' target='_blank'>", Title,"</a>"))  ) 


#save(data, file = "data/data.Rdata")

###### Import additional resources data ###############################

data_AR <- read_csv("data/Guide_additional_resources.csv") %>%
  mutate( Name = if_else(is.na(Link),
                         Name,
                         paste0("<a href='",Link,"' target='_blank'>", Name,"</a>"))) %>%
  select(Type, Name, Notes = Description)



###### Webpage layout ################################################

#load(file = "data/data.Rdata")

library(rsconnect)

### UI ###

ui <- fluidPage(
  includeCSS("www/bootstrap.css"),
  navbarPage("", 
             tabPanel("The Guide",
                      fluidRow(
                        column(12, offset = 0, style='padding-left:0px; padding-right:0px; margin-left: -1.1em ; margin-right: -1.1em',
                               img(src='MainPageBanner.png', width = '102.2%'))
                      ),
                      br(), br(),
                      fluidRow(
                        column(10, offset = 1,
                               titlePanel(h1("Search The Resource Guide")))),
                      fluidRow(
                        column(10, offset = 1,
                               helpText(p(style="text-align: justify;", 
                                          h4("Black Voices on the City is a student-organized database that 
                                          aims to accomplish two things: first, to catalog contributions
                                          of black researchers and practitioners to the field of urban planning,
                                          and second, to amplify black voices in a field that has been overwhelmingly dominated by white, cisgender,
                                          heterosexual men since its beginnings. As current students and alumni 
                                          of Canadian urban planning programs, 
                                          the development of this guide was born out of our 
                                          frustrations with the lack of diverse perspectives left unacknowledged in our 
                                          classrooms and curricula. This guide is just one part
                                          of a larger, nascent effort to organize students and faculty around 
                                          re-imagining what constitutes urban planning and who contributes to that dialogue."))),
                               helpText(p(style="text-align: justify;", 
                                          h4("If you would like to contribute a resource to this database, please use our", 
                                             tags$a(href = "https://forms.gle/EuVgpKqhT4aGCaYFA", "Google Form"),
                                             "or send us an email at",  tags$a(href = "bvotc.guide@gmail.com", "bvotc.guide@gmail.com"))))
                        )),
                      br(),
                      fluidRow(
                        column(10, offset = 1,
                               wellPanel(
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
                                                                inline = T)),
                                 h5(checkboxGroupInput(inputId = "type",
                                                       label = h3("Media Type"),
                                                       choices = unique(data$item_format_2),
                                                       selected = unique(data$item_format_2),
                                                       inline = T)),
                                 h5( chooseSliderSkin(
                                   skin = "Flat",
                                   #c("Shiny", "Flat", "Modern", "Nice", "Simple", "HTML5", "Round", "Square"),
                                   color = "#3A469D"
                                 ),
                                   sliderInput (inputId = "years",
                                                 label = h3("Year Released"),
                                                 1890, 2020, 
                                                 value = c(1890, 2020),
                                                 sep = "")),
                                 h5(selectInput(inputId = "location",
                                                 label = h3("Location"),
                                                 choices = c("All", "United States", "Canada", "Beyond North America"),
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
                        column(10,offset = 1,
                               dataTableOutput("table"))
                      )
                    
             ),
             
             tabPanel("How To Use",
                      br(), br(),
                      tags$a(href='https://bvotc.shinyapps.io/Guide/',tags$img(src='Logo-Dark-Fresno-6.png', width = "50")),
                      fluidRow(
                        column(12, offset = 0, style='padding-left:0px; padding-right:0px; margin-left: -1.1em ; margin-right: -1.1em',
                               img(src='MainPageBanner-Slimmest.png', width = '102.2%'))
                      ),
                      br(), br(),
                     
                      
                      fluidRow(
                        column(10, offset = 1,
                               helpText(p(style="text-align: justify;",
                                          h4("We hope this guide will be of great use to all members of the general
                                          public interested in urban planning and policy issues, but we particularly
                                          hope it provokes a deeper conversation within university-based planning
                                          programs to change the methods of teaching and learning to include Black voices
                                          and previously underrepresented approaches to the built environment."))),
                               br(),
                               helpText(h3("For Faculty,")),
                               helpText(p(style="text-align: justify;",
                                          h4("This means ensuring that syllabi and class discussions engage with
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
                               helpText(h3("For Students,")),
                               helpText(p(style="text-align: justify;",
                                          h4("This guide is a call to action. You should use it to organize with your peers to
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
                               helpText(h3("For the General Public,")),
                               helpText(p(style="text-align: justify;",
                                          h4("This guide offers an abundance of readings to challenge assumptions about race,
                                          place and meaning in the urban environment. It spans from the history of residential segregation and disparities
                                          in public transportation to mapping Black joy and the achievements of Black architects. While many of the journal
                                          articles and scholarly works are accessible only through institutional (i.e. university) access, please check if
                                          your local public library offers access online or through Interlibrary Loan (ILL) programs. Many of the books are
                                          available through your local bookstores."))),
                               br(),
                               helpText(p(style="text-align: justify;",
                                          h4("Please report any broken links or corrections to", tags$a(href = "bvotc.guide@gmail.com", "bvotc.guide@gmail.com")))),
                               br(),
                               br(),
                               br()))
                      
             ),
             
             tabPanel("About",
                      br(), br(),
                      tags$a(href='https://bvotc.shinyapps.io/Guide/',tags$img(src='Logo-Dark-Fresno-6.png', width = "50")),
                      fluidRow(
                        column(12, offset = 0, style='padding-left:0px; padding-right:0px; margin-left: -1.1em ; margin-right: -1.1em',
                               img(src='MainPageBanner-Slimmest.png', width = '102.2%'))
                      ),
                      br(), br(),
                      fluidRow(
                        column(10, offset = 1,
                               helpText(h3("Who We Are")),
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
                               with us and critique our work.", "For additional information on how we inform our allyship, please see Amélie Lamont’s",
                                             tags$a(href = "https://guidetoallyship.com/", "Guide to Allyship.")))),
                               br(),
                               helpText(h3("Acknowledgements")),
                               helpText(p(style="text-align: justify;", 
                                          h4("This guide is indebted first and foremost to the scholars, authors and creators
                                          listed here. It is also the result of the collective effort of students, alumni
                                          and others, who have dedicated their time and resources to changing the ways in
                                          which we learn what planning is and who planners are. Finally, we would like to
                                          acknowledge the efforts of individuals and organizations who have also created
                                          resource lists on anti-Blackness and anti-racism in planning and related fields, 
                                          which have helped in the creation of this guide.")))
                        )),
                      br(),
                     
                      br(),
                      br(),
                      br() ),

             
             tabPanel("More Resources",
                      br(), br(),
                      tags$a(href='https://bvotc.shinyapps.io/Guide/',tags$img(src='Logo-Dark-Fresno-6.png', width = "50")),
                      fluidRow(
                        column(12, offset = 0, style='padding-left:0px; padding-right:0px; margin-left: -1.1em ; margin-right: -1.1em',
                               img(src='MainPageBanner-Slimmest.png', width = '102.2%'))
                      ),
                      br(), br(),
                      fluidRow(
                        column(10, offset = 1,
                               helpText(p(style="text-align: justify;",
                                          h4("Here you can find a very inexhaustive list of Black-led urbanist organisations, media, and complementary resource lists." 
                                             
                                             
                                             ))),
                               helpText(p(style="text-align: justify;", 
                                          h4("If you would like to contribute to this list, you can send us an email at",  
                                             tags$a(href = "bvotc.guide@gmail.com", "bvotc.guide@gmail.com"), "."))))),
                      br(),
                      
                      fluidRow(
                        column(10, offset = 1,
                               wellPanel(
                                 tags$div(align = 'left',
                                          
                                          h5(radioButtons(inputId = "type2",
                                                          label = NULL,
                                                                choices = unique(data_AR$Type),
                                                                inline = FALSE))  ))    ) ),
                      br(),
                      fluidRow(
                        column(10,offset = 1,
                               dataTableOutput("table_AR")))  ),
             
          
                        tabPanel("Contact Us",
                                 br(), br(),
                                 tags$a(href='https://bvotc.shinyapps.io/Guide/',tags$img(src='Logo-Dark-Fresno-6.png', width = "50")),
                                 fluidRow(
                                   column(12, offset = 0, style='padding-left:0px; padding-right:0px; margin-left: -1.1em ; margin-right: -1.1em',
                                          img(src='MainPageBanner-Slimmest.png', width = '102.2%'))
                                 ),
                                 br(), br(), 
                              
                        
                                 fluidRow(
                                   column(10, offset = 1,
                                          helpText(p(style="text-align: justify;",
                                                     
                                                     h4("If you've come across literature or online media created by a Black urbanist, 
                                                        activist or planner, you can use our google form to add to the guide:"))))),
                                                        
                             fluidRow(column(10, offset = 1,
                             a(h4("ADD A RESOURCE", class = "btn btn-default btn-lg action-button" , ), target = "_blank",
                               href = "https://forms.gle/EuVgpKqhT4aGCaYFA"))),                       
                                                        br(),
                                                        fluidRow(
                                                          column(10, offset = 1,
                                                                 helpText(p(style="text-align: justify;",
                                                                h4("BVOTC's respository of urban-themed black perspectives
                                                        is 100% crowdsourced and we welcome all suggestions that match the criteria on the How to Use page."),
                                                        h4("To report any broken links or corrections, let us know how we can improve BVOTC, or collaborate and join the team, please email us at", tags$a(href = "bvotc.guide@gmail.com", "bvotc.guide@gmail.com"), "or reach out on",  tags$a(href = "https://www.instagram.com/blackvoicesonthecity/", "instagram"), "."),
                                                     br(), br(), br()
                                                     ))))),
             
             br(), br(), 
             
             fluidRow(column(12, offset = 0,
                             style = 'text-align: center',
                             tags$a(href='https://www.instagram.com/blackvoicesonthecity/',tags$img(src='Icon-IG.png', width = "40")),
                             tags$a(href="bvotc.guide@gmail.com",tags$img(src='Icon-Email.png', width = "40")),
                             
                             a(h4("ADD A RESOURCE", class = "btn btn-link" , ), target = "_blank",
                               href = "https://forms.gle/EuVgpKqhT4aGCaYFA"))) ,
             
             br(), br()
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

    
    datatable(data, options = list(autoWidth = TRUE,
                                   scrollX=TRUE,
                                   
                                   columnDefs = list(
                                     list(targets=c(1), visible=TRUE, width = '14%'),
                                     list(targets=c(2), visible=TRUE, width='8%'),
                                     list(targets=c(3), visible=TRUE, width='30%'),
                                     list(targets=c(4), visible=TRUE, width='5%'),
                                     list(targets=c(5), visible=TRUE, width='15%'),
                                     list(targets=c(6), visible=TRUE, width='15%'),
                                     list(targets=c(7), visible=TRUE, width='13%'),
                                     list(targets = c(0,8:28), visible = FALSE)),
                                   pageLength = 20),
              escape = FALSE #  makes HTML entities in the table not escaped (allows hyperlinks in table)
    )
  })  
  

  
  output$table_AR <- DT::renderDataTable({
    data_AR <- data_AR %>%
      filter(Type %in% input$type2 ) 
    datatable(data_AR,
              
              escape = FALSE,
              rownames= FALSE,
              options = list( #autoWidth = TRUE,
                             #scrollX=TRUE,
                             pageLength = 20,
                             columnDefs = list(
                                               list(targets= c(0), visible=FALSE)
                                              #  list(targets=c(1), visible=TRUE, width = '40%'),
                                              # list(dom='t',ordering=F, targets=c(2), visible=TRUE, width='60%')
                                               ) )
              )
  })
}

### Run the application ###
shinyApp(ui = ui, server = server)

