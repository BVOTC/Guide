###### Black Voices on the City Shiny App ########

# Load Libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(shinythemes)
library(DT)

Sys.setenv(LANG = "en")

## Load data
# data <- read_csv("data/Guide_Sources.csv")

## Put data in order
# data <- data %>% 
#  select(1:2, 4, 3, 5:7, 12, 8:11)

# Pivot Longer
#data <- data %>% 
#  pivot_longer(cols = 9:12,
#              names_to = "Theme Type",
#              values_to = "Themes") %>% 
#  select(-9)

# Pivot Wider
#data <- data %>% 
#  pivot_wider(names_from = Themes,
#               values_from = Themes) %>% 
#  select(-12)

#save(data, file = "data/data.Rdata")

load(file = "data/data.Rdata")

### UI ###

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
                                          "Black Voices on the City is a student-organized database that 
                                          aims to accomplish two things: first, to catalog the contributions
                                          of black researchers and practitioners to the field of urban planning
                                          and second, to amplify the unique voices and perspectives they bring to
                                          a field that has been overwhelmingly dominated by white, cisgender,
                                          heterosexual men since its beginnings. As current students and alumni 
                                          of urban planning programs, we feel that the best way to channel our 
                                          frustrations with the anti-Black racism left unacknowledged in our 
                                          classrooms and curriculum is to lead by example, proactively seeking out
                                          and learning from Black planners and scholars. This guide is just one part
                                          of a larger, nascent effort to organize students and faculty around 
                                          re-imagining what constitutes urban planning and who contributes to that dialogue."))
                        )),
                      br(),
                      fluidRow(
                        column(6, offset = 3,
                               helpText(h3("Choose one or more themes that interest you,
                                           select by region or type of publication and more..."), align="center"))),
                      br(),
                      fluidRow(
                        column(6, offset = 3,
                               wellPanel(
                                 checkboxGroupInput(inputId = "keyword",
                                                    label = h4("Themes and Keywords"),
                                                    choices = c("Architecture and Urban Design", "Being Black in Planning Practice and Education",
                                                                "Community Organizing and Citizen Participation",
                                                                "Crime, Policing, and Surveillance", "Culture, Placemaking, and Black Geographies", 
                                                                "Development and Gentrification", "Feminist and Queer Urbanism",
                                                                "Global Perspectives", "History of Cities and Urban Planning", "Housing", 
                                                                "Mapping and GIS", "Municipal Policy and Governance", 
                                                                "Planning Practice","Politics of Land and Property", "Public Space", 
                                                                "Racial and Social Justice", "Segregation and Redlining",
                                                                "Sustainability, Environment, and Health","Transportation"),
                                                    selected = "Being Black in Planning Practice and Education",
                                                    inline = T),
                                 checkboxGroupInput(inputId = "type",
                                                    label = h4("Media Type"),
                                                    choices = unique(data$`Item Format`),
                                                    selected = unique(data$`Item Format`),
                                                    inline = T),
                                 sliderInput (inputId = "years",
                                           label = h4("Year Released"),
                                           1850, 2020, 
                                           value = c(1850, 2020),
                                           sep = ""))),
                      ),
                      fluidRow(
                        column(8,offset = 1,
                               dataTableOutput("table"))
                      ),
                      br(),
                      fluidRow(
                        column(8, offset = 2,
                               helpText(h3("Acknowledgements"), align="center"),
                               helpText(p(style="text-align: justify;", 
                                          "This guide is indebted first and foremost to the scholars, authors and creators
                                          listed here. It is also the result of the collective effort of students, alumni
                                          and others, who have dedicated their time and resources to changing the ways in
                                          which we learn what planning is and who planners are. Finally, we would like to
                                          acknowledge the efforts of individuals and organizations who have also created
                                          complementary resource lists and guides on anti-Blackness and anti-racism in
                                          urban planning, all of which have helped  (see Additional Resources)."))
                        )),
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
                      fluidRow(
                        column(8, offset = 2,
                               br(),
                               helpText(h3("Who We Are"), align="center"),
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
                                          tags$a(href = "https://guidetoallyship.com/", "Guide to Allyship."))),
                               br(),
                               fluidRow(
                                 column(6,
                                        helpText(h3("What’s In This Guide"), align="center"),
                                        helpText(p(style="text-align: justify;", 
                                                   "This guide attempts to collect and curate the contributions of Black planners,
                                          scholars, artists, writers, organizers and practitioners from a variety of fields
                                          that are concerned with the process of organizing space and place in the urban
                                          environment. The works listed here represent both traditional planning preoccupations
                                          such as housing policy, transportation planning and urban design, as well as more
                                          interdisciplinary fields like urban sociology, cultural history, and Black-centered
                                          approaches to community building and organizing. Users will also find a number of
                                          critical approaches and novel methodologies employed to de-center Whiteness in the
                                          analysis of urban issues. Finally, while the majority of resources are books or journal
                                          articles, we have also endeavored to include various media such as films, podcasts, essays,
                                          and online essays."))
                                 ),
                                 column(6,
                                        helpText(h3("What’s NOT In This Guide"), align="center"),
                                        helpText(p(style="text-align: justify;", 
                                                   "Simply put, this guide does not include non-Black people writing about Black people.
                                          A wealth of important scholarship by non-Black scholars and planners exists on the racist
                                          practices embedded within urban planning, i.e. redlining, gentrification and “urban renewal.”
                                          They have also documented successful urban social movements and advocacy efforts within Black
                                          communities. While such materials are essential to a comprehensive understanding of the historical,
                                          social and economic dynamics within cities, this guide is meant to specifically highlight the ideas
                                          and works of Black creators."))
                                 )),
                               br(),
                               br()
                        ))
             ),
             
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
                                          "We hope this guide will be of great use to all members of the general
                                          public interested in urban planning and policy issues, but we particularly
                                          hope it provokes a deeper conversation within university-based planning
                                          programs to change the methods of teaching and learning to include Black voices
                                          and previously underrepresented approaches to the built environment.")),
                               br(),
                               helpText(h3("For Faculty...")),
                               helpText(p(style="text-align: justify;",
                                          "... this means ensuring that syllabi and class discussions engage with
                                          the writings and practice of Black planners, both historically and today. While 
                                          current curriculum may acknowledge historical planning racism, such as urban renewal,
                                          graduates of urban planning programs need exposure to the wealth of recent Black
                                          scholarship that addresses the forms of racism we will encounter in our professional
                                          lives. More importantly, this means including such materials in all areas of planning.
                                          Not all of these materials focus on racial issues, and it is essential not to limit the
                                          authority of these writers to issues of race. They are experts and leaders in their
                                          respective fields, including transportation, sustainability, and urban governance,
                                          and should be treated as such in undergraduate and graduate planning courses. If you are
                                          uncomfortable in presenting the material, especially if you are White, then this is an opportunity
                                          to learn and grow with your students, not an excuse to opt out.")),
                               br(),
                               helpText(h3("For Students...")),
                               helpText(p(style="text-align: justify;",
                                          "... this guide is a call to action. You should use it to organize with your peers to
                                          insist that more diverse perspectives from the field are included in your education - not just Black
                                          voices, but also those of women, scholars of color/visible minorities, LGBTQ folks, individuals
                                          with altered mobility, and both indigineous and immigrant communities. You should also use it as
                                          a resource for personal learning and as source material for your coursework. Every essay, term paper
                                          or thesis is an opportunity to read, learn from, and cite Black sources. It also an opportunity to push
                                          the boundaries of what constitutes “appropriate” source material for academic work. You will find videos,
                                          podcasts, blogs, interviews, and other forms of media that represent a greater breadth of Black thinking
                                          on urban issues than traditional academic journals and publishing houses have produced. Finally,
                                          this guide is intended as a launching point and we encourage you to pay it forward by contributing to this
                                          guide in the future as new resources become available.")),
                               br(),
                               helpText(h3("For the General Public...")),
                               helpText(p(style="text-align: justify;",
                                          "... this guide offers an abundance of readings to challenge assumptions about race,
                                          place and meaning in the urban environment. It spans from the history of residential segregation and disparities
                                          in public transportation to mapping Black joy and the achievements of Black architects. While many of the journal
                                          articles and scholarly works are accessible only through institutional (i.e. university) access, please check if
                                          your local public library offers access online or through Interlibrary Loan (ILL) programs. Many of the books are
                                          available through your local bookstores.")),
                               br(),
                               helpText(p(style="text-align: justify;",
                                          "Please report any broken links to", tags$a(href = "bvotc.guide@gmail.com", "bvotc.guide@gmail.com"))),
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
                      fluidRow(
                        column(4, offset = 2,
                               br(),
                               helpText(h3("Related Resource Guides")),
                               helpText(p(style="text-align: justify;",
                                          tags$a(href = "https://nmaahc.si.edu/shifting-landscape-black-architects-and-planners-1968-now-0#resources",
                                                 "The National Museum of African American History and Culture"))),
                               br(),
                               helpText(h3("Organizations")),
                               helpText(p(style="text-align: justify;",
                                          tags$a(href = "www.urbanconsulate.com",
                                                 "Urban Consulate"))),
                               helpText(p(style="text-align: justify;",
                                          tags$a(href = "https://blackcommunity.planning.org/",
                                                 "APA Planning and the Black Community Division"))),
                               helpText(p(style="text-align: justify;",
                                          tags$a(href = "https://noma.net/",
                                                 "National Organization of Minority Architects"))),
                               br(),
                               helpText(h3("People and Organizations on Social Media")),
                               helpText(p(style="text-align: justify;",
                                          "*Note: Please follow these accounts but be mindful of not overwhelming black practitioners with
                                 requests for collaboration or other questions unless you know them personally or had a working
                                 relationship with them before listening to black voices became trendy.")),
                               br(),
                               helpText(p(style="text-align: justify;",
                                          "BlackSpace (Insta: @blackspaceorg)")),
                               helpText(p(style="text-align: justify;",
                                          "Kristen Jeffers, The Black Urbanist (Insta: @blackurbanist)")),
                               helpText(p(style="text-align: justify;",
                                          "Black Urbanism (Insta: @blackurbanism)")),
                               helpText(p(style="text-align: justify;",
                                          "Brentin Mock, City Lab (Twitter: @Brentinmock)"))
                        ),
                        column(4,
                               br(),
                               helpText(h3("General Links")),
                               helpText(p(style="text-align: justify;",
                                          tags$a(href = "https://nextcity.org/daily/entry/black-people-have-been-building-a-better-world-who-will-join-them",
                                                 "'Black People Have Been Building a Better World. Who Will Join Them?', NextCity"))),
                               helpText(p(style="text-align: justify;",
                                          tags$a(href = "https://nmaahc.si.edu/shifting-landscape-black-architects-and-planners-1968-now-0",
                                                 "'Shifting the Landscape: Black Architects and Planners, 1968 to Now'. National Museum of African American History and Culture"))),
                               helpText(p(style="text-align: justify;",
                                          tags$a(href = "https://la.curbed.com/maps/los-angeles-black-architects-projects-map",
                                                 "'Mapped: 20 places in LA where black architects left their mark'. Curbed Los Angeles"))),
                               helpText(p(style="text-align: justify;",
                                          tags$a(href = "https://publicdomain.nypl.org/greenbook-map/",
                                                 "'Navigating the Green Book'. New York Public Library"))),
                               helpText(p(style="text-align: justify;",
                                          tags$a(href = "https://www.blackspace.org/neighborhood-strategy ",
                                                 "'Co-Designing Black Neighborhood Heritage Conservation Playbook'. BlackSpace")))
                        )
                      ),
                      br(),
                      br()
             )
  )
)

### Server ###
server <- function(input, output) {
  
    output$table <- DT::renderDataTable({
        data <- data %>%
            filter(`Sustainability, Environment, and Health` %in% input$keyword |
                     `Racial and Social Justice` %in% input$keyword |
                     `Development and Gentrification` %in% input$keyword |
                     `Community Organizing and Citizen Participation` %in% input$keyword |
                     `Culture, Placemaking, and Black Geographies` %in% input$keyword |
                     `Politics of Land and Property` %in% input$keyword |
                     `Transportation` %in% input$keyword |
                     `Architecture and Urban Design` %in% input$keyword |
                     `History of Cities and Urban Planning` %in% input$keyword |
                     `Being Black in Planning Practice and Education` %in% input$keyword |
                     `Public Space` %in% input$keyword |
                     `Municipal Policy and Governance` %in% input$keyword |
                     `Mapping and GIS` %in% input$keyword |
                     `Segregation and Redlining` %in% input$keyword |
                     `Housing` %in% input$keyword |
                     `Feminist and Queer Urbanism` %in% input$keyword |
                     `Crime, Policing, and Surveillance` %in% input$keyword |
                     `Global Perspectives` %in% input$keyword |
                     `Planning Practice` %in% input$keyword,
                   `Item Format` %in% input$type,
                    Year >= input$years[1] & Year <= input$years[2]) 
        datatable(data, options = list(columnDefs = list(list(visible = FALSE, targets = c(9:29)))))
            })
}

### Run the application ###
shinyApp(ui = ui, server = server)















