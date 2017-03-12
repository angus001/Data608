#Angus Huang
#Data 608, HW3
#Question 1: As a researcher, you frequently compare mortality rates from particular causes across different States.
# You need a visualization that will let you see (for 2010 only) the crude mortality rate, across all States,
# from one cause (for example, Neoplasms, which are effectively cancers). Create a visualization that allows you 
# to rank States by crude mortality for each cause of death. 


#UI
#install.packages("shiny")

library(shiny)
library(ggplot2)
library (dplyr)
library(ggthemes)








myfile <-file.path("https://github.com/angus001/Data608/raw/master/cdc_mortality_99_10.csv")
#print (myfile)

bcl <- read.csv(myfile, stringsAsFactors = FALSE)




ui <-  fluidPage( 
                  titlePanel("Mortality By Causes" ),
                  sidebarLayout(
                    sidebarPanel(
                      sliderInput("crudeInput", "Mortality Rate", 0, 500, c(0,500), post = "/mille"),
                      
                      selectInput("typeInput", "ICD CHAPTER / Causes", 
                                   choices = c('Certain infectious and parasitic diseases',
                                               'Neoplasms',
                                               'Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism',
                                               'Endocrine, nutritional and metabolic diseases',
                                               'Mental and behavioural disorders',
                                               'Diseases of the nervous system',
                                               'Diseases of the ear and mastoid process',
                                               'Diseases of the circulatory system',
                                               'Diseases of the respiratory system',
                                               'Diseases of the digestive system',
                                               'Diseases of the skin and subcutaneous tissue',
                                               'Diseases of the musculoskeletal system and connective tissue',
                                               'Diseases of the genitourinary system',
                                               'Pregnancy, childbirth and the puerperium',
                                               'Certain conditions originating in the perinatal period',
                                               'Congenital malformations, deformations and chromosomal abnormalities',
                                               'Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified',
                                               'Codes for special purposes',
                                               'External causes of morbidity and mortality'
                                                ),
                                   selected = "Neoplasms"),
                      
                      radioButtons("yearInput", "CalendarYear", choices = c('1999','2000','2001',
                                                                            '2002','2003','2004',
                                                                            '2005','2006','2007',
                                                                            '2008','2009','2010'
                                                                            ), selected = '2010')
                                )
  
                    ,

                  mainPanel( plotOutput("coolplot"),
                             br(),br(),
                             tableOutput("results")
                             )
                  #,
                  # numericInput(inputId = "n",
                  # "sample size", value = 25),
                  # plotOutput (outputId = "hist")
                  )
                )

server <- function (input, output, session)
          {
            # output$coolplot <- renderPlot
            #   ({
            output$typeOuput <- renderUI({selectInput("typeInput", "ICDCHAPTER", sort(unique(bcl$ICDCHAPTER)), 
                                                         selected = "Neoplasms")
                                            })
            
            filtered <- reactive({
                              if(is.null(input$typeInput)){
                                return(NULL)
                              }
                                
                                
                              bcl %>% filter(CRUDERATE >= input$crudeInput[1],
                                             CRUDERATE <= input$crudeInput[2],
                                             ICDCHAPTER == input$typeInput
                                             ,Year == input$yearInput
                                             )
                                      })
                  
                  
             output$coolplot <- renderPlot( {
                                                  if (is.null(filtered())){return()}
                    
                                                  ggplot(filtered(), aes(x=reorder(State,-CRUDERATE), y=CRUDERATE)) + geom_bar(stat = "identity")
                                                 # +theme(axis.text.x = element_text(angle = 70#, hjust =  1  ))
                                           } )
              
             output$results <- renderTable({
                                                  filtered()
                                           })
                  
           
  
          }

shinyApp(ui =ui, server = server)


