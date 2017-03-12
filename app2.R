#Angus Huang
#Data 608, HW3
#Question 2: Often you are asked whether particular States are improving their mortality rates 
#(per cause) faster than, or slower than, the national average. Create a visualization that lets 
#your clients see this for themselves for one cause of death at the time. Keep in mind that the 
#national average should be weighted by the national population. 


#UI
#install.packages("shiny")

library(shiny)
library(ggplot2)
library (dplyr)
library(ggthemes)



myfile <-file.path("https://github.com/angus001/Data608/raw/master/cdc_mortality_99_10.csv")
#print (myfile)

bcl <- read.csv(myfile, stringsAsFactors = FALSE)

#calculate national average for each cause per year


nationalavg <- aggregate( cbind(Deaths, Population) ~ ICDCHAPTER + Year, bcl, FUN = sum)
nationalavg$natAvgRate <- round(nationalavg$Deaths/nationalavg$Population *100000,4)

head(nationalavg)

testdata <- bcl %>% filter(bcl$State == 'NY', bcl$Year == '2010') 
bcl1 <- merge(bcl,nationalavg, by = c("ICDCHAPTER", "Year"))

drops <- c("Deaths.y","Population.y","X")
bcl1<-bcl1[ ,!(names(bcl1) %in% drops)]
names(bcl1)[4] <- "stateDeath"
names(bcl1)[5] <- "Population"

#<-"Death"


ui <-  fluidPage( 
                  titlePanel("Mortality Rate Over A Decade" ),
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
                      
                      selectInput("stateInput", "States", choices = c( 'AL',	'NE',	'ID',	'RI',
                                                                       'AK',	'NV',	'IL',	'SC',
                                                                       'AZ',	'NH',	'IN',	'SD',
                                                                       'AR',	'NJ',	'IA',	'TN',
                                                                       'CA',	'NM',	'KS',	'TX',
                                                                       'CO',	'NY',	'KY',	'UT',
                                                                       'CT',	'NC',	'LA',	'VT',
                                                                       'DE',	'ND',	'ME',	'VA',
                                                                       'DC',	'OH',	'MD',	'WA',
                                                                       'FL',	'OK',	'MA',	'WV',
                                                                       'GA',	'OR',	'MI',	'WI',
                                                                       'HI',	'PA',	'MN',	'WY',
                                                                       'MS',	'MT',	'MO'            ), selected = 'NY')
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
            output$typeOuput <- renderUI({selectInput("typeInput", "ICDCHAPTER", sort(unique(bcl1$ICDCHAPTER)), 
                                                         selected = "Neoplasms")
                                            })
            
            filtered <- reactive({
                              if(is.null(input$typeInput)){
                                return(NULL)
                              }
                                
                                
                              bcl1 %>% filter(CRUDERATE >= input$crudeInput[1],
                                              CRUDERATE <= input$crudeInput[2],
                                              ICDCHAPTER == input$typeInput
                                             ,State == input$stateInput 
                                             )
                                      })
                  
                  
             output$coolplot <- renderPlot( {
                                                  if (is.null(filtered())){return()}
                    
                                                  ggplot(filtered(), aes(x=Year))+
                                                  geom_bar(aes(y= CRUDERATE, color = "State"), stat = 'identity')+
                                                  geom_line(aes(y=natAvgRate, color = 'NationalAvg'))+
                                                  scale_colour_manual("", breaks = c("State", "NationalAvg"), values=c('red','grey'))
                                                 # +theme(axis.text.x = element_text(angle = 70#, hjust =  1  ))
                                           } )
              
             output$results <- renderTable({
                                                  filtered()
                                           })
                  
           
  
          }

shinyApp(ui =ui, server = server)


