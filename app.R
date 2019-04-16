##################################################################
#                             Jojo Emerson                       #
#                    GH CEA Registry League Tables               #
#                   Main directory: Shiny app file               #
#                     Code changes: April 4, 2019                #
#                    GitHub Upload: April 16, 2019               #
#                             Version 1.2.1                      #
##################################################################


############              REQUIRED FILES              ############
# Main directory: 
#   - app.R: R shiny app (current file)
#   - google-analytics.js: (removed for GitHub upload)
#   - cleaning.R: data cleaning file
#   - interventions_cleaning.R: data cleaning for interventions 
# Data directory: 
#   - METHODS.xlsx: GH CEA registry methods excel file from 
#                   update to 2017 data
#   - RATIOS.xlsx: GH CEA registry ratios excel file from 
#                   update to 2017 data
# www directory:
#  - BMGF.png: BMGF logo for footer
#  - cevr.png: CEVR logo for footer
#  - TMC.png: Tufts medical center logo for footer
# rsconnect directory:
#   - connection to shinyapps.io account for deployment 
#      (removed for GitHub upload)


#call libraries
library(shiny)
library(DT)
library(dplyr)
library(shinyjs)

#call data cleaning
source("cleaning.R")

####UI####
ui <- fluidPage(
  
  #call shinyjs
  useShinyjs(),
  
  fluidRow(
    #information text about the filters
    htmlOutput('generalinfo')
  ),
  
  #default filters:
  fluidRow(
    column(width = 12,
           #keywords filter
           selectizeInput(inputId = "keywords", label = "Keywords:", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'), width = '100%')
    )
  ),
  
  fluidRow(
    #disease filter
    column(width = 4, 
           selectizeInput('disease', label = "Disease:", choices = NULL,multiple = TRUE, options = list(placeholder = 'Begin typing disease...'))
    ),
    #intervention type filter
    column(width = 4, 
           selectizeInput('interventiontype', label = "Intervention type:", choices = NULL, multiple = TRUE, options = list(placeholder = 'Begin typing intervention type...'))
    ),
    #comparator filter
    column(width = 4, 
           selectizeInput('comparator', label = "Comparator:", choices = NULL, multiple = TRUE, options = list(placeholder = 'Begin typing comparator...'))
    )
  ), #close row 1
  
  fluidRow(
    #country filter
    column(width = 4, 
           selectizeInput('country', label = "Country:", choices = NULL, multiple = TRUE, options = list(placeholder = 'Begin typing country...'))
    ),
    #region filter
    column(width = 4, 
           selectizeInput('region', label = "Region:", choices = NULL, multiple = TRUE, options = list(placeholder = 'Begin typing region...'))
    ),
    #year filter
    column(width = 4, 
           sliderInput("year", "Publication year:", min = 1995, max = 2018, value = c(1995, 2018), step = 1, dragRange = TRUE, sep = "", ticks = FALSE)
    )
  ),#close row 2
  
  #optional (advanced) filters: 
  fluidRow(
    #intervention filter
    column(width = 4, 
           shinyjs::hidden(selectizeInput('intervention', label = "Intervention:", choices = NULL, multiple = TRUE, options = list(placeholder = 'Begin typing intervention...')))
    ),
    #sponsor filter
    column(width = 4,
           shinyjs::hidden(selectizeInput('sponsor', label = "Sponsor:", choices = NULL, multiple = TRUE, options = list(placeholder = 'Begin typing sponsor...')))
    ),
    #ICD10 filter
    column(width = 4,
           shinyjs::hidden(selectizeInput('ICD10', label = "ICD10 Code:", choices = NULL, multiple = TRUE, options = list(placeholder = "Begin typing ICD 10 code...")))
    )
  ), #close row 3
  
  fluidRow(
    #target pop gender  filter
    column(width = 4, 
           shinyjs::hidden(selectizeInput('gender', label = "Target population gender:", choices = NULL, multiple = TRUE, options = list(placeholder = 'Begin typing gender...')))
    ),
    #target pop age  filter
    column(width = 4,
           shinyjs::hidden(selectizeInput('age', label = "Target population age:", choices = NULL, multiple = TRUE, options = list(placeholder = 'Begin typing age...')))
    ),
    #author filter
    column(width = 4,
           shinyjs::hidden(selectizeInput('author', label = "Article author:", choices = NULL, multiple = TRUE, options = list(placeholder = 'Begin typing author name...')))
    )
  ), #close row 4
  
  
  #action buttons
  fluidRow(
    #clear filters button
    column(width = 6, align = 'left',
           actionButton('reset', "Reset filters",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    #advanced filter button 
    column(width = 6, align = 'right',
           actionButton('advanced', "Advanced filters",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )),
  
  fluidRow(
    column(width = 3),
    #currently filtered text
    column(width = 6, align = "center", style = "border: 2px solid silver; border-radius: 10px;",
           shinyjs::hidden(htmlOutput("filtered"))),
    
    tags$br()
  ), #close row 6
  
  
  #league table (datatable) ouptut
  fluidRow(
    DT::dataTableOutput("league_table")
  ),
  
  
  #download button
  fluidRow(align = "center",
           downloadButton('download', "Download data!",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
           
           #accompanying info about download        
           htmlOutput('downloadinfo')
  ), 
  
  #thresholds disclaimer text
  fluidRow(align = 'left',
           tags$br(),
           htmlOutput('percentGDPdisclaimer'),
           htmlOutput('thresholddisclaimer')
  ),
  
  #logos, hyperlinked to websites
  fluidRow(align = 'right',
           tags$br(),
           "Developer:", tags$a("Joanna Emerson", href="mailto:jemerson@tuftsmedicalcenter.org"),tags$br(),
           tags$a(tags$img(src ="cevr.png",  width = "120px", height = "60px"), href="http://healtheconomics.tuftsmedicalcenter.org/orchard", target = "_blank"),
           tags$a(tags$img(src = "BMGF.png",width = "240px", height = "55px"), href="https://www.gatesfoundation.org", target = "_blank")
  )
) #close UI



####SEVER####
server <- function(input, output, session) {
  
  #general info about filters
  output$generalinfo<-renderText({paste('<center>',"Use the menus below to filter data. Multiple selections are permitted:", '<br>', '<br>')})
  
  #populate select input options
  updateSelectizeInput(session, 'keywords', choices = c('',keywords$Keywords), server = TRUE)
  updateSelectizeInput(session, 'disease', choices = c('',disease_choices), server = TRUE)
  updateSelectizeInput(session, 'interventiontype', choices = c('', "Pharmaceutical", "Health Education or Behavior", "Surgical", "Multiple Interventions", "Immunization", "Environmental Augmentation", "Medical Procedure", "Other", "Nutrition", "Environmental Remediation", "Screening", "Diagnostic", "Medical Device", "Legislation", "Maternal or Neonatal Care", "Care Delivery"), server = TRUE)
  updateSelectizeInput(session, 'intervention', choices = c('', leaguetable_data$InterventionPhrase), server = TRUE)
  updateSelectizeInput(session, 'comparator', choices = c('', leaguetable_data$Comparator), server = TRUE)
  updateSelectizeInput(session, 'country', choices = c('', Names$Country, "Not reported"), server = TRUE)
  updateSelectizeInput(session, 'region', choices =  c('', Names$SuperRegion, "Not reported"), server = TRUE)
  updateSelectizeInput(session, 'sponsor', choices =  c('', Names$Sponsor, "Not reported"), server = TRUE)
  updateSelectizeInput(session, 'ICD10', choices = c("", leaguetable_data$ICD10Code), server = TRUE)
  updateSelectizeInput(session, 'gender', choices = c("", leaguetable_data$Gender), server = TRUE)
  updateSelectizeInput(session, 'age', choices = c("", Names$TargetPop), server = TRUE)
  updateSelectizeInput(session, 'author', choices = c("", leaguetable_data$Author), server = TRUE)
  
  #reset filters when reset button is hit
  observeEvent(input$reset,{
    updateSelectizeInput(session, 'keywords', choices = c('',keywords$Keywords), server = TRUE)
    updateSelectizeInput(session, 'disease', choices = c('',disease_choices), server = TRUE)
    updateSelectizeInput(session, 'interventiontype', choices = c('', "Pharmaceutical", "Health Education or Behavior", "Surgical", "Multiple Interventions", "Immunization", "Environmental Augmentation", "Medical Procedure", "Other", "Nutrition", "Environmental Remediation", "Screening", "Diagnostic", "Medical Device", "Legislation", "Maternal or Neonatal Care", "Care Delivery"), server = TRUE)
    updateSelectizeInput(session, 'intervention', choices = c('', leaguetable_data$InterventionPhrase), server = TRUE)
    updateSelectizeInput(session, 'comparator', choices = c('', leaguetable_data$Comparator), server = TRUE)
    updateSelectizeInput(session, 'country', choices = c('', Names$Country, "Not reported"), server = TRUE)
    updateSelectizeInput(session, 'region', choices =  c('', Names$SuperRegion, "Not reported"), server = TRUE)
    updateSelectizeInput(session, 'sponsor', choices =  c('', Names$Sponsor, "Not reported"), server = TRUE)
    updateSelectizeInput(session, 'ICD10', choices = c("", leaguetable_data$ICD10Code), server = TRUE)
    updateSelectizeInput(session, 'gender', choices = c("", leaguetable_data$Gender), server = TRUE)
    updateSelectizeInput(session, 'age', choices = c("", Names$TargetPop), server = TRUE)
    updateSelectizeInput(session, 'author', choices = c("", leaguetable_data$Author), server = TRUE)
    updateSliderInput(session, 'year', min = 1995, max = 2018, value = c(1995, 2018), step = 1)
  })
  
  #toggle advanced filters when advanced filter button clicked
  observeEvent(input$advanced,{
    shinyjs::toggleElement('intervention')
    shinyjs::toggleElement('sponsor')
    shinyjs::toggleElement('ICD10')
    shinyjs::toggleElement('gender')
    shinyjs::toggleElement('age')
    shinyjs::toggleElement('author')
  })
  
  #currently filtered text
  output$filtered<-renderText({paste('<center>','<font size="3">', '<b>','Showing ', nrow(output_table()), " of ", nrow(leaguetable_data), " total interventions.")})
  observe(
    if(!is.null(input$keywords) | !is.null(input$disease) | !is.null(input$interventiontype) | !is.null(input$intervention) | (input$year[1]>1995 | input$year[2]<2018)
       | !is.null(input$comparator) | !is.null(input$country) | !is.null(input$region) | !is.null(input$sponsor)
       | !is.null(input$ICD10) | !is.null(input$gender)| !is.null(input$age)| !is.null(input$author))
    {
      shinyjs::showElement('filtered')
    }
    else if(is.null(input$keywords) & is.null(input$disease) & is.null(input$interventiontype) & is.null(input$intervention) & (input$year[1]<=1995 | input$year[2]>=2018)
            & is.null(input$comparator) & is.null(input$country) & is.null(input$region) & is.null(input$sponsor)
            & is.null(input$ICD10) & is.null(input$gender)& is.null(input$age)& is.null(input$author))
    {
      shinyjs::hideElement('filtered')
    }
  )  
  
  #make table reactive to filters
  output_table<-reactive({
    
    #logic paths for input filters - will react to input drop downs and subset league table accordingly
    league_table_subset<-leaguetable_data
    
    #reactive to: keywords
    if(!is.null(input$keywords)){
      keywords_subset<-subset(keywords, keywords$Keywords %in% input$keywords)
      league_table_subset<-subset(league_table_subset, league_table_subset$RatioID %in% keywords_subset$RatioID)
    }
    
    #reactive to: disease filter
    if(!is.null(input$disease)){
      league_table_subset<-subset(league_table_subset, 
                                  GBDDis1_tier1 %in% input$disease |GBDDis1_tier2 %in% input$disease |GBDDis1_tier3 %in% input$disease |GBDDis1_tier4 %in% input$disease |
                                    GBDDis2_tier1 %in% input$disease |GBDDis2_tier2 %in% input$disease |GBDDis2_tier3 %in% input$disease |GBDDis2_tier4 %in% input$disease |
                                    GBDDis3_tier1 %in% input$disease |GBDDis3_tier2 %in% input$disease |GBDDis3_tier3 %in% input$disease |GBDDis3_tier4 %in% input$disease |
                                    GBDDis4_tier1 %in% input$disease |GBDDis4_tier2 %in% input$disease |GBDDis4_tier3 %in% input$disease |GBDDis4_tier4 %in% input$disease |
                                    GBDDis5_tier1 %in% input$disease |GBDDis5_tier2 %in% input$disease |GBDDis5_tier3 %in% input$disease |GBDDis5_tier4 %in% input$disease)
    }
    
    #reactive to: country filter
    if(!is.null(input$country)){
      league_table_subset<-subset(league_table_subset, Country_Afghanistan_s %in% input$country |	Country_Albania_s %in% input$country |	Country_Algeria_s %in% input$country |	
                                    Country_AmericanSamoa_s %in% input$country |	Country_Andorra_s %in% input$country |	Country_Angola_s %in% input$country |	Country_AntiguaandBarbuda_s %in% input$country |
                                    Country_Argentina_s %in% input$country |	Country_Armenia_s %in% input$country |	Country_Aruba_s %in% input$country |	Country_Australia_s %in% input$country |
                                    Country_Austria_s %in% input$country |	Country_Azerbaijan_s %in% input$country |	Country_Bahamas_s %in% input$country |	Country_Bahrain_s %in% input$country |
                                    Country_Bangladesh_s %in% input$country |	Country_Barbados_s %in% input$country |	Country_Belarus_s %in% input$country |	Country_Belgium_s %in% input$country |
                                    Country_Belize_s %in% input$country |	Country_Benin_s %in% input$country |	Country_Bermuda_s %in% input$country |	Country_Bhutan_s %in% input$country |
                                    Country_Bolivia_s %in% input$country |	Country_BosniaandHerzegovina_s %in% input$country |	Country_Botswana_s %in% input$country |	Country_Brazil_s %in% input$country |
                                    Country_BritishVirginIslands_s %in% input$country |	Country_Brunei_s %in% input$country |	Country_Bulgaria_s %in% input$country |	
                                    Country_BurkinaFaso_s %in% input$country |	Country_Burundi_s %in% input$country |	Country_CaboVerde_s %in% input$country |	Country_Cambodia_s %in% input$country |
                                    Country_Cameroon_s %in% input$country |	Country_Canada_s %in% input$country |	Country_CaymanIslands_s %in% input$country |	
                                    Country_CentralAfricanRepublic_s %in% input$country |	Country_Chad_s %in% input$country |	Country_ChannelIslands_s %in% input$country |	
                                    Country_Chile_s %in% input$country |	Country_China_s %in% input$country |	Country_Colombia_s %in% input$country |	Country_Comoros_s %in% input$country |
                                    Country_CongoDemRep_s %in% input$country |	Country_CongoRep_s %in% input$country |	Country_CostaRica_s %in% input$country |	Country_CotedIvoire_s %in% input$country |
                                    Country_Croatia_s %in% input$country |	Country_Cuba_s %in% input$country |	Country_Curaco_s %in% input$country |	Country_Cyprus_s %in% input$country |
                                    Country_CzechRepublic_s %in% input$country |	Country_Denmark_s %in% input$country |	Country_Djibouti_s %in% input$country |	Country_Dominica_s %in% input$country |
                                    Country_DominicanRepublic_s %in% input$country |	Country_Ecuador_s %in% input$country |	Country_EgyptArabRep_s %in% input$country |	Country_ElSalvador_s %in% input$country |
                                    Country_EquatorialGuinea_s %in% input$country |	Country_Eritrea_s %in% input$country |	Country_Estonia_s %in% input$country |	Country_Ethiopia_s %in% input$country |
                                    Country_FaroeIslands_s %in% input$country |	Country_Fiji_s %in% input$country |	Country_Finland_s %in% input$country |	Country_France_s %in% input$country |
                                    Country_FrenchPolynesia_s %in% input$country |	Country_Gabon_s %in% input$country |	Country_GambiaThe_s %in% input$country |	Country_Georgia_s %in% input$country |
                                    Country_Germany_s %in% input$country |	Country_Ghana_s %in% input$country |	Country_Gibraltar_s %in% input$country |	Country_Greece_s %in% input$country |
                                    Country_Greenland_s %in% input$country |	Country_Grenada_s %in% input$country |	Country_Guam_s %in% input$country |	Country_Guatemala_s %in% input$country |
                                    Country_Guinea_s %in% input$country |	Country_GuineaBissau_s %in% input$country |	Country_Guyana_s %in% input$country |	Country_Haiti_s %in% input$country |
                                    Country_Honduras_s %in% input$country |	Country_HongKongSARChina_s %in% input$country |	Country_Hungary_s %in% input$country |	Country_Iceland_s %in% input$country |
                                    Country_India_s %in% input$country |	Country_Indonesia_s %in% input$country |	Country_IranIslamicRep_s %in% input$country |	Country_Iraq_s %in% input$country |
                                    Country_Ireland_s %in% input$country |	Country_IsleofMan_s %in% input$country |	Country_Israel_s %in% input$country |	Country_Italy_s %in% input$country |
                                    Country_Jamaica_s %in% input$country |	Country_Japan_s %in% input$country |	Country_Jordan_s %in% input$country |	Country_Kazakhstan_s %in% input$country |
                                    Country_Kenya_s %in% input$country |	Country_Kiribati_s %in% input$country |	Country_KoreaDemPeoplesRep_s %in% input$country |	Country_KoreaRep_s %in% input$country |
                                    Country_Kosovo_s %in% input$country |	Country_Kuwait_s %in% input$country |	Country_KyrgyzRepublic_s %in% input$country |	Country_LaoPDR_s %in% input$country |
                                    Country_Latvia_s %in% input$country |	Country_Lebanon_s %in% input$country |	Country_Lesotho_s %in% input$country |	Country_Liberia_s %in% input$country |
                                    Country_Libya_s %in% input$country |	Country_Liechtenstein_s %in% input$country |	Country_Lithuania_s %in% input$country |	Country_Luxembourg_s %in% input$country |
                                    Country_MacaoSARChina_s %in% input$country |	Country_MacedoniaFYR_s %in% input$country |	Country_Madagascar_s %in% input$country |	Country_Malawi_s %in% input$country |
                                    Country_Malaysia_s %in% input$country |	Country_Maldives_s %in% input$country |	Country_Mali_s %in% input$country |	Country_Malta_s %in% input$country |
                                    Country_MarshallIslands_s %in% input$country |	Country_Mauritania_s %in% input$country |	Country_Mauritius_s %in% input$country |	Country_Mexico_s %in% input$country |
                                    Country_MicronesiaFedSts_s %in% input$country |	Country_Moldova_s %in% input$country |	Country_Monaco_s %in% input$country |	Country_Mongolia_s %in% input$country |
                                    Country_Montenegro_s %in% input$country |	Country_Morocco_s %in% input$country |	Country_Mozambique_s %in% input$country |	Country_Myanmar_s %in% input$country |
                                    Country_Namibia_s %in% input$country |	Country_Nauru_s %in% input$country |	Country_Nepal_s %in% input$country |	Country_Netherlands_s %in% input$country |
                                    Country_NewCaledonia_s %in% input$country |	Country_NewZealand_s %in% input$country |	Country_Nicaragua_s %in% input$country |	Country_Niger_s %in% input$country |
                                    Country_Nigeria_s %in% input$country |	Country_NorthernMarianaIslands_s %in% input$country |	Country_Norway_s %in% input$country |	Country_Oman_s %in% input$country |
                                    Country_Pakistan_s %in% input$country |	Country_Palau_s %in% input$country |	Country_Panama_s %in% input$country |	Country_PapuaNewGuinea_s %in% input$country |
                                    Country_Paraguay_s %in% input$country |	Country_Peru_s %in% input$country |	Country_Philippines_s %in% input$country |	Country_Poland_s %in% input$country |
                                    Country_Portugal_s %in% input$country |	Country_PuertoRico_s %in% input$country |	Country_Qatar_s %in% input$country |	Country_Romania_s %in% input$country |
                                    Country_RussianFederation_s %in% input$country |	Country_Rwanda_s %in% input$country |	Country_Samoa_s %in% input$country |	Country_SanMarino_s %in% input$country |
                                    Country_SaoTomeandPrincipe_s %in% input$country |	Country_SaudiArabia_s %in% input$country |	Country_Senegal_s %in% input$country |	Country_Serbia_s %in% input$country |
                                    Country_Seychelles_s %in% input$country |	Country_SierraLeone_s %in% input$country |	Country_Singapore_s %in% input$country |	Country_SintMaartenDutchpart_s %in% input$country |
                                    Country_SlovakRepublic_s %in% input$country |	Country_Slovenia_s %in% input$country |	Country_SolomonIslands_s %in% input$country |	Country_Somalia_s %in% input$country |
                                    Country_SouthAfrica_s %in% input$country |	Country_SouthSudan_s %in% input$country |	Country_Spain_s %in% input$country |	Country_SriLanka_s %in% input$country |
                                    Country_StKittsandNevis_s %in% input$country |	Country_StLucia_s %in% input$country |	Country_StMartinFrenchpart_s %in% input$country |	
                                    Country_StVincentandtheGrenadine_s %in% input$country |	Country_Sudan_s %in% input$country |	Country_Suriname_s %in% input$country |	Country_Swaziland_s %in% input$country |
                                    Country_Sweden_s %in% input$country |	Country_Switzerland_s %in% input$country |	Country_SyrianArabRepublic_s %in% input$country |	Country_Tajikistan_s %in% input$country |
                                    Country_Tanzania_s %in% input$country |	Country_Thailand_s %in% input$country |	Country_TimorLeste_s %in% input$country |	Country_Togo_s %in% input$country |
                                    Country_Tonga_s %in% input$country |	Country_TrinidadandTobago_s %in% input$country |	Country_Tunisia_s %in% input$country |	Country_Turkey_s %in% input$country |
                                    Country_Turkmenistan_s %in% input$country |	Country_TurksandCaicosIslands_s %in% input$country |	Country_Tuvalu_s %in% input$country |	Country_Uganda_s %in% input$country |
                                    Country_Ukraine_s %in% input$country |	Country_UnitedArabEmirates_s %in% input$country |	Country_UnitedKingdom_s %in% input$country |	
                                    Country_UnitedStates_s %in% input$country |	Country_Uruguay_s %in% input$country |	Country_Uzbekistan_s %in% input$country |	Country_Vanuatu_s %in% input$country |
                                    Country_VenezuelaRB_s %in% input$country |	Country_Vietnam_s %in% input$country |	Country_VirginIslandsUS_s %in% input$country |	Country_WestBankandGaza_s %in% input$country |
                                    Country_YemenRep_s %in% input$country |	Country_Zambia_s %in% input$country |	Country_Zimbabwe_s %in% input$country |	Country_TaiwanChina_s %in% input$country |
                                    Country_CookIslands_s %in% input$country |	Country_Niue_s %in% input$country |	Country_WallisandFutuna_s %in% input$country |	Country_NA_s %in% input$country|
                                    Country_display %in% input$country)
      
    }
    
    #reactive to: intervention filter
    if(!is.null(input$intervention)){
      league_table_subset<-subset(league_table_subset, InterventionPhrase %in% input$intervention)
    }
    
    #reactive to: intervention type filter
    if(!is.null(input$interventiontype)){
      league_table_subset<-subset(league_table_subset, Intervention1 %in% input$interventiontype | Intervention2 %in% input$interventiontype |
                                    Intervention3 %in% input$interventiontype | Intervention4 %in% input$interventiontype)
    }
    
    #reactive to: comparator filter    
    if(!is.null(input$comparator)){
      league_table_subset<-subset(league_table_subset, Comparator %in% input$comparator)
    }
    
    #reactive to: region filter
    if(!is.null(input$region)){
      league_table_subset<-subset(league_table_subset, SuperRegion_s_SSA %in% input$region | SuperRegion_s_SEA %in% input$region | SuperRegion_s_SA %in% input$region
                                  | SuperRegion_s_NAME %in% input$region | SuperRegion_s_HI %in% input$region | SuperRegion_s_ECA %in% input$region | SuperRegion_s_LA %in% input$region)
    }
    
    #reactive to: sponsor filter
    if(!is.null(input$sponsor)){
      league_table_subset<-subset(league_table_subset, Sponsor_s_academic%in% input$sponsor |Sponsor_s_government%in% input$sponsor 
                                  |Sponsor_s_intergovernmental%in% input$sponsor |Sponsor_s_foundation%in% input$sponsor |Sponsor_s_gates%in% input$sponsor 
                                  |Sponsor_s_industry%in% input$sponsor |Sponsor_s_healthcare%in% input$sponsor |Sponsor_s_proforg%in% input$sponsor 
                                  |Sponsor_s_none%in% input$sponsor)
    }
    
    #reactive to: year filter (two-sided)
    if(!is.null(input$year)){
      league_table_subset<-subset(league_table_subset, (Year >= input$year[1])&(Year <= input$year[2]))
    }
    
    #reactive to: ICD10 filter
    if(!is.null(input$ICD10)){
      league_table_subset<-subset(league_table_subset, ICD10Code %in% input$ICD10)
    }
    
    #reactive to: gender filter
    if(!is.null(input$gender)){
      league_table_subset<-subset(league_table_subset, Gender  %in% input$gender)
    }
    
    #reactive to: age filter
    if(!is.null(input$age)){
      league_table_subset<-subset(league_table_subset, TP_s_age05 %in% input$age | TP_s_age611 %in% input$age |  TP_s_age1218 %in% input$age | 
                                    TP_s_age1940 %in% input$age |  TP_s_age4164 %in% input$age |  TP_s_age65 %in% input$age |  TP_s_ageunspec %in% input$age | 
                                    TP_s_ageunknown %in% input$age)
    }
    
    #reactive to: author filter
    if(!is.null(input$author)){
      league_table_subset<-subset(league_table_subset, Author %in% input$author)
    }
    
    #reorder and set data to be appropriate for outputted league table
    league_table_output<-data.frame(league_table_subset$TitleAut, league_table_subset$Year, league_table_subset$Sponsor_display, league_table_subset$Disease_display,
                                    league_table_subset$Country_display, league_table_subset$TargetPopulation, league_table_subset$InterventionCombined,
                                    league_table_subset$Comparator, league_table_subset$DisplayRatio, league_table_subset$percent_GDP,
                                    league_table_subset$GDPCategory,
                                    #these columns are not visible in the output table
                                    league_table_subset$NumericICER,league_table_subset$RatioID)
    
    colnames(league_table_output)<-c("Article Title (Author)", "Year", "Sponsor", "Disease", "Country", "Target Population", "Intervention", "Comparator",
                                     "ICER ($/DALY averted)", "ICER as % of GDP*", "GDP Category (2016)^", "NumericICER", "RatioID")
    
    
    #order by descending articleID
    league_table_output<-league_table_output[order(league_table_output$Year, decreasing = TRUE), ]
    
    return(league_table_output)
  }) #close reactive table subsetting
  
  #render output table of filtered/selected data
  output$league_table = DT::renderDataTable({
    
    DT::datatable(output_table(),
                  options = list(
                    #disable search
                    searching = FALSE,
                    #default to show 5 per page
                    pageLength = 10,
                    #sort on numeric icer in ascending rder
                    order = list(11, 'asc'),
                    #hide columns 11-18
                    columnDefs = (list(list(visible = FALSE, targets=c(11,12))))
                    #targets=c(11,12,13, 14,15, 16,17, 18, 19))))
                  ),
                  #caption
                  caption = "GH CEA Registry League Table: Sorted by ICER from lowest to highest. Cost-Saving interventions (green) are considered lowest, Dominated interventions (red) highest.",
                  #do not show row numbers
                  rownames = FALSE) %>%
      #color depending on thier content: light green for cost-saving, yellow/green for less than 1xgdp, yellow for 1-3xgdp, orange for >3xgdp,  red for dominated
      formatStyle('GDP Category (2016)^', backgroundColor = styleEqual(c("Cost-Saving", "Cost-saving", 'Less than 1xGDP', "1-3xGDP", "Greater than 3xGDP", "Dominated"),
                                                                       c('#7BFD6D', '#7BFD6D', '#BAFD6D', '#EEFD6D', '#FDE06D', '#FD856D')))
    
  }) #close render function
  
  #Downloadable csv of filtered/selected dataset ----
  #subset ratios full dataset to just filtered selections
  data_download<-reactive({
    data_download<-suppressWarnings(ratiosfull)
    data_download<-subset(data_download, data_download$RatioID %in% output_table()$RatioID)
    return(data_download)
  })
  
  output$download <- downloadHandler(
    #name of the download file
    filename = "GHCEAregistry_leaguetable.csv",
    content = function(file) {
      write.csv(data_download(), file, row.names = FALSE)
    },
    #download as .csv
    contentType = "text/csv"
  ) #close download handler
  
  #info about downloading text output
  gates_url<-a("here!", href = 'http://healtheconomics.tuftsmedicalcenter.org/orchard/download-dataset', target="_blank")
  output$downloadinfo<-renderText({paste("This button downloads all ratio variables for selected data.", "<br>", "For access to the full registry (including information on methods and weights) and data dictionary, click ", gates_url, '<br>', '<br>')})
  
  #threshold disclaimer info text
  disclaimer_url<-a('found here.', href = 'http://healtheconomics.tuftsmedicalcenter.org/orchard/thresholds', target = "_blank")
  output$percentGDPdisclaimer<-renderText({paste("*Because numeric ratios are not calculated for cost-saving or dominated interventions, ICER as a percentage of GDP cannot be calculated.")})
  
  #threshold disclaimer info text
  disclaimer_url<-a('found here.', href = 'http://healtheconomics.tuftsmedicalcenter.org/orchard/thresholds', target = "_blank")
  output$thresholddisclaimer<-renderText({paste("^Although we color-coded the ICERs based on country-specific GDP per capita to improve visibility, we do not endorse any specific cost-effectiveness thresholds as reflective of good or reasonable value for money. More information ", disclaimer_url)})
  
} #close server

# Run the application 
shinyApp(ui = ui, server = server)