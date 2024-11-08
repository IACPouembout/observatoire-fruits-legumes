# Activation des packages

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(here)
library(leaflet)
library(showtext)
library(readxl)
library(sf)
library(scales)
library(hrbrthemes)
library(lubridate)
library(broom)
library(extrafont)
library(ggrepel)
library(report)
library(shinycssloaders)
library(leaflet.minicharts)
library(janitor)
library(data.table)
library(htmlwidgets)

options(scipen = 999)
tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"



# Import des données

davar_fruits_legumes_total <- read_csv(here("davar_fruits_legumes_total.csv"))
#données davar total

geo_prix_commune1 <-read_csv(here("geo_prix_commune.csv"))
geo_prix_categorie1 <- read_csv(here("geo_prix_categorie.csv"))

#données des prix par commune

davar_fruits_legumes <- read_csv(here("davar_fruits_legumes_V2.csv"))
#données davar détaillées

#Fichiers condensés d'évolution de la production
evo_prod1 <- read_rds(here("evo_prod1.rds"))
evo_prod1_total <- read_csv(here("evo_prod1_total.csv"))
davar_fruits_legumes_total <- read_csv(here("davar_fruits_legumes_total.csv"))

davar_fruits_legumes <- read_csv(here("davar_fruits_legumes_V2.csv"))

imports_fruits_legumes_V2 <- read_csv("imports_fruits_legumes_V2.csv")

#Fichiers condensés d'évolution des prix
evo_prix1<- read_csv( here("evo_prix1.csv"))

evo_prix1_total<- read_csv( here("evo_prix1_total.csv"))


# Création des cartes
nc_communes <- read_sf("nc_communes_simp.geojson")
prod_agri_tribu_region <- read_csv(here("prod_agri_tribu_region.csv"))%>%
  select(-c(Perdue,Total))%>%
  adorn_totals("col")%>%
  data.table()



#fichiers geographiques pour la cartographie des tribus
regions <- read_sf(here("regions_simp.geojson"))
pop_total <- read_rds(here("pop_total.rds"))
pop_total_province <- pop_total%>%select(Province,menages_province)%>%unique()


repartition_agri_tribu_region <- read_csv(here("repartition_agri_tribu_region.csv"))


#fichier pour la carte des destinations pour le total
map_agri_tribu_region_total <- regions%>%left_join(prod_agri_tribu_region%>%
                                                     filter(`N_group_prod veget`=="Productions végétales totales")%>%
                                                     group_by(N_region)%>%
                                                     summarise_if(is.numeric,sum)
)


#fichier de répartition des ménages agricoles pour la 1ere carte
percent_agri_tribu_region_total <- regions%>%left_join(repartition_agri_tribu_region%>%
  filter(`N_group_prod veget`=="Productions végétales totales")%>%
  select(-c(`N_group_prod veget`,`N_productions vegetales`))%>%
  unique()%>%
  group_by(N_region)%>%
  summarise(n=sum(Coef_extrap_GD))%>%
  left_join(pop_total)%>%
  mutate(percent=(n/menages_region)*100)  )
  
  
#fichier pomme de terre pour la 1ere carte qui s'affichera

geo_prix_nc1 <- geo_prix_commune1%>%
  filter(variete=="Pomme De Terre" )%>%
  pivot_longer(local:import,names_to = "categorie",values_to = "prix_moyen")%>%
  group_by(commune)%>%
  summarise(prix_moyen=mean(prix_moyen,na.rm=T))%>%
  inner_join(nc_communes)%>%st_as_sf()%>%
  mutate(text=paste0(commune,":\n",round(prix_moyen)," XPF"))  



#Création des listes

colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F")

fruits_legumes_tribu <- c("Arbre fruitier","Banane dessert","Banane poingo","Cocotier","Cultures spéciales","Légume & fruit plein champ","Productions végétales totales","Tubercule")

legumes <- c("Aubergine","Brocoli","Carotte","Chou De Chine","Chou Rouge","Chou-Fleur","Chouchoute","Concombre","Courgette","Céleri","Maïs","Navet","Oignons","Patate Douce","Persil","Poireau","Poivron","Radis","Salade","Tomate")
fruits <- c("Ananas","Avocat","Banane","Citron, Lime","Letchi","Mandarine, Clémentine","Mangue","Melon","Orange","Pamplemousse, Pomélo","Papaye","Pastèque")


legumes_full <-  c("Ail","Asperge","Aubergine","Basilic","Betterave","Brocoli","Carotte","Champignon","Chou : Autres","Chou De Chine","Chou Rouge","Chou Vert","Chou-Fleur","Chouchoute","Ciboulette, Oignon Vert","Citrouille","Concombre","Courgette","Cresson","Céleri","Échalote","Endive","Gingembre","Haricot Vert","Herbe Aromatique : Autres","Igname","Légume Frais : Autres","Légume Vert : Autres","Manioc","Maïs","Menthe","Navet","Oignon Blanc","Oignon Jaune","Oignon Rouge","Patate Carry","Patate Douce","Persil","Piment","Poireau","Poivron","Pomme De Terre","Potimarron","Potiron","Pâtisson","Radis","Salade","Squash","Taro","Tomate")
fruits_full <-  c("Abricot","Agrume : Autres","Ananas","Avocat","Banane","Banane Poingo","Banane Verte","Cerise","Citron, Lime","Combawa","Corossol","Fraise","Framboise","Fruit De La Passion, Pomme-Liane","Fruit Du Dragon, Pitaya","Fruit Exotique : Autres","Fruit Frais : Autres","Kiwi","Letchi","Mandarine, Clémentine","Mangue","Melon","Nectarine","Orange","Pamplemousse, Pomélo","Papaye","Pastèque","Poire","Pomme","Prune","Pêche","Raisin")


font_add_google(name = "Sedgwick Ave",   
                family = "sedgwick")
showtext_auto()



# Paramètres graphiques
#titres, styles et logos
header_img <- div(
  img(src="https://www.agripedia.nc/sites/default/files/2020-07/LOGO%20IAC%20COUL%20RVB.png", height="45px"),
  div(
    class = "my-title",
    h4('Observatoire calédonien des fruits et légumes'),
    tags$style(".my-title :is(h4, h5){font-family: 'Permanent Marker', cursive;color: #36648B; font-weight: bold;}")
  ),
  style = "display: flex;"
)

header <-  htmltools::tagQuery(dashboardHeader(title = ""))
header <- header$
  addAttrs(style = "position: relative")$ 
  find(".navbar.navbar-static-top")$ 
  append(header_img)$ 
  allTags() #Page d'acceuil

#Définition des onglets
ui<- dashboardPage( skin = "black",
                    header = header,
                    dashboardSidebar(width = 350,sidebarMenu(
                      menuItem("Presentation",tabName = "presentation",icon = icon("question")),
                      menuItem("Suivi mensuel de la production et des prix",tabName = "prix_prod",icon = icon("chart-line")),
                      menuItem("Géographie des prix",tabName = "map_prix",icon = icon("map")),
                      menuItem("Les importations dans le Pacifique",tabName = "pacific",icon = icon("chart-simple")),
                      menuItem("L'agriculture en tribu",tabName = "agri_tribu",icon = icon("leaf")),
                      
                      menuItem("Méthodes",tabName = "method",icon = icon("magnifying-glass-chart"))
                    )
                    
                    ),
                    dashboardBody(tags$head(tags$style(".pickerInput .dropdown-menu {z-index: 2000 !important;}",
                      ".leaflet .label {
                 font-family: 'Sedgwick Ave';
                 }",
                 ".leaflet .legend {
                 font-family: 'Sedgwick Ave';
                 }",
                 ".leaflet .legend i{
               font-family: 'Sedgwick Ave';
                 }"),
               tags$link(rel = "stylesheet", type = "text/css", href = "theme2.css")),
               tabItems(
                 tabItem(tabName = "presentation",mainPanel(
                   
                   #Onglet présentation
                   #texte et logo
                   fluidPage(
                     h1("Présentation de l'observatoire"),
                     
                     
                     p("Bienvenue sur l'observatoire calédonien des fruits et légumes",style="font-size:20px;"),
                     p("Ce tableau de bord a été crée dans le cadre du", a(href = 'https://dataviz.pacificdata.org/', ' Pacific Dataviz Challenge 2023 ', .noWS = "outside"),"pour permettre un suivi temporel et spatial des prix des fruits et légumes, en faisant le lien avec les données de ventes et d'importations."),
                     p("Ce dernier utilise le jeux de données de la DAE de suivi des prix, le jeux de données de la DAVAR de suivi des productions et ventes, ainsi que la Pacific Food Trade Database de la Communauté du Pacifique Sud (CPS)."),
                     tags$div(tags$ul(
                       tags$li(tags$span(icon("chart-line"),"L'onglet 'Suivi de la production et des prix' vous permet de suivre la production, les importations (uniquement à partir de 2021) et l'évolution des prix des fruits et légumes, sur la période 2011-2023." ,br(),"L'utilisateur peut également afficher les cyclones ayant eu lieu sur la période, la liste de ces derniers est issue de la base cyclonique de Météo France.",br(), "L'effet de l'évolution de la production sur les prix est représenté graphiquement et est calculé par régression linéaire.")),
                       tags$li(tags$span(icon("map"),"L'onglet 'Géographie des prix' vous permet de comparer le prix moyen selon les communes, sur la période 2011-2023.")),
                       tags$li(tags$span(icon("chart-simple"),"L'onglet 'Les importations dans le Pacifique' vous permet de comparer les quantités de fruits ou légumes importés dans chaque pays du Pacifique. Il est possible de visualiser le total et la moyenne, par habitant ou non, sur la période 1995-2018.")),
                       tags$li(tags$span(icon("leaf"),"L'onglet 'L'agriculture en tribu' vous permet de consulter les résultats de l'enquête Agriculture en tribu, menée en 2010 auprès de 1429 groupes domestiques représentatifs de la popuilation tribale, cette enquête a permis d'établir des extrapolations des quantités de productions végétales par grandes régions."))
                       
                     )),
                     p("Ce tableau de bord vous est présenté par l'équipe TeRAu (SHS) de l'Institut Agronomique Néo-Calédonien.",br(),"Cette initative s'inscrit dans la continuité de ses travaux de recherche menés par l'IAC sur la compréhension des systèmes alimentaires en Nouvelle-Calédonie et dans le Pacifique."),
                     p("Établissement public de recherche agronomique, l'IAC accompagne le développement d'une agriculture durable en Nouvelle-Calédonie. Avec ses partenaires, il mobilise la science, son expertise, l'innovation et la formation pour favoriser les transitions agroécologiques, la préservation de la biodiversité, l'adaptation face aux changements globaux et le développement durable des territoires ruraux."), a(href = 'https://www.iac.nc/', ' Visitez le site pour en savoir plus ', .noWS = "outside",style="margin: 0 auto; display:block; text-align: center"),
                     tags$figure(
                       align = "center",
                       tags$img(
                         src = "https://www.agripedia.nc/sites/default/files/2020-07/LOGO%20IAC%20COUL%20RVB.png",
                         width = 450
                       )),br()
                     
                   )
                   
                 )
                 
                 ),
                 
                 #onglet production et prix
                 tabItem(
                   tabName = "prix_prod",  
                   fluidPage(h1("Suivi de la production et des prix"),   
                             
                             
                             
                             
                             #1er graphique 
                             fluidRow(
                               column(6,
                                      withSpinner(plotOutput( "evo_prod_plot1",height = "45em"),image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true")  )  , 
                               column(6,
                                      box(title = p(style="font-family: 'Sedgwick Ave', cursive;","Type de produit et années"),  
                                          selectInput("prod_type1","Choisissez la famille de produit",
                                                      choices =c("Fruits","Légumes"),selected = "Légumes" ),
                                          #les catégories changent si on choisit fruit ou légumes
                                          conditionalPanel(condition = "input.prod_type1=='Légumes'",
                                                           selectInput("prod_variete1", "Choisissez le légume", 
                                                                       choices = c(legumes,"Total legumes"),selected ="Tomate")),
                                          conditionalPanel(condition = "input.prod_type1=='Fruits'",
                                                           selectInput("prod_variete1b", "Choisissez le fruit", 
                                                                       choices = c(fruits, "Total fruits (hors vanille)"),selected = "Orange")),
                                          #bouton pour choisir la période
                                          sliderInput(inputId = "prod_annee1",
                                                      label ="Années de suivi",
                                                      min = 2012,
                                                      max = 2023,
                                                      value = c(2020, 2023),sep = ""),
                                          #bouton pour visualiser les cyclones
                                          checkboxInput(inputId = "cyclones",label = "Afficher les cyclones",value = F),
                                          #sortie texte sur les corrélations prix et produciton
                                          uiOutput("text_prod1"),width = 10 ))),
                             
                             fluidRow( column(6,
                                              #graphique d'évolution des prix
                                              #withspinner pour chargement plus fluide
                                              withSpinner(
                                                plotOutput("evo_prix_plot1",height = "40em")
                                                            ,image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true")),
                                       #graphique à points prix vs production
                                       column(6,
                                              withSpinner(plotOutput("prix_prod_plot1",height = "40em") ,image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true"))),
                             
                             
                             
                             
                             
                             
                             
                   )),
                 tabItem(
                   tabName = "map_prix",  
                   #sortie leaflet du prix par commune
                   fluidPage(h1("Géographie des prix"),   
                             fluidRow(      
                               column(6,
                                      withSpinner(leafletOutput( "nc_prix2",height = "40em") ,image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true")),
                               column(6,#bouton de sélection du type de produit
                                      box(title = p(style="font-family: 'Sedgwick Ave', cursive;","Type de produit et années"),  
                                          selectInput("prix_type2","Choisissez la famille de produit",
                                                      choices =c("Fruits","Légumes"),selected = "Légumes" ),
                                          conditionalPanel(condition = "input.prix_type2=='Légumes'",
                                                           selectInput("prix_variete2", "Choisissez le légume", 
                                                                       choices = c(legumes_full),selected ="Pomme De Terre")),
                                          conditionalPanel(condition = "input.prix_type2=='Fruits'",
                                                           selectInput("prix_variete2b", "Choisissez le fruit", 
                                                                       choices = c(fruits_full),selected = "Orange")),
                                          
                                          sliderInput(inputId = "prix_annee2",
                                                      label ="Années de suivi",
                                                      min = 2012,
                                                      max = 2023,
                                                      value = c(2020, 2023),sep = ""),
                                          uiOutput("text_prix2"),width = 10))),
                             fluidRow(column(6,withSpinner(plotOutput("prix_province_plot",height = "40em" ) ,image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true"))))),
                 
                 tabItem(
                   tabName = "agri_tribu",  
                   fluidPage(tags$head(tags$style('.pickerInput .dropdown-menu {z-index: 2000 !important;}')),
                             fluidRow(column(4,h1("L'agriculture en tribu"),p("Que représentent réellement les activités agricoles des familles résidant en tribu en Nouvelle-Calédonie ? La nécessité de poser un regard nouveau et original sur ces activités a abouti en 2011 au projet d’une grande enquête sur le poids et les fonctions de l’agriculture en tribu. Cette enquête a été conduite par l’IAC avec l'appui du CIRAD.")),     
                                      column(4,  box(selectInput("agri_tribu_prod","Choisissez la famille de produit",
                                                                                                 
                                                                 choices =c(fruits_legumes_tribu),selected = "Productions végétales totales" ),
                                                                                     
                                                     #modalités conditionnelles selon la famille de produit                                 
                                                     conditionalPanel(condition ='input.agri_tribu_prod=="Productions végétales totales"|
                                                       input.agri_tribu_prod=="Arbre fruitier" |input.agri_tribu_prod=="Cultures spéciales"|
                                                       input.agri_tribu_prod=="Céréales et fourrages"|input.agri_tribu_prod=="Légume & fruit plein champ"|
                                                       input.agri_tribu_prod=="Tubercule"',
                                                       
                                                       pickerInput("Sélectionner le(s) produit(s)" , options = list(`actions-box` = TRUE, `deselect-all-text` = "Aucun",className = "pickerInput",virtualScroll=T,
                                                                                                                    `select-all-text` = "Tous"), 
                                                                   multiple=T,inputId = 'agri_tribu_prod2', 
                                                                   choices = c(),
                                                                   selected= c())),width = 10)),column(4,br(),br(),br(), p("Durant près de cinq mois, une équipe de chercheurs et d’enquêteurs de l’IAC a parcouru la Nouvelle-Calédonie afin de rencontrer 1 786 ménages répartis dans 288 tribus qui ont ainsi été interrogés. Les données ont ensuite été extrapolées pour fournir des résultats globaux qui valent pour l’ensemble de la population considérée."))),
                           
                             fluidRow(
                              
                               column(6,h1('Quantités totales par région (en tonnes)'),
                                      withSpinner(leafletOutput( "map_agri_total",height = "40em") ,image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true")),
                               column(6,h1('Quantités totales par région et destinations (en tonnes)'),
                                      withSpinner(leafletOutput( "map_agri_destinations",height = "40em") ,image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true"),
                               ))
                             ,fluidRow(column(6,withSpinner(plotOutput("agri_tribu_total",height = "40em" ) ,
                                                            image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true"))
                                       
                                       ,column(6,withSpinner(plotOutput("agri_tribu_destination",height = "40em" ) ,
                                                            image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true")),
                                       
                                       )        ,
                             fluidRow(column(6,withSpinner(plotOutput("agri_tribu_percent",height = "40em" ) ,
                                                                               image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true"))
                                                          
                                                          ,column(6, h1("Groupes domestiques concernés (%)"),   withSpinner(leafletOutput("map_agri_percent_total",height = "40em" ) ,
                                                                                image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true")),
                                                          
                                       )
                             
                   )),
                 
                 
                 tabItem(
                   tabName = "pacific",
                   fluidPage(h1("Importations de fruits et légumes dans le Pacifique"),
                             fluidRow(
                               column(8,
                                      withSpinner(plotOutput("imports_total_plot",height = "40em" ) ,image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true")),
                               column(4,
                                      box(title = p(style="font-family: 'Sedgwick Ave', cursive;","Type de produit et années"),
                                          selectInput("pacific_type3","Choisissez la famille de produit",
                                                      choices =c("Fruits","Légumes et tubercules"),selected = "Légumes et tubercules" ),
                                          checkboxInput(inputId = "par_hab",label = "Quantités par habitant",value = T),
                                          sliderInput(inputId = "pacific_annee3",
                                                      label ="Années de suivi",
                                                      min = 1995,
                                                      max = 2018,
                                                      value = c(2016, 2018),sep = ""),width = 10))),
                             fluidRow(column(8,withSpinner(plotOutput("imports_moyenne_plot",height = "40em" ) ,image ="https://github.com/daattali/shinycssloaders/blob/master/inst/img/custom.gif?raw=true")))
                   ) ),
                 
                 
                 
                 tabItem(tabName = "method",
                         fluidPage(
                           h1("Méthodes"),
                           p("Cet onglet tente de détailler la méthode utilisée pour la production des différents graphiques :"),
                           tags$div(tags$ul(
                             tags$li(tags$span("Au sein de l'onglet",  icon("chart-line"),"'Suivi de la production et des prix' :" ,
                                               br(),
                                               "Dans le", a(href ="https://data.gouv.nc/explore/dataset/prix-des-produits-alimentaires-en-2023-pacific-dataviz-challenge/information", " jeu de données de la DAE "), "le prix de chaque produit de la catégorie choisie est ramené au kg, ce qui permet ensuite de calculer une moyenne mensuelle.",
                                               br(),
                                               "A noter que pour faire le lien avec les données de la DAVAR, seuls les produits disponibles dans les deux jeux de données ont été gardés, un recodage a parfois été nécessaire pour faire correspondre les noms de produit.",
                                               br(),
                                               "Les", a(href="https://data.gouv.nc/explore/dataset/pacificdatavizchallenge_fruits_et_legumes_regroupement1/information", " données de la DAVAR ") ,"n'ont pas nécessité d'autres calculs que des sommes et des % pour le texte.",
                                               br(),
                                               "Celles-ci ont été combinées avec" ,a(href="https://georep-dtsi-sgt.opendata.arcgis.com/maps/63e27e6671324498838e4944035a3cc0/about", "les données de Météo France"), "pour visualiser les périodes cycloniques.",
                                               br(),
                                               "Enfin, la relation entre le prix au marché de gros (variable à expliquer) et le volume déclaré commercialisé (variable explicative) est calculée par",a(href = 'https://larmarange.github.io/guide-R/analyses/regression-lineaire.html', ' régression linéaire simple ', .noWS = "outside"), "sur le jeu de données de la DAVAR, les volumes importés ne sont pas inclus dans la régression car disponibles uniquement à partir de 2021."
                                               
                             )),br(),
                             tags$li("Au sein de l'onglet",  icon("map"),"'Géographie des prix' :" , 
                                     br(),
                                     "Seul le jeu de données de la DAE est utilisé, ce qui explique le plus large choix de produits.",
                                     br(),
                                     "Ici aussi, le prix de chaque produit est ramené au kg, ce qui permet de calculer un prix moyen par commune.", 
                                     br(),
                                     "Rappelons qu'il s'agit toujours de moyennes, possiblement sensibles aux valeurs extrêmes, la couverture du territoire dépend du produit et de la période choisie."
                             ),br(),
                             tags$li("Au sein de l'onglet",  icon("chart-simple"),"'Les importations dans le Pacifique' :" , 
                                     br(),
                                     "Le jeu de données" ,
                                     a(href="https://stats.pacificdata.org/vis?tm=trade&pg=0&snb=14&df%5Bds%5D=ds%3ASPC2&df%5Bid%5D=DF_TRADE_FOOD&df%5Bag%5D=SPC&df%5Bvs%5D=1.0&pd=%2C2018&dq=A.Q.AS%2BCK%2BFJ%2BFM%2BGU%2BKI%2BMH%2BMP%2BNC%2BNR%2BNU%2BPF%2BPG%2BPN%2BPW%2BSB%2BTK%2BTO%2BTV%2BVU%2BWF%2BWS.AU_NZ.10%2B11%2B12%2B15%2B16%2B17%2B18%2B19%2B20%2B21%2B22%2B24%2B02%2B03%2B04%2B07%2B08%2B09&ly%5Bcl%5D=TIME_PERIOD&ly%5Brw%5D=COMMODITY&to%5BTIME_PERIOD%5D=false", " Pacific Food Trade Database "), 
                                     "est utilisé ainsi que les" ,a(href="https://stats.pacificdata.org/vis?fs[0]=Topic%2C0%7CPopulation%23POP%23&pg=0&fc=Topic&bp=true&snb=9&df[ds]=ds%3ASPC2&df[id]=DF_POP_PROJ&df[ag]=SPC&df[vs]=3.0&pd=2017%2C2027&dq=A..MIDYEARPOPEST._T._T&ly[rw]=GEO_PICT&ly[cl]=TIME_PERIOD&to[TIME_PERIOD]=false", " données démographiques "), " disponibles sur le Pacific Data Hub pour le calcul des quantités par habitant.",
                                     br(),
                                     "Les importations de fruits/légumes n'y sont disponibles qu'aggrégées, distinguant les fruits des légumes et tubercules.",
                                     br(),
                                     "Les seuls calculs effectués sont des sommes et des moyennes."
                             ), br(),
                             tags$li("Au sein de l'onglet",  icon("leaf"),"'L'agriculture en tribu :" , 
                                     br(),
                                     "Les données brutes de l'enquête" ,
                                     a(href="https://www.cahiersagricultures.fr/articles/cagri/full_html/2018/02/cagri170184/cagri170184.html", " Agriculture en tribu (2010) "), 
                                     "sont utilisés pour extrapoler les quantités de productions végétales produites, en utilisant les coefficients d'extrapolation à l'echelle des groupes domestiques."
                             )
                             
                           )),
                           br(),
                           
                           p("Ce tableau de bord a été crée sous shinydashboard (v0.7.2) en utilisant R Statistical language (v4.1.0; R Core Team, 2021) sur Windows 10 x64, et en utilisant les packages hrbrthemes (v0.8.0), broom (v0.8.0), lubridate (v1.8.0), ggplot2 (v3.4.3), stringr (v1.5.0), forcats (v1.0.0), scales (v1.2.1), readxl (v1.4.0), purrr (v1.0.1), tidyr (v1.3.0), readr (v2.1.4), dplyr (v1.1.2), leaflet (v2.1.0), ggrepel (v0.9.1), here (v1.0.1), tibble (v3.2.1), report (v0.5.1), sf (v1.0.7), tidyverse (v1.3.1), extrafont (v0.18), shinydashboard (v0.7.2), shiny (v1.7.1),shinycssloaders (v1.0.0), showtextdb (v3.0), sysfonts (v0.8.8) et showtext (v0.9.5)."),
                           br(),
                           p("Les scripts et les données seront prochainement mis en ligne sur Github (ou autre solution de stockage), en attendant ceux-ci peuvent être fournies via demande à jonasbrouillon@gmail.com ou jonas.brouillon@iac.nc")
                           
                           
                           
                           
                           
                         ))
                 
                 
               )))




# Déclaration de la fonction server pour Shiny

server <- function(input, output, session) {

  # Arrête l'application lorsque la session de l'utilisateur se termine
  session$onSessionEnded(function() {
    stopApp()
  })
  
  
  # Fonction réactive pour calculer le pourcentage d'agriculteurs par région
  
percent_agri_tribu_region <- reactive({
  # Vérifie si `input$agri_tribu_prod2` n'est pas NULL
  
    if(!is.null(input$agri_tribu_prod2) ){
      # Jointure avec les données de répartition pour les productions végétales sélectionnées
      
          regions%>%left_join(repartition_agri_tribu_region%>%
      filter(`N_productions vegetales`%in% c(input$agri_tribu_prod2))%>%
        select(-c(`N_group_prod veget`,`N_productions vegetales`))%>%
  unique()%>%
      group_by(N_region)%>%
      summarise(n=sum(Coef_extrap_GD))%>%
      left_join(pop_total)%>%
      mutate(percent=(n/menages_region)*100  ))}else{       # Si `input$agri_tribu_prod2` est NULL, utilise `input$agri_tribu_prod`

        regions%>%left_join(repartition_agri_tribu_region%>%
          filter(`N_group_prod veget`==input$agri_tribu_prod)%>%
            select(-c(`N_group_prod veget`,`N_productions vegetales`))%>%
          unique()%>%
          group_by(N_region)%>%
          summarise(n=sum(Coef_extrap_GD))%>%
          left_join(pop_total)%>%
          mutate(percent=(n/menages_region)*100  ) )
        
      }})
# Fonction réactive pour calculer le pourcentage d'agriculteurs par province

percent_agri_tribu_province<- reactive({
  if(!is.null(input$agri_tribu_prod2) ){
repartition_agri_tribu_region%>%
                          filter(`N_productions vegetales`%in% c(input$agri_tribu_prod2))%>%
      select(-c(`N_group_prod veget`,`N_productions vegetales`))%>%
      
                          unique()%>%
                          group_by(Province)%>%
                          summarise(n=sum(Coef_extrap_GD))%>%
                          left_join(pop_total_province)%>%
                          mutate(percent=n/menages_province)%>%
      bind_rows(repartition_agri_tribu_region%>%
                  filter(`N_productions vegetales`%in% c(input$agri_tribu_prod2))%>%
                  select(-c(`N_group_prod veget`,`N_productions vegetales`))%>%
                  
                  unique()%>%mutate(Province="Total")%>%
                  group_by(Province)%>%
                  summarise(n=sum(Coef_extrap_GD))%>%
                  mutate(percent=n/sum(pop_total_province$menages_province)))%>%
      mutate(Province=factor(Province,c("Province Sud","Province des Îles","Province Nord","Total")))
      }else{
                            repartition_agri_tribu_region%>%
                                                  filter(`N_group_prod veget`==input$agri_tribu_prod)%>%
          select(-c(`N_group_prod veget`,`N_productions vegetales`))%>%
          
                                                  unique()%>%
                                                  group_by(Province)%>%
                                                  summarise(n=sum(Coef_extrap_GD))%>%
                                                  left_join(pop_total_province)%>%
                                                  mutate(percent=n/menages_province) %>%
          bind_rows(repartition_agri_tribu_region%>%
                      filter(`N_group_prod veget`==input$agri_tribu_prod)%>%
                      select(-c(`N_group_prod veget`,`N_productions vegetales`))%>%
                      
                      unique()%>%mutate(Province="Total")%>%
                      group_by(Province)%>%
                      summarise(n=sum(Coef_extrap_GD))%>%
                      mutate(percent=n/sum(pop_total_province$menages_province)))%>%
          mutate(Province=factor(Province,c("Province Sud","Province des Îles","Province Nord","Total")))
          
                            
                          }})
# Sortie graphique pour afficher le pourcentage d'agriculteurs par province

output$agri_tribu_percent <- renderPlot({
ggplot(percent_agri_tribu_province())+
  geom_segment( aes(x=Province , xend=Province , y=percent, yend=0), color="grey") +
  geom_point( aes(x=Province, y=percent), size=3 ,color="#ff8e3c") +
  geom_text_repel(
    box.padding = 0.9, alpha=0.5,family="sedgwick",
    nudge_x = .15,
    nudge_y = 0.02,
    segment.curvature = +0.3,min.segment.length = 0,
    arrow = arrow(length = unit(0.015, "npc")), 
    aes(x=Province,label=scales::percent(percent,accuracy = 0.1),y=percent),bg.color="white")+
  coord_flip()+
  scale_y_percent()+
  theme_ipsum(base_size =12,base_family ="sedgwick" )+
  labs(x="",y="",color="",title = "Part des groupes domestiques cultivant\nla production par Province (%)",caption = "Source : IAC")})


# Sortie carte Leaflet pour afficher les pourcentages par région

output$map_agri_percent_total <- renderLeaflet({
  pal <- colorNumeric(
    palette = "RdYlBu",
    domain = percent_agri_tribu_region_total$percent,reverse = T
  )
  
  leaflet(data = percent_agri_tribu_region_total)%>%
    addProviderTiles(provider=providers$OpenStreetMap.HOT)%>%
    addPolylines(data = regions,weight = 1,color = "black")%>%
    addPolygons(data = percent_agri_tribu_region_total,stroke = FALSE, layerId = ~N_region,
                fillOpacity = 0.5, smoothFactor = 0.5, color = ~pal(percent),label =~paste(N_region,":", round(percent,1),"%")  , 
                labelOptions = labelOptions( 
                  style = list("font-weight" = "cursive", padding = "3px 8px","font-family" = "Sedgwick Ave"), 
                  textsize = "13px", 
                  direction = "auto"))%>%
    addLegend(data =percent_agri_tribu_region_total,"topright", pal = pal, values = ~percent,
              labFormat = labelFormat( suffix = '%'),title = paste("Part des groupes domestiques cultivant la production (%)"), layerId = ~N_region, )
  
  
  })
# Observe pour mettre à jour la carte lorsqu'il y a des changements dans les données régionales

observe({
  if(nrow(percent_agri_tribu_region())>0){
  
  pal <- colorNumeric(
    palette = "RdYlBu",
    domain = percent_agri_tribu_region()$percent,reverse = T
  )
  
  
  leafletProxy("map_agri_percent_total")%>%
    clearShapes()%>%
    clearControls()%>%
    addPolylines(data = regions,weight = 1,color = "black")%>%
    addPolygons(data = percent_agri_tribu_region(),stroke = FALSE, layerId = ~N_region,
                fillOpacity = 0.5, smoothFactor = 0.5, color = ~pal(percent),label =~paste(N_region,":", round(percent,1),"%")  , 
                labelOptions = labelOptions( 
                  style = list("font-weight" = "cursive", padding = "3px 8px","font-family" = "Sedgwick Ave"), 
                  textsize = "13px", 
                  direction = "auto"))%>%
    addLegend(data =percent_agri_tribu_region(),"topright", pal = pal, values = ~percent,labFormat = labelFormat( suffix = '%'),title = paste("Part des groupes domestiques cultivant la production (%)"), layerId = ~N_region, )
  
  }
    })


  #Sortie graphique pour les quantités récoltés en tribu
output$agri_tribu_total <- renderPlot({
  if(!is.null(input$agri_tribu_prod2) ){
        prod_agri_tribu_region%>%ungroup()%>%
          filter(`N_productions vegetales`%in% c(input$agri_tribu_prod2))%>%
          group_by(Province)%>%
          summarise_if(is.numeric,sum)%>%
      janitor::adorn_totals("row")%>%
      ggplot()+
      geom_segment( aes(x=fct_reorder(Province,Total) , xend=fct_reorder(Province,Total) , y=Total, yend=0), color="grey") +
      geom_point( aes(x=fct_reorder(Province,Total) , y=Total), size=3 ,color="#ff8e3c") +
      geom_text_repel(
        box.padding = 0.9, alpha=0.5,family="sedgwick",
        nudge_x = .15,
        nudge_y = 0.2,
        segment.curvature = +0.3,min.segment.length = 0,
        arrow = arrow(length = unit(0.015, "npc")), 
        aes(x=fct_reorder(Province,Total),label=paste(format(round(Total),big.mark=" ") ,"tonnes"),y=Total),bg.color="white")+
      coord_flip()+
      scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"t")})+
      
      theme_ipsum(base_size =12,base_family ="sedgwick" )+
      labs(x="",y="",color="",title = "Quantités totales par Province",caption = "Source : IAC")
      
  }else{

        prod_agri_tribu_region%>%ungroup()%>%
          filter(`N_group_prod veget`==input$agri_tribu_prod)%>%
      group_by(Province)%>%
      summarise_if(is.numeric,sum)%>%
      janitor::adorn_totals("row")%>%
      ggplot()+
      geom_segment( aes(x=fct_reorder(Province,Total) , xend=fct_reorder(Province,Total) , y=Total, yend=0), color="grey") +
      geom_point( aes(x=fct_reorder(Province,Total) , y=Total), size=3 ,color="#ff8e3c") +
      geom_text_repel(
                      box.padding = 0.9, alpha=0.5,family="sedgwick",
                      nudge_x = .15,
                      nudge_y = 0.2,
                      segment.curvature = +0.3,min.segment.length = 0,
                      arrow = arrow(length = unit(0.015, "npc")), 
                      aes(x=fct_reorder(Province,Total),label=paste(format(round(Total),big.mark=" ") ,"tonnes"),y=Total),bg.color="white")+
      coord_flip()+
      scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"t")})+
      
      theme_ipsum(base_size =12,base_family ="sedgwick" )+
      labs(x="",y="",color="",title = "Quantités totales par Province",caption = "Source : IAC")
  }})
    
#Sortie graphique pour les productions en tribus selon la destination
output$agri_tribu_destination <- renderPlot({
  if(!is.null(input$agri_tribu_prod2) ){
    prod_agri_tribu_region%>%ungroup()%>%
      filter(`N_productions vegetales`%in% c(input$agri_tribu_prod2))%>%
      mutate(Province=fct_reorder(Province,Total,sum))%>%
      select(-c(Total))%>%
      group_by(Province)%>%
      summarise_if(is.numeric,sum)%>%
      janitor::adorn_totals("row")%>%
      janitor::adorn_percentages(denominator = 'row') %>%
      pivot_longer(Animaux:Semences)%>%
      ggplot(aes(y=Province , x=value,fill=name,label=percent(value,accuracy = 1)))+
      geom_bar( stat = "identity",alpha=0.6) +
scale_x_percent()+scale_fill_brewer(palette = "Set2")+geom_text(position = position_fill(.5))+
      theme_ipsum(base_size =12,base_family ="sedgwick" )+
      labs(x="",y="",fill="",title = "Quantités totales par destination (%)",caption = "Source : IAC")
    
  }else{
    
    prod_agri_tribu_region%>%ungroup()%>%
      filter(`N_group_prod veget`==input$agri_tribu_prod)%>%
      mutate(Province=fct_reorder(Province,Total,sum))%>%
      select(-c(Total))%>%
      group_by(Province)%>%
      summarise_if(is.numeric,sum)%>%
      janitor::adorn_totals("row")%>%
      janitor::adorn_percentages(denominator = 'row') %>%
      pivot_longer(Animaux:Semences)%>%
      ggplot(aes(y=Province , x=value,fill=name,label=percent(value,accuracy = 1)))+
      geom_bar(stat = "identity",alpha=0.6) +geom_text(position = position_fill(.5))+
      scale_x_percent()+scale_fill_brewer(palette = "Set2")+
      theme_ipsum(base_size =12,base_family ="sedgwick" )+
      labs(x="",y="",fill="",title = "Quantités totales par destination (%)",caption = "Source : IAC")
  }})


#Sortie leaflet pour les productions des tribus par région
  output$map_agri_total <- renderLeaflet({
    leaflet(data = map_agri_tribu_region_total)%>%
      addProviderTiles(provider=providers$OpenStreetMap.HOT)%>%
      addPolylines(data=map_agri_tribu_region_total,weight = 1,color = "black")%>%
      addMinicharts(map_agri_tribu_region_total$Long,map_agri_tribu_region_total$Lat,
                    layerId = map_agri_tribu_region_total$N_region, 
                    maxValues = max(as_tibble(map_agri_tribu_region_total$Total)),
                    chartdata = as_tibble(map_agri_tribu_region_total$Total),showLabels = T
      )})
  
  #Observe pour mettre à jour la carte
  observe({

    if(!is.null(input$agri_tribu_prod2) ){
    map_agri_tribu_region  <- regions%>%
          left_join(
            prod_agri_tribu_region%>%ungroup()%>%
              filter(`N_productions vegetales`%in% c(input$agri_tribu_prod2))%>%
              group_by(N_region)%>%
              summarise_if(is.numeric,sum))
      }else{
        map_agri_tribu_region  <-   regions%>%
          left_join(
            prod_agri_tribu_region%>%ungroup()%>%
              filter(`N_group_prod veget`==input$agri_tribu_prod)%>%
              group_by(N_region)%>%
              summarise_if(is.numeric,sum))
      }
      
    
    leafletProxy("map_agri_total")%>%
      updateMinicharts(layerId = map_agri_tribu_region$N_region,maxValues = max(as_tibble(map_agri_tribu_region$Total)),
                       chartdata = as_tibble(map_agri_tribu_region$Total),showLabels = T)})
  
  
  output$map_agri_destinations <- renderLeaflet({
    leaflet(data = map_agri_tribu_region_total)%>%
      addProviderTiles(provider=providers$OpenStreetMap.HOT)%>%
      addPolylines(data=map_agri_tribu_region_total,weight = 1,color = "black")%>%
      addMinicharts(map_agri_tribu_region_total$Long,map_agri_tribu_region_total$Lat,
                    layerId = map_agri_tribu_region_total$N_region, type = "pie",colorPalette = colors,
                    chartdata = as_tibble(map_agri_tribu_region_total)[5:9],
      )})
  
observe({

  #Construction des catégories en cascade pour la sélection des produits
  last_prod <- reactiveVal("default")
  
  if(input$agri_tribu_prod != last_prod()) {  
if(input$agri_tribu_prod=="Arbre fruitier"){
  updatePickerInput(session,'agri_tribu_prod2',      choices = c("Avocat","Carambole",
                                           "Citron et lime",
                                           "Cœur de bœuf","Combava",
                                           "Corossol","Fruit à pain",
                                           "Goyave","Jacques","Litchi","Mandarine",
                                           "Mangue","Orange","Pamplemousse","Papaye",
                                           "Pêche","Pomme cannelle","Pomme citerne","Pomme kanak",
                                           "Tamarin","Autres fruits (à préciser)","Cerisier","Murier",
                                           "Jamelonier","Kaki","Pomme rose"),
                               selected=c("Avocat","Carambole",
                                          "Citron et lime",
                                          "Cœur de bœuf","Combava",
                                          "Corossol","Fruit à pain",
                                          "Goyave","Jacques","Litchi","Mandarine",
                                          "Mangue","Orange","Pamplemousse","Papaye",
                                          "Pêche","Pomme cannelle","Pomme citerne","Pomme kanak",
                                          "Tamarin","Autres fruits (à préciser)","Cerisier","Murier",
                                          "Jamelonier","Kaki","Pomme rose"))}
  else if(input$agri_tribu_prod=="Cultures spéciales"){
  
updatePickerInput(session,'agri_tribu_prod2', 
                               choices = c("Café arabica","Café robusta","Vanille",
                                           "Plantes médicinale de plein champ",
                                           "Condiment et épices divers","Canne à sucre",
                                           "Citronnelle","Autre culture spéciale (à préciser)",
                                           "Bois de santal","Autre bois (à préciser)","Feuilles de niaouli"),
                               selected=c("Café arabica","Café robusta","Vanille",
                                          "Plantes médicinale de plein champ",
                                          "Condiment et épices divers","Canne à sucre",
                                          "Citronnelle","Autre culture spéciale (à préciser)",
                                          "Bois de santal","Autre bois (à préciser)","Feuilles de niaouli"))}
  else if(input$agri_tribu_prod=="Céréales et fourrages"){

updatePickerInput(session,'agri_tribu_prod2', 
                               choices = c("Blé","Maïs","Sorgho","Autre céréale : préciser",
                                           "Maïs fourrage / ensilage","Sorgho fourrager","Graminées",
                                           "Légumineuses","Autre fourrage : préciser :"),
                               selected=c("Blé","Maïs","Sorgho","Autre céréale : préciser",
                                          "Maïs fourrage / ensilage","Sorgho fourrager","Graminées",
                                          "Légumineuses","Autre fourrage : préciser :"))}
  else if(input$agri_tribu_prod=="Légume & fruit plein champ"){
  
updatePickerInput(session,'agri_tribu_prod2', 
                               choices = c("Aubergine","Brèdes","Carotte","Céleri","Chou de chine","Chou rouge",
                                           "Chou vert","Chou kanak","Chouchoute","Citrouille","Concombre",
                                           "Courgette","Echalote","Haricot beurre","Haricot chinois",
                                           "Haricot vert","Maïs doux","Navet","Oignon vert","Persil",
                                           "Piment","Poireau","Poivron","Pomme de terre","Radis","Salade",
                                           "Tomate","Fines herbes","Autres légumes (à préciser)",
                                           "Haricot toutes sortes","Oignon sec","Squash",
                                           "Pomme de terre saison/conservation","Autre légume de plein champ (à préciser)",
                                           "Ananas","Fraise","Melon","Pastèque","Pomme liane","Ambrevade","Tomate cerise",
                                           "Choux de montagne (iles)","Framboisier"),
                               selected=c("Aubergine","Brèdes","Carotte","Céleri","Chou de chine","Chou rouge",
                                          "Chou vert","Chou kanak","Chouchoute","Citrouille","Concombre",
                                          "Courgette","Echalote","Haricot beurre","Haricot chinois",
                                          "Haricot vert","Maïs doux","Navet","Oignon vert","Persil",
                                          "Piment","Poireau","Poivron","Pomme de terre","Radis","Salade",
                                          "Tomate","Fines herbes","Autres légumes (à préciser)",
                                          "Haricot toutes sortes","Oignon sec","Squash",
                                          "Pomme de terre saison/conservation","Autre légume de plein champ (à préciser)",
                                          "Ananas","Fraise","Melon","Pastèque","Pomme liane","Ambrevade","Tomate cerise",
                                          "Choux de montagne (iles)","Framboisier"))}
  else if(input$agri_tribu_prod=="Tubercule"){
updatePickerInput(session,'agri_tribu_prod2', 
                               choices = c("Igname de saison","Manioc","Patate douce",
                                           "Taro bourbon","Taro de montagne","Taro d'eau",
                                           "Autre tubercule tropical (à préciser)"),
                               selected=c("Igname précoce","Igname de saison","Igname tardif",
                                          "Manioc","Patate douce","Taro bourbon","Taro de montagne",
                                          "Taro d'eau","Autre tubercule tropical (à préciser)"))}
  else if(input$agri_tribu_prod=="Productions végétales totales"){
    updatePickerInput(session,'agri_tribu_prod2', 
                      choices = c(fruits_legumes_tribu[-c(7)]),
                      selected=c(fruits_legumes_tribu[-c(7)])) }else if(input$agri_tribu_prod %in% c("Banane dessert","Banane poingo","Cocotier")){
                        updatePickerInput(session, 'agri_tribu_prod2', choices =NULL, selected = "")
                        
}} })
  


  
  observe({

    if(!is.null(input$agri_tribu_prod2) ){
      map_agri_tribu_region  <- regions%>%
        left_join(
          prod_agri_tribu_region%>%ungroup()%>%
            filter(`N_productions vegetales`%in% c(input$agri_tribu_prod2))%>%
            group_by(N_region)%>%
            summarise_if(is.numeric,sum))
    }else{
      map_agri_tribu_region  <-   regions%>%
        left_join(
          prod_agri_tribu_region%>%ungroup()%>%
            filter(`N_group_prod veget`==input$agri_tribu_prod)%>%
            group_by(N_region)%>%
            summarise_if(is.numeric,sum))
    }
    
    leafletProxy("map_agri_destinations")%>%
      updateMinicharts(layerId = map_agri_tribu_region$N_region,type = "pie",colorPalette = colors,
                       chartdata = as_tibble(map_agri_tribu_region)[5:9])


    })
  
  
  
  
  
  input_variete1 <- reactive({if(input$prod_type1=="Légumes"){
    input$prod_variete1}else{input$prod_variete1b}})
  
  evo_prod_1 <- reactive({
    
    if(!input_variete1()%in% c("Total fruits (hors vanille)","Total legumes")){
      
      evo_prod1%>%
        filter(regroupement1==input_variete1() & annee>=input$prod_annee1[1]&annee <=input$prod_annee1[2] )
    }else{ evo_prod1_total%>%
        filter(regroupement1==input_variete1() & annee>=input$prod_annee1[1]&annee <=input$prod_annee1[2] )    }
    
    
    
  })
  
  evo_prod_1bis <-  reactive({
    
    if(!input_variete1()%in% c("Total fruits (hors vanille)","Total legumes")){
      
      davar_fruits_legumes%>%
        filter(regroupement1==input_variete1() & annee>=input$prod_annee1[1]&annee <=input$prod_annee1[2])
      
    }else{
      
      davar_fruits_legumes_total%>%
        filter(regroupement1==input_variete1() & annee>=input$prod_annee1[1]&annee <=input$prod_annee1[2])
      
    }
    
  })
  
  
  
  evo_prod_besoin<-  reactive({
    evo_prod_1bis()%>%
      summarise(percent=sum(poids_enquete_kg,na.rm=T)/sum(besoin_marche_kg,na.rm = T) )
    
    
  })
  
  
  dates_quotas <- reactive({evo_prod_1()%>%
      select(quotas_date_fin,quotas_date_debut)%>%
      unique()})
  
  
  
  #Création du graphique d'évolution de la production
  #Affichage ou non des cyclones si activation du bouton
  output$evo_prod_plot1<-renderPlot({
    if(input$cyclones==T){
      ggplot( )+
        geom_line(data=evo_prod_1(),aes(x=date_arrondie,y=value/1000,color=name,linetype=name),stat = "identity",size=1.2)+
        geom_label_repel(data=evo_prod_1()%>%filter(name=="Volume déclaré\ncommercialisé par\nles producteurs"), 
                         box.padding = 0.6, alpha=0.5,family="sedgwick",
                         nudge_x = -.15,
                         nudge_y = .15,
                         segment.curvature = +0.3,min.segment.length = 0,
                         arrow = arrow(length = unit(0.015, "npc")), 
                         aes(x=date_arrondie,label=nom,y=value/1000,fill=type_max),bg.color="white")+
        scale_color_manual(values = c("Besoin estimé\ndu marché"="black", 
                                      "Volume déclaré\ncommercialisé par\nles producteurs"="#078080",
                                      "Volume écoulé par\nle marché de gros"="#d9376e",
                                      "Volumes importés\navec quotas"="#ff8e3c",
                                      "Quotas autorisés"="#ff8e3c"
        ))+
        scale_linetype_manual(values = c("Besoin estimé\ndu marché"=3,
                                         "Volume déclaré\ncommercialisé par\nles producteurs"=1,
                                         "Volume écoulé par\nle marché de gros"=3,
                                         "Volumes importés\navec quotas"=1,
                                         "Quotas autorisés"=3
        ))+
        geom_rect(data =dates_quotas(), aes(fill="Périodes d'autorisations\nde quotas",  xmin = quotas_date_debut, xmax = quotas_date_fin, ymin = -Inf, ymax = Inf),alpha=0.3) +
        scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"tonnes")})+
        theme_ipsum(base_size =14,base_family ="sedgwick" )+theme(legend.position = "bottom",legend.box="vertical")+
        guides(color = guide_legend(nrow = 2, byrow = TRUE,linetype=guide_legend(nrow = 2, byrow = TRUE),fill=guide_legend(nrow = 2, byrow = TRUE)))+
        labs(x="",y="",color="",linetype="",fill="",title = "Volumes écoulés",caption = "Sources : DAVAR, Météo France\nLes données d'importations ne sont disponibles qu'à partir de 2021")+
        scale_fill_manual(values = c("Cyclone tropical\ntrès intense"="#EE2C2C",
                                     "Cyclone tropical\nintense"="#FF7256",
                                     "Périodes d'autorisations\nde quotas"="grey"
                                     
        ),  
        guide = guide_legend(override.aes = list(alpha = 0.3,label = "")))}else{
          
          ggplot( )+
            geom_line(data=evo_prod_1(),aes(x=date_arrondie,y=value/1000,color=name,linetype=name),stat = "identity",size=1.2)+
            
            scale_color_manual(values = c("Besoin estimé\ndu marché"="black", 
                                          "Volume déclaré\ncommercialisé par\nles producteurs"="#078080",
                                          "Volume écoulé par\nle marché de gros"="#d9376e",
                                          "Volumes importés\navec quotas"="#ff8e3c",
                                          "Quotas autorisés"="#ff8e3c"))+
            scale_linetype_manual(values = c("Besoin estimé\ndu marché"=3,
                                             "Volume déclaré\ncommercialisé par\nles producteurs"=1,
                                             "Volume écoulé par\nle marché de gros"=3,
                                             "Volumes importés\navec quotas"=1,
                                             "Quotas autorisés"=3
            ))+
            geom_rect(data =dates_quotas(), aes(fill="Périodes d'autorisations\nde quotas",  xmin = quotas_date_debut, xmax = quotas_date_fin, ymin = -Inf, ymax = Inf),alpha=0.3) +
            scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"tonnes")})+
            theme_ipsum(base_size =14,base_family ="sedgwick" )+theme(legend.position = "bottom")+
            guides(color = guide_legend(nrow = 3, byrow = TRUE),linetype=guide_legend(nrow = 3, byrow = TRUE),fill=guide_legend(nrow = 3, byrow = TRUE))+
            labs(x="",y="",color="",linetype="",fill="",title = "Volumes écoulés",caption = "Sources : DAVAR\nLes données d'importations ne sont disponibles qu'à partir de 2021")+
            scale_fill_manual(values = 'grey',  
                              guide = guide_legend(override.aes = list(alpha = 0.3)))                  
          
          
        }
    
    
  })
  
  #Sortie graphique de l'évolution des prix
  
  output$prix_prod_plot1 <- renderPlot({evo_prod_1()%>%
      filter(name%in% c("Volume déclaré\ncommercialisé par\nles producteurs",
                        "Volumes importés\navec quotas"))%>%
      ggplot(aes(x=value/1000,y=valeur_moy,color=name))+
      geom_point()+
      scale_color_manual(values = c("#078080","#ff8e3c"))+
      geom_smooth(data=evo_prod_1()%>%
                    filter(name%in% c("Volume déclaré\ncommercialisé par\nles producteurs"))
                  ,aes(x=value/1000,y=valeur_moy),method = "lm",colour="black")+
      scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"XPF/kg")})+
      scale_x_continuous(labels = function(x){paste(format(x,big.mark=" "),"t")})+
      theme(plot.caption.position = "panel")+
      theme_ipsum(base_size =14,base_family ="sedgwick" )+labs(caption = "Source : DAVAR\nLa droite de régression représente la relation linéaire entre le prix moyen au marché de\ngros et le volume déclaré commercialisé par les producteurs",  x="Volumes (tonnes)",y="Prix moyen au marché de gros (XPF)",color="",title = "Relation entre le prix au marché de gros et\nles quantités écoulées")})
  
  
  evo_prix_1 <- reactive({
    
    if(!input_variete1()%in% c("Total fruits (hors vanille)","Total legumes")){
      
      evo_prix1%>%
        filter(variete==input_variete1()& annee>=input$prod_annee1[1]&annee <=input$prod_annee1[2] )%>%
        bind_rows(evo_prod_1()%>%
                    mutate(prix_moyen=valeur_moy)%>% 
                    select(date_arrondie,prix_moyen)%>%
                    unique()%>%
                    mutate(categorie="Prix moyen au marché de gros") )}else{
                      
                      evo_prix1_total%>%
                        filter(sous_famille==input_variete1()& annee>=input$prod_annee1[1]&annee <=input$prod_annee1[2] )%>%
                        bind_rows(evo_prod_1()%>%
                                    mutate(prix_moyen=valeur_moy)%>% 
                                    select(date_arrondie,prix_moyen)%>%
                                    unique()%>%
                                    mutate(categorie="Prix moyen au marché de gros") )
                      
                      
                      
                    }
    
    
    
  })
  
  
  #sortie graphique pour l'évolution des prix
  output$evo_prix_plot1<-renderPlot({ggplot( )+
      geom_line(data=evo_prix_1(),aes(x=date_arrondie,y=prix_moyen,color=categorie),size=1.2)+
      scale_color_manual(values = c("Prix moyen au marché de gros"="#d9376e",
                                    "Prix moyen du produit local en magasin"="#078080",
                                    "Prix moyen du produit importé en magasin"="#ff8e3c"))+
      geom_rect(data =dates_quotas(), aes(fill="Périodes d'autorisations\nde quotas",  xmin = quotas_date_debut, xmax = quotas_date_fin, ymin = -Inf, ymax = Inf),alpha=0.3) +
      scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"XPF/kg")})+
      theme_ipsum(base_size =14,base_family ="sedgwick" )+theme(legend.position = "bottom")+
      guides(color = guide_legend(nrow = 3, byrow = TRUE))+
      labs(x="",y="",color="",linetype="",fill="",title = "Évolution du prix",caption = "Sources : DAE, DAVAR")+
      scale_fill_manual(values = 'grey',  
                        guide = guide_legend(override.aes = list(alpha = 0.3))) })
  
  
  #régression linéaire selon les données choisies
  model=reactive( {lm(data = evo_prod_1bis(),valeur_moy~poids_enquete_t)%>%
      tidy()%>%
      mutate(interpretation=ifelse(p.value<=0.05,
                                   paste("l'effet de l'augmentation de la production sur le prix est <b>siginificatif : 100 tonnes</b> de production supplémentaires équivaut à<b>",round(estimate *100),"XPF</b> sur le prix moyen au marché de gros"),
                                   "<b>l'effet de l'augmentation de la production sur le prix n'est pas siginificatif.</b>"))
    
    
  })
  
  #texte réactif en fonction des données
  output$text_prod1 <-  renderUI({
    if(is.na(evo_prod_1bis()$besoin_marche_t[1])==F){
      HTML(paste("De",
                 input$prod_annee1[1],"à",input$prod_annee1[2], 
                 ", les", "<font color='#078080'><b>" , 
                 "producteurs" ,
                 "</b></font>", 
                 "ont déclaré avoir commercialisé",
                 "<font color='#078080'><b>", 
                 format(round(sum(evo_prod_1bis()$poids_enquete_kg,na.rm=T)/1000),big.mark=" "),
                 "tonnes</b></font> de", 
                 input_variete1(),
                 ", tandis que le","<font color='#d9376e'><b>",
                 "marché de gros",
                 "</b></font>", 
                 "en a écoulé",
                 "<font color='#d9376e'><b>", 
                 format(round(sum(evo_prod_1bis()$poids_mg_kg,na.rm=T)/1000),big.mark=" "),
                 "tonnes</b></font>, soit <font color='#d9376e'><b>",
                 percent(sum(evo_prod_1bis()$poids_mg_kg,na.rm=T)/sum(evo_prod_1bis()$poids_enquete_kg,na.rm=T),accuracy=1),
                 "</b></font> du total.<br/>",
                 "Sur la période, la <font color='#078080'><b>production locale</b></font> a répondu aux besoins du marché à",
                 "<font color='#094067'><b>",
                 percent(evo_prod_besoin()$percent[1],accuracy = 1),
                 "</b></font>",
                 "<br/>",
                 "Concernant l'effet de la production sur le prix, pour les <font color='#078080'><b>produits locaux</b></font>,",
                 model()$interpretation[2],
                 ".<br/>"))}else{
                   
                   HTML(paste("De",
                              input$prod_annee1[1],"à",input$prod_annee1[2], 
                              ", les", "<font color='#078080'><b>" , 
                              "producteurs" ,
                              "</b></font>", 
                              "ont déclaré avoir commercialisé",
                              "<font color='#078080'><b>", 
                              format(round(sum(evo_prod_1bis()$poids_enquete_kg,na.rm=T)/1000),big.mark=" "),
                              "tonnes </b></font> de", 
                              input_variete1(),
                              ", tandis que le","<font color='#d9376e'><b>",
                              "marché de gros",
                              "</b></font>", 
                              "en a écoulé",
                              "<font color='#d9376e'><b>", 
                              format(round(sum(evo_prod_1bis()$poids_mg_kg,na.rm=T)/1000),big.mark=" "),
                              "tonnes </b></font>, soit <font color='#d9376e'><b>",
                              percent(sum(evo_prod_1bis()$poids_mg_kg,na.rm=T)/sum(evo_prod_1bis()$poids_enquete_kg,na.rm=T),accuracy=1),
                              "</b></font> du total.<br/>",
                              "Concernant l'effet de la production sur le prix, pour les <font color='#078080'><b>produits locaux</b></font>,",
                              model()$interpretation[2],
                              ".<br/>"))} } )
  
  
  evo_prod_2 <- reactive({
    productions_imports_davar%>%
      filter(filiere1== input$prod_type2 & annee>=input$prod_annee2[1]&annee <=input$prod_annee2[2] )
  })
  
  
  output$evo_prod_plot2<-renderPlot({
    if(input$prod_type2 %in% c("Lait")){
      ggplot( )+
        geom_line(data=evo_prod_2(),
                  aes(x=annee,y=value,color=filiere_animale,linetype=type),size=1.2)+
        
        scale_color_brewer(palette = "Set1")+
        scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"m de litres")})+
        scale_linetype_manual(values = c("Production locale"= 1,"Importations"= 3))+
        theme_ipsum(base_size =14,base_family ="sedgwick" )+
        theme(legend.position = "bottom",legend.box="vertical")+
        guides(color = guide_legend(nrow = 2, byrow = TRUE,linetype=guide_legend(nrow = 2, byrow = TRUE),fill=guide_legend(nrow = 2, byrow = TRUE)))+
        labs(x="",y="",color="",linetype="",fill="",title = "Volumes écoulés",caption = "Sources : DAVAR") }
    else if(input$prod_type2=="Oeufs"){
      ggplot( )+
        geom_line(data=evo_prod_2(),
                  aes(x=annee,y=value,color=filiere_animale,linetype=type),size=1.2)+
        facet_wrap(~filiere_animale,scales = "free_y")+
        scale_color_brewer(palette = "Set1")+
        scale_linetype_manual(values = c("Production locale"= 1,"Importations"= 3))+
        theme_ipsum(base_size =14,base_family ="sedgwick" )+
        theme(legend.position = "bottom",legend.box="vertical")+
        guides(color = guide_legend(nrow = 2, byrow = TRUE,linetype=guide_legend(nrow = 2, byrow = TRUE),fill=guide_legend(nrow = 2, byrow = TRUE)))+
        labs(x="",y="",color="",linetype="",fill="",title = "Volumes écoulés",caption = "Sources : DAVAR")   
    }else{
      
      ggplot( )+
        geom_line(data=evo_prod_2(),
                  aes(x=annee,y=value,color=filiere_animale,linetype=type),size=1.2)+
        
        scale_color_brewer(palette = "Set1")+
        scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"tonnes")})+
        scale_linetype_manual(values = c("Production locale"= 1,"Importations"= 3))+
        theme_ipsum(base_size =14,base_family ="sedgwick" )+
        theme(legend.position = "bottom",legend.box="vertical")+
        guides(color = guide_legend(nrow = 2, byrow = TRUE,linetype=guide_legend(nrow = 2, byrow = TRUE),fill=guide_legend(nrow = 2, byrow = TRUE)))+
        labs(x="",y="",color="",linetype="",fill="",title = "Volumes écoulés",caption = "Sources : DAVAR")
      
    }
    
    
    
    
  })
  
  # Calcul des prix moyen, prix min, prix max,pour les inclure dans le texte réactif
  
  input_variete2 <- reactive({if(input$prix_type2=="Légumes"){
    input$prix_variete2}else{input$prix_variete2b}})
  
  
  geo_prix_categorie <- reactive({
    
    geo_prix_categorie1%>%
      filter(variete==input_variete2()& annee>=input$prix_annee2[1]&annee <=input$prix_annee2[2])%>%
      summarise(categorie=paste(categorie,collapse = "|"))
  })
  
  
  
  geo_prix_commune <- reactive({
    
    
    geo_prix_commune1%>%
      filter(variete==input_variete2()& annee>=input$prix_annee2[1]&annee <=input$prix_annee2[2])%>%
      group_by(commune)%>%
      summarise(import=mean(import,na.rm=T),local=mean(local,na.rm=T))
    
  })
  
  
  prix_max_local <- reactive({
    if(str_detect(geo_prix_categorie()$categorie[1],"local")==T){
      geo_prix_commune()%>%
        ungroup()%>%
        slice_max(local,na_rm = T)  }else{geo_prix_commune()}
    
    
    
  })  
  
  prix_max_import <- reactive({
    if(str_detect(geo_prix_categorie()$categorie[1],"import")==T){
      
      geo_prix_commune()%>%
        ungroup()%>%
        slice_max(import,na_rm = T)}else{geo_prix_commune()} })  
  
  prix_max_ecart <- reactive({
    if(str_detect(geo_prix_categorie()$categorie[1],"import")==T&str_detect(geo_prix_categorie()$categorie[1],"local")==T ){
      
      
      geo_prix_commune()%>%
        ungroup()%>%mutate(ecart=ifelse(local>import,local-import,import-local))%>%
        slice_max(ecart,na_rm = T)
    }else{geo_prix_commune()}
  } )
  
  
  output$text_prix2 <-  renderUI({
    if(str_detect(geo_prix_categorie()$categorie[1],"import")==T&str_detect(geo_prix_categorie()$categorie[1],"local")==T ){
      HTML(paste("De",
                 input$prix_annee2[1],"à",input$prix_annee2[2], 
                 "le prix moyen (au kg) de",input_variete2(),"<font color='#078080'><b>locale</b></font> était de<b>",
                 format(round(mean(geo_prix_commune()$local,na.rm=T)),big.mark=" "),
                 "XPF/kg</b> tandis que le prix moyen à<font color='#ff8e3c'><b> l'import </b></font> était de<b>",  
                 format(round(mean(geo_prix_commune()$import,na.rm=T)),big.mark=" "),
                 "XPF</b>.<br/>La commune pour laquelle le prix du<font color='#078080'><b> produit local</b></font>  était le plus important était<font color='#078080'><b>", 
                 prix_max_local()$commune[1],"</b></font> avec un prix moyen de<b>",
                 format(round(prix_max_local()$local[1]),big.mark=" "),              
                 "XPF/kg</b>. Celle pour laquellel le prix du produit <font color='#ff8e3c'><b>importé</b></font> était le plus important était<font color='#ff8e3c'><b>", 
                 prix_max_import()$commune[1],"</b></font> avec un prix moyen de<b>",
                 format(round(prix_max_import()$import[1]),big.mark=" "), 
                 "XPF/kg</b>.<br/>La commune pour laquelle l'écart entre le local et l'import était le plus important était<b>", 
                 prix_max_ecart()$commune[1],"</b>avec un écart moyen de<b>",
                 format(round(prix_max_ecart()$ecart[1]),big.mark=" "),"XPF/kg."))}else if(str_detect(geo_prix_categorie()$categorie[1],"import")==T&str_detect(geo_prix_categorie()$categorie[1],"local")==F ){
                   
                   HTML(paste("De",
                              input$prix_annee2[1],"à",input$prix_annee2[2], 
                              "le prix moyen (au kg) de",input_variete2(),"à<font color='#ff8e3c'><b> l'import </b></font> était de<b>",  
                              format(round(mean(geo_prix_commune()$import,na.rm=T)),big.mark=" "),
                              "XPF/kg</b>.<br/>",
                              "La commune pour laquelle le prix du produit importé était le plus important était", 
                              prix_max_import()$commune[1],"avec un prix moyen de<b>",
                              format(round(prix_max_import()$import[1]),big.mark=" "), 
                              "XPF/kg</b>."))}else if(str_detect(geo_prix_categorie()$categorie[1],"import")==F&str_detect(geo_prix_categorie()$categorie[1],"local")==T){
                                
                                HTML(paste("De",
                                           input$prix_annee2[1],"à",input$prix_annee2[2], 
                                           "le prix moyen (au kg) de",input_variete2(),"locale était de<b>",
                                           format(round(mean(geo_prix_commune()$local,na.rm=T)),big.mark=" "),
                                           "XPF/kg</b>.<br/>La commune pour laquelle le prix du<font color='#078080'><b> produit local</b></font>  était le plus important était", 
                                           prix_max_local()$commune[1],"avec un prix moyen de<b>",
                                           format(round(prix_max_local()$local[1]),big.mark=" "),              
                                           "XPF/kg.")) 
                                
                              }
    
    
    
    
  } )
  
  
  
  output$prix_province_plot <- renderPlot({
    if(str_detect(geo_prix_categorie()$categorie[1],"import")==T&str_detect(geo_prix_categorie()$categorie[1],"local")==T ){
      
      
      ggplot(geo_prix_commune())+
        geom_segment( aes(x=fct_reorder(commune,local) , xend=fct_reorder(commune,local) , y=import, yend=local), color="grey") +
        geom_point( aes(x=fct_reorder(commune,local) , y=import,color="Import"), size=3 ) +
        geom_point( aes(x=fct_reorder(commune,local) , y=local,color="Local"),  size=3 ) +
        coord_flip()+
        scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"XPF")})+
        
        scale_colour_manual(values=c("Import"="#ff8e3c", "Local"="#078080"))+
        theme_ipsum(base_size =15,base_family ="sedgwick" )+
        labs(x="",y="",color="",title = "Prix moyen (au kg) par commune",caption = "Source : DAE")}else if(str_detect(geo_prix_categorie()$categorie[1],"import")==T&str_detect(geo_prix_categorie()$categorie[1],"local")==F){
          
          ggplot(geo_prix_commune())+
            geom_segment( aes(x=fct_reorder(commune,import) , xend=fct_reorder(commune,import) , y=import,yend=0), color="grey") +
            geom_point( aes(x=fct_reorder(commune,import) , y=import,color="Import"), size=3 ) +
            coord_flip()+
            scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"XPF")})+
            
            scale_colour_manual(values=c("Import"="#ff8e3c", "Local"="#078080"))+
            theme_ipsum(base_size =15,base_family ="sedgwick" )+
            labs(x="",y="",color="",title = "Prix moyen (au kg) par commune",caption = "Source : DAE")}else if(str_detect(geo_prix_categorie()$categorie[1],"import")==F&str_detect(geo_prix_categorie()$categorie[1],"local")==T){
              
              ggplot(geo_prix_commune())+
                geom_segment( aes(x=fct_reorder(commune,local) , xend=fct_reorder(commune,local) , y=local,yend=0), color="grey") +
                geom_point( aes(x=fct_reorder(commune,local) , y=local,color="Local"), size=3 ) +
                coord_flip()+
                scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"XPF")})+
                
                scale_colour_manual(values=c("Import"="#ff8e3c", "Local"="#078080"))+
                theme_ipsum(base_size =15,base_family ="sedgwick" )+
                labs(x="",y="",color="",title = "Prix moyen (au kg) par commune",caption = "Source : DAE")}})  
  
  
  geo_prix_nc <- reactive({
    nc_communes%>%inner_join(geo_prix_commune()%>%
                               pivot_longer(local:import,names_to = "categorie",values_to = "prix_moyen")%>%
                               group_by(commune)%>%
                               summarise(prix_moyen=mean(prix_moyen,na.rm=T))%>%
                               mutate(text=paste0(commune,":\n",round(prix_moyen)," XPF")))
  })
  
  #Carte réactive des prix par commune
  
  output$nc_prix2 <- renderLeaflet({
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = geo_prix_nc1$prix_moyen,reverse = T
    )
    leaflet(data =geo_prix_nc1) %>%  addProviderTiles(provider=providers$OpenStreetMap.HOT)%>%
      addPolylines(data = nc_communes,weight = 1,color = "black")%>%
      addPolygons(data = geo_prix_nc1,stroke = FALSE, layerId = ~commune,
                  fillOpacity = 0.5, smoothFactor = 0.5, color = ~pal(prix_moyen),label =~paste(text)  , 
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "cursive", padding = "3px 8px","font-family" = "Sedgwick Ave"), 
                    textsize = "13px", 
                    direction = "auto"))%>%
      addLegend(data =geo_prix_nc1,"topright", pal = pal, values = ~prix_moyen,labFormat = labelFormat( suffix = 'XPF',big.mark = " "),title = paste("Prix moyen de","Pomme de terre","\npar commune sur la période"), layerId = ~commune, )
  }) 
  
  observe({
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = geo_prix_nc()$prix_moyen,reverse = T
    )
    
    
    leafletProxy("nc_prix2")%>%
      clearShapes()%>%
      clearControls()%>%
      addPolylines(data = nc_communes,weight = 1,color = "black")%>%
      addPolygons(data = geo_prix_nc(),stroke = FALSE, layerId = ~commune,
                  fillOpacity = 0.5, smoothFactor = 0.5, color = ~pal(prix_moyen),label =~paste(text)  , 
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "cursive", padding = "3px 8px","font-family" = "Sedgwick Ave"), 
                    textsize = "13px", 
                    direction = "auto"))%>%
      addLegend(data = geo_prix_nc(),"topright", pal = pal, values = ~prix_moyen,labFormat = labelFormat( suffix = 'XPF',big.mark = " "),title = paste("Prix moyen de",input_variete2(),"\npar commune sur la période") , layerId = ~commune)
    
    
  })
  
  #sortie graphique sur le importations dans le pacifique
  imports_total <- reactive({ if(input$par_hab==T){ imports_fruits_legumes_V2%>%
      filter(type==input$pacific_type3 & time_period>=input$pacific_annee3[1]&time_period<=input$pacific_annee3[2])%>%
      group_by(Name)%>%
      summarise(total_hab_kg=sum(import_hab_kg,na.rm = T))%>%
      ungroup()%>%
      arrange(desc(total_hab_kg))%>%
      mutate(classement=row_number(),
             text=case_when(classement==1~paste(Name,"est le plus gros importateur\navec",round(total_hab_kg),"kgs/hab au total"),
                            classement==max(classement,na.rm = T) ~paste(Name,"est le plus petit importateur\navec",round(total_hab_kg,2),"kgs/hab au total"),
                            Name=="Nouvelle-Calédonie" & classement!=1 & classement!=min(classement)~paste0("La Nouvelle-Calédonie est ",classement,"eme\navec ",round(total_hab_kg)," kgs/hab au total"),
                            TRUE~NA))}else{
                              
                              imports_fruits_legumes_V2%>%
                                filter(type==input$pacific_type3 & time_period>=input$pacific_annee3[1]&time_period<=input$pacific_annee3[2])%>%
                                group_by(Name)%>%
                                summarise(imports_t=sum(imports_t,na.rm = T))%>%
                                ungroup()%>%
                                arrange(desc(imports_t))%>%
                                mutate(classement=row_number(),
                                       text=case_when(classement==1~paste(Name,"est le plus gros importateur \navec",format(round(imports_t),big.mark=" "),"tonnes au total"),
                                                      classement==max(classement,na.rm = T) ~paste(Name,"est le plus petit importateur \navec",format(round(imports_t,2),big.mark=" "),"tonnes au total"),
                                                      Name=="Nouvelle-Calédonie" & classement!=1 & classement!=min(classement)~paste0("La Nouvelle-Calédonie est ",classement,"eme\navec ",format(round(imports_t),big.mark=" ")," tonnes au total"),
                                                      TRUE~NA))} })    
  
  output$imports_total_plot <- renderPlot({ 
    
    if(input$par_hab==T){ 
      ggplot()+
        geom_segment(data=imports_total(), aes(x=fct_reorder(Name,total_hab_kg) , xend=fct_reorder(Name,total_hab_kg) , y=total_hab_kg,yend=0), color="grey") +
        geom_point(data=imports_total(), aes(x=fct_reorder(Name,total_hab_kg) , y=total_hab_kg), size=3,color="#ff8e3c" ) +
        coord_flip()+
        scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"kgs")},limits = c(0,max(imports_total()$total_hab_kg)+0.40*max(imports_total()$total_hab_kg)))+
        geom_text_repel(data=imports_total()%>%filter(is.na(text)==F), 
                        box.padding = 0.9, alpha=0.5,family="sedgwick",
                        nudge_x = .15,
                        nudge_y = 0.2*max(imports_total()$total_hab_kg),
                        segment.curvature = +0.3,min.segment.length = 0,
                        arrow = arrow(length = unit(0.015, "npc")), 
                        aes(x=Name,label=text,y=total_hab_kg),bg.color="white")+
        theme_ipsum(base_size =15,base_family ="sedgwick" )+
        labs(x="",y="",color="",title = "Importations totales (par habitant)",caption = "Source : Pacific Food Trade Database")}else{
          ggplot()+
            geom_segment(data=imports_total(), aes(x=fct_reorder(Name,imports_t) , xend=fct_reorder(Name,imports_t) , y=imports_t,yend=0), color="grey") +
            geom_point(data=imports_total(), aes(x=fct_reorder(Name,imports_t) , y=imports_t), size=3,color="#ff8e3c" ) +
            coord_flip()+
            scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"t")},limits = c(0,max(imports_total()$imports_t)+0.6*max(imports_total()$imports_t)))+
            geom_text_repel(data=imports_total()%>%filter(is.na(text)==F), 
                            box.padding = 0.9, alpha=0.5,family="sedgwick",
                            nudge_x = .15,
                            nudge_y = 0.2*max(imports_total()$imports_t),
                            segment.curvature = +0.3,min.segment.length = 0,
                            arrow = arrow(length = unit(0.015, "npc")), 
                            aes(x=Name,label=text,y=imports_t),bg.color="white")+
            theme_ipsum(base_size =15,base_family ="sedgwick" )+
            labs(x="",y="",color="",title = "Importations totales",caption = "Source : Pacific Food Trade Database")} })
  
  
  
  
  imports_moyenne <- reactive({ 
    if(input$par_hab==T){
      imports_fruits_legumes_V2%>%
        filter(type==input$pacific_type3 & time_period>=input$pacific_annee3[1]&time_period<=input$pacific_annee3[2])%>%
        group_by(Name)%>%
        summarise(moyenne_hab_kg=mean(import_hab_kg,na.rm = T))%>%
        ungroup()%>%
        arrange(desc(moyenne_hab_kg))%>%
        mutate(classement=row_number(),
               text=case_when(classement==1~paste(Name,"est le plus gros importateur\navec",round(moyenne_hab_kg),"kgs/hab/an en moyenne"),
                              classement==max(classement,na.rm = T) ~paste(Name,"est le plus petit importateur\navec",round(moyenne_hab_kg,2),"kgs/hab/an en moyenne"),
                              Name=="Nouvelle-Calédonie" & classement!=1 & classement!=min(classement)~paste0("La Nouvelle-Calédonie est ",classement,"eme\navec ",round(moyenne_hab_kg)," kgs/hab/an en moyenne"),
                              TRUE~NA))}else{
                                
                                imports_fruits_legumes_V2%>%
                                  filter(type==input$pacific_type3 & time_period>=input$pacific_annee3[1]&time_period<=input$pacific_annee3[2])%>%
                                  group_by(Name)%>%
                                  summarise(moyenne_t=mean(imports_t,na.rm = T))%>%
                                  ungroup()%>%
                                  arrange(desc(moyenne_t))%>%
                                  mutate(classement=row_number(),
                                         text=case_when(classement==1~paste(Name,"est le plus gros importateur\navec",format(round(moyenne_t),big.mark=" "),"tonnes/an en moyenne"),
                                                        classement==max(classement,na.rm = T) ~paste(Name,"est le plus petit importateur\navec",format(round(moyenne_t,2),big.mark=" "),"tonnes/an en moyenne"),
                                                        Name=="Nouvelle-Calédonie" & classement!=1 & classement!=min(classement)~paste0("La Nouvelle-Calédonie est ",classement,"eme\navec ",format(round(moyenne_t),big.mark=" ") ," tonnes/an en moyenne"),
                                                        TRUE~NA))            
                                
                              }
    
    
  })    
  
  output$imports_moyenne_plot <- renderPlot({ 
    
    if(input$par_hab==T){
      ggplot()+
        geom_segment(data=imports_moyenne(), aes(x=fct_reorder(Name,moyenne_hab_kg) , xend=fct_reorder(Name,moyenne_hab_kg) , y=moyenne_hab_kg,yend=0), color="grey") +
        geom_point(data=imports_moyenne(), aes(x=fct_reorder(Name,moyenne_hab_kg) , y=moyenne_hab_kg), size=3,color="#ff8e3c" ) +
        coord_flip()+
        scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"kgs")},limits = c(0,max(imports_moyenne()$moyenne_hab_kg)+0.6*max(imports_moyenne()$moyenne_hab_kg)))+
        geom_text_repel(data=imports_moyenne()%>%filter(is.na(text)==F), 
                        box.padding = 0.9, alpha=0.5,
                        nudge_x = .15,
                        nudge_y = 0.2*max(imports_moyenne()$moyenne_hab_kg),
                        segment.curvature = +0.3,min.segment.length = 0,family="sedgwick",
                        arrow = arrow(length = unit(0.015, "npc")), 
                        aes(x=Name,label=text,y=moyenne_hab_kg),bg.color="white")+
        theme_ipsum(base_size =15,base_family ="sedgwick" )+
        labs(x="",y="",color="",title = "Importations moyennes (par an/habitant)",caption = "Source : Pacific Food Trade Database")}else{
          ggplot()+
            geom_segment(data=imports_moyenne(), aes(x=fct_reorder(Name,moyenne_t) , xend=fct_reorder(Name,moyenne_t) , y=moyenne_t,yend=0), color="grey") +
            geom_point(data=imports_moyenne(), aes(x=fct_reorder(Name,moyenne_t) , y=moyenne_t), size=3,color="#ff8e3c" ) +
            coord_flip()+
            scale_y_continuous(labels = function(x){paste(format(x,big.mark=" "),"t")},limits = c(0,max(imports_moyenne()$moyenne_t)+0.6*max(imports_moyenne()$moyenne_t)))+
            geom_text_repel(data=imports_moyenne()%>%filter(is.na(text)==F), 
                            box.padding = 0.9, alpha=0.5,family="sedgwick",
                            nudge_x = .15,
                            nudge_y = 0.2*max(imports_moyenne()$moyenne_t),
                            segment.curvature = +0.3,min.segment.length = 0,
                            arrow = arrow(length = unit(0.015, "npc")), 
                            aes(x=Name,label=text,y=moyenne_t),bg.color="white")+
            theme_ipsum(base_size =15,base_family ="sedgwick" )+
            labs(x="",y="",color="",title = "Importations moyennes (par an)",caption = "Source : Pacific Food Trade Database")
          
          
        }
    
    
  })  
  
  
  
}



shinyApp(ui = ui, server = server)

