pacman::p_load(here,tidyverse,readr,janitor,readxl)
prod_agri_davar <- read_xlsx(here("prod_agri_davar.xlsx"))%>%
  pivot_longer(2:35,names_to = "annee")%>%
  clean_names()%>%
  mutate(filiere1=ifelse(str_detect(filiere_animale,"\\-")==T,NA,filiere_animale))%>%
  fill(filiere1)%>%
  filter(is.na(value)==F)%>%
  mutate(value=ifelse(value %in% c("nd","-"),NA,as.numeric(value)),
         filiere_animale=str_remove_all(filiere_animale,"\\-\\s"),
         filiere_animale=str_to_sentence(filiere_animale),
         filiere1=ifelse(str_detect(filiere_animale,"Lait|lait")==T,"Lait",filiere1),
         filiere1=ifelse(str_detect(filiere_animale,"Oeufs|Œufs")==T,"Oeufs",filiere1),
         
         
         )%>%
  filter(filiere_animale!="Fruits et légumes")


importations_davar <- read_xlsx(here("importations_davar.xlsx"))%>%
  pivot_longer(2:35,names_to = "annee")%>%
  clean_names()%>%
  mutate(filiere1=ifelse(str_detect(filiere_animale,"\\-")==T,NA,filiere_animale))%>%
  fill(filiere1)%>%
  filter(is.na(value)==F)%>%
  mutate(value=ifelse(value %in% c("nd","-"),NA,as.numeric(value)),
         filiere_animale=str_remove_all(filiere_animale,"\\-\\s"),
         filiere_animale=str_to_sentence(filiere_animale),
         filiere_animale=recode(filiere_animale,"Porcins (viande)"="Viande"),
         filiere1=ifelse(str_detect(filiere_animale,"Lait|lait")==T,"Lait",filiere1),
         filiere1=ifelse(str_detect(filiere_animale,"Oeufs|Œufs")==T,"Oeufs",filiere1),
         filiere1=recode(filiere1,"Fruits et légumes, dont :"="Fruits et légumes","Porcins (viande)"="Porcins")
         
         )%>%
  filter(filiere_animale!="Fruits et légumes, dont :")

productions_imports_davar <- prod_agri_davar%>%
  mutate( type="Production locale" )%>%
  bind_rows(importations_davar%>%
              mutate( type="Importations" ))


write_csv(productions_imports_davar,"productions_imports_davar.csv")





prod_agri_tribu_region%>%
  select(`N_group_prod veget`,`N_productions vegetales`)%>%
  unique()%>%
  write_csv(here("groupe_prod_veg.csv"))



for (i in 2012:2023) {
  
  url=paste0("https://data.gouv.nc/api/explore/v2.1/catalog/datasets/prix-des-produits-alimentaires-en-",i  ,"-pacific-dataviz-challenge/exports/csv?lang=fr&refine=famille_produit%3A%22viande%22&refine=famille_produit%3A%22lait%22&refine=famille_produit%3A%22volaille%2C%20lapin%22&facet=facet(name%3D%22famille_produit%22%2C%20disjunctive%3Dtrue)&timezone=Pacific%2FNoumea&use_labels=true&delimiter=%2C")
  
  download.file(url = url,destfile = paste0(here("viande"),"\\/","prix_viande_",i,".csv"))
  
}


import_recode <- function(x){
  
x <-   read_csv(x)%>%
    janitor::clean_names()%>%
    mutate(prix_kg=prix_releve/conditionnement,categorie=str_to_lower(categorie),
           annee=year(date_releve)
           )%>%
    group_by(annee,sous_famille,categorie)%>%
    summarise(prix_moyen=mean(prix_kg,na.rm=T))%>%
    mutate(sous_famille=str_remove(sous_famille,"viande : "),
           sous_famille=str_to_sentence(sous_famille),
           type=case_when(sous_famille %in% c("Bœuf","Bœuf haché","Veau")~"Bovins",
                          sous_famille %in% c("Canard","Poulet, dinde","Volaille : autres","Volaille panée","Volaille")~"Aviculture",
                          sous_famille %in% c("Porc")~"Porcins",
                          sous_famille %in% c("Agneau")~"Petits ruminants",
                          str_detect(sous_famille,"Lait")==T~"Lait",
                          TRUE~NA))}

import_recode2 <- function(x){
  
  x <-   read_csv(x)%>%
    janitor::clean_names()%>%
    mutate(prix_kg=as.numeric(prix_releve)/as.numeric(conditionnement),categorie=str_to_lower(categorie),
           annee=year(date_releve)
    )%>%
    group_by(annee,sous_famille,categorie)%>%
    summarise(prix_moyen=mean(prix_kg,na.rm=T))%>%
    mutate(sous_famille=str_remove(sous_famille,"viande : "),
           sous_famille=str_to_sentence(sous_famille),
           type=case_when(sous_famille %in% c("Bœuf","Bœuf haché","Veau")~"Bovins",
                          sous_famille %in% c("Canard","Poulet, dinde","Volaille : autres","Volaille panée","Volaille")~"Aviculture",
                          sous_famille %in% c("Porc")~"Porcins",
                          sous_famille %in% c("Agneau")~"Petits ruminants",
                          str_detect(sous_famille,"Lait")==T~"Lait",
                          TRUE~NA))}



import_recode_xl <- function(x){
  
  x <-   readxl::read_xlsx(x)%>%
    janitor::clean_names()%>%
    mutate(prix_kg=as.numeric(prix_releve)/as.numeric(conditionnement),categorie=str_to_lower(categorie),
           annee=year(date_releve)
    )%>%
    group_by(annee,sous_famille,categorie)%>%
    summarise(prix_moyen=mean(prix_kg,na.rm=T))%>%
    mutate(sous_famille=str_remove(sous_famille,"viande : "),
           sous_famille=str_to_sentence(sous_famille),
           type=case_when(sous_famille %in% c("Bœuf","Bœuf haché","Veau")~"Bovins",
                          sous_famille %in% c("Canard","Poulet, dinde","Volaille : autres","Volaille panée","Volaille")~"Aviculture",
                          sous_famille %in% c("Porc")~"Porcins",
                          sous_famille %in% c("Agneau")~"Petits ruminants",
                          str_detect(sous_famille,"Lait")==T~"Lait",
                          TRUE~NA))}
prix_viande_2023 <- import_recode("viande/prix_viande_2023.csv")
prix_viande_2022 <- import_recode("viande/prix_viande_2022.csv")
prix_viande_2021 <- import_recode("viande/prix_viande_2021.csv")
prix_viande_2019 <- import_recode("viande/prix_viande_2019.csv")
prix_viande_2018 <- import_recode("viande/prix_viande_2018.csv")
prix_viande_2017 <- import_recode_xl("viande/prix_viande_2017.xlsx")
prix_viande_2016 <- import_recode("viande/prix_viande_2016.csv")
prix_viande_2015 <- import_recode("viande/prix_viande_2015.csv")
prix_viande_2014 <- import_recode("viande/prix_viande_2014.csv")
prix_viande_2013 <- import_recode("viande/prix_viande_2013.csv")


prix_viande <- prix_viande_2023%>%
  bind_rows(prix_viande_2022)%>%
  bind_rows(prix_viande_2021)%>%
  bind_rows(prix_viande_2019)%>%
  bind_rows(prix_viande_2018)%>%
  bind_rows(prix_viande_2017)%>%
  bind_rows(prix_viande_2016)%>%
  bind_rows(prix_viande_2015)%>%
  bind_rows(prix_viande_2014)%>%
  bind_rows(prix_viande_2013)

write_csv(prix_viande,"prix_viande.csv")
