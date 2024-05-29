# Importation des bibliotheques necessaires
library(haven)
library(readxl)
library(dplyr)

#1.  Fonction d'importation et de renommage
base_traitee <- function(produit_path, conversion_path) {
  produit_dta <- read_dta(produit_path)
  conversion_data <- read_excel(conversion_path)
  names(produit_dta)[names(produit_dta) == paste0(produit,"__id")] <- "produitID"
  names(produit_dta)[names(produit_dta) == paste0("s07Bq03b_", produit)] <- "uniteID"
  names(produit_dta)[names(produit_dta) == paste0("s07Bq03c_", produit)] <- "tailleID"
  list(produit = produit_dta, conversion_data = conversion_data)

}




#2.  Fonction pour merger produit_merge et ehcvmmod
merge_ehcvm <- function(produit_merge, ehcvm_path) {
  ehcvmmod <- read_dta(ehcvm_path)
  fusion <- merge(produit_merge, ehcvmmod, by = "interview__key", all.x = TRUE)
  return(fusion)
}


#3.  Fonction achats

achat <- function(produit_merge, produit) {
  achats_cols <- c("produitID", paste0("s07Bq07a_", produit), paste0("s07Bq07b_", produit), paste0("s07Bq07c_", produit), paste0("s07Bq08_", produit),"poids")
  prodachat <- produit_merge[achats_cols]
  colnames(prodachat)[2:5] <- c("qte", "uniteID", "tailleID", "Valeur")
  prodachat <- prodachat %>%
    group_by(produitID, qte, uniteID, tailleID) %>%
    mutate(Valeur = mean(Valeur)) %>%
    ungroup() %>%
    unique()
  names(produit_merge)[names(produit_merge) == paste0("s07Bq03a_", produit)] <- "qte"
  produit_merge <- merge(produit_merge, prodachat, by = c("produitID", "uniteID", "tailleID", "qte"), all.x = TRUE)
  taux_unmatching <- sum(is.na(produit_merge$Valeur)) / length(produit_merge$Valeur) * 100
  print(taux_unmatching)

  return(viandes_achat = prodachat)
}

