library(lubridate)
data <- import_dataset("/home/ngirard/Webinaire_MLOPS/data/uk-retailer-ii.xlsx")


dim(data)

create_var_reponse <- function(data, start_rep="2011-10-01", end_rep="2011-10-31"){
  customer_id_achat <- data[InvoiceDate >= start_rep & InvoiceDate <= end_rep, unique(Customer.ID)]
  df_var_reponse <- data[, .(Customer.ID = unique(Customer.ID), VAR_REP = 0)]
  df_var_reponse[Customer.ID %in% customer_id_achat, VAR_REP := 1]
  return(df_var_reponse)
}

agg <- create_var_reponse(data, "2011-10-01", "2011-10-31")


# Exemple : Calcul le prix du panier moyen sur les 3 derniers mois à partir de end_rep
create_agg_prix <- function(data, end_rep="2011-10-01", window_months = 3){
  end_agg <- as.Date(end_rep)
  lubridate::day(end_agg) <- 1
  start_agg <- end_rep %m-% months(window_months)
  sub_data_agg <- data[InvoiceDate >= start_agg & InvoiceDate < end_agg,]
  
  basket_customer <- sub_data_agg[Quantity > 0, .(BASKET_PRICE = sum(Quantity*Price)), .(Customer.ID, Invoice)]
  agg_panier <- basket_customer[, .(BASKET_PRICE_MEAN = mean(BASKET_PRICE), 
                                    BASKET_PRICE_MIN = min(BASKET_PRICE),
                                    BASKET_PRICE_MAX = max(BASKET_PRICE), 
                                    NB_BASKETS = .N), # Correspond à la fréquence d'achats
                                .(Customer.ID)]
  agg_panier_full <- merge(agg_panier, data[, .(Customer.ID = unique(Customer.ID))], 
        by = "Customer.ID", all.y=TRUE)
  agg_panier_full[is.na(agg_panier_full)] <- 0
  
  
  agg_qty <- sub_data_agg[Quantity > 0, .(QTY_TOTAL_PER_CMD = sum(Quantity), 
                                          NB_PROD_PER_CMD = .N), .(Customer.ID, Invoice)]
  agg_qty <- agg_qty[, .(QTY_TOTAL_PER_CMD_MOY = mean(QTY_TOTAL_PER_CMD), 
              QTY_TOTAL_PER_CMD_MIN = min(QTY_TOTAL_PER_CMD),
              QTY_TOTAL_PER_CMD_MAX = max(QTY_TOTAL_PER_CMD), 
              NB_PROD_PER_CMD_MOY = mean(NB_PROD_PER_CMD),
              NB_PROD_PER_CMD_MIN = min(NB_PROD_PER_CMD),
              NB_PROD_PER_CMD_MAX = max(NB_PROD_PER_CMD)), .(Customer.ID)]
  agg_qty_full <- merge(agg_qty, data[, .(Customer.ID = unique(Customer.ID))], 
                           by = "Customer.ID", all.y=TRUE)
  agg_qty_full[is.na(agg_qty_full)] <- 0
  
  all_agg <- merge(agg_panier_full, agg_qty_full, by = "Customer.ID")
  
  return(all_agg)
}



