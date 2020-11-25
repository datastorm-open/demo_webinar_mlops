require(data.table)
require(openxlsx)

# UNCHOSEN DATASETS 
# 
# # https://www.kaggle.com/carrie1/ecommerce-data/home
# # https://archive.ics.uci.edu/ml/datasets/Online+Retail 
# # used for this paper : https://link.springer.com/article/10.1057/dbm.2012.17
# uk_retailer = fread('/home/mmasson/data/mlops-wbr/uk-retailer.csv')
# 
# # https://www.kaggle.com/olistbr/brazilian-ecommerce/home
# bzl_orders = fread('/home/mmasson/data/mlops-wbr/brazilian-e-commerce/olist_orders_dataset.csv')
# bzl_oitems = fread('/home/mmasson/data/mlops-wbr/brazilian-e-commerce/olist_order_items_dataset.csv')
# bzl_ecom = merge(bzl_orders, bzl_oitems, by = 'order_id')
# bzl_products = fread('/home/mmasson/data/mlops-wbr/brazilian-e-commerce/olist_products_dataset.csv')
# bzl_ecom = merge(bzl_ecom, bzl_products, by = 'product_id')
# bzl_customers = fread('/home/mmasson/data/mlops-wbr/brazilian-e-commerce/olist_customers_dataset.csv')
# bzl_ecom = merge(bzl_ecom, bzl_customers, by = 'customer_id')
# 
# # https://www.kaggle.com/AppleEcomerceInfo/ecommerce-information
# alibaba_order = fread('/home/mmasson/data/mlops-wbr/alibaba-e-commerce/order.txt')
# alibaba_join_prod = fread('/home/mmasson/data/mlops-wbr/alibaba-e-commerce/orders_has_products.txt')
# alibaba_prod = fread('/home/mmasson/data/mlops-wbr/alibaba-e-commerce/products.txt')
# alibaba_join_user = fread('/home/mmasson/data/mlops-wbr/alibaba-e-commerce/orders_placed_user.txt')
# alibaba_user = fread('/home/mmasson/data/mlops-wbr/alibaba-e-commerce/user.txt')
# alibaba_ecom = merge(alibaba_order, alibaba_join_prod, by = 'order_id')
# alibaba_ecom = merge(alibaba_ecom, alibaba_prod, by = 'product_id')
# alibaba_ecom = merge(alibaba_ecom, alibaba_join_user, by = 'order_id')
# alibaba_ecom = merge(alibaba_ecom, alibaba_user, by = 'user_id')
# 
# # https://www.kaggle.com/retailrocket/ecommerce-dataset/home
# rocket_cat = fread('/home/mmasson/data/mlops-wbr/retailrocket/category_tree.csv')
# rocket_event = fread('/home/mmasson/data/mlops-wbr/retailrocket/events.csv')
# rocket_props1 = fread('/home/mmasson/data/mlops-wbr/retailrocket/item_properties_part1.csv')
# rocket_props2 = fread('/home/mmasson/data/mlops-wbr/retailrocket/item_properties_part2.csv')
# 
# # https://www.kaggle.com/aungpyaeap/supermarket-sales
# supermarket = fread('/home/mmasson/data/mlops-wbr/supermarket/supermarket_sales.csv')


#' Import dataset
#'
#' @param path : \code{character}. Path to xlsx file
#' 
#' @return a data.table object
#'
#' @import data.table
#' @import openxlsx
#' @export
#'
#'
#' @examples 
#' \dontrun{
#' dt = import_dataset()
#'
#'
import_dataset <- function(path='/home/mmasson/data/mlops-wbr/uk-retailer-ii.xlsx'){
  # https://archive.ics.uci.edu/ml/datasets/Online+Retail+II
  # https://www.kaggle.com/mathchi/online-retail-ii-data-set-from-ml-repository
  uk_retailer_2_p1 = read.xlsx(path, sheet = 1)
  uk_retailer_2_p2 = read.xlsx(path, sheet = 2)
  uk_retailer_2 = as.data.table(rbind(uk_retailer_2_p1, uk_retailer_2_p2))
  uk_retailer_2 <- uk_retailer_2[!is.na(Customer.ID)]
  uk_retailer_2$InvoiceDate = as.Date(uk_retailer_2$InvoiceDate, origin = "1900-01-01")
  uk_retailer_2$Invoice = as.character(uk_retailer_2$Invoice)
  return(uk_retailer_2)
}

#' Create response variable : 0 if customer did not buy anything in targetted month, 1 if the it did.
#'
#' @param start_rep : \code{character}. Start of the targeted period. Character to be parsed as date (format YYYY-MM-DD)
#' @param end_rep : \code{character}. End of the targeted period. Character to be parsed as date (format YYYY-MM-DD)
#' 
#' @return a data.table object
#'
#' @import data.table
#' @export
#'
#'
#' @examples 
#' \dontrun{
#' TODO
#'
#'
create_var_reponse <- function(data, start_rep="2011-10-01", end_rep="2011-10-31"){
  customer_id_achat <- data[InvoiceDate >= start_rep & InvoiceDate <= end_rep, unique(Customer.ID)]
  df_var_reponse <- data[, .(Customer.ID = unique(Customer.ID), VAR_REP = 0)]
  df_var_reponse[Customer.ID %in% customer_id_achat, VAR_REP := 1]
  df_var_reponse[, MONTH := month(start_rep)]
  df_var_reponse[, YEAR := year(start_rep)]
  return(df_var_reponse)
}

#' Subset dataset from a specified date of end for as long as specified (in months).
#'
#' @param data : \code{data.talbe}. Complete dataset to extract period from.
#' @param to : \code{character}. Start of the targeted period. Character to be parsed as date (format YYYY-MM-DD)
#' @param window_months : \code{integer}. Size of period window.
#' 
#' @return a data.table object
#'
#' @import data.table
#' @import lubridate
#' @export
#'
#'
#' @examples 
#' \dontrun{
#' TODO
#'
#'
create_subset_data <- function(data, to="2011-10-01", window_months = 3){
  end_agg <- as.Date(to)
  lubridate::day(end_agg) <- 1
  start_agg <- end_agg %m-% months(window_months)
  sub_data_agg <- data[InvoiceDate >= start_agg & InvoiceDate < end_agg,]
  return(sub_data_agg)
}



#' First part of features computing. Calcul le prix du panier moyen sur les 3 derniers mois à partir de end_rep (TO TRANSLATE)
#'
#' @param sub_data_agg : \code{data.table}. A subset of the complete dataset(Use create_subset_data)
#' @param all_customers : \code{data.table}. List of all the considered customers.
#' 
#' @return a data.table object
#'
#' @import data.table
#' @export
#'
#'
#' @examples 
#' \dontrun{
#' TODO
#'
#'
create_agg_prix_qty <- function(sub_data_agg, all_customers){

  basket_customer <- sub_data_agg[Quantity > 0, .(BASKET_PRICE = sum(Quantity*Price)), .(Customer.ID, Invoice)]
  agg_panier <- basket_customer[, .(BASKET_PRICE_MEAN = mean(BASKET_PRICE), 
                                    BASKET_PRICE_MIN = min(BASKET_PRICE),
                                    BASKET_PRICE_MAX = max(BASKET_PRICE), 
                                    NB_BASKETS = .N), # Correspond à la fréquence d'achats
                                .(Customer.ID)]
  agg_panier_full <- merge(agg_panier, all_customers, 
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
  agg_qty_full <- merge(agg_qty, all_customers, 
                        by = "Customer.ID", all.y=TRUE)
  agg_qty_full[is.na(agg_qty_full)] <- 0
  
  all_agg <- merge(agg_panier_full, agg_qty_full, by = "Customer.ID")
  
  return(all_agg)
}


#' Second part of features computing.
#'
#' @param dt : \code{data.table}. A data.table obtained from a groupby on customers
#' 
#' @return a data.table object
#'
#' @import data.table
#' @export
#'
#'
#' @examples 
#' \dontrun{
#' TODO
#'
create_agg_freq_cncl <- function(dt){
   
  # subset on product actually bought and products cancelled
  is_C = function (x) startsWith(x,"C")
  bought = dt[Quantity>0]
  cancelled = dt[Quantity<=0]

  bght_prices_qtty=bought$Price*bought$Quantity
  cncl_prices_qtty=cancelled$Price*cancelled$Quantity
  
  if(nrow(bought)>0){
    out = list("FREQ_ACHAT" = 1/length(unique(as.character(bought$InvoiceDate))),
               "NB_SKU" = length(unique(bought$StockCode)),
               "ITEM_PRICE_MAX" = max(bght_prices_qtty),
               "EXPENSES" = sum(bght_prices_qtty))
    
    ## Achats annulés
    out$NB_CANCELLED = nrow(cancelled)
    out$EXPENSES_CANCELLED = -sum(cncl_prices_qtty)
    out$PCT_EXP_CANCELLED = out$EXPENSES_CANCELLED / out$EXPENSES
    
    ## Achats fréquents
    NB_FREQ = bought[,.(NB_CMD=length(unique(as.character(InvoiceDate)))),by=StockCode]
    bought = merge(bought, NB_FREQ, by="StockCode")
    out$NB_CMD_MOST_FREQ = as.double(max(NB_FREQ$NB_CMD))
    out$NB_SKU_FREQ = nrow(NB_FREQ[NB_CMD>1])
    out$PCT_SKU_FREQ = out$NB_SKU_FREQ/out$NB_SKU
    out$EXPENSES_FREQ = bought[NB_CMD>1, sum(Price*Quantity)]
    out$PCT_EXP_FREQ = out$EXPENSES_FREQ / out$EXPENSES
   
  } else {
    out = list("FREQ_ACHAT" = 0,
               "NB_SKU" = 0,
               "ITEM_PRICE_MAX" = 0,
               "EXPENSES" = 0,
               "NB_CANCELLED" = 0,
               "EXPENSES_CANCELLED" = 0,
               "PCT_EXP_CANCELLED" = 0,
               "NB_CMD_MOST_FREQ" = 0,
               "NB_SKU_FREQ" = 0,
               "PCT_SKU_FREQ" = 0,
               "EXPENSES_FREQ" = 0,
               "PCT_EXP_FREQ" = 0)
  }
  return(lapply(out, as.numeric))
}


#' Compute features on a specific targeted period
#'
#' @param data : \code{data.table}. Complete dataset.
#' @param start_rep : \code{character}. Start of the targeted period. Character to be parsed as date (format YYYY-MM-DD)
#' @param end_rep : \code{character}. End of the targeted period. Character to be parsed as date (format YYYY-MM-DD)
#' @param windows : \code{integers}. windows=c("M-1", "M-2", "M-3") or windows=c("T-1", "T-2", "T-3")
#' @param kind : \code{character}. One of c("lapse", "cumulative").
#' 
#' 
#' @return a data.table object
#'
#' @import data.table
#' @import stringr
#' @export
#'
#'
#' @examples 
#' \dontrun{
#' start_rep = "2011-05-01"
#' TODO
#'
create_features_on_period <- function(data, start_rep, end_rep, windows=c("M-1", "M-2", "M-3"), kind="lag"){
  
  agg <- create_var_reponse(data, start_rep, end_rep)
  
  for(wdw in windows){
    len = unname(c("Y"=12, "S"=6, "T"=3, "M"=1)[stringr::str_split(wdw, "-")[[1]][1]])
    n = as.numeric(stringr::str_split(wdw, "-")[[1]][2]) 

    if(kind=="cumulative"){
      latest_X = start_rep # stay unchanged <=> beginning of response period
      length_X = len*n     # variable length : n x Y=12months S=6months T=3months M=1Month
    }else if(kind=="lag"){
      latest_X = as.Date(start_rep)-months(len*(n-1)) # (n-1) x Y=12months S=6months T=3months M=1Month
      length_X = len # defined by Y/S/T/M
    }
    
    # print(paste("Beg.", latest_X-months(length_X)))
    # print(paste("End ", latest_X))
    
    all_customers = data[, .(Customer.ID = unique(Customer.ID))]
    sub_data_agg <- create_subset_data(data, latest_X, length_X)

    agg_pt1 = create_agg_prix_qty(sub_data_agg, all_customers)
    colnames(agg_pt1)[-1] <- paste0(colnames(agg_pt1)[-1], "_", wdw)
    agg <- merge(agg, agg_pt1, by = "Customer.ID")

    agg_pt2 = sub_data_agg[, create_agg_freq_cncl(.SD), by="Customer.ID"]
    colnames(agg_pt2)[-1] <- paste0(colnames(agg_pt2)[-1], "_", wdw)

    agg <- merge(agg, agg_pt2, by = "Customer.ID", all.x=TRUE)
    agg[is.na(agg)] <- 0
  }

  return(agg)
}

#' Compute features on a rolling windows
#'
#' @param data : \code{data.table}. Complete dataset.
#' @param from : \code{Date}. starting date. Required
#' @param to : \code{Date}. end date. Required
#' @param by : \code{character}. increment of the sequence of Dates
#' 
#' 
#' @return a data.table object
#'
#' @import data.table
#' @export
#'
#'
#' @examples 
#' \dontrun{
#' TODO
#'
create_features <- function(from=as.Date("2010/03/01"), to=as.Date("2011/12/01"), by="month", windows=c("M-1", "M-2", "M-3"), kind="lag"){
  agg <- NULL
  start <- seq.Date(from = from, to = to, by)
  end <- seq.Date(from = from+month(1), to = to+month(1), by)
  for(i in 1:length(start)){
    append <- create_features_on_period(data, start[i], end[i])
    if(!is.null(agg)){
      agg = rbind(agg,append)
    } else {
      agg = append
    }  
  }
  return(agg)
}



keywords = c("CHRISTMAS", "UMBRELLA", "CARD", "MUG", "TEA", "BOTTLE", "BAG", 
             "CANDLE", "DOILY", "DOILIES", "MIROR", "WALL ART", "TISSUES", "BOWL", "LIGHT", "BRACELET",
             "T-LIGHT", "BOX", "GLASS", "RUBBER", "PENCIL", "PEN", "DECORATION", "DRESS", "T-SHIRT", "MAGNET", "FRAME",
             "ORGANISER", "ERASER", "NECKL", "CLOCK", "LAMP", "STICKER", "COVER")


#' Compute features on a rolling windows
#'
#' @param labels : \code{characters}. Characters to look patterns into.
#' @param patterns : \code{characters} pretty obvious
#' 
#' @return a data.table object
#'
#' @import a list of (data.table, a coverage indicator, summary of keywords)
#' @export
#'
#'
#' @examples 
#' \dontrun{
#' sample(unique(data$Description), 5000)
#' l = count_keywords(unique(data$Description))
#'
count_keywords <- function(labels, patterns=keywords){
  find = function(label) as.numeric(lapply(patterns, function(pattern, x) {grepl(pattern, x)}, label))
  dt = lapply(labels, find)
  sparse_dt = t(as.data.table(dt))
  colnames(sparse_dt) = patterns
  rownames(sparse_dt) = labels
  return(list(
    "dt" = sparse_dt,
    "summary" = sort(colMeans(sparse_dt))*100, # Mot-clé les plus fréquents
    "coverage" = mean(rowSums(sparse_dt)>0)*100 # Pct de libellés couverts
  ))
}

