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
#' @return a connection object
#'
#' @import DBI utils
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

#' Import dataset
#'
#' @param path : \code{character}. Path to xlsx file
#' 
#' @return a connection object
#'
#' @import DBI utils
#' @export
#'
#'
#' @examples 
#' \dontrun{
#' dt = import_dataset()
#' compute_features(dt[Customer.ID==13085])
#' compute_features(dt[Customer.ID==15098])
#'
compute_features <- function(dt){
   
  # # subset on product actually bought and products cancelled
  is_C = function (x) startsWith(x,"C")
  bought = dt[which(!is_C(dt$Invoice))]
  cancelled = dt[which(is_C(dt$Invoice))]

  bght_prices_qtty=bought$Price*bought$Quantity
  cncl_prices_qtty=cancelled$Price*cancelled$Quantity
  
  out = list("FREQ_ACHAT" = 1/length(unique(as.character(bought$InvoiceDate))),
             "NB_SKU" = length(unique(bought$StockCode)),
             "ITEM_PRICE_MAX" = ifelse(nrow(bought)>0, max(bght_prices_qtty), 0),
             "EXPENSES" = sum(bght_prices_qtty))
  
  ## Achats annulés
  out$NB_CANCELLED = nrow(cancelled)
  out$EXPENSES_CANCELLED = -sum(cncl_prices_qtty)
  out$PCT_EXP_CANCELLED = out$EXPENSES_CANCELLED / out$EXPENSES
  
  ## Achats fréquents
  NB_FREQ = bought[,.(NB_CMD=length(unique(as.character(InvoiceDate)))),by=StockCode]
  bought = merge(bought, NB_FREQ, by="StockCode")
  out$NB_CMD_MOST_FREQ = ifelse(nrow(bought)>0, as.double(max(NB_FREQ$NB_CMD)), 0)
  out$NB_SKU_FREQ = nrow(NB_FREQ[NB_CMD>1])
  out$PCT_SKU_FREQ = out$NB_SKU_FREQ/out$NB_SKU
  out$EXPENSES_FREQ = bought[NB_CMD>1, sum(Price*Quantity)]
  out$PCT_EXP_FREQ = out$EXPENSES_FREQ / out$EXPENSES
  return(out)
}






