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
  return(uk_retailer_2)
}
