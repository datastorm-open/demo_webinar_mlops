output$auc_global <- renderAmCharts({makeMonitoringCharts(copy(scores_at_t()), "AUC_GLOBAL", "AUC Global", .6)})
output$acc_global <- renderAmCharts({makeMonitoringCharts(copy(scores_at_t()), "ACC_GLOBAL", "ACC Global", .6)})
output$kappa <- renderAmCharts({makeMonitoringCharts(copy(scores_at_t()), "Kappa", "Kappa de Cohen", .2)})
output$`taux-achat-top-100` <- renderAmCharts({makeMonitoringCharts(copy(scores_at_t()), "TauxAchat.TOP.100", "Taux d'achat dans les 100 client.e.s les mieux classé.e.s", .2)})