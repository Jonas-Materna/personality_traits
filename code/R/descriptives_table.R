library(stats)


t.test(big5$agree[big5$acc==T], big5$agree[big5$prof==T])
length(!is.na(big5$agree[big5$acc==T]))
length(!is.na(big5$agree[big5$prof==T]))

t.test(big5$consc[big5$acc==T], big5$consc[big5$prof==T])
length(!is.na(big5$consc[big5$acc==T]))
length(!is.na(big5$consc[big5$prof==T]))

t.test(big5$extra[big5$acc==T], big5$extra[big5$prof==T])
length(!is.na(big5$extra[big5$acc==T]))
length(!is.na(big5$extra[big5$prof==T]))


t.test(big5$neuro[big5$acc==T], big5$neuro[big5$prof==T])
length(!is.na(big5$neuro[big5$acc==T]))
length(!is.na(big5$neuro[big5$prof==T]))

t.test(big5$openn[big5$acc==T], big5$openn[big5$prof==T])
length(!is.na(big5$openn[big5$acc==T]))
length(!is.na(big5$openn[big5$prof==T]))

t.test(big5$female[big5$acc==T], big5$female[big5$prof==T])
length(!is.na(big5$female[big5$acc==T]))
length(!is.na(big5$female[big5$prof==T]))

t.test(big5$german[big5$acc==T], big5$german[big5$prof==T])
length(!is.na(big5$german[big5$acc==T]))
length(!is.na(big5$german[big5$prof==T]))

t.test(soep_data$married[big5$acc==T], soep_data$married[big5$prof==T])

t.test(soep_data$pglabgro[big5$acc==T], soep_data$pglabgro[big5$prof==T])

t.test(soep_data$plh0173[big5$acc==T], soep_data$plh0173[big5$prof==T]) #Work

t.test(soep_data$plh0182[big5$acc==T], soep_data$plh0182[big5$prof==T]) # Life

t.test(soep_data$plh0176[big5$acc==T], soep_data$plh0176[big5$prof==T]) # Income

t.test(soep_data$plh0178[big5$acc==T], soep_data$plh0178[big5$prof==T]) # Leisrue

!is.na(soep_data$pglabgro[big5$acc==T])

length(unique(soep_data$pid[soep_data$acc == T & !is.na(soep_data$plh0173)]))
length(unique(soep_data$pid[soep_data$prof == T & !is.na(soep_data$plh0173)]))

length(unique(soep_data$pid[soep_data$acc == T & !is.na(soep_data$plh0182)]))
length(unique(soep_data$pid[soep_data$prof == T & !is.na(soep_data$plh0182)]))

length(unique(soep_data$pid[soep_data$acc == T & !is.na(soep_data$plh0176)]))
length(unique(soep_data$pid[soep_data$prof == T & !is.na(soep_data$plh0176)]))

length(unique(soep_data$pid[soep_data$acc == T & !is.na(soep_data$plh0178)]))
length(unique(soep_data$pid[soep_data$prof == T & !is.na(soep_data$plh0178)]))
