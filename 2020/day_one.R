library(data.table)
#read in the data as a data.table
day_1_input = fread("/Users/annaleigh/Documents/GitHub/advent_of_code/input/day_one",header = F)


# part one of the puzzle --------------------------------------------------
#small helper that uses shift to lag the values - I think this means that the longest will be the length of the input
lagger <- function(dt,n){
    dt_cop = copy(dt)
    dt_cop$d2 = NULL
    dt_cop[,d2 := shift(V1,n = n)]
    dt_cop[is.na(d2),d2 := tail(day_1_input$V1,n)]
}

#Not the cleanest solution but the idea is to shift the data to itself until we find the match
not_found = T
i = 1
while(not_found){
    print(i)
    i = i + 1
    lagged = lagger(day_1_input,i)
    lagged[,s := V1 + d2]
    if(any(lagged$s == 2020)){
        not_found = FALSE
    }else{
        rm(lagged)
    }
}

# part two of the puzzle --------------------------------------------------
#same concept as before, but now we know that it number + number + a third equalling to 2020, so again I think max n steps in length of input
not_found_2 = T
j = 1
while(not_found_2){
    print(j)
    j = j + 1
    lagged = lagger(day_1_input,j)
    lagged[,s := V1 + d2]
    lagged[,rm := s - 2020]

    if(any((lagged[rm < 0,rm] * -1) %in% lagged$V1)){
        not_found_2 = FALSE
    }
}
#this is just gettingt he numers and confirming
row_is = lagged[rm < 0][which((lagged[rm < 0,rm] * -1) %in% lagged$V1)]
s_conf = row_is$V1 + row_is$d2 + (row_is$rm * -1)
mult = row_is$V1 * row_is$d2 * (row_is$rm * -1)
