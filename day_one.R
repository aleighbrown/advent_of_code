day_1_input = fread("/Users/annaleigh/Documents/GitHub/advent_of_code/input/day_one",header = F)


# part one of the puzzle --------------------------------------------------
lagger <- function(dt,n){
    dt_cop = copy(dt)
    dt_cop$d2 = NULL
    dt_cop[,d2 := shift(V1,n = n)]
    dt_cop[is.na(d2),d2 := tail(day_1_input$V1,n)]
}

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

row_is = lagged[rm < 0][which((lagged[rm < 0,rm] * -1) %in% lagged$V1)]
s_conf = row_is$V1 + row_is$d2 + (row_is$rm * -1)
mult = row_is$V1 * row_is$d2 * (row_is$rm * -1)
