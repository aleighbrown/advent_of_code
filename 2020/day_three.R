
# part one ----------------------------------------------------------------


#we travel at this slope
travel_slope = c(3,1)
travel_map = data.table::fread("/Users/annaleigh/Documents/GitHub/advent_of_code/2020/input/day_three.txt",header = F)
#some sugar to make trees and open spaces
travel_map[,V1 := gsub("#","1",V1)]
travel_map[,V1 := gsub("\\.","0",V1)]
#turn our hit's into a dt
locations_hit = purrr::map(1:nrow(travel_map), ~ (.x * travel_slope) + c(1,1)) %>% transpose() %>% as.data.table()
#we need to make the map wider to reach the end(which i did NOT understand until I peaked at Tan's solution)
reps_needed = ceiling(locations_hit[V2 == nrow(travel_map), V1 / travel_map[1,nchar(V1)]])
#repeat until we have the right number of columns to reach the bottom
travel_map[,wider := paste0(rep(V1,reps_needed),collapse = ""), by = 1:nrow(travel_map)]
#turn into a matrix of 0 and 1, use type.convert true to keep things easier
wider_map = as.matrix(travel_map[,tstrsplit(wider,"",type.convert=TRUE)])
#we go a bit far in the x coords this way so chop it off
locations_hit = locations_hit[V2 <= nrow(travel_map)]
#now we index the matrix with a matrix of our coords and sum
part_one_ans = sum(wider_map[cbind(locations_hit$V2, locations_hit$V1)])

# part two ----------------------------------------------------------------
slopes = list(c(1,1),c(5,1),c(7,1),c(1,2))

build_locations_hits = function(slope){
    purrr::map(1:nrow(travel_map), ~ (.x * slope) + c(1,1)) %>% transpose() %>% as.data.table()
}

make_wider_map = function(hit_table){
    #we need to make the map wider to reach the end(which i did NOT understand until I peaked at Tan's solution)
    reps_needed = ceiling(hit_table[V2 == nrow(travel_map), V1 / travel_map[1,nchar(V1)]])
    #repeat until we have the right number of columns to reach the bottom
    travel_map[,wider := paste0(rep(V1,reps_needed),collapse = ""), by = 1:nrow(travel_map)]
    #turn into a matrix of 0 and 1, use type.convert true to keep things easier
    wider_map = as.matrix(travel_map[,tstrsplit(wider,"",type.convert=TRUE)])
}

index_into_the_path = function(hit_table, wide_map){
    hit_table = hit_table[V2 <= nrow(travel_map)]
    # print(hit_table)
    sum(wide_map[cbind(hit_table$V2, hit_table$V1)])
    
}

various_paths = purrr::map(slopes, build_locations_hits)
various_wider_maps = purrr::map(various_paths, make_wider_map)
various_sums = purrr::map2(various_paths, various_wider_maps,index_into_the_path)

various_sums %>% unlist() %>% c(part_one_ans) %>% prod()