
fileName = "/Users/annaleigh/Documents/GitHub/advent_of_code/2020/input/day_seven_example.txt"
full_batch = readChar(fileName, file.info(fileName)$size)
bag_rules = strsplit(full_batch,split = "\n")[[1]]

make_bag_list = function(bag_string){
    number_of = str_extract_all(bag_string, "[[:digit:]]+") %>% unlist() %>% as.numeric()
    type_of_bags = strsplit(bag_string,", ") %>% 
        unlist() %>% 
        gsub("[^a-zA-Z] ", "", .) %>% 
        gsub("bags","",.) %>% 
        gsub("bag","",.) %>% 
        gsub("\\.","",.)
    type_of_bags <- trimws(type_of_bags)
    print(type_of_bags)
    if(length(type_of_bags) == length(number_of)){
        names(number_of) <- type_of_bags
        return(number_of)
    }else{
        return(type_of_bags)
    }

}

bag_search <- function(bag_rules, 
                                 cur_search_color, 
                                 possible_containers = NULL,
                                 searched_colors = NULL){
    if(is.null(possible_containers)){
        possible_containers <- vector(mode = 'character')
        searched_colors <- vector(mode = 'character')
    }
    possible_containers <- bag_rules[grep(cur_search_color,bag_rules$bag_list),'bag_color'] %>% 
        unlist() %>% c(possible_containers,.) %>% unique()
    searched_colors <- c(searched_colors, cur_search_color) %>% unique()
    
    remaining_colors = possible_containers[!possible_containers %in% searched_colors]

    
    return_list = list(possible_containers,searched_colors)
    names(return_list) <- c("possible_containers","searched_colors")
    return(return_list)
}
    
bag_rules = as_tibble(bag_rules) %>% 
    separate(value,sep = " bags contain ", into = c("bag_color","contents")) 

bag_rules$bag_list <- purrr::map(bag_rules$contents, make_bag_list) 


all_poss = bag_search(bag_rules,"shiny gold")$possible_containers
all_searched = bag_search(bag_rules,"shiny gold")$searched_colors

remaining_colors = all_poss[!all_poss %in% all_searched]
while(length(remaining_colors) > 0 ){
    for(r in remaining_colors){
        ps = bag_search(bag_rules,r)$possible_containers
        src = bag_search(bag_rules,r)$searched_colors
        all_poss = c(all_poss,ps) %>% unique()
        all_searched = c(all_searched,src) %>% unique()
        remaining_colors = all_poss[!all_poss %in% all_searched]
    }
}

length(all_poss)

# part two ----------------------------------------------------------------


bag_rules = bag_rules %>% 
    mutate(number_bags = ifelse(grepl("no other",contents), 0,
                                      bag_list))
bag_rules$number_bags %>% names()

bag_search_down <- function(bag_rules, 
                       cur_search_color, 
                       contained_bags = NULL,
                       searched_colors = NULL,
                       how_many_current = 1){
    if(is.null(contained_bags)){
        contained_bags <- data.frame()
        searched_colors <- vector(mode = 'character')
    }
    
    bag_contains = bag_rules %>% 
        filter(bag_color == cur_search_color) %>% 
        .$number_bags
    
    
    if(cur_search_color == "shiny gold"){
        how_many_current = 1 * bag_contains[[1]]

    }else{
        print(cur_search_color)
        print(how_many_current)
        how_many_current = how_many_current * bag_contains[[1]]
        print(bag_contains[[1]])
    }
    
    if(!is.null(names(bag_contains[[1]]))){
        new_bags = tibble(container = cur_search_color,
                          count = bag_contains[[1]],
                          bag_inside = names(bag_contains[[1]]), 
                          number_of_these_in_gold = how_many_current)
        contained_bags <- rbind(contained_bags,new_bags)
    }

    
    searched_colors <- c(searched_colors, cur_search_color) %>% unique()
    
    
    return_list = list(contained_bags,searched_colors,how_many_current)
    names(return_list) <- c("contained_bags","searched_colors","how_many_current")
    return(return_list)
}

tst = bag_search_down(bag_rules,"shiny gold")


all_contained = bag_search_down(bag_rules,"shiny gold")$contained_bags
all_searched = bag_search_down(bag_rules,"shiny gold")$searched_colors
cur = bag_search_down(bag_rules,"shiny gold")$how_many_current
remaining_colors = all_contained$bag_inside[!all_contained$bag_inside %in% all_searched]
while(length(remaining_colors) > 0 ){
    for(r in remaining_colors){
        print(r)
        ps = bag_search_down(bag_rules,r,all_contained,how_many_current = cur)$contained_bags
        src = bag_search_down(bag_rules,r,all_contained,how_many_current = cur)$searched_colors
        cur = bag_search_down(bag_rules,r,all_contained,how_many_current = cur)$how_many_current
        
        all_contained = rbind(all_contained,ps) %>% unique()
        all_searched = c(all_searched,src) %>% unique()
        remaining_colors = all_contained$bag_inside[!all_contained$bag_inside %in% all_searched]
        print(remaining_colors)
    }
}

all_contained
