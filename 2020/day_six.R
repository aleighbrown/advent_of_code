
# part one ----------------------------------------------------------------
fileName = "/Users/annaleigh/Documents/GitHub/advent_of_code/2020/input/day_six.txt"
full_batch = readChar(fileName, file.info(fileName)$size)
customs = strsplit(full_batch,split = "\n\n")[[1]]
customs = gsub("\n"," ",customs)


custom_df = as.data.table(trimws(customs))
custom_df <- custom_df %>% 
    mutate(num_members = str_count(V1, " ") + 1)  %>% 
    mutate(cleaned = gsub("\\ ","",V1))  %>% 
    as.data.table()

custom_df[,unique_chars := rawToChar(unique(charToRaw(cleaned))), by = 1:nrow(custom_df)]
sum(custom_df[,nchar(unique_chars)])
# part two ----------------------------------------------------------------

lett_count = purrr::map(custom_df$cleaned, ~str_count(.x,letters))
purrr::map2(lett_count, custom_df$num_members, ~ sum(.x == .y)) %>% unlist() %>% sum()
