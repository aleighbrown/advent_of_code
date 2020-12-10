file_name <-  "/Users/annaleigh/Documents/GitHub/advent_of_code/2020/input/day_nine.txt"
input_code <- scan(file_name)

generate_possible_values <- function(vector){
    #outer of sum
    possible_vals <- outer(vector, vector, FUN = "+")
    #2 things times eaech otehr not allowed
    diag(possible_vals) <- NA
    return(possible_vals)
}

check_val_possible <- function(ind, vect, window_length){
    sub_vect <- vect[ind:(window_length + ind - 1)]

    p = generate_possible_values(sub_vect)

    check_number = vect[window_length + ind]

    acceptable_number <- check_number %in% p

    return(acceptable_number)
}

find_false_number <- function(vector, preamble_len = 5, window_length){
    message_start = vector[(preamble_len + 1):length(vector)]
    false_number = message_start[which(!map(1:length(message_start), check_val_possible, message_start, window_length) %>% unlist()) + window_length]
    return(false_number)
}


find_false_number(input_code, preamble_len = 25, window_length = 25)

# part two ----------------------------------------------------------------

false_number = find_false_number(input_code, preamble_len = 25, window_length = 25)

possible_lengths = 2:(length(input_code) - 1)

return_chunk <- function(ind, chunk_size,vect){
    vect[ind:(ind + chunk_size -1)]
}

slide_chunk <- function(chunk_size, vect){
    map(1:length(vect), return_chunk, chunk_size = chunk_size,vect)
}

sum_chunk <- function(chunk_size,vect){
    map(map(1:length(vect), return_chunk, chunk_size = chunk_size,vect),sum) %>% unlist()
    
}


chunk_results <-  map(possible_lengths,sum_chunk, input_code)
correct_length_indx <-  map(chunk_results, ~ false_number %in% .x) %>% unlist() %>% which()
correct_length <- possible_lengths[correct_length_indx]
correct_contig <- which(chunk_results[[correct_length_indx]] == false_number)

contig_vect <- slide_chunk(correct_length,input_code)[[correct_contig]]
min(contig_vect) + max(contig_vect)
