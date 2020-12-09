

nop_rule = function(ind){
    return(ind + 1)
}

acc_rule = function(ind,V2){
    accum <<- accum + V2
    return(ind + 1)
}

jump_rule = function(ind,V2){
    return(ind + V2)
}

operator = function(instruction,ind,V2){
    switch(instruction, 
           nop={

               new_ind = nop_rule(ind)
           },
           acc={
               # case 'bar' here...
               new_ind = acc_rule(ind,V2)   
           },
           jmp={
               new_ind = jump_rule(ind,V2)
           }
    )
    return(new_ind)
}

boot_code = fread("/Users/annaleigh/Documents/GitHub/advent_of_code/2020/input/day_eight_example.txt",header = F)
boot_code[,num_exc := 0]
boot_code[,ind := 1:nrow(boot_code)]

accum <- 0

run_path = vector()
cur_ind = 1
never_run = TRUE
while(never_run){
    run_path = c(run_path,cur_ind)
    #check that this operation hasn't been run
    never_run = boot_code[cur_ind,num_exc] == 0

    boot_code[cur_ind,num_exc := num_exc + 1]
    if(never_run){
        new_ind = operator(boot_code[cur_ind,V1],cur_ind,boot_code[cur_ind,V2])
    }
    cur_ind = new_ind

}
print(accum)

#to make sure I end, I need to ensure that the path will at some point end at either the last jmp or not
completed_path_generator <- function(boot_code){
    internal_boot_code = copy(boot_code)
    cur_ind = 1
    never_run = TRUE
    while(never_run){
        run_path = c(run_path,cur_ind)
        if(cur_ind > nrow(internal_boot_code)){
            return(TRUE)
        }
        #check that this operation hasn't been run
        never_run = internal_boot_code[cur_ind,num_exc] == 0
        
        internal_boot_code[cur_ind,num_exc := num_exc + 1]
        if(never_run){
            new_ind = operator(internal_boot_code[cur_ind,V1],cur_ind,internal_boot_code[cur_ind,V2])
        }
        cur_ind = new_ind
        
    }
    return(!any(table(run_path) == 2))
}

swap_postion <- function(ind, boot_code,new_intruction){
    internal_boot_code = copy(boot_code)
    return(internal_boot_code[ind,V1 := new_intruction])
}

generate_potential_boot_codes <- function(boot_code){
    nop_positions = boot_code[V1 == 'nop',ind]
    jmp_positions = boot_code[V1 == 'jmp',ind]
    nop_swapped = map(nop_positions, swap_postion, boot_code,"jmp")
    jmp_swapped = map(jmp_positions, swap_postion, boot_code,"nop")
    return(c(nop_swapped,jmp_swapped))
}

boot_code = fread("/Users/annaleigh/Documents/GitHub/advent_of_code/2020/input/day_eight.txt",header = F)
boot_code[,num_exc := 0]
boot_code[,ind := 1:nrow(boot_code)]

accum <- 0


cur_ind = 1
never_run = TRUE

possible_paths = generate_potential_boot_codes(boot_code)
cor_path = map(possible_paths,completed_path_generator) %>% unlist() %>% which()
correct_path = possible_paths[[cor_path]]

accum <- 0
cur_ind = 1
never_run = TRUE
while(never_run){
    run_path = c(run_path,cur_ind)
    #check that this operation hasn't been run
    never_run = correct_path[cur_ind,num_exc] == 0

    correct_path[cur_ind,num_exc := num_exc + 1]
    if(never_run){
        new_ind = operator(correct_path[cur_ind,V1],cur_ind,correct_path[cur_ind,V2])
    }
    cur_ind = new_ind
    
}
