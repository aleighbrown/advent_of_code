fileName = "/Users/annaleigh/Documents/GitHub/advent_of_code/2020/input/day_four.txt"
full_batch = readChar(fileName, file.info(fileName)$size)
passports = strsplit(full_batch,split = "\n\n")[[1]]
passports = gsub("\n"," ",passports)

req_fields = c("byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid")

fix_passport = function(x){
    
    tidied = str_replace(sort(unlist(strsplit(x, " "))),":.*","")
    tidied = gsub("cid","",tidied)
    collapsed = paste0(tidied,collapse = "")
    req_collapse = paste0(req_fields, collapse = "") 

    return(collapsed == req_collapse)

}

map(passports, fix_passport ) %>% unlist() %>% sum()

# part two ----------------------------------------------------------------
valid_birthyear <- function(value){
    as.numeric(value['byr']) <= 2002 & as.numeric(value['byr'] >= 1920)
}

valid_iyr <- function(value){
    as.numeric(value['iyr']) <= 2020 & as.numeric(value['iyr'] >= 2010)
    
}

valid_eyr <- function(value){
    as.numeric(value['eyr']) <= 2030 & as.numeric(value['eyr'] >= 2020)
    
}

valid_height <- function(value){
    if(grepl("cm|in",value['hgt'])){
        if(str_extract(value['hgt'], "[aA-zZ]+") == "cm"){
            return(as.numeric(str_extract(value['hgt'], "[0-9]+")) >= 150 & 
                       as.numeric(str_extract(value['hgt'], "[0-9]+")) <=193)
        }else{
            return(as.numeric(str_extract(value['hgt'], "[0-9]+")) >= 59 & 
                       as.numeric(str_extract(value['hgt'], "[0-9]+")) <=76)
        }
    }else{
        return(FALSE)
    }
}

valid_hcl <- function(value){
    clr = value['hcl']
    if(substr(clr,1,1) == "#"){
        end_bit = gsub("#","",clr)
        return(grepl("^([[:alnum:]])+", x=end_bit))
    }else{
        return(FALSE)
    }
}

valid_ecl <- function(value){
    valid_colors = c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    return(value['ecl'] %in% valid_colors)
}

valid_pid <- function(value){
    if(nchar(value['pid']) == 9){
        return(nchar(str_extract(value['pid'], "[0-9]+")) == 9)
    }else{
        return(FALSE)
    }
}

validate_passport = function(x){

    info = sort(unlist(strsplit(x, " "))) %>% strsplit(":") %>% transpose() 
    ks = info[[1]] %>% unlist()
    vals = info[[2]] %>% unlist()
    names(vals) = ks

    
    br = valid_birthyear(vals)
    iyr = valid_iyr(vals)
    eyr = valid_eyr(vals)
    hgt = valid_height(vals)
    hcl = valid_hcl(vals)
    ecl = valid_ecl(vals)
    pid = valid_pid(vals)
    
    all_correct = c(br,iyr,eyr,hgt,hcl,ecl,pid)
    if(!all(all_correct)){
        checks = c("br","iyr","eyr","hgt","hcl","ecl","pid")
        print(checks[!all_correct])
        print(vals)
        print(x)
    }
    all(all_correct)
    

    
}


first_valid = passports[map(passports, fix_passport ) %>% unlist() %>% which()]
map(first_valid, validate_passport ) %>% unlist() %>% sum()
