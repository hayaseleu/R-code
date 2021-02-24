library(tidyverse)
## https://wsyang.com/2014/07/stringr-package/ 간단한 정리 
# 다른 함수의 입력값으로 사용하기 편리한 출력값.
# 길이 0인 입력값에 대해 길이 0인 결과를 돌려줌
# 입력값 NA가 포함되어 있을 때는 그 부분의 결과를 NA로 돌려줌
# 사용빈도가 떨어지는 문자열 조작 처리를 과감하게 제거하여 간략화시킴
# 
# stringr::fixed(string)
# 입력값 그대로의 문자로 매칭 시킴
# stringr::ignore.case(string)
# 대문자 소문자를 무시하여 매치 시킴


my_toppings <- c("green peppers", "olives", "onions")
paste(c("","","and "), my_toppings)


str_subset(  , pattern = fixed("")) ## pattern을 포함하는 항을 전부 return
str_extract( , pattern = "") ## input과 같은 길이의 vector를 반환하지만
#패턴에 해당하는 부분만 뽑아냄 

str_split() # datacamp 봐라   ## list로 반환되는것에 주의 

str_split(string = "Tom & Jerry", pattern = " & ")

str_split(string = "Alvin & Simon & Theodore", pattern = " & ")

str_split("Alivin & Simon & Theodore", pattern = " & " , n =  2) ## n 갯수만큼으로 나누는것. Alivin과 나머지로 나뉨. 


#list로 반환되는 이유는 각 vector의 항마다 같은 숫자로 나뉜다는 보장이 없기 때문

chars <- c("Tom & Jerry", "Alivin & Simon & Theodore")
str_split(chars, pattern = " & ")
str_split(chars, pattern = " & ", simplify = T)

split_chars <- str_split(chars, pattern = " & ", simplify = F)
## simplify T하면 대신 매트릭스 형태로 반환
## 없는 값들은 공백 ""로 표현됨 

## lappy 로 list 처리 가능

lapply(split_chars, length)


# Split lines into words
words <- str_split(lines, pattern = " ")

# Number of words per line
lapply(words, length)
  
# Number of characters in each word
word_lengths <- lapply(words, str_length)
  
# Average word length per line
lapply(words_lengths, mean)

# Some (fake) phone numbers
phone_numbers <- c("510-555-0123", "541-555-0167")

# Use str_replace() to replace "-" with " "
str_replace(phone_numbers, fixed("-"), " ")

# Use str_replace_all() to replace "-" with " "
str_replace_all(phone_numbers, fixed("-"), " ")

# Turn phone numbers into the format xxx.xxx.xxxx


str_replace_all(phone_numbers, fixed("-"), ".")
# > # Define some full names
 names <- c("Diana Prince", "Clark Kent")
# > 
# > # Split into first and last names
 names_split <- str_split(names, pattern = " ", simplify = T)
# > 
# > # Extract the first letter in the first name
 abb_first <- str_sub(names_split[,1],1,1)
# > 
# > # Combine the first letter ". " and last name
str_c(abb_first,". ",names_split[,2])
# [1] "D. Prince" "C. Kent"



# 
# > # Define some full names
 names <- c("Diana Prince", "Clark Kent")
# > 
# > # Split into first and last names
 names_split <- str_split(names, pattern = " ", simplify = T)
# > 
# > # Extract the first letter in the first name
# > abb_first <- str_sub(names_split[,1],1,1)
# > 
# > # Combine the first letter ". " and last name
# > str_c(abb_first,". ",names_split[,2])
# [1] "D. Prince" "C. Kent"

START
