# NA, NULL, 변수의 종류,  두 변수의 길이, Inf 

exists("a")
exists(a)
# logical, integer, double, character, complex, raw 
# NA, NULL na는 logical 이고 길이가 1 
typeof()

str_trim # 을 쓴다음에 key를 지정? 

paste0("H",1:10)

# NA == NA는 NA 이다. ]

# seq_along()

say_good <- function() {
  now_time <- NULL
  now_time <- lubridate::now() %>% str_split(., pattern = "[ :]"  , simplify = T) %>% 
    .[,2] %>% as.double()
  if (is.null(now_time)) { 
    return("error. maybe you should install lubridate package")}
  if  (between(now_time,6,11)) {
    return("good morning")
  } else if (between(now_time,12,17)) {
    return("good afternoon")
  } else {
    return("good evening")
  }
}

stopifnot(#조건들1, #조건2 )  #조건들을 다 만족하면 진행 아니라면 stop!  )
)
stop(is.logical(na.rm),length(na.rm) == 1 ) #na.rm은 T,F LOGCAL이고 VECTOR꼴이 아닌 하나씩 들어와야한다. 그래서 길이 1로 확인 


str_split(., pattern = "[ :]"  , simplify = T) %>% 
  .[,2] %>% as.double()

now_time <- lubridate::now() %>% str_split(., pattern = "[ :]"  , simplify = T) %>% 
  .[,2] %>% as.double()

now_time <- lubridate::now() %>% str_split(., pattern = " ", simplify = T) %>% 
   .[,2] %>% str_split(., pattern = ":",simplify = T) %>% 
     .[,1] %>% as.double()


now_time <- lubridate::now() %>% str_split(., pattern = c(" ",":")  , simplify = T)
extract_numeric( ) # 숫자만 빼주는 좋은 function 


a <- ""
is.null(a)
is.na(a)
