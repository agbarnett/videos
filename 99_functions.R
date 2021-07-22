# 99_functions.R

# function for rounding numbers with zeros kept
roundz = function(x, digits=0){
  dformat = paste('%.', digits, 'f', sep='')
  x = sprintf(dformat, round(x, digits))
  return(x)
}

# function to rename columns
nice_rename = function(intext){
  out = case_when( # nicer variable name
    str_detect(intext, 'EducationMedium') ~ 'Education = Medium',
    str_detect(intext, 'edu1') ~ 'Education = Medium',
    str_detect(intext, 'EducationHigh') ~ 'Education = High',
    str_detect(intext, 'edu2') ~ 'Education = High',
    str_detect(intext, 'EducationLow') ~ 'Education = Low',
    str_detect(intext, 'RaceOther') ~ 'Race = Other',
    str_detect(intext, 'race1') ~ 'Race = Other',
    str_detect(intext, 'RaceBlack') ~ 'Race = Black',
    str_detect(intext, 'race2') ~ 'Race = Black',
    str_detect(intext, 'RaceWhite') ~ 'Race = White',
    str_detect(intext, 'trt2_c') ~ 'Cash Voucher Incentive',
    str_detect(intext, 'trt2_l') ~ 'Lottery Incentive',
    str_detect(intext, 'age') ~ 'Age (+10 years)',
    str_detect(intext, 'gender|^male') ~ 'Gender = Male',
    str_detect(intext, 'rumppercent') ~ 'State % Trump vote (+10%)',
    is.character(intext) == TRUE ~ as.character(intext)
  )
  return(out)
}

## function to make interaction estimates from chains and design matrix
# makes probabilities or odds ratios
make_ests = function(design, chains, type='OR', 
                     ref = 'relative'){ # reference = relative (within-category) or fixed reference group
  design = mutate(design, num = 1:n())
  design_long = pivot_longer(design, cols=starts_with('x')) %>%
    filter(value == 1) %>%
    mutate(name = as.numeric(str_remove(name, 'x')))
  long_chains = NULL
  for (i in 1:nrow(design)){
    cols = filter(design_long, num == i) %>% pull(name) # get columns to use
    if(length(cols)==1){
      p1 = data.frame(num=i, chain=1, est=chains[cols,,1]) %>% mutate(x=1:n())
      p2 = data.frame(num=i, chain=2, est=chains[cols,,2]) %>% mutate(x=1:n())
    }
    if(length(cols)>1){
      p1 = data.frame(num=i, chain=1, est=colSums(chains[cols,,1])) %>% mutate(x=1:n())
      p2 = data.frame(num=i, chain=2, est=colSums(chains[cols,,2])) %>% mutate(x=1:n())
    }
    long_chains = bind_rows(long_chains, p1, p2)
  }
long_chains = mutate(long_chains, p = exp(est)/(1+exp(est)))
# now summarise probabilities
summary = group_by(long_chains, num) %>%
  summarise(mean = mean(p),
            lower = quantile(p, 0.025),
            upper = quantile(p, 0.975)) %>%
  ungroup() %>%
  left_join(design, by='num') %>% select(-starts_with('x'))

## if odds ratios needed
if(type=='OR'){
  control = filter(design, Treatment == 'control')
  not_control = filter(design, Treatment != 'control')
  all_combs = NULL 
  for (i in 1:nrow(not_control)){
    numerator = filter(long_chains, num == not_control$num[i])
    if(ref=='relative'){index = which(control[,1] == not_control[i,1])} # match on variable
    if(ref=='fixed'){index = 1} # always first group - THIS IS IMPORTANT, controls reference group, if turned off then reference is relative
    denominator = filter(long_chains, num == index) %>% select(-num)
    comb = left_join(numerator, denominator, by=c('chain','x'))
    all_combs = bind_rows(all_combs, comb)
  }
  all_combs = mutate(all_combs, 
                     pr = p.x / p.y, # prevalence ratio
                     orx = est.x -  est.y, # on logit scale
                     or = (p.x / (1-p.x)) / (p.y / (1-p.y))) # on probability scale
  # now summarise
  summary = group_by(all_combs, num) %>%
    summarise(pmean = median(p.x),
              plower = quantile(p.x, 0.025),
              pupper = quantile(p.x, 0.975),
              prmean = median(pr), # probability ratio
              prlower = quantile(pr, 0.025),
              prupper = quantile(pr, 0.975),
            #  meanl = median(orx), # on logit scale, take exp to get OR - gives same answers
            #  lowerl = quantile(orx, 0.025),
            #  upperl = quantile(orx, 0.975),
              mean = median(or),
              lower = quantile(or, 0.025),
              upper = quantile(or, 0.975)) %>%
    ungroup() %>%
    left_join(design, by='num') %>% select(-starts_with('x'))
}

return(summary)

} # end of function