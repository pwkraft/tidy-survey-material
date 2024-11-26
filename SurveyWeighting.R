## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, tidy = FALSE)


## .small .remark-code { /*Change made here*/
## font-size: 80% !important;
## }
## .smaller .remark-code { /*Change made here*/
## font-size: 70% !important;
## }

## -------------------------------------------------------------------------------------------------------------------------------
install.packages(setdiff("sampling", rownames(installed.packages())))
library(sampling)


## -------------------------------------------------------------------------------------------------------------------------------
s <- srswor(
   n = 10,  ## Sample size
   N = 50   ## Population size
)

matrix(s, nrow = 5, byrow = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
which(s==1)


## -------------------------------------------------------------------------------------------------------------------------------
group <- rep(1:3, each = 5)


## -------------------------------------------------------------------------------------------------------------------------------
X <- cbind(1:15)


## -------------------------------------------------------------------------------------------------------------------------------
pik <- rep(3/5, times = 15)


## -------------------------------------------------------------------------------------------------------------------------------
s <- balancedstratification(
   X = X, strata = group, pik = pik, 
   comment = FALSE
)


## -------------------------------------------------------------------------------------------------------------------------------
matrix(s, nrow = 3, byrow = TRUE)
which(s==1)


## -------------------------------------------------------------------------------------------------------------------------------
s <- balancedcluster(
   X = X, m = 2, cluster = group,
   selection = 2, # cluster selection equal or by size
   comment = FALSE
)


## -------------------------------------------------------------------------------------------------------------------------------
head(s)


## -------------------------------------------------------------------------------------------------------------------------------
unique(group[s[,1]==1])


## -------------------------------------------------------------------------------------------------------------------------------
which(s[,1]==1)


## ----inst_srv2, message=FALSE, warning=FALSE------------------------------------------------------------------------------------
# Install survey and srvyr packages (only once!)
install.packages(setdiff(c("survey", "srvyr", "tidyverse", "here"), 
                         rownames(installed.packages())))

# Load packages
library(tidyverse) # for tidyverse
library(here) # for file paths
library(survey) # for survey analysis
library(srvyr) # for tidy survey analysis


## ----recsexamp, eval=FALSE------------------------------------------------------------------------------------------------------
## library(survey)
## RECS15 <- read.csv(file='< location where file is stored >', header=TRUE, sep=",")
## sampweights <- RECS15$NWEIGHT
## brrwts <- RECS15[, grepl(“^BRRWT”, names(RECS15))]
## des <- svrepdesign(weights=sampweights, repweights=brrwts, type="Fay",
##                    rho=0.5, mse=TRUE, data=RECS15)


## ----sd_tsl_syn, eval=FALSE-----------------------------------------------------------------------------------------------------
## as_survey_design(
##    .data,
##    ids = NULL,#cluster IDs/PSUs
##    strata = NULL,#strata variables
##    variables = NULL,#defaults to all in .data
##    fpc = NULL,#variables defining the finite population correct
##    nest = FALSE,#TRUE/FALSE - relabel clusters to nest within strata
##    check_strata = !nest, #check that clusters are nested in strata
##    weights = NULL,# weight variable
##    ...
## )


## ----sd_tsl_gen_ex, eval=FALSE--------------------------------------------------------------------------------------------------
## # simple random sample (SRS)
## apisrs %>% as_survey_design(fpc = fpc)
## 
## # stratified sample
## apistrat %>% as_survey_design(strata = stype, weights = pw)
## 
## # one-stage cluster sample
## apiclus1 %>% as_survey_design(ids = dnum, weights = pw, fpc = fpc)
## 
## # two-stage cluster sample, weights computed from pop size
## apiclus2 %>% as_survey_design(ids = c(dnum, snum), fpc = c(fpc1, fpc2))
## 
## # stratified, cluster design
## apistrat %>% as_survey_design(ids = dnum, strata = stype, weights =pw, nest = TRUE)
## 


## ----anesdatin, eval=FALSE------------------------------------------------------------------------------------------------------
## options(width=130)
## anes <- read_rds(here("Data", "anes_2020.rds")) %>%
##    mutate(Weight=V200010b/sum(V200010b)*231592693)
##    # adjust weight to sum to citizen pop, 18+ in Nov 2020 per ANES methodology documentation
## 
## anes_des <- anes %>%
##    as_survey_design(weights = Weight,
##                     strata = V200010d,
##                     ids = V200010c,
##                     nest = TRUE)
## summary(anes_des)


## ----anesprint, ref.label="anesdatin", echo=FALSE-------------------------------------------------------------------------------


## ----recsin, error=FALSE, warning=FALSE, eval=FALSE-----------------------------------------------------------------------------
## options(width=130)
## recs <- read_rds(here("Data", "recs.rds"))
## 
## recs_des <- recs %>%
##    as_survey_rep(weights=NWEIGHT,
##                  repweights=starts_with("BRRWT"),
##                  type="Fay",
##                  rho=0.5,
##                  mse=TRUE)
## 
## summary(recs_des)


## ----recsprint, ref.label="recsin", echo=FALSE----------------------------------------------------------------------------------


## ----survey_mean_syn3, eval=FALSE-----------------------------------------------------------------------------------------------
## survey_mean(
##   x,
##   na.rm = FALSE,
##   vartype = c("se", "ci", "var", "cv"),
##   level = 0.95,
##   proportion = FALSE,
##   deff = FALSE,
##   df = NULL,
##   ...
## )


## ----survey_mean_syn2, eval=FALSE-----------------------------------------------------------------------------------------------
## survey_design_object %>%
##    summarize(
##       mean_varname=survey_mean(x = continuous_varname)
##       )


## ----survey_mean_ex1------------------------------------------------------------------------------------------------------------
recs_des %>%
   summarize(
      TD_mean=survey_mean(x = TOTALDOL)
      )


## ----survey_mean_ex2, eval=FALSE------------------------------------------------------------------------------------------------
## recs_des %>%
##    summarize(
##       TD_mean=survey_mean(x = SummerTempDay)
##       )


## ----survey_mean_ex2_r, error=TRUE----------------------------------------------------------------------------------------------
recs_des %>%
   summarize(
      TD_mean=survey_mean(x = SummerTempDay)
      )


## ----survey_mean_ex2_sol, error=TRUE, tidy=FALSE--------------------------------------------------------------------------------
recs_des %>%
   summarize(
      TD_mean = survey_mean(
        x = SummerTempDay, 
        na.rm = TRUE )#<<
      )


## ----survey_median_syn, eval=FALSE----------------------------------------------------------------------------------------------
## survey_median(
##   x,
##   na.rm = FALSE,
##   vartype = c("se", "ci"),
##   level = 0.95,
##   df = NULL,
##   ...
## )


## ----survey_median_fib, eval=FALSE----------------------------------------------------------------------------------------------
## recs_des %>%
##    summarize(
##       TD_median=survey_median(x=_________,
##                           na.rm=_________)
##       )


## ----survey_median_fib_sol------------------------------------------------------------------------------------------------------
recs_des %>%
 summarize(
   TD_median=survey_median(x=SummerTempDay,
                           na.rm=TRUE)
 )


## ----survey_ratio_syn, eval=FALSE-----------------------------------------------------------------------------------------------
## survey_ratio(
##   numerator, #<<
##   denominator, #<<
##   na.rm = FALSE,
##   vartype = c("se", "ci", "var", "cv"),
##   level = 0.95,
##   deff = FALSE,
##   df = NULL,
##   ...
## )


## ----survey_ratio_ex------------------------------------------------------------------------------------------------------------
recs_des %>%
   summarize(
      DolPerBTU=survey_ratio(
         numerator = TOTALDOL, #<<
         denominator = TOTALBTU, #<<
         na.rm = TRUE
         )
      )


## ----domain_ex------------------------------------------------------------------------------------------------------------------
recs_des %>%
  group_by(ACUsed) %>% #<<
  summarize(
    ElBill=survey_mean(DOLLAREL, 
                       na.rm=TRUE)
  )


## ----domain_ex_casc-------------------------------------------------------------------------------------------------------------
recs_des %>%
   group_by(ACUsed) %>%
   cascade(
      ElBill=survey_mean(DOLLAREL, 
                         na.rm=TRUE)
   )



## ----domain_tot-----------------------------------------------------------------------------------------------------------------
recs_des %>%
   group_by(ACUsed) %>%
   cascade(
      ElBill=survey_mean(DOLLAREL, na.rm=TRUE),
      N=survey_total(!is.na(DOLLAREL)), #<<
      n=unweighted(sum(!is.na(DOLLAREL))) #<<
   )



## ----filter_bad, eval = FALSE---------------------------------------------------------------------------------------------------
## data %>%
##   filter(state=="NC") %>% #<<
##   as_survey_design(...) %>%
##   summarize(AvgAge=mean(Age))


## ----filter_good, eval=FALSE----------------------------------------------------------------------------------------------------
## data %>%
##   as_survey_design(...) %>%
##   filter(state=="NC") %>% #<<
##   summarize(AvgAge=mean(Age))


## ----subpop1--------------------------------------------------------------------------------------------------------------------
recs_des %>%
  filter(HousingUnitType %in% c("Single-family detached",
                                "Single-family attached")) %>%
  summarize(
    ElBill=survey_mean(DOLLAREL, 
                       na.rm=TRUE)
  )


## ----ttest_syn1, eval=FALSE-----------------------------------------------------------------------------------------------------
## svyttest(formula, # outcome~group for two-sample, outcome~0 for one-sample
##          design,
##          na.rm = FALSE
##          ....)


## ----ttest_syn2, eval=FALSE-----------------------------------------------------------------------------------------------------
## recs_des %>%
##    svyttest(formula=,
##             design=., #<<
##             na.rm=TRUE)


## ----ttest_syn3, eval=FALSE-----------------------------------------------------------------------------------------------------
## recs_des %>%
##    svyttest(design=., #<<
##             formula=,
##             na.rm=TRUE)


## ----ttest_ex1------------------------------------------------------------------------------------------------------------------
recs_des %>%
   svyttest(design=.,
            formula=I(SummerTempNight-68)~0, #<<
            na.rm=TRUE)


## ----ttest_ex2------------------------------------------------------------------------------------------------------------------
recs_des %>%
   svyttest(design=.,
            formula=I(SummerTempNight-WinterTempNight)~0,
            na.rm=TRUE)


## ----ttest_ex3------------------------------------------------------------------------------------------------------------------
recs_des %>%
   svyttest(design=.,
            formula=DOLLAREL~ACUsed,
            na.rm=TRUE)


## ----glm_syn, eval=FALSE--------------------------------------------------------------------------------------------------------
## svyglm(formula,
##        design,
##        na.action, #default is na.omit
##        ....)


## ----twosamp_ex_ttest, eval=FALSE-----------------------------------------------------------------------------------------------
## recs_des %>%
##    svyttest(design=.,
##             formula=DOLLAREL~ACUsed,
##             na.rm=TRUE) #<<


## ----twosamp_ex_glm, eval=FALSE-------------------------------------------------------------------------------------------------
## recs_des %>%
##    svyglm(design=.,
##           formula=DOLLAREL~ACUsed,
##           na.action=na.omit) #<<


## ----twosamp_ex_ttest_run-------------------------------------------------------------------------------------------------------
recs_des %>%
   svyglm(design=.,
          formula=DOLLAREL~ACUsed,
          na.action=na.omit) %>%
  summary()


## ----anova_ex-------------------------------------------------------------------------------------------------------------------
recs_des %>%
   svyglm(design=.,
          formula=SummerTempNight~Region,
          na.action=na.omit) %>%
  summary()



## ----plot_sf_elbill-------------------------------------------------------------------------------------------------------------
p <- recs %>%
  ggplot(aes(x=TOTSQFT_EN, y=DOLLAREL, weight=NWEIGHT)) +
  geom_hex(color="white") + 
  scale_fill_gradient(guide="colourbar",name="Count of Housing Units")



## ----plot_sf_elbill_disp, echo=FALSE, fig.asp=9/16, fig.align="center", out.width="90%", dpi=300--------------------------------
p +
  theme_bw() 


## ----lm_ex----------------------------------------------------------------------------------------------------------------------
m_electric_sqft <- recs_des %>%
   svyglm(design=.,
          formula=DOLLAREL~TOTSQFT_EN,
          na.action=na.omit)
summary(m_electric_sqft)


## ----survey_count_syn, eval=FALSE, tidy=FALSE-----------------------------------------------------------------------------------
## survey_count(
##    x,
##    ...,
##    wt = NULL,
##    sort = FALSE,
##    name = "n",
##    .drop = dplyr::group_by_drop_default(x),
##    vartype = c("se", "ci", "var", "cv")
## )


## ----survey_count_ex------------------------------------------------------------------------------------------------------------
anes_des %>%
   survey_count(AgeGroup, Gender, name="N")



## ----survey_mean_syn, eval=FALSE------------------------------------------------------------------------------------------------
## survey_mean(
##    x,
##    na.rm = FALSE,
##    vartype = c("se", "ci", "var", "cv"),
##    level = 0.95,
##    proportion = FALSE,
##    prop_method = c("logit", "likelihood", "asin", "beta", "mean"),
##    deff = FALSE,
##    df = NULL,
##    ...
## )
## 
## survey_prop(
##   vartype = c("se", "ci", "var", "cv"),
##   level = 0.95,
##   proportion = FALSE,
##   prop_method = c("logit", "likelihood", "asin", "beta", "mean"),
##   deff = FALSE,
##   df = NULL,
##   ...
## )


## ----survey_p_ex1---------------------------------------------------------------------------------------------------------------
anes_des %>%
   group_by(AgeGroup) %>%
   summarize(
      p1=survey_mean(),
      p2=survey_prop(),
      N=survey_total(),
      n=unweighted(n()), # this gets unweighted counts aka sample sizes
      .groups="drop" # summarize option to remove groups
   )


## ----survey_p_cond, tidy=FALSE--------------------------------------------------------------------------------------------------
anes_des %>%
   filter(!is.na(VotedPres2016), !is.na(VotedPres2020)) %>%
   group_by(VotedPres2016, VotedPres2020) %>%
   summarize(
      p=survey_mean(),
      N=survey_total(),
      n=unweighted(n()), 
      .groups="drop"
   )


## ----survey_p_joint-------------------------------------------------------------------------------------------------------------
anes_des %>%
   filter(!is.na(VotedPres2020), !is.na(VotedPres2016)) %>%
   group_by(interact(VotedPres2016, VotedPres2020)) %>% #<<
   summarize(
      p=survey_mean(),
      N=survey_total(),
      .groups="drop"
   )


## ----survey_p_deff--------------------------------------------------------------------------------------------------------------
anes_des %>%
   filter(!is.na(VotedPres2016), !is.na(VotedPres2020)) %>%
   group_by(interact(VotedPres2016, VotedPres2020)) %>% 
   summarize(
      p=survey_mean(deff=TRUE), #<<
      N=survey_total()
   )


## ----survey_p_ci, eval=FALSE----------------------------------------------------------------------------------------------------
## anes_des %>%
##    group_by(interact(Income7, VotedPres2016, VotedPres2020)) %>%
##    summarize(
##       pd=survey_prop(vartype="ci") %>% round(4)
##    ) %>% select(Income7, VotedPres2016, VotedPres2020, pd, contains("_")) %>%
##    DT::datatable(fillContainer = FALSE, options = list(pageLength = 4))


## ----survey_p_ci_print, ref.label="survey_p_ci", eval=TRUE, echo=FALSE----------------------------------------------------------


## ----logisticsyntax, eval=FALSE-------------------------------------------------------------------------------------------------
## svyglm(formula, # response ~ terms
##        design,
##        na.action, #default is na.omit
##        family = quasibinomial, # use this to avoid warning about non-integers
##        ....)


## ----logisticexamp--------------------------------------------------------------------------------------------------------------
filter(anes_des, Weight>0) %>%
   svyglm(design=.,
          formula=TrustGovernment~ VotedPres2020_selection,
          family = quasibinomial) %>%
   summary()

