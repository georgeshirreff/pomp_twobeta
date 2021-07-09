library(pomp)

posneg = read_csv("~/tars/input/posneg_alltests.csv") %>%
  transmute(Date = Date %>% as.numeric, pos, neg, Patients, adm, dd) %>%
  filter(Date >= as.numeric(as.Date("2020-02-01"))
         , Date <= as.numeric(as.Date("2020-04-30")))



tests_tab <- posneg %>% transmute(Date
                                  , tests = pos + neg
                                  , tomorrows_tests = c(tests[-1], 0)
                                  , tomorrows_adm = c(adm[-1], 0)
                                  , tomorrows_dd = c(dd[-1], 0)) %>%
  covariate_table(times = "Date", order = "constant")



posneg %>% 
  pomp(t0 = posneg$Date[1]
       , times="Date"
       , params=c(beta1 = 0.5 # frequency-dependent transmission rate from Is (symptomatic infection)
                  , beta2 = 0.2
                  , t_inflect = "2020-03-19" %>% as.Date %>% as.numeric
                  
                  , alpha = 1/3.4 #Li et al. latent period #0.2
                  , gamma = 1/2.3 #He et al. pre-symptomatic infectious period #Zhang et al. incubation period - Li et al. latent period # 0.5 # Pashka
                  , delta = 1/(9.3-2.3) #He et al. symptomatic infectious period (all - presymptomatic) # 0.2 # Pashka
                  
                  , epsilon = 0.63 # multiplication factor of transmission rate for contagious incubation period leading to symptomatic infection (Es*beta*epsilon) # progression  # Buitrago-Garcia et al. - risk ratio for pre-symptomatic transmission relative to symptomatic
                  
                  , psi = 0.7 # proportion symptomatic # Buitrago-Garcia et al. 
                  , kappa1 = 0.35 # multiplication factor of transmission rate for full asymptomatic infections (Ia*beta*kappa1) # Buitrago-Garcia et al. - risk ratio for asymptomatic transmission relative to symptomatic
                  , kappa2 = 1
                  , kappa3 = 1
                  
                  , omega = 0.04
                  
                  # , z = 0.8
                  , Ze = 0.1 # Kucirka et al.
                  , Zes = 0.7 # Kucirka et al.
                  , Zea = 0.7 #Ra et al. BMJ - asymptomatic and symptomatic patients have similar viral loads
                  , Zis = 0.8 # Kucirka et al.
                  , Zia = 0.8 #Ra et al. BMJ - asymptomatic and symptomatic patients have similar viral loads
                  , Zrp = 0.3 # Kucirka et al.
                  , v = 0.99
                  
                  , phi = 0.6 #estimated from data
                  
                  , mu = 1 # relative discharge rate of symptomatic infection
                  
                  , N_0 = posneg$Patients[1]
                  
                  , E_init = 1
                  , t_init = "2020-03-05" %>% as.Date %>% as.numeric
       )
       , covar = tests_tab
       , paramnames=c("beta1"
                      , "beta2"
                      , "t_inflect"
                      
                      , "alpha"
                      , "gamma"
                      , "delta"
                      
                      , "epsilon"
                      , "psi"
                      , "kappa1"
                      , "kappa2"
                      , "kappa3"
                      
                      , "omega"
                      
                      # , "z"
                      , "Ze"
                      , "Zes"
                      , "Zea"
                      , "Zis"
                      , "Zia"
                      , "Zrp"
                      
                      , "v"
                      
                      # , "rho"
                      , "phi"
                      
                      , "mu"
                      
                      , "N_0"
                      , "E_init"
                      , "t_init"
                      
                      
       )
       , obsnames = c("pos", "neg")
       , statenames=c("S","E", "Es", "Ea", "Is", "Ia", "Rp", "R", "NUntested", "N"
                      , "E_toInfect"
                      , "ST", "ET", "EsT", "EaT", "IsT", "IaT", "RpT", "RT", "NT"
                      , "SToday", "EToday", "EsToday", "EaToday", "IsToday", "IaToday", "RpToday", "RToday", "NToday"
                      , "SAR_numer", "SAR_denom"
                      , "Ninfected"
                      , "AToday", "DDToday"
                      , "DailyInc", "Undetected"
                      , "E_untested_dis", "sympt_untested_rec", "sympt_untested_dis", "asympt_untested_rec", "asympt_untested_dis"
                      , "retest")
       , rinit=Csnippet("
                        S = N_0;
                        E = 0;
                        E_toInfect = E_init;
                        Es = 0;
                        Ea = 0;
                        Is = 0;
                        Ia = 0;
                        Rp = 0;
                        R = 0;
                        NUntested = N_0;
                        N = N_0;
                        
                        ST = 0;
                        ET = 0;
                        EsT = 0;
                        EaT = 0;
                        IsT = 0;
                        IaT = 0;
                        RpT = 0;
                        RT = 0;
                        NT = 0;
                        
                        SToday = 0;
                        EToday = 0;
                        EsToday = 0;
                        EaToday = 0;
                        IsToday = 0;
                        IaToday = 0;
                        RpToday = 0;
                        RToday = 0;
                        NToday = 0;
                        
                        SAR_numer = 0;
                        SAR_denom = N_0;
                        
                        Ninfected = 0;
                        
                        AToday = 0;
                        DDToday = 0;
                        
                        DailyInc = 0;
                        Undetected = 0;
                        
                        E_untested_dis = 0;
                        sympt_untested_rec = 0;
                        sympt_untested_dis = 0;
                        asympt_untested_rec = 0;
                        asympt_untested_dis = 0;
                        
                        retest = 0;
                        
                        ")
       , rprocess=gillespie_hl(
         
         # initial seed of infection
         
         # transitions between untested compartments
         initEfromS = list("rate = (floor(t) == nearbyint(t_init)-1 & nearbyint(E_toInfect) > 0.1) ? 1e6*S : 0;"
                           , c(S=-1, E=+1, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect=-1, SAR_numer=+1, SAR_denom= 0, Ninfected=+1, AToday= 0, DDToday= 0, DailyInc = +1, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , initETfromST = list("rate = (floor(t) == nearbyint(t_init)-1 & nearbyint(E_toInfect) > 0.1) ? 1e6*ST : 0;"
                               , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST=-1, ET=+1, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect=-1, SAR_numer=+1, SAR_denom= 0, Ninfected=+1, AToday= 0, DDToday= 0, DailyInc = +1, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , StoE = list("double beta = t < t_inflect ? beta1 : beta2;
                       rate = S*beta*(epsilon*(Es + EsT + kappa1*(Ea + EaT)) + (Is + IsT + kappa1*(Ia + IaT)))/N;"
                       , c(S=-1, E=+1, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer=+1, SAR_denom= 0, Ninfected=+1, AToday= 0, DDToday= 0, DailyInc = +1, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , EtoEs = list("rate = psi*alpha*E;"
                        , c(S= 0, E=-1, Es=+1, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , EtoEa = list("rate = (1-psi)*alpha*E;"
                        , c(S= 0, E=-1, Es= 0, Ea=+1, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , EstoIs = list("rate = gamma*Es;"
                         , c(S= 0, E= 0, Es=-1, Ea= 0, Is=+1, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , EatoIa = list("rate = kappa2*gamma*Ea;"
                         , c(S= 0, E= 0, Es= 0, Ea=-1, Is= 0, Ia=+1, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , IstoRp = list("rate = delta*Is;"
                         , c(S= 0, E= 0, Es= 0, Ea= 0, Is=-1, Ia= 0, Rp=+1, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday= 0, DailyInc = 0, Undetected =+1, E_untested_dis = 0, sympt_untested_rec =+1, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , IatoRp = list("rate = kappa3*delta*Ia;"
                         , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia=-1, Rp=+1, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday= 0, DailyInc = 0, Undetected =+1, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec =+1, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , RptoR = list("rate = omega*Rp;"
                        , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp=-1, R=+1, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         
         , STtoET = list("double beta = t < t_inflect ? beta1 : beta2;
                         rate = ST*beta*(epsilon*(Es + EsT + kappa1*(Ea + EaT)) + (Is + IsT + kappa1*(Ia + IaT)))/N;"
                         , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST=-1, ET=+1, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer=+1, SAR_denom= 0, Ninfected=+1, AToday= 0, DDToday= 0, DailyInc = +1, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , ETtoEsT = list("rate = psi*alpha*ET;"
                          , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET=-1, EsT=+1, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , ETtoEaT = list("rate = (1-psi)*alpha*ET;"
                          , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET=-1, EsT= 0, EaT=+1, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , EsTtoIs = list("rate = gamma*EsT;"
                          , c(S= 0, E= 0, Es= 0, Ea= 0, Is=+1, Ia= 0, Rp= 0, R= 0, NUntested=+1, N= 0, ST= 0, ET= 0, EsT=-1, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT=-1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , EaTtoIaT = list("rate = kappa2*gamma*EaT;"
                           , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT=-1, IsT= 0, IaT=+1, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , IsTtoRpT = list("rate = delta*IsT;"
                           , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT=-1, IaT= 0, RpT=+1, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , IaTtoRpT = list("rate = kappa3*delta*IaT;"
                           , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT=-1, RpT=+1, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday= 0, DailyInc = 0, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , RpTtoRT = list("rate = omega*RpT;"
                          , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT=-1, RT=+1, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday= 0, DailyInc = 0, Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         
         , Stest = list("rate = tomorrows_tests - NToday > 0 ? S*1e6 : 0;"
                        , c(S=-1, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested=-1, N= 0, ST=+1, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT=+1, SToday=+1, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Etest = list("rate = tomorrows_tests - NToday > 0 ? E*1e6 : 0;"
                        , c(S= 0, E=-1, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested=-1, N= 0, ST= 0, ET=+1, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT=+1, SToday= 0, EToday=+1, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Estest = list("rate = tomorrows_tests - NToday > 0 ? Es*1e6 : 0;"
                         , c(S= 0, E= 0, Es=-1, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested=-1, N= 0, ST= 0, ET= 0, EsT=+1, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT=+1, SToday= 0, EToday= 0, EsToday=+1, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Eatest = list("rate = tomorrows_tests - NToday > 0 ? Ea*1e6 : 0;"
                         , c(S= 0, E= 0, Es= 0, Ea=-1, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested=-1, N= 0, ST= 0, ET= 0, EsT= 0, EaT=+1, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT=+1, SToday= 0, EToday= 0, EsToday= 0, EaToday=+1, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Istest = list("rate = tomorrows_tests - NToday > 0 ? Is*1e6*1e6 : 0;"
                         , c(S= 0, E= 0, Es= 0, Ea= 0, Is=-1, Ia= 0, Rp= 0, R= 0, NUntested=-1, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT=+1, IaT= 0, RpT= 0, RT= 0, NT=+1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday=+1, IaToday= 0, RpToday= 0, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Iatest = list("rate = tomorrows_tests - NToday > 0 ? Ia*1e6 : 0;"
                         , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia=-1, Rp= 0, R= 0, NUntested=-1, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT=+1, RpT= 0, RT= 0, NT=+1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday=+1, RpToday= 0, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Rptest = list("rate = tomorrows_tests - NToday > 0 ? Rp*1e6 : 0;"
                         , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp=-1, R= 0, NUntested=-1, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT=+1, RT= 0, NT=+1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday=+1, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Rtest = list("rate = tomorrows_tests - NToday > 0 ? R*1e6 : 0;"
                        , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R=-1, NUntested=-1, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT=+1, NT=+1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday=+1, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Sretest = list("rate = tomorrows_tests - NToday > 0 ? phi*ST*1e6 : 0;"
                          , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday=+1, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Eretest = list("rate = tomorrows_tests - NToday > 0 ? phi*ET*1e6 : 0;"
                          , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday=+1, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Esretest = list("rate = tomorrows_tests - NToday > 0 ? phi*EsT*1e6 : 0;"
                           , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday=+1, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Earetest = list("rate = tomorrows_tests - NToday > 0 ? phi*EaT*1e6 : 0;"
                           , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday=+1, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Isretest = list("rate = tomorrows_tests - NToday > 0 ? phi*IsT*1e6 : 0;"
                           , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday=+1, IaToday= 0, RpToday= 0, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Iaretest = list("rate = tomorrows_tests - NToday > 0 ? phi*IaT*1e6 : 0;"
                           , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday=+1, RpToday= 0, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Rpretest = list("rate = tomorrows_tests - NToday > 0 ? phi*RpT*1e6 : 0;"
                           , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday=+1, RToday= 0, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Rretest = list("rate = tomorrows_tests - NToday > 0 ? phi*RT*1e6 : 0;"
                          , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N= 0, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday=+1, NToday=+1, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         
         , Sin = list("rate = (tomorrows_adm - AToday > 0) ? 1e6 : 0;"
                      , c(S=+1, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested=+1, N=+1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom=+1, Ninfected= 0, AToday=+1, DDToday= 0, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         
         , Sout = list("rate = (tomorrows_dd - DDToday > 0) ? S*1e6 : 0;"
                       , c(S=-1, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested=-1, N=-1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Eout = list("rate = (tomorrows_dd - DDToday > 0) ? E*1e6 : 0;"
                       , c(S= 0, E=-1, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested=-1, N=-1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected =+1, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Esout = list("rate = (tomorrows_dd - DDToday > 0) ? Es*1e6 : 0;"
                        , c(S= 0, E= 0, Es=-1, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested=-1, N=-1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected =+1, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis =+1, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Eaout = list("rate = (tomorrows_dd - DDToday > 0) ? Ea*1e6 : 0;"
                        , c(S= 0, E= 0, Es= 0, Ea=-1, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested=-1, N=-1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected =+1, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis =+1, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Isout = list("rate = (tomorrows_dd - DDToday > 0) ? mu*Is*1e6 : 0;"
                        , c(S= 0, E= 0, Es= 0, Ea= 0, Is=-1, Ia= 0, Rp= 0, R= 0, NUntested=-1, N=-1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected =+1, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis =+1, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Iaout = list("rate = (tomorrows_dd - DDToday > 0) ? Ia*1e6 : 0;"
                        , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia=-1, Rp= 0, R= 0, NUntested=-1, N=-1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected =+1, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis =+1, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Rpout = list("rate = (tomorrows_dd - DDToday > 0) ? Rp*1e6 : 0;"
                        , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp=-1, R= 0, NUntested=-1, N=-1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , Rout = list("rate = (tomorrows_dd - DDToday > 0) ? R*1e6 : 0;"
                       , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R=-1, NUntested=-1, N=-1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT= 0, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , STout = list("rate = (tomorrows_dd - DDToday > 0) ? ST*1e6 : 0;"
                        , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N=-1, ST=-1, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT=-1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , ETout = list("rate = (tomorrows_dd - DDToday > 0) ? ET*1e6 : 0;"
                        , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N=-1, ST= 0, ET=-1, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT=-1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , EsTout = list("rate = (tomorrows_dd - DDToday > 0) ? EsT*1e6 : 0;"
                         , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N=-1, ST= 0, ET= 0, EsT=-1, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT=-1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , EaTout = list("rate = (tomorrows_dd - DDToday > 0) ? EaT*1e6 : 0;"
                         , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N=-1, ST= 0, ET= 0, EsT= 0, EaT=-1, IsT= 0, IaT= 0, RpT= 0, RT= 0, NT=-1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , IsTout = list("rate = (tomorrows_dd - DDToday > 0) ? mu*IsT*1e6 : 0;"
                         , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N=-1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT=-1, IaT= 0, RpT= 0, RT= 0, NT=-1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , IaTout = list("rate = (tomorrows_dd - DDToday > 0) ? IaT*1e6 : 0;"
                         , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N=-1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT=-1, RpT= 0, RT= 0, NT=-1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected=-1, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , RpTout = list("rate = (tomorrows_dd - DDToday > 0) ? RpT*1e6 : 0;"
                         , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N=-1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT=-1, RT= 0, NT=-1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         , RTout = list("rate = (tomorrows_dd - DDToday > 0) ? RT*1e6 : 0;"
                        , c(S= 0, E= 0, Es= 0, Ea= 0, Is= 0, Ia= 0, Rp= 0, R= 0, NUntested= 0, N=-1, ST= 0, ET= 0, EsT= 0, EaT= 0, IsT= 0, IaT= 0, RpT= 0, RT=-1, NT=-1, SToday= 0, EToday= 0, EsToday= 0, EaToday= 0, IsToday= 0, IaToday= 0, RpToday= 0, RToday= 0, NToday= 0, E_toInfect= 0, SAR_numer= 0, SAR_denom= 0, Ninfected= 0, AToday= 0, DDToday=+1, DailyInc = 0 , Undetected = 0, E_untested_dis = 0, sympt_untested_rec = 0, sympt_untested_dis = 0, asympt_untested_rec = 0, asympt_untested_dis = 0, Eretest = 0, Earetest = 0, Esretest = 0, Iaretest = 0, Isretest = 0))
         
         , hmax = 0.5
       )
       , accumvars = c("SToday", "EToday", "EsToday", "EaToday", "IsToday", "IaToday", "RpToday", "RToday", "NToday", "AToday", "DDToday", "DailyInc", "E_untested_dis", "sympt_untested_rec", "sympt_untested_dis", "asympt_untested_rec", "asympt_untested_dis", "retest")
       , rmeasure=function (SToday, EToday, EsToday, EaToday, IsToday, IaToday, RpToday, RToday, v, Ze, Zes, Zea, Zis, Zia, Zrp, ...) {
         
         neg = rbinom(n = 1, size = SToday, prob = v) +
           rbinom(n = 1, size = EToday, prob = (1-Ze)) +
           rbinom(n = 1, size = EsToday, prob = (1-Zes)) +
           rbinom(n = 1, size = EaToday, prob = (1-Zea)) +
           rbinom(n = 1, size = IsToday, prob = (1-Zis)) +
           rbinom(n = 1, size = IaToday, prob = (1-Zia)) +
           rbinom(n = 1, size = RpToday, prob = (1-Zrp)) +
           rbinom(n = 1, size = RToday, prob = v)
         
         pos = SToday + EToday + EsToday + EaToday + IsToday + IaToday + RpToday + RToday - neg
         
         c(pos = pos, neg = neg)
       }
       , dmeasure = Csnippet("
                             
                             //int factorial(int n)
                             //{
                             // single line to find factorial
                             //return (n==1 || n==0) ? 1: n * factorial(n - 1);
                             //}
                             
                             //double lfactorial(int n)
                             //{
                             //return (n==1 || n==0) ? 0: log(n) + lfactorial(n - 1);
                             //}
                             
                             double E_pos, E_neg;
                             double p_pos, p_neg;
                             
                             E_neg = SToday*v + 
                             EToday*(1-Ze) + 
                             EsToday*(1-Zes) + 
                             EaToday*(1-Zea) + 
                             IsToday*(1-Zis) + 
                             IaToday*(1-Zia) + 
                             RpToday*(1-Zrp) + 
                             RToday*v;
                             
                             E_pos = SToday + EToday + EsToday + EaToday + IsToday + IaToday + RpToday + RToday - E_neg;
                             
                             p_pos = E_pos/(E_pos + E_neg);
                             p_neg = E_neg/(E_pos + E_neg);
                             
                             if(E_pos > 0){
                             if(give_log){
                             //lik = -E_pos + pos*log(E_pos) - lfactorial(pos);
                             lik = pos*log(p_pos) + neg*log(p_neg);
                             } else {
                             //lik = (exp(-E_pos)*pow(E_pos, pos))/factorial(pos);
                             lik = pow(p_pos, pos)*pow(p_neg, neg);
                             }
                             } else {
                             if(give_log){
                             lik = 0;
                             } else {
                             lik = 1;
                             }
                             }
                             
                             ")
       ) -> seirInflect
