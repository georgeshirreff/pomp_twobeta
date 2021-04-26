
# pompModel = seirInflect
# param_df = res %>%  
#   group_by(t_inflect) %>% 
#   mutate(ci_boundary = max(loglik) - ci_interval
#          , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
#   filter(t_inflect == t_i) %>% 
#   filter(ci == "in_ci") %>% 
#   select(names(seirInflect@params))
# NSIM = 10
# SAR_numer_threshold = 3

pomp_sim_distribution <- function(pompModel, param_df, NSIM = 10, SAR_numer_threshold = 3, calculate_logLik = F){
  
  # extract the original observations as a data frame
  obs_df <- pompModel %>% simulate(params = pompModel@params
                                   , seed = 1, nsim = 1, include.data = T, format = "data.frame") %>% 
    filter(.id == "data")
  
  ### make the simulations
  
  if(nrow(param_df) == 1){
    # simulate single
    
    sims <- pompModel %>% simulate(params = param_df %>% unlist
                                   , seed = 1, nsim = NSIM, include.data = F, format = "data.frame")
    
    if(calculate_logLik){
      stop("Can't calculate logLik for a single parameter set yet")
    }
    
    
  } else {
    # simulate range
    
    sampled_rows = sample(nrow(param_df), size = NSIM, replace = NSIM > nrow(param_df))
    sampled_param_df = param_df[sampled_rows, ]
    
    for(i in 1:NSIM){
      sims_piece <- simulate(pompModel, params = sampled_param_df[i, ] %>% unlist
                             , seed = i, nsim = 1, include.data = F, format = "data.frame") %>% 
        mutate(.id = i)
      
      if(calculate_logLik){
        sims_obj <- simulate(pompModel, params = sampled_param_df[i, ] %>% unlist
                             , seed = i, nsim = 1, include.data = F)
        
        
        sims_piece %<>% mutate(logLik = dmeasure(pompModel
                                                , x = states(sims_obj)
                                                , y = obs(pompModel)
                                                , params = sims_obj@params
                                                , times = pompModel@times
                                                , log = T) %>% as.vector
        )
        
      }
      
      if(i == 1){
        sims = sims_piece
      } else {
        sims = rbind(sims, sims_piece)
      }
    }
    
    
    

    
  }
  
  if(calculate_logLik){
    sims_filtered <- sims %>% 
      group_by(.id) %>% 
      mutate(not_extinct = any(SAR_numer >= SAR_numer_threshold)) %>% 
      filter(not_extinct) %>% 
      ungroup %>% 
      select(-not_extinct)
      
    max_logLik_id <- sims_filtered %>% 
      group_by(.id) %>% 
      summarise(logLik = sum(logLik)) %>% 
      filter(logLik == max(logLik)) %>% 
      pull(.id)
    
    sims_filtered %>% 
      group_by(Date) %>% 
      summarise(lowCI = quantile(pos, 0.025)
                , highCI = quantile(pos, 0.975)
                , median = quantile(pos, 0.5)
                , max = quantile(pos, 1)) %>% 
      left_join(obs_df %>% transmute(Date, Data = pos)) %>% 
      left_join(sims_filtered %>% 
                  filter(.id == max_logLik_id) %>% 
                  transmute(Date, max_logLik = pos))
  } else {
    sims %>% 
      group_by(.id) %>% 
      mutate(not_extinct = any(SAR_numer >= SAR_numer_threshold)) %>% 
      filter(not_extinct) %>% 
      ungroup %>% 
      select(-not_extinct) %>% 
      group_by(Date) %>% 
      summarise(lowCI = quantile(pos, 0.025)
                , highCI = quantile(pos, 0.975)
                , median = quantile(pos, 0.5)
                , max = quantile(pos, 1)) %>% 
      left_join(obs_df %>% transmute(Date, Data = pos))
    
  }
  
  
  
}

pomp_sim_plot <- function(res_ward, pompModel_source, NSIM = 10, SAR_numer_threshold = 3, calculate_logLik = F){

  sim_distribution_df = NULL
  wardLetter = "A"
  floor = 0
  for(wardLetter in c("A", "B", "C")){
    # print(wardLetter)
    
    wardName = c("Galien", "Hamburger", "Rabelais")[match(wardLetter, c("A", "B", "C"))]
    for(floor in 0:3){
      this_wardCode = paste0(wardLetter, floor)
      # print(floor)
      
      
      if(this_wardCode %in% res_ward$wardCode %>% unique){
        print(this_wardCode)
        
        this_ward = paste0(wardName, "_", floor)
        
        source(pompModel_source)
        pompModel = call_pomp_ward(this_ward)
               
        
        sim_distribution_df_piece = pomp_sim_distribution(pompModel = pompModel
                                                          , param_df = res_ward %>% 
                                                            filter(ci == "in_ci") %>% 
                                                            filter(wardCode == this_wardCode) %>% 
                                                            # {.[which(.$loglik == max(.$loglik, na.rm = T))[1], ]} %>% 
                                                            select(names(pompModel@params))
                                                          , NSIM = NSIM
                                                          , SAR_numer_threshold = SAR_numer_threshold
                                                          , calculate_logLik = calculate_logLik) %>% 
          mutate(wardCode = this_wardCode)
        
        # print("done distribution")
        
        if(is.null(sim_distribution_df)){
          sim_distribution_df = sim_distribution_df_piece
        } else {
          sim_distribution_df = rbind(sim_distribution_df, sim_distribution_df_piece)
        }
        
        # print("binded")
      }
    }
  }
  
  print(c("calculate_loglik", calculate_logLik))
  
  if(!calculate_logLik){
    sim_plot = sim_distribution_df %>% 
      filter(Date > as.Date("2020-03-01")) %>% 
      ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + 
      # geom_bar(aes(y = Data, fill = "Data"), stat = "identity") + 
      geom_ribbon(aes(ymin = lowCI, ymax = highCI), alpha = 0.6, fill = "grey") +
      geom_line(aes(y = highCI, colour = "CI", size = "CI", linetype = "CI")) +
      geom_line(aes(y = lowCI, colour = "CI", size = "CI", linetype = "CI")) +
      geom_line(aes(y = median, colour = "median", size = "median", linetype = "median")) +
      geom_point(aes(y = Data, colour = "Data", size = "Data", linetype = "Data")) + 
      scale_colour_manual(values = c(  Data = "red"   , median = "black" , mode = "blue", CI = "grey")) +
      scale_linetype_manual(values = c(Data = "blank", median = "dashed", mode = "dotted", CI = "solid")) +
      scale_size_manual(values = c(    Data = 2       , median = 1, mode = 1       , CI = 1)) +
      labs(x = "", y = "Positive tests", linetype = "Simulations", size = "Simulations", colour = "Simulations") + 
      theme_bw() + theme(text = element_text(size = 20)
                         # , legend.position = c(0.8, 0.2)
                         , legend.background = element_rect(fill="white",
                                                            size=0.5, linetype="solid")
                         , axis.text.x = element_text(angle = 90)
                         
      ) + 
      facet_wrap(.~wardCode, scales = "free_y")
  } else {
    sim_plot = sim_distribution_df %>% 
      filter(Date > as.Date("2020-03-01")) %>% 
      ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + 
      # geom_bar(aes(y = Data, fill = "Data"), stat = "identity") + 
      geom_ribbon(aes(ymin = lowCI, ymax = highCI), alpha = 0.6, fill = "grey") +
      geom_line(aes(y = highCI, colour = "CI", size = "CI", linetype = "CI")) +
      geom_line(aes(y = lowCI, colour = "CI", size = "CI", linetype = "CI")) +
      geom_line(aes(y = max_logLik, colour = "bestFit", size = "bestFit", linetype = "bestFit")) +
      # geom_line(aes(y = median, colour = "median", size = "median", linetype = "median")) +
      geom_point(aes(y = Data, colour = "Data", size = "Data", linetype = "Data")) + 
      scale_colour_manual(values = c(  Data = "red"   , bestFit = "blue", median = "black" , mode = "blue", CI = "grey")) +
      scale_linetype_manual(values = c(Data = "blank", bestFit = "dashed", median = "dashed", mode = "dotted", CI = "solid")) +
      scale_size_manual(values = c(    Data = 2       , bestFit = 1, median = 1, mode = 1       , CI = 1)) +
      labs(x = "", y = "Positive tests", linetype = "Simulations", size = "Simulations", colour = "Simulations") + 
      theme_bw() + theme(text = element_text(size = 20)
                         # , legend.position = c(0.8, 0.2)
                         , legend.background = element_rect(fill="white",
                                                            size=0.5, linetype="solid")
                         , axis.text.x = element_text(angle = 90)
                         
      ) + 
      facet_wrap(.~wardCode, scales = "free_y")    
  }

  
  sim_plot
}





