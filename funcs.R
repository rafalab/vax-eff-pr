mytab <- function(...) table(..., useNA = "always")
eff <- function(x) return((1-(1/x)))

ma7 <- function(d, y, k = 7) 
  tibble(date = d, moving_avg = as.numeric(stats::filter(y, rep(1/k, k), side = 1)))

make_pretty <- function(x) prettyNum(replace_na(x, " "), big.mark = ",")

dynamic_round <- function(x, min_rounded = 10){
  ifelse(round(x) >= min_rounded, 
         prettyNum(round(x), big.mark = ","),
         prettyNum(round(x, digits = 1), nsmall = 1, big.mark = ",")) %>%
    replace_na("")
}
make_pct <- function(x, digit = 0) ifelse(is.na(x), "", paste0(format(round(100*x, digit = digit), nsmall = digit), "%"))

collapse_age_range <- function(agegroup, breaks){
  
  breaks <- sort(breaks)
  
  tmp <- tibble(agegroup) %>%
    mutate(agegroup = str_replace(agegroup, "\\+", "-Inf"))
  tmp <- separate(tmp, agegroup, c("start", "end"), sep = "-", remove = FALSE, convert = TRUE)
  
  edges <- c(unique(tmp$start), Inf)
  
  if(!all(breaks %in% edges)) stop("Age breaks must be a subset of", paste(edges, collapse = ","))
  
  labels <- c(paste(breaks[-length(breaks)], c(breaks[-1]-1), sep="-"),
              paste0(breaks[length(breaks)], "+"))
  g <- factor(as.character(cut(tmp$start, c(breaks, Inf), right = FALSE,
                               labels = labels)), levels = labels)
  
  return(g)
}

compute_expected <- function(tab, outcome = "cases", days.between.knots = 21){
  df <- round(length(unique(tab$date))/days.between.knots)
  
  dat <- rename(tab, obs = !!sym(outcome)) %>%
    mutate(x = as.numeric(date) - as.numeric(min(date)),
           wd = factor(wday(date)))
  
  fit <- glm(obs ~ gender + wd + ageRange*ns(x, df = df), offset = log(poblacion), 
             family = "poisson", data = dat)
  
  pred <- predict(fit, se.fit = TRUE, type = "response")
  tab <- mutate(tab, fit = pred$fit, se.fit = pred$se.fit)
  return(tab)  
}

fit_waning_model <- function(tab, knots = NULL, alpha = 0.01, transf = function(x){ 1-(1/exp(-x)) }){
  
  max.day <- unique(tab$max.day)
  if(is.null(knots)){
    knots <- seq(1, max.day, len=round(max.day/28))
    knots <- knots[-c(1, length(knots))]
  }
  
  tab <- filter(tab, day <= max.day)
  
  fit <- glm(obs ~ ns(day, knots = knots), offset = log(exp), family = "poisson", data = tab)
  
  pred <- predict(fit, newdata = list(day=tab$day, exp = 1), se = TRUE)
  
  mutate(tab, 
         fit = transf(pred$fit),
         conf.low = transf(pred$fit + qnorm(1-alpha/2) * pred$se.fit),
         conf.high = transf(pred$fit - qnorm(1-alpha/2) * pred$se.fit))
}


fit_prob_model <- function(tab, outcome = "hosp", group = "manu"){
  ilogit <- function(x) 1/(1+exp(-x))
  
  dat <- mutate(tab, wd = factor(wday(date, label = TRUE))) %>%
    rename(obs = !!sym(outcome)) %>%
    rename(group = !!sym(group)) %>%
    filter(cases>0)
  
  fit <- glm(cbind(obs, cases - obs) ~  gender + wd + group,
               family = "binomial", data = dat,
             contrasts = list(wd = contr.sum, gender = contr.sum))
    
  the_names <- c("(Intercept)", paste0("group", fit$xlevels$group[-1]))

  a <- sapply(the_names, function(n){
      as.numeric(str_detect(names(fit$coef), paste0(n,"|(Intercept)")))})
  se <- sqrt(diag(t(a)%*%vcov(fit)%*%a))
  estimate <- as.numeric(t(a) %*% fit$coef)
  
  
  tibble(
    group = factor(fit$xlevels$group, levels = manu_levels),
    rate = ilogit(estimate), 
    conf.low = ilogit(estimate-qt(0.975, fit$df.residual) * se),
    conf.high = ilogit(estimate+qt(0.975, fit$df.residual) * se))
}
 

fit_model <- function(tab,  days.between.knots = 21){
  
  df <- round(length(unique(tab$date))/days.between.knots)
  
  dat <- mutate(tab, wd = factor(wday(date, label = TRUE))) %>%
    mutate(x = as.numeric(date)-min(as.numeric(date))) %>%
    filter(poblacion > 0)
  
  glm(obs ~ gender + wd + ageRange * ns(x, df = df) + group, 
             offset = log(poblacion), family = "quasipoisson", 
             contrasts = list(wd = contr.sum, gender = contr.sum),
             data = dat)
}

get_age <- function(x) as.numeric(str_extract(x, "^\\d+"))

to_english <-  function(x){
  replace_na(x, "") %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u") %>%
    str_replace_all("ü", "u") %>%
    str_replace_all("ñ", "n") %>%
    str_remove_all("\\s+") 
}

fix_municipio <- function(x, min.dist = 2, 
                          min.score = 1/3, 
                          min.score.barrio = 2/9,
                          first.letter.match = TRUE,
                          return.dist = FALSE){
  
  
  municipios <- c("Adjuntas", "Aguada", "Aguadilla", "Aguas Buenas", "Aibonito", "Añasco",
                  "Arecibo", "Arroyo", "Barceloneta", "Barranquitas", "Bayamón", "Cabo Rojo", 
                  "Caguas", "Camuy", "Canóvanas", "Carolina", "Cataño", "Cayey", "Ceiba",
                  "Ciales", "Cidra", "Coamo", "Comerío", "Corozal", "Culebra", "Dorado", 
                  "Fajardo", "Florida", "Guánica", "Guayama", "Guayanilla", "Guaynabo", 
                  "Gurabo", "Hatillo", "Hormigueros", "Humacao", "Isabela", "Jayuya", 
                  "Juana Díaz", "Juncos", "Lajas", "Lares", "Las Marías", "Las Piedras", 
                  "Loíza", "Luquillo", "Manatí", "Maricao", "Maunabo", "Mayagüez", "Moca", 
                  "Morovis", "Naguabo", "Naranjito", "Orocovis", "Patillas", "Peñuelas", 
                  "Ponce", "Quebradillas", "Rincón", "Río Grande", "Sabana Grande", "Salinas", 
                  "San Germán", "San Juan", "San Lorenzo", "San Sebastián", "Santa Isabel", 
                  "Toa Alta", "Toa Baja", "Trujillo Alto", "Utuado", "Vega Alta", "Vega Baja", 
                  "Vieques", "Villalba", "Yabucoa", "Yauco")
  
  x <- as.factor(x)
  query <- levels(x)
  ans <- as.character(rep(NA, length(query)))
  barrio <- as.character(rep(NA, length(query)))
  the_dist <- rep(NA, length(ans))
  the_score <- rep(NA, length(ans)) 
  
  if(all(query %in% municipios)){ # if all map, we are done
    ans <- query
    barrio <- as.character(rep(NA, length(query)))
  } else{ ## if not, we will convert to lower case, remvoe accents and non-characters
    map_mun <- data.frame(id = to_english(municipios), municipio = municipios)
    
    query <- query %>%
      to_english() %>%
      str_remove_all("[^a-zA-Z]+") 
    
    ## remove commone errors
    query[query=="puertorico" | query == "unknown" | query == "pr"] <- ""
    
    ## we will store answers in ans and if matching barrios, store that in barrio
    ind1 <- query %in% map_mun$id
    ans[ind1] <- map_mun$municipio[match(query[ind1], map_mun$id)]
    
    ## if not all matched keep going
    if(!all(ind1)){
      ## read-in barrios and keep only the unique ones
      ## if there is ambiguity we do not assing a municipio
      
      url <- "https://raw.githubusercontent.com/rafalab/pr-covid/master/coalicion/fix-municipio/data/pr-barrios.csv"
      barrios <- read_csv(url,
                          col_types = cols(municipio = col_character(),
                                           barrio = col_character())) %>%
        group_by(barrio) %>% ##remove duplicates
        mutate(n=n()) %>% 
        filter(n==1) %>%
        select(-n) %>% ## add commonly used names by hand
        bind_rows(data.frame(
          barrio = c("Río Piedras", "Hato Rey", "Barrio Obrero", "Bo Obrero", "Condado", "Miramar", 
                     "Caparra Heights","College Park","Villa Palmeras", 
                     "Fort Buchanan", "Caparra",
                     "Mercedita", 
                     "Saint Just", "Trujillo", "Tru Alto",
                     "Levittown", 
                     "Bajadero", 
                     "Ramey", "Aquadilla",
                     "Palmer",
                     "Isla Verde", "Urb Country Club",
                     "La Plata",
                     "Villa Rica", "Rio Hondo",
                     "Palmas del Mar",
                     "Cieba", "Roosvelt Roads",
                     "Campanilla",
                     "Caomo",
                     "Yuaco",
                     "Lioza",
                     "Tao Baja",
                     "Tao Alta",
                     "A Buenas"),
          municipio = c("sanjuan","sanjuan","sanjuan","sanjuan","sanjuan", "sanjuan",
                        "sanjuan","sanjuan","sanjuan", 
                        "guaynabo","guaynabo",
                        "ponce", 
                        "trujilloalto", "trujilloalto","trujilloalto",
                        "catano", 
                        "arecibo",
                        "aguadilla", "aguadilla",
                        "riogrande",
                        "carolina", "carolina",
                        "aibonito",
                        "bayamon", "bayamon",
                        "humacao",
                        "ceiba", "ceiba",
                        "toabaja",
                        "coamo",
                        "yauco",
                        "loiza",
                        "toabaja",
                        "toalta",
                        "aguasbuenas"))) %>%
        mutate(original_barrio = barrio) %>%
        mutate(barrio=to_english(barrio), municipio = to_english(municipio)) %>%
        rename(id = municipio)%>%
        left_join(map_mun, by = "id")
      
      ## fix one barrio name
      barrios$barrio[barrios$barrio == "barriopueblo(isabelii)"] <- "isabellii"
      
      ## use tmp to match barrios or mispellings in query to barrios in our table
      tmp <- data.frame(barrio = query[!ind1]) %>% left_join(barrios, by = "barrio") 
      ans[!ind1] <- tmp$municipio
      barrio[!ind1] <- tmp$original_barrio
      
      ## keep those not matched to search and see if municioio a subset like sanjuan in sanjuanpr 
      ind <- which(is.na(ans))
      if(length(ind)>0){
        
        for(i in 1:nrow(map_mun)){
          ind2 <- str_which(query[ind], map_mun$id[i])
          if(length(ind2)>0) ans[ind[ind2]] <- map_mun$municipio[i]
        }
        
        
        ##for those not matched and find fuzzy match with municipios
        ind <- which(is.na(ans))
        
        if(length(ind)>0){  
          
          ## distance to municipio
          d_mun <- stringdistmatrix(query[ind], map_mun$id, method = "lv")
          ind_mun <- apply(d_mun, 1, which.min)
          min_d_mun <- apply(d_mun, 1, min) 
          if(first.letter.match){ ##require first letter matches
            min_d_mun[str_sub(query[ind],1,1) != str_sub(map_mun$id[ind_mun],1,1)] <- Inf
          }
          score_mun <- min_d_mun / nchar(query[ind])
          
          ##if criteria met, keep it
          keep <- min_d_mun <= min.dist & score_mun <= min.score 
          
          if(length(keep)>0){
            ans[ind][keep] <-map_mun$municipio[ind_mun][keep]
            the_dist[ind][keep] <- min_d_mun[keep]
            the_score[ind][keep] <- score_mun[keep]
          }
          ##for those not matched check for fuzzy match with barrio
          ind <- which(is.na(ans))
          
          if(length(ind)>0){
            #distance to barrio
            d_bar <- stringdistmatrix(query[ind], barrios$barrio, method = "lv")
            ind_bar <- apply(d_bar, 1, which.min)
            min_d_bar <- apply(d_bar, 1, min) 
            if(first.letter.match){
              min_d_bar[str_sub(query[ind], 1, 1) != str_sub(barrios$barrio[ind_bar], 1, 1)] <- Inf
            }
            score_bar <- min_d_bar / nchar(query[ind])
            
            keep <- min_d_bar <= min.dist & score_bar <= min.score.barrio
            if(length(keep)>0){
              ans[ind][keep] <- barrios$municipio[ind_bar][keep]
              barrio[ind][keep] <- barrios$original_barrio[ind_bar][keep]
              the_dist[ind][keep] <- min_d_bar[keep]
              the_score[ind][keep] <- score_bar[keep]
            }
            
            ind <- which(is.na(ans))
            
            if(length(ind)>0){
              ##check if barrio included
              for(i in 1:nrow(barrios)){
                ind2 <- str_which(query[ind], barrios$barrio[i])
                if(length(ind2)>0){
                  ans[ind[ind2]] <- barrios$municipio[i]
                  barrio[ind[ind2]] <- barrios$original_barrio[i]
                }
              }
            }
          }
        }
      }
    }
  }  
  look_up <- data.frame(original = levels(x),
                        predicted = ans,
                        barrio.match = barrio,
                        dist = the_dist,
                        score = the_score)
  
  ret <- data.frame(original = as.character(x)) %>% 
    left_join(look_up, by = "original")
  
  if(return.dist){
    return(ret)
  } else{
    return(ret$predicted)
  }  
}

simplify_proveedor <- function(x, col_name = "proveedor") {
  x %>%
    mutate("{col_name}" := case_when(str_detect(proveedor, "WALGREENS") ~ "Walgreens",
                                     str_detect(proveedor, "WALMART|SAM'S") ~ "Walmart/Sams",
                                     str_detect(proveedor, "CVS") ~ "CVS",
                                     str_detect(proveedor, "VOCES") ~ "Voces",
                                     str_detect(proveedor, "PONCE HEALTH SCIENCE UNIV|PONCE HLTH SCI UNIV HUB") ~ "Ponce Health",
                                     str_detect(proveedor, "PR NATIONAL GUARD") ~ "Guardia Nacional",
                                     str_detect(proveedor, "CDVA") ~ "CDVA",           
                                     str_detect(proveedor, "COLEGIO MEDICOS CIRUJANOS") ~ "Colegio de Médicos", #Victor y su pandilla",
                                     str_detect(proveedor, "DESARROLLO SALUDN") ~ "Desarrollo de la Salud",
                                     str_detect(proveedor, "AUXILIO MUTUO") ~ "Auxilio Mutuo",
                                     str_detect(proveedor, "CORRECCIONAL") ~ "Correccional",
                                     str_detect(proveedor, "FARMACIA|FARMACIAS|PHARMACY") ~ "Otras farmacias",
                                     str_detect(proveedor, "HOSP|HOSPTIAL") ~ "Otros",
                                     str_detect(proveedor, "MEDICAL SCIENCE CAMPUS UPR") ~ "Ciencias Médcias", 
                                     str_detect(proveedor, "UNIVERSIDAD|UPR|UNIV|COLEGIO") ~ "Otras Universidades",
                                     str_detect(proveedor, "CDT") ~ "CDTs",
                                     str_detect(proveedor, "DR ") ~ "Doctores",
                                     TRUE ~ "Otros"))
}

fit_onedose_waning_model <- function(tab, knots = NULL, alpha = 0.01, transf = function(x){ 1-(1/exp(-x)) }){
  
  if(is.null(knots)){
    #knots <- c(4, 8, 12, seq(28, 210, 28))
    knots <- c(7, seq(14, max(tab$day), 28))
    knots <- knots[knots< unique(tab$max.day)]
  }
  
  tab <- filter(tab, day <= max.day)
  
  fit <- glm(obs ~ ns(day, knots = knots), offset = log(exp), family = "poisson", data = tab)
  
  pred <- predict(fit, newdata = list(day=tab$day, exp = 1), se = TRUE)
  
  mutate(tab, 
         fit = transf(pred$fit),
         conf.low = transf(pred$fit + qnorm(1-alpha/2) * pred$se.fit),
         conf.high = transf(pred$fit - qnorm(1-alpha/2) * pred$se.fit))
}
