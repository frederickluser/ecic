##' @title Estimate a changes-in-changes model with multiple periods and cohorts
##' 
##' @description Calculates a changes-in-changes model as in Athey and Imbens (2006) 
##' for multiple periods and cohorts.
##' 
##' @param yvar Dependent variable.
##' @param gvar Group variable. Can be either a string (e.g., "first_treated") 
##' or an expression (e.g., first_treated). In a staggered treatment setting, 
##' the group variable typically denotes treatment cohort.
##' @param tvar Time variable. Can be a string (e.g., "year") or an expression
##' (e.g., year).
##' @param ivar Individual Index variable. Can be a string (e.g., "country") or an 
##' expression (e.g., country). Only needed to check cohort sizes.
##' @param dat The data set.
##' @param myProbs Quantiles that the quantile treatment effects should be calculated for.
##' @param nMin Minimum observations per groups. Small groups are deleted.
##' @param boot Bootstrap. Resampling is done over the entire data set ("normal"), 
##' but might be weighted by period-cohort size ("weighted"). If you do not want
##' to calculate standard error, set boot = "no".
##' @param nReps Number of bootstrap replications.
##' @param weight_n0 Weight for the aggregation of the CDFs in the control group. 
##'  `n1` uses cohort sizes (Alternative: `n0`).
##' @param weight_n1 Weight for the aggregation of the CDFs in the treatment group. 
##' `n1` uses cohort sizes (Alternative: `n0`).
##' @param quant_algo Quantile algorithm (see Wikipedia for definitions).
##' @param es Event Study (Logical). If TRUE, a quantile treatment effect is estimated 
##' for each event-period.
##' @param n_digits Rounding the dependent variable before aggregating the empirical CDFs 
##' reduces the size of the imputation grid. This can significantly reduce the amount 
##' of RAM used in large data sets and improve running time, while reducing 
##' precision (Use with caution).
##' @param periods_es Periods of the event study.
##' @param save_to_temp Logical. If TRUE, results are temporarily saved. This reduces the
##' RAM needed, but increases running time.
##' @param progress_bar Whether progress bar should be printed (select "void" for no
##' progress bar or "cli" for another type of bar).
##' @param nCores Number of cores used. If set > 1, bootstrapping will run in parallel.
##' @return An `ecic` object.
##' @references 
##' Athey, Susan and Guido W. Imbens (2006). \cite{Identification and Inference in 
##' Nonlinear Difference-in-Differences Models}. 
##' \doi{10.1111/j.1468-0262.2006.00668.x}
##' @examples 
##' # Example 1. Using the small mpdta data in the did package
##' data(dat, package = "ecic")
##' dat = dat[dat$first.treat <= 1983 & dat$countyreal <= 1000,] # small data for fast run
##' 
##' mod_res = 
##'   summary(
##'   ecic(
##'     yvar  = lemp,         # dependent variable
##'     gvar  = first.treat,  # group indicator
##'     tvar  = year,         # time indicator
##'     ivar  = countyreal,   # unit ID
##'     dat   = dat,          # dataset
##'     boot  = "normal",     # bootstrap proceduce ("no", "normal", or "weighted")
##'     nReps = 3             # number of bootstrap runs
##'     )
##'     )
##' 
##' # Basic Plot
##' ecic_plot(mod_res)
##' 
##' \donttest{
##' # Example 2. Load some larger sample data
##' data(dat, package = "ecic")
##' 
##' # Estimate a basic model with the package's sample data
##' mod_res =
##'   summary(
##'   ecic(
##'     yvar  = lemp,         # dependent variable
##'     gvar  = first.treat,  # group indicator
##'     tvar  = year,         # time indicator
##'     ivar  = countyreal,   # unit ID
##'     dat   = dat,          # dataset
##'     boot  = "weighted",   # bootstrap proceduce ("no", "normal", or "weighted")
##'     nReps = 20            # number of bootstrap runs
##'   )
##'   )
##'   
##' # Basic Plot
##' ecic_plot(mod_res)
##' 
##' # Example 3. An Event-Study Example
##' mod_res =
##'   summary(
##'   ecic(
##'     es    = TRUE,         # aggregate for every event period
##'     yvar  = lemp,         # dependent variable
##'     gvar  = first.treat,  # group indicator
##'     tvar  = year,         # time indicator
##'     ivar  = countyreal,   # unit ID
##'     dat   = dat,          # dataset
##'     boot  = "weighted",   # bootstrap proceduce ("no", "normal", or "weighted")
##'     nReps = 20            # number of bootstrap runs
##'   )
##'   )
##'   
##' # Plots
##' ecic_plot(mod_res) # aggregated in one plot
##' ecic_plot(mod_res, es_type = "for_quantiles") # individually for every quantile
##' ecic_plot(mod_res, es_type = "for_periods")   # individually for every period
##' }
##' @importFrom stats aggregate quantile sd
##' @import future
##' @import furrr
##' @import progress
##' @export
ecic = function(
                yvar = NULL, 
                gvar = NULL, 
                tvar = NULL, 
                ivar = NULL, 
                dat  = NULL, 
                myProbs = seq(.1, .9, .1),
                nMin  = 40, 
                boot  = c("weighted", "normal", "no"),
                nReps = 10,
                weight_n0 = c("n1", "n0"),
                weight_n1 = c("n1", "n0"),
                quant_algo = 1, 
                es = FALSE, 
                n_digits = NULL,
                periods_es = NULL, 
                save_to_temp = FALSE, 
                progress_bar = c("progress", "void", "cli"),
                nCores = 1
)
{
  #-----------------------------------------------------------------------------
  # Check Input Arguments
  boot            = match.arg(boot)
  weight_n0       = match.arg(weight_n0)
  weight_n1       = match.arg(weight_n1)
  treat           = NULL
  progress_bar    = match.arg(progress_bar)
  
  if (is.null(dat)) stop("A non-NULL `dat` argument is required.")
  
  # Prepare String Inputs for Variables
  nl = as.list(seq_along(dat))
  names(nl) = names(dat)
  yvar = eval(substitute(yvar), nl, parent.frame())
  if (is.numeric(yvar)) yvar = names(dat)[yvar]
  tvar = eval(substitute(tvar), nl, parent.frame())
  if (is.numeric(tvar)) tvar = names(dat)[tvar]
  gvar = eval(substitute(gvar), nl, parent.frame())
  if (is.numeric(gvar)) gvar = names(dat)[gvar]
  ivar = eval(substitute(ivar), nl, parent.frame())
  if (is.numeric(ivar)) ivar = names(dat)[ivar]
  
  # Check Validity of inputs
  if (is.null(gvar))              stop("A non-NULL `gvar` argument is required.")
  if (is.null(tvar))              stop("A non-NULL `tvar` argument is required.")
  if (is.null(ivar))              stop("A non-NULL `ivar` argument is required.")
  if (is.null(yvar))              stop("A non-NULL `yvar` argument is required.")
  if (!tvar %in% names(dat))      stop(paste0("`", tvar, "` is not a variable in the data set."))
  if (!gvar %in% names(dat))      stop(paste0("`", gvar, "` is not a variable in the data set."))
  if (!ivar %in% names(dat))      stop(paste0("`", ivar, "` is not a variable in the data set."))
  if (!yvar %in% names(dat))      stop(paste0("`", yvar, "` is not a variable in the data set."))
  if (!is.logical(es))            stop("`es` must be logical.")
  if (!is.logical(save_to_temp))  stop("`save_to_temp` must be logical.")
  if (!quant_algo %in% 1:9)       stop("Invalid quantile algorithm.")
  if (!is.numeric(nMin))          stop("`nMin` must be numerical.")
  if (!is.numeric(nReps))         stop("`nReps` must be numerical.")
  if (!is.numeric(nCores))        stop("`nCores` must be numerical.")
  if (!is.numeric(myProbs))       stop("`myProbs` must be numerical.")
  if (!is.null(n_digits)) if (!is.numeric(n_digits)) stop("`n_digits` must be numerical.")
  
  # Check bootstrap
  if (boot == "no") boot = NULL
  nReps = as.integer(nReps)
  if (! nReps > 0) stop("nReps must be a positive integer.")
  if (is.null(boot) & nReps != 1){
    warning("nReps > 1 but bootstrap is deactivated. nReps is set to 1.")
    nReps = 1
  }
  
  # for saving to disk
  if (save_to_temp == TRUE) {
    temp_dir   = tempdir() # set same tempdir() across cores in parallel
    random_num = sample(1e5:5e6, 1) # add a big random number to file names
    temp_files = list.files( # list all files in temp_dir that have the number
      temp_dir, pattern = as.character(random_num)
      )
    while (length(temp_files)!= 0) { # check that no temp-files contain the number
      random_num = sample(1e5:5e6, 1)
      temp_files = list.files( # re-check 
        temp_dir, pattern = as.character(random_num)
      )
    }
  }

  #-----------------------------------------------------------------------------
  # Prepare the data
  dat = subset(dat, get(gvar) %in% unique(dat[[tvar]])) # exclude never-treated units
  dat = dat[,c(yvar, gvar, tvar, ivar)] # keep relevant vars (lower RAM in parallel)
  
  # star tvar and gvar at zero
  first_period = min(dat[[tvar]], na.rm = TRUE)
  last_cohort  = max(dat[[gvar]], na.rm = TRUE) - first_period

  dat[[tvar]]  = dat[[tvar]]-(first_period-1) 
  dat[[gvar]]  = dat[[gvar]]-(first_period-1) 
  
  # calculate group / cohort sizes
  group_sizes = stats::aggregate(stats::as.formula(paste(yvar, " ~ ", gvar)), 
                                 data = dat[!duplicated(dat[, ivar]), ], FUN = length)
  names(group_sizes)[names(group_sizes) == yvar] = "N"
  
  # check number of too small groups / cohorts and exclude them
  diffGroup = sum(group_sizes$N < nMin)
  if (diffGroup != 0)  {
    skip_cohorts = group_sizes[group_sizes$N < nMin, gvar] # find the small cohorts
    dat = dat[!dat[[gvar]] %in% skip_cohorts, ] # delete too small cohorts
    warning(paste0("You have ", diffGroup, " (", round(100 * diffGroup / nrow(group_sizes)), 
                   "%) too small groups (less than ", nMin, " observations). They were dropped."))
  }
  if (diffGroup == nrow(group_sizes)) stop("All treated cohorts are too small (you can adjust `nMin` with caution).")
  
  # identify the treatment cohorts
  list_periods = sort(unique(dat[[tvar]]))
  list_cohorts = sort(unique(dat[[gvar]]))
  
  qte_cohort   = list_cohorts[-length(list_cohorts)] # omit last g (no comparison group)
  qte_cohort   = qte_cohort[qte_cohort != 1] # omit first g (no pre-period)
  if(length(qte_cohort) == 0) stop("Not enough cohorts / groups in the data set!")
  
  # Print settings
  if (is.null(boot)) {
    message(paste0("Estimating a changes-in-changes model for ", length(unique(dat[[gvar]])) - 1, 
                   " groups and ", nrow(dat), " observations. No standard errors computed."))
  } else {
    message(paste0("Estimating a changes-in-changes model for ", length(unique(dat[[gvar]])) - 1, 
                   " groups and ", nrow(dat), " observations with ", nReps, " (", boot, ") bootstrap replications."))
  }
  
  # max event time QTEs can be calculated for
  periods_es = length(
    list_periods[which(list_periods == qte_cohort[1]):(length(list_periods)-1)]
  ) - 1
  
  ################################################################################
  # Calculate all 2-by-2 CIC combinations

  future::plan(future::multisession, workers = nCores, gc = TRUE)
  progressr::handlers(progress_bar) # choose whether to print output
  
  # Calculate bootstrap for all possible 2x2 combinations

  my_fcn <- function(xs) {
    p <- progressr::progressor(along = xs)
    return(
    furrr::future_map(xs, function(j) {

    n1 = n0 = vector()
    y1 = y0 = name_runs = vector("list")

    # resampling for bootstrapping
      if ( !is.null(boot) ) {
        if (boot == "weighted") {
          cell_sizes = stats::aggregate(stats::as.formula(paste(yvar, " ~ ", gvar, "+", tvar)), 
                                        data = dat, FUN = length) # count cohort-period cells
          names(cell_sizes)[names(cell_sizes) == yvar] = "N"
          dat = merge(dat, cell_sizes, all.x = TRUE)
          dat = dat[!is.na(dat$N) & dat$N > 0,] # additional check (weights have to be positive)
          data_boot = dat[sample(1:nrow(dat), size = nrow(dat), replace = TRUE, prob = dat$N), ]
        } else if (boot == "normal") {
          data_boot = dat[sample(1:nrow(dat), size = nrow(dat), replace = TRUE), ]
        }
      } else {
        data_boot = dat
      }
    
    # 1) treated cohorts ----
    i = 1 # start the counter for the inner loop

    for (qteCohort in qte_cohort) {

      # 2) comparison groups ----
      pre_cohort = list_cohorts[(which(list_cohorts == qteCohort)+1):length(list_cohorts)]
      #pre_cohort = pre_cohort[pre_cohort - qteCohort >= 4]
      
      for (preCohort in pre_cohort) {
      
        # 3) post-treatment periods ----
        qte_year = list_periods[which(list_periods == qteCohort):(which(list_periods == last_cohort))]
        qte_year = qte_year[qte_year < preCohort] # control has to be untreated
        
        # for event study: only calculate periods you're interested in
        if (es == TRUE) qte_year = qte_year[qte_year - qteCohort <= periods_es]
        
        for (qteYear in qte_year) {
          
          # 4) pre-treatment comparison periods ----
          pre_year = list_periods[list_periods < qteCohort] # both have to  be untreated in this period
          
          for (preYear in pre_year) {
            
            # prepare the data for this loop
            data_loop = subset(data_boot, get(gvar) %in% c(qteCohort, preCohort) & get(tvar) %in% c(qteYear, preYear))
            data_loop$treat = ifelse(data_loop[[gvar]] == qteCohort, 1, 0) # add a treatment dummy
            
            # catch empty groups
            nrow_treat = nrow(subset(data_loop, treat == 1))
            nrow_control = nrow(subset(data_loop, treat == 0))
            
            # save the combinations (cohort / year) of this run
            name_runs[[i]] = data.frame(i, qteCohort, preCohort, qteYear, preYear)
            
            # save the group sizes for the weighting
            n1[i] = nrow_treat
            n0[i] = nrow_control

            #-------------------------------------------------------------------
            # catch empty groups
            if (nrow_treat < nMin){
              warning(paste0("Skipped a period-cohort group in bootstrap run ", j, " (too small treatment group)"))
              next
            }
            if (nrow_control < nMin){
              warning(paste0("Skipped a period-cohort group in bootstrap run ", j, " (too small treatment group)"))
              next
            }            
            
            #-------------------------------------------------------------------
            # Y(1)
            y1[[i]] = stats::ecdf(subset(data_loop, treat == 1 & get(tvar) == qteYear)[[yvar]])
            
            # Y(0): Construct the counterfactual
            y0[[i]] = stats::ecdf(
              stats::quantile(subset(data_loop, treat == 0 & get(tvar) == qteYear)[[yvar]],
                     probs = stats::ecdf(subset(data_loop, treat == 0 & get(tvar) == preYear)[[yvar]]) (
                       subset(data_loop, treat == 1 & get(tvar) == preYear)[[yvar]]
                     ), type = quant_algo
                     )
              )
            #-------------------------------------------------------------------
            i = i + 1 # update counter
          }
        }
      }
    }
    ############################################################################
    # Aggregate Results for 1 bootstrap run
    
    # collapse
    name_runs = cbind(do.call(rbind, name_runs), n1, n0) # specifications of the runs

    # check that not all runs were empty 
    if (nrow(name_runs[name_runs$n1 >= nMin & name_runs$n0 >= nMin,]) == 0 ) {
      stop("There was no non-empty cohort-group combination (all combinations were too small, check \"nMin\").", call. = F)
    }
    
    # prepare imputation values
    if (!is.null(n_digits)) {
      values_to_impute = sort(unique( round( dat[[yvar]], digits = n_digits)) )
    } else {
      values_to_impute = sort(unique( dat[[yvar]] ))
    }  
    
    #-----------------------------------------------------------------------------
    # impute Y(0)
    y0_imp = lapply(y0, function(ecdf_temp) {
      ecdf_temp(values_to_impute)
    })
    
    # bind rows into a matrix
    y0_imp = as.matrix(do.call(rbind, y0_imp))
    
    # aggregate all 2x2-Y(0) (weighted by cohort size)
    test0 = data.frame(values_to_impute, value = colSums(y0_imp * (n1/sum(n1))) )
    rm(y0_imp)

    # get the quantiles of interest
    y0_quant = do.call(rbind, lapply(myProbs, function(r) {
      test0$diff = test0$value - r
      test0 = subset(test0, diff >= 0)
      test0[which.min(test0$diff),]
    }))
    y0_quant = y0_quant[, !(colnames(y0_quant) == "diff")]
    
    rm(test0)
    gc()
    
    #---------------------------------------------------------------------------
    if (es == FALSE) { # average QTE
      
      # impute Y(1)
      y1_imp = lapply(y1, function(ecdf_temp) {
        ecdf_temp(values_to_impute)
      })
      
      # bind rows into a matrix
      y1_imp = as.matrix(do.call(rbind, y1_imp))
      
      # aggregate all 2x2-Y(0) (weighted by cohort size)
      test1 = data.frame(values_to_impute, value = colSums(y1_imp * (n1/sum(n1))) )
      rm(y1_imp)

      # get the quantiles of interest
      y1_quant = do.call(rbind, lapply(myProbs, function(r) {
        test1$diff = test1$value - r
        test1 = subset(test1, diff >= 0)
        test1[which.min(test1$diff),]
      }))
      y1_quant = y1_quant[, !(colnames(y1_quant) == "diff")]

      rm(test1)
      gc()
      
      # compute the QTE
      myQuant = data.frame(
        perc = myProbs,
        values = y1_quant$values_to_impute - y0_quant$values_to_impute # from CDFs
      )
      
      #-------------------------------------------------------------------------
    } else { # "event study"
      
      # impute Y(1)
      y1_imp = lapply(y1, function(ecdf_temp) {
        ecdf_temp(values_to_impute)
      })

      # check event study settings
      name_runs$diff = name_runs$qteYear - name_runs$qteCohort
      max_es = max(name_runs[["diff"]])

      if (periods_es > max_es) {
        periods_es = max_es
        warning(paste0("Bootstrap run ", j, ": Only ", periods_es, 
                       " post-treatment periods can be calculated (plus contemporaneous)."))
      }

      myQuant = lapply(0:periods_es, function(e) { # time-after-treat

        weights_temp = n1[subset(name_runs, diff == e)[["i"]]] # weights for this event time

        y1_agg = colSums(
          as.matrix(do.call(rbind, y1_imp[subset(name_runs, diff == e)[["i"]]]
          )) * (weights_temp / sum(weights_temp)) )

        test1 = data.frame(values_to_impute, value = y1_agg)
        rm(y1_agg)

        y1_quant = do.call(rbind, lapply(myProbs, function(r) {
          test1$diff = test1$value - r
          test1 = subset(test1, diff >= 0)
          test1 = test1[which.min(test1$diff), ]
        }))
        y1_quant = y1_quant[, !(colnames(y1_quant) == "diff")]

        # compute the QTE
        quant_temp = data.frame(
          perc   = myProbs,
          values = y1_quant$values_to_impute - y0_quant$values_to_impute # from CDFs
        )
        return(quant_temp)
      })

      rm(y1_imp)
      gc()
    }
    
    #---------------------------------------------------------------------------
    # update progress bar
    p() 

    # save to disk (saver, but slower)
    if (save_to_temp == TRUE) {
      tmp_quant = tempfile(paste0(pattern = "myQuant_", random_num, "_", j, "_"), fileext = ".rds", tmpdir = temp_dir)
      tmp_name  = tempfile(paste0(pattern = "name_runs_", random_num, "_", j, "_"), fileext = ".rds", tmpdir = temp_dir)
    
      saveRDS(myQuant, file = tmp_quant)
      saveRDS(name_runs, file = tmp_name)
      return(j)
      
    } else { # just work in the RAM (output lost if crash and RAM may be too small)
      return(list(coefs = myQuant, name_runs = name_runs))
    } 
    
    },
  .options = furrr::furrr_options(seed = 123)
    )
  )
  }
  reg = progressr::with_progress(my_fcn(1:nReps))
  future::plan(future::sequential)
  
  ##############################################################################
  # post-loop: combine the outputs files 
  if(save_to_temp == TRUE) {
    list_files = list.files(path = temp_dir, pattern = paste0("myQuant_", random_num))

    reg = lapply(1:nReps, function(j){
      list(
        coefs     =   readRDS(paste0(temp_dir, "\\", list.files( 
          path = temp_dir, pattern = paste0("myQuant_", random_num, "_", j, "_")))),
        name_runs =   readRDS(paste0(temp_dir, "\\", list.files( 
          path = temp_dir, pattern = paste0("name_runs_", random_num, "_", j, "_"))))
      )
    })
  }

  # post-loop: Overload class and new attributes (for post-estimation) ----
  if(es == TRUE) {
    periods_es = max(lengths(lapply(reg, "[[", 1))-1) # substact contemporary
  } else {
    periods_es = NA
  }
  
  class(reg) = c("ecic")
  attr(reg, "ecic") = list(
    myProbs    = myProbs,
    es         = es,
    periods_es = periods_es
  )
  
  return(reg)
}
