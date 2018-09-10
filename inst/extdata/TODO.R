## TODO: desired interface

e14 <- nm("e14", type = "execute", based_on = e13)
build_run({
  e14 %>% update_parameters() %>%
    manual_edit("KEFF log-normal -> normal")
})

e14$cmd("qpsn -c auto -r 1000 -t 3000 -- execute run14.mod -dir=14")

e14 %>% run_nm
e14 %>% wait_for_finished %>% update_parameters %>% run_nm
e14 %>% process_output  ## uses wait_for_finished if needed

future({ ## asynchronous events
  #e14 %>% wait_for_finished - no longer needed
  e14 %>% covariance_result() %>% file.show()
  e14 %>% basic_diag1(type = 12)
  e14 %>% ind_diag1(type = 12, n = 20)  
})

## TODO: functions to be used in function templates:
##     - require_finished() - will wait if not finsihed
##     - require_processed() - will process if needed

## TODO: new convention run_ids start with letter

## TODO: Better database access

## TODO: Store SHA's to all inputs - check before running.
##   then eliminate interactive/non-interactive

## TODO: an easy way to copy-modify this code:
##  -- add ons

## TODO: need a modular way of referring to previous run

e14 <- nm("e14", type = "execute", based_on = e13)
e14 <- nm("e14", type = "execute", based_on = "e13") ## picks execute
e14 <- nm("e14", type = "execute", based_on = "ADVAN13.mod") ## code library

## would be easiest if i had all info in e13 e.g. cmd

## TODO: save nm objects and load them easily

## TODO: want a way to ctrl + alt + b without messing things up.

## TODO: make functions work on lists of nm objects
##   e.g. dsc$ofv <- ofv(dsc$m)
##        run_nm(dsc$m, batches = 10)  ## works like run_batch_list
