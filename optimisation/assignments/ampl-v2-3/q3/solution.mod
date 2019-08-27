set A;
set DAYS := {1..30};
param maxdemand{i in A};
param sellingprice{i in A};
param prodcost{i in A};
param prodquota{i in A};
param minbatch{i in A};

var is_prod_on_day{A, DAYS} binary;
var nprod_on_day {A, DAYS} integer;

max: 
  sum {a in A, day in DAYS} 
      is_prod_on_day[a, day] * nprod_on_day[a, day] * 
      (sellingprice[a] - prodcost[a]);

# total produce less than max demand
subject to max_demands_per_month {a in A}:
    (sum {day in DAYS} 
        is_prod_on_day[a, day] * nprod_on_day[a, day]) <= maxdemand[a];

# produce per day is less than quota
subject to max_quota_per_day {a in A, day in DAYS}: 
    is_prod_on_day[a, day] * nprod_on_day[a, day]  <= prodquota[a];

# Production can only be active 22 days of the month
subject to prod_only_active_on_22_days{a in A}: 
    sum {day in DAYS} is_prod_on_day[a, day] <= 22;

# Minimum production batch on days production happens
subject to min_prod_batch{a in A, day in DAYS}:
    is_prod_on_day[a, day] * nprod_on_day[a, day] + (1 - is_prod_on_day[a, day]) * 100000 >= minbatch[a]
