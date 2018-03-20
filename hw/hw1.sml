val mon_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
val mon_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

(* test 1 *)
fun is_older (f: int*int*int, s: int*int*int) =
    if #1 f = #1 s
    then if #2 f = #2 s
	 then #3 f < #3 s
	 else #2 f < #2 s
    else #1 f < #1 s

(* test 2 *)
fun number_in_month (xs: (int*int*int) list, m: int) =
    if null xs
    then 0
    else if #2 (hd xs) = m
    then 1 + number_in_month (tl xs, m)
    else number_in_month (tl xs, m)

(* test 3 *)
fun number_in_months (xs: (int*int*int) list, ms: int list) =
    if null ms
    then 0
    else number_in_month (xs, hd ms) + number_in_months (xs, tl ms)

(* test 4 *)
fun dates_in_month (xs: (int*int*int) list, m: int) =
    if null xs
    then []
    else if #2 (hd xs) = m
    then hd xs :: dates_in_month (tl xs, m)
    else dates_in_month (tl xs, m)

(* test 5 *)
fun dates_in_months (xs: (int*int*int) list, ms: int list ) =
    if null ms
    then []
    else dates_in_month (xs, hd ms) @ dates_in_months (xs, tl ms)
						       
(* test 6 *)
fun get_nth (xs: string list, n: int ) =
    if n = 1
    then hd xs
    else get_nth (tl xs, n - 1)

(* test 7 *)
fun date_to_string (d: int*int*int) =
    let
	val mon_name = get_nth (mon_names, #2 d)
	val day_name = Int.toString (#3 d)
	val year_name = Int.toString (#1 d)
    in
	mon_name ^ " " ^ day_name ^ ", " ^ year_name
    end
	
(* test 8 *)
fun number_before_reaching_sum (sum: int, xs: int list ) =
    if sum <= hd xs
    then 0
    else 1 + number_before_reaching_sum (sum - hd xs, tl xs)

(* test 9 *)
fun what_month (day: int) =
   (number_before_reaching_sum (day, mon_days)) + 1

(* test 10 *)
fun month_range (x: int, y: int) =
    if x > y
    then []
    else (what_month x) :: month_range (x + 1, y)

(* test 11 *)
fun oldest (xs: (int*int*int) list) =
    if null xs
    then NONE
    else
	let val tl_oldest = oldest (tl xs)
	in
	    if isSome tl_oldest andalso is_older (valOf tl_oldest, hd xs)
	    then tl_oldest
	    else SOME (hd xs)
	end

(* helper function to remove duplicate item in a list *)
fun no_repeat (xs: int list) =
    let
	fun remove_item (xs: int list, x: int) =
	    if null xs
	    then []
	    else if x = hd xs
	    then remove_item (tl xs, x)
	    else hd xs :: remove_item (tl xs, x)
    in
	if null xs
	then []
	else hd xs :: no_repeat (remove_item (tl xs, hd xs))
    end			  
	    	    
fun number_in_months_challenge (xs: (int*int*int) list, ms: int list) =
    number_in_months (xs, no_repeat ms)
		     
fun dates_in_months_challenge (xs: (int*int*int) list, ms: int list) =
    dates_in_months (xs, no_repeat ms)

fun get_nth_all (xs: 'a list, n) =
    if n = 1
    then hd xs
    else get_nth_all (tl xs, n - 1)

fun reasonable_date (x: int*int*int) =
    let
	fun reasonable_year (y: int) = y > 0
	fun reasonable_mon (m: int) = m > 0 andalso m <=12
	fun is_leap_year (y: int) =
	    y mod 400 = 0 orelse (y mod 4 = 0 andalso y mod 100 > 0)
	fun reasonable_day (d: int, m: int, y: int) =
	    let
		val mon_day = if is_leap_year y andalso m = 2
			      then 29
			      else get_nth_all (mon_days, m)
	    in
		d > 0 andalso d <= mon_day
	    end
    in
	reasonable_year (#1 x) andalso reasonable_mon (#2 x) andalso reasonable_day (#3 x, #2 x, #1 x)
    end
	
	    

			     
		     
			    
					  

					  
