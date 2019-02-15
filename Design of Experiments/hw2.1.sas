/* HW2 */

data surv_resp;
	infile 'C:\Users\Steven\Documents\MSA\Applications and Methods\DOE\exercises\HW2\survey_data_with_new_var.csv' dsd firstobs=2;
	input newID $ :10. origID $ :10. LONG	LAT	ages	race $	sex $	income $	loc $	price $	expr $	other $	will_attend d_deg price_cont;
run;

proc means data=surv_resp;
class loc;
run;

proc glm data=surv_resp alpha=0.0125;
	/* Questions: 
		leave in both location (factor predictor) and distance to loc (continuous predictor)? 
	*/

	*Four class variables; 
	class price expr loc other; 
	/*Interactions;
	price*location barely sig a=0.039
	price*experience not sig a>0.05
	*/
	model will_attend = price expr loc other expr*loc price*loc; *this should eventually include distance to location as predictor;
	lsmeans loc price expr other expr*loc price*loc /cl adjust=tukey;
	contrast 'Location 1 vs. (location 3 and 5)' loc 1 0 -.5 0 -0.5;  
	contrast 'Location 2 vs. (location 1, 3 and 5)' loc -(1/3) 1 -(1/3) 0 -(1/3);  
	estimate 'Price 1 vs. Price 2' price 1 -1 0 0; 
/* Preliminary findings:
	location 4 is unpopular (potentially nobody attending)
	location and experience have a sig interaction
	loc 3 and expr 2 seemed to have the best results.
	everybody loves more 'other' attractions
	response to price 1 and price 2 are barely different (0.114 vs 0.119 response rates)
*/

run; 
quit; 



/* Repeat with continuous variables */
proc glm data=surv_resp alpha=0.0125;
	*Four class variables; 
	class price expr loc other; 
	*Only main effects for now;
	model will_attend = price price_cont expr loc d_deg other ; *this should eventually include distance to location as predictor;
	*lsmeans location price experience other /cl adjust=tukey;
	*contrast 'Location 1 vs. (location 3 and 5)' d_mi 1 0 -.5 0 -0.5;  
	*contrast 'Location 2 vs. (location 1, 3 and 5)' d_mi -(1/3) 1 -(1/3) 0 -(1/3);  
	*lsmeans price_cont experience d_deg other;
	*estimate 'Distance from Polled Location' d_deg;
	*estimate 'Price 1 vs. Price 2' price_cont;
run;
quit;


/* with proc logistic */
proc logistic data=surv_resp;
	class expr(ref="1") other(ref="1") price(ref='1') loc(ref='1')/ param=ref;
	model will_attend(event='1') = price_cont expr d_deg other;
	/* output predicted probabilities */
	output out=predprobs p=phat;
	footnote;
run;

/* with proc logistic & INTERACTIONS of expr*loc */
proc logistic data=surv_resp;
	class expr(ref="1") other(ref="1") price(ref='1') loc(ref='1')/ param=ref;
	model will_attend(event='1') = price_cont expr d_deg other expr*loc;
	/* output predicted probabilities */
	output out=predprobs p=phat;
	footnote;
	*experience 3 at location 2 looks like a significant positive interaction;
run;


/* genmond w/ NO interactions */
proc genmod data=surv_resp descending; 
	class loc(ref='1') price(ref='1') expr(ref='1') other(ref='1')/param=glm;
	model will_attend = expr price_cont other d_deg/ dist= binomial link= LOGIT ;
	
	*all estimates below assume that we want 'other #4' (arcade and mini-golf);

	*understanding effect of price;
	estimate "$15 (0.24 degrees away, exp=1)" intercept 1 price_cont 15 d_deg 0.24 expr 1 0 0 other 0 0 0 1;
	estimate "$17.5 (0.24 degrees away, exp=1)" intercept 1 price_cont 17.5 d_deg 0.24 expr 1 0 0 other 0 0 0 1;
	estimate "$20 (0.24 degrees away, exp=1)" intercept 1 price_cont 20 d_deg 0.24 expr 1 0 0 other 0 0 0 1;
	estimate "$22.5 (0.24 degrees away, exp=1)" intercept 1 price_cont 22.5 d_deg 0.24 expr 1 0 0 other 0 0 0 1;
	estimate "$25 (0.24 degrees away, exp=1)" intercept 1 price_cont 25 d_deg 0.24 expr 1 0 0 other 0 0 0 1;

	*understanding effect of experience;
	estimate "exp 1 (0.24 degrees away, price=$20)" intercept 1 price_cont 20 d_deg 0.24 expr 1 0 0 other 0 0 0 1;
	estimate "exp 2 (0.24 degrees away, price=$20)" intercept 1 price_cont 20 d_deg 0.24 expr 0 1 0 other 0 0 0 1;
	estimate "exp 3 (0.24 degrees away, price=$20)" intercept 1 price_cont 20 d_deg 0.24 expr 0 0 1 other 0 0 0 1;
	estimate "exp 1 (0.24 degrees away, price=$15)" intercept 1 d_deg 0.24 price_cont 15 expr 1 0 0 other 0 0 0 1;
	estimate "exp 2 (0.24 degrees away, price=$15)" intercept 1 d_deg 0.24 price_cont 15 expr 0 1 0 other 0 0 0 1;
	estimate "exp 3 (0.24 degrees away, price=$15)" intercept 1 d_deg 0.24 price_cont 15 expr 0 0 1 other 0 0 0 1;

	*understanding effect of distance;
	estimate "0.1 degrees away (price=$20, exp 2)" intercept 1 price_cont 20 d_deg 0.1 expr 0 1 0 other 0 0 0 1;
	estimate "0.2 degrees away (price=$20, exp 2)" intercept 1 price_cont 20 d_deg 0.2 expr 0 1 0 other 0 0 0 1;
	estimate "0.3 degrees away (price=$20, exp 2)" intercept 1 price_cont 20 d_deg 0.3 expr 0 1 0 other 0 0 0 1;
	estimate "0.4 degrees away (price=$20, exp 2)" intercept 1 price_cont 20 d_deg 0.4 expr 0 1 0 other 0 0 0 1;
	estimate "0.1 degrees away (price=$20, exp 3)"  intercept 1 d_deg 0.1 expr 0 0 1 other 0 0 0 1;
	estimate "0.2 degrees away (price=$20, exp 3)"  intercept 1 d_deg 0.2 expr 0 0 1 other 0 0 0 1;
	estimate "0.3 degrees away (price=$20, exp 3)"  intercept 1 d_deg 0.3 expr 0 0 1 other 0 0 0 1;
	estimate "0.4 degrees away (price=$20, exp 3)"  intercept 1 d_deg 0.4 expr 0 0 1 other 0 0 0 1;

	footnote 'All estimates above assume both arcade and mini-golf amenities.';

	run; 
quit;

/* genmond w/ INTERACTIONS of expr*loc */
proc genmod data=surv_resp descending; 
	class loc(ref='1') price(ref='1') expr(ref='1') other(ref='1')/param=glm;
	model will_attend = expr price_cont other d_deg expr*loc/ dist= binomial link= LOGIT ;
	
	lsmeans expr*loc /cl adjust=tukey;

	*all estimates below assume that we want 'other #4' (arcade and mini-golf);
	
	run; 
quit;
