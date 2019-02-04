/* HW2 */

data surv_resp;
	infile 'C:\Users\Steven\Documents\MSA\Applications and Methods\DOE\exercises\HW2\orange_team_5.csv' dsd firstobs=2;
	input newID $ :10. origID $ :10. LONG	LAT	ages	race $	sex $	income $	location $	price $	experience $	other $	will_attend;
run;

proc glm data=surv_resp alpha=0.125;
	*Three class variables; 
	class price experience location other; 
	*Only main effects for now;
	model will_attend = price experience location other ; *this should eventually include distance to location as predictor;
	lsmeans location price experience other /cl adjust=tukey;
	contrast 'Location 1 vs. (location 3 and 5)' location 1 0 -.5 0 -0.5;  
	contrast 'Location 2 vs. (location 1, 3 and 5)' location -(1/3) 1 -(1/3) 0 -(1/3);  
	estimate 'Price 1 vs. Price 2' price 1 -1 0 0;  
run; 
quit; 

proc glm data=surv_resp alpha=0.125;
	/* Questions: 
		what alpha to use?
		adapt to genmod? or... adapt to logistic? TODO Carlos to ask Wheeler
		should we include distance from location as continuous predictor? TODO Steven
		should we turn convert price to a continuous predictor? TODO Carlos and Steven
		leave in both location (factor predictor) and distance to loc (continuous predictor)? 
	*/
	*Three class variables; 
	class price experience location other; 
	/*Interactions;
	price*location barely sig a=0.039
	price*experience not sig a>0.05
	*/
	model will_attend = price experience location other experience*location price*location; *this should eventually include distance to location as predictor;
	lsmeans location price experience other experience*location price*location /cl adjust=tukey;
	contrast 'Location 1 vs. (location 3 and 5)' location 1 0 -.5 0 -0.5;  
	contrast 'Location 2 vs. (location 1, 3 and 5)' location -(1/3) 1 -(1/3) 0 -(1/3);  
	estimate 'Price 1 vs. Price 2' price 1 -1 0 0; 
/* Preliminary findings:
	location 4 is unpopular (potentially nobody attending)
	location and experience have a sig interaction
	loc 3 and exp 2 seemed to have the best results.
	everybody loves more 'other' attractions
	response to price 1 and price 2 are barely different (0.114 vs 0.119 response rates)
*/

run; 
quit; 

/* with proc logistic */
proc logistic data=surv_resp;
	class price(ref="1") experience(ref="1") location(ref="1") other(ref="1") / param=ref;
	model will_attend(event='1') = price experience location other;
	/* output predicted probabilities */
	output out=predprobs p=phat;
run;


/* with interactions */
