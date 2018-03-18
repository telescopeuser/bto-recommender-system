(defmodule MAIN (export ?ALL))

;;**********
;;* CLIPS **
;;**********

; (deffunction debug (?message)
; 	(printout t "DEBUG: " ?message crlf)
; )

; (deffunction alert (?message)
; 	(printout t ?message crlf)
; )

; (deffunction prompt2 (?question ?allowed-values-display $?allowed-values)
; 	(printout t ?question crlf)
; 	(printout t ?allowed-values-display crlf)
; 	(bind ?answer (read))
; 	?answer
; )

; (deffunction final (?message)
; 	(printout t ?message crlf)
; )


;;*****************
;;* DEFFUNCTIONS **
;;*****************

(deffunction MAIN::ask-question (?question ?allowed-values-display $?allowed-values)
	(bind ?answer (prompt2 ?question ?allowed-values-display $?allowed-values))
;	(debug (str-cat "B-2= " (implode$ ?allowed-values) ", response: " ?answer))
	(if (lexemep ?answer) 
		then (bind ?answer (lowcase ?answer))
	)
	(while (not (member ?answer ?allowed-values)) do
		(bind ?answer (prompt2 ?question ?allowed-values-display $?allowed-values))
;		(debug (str-cat "B-2= " (implode$ ?allowed-values) ", response: " ?answer))
		(if (lexemep ?answer) 
			then (bind ?answer (lowcase ?answer))
		)
	)
	?answer
)

;;******************
;;* INITIAL STATE **
;;******************

(deftemplate MAIN::attribute
	(slot name)
	(slot value)
	(slot certainty (default 0.0)))					;;Default certainty factor

(deffacts MAIN::default-attribute-list
	(attribute (name number-of-rooms) (value 2-room))
	(attribute (name number-of-rooms) (value 3-room))
	(attribute (name number-of-rooms) (value 4-room))
	(attribute (name number-of-rooms) (value 5-room))
	(attribute (name number-of-rooms) (value 3-Gen))
	(attribute (name amenities) (value school))
	(attribute (name amenities) (value child-care))
	(attribute (name amenities) (value hawker))
	(attribute (name amenities) (value supermarket))
	(attribute (name amenities) (value mall))
	(attribute (name amenities) (value transport))
	(attribute (name type-of-flat) (value orientation_N-S))
	(attribute (name type-of-flat) (value orientation_E-W))
	(attribute (name type-of-flat) (value noWestSun))
	(attribute (name type-of-flat) (value high-level))
)

(defrule MAIN::start
	(declare (salience 10000))
	=>
	(set-fact-duplication TRUE)
	(focus QUESTIONS CHOOSE-BTO PRINT-RESULTS))		;;Set the sequence to fire the module

	
;;*********************
;;* INFERENCE ENGINE **
;;*********************
	
(defrule MAIN::combine-positive ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name ?rel) (value ?val) (certainty ?per1&:(>= ?per1 0)))
	?f2 <- (attribute (name ?rel) (value ?val) (certainty ?per2&:(>= ?per2 0)))
	(test (neq ?f1 ?f2))
	=>
	(retract ?f2)
	(modify ?f1 (certainty =(+ ?per1 (* ?per2 (- 1 ?per1))))))

(defrule MAIN::combine-negative ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name ?rel) (value ?val) (certainty ?per1&:(<= ?per1 0)))
	?f2 <- (attribute (name ?rel) (value ?val) (certainty ?per2&:(<= ?per2 0)))
	(test (neq ?f1 ?f2))
	=>
	(retract ?f2)
	(modify ?f1 (certainty =(+ ?per1 (* ?per2 (+ 1 ?per1))))))

(defrule MAIN::combine-positive-negative ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name ?rel) (value ?val) (certainty ?per1))
	?f2 <- (attribute (name ?rel) (value ?val) (certainty ?per2))
	(test (neq ?per1 ?per2))
	(test (< (* ?per1 ?per2) 0))
	=>
	(retract ?f2)
	(modify ?f1 (certainty =(/ (+ ?per1 ?per2) (- 1 (min (abs ?per1) (abs ?per2)))))))

(defrule MAIN::getGrant ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name average-income) (value ?val) (certainty ?per1))
	=>
	(retract ?f1)
	(switch ?val
		(case 1 then (assert (attribute (name grant) (value 80000) (certainty 1))))
		(case 2 then (assert (attribute (name grant) (value 75000) (certainty 2))))
		(case 3 then (assert (attribute (name grant) (value 70000) (certainty 3))))
		(case 4 then (assert (attribute (name grant) (value 65000) (certainty 4))))
		(case 5 then (assert (attribute (name grant) (value 60000) (certainty 5))))
		(case 6 then (assert (attribute (name grant) (value 55000) (certainty 6))))
		(case 7 then (assert (attribute (name grant) (value 50000) (certainty 7))))
		(case 8 then (assert (attribute (name grant) (value 45000) (certainty 8))))
		(case 9 then (assert (attribute (name grant) (value 40000) (certainty 9))))
		(case 10 then (assert (attribute (name grant) (value 35000) (certainty 10))))
		(case 11 then (assert (attribute (name grant) (value 30000) (certainty 11))))
		(case 12 then (assert (attribute (name grant) (value 25000) (certainty 12))))
		(case 13 then (assert (attribute (name grant) (value 20000) (certainty 13))))
		(case 14 then (assert (attribute (name grant) (value 15000) (certainty 14))))
		(case 15 then (assert (attribute (name grant) (value 10000) (certainty 15))))
		(case 16 then (assert (attribute (name grant) (value 5000) (certainty 16))))
	)
)
		
(defrule MAIN::getPriceTagR ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name grant) (value ?val1) (certainty ?option1))
	?f2 <- (attribute (name pSources) (value ?val2) (certainty ?option2))
	=>
	(retract ?f1 ?f2)
	(assert (attribute (name priceTagR) 
				(value yes)
				(certainty = (+ ?val1 (* 25 (* ?val2 (/ (* 12 (* 500 (+ ?option1 2))) 10)))))
			)
	)
)

(defrule MAIN::compareMAXsmall ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name priceTagR) (value ?val) (certainty ?price))
	(test (<= ?price 591000))
	=>
	(retract ?f1)
	(assert (attribute (name askSaving) (value yes) (certainty ?price))))

(defrule MAIN::compareMAXbig ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name priceTagR) (value ?val) (certainty ?price))
	(test (> ?price 591000))
	=>
	(retract ?f1)
	(assert (attribute (name askSaving) (value no) (certainty ?price)))
	(assert (attribute (name priceTag) (value ?price) (certainty 1.0)))
)
	
(defrule MAIN::getPriceTag ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name askSaving) (value ?val1) (certainty ?price))
	?f2 <- (attribute (name savingSources) (value ?val2) (certainty ?filler))
	=>
	(retract ?f1 ?f2)
	(assert (attribute (name priceTag) (value = (+ ?price (* ?val2 20000))) 
		(certainty 1.0))
	)
)
	
(defrule MAIN::getMaxPriceTagCase1 ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name priceTag) (value ?val) (certainty ?per1))
	(test (< ?val 150000))
	=>
	(retract ?f1)
	(assert (attribute (name maxPriceTagCase) (value 1) (certainty 1.0)))
	(assert (attribute (name priceOfUnit) (value ?val) (certainty 1.0))))

(defrule MAIN::getMaxPriceTagCase2 ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name priceTag) (value ?val) (certainty ?per1))
	(test (>= ?val 150000))
	(test (< ?val 250000))
	=>
	(retract ?f1)
	(assert (attribute (name maxPriceTagCase) (value 2) (certainty 1.0)))
	(assert (attribute (name priceOfUnit) (value ?val) (certainty 1.0))))

(defrule MAIN::getMaxPriceTagCase3 ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name priceTag) (value ?val) (certainty ?per1))
	(test (>= ?val 250000))
	(test (< ?val 350000))
	=>
	(retract ?f1)
	(assert (attribute (name maxPriceTagCase) (value 3) (certainty 1.0)))
	(assert (attribute (name priceOfUnit) (value ?val) (certainty 1.0))))

(defrule MAIN::getMaxPriceTagCase4 ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name priceTag) (value ?val) (certainty ?per1))
	(test (>= ?val 350000))
	(test (< ?val 450000))
	=>
	(retract ?f1)
	(assert (attribute (name maxPriceTagCase) (value 4) (certainty 1.0)))
	(assert (attribute (name priceOfUnit) (value ?val) (certainty 1.0))))
	
(defrule MAIN::getMaxPriceTagCase5 ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name priceTag) (value ?val) (certainty ?per1))
	(test (>= ?val 450000))
	(test (< ?val 550000))
	=>
	(retract ?f1)
	(assert (attribute (name maxPriceTagCase) (value 5) (certainty 1.0)))
	(assert (attribute (name priceOfUnit) (value ?val) (certainty 1.0))))
	
(defrule MAIN::getMaxPriceTagCase6 ""
	(declare (auto-focus TRUE))
	?f1 <- (attribute (name priceTag) (value ?val) (certainty ?per1))
	(test (>= ?val 550000))
	=>
	(retract ?f1)
	(assert (attribute (name maxPriceTagCase) (value 6) (certainty 1.0)))
	(assert (attribute (name priceOfUnit) (value ?val) (certainty 1.0))))
	
	
;;*******************
;;* QUESTION RULES **
;;*******************

(defmodule QUESTIONS (import MAIN ?ALL) (export ?ALL))

(deftemplate QUESTIONS::question
	(slot attribute (default ?NONE))
	(slot the-question (default ?NONE))
	(multislot valid-answers (default ?NONE))
	(slot valid-answers-display (default ""))
	(slot already-asked (default FALSE))
	(multislot precursors (default ?DERIVE)))

(defrule QUESTIONS::ask-a-question
	?f <- (question (already-asked FALSE)
	(precursors)
	(the-question ?the-question)
	(attribute ?the-attribute)
	(valid-answers $?valid-answers)
	(valid-answers-display ?valid-answers-display))
	=>
	(modify ?f (already-asked TRUE))
	(assert (attribute (name ?the-attribute)
		(value (ask-question ?the-question ?valid-answers-display ?valid-answers )))))

(defrule QUESTIONS::precursor-is-satisfied
	?f <- (question (already-asked FALSE)
	(precursors ?name is ?value $?rest))
	(attribute (name ?name) (value ?value))
	=>
	(if (eq (nth 1 ?rest) and) 
	then (modify ?f (precursors (rest$ ?rest)))
	else (modify ?f (precursors ?rest))))

(defrule QUESTIONS::precursor-is-not-satisfied
	?f <- (question (already-asked FALSE)
	(precursors ?name is-not ?value $?rest))
	(attribute (name ?name) (value ~?value))
	=>
	(if (eq (nth 1 ?rest) and) 
	then (modify ?f (precursors (rest$ ?rest)))
	else (modify ?f (precursors ?rest))))

;;**********************************
;;* BTO-INPUT KNOWLEDGE BASE FACT **
;;**********************************

(defmodule BTO-QUESTIONS (import QUESTIONS ?ALL))

(deffacts BTO-QUESTIONS::question-attributes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(question (attribute orientation)
		(the-question "Do you have a preferred orientation of your unit's window?")
		(valid-answers-display "North-South
East-West
No preference
Others")
		(valid-answers 1 2 3 4))

		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(question (attribute sleepPattern)
		(the-question "What is your sleeping pattern?")
		(valid-answers-display "Early sleeper
Light sleeper
Both
None of the above")
		(valid-answers 1 2 3 4))

		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(question (attribute birdEyeView)
		(the-question "Do you enjoy a bird's eye view? ")
		(valid-answers-display "Yes
No")
		(valid-answers yes no))
		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(question (attribute breeze)
		(the-question "Do you enjoy a natural breeze? ")
		(valid-answers-display "Yes
No")
		(valid-answers yes no))
		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(question (attribute guest)
		(the-question "Do you often host guests at your home? ")
		(valid-answers-display "Yes
No")
		(valid-answers yes no))
		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(question (attribute commute)
		(the-question "How do you normally commute?")
		(valid-answers-display "Bus/MRT
Taxis
Car")
		(valid-answers 1 2 3))

		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(question (attribute eatOut)
		(the-question "Do you eat out often? ")
		(valid-answers-display "Yes
No")		
		(valid-answers yes no))
		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(question (attribute savingSources)
		(precursors askSaving is yes)
		(the-question "What is the amount of money, inclusive of your CPF, which you will use to finance the first payment of your flat?")
		(valid-answers-display "0 < x < 20k
20k < x < 40k
40k < x < 60k
60k < x < 80k
80k and above")
		(valid-answers 1 2 3 4 5))

	(question (attribute pSources)
		(the-question "What is the percentage of your annual income that you will put aside to finance your flat?")
		(valid-answers-display "10%
20%
30%")
		(valid-answers 1 2 3))
		
	(question (attribute average-income)
		(the-question "What is your average monthly household income (value)?")
		(valid-answers-display "Up to $1500
$1,501 to 2,000
$2,001 to 2,500
$2,501 to 3,000
$3,001 to 3,500
$3,501 to 4,000
$4,001 to 4,500
$4,501 to 5,000
$5,001 to 5,500
$5,501 to 6,000
$6,001 to 6,500
$6,501 to 7,000
$7,001 to 7,500
$7,501 to 8,000
$8,001 to 8,500
$8501 and above")
		(valid-answers 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(question (attribute wParents)
		(the-question "Do you plan to live with your parents? ")
		(valid-answers-display "Yes
No")		
		(valid-answers yes no))
	(question (attribute nParents)
		(precursors wParents is no)
		(the-question "Do you plan to live near your parents? ")
		(valid-answers-display "Yes
No")
		(valid-answers yes no))
		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(question (attribute married)
		(the-question "Are you married?")
		(valid-answers-display "Yes
No")
		(valid-answers yes no))
	(question (attribute kids)
		(precursors married is yes)
		(the-question "Do you have kid\(s\)?")
		(valid-answers-display "Yes
No")
		(valid-answers yes no))
	(question (attribute kid-age)
		(precursors kids is yes)
		(the-question "How old is your youngest kid? ")
		(valid-answers-display "0-6 years old
7-12 years old
None of the above")
		(valid-answers 1 2 3))
		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 0 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(question (attribute area)
		(the-question "What is your preferred area in this BTO sales launch?")
		(valid-answers 1 2 3 4 5)
		(valid-answers-display "Eunos
Punggol
Sengkang
Tampines
No Preferences"))
) 

;;********************
;; The RULES module **
;;********************

(defmodule RULES (import MAIN ?ALL) (export ?ALL))

(deftemplate RULES::rule
	(slot certainty (default 1.0))
	(multislot if)
	(multislot then))

(defrule RULES::throw-away-ands-in-antecedent
	?f <- (rule (if and $?rest))
	=>
	(modify ?f (if ?rest)))

(defrule RULES::throw-away-ands-in-consequent
	?f <- (rule (then and $?rest))
	=>
	(modify ?f (then ?rest)))

(defrule RULES::remove-is-condition-when-satisfied
	(declare (salience -1))
	?f <- (rule (certainty ?c1) 
	(if ?attribute is ?value $?rest))
	(attribute (name ?attribute) 
	(value ?value) 
	(certainty ?c2))
	=>
	(modify ?f (certainty ?c2) (if ?rest)))

(defrule RULES::remove-is-not-condition-when-satisfied
	?f <- (rule (certainty ?c1) 
	(if ?attribute is-not ?value $?rest))
	(attribute (name ?attribute) (value ~?value) (certainty ?c2))
	=>
	(modify ?f (certainty ?c2) (if ?rest)))

(defrule RULES::perform-rule-consequent-with-certainty
	?f <- (rule (certainty ?c1) 
	(if) 
	(then ?attribute is ?value with certainty ?c2 $?rest))
	=>
	(modify ?f (then ?rest))
	(assert (attribute (name ?attribute) 
	(value ?value)
	(certainty ?c2))))

(defrule RULES::perform-rule-consequent-without-certainty
	?f <- (rule (certainty ?c1)
	(if)
	(then ?attribute is ?value $?rest))
	(test (or (eq (length$ ?rest) 0)
	(neq (nth 1 ?rest) with)))
	=>
	(modify ?f (then ?rest))
	(assert (attribute (name ?attribute) (value ?value) (certainty ?c1))))

;;******************************
;;* CHOOSE BTO KNOWLEDGE BASE **
;;******************************

(defmodule CHOOSE-BTO
	(import RULES ?ALL)
	(import QUESTIONS ?ALL)
	(import MAIN ?ALL)
)

(defrule CHOOSE-BTO::startit => (focus RULES))

(deffacts the-bto-rules

	;;;;;;;;;;;;;;;;;;;;;;;;;;; Rules for Question 10 ;;;;;;;;;;;;;;;;;;;;;;;;;;
	(rule (if orientation is 1) (
		then type-of-flat is orientation_N-S with certainty 0.8
	))		

	(rule (if orientation is 2) (
		then type-of-flat is orientation_E-W with certainty 0.8
	))	

	
	;;;;;;;;;;;;;;;;;;;;;;;;;;; Rules for Question 9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(rule (if sleepPattern is-not 4) (
		then type-of-flat is high-level with certainty 0.5
	))
	
	(rule (if sleepPattern is 1) (
		then type-of-flat is noWestSun with certainty 0.85
	))	

	(rule (if sleepPattern is 3) (
		then type-of-flat is noWestSun with certainty 0.85
	))	

	
	;;;;;;;;;;;;;;;;;;;;;;;;;;; Rules for Question 8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(rule (if birdEyeView is yes) (
		then type-of-flat is high-level with certainty 0.8
	))
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;; Rules for Question 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(rule (if breeze is yes) (
		then type-of-flat is high-level with certainty 0.5 and
		type-of-flat is orientation_N-S with certainty 0.5
	))
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;; Rules for Question 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(rule (if guest is yes) (
		then number-of-rooms is 4-room with certainty 0.5 and 
		number-of-rooms is 5-room with certainty 0.5 and
		amenities is transport with certainty 0.5
	))

	(rule (if guest is yes and kids is yes) (
		then number-of-rooms is 4-room with certainty 0.2 and 
		number-of-rooms is 5-room with certainty 0.4
	))	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;; Rules for Question 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(rule (if commute is 1) (
		then amenities is transport with certainty 0.8
	))
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;; Rules for Question 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(rule (if eatOut is yes) (
		then amenities is hawker with certainty 0.75	and
		amenities is mall with certainty 0.4
	))	

	(rule (if eatOut is no) (
		then amenities is supermarket with certainty 0.75	
	))		
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;; Rules for Question 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(rule (if maxPriceTagCase is 1) (
		then number-of-rooms is 2-room with certainty 0.7
	))

	(rule (if maxPriceTagCase is 2) (
		then number-of-rooms is 3-room with certainty 0.7
	))
	
	(rule (if maxPriceTagCase is 3) (
		then number-of-rooms is 4-room with certainty 0.7
	))

	(rule (if maxPriceTagCase is 4) (
		then number-of-rooms is 4-room with certainty 0.6 and
		number-of-rooms is 5-room with certainty 0.4
	))

	(rule (if maxPriceTagCase is 5) (
		then number-of-rooms is 4-room with certainty 0.4 and
		number-of-rooms is 5-room with certainty 0.6
	))

	(rule (if maxPriceTagCase is 6) (
		then number-of-rooms is 4-room with certainty 0.2 and
		number-of-rooms is 5-room with certainty 0.7
	))	

	(rule (if married is yes and wParents is yes and maxPriceTagCase is 3) (
		then number-of-rooms is 3-Gen with certainty 0.75
	))	

	(rule (if married is yes and wParents is yes and maxPriceTagCase is 4) (
		then number-of-rooms is 3-Gen with certainty 0.85
	))		

	(rule (if married is yes and wParents is yes and maxPriceTagCase is 5) (
		then number-of-rooms is 3-Gen with certainty 0.95
	))		

	(rule (if married is yes and wParents is yes and maxPriceTagCase is 6) (
		then number-of-rooms is 3-Gen with certainty 0.95
	))	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;; Rules for Question 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(rule (if kid-age is 1 and wParents is yes) (
		then amenities is child-care with certainty -0.55	
	))
	
	(rule (if kid-age is 1 and nParents is yes) (
		then amenities is child-care with certainty -0.4	
	))
	
	(rule (if kid-age is 1 and wParents is no and nParents is no) (
		then amenities is child-care with certainty 0.55	
	))	

	(rule (if wParents is yes) (
		then number-of-rooms is 2-room with certainty -0.5 and
		number-of-rooms is 3-room with certainty 0.1 and 
		number-of-rooms is 4-room with certainty 0.25 and 
		number-of-rooms is 5-room with certainty 0.65 and
		number-of-rooms is 3-Gen with certainty 0.8 and
		amenities is supermarket with certainty 0.5 and
		amenities is hawker with certainty -0.5		
	))
	
	(rule (if married is no and wParents is no) (
		then number-of-rooms is 2-room with certainty 0.1 and
		number-of-rooms is 3-room with certainty 0.45 and 
		number-of-rooms is 4-room with certainty 0.25 and 
		number-of-rooms is 5-room with certainty 0.2		
	))
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;; Rules for Question 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(rule (if kid-age is 2) (
		then amenities is school with certainty 0.55
	))
	
	(rule (if kid-age is 1) (
		then amenities is child-care with certainty 0.55
	))

	(rule (if kids is yes) (
		then number-of-rooms is 2-room with certainty -0.5 and
		number-of-rooms is 3-room with certainty 0.15 and
		number-of-rooms is 4-room with certainty 0.4 and
		number-of-rooms is 5-room with certainty 0.5
	))
	
	(rule (if married is no) (
		then number-of-rooms is 3-Gen with certainty -1.0
	))
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;; Rules for Question 0 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(rule (if area is 1) (
		then location is Eunos with certainty 1.0
	))
	(rule (if area is 2) (
		then location is Punggol with certainty 1.0
	))
	(rule (if area is 3) (
		then location is Sengkang with certainty 1.0
	))
	(rule (if area is 4) (
		then location is Tampines with certainty 1.0
	))
	(rule (if area is 5) (
		then location is noPreferences with certainty 1.0
	))	
)


;;*****************************
;;* PRINT SELECTED BTO RULES **
;;*****************************

(defmodule PRINT-RESULTS (import MAIN ?ALL))

(defrule PRINT-RESULTS::print-number-of-rooms ""
	?f0 <- (attribute (name location) (value ?area) (certainty ?cf0))
	?f1 <- (attribute (name number-of-rooms) (value 2-room) (certainty ?cf1))
	?f2 <- (attribute (name number-of-rooms) (value 3-room) (certainty ?cf2))
	?f3 <- (attribute (name number-of-rooms) (value 4-room) (certainty ?cf3))
	?f4 <- (attribute (name number-of-rooms) (value 5-room) (certainty ?cf4))
	?f5 <- (attribute (name number-of-rooms) (value 3-Gen) (certainty ?cf5))
	?f6 <- (attribute (name amenities) (value school) (certainty ?cf6))
	?f7 <- (attribute (name amenities) (value child-care) (certainty ?cf7))
	?f8 <- (attribute (name amenities) (value hawker) (certainty ?cf8))
	?f9 <- (attribute (name amenities) (value supermarket) (certainty ?cf9))
	?f10 <- (attribute (name amenities) (value mall) (certainty ?cf10))
	?f11 <- (attribute (name amenities) (value transport) (certainty ?cf11))
	?f12 <- (attribute (name type-of-flat) (value orientation_N-S) (certainty ?cf12))
	?f13 <- (attribute (name type-of-flat) (value orientation_E-W) (certainty ?cf13))
	?f14 <- (attribute (name type-of-flat) (value noWestSun) (certainty ?cf14))
	?f15 <- (attribute (name type-of-flat) (value high-level) (certainty ?cf15))
	?f16 <- (attribute (name priceOfUnit) (value ?price) (certainty ?cf16))	
	=>
	(retract ?f0 ?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7 ?f8 ?f9 ?f10 ?f11 ?f12 ?f13 ?f14 ?f15 ?f16)
	(final (str-cat "\{
	\""location_area"\":\""?area"\",
	\""2-room"\":"?cf1",
	\""3-room"\":"?cf2",
	\""4-room"\":"?cf3",
	\""5-room"\":"?cf4",
	\""3-Gen"\":"?cf5",
	\""school"\":"?cf6",
	\""childcare"\":"?cf7",
	\""hawker_center"\":"?cf8",
	\""supermarket"\":"?cf9",
	\""malls"\":"?cf10",
	\""interchange_mrt"\":"?cf11",
	\""orientation_N_S"\":"?cf12",
	\""orientation_E_W"\":"?cf13",
	\""no_west_sun"\":"?cf14",
	\""high_level"\":"?cf15",
	\""max_price"\":"?price"
\}")))

