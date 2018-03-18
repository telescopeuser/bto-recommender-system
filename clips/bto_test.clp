;; lexemep - The lexemep function returns the symbol TRUE if its argument 
;; is a string or symbol, otherwise it returns the symbol FALSE.

(defmodule MAIN (export ?ALL))

;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction MAIN::ask-question (?question ?allowed-values-display $?allowed-values)
	(bind ?answer (prompt2 ?question ?allowed-values-display $?allowed-values))
	(debug (str-cat "B-2= " (implode$ ?allowed-values) ", response: " ?answer))
	(if (lexemep ?answer) 
		then (bind ?answer (lowcase ?answer))
	)
	(while (not (member ?answer ?allowed-values)) do
		(bind ?answer (prompt2 ?question ?allowed-values-display $?allowed-values))
		(debug (str-cat "B-2= " (implode$ ?allowed-values) ", response: " ?answer))
		(if (lexemep ?answer) 
			then (bind ?answer (lowcase ?answer))
		)
	)
	?answer
)

;;*****************
;;* INITIAL STATE *
;;*****************

(deftemplate MAIN::attribute
	(slot name)
	(slot value)
	(slot certainty (default 1.0)))					;;Default certainty factor

(defrule MAIN::start
	(declare (salience 10000))
	=>
	(set-fact-duplication TRUE)
	(focus QUESTIONS CHOOSE-BTO BTO PRINT-RESULTS))		;;Set the sequence to fire the module

(defrule MAIN::combine-positive ""
	(declare (salience 100)
		(auto-focus TRUE))
	?f1 <- (attribute (name ?rel) (value ?val) (certainty ?per1&:(>= ?per1 0)))
	?f2 <- (attribute (name ?rel) (value ?val) (certainty ?per2&:(>= ?per2 0)))
	(test (neq ?f1 ?f2))
	=>
	(retract ?f2)
	(modify ?f1 (certainty =(+ ?per1 (* ?per2 (- 1 ?per1))))))

(defrule MAIN::combine-negative ""
	(declare (salience 100)
		(auto-focus TRUE))
	?f1 <- (attribute (name ?rel) (value ?val) (certainty ?per1&:(< ?per1 0)))
	?f2 <- (attribute (name ?rel) (value ?val) (certainty ?per2&:(< ?per2 0)))
	(test (neq ?f1 ?f2))
	=>
	(retract ?f2)
	(modify ?f1 (certainty =(+ ?per1 (* ?per2 (+ 1 ?per1))))))

(defrule MAIN::combine-positive-negative ""
	(declare (salience 100)
		(auto-focus TRUE))
	?f1 <- (attribute (name ?rel) (value ?val) (certainty ?per1))
	?f2 <- (attribute (name ?rel) (value ?val) (certainty ?per2))
	(test (neq ?per1 ?per2))
	(test (< (* ?per1 ?per2) 0))
	=>
	(retract ?f2)
	(modify ?f1 (certainty =(/ (+ ?per1 ?per2) (- 1 (min (abs ?per1) (abs ?per2)))))))

;;******************
;;* QUESTION RULES *
;;******************

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
	(valid-answers-display ?valid-answers-display)
	(valid-answers $?valid-answers))
	=>
	(modify ?f (already-asked TRUE))
	(assert (attribute (name ?the-attribute)
		(value (ask-question ?the-question ?valid-answers-display ?valid-answers)))))

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

;;************************
;;* BTO-INPUT QUESTIONS **
;;************************

(defmodule BTO-QUESTIONS (import QUESTIONS ?ALL))

(deffacts BTO-QUESTIONS::question-attributes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(question (attribute live-with-parents)
		(the-question "Do you plan to live with your parents (yes/no)? ")
		(valid-answers-display "Yes
No")
		(valid-answers yes no))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Question 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(question (attribute married)
		(the-question "Are you married (yes/no)? ")
		(valid-answers-display "Yes
No")
		(valid-answers yes no))
	(question (attribute kids)
		(precursors married is yes)
		(the-question "Do you have kids (yes/no)? ")
		(valid-answers-display "Yes
No")
		(valid-answers yes no))
	(question (attribute kid-age)
		(precursors kids is yes)
		(the-question "How old is your youngest kid? (0-6) press 1, (7-16) press 2")
		(valid-answers-display "Yes
No")
		(valid-answers 1 2))
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
	(debug (str-cat "rest= " (implode$ ?rest)))
	(modify ?f (certainty (min ?c1 ?c2)) (if ?rest)))

(defrule RULES::remove-is-not-condition-when-satisfied
	?f <- (rule (certainty ?c1) 
	(if ?attribute is-not ?value $?rest))
	(attribute (name ?attribute) (value ~?value) (certainty ?c2))
	=>
	(modify ?f (certainty (min ?c1 ?c2)) (if ?rest)))

(defrule RULES::perform-rule-consequent-with-certainty
	?f <- (rule (certainty ?c1) 
	(if) 
	(then ?attribute is ?value with certainty ?c2 $?rest))
	=>
	(modify ?f (then ?rest))
	(assert (attribute (name ?attribute) 
	(value ?value)
	(certainty (* ?c1 ?c2)))))

(defrule RULES::perform-rule-consequent-without-certainty
	?f <- (rule (certainty ?c1)
	(if)
	(then ?attribute is ?value $?rest))
	(test (or (eq (length$ ?rest) 0)
	(neq (nth 1 ?rest) with)))
	=>
	(modify ?f (then ?rest))
	(assert (attribute (name ?attribute) (value ?value) (certainty ?c1))))

;;**********************
;;* CHOOSE BTO RULES  **
;;**********************

(defmodule CHOOSE-BTO
	(import RULES ?ALL)
	(import QUESTIONS ?ALL)
	(import MAIN ?ALL)
)

(defrule CHOOSE-BTO::startit => (focus RULES))

(deffacts the-bto-rules

	;; Rules for picking best-size
	(rule (if kid-age is 1) (
		then nearby-childcare is yes and 
		number-of-rooms is 3-room with certainty -0.5 and
		number-of-rooms is 5-room with certainty 0.6 and
		amenities is child-care
	))

	(rule (if kid-age is 2) (
		then nearby-school is yes and 
		number-of-rooms is 3-room with certainty 0.2 and
		number-of-rooms is 5-room with certainty 0.7 and
		amenities is child-care)
	)

	;; Rules for picking best-amenities
	(rule (if live-with-parents is yes) (
		then number-of-rooms is 3-room with certainty 0.3 and
		number-of-rooms is 5-room with certainty 0.8 and
		amenities is child-care)
	)
)

;;************************
;;* BTO SELECTION RULES **
;;************************

(defmodule BTO (import MAIN ?ALL))

(deffacts any-attributes
	(attribute (name number-of-rooms) (value any))
	(attribute (name amenities) (value any))
)

(deftemplate BTO::bto
	(slot name (default ?NONE))
	(multislot address (default any))
	(multislot project (default any))
	(multislot room_type (default any)))

(deffacts BTO::the-bto-list
	(bto (name Eunos) (address "EUNOS RD 2") (project "EUNOS COURT") (room_type "3-room"))
	(bto (name Eunos) (address "ANCHORVALE RD") (project "ANCHORVALE VILLAGE") (room_type "4-room"))
	(bto (name Eunos) (address "SENGKANG WEST WAY") (project "FERNVALE GLADES") (room_type "3-room"))
	(bto (name Eunos) (address "NORTHSHORE DR") (project "NORTHSHORE EDGE") (room_type "5-room"))
	(bto (name Eunos) (address "TAMPINES NTH DR 2") (project "TAMPINES GREENCOURT") (room_type "5-room"))
)

(defrule BTO::generate-bto
	(bto (name ?name)
	(address $? ?c $?)
	(project $? ?b $?)
	(room_type $? ?s $?))
	(attribute (name number-of-rooms) (value ?c) (certainty ?certainty-1))
	(attribute (name amenities) (value ?b) (certainty ?certainty-2))
	=>
	(assert (attribute (name bto) (value ?name)
		(certainty (min ?certainty-1 ?certainty-2)))))

;;*****************************
;;* PRINT SELECTED BTO RULES **
;;*****************************

(defmodule PRINT-RESULTS (import MAIN ?ALL))

(defrule PRINT-RESULTS::header ""
	(declare (salience 10))
	=>
	(final (str-cat "\{
	\""2-room"\":0.1,
	\""3-room"\":0.3,
	\""interchange_mrt"\":0.7,
	\""hawker_center"\":0.65,
	\""school"\":0.65,
	\""supermarket"\":0.4,
	\""orientation_E_W"\":0.3,
	\""orientation_N_S"\":0.2,
	\""location_area"\":\"Sengkang\",
	\""max_price"\":300000
\}")))

(defrule PRINT-RESULTS::print-bto ""
	?rem <- (attribute (name bto) (value ?name) (certainty ?per))		  
	(not (attribute (name bto) (certainty ?per1&:(> ?per1 ?per))))
	=>
	(retract ?rem)
	(alert (str-cat ?name ?per)))

(defrule PRINT-RESULTS::remove-poor-bto-choices ""
	?rem <- (attribute (name bto) (certainty ?per&:(< ?per 0)))
	=>
	(retract ?rem))

(defrule PRINT-RESULTS::end-spaces ""
	(not (attribute (name bto)))
	=>
	(alert ""))
