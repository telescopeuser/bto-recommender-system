;;****************
;;* clips *
;;****************

(deffunction debug (?message)
	(printout t ?message crlf)
)

(deffunction alert (?message)
	(printout t ?message crlf)
)

(deffunction prompt (?question $?allowed-values)
	(printout t ?question crlf)
	(bind ?answer (read))
	?answer
)

(deffunction prompt2 (?question ?allowed-values-display $?allowed-values)
	(printout t ?question crlf)
	(printout t ?allowed-values-display crlf)
	(bind ?answer (read))
	?answer
)

(deffunction final (?message)
	(printout t ?message crlf)
)
