;;****************
;;* python *
;;****************

(deffunction debug (?message)
	(python-call debug ?message)
)

(deffunction alert (?message)
	(python-call print ?message)
)

(deffunction prompt (?question $?allowed-values)
	(bind ?answer (python-call ask ?question $?allowed-values))
	?answer
)

(deffunction prompt2 (?question ?allowed-values-display $?allowed-values)
	(bind ?answer (python-call ask ?question $?allowed-values))
	?answer
)

(deffunction final (?message)
	(python-call final ?message)
)
