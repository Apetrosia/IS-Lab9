;========================================================================
; Этот блок реализует логику обмена информацией с графической оболочкой,
; а также механизм остановки и повторного пуска машины вывода
; Русский текст в комментариях разрешён!

(deftemplate ioproxy  ; шаблон факта-посредника для обмена информацией с GUI
	(slot fact-id)        ; теоретически тут id факта для изменения
	(multislot answers)   ; возможные ответы
	(multislot messages)  ; исходящие сообщения
	(slot reaction)       ; возможные ответы пользователя
	(slot value)          ; выбор пользователя
	(slot restore)        ; забыл зачем это поле
)

; Собственно экземпляр факта ioproxy
(deffacts proxy-fact
	(ioproxy
		(fact-id 0112) ; это поле пока что не задействовано
		(value none)   ; значение пустое
		(messages)     ; мультислот messages изначально пуст
	)
)

(defrule clear-messages
   	(declare (salience 130))
	?clear-msg-flg <- (clearmessage)
	?proxy <- (ioproxy)
	=>
	(modify ?proxy (messages))
	(retract ?clear-msg-flg)
	(printout t "Messages cleared ..." crlf)	
)

(defrule set-output-and-halt
   	(declare (salience 129))
	?current-message <- (sendmessagehalt ?new-msg)
	?proxy <- (ioproxy (messages $?msg-list))
	=>
	(printout t "Message set : " ?new-msg " ... halting ..." crlf)
	(modify ?proxy (messages ?new-msg))
	(retract ?current-message)
	(halt)
)

;  Аналогичен предыдущему, но с добавлением сообщения, а не с заменой
(defrule append-output-and-halt
   	(declare (salience 128))
	?current-message <- (sendmessagehalt ?new-msg)
	?proxy <- (ioproxy (messages $?msg-list))
	=>
	(printout t "Message set : " ?new-msg " ... halting ..." crlf)
	(modify ?proxy (messages $?msg-list ?new-msg))
	(retract ?current-message)
	(halt)
)

;  Аналогичен предыдущему, но с установкой сообщения и продолжением работы (извлекая факт с текущим сообщением)
(defrule set-output-and-proceed
   	(declare (salience 127))
	?current-message <- (sendmessage ?new-msg)
	?proxy <- (ioproxy (messages $?msg-list))
	=>
	(printout t "Message appended : " ?new-msg " ... proceeding ..." crlf)
	(modify ?proxy (messages ?new-msg))
	(retract ?current-message)	
)

;  По аналогии
(defrule append-output-and-proceed
   	(declare (salience 126))
	?current-message <- (sendmessage ?new-msg)
	?proxy <- (ioproxy (messages $?msg-list))
	=>
	(printout t "Message appended : " ?new-msg " ... proceeding ..." crlf)
	(modify ?proxy (messages $?msg-list ?new-msg))
	(retract ?current-message)	
)

;======================================================================================

(deffacts couple
;	(master H)
;	(rank none)
;	(scenic normal)
;	(age 1)
;	(technique 0)
;	(musicality 0)
;	(partnerskills 0)
;	(presentation 0)
;	(recognition 1)
;	(program 10dance)
;	(points 0)
)

;======================================================================================
(defrule rule0
   (declare (salience 125))
   (age ?age)
   (technique ?technique)
   (musicality ?musicality)
   (partnerskills ?partnerskills)
   (presenation ?presentation)
   (test (or (< ?age 0) (> ?technique 10) (< ?technique 0) (> ?musicality 10) (< ?musicality 0) (> ?partnerskills 10) (< ?partnerskills 0) (> ?presentation 10) (< ?presentation 0)))
   =>
   (printout t "Uncorrect input => uncorrect output" crlf) 
)

(defrule rule01
   (declare (salience 124))
   ?t <- (technique ?technique)
   (musicality ?musicality)
   (partnerskills ?partnerskills)
   (presenation ?presentation)
   (test(or (eq ?technique 0) (eq ?technique n)))
   =>
   (assert (technique (/ (+ ?musicality (+ ?partnerskills ?presentation)) 3)))
   (retract ?t)
)

(defrule rule02
   (declare (salience 123))
   (technique ?technique)
   ?m <- (musicality ?musicality)
   (partnerskills ?partnerskills)
   (presenation ?presentation)
   (test(or (eq ?musicality 0) (eq ?musicality n)))
   =>
   (assert (musicality (/ (+ ?technique (+ ?partnerskills ?presentation)) 3)))
   (retract ?m)
)

(defrule rule03
   (declare (salience 122))
   (technique ?technique)
   (musicality ?musicality)
   ?p <- (partnerskills ?partnerskills)
   (presenation ?presentation)
   (test(or (eq ?partnerskills 0) (eq ?partnerskills n)))
   =>
   (assert (partnerskills (/ (+ ?musicality (+ ?technique ?presentation)) 3)))
   (retract ?p)
)

(defrule rule04
   (declare (salience 121))
   (technique ?technique)
   (musicality ?musicality)
   (partnerskills ?partnerskills)
   ?p <- (presenation ?presentation)
   (test(or (eq presentation 0) (eq presentation n)))
   =>
   (assert (presentation (/ (+ ?technique (+ ?partnerskills ?musicality)) 3)))
   (retract ?p)
)

;; Правила для прямого вывода
(defrule rule1
   (declare (salience 120))
   (age ?age)
   (test(and (> ?age 0) (< ?age 8)))
   =>
   (assert (ageCat "Baby"))
   (printout t "----------------------" crlf) 
   (printout t "Age category: Baby"crlf) 
)

(defrule rule2
   (declare (salience 119))
   (age ?age)
   (test(and (> ?age 7) (< ?age 10)))
   =>
   (assert (ageCat "Juviniles 1"))
   (printout t "----------------------" crlf) 
   (printout t "Age category: Juviniles 1" crlf) 
)

(defrule rule3
   (declare (salience 118))
   (age ?age)
   (test(and (> ?age 9) (< ?age 12)))
   =>
   (assert (ageCat "Juviniles 2"))
   (printout t "----------------------" crlf) 
   (printout t "Age category: Juviniles 2" crlf)
)

(defrule rule4
   (declare (salience 117))
   (age ?age)
   (test (and (> ?age 11) (< ?age 14)))
   =>
   (assert (ageCat "Junior 1"))
   (printout t "----------------------" crlf) 
   (printout t "Age category: Junior 1" crlf)
)

(defrule rule5
   (declare (salience 116))
   (age ?age)
   (test (and (> ?age 13) (< ?age 16)))
   =>
   (assert (ageCat "Junior 2"))
   (printout t "----------------------" crlf) 
   (printout t "Age category: Junior 2" crlf)
)

(defrule rule6
   (declare (salience 115))
   (age ?age)
   (test (and (> ?age 15) (< ?age 19)))
   =>
   (assert (ageCat "Youth"))
   (printout t "----------------------" crlf) 
   (printout t "Age category: Youth" crlf)
)

(defrule rule7
   (declare (salience 114))
   (age ?age)
   (test(and (> ?age 18) (< ?age 21)))
   =>
   (assert (ageCat "Youth 2"))
   (printout t "----------------------" crlf) 
   (printout t "Age category: Youth 2" crlf)
)

(defrule rule8
   (declare (salience 113))
   (age ?age)
   (test(and (> ?age 20) (< ?age 45)))
   =>
   (assert (ageCat "Adult"))
   (printout t "----------------------" crlf) 
   (printout t "Age category: Adult" crlf)
)

(defrule rule9
   (declare (salience 112))
   (age ?age)
   (test(> ?age 44))
   =>
   (assert (ageCat "Senior"))
   (printout t "----------------------" crlf) 
   (printout t "Age category: Senior" crlf)
)


(defrule rule10
   (declare (salience 111))
   (ageCat ?ageCat)
   (test(or (eq ?ageCat "Senior") 
            (eq ?ageCat "Adult") 
            (eq ?ageCat "Youth 2")))
   =>
   (assert (rangeOfClasses M))
   (printout t "----------------------" crlf) 
   (printout t "Possible range of classes: H E D C B A S M" crlf)
)

(defrule rule11
   (declare (salience 110))
   (ageCat ?ageCat)
   (test(eq ?ageCat "Youth"))
   =>
   (assert (rangeOfClasses S))
   (printout t "----------------------" crlf) 
   (printout t "Possible range of classes: H E D C B A S" crlf)
)

(defrule rule12
   (declare (salience 109))
   (ageCat ?ageCat)
   (test(eq ?ageCat "Junior 2"))
   =>
   (assert (rangeOfClasses A))
   (printout t "----------------------" crlf) 
   (printout t "Possible range of classes: H E D C B A" crlf)
)

(defrule rule13
   (declare (salience 108))
   (ageCat ?ageCat)
   (test(eq ?ageCat "Junior 1"))
   =>
   (assert (rangeOfClasses B))
   (printout t "----------------------" crlf) 
   (printout t "Possible range of classes: H E D C B" crlf)
)

(defrule rule14
   (declare (salience 107))
   (ageCat ?ageCat)
   (test(eq ?ageCat "Juviniles 2"))
   =>
   (assert (rangeOfClasses C))
   (printout t "----------------------" crlf) 
   (printout t "Possible range of classes: H E D C" crlf)
)

(defrule rule15
   (declare (salience 106))
   (ageCat ?ageCat)
   (test(eq ?ageCat "Juviniles 1"))
   =>
   (assert (rangeOfClasses D))
   (printout t "----------------------" crlf) 
   (printout t "Possible range of classes: H E D" crlf)
)

(defrule rule16
   (declare (salience 105))
   (ageCat ?ageCat)
   (test(eq ?ageCat "Baby"))
   =>
   (assert (rangeOfClasses E))
   (printout t "----------------------" crlf) 
   (printout t "Possible range of classes: H E" crlf)
)


(defrule rule17
   (declare (salience 104))
   (ageCat ?ageCat)
   (test(or (eq ?ageCat "Senior") (eq ?ageCat "Baby")))
   =>
   (assert (rangeOfRanks none))
   (printout t "----------------------" crlf) 
   (printout t "Possible range of ranks: none" crlf)
)

; Правила диапазона рангов
(defrule rule18
  (declare (salience 103))
   (ageCat ?ageCat)
  (test(eq ?ageCat "Adult"))
  =>
  (assert (rangeOfRanks MSIC))
  (printout t "----------------------" crlf) 
  (printout t "Possible range of ranks: MSIC" crlf)
)

(defrule rule19
   (declare (salience 102))
   (ageCat ?ageCat)
  (test(eq ?ageCat "Youth 2"))
  =>
  (assert (rangeOfRanks MS))
  (printout t "----------------------" crlf) 
  (printout t "Possible range of ranks: MS" crlf)
)

(defrule rule20
   (declare (salience 101))
   (ageCat ?ageCat)
  (test (eq ?ageCat "Youth"))
  =>
  (assert (rangeOfRanks CMS))
  (printout t "----------------------" crlf) 
  (printout t "Possible range of ranks: CMS" crlf)
)

(defrule rule21
   (declare (salience 100))
   (ageCat ?ageCat)
  (test (or (eq ?ageCat "Junior 2") (eq ?ageCat "Junior 1")))
  =>
  (assert (rangeOfRanks "1 adult"))
  (printout t "----------------------" crlf) 
  (printout t "Possible range of ranks: 1 adult" crlf)
)

(defrule rule22
   (declare (salience 99))
   (ageCat ?ageCat)
  (test(or (eq ?ageCat "Juviniles 2") (eq ?ageCat "Juviniles 1")))
  =>
  (assert (rangeOfRanks "1 junior"))
  (printout t "----------------------" crlf) 
  (printout t "Possible range of ranks: 1 junior" crlf)
)


; Правила диапазона классов по программе
(defrule rule23
   (declare (salience 98))
  (program ?program)
  (ageCat ?ageCat)
  ?rc <- (rangeOfClasses ?rangeOfClasses)
  (test (and (eq ?program "10 dance")
  (or (eq ?ageCat "Senior") (eq ?ageCat "Adult") (eq ?ageCat "Youth 2") (eq ?ageCat "Youth")(eq ?ageCat "Junior 2") (eq ?ageCat "Junior 1") (eq ?ageCat "Juviniles 2"))))
  =>
  (retract ?rc)
  (assert (rangeOfClasses C))
  (printout t "----------------------" crlf) 
  (printout t "Possible range of classes: H E D C" crlf)
)

(defrule rule241
   (declare (salience 97))
  (program ?program)
  (ageCat ?ageCat)
  ?rc <- (rangeOfClasses ?rangeOfClasses)
  (test (and (eq ?program "10 dance") (eq ?ageCat "Juviniles 1")))
  =>
  (retract ?rc)
  (assert (rangeOfClasses D))
  (printout t "----------------------" crlf) 
  (printout t "Possible range of classes: E D" crlf)
)

(defrule rule242
   (declare (salience 97))
  (program ?program)
  (ageCat ?ageCat)
  ?rc <- (rangeOfClasses ?rangeOfClasses)
  (test (and (eq ?program "10 dance")(eq ?ageCat "Baby")))
  =>
  (retract ?rc)
  (assert (rangeOfClasses E))
  (printout t "----------------------" crlf) 
  (printout t "Possible range of classes: H E" crlf)
)

(defrule rule25
   (declare (salience 96))
  (program ?program)
  (ageCat ?ageCat)
  ?rc <- (rangeOfClasses ?rangeOfClasses)
  (test (and (neq ?program "10 dance")
  (or (eq ?ageCat "Senior") (eq ?ageCat "Adult") (eq ?ageCat "Youth 2"))))
  =>
  (retract ?rc)
  (assert (rangeOfClasses M))
  (printout t "----------------------" crlf) 
  (printout t "Possible range of classes: B A S M" crlf)
)

(defrule rule26
   (declare (salience 95))
  (program ?program)
  (ageCat ?ageCat)
  ?rc <- (rangeOfClasses ?rangeOfClasses)
  (test (and (neq ?program "10 dance")
  (eq ?ageCat "Youth")))
  =>
  (retract ?rc)
  (assert (rangeOfClasses S))
  (printout t "----------------------" crlf) 
  (printout t "Possible range of classes: B A S" crlf)
)

(defrule rule27
   (declare (salience 94))
  (program ?program)
  (ageCat ?ageCat)
  ?rc <- (rangeOfClasses ?rangeOfClasses)
  (test (and (neq ?program "10 dance")
  (eq ?ageCat "Junior 2")))
  =>
  (retract ?rc)
  (assert (rangeOfClasses A))
  (printout t "----------------------" crlf) 
  (printout t "Possible range of classes: B A" crlf)
)

(defrule rule28
   (declare (salience 93))
  (program ?program)
  (ageCat ?ageCat)
  ?rc <- (rangeOfClasses ?rangeOfClasses)
  (test (and (neq ?program "10 dance")
  (eq ?ageCat "Junior 1")))
  =>
  (retract ?rc)
  (assert (rangeOfClasses B))
  (printout t "----------------------" crlf) 
  (printout t "Possible range of classes: B" crlf)
)


; Правила коэффициента сценичности
(defrule rule29
   (declare (salience 92))
  (scenic ?scenic)
  (program ?program)
  (test (and (eq ?scenic "extra")
  (neq ?program "10 dance")))
  =>
  (assert (coeffScenic (/ 1.0 3)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: 0.3" crlf)
)

(defrule rule30
   (declare (salience 91))
  (scenic ?scenic)
  (program ?program)
  (test (and (eq ?scenic "extra")
  (eq ?program 10dance)))
  =>
  (assert (coeffScenic (/ 1.0 4)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: 0.25" crlf)
)

(defrule rule31
   (declare (salience 90))
  (scenic ?scenic)
  (program ?program)
  (test (and (eq ?scenic "bright")
  (neq ?program "10 dance")))
  =>
  (assert (coeffScenic (/ 1.0 5)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: 0.2" crlf)
)

(defrule rule32
   (declare (salience 89))
  (scenic ?scenic)
  (program ?program)
  (test (and (eq ?scenic "bright")
  (eq ?program "10 dance")))
  =>
  (assert (coeffScenic (/ 1 6)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: 0.15" crlf)
)

(defrule rule33
   (declare (salience 88))
  (scenic ?scenic)
  (program ?program)
  (test (and (eq ?scenic "normal")
  (neq ?program "10 dance")))
  =>
  (assert (coeffScenic (/ 1.0 10)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: 0.1" crlf)
)

(defrule rule34
   (declare (salience 87))
  (scenic ?scenic)
  (program ?program)
  (test (and (eq ?scenic "normal")
  (eq ?program "10 dance")))
  =>
  (assert (coeffScenic (+ 0 0.05)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: 0.05" crlf)
)

(defrule rule35
   (declare (salience 86))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?s <- (coeffScenic ?coeffScenic)
  (test (and (eq ?scenic "extra")
  (eq ?ageCat "Adult")))
  =>
  (assert (coeffScenic (+ 0.3 ?s))))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ 0.3 ?s))  crlf)
  (retract ?cs)
)

(defrule rule36
   (declare (salience 85))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (and (eq ?scenic "extra")
  (eq ?ageCat "Youth 2")))
  =>
  (assert (coeffScenic (+ ?cs 0.29)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.29)  crlf)
  (retract ?cs)
)

(defrule rule37
   (declare (salience 84))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (and (eq ?scenic "extra")
  (eq ?ageCat "Youth")))
  =>
  (assert (coeffScenic (+ ?cs 0.28)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.28)  crlf)
  (retract ?cs)
)

(defrule rule38
   (declare (salience 83))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (and (eq ?scenic "extra")
  (eq ?ageCat "Junior 2")))
  =>
  (assert (coeffScenic (+ ?cs 0.27)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.27)  crlf)
  (retract ?cs)
)

(defrule rule39
   (declare (salience 82))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (and (eq ?scenic "extra")
  (eq ?ageCat "Senior")))
  =>
  (assert (coeffScenic (+ ?cs 0.26)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.26)  crlf)
  (retract ?cs)
)

(defrule rule40
   (declare (salience 81))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (or 
	(and(eq ?scenic "extra")(eq ?ageCat "Junior 1"))
	(and(eq ?scenic "bright")(eq ?ageCat "Adult"))
	)
  )
  =>
  (assert (coeffScenic (+ ?cs 0.25)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.25)  crlf)
  (retract ?cs)
)

(defrule rule41
   (declare (salience 80))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (or 
	(and(eq ?scenic "extra")(eq ?ageCat "Juviniles 2"))
	(and(eq ?scenic "bright")(eq ?ageCat "Youth 2"))
	)
  )
  =>
  (assert (coeffScenic (+ ?cs 0.24)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.24)  crlf)
  (retract ?cs)

)

(defrule rule42
   (declare (salience 79))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (or 
	(and(eq ?scenic "extra")(eq ?ageCat "Juviniles 1"))
	(and(eq ?scenic "bright")(eq ?ageCat "Youth"))
	)
  )
  =>
  (assert (coeffScenic (+ ?cs 0.23)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.23)  crlf)
  (retract ?cs)
)

(defrule rule43
   (declare (salience 78))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (and (eq ?scenic "bright")
  (eq ?ageCat "Junior 2")))
  =>
  (assert (coeffScenic (+ ?cs 0.22)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.22)  crlf)
  (retract ?cs)
)

(defrule rule44
   (declare (salience 77))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (and (eq ?scenic "bright")
  (eq ?ageCat "Senior")))
  =>
  (assert (coeffScenic (+ ?cs 0.21)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.21)  crlf)
  (retract ?cs)
)

(defrule rule45
   (declare (salience 76))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (or 
	(and(eq ?scenic "bright")(eq ?ageCat "Junior 1"))
	(and(eq ?scenic "normal")(eq ?ageCat "Adult"))
	)
  )
  =>
  (assert (coeffScenic (+ ?cs 0.2)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.2)  crlf)
  (retract ?cs)
)

(defrule rule46
   (declare (salience 75))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (or 
	(and(eq ?scenic "bright")(eq ?ageCat "Juviniles 2"))
	(and(eq ?scenic "normal")(eq ?ageCat "Youth 2"))
	)
  )
  =>
  (assert (coeffScenic (+ ?cs 0.19)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.19)  crlf)
  (retract ?cs)
)

(defrule rule47
   (declare (salience 74))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (or 
	(and(eq ?scenic "bright")(eq ?ageCat "Juviniles 1"))
	(and(eq ?scenic "bright")(eq ?ageCat "Youth"))
	)
  )
  =>
  (assert (coeffScenic (+ ?cs 0.18)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.18)  crlf)
  (retract ?cs)
)

(defrule rule48
   (declare (salience 73))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (and (eq ?scenic "normal")
  (eq ?ageCat "Junior 2")))
  =>
  (assert (coeffScenic (+ ?cs 0.17)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.17)  crlf)
  (retract ?cs)
)

(defrule rule49
   (declare (salience 72))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (and (eq ?scenic "normal")
  (eq ?ageCat "Junior 1")))
  =>
  (assert (coeffScenic (+ ?cs 0.16)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.16)  crlf)
  (retract ?cs)
)

(defrule rule50
   (declare (salience 71))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (and (eq ?scenic "normal")
  (eq ?ageCat "Juviniles 2")))
  =>
  (assert (coeffScenic (+ ?cs 0.15)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.15)  crlf)
  (retract ?cs)
)

(defrule rule51
   (declare (salience 70))
  (scenic ?scenic)
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (and (eq ?scenic "normal")
  (eq ?ageCat "Juviniles 1")))
  =>
  (assert (coeffScenic (+ ?cs 0.14)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.14)  crlf)
  (retract ?cs)
)

(defrule rule52
   (declare (salience 69))
  (ageCat ?ageCat)
  ?cs <- (coeffScenic ?coeffScenic)
  (test (or 
	(eq ?ageCat "Senior")
	(eq ?ageCat "Baby")
	)
  )
  =>
  (assert (coeffScenic (+ ?cs 0.13)))
  (printout t "----------------------" crlf) 
  (printout t "Scenic coeff: "(+ ?cs 0.13)  crlf)
  (retract ?cs)
)


(defrule rule53
   (declare (salience 68))
   (program ?program)
   (recognition ?recognition)
   ?cs <- (coeffScenic ?coeffScenic)
   (test (eq ?program "10 dance"))
   =>
   (assert (coeffScenic (+ ?cs (* 0.01 ?recognition))))
   (printout t "----------------------" crlf) 
   (printout t "Scenic coeff: "(+ ?cs (* 0.01 ?recognition))  crlf)
   (retract ?cs)
)

(defrule rule54
   (declare (salience 67))
   (program ?program)
   (recognition ?recognition)
   ?cs <- (coeffScenic ?coeffScenic)
   (test (neq ?program "10 dance"))
    =>
   (assert (coeffScenic (+ ?cs (+ 0.15 (* 0.01 ?recognition)))))
   (printout t "----------------------" crlf) 
   (printout t "Scenic coeff: "(+ ?cs (recognition ?recognition))  crlf)
   (retract ?cs)
)


;коэффициент возраста, глубина 2
(defrule rule55
   (declare (salience 66))
   (age ?age)
  (ageCat ?ageCat)
  (test (or 
		(and 
		 (or (eq ?ageCat "Youth 2") 
		  (eq ?ageCat "Junior 2")
		  (eq ?ageCat "Junior 1")
		  (eq ?ageCat "Juviniles 2")
		  (eq ?ageCat "Juviniles 1")
		 )
             	(eq (mod ?age 2) 0)
		)
		(and 
		  (eq ?ageCat "Youth")
             	  (neq ?age 18)
		)
	)
  )
  =>
  (assert (coeffAge 0.9))
  (printout t "----------------------" crlf) 
  (printout t "Age coeff: 0.9" crlf)
)

(defrule rule56
   (declare (salience 65))
  (ageCat ?ageCat)
  (test (eq ?ageCat "Baby"))
  =>
  (assert (coeffAge 1.15))
  (printout t "----------------------" crlf) 
  (printout t "Age coeff: 1.15" crlf)
)

(defrule rule57
   (declare (salience 64))
  (age ?age)
  (ageCat ?ageCat)
  (test (or 
		(and 
		 (or (eq ?ageCat "Youth 2") 
		  (eq ?ageCat "Junior 2")
		  (eq ?ageCat "Junior 1")
		  (eq ?ageCat "Juviniles 2")
		  (eq ?ageCat "Juviniles 1")
		 )
             	(eq (mod ?age 2) 1)
		)
		(and 
		  (eq ?ageCat "Youth")
             	  (eq ?age 18)
		)
	)
  )
  =>
  (assert (coeffAge 1.1)
  (printout t "----------------------" crlf) 
  (printout t "Age coeff: 1.1" crlf)
)

(defrule rule58
   (declare (salience 63))
    (age ?age)
   (ageCat ?ageCat)
    (test (and (eq ?ageCat "Adult")
    (< ?age 31)))
    =>
    (assert (coeffAge (+ 1 (* 0.02 (- ?age 20))))
    (printout t "----------------------" crlf) 
    (printout t "Age coeff: <1" crlf)
)

(defrule rule59
   (declare (salience 62))
    (age ?age)
   (ageCat ?ageCat)
    (test (and (eq ?ageCat "Adult")
    (> ?age 30)))
    =>
    (assert (coeffAge (- 1 (* 0.01 (- ?age 30))))
    (printout t "----------------------" crlf) 
    (printout t "Age coeff: >1" crlf)
)


(defrule rule60
   (declare (salience 61))
   (rangeOfClasses ?rangeOfClasses)
   ?cs <- (coeffScenic ?coeffScenic)
    (test (eq ?rangeOfClasses M))
    =>
    (assert (coeffScenic (+ ?cs 0.3))
    (printout t "----------------------" crlf) 
    (printout t "Scenic coeff: " (+ ?cs 0.3) crlf)
    (retract ?cs)
)

(defrule rule61
   (declare (salience 60))
   (rangeOfClasses ?rangeOfClasses)
   ?cs <- (coeffScenic ?coeffScenic)
    (test (eq ?rangeOfClasses S))
    =>
    (assert (coeffScenic (+ ?cs 0.29))
    (printout t "----------------------" crlf) 
    (printout t "Scenic coeff: " (+ ?cs 0.29) crlf)
    (retract ?cs)
)

(defrule rule62
   (declare (salience 59))
   (rangeOfClasses ?rangeOfClasses)
   ?cs <- (coeffScenic ?coeffScenic)
    (test (eq ?rangeOfClasses A))
    =>
    (assert (coeffScenic (+ ?cs 0.28))
    (printout t "----------------------" crlf) 
    (printout t "Scenic coeff: " (+ ?cs 0.28) crlf)
    (retract ?cs)
)

(defrule rule63
   (declare (salience 58))
   (rangeOfClasses ?rangeOfClasses)
   ?cs <- (coeffScenic ?coeffScenic)
    (test (eq ?rangeOfClasses B))
    =>
    (assert (coeffScenic (+ ?cs 0.27))
    (printout t "----------------------" crlf) 
    (printout t "Scenic coeff: " (+ ?cs 0.27) crlf)
    (retract ?cs)
)

(defrule rule64
   (declare (salience 57))
   (rangeOfClasses ?rangeOfClasses)
   ?cs <- (coeffScenic ?coeffScenic)
    (test (eq ?rangeOfClasses C))
    =>
    (assert (coeffScenic (+ ?cs 0.26))
    (printout t "----------------------" crlf) 
    (printout t "Scenic coeff: " (+ ?cs 0.26) crlf)
    (retract ?cs)
)

(defrule rule65
   (declare (salience 56))
   (rangeOfClasses ?rangeOfClasses)
   ?cs <- (coeffScenic ?coeffScenic)
    (test (eq ?rangeOfClasses D))
    =>
    (assert (coeffScenic (+ ?cs 0.25))
    (printout t "----------------------" crlf) 
    (printout t "Scenic coeff: " (+ ?cs 0.25) crlf)
    (retract ?cs)
)

(defrule rule66
   (declare (salience 55))
   (rangeOfClasses ?rangeOfClasses)
   ?cs <- (coeffScenic ?coeffScenic)
    (test (eq ?rangeOfClasses E))
    =>
    (assert (coeffScenic (+ ?cs 0.24))
    (printout t "----------------------" crlf) 
    (printout t "Scenic coeff: " (+ ?cs 0.24) crlf)
    (retract ?cs)
)

(defrule rule67
   (declare (salience 54))
   (rangeOfClasses ?rangeOfClasses)
   ?cs <- (coeffScenic ?coeffScenic)
    (test (eq ?rangeOfClasses H))
    =>
    (assert (coeffScenic (+ ?cs 0.23))
    (printout t "----------------------" crlf) 
    (printout t "Scenic coeff: " (+ ?cs 0.23) crlf)
    (retract ?cs)
)


;; выводы по технике, глубина 3
(defrule rule68
   (declare (salience 53))
    (technique ?technique)
   ?ca <- (coeffAge ?coeffAge)
    (test (> (/ ?technique ?ca) 8.75))
    =>
    (assert (tqclass 7))
    (printout t "----------------------" crlf) 
    (printout t "Real technique score: " (/ ?technique ?ca) crlf)
)

(defrule rule69
   (declare (salience 52))
    (technique ?technique)
   ?ca <- (coeffAge ?coeffAge)
    (test (and (> (/ ?technique ?ca) 7.5)
               (<= (/ ?technique ?ca) 8.75)
	  )
    )
    =>
    (assert (tqclass 6))
    (printout t "----------------------" crlf) 
    (printout t "Real technique score: " (/ ?technique ?ca) crlf)
)

(defrule rule70
   (declare (salience 51))
    (technique ?technique)
   ?ca <- (coeffAge ?coeffAge)
    (test (and (> (/ ?technique ?ca) 6.25)
               (<= (/ ?technique ?ca) 7.5)
	  )
    )
    =>
    (assert (tqclass 5))
    (printout t "----------------------" crlf) 
    (printout t "Real technique score: " (/ ?technique ?ca) crlf)
)

(defrule rule71
   (declare (salience 50))
    (technique ?technique)
   ?ca <- (coeffAge ?coeffAge)
    (test (and (> (/ ?technique ?ca) 5)
               (<= (/ ?technique ?ca) 6.25)
	  )
    )
    =>
    (assert (tqclass 4))
    (printout t "----------------------" crlf) 
    (printout t "Real technique score: " (/ ?technique ?ca) crlf)
)

(defrule rule72
   (declare (salience 49))
    (technique ?technique)
   ?ca <- (coeffAge ?coeffAge)
    (test (and (> (/ ?technique ?ca) 3.75)
               (<= (/ ?technique ?ca) 5)
	  )
    )
    =>
    (assert (tqclass 3))
    (printout t "----------------------" crlf) 
    (printout t "Real technique score: " (/ ?technique ?ca) crlf)
)

(defrule rule73
   (declare (salience 48))
    (technique ?technique)
   ?ca <- (coeffAge ?coeffAge)
    (test (and (> (/ ?technique ?ca) 2.5)
               (<= (/ ?technique ?ca) 3.75)
	  )
    )
    =>
    (assert (tqclass 2))
    (printout t "----------------------" crlf) 
    (printout t "Real technique score: " (/ ?technique ?ca) crlf)
)

(defrule rule74
   (declare (salience 47))
    (technique ?technique)
   ?ca <- (coeffAge ?coeffAge)
    (test (and (> (/ ?technique ?ca) 1.25)
               (<= (/ ?technique ?ca) 2.5)
	  )
    )
    =>
    (assert (tqclass 1))
    (printout t "----------------------" crlf) 
    (printout t "Real technique score: " (/ ?technique ?ca) crlf)
)

(defrule rule75
   (declare (salience 46))
   ?ca <- (coeffAge ?coeffAge)
    (technique ?technique)
    (test (<= (/ ?technique ?ca) 1.25))
    =>
    (assert (tqclass 0))
    (printout t "----------------------" crlf) 
    (printout t "Real technique score: " (/ ?technique ?ca) crlf)
)


(defrule rule76
   (declare (salience 45))
    (recognition ?recognition)
    ?cs <- (coeffScenic ?coeffScenic)
    (test (> ?cs 0.3))
    =>
    (assert (coeffRec (+ ?cs (* 0.01 ?recognition))))
    (printout t "----------------------" crlf) 
    (printout t "Recognition coeff: " (+ ?cs (* 0.01 ?recognition)) crlf)
)

(defrule rule77
   (declare (salience 44))
    (recognition ?recognition)
    ?cr <- (coeffRec ?coeffRec)
    ?cs <- (coeffScenic ?coeffScenic)
    (test (> ?cs 0.6))
    =>
    (assert (coeffRec (+ ?cr (* 0.01 ?recognition))))
    (printout t "----------------------" crlf) 
    (printout t "Recognition coeff: " (+ ?cr (* 0.01 ?recognition)) crlf)
    (retract ?cr)
)

(defrule rule78
   (declare (salience 43))
    (recognition ?recognition)
    ?cr <- (coeffRec ?coeffRec)
    ?cs <- (coeffScenic ?coeffScenic)
    (test (> ?cs 0.9))
    =>
    (assert (coeffRec (+ ?cr (* 0.01 ?recognition))))
    (printout t "----------------------" crlf) 
    (printout t "Recognition coeff: " (+ ?cr (* 0.01 ?recognition)) crlf)
    (retract ?cr)
)

(defrule rule79
   (declare (salience 42))
    (recognition ?recognition)
    ?cr <- (coeffRec ?coeffRec)
    ?cs <- (coeffScenic ?coeffScenic)
    (test (> ?cs 1.2))
    =>
    (assert (coeffRec (+ ?cr (* 0.01 ?recognition))))
    (printout t "----------------------" crlf) 
    (printout t "Recognition coeff: " (+ ?cr (* 0.01 ?recognition)) crlf)
    (retract ?cr)
)


;; выводы по музыке, глубина 4
(defrule rule80
   (declare (salience 41))
    (musicality ?musicality)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    (test (> (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 8.75))
    =>
    (assert (mmclass 7))
    (printout t "----------------------" crlf) 
    (printout t "Real musicality score: " (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) crlf)
)

(defrule rule81
   (declare (salience 40))
    (musicality ?musicality)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    (test (and (> (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 7.5)
               (<= (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 8.75)))
    =>
    (assert (mmclass 6))
    (printout t "----------------------" crlf) 
    (printout t "Real musicality score: " (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) crlf)
)

(defrule rule82
   (declare (salience 39))
    (musicality ?musicality)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    (test (and (> (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 6.25)
               (<= (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 7.5)))
    =>
    (assert (mmclass 5))
    (printout t "----------------------" crlf) 
    (printout t "Real musicality score: " (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) crlf)
)

(defrule rule83
   (declare (salience 38))
    (musicality ?musicality)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    (test (and (> (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 5)
               (<= (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 6.25)))
    =>
    (assert (mmclass 4))
    (printout t "----------------------" crlf) 
    (printout t "Real musicality score: " (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) crlf)
)

(defrule rule84
   (declare (salience 37))
    (musicality ?musicality)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    (test (and (> (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 3.75)
               (<= (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 5)))
    =>
    (assert (mmclass 3))
    (printout t "----------------------" crlf) 
    (printout t "Real musicality score: " (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) crlf)
)

(defrule rule85
   (declare (salience 36))
    (musicality ?musicality)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    (test (and (> (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 2.5)
               (<= (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 3.75)))
    =>
    (assert (mmclass 2))
    (printout t "----------------------" crlf) 
    (printout t "Real musicality score: " (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) crlf)
)

(defrule rule86
   (declare (salience 35))
    (musicality ?musicality)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    (test (and (> (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 1.25)
               (<= (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 2.5)))
    =>
    (assert (mmclass 1))
    (printout t "----------------------" crlf) 
    (printout t "Real musicality score: " (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) crlf)
)

(defrule rule87
   (declare (salience 34))
    (musicality ?musicality)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    (test (<= (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) 1.25))
    =>
    (assert (mmclass 0))
    (printout t "----------------------" crlf) 
    (printout t "Real musicality score: " (/ (+ (/ ?musicality ?ca) (/ ?musicality ?cs)) 2) crlf)
)


;; Выводы по взаимодействию, глубина 4
(defrule rule88
   (declare (salience 33))
    (partnerskills ?partnerskills)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (> (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) 8.75))
    =>
    (assert (psclass 7))
    (printout t "----------------------" crlf) 
    (printout t "Real partner-skills score: " (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) crlf)
)

(defrule rule89
   (declare (salience 32))
    (partnerskills ?partnerskills)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (and (> (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cs)) 3) 7.5)
               (<= (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cs)) 3) 8.75)))
    =>
    (assert (psclass 6))
    (printout t "----------------------" crlf) 
    (printout t "Real partner-skills score: " (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) crlf)
)

(defrule rule90
   (declare (salience 31))
    (partnerskills ?partnerskills)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (and (> (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) 6.25)
               (<= (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) 7.5)))
    =>
    (assert (psclass 5))
    (printout t "----------------------" crlf) 
    (printout t "Real partner-skills score: " (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) crlf)
)

(defrule rule91
   (declare (salience 30))
    (partnerskills ?partnerskills)
    (test (and (> (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) 5)
               (<= (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) 6.25)))
    =>
    (assert (psclass 4))
    (printout t "----------------------" crlf) 
    (printout t "Real partner-skills score: " (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) crlf)
)

(defrule rule92
   (declare (salience 29))
    (partnerskills ?partnerskills)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (and (> (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) 3.75)
               (<= (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) 5)))
    =>
    (assert (psclass 3))
    (printout t "----------------------" crlf) 
    (printout t "Real partner-skills score: " (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) crlf)
)

(defrule rule93
   (declare (salience 28))
    (partnerskills ?partnerskills)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (and (> (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) 2.5)
               (<= (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) 3.75)))
    =>
    (assert (psclass 2))
    (printout t "----------------------" crlf) 
    (printout t "Real partner-skills score: " (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) crlf)
)

(defrule rule94
   (declare (salience 27))
    (partnerskills ?partnerskills)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (and (> (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) 1.25)
               (<= (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) 2.5)))
    =>
    (assert (psclass 1))
    (printout t "----------------------" crlf) 
    (printout t "Real partner-skills score: " (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) crlf)
)

(defrule rule95
   (declare (salience 26))
    (partnerskills ?partnerskills)
    ?ca <- (coeffAge ?coeffAge)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (<= (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) 1.25))
    =>
    (assert (psclass 0))
    (printout t "----------------------" crlf) 
    (printout t "Real partner-skills score: " (/ (+ (/ ?partnerskills ?ca) (/ ?partnerskills ?cs) (/ ?partnerskills ?cr)) 3) crlf)
)


;; выводы по презентации, глубина 4
(defrule rule96
   (declare (salience 25))
    (presentation ?presentation)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (> (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 8.75))
    =>
    (assert (pcclass 7))
    (printout t "----------------------" crlf) 
    (printout t "Real choreography-presentation score: " (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) crlf)
)

(defrule rule97
   (declare (salience 24))
    (presentation ?presentation)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (and (> (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 7.5)
               (<= (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 8.75)))
    =>
    (assert (pcclass 6))
    (printout t "----------------------" crlf) 
    (printout t "Real choreography-presentation score: " (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) crlf)
)

(defrule rule98
    (declare (salience 23))
    (presentation ?presentation)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (and (> (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 6.25)
               (<= (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 7.5)))
    =>
    (assert (pcclass 5))
    (printout t "----------------------" crlf) 
    (printout t "Real choreography-presentation score: " (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) crlf)
)

(defrule rule99
    (declare (salience 22))
    (presentation ?presentation)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (and (> (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 5)
               (<= (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 6.25)))
    =>
    (assert (pcclass 4))
    (printout t "----------------------" crlf) 
    (printout t "Real choreography-presentation score: " (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) crlf)
)

(defrule rule100
    (declare (salience 21))
    (presentation ?presentation)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (and (> (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 3.75)
               (<= (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 5)))
    =>
    (assert (pcclass 3))
    (printout t "----------------------" crlf) 
    (printout t "Real choreography-presentation score: " (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) crlf)
)

(defrule rule101
    (declare (salience 20))
    (presentation ?presentation)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (and (> (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 2.5)
               (<= (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 3.75)))
    =>
    (assert (pcclass 2))
    (printout t "----------------------" crlf) 
    (printout t "Real choreography-presentation score: " (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) crlf)
)

(defrule rule102
    (declare (salience 19))
    (presentation ?presentation)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (and (> (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 1.25)
               (<= (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 2.5)))
    =>
    (assert (pcclass 1))
    (printout t "----------------------" crlf) 
    (printout t "Real choreography-presentation score: " (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) crlf)
)

(defrule rule103
    (declare (salience 18))
    (presentation ?presentation)
    ?cs <- (coeffScenic ?coeffScenic)
    ?cr <- (coeffRec ?coeffRec)
    (test (<= (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) 1.25))
    =>
    (assert (pcclass 0))
    (printout t "----------------------" crlf) 
    (printout t "Real choreography-presentation score: " (/ (+ (/ ?presentation ?cs) (/ ?presentation ?cr)) 2) crlf)
)


(defrule rule1041
    (declare (salience 17))
    (program ?program)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (neq ?program "10 dance") (>= (/ (+ (* 2 ?tqclass) (* 3 ?mmclass) ?psclass ?pcclass) 7) 7)(or (eq ?ageCat "Senior")(eq ?ageCat "Adult") (eq ?ageCat "Youth 2"))))
    =>
    (assert (master M))
    (printout t "----------------------" crlf) 
    (printout t "Class: M" crlf)
)

(defrule rule1042
    (declare (salience 16))
    (program ?program)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (neq ?program "10 dance") (or 
    (and(< (/ (+ (* 2 ?tqclass) (* 3 ?mmclass) ?psclass ?pcclass) 7) 7) (>= (/ (+ (* 2 ?tqclass) (* 3 ?mmclass) ?psclass ?pcclass) 7) 6)(or (eq ?ageCat "Senior")(eq ?ageCat "Adult") (eq ?ageCat "Youth 2")))
    (and (eq ?ageCat "Youth")(>= (/ (+ (* 2 ?tqclass) (* 3 ?mmclass) ?psclass ?pcclass) 7) 6)))))
    =>
    (assert (master S))
    (printout t "----------------------" crlf) 
    (printout t "Class: S" crlf)
)

(defrule rule1043
    (declare (salience 15))
    (program ?program)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (neq ?program "10 dance") (or 
    (and(< (/ (+ (* 2 ?tqclass) (* 3 ?mmclass) ?psclass ?pcclass) 7) 6) (>= (/ (+ (* 2 ?tqclass) (* 3 ?mmclass) ?psclass ?pcclass) 7) 5)(or (eq ?ageCat "Senior")(eq ?ageCat "Adult") (eq ?ageCat "Youth 2")(eq ?ageCat "Youth")))
    (and (eq ?ageCat "Junior 2")(>= (/ (+ (* 2 ?tqclass) (* 3 ?mmclass) ?psclass ?pcclass) 7) 5)))))
    =>
    (assert (master A))
    (printout t "----------------------" crlf) 
    (printout t "Class: A" crlf)
)

(defrule rule1044
    (declare (salience 14))
    (program ?program)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (neq ?program "10 dance") (or 
    (and(< (/ (+ (* 2 ?tqclass) (* 3 ?mmclass) ?psclass ?pcclass) 7) 5) (>= (/ (+ (* 2 ?tqclass) (* 3 ?mmclass) ?psclass ?pcclass) 7) 4)(or (eq ?ageCat "Senior")(eq ?ageCat "Adult") (eq ?ageCat "Youth 2")(eq ?ageCat "Youth")(eq ?ageCat "Junior 2")))
    (and (eq ?ageCat "Junior 1")(>= (/ (+ (* 2 ?tqclass) (* 3 ?mmclass) ?psclass ?pcclass) 7) 4)))))
    =>
    (assert (master B))
    (printout t "----------------------" crlf) 
    (printout t "Class: B" crlf)
)

(defrule rule1051
    (declare (salience 13))
    (program ?program)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (eq ?program "10 dance") (or
    (and(< (/ (+ (* 2 ?tqclass) (* 2 ?mmclass) ?psclass ?pcclass) 6) 4) (>= (/ (+ (* 2 ?tqclass) (* 2 ?mmclass) ?psclass ?pcclass) 6) 3)(or (eq ?ageCat "Senior")(eq ?ageCat "Adult") (eq ?ageCat "Youth 2")(eq ?ageCat "Youth")(eq ?ageCat "Junior 2")(eq ?ageCat "Junior 1")))
    (and (eq ?ageCat "Juvinilles 2")(>= (/ (+ (* 2 ?tqclass) (* 2 ?mmclass) ?psclass ?pcclass) 6) 3)))))
    =>
    (assert (master C))
    (printout t "----------------------" crlf) 
    (printout t "Class: C" crlf)
)

(defrule rule1052
    (declare (salience 12))
    (program ?program)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (eq ?program "10 dance") (or
    (and(< (/ (+ (* 2 ?tqclass) (* 2 ?mmclass) ?psclass ?pcclass) 6) 3) (>= (/ (+ (* 2 ?tqclass) (* 2 ?mmclass) ?psclass ?pcclass) 6) 2)(or (eq ?ageCat "Senior")(eq ?ageCat "Adult") (eq ?ageCat "Youth 2")(eq ?ageCat "Youth")(eq ?ageCat "Junior 2")(eq ?ageCat "Junior 1")(eq ?ageCat "Juvinilles 2")))
    (and (eq ?ageCat "Juvinilles 1")(>= (/ (+ (* 2 ?tqclass) (* 2 ?mmclass) ?psclass ?pcclass) 6) 2)))))
    =>
    (assert (master D))
    (printout t "----------------------" crlf) 
    (printout t "Class: D" crlf)
)

(defrule rule1053
    (declare (salience 11))
    (program ?program)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (eq ?program "10 dance") (or
    (and(< (/ (+ (* 2 ?tqclass) (* 2 ?mmclass) ?psclass ?pcclass) 6) 2) (>= (/ (+ (* 2 ?tqclass) (* 2 ?mmclass) ?psclass ?pcclass) 6) 1)(or (eq ?ageCat "Senior")(eq ?ageCat "Adult") (eq ?ageCat "Youth 2")(eq ?ageCat "Youth")(eq ?ageCat "Junior 2")(eq ?ageCat "Junior 1")(eq ?ageCat "Juvinilles 2")(eq ?ageCat "Juvinilles 1")))
    (and (eq ?ageCat "Baby")(>= (/ (+ (* 2 ?tqclass) (* 2 ?mmclass) ?psclass ?pcclass) 6) 1)))))
    =>
    (assert (master E))
    (printout t "----------------------" crlf) 
    (printout t "Class: E" crlf)
)

(defrule rule1054
    (declare (salience 10))
    (program ?program)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (eq ?program "10 dance") (or (< (/ (+ (* 2 ?tqclass) (* 3 ?mmclass) ?psclass ?pcclass) 7) 1) (>= (/ (+ (* 2 ?tqclass) (* 3 ?mmclass) ?psclass ?pcclass) 7) 0))))
    =>
    (assert (master H))
    (printout t "----------------------" crlf) 
    (printout t "Class: H" crlf)
)

(defrule rule106
    (declare (salience 9))
    (points ?points)
    ?m <- (master ?master)
    (ageCat ?ageCat)
    (test (and (or (eq ?ageCat "Adult") (eq ?ageCat "Youth 2"))
    (eq ?master M)
    (neq ?points 0)))
    =>
    (assert (master S))
    (retract ?m)
    (printout t "----------------------" crlf) 
    (printout t "Class: S" crlf)
)


(defrule rule107
    (declare (salience 8))
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and(eq ?ageCat "Adult") (= (+ ?tqclass ?mmclass ?psclass ?pcclass) 28)))
    =>
    (assert (rank "MSIC"))
    (printout t "----------------------" crlf) 
    (printout t "Rank: MSIC" crlf)
)

(defrule rule108
    (declare (salience 7))
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (or (eq ?ageCat "Adult") (eq ?ageCat "Youth 2"))(= (+ ?tqclass ?mmclass ?psclass ?pcclass) 27)))
    =>
    (assert (rank "MS"))
    (printout t "----------------------" crlf) 
    (printout t "Rank: MS" crlf)
)

(defrule rule109
    (declare (salience 6))
    (master ?master)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (or (eq ?ageCat "Adult") (eq ?ageCat "Youth 2") (eq ?ageCat "Youth"))(or (eq ?master M) (eq ?master S))(< (+ ?tqclass ?mmclass ?psclass ?pcclass) 27)))
    =>
    (assert (rank "CMS"))
    (printout t "----------------------" crlf) 
    (printout t "Rank: CMS" crlf)
)

(defrule rule110
    (declare (salience 5))
    (master ?master)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (or (eq ?ageCat "Adult") (eq ?ageCat "Youth 2") 
              (eq ?ageCat "Youth") (eq ?ageCat "Junior 2") 
              (eq ?ageCat "Junior 1"))
    (or (and (eq ?master B) (>= (+ ?tqclass ?mmclass ?psclass ?pcclass) 18))
              (eq ?master A))))
    =>
    (assert (rank "1 adult"))
    (printout t "----------------------" crlf) 
    (printout t "Rank: 1 adult" crlf)
)

(defrule rule111
    (declare (salience 4))
    (master ?master)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (or (eq ?ageCat "Adult") (eq ?ageCat "Youth 2") (eq ?ageCat "Youth") (eq ?ageCat "Junior 2") (eq ?ageCat "Junior 1"))
    (or (>= (+ ?tqclass ?mmclass ?psclass ?pcclass) 15) (= (+ ?tqclass ?mmclass ?psclass ?pcclass) 16))))
    =>
    (assert (rank "2 adult"))
    (printout t "----------------------" crlf) 
    (printout t "Rank: 2 adult" crlf)
)

(defrule rule112
    (declare (salience 3))
    (master ?master)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (and (or (eq ?ageCat "Adult") (eq ?ageCat "Youth 2") 
              (eq ?ageCat "Youth") (eq ?ageCat "Junior 2") 
              (eq ?ageCat "Junior 1"))
    (or (and (eq ?master B) (< (+ ?tqclass ?mmclass ?psclass ?pcclass) 15))
              (and (eq ?master C) (> (+ ?tqclass ?mmclass ?psclass ?pcclass) 10)))))
    =>
    (assert (rank "3 adult"))
    (printout t "----------------------" crlf) 
    (printout t "Rank: 3 adult" crlf)
)

(defrule rule113
    (declare (salience 2))
    (master ?master)
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (or (and (or (eq ?ageCat "Adult") (eq ?ageCat "Youth 2") 
                       (eq ?ageCat "Youth") (eq ?ageCat "Junior 2") 
                       (eq ?ageCat "Junior 1"))
                   (or (and (eq ?master C) (<= (+ ?tqclass ?mmclass ?psclass ?pcclass) 10))
                       (and (eq ?master D) (> (+ ?tqclass ?mmclass ?psclass ?pcclass) 6))))
              (and (or (eq ?ageCat "Juviniles 2") (eq ?ageCat "Juviniles 1"))
                   (or (eq ?master C) (and (eq ?master D) 
                                             (> (+ ?tqclass ?mmclass ?psclass ?pcclass) 6))))))
    =>
    (assert (rank "1 junior"))
    (printout t "----------------------" crlf) 
    (printout t "Rank: 1 junior"crlf)
)

(defrule rule114
    (declare (salience 1))
    (ageCat ?ageCat)
    (tqclass ?tqclass)
    (mmclass ?mmclass)
    (psclass ?psclass)
    (pcclass ?pcclass)
    (test (<= (+ ?tqclass ?mmclass ?psclass ?pcclass) 6))
    =>
    (assert (rank "2 junior"))
    (printout t "----------------------" crlf) 
    (printout t "Rank: 2 junior" crlf)
)

(defrule rule115
    (declare (salience 0))
    (master ?master)
    (ageCat ?ageCat)
    (test (or (eq ?ageCat "Senior") 
              (eq ?ageCat "Baby") 
              (eq ?master E) 
              (eq ?master H)))
    =>
    (assert (rank none))
    (printout t "----------------------" crlf) 
    (printout t "Rank: none" crlf)
)
