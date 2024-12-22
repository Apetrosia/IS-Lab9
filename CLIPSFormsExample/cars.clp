;========================================================================
; Этот блок реализует логику обмена информацией с графической оболочкой,
; а также механизм остановки и повторного пуска машины вывода
; Русский текст в комментариях разрешён!

(deftemplate ioproxy  ; шаблон факта-посредника для обмена информацией с GUI
	(slot fact-id)        ; теоретически тут id факта для изменения
	(multislot questions)   ; возможные ответы
	(multislot messages)  ; исходящие сообщения
	(slot reaction)       ; возможные ответы пользователя
	(slot value)          ; выбор пользователя
	(slot restore)        ; забыл зачем это поле
    (multislot answers)
)

; Собственно экземпляр факта ioproxy
(deffacts proxy-fact
	(ioproxy
		(fact-id 0112) ; это поле пока что не задействовано
		(value none)   ; значение пустое
		(messages)     ; мультислот messages изначально пуст
		(questions)
	)
)

(defrule clear-messages
	(declare (salience 90))
	?clear-msg-flg <- (clearmessage)
	?proxy <- (ioproxy)
	=>
	(modify ?proxy (messages))
	(retract ?clear-msg-flg)
	(printout t "Messages cleared ..." crlf)
)

(defrule set-output-and-halt
	(declare (salience 99))
	?current-message <- (sendmessagehalt ?new-msg)
	?proxy <- (ioproxy (messages $?msg-list))
	=>
	(printout t "Message set : " ?new-msg " ... halting ..." crlf)
	(modify ?proxy (messages ?new-msg))
	(retract ?current-message)
	(halt)
)

(defrule set-output-and-proceed
	(declare (salience 100))
	?current-message <- (sendmessage ?new-msg)
	?proxy <- (ioproxy (messages $?msg-list))
	=>
	(printout t "Message set : " ?new-msg " ... halting ..." crlf)
	(modify ?proxy (messages $?msg-list ?new-msg))
	(retract ?current-message)
)

(deftemplate question
    (slot value)
    (slot type)
)

(defrule set-question-and-halt
    (declare (salience 102))
    ?q <- (question (value ?val))
    ?proxy <- (ioproxy)
    =>
    (modify ?proxy (questions ?val))
    (retract ?q)
    (halt)
)

(defrule clear-questions
    (declare (salience 101))
    ?proxy <- (ioproxy (questions $?question-list&:(not(eq(length$ ?question-list) 0))))
    =>
    (modify ?proxy (questions))
)

;===========================================================================
(deftemplate input-question
	(multislot name)
	(slot certainty (type NUMBER))
)
(deftemplate possible-fact
	(multislot name)
)
(deftemplate fact
    (multislot name)
	(slot certainty (type NUMBER))
)

(deftemplate target
    (multislot name)
)

(deftemplate token
	(slot name)
)

; possible-fact - факт, который ВОЗМОЖНО может быть выводим
; fact - факт, который УЖЕ выведен в системе
; Разграничение можно использовать для отлова неверного ввода
(defrule match-facts
	(declare (salience 11))
	?f <- (fact (name ?val) (certainty ?c))
	?q <- (input-question (name ?n&?val) (certainty ?cert))
	=>
	(modify ?f (certainty ?cert))
	(retract ?q)
)

(defrule no-facts
      (declare (salience 10))
      ?q <- (input-question (name ?n))
      =>
      (retract ?q)
      (assert (sendmessagehalt (str-cat "The fact input is incorrect: " ?n)))
)

(defrule match-target
    (declare (salience 10))
    (target (name ?val))
    (fact (name ?n&?val))
    =>
    (do-for-all-facts ((?f fact)) TRUE (retract ?f))
    (assert (sendmessage "Целевой факт найден, стоп"))
)

(deffunction combine (?rule-confidence ?prev-confidence)
    (if (and (>= ?rule-confidence 0) (>= ?prev-confidence 0))
        then (return (- (+ ?rule-confidence ?prev-confidence) (* ?rule-confidence ?prev-confidence)))
        else
          (if (and (< ?rule-confidence 0) (< ?prev-confidence 0)) then
          (return (+ (+ ?rule-confidence ?prev-confidence) (* ?rule-confidence ?prev-confidence)))
          else (return (/ (+ ?rule-confidence ?prev-confidence) (- 1 (min (abs ?rule-confidence) (abs ?prev-confidence) )) ))
          ))
)

(deftemplate token
	(slot name))

;===========================================================================


(deffacts possible-facts
(possible-fact (name "студент") (certainty 0.0))
(possible-fact (name "работник") (certainty 0.0))
(possible-fact (name "бизнесмен") (certainty 0.0))

(possible-fact (name "холост") (certainty 0.0))
(possible-fact (name "есть семья") (certainty 0.0))
(possible-fact (name "есть дети") (certainty 0.0))

(possible-fact (name "мало общения") (certainty 0.0))
(possible-fact (name "средне общения") (certainty 0.0))
(possible-fact (name "много общения") (certainty 0.0))

(possible-fact (name "не везу грузы") (certainty 0.0))
(possible-fact (name "иногда везу грузы") (certainty 0.0))
(possible-fact (name "часто везу грузы") (certainty 0.0))

(possible-fact (name "не еду загород") (certainty 0.0))
(possible-fact (name "иногда еду загород") (certainty 0.0))
(possible-fact (name "часто еду загород") (certainty 0.0))

(possible-fact (name "не еду на природу") (certainty 0.0))
(possible-fact (name "иногда еду на природу") (certainty 0.0))
(possible-fact (name "часто еду на природу") (certainty 0.0))


(possible-fact (name "кузов SUV") (certainty 0.0))
(possible-fact (name "кузов купе") (certainty 0.0))
(possible-fact (name "кузов хетчбек") (certainty 0.0))
(possible-fact (name "кузов седан") (certainty 0.0))
(possible-fact (name "кузов универсал") (certainty 0.0))

(possible-fact (name "количество дверей две") (certainty 0.0))
(possible-fact (name "количество дверей три") (certainty 0.0))
(possible-fact (name "количество дверей четыре") (certainty 0.0))
(possible-fact (name "количество дверей пять") (certainty 0.0))

(possible-fact (name "класс A (мини") (certainty 0.0))
(possible-fact (name "класс B (небольшие городские автомобили)") (certainty 0.0))
(possible-fact (name "класс C (низший средний класс или гольф") (certainty 0.0))
(possible-fact (name "класс D (полноценный средний класс)") (certainty 0.0))
(possible-fact (name "класс E (бизнес") (certainty 0.0))
(possible-fact (name "класс F (люксовые автомобили)") (certainty 0.0))
(possible-fact (name "класс J (внедорожники)") (certainty 0.0))
(possible-fact (name "класс M (минивэны)") (certainty 0.0))
(possible-fact (name "класс S (спорткары)") (certainty 0.0))

(possible-fact (name "привод передний") (certainty 0.0))
(possible-fact (name "привод задний") (certainty 0.0))
(possible-fact (name "привод полный") (certainty 0.0))

(possible-fact (name "коробка передач МКПП") (certainty 0.0))
(possible-fact (name "коробка передач АКПП") (certainty 0.0))

(possible-fact (name "цена низкая") (certainty 0.0))
(possible-fact (name "цена средняя") (certainty 0.0))
(possible-fact (name "цена высокая") (certainty 0.0))

(possible-fact (name "VW Passat") (certainty 0.0))
(possible-fact (name "VW Tiguan") (certainty 0.0))
(possible-fact (name "VW Touareg") (certainty 0.0))
(possible-fact (name "VW Golf") (certainty 0.0))
(possible-fact (name "BMW X6") (certainty 0.0))
(possible-fact (name "BMW Z4") (certainty 0.0))
(possible-fact (name "Daewoo Matiz") (certainty 0.0))
(possible-fact (name "Hyundai Solaris") (certainty 0.0))
(possible-fact (name "Lada Niva") (certainty 0.0))
)
(deffacts rule-token-1 (token (name "rule_1")))
(defrule rule1_2
(fact (name "бизнесмен") (certainty ?cTAB))
(test (> (abs ?cTAB) 0.4))
?f <- (fact (name "коробка передач АКПП") (certainty ?cf_))
?tk <- (token (name "rule_1"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAB) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "бизнесмен -> коробка передач АКПП" " (коробка передач АКПП " ?cnew ")"))))
(deffacts rule-token-2 (token (name "rule_2")))
(defrule rule2_2
(fact (name "студент") (certainty ?cTAS))
(test (> (abs ?cTAS) 0.4))
?f <- (fact (name "коробка передач МКПП") (certainty ?cf_))
?tk <- (token (name "rule_2"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAS) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "студент -> коробка передач МКПП" " (коробка передач МКПП " ?cnew ")"))))
(deffacts rule-token-3 (token (name "rule_3")))
(defrule rule3_2
(fact (name "студент") (certainty ?cTAS))
(test (> (abs ?cTAS) 0.4))
?f <- (fact (name "коробка передач АКПП") (certainty ?cf_))
?tk <- (token (name "rule_3"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAS) 0.3) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "студент -> коробка передач АКПП" " (коробка передач АКПП " ?cnew ")"))))
(deffacts rule-token-4 (token (name "rule_4")))
(defrule rule4_2
(fact (name "работник") (certainty ?cTAW))
(test (> (abs ?cTAW) 0.4))
?f <- (fact (name "коробка передач МКПП") (certainty ?cf_))
?tk <- (token (name "rule_4"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAW) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "работник -> коробка передач МКПП" " (коробка передач МКПП " ?cnew ")"))))
(deffacts rule-token-5 (token (name "rule_5")))
(defrule rule5_2
(fact (name "работник") (certainty ?cTAW))
(test (> (abs ?cTAW) 0.4))
?f <- (fact (name "коробка передач АКПП") (certainty ?cf_))
?tk <- (token (name "rule_5"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAW) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "работник -> коробка передач АКПП" " (коробка передач АКПП " ?cnew ")"))))

(deffacts rule-token-8 (token (name "rule_8")))
(defrule rule8_2
(fact (name "бизнесмен") (certainty ?cTAB))
(test (> (abs ?cTAB) 0.4))
?f <- (fact (name "цена высокая") (certainty ?cf_))
?tk <- (token (name "rule_8"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAB) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "бизнесмен -> цена высокая" " (цена высокая " ?cnew ")"))))
(deffacts rule-token-9 (token (name "rule_9")))
(defrule rule9_2
(fact (name "бизнесмен") (certainty ?cTAB))
(test (> (abs ?cTAB) 0.4))
?f <- (fact (name "цена средняя") (certainty ?cf_))
?tk <- (token (name "rule_9"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAB) 0.1) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "бизнесмен -> цена средняя" " (цена средняя " ?cnew ")"))))
(deffacts rule-token-10 (token (name "rule_10")))
(defrule rule10_2
(fact (name "работник") (certainty ?cTAW))
(test (> (abs ?cTAW) 0.4))
?f <- (fact (name "цена средняя") (certainty ?cf_))
?tk <- (token (name "rule_10"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAW) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "работник -> цена средняя" " (цена средняя " ?cnew ")"))))
(deffacts rule-token-11 (token (name "rule_11")))
(defrule rule11_2
(fact (name "работник") (certainty ?cTAW))
(test (> (abs ?cTAW) 0.4))
?f <- (fact (name "цена высокая") (certainty ?cf_))
?tk <- (token (name "rule_11"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAW) 0.2) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "работник -> цена высокая" " (цена высокая " ?cnew ")"))))
(deffacts rule-token-12 (token (name "rule_12")))
(defrule rule12_2
(fact (name "студент") (certainty ?cTAS))
(test (> (abs ?cTAS) 0.4))
?f <- (fact (name "цена низкая") (certainty ?cf_))
?tk <- (token (name "rule_12"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "студент -> цена низкая" " (цена низкая " ?cnew ")"))))
(deffacts rule-token-13 (token (name "rule_13")))
(defrule rule13_2
(fact (name "студент") (certainty ?cTAS))
(test (> (abs ?cTAS) 0.4))
?f <- (fact (name "цена средняя") (certainty ?cf_))
?tk <- (token (name "rule_13"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAS) 0.1) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "студент -> цена средняя" " (цена средняя " ?cnew ")"))))

(deffacts rule-token-16 (token (name "rule_16")))
(defrule rule16_2
(fact (name "не еду на природу") (certainty ?cTNN))
(test (> (abs ?cTNN) 0.4))
?f <- (fact (name "привод передний") (certainty ?cf_))
?tk <- (token (name "rule_16"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNN) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "не еду на природу -> привод передний" " (привод передний " ?cnew ")"))))
(deffacts rule-token-17 (token (name "rule_17")))
(defrule rule17_2
(fact (name "иногда еду на природу") (certainty ?cTNR))
(test (> (abs ?cTNR) 0.4))
?f <- (fact (name "привод передний") (certainty ?cf_))
?tk <- (token (name "rule_17"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNR) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "иногда еду на природу -> привод передний" " (привод передний " ?cnew ")"))))
(deffacts rule-token-18 (token (name "rule_18")))
(defrule rule18_2
(fact (name "иногда еду на природу") (certainty ?cTNR))
(test (> (abs ?cTNR) 0.4))
?f <- (fact (name "привод задний") (certainty ?cf_))
?tk <- (token (name "rule_18"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNR) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "иногда еду на природу -> привод задний" " (привод задний " ?cnew ")"))))
(deffacts rule-token-19 (token (name "rule_19")))
(defrule rule19_2
(fact (name "не еду на природу") (certainty ?cTNN))
(test (> (abs ?cTNN) 0.4))
?f <- (fact (name "привод задний") (certainty ?cf_))
?tk <- (token (name "rule_19"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNN) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "не еду на природу -> привод задний" " (привод задний " ?cnew ")"))))
(deffacts rule-token-20 (token (name "rule_20")))
(defrule rule20_2
(fact (name "часто еду на природу") (certainty ?cTNO))
(test (> (abs ?cTNO) 0.4))
?f <- (fact (name "привод полный") (certainty ?cf_))
?tk <- (token (name "rule_20"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNO) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "часто еду на природу -> привод полный" " (привод полный " ?cnew ")"))))
(deffacts rule-token-21 (token (name "rule_21")))
(defrule rule21_2
(fact (name "часто еду на природу") (certainty ?cTNO))
(test (> (abs ?cTNO) 0.4))
?f <- (fact (name "привод полный") (certainty ?cf_))
?tk <- (token (name "rule_21"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNO) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "часто еду на природу -> привод полный" " (привод полный " ?cnew ")"))))
(deffacts rule-token-22 (token (name "rule_22")))
(defrule rule22_2
(fact (name "часто еду на природу") (certainty ?cTNO))
(test (> (abs ?cTNO) 0.4))
?f <- (fact (name "привод полный") (certainty ?cf_))
?tk <- (token (name "rule_22"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNO) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "часто еду на природу -> привод полный" " (привод полный " ?cnew ")"))))

(deffacts rule-token-25 (token (name "rule_25")))
(defrule rule25_2
(fact (name "холост") (certainty ?cFSS))
(test (> (abs ?cFSS) 0.4))
?f <- (fact (name "количество дверей две") (certainty ?cf_))
?tk <- (token (name "rule_25"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cFSS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "холост -> количество дверей две" " (количество дверей две " ?cnew ")"))))
(deffacts rule-token-26 (token (name "rule_26")))
(defrule rule26_2
(fact (name "мало общения") (certainty ?cCR))
(test (> (abs ?cCR) 0.4))
?f <- (fact (name "количество дверей две") (certainty ?cf_))
?tk <- (token (name "rule_26"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCR) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "мало общения -> количество дверей две" " (количество дверей две " ?cnew ")"))))
(deffacts rule-token-27 (token (name "rule_27")))
(defrule rule27_2
(fact (name "средне общения") (certainty ?cCM))
(test (> (abs ?cCM) 0.4))
?f <- (fact (name "количество дверей две") (certainty ?cf_))
?tk <- (token (name "rule_27"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCM) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "средне общения -> количество дверей две" " (количество дверей две " ?cnew ")"))))
(deffacts rule-token-28 (token (name "rule_28")))
(defrule rule28_2
(fact (name "холост") (certainty ?cFSS))
(test (> (abs ?cFSS) 0.4))
?f <- (fact (name "количество дверей три") (certainty ?cf_))
?tk <- (token (name "rule_28"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cFSS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "холост -> количество дверей три" " (количество дверей три " ?cnew ")"))))
(deffacts rule-token-29 (token (name "rule_29")))
(defrule rule29_2
(fact (name "холост") (certainty ?cFSS))
(test (> (abs ?cFSS) 0.4))
?f <- (fact (name "количество дверей две") (certainty ?cf_))
?tk <- (token (name "rule_29"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cFSS) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "холост -> количество дверей две" " (количество дверей две " ?cnew ")"))))
(deffacts rule-token-30 (token (name "rule_30")))
(defrule rule30_2
(fact (name "средне общения") (certainty ?cCM))
(test (> (abs ?cCM) 0.4))
?f <- (fact (name "количество дверей три") (certainty ?cf_))
?tk <- (token (name "rule_30"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCM) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "средне общения -> количество дверей три" " (количество дверей три " ?cnew ")"))))
(deffacts rule-token-31 (token (name "rule_31")))
(defrule rule31_2
(fact (name "средне общения") (certainty ?cCM))
(test (> (abs ?cCM) 0.4))
?f <- (fact (name "количество дверей две") (certainty ?cf_))
?tk <- (token (name "rule_31"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCM) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "средне общения -> количество дверей две" " (количество дверей две " ?cnew ")"))))
(deffacts rule-token-32 (token (name "rule_32")))
(defrule rule32_2
(fact (name "мало общения") (certainty ?cCR))
(test (> (abs ?cCR) 0.4))
?f <- (fact (name "количество дверей три") (certainty ?cf_))
?tk <- (token (name "rule_32"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCR) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "мало общения -> количество дверей три" " (количество дверей три " ?cnew ")"))))
(deffacts rule-token-33 (token (name "rule_33")))
(defrule rule33_2
(fact (name "мало общения") (certainty ?cCR))
(test (> (abs ?cCR) 0.4))
?f <- (fact (name "количество дверей две") (certainty ?cf_))
?tk <- (token (name "rule_33"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCR) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "мало общения -> количество дверей две" " (количество дверей две " ?cnew ")"))))
(deffacts rule-token-34 (token (name "rule_34")))
(defrule rule34_2
(fact (name "есть семья") (certainty ?cFSF))
(test (> (abs ?cFSF) 0.4))
?f <- (fact (name "количество дверей четыре") (certainty ?cf_))
?tk <- (token (name "rule_34"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cFSF) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "есть семья -> количество дверей четыре" " (количество дверей четыре " ?cnew ")"))))
(deffacts rule-token-35 (token (name "rule_35")))
(defrule rule35_2
(fact (name "есть дети") (certainty ?cFSC))
(test (> (abs ?cFSC) 0.4))
?f <- (fact (name "количество дверей четыре") (certainty ?cf_))
?tk <- (token (name "rule_35"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cFSC) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "есть дети -> количество дверей четыре" " (количество дверей четыре " ?cnew ")"))))
(deffacts rule-token-36 (token (name "rule_36")))
(defrule rule36_2
(fact (name "средне общения") (certainty ?cCM))
(test (> (abs ?cCM) 0.4))
?f <- (fact (name "количество дверей четыре") (certainty ?cf_))
?tk <- (token (name "rule_36"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCM) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "средне общения -> количество дверей четыре" " (количество дверей четыре " ?cnew ")"))))
(deffacts rule-token-37 (token (name "rule_37")))
(defrule rule37_2
(fact (name "много общения") (certainty ?cCO))
(test (> (abs ?cCO) 0.4))
?f <- (fact (name "количество дверей четыре") (certainty ?cf_))
?tk <- (token (name "rule_37"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCO) 0.95) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "много общения -> количество дверей четыре" " (количество дверей четыре " ?cnew ")"))))
(deffacts rule-token-38 (token (name "rule_38")))
(defrule rule38_2
(fact (name "есть семья") (certainty ?cFSF))
(test (> (abs ?cFSF) 0.4))
?f <- (fact (name "количество дверей пять") (certainty ?cf_))
?tk <- (token (name "rule_38"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cFSF) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "есть семья -> количество дверей пять" " (количество дверей пять " ?cnew ")"))))
(deffacts rule-token-39 (token (name "rule_39")))
(defrule rule39_2
(fact (name "много общения") (certainty ?cCO))
(test (> (abs ?cCO) 0.4))
?f <- (fact (name "количество дверей пять") (certainty ?cf_))
?tk <- (token (name "rule_39"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCO) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "много общения -> количество дверей пять" " (количество дверей пять " ?cnew ")"))))
(deffacts rule-token-40 (token (name "rule_40")))
(defrule rule40_2
(fact (name "средне общения") (certainty ?cCM))
(test (> (abs ?cCM) 0.4))
?f <- (fact (name "количество дверей пять") (certainty ?cf_))
?tk <- (token (name "rule_40"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCM) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "средне общения -> количество дверей пять" " (количество дверей пять " ?cnew ")"))))

(deffacts rule-token-43 (token (name "rule_43")))
(defrule rule43_2
(fact (name "цена высокая") (certainty ?cHP))
(test (> (abs ?cHP) 0.4))
(fact (name "количество дверей четыре") (certainty ?cFRD))
(test (> (abs ?cFRD) 0.4))
?f <- (fact (name "кузов SUV") (certainty ?cf_))
?tk <- (token (name "rule_43"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cHP ?cFRD) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена высокая, количество дверей четыре -> кузов SUV" " (кузов SUV " ?cnew ")"))))
(deffacts rule-token-44 (token (name "rule_44")))
(defrule rule44_2
(fact (name "цена высокая") (certainty ?cHP))
(test (> (abs ?cHP) 0.4))
(fact (name "количество дверей пять") (certainty ?cFD))
(test (> (abs ?cFD) 0.4))
?f <- (fact (name "кузов SUV") (certainty ?cf_))
?tk <- (token (name "rule_44"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cHP ?cFD) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена высокая, количество дверей пять -> кузов SUV" " (кузов SUV " ?cnew ")"))))
(deffacts rule-token-45 (token (name "rule_45")))
(defrule rule45_2
(fact (name "цена высокая") (certainty ?cHP))
(test (> (abs ?cHP) 0.4))
(fact (name "количество дверей три") (certainty ?cTHD))
(test (> (abs ?cTHD) 0.4))
?f <- (fact (name "кузов купе") (certainty ?cf_))
?tk <- (token (name "rule_45"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cHP ?cTHD) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена высокая, количество дверей три -> кузов купе" " (кузов купе " ?cnew ")"))))
(deffacts rule-token-46 (token (name "rule_46")))
(defrule rule46_2
(fact (name "цена высокая") (certainty ?cHP))
(test (> (abs ?cHP) 0.4))
(fact (name "количество дверей две") (certainty ?cTWD))
(test (> (abs ?cTWD) 0.4))
?f <- (fact (name "кузов купе") (certainty ?cf_))
?tk <- (token (name "rule_46"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cHP ?cTWD) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена высокая, количество дверей две -> кузов купе" " (кузов купе " ?cnew ")"))))
(deffacts rule-token-47 (token (name "rule_47")))
(defrule rule47_2
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "иногда везу грузы") (certainty ?cCGR))
(test (> (abs ?cCGR) 0.4))
?f <- (fact (name "кузов хетчбек") (certainty ?cf_))
?tk <- (token (name "rule_47"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cCGR) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, иногда везу грузы -> кузов хетчбек" " (кузов хетчбек " ?cnew ")"))))
(deffacts rule-token-48 (token (name "rule_48")))
(defrule rule48_2
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "иногда везу грузы") (certainty ?cCGR))
(test (> (abs ?cCGR) 0.4))
?f <- (fact (name "кузов хетчбек") (certainty ?cf_))
?tk <- (token (name "rule_48"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cCGR) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, иногда везу грузы -> кузов хетчбек" " (кузов хетчбек " ?cnew ")"))))
(deffacts rule-token-49 (token (name "rule_49")))
(defrule rule49_2
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "иногда еду загород") (certainty ?cTCR))
(test (> (abs ?cTCR) 0.4))
?f <- (fact (name "кузов хетчбек") (certainty ?cf_))
?tk <- (token (name "rule_49"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cTCR) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, иногда еду загород -> кузов хетчбек" " (кузов хетчбек " ?cnew ")"))))
(deffacts rule-token-50 (token (name "rule_50")))
(defrule rule50_2
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "иногда еду загород") (certainty ?cTCR))
(test (> (abs ?cTCR) 0.4))
?f <- (fact (name "кузов хетчбек") (certainty ?cf_))
?tk <- (token (name "rule_50"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cTCR) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, иногда еду загород -> кузов хетчбек" " (кузов хетчбек " ?cnew ")"))))
(deffacts rule-token-51 (token (name "rule_51")))
(defrule rule51_2
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "не еду загород") (certainty ?cTCN))
(test (> (abs ?cTCN) 0.4))
?f <- (fact (name "кузов седан") (certainty ?cf_))
?tk <- (token (name "rule_51"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cTCN) 0.95) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, не еду загород -> кузов седан" " (кузов седан " ?cnew ")"))))
(deffacts rule-token-52 (token (name "rule_52")))
(defrule rule52_2
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "не везу грузы") (certainty ?cCGN))
(test (> (abs ?cCGN) 0.4))
?f <- (fact (name "кузов седан") (certainty ?cf_))
?tk <- (token (name "rule_52"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cCGN) 0.95) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, не везу грузы -> кузов седан" " (кузов седан " ?cnew ")"))))
(deffacts rule-token-53 (token (name "rule_53")))
(defrule rule53_2
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "не везу грузы") (certainty ?cCGN))
(test (> (abs ?cCGN) 0.4))
?f <- (fact (name "кузов седан") (certainty ?cf_))
?tk <- (token (name "rule_53"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cCGN) 0.95) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, не везу грузы -> кузов седан" " (кузов седан " ?cnew ")"))))
(deffacts rule-token-54 (token (name "rule_54")))
(defrule rule54_2
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "не еду загород") (certainty ?cTCN))
(test (> (abs ?cTCN) 0.4))
?f <- (fact (name "кузов седан") (certainty ?cf_))
?tk <- (token (name "rule_54"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cTCN) 0.95) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, не еду загород -> кузов седан" " (кузов седан " ?cnew ")"))))
(deffacts rule-token-55 (token (name "rule_55")))
(defrule rule55_2
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "часто везу грузы") (certainty ?cCGO))
(test (> (abs ?cCGO) 0.4))
?f <- (fact (name "кузов универсал") (certainty ?cf_))
?tk <- (token (name "rule_55"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cCGO) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, часто везу грузы -> кузов универсал" " (кузов универсал " ?cnew ")"))))
(deffacts rule-token-56 (token (name "rule_56")))
(defrule rule56_2
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "часто еду загород") (certainty ?cTCO))
(test (> (abs ?cTCO) 0.4))
?f <- (fact (name "кузов универсал") (certainty ?cf_))
?tk <- (token (name "rule_56"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cTCO) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, часто еду загород -> кузов универсал" " (кузов универсал " ?cnew ")"))))
(deffacts rule-token-57 (token (name "rule_57")))
(defrule rule57_2
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "часто еду загород") (certainty ?cTCO))
(test (> (abs ?cTCO) 0.4))
?f <- (fact (name "кузов универсал") (certainty ?cf_))
?tk <- (token (name "rule_57"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cTCO) 0.65) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, часто еду загород -> кузов универсал" " (кузов универсал " ?cnew ")"))))
(deffacts rule-token-58 (token (name "rule_58")))
(defrule rule58_2
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "часто везу грузы") (certainty ?cCGO))
(test (> (abs ?cCGO) 0.4))
?f <- (fact (name "кузов универсал") (certainty ?cf_))
?tk <- (token (name "rule_58"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cCGO) 0.65) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, часто везу грузы -> кузов универсал" " (кузов универсал " ?cnew ")"))))

(deffacts rule-token-61 (token (name "rule_61")))
(defrule rule61_2
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
?f <- (fact (name "класс A (мини") (certainty ?cf_))
?tk <- (token (name "rule_61"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая -> класс A (мини" " (класс A (мини " ?cnew ")"))))
(deffacts rule-token-62 (token (name "rule_62")))
(defrule rule62_2
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
?f <- (fact (name "класс B (небольшие городские автомобили)") (certainty ?cf_))
?tk <- (token (name "rule_62"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cBH) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, кузов хетчбек -> класс B (небольшие городские автомобили)" " (класс B (небольшие городские автомобили) " ?cnew ")"))))
(deffacts rule-token-63 (token (name "rule_63")))
(defrule rule63_2
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "класс B (небольшие городские автомобили)") (certainty ?cf_))
?tk <- (token (name "rule_63"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cBS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, кузов седан -> класс B (небольшие городские автомобили)" " (класс B (небольшие городские автомобили) " ?cnew ")"))))
(deffacts rule-token-64 (token (name "rule_64")))
(defrule rule64_2
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
?f <- (fact (name "класс B (небольшие городские автомобили)") (certainty ?cf_))
?tk <- (token (name "rule_64"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cBH) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, кузов хетчбек -> класс B (небольшие городские автомобили)" " (класс B (небольшие городские автомобили) " ?cnew ")"))))
(deffacts rule-token-65 (token (name "rule_65")))
(defrule rule65_2
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "класс B (небольшие городские автомобили)") (certainty ?cf_))
?tk <- (token (name "rule_65"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cBS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, кузов седан -> класс B (небольшие городские автомобили)" " (класс B (небольшие городские автомобили) " ?cnew ")"))))
(deffacts rule-token-66 (token (name "rule_66")))
(defrule rule66_2
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
(fact (name "количество дверей три") (certainty ?cTHD))
(test (> (abs ?cTHD) 0.4))
?f <- (fact (name "класс C (низший средний класс или гольф") (certainty ?cf_))
?tk <- (token (name "rule_66"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cBH ?cTHD) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, кузов хетчбек, количество дверей три -> класс C (низший средний класс или гольф" " (класс C (низший средний класс или гольф " ?cnew ")"))))
(deffacts rule-token-67 (token (name "rule_67")))
(defrule rule67_2
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
(fact (name "количество дверей три") (certainty ?cTHD))
(test (> (abs ?cTHD) 0.4))
?f <- (fact (name "класс C (низший средний класс или гольф") (certainty ?cf_))
?tk <- (token (name "rule_67"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cBH ?cTHD) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, кузов хетчбек, количество дверей три -> класс C (низший средний класс или гольф" " (класс C (низший средний класс или гольф " ?cnew ")"))))
(deffacts rule-token-68 (token (name "rule_68")))
(defrule rule68_2
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "класс D (полноценный средний класс)") (certainty ?cf_))
?tk <- (token (name "rule_68"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cBS) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, кузов седан -> класс D (полноценный средний класс)" " (класс D (полноценный средний класс) " ?cnew ")"))))
(deffacts rule-token-69 (token (name "rule_69")))
(defrule rule69_2
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "кузов универсал") (certainty ?cBU))
(test (> (abs ?cBU) 0.4))
?f <- (fact (name "класс D (полноценный средний класс)") (certainty ?cf_))
?tk <- (token (name "rule_69"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cBU) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, кузов универсал -> класс D (полноценный средний класс)" " (класс D (полноценный средний класс) " ?cnew ")"))))
(deffacts rule-token-70 (token (name "rule_70")))
(defrule rule70_2
(fact (name "цена высокая") (certainty ?cHP))
(test (> (abs ?cHP) 0.4))
(fact (name "кузов SUV") (certainty ?cSUV))
(test (> (abs ?cSUV) 0.4))
?f <- (fact (name "класс E (бизнес") (certainty ?cf_))
?tk <- (token (name "rule_70"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cHP ?cSUV) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена высокая, кузов SUV -> класс E (бизнес" " (класс E (бизнес " ?cnew ")"))))
(deffacts rule-token-71 (token (name "rule_71")))
(defrule rule71_2
(fact (name "привод полный") (certainty ?cAWD))
(test (> (abs ?cAWD) 0.4))
?f <- (fact (name "класс J (внедорожники)") (certainty ?cf_))
?tk <- (token (name "rule_71"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAWD) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "привод полный -> класс J (внедорожники)" " (класс J (внедорожники) " ?cnew ")"))))
(deffacts rule-token-72 (token (name "rule_72")))
(defrule rule72_2
(fact (name "цена высокая") (certainty ?cHP))
(test (> (abs ?cHP) 0.4))
(fact (name "кузов купе") (certainty ?cCP))
(test (> (abs ?cCP) 0.4))
?f <- (fact (name "класс S (спорткары)") (certainty ?cf_))
?tk <- (token (name "rule_72"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cHP ?cCP) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена высокая, кузов купе -> класс S (спорткары)" " (класс S (спорткары) " ?cnew ")"))))

(deffacts rule-token-75 (token (name "rule_75")))
(defrule rule75_2
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс D (полноценный средний класс)") (certainty ?cD))
(test (> (abs ?cD) 0.4))
(fact (name "кузов универсал") (certainty ?cBU))
(test (> (abs ?cBU) 0.4))
?f <- (fact (name "VW Passat") (certainty ?cf_))
?tk <- (token (name "rule_75"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMT ?cD ?cBU) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач МКПП, класс D (полноценный средний класс), кузов универсал -> VW Passat" " (VW Passat " ?cnew ")"))))
(deffacts rule-token-76 (token (name "rule_76")))
(defrule rule76_2
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс D (полноценный средний класс)") (certainty ?cD))
(test (> (abs ?cD) 0.4))
(fact (name "кузов универсал") (certainty ?cBU))
(test (> (abs ?cBU) 0.4))
?f <- (fact (name "VW Passat") (certainty ?cf_))
?tk <- (token (name "rule_76"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cD ?cBU) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс D (полноценный средний класс), кузов универсал -> VW Passat" " (VW Passat " ?cnew ")"))))
(deffacts rule-token-77 (token (name "rule_77")))
(defrule rule77_2
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс D (полноценный средний класс)") (certainty ?cD))
(test (> (abs ?cD) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "VW Passat") (certainty ?cf_))
?tk <- (token (name "rule_77"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cD ?cBS) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс D (полноценный средний класс), кузов седан -> VW Passat" " (VW Passat " ?cnew ")"))))
(deffacts rule-token-78 (token (name "rule_78")))
(defrule rule78_2
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс D (полноценный средний класс)") (certainty ?cD))
(test (> (abs ?cD) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "VW Passat") (certainty ?cf_))
?tk <- (token (name "rule_78"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMT ?cD ?cBS) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач МКПП, класс D (полноценный средний класс), кузов седан -> VW Passat" " (VW Passat " ?cnew ")"))))
(deffacts rule-token-79 (token (name "rule_79")))
(defrule rule79_2
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс J (внедорожники)") (certainty ?cJ))
(test (> (abs ?cJ) 0.4))
?f <- (fact (name "VW Tiguan") (certainty ?cf_))
?tk <- (token (name "rule_79"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cMT ?cJ) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, коробка передач МКПП, класс J (внедорожники) -> VW Tiguan" " (VW Tiguan " ?cnew ")"))))
(deffacts rule-token-80 (token (name "rule_80")))
(defrule rule80_2
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс J (внедорожники)") (certainty ?cJ))
(test (> (abs ?cJ) 0.4))
?f <- (fact (name "VW Tiguan") (certainty ?cf_))
?tk <- (token (name "rule_80"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cJ) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс J (внедорожники) -> VW Tiguan" " (VW Tiguan " ?cnew ")"))))
(deffacts rule-token-81 (token (name "rule_81")))
(defrule rule81_2
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс J (внедорожники)") (certainty ?cJ))
(test (> (abs ?cJ) 0.4))
?f <- (fact (name "VW Tiguan") (certainty ?cf_))
?tk <- (token (name "rule_81"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cJ) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс J (внедорожники) -> VW Tiguan" " (VW Tiguan " ?cnew ")"))))
(deffacts rule-token-82 (token (name "rule_82")))
(defrule rule82_2
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс C (низший средний класс или гольф") (certainty ?cC))
(test (> (abs ?cC) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
?f <- (fact (name "VW Golf") (certainty ?cf_))
?tk <- (token (name "rule_82"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMT ?cC ?cBH) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач МКПП, класс C (низший средний класс или гольф, кузов хетчбек -> VW Golf" " (VW Golf " ?cnew ")"))))
(deffacts rule-token-83 (token (name "rule_83")))
(defrule rule83_2
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс C (низший средний класс или гольф") (certainty ?cC))
(test (> (abs ?cC) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
?f <- (fact (name "VW Golf") (certainty ?cf_))
?tk <- (token (name "rule_83"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cC ?cBH) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс C (низший средний класс или гольф, кузов хетчбек -> VW Golf" " (VW Golf " ?cnew ")"))))
(deffacts rule-token-84 (token (name "rule_84")))
(defrule rule84_2
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс J (внедорожники)") (certainty ?cJ))
(test (> (abs ?cJ) 0.4))
?f <- (fact (name "VW Touareg") (certainty ?cf_))
?tk <- (token (name "rule_84"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cJ) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс J (внедорожники) -> VW Touareg" " (VW Touareg " ?cnew ")"))))
(deffacts rule-token-85 (token (name "rule_85")))
(defrule rule85_2
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс E (бизнес") (certainty ?cE))
(test (> (abs ?cE) 0.4))
(fact (name "кузов SUV") (certainty ?cSUV))
(test (> (abs ?cSUV) 0.4))
?f <- (fact (name "BMW X6") (certainty ?cf_))
?tk <- (token (name "rule_85"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cE ?cSUV) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс E (бизнес, кузов SUV -> BMW X6" " (BMW X6 " ?cnew ")"))))
(deffacts rule-token-86 (token (name "rule_86")))
(defrule rule86_2
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс S (спорткары)") (certainty ?cS))
(test (> (abs ?cS) 0.4))
(fact (name "кузов купе") (certainty ?cCP))
(test (> (abs ?cCP) 0.4))
?f <- (fact (name "BMW Z4") (certainty ?cf_))
?tk <- (token (name "rule_86"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cS ?cCP) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс S (спорткары), кузов купе -> BMW Z4" " (BMW Z4 " ?cnew ")"))))
(deffacts rule-token-87 (token (name "rule_87")))
(defrule rule87_2
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс A (мини") (certainty ?cA))
(test (> (abs ?cA) 0.4))
?f <- (fact (name "Daewoo Matiz") (certainty ?cf_))
?tk <- (token (name "rule_87"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMT ?cA) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач МКПП, класс A (мини -> Daewoo Matiz" " (Daewoo Matiz " ?cnew ")"))))
(deffacts rule-token-88 (token (name "rule_88")))
(defrule rule88_2
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс A (мини") (certainty ?cA))
(test (> (abs ?cA) 0.4))
?f <- (fact (name "Daewoo Matiz") (certainty ?cf_))
?tk <- (token (name "rule_88"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cA) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс A (мини -> Daewoo Matiz" " (Daewoo Matiz " ?cnew ")"))))
(deffacts rule-token-89 (token (name "rule_89")))
(defrule rule89_2
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс B (небольшие городские автомобили)") (certainty ?cB))
(test (> (abs ?cB) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "Hyundai Solaris") (certainty ?cf_))
?tk <- (token (name "rule_89"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMT ?cB ?cBS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач МКПП, класс B (небольшие городские автомобили), кузов седан -> Hyundai Solaris" " (Hyundai Solaris " ?cnew ")"))))
(deffacts rule-token-90 (token (name "rule_90")))
(defrule rule90_2
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс B (небольшие городские автомобили)") (certainty ?cB))
(test (> (abs ?cB) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
?f <- (fact (name "Hyundai Solaris") (certainty ?cf_))
?tk <- (token (name "rule_90"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMT ?cB ?cBH) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач МКПП, класс B (небольшие городские автомобили), кузов хетчбек -> Hyundai Solaris" " (Hyundai Solaris " ?cnew ")"))))
(deffacts rule-token-91 (token (name "rule_91")))
(defrule rule91_2
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс B (небольшие городские автомобили)") (certainty ?cB))
(test (> (abs ?cB) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
?f <- (fact (name "Hyundai Solaris") (certainty ?cf_))
?tk <- (token (name "rule_91"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cB ?cBH) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс B (небольшие городские автомобили), кузов хетчбек -> Hyundai Solaris" " (Hyundai Solaris " ?cnew ")"))))
(deffacts rule-token-92 (token (name "rule_92")))
(defrule rule92_2
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс B (небольшие городские автомобили)") (certainty ?cB))
(test (> (abs ?cB) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "Hyundai Solaris") (certainty ?cf_))
?tk <- (token (name "rule_92"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cB ?cBS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс B (небольшие городские автомобили), кузов седан -> Hyundai Solaris" " (Hyundai Solaris " ?cnew ")"))))
(deffacts rule-token-93 (token (name "rule_93")))
(defrule rule93_2
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс J (внедорожники)") (certainty ?cJ))
(test (> (abs ?cJ) 0.4))
?f <- (fact (name "Lada Niva") (certainty ?cf_))
?tk <- (token (name "rule_93"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cMT ?cJ) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, коробка передач МКПП, класс J (внедорожники) -> Lada Niva" " (Lada Niva " ?cnew ")"))))