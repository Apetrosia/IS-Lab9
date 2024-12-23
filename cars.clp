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

;===========================================================================

(deffacts facts
(fact (name "студент") (certainty 0.0))
(fact (name "работник") (certainty 0.0))
(fact (name "бизнесмен") (certainty 0.0))

(fact (name "холост") (certainty 0.0))
(fact (name "есть семья") (certainty 0.0))
(fact (name "есть дети") (certainty 0.0))

(fact (name "мало общения") (certainty 0.0))
(fact (name "средне общения") (certainty 0.0))
(fact (name "много общения") (certainty 0.0))

(fact (name "не везу грузы") (certainty 0.0))
(fact (name "иногда везу грузы") (certainty 0.0))
(fact (name "часто везу грузы") (certainty 0.0))

(fact (name "не еду загород") (certainty 0.0))
(fact (name "иногда еду загород") (certainty 0.0))
(fact (name "часто еду загород") (certainty 0.0))

(fact (name "не еду на природу") (certainty 0.0))
(fact (name "иногда еду на природу") (certainty 0.0))
(fact (name "часто еду на природу") (certainty 0.0))


(fact (name "кузов SUV") (certainty 0.0))
(fact (name "кузов купе") (certainty 0.0))
(fact (name "кузов хетчбек") (certainty 0.0))
(fact (name "кузов седан") (certainty 0.0))
(fact (name "кузов универсал") (certainty 0.0))

(fact (name "количество дверей две") (certainty 0.0))
(fact (name "количество дверей три") (certainty 0.0))
(fact (name "количество дверей четыре") (certainty 0.0))
(fact (name "количество дверей пять") (certainty 0.0))

(fact (name "класс A (мини") (certainty 0.0))
(fact (name "класс B (небольшие городские автомобили)") (certainty 0.0))
(fact (name "класс C (низший средний класс или гольф") (certainty 0.0))
(fact (name "класс D (полноценный средний класс)") (certainty 0.0))
(fact (name "класс E (бизнес") (certainty 0.0))
(fact (name "класс F (люксовые автомобили)") (certainty 0.0))
(fact (name "класс J (внедорожники)") (certainty 0.0))
(fact (name "класс M (минивэны)") (certainty 0.0))
(fact (name "класс S (спорткары)") (certainty 0.0))

(fact (name "привод передний") (certainty 0.0))
(fact (name "привод задний") (certainty 0.0))
(fact (name "привод полный") (certainty 0.0))

(fact (name "коробка передач МКПП") (certainty 0.0))
(fact (name "коробка передач АКПП") (certainty 0.0))

(fact (name "цена низкая") (certainty 0.0))
(fact (name "цена средняя") (certainty 0.0))
(fact (name "цена высокая") (certainty 0.0))

(fact (name "VW Passat") (certainty 0.0))
(fact (name "VW Tiguan") (certainty 0.0))
(fact (name "VW Touareg") (certainty 0.0))
(fact (name "VW Golf") (certainty 0.0))
(fact (name "BMW X6") (certainty 0.0))
(fact (name "BMW Z4") (certainty 0.0))
(fact (name "Daewoo Matiz") (certainty 0.0))
(fact (name "Hyundai Solaris") (certainty 0.0))
(fact (name "Lada Niva") (certainty 0.0))
)

(deffacts tokens
(token (name "rule1"))
(token (name "rule2"))
(token (name "rule3"))
(token (name "rule4"))
(token (name "rule5"))
(token (name "rule8"))
(token (name "rule9"))
(token (name "rule10"))
(token (name "rule11"))
(token (name "rule12"))
(token (name "rule13"))
(token (name "rule16"))
(token (name "rule17"))
(token (name "rule18"))
(token (name "rule19"))
(token (name "rule20"))
(token (name "rule21"))
(token (name "rule22"))
(token (name "rule25"))
(token (name "rule26"))
(token (name "rule27"))
(token (name "rule28"))
(token (name "rule29"))
(token (name "rule30"))
(token (name "rule31"))
(token (name "rule32"))
(token (name "rule33"))
(token (name "rule34"))
(token (name "rule35"))
(token (name "rule36"))
(token (name "rule37"))
(token (name "rule38"))
(token (name "rule39"))
(token (name "rule40"))
(token (name "rule43"))
(token (name "rule44"))
(token (name "rule45"))
(token (name "rule46"))
(token (name "rule47"))
(token (name "rule48"))
(token (name "rule49"))
(token (name "rule50"))
(token (name "rule51"))
(token (name "rule52"))
(token (name "rule53"))
(token (name "rule54"))
(token (name "rule55"))
(token (name "rule56"))
(token (name "rule57"))
(token (name "rule58"))
(token (name "rule61"))
(token (name "rule62"))
(token (name "rule63"))
(token (name "rule64"))
(token (name "rule65"))
(token (name "rule66"))
(token (name "rule67"))
(token (name "rule68"))
(token (name "rule69"))
(token (name "rule70"))
(token (name "rule71"))
(token (name "rule72"))
(token (name "rule75"))
(token (name "rule76"))
(token (name "rule77"))
(token (name "rule78"))
(token (name "rule79"))
(token (name "rule80"))
(token (name "rule81"))
(token (name "rule82"))
(token (name "rule83"))
(token (name "rule84"))
(token (name "rule85"))
(token (name "rule86"))
(token (name "rule87"))
(token (name "rule88"))
(token (name "rule89"))
(token (name "rule90"))
(token (name "rule91"))
(token (name "rule92"))
(token (name "rule93"))
)


(defrule rule1
(declare (salience 2))
(fact (name "бизнесмен") (certainty ?cTAB))
(test (> (abs ?cTAB) 0.4))
?f <- (fact (name "коробка передач АКПП") (certainty ?cf_))
?tk <- (token (name "rule1"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAB) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "бизнесмен -> коробка передач АКПП" " (" ?cnew ")"))))



(defrule rule2
(declare (salience 2))
(fact (name "студент") (certainty ?cTAS))
(test (> (abs ?cTAS) 0.4))
?f <- (fact (name "коробка передач МКПП") (certainty ?cf_))
?tk <- (token (name "rule2"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAS) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "студент -> коробка передач МКПП"  " (" ?cnew ")"))))



(defrule rule3
(declare (salience 2))
(fact (name "студент") (certainty ?cTAS))
(test (> (abs ?cTAS) 0.4))
?f <- (fact (name "коробка передач АКПП") (certainty ?cf_))
?tk <- (token (name "rule3"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAS) 0.3) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "студент -> коробка передач АКПП"  " (" ?cnew ")"))))



(defrule rule4
(declare (salience 2))
(fact (name "работник") (certainty ?cTAW))
(test (> (abs ?cTAW) 0.4))
?f <- (fact (name "коробка передач МКПП") (certainty ?cf_))
?tk <- (token (name "rule4"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAW) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "работник -> коробка передач МКПП"  " (" ?cnew ")"))))



(defrule rule5
(declare (salience 2))
(fact (name "работник") (certainty ?cTAW))
(test (> (abs ?cTAW) 0.4))
?f <- (fact (name "коробка передач АКПП") (certainty ?cf_))
?tk <- (token (name "rule5"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAW) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "работник -> коробка передач АКПП"  " (" ?cnew ")"))))




(defrule rule8
(declare (salience 2))
(fact (name "бизнесмен") (certainty ?cTAB))
(test (> (abs ?cTAB) 0.4))
?f <- (fact (name "цена высокая") (certainty ?cf_))
?tk <- (token (name "rule8"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAB) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "бизнесмен -> цена высокая"  " (" ?cnew ")"))))



(defrule rule9
(declare (salience 2))
(fact (name "бизнесмен") (certainty ?cTAB))
(test (> (abs ?cTAB) 0.4))
?f <- (fact (name "цена средняя") (certainty ?cf_))
?tk <- (token (name "rule9"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAB) 0.1) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "бизнесмен -> цена средняя"  " (" ?cnew ")"))))



(defrule rule10
(declare (salience 2))
(fact (name "работник") (certainty ?cTAW))
(test (> (abs ?cTAW) 0.4))
?f <- (fact (name "цена средняя") (certainty ?cf_))
?tk <- (token (name "rule10"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAW) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "работник -> цена средняя"  " (" ?cnew ")"))))



(defrule rule11
(declare (salience 2))
(fact (name "работник") (certainty ?cTAW))
(test (> (abs ?cTAW) 0.4))
?f <- (fact (name "цена высокая") (certainty ?cf_))
?tk <- (token (name "rule11"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAW) 0.2) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "работник -> цена высокая"  " (" ?cnew ")"))))



(defrule rule12
(declare (salience 2))
(fact (name "студент") (certainty ?cTAS))
(test (> (abs ?cTAS) 0.4))
?f <- (fact (name "цена низкая") (certainty ?cf_))
?tk <- (token (name "rule12"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "студент -> цена низкая"  " (" ?cnew ")"))))



(defrule rule13
(declare (salience 2))
(fact (name "студент") (certainty ?cTAS))
(test (> (abs ?cTAS) 0.4))
?f <- (fact (name "цена средняя") (certainty ?cf_))
?tk <- (token (name "rule13"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTAS) 0.1) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "студент -> цена средняя"  " (" ?cnew ")"))))




(defrule rule16
(declare (salience 2))
(fact (name "не еду на природу") (certainty ?cTNN))
(test (> (abs ?cTNN) 0.4))
?f <- (fact (name "привод передний") (certainty ?cf_))
?tk <- (token (name "rule16"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNN) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "не еду на природу -> привод передний"  " (" ?cnew ")"))))



(defrule rule17
(declare (salience 2))
(fact (name "иногда еду на природу") (certainty ?cTNR))
(test (> (abs ?cTNR) 0.4))
?f <- (fact (name "привод передний") (certainty ?cf_))
?tk <- (token (name "rule17"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNR) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "иногда еду на природу -> привод передний"  " (" ?cnew ")"))))



(defrule rule18
(declare (salience 2))
(fact (name "иногда еду на природу") (certainty ?cTNR))
(test (> (abs ?cTNR) 0.4))
?f <- (fact (name "привод задний") (certainty ?cf_))
?tk <- (token (name "rule18"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNR) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "иногда еду на природу -> привод задний"  " (" ?cnew ")"))))



(defrule rule19
(declare (salience 2))
(fact (name "не еду на природу") (certainty ?cTNN))
(test (> (abs ?cTNN) 0.4))
?f <- (fact (name "привод задний") (certainty ?cf_))
?tk <- (token (name "rule19"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNN) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "не еду на природу -> привод задний"  " (" ?cnew ")"))))



(defrule rule20
(declare (salience 2))
(fact (name "часто еду на природу") (certainty ?cTNO))
(test (> (abs ?cTNO) 0.4))
?f <- (fact (name "привод полный") (certainty ?cf_))
?tk <- (token (name "rule20"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNO) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "часто еду на природу -> привод полный"  " (" ?cnew ")"))))



(defrule rule21
(declare (salience 2))
(fact (name "часто еду на природу") (certainty ?cTNO))
(test (> (abs ?cTNO) 0.4))
?f <- (fact (name "привод полный") (certainty ?cf_))
?tk <- (token (name "rule21"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNO) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "часто еду на природу -> привод полный"  " (" ?cnew ")"))))



(defrule rule22
(declare (salience 2))
(fact (name "часто еду на природу") (certainty ?cTNO))
(test (> (abs ?cTNO) 0.4))
?f <- (fact (name "привод полный") (certainty ?cf_))
?tk <- (token (name "rule22"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cTNO) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "часто еду на природу -> привод полный"  " (" ?cnew ")"))))




(defrule rule25
(declare (salience 3))
(fact (name "холост") (certainty ?cFSS))
(test (> (abs ?cFSS) 0.4))
?f <- (fact (name "количество дверей две") (certainty ?cf_))
?tk <- (token (name "rule25"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cFSS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "холост -> количество дверей две"  " (" ?cnew ")"))))



(defrule rule26
(declare (salience 3))
(fact (name "мало общения") (certainty ?cCR))
(test (> (abs ?cCR) 0.4))
?f <- (fact (name "количество дверей две") (certainty ?cf_))
?tk <- (token (name "rule26"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCR) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "мало общения -> количество дверей две"  " (" ?cnew ")"))))



(defrule rule27
(declare (salience 3))
(fact (name "средне общения") (certainty ?cCM))
(test (> (abs ?cCM) 0.4))
?f <- (fact (name "количество дверей две") (certainty ?cf_))
?tk <- (token (name "rule27"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCM) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "средне общения -> количество дверей две"  " (" ?cnew ")"))))



(defrule rule28
(declare (salience 3))
(fact (name "холост") (certainty ?cFSS))
(test (> (abs ?cFSS) 0.4))
?f <- (fact (name "количество дверей три") (certainty ?cf_))
?tk <- (token (name "rule28"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cFSS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "холост -> количество дверей три"  " (" ?cnew ")"))))



(defrule rule30
(declare (salience 3))
(fact (name "средне общения") (certainty ?cCM))
(test (> (abs ?cCM) 0.4))
?f <- (fact (name "количество дверей три") (certainty ?cf_))
?tk <- (token (name "rule30"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCM) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "средне общения -> количество дверей три"  " (" ?cnew ")"))))



(defrule rule31
(declare (salience 3))
(fact (name "средне общения") (certainty ?cCM))
(test (> (abs ?cCM) 0.4))
?f <- (fact (name "количество дверей две") (certainty ?cf_))
?tk <- (token (name "rule31"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCM) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "средне общения -> количество дверей две"  " (" ?cnew ")"))))



(defrule rule32
(declare (salience 3))
(fact (name "мало общения") (certainty ?cCR))
(test (> (abs ?cCR) 0.4))
?f <- (fact (name "количество дверей три") (certainty ?cf_))
?tk <- (token (name "rule32"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCR) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "мало общения -> количество дверей три"  " (" ?cnew ")"))))



(defrule rule34
(declare (salience 3))
(fact (name "есть семья") (certainty ?cFSF))
(test (> (abs ?cFSF) 0.4))
?f <- (fact (name "количество дверей четыре") (certainty ?cf_))
?tk <- (token (name "rule34"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cFSF) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "есть семья -> количество дверей четыре"  " (" ?cnew ")"))))



(defrule rule35
(declare (salience 3))
(fact (name "есть дети") (certainty ?cFSC))
(test (> (abs ?cFSC) 0.4))
?f <- (fact (name "количество дверей четыре") (certainty ?cf_))
?tk <- (token (name "rule35"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cFSC) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "есть дети -> количество дверей четыре"  " (" ?cnew ")"))))



(defrule rule36
(declare (salience 3))
(fact (name "средне общения") (certainty ?cCM))
(test (> (abs ?cCM) 0.4))
?f <- (fact (name "количество дверей четыре") (certainty ?cf_))
?tk <- (token (name "rule36"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCM) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "средне общения -> количество дверей четыре"  " (" ?cnew ")"))))



(defrule rule37
(declare (salience 3))
(fact (name "много общения") (certainty ?cCO))
(test (> (abs ?cCO) 0.4))
?f <- (fact (name "количество дверей четыре") (certainty ?cf_))
?tk <- (token (name "rule37"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCO) 0.95) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "много общения -> количество дверей четыре"  " (" ?cnew ")"))))



(defrule rule38
(declare (salience 3))
(fact (name "есть семья") (certainty ?cFSF))
(test (> (abs ?cFSF) 0.4))
?f <- (fact (name "количество дверей пять") (certainty ?cf_))
?tk <- (token (name "rule38"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cFSF) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "есть семья -> количество дверей пять"  " (" ?cnew ")"))))



(defrule rule39
(declare (salience 3))
(fact (name "много общения") (certainty ?cCO))
(test (> (abs ?cCO) 0.4))
?f <- (fact (name "количество дверей пять") (certainty ?cf_))
?tk <- (token (name "rule39"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCO) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "много общения -> количество дверей пять"  " (" ?cnew ")"))))



(defrule rule40
(declare (salience 3))
(fact (name "средне общения") (certainty ?cCM))
(test (> (abs ?cCM) 0.4))
?f <- (fact (name "количество дверей пять") (certainty ?cf_))
?tk <- (token (name "rule40"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cCM) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "средне общения -> количество дверей пять"  " (" ?cnew ")"))))




(defrule rule43
(declare (salience 4))
(fact (name "цена высокая") (certainty ?cHP))
(test (> (abs ?cHP) 0.4))
(fact (name "количество дверей четыре") (certainty ?cFRD))
(test (> (abs ?cFRD) 0.4))
?f <- (fact (name "кузов SUV") (certainty ?cf_))
?tk <- (token (name "rule43"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cHP ?cFRD) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена высокая, количество дверей четыре -> кузов SUV"  " (" ?cnew ")"))))



(defrule rule44
(declare (salience 4))
(fact (name "цена высокая") (certainty ?cHP))
(test (> (abs ?cHP) 0.4))
(fact (name "количество дверей пять") (certainty ?cFD))
(test (> (abs ?cFD) 0.4))
?f <- (fact (name "кузов SUV") (certainty ?cf_))
?tk <- (token (name "rule44"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cHP ?cFD) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена высокая, количество дверей пять -> кузов SUV"  " (" ?cnew ")"))))



(defrule rule45
(declare (salience 4))
(fact (name "цена высокая") (certainty ?cHP))
(test (> (abs ?cHP) 0.4))
(fact (name "количество дверей три") (certainty ?cTHD))
(test (> (abs ?cTHD) 0.4))
?f <- (fact (name "кузов купе") (certainty ?cf_))
?tk <- (token (name "rule45"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cHP ?cTHD) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена высокая, количество дверей три -> кузов купе"  " (" ?cnew ")"))))



(defrule rule46
(declare (salience 4))
(fact (name "цена высокая") (certainty ?cHP))
(test (> (abs ?cHP) 0.4))
(fact (name "количество дверей две") (certainty ?cTWD))
(test (> (abs ?cTWD) 0.4))
?f <- (fact (name "кузов купе") (certainty ?cf_))
?tk <- (token (name "rule46"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cHP ?cTWD) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена высокая, количество дверей две -> кузов купе"  " (" ?cnew ")"))))



(defrule rule47
(declare (salience 4))
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "иногда везу грузы") (certainty ?cCGR))
(test (> (abs ?cCGR) 0.4))
?f <- (fact (name "кузов хетчбек") (certainty ?cf_))
?tk <- (token (name "rule47"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cCGR) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, иногда везу грузы -> кузов хетчбек"  " (" ?cnew ")"))))



(defrule rule48
(declare (salience 4))
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "иногда везу грузы") (certainty ?cCGR))
(test (> (abs ?cCGR) 0.4))
?f <- (fact (name "кузов хетчбек") (certainty ?cf_))
?tk <- (token (name "rule48"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cCGR) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, иногда везу грузы -> кузов хетчбек"  " (" ?cnew ")"))))



(defrule rule49
(declare (salience 4))
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "иногда еду загород") (certainty ?cTCR))
(test (> (abs ?cTCR) 0.4))
?f <- (fact (name "кузов хетчбек") (certainty ?cf_))
?tk <- (token (name "rule49"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cTCR) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, иногда еду загород -> кузов хетчбек"  " (" ?cnew ")"))))



(defrule rule50
(declare (salience 4))
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "иногда еду загород") (certainty ?cTCR))
(test (> (abs ?cTCR) 0.4))
?f <- (fact (name "кузов хетчбек") (certainty ?cf_))
?tk <- (token (name "rule50"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cTCR) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, иногда еду загород -> кузов хетчбек"  " (" ?cnew ")"))))



(defrule rule51
(declare (salience 4))
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "не еду загород") (certainty ?cTCN))
(test (> (abs ?cTCN) 0.4))
?f <- (fact (name "кузов седан") (certainty ?cf_))
?tk <- (token (name "rule51"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cTCN) 0.95) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, не еду загород -> кузов седан"  " (" ?cnew ")"))))



(defrule rule52
(declare (salience 4))
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "не везу грузы") (certainty ?cCGN))
(test (> (abs ?cCGN) 0.4))
?f <- (fact (name "кузов седан") (certainty ?cf_))
?tk <- (token (name "rule52"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cCGN) 0.95) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, не везу грузы -> кузов седан"  " (" ?cnew ")"))))



(defrule rule53
(declare (salience 4))
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "не везу грузы") (certainty ?cCGN))
(test (> (abs ?cCGN) 0.4))
?f <- (fact (name "кузов седан") (certainty ?cf_))
?tk <- (token (name "rule53"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cCGN) 0.95) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, не везу грузы -> кузов седан"  " (" ?cnew ")"))))



(defrule rule54
(declare (salience 4))
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "не еду загород") (certainty ?cTCN))
(test (> (abs ?cTCN) 0.4))
?f <- (fact (name "кузов седан") (certainty ?cf_))
?tk <- (token (name "rule54"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cTCN) 0.95) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, не еду загород -> кузов седан"  " (" ?cnew ")"))))



(defrule rule55
(declare (salience 4))
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "часто везу грузы") (certainty ?cCGO))
(test (> (abs ?cCGO) 0.4))
?f <- (fact (name "кузов универсал") (certainty ?cf_))
?tk <- (token (name "rule55"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cCGO) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, часто везу грузы -> кузов универсал"  " (" ?cnew ")"))))



(defrule rule56
(declare (salience 4))
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "часто еду загород") (certainty ?cTCO))
(test (> (abs ?cTCO) 0.4))
?f <- (fact (name "кузов универсал") (certainty ?cf_))
?tk <- (token (name "rule56"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cTCO) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, часто еду загород -> кузов универсал"  " (" ?cnew ")"))))



(defrule rule57
(declare (salience 4))
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "часто еду загород") (certainty ?cTCO))
(test (> (abs ?cTCO) 0.4))
?f <- (fact (name "кузов универсал") (certainty ?cf_))
?tk <- (token (name "rule57"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cTCO) 0.65) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, часто еду загород -> кузов универсал"  " (" ?cnew ")"))))



(defrule rule58
(declare (salience 4))
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "часто везу грузы") (certainty ?cCGO))
(test (> (abs ?cCGO) 0.4))
?f <- (fact (name "кузов универсал") (certainty ?cf_))
?tk <- (token (name "rule58"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cCGO) 0.65) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, часто везу грузы -> кузов универсал"  " (" ?cnew ")"))))




(defrule rule61
(declare (salience 7))
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
?f <- (fact (name "класс A (мини") (certainty ?cf_))
?tk <- (token (name "rule61"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая -> класс A (мини"  " ((мини " ?cnew ")"))))



(defrule rule62
(declare (salience 7))
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
?f <- (fact (name "класс B (небольшие городские автомобили)") (certainty ?cf_))
?tk <- (token (name "rule62"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cBH) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, кузов хетчбек -> класс B (небольшие городские автомобили)"  " ((небольшие городские автомобили) " ?cnew ")"))))



(defrule rule63
(declare (salience 7))
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "класс B (небольшие городские автомобили)") (certainty ?cf_))
?tk <- (token (name "rule63"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cBS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, кузов седан -> класс B (небольшие городские автомобили)"  " ((небольшие городские автомобили) " ?cnew ")"))))



(defrule rule64
(declare (salience 7))
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
?f <- (fact (name "класс B (небольшие городские автомобили)") (certainty ?cf_))
?tk <- (token (name "rule64"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cBH) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, кузов хетчбек -> класс B (небольшие городские автомобили)"  " ((небольшие городские автомобили) " ?cnew ")"))))



(defrule rule65
(declare (salience 7))
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "класс B (небольшие городские автомобили)") (certainty ?cf_))
?tk <- (token (name "rule65"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cBS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, кузов седан -> класс B (небольшие городские автомобили)"  " ((небольшие городские автомобили) " ?cnew ")"))))



(defrule rule66
(declare (salience 7))
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
(fact (name "количество дверей три") (certainty ?cTHD))
(test (> (abs ?cTHD) 0.4))
?f <- (fact (name "класс C (низший средний класс или гольф") (certainty ?cf_))
?tk <- (token (name "rule66"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cBH ?cTHD) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, кузов хетчбек, количество дверей три -> класс C (низший средний класс или гольф"  " ((низший средний класс или гольф " ?cnew ")"))))



(defrule rule67
(declare (salience 7))
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
(fact (name "количество дверей три") (certainty ?cTHD))
(test (> (abs ?cTHD) 0.4))
?f <- (fact (name "класс C (низший средний класс или гольф") (certainty ?cf_))
?tk <- (token (name "rule67"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cBH ?cTHD) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, кузов хетчбек, количество дверей три -> класс C (низший средний класс или гольф"  " ((низший средний класс или гольф " ?cnew ")"))))



(defrule rule68
(declare (salience 7))
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "класс D (полноценный средний класс)") (certainty ?cf_))
?tk <- (token (name "rule68"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cBS) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, кузов седан -> класс D (полноценный средний класс)"  " ((полноценный средний класс) " ?cnew ")"))))



(defrule rule69
(declare (salience 7))
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "кузов универсал") (certainty ?cBU))
(test (> (abs ?cBU) 0.4))
?f <- (fact (name "класс D (полноценный средний класс)") (certainty ?cf_))
?tk <- (token (name "rule69"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cBU) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, кузов универсал -> класс D (полноценный средний класс)"  " ((полноценный средний класс) " ?cnew ")"))))



(defrule rule70
(declare (salience 7))
(fact (name "цена высокая") (certainty ?cHP))
(test (> (abs ?cHP) 0.4))
(fact (name "кузов SUV") (certainty ?cSUV))
(test (> (abs ?cSUV) 0.4))
?f <- (fact (name "класс E (бизнес") (certainty ?cf_))
?tk <- (token (name "rule70"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cHP ?cSUV) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена высокая, кузов SUV -> класс E (бизнес"  " ((бизнес " ?cnew ")"))))



(defrule rule71
(declare (salience 7))
(fact (name "привод полный") (certainty ?cAWD))
(test (> (abs ?cAWD) 0.4))
?f <- (fact (name "класс J (внедорожники)") (certainty ?cf_))
?tk <- (token (name "rule71"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAWD) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "привод полный -> класс J (внедорожники)"  " ((внедорожники) " ?cnew ")"))))



(defrule rule72
(declare (salience 7))
(fact (name "цена высокая") (certainty ?cHP))
(test (> (abs ?cHP) 0.4))
(fact (name "кузов купе") (certainty ?cCP))
(test (> (abs ?cCP) 0.4))
?f <- (fact (name "класс S (спорткары)") (certainty ?cf_))
?tk <- (token (name "rule72"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cHP ?cCP) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена высокая, кузов купе -> класс S (спорткары)"  " ((спорткары) " ?cnew ")"))))




(defrule rule75
(declare (salience 1))
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс D (полноценный средний класс)") (certainty ?cD))
(test (> (abs ?cD) 0.4))
(fact (name "кузов универсал") (certainty ?cBU))
(test (> (abs ?cBU) 0.4))
?f <- (fact (name "VW Passat") (certainty ?cf_))
?tk <- (token (name "rule75"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMT ?cD ?cBU) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач МКПП, класс D (полноценный средний класс), кузов универсал -> VW Passat"  " (" ?cnew ")"))))



(defrule rule76
(declare (salience 1))
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс D (полноценный средний класс)") (certainty ?cD))
(test (> (abs ?cD) 0.4))
(fact (name "кузов универсал") (certainty ?cBU))
(test (> (abs ?cBU) 0.4))
?f <- (fact (name "VW Passat") (certainty ?cf_))
?tk <- (token (name "rule76"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cD ?cBU) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс D (полноценный средний класс), кузов универсал -> VW Passat"  " (" ?cnew ")"))))



(defrule rule77
(declare (salience 1))
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс D (полноценный средний класс)") (certainty ?cD))
(test (> (abs ?cD) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "VW Passat") (certainty ?cf_))
?tk <- (token (name "rule77"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cD ?cBS) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс D (полноценный средний класс), кузов седан -> VW Passat"  " (" ?cnew ")"))))



(defrule rule78
(declare (salience 1))
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс D (полноценный средний класс)") (certainty ?cD))
(test (> (abs ?cD) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "VW Passat") (certainty ?cf_))
?tk <- (token (name "rule78"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMT ?cD ?cBS) 0.7) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач МКПП, класс D (полноценный средний класс), кузов седан -> VW Passat"  " (" ?cnew ")"))))



(defrule rule79
(declare (salience 1))
(fact (name "цена средняя") (certainty ?cMP))
(test (> (abs ?cMP) 0.4))
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс J (внедорожники)") (certainty ?cJ))
(test (> (abs ?cJ) 0.4))
?f <- (fact (name "VW Tiguan") (certainty ?cf_))
?tk <- (token (name "rule79"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMP ?cMT ?cJ) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена средняя, коробка передач МКПП, класс J (внедорожники) -> VW Tiguan"  " (" ?cnew ")"))))



(defrule rule80
(declare (salience 1))
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс J (внедорожники)") (certainty ?cJ))
(test (> (abs ?cJ) 0.4))
?f <- (fact (name "VW Tiguan") (certainty ?cf_))
?tk <- (token (name "rule80"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cJ) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс J (внедорожники) -> VW Tiguan"  " (" ?cnew ")"))))



(defrule rule81
(declare (salience 1))
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс J (внедорожники)") (certainty ?cJ))
(test (> (abs ?cJ) 0.4))
?f <- (fact (name "VW Tiguan") (certainty ?cf_))
?tk <- (token (name "rule81"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cJ) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс J (внедорожники) -> VW Tiguan"  " (" ?cnew ")"))))



(defrule rule82
(declare (salience 1))
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс C (низший средний класс или гольф") (certainty ?cC))
(test (> (abs ?cC) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
?f <- (fact (name "VW Golf") (certainty ?cf_))
?tk <- (token (name "rule82"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMT ?cC ?cBH) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач МКПП, класс C (низший средний класс или гольф, кузов хетчбек -> VW Golf"  " (" ?cnew ")"))))



(defrule rule83
(declare (salience 1))
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс C (низший средний класс или гольф") (certainty ?cC))
(test (> (abs ?cC) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
?f <- (fact (name "VW Golf") (certainty ?cf_))
?tk <- (token (name "rule83"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cC ?cBH) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс C (низший средний класс или гольф, кузов хетчбек -> VW Golf"  " (" ?cnew ")"))))



(defrule rule84
(declare (salience 1))
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс J (внедорожники)") (certainty ?cJ))
(test (> (abs ?cJ) 0.4))
?f <- (fact (name "VW Touareg") (certainty ?cf_))
?tk <- (token (name "rule84"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cJ) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс J (внедорожники) -> VW Touareg"  " (" ?cnew ")"))))



(defrule rule85
(declare (salience 1))
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс E (бизнес") (certainty ?cE))
(test (> (abs ?cE) 0.4))
(fact (name "кузов SUV") (certainty ?cSUV))
(test (> (abs ?cSUV) 0.4))
?f <- (fact (name "BMW X6") (certainty ?cf_))
?tk <- (token (name "rule85"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cE ?cSUV) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс E (бизнес, кузов SUV -> BMW X6" " (BMW X6 " ?cnew ")"))))



(defrule rule86
(declare (salience 1))
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс S (спорткары)") (certainty ?cS))
(test (> (abs ?cS) 0.4))
(fact (name "кузов купе") (certainty ?cCP))
(test (> (abs ?cCP) 0.4))
?f <- (fact (name "BMW Z4") (certainty ?cf_))
?tk <- (token (name "rule86"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cS ?cCP) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс S (спорткары), кузов купе -> BMW Z4" " (BMW Z4 " ?cnew ")"))))



(defrule rule87
(declare (salience 1))
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс A (мини") (certainty ?cA))
(test (> (abs ?cA) 0.4))
?f <- (fact (name "Daewoo Matiz") (certainty ?cf_))
?tk <- (token (name "rule87"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMT ?cA) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач МКПП, класс A (мини -> Daewoo Matiz"  " (" ?cnew ")"))))



(defrule rule88
(declare (salience 1))
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс A (мини") (certainty ?cA))
(test (> (abs ?cA) 0.4))
?f <- (fact (name "Daewoo Matiz") (certainty ?cf_))
?tk <- (token (name "rule88"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cA) 1.0) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс A (мини -> Daewoo Matiz"  " (" ?cnew ")"))))



(defrule rule89
(declare (salience 1))
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс B (небольшие городские автомобили)") (certainty ?cB))
(test (> (abs ?cB) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "Hyundai Solaris") (certainty ?cf_))
?tk <- (token (name "rule89"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMT ?cB ?cBS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач МКПП, класс B (небольшие городские автомобили), кузов седан -> Hyundai Solaris"  " (" ?cnew ")"))))



(defrule rule90
(declare (salience 1))
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс B (небольшие городские автомобили)") (certainty ?cB))
(test (> (abs ?cB) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
?f <- (fact (name "Hyundai Solaris") (certainty ?cf_))
?tk <- (token (name "rule90"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cMT ?cB ?cBH) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач МКПП, класс B (небольшие городские автомобили), кузов хетчбек -> Hyundai Solaris"  " (" ?cnew ")"))))



(defrule rule91
(declare (salience 1))
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс B (небольшие городские автомобили)") (certainty ?cB))
(test (> (abs ?cB) 0.4))
(fact (name "кузов хетчбек") (certainty ?cBH))
(test (> (abs ?cBH) 0.4))
?f <- (fact (name "Hyundai Solaris") (certainty ?cf_))
?tk <- (token (name "rule91"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cB ?cBH) 0.8) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс B (небольшие городские автомобили), кузов хетчбек -> Hyundai Solaris"  " (" ?cnew ")"))))



(defrule rule92
(declare (salience 1))
(fact (name "коробка передач АКПП") (certainty ?cAT))
(test (> (abs ?cAT) 0.4))
(fact (name "класс B (небольшие городские автомобили)") (certainty ?cB))
(test (> (abs ?cB) 0.4))
(fact (name "кузов седан") (certainty ?cBS))
(test (> (abs ?cBS) 0.4))
?f <- (fact (name "Hyundai Solaris") (certainty ?cf_))
?tk <- (token (name "rule92"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cAT ?cB ?cBS) 0.9) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "коробка передач АКПП, класс B (небольшие городские автомобили), кузов седан -> Hyundai Solaris"  " (" ?cnew ")"))))



(defrule rule93
(declare (salience 1))
(fact (name "цена низкая") (certainty ?cLP))
(test (> (abs ?cLP) 0.4))
(fact (name "коробка передач МКПП") (certainty ?cMT))
(test (> (abs ?cMT) 0.4))
(fact (name "класс J (внедорожники)") (certainty ?cJ))
(test (> (abs ?cJ) 0.4))
?f <- (fact (name "Lada Niva") (certainty ?cf_))
?tk <- (token (name "rule93"))
=>
(retract ?tk)
(bind ?cnew (combine (* (min ?cLP ?cMT ?cJ) 0.99) ?cf_))
(modify ?f (certainty ?cnew))
(assert (sendmessage (str-cat "цена низкая, коробка передач МКПП, класс J (внедорожники) -> Lada Niva"  " (" ?cnew ")"))))