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
)
(deftemplate possible-fact
	(multislot name)
)
(deftemplate fact
    (multislot name)
)

(deftemplate target
    (multislot name)
)

(defrule match-facts
	(declare (salience 9))
	(possible-fact (name ?val))
	?q <- (input-question (name ?n&?val))
	=>
	(retract ?q)
	(assert (fact (name ?val)))
)

(defrule match-target
    (declare (salience 10))
    (target (name ?val))
    (fact (name ?n&?val))
    =>
    (do-for-all-facts ((?f fact)) TRUE (retract ?f))
    (assert (sendmessage "Целевой факт найден, стоп"))
)

;===========================================================================

(deffacts possible-facts
(possible-fact (name "студент"))
(possible-fact (name "работник"))
(possible-fact (name "бизнесмен"))

(possible-fact (name "холост"))
(possible-fact (name "есть семья"))
(possible-fact (name "есть дети"))

(possible-fact (name "мало общения"))
(possible-fact (name "средне общения"))
(possible-fact (name "много общения"))

(possible-fact (name "не везу грузы"))
(possible-fact (name "иногда везу грузы"))
(possible-fact (name "часто везу грузы"))

(possible-fact (name "не еду загород"))
(possible-fact (name "иногда еду загород"))
(possible-fact (name "часто еду загород"))

(possible-fact (name "не еду на природу"))
(possible-fact (name "иногда еду на природу"))
(possible-fact (name "часто еду на природу"))

(possible-fact (name "кузов SUV"))
(possible-fact (name "кузов купе"))
(possible-fact (name "кузов хетчбек"))
(possible-fact (name "кузов седан"))
(possible-fact (name "кузов универсал"))

(possible-fact (name "количество дверей две"))
(possible-fact (name "количество дверей три"))
(possible-fact (name "количество дверей четыре"))
(possible-fact (name "количество дверей пять"))

(possible-fact (name "класс A (мини)"))
(possible-fact (name "класс B (небольшие городские автомобили)"))
(possible-fact (name "класс C (низший средний класс или гольф)"))
(possible-fact (name "класс D (полноценный средний класс)"))
(possible-fact (name "класс E (бизнес)"))
(possible-fact (name "класс F (люксовые автомобили)"))
(possible-fact (name "класс J (внедорожники)"))
(possible-fact (name "класс M (минивэны)"))
(possible-fact (name "класс S (спорткары)"))

(possible-fact (name "привод передний"))
(possible-fact (name "привод задний"))
(possible-fact (name "привод полный"))

(possible-fact (name "коробка передач МКПП"))
(possible-fact (name "коробка передач АКПП"))

(possible-fact (name "цена низкая"))
(possible-fact (name "цена средняя"))
(possible-fact (name "цена высокая"))

(possible-fact (name "VW Passat"))
(possible-fact (name "VW Tiguan"))
(possible-fact (name "VW Touareg"))
(possible-fact (name "VW Golf"))
(possible-fact (name "BMW X6"))
(possible-fact (name "BMW Z4"))
(possible-fact (name "Daewoo Matiz"))
(possible-fact (name "Hyundai Solaris"))
(possible-fact (name "Lada Niva"))
)

(defrule rule1
(fact (name "бизнесмен"))
(not (exists (fact (name "коробка передач АКПП"))))
=>
(assert (fact (name "коробка передач АКПП")))
(assert (sendmessage "бизнесмен -> коробка передач АКПП")))
(defrule rule2
(fact (name "студент"))
(not (exists (fact (name "коробка передач МКПП"))))
=>
(assert (fact (name "коробка передач МКПП")))
(assert (sendmessage "студент -> коробка передач МКПП")))
(defrule rule3
(fact (name "работник"))
(not (exists (fact (name "коробка передач МКПП"))))
=>
(assert (fact (name "коробка передач МКПП")))
(assert (sendmessage "работник -> коробка передач МКПП")))
(defrule rule4
(fact (name "работник"))
(not (exists (fact (name "коробка передач АКПП"))))
=>
(assert (fact (name "коробка передач АКПП")))
(assert (sendmessage "работник -> коробка передач АКПП")))
(defrule rule7
(fact (name "бизнесмен"))
(not (exists (fact (name "цена высокая"))))
=>
(assert (fact (name "цена высокая")))
(assert (sendmessage "бизнесмен -> цена высокая")))
(defrule rule8
(fact (name "работник"))
(not (exists (fact (name "цена средняя"))))
=>
(assert (fact (name "цена средняя")))
(assert (sendmessage "работник -> цена средняя")))
(defrule rule9
(fact (name "студент"))
(not (exists (fact (name "цена низкая"))))
=>
(assert (fact (name "цена низкая")))
(assert (sendmessage "студент -> цена низкая")))
(defrule rule12
(fact (name "не еду на природу"))
(not (exists (fact (name "привод передний"))))
=>
(assert (fact (name "привод передний")))
(assert (sendmessage "не еду на природу -> привод передний")))
(defrule rule13
(fact (name "иногда еду на природу"))
(not (exists (fact (name "привод передний"))))
=>
(assert (fact (name "привод передний")))
(assert (sendmessage "иногда еду на природу -> привод передний")))
(defrule rule14
(fact (name "иногда еду на природу"))
(not (exists (fact (name "привод задний"))))
=>
(assert (fact (name "привод задний")))
(assert (sendmessage "иногда еду на природу -> привод задний")))
(defrule rule15
(fact (name "не еду на природу"))
(not (exists (fact (name "привод задний"))))
=>
(assert (fact (name "привод задний")))
(assert (sendmessage "не еду на природу -> привод задний")))
(defrule rule16
(fact (name "часто еду на природу"))
(not (exists (fact (name "привод полный"))))
=>
(assert (fact (name "привод полный")))
(assert (sendmessage "часто еду на природу -> привод полный")))
(defrule rule19
(fact (name "холост"))
(not (exists (fact (name "количество дверей две"))))
=>
(assert (fact (name "количество дверей две")))
(assert (sendmessage "холост -> количество дверей две")))
(defrule rule20
(fact (name "мало общения"))
(not (exists (fact (name "количество дверей две"))))
=>
(assert (fact (name "количество дверей две")))
(assert (sendmessage "мало общения -> количество дверей две")))
(defrule rule21
(fact (name "средне общения"))
(not (exists (fact (name "количество дверей две"))))
=>
(assert (fact (name "количество дверей две")))
(assert (sendmessage "средне общения -> количество дверей две")))
(defrule rule22
(fact (name "холост"))
(not (exists (fact (name "количество дверей три"))))
=>
(assert (fact (name "количество дверей три")))
(assert (sendmessage "холост -> количество дверей три")))
(defrule rule23
(fact (name "средне общения"))
(not (exists (fact (name "количество дверей три"))))
=>
(assert (fact (name "количество дверей три")))
(assert (sendmessage "средне общения -> количество дверей три")))
(defrule rule24
(fact (name "мало общения"))
(not (exists (fact (name "количество дверей три"))))
=>
(assert (fact (name "количество дверей три")))
(assert (sendmessage "мало общения -> количество дверей три")))
(defrule rule25
(fact (name "есть семья"))
(not (exists (fact (name "количество дверей четыре"))))
=>
(assert (fact (name "количество дверей четыре")))
(assert (sendmessage "есть семья -> количество дверей четыре")))
(defrule rule26
(fact (name "есть дети"))
(not (exists (fact (name "количество дверей четыре"))))
=>
(assert (fact (name "количество дверей четыре")))
(assert (sendmessage "есть дети -> количество дверей четыре")))
(defrule rule27
(fact (name "средне общения"))
(not (exists (fact (name "количество дверей четыре"))))
=>
(assert (fact (name "количество дверей четыре")))
(assert (sendmessage "средне общения -> количество дверей четыре")))
(defrule rule28
(fact (name "много общения"))
(not (exists (fact (name "количество дверей четыре"))))
=>
(assert (fact (name "количество дверей четыре")))
(assert (sendmessage "много общения -> количество дверей четыре")))
(defrule rule29
(fact (name "есть семья"))
(not (exists (fact (name "количество дверей пять"))))
=>
(assert (fact (name "количество дверей пять")))
(assert (sendmessage "есть семья -> количество дверей пять")))
(defrule rule30
(fact (name "много общения"))
(not (exists (fact (name "количество дверей пять"))))
=>
(assert (fact (name "количество дверей пять")))
(assert (sendmessage "много общения -> количество дверей пять")))
(defrule rule31
(fact (name "средне общения"))
(not (exists (fact (name "количество дверей пять"))))
=>
(assert (fact (name "количество дверей пять")))
(assert (sendmessage "средне общения -> количество дверей пять")))
(defrule rule32
(fact (name "много общения"))
(not (exists (fact (name "количество дверей пять"))))
=>
(assert (fact (name "количество дверей пять")))
(assert (sendmessage "много общения -> количество дверей пять")))
(defrule rule35
(fact (name "цена высокая"))
(fact (name "количество дверей четыре"))
(not (exists (fact (name "кузов SUV"))))
=>
(assert (fact (name "кузов SUV")))
(assert (sendmessage "цена высокая, количество дверей четыре -> кузов SUV")))
(defrule rule36
(fact (name "цена высокая"))
(fact (name "количество дверей пять"))
(not (exists (fact (name "кузов SUV"))))
=>
(assert (fact (name "кузов SUV")))
(assert (sendmessage "цена высокая, количество дверей пять -> кузов SUV")))
(defrule rule37
(fact (name "цена высокая"))
(fact (name "количество дверей три"))
(not (exists (fact (name "кузов купе"))))
=>
(assert (fact (name "кузов купе")))
(assert (sendmessage "цена высокая, количество дверей три -> кузов купе")))
(defrule rule38
(fact (name "цена высокая"))
(fact (name "количество дверей две"))
(not (exists (fact (name "кузов купе"))))
=>
(assert (fact (name "кузов купе")))
(assert (sendmessage "цена высокая, количество дверей две -> кузов купе")))
(defrule rule39
(fact (name "цена средняя"))
(fact (name "иногда везу грузы"))
(not (exists (fact (name "кузов хетчбек"))))
=>
(assert (fact (name "кузов хетчбек")))
(assert (sendmessage "цена средняя, иногда везу грузы -> кузов хетчбек")))
(defrule rule40
(fact (name "цена низкая"))
(fact (name "иногда везу грузы"))
(not (exists (fact (name "кузов хетчбек"))))
=>
(assert (fact (name "кузов хетчбек")))
(assert (sendmessage "цена низкая, иногда везу грузы -> кузов хетчбек")))
(defrule rule41
(fact (name "цена низкая"))
(fact (name "иногда еду загород"))
(not (exists (fact (name "кузов хетчбек"))))
=>
(assert (fact (name "кузов хетчбек")))
(assert (sendmessage "цена низкая, иногда еду загород -> кузов хетчбек")))
(defrule rule42
(fact (name "цена средняя"))
(fact (name "иногда еду загород"))
(not (exists (fact (name "кузов хетчбек"))))
=>
(assert (fact (name "кузов хетчбек")))
(assert (sendmessage "цена средняя, иногда еду загород -> кузов хетчбек")))
(defrule rule43
(fact (name "цена средняя"))
(fact (name "не еду загород"))
(not (exists (fact (name "кузов седан"))))
=>
(assert (fact (name "кузов седан")))
(assert (sendmessage "цена средняя, не еду загород -> кузов седан")))
(defrule rule44
(fact (name "цена средняя"))
(fact (name "не везу грузы"))
(not (exists (fact (name "кузов седан"))))
=>
(assert (fact (name "кузов седан")))
(assert (sendmessage "цена средняя, не везу грузы -> кузов седан")))
(defrule rule45
(fact (name "цена низкая"))
(fact (name "не везу грузы"))
(not (exists (fact (name "кузов седан"))))
=>
(assert (fact (name "кузов седан")))
(assert (sendmessage "цена низкая, не везу грузы -> кузов седан")))
(defrule rule46
(fact (name "цена низкая"))
(fact (name "не еду загород"))
(not (exists (fact (name "кузов седан"))))
=>
(assert (fact (name "кузов седан")))
(assert (sendmessage "цена низкая, не еду загород -> кузов седан")))
(defrule rule47
(fact (name "цена средняя"))
(fact (name "часто везу грузы"))
(not (exists (fact (name "кузов универсал"))))
=>
(assert (fact (name "кузов универсал")))
(assert (sendmessage "цена средняя, часто везу грузы -> кузов универсал")))
(defrule rule48
(fact (name "цена средняя"))
(fact (name "часто еду загород"))
(not (exists (fact (name "кузов универсал"))))
=>
(assert (fact (name "кузов универсал")))
(assert (sendmessage "цена средняя, часто еду загород -> кузов универсал")))
(defrule rule49
(fact (name "цена низкая"))
(fact (name "часто еду загород"))
(not (exists (fact (name "кузов универсал"))))
=>
(assert (fact (name "кузов универсал")))
(assert (sendmessage "цена низкая, часто еду загород -> кузов универсал")))
(defrule rule50
(fact (name "цена низкая"))
(fact (name "часто везу грузы"))
(not (exists (fact (name "кузов универсал"))))
=>
(assert (fact (name "кузов универсал")))
(assert (sendmessage "цена низкая, часто везу грузы -> кузов универсал")))
(defrule rule53
(fact (name "цена низкая"))
(not (exists (fact (name "класс A (мини"))))
=>
(assert (fact (name "класс A (мини")))
(assert (sendmessage "цена низкая -> класс A (мини")))
(defrule rule54
(fact (name "цена низкая"))
(fact (name "кузов хетчбек"))
(not (exists (fact (name "класс B (небольшие городские автомобили)"))))
=>
(assert (fact (name "класс B (небольшие городские автомобили)")))
(assert (sendmessage "цена низкая, кузов хетчбек -> класс B (небольшие городские автомобили)")))
(defrule rule55
(fact (name "цена низкая"))
(fact (name "кузов седан"))
(not (exists (fact (name "класс B (небольшие городские автомобили)"))))
=>
(assert (fact (name "класс B (небольшие городские автомобили)")))
(assert (sendmessage "цена низкая, кузов седан -> класс B (небольшие городские автомобили)")))
(defrule rule56
(fact (name "цена средняя"))
(fact (name "кузов хетчбек"))
(not (exists (fact (name "класс B (небольшие городские автомобили)"))))
=>
(assert (fact (name "класс B (небольшие городские автомобили)")))
(assert (sendmessage "цена средняя, кузов хетчбек -> класс B (небольшие городские автомобили)")))
(defrule rule57
(fact (name "цена средняя"))
(fact (name "кузов седан"))
(not (exists (fact (name "класс B (небольшие городские автомобили)"))))
=>
(assert (fact (name "класс B (небольшие городские автомобили)")))
(assert (sendmessage "цена средняя, кузов седан -> класс B (небольшие городские автомобили)")))
(defrule rule58
(fact (name "цена средняя"))
(fact (name "кузов хетчбек"))
(fact (name "количество дверей три"))
(not (exists (fact (name "класс C (низший средний класс или гольф"))))
=>
(assert (fact (name "класс C (низший средний класс или гольф")))
(assert (sendmessage "цена средняя, кузов хетчбек, количество дверей три -> класс C (низший средний класс или гольф")))
(defrule rule59
(fact (name "цена низкая"))
(fact (name "кузов хетчбек"))
(fact (name "количество дверей три"))
(not (exists (fact (name "класс C (низший средний класс или гольф"))))
=>
(assert (fact (name "класс C (низший средний класс или гольф")))
(assert (sendmessage "цена низкая, кузов хетчбек, количество дверей три -> класс C (низший средний класс или гольф")))
(defrule rule60
(fact (name "цена средняя"))
(fact (name "кузов седан"))
(not (exists (fact (name "класс D (полноценный средний класс)"))))
=>
(assert (fact (name "класс D (полноценный средний класс)")))
(assert (sendmessage "цена средняя, кузов седан -> класс D (полноценный средний класс)")))
(defrule rule61
(fact (name "цена средняя"))
(fact (name "кузов универсал"))
(not (exists (fact (name "класс D (полноценный средний класс)"))))
=>
(assert (fact (name "класс D (полноценный средний класс)")))
(assert (sendmessage "цена средняя, кузов универсал -> класс D (полноценный средний класс)")))
(defrule rule62
(fact (name "цена высокая"))
(fact (name "кузов SUV"))
(not (exists (fact (name "класс E (бизнес"))))
=>
(assert (fact (name "класс E (бизнес")))
(assert (sendmessage "цена высокая, кузов SUV -> класс E (бизнес")))
(defrule rule63
(fact (name "привод полный"))
(not (exists (fact (name "класс J (внедорожники)"))))
=>
(assert (fact (name "класс J (внедорожники)")))
(assert (sendmessage "привод полный -> класс J (внедорожники)")))
(defrule rule64
(fact (name "цена высокая"))
(fact (name "кузов купе"))
(not (exists (fact (name "класс S (спорткары)"))))
=>
(assert (fact (name "класс S (спорткары)")))
(assert (sendmessage "цена высокая, кузов купе -> класс S (спорткары)")))
(defrule rule67
(fact (name "коробка передач МКПП"))
(fact (name "класс D (полноценный средний класс)"))
(fact (name "кузов универсал"))
(not (exists (fact (name "VW Passat"))))
=>
(assert (fact (name "VW Passat")))
(assert (sendmessage "коробка передач МКПП, класс D (полноценный средний класс), кузов универсал -> VW Passat")))
(defrule rule68
(fact (name "коробка передач АКПП"))
(fact (name "класс D (полноценный средний класс)"))
(fact (name "кузов универсал"))
(not (exists (fact (name "VW Passat"))))
=>
(assert (fact (name "VW Passat")))
(assert (sendmessage "коробка передач АКПП, класс D (полноценный средний класс), кузов универсал -> VW Passat")))
(defrule rule69
(fact (name "коробка передач АКПП"))
(fact (name "класс D (полноценный средний класс)"))
(fact (name "кузов седан"))
(not (exists (fact (name "VW Passat"))))
=>
(assert (fact (name "VW Passat")))
(assert (sendmessage "коробка передач АКПП, класс D (полноценный средний класс), кузов седан -> VW Passat")))
(defrule rule70
(fact (name "коробка передач МКПП"))
(fact (name "класс D (полноценный средний класс)"))
(fact (name "кузов седан"))
(not (exists (fact (name "VW Passat"))))
=>
(assert (fact (name "VW Passat")))
(assert (sendmessage "коробка передач МКПП, класс D (полноценный средний класс), кузов седан -> VW Passat")))
(defrule rule71
(fact (name "цена средняя"))
(fact (name "коробка передач МКПП"))
(fact (name "класс J (внедорожники)"))
(not (exists (fact (name "VW Tiguan"))))
=>
(assert (fact (name "VW Tiguan")))
(assert (sendmessage "цена средняя, коробка передач МКПП, класс J (внедорожники) -> VW Tiguan")))
(defrule rule72
(fact (name "коробка передач АКПП"))
(fact (name "класс J (внедорожники)"))
(not (exists (fact (name "VW Tiguan"))))
=>
(assert (fact (name "VW Tiguan")))
(assert (sendmessage "коробка передач АКПП, класс J (внедорожники) -> VW Tiguan")))
(defrule rule73
(fact (name "коробка передач АКПП"))
(fact (name "класс J (внедорожники)"))
(not (exists (fact (name "VW Tiguan"))))
=>
(assert (fact (name "VW Tiguan")))
(assert (sendmessage "коробка передач АКПП, класс J (внедорожники) -> VW Tiguan")))
(defrule rule74
(fact (name "коробка передач МКПП"))
(fact (name "класс C (низший средний класс или гольф"))
(fact (name "кузов хетчбек"))
(not (exists (fact (name "VW Golf"))))
=>
(assert (fact (name "VW Golf")))
(assert (sendmessage "коробка передач МКПП, класс C (низший средний класс или гольф, кузов хетчбек -> VW Golf")))
(defrule rule75
(fact (name "коробка передач АКПП"))
(fact (name "класс C (низший средний класс или гольф"))
(fact (name "кузов хетчбек"))
(not (exists (fact (name "VW Golf"))))
=>
(assert (fact (name "VW Golf")))
(assert (sendmessage "коробка передач АКПП, класс C (низший средний класс или гольф, кузов хетчбек -> VW Golf")))
(defrule rule76
(fact (name "коробка передач АКПП"))
(fact (name "класс J (внедорожники)"))
(not (exists (fact (name "VW Touareg"))))
=>
(assert (fact (name "VW Touareg")))
(assert (sendmessage "коробка передач АКПП, класс J (внедорожники) -> VW Touareg")))
(defrule rule77
(fact (name "коробка передач АКПП"))
(fact (name "класс E (бизнес"))
(fact (name "кузов SUV"))
(not (exists (fact (name "BMW X6"))))
=>
(assert (fact (name "BMW X6")))
(assert (sendmessage "коробка передач АКПП, класс E (бизнес, кузов SUV -> BMW X6")))
(defrule rule78
(fact (name "коробка передач АКПП"))
(fact (name "класс S (спорткары)"))
(fact (name "кузов купе"))
(not (exists (fact (name "BMW Z4"))))
=>
(assert (fact (name "BMW Z4")))
(assert (sendmessage "коробка передач АКПП, класс S (спорткары), кузов купе -> BMW Z4")))
(defrule rule79
(fact (name "коробка передач МКПП"))
(fact (name "класс A (мини"))
(not (exists (fact (name "Daewoo Matiz"))))
=>
(assert (fact (name "Daewoo Matiz")))
(assert (sendmessage "коробка передач МКПП, класс A (мини -> Daewoo Matiz")))
(defrule rule80
(fact (name "коробка передач АКПП"))
(fact (name "класс A (мини"))
(not (exists (fact (name "Daewoo Matiz"))))
=>
(assert (fact (name "Daewoo Matiz")))
(assert (sendmessage "коробка передач АКПП, класс A (мини -> Daewoo Matiz")))
(defrule rule81
(fact (name "коробка передач МКПП"))
(fact (name "класс B (небольшие городские автомобили)"))
(fact (name "кузов седан"))
(not (exists (fact (name "Hyundai Solaris"))))
=>
(assert (fact (name "Hyundai Solaris")))
(assert (sendmessage "коробка передач МКПП, класс B (небольшие городские автомобили), кузов седан -> Hyundai Solaris")))
(defrule rule82
(fact (name "коробка передач МКПП"))
(fact (name "класс B (небольшие городские автомобили)"))
(fact (name "кузов хетчбек"))
(not (exists (fact (name "Hyundai Solaris"))))
=>
(assert (fact (name "Hyundai Solaris")))
(assert (sendmessage "коробка передач МКПП, класс B (небольшие городские автомобили), кузов хетчбек -> Hyundai Solaris")))
(defrule rule83
(fact (name "коробка передач АКПП"))
(fact (name "класс B (небольшие городские автомобили)"))
(fact (name "кузов хетчбек"))
(not (exists (fact (name "Hyundai Solaris"))))
=>
(assert (fact (name "Hyundai Solaris")))
(assert (sendmessage "коробка передач АКПП, класс B (небольшие городские автомобили), кузов хетчбек -> Hyundai Solaris")))
(defrule rule84
(fact (name "коробка передач АКПП"))
(fact (name "класс B (небольшие городские автомобили)"))
(fact (name "кузов седан"))
(not (exists (fact (name "Hyundai Solaris"))))
=>
(assert (fact (name "Hyundai Solaris")))
(assert (sendmessage "коробка передач АКПП, класс B (небольшие городские автомобили), кузов седан -> Hyundai Solaris")))
(defrule rule85
(fact (name "цена низкая"))
(fact (name "коробка передач МКПП"))
(fact (name "класс J (внедорожники)"))
(not (exists (fact (name "Lada Niva"))))
=>
(assert (fact (name "Lada Niva")))
(assert (sendmessage "цена низкая, коробка передач МКПП, класс J (внедорожники) -> Lada Niva")))