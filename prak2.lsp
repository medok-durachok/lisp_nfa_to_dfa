; БЛОК ОБЩИХ ВСПОМОГАТЕЛЬНЫХ ФУНКЦИЙ
; получаем элементы с некоторого уровня списка
(defun level (N S) (lvl N S nil))

(defun lvl (N S Res) (cond
	((eq N 0) (cons S Res))
  	((atom S) Res)
  	(T (lvl (- N 1) (car S) (lvl N (cdr S) Res)))
))

; вычисление длины списка
(defun len (L) (cond
	((eq L nil) 1)
	(T (+ 1 (len (cdr L))))
))

; --------------------------------------------

; БЛОК ПОЛУЧЕНИЯ ГРАММАТИКИ ПО КОНЕЧНОМУ АВТОМАТУ 
; составляем правила грамматики
(defun make_gram (Rule Unique) (cond
	((null Rule) Rule)
	((member (caaar Rule) Unique) (make_gram (cdr Rule) Unique))
	(T (cons (plus (caaar Rule) Rule nil ) (make_gram (cdr Rule) (cons (caaar Rule) Unique))))
))

    
; из ребра графа делаем грамматическое правило
(defun make_rule (KA Gram) (cond
	((null KA) Gram)
	(T (make_rule (cdr KA) (cons (change (car KA)) Gram)))
))

; приводим к виду ((Состояние) = (Правило))
(defun change (toRule) (cons (car toRule) (cons '= (cons (cadr toRule) (caddr toRule)))))


; соединяем правила для одного состояния в один список
(defun plus (A L R) (cond
	((null L) (cons (cons A nil) (replace_eq (cons '= (check_same_in_rule(level 2 R))) 0))) ;(replace_eq (cons '= (cons (check_same_in_rule (level 2 R)) nil)) 0)))
	((eq A (caaar L)) (plus A (cdr L) (cons (cdar L) R)))
	(T (plus A (cdr L) R))
))


(defun check_same_in_rule (Rule) (cond
    	((null (cdr Rule)) Rule)
    	((null (cddr Rule)) Rule)
    	((eq (car Rule) '=) (check_same_in_rule (cdr Rule)))
    	((and (eq (car Rule) (cadddr Rule)) (eq (cadr Rule) (car (cddddr Rule))) (check_same_in_rule (cdddr Rule))))
	(T (cons (car Rule) (cons (cadr Rule) (cons '= (check_same_in_rule (cdddr Rule))))))
))

; расставляем разделители внутри правила
(defun replace_eq (S Flag) (cond
	((null S) S)
  	((eq (car S) '=) (cond 
		((= Flag 0) (cons (car S) (cons (replace_eq (cdr S) 1) nil)))
		(T (cons '#\\ (replace_eq (cdr S) 1)))))
  	(T (cons (car S) (replace_eq (cdr S) Flag)))
))


(defun get_all_states (Gram States) (cond
	((null Gram) States)
	(T (get_all_states (cdr Gram) (cons (caar Gram) (get_states (cddar Gram) States))))
))


(defun get_states (Rule States) (cond
	((null (cddr Rule)) States)
	((is_in (cdar Rule) States) (get_states (cdddr Rule) States))
	(T (get_states (cdddr Rule) (cons (cdar Rule) States)))
))

(defun is_in (El L) (cond
	((null L) nil)
	((check_similarity El (car L)) T)
	(T (is_in El (cdr L)))
))

; здесь проверяем, одинаковые ли длины, и если да, то передаем на проверку "равенства" списков
(defun check_similarity (L1 L2) (cond
	((eq (len L1) (len L2)) (is_similar_els L1 L2))
))


; проверяем списки на совпадение по множеству символов
(defun is_similar_els (L1 L2) (cond
	((null L1) T)
	((member (car L1) L2) (is_similar_set (cdr L1) L2))
))

(defun is_similar_set (L1 L2) (cond
	((null L1) L2)
	((member (car L1) L2) (is_similar_set (cdr L1) L2))
	(T L1)
))

; удаляем S из грамматики после получения ДКА
(defun delete_S (Gram) (cond
	((null Gram) (print 'grammar) Gram)
	(T (cons (cons (caar Gram) (cons (cadar Gram) (cons (del_S (caddar Gram)) nil))) (delete_S (cdr Gram))))
))


(defun del_S (Rule) (cond
	((null Rule) Rule)
	((eq (car Rule) 'S ) (del_S (cdr Rule)))
	(T (cons (car Rule) (del_S (cdr Rule))))
))

; --------------------------------------------

; БЛОК ПРИВЕДЕНИЯ ГРАММАТИКИ
; возвращаем старый автомат с новыми правилами + новые состояния в конце
(defun to_DKA (Rule Set_r NewRule NewState) (cond
	((null Set_r) (cons (cdr NewRule) (cons NewState nil)))
    	((null (cdadr (update (car Set_r) Rule nil))) (to_DKA Rule (cdr Set_r) (cons '(#\\) (cons (car (update (car Set_r) Rule nil)) NewRule)) NewState))
    	((is_in (cadr (update (car Set_r) Rule nil)) NewState) (to_DKA Rule (cdr Set_r) (cons '(#\\) (cons (car (update (car Set_r) Rule nil)) NewRule)) NewState))
    	(T (to_DKA Rule (cdr Set_r) (cons '(#\\) (cons (car (update (car Set_r) Rule nil)) NewRule)) (cons (cadr (update (car Set_r) Rule nil)) NewState)))                                          
))

; получаем новые состояния
(defun update (A Rule New) (cond
	((null (cddr Rule)) (cond
		((eq A (car Rule)) (cond 
        		((and (null (cdr New)) (not (eq (member (cadr Rule) New) nil))) (cons (cons A New)nil))
			((not (eq (member (cadr Rule) New) nil)) (cons (cons A (cons New nil)) (cons New nil)))
                        (T (cons (cons A (cons (cons (cadr Rule) New) nil)) (cons (cons (cadr  Rule) New) nil)))))
		((null (cdr New)) (cons (cons A New) (cons New nil)))
        	(T (cons (cons A (cons New nil)) (cons New nil)))
		))
	((eq A (car Rule))(update A (cdddr Rule) (cond 
		((member (cadr Rule) New) New)
	        (T (cons (cadr Rule) New)))))
	(T (update A (cdddr Rule) New))
))

; получаем множество нетерминальных символов в правиле
(defun set_in_rule (Rule Set_r) (cond
	((null Rule) Set_r)
	((null (cddr Rule)) (cond
		((member (car Rule) Set_r) Set_r)
		(T (cons (car Rule) Set_r))
	))
	((member (car Rule) Set_r) (set_in_rule (cdddr Rule) Set_r))
	(T (set_in_rule (cdddr Rule) (cons (car Rule) Set_r)))
))

; проверка на детерминированность
(defun is_DKA (KA NewStates AllStates Flag) (cond
	((null KA) (cond
        	((null NewStates) Flag)
                (T NewStates)))
	((and (check_rule (caddar KA) nil) (eq (null (cdar KA)) nil)) (cond
	        ((null (caar KA)) (is_DKA (cdr KA) NewStates AllStates nil))
                ((eq (is_DKA (cdr KA) NewStates AllStates Flag) T) T)
                (T (cons (car KA) (is_DKA (cdr KA) NewStates AllStates nil)))
	))
    	((and(is_in (caar KA) AllStates) (null (cdar KA))) (is_DKA (cdr KA) NewStates AllStates nil))
    	((and (is_in (caar KA) AllStates) (is_in (caadr (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil)) AllStates)) (cons (cons (caar KA) (cons '= (cons (level 2 (car (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil))) nil))) 
			 (is_DKA (cdr KA) NewStates ALLStates nil)))
    	((is_in (caar KA) AllStates) (cons (cons (caar KA) (cons '= (cons (level 2 (car (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil))) nil))) 
			 (is_DKA (cdr KA) (cons (cadr (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil)) NewStates) (cons (caadr (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil)) ALLStates) nil)))
    	((is_in (caadr (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil)) ALLStates) (cons (cons (caar KA) (cons '= (cons (level 2 (car (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil))) nil))) 
			 (is_DKA (cdr KA) NewStates AllStates nil)))
	(T (cons (cons (caar KA) (cons '= (cons (level 2 (car (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil))) nil))) 
			 (is_DKA (cdr KA) (cons (cadr (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil)) NewStates) (cons (caadr (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil)) ALLStates) nil)))
))  


; проверяем правила на "детерминированность"
(defun check_rule (Rule Unique) (cond
	((null Rule) T)
	((null (cddr Rule)) (eq (member (car Rule) Unique) nil))
	((member (car Rule) Unique) nil)
	(T (check_rule (cdddr Rule) (cons (car Rule) Unique)))
))

(defun is_empty_new (Gram) (cond
	((null Gram) T)
    	((null (cdar Gram)) nil)
	(T (is_empty_new (cdr Gram)))
))


(defun update_states1 (Rules Full) (cond
    ((is_empty_new (update_states Rules Full)) (update_states Rules Full))
    (T (level 2 (cons (update_states1 (to_new_states (update_states Rules Full) nil (get_all_states (to_old_states (update_states Rules Full) nil) nil)) (cons Full (to_old_states (update_states Rules Full) nil))) (cons (to_old_states (update_states Rules Full) nil) nil))))
))


; обновляем правила для состояний
(defun update_states (Rules Full) (cond
	((null Rules) Rules)
    	((is_DKA (cons (cons (caar Rules) (cons '= (cons (cdr (level 4 (update_state (caar Rules) Full))) nil))) 
								(update_states (cdr Rules) Full)) nil (get_all_states (cons (cons (caar Rules) (cons '= (cons (cdr (level 4 (update_state (caar Rules) Full))) nil))) 
								(update_states (cdr Rules) Full)) nil) T) (is_DKA (cons (cons (caar Rules) (cons '= (cons (cdr (level 4 (update_state (caar Rules) Full))) nil))) 
								(update_states (cdr Rules) Full)) nil (get_all_states (cons (cons (caar Rules) (cons '= (cons (cdr (level 4 (update_state (caar Rules) Full))) nil))) 
								(update_states (cdr Rules) Full)) nil) nil))
	(T (is_DKA (cons (cons (caar Rules) (cons '= (cons (cdr (level 4 (update_state Rules Full))) nil))) 
								(update_states (cdr Rules) Full)) nil (get_all_states (cons (cons (caar Rules) (cons '= (cons (cdr (level 4 (update_state (caar Rules) Full))) nil))) 
								(update_states (cdr Rules) Full)) nil) T))
))


; добавляем правила для новых состояний
(defun update_state (State Rules) (cond
	((null State) State)
    	(T (cons (find_state (car State) Rules nil) (update_state (cdr State) Rules)))
))       


; находим правила, где встречаются новые состояния
(defun find_state (NewState Rules NewRule) (cond
    	((null (cadar Rules)) NewRule)
    	((eq NewState (caaar Rules)) (find_state NewState (cdr Rules) (cons (cons '(#\\) (cddar Rules)) NewRule)))
    	((and (eq (atom (caaar Rules)) nil) (member NewState (caaar Rules))) (find_state NewState (cdr Rules) (cons (cons '(#\\) (cddaar Rules)) NewRule)))
    	(T (find_state NewState (cdr Rules) NewRule))
))


; --------------------------------------------

; БЛОК ПОЛУЧЕНИЯ ДЕТЕРМИНИРОВАННОГО КОНЕЧНОГО АВТОМАТА
; получаем список только из новых состояний
(defun to_new_states (Gram NewStates AllStates) (cond
	((null Gram) NewStates)
	((and (null (cdar Gram)) (is_in (car Gram) AllStates)) (to_new_states (cdr Gram) NewStates AllStates))
    	((null (cdar Gram)) (to_new_states (cdr Gram) (cons (car Gram) NewStates) (cons (car Gram) AllStates)))
	(T (to_new_states (cdr Gram) NewStates AllStates))
))

(defun to_old_states (Gram OldStates) (cond
	((null (cdar Gram)) OldStates)
	(T (to_old_states (cdr Gram) (cons (car Gram) OldStates)))
))


; очистка от лишних состояний
(defun clear (Gram NewGram) (cond
    	((null Gram) NewGram)
    	(T (clear (cdr Gram) (cons (cons (caar Gram) (cons (cadar Gram) (cons (clear_rule (reverse (clear_rule (reverse (caddar Gram))))) nil))) NewGram)))
))
                                

(defun clear_rule (Rule) (cond
    	((eq (car Rule) '#\\) (cdr Rule))
    	(T Rule))) 


(defun check_final_state (Gram) (cond
	((null Gram) Gram)
    	(T (cons (cons (caar Gram) (cons '= (cons (check_fin (caddar Gram) (caar Gram)) nil))) (check_final_state (cdr Gram))))
))


(defun check_fin (Rule State) (cond
    	((and (null Rule) (member 'S State)) '(#\\ #\* S))
    	((null Rule) Rule)
    	((and (eq (car Rule) 'S) (not (eq (member 'S State) T))) (cons '(S S) (check_fin (cdr Rule) State)))
    	((eq (car Rule) 'S) (cons '(S S) (append (check_fin (cdr Rule) State) '(#\\ #\* S))))
    	(T (cons (car Rule) (check_fin (cdr Rule) State)))
))


; переводим приведенную грамматику в ДКА
(defun gram_to_DKA (Gram) (cond
	((null Gram) (print 'DKA) Gram)
	(T (append (to_graph (caar Gram) (caddar Gram) nil) (gram_to_DKA(cdr Gram))))
))


; каждое правило превращаем в ребра графа
(defun to_graph (State Rule Graph)  (cond
	((null Rule) Graph)                       
	((atom (cadr Rule)) (to_graph State (cdddr Rule) (cons (cons State (cons (car Rule) (cons (cons (cadr Rule) nil) nil))) Graph)))
	(T (to_graph State (cdddr Rule) (cons (cons State (cons (car Rule) (cons (cadr Rule) nil))) Graph)))
))

; --------------------------------------------

; БЛОК MAIN

(defun main (KA)
	(let ((is_DKA_var (is_DKA (make_gram (make_rule KA nil) nil) nil (make_gram (make_rule KA nil) nil) T)))
	(cond 
        	((eq is_DKA_var T) (print 'deterministic) (delete_S (make_gram (make_rule KA nil) nil)))	
        	(T 
        		(print 'non-deterministic)
         		;(print(check_final_state(clear (level 2 (cons (update_states1 (to_new_states is_DKA_var nil (get_all_states (make_gram (make_rule KA nil) nil) nil)) (make_gram (make_rule KA nil) nil))  (cons (to_old_states is_DKA_var nil) nil)))nil)))
         		(let ((gram_with_S (check_final_state(clear(level 2 (cons (update_states1 (to_new_states is_DKA_var nil (get_all_states (make_gram (make_rule KA nil) nil) nil)) (make_gram (make_rule KA nil) nil))  (cons (to_old_states is_DKA_var nil) nil))) nil))))
         		(print (gram_to_DKA gram_with_S))
         		(delete_S gram_with_S))
))))

(print (main '(((H) #\s (S)))))
(terpri)
(print (main '(((H) #\a (A)) ((H) #\b (B)) ((H) #\s (S)))))
(terpri)
(print (main '(((H) #\a (S)) ((H) #\b (A)) ((A) #\a (A)) ((A) #\a (S)))))
(terpri)
(print (main '(((H) #\a (A)) ((H) #\a (B)) ((A) #\a (A)) ((A) #\b (B)) ((A) #\s (S)) ((B) #\a (B)) ((B) #\b (B)) ((B) #\s (S)))))
(terpri)
(print (main '(((H) #\1 (A)) ((H) #\1 (B)) ((A) #\0 (B)) ((A) #\1 (C)) ((B) #\1 (A)) ((B) #\s (S)) ((C) #\1 (A)) ((C) #\1 (C)))))
(terpri)
(print (main '(((H) #\0 (A)) ((A) #\1 (B)) ((A) #\1 (A)) ((B) #\1 (C))  ((C) #\0 (S)))))
(terpri)
(print (main '(((H) #\0 (A)) ((A) #\1 (A)) ((A) #\1 (A)) ((B) #\1 (C))  ((C) #\0 (S)))))
(terpri)
(print (main '(((H) #\1 (B)	) ((B) #\1 (B)) ((B) #\1 (A)) ((A) #\1 (C)) ((C) #\0 (A)) ((C) #\* (S)))))
(terpri)
(print (main '(((H) #\a (A)) ((H) #\a (B)) ((H) #\b (B)) ((H) #\* (S)) ((A) #\a (A)) ((A) #\a (B)) ((A) #\* (S)) ((B) #\b (A)) ((B) #\* (S)))))
(terpri)
(print (main '(((H) #\a (B)) ((H) #\b (A)) ((A) #\a (A)) ((A) #\a (B)) ((A) #\b (A)) ((A) #\b (B)) ((A) #\* (S)) ((B) #\a (A)) ((B) #\a (B))((B) #\b (A)) ((B) #\b (B)) ((B) #\* (S)))))
(terpri)
(print (main '(((H) #\b (A)) ((H) #\b (B)) ((H) #\a (C)) ((A) #\b (C)) ((B) #\a (C)) ((C) #\a (A)) ((C) #\b (B)) ((C) #\s (S)))))
(terpri)
(print (main '(((H) #\p (A)) ((H) #\m (B)) ((H) #\s (S)) ((A) #\p (A)) ((A) #\p (B)) ((A) #\m (A)) ((A) #\s (S)) ((B) #\p (B)) ((B) #\m (B)) ((B) #\m (A)) ((B) #\s (S)))))
