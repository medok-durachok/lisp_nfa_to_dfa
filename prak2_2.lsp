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
	(T (cons (plus (caaar Rule) Rule nil) (make_gram (cdr Rule) (cons (caaar Rule) Unique))))
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
	((null L) (cons (cons A nil) (replace_eq (cons '= (check_same_in_rule(level 2 R))) 0))) 
	((eq A (caaar L)) (plus A (cdr L) (cons (cdar L) R)))
	(T (plus A (cdr L) R))
))


; проверка на "одинаковость" в переходах: например, A —> aB | aB будет переведено в А —> aB
; такой автомат будет детерминированным
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


; получаем все состояния в грамматике
(defun get_all_states (Gram States) (cond
	((null Gram) States)
	(T (get_all_states (cdr Gram) (cons (caar Gram) (get_states (cddar Gram) States))))
))


; получаем состояния из правила
(defun get_states (Rule States) (cond
	((null (cddr Rule)) States)
	((is_in (cdar Rule) States) (get_states (cdddr Rule) States))
	(T (get_states (cdddr Rule) (cons (cdar Rule) States)))
))


; проверка на наличие спискового элемента в списке списков
(defun is_in (El L)(cond
	((null L) nil)
	((check_similarity El (car L)) T)
	(T (is_in El (cdr L)))
))


; проверяем на равенство, чтобы вернуть список
(defun is_equal (L1 L2) (cond
	((eq (len L1) (len L2)) (is_similar_set L1 L2 L1))
	(T L1)
))


; здесь проверяем, одинаковые ли длины, и если да, то передаем на проверку "равенства" списков, возвращаем T/nil
(defun check_similarity (L1 L2) (cond
	((eq (len L1) (len L2)) (is_similar_els L1 L2))
))


; проверяем списки на совпадение по множеству символов
(defun is_similar_els (L1 L2) (cond
	((null L1) T)
	((member (car L1) L2) (is_similar_els (cdr L1) L2))
))


; проверка на совпадение по множеству для приведения (А В), (В А) —> (B A), (B A)
(defun is_similar_set (L1 L2 L1_not) (cond
	((null L1) L2)
	((member (car L1) L2) (is_similar_set (cdr L1) L2 L1_not))
	(T L1_not)
))


; удаляем S из грамматики после получения ДКА
(defun delete_S (Gram) (cond
	((null Gram) (print 'grammar) Gram)
	(T (cons (cons (caar Gram) (cons (cadar Gram) (cons (del_S (caddar Gram)) nil))) (delete_S (cdr Gram))))
))


; удаляем S из правила
(defun del_S (Rule) (cond
	((null Rule) Rule)
	((eq (car Rule) 'S ) (del_S (cdr Rule)))
	(T (cons (car Rule) (del_S (cdr Rule))))
))

; --------------------------------------------

; БЛОК ПРИВЕДЕНИЯ ГРАММАТИКИ
; возвращаем старый автомат с новыми правилами + новые состояния в конце
(defun to_DKA (Rule Set_r NewRule NewState) (cond
	((null Set_r) (cons (cdr NewRule) (to_list NewState)))
    	((null (cdadr (update (car Set_r) Rule nil))) (to_DKA Rule (cdr Set_r) (cons '(#\\) (cons (cons (caar (update (car Set_r) Rule nil)) (cadr (update (car Set_r) Rule nil)))  NewRule)) NewState))
    	(T (to_DKA Rule (cdr Set_r) (cons '(#\\) (cons (to_list_for_rule(car (update (car Set_r) Rule nil))) NewRule)) (cons (cadr (update (car Set_r) Rule nil)) NewState)))                                          
))


(defun to_list_for_rule (r) (cons (car r) (cons (to_one_state (cadr r) nil)nil)))

(defun to_list (l) (cond
	((null l) l)
    	(T (cons (cons (to_one_state (car l) nil) nil) (to_list (cdr l))))))


(defun to_one_state (State Atoms) (cond
    	((null State) (unique Atoms nil))
    	((atom (car State)) (to_one_state (cdr State) (cons (car State) Atoms)))
   	(T (to_one_state (cdr State) (level 2 (cons (car State) (cons Atoms nil)))))
))


(defun unique (Old New) (cond
    	((null Old) New)
    	((member (car Old) New) (unique (cdr Old) New))
    	(T (unique (cdr Old) (cons (car Old) New)))
))

(defun update (A Rule New) (cond
    	((null (cddr Rule)) (cond 
	    	((eq A (car Rule)) (cons (cons A (cons (cons (cadr Rule) New) nil)) (cons (cons (cadr  Rule) New) nil)))
        	(T (cons (cons A (cons New nil)) (cons New nil)))
	))
    	((eq A (car Rule)) (update A (cdddr Rule) (cons (cadr Rule) New)))
    	(T (update A (cdddr Rule) New))
))


; получаем множество терминальных символов в правиле
(defun set_in_rule (Rule Set_r) (cond
	((null Rule) Set_r)
	((null (cddr Rule)) (cond
		((member (car Rule) Set_r) Set_r)
		(T (cons (car Rule) Set_r))
	))
	((member (car Rule) Set_r) (set_in_rule (cdddr Rule) Set_r))
	(T (set_in_rule (cdddr Rule) (cons (car Rule) Set_r)))
))


; получаем множество нетерминальных символов в правиле
(defun state_set_in_rule (Rule Set) (cond
	((null Rule) Set)
	((null (cddr Rule)) (cond
        ((and (atom (cadr Rule)) (is_in (cons (cadr Rule) nil) Set) Set)) 
        ((atom (cadr Rule)) (cons (cons (cadr Rule) nil) Set))
        ((is_in (cadr Rule) Set) Set)        
		(T (cons (cadr Rule) Set))
	))
    	((and (atom (cadr Rule)) (is_in (cons (cadr Rule) nil) Set)) (state_set_in_rule (cdddr Rule) Set))
    	((atom (cadr Rule)) (state_set_in_rule (cdddr Rule) (cons (cons (cadr Rule)nil) Set)))           
	((is_in (cadr Rule) Set) (state_set_in_rule (cdddr Rule) Set))
	(T (state_set_in_rule (cdddr Rule) (cons (cadr Rule) Set)))
))


(defun state_set (L Set) (cond
	((null L) (to_list Set))
	((is_in (caar L) Set) (state_set (cdr L) Set))
	(T (state_set (cdr L) (cons (caar L) Set)))
))


(defun New_wo_Old (New Old) (cond
    	((null New) New)
    	((is_in (car New) Old) (new_wo_old (cdr New) Old))
    	(T (cons (car New) (new_wo_old (cdr new) old)))
))

; проверка на детерминированность
(defun is_DKA (KA NewStates AllStates Flag) (cond
    	((null KA) (cond 
        	((null NewStates)  Flag)
                (T (level 2 (new_wo_old NewStates AllStates)))
    	))
    	((and (check_rule (caddar KA) nil) (eq (null (cdar KA)) nil)) (cond
	        ((null (caar KA)) (is_DKA (cdr KA) NewStates AllStates nil))
                ((eq (is_DKA (cdr KA) NewStates AllStates Flag) T) T)
                (T (cons (car KA) (is_DKA (cdr KA) NewStates AllStates nil)))
	))
    	(T (cons (cons (caar KA) (cons '= (cons (level 2 (car (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil))) nil))) 
		(is_DKA (cdr KA) (cons (cdr (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil)) NewStates) 
		(cons (caadr (to_DKA (caddar KA) (set_in_rule (caddar KA) nil) nil nil)) ALLStates) nil)))
))

; проверяем правила на детерминированность
(defun check_rule (Rule Unique) (cond
	((null Rule) T)
	((null (cddr Rule)) (eq (member (car Rule) Unique) nil))
	((member (car Rule) Unique) nil)
	(T (check_rule (cdddr Rule) (cons (car Rule) Unique)))
))


; проверяем, появились ли новые состояния после детерминирования
(defun is_empty_new (Gram All) (cond
	((null Gram) T)
   	((null (cdar Gram)) (cond 
                ((is_in (caar Gram) All) (is_empty_new (cdr Gram) All))
                (T nil)
        ))
	(T (is_empty_new (cdr Gram) All))
))


(defun get_first_states (Gram Res) (cond
    	((null Gram) Res)
    	(T (get_first_states (cdr Gram) (cons (caar Gram) Res)))
))

; обновление новых состояний и либо возвращаем ДКА, либо еще раз детерминируем
(defun update_states1 (Rules Full) (cond
    	((is_empty_new (update_states (new_st Rules Full) Full) (get_first_states(level 2 (cons Full (cons (new_st Rules Full) nil)))nil)) (update_states (new_st Rules Full) Full))
    	(T (level 2 (cons (update_states1(state_set(to_new_states (update_states (new_st Rules Full) Full) nil (get_all_states (to_old_states (update_states (new_st Rules Full) Full) nil) nil))nil) (level 2 (cons Full (cons (to_old_states (update_states (new_st Rules Full) Full) nil)nil)))) (cons (to_old_states (update_states (new_st Rules Full) Full) nil) nil))))
))


(defun new_st (Rules Full) (cond
    	((null Rules) Rules)
    	(T (cons (cons (caar Rules) (cons '= (cons (cdr (level 4 (update_state (caar Rules) Full))) nil))) (new_st (cdr Rules) Full)))
))


; обновляем правила для состояний
(defun update_states (Rules Full) (cond
	((null Rules) Rules)
    	((is_DKA Rules nil (get_all_states (cons (cons (caar Rules) (cons '= (cons (cdr (level 4 (update_state (caar Rules) Full))) nil))) 
		(update_states (cdr Rules) Full)) nil) T) (is_DKA Rules nil (get_all_states (cons (cons (caar Rules) (cons '= (cons (cdr (level 4 (update_state (caar Rules) Full))) nil))) 
		(update_states (cdr Rules) Full)) nil) nil))
	(T (is_DKA Rules nil (get_all_states (cons (cons (caar Rules) (cons '= (cons (cdr (level 4 (update_state (caar Rules) Full))) nil))) 
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
    	((and (eq NewState (caaar Rules)) (not (cdaar Rules))) (find_state NewState (cdr Rules) (cons (cons '(#\\) (cddar Rules)) NewRule)))
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


; получаем список только из старых состояний
(defun to_old_states (Gram OldStates) (cond
	((null (cdar Gram)) OldStates)
	(T (to_old_states (cdr Gram) (cons (car Gram) OldStates)))
))


; получаем "канонический" порядок для новых состояний; к нему приведем все остальные новые, 
; чтобы не было двух состояний вида (Y X) (X Y), которые являются одним и тем же состоянием
(defun find_orders (Gram Orders) (cond
	((null Gram) Orders)
	((null (cdar Gram)) (find_orders (cdr Gram) Orders))
	(T (find_orders (cdr Gram) (cons (car Gram) Orders)))
))


; проходим по всей грамматике
(defun check_all_orders (Gram Orders) (cond
	((null Gram) Gram)
	(T (cons (cons (caar Gram) (cons '= (check_order Orders (caddar Gram)))) (check_all_orders (cdr Gram) Orders)))
))


; проверяем порядок для приведения состояния (Y X) к состоянию (X Y)
; проверяем каждое из новых состояний в правиле
(defun check_order (Orders Rule)(cond
	((null Orders) (cons Rule nil))
	(T (check_order (cdr Orders) (check_state_order (car Orders) Rule)))
))


; проход внутри правила, ищем "списковое" состояние.
; если списковое, то передаем на проверку
(defun check_state_order (State Rule) (cond
    	((null Rule) Rule)
	((atom (car Rule)) (cons (car Rule) (check_state_order State (cdr Rule))))
	(T (cons (is_equal (car Rule) State) (check_state_order State (cdr Rule))))
))


; очистка от лишних состояний
(defun clear (Gram NewGram) (cond
    	((null Gram) (check_all_orders NewGram (find_orders (get_all_states NewGram nil) nil)))
        ((null (caddar Gram)) (clear (cdr Gram) NewGram))
    	(T (clear (cdr Gram) (cons (cons (caar Gram) (cons (cadar Gram) (cons (clear_rule (reverse (clear_rule (reverse (caddar Gram))))) nil))) NewGram)))
))
                                

(defun clear_rule (Rule) (cond
    	((eq (car Rule) '#\\) (cdr Rule))
    	(T Rule))) 


(defun unique_rules (Gram Set) (cond
	((null Gram) Gram)
	((is_in (caar Gram) Set) (unique_rules (cdr Gram) Set))
	(T (cons (car Gram) (unique_rules (cdr Gram) (cons (caar Gram) Set))))
))


; проверяем, появились ли новые конечные состояния
(defun if_S_in (Gram) (cond
	((null Gram) nil)
	((not (eq (member 'S (caar Gram)) nil)) T)
	(T (if_S_in (cdr Gram)))
))


; 
(defun check_final_state (Gram Flag Check) (cond
	((null Gram) (cond
	        (Flag '(((S S) = (#\* S))))
                (T Gram)
        ))
	((= Check 0) (check_final_state Gram (if_S_in Gram) 1))
    	(T (cons (cons (caar Gram) (cons '= (cons (check_fin (caddar Gram) (caar Gram) Flag) nil))) (check_final_state (cdr Gram) Flag 1)))
))


; проверяем состояния S, чтобы добавить переходы, если у нас несколько конечных состояний
(defun check_fin (Rule State Flag) (cond
    	((and (null Rule) (not (eq (member 'S State) nil))) '(#\\ #\* S))
    	((null Rule) Rule)
    	((and (eq (car Rule) 'S) Flag) (cons '(S S) (check_fin (cdr Rule) State Flag)))
    	((and (not(atom (car Rule))) (null (cdar Rule))) (cons (caar Rule) (check_fin (cdr Rule) State Flag)))
    	(T (cons (car Rule) (check_fin (cdr Rule) State Flag)))
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


; БЛОК УДАЛЕНИЯ НЕДОСТИЖИМЫХ СОСТОЯНИЙ
; находим начальное состояние Н, отправляем состояния, в которые переходит Н и проходим по ним, получая свои состояния
(defun delete_unreach (Gram) (delete_non Gram (find_others Gram (find_H Gram) (find_H Gram))))


; удаляем недостижимые
(defun delete_non (Gram List) (cond
    	((null Gram) Gram)
    	((is_in (caar Gram) List) (cons (car Gram) (delete_non (cdr Gram) List)))
    	(T (delete_non (cdr Gram) List))
))


; находим начальное состояние
(defun find_H (Gram) (cond
	((eq (caaar Gram) 'H) (state_set_in_rule (caddar Gram) nil))
	(T (find_H (cdr Gram)))
))


; проходим по состояниям, которые являются достижимыми
(defun find_others (Gram Reach AllReach) (cond
	((null Reach)(cons '(H) AllReach))
    	((null (find_one Gram (car Reach))) (find_others Gram (cdr Reach) AllReach))
	(T (find_others Gram (cdr (updated_reach (find_one Gram (car Reach)) Reach AllReach)) (level 2 (cons AllReach (cons (updated_reach (find_one Gram (car Reach)) Reach AllReach) nil)))))
))


; находим состояния, в которые переходит текущее состояние из списка достижимых
(defun find_one (Gram State) (cond
    	((null Gram) nil)
	((check_similarity State (caar Gram)) (state_set_in_rule (caddar Gram) nil))
	(T (find_one (cdr Gram) State))
))


; добавление в конец
(defun add_to_end (El L) (cond
    	((null L) (cons El nil))
    	(T (cons (car L) (add_to_end EL (cdr L))))
))


; обновляем список достижимых
(defun updated_reach (New Old All) (cond
	((null New) Old)
	((is_in (car New) All) (updated_reach (cdr New) Old All))
	(T (updated_reach (cdr New) (add_to_end (car New) Old) All))
))
; --------------------------------------------

; БЛОК MAIN
(defun main (KA)
	(let ((is_DKA_var (is_DKA (make_gram (make_rule KA nil) nil) nil (make_gram (make_rule KA nil) nil) T)))
	(cond 
        	((eq is_DKA_var T) (print '|deterministic|) (delete_S (make_gram (make_rule KA nil) nil)))	
        	(T 
        		(print '|non-deterministic|)
         		(let ((gram_with_S (delete_unreach(check_final_state(clear(unique_rules(level 2 (cons (update_states1 (state_set(to_new_states is_DKA_var nil (get_all_states (make_gram (make_rule KA nil) nil) nil))nil) (make_gram (make_rule KA nil) nil)) (cons (to_old_states is_DKA_var nil) nil)))nil) nil) nil 0))))
                	(print (gram_to_DKA gram_with_S))
         		(delete_S gram_with_S))
))))

; ТЕСТЫ
(print (main '(((H) #\a (A)) ((A) #\b (A)) ((A) #\b (B)) ((B) #\a (B)) ((B) #\a (A)) ((B) #\b (S)))))
(terpri)
(print (main '(((H) #\a (A)) ((H) #\a (B)) ((A) #\a (A)) ((A) #\b (S)) ((B) #\b (B)) ((B) #\a (S)))))
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
(print (main '(((H) #\1 (B)) ((B) #\1 (B)) ((B) #\1 (A)) ((A) #\1 (C)) ((C) #\0 (A)) ((C) #\* (S)))))
(terpri)
(print (main '(((H) #\a (A)) ((H) #\a (B)) ((H) #\b (B)) ((H) #\* (S)) ((A) #\a (A)) ((A) #\a (B)) ((A) #\* (S)) ((B) #\b (A)) ((B) #\* (S)))))
(terpri)
(print (main '(((H) #\a (B)) ((H) #\b (A)) ((A) #\a (A)) ((A) #\a (B)) ((A) #\b (A)) ((A) #\b (B)) ((A) #\* (S)) ((B) #\a (A)) ((B) #\a (B))((B) #\b (A)) ((B) #\b (B)) ((B) #\* (S)))))
(terpri)
(print (main '(((H) #\b (A)) ((H) #\b (B)) ((H) #\a (C)) ((A) #\b (C)) ((B) #\a (C)) ((C) #\a (A)) ((C) #\b (B)) ((C) #\s (S)))))
(terpri)
(print (main '(((H) #\p (A)) ((H) #\m (B)) ((H) #\s (S)) ((A) #\p (A)) ((A) #\p (B)) ((A) #\m (A)) ((A) #\s (S)) ((B) #\p (B)) ((B) #\m (B)) ((B) #\m (A)) ((B) #\s (S)))))
(terpri)
(print (main '(((H) #\b (A)) ((H) #\b (B)) ((H) #\a (C)) ((A) #\b (C)) ((B) #\a (C)) ((C) #\a (A)) ((C) #\b (B)) ((C) #\s (S)))))
(terpri)
(print (main '(((H) #\a (A)) ((H) #\b (B)) ((H) #\s (S)) ((A) #\a (A)) ((A) #\a (B)) ((A) #\b (B)) ((A) #\s (S)) ((B) #\a (A)) ((B) #\b (A)) ((B) #\b (B)) ((B) #\s (S)))))
(terpri)
(print (main '(((H) #\0 (A)) ((H) #\0 (B)) ((A) #\1 (C)) ((B) #\1 (B)) ((B) #\1 (A)) ((C) #\0 (A)) ((C) #\s (S)))))
(terpri)
(print (main '(((H) #\0 (A)) ((H) #\0 (B)) ((H) #\1 (C)) ((A) #\0 (B)) ((A) #\1 (C)) ((B) #\1 (B)) ((B) #\1 (A)) ((C) #\s (S)))))
(terpri)
(print (main '(((H) #\1 (A)) ((H) #\1 (A)) ((A) #\0 (C)) ((C) #\0 (S)) ((C) #\1 (D)) ((C) #\1 (D)))))
(terpri)
(print (main '(((H) #\1 (A)) ((H) #\1 (A)) ((A) #\0 (B)) ((A) #\0 (C)) ((C) #\0 (S)) ((C) #\1 (D)) ((C) #\1 (D)))))
(terpri)
(print (main '(((H) #\a (S)) ((H) #\b (C)) ((H) #\b (B)) ((H) #\c (A)) ((A) #\a (A)) ((A) #\b (C)) ((A) #\b (B)))))
(terpri)
(print (main '(((H) #\a (S)) ((H) #\b (C)) ((H) #\b (B)) ((H) #\c (A)) ((A) #\a (B)) ((A) #\a (D)) ((A) #\b (C)) ((A) #\b (B)))))
