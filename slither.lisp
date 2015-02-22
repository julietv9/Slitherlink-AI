(defun slither()	
	(format t "~%WELCOME TO SLITHERLINK~%~%")
	(format t "~%This game is played on a grid with '+'s at each of the grid corners. ~%Some of the squares contain a number between 0 and 3 inclusively. The number identifies the number of lines that will surround that particular square. ~%Your objective is to find a single looped path with no crosses or branches.~%~%")
	(loop while(yes-or-no-p "START A NEW GAME?~%")
		do
		(return (start-game))))
		
(defun start-game()
	(defvar FileName)
	(defvar boardlist)
	(defvar row)
	(defvar column)
	(defvar board)
	(defvar buffer)
	(defvar check)
	(defvar input)
	(defvar input_init)

	(setq FileName (input-board))
	(setq boardlist (print-list FileName)) 
	
	(setq row (length boardlist))
	(setq column (length (car boardlist)))
	
	(format t "~%Loading the Board~%~%~%")
	(print-board boardlist column)

	(setq board (make-array (list row column) :initial-contents boardlist))
	
    (format t "~%Each move in this game will be specified as a triple 'N N L': two numbers separated by a blank followed by blank and a letter. No other punctuation or characters is allowed. ~%The two numbers will identify the row then column, starting in the upper left corner, where a line will be added and the letter will identify the its position on the square. ~%T for top, B for bottom, L for left, and R for right. ~%For example: 2 1 T specifies the second row, first column, and top side. This could have also been specified as: 1 1 B. ~%Specifying a position where there already is a line will remove the line.~%The rows and columns are numbered for the board!~%~%")
	(loop
		(setq input (read-user-moves))

		(if (string= (string-downcase (string-trim " " input)) "quit") (progn (slither) (return)))
		
		(if (string= (string-downcase (string-trim " " input)) "solve") (progn (time (primary-constraints)) (slither) (return)))
			
		(setq buffer (check-moves (extract-moves input) row column))
		(if (> (length buffer) 1) (progn (setq check (make-moves buffer row column))
										 (if check (progn (format t "~%~%YOU WIN! CONGRATULATIONS!!!~%~%") (slither) (return))))))
)

(defun input-board()
	(format t "~%Enter File Name of the board you want to play! Format: 'abc.txt'~%")
    (loop for filename = (read-line *query-io*)
       until (probe-file filename)
       do (format t "~%File for the board not found! Enter File Name Again:~%")
       finally (return filename)))

(defun print-list(filename)
	(with-open-file (stream filename)
	(loop for line = (read-line stream nil nil)
		while line
			collecting line into lines
			finally(return lines))))

(defun print-board(boardasalist column)
	(defvar print_num)
	(defvar element_count)
	(setq print_num 97)
	(setq element_count 0)
	(format t "  ")
    (loop for i from 2 to column
	  do
		 (if (= (rem i 2) 0) (format t " ") (progn (format t "~a" (code-char print_num)) (incf print_num))))
	(setq print_num 97)
	(format t  "~%~%")
	(dolist(element boardasalist)
		(if (= (rem element_count 2) 0) (format t "  ") (progn (format t "~a" (code-char print_num)) (format t " ") (incf print_num)))
		(format t "~A~%" element)
		(incf element_count)))
		
(defun read-user-moves()
	(format t "~% ~%Type Solve for Intelligence Play or Make your move in the above specified format for Human Play. If neither, type Quit to exit the current game!~%~%")
	(read-line))

(defun extract-moves (input)
    (loop for s = 0 then (1+ e)
          as e = (position #\Space input :start s)
          collect (subseq input s e) into moves
          while e
          finally (return moves)))

(defun check-moves(var row column)
    (if (equal (length var) 3)
	    (if (or (equal (car var) "") (equal (cadr var) "") (equal (caddr var) "")) 
			(progn (format t "~%~%Invalid Input: Not as per choices~%~%") nil)
			(if (and (and (numberp (read-from-string (car var)))
						  (<= (read-from-string (car var)) (floor row 2)))
					 (and (numberp (read-from-string (cadr var)))
					      (<= (read-from-string (cadr var)) (floor column 2)))
					 (case (char-downcase (character (caddr var)))
								(#\t t)
								(#\b t)
								(#\l t)
								(#\r t)))
			var (progn (format t "~%~%Invalid Referencing of Position~%~%") nil))) (progn (format t "~%~%Invalid Input: Not as per choices~%~%") nil)))
			
(defun make-moves(moves row column)
	(defvar row1)
	(defvar column1)
	(defvar position)
	(defvar horizontal-line)
	(defvar vertical-line)
	(defvar space)
	(defvar row_pos)
	(defvar column_pos)
	(defvar line)
	(defvar condition)
	(setq row1 (parse-integer (car moves)))
	(setq column1 (parse-integer (cadr moves)))
    (setq position (string-downcase (caddr moves)))
	(setq horizontal-line "-")
	(setq vertical-line "|")
	(setq space #\Space)
	(cond((equal position "t") (progn 
									(setq row_pos (- (* 2 row1) 2))
									(setq column_pos (- (* 2 column1) 1))
									(setq line horizontal-line)
									(if (equal (aref board (- (* 2 row1) 2) (- (* 2 column1) 1)) space)
									(setf (aref board (- (* 2 row1) 2) (- (* 2 column1) 1)) horizontal-line) (setf (aref board (- (* 2 row1) 2) (- (* 2 column1) 1)) space))))
									
		 ((equal position "b") (progn
									(setq row_pos (* 2 row1))
									(setq column_pos (- (* 2 column1) 1))
									(setq line horizontal-line)									
									(if (equal (aref board  (* 2 row1)  (- (* 2 column1) 1)) space)
									(setf (aref board  (* 2 row1)  (- (* 2 column1) 1)) horizontal-line) (setf (aref board  (* 2 row1)  (- (* 2 column1) 1)) space))))
									
		 ((equal position "l") (progn
									(setq row_pos (- (* 2 row1) 1))
									(setq column_pos (- (* 2 column1) 2))	
									(setq line vertical-line)									
									(if (equal (aref board (- (* 2 row1) 1) (- (* 2 column1) 2)) space)
									(setf (aref board (- (* 2 row1) 1) (- (* 2 column1) 2)) vertical-line) (setf (aref board (- (* 2 row1) 1) (- (* 2 column1) 2)) space))))
									
		 ((equal position "r") (progn 
									(setq row_pos (- (* 2 row1) 1))
									(setq column_pos (* 2 column1))
									(setq line vertical-line)									
									(if (equal (aref board (- (* 2 row1) 1)  (* 2 column1)) space)
									(setf (aref board (- (* 2 row1) 1)  (* 2 column1)) vertical-line) (setf (aref board (- (* 2 row1) 1)  (* 2 column1)) space)))))								
	(print-moves row column)
	(setq condition (if (equal line "-") (horizontal-condition row column row_pos column_pos) (vertical-condition row column row_pos column_pos)))
	(if (and (= (car condition) 1) (= (cadr condition) 1)) (circuit-check row column row_pos column_pos)))

(defun print-moves(row column)
	(defvar print_num)
	(defvar element_count)
	
	(setq print_num 97)
	(setq element_count 0)
	
	(format t  "~%")
	(format t "  ")
	
    (loop for i from 2 to column
	  do
		 (if (= (rem i 2) 0) (format t " ") (progn (format t "~a" (code-char print_num)) (incf print_num))))
	(format t  "~%~%")
	
	(setq print_num 97)	 
	(loop for i from 0 to (- row 1) 
	do
	  (if (= (rem i 2) 0) (format t "  ") (progn (format t "~a" (code-char print_num)) (format t " ") (incf print_num)))
      (loop for j from 0 to (- column 1) 
	   do
         (format t "~A" (aref board i j)))
    (format t "~%")))
	
(defun boundary-check (r c i j)
	(if (and (>= i 0) (>= j 0) (< i r) (< j c)) t nil))
	
(defun horizontal-condition (r c rp cp)
    (defvar rp1)
	(defvar cp1)
	(defvar rp2)
	(defvar cp2)
	(defvar counter1)
	(defvar counter2)
	(defvar hl)
	(defvar vl)
	(setq rp1 0)
	(setq cp1 0)
	(setq rp2 0)
	(setq cp2 0)
	(setq counter1 0)
	(setq counter2 0)
	(setq hl "-")
	(setq vl "|")
	
	(if (and (boundary-check r c (- rp 1) (- cp 1)) (equal (aref board (- rp 1) (- cp 1)) vl)) 
		(progn (setq rp1 (- rp 1)) (setq cp1 (- cp 1)) (incf counter1)))
	
	(if (and (boundary-check r c (+ rp 1) (- cp 1)) (equal (aref board (+ rp 1) (- cp 1)) vl)) 
		(progn (setq rp1 (+ rp 1)) (setq cp1 (- cp 1)) (incf counter1)))
		
	(if (and (boundary-check r c rp (- cp 2)) (equal (aref board rp (- cp 2)) hl)) 
		(progn (setq rp1 rp) (setq cp1 (- cp 2)) (incf counter1)))
	
	(if (and (boundary-check r c (- rp 1) (+ cp 1)) (equal (aref board (- rp 1) (+ cp 1)) vl)) 
		(progn (setq rp2 (- rp 1)) (setq cp2 (+ cp 1)) (incf counter2)))
	
	(if (and (boundary-check r c (+ rp 1) (+ cp 1)) (equal (aref board (+ rp 1) (+ cp 1)) vl)) 
		(progn (setq rp2 (+ rp 1)) (setq cp2 (+ cp 1)) (incf counter2)))
	
	(if (and (boundary-check r c rp (+ cp 2)) (equal (aref board rp (+ cp 2)) hl)) 
		(progn (setq rp2 rp) (setq cp2 (+ cp 2)) (incf counter2)))
		
	(list counter1 counter2 (list rp1 cp1) (list rp2 cp2)))

(defun vertical-condition (r c rp cp)
    (defvar rp1)
	(defvar cp1)
	(defvar rp2)
	(defvar cp2)
	(defvar counter1)
	(defvar counter2)
	(defvar hl)
	(defvar vl)
	(setq rp1 0)
	(setq cp1 0)
	(setq rp2 0)
	(setq cp2 0)
	(setq counter1 0)
	(setq counter2 0)
	(setq hl "-")
	(setq vl "|")
	
	(if (and (boundary-check r c (- rp 1) (- cp 1)) (equal (aref board(- rp 1) (- cp 1)) hl)) 
		(progn (setq rp1 (- rp 1)) (setq cp1 (- cp 1)) (incf counter1)))
	
	(if (and (boundary-check r c (- rp 1) (+ cp 1)) (equal (aref board (- rp 1) (+ cp 1)) hl)) 
		(progn (setq rp1 (- rp 1)) (setq cp1 (+ cp 1)) (incf counter1)))
	
	(if (and (boundary-check r c (- rp 2) cp) (equal (aref board (- rp 2) cp) vl)) 
		(progn (setq rp1 (- rp 2)) (setq cp1 cp) (incf counter1)))

	(if (and (boundary-check r c (+ rp 1) (- cp 1)) (equal (aref board (+ rp 1) (- cp 1)) hl)) 
		(progn (setq rp2 (+ rp 1)) (setq cp2 (- cp 1)) (incf counter2)))
	
	(if (and (boundary-check r c (+ rp 1) (+ cp 1)) (equal (aref board (+ rp 1) (+ cp 1)) hl)) 
		(progn (setq rp2 (+ rp 1)) (setq cp2 (+ cp 1)) (incf counter2)))
	
	(if (and (boundary-check r c (+ rp 2) cp) (equal (aref board (+ rp 2) cp) vl)) 
		(progn (setq rp2 (+ rp 2)) (setq cp2 cp) (incf counter2)))
	
	(list counter1 counter2 (list rp1 cp1) (list rp2 cp2)))
	
(defun circuit-check(r c rp_new cp_new)
	(defvar number_counter)
	(defvar line_counter)
	(defvar loop_counter)
	(defvar var)
	(defvar line_count)
	(setq number_counter 0)
	(setq line_counter 0)

	(loop for i from 1 to (- r 1)
		do
			(loop for j from 1 to (- c 1)
				do
					(setq var (aref board i j))
					(if (numberp (digit-char-p var)) 
						(progn (incf number_counter) (setq line_count ( car (node-condition r c i j))) (if (= line_count (digit-char-p var)) (incf line_counter))))
					(incf j))
			(incf i))
	(if (equal line_counter number_counter) (loop-check r c rp_new cp_new) nil))
	
(defun node-condition (r c rp cp)
	(defvar counter1)
	(defvar counter2)
	(defvar hl)
	(defvar vl)
	(setq counter1 0)
	(setq counter2 0)
	(setq hl "-")
	(setq vl "|")
	(if (and (boundary-check r c rp (- cp 1)) (equal (aref board rp (- cp 1)) vl)) (incf counter1))
	(if (and (boundary-check r c rp (+ cp 1)) (equal (aref board rp (+ cp 1)) vl)) (incf counter1))
	(if (and (boundary-check r c (- rp 1) cp) (equal (aref board (- rp 1) cp) hl)) (incf counter1))
	(if (and (boundary-check r c (+ rp 1) cp) (equal (aref board (+ rp 1) cp) hl)) (incf counter1))
	
	(if (and (boundary-check r c rp (- cp 1)) (equal (aref board rp (- cp 1)) #\x)) (incf counter2))
	(if (and (boundary-check r c rp (+ cp 1)) (equal (aref board rp (+ cp 1)) #\x)) (incf counter2))
	(if (and (boundary-check r c (- rp 1) cp) (equal (aref board (- rp 1) cp) #\x)) (incf counter2))
	(if (and (boundary-check r c (+ rp 1) cp) (equal (aref board (+ rp 1) cp) #\x)) (incf counter2))

	(list counter1 counter2))
	
(defun loop-check (r c rp cp)
	(defvar node_counter)
	(defvar count_counter)
	(setq node_counter 0)
	(setq count_counter 0)
	(loop for i from 0 to (- r 1)
		do
			(loop for j from 0 to (- c 1)
				do
					(setq line_count (car (vertex-condition r c i j)))
					(if (> line_count 0) (incf count_counter))
					(if (= line_count 2) (incf node_counter))
					(incf j))
				(incf i))
	(if (= count_counter node_counter) (multiple-loop-check r c rp cp) nil)
	;;
	)
	
(defun vertex-condition (r c rp cp)
	(defvar counter1)
	(defvar counter2)
	(defvar counter3)
	(defvar hl)
	(defvar vl)
	(setq counter1 0)
	(setq counter2 0)
	(setq counter3 0)
	(setq hl "-")
	(setq vl "|")
	(if (and (boundary-check r c rp (- cp 1)) (equal (aref board rp (- cp 1)) hl)) (incf counter1))
	(if (and (boundary-check r c rp (+ cp 1)) (equal (aref board rp (+ cp 1)) hl)) (incf counter1))
	
	(if (and (boundary-check r c (- rp 1) cp) (equal (aref board (- rp 1) cp) vl)) (incf counter1))
	(if (and (boundary-check r c (+ rp 1) cp) (equal (aref board (+ rp 1) cp) vl)) (incf counter1))
	
	(if (and (boundary-check r c rp (- cp 1)) (equal (aref board rp (- cp 1)) #\x)) (incf counter2))
	(if (and (boundary-check r c rp (+ cp 1)) (equal (aref board rp (+ cp 1)) #\x)) (incf counter2))
	(if (and (boundary-check r c (- rp 1) cp) (equal (aref board (- rp 1) cp) #\x)) (incf counter2))
	(if (and (boundary-check r c (+ rp 1) cp) (equal (aref board (+ rp 1) cp) #\x)) (incf counter2))	
	
	(if (and (boundary-check r c rp (- cp 1)) (equal (aref board rp (- cp 1)) #\Space)) (incf counter3))
	(if (and (boundary-check r c rp (+ cp 1)) (equal (aref board rp (+ cp 1)) #\Space)) (incf counter3))
	(if (and (boundary-check r c (- rp 1) cp) (equal (aref board (- rp 1) cp) #\Space)) (incf counter3))
	(if (and (boundary-check r c (+ rp 1) cp) (equal (aref board (+ rp 1) cp) #\Space)) (incf counter3))	

	(list counter1 counter2 counter3))
	
(defun multiple-loop-check (r c rp cp)
	(defvar loop_path)
	(defvar i)
	(defvar j)
	(defvar final_counter)
	(defvar fag)
	(defvar ij1)
	(defvar ij2)
	(defvar ij)
	(defvar flag)
	(defvar flag_1)
	(defvar flag_2)
	(defvar check_full)
    (setq loop_path (list (list rp cp)))
	(setq final_counter 0)
	(setq flag 0)
	
	(if (line-check rp cp) (progn (setq i rp) (setq j cp)) 
										(progn
											(cond
												((and (boundary-check row column rp (+ cp 1)) (line-check rp (+ cp 1))) (progn (setq i rp) (setq j (+ cp 1))))
												((and (boundary-check row column rp (- cp 1)) (line-check rp (- cp 1))) (progn (setq i rp) (setq j (- cp 1))))
												((and (boundary-check row column (+ rp 1) cp) (line-check (+ rp 1) cp)) (progn (setq i (+ rp 1)) (setq j cp)))
												((and (boundary-check row column (- rp 1) cp) (line-check (- rp 1) cp)) (progn (setq i (- rp 1)) (setq j cp))))))
	
	(loop 
		do
			(setq condition (if (equal (aref board i j) "-") (horizontal-condition r c i j) (vertical-condition r c i j)))
			(setq ij1 (caddr condition))
			(setq ij2 (cadddr condition))
			(setq check (member-check loop_path ij1))
			(if (= 1 check) (setq ij ij1) (setq ij ij2))
			(if (= 0 check)
				(if (= 0 (member-check loop_path ij2))
					(setq flag 1)))
			(setq loop_path (append loop_path (list ij)))
			(setq i (car ij))
			(setq j (cadr ij))
			until (= flag 1))
				
	(loop for i from 0 to (- r 1)
		do
			(loop for j from 0 to (- column 1)
				do
					(if (or (equal (aref board i j) "-") (equal (aref board i j) "|"))
						(if (= 1 (setq check_full (member-check loop_path (list i j)))) (setq final_counter 1)))))
					
(if (= 0 final_counter) t nil))

(defun member-check (loop_path ij)
(defvar check)
(defvar tuple)
(setq check 1)
	(loop
		do	
			(setq tuple (car loop_path))
			(if (equal tuple ij) (progn (setq check 0) (setq loop_path nil)) (setq loop_path (cdr loop_path)))
		until (equal nil loop_path))
(return-from member-check check))

(defun line-check (rp cp)
	(if (or (equal (aref board rp cp) "|") (equal (aref board rp cp) "-")) t nil))

(defun primary-constraints ()
(defvar val)
(defvar cross)
(setq cross #\x)

;; SQUARES WITH 0
	(loop for i from 1 to (- row 1)
		do
			(loop for j from 1 to (- column 1)
				do
					(setq val (aref board i j))
					(if (and (numberp (digit-char-p val)) (= 0 (digit-char-p val))) 
						(progn (setf (aref board i (- j 1)) cross) (setf (aref board i (+ j 1)) cross) (setf (aref board (- i 1) j) cross) (setf (aref board (+ 1 i) j) cross)))))
	
;; CORNERS	
	(cond
	((equal (aref board 1 1) #\1) (progn (setf (aref board 1 0) cross) (setf (aref board 0 1) cross)))
	((equal (aref board 1 1) #\3) (progn (setf (aref board 1 0) "|") (setf (aref board 0 1) "-")))
	((equal (aref board 1 1) #\2) (progn (setf (aref board 3 0) "|") (setf (aref board 0 3) "-"))))	

	(cond
	((equal (aref board 1 (- column 2)) #\1) (progn (setf (aref board 1 (- column 1)) cross) (setf (aref board 0 (- column 2)) cross)))
	((equal (aref board 1 (- column 2)) #\3) (progn (setf (aref board 1 (- column 1)) "|") (setf (aref board 0 (- column 2)) "-")))
	((equal (aref board 1 (- column 2)) #\2) (progn (setf (aref board 3 (- column 1)) "|") (setf (aref board 0 (- column 4)) "-"))))		
	
	(cond
	((equal (aref board (- row 2) 1) #\1) (progn (setf (aref board (- row 2) 0) cross) (setf (aref board (- row 1) 1) cross)))
	((equal (aref board (- row 2) 1) #\3) (progn (setf (aref board (- row 2) 0) "|") (setf (aref board (- row 1) 1) "-")))
	((equal (aref board (- row 2) 1) #\2) (progn (setf (aref board (- row 4) 0) "|") (setf (aref board (- row 1) 3) "-"))))
	
	(cond
	((equal (aref board (- row 2) (- column 2)) #\1) (progn (setf (aref board (- row 2) (- column 1)) cross) (setf (aref board (- row 1) (- column 2)) cross)))
	((equal (aref board (- row 2) (- column 2)) #\3) (progn (setf (aref board (- row 2) (- column 1)) "|") (setf (aref board (- row 1) (- column 2)) "-")))
	((equal (aref board (- row 2) (- column 2)) #\2) (progn (setf (aref board (- row 4) (- column 1)) "|") (setf (aref board (- row 1) (- column 4)) "-"))))
	
   	(loop for i from 1 to (- row 1)
		do
			(loop for j from 1 to (- column 1)
				do
				;; ADJACENT 3 AND 0
					(if (and (equal (aref board i j) #\3) (boundary-check row column i (- j 2))(equal (aref board i (- j 2)) #\0))
						(progn
							(if (boundary-check row column (- i 2) (- j 1)) (setf (aref board (- i 2) (- j 1)) "|"))
							(if (boundary-check row column (+ i 2) (- j 1)) (setf (aref board (+ i 2) (- j 1)) "|"))
							(if (boundary-check row column i (+ j 1)) (setf (aref board i (+ j 1)) "|"))
							(if (boundary-check row column (+ i 1) j) (setf (aref board (+ i 1) j) "-"))
							(if (boundary-check row column (- i 1) j) (setf (aref board (- i 1) j) "-"))))
							
					(if (and (equal (aref board i j) #\3) (boundary-check row column i (+ j 2))(equal (aref board i (+ j 2)) #\0))
						(progn
							(if (boundary-check row column (- i 2) (+ j 1)) (setf (aref board (- i 2) (+ j 1)) "|"))
							(if (boundary-check row column (+ i 2) (+ j 1)) (setf (aref board (+ i 2) (+ j 1)) "|"))
							(if (boundary-check row column i (- j 1)) (setf (aref board i (- j 1)) "|"))
							(if (boundary-check row column (+ i 1) j) (setf (aref board (+ i 1) j) "-"))
							(if (boundary-check row column (- i 1) j) (setf (aref board (- i 1) j) "-"))))
							
					(if (and (equal (aref board i j) #\3) (boundary-check row column (- i 2) j)(equal (aref board (- i 2) j) #\0))
						(progn
							(if (boundary-check row column (- i 1) (- j 2)) (setf (aref board (- i 1) (- j 2)) "-"))
							(if (boundary-check row column (- i 1) (+ j 2)) (setf (aref board (- i 1) (+ j 2)) "-"))
							(if (boundary-check row column (+ i 1) j) (setf (aref board (+ i 1) j) "-"))
							(if (boundary-check row column i (- j 1)) (setf (aref board i (- j 1)) "|"))
							(if (boundary-check row column i (+ j 1)) (setf (aref board i (+ j 1)) "|"))))
							
					(if (and (equal (aref board i j) #\3) (boundary-check row column (+ i 2) j)(equal (aref board (+ i 2) j) #\0))
						(progn
							(if (boundary-check row column (+ i 1) (- j 2)) (setf (aref board (+ i 1) (- j 2)) "-"))
							(if (boundary-check row column (+ i 1) (+ j 2)) (setf (aref board (+ i 1) (+ j 2)) "-"))
							(if (boundary-check row column (- i 1) j) (setf (aref board (- i 1) j) "-"))
							(if (boundary-check row column i (- j 1)) (setf (aref board i (- j 1)) "|"))
							(if (boundary-check row column i (+ j 1)) (setf (aref board i (+ j 1)) "|"))))
							
				;; ADJACENT 3 AND 3
                    (if (and (equal (aref board i j) #\3) (boundary-check row column i (+ j 2))(equal (aref board i (+ j 2)) #\3))
						(progn				 	
							(if (boundary-check row column i (- j 1)) (setf (aref board i (- j 1)) "|"))
							(if (boundary-check row column i (+ j 1)) (setf (aref board i (+ j 1)) "|"))
							(if (boundary-check row column i (+ j 3)) (setf (aref board i (+ j 3)) "|"))))
							
                    (if (and (equal (aref board i j) #\3) (boundary-check row column (+ i 2) j)(equal (aref board (+ i 2) j) #\3))
						(progn				 	
							(if (boundary-check row column (- i 1) j) (setf (aref board (- i 1) j) "-"))
							(if (boundary-check row column (+ i 1) j) (setf (aref board (+ i 1) j) "-"))
							(if (boundary-check row column (+ i 3) j) (setf (aref board (+ i 3) j) "-"))))
					
				;;DIAGONAL 3 AND 0
					(if (and (equal (aref board i j) #\3) (boundary-check row column (+ i 2) (- j 2))(equal (aref board (+ i 2) (- j 2)) #\0))
						(progn
							(if (boundary-check row column (+ i 1) j) (setf (aref board (+ i 1) j) "-"))
							(if (boundary-check row column i (- j 1)) (setf (aref board i (- j 1)) "|"))))
					
					(if (and (equal (aref board i j) #\3) (boundary-check row column (+ i 2) (+ j 2))(equal (aref board (+ i 2) (+ j 2)) #\0))
						(progn
							(if (boundary-check row column (+ i 1) j) (setf (aref board (+ i 1) j) "-"))
							(if (boundary-check row column i (+ j 1)) (setf (aref board i (+ j 1)) "|"))))			

					(if (and (equal (aref board i j) #\3) (boundary-check row column (- i 2) (- j 2))(equal (aref board (- i 2) (- j 2)) #\0))
						(progn
							(if (boundary-check row column (- i 1) j) (setf (aref board (- i 1) j) "-"))
							(if (boundary-check row column i (- j 1)) (setf (aref board i (- j 1)) "|"))))	

					(if (and (equal (aref board i j) #\3) (boundary-check row column (- i 2) (+ j 2))(equal (aref board (- i 2) (+ j 2)) #\0))
						(progn
							(if (boundary-check row column (- i 1) j) (setf (aref board (- i 1) j) "-"))
							(if (boundary-check row column i (+ j 1)) (setf (aref board i (+ j 1)) "|"))))

				;; DIAGONAL 3 AND 3
					(if (and (equal (aref board i j) #\3) (boundary-check row column (+ i 2) (+ j 2))(equal (aref board (+ i 2) (+ j 2)) #\3))
						(progn
							(if (boundary-check row column (- i 1) j) (setf (aref board (- i 1) j) "-"))
							(if (boundary-check row column (+ i 3) (+ j 2)) (setf (aref board (+ i 3) (+ j 2)) "-"))
							(if (boundary-check row column i (- j 1)) (setf (aref board i (- j 1)) "|"))
							(if (boundary-check row column (+ i 2) (+ j 3)) (setf (aref board (+ i 2) (+ j 3)) "|"))))
							
					(if (and (equal (aref board i j) #\3) (boundary-check row column (+ i 2) (- j 2))(equal (aref board (+ i 2) (- j 2)) #\3))
						(progn
							(if (boundary-check row column (- i 1) j) (setf (aref board (- i 1) j) "-"))
							(if (boundary-check row column (+ i 3) (- j 2)) (setf (aref board (+ i 3) (- j 2)) "-"))
							(if (boundary-check row column i (+ j 1)) (setf (aref board i (+ j 1)) "|"))
							(if (boundary-check row column (+ i 2) (- j 3)) (setf (aref board (+ i 2) (- j 3)) "|"))))
					(incf j))
					(incf i))
					
	(loop
		do
			(setq flag_1 (numbers-and-lines-constraints))
			(setq flag_2 (vertex-constraints))
		until (and (equal flag_1 0) (equal flag_2 0)))
		
(format t "~%~%After Initial Pruning:~%~%")		
(print-moves row column)
(start-DFS))

(defun numbers-and-lines-constraints ()
(defvar flag1)
(defvar number)
(defvar x_count)
(defvar line_count)
(setq flag1 0)
	(loop for i from 1 to (- row 1)
		do
			(loop for j from 1 to (- column 1)
				do
					(setq number (aref board i j))
					(setq x_count (cadr (node-condition row column i j)))
					(setq line_count (car (node-condition row column i j)))
					
					;; PlACE MISSING LINES
					(if (and (not (equal number #\0)) (not (equal number #\Space)) (equal (- 4 x_count) (digit-char-p number)))
						(progn
							(if (equal (aref board i (- j 1)) #\Space) (progn (setq flag1 1) (setf (aref board i (- j 1)) "|")))
							(if (equal (aref board i (+ j 1)) #\Space) (progn (setq flag1 1) (setf (aref board i (+ j 1)) "|")))
							(if (equal (aref board (- i 1) j) #\Space) (progn (setq flag1 1) (setf (aref board (- i 1) j) "-")))
							(if (equal (aref board (+ i 1) j) #\Space) (progn (setq flag1 1) (setf (aref board (+ i 1) j) "-")))))

					;; PLACE MISSING CROSSES
					(if (and (not (equal number #\0)) (not (equal number #\Space)) (equal (digit-char-p number) line_count))
						(progn
							(if (equal (aref board i (- j 1)) #\Space) (progn (setq flag1 1) (setf (aref board i (- j 1)) cross)))
							(if (equal (aref board i (+ j 1)) #\Space) (progn (setq flag1 1) (setf (aref board i (+ j 1)) cross)))
							(if (equal (aref board (- i 1) j) #\Space) (progn (setq flag1 1) (setf (aref board (- i 1) j) cross)))
							(if (equal (aref board (+ i 1) j) #\Space) (progn (setq flag1 1) (setf (aref board (+ i 1) j) cross)))))
							
					;; If there are 3 lines then cross the remaining edge
					
					(if (equal line_count 3)
						(progn
							(if (equal (aref board i (- j 1)) #\Space) (progn (setq flag1 1) (setf (aref board i (- j 1)) cross)))
							(if (equal (aref board i (+ j 1)) #\Space) (progn (setq flag1 1) (setf (aref board i (+ j 1)) cross)))
							(if (equal (aref board (- i 1) j) #\Space) (progn (setq flag1 1) (setf (aref board (- i 1) j) cross)))
							(if (equal (aref board (+ i 1) j) #\Space) (progn (setq flag1 1) (setf (aref board (+ i 1) j) cross)))))
							
							(incf j))
							(incf i))
	(return-from numbers-and-lines-constraints flag1))

(defun vertex-constraints ()
(defvar flag2)
(defvar x_count)
(defvar line_count)
(defvar space_count)
(setq flag2 0)	
	(loop for i from 0 to (- row 1)
		do
			(loop for j from 0 to (- column 1)
				do
					(setq x_count (cadr (vertex-condition row column i j)))
					(setq line_count (car (vertex-condition row column i j)))
					(setq space_count (caddr (vertex-condition row column i j)))
					
					;; 2 lines present: Place 2 crosses
					(if (and (equal line_count 2) (not (equal x_count 2)))
						(progn
							(if (and (boundary-check row column i (- j 1)) (equal (aref board i (- j 1)) #\Space)) (progn (setq flag2 1) (setf (aref board i (- j 1)) cross)))
							(if (and (boundary-check row column i (+ j 1)) (equal (aref board i (+ j 1)) #\Space)) (progn (setq flag2 1) (setf (aref board i (+ j 1)) cross)))
							(if (and (boundary-check row column (- i 1) j) (equal (aref board (- i 1) j) #\Space)) (progn (setq flag2 1) (setf (aref board (- i 1) j) cross)))
							(if (and (boundary-check row column (+ i 1) j) (equal (aref board (+ i 1) j) #\Space)) (progn (setq flag2 1) (setf (aref board (+ i 1) j) cross)))))
					
					;; 2 crosses and 1 line present: Place 1 line
					(if (and (equal line_count 1) (equal x_count 2))
						(progn
							(if (and (boundary-check row column i (- j 1)) (equal (aref board i (- j 1)) #\Space)) (progn (setq flag2 1) (setf (aref board i (- j 1)) "-")))
							(if (and (boundary-check row column i (+ j 1)) (equal (aref board i (+ j 1)) #\Space)) (progn (setq flag2 1) (setf (aref board i (+ j 1)) "-")))
							(if (and (boundary-check row column (- i 1) j) (equal (aref board (- i 1) j) #\Space)) (progn (setq flag2 1) (setf (aref board (- i 1) j) "|")))
							(if (and (boundary-check row column (+ i 1) j) (equal (aref board (+ i 1) j) #\Space)) (progn (setq flag2 1) (setf (aref board (+ i 1) j) "|")))))

					;; 3 crosses present: Place 1 cross
					(if (and (equal space_count 1) (equal line_count 0))
						(progn
							(if (and (boundary-check row column i (- j 1)) (equal (aref board i (- j 1)) #\Space)) (progn (setq flag2 1) (setf (aref board i (- j 1)) cross)))
							(if (and (boundary-check row column i (+ j 1)) (equal (aref board i (+ j 1)) #\Space)) (progn (setq flag2 1) (setf (aref board i (+ j 1)) cross)))
							(if (and (boundary-check row column (- i 1) j) (equal (aref board (- i 1) j) #\Space)) (progn (setq flag2 1) (setf (aref board (- i 1) j) cross)))
							(if (and (boundary-check row column (+ i 1) j) (equal (aref board (+ i 1) j) #\Space)) (progn (setq flag2 1) (setf (aref board (+ i 1) j) cross)))))	

					(if (and (equal space_count 1) (equal line_count 1))
						(progn
							(if (and (boundary-check row column i (- j 1)) (equal (aref board i (- j 1)) #\Space)) (progn (setq flag2 1) (setf (aref board i (- j 1)) "-")))
							(if (and (boundary-check row column i (+ j 1)) (equal (aref board i (+ j 1)) #\Space)) (progn (setq flag2 1) (setf (aref board i (+ j 1)) "-")))
							(if (and (boundary-check row column (- i 1) j) (equal (aref board (- i 1) j) #\Space)) (progn (setq flag2 1) (setf (aref board (- i 1) j) "|")))
							(if (and (boundary-check row column (+ i 1) j) (equal (aref board (+ i 1) j) #\Space)) (progn (setq flag2 1) (setf (aref board (+ i 1) j) "|")))))								
					
					(incf j))
					(incf i))
					(return-from vertex-constraints flag2))
					
(defun start-DFS ()
	(defvar start_vertex)
	(defvar count)
	(defvar DFS_List)
	(setq DFS_List nil)
	(setq start_vertex (get-start-vertex))
	(setq count 0)
	(DFS (car start_vertex) (cadr start_vertex))
	(format t "~%~%Eureka!!! SOLVED! Below is the solution:~%~%")
	(print-moves row column)
	;;(format t "~%~%Moves made:~%~%")
	;;(print DFS_List)
)

(defun get-start-vertex()
(defvar val)
	(loop for i from 0 to (- row 1)
		do
			(loop for j from 0 to (- column 1)
				do
					(setq val (car (vertex-condition row column i j)))
					(if (equal val 2) (return-from get-start-vertex (list i j))))))
		

(defun DFS(x y &optional (x-parent -1) (y-parent -1))
    (let ((flag 0)
            (result nil)
            (edges_loop nil))
    (setf count (+ count 1))
    (if(and (= (car (vertex-condition row column x y)) 2) (circuit-check row column x y))
        (progn
        (setf result t)
        (return-from DFS t)))
    (if(> (car (vertex-condition row column x y)) 2)
        (progn
            (BackTrack)
            (return-from DFS nil)))
    (if(equal result t)
        (return-from DFS t))
    (if(and (boundary-check row column (- x 1) (- y 1)) (numberp (aref board (- x 1) (- y 1))) (> (car (node-condition row column (- x 1) (- y 1))) (aref board (- x 1) (- y 1))))
        (progn
            (BackTrack)
            (return-from DFS nil)))
    (if(equal result t)
        (return-from DFS t))
    (if(and (boundary-check row column (+ x 1) (- y 1)) (numberp (aref board (+ x 1) (- y 1))) (> (car (node-condition row column (+ x 1) (- y 1))) (aref board (+ x 1) (- y 1))))
        (progn
            (BackTrack)
            (return-from DFS nil)))
    (if(equal result t)
        (return-from DFS t))
    (if(and (boundary-check row column (- x 1) (+ y 1)) (numberp (aref board (- x 1) (+ y 1))) (> (car (node-condition row column (- x 1) (+ y 1))) (aref board (- x 1) (+ y 1))))
        (progn
            (BackTrack)
            (return-from DFS nil)))
    (if(equal result t)
        (return-from DFS t))
    (if(and (boundary-check row column (+ x 1) (+ y 1)) (numberp (aref board (+ x 1) (+ y 1))) (> (car (node-condition row column (+ x 1) (+ y 1))) (aref board (+ x 1) (+ y 1))))
        (progn
            (BackTrack)
            (return-from DFS nil)))
    (if(equal result t)
        (return-from DFS t))
    (if(and (or (/= (+ x 2) x-parent) (/= y y-parent)) (boundary-check row column (+ x 2) y) (line-check (+ x 1) y))
        (progn
            (setf flag 1)
            (setf DFS_List (append (list nil) DFS_List))
            (setf result (DFS (+ x 2) y x y))))
    (if(equal result t)
        (return-from DFS t))
    (if(and (or (/= (- x 2) x-parent) (/= y y-parent)) (boundary-check row column (- x 2) y) (line-check (- x 1) y))
        (progn
            (setf flag 1)
            (setf DFS_List (append (list nil) DFS_List))
            (setf result (DFS (- x 2) y x y))))
    (if(and (or (/= x x-parent) (/= (+ y 2) y-parent)) (boundary-check row column x (+ y 2)) (line-check x (+ y 1)))
        (progn
            (setf flag 1)
            (setf DFS_List (append (list nil) DFS_List))
            (setf result (DFS x (+ y 2) x y))))
    (if(equal result t)
        (return-from DFS t))
    (if(and (or (/= x x-parent) (/= (- y 2) y-parent)) (boundary-check row column x (- y 2)) (line-check x (- y 1)))
        (progn
            (setf flag 1)
            (setf DFS_List (append (list nil) DFS_List))
            (setf result (DFS x (- y 2) x y))))
    (if(equal result t)
        (return-from DFS t))
 
    (if(= flag 0)
        (progn
		
            (if(and (or (/= (+ x 2) x-parent) (/= y y-parent)) (boundary-check row column (+ x 2) y) (not (equal #\x (aref board (+ x 1) y))))
                (progn
                    (setf (aref board (+ x 1) y) "|")
                    (setf DFS_List (append (list (list (+ x 1) y)) DFS_List))
                    (setf edges_loop (find-loop board edges_loop (+ x 1) y))
                    (if(and(check-loop edges_loop) (not(circuit-check row column (+ x 1) y)))
                        (BackTrack)
                        (setf result (DFS (+ x 2) y x y)))
                    (setf edges_loop nil)))
            (if(equal result t)
                (return-from DFS t))
				
            (if(and (or (/= (- x 2) x-parent) (/= y y-parent)) (boundary-check row column (- x 2) y) (not (equal #\x (aref board (- x 1) y))))
                (progn
                    (setf (aref board (- x 1) y) "|")
                    (setf DFS_List (append (list (list (- x 1) y)) DFS_List))
                    (setf edges_loop (find-loop board edges_loop (- x 1) y))
                    (if
					(and(check-loop edges_loop) (not (circuit-check row column (- x 1) y)))
					    (BackTrack)
                        (setf result (DFS (- x 2) y x y)))
                    (setf edges_loop nil)))
            (if(equal result t)
                (return-from DFS t))
				
            (if(and (or (/= x x-parent) (/= (+ y 2) y-parent)) (boundary-check row column x (+ y 2)) (not (equal #\x (aref board x (+ y 1)))))
                (progn
                    (setf (aref board x (+ y 1)) "-")
                    (setf DFS_List (append (list (list x (+ y 1))) DFS_List))
                    (setf edges_loop (find-loop board edges_loop x (+ y 1)))
                    (if
					(and(check-loop edges_loop) (not (circuit-check row column x (+ y 1))))
					   (BackTrack)
                        (setf result (DFS x (+ y 2) x y)))
                    (setf edges_loop nil)))
            (if(equal result t)
                (return-from DFS t))
				
            (if(and (or (/= x x-parent) (/= (- y 2) y-parent)) (boundary-check row column x (- y 2)) (not (equal #\x (aref board x (- y 1)))))
                (progn
                    (setf (aref board x (- y 1)) "-")
                    (setf DFS_List (append (list (list x (- y 1))) DFS_List))
                  (setf edges_loop (find-loop board edges_loop x (- y 1)))
                    (if
					(and(check-loop edges_loop) (not (circuit-check row column x (- y 1))))
                        (BackTrack)
                        (setf result (DFS x (- y 2) x y)))
                    (setf edges_loop nil)))))
            (if(equal result t)
                (return-from DFS t))
    (BackTrack)
    (return-from DFS result)))

(defun BackTrack()
    (if(not (equal DFS_List nil))
        (progn
        (if(not(equal (car DFS_List) nil))
        (setf (aref board (car (car DFS_List)) (cadr (car DFS_List))) #\Space))
        (setf DFS_List (cdr DFS_List)))))

(defun check-loop(edges_loop)
(let ((len (list-length edges_loop))
        (x (car (car edges_loop)))
        (y (cadr (car edges_loop)))
        (x-max (array-dimension board 0))
        (y-max (array-dimension board 1))
        (edges_loop_rev (reverse edges_loop))
        (x_rev nil)
        (y_rev nil))
        (setf x_rev (car (car edges_loop_rev)))
        (setf y_rev (cadr (car edges_loop_rev)))
        (if(< len 3)
        (return-from check-loop nil))
        (if (and (not (< (- x 1) 0)) (not (< (- y 1) 0)) (line-check (- x 1) (- y 1)))
            (if(and(= (- x 1) x_rev) (= (- y 1) x_rev))
                (return-from check-loop t)))
        (if (and (not (< (- x 1) 0)) (< (+ y 1) y-max) (line-check (- x 1) (+ y 1)))
            (if(and(= (- x 1) x_rev) (= (+ y 1) y_rev))
                (return-from check-loop t)))
        (if (and (< (+ x 1) x-max) (not (< (- y 1) 0)) (line-check (+ x 1) (- y 1)))
                (if(and(= (+ x 1) x_rev) (= (- y 1) y_rev))
                (return-from check-loop t)))
        (if (and (< (+ x 1) x-max) (< (+ y 1) y-max) (line-check (+ x 1) (+ y 1)))
                (if(and(= (+ x 1) x_rev) (= (+ y 1) y_rev))
                (return-from check-loop t)))
        (cond ((equal (aref board x y) "|")
            (if(and (not (< (- x 2) 0)) (line-check (- x 2) y))
                (if(and(= (- x 2) x_rev) (= y y_rev))
                    (return-from check-loop t)))
            (if(and (<(+ x 2) x-max) (line-check (+ x 2) y))
                (if(and(= (+ x 2) x_rev) (= y y_rev))
                    (return-from check-loop t))))
        ((equal (aref board x y) "-")
            (if(and(not (< (- y 2) 0)) (line-check x (- y 2)))
                (if(and(= x x_rev) (= (- y 2) y_rev))
                    (return-from check-loop t)))
            (if(and (< (+ y 2) y-max) (line-check  x (+ y 2)))
                (if(and(= x x_rev) (= (+ y 2) y_rev))
                    (return-from check-loop t)))))
        (return-from check-loop nil)))        

(defun find-loop(board list_loop x y &optional (x-parent -1) (y-parent -1))
    (let ((x-max (array-dimension board 0))
            (y-max (array-dimension board 1)))
        (cond ((equal (member-check list_loop (list x y)) 0)
                    (return-from find-loop list_loop))
            (T
                (setq list_loop (append list_loop (list (list x y))))
                (if (and (not (< (- x 1) 0)) (not (< (- y 1) 0)))
                    (if (and (line-check (- x 1) (- y 1)) (or (/= (- x 1) x-parent) (/= (- y 1) y-parent))) (setq list_loop (find-loop board list_loop (- x 1) (- y 1) x y))))
                (if (and (not (< (- x 1) 0)) (< (+ y 1) y-max))
                    (if (and (line-check (- x 1) (+ y 1)) (or (/= (- x 1) x-parent) (/= (+ y 1) y-parent))) (setq list_loop (find-loop board list_loop (- x 1) (+ y 1) x y))))
                (if (and (< (+ x 1) x-max) (not (< (- y 1) 0)))
                    (if (and (line-check (+ x 1) (- y 1)) (or (/= (+ x 1) x-parent) (/= (- y 1) y-parent))) (setq list_loop (find-loop board list_loop (+ x 1) (- y 1) x y))))
                (if (and (< (+ x 1) x-max) (< (+ y 1) y-max))
                    (if (and (line-check (+ x 1) (+ y 1)) (or (/= (+ x 1) x-parent) (/= (+ y 1) y-parent))) (setq list_loop (find-loop board list_loop (+ x 1) (+ y 1) x y))))
                (cond ((equal (aref board x y) "|")
                    (if(not (< (- x 2) 0))
                        (if (and (line-check (- x 2) y) (/= (- x 2) x-parent)) (setq list_loop (find-loop board list_loop (- x 2) y x y))))
                    (if(< (+ x 2) x-max)
                        (if (and (line-check (+ 2 x) y) (/= (+ 2 x) x-parent)) (setq list_loop (find-loop board list_loop (+ 2 x) y x y)))))
                ((equal (aref board x y) "-")
                    (if(not (< (- y 2) 0))
                        (if (and (line-check x (- y 2)) (/= (- y 2) y-parent)) (setq list_loop (find-loop board list_loop x (- y 2) x y))))
                    (if(< (+ y 2) y-max)
                        (if (and (line-check x (+ y 2)) (/= (+ y 2) y-parent)) (setq list_loop (find-loop board list_loop x (+ 2 y) x y))))))
                (return-from find-loop list_loop)))))
			
(slither)

(format t "~%~%EXITING... Thank You for Playing!!!~%")
