# Slitherlink-AI
Steps to execute:

1) In the LISP command prompt, compile file using:

    (compile-file “slither.lisp”)

2) Then load the file using: 

    (load “slither”)
	
Inputs required by the program at various stages:

HUMAN PLAY

1) At the start, the user will be prompted as to whether to start the game or not
2) User will then be asked to input the name of the file containing the board. This repeats till the correct file name is entered.
4) User then will have to input moves in the described format. A move will either place a line or remove a line at the position specified.
5) User has the option to quit the game whenever he/she chooses whereupon they'd be prompted for a new game again. Declining here will terminate the program.
6) On reaching the solution, the user will be intimated and prompted for a new game!

INTELLIGENT PLAY
1) At the start, the user will be prompted as to whether to start the game or not
2) User will then be asked to input the name of the file containing the board. This repeats till the correct file name is entered.
3)User should type "SOLVE" when prompted for intelligence play.
