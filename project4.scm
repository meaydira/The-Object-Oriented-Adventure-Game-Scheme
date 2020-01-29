;_BANNER_
;;;   The Object-Oriented Adventure Game
;_BANNER_
;;;
;;; dbozyilan16@ku.edu.tr     23.12.2019
;;;                                
;;; Before you start:
;;; * Please read the project handout available on the course
;;;   web site first to get a basic idea about the project and the 
;;;   logic behind it, then to find out the details about what 
;;;   your tasks are for the rest of the project.
;;;
;;; * The following code should be studied and loaded for this
;;;   project.  Please do not modify these files, put all your work in
;;;   this file.
;;;
;;; - objsys.scm: support for an elementary object system
;;; - objtypes.scm: a few nice object classes
;;; - setup.scm: a bizarre world constructed using these classes
;;;
;;; * Plan your work with pencil and paper before starting to code.
;;;
;;; While you are working:
;;; * Type all your work and notes in the appropriate sections of this file.
;;; * Please do not delete any of the existing lines.
;;; * Use the procedure names given in the instructions.
;;; * Remember to frequently save your file.
;;; * Use semicolon (;) to comment out text, tests and unused code.
;;; * Remember to document your code (in this file)
;;; * Remember our collaboration policy: you can discuss with your friends but:
;;;
;;;   *** NOTHING WRITTEN GETS EXCHANGED ***
;;;
;;;
;;; When you are done:
;;; * Perform a final save and submit your work following the instructions.
;;; * Please do not make any modifications after midnight on the due date.
;;; * Please send an email to helpline [comp200-group@ku.edu.tr] if you have any questions.
;;; * Make sure your file loads without errors:
;;; ****************************************************************************
;;; ***  Your code should run without any syntactic errors. Projects  causing error will NOT be graded. ***
;;; ****************************************************************************
;;;
;; Do NOT modify or delete the lines below.
(#%require (only racket/base random))
(load "objsys.scm")     	   	  	
(load "objtypes.scm")     	   	  	
(load "setup.scm")     	   	  	
(define nil '())   
(define your-answer-here #f)
;;;;;;;;;

;_BANNER_
;;; PART II. Programming Assignment
;;;
;;; The answers to the computer exercises in Section 5 go in the
;;; appropriate sections below.
;;;



;_BANNER_
;;;;;;;;;;;;; Setting up ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;useful expressions from setup.scm
;(setup 'ben-bitdiddle)    	  		    
 ;(run-clock 5)    	  		    
    	  		    
;(ask screen 'DEITY-MODE #f)    	  		    
 ;(ask me 'look-around)    	  		    
 ;(ask me 'go 'up)    	  		    
 ;(ask me 'go 'north)    	  		    
 ;(ask me 'take (thing-named 'problem-set))

 

;;;;;;;;;;;;; CODES: ;;;;;;;;;;;;;
;;

(setup 'dilara)   

;(ask (ask me 'location) 'name)  ;; to see where my avatar is

;(ask me 'SAY (list "hi, my name is " (ask me 'NAME)))

;(ask me 'SAY '("Hello World"))

;(ask me 'GO 'up)

;(ask me 'LOOK-AROUND)  ;;To see whats in room

;(if (not(null?(ask me 'STUFF-AROUND)) )  ;;if there is a stuff in the room, takes it
;(ask me 'TAKE (thing-named (car (names-of (ask me 'STUFF-AROUND))))))



;;(ask me 'take (thing-named 'diploma) )
;; take an object

;;if there is a stuff I have, toss it 
;;(ask me 'TOSS (ask (car(ask me 'THINGS)) 'NAME))


;;(ask me 'DIE)



;_BANNER_
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;

#|
 (ask (ask me 'location) 'name)
cas-building
> (ask me 'SAY (list "hi, my name is " (ask me 'NAME)))

At cas-building dilara says -- hi, my name is  dilara said-and-heard
> (ask me 'SAY '("Hello World"))

At cas-building dilara says -- Hello World said-and-heard
> (ask me 'GO 'up)

No exit in up direction #f
> (ask me 'LOOK-AROUND)

You are in cas-building 
You are not holding anything. 
There is no stuff in the room. 
There are no other people around you. 
The exits are in directions: south north ok
> (ask me 'GO 'south)

dilara moves from cas-building to great-court 
lambda-man moves from gym to library 
lambda-man moves from library to great-court 
At great-court lambda-man says -- Hi dilara 
lambda-man moves from great-court to cas-building 
comp200-student moves from divan to cici-bufe 
At cici-bufe comp200-student says -- Hi cici 
comp200-student moves from cici-bufe to suzy-cafe 
At suzy-cafe comp200-student says -- Hi suzy 
At suzy-cafe comp200-student says -- I take coke from suzy-cafe 
prof-yuret moves from migros to cici-bufe 
At cici-bufe prof-yuret says -- Hi cici 
alyssa-p-hacker moves from sos-building to amphitheater 
alyssa-p-hacker moves from amphitheater to sos-building 
ben-bitdiddle moves from bursar-office to registrar-office 
At registrar-office ben-bitdiddle says -- I take diploma from registrar-office 
--- the-clock Tick 0 --- 
You are in great-court 
You are not holding anything. 
There is no stuff in the room. 
There are no other people around you. 
The exits are in directions: up east west south north #t

|#

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;_BANNER_
     

;_BANNER_
;;;;;; Understanding installation;;;;;
;_BANNER_
;;
;;;;;;;;;;;;; ANSWER: ;;;;;;;;;;;;;
;;
     
;;In this problem, Alyssa points out to the main difference between ask and delegate
;;for install, if we use ask method and move the autonomous person, the superclass's location will be changed,  an the person will have 2 different locations.
;;But rather than this, if we use delegate, the program will get the install method from its superclass and call it for subclass

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;_BANNER_


;_BANNER_
;;;;;;;;;;;; Who just died? ;;;;;;;;;;
;_BANNER_
;;
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;


     
(let* ((in-room (ask heaven 'THINGS))  
		  (people (myfilter (lambda (x) (is-a x 'PERSON?)) in-room)))
  (if (not (null? people)) (ask (car people) 'NAME) )
	     )


     
;_BANNER_
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;

;;;when a person dies, he goes to heaven,

;;we can simply get the person by asking heaven location's things, and filter persons from it,and then get it's car to get the first item and ask name

;;
     
;_BANNER_
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;

;;(let* ((in-room (ask heaven 'THINGS))  
;;		  (people (myfilter (lambda (x) (is-a x 'PERSON?)) in-room)))   
;;	     (ask (car people) 'NAME))

;;dilara

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;_BANNER_
     
 
;_BANNER_
;;;;;; Computer exercise: But I'm too young to die!! ;;;;
;_BANNER_
;;
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;

(define (make-person name birthplace) ; symbol, location -> person
  (let ((mobile-thing-part (make-mobile-thing name birthplace))
	(container-part    (make-container))
	(health            3)
	(strength          1)
   (lives             3))
    (lambda (message)
      (case message
	((PERSON?) (lambda (self) #T))
	((STRENGTH) (lambda (self) strength))
	((HEALTH) (lambda (self) health))
        ((SAY)
         (lambda (self list-of-stuff)
           (ask screen 'TELL-ROOM (ask self 'location)
                (append (list "At" (ask (ask self 'LOCATION) 'NAME)
                                 (ask self 'NAME) "says --")
                           list-of-stuff))
                  'SAID-AND-HEARD))
	((HAVE-FIT)
	 (lambda (self)
	   (ask self 'SAY '("Yaaaah! I am upset!"))
	   'I-feel-better-now))

	((PEOPLE-AROUND)	; other people in room...
	 (lambda (self)
	   (let* ((in-room (ask (ask self 'LOCATION) 'THINGS))
		  (people (myfilter (lambda (x) (is-a x 'PERSON?)) in-room)))
	     (delq self people))))

	((STUFF-AROUND)		; stuff (non people) in room...
	 (lambda (self)
	   (let* ((in-room (ask (ask self 'LOCATION) 'THINGS))
		  (stuff (myfilter (lambda (x) (not (is-a x 'PERSON?))) in-room)))
	     stuff)))
	 
	((PEEK-AROUND)		; other people's stuff...
	 (lambda (self)
	   (let ((people (ask self 'PEOPLE-AROUND)))
	     (accumulate append '() (map (lambda (p) (ask p 'THINGS)) people)))))

	((TAKE)
	 (lambda (self thing)
	   (cond ((ask self 'HAVE-THING? thing)  ; already have it
		  (ask self 'SAY (list "I am already carrying"
				       (ask thing 'NAME)))
		  #f)
		 ((or (is-a thing 'PERSON?)
		      (not (is-a thing 'MOBILE-THING?)))
		  (ask self 'SAY (list "I try but cannot take"
				       (ask thing 'NAME)))
		  #F)
		 (else
		  (let ((owner (ask thing 'LOCATION)))
		    (ask self 'SAY (list "I take" (ask thing 'NAME) 
					 "from" (ask owner 'NAME)))
		    (if (is-a owner 'PERSON?)
			(ask owner 'LOSE thing self)
			(ask thing 'CHANGE-LOCATION self))
		    thing)))))

	((LOSE)
	 (lambda (self thing lose-to)
	   (ask self 'SAY (list "I lose" (ask thing 'NAME)))
	   (ask self 'HAVE-FIT)
	   (ask thing 'CHANGE-LOCATION lose-to)))

	((DROP)
	 (lambda (self thing)
	   (ask self 'SAY (list "I drop" (ask thing 'NAME)
				"at" (ask (ask self 'LOCATION) 'NAME)))
	   (ask thing 'CHANGE-LOCATION (ask self 'LOCATION))))

        ((GO-EXIT)
         (lambda (self exit)
           (ask exit 'USE self)))

	((GO)
	 (lambda (self direction) ; person, symbol -> boolean
	   (let ((exit (ask (ask self 'LOCATION) 'EXIT-TOWARDS direction)))
	     (if (is-a exit 'EXIT?)
                 (ask self 'GO-EXIT exit)
		 (begin (ask screen 'TELL-ROOM (ask self 'LOCATION)
			     (list "No exit in" direction "direction"))
			#F)))))
	((SUFFER)
	 (lambda (self hits)
	   (ask self 'SAY (list "Ouch!" hits "hits is more than I want!"))
	   (set! health (- health hits))
	   (if (<= health 0) (ask self 'DIE))
	   health))

	((DEATH-SCREAM)
	 (lambda (self)
	   (ask screen 'TELL-WORLD
		'("An earth-shattering, soul-piercing scream is heard..."))))

	((ENTER-ROOM)
	 (lambda (self)
	   (let ((others (ask self 'PEOPLE-AROUND)))
	     (if (not (null? others))
		 (ask self 'SAY (cons "Hi" (names-of others)))))
	   (ask (ask self 'location) 'make-noise self)
	   #T))
	
	;; Here is the original DIE method
   #|
	 ((DIE)
	  (lambda (self)
	    (ask self 'SAY '("SHREEEEK!  I, uh, suddenly feel very faint..."))
	    (for-each (lambda (item) (ask self 'LOSE item (ask self 'LOCATION)))
	 	     (ask self 'THINGS))
	    (ask self 'DEATH-SCREAM)
	    (ask death-exit 'USE self)
	    'GAME-OVER-FOR-YOU-DUDE))
   |#
	;; Your version goes here:
 
          ((DIE)
  (lambda (self)
    (if (= lives 0)
	    (begin (ask self 'SAY '("SHREEEEK!  I, uh, suddenly feel very faint..."))
	    (for-each (lambda (item) (ask self 'LOSE item (ask self 'LOCATION)))
	 	     (ask self 'THINGS))
	    (ask self 'DEATH-SCREAM)
	    (ask death-exit 'USE self)
	    'GAME-OVER-FOR-YOU-DUDE)
            (begin
              (set! lives (- lives 1))
              (set! health 3)
               (for-each (lambda (item) (ask self 'LOSE item (ask self 'LOCATION)))
	 	     (ask self 'THINGS))
	    (ask self 'DEATH-SCREAM)
	    (ask death-exit 'USE self)
             (ask self 'CHANGE-LOCATION birthplace)
             (display(list "Oh I'm dead, but don't worry I born again at " (ask birthplace 'NAME))  )
             )
              )
    
	   ))

	(else (find-method message mobile-thing-part container-part))))))

     
;_BANNER_
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;


;;;at die procedure, i first split it into to parts,
;;first one in which lives == 0, so person can't reborn (old die procedure)
;;second one, which lives>0 person's location is set to birthplace, lives is decreased by 1 and loses all items.

;;(ask me 'die)
     
     
     
;_BANNER_
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;     
     
;;An earth-shattering, soul-piercing scream is heard... 
;;dilara moves from bookstore to heaven ("I am are reborn at " . bookstore)
     
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;_BANNER_
     
     
     
     
     
     
     
     
     

;_BANNER_
;;; Computer exercise: Perhaps to arm oneself against a sea of .... ;;;;
;_BANNER_
;;
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;
(define (make-weapon name location damage)
  (let (
        (mobile-thing-part (make-mobile-thing name location))
       )
       
    (lambda (message)    	  		    
      (case message    	  		    
	((WEAPON?) (lambda (self) #t))
	((DAMAGE) (lambda (self)
		     damage))
		    
	((HIT)    	  		    
	 (lambda (self attacker victim)    	  		    
	      	  		    
	 (begin 
                (ask screen 'TELL-ROOM (ask self 'LOCATION) (list (ask attacker 'NAME) "is hitting to " (ask victim 'NAME) "with" (ask self 'NAME)))
                (ask victim 'SUFFER (random-number damage))
                )))
	  		    
	(else (get-method message mobile-thing-part))))))

(define (create-weapon name location damage)
  (create make-weapon name location damage))
;_BANNER_
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;

;;weapon can change its location, so it should inherit the mobile thing part class
;;hit method first calls suffer procedure of victim and than prints the information of who hit whom with what
     

;_BANNER_
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;
(define weapon (create-weapon 'chair-of-the-faculty (ask me 'LOCATION) 5))



(define person (create-person 'ben (ask me 'LOCATION)))

(ask weapon 'hit me person)



#|


You are in sos-building 
You are not holding anything. 
You see stuff in the room: chair-of-the-faculty 
You see other people: ben ben-bitdiddle 
The exits are in directions: east south north ok

> (ask me 'take (thing-named 'chair-of-the-faculty))

At sos-building dilara says -- I take chair-of-the-faculty from sos-building 
> (ask (thing-named 'chair-of-the-faculty) 'hit me (thing-named 'ben-bitdiddle))

dilara is hitting to  ben-bitdiddle with chair-of-the-faculty 
At sos-building ben-bitdiddle says -- Ouch! 5 hits is more than I want! 
An earth-shattering, soul-piercing scream is heard... 
ben-bitdiddle moves from sos-building to heaven 

|#
      
      
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;_BANNER_
      
      
      
      
      
      
     
     
      
      
;_BANNER_
;;;;;;;; Computer exercise: Good thing I'm armed and dangerous ;;;;;;;;;
;_BANNER_
;;
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;

(define (decide frequency)
   (let ((probability (random-number frequency)))
     ( if(eq? probability 1)
         #t
         #f))
   
  )
(define (make-violent-person name birthplace activity miserly frequency)
  (let  ((auto-person-part (make-autonomous-player name birthplace activity miserly)))    
    (lambda (message)    	  		    
      (case message
        ((INSTALL) (lambda (self)
		     (ask clock 'ADD-CALLBACK
			  (make-clock-callback 'act-violently self
					       'ACT-VIOLENTLY))
		     (delegate auto-person-part self 'INSTALL)))
        ((ACT-VIOLENTLY) (lambda (self)
                            (let ((isViolent (decide frequency)))
                              (if (eq? isViolent #t)
                                  (ask self 'TRY-ATTACK))
                           )))
       	 ((TRY-ATTACK) (lambda (self)
                            (let ((people (ask self 'PEOPLE-AROUND)))
                              (if (not(null? people))
                                  (let ((victim (ask self 'PICK-VICTIM))
                                        (weapons (ask self 'GET-WEAPONS)))
                                 (if (and (not (null? weapons)  ) (pair? weapons))
                                            (begin (let ((weapon (pick-random weapons)))
                                             (ask weapon 'HIT self victim)))
                                            
                                         ))
                                        
                                       )
                           )))
         ((PICK-VICTIM) (lambda (self)
                            (let ((people (ask self 'PEOPLE-AROUND)))
                              (if (not(null? people))
                                 (let ((victim (pick-random people)))
                                  victim)
                           ))))
        ((GET-WEAPONS) (lambda (self)
                            (let ((things (ask self 'THINGS)))
                              (if (not(null? things))
                                 (let ((weapons (myfilter (lambda (x)  (is-a x 'WEAPON?)) things)))
                                  weapons
                           )))))      
	(else (get-method message auto-person-part))))))

(define (create-violent-person name birthplace activity miserly frequency)
  (create make-violent-person name birthplace activity miserly frequency))


(define (populate-violent-people)
(begin
 (create-violent-person 'ben-bitdiddle (pick-random all-rooms)
			    2 2 4)
  (create-violent-person 'alyssa-p-hacker (pick-random all-rooms)
			    2 2 3)
  (create-violent-person 'prof-yuret (pick-random all-rooms)
			    1 2 2)
  (create-violent-person 'comp200-student (pick-random all-rooms)
			    2 1 1)
  (create-violent-person 'lambda-man (pick-random all-rooms)
			    3 3 5)

  (create-violent-person 'deniz (pick-random all-rooms)
			    2 2 4)
  (create-violent-person 'dilara (pick-random all-rooms)
			    2 2 3)
  (create-violent-person 'prof-guney (pick-random all-rooms)
			    1 2 2)
  (create-violent-person 'comp200ghost (pick-random all-rooms)
			    2 1 1)
  (create-violent-person 'superman (pick-random all-rooms)
			    3 3 5)

  'populated-violent-people)
)

;(setup 'dilara)    
;(populate-weapons all-rooms)
;(populate-violent-people)


;_BANNER_
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;

;;a violent person,at every clock-tick, decides whether or not to act violent according to the frequency probability,
;;if(decide frequency) returns true(isViolent), it will TRY-ATTACK
;;in TRY-ATTACK, first it will look-for people-around, if people = null , automated violent person will not do anything
;;if there are people in the room, the procedure will pick-random to decide the victim (PICK-VICTIM)
;;after that, it looks for a weapon(GET-WEAPONS), if the person has, it will pick-random from weapons list
;;then it will call 'HIT method of weapon.

     

;_BANNER_
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;
#|
At cas-building dilara says -- Hi lambda-man #t
> (ask me 'look-around)

You are in cas-building 
You are not holding anything. 
There is no stuff in the room. 
You see other people: lambda-man 
The exits are in directions: south north ok
> (ask me 'go 'north)

dilara moves from cas-building to sos-building 
At sos-building dilara says -- Hi deniz 
superman moves from sci-building to sos-building 
At sos-building superman says -- Hi dilara deniz 
superman moves from sos-building to amphitheater 
At amphitheater superman says -- Hi comp200-student 
superman moves from amphitheater to sos-building 
At sos-building superman says -- Hi dilara deniz 
comp200ghost moves from bursar-office to registrar-office 
comp200ghost moves from registrar-office to bursar-office 
prof-guney moves from adm-building to great-court 
dilara moves from deans-office to eng-z21 
At eng-z21 dilara says -- Hi ben-bitdiddle 
At eng-z21 dilara says -- I take problem-set from eng-z21 
deniz moves from sos-building to amphitheater 
At amphitheater deniz says -- Hi comp200-student 
At amphitheater deniz says -- I take umbrella from amphitheater 
lambda-man moves from cas-building to sos-building 
At sos-building lambda-man says -- Hi superman dilara 
lambda-man moves from sos-building to amphitheater 
At amphitheater lambda-man says -- Hi deniz comp200-student 
At amphitheater lambda-man says -- I take umbrella from deniz 
At amphitheater deniz says -- I lose umbrella 
At amphitheater deniz says -- Yaaaah! I am upset! 
comp200-student moves from dorms to adm-building 
comp200-student moves from adm-building to great-court 
At great-court comp200-student says -- Hi prof-guney 
At great-court comp200-student says -- I take inflatable-lambda from great-court 
comp200-student is hitting to  prof-guney with inflatable-lambda 
At great-court prof-guney says -- Ouch! 3 hits is more than I want! 
An earth-shattering, soul-piercing scream is heard... 
prof-guney moves from adm-building to heaven ((Oh I'm dead ! :( but don't worry, my left number of lives: ) 2 (i am reborn at) adm-building)
prof-yuret moves from dorms to adm-building 
alyssa-p-hacker moves from gym to library 
ben-bitdiddle moves from cici-bufe to migros 
At migros ben-bitdiddle says -- I take chair-of-the-faculty from migros 
cici moves from cici-bufe to migros 
At migros cici says -- Hi ben-bitdiddle 
At migros cici says -- Prepare to suffer, ben-bitdiddle ! 
At migros ben-bitdiddle says -- Ouch! 2 hits is more than I want! 
lambda-man moves from soccer-field to eng-building 
lambda-man moves from eng-building to soccer-field 
comp200-student moves from amphitheater to sos-building 
At sos-building comp200-student says -- Hi superman dilara 
comp200-student moves from sos-building to cas-building 
prof-yuret moves from student-center to suzy-cafe 
At suzy-cafe prof-yuret says -- Hi suzy 
alyssa-p-hacker moves from computer-club to cici-bufe 
alyssa-p-hacker moves from cici-bufe to computer-club 
At computer-club alyssa-p-hacker says -- I take scheme-manual from computer-club 
ben-bitdiddle moves from eng-z21 to eng-building 
ben-bitdiddle moves from eng-building to parking-lot 
--- the-clock Tick 0 --- 
You are in sos-building 
You are not holding anything. 
You see stuff in the room: stick-of-chalk 
You see other people: superman 
The exits are in directions: east south north #t
> 


|#
      
      
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;_BANNER_
     
     
     
     
     
     
     
     
;_BANNER_
;;; Computer exercise: A good hacker could defuse this situation ;;;;;;;
;_BANNER_
;;
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;
(define (make-bomb name location damage)
  (let (
        (mobile-thing-part (make-mobile-thing name location))
        (aware-thing-part (make-aware-thing))
        (isDestroyed #f)
        (armed #f)
       )
       
    (lambda (message)    	  		    
      (case message    	  		    
	((BOMB?) (lambda (self) #t))
	((DAMAGE) (lambda (self)
		     damage))
        ((ISDESTROYED?) (lambda (self)
		     isDestroyed))
        
        ((DESTROY) (lambda (self)
		     (set! isDestroyed #t)))
		    
	((ARM)    	  		    
	 (lambda (self)    	  		       	  		    
	 (begin
                (set! armed #t)
                (ask self 'emit (list "the bomb is armed"))
                )))
        
        ((DISARM)    	  		    
	 (lambda (self)    	  		       	  		    
	 (begin
           (set! armed #f)
                )))
        
         ((TRIGGER)    	  		    
	 (lambda (self)    	  		       	  		    
	 (begin
           (if (eq? armed #t)  (begin (ask self 'emit (list "the bomb is trigerred")) (ask self 'ACTIVATE)))
          
                )))
        
        ((HEARD-NOISE)    	  		    
	 (lambda (self)    	  		       	  		   
            (ask self 'TRIGGER)
                ))

        ((ACTIVATE)    	  		    
	 (lambda (self)
           (begin
             (ask self 'emit (list "THE BOMB IS ACTIVATED"))
           (for-each (lambda (person) (ask person 'SUFFER damage))
	 	     (ask self 'PEOPLE-AROUND))
             (ask self 'DESTROY)
           ) ))

        ((PEOPLE-AROUND)
	 (lambda (self)
	   (let* ((in-room (ask (ask self 'LOCATION) 'THINGS))
		  (people (myfilter (lambda (x) (is-a x 'PERSON?)) in-room)))
	      people)))
	  		    
	(else (get-method message mobile-thing-part))))))

(define (create-bomb name location damage)
  (create make-bomb name location damage))
     

;_BANNER_
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;

#|

> (ask (thing-named 'super-bomb) 'arm)

At sos-building the bomb is armed 
>  (ask (thing-named 'super-bomb) 'trigger)

At sos-building the bomb is trigerred 
At sos-building THE BOMB IS ACTIVATED 
At sos-building dilara says -- Ouch! 7 hits is more than I want! 
An earth-shattering, soul-piercing scream is heard... 
dilara moves from sos-building to heaven (Oh I'm dead, but don't worry I born again at  sos-building)
> 

|#
(define (populate-bombs)
(begin
 (create-bomb 'mini-bomb (pick-random all-rooms) 3
			 )
    (create-bomb 'midi-bomb (pick-random all-rooms) 4
			 )
     (create-bomb 'super-bomb (pick-random all-rooms) 5
			 )
  'populated-bombs)
)


;(populate-bombs)

      
      
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;_BANNER_
     
     
     
     
     
;_BANNER_
;# DO NOT FORGET TO SUBMIT YOUR WORK BEFORE THE DEADLINE!         #
;_BANNER_
;# GOOD LUCK!                                                     #
;_BANNER_
     
     




