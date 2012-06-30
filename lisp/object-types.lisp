;;new file cause object.lisp was getting a little bit full. Here's where all the different types of objects go to play.
(in-package :4d)

(defun num-to-object-type (num)
  (elt #(:undefined :light :scroll :wand :staff :weapon :fireweapon :missile :treasure :armor :potion :worn :other :trash :trap :container :note :drinkcon :key :food :money :pen :boat :fountain :throw :grenade :bow :sling :crossbow :bolt :arrow :rock :vehicle :vehicle-controls :vehicle-hatch :vehicle-window :portal :gun :ammo :wings :spacesuit :aqualung :climbable :poison-1 :poison-2 :poison-3 :poison-4 :antidote-1 :antidote-2 :antidote-3 :descendable :portal-bush :portal-water :portal-hole :meat :nugget :metal-detector :tree :oak-bark :anvil :hammer :grindstone :oil :ore :axe :gem-cluster :element :shovel :wood :machine :pickaxe :gall-nut :skin :furniture :portal-hurdle :thermal-protection :radio :focus-minor :focus-major :lightsabre-hilt :zone-flag :locker :garotte :vial :bankbook :spacebike :vehicle2)
       num))

(defmacro def-object-type (name &rest value-names)
  (let* ((class-name (concatenate 'string
				  (symbol-name name)
				  "-OBJECT"))
	 (class (intern class-name))
	 (proto-class (intern (concatenate 'string
					   class-name
					   "-PROTO")))
	 (instance-class (intern (concatenate 'string
					      class-name
					      "-INSTANCE"))))
  `(progn
     (defclass ,class (obj) ())
     (defclass ,proto-class (,class object-prototype) ())
     (defclass ,instance-class (,class object-instance) ())

     ,@(mapcar #'(lambda (vn)
		   (destructuring-bind (num (name) &body body) vn
		     `(defmethod ,name ((,class ,class))
			(let ((,name (object-val ,class ,num)))
			  ,name
			  ,@body))))
	       value-names))))

(defun spell-type (n)
      (if (= -1 n)
	:undefined
	(elt #(:none :armor :teleport :bless :blindness :burning-hands :call-lightning :charm :chill-touch :clone :colour-spray :control-weather :create-food :create-water :cure-blind :cure-critic :cure-light :curse :detect-align :detect-invis :detect-magic :detect-poison :dispel-evil :earthquake :enchant-weapon :energy-drain :fireball :harm :heal :invisible :lightning-bolt :locate-object :magic-misisle :poison :protection-from-evil :remove-curse :sanctuary :shocking-grasp :sleep :strength :summon :suffocate :word-of-recall :antidote-1 :sense-life :animate-dead :dispel-good :group-armor :group-heal :group-recall :infravision :waterwalk :gate :minor-identify :remove-alignment :locate-person :poison-2 :poison-3 :poison-4 :antidote-2 :antidote-3 :evil-eye :absolve :chain-lightning :recharge :meteor-shower :stoneskin :steelskin :hold-person :paralyze :holy-word :holy-shout :haste :shield :group-shield :acid-arrow :flame-arrow :cone-of-cold :knock :protection-from-fire :protection-from-cold :earth-elemental :water-elemental :air-elemental :fire-elemental :fire-shield :life-transfer :mana-transfer :protection-from-good :mind-fire :mind-electricity :mind-water :mind-ice :shield-ice :shield-thorn :shield-mana :shield-mirror :shield-holy :shield-static :fortify-mind :fortify-body :sweet-dreams :divine-mind :numb-mind :slow :fight :battle-rage :enchant-armor :magic-bubble :psi-panic :nightmare :vitalize :dispel-sanctuary :forsee :mana-blast :confuse :corrupt-armor :weaken :soulsmash :demonshriek :lifesuck :burningskull :heartsqueeze :facemelt :electric-blast :inferno :water-to-wine :midas-touch :polymorph :darkness :reserve :backstab :bash :hide :kick :pick-lock :flank :rescue :sneak :steal :track :mount :riding :tame :snare :throw :bow :sling :crossbow :dual :circle :blackjack :second-attack :firearm :push :scan :brew :scribe :tinker :poison-weapon :retreat :filet :disarm :forage :trap-aware :parry :mounted-combat :trample :joust :grapple :drunk :handtohand :melee :third-attack :hamstring :short-blade :dodge :phase :charge :grip :face :focus :martial-arts :slip :manipulate :holy-strength :berserk :meditate :sing-wood :hyperactivity :true-stryke :strangle :fortify :manifest :scalp :brace :behead :blade-dance :longarm :cleave :smash :identify :fire-breath :gas-breath :frost-breath :acid-breath :lightning-breath :burn :freeze :acid :resist-fire :resist-cold :resist-electricity :wall-fire :wall-force :dispel-bubble :slit :thrust :bleed :embowel :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :spare :silenced :immfreeze :dg-affect :underwater :space :desert :outcast)
	     n)))

(macrolet ((stub-object-types (&rest types)
	     `(progn
		,@(mapcar #'(lambda (type)
			      `(def-object-type ,type))
			  types))))
  (stub-object-types :undefined :light :scroll :wand :staff :weapon :fireweapon :missile :treasure :armor :potion :worn :other :trash :trap :container :note :drinkcon :key :food :money :pen :boat :fountain :throw :grenade :bow :sling :crossbow :bolt :arrow :rock :vehicle :vehicle-controls :vehicle-hatch :vehicle-window :portal :gun :ammo :wings :spacesuit :aqualung :climbable :poison-1 :poison-2 :poison-3 :poison-4 :antidote-1 :antidote-2 :antidote-3 :descendable :portal-bush :portal-water :portal-hole :meat :nugget :metal-detector :tree :oak-bark :anvil :hammer :grindstone :oil :ore :axe :gem-cluster :element :shovel :wood :machine :pickaxe :gall-nut :skin :furniture :portal-hurdle :thermal-protection :radio :focus-minor :focus-major :lightsabre-hilt :zone-flag :locker :garotte :vial :bankbook :spacebike :vehicle2))


(def-object-type undefined)
(def-object-type light
    (2 (light-timer)))
(def-object-type scroll
    (0 (spell-level))
  (1 (first-spell)
     (spell-type first-spell))
  (2 (second-spell)
     (spell-type second-spell))
  (3 (third-spell)
     (spell-type third-spell)))


(def-object-type wand
    (0 (spell-level)))

(def-object-type staff
    (0 (spell-level)))

(defun num-to-attack-type (n)
  (if n
      (elt '(:hit :sting :whip :slash :bite :bludgeon :crush :pound :claw :maul :thrash :pierce :blast :punch :stab :kick :gore :orb :spark :pulse :beam :spear :bolt :blast :burst :discharge :eruption :torrent :torpedo) n)
      :hit))

(def-object-type weapon
    (1 (damdice-num))
  (2 (damdice-size))
  (3 (attack-type)
     (num-to-attack-type attack-type)))

(def-object-type tree
    (0 (creation-time))
  (1 (tree-age))
  (2 (tree-type)
     (handler-case 
	 (elt #(:pine :oak :willow :dogwood :ironwood :fir :maple :elder :elm)
	      tree-type)
       (error () :undefined)))
  (4 (log-prototype)
     (let ((vnum
	    (cond ((and (/= 0 log-prototype)
			(ignore-errors (object-prototype log-prototype)))
		   log-prototype)
		  ((and (>= (vnum tree-object) 52730)
			(<= (vnum tree-object) 52749))
		   (+ 20 (vnum tree-object)))
		  ((= -1 (vnum tree-object))
		   (+ 52750 (object-val tree-object 2))))))
       (and vnum
	    (object-prototype vnum))))
  (5 (log-amount)))