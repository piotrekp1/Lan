{-# OPTIONS_GHC -w #-}
module Parser (getTree) where
import Data.Char
import ParseDatatypes
import Datatypes
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

action_0 (15) = happyShift action_12
action_0 (16) = happyShift action_13
action_0 (18) = happyShift action_14
action_0 (21) = happyShift action_15
action_0 (23) = happyShift action_16
action_0 (25) = happyShift action_17
action_0 (31) = happyShift action_18
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_5
action_0 (6) = happyGoto action_6
action_0 (7) = happyGoto action_7
action_0 (11) = happyGoto action_8
action_0 (12) = happyGoto action_9
action_0 (13) = happyGoto action_10
action_0 (14) = happyGoto action_11
action_0 _ = happyReduce_4

action_1 (21) = happyShift action_3
action_1 (7) = happyGoto action_2
action_1 _ = happyFail

action_2 (15) = happyShift action_12
action_2 (16) = happyShift action_13
action_2 (18) = happyShift action_14
action_2 (21) = happyShift action_20
action_2 (23) = happyShift action_16
action_2 (25) = happyShift action_17
action_2 (31) = happyShift action_18
action_2 (38) = happyShift action_38
action_2 (5) = happyGoto action_37
action_2 (6) = happyGoto action_6
action_2 (11) = happyGoto action_8
action_2 (12) = happyGoto action_9
action_2 (13) = happyGoto action_10
action_2 (14) = happyGoto action_11
action_2 _ = happyFail

action_3 (25) = happyShift action_40
action_3 _ = happyFail

action_4 (44) = happyAccept
action_4 _ = happyFail

action_5 (38) = happyShift action_39
action_5 _ = happyReduce_3

action_6 _ = happyReduce_6

action_7 (15) = happyShift action_12
action_7 (16) = happyShift action_13
action_7 (18) = happyShift action_14
action_7 (21) = happyShift action_20
action_7 (23) = happyShift action_16
action_7 (25) = happyShift action_17
action_7 (31) = happyShift action_18
action_7 (38) = happyShift action_38
action_7 (44) = happyReduce_4
action_7 (5) = happyGoto action_37
action_7 (6) = happyGoto action_6
action_7 (11) = happyGoto action_8
action_7 (12) = happyGoto action_9
action_7 (13) = happyGoto action_10
action_7 (14) = happyGoto action_11
action_7 _ = happyReduce_4

action_8 _ = happyReduce_10

action_9 (27) = happyShift action_35
action_9 (28) = happyShift action_36
action_9 _ = happyReduce_26

action_10 (29) = happyShift action_33
action_10 (30) = happyShift action_34
action_10 _ = happyReduce_29

action_11 _ = happyReduce_32

action_12 (21) = happyShift action_20
action_12 (23) = happyShift action_16
action_12 (25) = happyShift action_21
action_12 (31) = happyShift action_28
action_12 (42) = happyShift action_29
action_12 (43) = happyShift action_30
action_12 (8) = happyGoto action_32
action_12 (9) = happyGoto action_25
action_12 (10) = happyGoto action_26
action_12 (11) = happyGoto action_27
action_12 (12) = happyGoto action_9
action_12 (13) = happyGoto action_10
action_12 (14) = happyGoto action_11
action_12 _ = happyFail

action_13 (15) = happyShift action_12
action_13 (16) = happyShift action_13
action_13 (18) = happyShift action_14
action_13 (21) = happyShift action_20
action_13 (23) = happyShift action_16
action_13 (25) = happyShift action_17
action_13 (31) = happyShift action_18
action_13 (5) = happyGoto action_31
action_13 (6) = happyGoto action_6
action_13 (11) = happyGoto action_8
action_13 (12) = happyGoto action_9
action_13 (13) = happyGoto action_10
action_13 (14) = happyGoto action_11
action_13 _ = happyReduce_4

action_14 (21) = happyShift action_20
action_14 (23) = happyShift action_16
action_14 (25) = happyShift action_21
action_14 (31) = happyShift action_28
action_14 (42) = happyShift action_29
action_14 (43) = happyShift action_30
action_14 (8) = happyGoto action_24
action_14 (9) = happyGoto action_25
action_14 (10) = happyGoto action_26
action_14 (11) = happyGoto action_27
action_14 (12) = happyGoto action_9
action_14 (13) = happyGoto action_10
action_14 (14) = happyGoto action_11
action_14 _ = happyFail

action_15 (25) = happyShift action_23
action_15 _ = happyFail

action_16 _ = happyReduce_33

action_17 (26) = happyShift action_22
action_17 _ = happyReduce_34

action_18 (21) = happyShift action_20
action_18 (23) = happyShift action_16
action_18 (25) = happyShift action_21
action_18 (31) = happyShift action_18
action_18 (11) = happyGoto action_19
action_18 (12) = happyGoto action_9
action_18 (13) = happyGoto action_10
action_18 (14) = happyGoto action_11
action_18 _ = happyFail

action_19 (32) = happyShift action_61
action_19 _ = happyFail

action_20 (25) = happyShift action_60
action_20 _ = happyFail

action_21 _ = happyReduce_34

action_22 (15) = happyShift action_12
action_22 (16) = happyShift action_13
action_22 (18) = happyShift action_14
action_22 (21) = happyShift action_20
action_22 (23) = happyShift action_16
action_22 (25) = happyShift action_17
action_22 (31) = happyShift action_18
action_22 (6) = happyGoto action_59
action_22 (11) = happyGoto action_8
action_22 (12) = happyGoto action_9
action_22 (13) = happyGoto action_10
action_22 (14) = happyGoto action_11
action_22 _ = happyFail

action_23 (26) = happyShift action_58
action_23 (41) = happyShift action_41
action_23 _ = happyFail

action_24 (19) = happyShift action_57
action_24 (34) = happyShift action_48
action_24 _ = happyFail

action_25 (33) = happyShift action_56
action_25 _ = happyReduce_16

action_26 _ = happyReduce_21

action_27 (35) = happyShift action_53
action_27 (36) = happyShift action_54
action_27 (37) = happyShift action_55
action_27 _ = happyFail

action_28 (21) = happyShift action_20
action_28 (23) = happyShift action_16
action_28 (25) = happyShift action_21
action_28 (31) = happyShift action_28
action_28 (42) = happyShift action_29
action_28 (43) = happyShift action_30
action_28 (8) = happyGoto action_51
action_28 (9) = happyGoto action_25
action_28 (10) = happyGoto action_26
action_28 (11) = happyGoto action_52
action_28 (12) = happyGoto action_9
action_28 (13) = happyGoto action_10
action_28 (14) = happyGoto action_11
action_28 _ = happyFail

action_29 _ = happyReduce_18

action_30 _ = happyReduce_19

action_31 (17) = happyShift action_50
action_31 (38) = happyShift action_39
action_31 _ = happyFail

action_32 (34) = happyShift action_48
action_32 (40) = happyShift action_49
action_32 _ = happyFail

action_33 (23) = happyShift action_16
action_33 (25) = happyShift action_21
action_33 (31) = happyShift action_18
action_33 (14) = happyGoto action_47
action_33 _ = happyFail

action_34 (23) = happyShift action_16
action_34 (25) = happyShift action_21
action_34 (31) = happyShift action_18
action_34 (14) = happyGoto action_46
action_34 _ = happyFail

action_35 (23) = happyShift action_16
action_35 (25) = happyShift action_21
action_35 (31) = happyShift action_18
action_35 (12) = happyGoto action_45
action_35 (13) = happyGoto action_10
action_35 (14) = happyGoto action_11
action_35 _ = happyFail

action_36 (23) = happyShift action_16
action_36 (25) = happyShift action_21
action_36 (31) = happyShift action_18
action_36 (12) = happyGoto action_44
action_36 (13) = happyGoto action_10
action_36 (14) = happyGoto action_11
action_36 _ = happyFail

action_37 (38) = happyShift action_39
action_37 _ = happyReduce_1

action_38 (21) = happyShift action_3
action_38 (7) = happyGoto action_43
action_38 _ = happyReduce_14

action_39 (15) = happyShift action_12
action_39 (16) = happyShift action_13
action_39 (18) = happyShift action_14
action_39 (21) = happyShift action_20
action_39 (23) = happyShift action_16
action_39 (25) = happyShift action_17
action_39 (31) = happyShift action_18
action_39 (5) = happyGoto action_42
action_39 (6) = happyGoto action_6
action_39 (11) = happyGoto action_8
action_39 (12) = happyGoto action_9
action_39 (13) = happyGoto action_10
action_39 (14) = happyGoto action_11
action_39 _ = happyReduce_4

action_40 (41) = happyShift action_41
action_40 _ = happyFail

action_41 (39) = happyShift action_71
action_41 _ = happyFail

action_42 (38) = happyShift action_39
action_42 _ = happyReduce_5

action_43 (38) = happyShift action_38
action_43 _ = happyReduce_13

action_44 (27) = happyShift action_35
action_44 (28) = happyShift action_36
action_44 _ = happyReduce_28

action_45 (27) = happyShift action_35
action_45 (28) = happyShift action_36
action_45 _ = happyReduce_27

action_46 _ = happyReduce_31

action_47 _ = happyReduce_30

action_48 (21) = happyShift action_20
action_48 (23) = happyShift action_16
action_48 (25) = happyShift action_21
action_48 (31) = happyShift action_28
action_48 (42) = happyShift action_29
action_48 (43) = happyShift action_30
action_48 (8) = happyGoto action_70
action_48 (9) = happyGoto action_25
action_48 (10) = happyGoto action_26
action_48 (11) = happyGoto action_27
action_48 (12) = happyGoto action_9
action_48 (13) = happyGoto action_10
action_48 (14) = happyGoto action_11
action_48 _ = happyFail

action_49 (15) = happyShift action_12
action_49 (16) = happyShift action_13
action_49 (18) = happyShift action_14
action_49 (21) = happyShift action_20
action_49 (23) = happyShift action_16
action_49 (25) = happyShift action_17
action_49 (31) = happyShift action_18
action_49 (6) = happyGoto action_69
action_49 (11) = happyGoto action_8
action_49 (12) = happyGoto action_9
action_49 (13) = happyGoto action_10
action_49 (14) = happyGoto action_11
action_49 _ = happyFail

action_50 _ = happyReduce_11

action_51 (32) = happyShift action_68
action_51 (34) = happyShift action_48
action_51 _ = happyFail

action_52 (32) = happyShift action_61
action_52 (35) = happyShift action_53
action_52 (36) = happyShift action_54
action_52 (37) = happyShift action_55
action_52 _ = happyFail

action_53 (21) = happyShift action_20
action_53 (23) = happyShift action_16
action_53 (25) = happyShift action_21
action_53 (31) = happyShift action_18
action_53 (11) = happyGoto action_67
action_53 (12) = happyGoto action_9
action_53 (13) = happyGoto action_10
action_53 (14) = happyGoto action_11
action_53 _ = happyFail

action_54 (21) = happyShift action_20
action_54 (23) = happyShift action_16
action_54 (25) = happyShift action_21
action_54 (31) = happyShift action_18
action_54 (11) = happyGoto action_66
action_54 (12) = happyGoto action_9
action_54 (13) = happyGoto action_10
action_54 (14) = happyGoto action_11
action_54 _ = happyFail

action_55 (21) = happyShift action_20
action_55 (23) = happyShift action_16
action_55 (25) = happyShift action_21
action_55 (31) = happyShift action_18
action_55 (11) = happyGoto action_65
action_55 (12) = happyGoto action_9
action_55 (13) = happyGoto action_10
action_55 (14) = happyGoto action_11
action_55 _ = happyFail

action_56 (21) = happyShift action_20
action_56 (23) = happyShift action_16
action_56 (25) = happyShift action_21
action_56 (31) = happyShift action_28
action_56 (42) = happyShift action_29
action_56 (43) = happyShift action_30
action_56 (9) = happyGoto action_64
action_56 (10) = happyGoto action_26
action_56 (11) = happyGoto action_27
action_56 (12) = happyGoto action_9
action_56 (13) = happyGoto action_10
action_56 (14) = happyGoto action_11
action_56 _ = happyFail

action_57 (15) = happyShift action_12
action_57 (16) = happyShift action_13
action_57 (18) = happyShift action_14
action_57 (21) = happyShift action_20
action_57 (23) = happyShift action_16
action_57 (25) = happyShift action_17
action_57 (31) = happyShift action_18
action_57 (6) = happyGoto action_63
action_57 (11) = happyGoto action_8
action_57 (12) = happyGoto action_9
action_57 (13) = happyGoto action_10
action_57 (14) = happyGoto action_11
action_57 _ = happyFail

action_58 (21) = happyShift action_20
action_58 (23) = happyShift action_16
action_58 (25) = happyShift action_21
action_58 (31) = happyShift action_18
action_58 (11) = happyGoto action_62
action_58 (12) = happyGoto action_9
action_58 (13) = happyGoto action_10
action_58 (14) = happyGoto action_11
action_58 _ = happyFail

action_59 _ = happyReduce_7

action_60 (26) = happyShift action_58
action_60 _ = happyFail

action_61 _ = happyReduce_35

action_62 (22) = happyShift action_73
action_62 _ = happyFail

action_63 (20) = happyShift action_72
action_63 _ = happyFail

action_64 (33) = happyShift action_56
action_64 _ = happyReduce_17

action_65 _ = happyReduce_24

action_66 _ = happyReduce_23

action_67 _ = happyReduce_22

action_68 _ = happyReduce_20

action_69 _ = happyReduce_9

action_70 (34) = happyShift action_48
action_70 _ = happyReduce_15

action_71 _ = happyReduce_12

action_72 (15) = happyShift action_12
action_72 (16) = happyShift action_13
action_72 (18) = happyShift action_14
action_72 (21) = happyShift action_20
action_72 (23) = happyShift action_16
action_72 (25) = happyShift action_17
action_72 (31) = happyShift action_18
action_72 (6) = happyGoto action_75
action_72 (11) = happyGoto action_8
action_72 (12) = happyGoto action_9
action_72 (13) = happyGoto action_10
action_72 (14) = happyGoto action_11
action_72 _ = happyFail

action_73 (21) = happyShift action_20
action_73 (23) = happyShift action_16
action_73 (25) = happyShift action_21
action_73 (31) = happyShift action_18
action_73 (11) = happyGoto action_74
action_73 (12) = happyGoto action_9
action_73 (13) = happyGoto action_10
action_73 (14) = happyGoto action_11
action_73 _ = happyFail

action_74 _ = happyReduce_25

action_75 _ = happyReduce_8

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (PBegin happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (PDecl happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (PSntnc happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  5 happyReduction_4
happyReduction_4  =  HappyAbsSyn5
		 (PSkip
	)

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (PScln happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (PExp0 happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn6
		 (PAsgn happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 6 6 happyReduction_8
happyReduction_8 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (PIfStmt happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 6 happyReduction_9
happyReduction_9 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (PWhile happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn6
		 (PExp happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (SntBrack happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 7 happyReduction_12
happyReduction_12 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (PSingDecl happy_var_2 (Num 0)
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (PDScln happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  7 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (PDScln happy_var_1 PDSkip
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  8 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Or happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  8 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (BExp2 happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (And happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  9 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn9
		 (BVal True
	)

happyReduce_19 = happySpecReduce_1  9 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn9
		 (BVal False
	)

happyReduce_20 = happySpecReduce_3  9 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (BBrack happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  9 happyReduction_21
happyReduction_21 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (PCmp happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  10 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (PCmpExp EQ happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  10 happyReduction_23
happyReduction_23 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (PCmpExp GT happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  10 happyReduction_24
happyReduction_24 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (PCmpExp LT happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happyReduce 6 11 happyReduction_25
happyReduction_25 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_1  11 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (Exp1 happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  12 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (E1Op OpAdd happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  12 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (E1Op OpSub happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  12 happyReduction_29
happyReduction_29 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (Term happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  13 happyReduction_30
happyReduction_30 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (TOp OpMul happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  13 happyReduction_31
happyReduction_31 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (TOp OpDiv happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  13 happyReduction_32
happyReduction_32 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (Factor happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  14 happyReduction_33
happyReduction_33 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn14
		 (Int happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  14 happyReduction_34
happyReduction_34 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn14
		 (Var happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  14 happyReduction_35
happyReduction_35 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (Brack happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 44 44 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenWhile -> cont 15;
	TokenLBracket -> cont 16;
	TokenRBracket -> cont 17;
	TokenIf -> cont 18;
	TokenThen -> cont 19;
	TokenElse -> cont 20;
	TokenLet -> cont 21;
	TokenIn -> cont 22;
	TokenInt happy_dollar_dollar -> cont 23;
	TokenBool happy_dollar_dollar -> cont 24;
	TokenVar happy_dollar_dollar -> cont 25;
	TokenEq -> cont 26;
	TokenPlus -> cont 27;
	TokenMinus -> cont 28;
	TokenTimes -> cont 29;
	TokenDiv -> cont 30;
	TokenOB -> cont 31;
	TokenCB -> cont 32;
	TokenAnd -> cont 33;
	TokenOr -> cont 34;
	TokenCmp -> cont 35;
	TokenGT -> cont 36;
	TokenLT -> cont 37;
	TokenSep -> cont 38;
	TokenIntType -> cont 39;
	TokenTwoDots -> cont 40;
	TokenDecl -> cont 41;
	TokenTrue -> cont 42;
	TokenFalse -> cont 43;
	_ -> happyError' (tk:tks)
	}

happyError_ 44 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

lang tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError list = error ("Parse error" ++ show list)

data Token
      = TokenLet
      | TokenIn
      | TokenInt Int
      | TokenBool Bool
      | TokenVar String
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenOB
      | TokenCB
      | TokenSep
      | TokenIf
      | TokenThen
      | TokenElse
      | TokenRBracket
      | TokenLBracket
      | TokenWhile
      | TokenIntType
      | TokenDecl
      | TokenAnd
      | TokenOr
      | TokenCmp
      | TokenGT
      | TokenLT
      | TokenTrue
      | TokenFalse
      | TokenTwoDots
 deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('=':'=':cs) = TokenCmp : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer (';':cs) = TokenSep : lexer cs
lexer ('\n':cs) = TokenSep : lexer cs
lexer ('{':cs) = TokenLBracket : lexer cs
lexer ('}':cs) = TokenRBracket : lexer cs
lexer ('<':cs) = TokenLT : lexer cs
lexer ('>':cs) = TokenGT : lexer cs
lexer (':':':':cs) = TokenDecl : lexer cs
lexer (':':cs) = TokenTwoDots : lexer cs

numBool :: Int -> Bool
numBool 1 = True
numBool 0 = False

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs
{-


lexBool cs = TokenBool (numBool $ read num) : lexer rest
      where (num,rest) = span isDigit cs
-}

lexVar cs =
   case span isAlpha cs of
      ("while", rest) -> TokenWhile : lexer rest
      ("if", rest) -> TokenIf : lexer rest
      ("then", rest) -> TokenThen : lexer rest
      ("else", rest) -> TokenElse : lexer rest
      ("let",rest) -> TokenLet : lexer rest
      ("in",rest)  -> TokenIn : lexer rest
      ("Int",rest)  -> TokenIntType : lexer rest
      ("True", rest) -> TokenTrue : lexer rest
      ("False", rest) -> TokenFalse : lexer rest
      ("and", rest) -> TokenAnd : lexer rest
      ("or", rest) -> TokenOr : lexer rest
      (var,rest)   -> TokenVar var : lexer rest

--main = getContents >>= print . calc . lexer
getTree = lang . lexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
