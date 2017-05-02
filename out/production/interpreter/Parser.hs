{-# OPTIONS_GHC -w #-}
module Parser (getTree) where
import Data.Char
import ParseDatatypes
import Datatypes
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
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

action_0 (13) = happyShift action_8
action_0 (16) = happyShift action_9
action_0 (19) = happyShift action_10
action_0 (21) = happyShift action_11
action_0 (23) = happyShift action_12
action_0 (29) = happyShift action_13
action_0 (4) = happyGoto action_2
action_0 (5) = happyGoto action_3
action_0 (9) = happyGoto action_4
action_0 (10) = happyGoto action_5
action_0 (11) = happyGoto action_6
action_0 (12) = happyGoto action_7
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (36) = happyShift action_33
action_2 (41) = happyAccept
action_2 _ = happyFail

action_3 (13) = happyShift action_8
action_3 (16) = happyShift action_9
action_3 (19) = happyShift action_10
action_3 (21) = happyShift action_11
action_3 (23) = happyShift action_12
action_3 (29) = happyShift action_13
action_3 (36) = happyShift action_32
action_3 (4) = happyGoto action_31
action_3 (5) = happyGoto action_3
action_3 (9) = happyGoto action_4
action_3 (10) = happyGoto action_5
action_3 (11) = happyGoto action_6
action_3 (12) = happyGoto action_7
action_3 _ = happyReduce_1

action_4 _ = happyReduce_7

action_5 (25) = happyShift action_29
action_5 (26) = happyShift action_30
action_5 _ = happyReduce_22

action_6 (27) = happyShift action_27
action_6 (28) = happyShift action_28
action_6 _ = happyReduce_25

action_7 _ = happyReduce_28

action_8 (19) = happyShift action_15
action_8 (21) = happyShift action_11
action_8 (23) = happyShift action_16
action_8 (29) = happyShift action_23
action_8 (39) = happyShift action_24
action_8 (40) = happyShift action_25
action_8 (6) = happyGoto action_26
action_8 (7) = happyGoto action_20
action_8 (8) = happyGoto action_21
action_8 (9) = happyGoto action_22
action_8 (10) = happyGoto action_5
action_8 (11) = happyGoto action_6
action_8 (12) = happyGoto action_7
action_8 _ = happyFail

action_9 (19) = happyShift action_15
action_9 (21) = happyShift action_11
action_9 (23) = happyShift action_16
action_9 (29) = happyShift action_23
action_9 (39) = happyShift action_24
action_9 (40) = happyShift action_25
action_9 (6) = happyGoto action_19
action_9 (7) = happyGoto action_20
action_9 (8) = happyGoto action_21
action_9 (9) = happyGoto action_22
action_9 (10) = happyGoto action_5
action_9 (11) = happyGoto action_6
action_9 (12) = happyGoto action_7
action_9 _ = happyFail

action_10 (23) = happyShift action_18
action_10 _ = happyFail

action_11 _ = happyReduce_29

action_12 (24) = happyShift action_17
action_12 _ = happyReduce_30

action_13 (19) = happyShift action_15
action_13 (21) = happyShift action_11
action_13 (23) = happyShift action_16
action_13 (29) = happyShift action_13
action_13 (9) = happyGoto action_14
action_13 (10) = happyGoto action_5
action_13 (11) = happyGoto action_6
action_13 (12) = happyGoto action_7
action_13 _ = happyFail

action_14 (30) = happyShift action_54
action_14 _ = happyFail

action_15 (23) = happyShift action_53
action_15 _ = happyFail

action_16 _ = happyReduce_30

action_17 (13) = happyShift action_8
action_17 (16) = happyShift action_9
action_17 (19) = happyShift action_10
action_17 (21) = happyShift action_11
action_17 (23) = happyShift action_12
action_17 (29) = happyShift action_13
action_17 (4) = happyGoto action_52
action_17 (5) = happyGoto action_3
action_17 (9) = happyGoto action_4
action_17 (10) = happyGoto action_5
action_17 (11) = happyGoto action_6
action_17 (12) = happyGoto action_7
action_17 _ = happyReduce_1

action_18 (24) = happyShift action_50
action_18 (38) = happyShift action_51
action_18 _ = happyFail

action_19 (17) = happyShift action_49
action_19 (32) = happyShift action_42
action_19 _ = happyFail

action_20 (31) = happyShift action_48
action_20 _ = happyReduce_12

action_21 _ = happyReduce_17

action_22 (33) = happyShift action_45
action_22 (34) = happyShift action_46
action_22 (35) = happyShift action_47
action_22 _ = happyFail

action_23 (19) = happyShift action_15
action_23 (21) = happyShift action_11
action_23 (23) = happyShift action_16
action_23 (29) = happyShift action_23
action_23 (39) = happyShift action_24
action_23 (40) = happyShift action_25
action_23 (6) = happyGoto action_43
action_23 (7) = happyGoto action_20
action_23 (8) = happyGoto action_21
action_23 (9) = happyGoto action_44
action_23 (10) = happyGoto action_5
action_23 (11) = happyGoto action_6
action_23 (12) = happyGoto action_7
action_23 _ = happyFail

action_24 _ = happyReduce_14

action_25 _ = happyReduce_15

action_26 (14) = happyShift action_41
action_26 (32) = happyShift action_42
action_26 _ = happyFail

action_27 (21) = happyShift action_11
action_27 (23) = happyShift action_16
action_27 (29) = happyShift action_13
action_27 (12) = happyGoto action_40
action_27 _ = happyFail

action_28 (21) = happyShift action_11
action_28 (23) = happyShift action_16
action_28 (29) = happyShift action_13
action_28 (12) = happyGoto action_39
action_28 _ = happyFail

action_29 (21) = happyShift action_11
action_29 (23) = happyShift action_16
action_29 (29) = happyShift action_13
action_29 (10) = happyGoto action_38
action_29 (11) = happyGoto action_6
action_29 (12) = happyGoto action_7
action_29 _ = happyFail

action_30 (21) = happyShift action_11
action_30 (23) = happyShift action_16
action_30 (29) = happyShift action_13
action_30 (10) = happyGoto action_37
action_30 (11) = happyGoto action_6
action_30 (12) = happyGoto action_7
action_30 _ = happyFail

action_31 (36) = happyShift action_33
action_31 _ = happyReduce_6

action_32 (19) = happyShift action_36
action_32 (5) = happyGoto action_35
action_32 _ = happyReduce_10

action_33 (13) = happyShift action_8
action_33 (16) = happyShift action_9
action_33 (19) = happyShift action_10
action_33 (21) = happyShift action_11
action_33 (23) = happyShift action_12
action_33 (29) = happyShift action_13
action_33 (4) = happyGoto action_34
action_33 (5) = happyGoto action_3
action_33 (9) = happyGoto action_4
action_33 (10) = happyGoto action_5
action_33 (11) = happyGoto action_6
action_33 (12) = happyGoto action_7
action_33 _ = happyReduce_1

action_34 (36) = happyShift action_33
action_34 _ = happyReduce_3

action_35 (36) = happyShift action_32
action_35 _ = happyReduce_9

action_36 (23) = happyShift action_65
action_36 _ = happyFail

action_37 (25) = happyShift action_29
action_37 (26) = happyShift action_30
action_37 _ = happyReduce_24

action_38 (25) = happyShift action_29
action_38 (26) = happyShift action_30
action_38 _ = happyReduce_23

action_39 _ = happyReduce_27

action_40 _ = happyReduce_26

action_41 (13) = happyShift action_8
action_41 (16) = happyShift action_9
action_41 (19) = happyShift action_10
action_41 (21) = happyShift action_11
action_41 (23) = happyShift action_12
action_41 (29) = happyShift action_13
action_41 (4) = happyGoto action_64
action_41 (5) = happyGoto action_3
action_41 (9) = happyGoto action_4
action_41 (10) = happyGoto action_5
action_41 (11) = happyGoto action_6
action_41 (12) = happyGoto action_7
action_41 _ = happyReduce_1

action_42 (19) = happyShift action_15
action_42 (21) = happyShift action_11
action_42 (23) = happyShift action_16
action_42 (29) = happyShift action_23
action_42 (39) = happyShift action_24
action_42 (40) = happyShift action_25
action_42 (6) = happyGoto action_63
action_42 (7) = happyGoto action_20
action_42 (8) = happyGoto action_21
action_42 (9) = happyGoto action_22
action_42 (10) = happyGoto action_5
action_42 (11) = happyGoto action_6
action_42 (12) = happyGoto action_7
action_42 _ = happyFail

action_43 (30) = happyShift action_62
action_43 (32) = happyShift action_42
action_43 _ = happyFail

action_44 (30) = happyShift action_54
action_44 (33) = happyShift action_45
action_44 (34) = happyShift action_46
action_44 (35) = happyShift action_47
action_44 _ = happyFail

action_45 (19) = happyShift action_15
action_45 (21) = happyShift action_11
action_45 (23) = happyShift action_16
action_45 (29) = happyShift action_13
action_45 (9) = happyGoto action_61
action_45 (10) = happyGoto action_5
action_45 (11) = happyGoto action_6
action_45 (12) = happyGoto action_7
action_45 _ = happyFail

action_46 (19) = happyShift action_15
action_46 (21) = happyShift action_11
action_46 (23) = happyShift action_16
action_46 (29) = happyShift action_13
action_46 (9) = happyGoto action_60
action_46 (10) = happyGoto action_5
action_46 (11) = happyGoto action_6
action_46 (12) = happyGoto action_7
action_46 _ = happyFail

action_47 (19) = happyShift action_15
action_47 (21) = happyShift action_11
action_47 (23) = happyShift action_16
action_47 (29) = happyShift action_13
action_47 (9) = happyGoto action_59
action_47 (10) = happyGoto action_5
action_47 (11) = happyGoto action_6
action_47 (12) = happyGoto action_7
action_47 _ = happyFail

action_48 (19) = happyShift action_15
action_48 (21) = happyShift action_11
action_48 (23) = happyShift action_16
action_48 (29) = happyShift action_23
action_48 (39) = happyShift action_24
action_48 (40) = happyShift action_25
action_48 (7) = happyGoto action_58
action_48 (8) = happyGoto action_21
action_48 (9) = happyGoto action_22
action_48 (10) = happyGoto action_5
action_48 (11) = happyGoto action_6
action_48 (12) = happyGoto action_7
action_48 _ = happyFail

action_49 (14) = happyShift action_57
action_49 _ = happyFail

action_50 (19) = happyShift action_15
action_50 (21) = happyShift action_11
action_50 (23) = happyShift action_16
action_50 (29) = happyShift action_13
action_50 (9) = happyGoto action_56
action_50 (10) = happyGoto action_5
action_50 (11) = happyGoto action_6
action_50 (12) = happyGoto action_7
action_50 _ = happyFail

action_51 (37) = happyShift action_55
action_51 _ = happyFail

action_52 (36) = happyShift action_33
action_52 _ = happyReduce_2

action_53 (24) = happyShift action_50
action_53 _ = happyFail

action_54 _ = happyReduce_31

action_55 _ = happyReduce_8

action_56 (20) = happyShift action_68
action_56 _ = happyFail

action_57 (13) = happyShift action_8
action_57 (16) = happyShift action_9
action_57 (19) = happyShift action_10
action_57 (21) = happyShift action_11
action_57 (23) = happyShift action_12
action_57 (29) = happyShift action_13
action_57 (4) = happyGoto action_67
action_57 (5) = happyGoto action_3
action_57 (9) = happyGoto action_4
action_57 (10) = happyGoto action_5
action_57 (11) = happyGoto action_6
action_57 (12) = happyGoto action_7
action_57 _ = happyReduce_1

action_58 (31) = happyShift action_48
action_58 _ = happyReduce_13

action_59 _ = happyReduce_20

action_60 _ = happyReduce_19

action_61 _ = happyReduce_18

action_62 _ = happyReduce_16

action_63 (32) = happyShift action_42
action_63 _ = happyReduce_11

action_64 (15) = happyShift action_66
action_64 (36) = happyShift action_33
action_64 _ = happyFail

action_65 (38) = happyShift action_51
action_65 _ = happyFail

action_66 _ = happyReduce_5

action_67 (15) = happyShift action_70
action_67 (36) = happyShift action_33
action_67 _ = happyFail

action_68 (19) = happyShift action_15
action_68 (21) = happyShift action_11
action_68 (23) = happyShift action_16
action_68 (29) = happyShift action_13
action_68 (9) = happyGoto action_69
action_68 (10) = happyGoto action_5
action_68 (11) = happyGoto action_6
action_68 (12) = happyGoto action_7
action_68 _ = happyFail

action_69 _ = happyReduce_21

action_70 (18) = happyShift action_71
action_70 _ = happyFail

action_71 (14) = happyShift action_72
action_71 _ = happyFail

action_72 (13) = happyShift action_8
action_72 (16) = happyShift action_9
action_72 (19) = happyShift action_10
action_72 (21) = happyShift action_11
action_72 (23) = happyShift action_12
action_72 (29) = happyShift action_13
action_72 (4) = happyGoto action_73
action_72 (5) = happyGoto action_3
action_72 (9) = happyGoto action_4
action_72 (10) = happyGoto action_5
action_72 (11) = happyGoto action_6
action_72 (12) = happyGoto action_7
action_72 _ = happyReduce_1

action_73 (15) = happyShift action_74
action_73 (36) = happyShift action_33
action_73 _ = happyFail

action_74 _ = happyReduce_4

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 (PSkip
	)

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (PAsgn happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (PScln happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 10 4 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (PIfStmt happy_var_2 happy_var_5 happy_var_9
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 5 4 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (PWhile happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_2  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (PBegin happy_var_1 happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  4 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (PExp happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 5 happyReduction_8
happyReduction_8 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (PDecl happy_var_2 (Num 0)
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (PDScln happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  5 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (PDScln happy_var_1 PDSkip
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Or happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (BExp2 happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (And happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  7 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn7
		 (BVal True
	)

happyReduce_15 = happySpecReduce_1  7 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn7
		 (BVal False
	)

happyReduce_16 = happySpecReduce_3  7 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (BBrack happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  7 happyReduction_17
happyReduction_17 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (PCmp happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  8 happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (PCmpExp EQ happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  8 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (PCmpExp GT happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  8 happyReduction_20
happyReduction_20 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (PCmpExp LT happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 6 9 happyReduction_21
happyReduction_21 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_1  9 happyReduction_22
happyReduction_22 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Exp1 happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  10 happyReduction_23
happyReduction_23 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (E1Op OpAdd happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  10 happyReduction_24
happyReduction_24 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (E1Op OpSub happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  10 happyReduction_25
happyReduction_25 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (Term happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  11 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (TOp OpMul happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  11 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (TOp OpDiv happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  11 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (Factor happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  12 happyReduction_29
happyReduction_29 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn12
		 (Int happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  12 happyReduction_30
happyReduction_30 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn12
		 (Var happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  12 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Brack happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 41 41 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenWhile -> cont 13;
	TokenLBracket -> cont 14;
	TokenRBracket -> cont 15;
	TokenIf -> cont 16;
	TokenThen -> cont 17;
	TokenElse -> cont 18;
	TokenLet -> cont 19;
	TokenIn -> cont 20;
	TokenInt happy_dollar_dollar -> cont 21;
	TokenBool happy_dollar_dollar -> cont 22;
	TokenVar happy_dollar_dollar -> cont 23;
	TokenEq -> cont 24;
	TokenPlus -> cont 25;
	TokenMinus -> cont 26;
	TokenTimes -> cont 27;
	TokenDiv -> cont 28;
	TokenOB -> cont 29;
	TokenCB -> cont 30;
	TokenAnd -> cont 31;
	TokenOr -> cont 32;
	TokenCmp -> cont 33;
	TokenGT -> cont 34;
	TokenLT -> cont 35;
	TokenSep -> cont 36;
	TokenIntType -> cont 37;
	TokenDecl -> cont 38;
	TokenTrue -> cont 39;
	TokenFalse -> cont 40;
	_ -> happyError' (tk:tks)
	}

happyError_ 41 tk tks = happyError' tks
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
