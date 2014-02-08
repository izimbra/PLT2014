{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParCPP where
import AbsCPP
import LexCPP
import ErrM

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn29 (Integer)
	| HappyAbsSyn30 (Double)
	| HappyAbsSyn31 (Id)
	| HappyAbsSyn32 (Program)
	| HappyAbsSyn33 (Def)
	| HappyAbsSyn34 ([Def])
	| HappyAbsSyn35 (Arg)
	| HappyAbsSyn36 ([Arg])
	| HappyAbsSyn37 (Stm)
	| HappyAbsSyn38 ([Stm])
	| HappyAbsSyn39 (Exp)
	| HappyAbsSyn55 ([Exp])
	| HappyAbsSyn56 (Type)
	| HappyAbsSyn57 ([Id])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (32) = happyGoto action_93
action_0 (34) = happyGoto action_94
action_0 _ = happyReduce_31

action_1 (76) = happyShift action_32
action_1 (77) = happyShift action_33
action_1 (81) = happyShift action_34
action_1 (84) = happyShift action_35
action_1 (33) = happyGoto action_91
action_1 (56) = happyGoto action_92
action_1 _ = happyFail

action_2 (34) = happyGoto action_90
action_2 _ = happyReduce_31

action_3 (76) = happyShift action_32
action_3 (77) = happyShift action_33
action_3 (81) = happyShift action_34
action_3 (84) = happyShift action_35
action_3 (35) = happyGoto action_89
action_3 (56) = happyGoto action_88
action_3 _ = happyFail

action_4 (76) = happyShift action_32
action_4 (77) = happyShift action_33
action_4 (81) = happyShift action_34
action_4 (84) = happyShift action_35
action_4 (35) = happyGoto action_86
action_4 (36) = happyGoto action_87
action_4 (56) = happyGoto action_88
action_4 _ = happyReduce_34

action_5 (60) = happyShift action_56
action_5 (64) = happyShift action_57
action_5 (67) = happyShift action_58
action_5 (76) = happyShift action_32
action_5 (77) = happyShift action_33
action_5 (79) = happyShift action_59
action_5 (80) = happyShift action_82
action_5 (81) = happyShift action_34
action_5 (82) = happyShift action_83
action_5 (83) = happyShift action_60
action_5 (84) = happyShift action_35
action_5 (85) = happyShift action_84
action_5 (86) = happyShift action_85
action_5 (89) = happyShift action_27
action_5 (90) = happyShift action_61
action_5 (91) = happyShift action_30
action_5 (29) = happyGoto action_36
action_5 (30) = happyGoto action_37
action_5 (31) = happyGoto action_38
action_5 (37) = happyGoto action_79
action_5 (39) = happyGoto action_39
action_5 (40) = happyGoto action_40
action_5 (41) = happyGoto action_41
action_5 (42) = happyGoto action_42
action_5 (43) = happyGoto action_43
action_5 (44) = happyGoto action_44
action_5 (45) = happyGoto action_45
action_5 (46) = happyGoto action_46
action_5 (47) = happyGoto action_47
action_5 (48) = happyGoto action_48
action_5 (49) = happyGoto action_80
action_5 (50) = happyGoto action_50
action_5 (51) = happyGoto action_51
action_5 (52) = happyGoto action_52
action_5 (53) = happyGoto action_53
action_5 (54) = happyGoto action_54
action_5 (56) = happyGoto action_81
action_5 _ = happyFail

action_6 (38) = happyGoto action_78
action_6 _ = happyReduce_44

action_7 (60) = happyShift action_56
action_7 (79) = happyShift action_59
action_7 (83) = happyShift action_60
action_7 (89) = happyShift action_27
action_7 (90) = happyShift action_61
action_7 (91) = happyShift action_30
action_7 (29) = happyGoto action_36
action_7 (30) = happyGoto action_37
action_7 (31) = happyGoto action_38
action_7 (39) = happyGoto action_77
action_7 _ = happyFail

action_8 (60) = happyShift action_56
action_8 (79) = happyShift action_59
action_8 (83) = happyShift action_60
action_8 (89) = happyShift action_27
action_8 (90) = happyShift action_61
action_8 (91) = happyShift action_30
action_8 (29) = happyGoto action_36
action_8 (30) = happyGoto action_37
action_8 (31) = happyGoto action_38
action_8 (39) = happyGoto action_39
action_8 (40) = happyGoto action_76
action_8 _ = happyFail

action_9 (60) = happyShift action_56
action_9 (64) = happyShift action_57
action_9 (67) = happyShift action_58
action_9 (79) = happyShift action_59
action_9 (83) = happyShift action_60
action_9 (89) = happyShift action_27
action_9 (90) = happyShift action_61
action_9 (91) = happyShift action_30
action_9 (29) = happyGoto action_36
action_9 (30) = happyGoto action_37
action_9 (31) = happyGoto action_38
action_9 (39) = happyGoto action_39
action_9 (40) = happyGoto action_40
action_9 (41) = happyGoto action_75
action_9 _ = happyFail

action_10 (60) = happyShift action_56
action_10 (64) = happyShift action_57
action_10 (67) = happyShift action_58
action_10 (79) = happyShift action_59
action_10 (83) = happyShift action_60
action_10 (89) = happyShift action_27
action_10 (90) = happyShift action_61
action_10 (91) = happyShift action_30
action_10 (29) = happyGoto action_36
action_10 (30) = happyGoto action_37
action_10 (31) = happyGoto action_38
action_10 (39) = happyGoto action_39
action_10 (40) = happyGoto action_40
action_10 (41) = happyGoto action_41
action_10 (42) = happyGoto action_74
action_10 _ = happyFail

action_11 (60) = happyShift action_56
action_11 (64) = happyShift action_57
action_11 (67) = happyShift action_58
action_11 (79) = happyShift action_59
action_11 (83) = happyShift action_60
action_11 (89) = happyShift action_27
action_11 (90) = happyShift action_61
action_11 (91) = happyShift action_30
action_11 (29) = happyGoto action_36
action_11 (30) = happyGoto action_37
action_11 (31) = happyGoto action_38
action_11 (39) = happyGoto action_39
action_11 (40) = happyGoto action_40
action_11 (41) = happyGoto action_41
action_11 (42) = happyGoto action_42
action_11 (43) = happyGoto action_73
action_11 _ = happyFail

action_12 (60) = happyShift action_56
action_12 (64) = happyShift action_57
action_12 (67) = happyShift action_58
action_12 (79) = happyShift action_59
action_12 (83) = happyShift action_60
action_12 (89) = happyShift action_27
action_12 (90) = happyShift action_61
action_12 (91) = happyShift action_30
action_12 (29) = happyGoto action_36
action_12 (30) = happyGoto action_37
action_12 (31) = happyGoto action_38
action_12 (39) = happyGoto action_39
action_12 (40) = happyGoto action_40
action_12 (41) = happyGoto action_41
action_12 (42) = happyGoto action_42
action_12 (43) = happyGoto action_43
action_12 (44) = happyGoto action_72
action_12 (54) = happyGoto action_54
action_12 _ = happyFail

action_13 (60) = happyShift action_56
action_13 (64) = happyShift action_57
action_13 (67) = happyShift action_58
action_13 (79) = happyShift action_59
action_13 (83) = happyShift action_60
action_13 (89) = happyShift action_27
action_13 (90) = happyShift action_61
action_13 (91) = happyShift action_30
action_13 (29) = happyGoto action_36
action_13 (30) = happyGoto action_37
action_13 (31) = happyGoto action_38
action_13 (39) = happyGoto action_39
action_13 (40) = happyGoto action_40
action_13 (41) = happyGoto action_41
action_13 (42) = happyGoto action_42
action_13 (43) = happyGoto action_43
action_13 (44) = happyGoto action_44
action_13 (45) = happyGoto action_71
action_13 (54) = happyGoto action_54
action_13 _ = happyFail

action_14 (60) = happyShift action_56
action_14 (64) = happyShift action_57
action_14 (67) = happyShift action_58
action_14 (79) = happyShift action_59
action_14 (83) = happyShift action_60
action_14 (89) = happyShift action_27
action_14 (90) = happyShift action_61
action_14 (91) = happyShift action_30
action_14 (29) = happyGoto action_36
action_14 (30) = happyGoto action_37
action_14 (31) = happyGoto action_38
action_14 (39) = happyGoto action_39
action_14 (40) = happyGoto action_40
action_14 (41) = happyGoto action_41
action_14 (42) = happyGoto action_42
action_14 (43) = happyGoto action_43
action_14 (44) = happyGoto action_44
action_14 (45) = happyGoto action_45
action_14 (46) = happyGoto action_70
action_14 (51) = happyGoto action_51
action_14 (52) = happyGoto action_52
action_14 (53) = happyGoto action_53
action_14 (54) = happyGoto action_54
action_14 _ = happyFail

action_15 (60) = happyShift action_56
action_15 (64) = happyShift action_57
action_15 (67) = happyShift action_58
action_15 (79) = happyShift action_59
action_15 (83) = happyShift action_60
action_15 (89) = happyShift action_27
action_15 (90) = happyShift action_61
action_15 (91) = happyShift action_30
action_15 (29) = happyGoto action_36
action_15 (30) = happyGoto action_37
action_15 (31) = happyGoto action_38
action_15 (39) = happyGoto action_39
action_15 (40) = happyGoto action_40
action_15 (41) = happyGoto action_41
action_15 (42) = happyGoto action_42
action_15 (43) = happyGoto action_43
action_15 (44) = happyGoto action_44
action_15 (45) = happyGoto action_45
action_15 (46) = happyGoto action_46
action_15 (47) = happyGoto action_69
action_15 (51) = happyGoto action_51
action_15 (52) = happyGoto action_52
action_15 (53) = happyGoto action_53
action_15 (54) = happyGoto action_54
action_15 _ = happyFail

action_16 (60) = happyShift action_56
action_16 (64) = happyShift action_57
action_16 (67) = happyShift action_58
action_16 (79) = happyShift action_59
action_16 (83) = happyShift action_60
action_16 (89) = happyShift action_27
action_16 (90) = happyShift action_61
action_16 (91) = happyShift action_30
action_16 (29) = happyGoto action_36
action_16 (30) = happyGoto action_37
action_16 (31) = happyGoto action_38
action_16 (39) = happyGoto action_39
action_16 (40) = happyGoto action_40
action_16 (41) = happyGoto action_41
action_16 (42) = happyGoto action_42
action_16 (43) = happyGoto action_43
action_16 (44) = happyGoto action_44
action_16 (45) = happyGoto action_45
action_16 (46) = happyGoto action_46
action_16 (47) = happyGoto action_47
action_16 (48) = happyGoto action_68
action_16 (51) = happyGoto action_51
action_16 (52) = happyGoto action_52
action_16 (53) = happyGoto action_53
action_16 (54) = happyGoto action_54
action_16 _ = happyFail

action_17 (60) = happyShift action_56
action_17 (64) = happyShift action_57
action_17 (67) = happyShift action_58
action_17 (79) = happyShift action_59
action_17 (83) = happyShift action_60
action_17 (89) = happyShift action_27
action_17 (90) = happyShift action_61
action_17 (91) = happyShift action_30
action_17 (29) = happyGoto action_36
action_17 (30) = happyGoto action_37
action_17 (31) = happyGoto action_38
action_17 (39) = happyGoto action_39
action_17 (40) = happyGoto action_40
action_17 (41) = happyGoto action_41
action_17 (42) = happyGoto action_42
action_17 (43) = happyGoto action_43
action_17 (44) = happyGoto action_44
action_17 (45) = happyGoto action_45
action_17 (46) = happyGoto action_46
action_17 (47) = happyGoto action_47
action_17 (48) = happyGoto action_48
action_17 (49) = happyGoto action_67
action_17 (50) = happyGoto action_50
action_17 (51) = happyGoto action_51
action_17 (52) = happyGoto action_52
action_17 (53) = happyGoto action_53
action_17 (54) = happyGoto action_54
action_17 _ = happyFail

action_18 (60) = happyShift action_56
action_18 (64) = happyShift action_57
action_18 (67) = happyShift action_58
action_18 (79) = happyShift action_59
action_18 (83) = happyShift action_60
action_18 (89) = happyShift action_27
action_18 (90) = happyShift action_61
action_18 (91) = happyShift action_30
action_18 (29) = happyGoto action_36
action_18 (30) = happyGoto action_37
action_18 (31) = happyGoto action_38
action_18 (39) = happyGoto action_39
action_18 (40) = happyGoto action_40
action_18 (41) = happyGoto action_41
action_18 (42) = happyGoto action_42
action_18 (43) = happyGoto action_43
action_18 (44) = happyGoto action_44
action_18 (45) = happyGoto action_45
action_18 (46) = happyGoto action_46
action_18 (47) = happyGoto action_47
action_18 (48) = happyGoto action_48
action_18 (50) = happyGoto action_66
action_18 (51) = happyGoto action_51
action_18 (52) = happyGoto action_52
action_18 (53) = happyGoto action_53
action_18 (54) = happyGoto action_54
action_18 _ = happyFail

action_19 (60) = happyShift action_56
action_19 (64) = happyShift action_57
action_19 (67) = happyShift action_58
action_19 (79) = happyShift action_59
action_19 (83) = happyShift action_60
action_19 (89) = happyShift action_27
action_19 (90) = happyShift action_61
action_19 (91) = happyShift action_30
action_19 (29) = happyGoto action_36
action_19 (30) = happyGoto action_37
action_19 (31) = happyGoto action_38
action_19 (39) = happyGoto action_39
action_19 (40) = happyGoto action_40
action_19 (41) = happyGoto action_41
action_19 (42) = happyGoto action_42
action_19 (43) = happyGoto action_43
action_19 (44) = happyGoto action_44
action_19 (45) = happyGoto action_45
action_19 (51) = happyGoto action_65
action_19 (52) = happyGoto action_52
action_19 (53) = happyGoto action_53
action_19 (54) = happyGoto action_54
action_19 _ = happyFail

action_20 (60) = happyShift action_56
action_20 (64) = happyShift action_57
action_20 (67) = happyShift action_58
action_20 (79) = happyShift action_59
action_20 (83) = happyShift action_60
action_20 (89) = happyShift action_27
action_20 (90) = happyShift action_61
action_20 (91) = happyShift action_30
action_20 (29) = happyGoto action_36
action_20 (30) = happyGoto action_37
action_20 (31) = happyGoto action_38
action_20 (39) = happyGoto action_39
action_20 (40) = happyGoto action_40
action_20 (41) = happyGoto action_41
action_20 (42) = happyGoto action_42
action_20 (43) = happyGoto action_43
action_20 (44) = happyGoto action_44
action_20 (45) = happyGoto action_45
action_20 (52) = happyGoto action_64
action_20 (53) = happyGoto action_53
action_20 (54) = happyGoto action_54
action_20 _ = happyFail

action_21 (60) = happyShift action_56
action_21 (64) = happyShift action_57
action_21 (67) = happyShift action_58
action_21 (79) = happyShift action_59
action_21 (83) = happyShift action_60
action_21 (89) = happyShift action_27
action_21 (90) = happyShift action_61
action_21 (91) = happyShift action_30
action_21 (29) = happyGoto action_36
action_21 (30) = happyGoto action_37
action_21 (31) = happyGoto action_38
action_21 (39) = happyGoto action_39
action_21 (40) = happyGoto action_40
action_21 (41) = happyGoto action_41
action_21 (42) = happyGoto action_42
action_21 (43) = happyGoto action_43
action_21 (44) = happyGoto action_44
action_21 (45) = happyGoto action_45
action_21 (53) = happyGoto action_63
action_21 (54) = happyGoto action_54
action_21 _ = happyFail

action_22 (60) = happyShift action_56
action_22 (64) = happyShift action_57
action_22 (67) = happyShift action_58
action_22 (79) = happyShift action_59
action_22 (83) = happyShift action_60
action_22 (89) = happyShift action_27
action_22 (90) = happyShift action_61
action_22 (91) = happyShift action_30
action_22 (29) = happyGoto action_36
action_22 (30) = happyGoto action_37
action_22 (31) = happyGoto action_38
action_22 (39) = happyGoto action_39
action_22 (40) = happyGoto action_40
action_22 (41) = happyGoto action_41
action_22 (42) = happyGoto action_42
action_22 (43) = happyGoto action_43
action_22 (54) = happyGoto action_62
action_22 _ = happyFail

action_23 (60) = happyShift action_56
action_23 (64) = happyShift action_57
action_23 (67) = happyShift action_58
action_23 (79) = happyShift action_59
action_23 (83) = happyShift action_60
action_23 (89) = happyShift action_27
action_23 (90) = happyShift action_61
action_23 (91) = happyShift action_30
action_23 (29) = happyGoto action_36
action_23 (30) = happyGoto action_37
action_23 (31) = happyGoto action_38
action_23 (39) = happyGoto action_39
action_23 (40) = happyGoto action_40
action_23 (41) = happyGoto action_41
action_23 (42) = happyGoto action_42
action_23 (43) = happyGoto action_43
action_23 (44) = happyGoto action_44
action_23 (45) = happyGoto action_45
action_23 (46) = happyGoto action_46
action_23 (47) = happyGoto action_47
action_23 (48) = happyGoto action_48
action_23 (49) = happyGoto action_49
action_23 (50) = happyGoto action_50
action_23 (51) = happyGoto action_51
action_23 (52) = happyGoto action_52
action_23 (53) = happyGoto action_53
action_23 (54) = happyGoto action_54
action_23 (55) = happyGoto action_55
action_23 _ = happyReduce_85

action_24 (76) = happyShift action_32
action_24 (77) = happyShift action_33
action_24 (81) = happyShift action_34
action_24 (84) = happyShift action_35
action_24 (56) = happyGoto action_31
action_24 _ = happyFail

action_25 (91) = happyShift action_30
action_25 (31) = happyGoto action_28
action_25 (57) = happyGoto action_29
action_25 _ = happyFail

action_26 (89) = happyShift action_27
action_26 _ = happyFail

action_27 _ = happyReduce_26

action_28 (65) = happyShift action_127
action_28 _ = happyReduce_92

action_29 (93) = happyAccept
action_29 _ = happyFail

action_30 _ = happyReduce_28

action_31 (93) = happyAccept
action_31 _ = happyFail

action_32 _ = happyReduce_88

action_33 _ = happyReduce_90

action_34 _ = happyReduce_89

action_35 _ = happyReduce_91

action_36 _ = happyReduce_48

action_37 _ = happyReduce_49

action_38 (60) = happyShift action_126
action_38 _ = happyReduce_50

action_39 (64) = happyShift action_124
action_39 (67) = happyShift action_125
action_39 _ = happyReduce_55

action_40 _ = happyReduce_58

action_41 _ = happyReduce_61

action_42 (62) = happyShift action_107
action_42 (68) = happyShift action_108
action_42 _ = happyReduce_64

action_43 (63) = happyShift action_109
action_43 (66) = happyShift action_110
action_43 _ = happyReduce_84

action_44 (70) = happyShift action_111
action_44 (71) = happyShift action_112
action_44 (74) = happyShift action_113
action_44 (75) = happyShift action_114
action_44 _ = happyReduce_72

action_45 (58) = happyShift action_115
action_45 (73) = happyShift action_116
action_45 _ = happyReduce_83

action_46 (59) = happyShift action_117
action_46 _ = happyReduce_76

action_47 (72) = happyShift action_123
action_47 (87) = happyShift action_118
action_47 _ = happyReduce_78

action_48 _ = happyReduce_80

action_49 (65) = happyShift action_122
action_49 _ = happyReduce_86

action_50 _ = happyReduce_79

action_51 _ = happyReduce_74

action_52 _ = happyReduce_81

action_53 _ = happyReduce_82

action_54 _ = happyReduce_69

action_55 (93) = happyAccept
action_55 _ = happyFail

action_56 (60) = happyShift action_56
action_56 (64) = happyShift action_57
action_56 (67) = happyShift action_58
action_56 (79) = happyShift action_59
action_56 (83) = happyShift action_60
action_56 (89) = happyShift action_27
action_56 (90) = happyShift action_61
action_56 (91) = happyShift action_30
action_56 (29) = happyGoto action_36
action_56 (30) = happyGoto action_37
action_56 (31) = happyGoto action_38
action_56 (39) = happyGoto action_39
action_56 (40) = happyGoto action_40
action_56 (41) = happyGoto action_41
action_56 (42) = happyGoto action_42
action_56 (43) = happyGoto action_43
action_56 (44) = happyGoto action_44
action_56 (45) = happyGoto action_45
action_56 (46) = happyGoto action_46
action_56 (47) = happyGoto action_47
action_56 (48) = happyGoto action_48
action_56 (49) = happyGoto action_121
action_56 (50) = happyGoto action_50
action_56 (51) = happyGoto action_51
action_56 (52) = happyGoto action_52
action_56 (53) = happyGoto action_53
action_56 (54) = happyGoto action_54
action_56 _ = happyFail

action_57 (60) = happyShift action_56
action_57 (79) = happyShift action_59
action_57 (83) = happyShift action_60
action_57 (89) = happyShift action_27
action_57 (90) = happyShift action_61
action_57 (91) = happyShift action_30
action_57 (29) = happyGoto action_36
action_57 (30) = happyGoto action_37
action_57 (31) = happyGoto action_38
action_57 (39) = happyGoto action_39
action_57 (40) = happyGoto action_120
action_57 _ = happyFail

action_58 (60) = happyShift action_56
action_58 (79) = happyShift action_59
action_58 (83) = happyShift action_60
action_58 (89) = happyShift action_27
action_58 (90) = happyShift action_61
action_58 (91) = happyShift action_30
action_58 (29) = happyGoto action_36
action_58 (30) = happyGoto action_37
action_58 (31) = happyGoto action_38
action_58 (39) = happyGoto action_39
action_58 (40) = happyGoto action_119
action_58 _ = happyFail

action_59 _ = happyReduce_47

action_60 _ = happyReduce_46

action_61 _ = happyReduce_27

action_62 (93) = happyAccept
action_62 _ = happyFail

action_63 (93) = happyAccept
action_63 _ = happyFail

action_64 (93) = happyAccept
action_64 _ = happyFail

action_65 (93) = happyAccept
action_65 _ = happyFail

action_66 (93) = happyAccept
action_66 _ = happyFail

action_67 (93) = happyAccept
action_67 _ = happyFail

action_68 (93) = happyAccept
action_68 _ = happyFail

action_69 (87) = happyShift action_118
action_69 (93) = happyAccept
action_69 _ = happyFail

action_70 (59) = happyShift action_117
action_70 (93) = happyAccept
action_70 _ = happyFail

action_71 (58) = happyShift action_115
action_71 (73) = happyShift action_116
action_71 (93) = happyAccept
action_71 _ = happyFail

action_72 (70) = happyShift action_111
action_72 (71) = happyShift action_112
action_72 (74) = happyShift action_113
action_72 (75) = happyShift action_114
action_72 (93) = happyAccept
action_72 _ = happyFail

action_73 (63) = happyShift action_109
action_73 (66) = happyShift action_110
action_73 (93) = happyAccept
action_73 _ = happyFail

action_74 (62) = happyShift action_107
action_74 (68) = happyShift action_108
action_74 (93) = happyAccept
action_74 _ = happyFail

action_75 (93) = happyAccept
action_75 _ = happyFail

action_76 (93) = happyAccept
action_76 _ = happyFail

action_77 (93) = happyAccept
action_77 _ = happyFail

action_78 (60) = happyShift action_56
action_78 (64) = happyShift action_57
action_78 (67) = happyShift action_58
action_78 (76) = happyShift action_32
action_78 (77) = happyShift action_33
action_78 (79) = happyShift action_59
action_78 (80) = happyShift action_82
action_78 (81) = happyShift action_34
action_78 (82) = happyShift action_83
action_78 (83) = happyShift action_60
action_78 (84) = happyShift action_35
action_78 (85) = happyShift action_84
action_78 (86) = happyShift action_85
action_78 (89) = happyShift action_27
action_78 (90) = happyShift action_61
action_78 (91) = happyShift action_30
action_78 (93) = happyAccept
action_78 (29) = happyGoto action_36
action_78 (30) = happyGoto action_37
action_78 (31) = happyGoto action_38
action_78 (37) = happyGoto action_106
action_78 (39) = happyGoto action_39
action_78 (40) = happyGoto action_40
action_78 (41) = happyGoto action_41
action_78 (42) = happyGoto action_42
action_78 (43) = happyGoto action_43
action_78 (44) = happyGoto action_44
action_78 (45) = happyGoto action_45
action_78 (46) = happyGoto action_46
action_78 (47) = happyGoto action_47
action_78 (48) = happyGoto action_48
action_78 (49) = happyGoto action_80
action_78 (50) = happyGoto action_50
action_78 (51) = happyGoto action_51
action_78 (52) = happyGoto action_52
action_78 (53) = happyGoto action_53
action_78 (54) = happyGoto action_54
action_78 (56) = happyGoto action_81
action_78 _ = happyFail

action_79 (93) = happyAccept
action_79 _ = happyFail

action_80 (69) = happyShift action_105
action_80 _ = happyFail

action_81 (91) = happyShift action_30
action_81 (31) = happyGoto action_103
action_81 (57) = happyGoto action_104
action_81 _ = happyFail

action_82 (60) = happyShift action_102
action_82 _ = happyFail

action_83 (60) = happyShift action_56
action_83 (64) = happyShift action_57
action_83 (67) = happyShift action_58
action_83 (79) = happyShift action_59
action_83 (83) = happyShift action_60
action_83 (89) = happyShift action_27
action_83 (90) = happyShift action_61
action_83 (91) = happyShift action_30
action_83 (29) = happyGoto action_36
action_83 (30) = happyGoto action_37
action_83 (31) = happyGoto action_38
action_83 (39) = happyGoto action_39
action_83 (40) = happyGoto action_40
action_83 (41) = happyGoto action_41
action_83 (42) = happyGoto action_42
action_83 (43) = happyGoto action_43
action_83 (44) = happyGoto action_44
action_83 (45) = happyGoto action_45
action_83 (46) = happyGoto action_46
action_83 (47) = happyGoto action_47
action_83 (48) = happyGoto action_48
action_83 (49) = happyGoto action_101
action_83 (50) = happyGoto action_50
action_83 (51) = happyGoto action_51
action_83 (52) = happyGoto action_52
action_83 (53) = happyGoto action_53
action_83 (54) = happyGoto action_54
action_83 _ = happyFail

action_84 (60) = happyShift action_100
action_84 _ = happyFail

action_85 (38) = happyGoto action_99
action_85 _ = happyReduce_44

action_86 (65) = happyShift action_98
action_86 _ = happyReduce_35

action_87 (93) = happyAccept
action_87 _ = happyFail

action_88 (91) = happyShift action_30
action_88 (31) = happyGoto action_97
action_88 _ = happyFail

action_89 (93) = happyAccept
action_89 _ = happyFail

action_90 (76) = happyShift action_32
action_90 (77) = happyShift action_33
action_90 (81) = happyShift action_34
action_90 (84) = happyShift action_35
action_90 (93) = happyAccept
action_90 (33) = happyGoto action_95
action_90 (56) = happyGoto action_92
action_90 _ = happyFail

action_91 (93) = happyAccept
action_91 _ = happyFail

action_92 (91) = happyShift action_30
action_92 (31) = happyGoto action_96
action_92 _ = happyFail

action_93 (93) = happyAccept
action_93 _ = happyFail

action_94 (76) = happyShift action_32
action_94 (77) = happyShift action_33
action_94 (81) = happyShift action_34
action_94 (84) = happyShift action_35
action_94 (33) = happyGoto action_95
action_94 (56) = happyGoto action_92
action_94 _ = happyReduce_29

action_95 _ = happyReduce_32

action_96 (60) = happyShift action_152
action_96 _ = happyFail

action_97 _ = happyReduce_33

action_98 (76) = happyShift action_32
action_98 (77) = happyShift action_33
action_98 (81) = happyShift action_34
action_98 (84) = happyShift action_35
action_98 (35) = happyGoto action_86
action_98 (36) = happyGoto action_151
action_98 (56) = happyGoto action_88
action_98 _ = happyReduce_34

action_99 (60) = happyShift action_56
action_99 (64) = happyShift action_57
action_99 (67) = happyShift action_58
action_99 (76) = happyShift action_32
action_99 (77) = happyShift action_33
action_99 (79) = happyShift action_59
action_99 (80) = happyShift action_82
action_99 (81) = happyShift action_34
action_99 (82) = happyShift action_83
action_99 (83) = happyShift action_60
action_99 (84) = happyShift action_35
action_99 (85) = happyShift action_84
action_99 (86) = happyShift action_85
action_99 (88) = happyShift action_150
action_99 (89) = happyShift action_27
action_99 (90) = happyShift action_61
action_99 (91) = happyShift action_30
action_99 (29) = happyGoto action_36
action_99 (30) = happyGoto action_37
action_99 (31) = happyGoto action_38
action_99 (37) = happyGoto action_106
action_99 (39) = happyGoto action_39
action_99 (40) = happyGoto action_40
action_99 (41) = happyGoto action_41
action_99 (42) = happyGoto action_42
action_99 (43) = happyGoto action_43
action_99 (44) = happyGoto action_44
action_99 (45) = happyGoto action_45
action_99 (46) = happyGoto action_46
action_99 (47) = happyGoto action_47
action_99 (48) = happyGoto action_48
action_99 (49) = happyGoto action_80
action_99 (50) = happyGoto action_50
action_99 (51) = happyGoto action_51
action_99 (52) = happyGoto action_52
action_99 (53) = happyGoto action_53
action_99 (54) = happyGoto action_54
action_99 (56) = happyGoto action_81
action_99 _ = happyFail

action_100 (60) = happyShift action_56
action_100 (64) = happyShift action_57
action_100 (67) = happyShift action_58
action_100 (79) = happyShift action_59
action_100 (83) = happyShift action_60
action_100 (89) = happyShift action_27
action_100 (90) = happyShift action_61
action_100 (91) = happyShift action_30
action_100 (29) = happyGoto action_36
action_100 (30) = happyGoto action_37
action_100 (31) = happyGoto action_38
action_100 (39) = happyGoto action_39
action_100 (40) = happyGoto action_40
action_100 (41) = happyGoto action_41
action_100 (42) = happyGoto action_42
action_100 (43) = happyGoto action_43
action_100 (44) = happyGoto action_44
action_100 (45) = happyGoto action_45
action_100 (46) = happyGoto action_46
action_100 (47) = happyGoto action_47
action_100 (48) = happyGoto action_48
action_100 (49) = happyGoto action_149
action_100 (50) = happyGoto action_50
action_100 (51) = happyGoto action_51
action_100 (52) = happyGoto action_52
action_100 (53) = happyGoto action_53
action_100 (54) = happyGoto action_54
action_100 _ = happyFail

action_101 (69) = happyShift action_148
action_101 _ = happyFail

action_102 (60) = happyShift action_56
action_102 (64) = happyShift action_57
action_102 (67) = happyShift action_58
action_102 (79) = happyShift action_59
action_102 (83) = happyShift action_60
action_102 (89) = happyShift action_27
action_102 (90) = happyShift action_61
action_102 (91) = happyShift action_30
action_102 (29) = happyGoto action_36
action_102 (30) = happyGoto action_37
action_102 (31) = happyGoto action_38
action_102 (39) = happyGoto action_39
action_102 (40) = happyGoto action_40
action_102 (41) = happyGoto action_41
action_102 (42) = happyGoto action_42
action_102 (43) = happyGoto action_43
action_102 (44) = happyGoto action_44
action_102 (45) = happyGoto action_45
action_102 (46) = happyGoto action_46
action_102 (47) = happyGoto action_47
action_102 (48) = happyGoto action_48
action_102 (49) = happyGoto action_147
action_102 (50) = happyGoto action_50
action_102 (51) = happyGoto action_51
action_102 (52) = happyGoto action_52
action_102 (53) = happyGoto action_53
action_102 (54) = happyGoto action_54
action_102 _ = happyFail

action_103 (65) = happyShift action_127
action_103 (72) = happyShift action_146
action_103 _ = happyReduce_92

action_104 (69) = happyShift action_145
action_104 _ = happyFail

action_105 _ = happyReduce_37

action_106 _ = happyReduce_45

action_107 (60) = happyShift action_56
action_107 (64) = happyShift action_57
action_107 (67) = happyShift action_58
action_107 (79) = happyShift action_59
action_107 (83) = happyShift action_60
action_107 (89) = happyShift action_27
action_107 (90) = happyShift action_61
action_107 (91) = happyShift action_30
action_107 (29) = happyGoto action_36
action_107 (30) = happyGoto action_37
action_107 (31) = happyGoto action_38
action_107 (39) = happyGoto action_39
action_107 (40) = happyGoto action_40
action_107 (41) = happyGoto action_144
action_107 _ = happyFail

action_108 (60) = happyShift action_56
action_108 (64) = happyShift action_57
action_108 (67) = happyShift action_58
action_108 (79) = happyShift action_59
action_108 (83) = happyShift action_60
action_108 (89) = happyShift action_27
action_108 (90) = happyShift action_61
action_108 (91) = happyShift action_30
action_108 (29) = happyGoto action_36
action_108 (30) = happyGoto action_37
action_108 (31) = happyGoto action_38
action_108 (39) = happyGoto action_39
action_108 (40) = happyGoto action_40
action_108 (41) = happyGoto action_143
action_108 _ = happyFail

action_109 (60) = happyShift action_56
action_109 (64) = happyShift action_57
action_109 (67) = happyShift action_58
action_109 (79) = happyShift action_59
action_109 (83) = happyShift action_60
action_109 (89) = happyShift action_27
action_109 (90) = happyShift action_61
action_109 (91) = happyShift action_30
action_109 (29) = happyGoto action_36
action_109 (30) = happyGoto action_37
action_109 (31) = happyGoto action_38
action_109 (39) = happyGoto action_39
action_109 (40) = happyGoto action_40
action_109 (41) = happyGoto action_41
action_109 (42) = happyGoto action_142
action_109 _ = happyFail

action_110 (60) = happyShift action_56
action_110 (64) = happyShift action_57
action_110 (67) = happyShift action_58
action_110 (79) = happyShift action_59
action_110 (83) = happyShift action_60
action_110 (89) = happyShift action_27
action_110 (90) = happyShift action_61
action_110 (91) = happyShift action_30
action_110 (29) = happyGoto action_36
action_110 (30) = happyGoto action_37
action_110 (31) = happyGoto action_38
action_110 (39) = happyGoto action_39
action_110 (40) = happyGoto action_40
action_110 (41) = happyGoto action_41
action_110 (42) = happyGoto action_141
action_110 _ = happyFail

action_111 (60) = happyShift action_56
action_111 (64) = happyShift action_57
action_111 (67) = happyShift action_58
action_111 (79) = happyShift action_59
action_111 (83) = happyShift action_60
action_111 (89) = happyShift action_27
action_111 (90) = happyShift action_61
action_111 (91) = happyShift action_30
action_111 (29) = happyGoto action_36
action_111 (30) = happyGoto action_37
action_111 (31) = happyGoto action_38
action_111 (39) = happyGoto action_39
action_111 (40) = happyGoto action_40
action_111 (41) = happyGoto action_41
action_111 (42) = happyGoto action_42
action_111 (43) = happyGoto action_43
action_111 (54) = happyGoto action_140
action_111 _ = happyFail

action_112 (60) = happyShift action_56
action_112 (64) = happyShift action_57
action_112 (67) = happyShift action_58
action_112 (79) = happyShift action_59
action_112 (83) = happyShift action_60
action_112 (89) = happyShift action_27
action_112 (90) = happyShift action_61
action_112 (91) = happyShift action_30
action_112 (29) = happyGoto action_36
action_112 (30) = happyGoto action_37
action_112 (31) = happyGoto action_38
action_112 (39) = happyGoto action_39
action_112 (40) = happyGoto action_40
action_112 (41) = happyGoto action_41
action_112 (42) = happyGoto action_42
action_112 (43) = happyGoto action_43
action_112 (54) = happyGoto action_139
action_112 _ = happyFail

action_113 (60) = happyShift action_56
action_113 (64) = happyShift action_57
action_113 (67) = happyShift action_58
action_113 (79) = happyShift action_59
action_113 (83) = happyShift action_60
action_113 (89) = happyShift action_27
action_113 (90) = happyShift action_61
action_113 (91) = happyShift action_30
action_113 (29) = happyGoto action_36
action_113 (30) = happyGoto action_37
action_113 (31) = happyGoto action_38
action_113 (39) = happyGoto action_39
action_113 (40) = happyGoto action_40
action_113 (41) = happyGoto action_41
action_113 (42) = happyGoto action_42
action_113 (43) = happyGoto action_43
action_113 (54) = happyGoto action_138
action_113 _ = happyFail

action_114 (60) = happyShift action_56
action_114 (64) = happyShift action_57
action_114 (67) = happyShift action_58
action_114 (79) = happyShift action_59
action_114 (83) = happyShift action_60
action_114 (89) = happyShift action_27
action_114 (90) = happyShift action_61
action_114 (91) = happyShift action_30
action_114 (29) = happyGoto action_36
action_114 (30) = happyGoto action_37
action_114 (31) = happyGoto action_38
action_114 (39) = happyGoto action_39
action_114 (40) = happyGoto action_40
action_114 (41) = happyGoto action_41
action_114 (42) = happyGoto action_42
action_114 (43) = happyGoto action_43
action_114 (54) = happyGoto action_137
action_114 _ = happyFail

action_115 (60) = happyShift action_56
action_115 (64) = happyShift action_57
action_115 (67) = happyShift action_58
action_115 (79) = happyShift action_59
action_115 (83) = happyShift action_60
action_115 (89) = happyShift action_27
action_115 (90) = happyShift action_61
action_115 (91) = happyShift action_30
action_115 (29) = happyGoto action_36
action_115 (30) = happyGoto action_37
action_115 (31) = happyGoto action_38
action_115 (39) = happyGoto action_39
action_115 (40) = happyGoto action_40
action_115 (41) = happyGoto action_41
action_115 (42) = happyGoto action_42
action_115 (43) = happyGoto action_43
action_115 (44) = happyGoto action_136
action_115 (54) = happyGoto action_54
action_115 _ = happyFail

action_116 (60) = happyShift action_56
action_116 (64) = happyShift action_57
action_116 (67) = happyShift action_58
action_116 (79) = happyShift action_59
action_116 (83) = happyShift action_60
action_116 (89) = happyShift action_27
action_116 (90) = happyShift action_61
action_116 (91) = happyShift action_30
action_116 (29) = happyGoto action_36
action_116 (30) = happyGoto action_37
action_116 (31) = happyGoto action_38
action_116 (39) = happyGoto action_39
action_116 (40) = happyGoto action_40
action_116 (41) = happyGoto action_41
action_116 (42) = happyGoto action_42
action_116 (43) = happyGoto action_43
action_116 (44) = happyGoto action_135
action_116 (54) = happyGoto action_54
action_116 _ = happyFail

action_117 (60) = happyShift action_56
action_117 (64) = happyShift action_57
action_117 (67) = happyShift action_58
action_117 (79) = happyShift action_59
action_117 (83) = happyShift action_60
action_117 (89) = happyShift action_27
action_117 (90) = happyShift action_61
action_117 (91) = happyShift action_30
action_117 (29) = happyGoto action_36
action_117 (30) = happyGoto action_37
action_117 (31) = happyGoto action_38
action_117 (39) = happyGoto action_39
action_117 (40) = happyGoto action_40
action_117 (41) = happyGoto action_41
action_117 (42) = happyGoto action_42
action_117 (43) = happyGoto action_43
action_117 (44) = happyGoto action_44
action_117 (45) = happyGoto action_45
action_117 (51) = happyGoto action_134
action_117 (52) = happyGoto action_52
action_117 (53) = happyGoto action_53
action_117 (54) = happyGoto action_54
action_117 _ = happyFail

action_118 (60) = happyShift action_56
action_118 (64) = happyShift action_57
action_118 (67) = happyShift action_58
action_118 (79) = happyShift action_59
action_118 (83) = happyShift action_60
action_118 (89) = happyShift action_27
action_118 (90) = happyShift action_61
action_118 (91) = happyShift action_30
action_118 (29) = happyGoto action_36
action_118 (30) = happyGoto action_37
action_118 (31) = happyGoto action_38
action_118 (39) = happyGoto action_39
action_118 (40) = happyGoto action_40
action_118 (41) = happyGoto action_41
action_118 (42) = happyGoto action_42
action_118 (43) = happyGoto action_43
action_118 (44) = happyGoto action_44
action_118 (45) = happyGoto action_45
action_118 (46) = happyGoto action_133
action_118 (51) = happyGoto action_51
action_118 (52) = happyGoto action_52
action_118 (53) = happyGoto action_53
action_118 (54) = happyGoto action_54
action_118 _ = happyFail

action_119 _ = happyReduce_57

action_120 _ = happyReduce_56

action_121 (61) = happyShift action_132
action_121 _ = happyFail

action_122 (60) = happyShift action_56
action_122 (64) = happyShift action_57
action_122 (67) = happyShift action_58
action_122 (79) = happyShift action_59
action_122 (83) = happyShift action_60
action_122 (89) = happyShift action_27
action_122 (90) = happyShift action_61
action_122 (91) = happyShift action_30
action_122 (29) = happyGoto action_36
action_122 (30) = happyGoto action_37
action_122 (31) = happyGoto action_38
action_122 (39) = happyGoto action_39
action_122 (40) = happyGoto action_40
action_122 (41) = happyGoto action_41
action_122 (42) = happyGoto action_42
action_122 (43) = happyGoto action_43
action_122 (44) = happyGoto action_44
action_122 (45) = happyGoto action_45
action_122 (46) = happyGoto action_46
action_122 (47) = happyGoto action_47
action_122 (48) = happyGoto action_48
action_122 (49) = happyGoto action_49
action_122 (50) = happyGoto action_50
action_122 (51) = happyGoto action_51
action_122 (52) = happyGoto action_52
action_122 (53) = happyGoto action_53
action_122 (54) = happyGoto action_54
action_122 (55) = happyGoto action_131
action_122 _ = happyReduce_85

action_123 (60) = happyShift action_56
action_123 (64) = happyShift action_57
action_123 (67) = happyShift action_58
action_123 (79) = happyShift action_59
action_123 (83) = happyShift action_60
action_123 (89) = happyShift action_27
action_123 (90) = happyShift action_61
action_123 (91) = happyShift action_30
action_123 (29) = happyGoto action_36
action_123 (30) = happyGoto action_37
action_123 (31) = happyGoto action_38
action_123 (39) = happyGoto action_39
action_123 (40) = happyGoto action_40
action_123 (41) = happyGoto action_41
action_123 (42) = happyGoto action_42
action_123 (43) = happyGoto action_43
action_123 (44) = happyGoto action_44
action_123 (45) = happyGoto action_45
action_123 (46) = happyGoto action_46
action_123 (47) = happyGoto action_47
action_123 (48) = happyGoto action_130
action_123 (51) = happyGoto action_51
action_123 (52) = happyGoto action_52
action_123 (53) = happyGoto action_53
action_123 (54) = happyGoto action_54
action_123 _ = happyFail

action_124 _ = happyReduce_53

action_125 _ = happyReduce_54

action_126 (60) = happyShift action_56
action_126 (64) = happyShift action_57
action_126 (67) = happyShift action_58
action_126 (79) = happyShift action_59
action_126 (83) = happyShift action_60
action_126 (89) = happyShift action_27
action_126 (90) = happyShift action_61
action_126 (91) = happyShift action_30
action_126 (29) = happyGoto action_36
action_126 (30) = happyGoto action_37
action_126 (31) = happyGoto action_38
action_126 (39) = happyGoto action_39
action_126 (40) = happyGoto action_40
action_126 (41) = happyGoto action_41
action_126 (42) = happyGoto action_42
action_126 (43) = happyGoto action_43
action_126 (44) = happyGoto action_44
action_126 (45) = happyGoto action_45
action_126 (46) = happyGoto action_46
action_126 (47) = happyGoto action_47
action_126 (48) = happyGoto action_48
action_126 (49) = happyGoto action_49
action_126 (50) = happyGoto action_50
action_126 (51) = happyGoto action_51
action_126 (52) = happyGoto action_52
action_126 (53) = happyGoto action_53
action_126 (54) = happyGoto action_54
action_126 (55) = happyGoto action_129
action_126 _ = happyReduce_85

action_127 (91) = happyShift action_30
action_127 (31) = happyGoto action_28
action_127 (57) = happyGoto action_128
action_127 _ = happyFail

action_128 _ = happyReduce_93

action_129 (61) = happyShift action_157
action_129 _ = happyFail

action_130 _ = happyReduce_77

action_131 _ = happyReduce_87

action_132 _ = happyReduce_52

action_133 (59) = happyShift action_117
action_133 _ = happyReduce_75

action_134 _ = happyReduce_73

action_135 (70) = happyShift action_111
action_135 (71) = happyShift action_112
action_135 (74) = happyShift action_113
action_135 (75) = happyShift action_114
action_135 _ = happyReduce_70

action_136 (70) = happyShift action_111
action_136 (71) = happyShift action_112
action_136 (74) = happyShift action_113
action_136 (75) = happyShift action_114
action_136 _ = happyReduce_71

action_137 _ = happyReduce_68

action_138 _ = happyReduce_66

action_139 _ = happyReduce_67

action_140 _ = happyReduce_65

action_141 (62) = happyShift action_107
action_141 (68) = happyShift action_108
action_141 _ = happyReduce_63

action_142 (62) = happyShift action_107
action_142 (68) = happyShift action_108
action_142 _ = happyReduce_62

action_143 _ = happyReduce_60

action_144 _ = happyReduce_59

action_145 _ = happyReduce_38

action_146 (60) = happyShift action_56
action_146 (64) = happyShift action_57
action_146 (67) = happyShift action_58
action_146 (79) = happyShift action_59
action_146 (83) = happyShift action_60
action_146 (89) = happyShift action_27
action_146 (90) = happyShift action_61
action_146 (91) = happyShift action_30
action_146 (29) = happyGoto action_36
action_146 (30) = happyGoto action_37
action_146 (31) = happyGoto action_38
action_146 (39) = happyGoto action_39
action_146 (40) = happyGoto action_40
action_146 (41) = happyGoto action_41
action_146 (42) = happyGoto action_42
action_146 (43) = happyGoto action_43
action_146 (44) = happyGoto action_44
action_146 (45) = happyGoto action_45
action_146 (46) = happyGoto action_46
action_146 (47) = happyGoto action_47
action_146 (48) = happyGoto action_48
action_146 (49) = happyGoto action_156
action_146 (50) = happyGoto action_50
action_146 (51) = happyGoto action_51
action_146 (52) = happyGoto action_52
action_146 (53) = happyGoto action_53
action_146 (54) = happyGoto action_54
action_146 _ = happyFail

action_147 (61) = happyShift action_155
action_147 _ = happyFail

action_148 _ = happyReduce_40

action_149 (61) = happyShift action_154
action_149 _ = happyFail

action_150 _ = happyReduce_42

action_151 _ = happyReduce_36

action_152 (76) = happyShift action_32
action_152 (77) = happyShift action_33
action_152 (81) = happyShift action_34
action_152 (84) = happyShift action_35
action_152 (35) = happyGoto action_86
action_152 (36) = happyGoto action_153
action_152 (56) = happyGoto action_88
action_152 _ = happyReduce_34

action_153 (61) = happyShift action_161
action_153 _ = happyFail

action_154 (60) = happyShift action_56
action_154 (64) = happyShift action_57
action_154 (67) = happyShift action_58
action_154 (76) = happyShift action_32
action_154 (77) = happyShift action_33
action_154 (79) = happyShift action_59
action_154 (80) = happyShift action_82
action_154 (81) = happyShift action_34
action_154 (82) = happyShift action_83
action_154 (83) = happyShift action_60
action_154 (84) = happyShift action_35
action_154 (85) = happyShift action_84
action_154 (86) = happyShift action_85
action_154 (89) = happyShift action_27
action_154 (90) = happyShift action_61
action_154 (91) = happyShift action_30
action_154 (29) = happyGoto action_36
action_154 (30) = happyGoto action_37
action_154 (31) = happyGoto action_38
action_154 (37) = happyGoto action_160
action_154 (39) = happyGoto action_39
action_154 (40) = happyGoto action_40
action_154 (41) = happyGoto action_41
action_154 (42) = happyGoto action_42
action_154 (43) = happyGoto action_43
action_154 (44) = happyGoto action_44
action_154 (45) = happyGoto action_45
action_154 (46) = happyGoto action_46
action_154 (47) = happyGoto action_47
action_154 (48) = happyGoto action_48
action_154 (49) = happyGoto action_80
action_154 (50) = happyGoto action_50
action_154 (51) = happyGoto action_51
action_154 (52) = happyGoto action_52
action_154 (53) = happyGoto action_53
action_154 (54) = happyGoto action_54
action_154 (56) = happyGoto action_81
action_154 _ = happyFail

action_155 (60) = happyShift action_56
action_155 (64) = happyShift action_57
action_155 (67) = happyShift action_58
action_155 (76) = happyShift action_32
action_155 (77) = happyShift action_33
action_155 (79) = happyShift action_59
action_155 (80) = happyShift action_82
action_155 (81) = happyShift action_34
action_155 (82) = happyShift action_83
action_155 (83) = happyShift action_60
action_155 (84) = happyShift action_35
action_155 (85) = happyShift action_84
action_155 (86) = happyShift action_85
action_155 (89) = happyShift action_27
action_155 (90) = happyShift action_61
action_155 (91) = happyShift action_30
action_155 (29) = happyGoto action_36
action_155 (30) = happyGoto action_37
action_155 (31) = happyGoto action_38
action_155 (37) = happyGoto action_159
action_155 (39) = happyGoto action_39
action_155 (40) = happyGoto action_40
action_155 (41) = happyGoto action_41
action_155 (42) = happyGoto action_42
action_155 (43) = happyGoto action_43
action_155 (44) = happyGoto action_44
action_155 (45) = happyGoto action_45
action_155 (46) = happyGoto action_46
action_155 (47) = happyGoto action_47
action_155 (48) = happyGoto action_48
action_155 (49) = happyGoto action_80
action_155 (50) = happyGoto action_50
action_155 (51) = happyGoto action_51
action_155 (52) = happyGoto action_52
action_155 (53) = happyGoto action_53
action_155 (54) = happyGoto action_54
action_155 (56) = happyGoto action_81
action_155 _ = happyFail

action_156 (69) = happyShift action_158
action_156 _ = happyFail

action_157 _ = happyReduce_51

action_158 _ = happyReduce_39

action_159 (78) = happyShift action_163
action_159 _ = happyFail

action_160 _ = happyReduce_41

action_161 (86) = happyShift action_162
action_161 _ = happyFail

action_162 (38) = happyGoto action_165
action_162 _ = happyReduce_44

action_163 (60) = happyShift action_56
action_163 (64) = happyShift action_57
action_163 (67) = happyShift action_58
action_163 (76) = happyShift action_32
action_163 (77) = happyShift action_33
action_163 (79) = happyShift action_59
action_163 (80) = happyShift action_82
action_163 (81) = happyShift action_34
action_163 (82) = happyShift action_83
action_163 (83) = happyShift action_60
action_163 (84) = happyShift action_35
action_163 (85) = happyShift action_84
action_163 (86) = happyShift action_85
action_163 (89) = happyShift action_27
action_163 (90) = happyShift action_61
action_163 (91) = happyShift action_30
action_163 (29) = happyGoto action_36
action_163 (30) = happyGoto action_37
action_163 (31) = happyGoto action_38
action_163 (37) = happyGoto action_164
action_163 (39) = happyGoto action_39
action_163 (40) = happyGoto action_40
action_163 (41) = happyGoto action_41
action_163 (42) = happyGoto action_42
action_163 (43) = happyGoto action_43
action_163 (44) = happyGoto action_44
action_163 (45) = happyGoto action_45
action_163 (46) = happyGoto action_46
action_163 (47) = happyGoto action_47
action_163 (48) = happyGoto action_48
action_163 (49) = happyGoto action_80
action_163 (50) = happyGoto action_50
action_163 (51) = happyGoto action_51
action_163 (52) = happyGoto action_52
action_163 (53) = happyGoto action_53
action_163 (54) = happyGoto action_54
action_163 (56) = happyGoto action_81
action_163 _ = happyFail

action_164 _ = happyReduce_43

action_165 (60) = happyShift action_56
action_165 (64) = happyShift action_57
action_165 (67) = happyShift action_58
action_165 (76) = happyShift action_32
action_165 (77) = happyShift action_33
action_165 (79) = happyShift action_59
action_165 (80) = happyShift action_82
action_165 (81) = happyShift action_34
action_165 (82) = happyShift action_83
action_165 (83) = happyShift action_60
action_165 (84) = happyShift action_35
action_165 (85) = happyShift action_84
action_165 (86) = happyShift action_85
action_165 (88) = happyShift action_166
action_165 (89) = happyShift action_27
action_165 (90) = happyShift action_61
action_165 (91) = happyShift action_30
action_165 (29) = happyGoto action_36
action_165 (30) = happyGoto action_37
action_165 (31) = happyGoto action_38
action_165 (37) = happyGoto action_106
action_165 (39) = happyGoto action_39
action_165 (40) = happyGoto action_40
action_165 (41) = happyGoto action_41
action_165 (42) = happyGoto action_42
action_165 (43) = happyGoto action_43
action_165 (44) = happyGoto action_44
action_165 (45) = happyGoto action_45
action_165 (46) = happyGoto action_46
action_165 (47) = happyGoto action_47
action_165 (48) = happyGoto action_48
action_165 (49) = happyGoto action_80
action_165 (50) = happyGoto action_50
action_165 (51) = happyGoto action_51
action_165 (52) = happyGoto action_52
action_165 (53) = happyGoto action_53
action_165 (54) = happyGoto action_54
action_165 (56) = happyGoto action_81
action_165 _ = happyFail

action_166 _ = happyReduce_30

happyReduce_26 = happySpecReduce_1  29 happyReduction_26
happyReduction_26 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn29
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  30 happyReduction_27
happyReduction_27 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn30
		 ((read ( happy_var_1)) :: Double
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  31 happyReduction_28
happyReduction_28 (HappyTerminal (PT _ (T_Id happy_var_1)))
	 =  HappyAbsSyn31
		 (Id (happy_var_1)
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  32 happyReduction_29
happyReduction_29 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn32
		 (PDefs (reverse happy_var_1)
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happyReduce 8 33 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn38  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (DFun happy_var_1 happy_var_2 happy_var_4 (reverse happy_var_7)
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_0  34 happyReduction_31
happyReduction_31  =  HappyAbsSyn34
		 ([]
	)

happyReduce_32 = happySpecReduce_2  34 happyReduction_32
happyReduction_32 (HappyAbsSyn33  happy_var_2)
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  35 happyReduction_33
happyReduction_33 (HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn35
		 (ADecl happy_var_1 happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_0  36 happyReduction_34
happyReduction_34  =  HappyAbsSyn36
		 ([]
	)

happyReduce_35 = happySpecReduce_1  36 happyReduction_35
happyReduction_35 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn36
		 ((:[]) happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  36 happyReduction_36
happyReduction_36 (HappyAbsSyn36  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn36
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  37 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn37
		 (SExp happy_var_1
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  37 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn57  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn37
		 (SDecls happy_var_1 happy_var_2
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 5 37 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn39  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 (SInit happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_3  37 happyReduction_40
happyReduction_40 _
	(HappyAbsSyn39  happy_var_2)
	_
	 =  HappyAbsSyn37
		 (SReturn happy_var_2
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happyReduce 5 37 happyReduction_41
happyReduction_41 ((HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 (SWhile happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_3  37 happyReduction_42
happyReduction_42 _
	(HappyAbsSyn38  happy_var_2)
	_
	 =  HappyAbsSyn37
		 (SBlock (reverse happy_var_2)
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 7 37 happyReduction_43
happyReduction_43 ((HappyAbsSyn37  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 (SIfElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_0  38 happyReduction_44
happyReduction_44  =  HappyAbsSyn38
		 ([]
	)

happyReduce_45 = happySpecReduce_2  38 happyReduction_45
happyReduction_45 (HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  39 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn39
		 (ETrue
	)

happyReduce_47 = happySpecReduce_1  39 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn39
		 (EFalse
	)

happyReduce_48 = happySpecReduce_1  39 happyReduction_48
happyReduction_48 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn39
		 (EInt happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  39 happyReduction_49
happyReduction_49 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn39
		 (EDouble happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  39 happyReduction_50
happyReduction_50 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn39
		 (EId happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happyReduce 4 39 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn55  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (EApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_52 = happySpecReduce_3  39 happyReduction_52
happyReduction_52 _
	(HappyAbsSyn39  happy_var_2)
	_
	 =  HappyAbsSyn39
		 (happy_var_2
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  40 happyReduction_53
happyReduction_53 _
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (EPIncr happy_var_1
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  40 happyReduction_54
happyReduction_54 _
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (EPDecr happy_var_1
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  40 happyReduction_55
happyReduction_55 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  41 happyReduction_56
happyReduction_56 (HappyAbsSyn39  happy_var_2)
	_
	 =  HappyAbsSyn39
		 (EIncr happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  41 happyReduction_57
happyReduction_57 (HappyAbsSyn39  happy_var_2)
	_
	 =  HappyAbsSyn39
		 (EDecr happy_var_2
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  41 happyReduction_58
happyReduction_58 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  42 happyReduction_59
happyReduction_59 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (ETimes happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  42 happyReduction_60
happyReduction_60 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (EDiv happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  42 happyReduction_61
happyReduction_61 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  43 happyReduction_62
happyReduction_62 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (EPlus happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  43 happyReduction_63
happyReduction_63 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (EMinus happy_var_1 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  43 happyReduction_64
happyReduction_64 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  44 happyReduction_65
happyReduction_65 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (ELt happy_var_1 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  44 happyReduction_66
happyReduction_66 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (EGt happy_var_1 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  44 happyReduction_67
happyReduction_67 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (ELtEq happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  44 happyReduction_68
happyReduction_68 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (EGtWq happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  44 happyReduction_69
happyReduction_69 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  45 happyReduction_70
happyReduction_70 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (EEq happy_var_1 happy_var_3
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  45 happyReduction_71
happyReduction_71 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (ENEq happy_var_1 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  45 happyReduction_72
happyReduction_72 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  46 happyReduction_73
happyReduction_73 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (EAnd happy_var_1 happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  46 happyReduction_74
happyReduction_74 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  47 happyReduction_75
happyReduction_75 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (EOr happy_var_1 happy_var_3
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  47 happyReduction_76
happyReduction_76 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  48 happyReduction_77
happyReduction_77 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (EAss happy_var_1 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  48 happyReduction_78
happyReduction_78 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  49 happyReduction_79
happyReduction_79 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  50 happyReduction_80
happyReduction_80 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  51 happyReduction_81
happyReduction_81 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  52 happyReduction_82
happyReduction_82 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  53 happyReduction_83
happyReduction_83 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  54 happyReduction_84
happyReduction_84 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_0  55 happyReduction_85
happyReduction_85  =  HappyAbsSyn55
		 ([]
	)

happyReduce_86 = happySpecReduce_1  55 happyReduction_86
happyReduction_86 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn55
		 ((:[]) happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_3  55 happyReduction_87
happyReduction_87 (HappyAbsSyn55  happy_var_3)
	_
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn55
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_87 _ _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  56 happyReduction_88
happyReduction_88 _
	 =  HappyAbsSyn56
		 (Type_bool
	)

happyReduce_89 = happySpecReduce_1  56 happyReduction_89
happyReduction_89 _
	 =  HappyAbsSyn56
		 (Type_int
	)

happyReduce_90 = happySpecReduce_1  56 happyReduction_90
happyReduction_90 _
	 =  HappyAbsSyn56
		 (Type_double
	)

happyReduce_91 = happySpecReduce_1  56 happyReduction_91
happyReduction_91 _
	 =  HappyAbsSyn56
		 (Type_void
	)

happyReduce_92 = happySpecReduce_1  57 happyReduction_92
happyReduction_92 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn57
		 ((:[]) happy_var_1
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  57 happyReduction_93
happyReduction_93 (HappyAbsSyn57  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn57
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 93 93 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 58;
	PT _ (TS _ 2) -> cont 59;
	PT _ (TS _ 3) -> cont 60;
	PT _ (TS _ 4) -> cont 61;
	PT _ (TS _ 5) -> cont 62;
	PT _ (TS _ 6) -> cont 63;
	PT _ (TS _ 7) -> cont 64;
	PT _ (TS _ 8) -> cont 65;
	PT _ (TS _ 9) -> cont 66;
	PT _ (TS _ 10) -> cont 67;
	PT _ (TS _ 11) -> cont 68;
	PT _ (TS _ 12) -> cont 69;
	PT _ (TS _ 13) -> cont 70;
	PT _ (TS _ 14) -> cont 71;
	PT _ (TS _ 15) -> cont 72;
	PT _ (TS _ 16) -> cont 73;
	PT _ (TS _ 17) -> cont 74;
	PT _ (TS _ 18) -> cont 75;
	PT _ (TS _ 19) -> cont 76;
	PT _ (TS _ 20) -> cont 77;
	PT _ (TS _ 21) -> cont 78;
	PT _ (TS _ 22) -> cont 79;
	PT _ (TS _ 23) -> cont 80;
	PT _ (TS _ 24) -> cont 81;
	PT _ (TS _ 25) -> cont 82;
	PT _ (TS _ 26) -> cont 83;
	PT _ (TS _ 27) -> cont 84;
	PT _ (TS _ 28) -> cont 85;
	PT _ (TS _ 29) -> cont 86;
	PT _ (TS _ 30) -> cont 87;
	PT _ (TS _ 31) -> cont 88;
	PT _ (TI happy_dollar_dollar) -> cont 89;
	PT _ (TD happy_dollar_dollar) -> cont 90;
	PT _ (T_Id happy_dollar_dollar) -> cont 91;
	_ -> cont 92;
	_ -> happyError' (tk:tks)
	}

happyError_ 93 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn32 z -> happyReturn z; _other -> notHappyAtAll })

pDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pListDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn34 z -> happyReturn z; _other -> notHappyAtAll })

pArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn35 z -> happyReturn z; _other -> notHappyAtAll })

pListArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn36 z -> happyReturn z; _other -> notHappyAtAll })

pStm tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn37 z -> happyReturn z; _other -> notHappyAtAll })

pListStm tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn38 z -> happyReturn z; _other -> notHappyAtAll })

pExp15 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp14 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp13 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp12 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp11 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp9 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_12 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp8 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_13 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp4 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_14 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_15 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_16 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_17 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_18 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp5 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_19 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp6 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_20 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp7 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_21 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pExp10 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_22 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pListExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_23 tks) (\x -> case x of {HappyAbsSyn55 z -> happyReturn z; _other -> notHappyAtAll })

pType tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_24 tks) (\x -> case x of {HappyAbsSyn56 z -> happyReturn z; _other -> notHappyAtAll })

pListId tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_25 tks) (\x -> case x of {HappyAbsSyn57 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

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

{-# LINE 148 "templates/GenericTemplate.hs" #-}

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
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
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
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
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
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
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
