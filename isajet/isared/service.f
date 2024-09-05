*******************************
*    CompHEP version 33.24    *
*******************************
      FUNCTION NIN()
         NIN=2
      RETURN
      END

      FUNCTION NOUT()
         NOUT=2
      RETURN
      END

      FUNCTION LENR()
         LENR=8
      RETURN
      END

      FUNCTION NPRC()
         NPRC=1820
      RETURN
      END

      FUNCTION PINF(NSUB,NPRTCL)
      CHARACTER*6 PINF,NAMES(1820,4)
      DATA ( NAMES(1,I),I=1,4)/'~o1','~o1','Z','Z'/
      DATA ( NAMES(2,I),I=1,4)/'~o1','~o1','Z','h'/
      DATA ( NAMES(3,I),I=1,4)/'~o1','~o1','Z','H'/
      DATA ( NAMES(4,I),I=1,4)/'~o1','~o1','Z','H3'/
      DATA ( NAMES(5,I),I=1,4)/'~o1','~o1','W+','W-'/
      DATA ( NAMES(6,I),I=1,4)/'~o1','~o1','W+','H-'/
      DATA ( NAMES(7,I),I=1,4)/'~o1','~o1','W-','H+'/
      DATA ( NAMES(8,I),I=1,4)/'~o1','~o1','n1','N1'/
      DATA ( NAMES(9,I),I=1,4)/'~o1','~o1','n2','N2'/
      DATA ( NAMES(10,I),I=1,4)/'~o1','~o1','n3','N3'/
      DATA ( NAMES(11,I),I=1,4)/'~o1','~o1','e1','E1'/
      DATA ( NAMES(12,I),I=1,4)/'~o1','~o1','e2','E2'/
      DATA ( NAMES(13,I),I=1,4)/'~o1','~o1','e3','E3'/
      DATA ( NAMES(14,I),I=1,4)/'~o1','~o1','u','U'/
      DATA ( NAMES(15,I),I=1,4)/'~o1','~o1','d','D'/
      DATA ( NAMES(16,I),I=1,4)/'~o1','~o1','c','C'/
      DATA ( NAMES(17,I),I=1,4)/'~o1','~o1','s','S'/
      DATA ( NAMES(18,I),I=1,4)/'~o1','~o1','t','T'/
      DATA ( NAMES(19,I),I=1,4)/'~o1','~o1','b','B'/
      DATA ( NAMES(20,I),I=1,4)/'~o1','~o1','h','h'/
      DATA ( NAMES(21,I),I=1,4)/'~o1','~o1','h','H'/
      DATA ( NAMES(22,I),I=1,4)/'~o1','~o1','h','H3'/
      DATA ( NAMES(23,I),I=1,4)/'~o1','~o1','H','H'/
      DATA ( NAMES(24,I),I=1,4)/'~o1','~o1','H','H3'/
      DATA ( NAMES(25,I),I=1,4)/'~o1','~o1','H3','H3'/
      DATA ( NAMES(26,I),I=1,4)/'~o1','~o1','H+','H-'/
      DATA ( NAMES(27,I),I=1,4)/'~o1','~o2','Z','Z'/
      DATA ( NAMES(28,I),I=1,4)/'~o1','~o2','Z','h'/
      DATA ( NAMES(29,I),I=1,4)/'~o1','~o2','Z','H'/
      DATA ( NAMES(30,I),I=1,4)/'~o1','~o2','Z','H3'/
      DATA ( NAMES(31,I),I=1,4)/'~o1','~o2','W+','W-'/
      DATA ( NAMES(32,I),I=1,4)/'~o1','~o2','W+','H-'/
      DATA ( NAMES(33,I),I=1,4)/'~o1','~o2','W-','H+'/
      DATA ( NAMES(34,I),I=1,4)/'~o1','~o2','n1','N1'/
      DATA ( NAMES(35,I),I=1,4)/'~o1','~o2','n2','N2'/
      DATA ( NAMES(36,I),I=1,4)/'~o1','~o2','n3','N3'/
      DATA ( NAMES(37,I),I=1,4)/'~o1','~o2','e1','E1'/
      DATA ( NAMES(38,I),I=1,4)/'~o1','~o2','e2','E2'/
      DATA ( NAMES(39,I),I=1,4)/'~o1','~o2','e3','E3'/
      DATA ( NAMES(40,I),I=1,4)/'~o1','~o2','u','U'/
      DATA ( NAMES(41,I),I=1,4)/'~o1','~o2','d','D'/
      DATA ( NAMES(42,I),I=1,4)/'~o1','~o2','c','C'/
      DATA ( NAMES(43,I),I=1,4)/'~o1','~o2','s','S'/
      DATA ( NAMES(44,I),I=1,4)/'~o1','~o2','t','T'/
      DATA ( NAMES(45,I),I=1,4)/'~o1','~o2','b','B'/
      DATA ( NAMES(46,I),I=1,4)/'~o1','~o2','h','h'/
      DATA ( NAMES(47,I),I=1,4)/'~o1','~o2','h','H'/
      DATA ( NAMES(48,I),I=1,4)/'~o1','~o2','h','H3'/
      DATA ( NAMES(49,I),I=1,4)/'~o1','~o2','H','H'/
      DATA ( NAMES(50,I),I=1,4)/'~o1','~o2','H','H3'/
      DATA ( NAMES(51,I),I=1,4)/'~o1','~o2','H3','H3'/
      DATA ( NAMES(52,I),I=1,4)/'~o1','~o2','H+','H-'/
      DATA ( NAMES(53,I),I=1,4)/'~o1','~1+','A','W+'/
      DATA ( NAMES(54,I),I=1,4)/'~o1','~1+','A','H+'/
      DATA ( NAMES(55,I),I=1,4)/'~o1','~1+','Z','W+'/
      DATA ( NAMES(56,I),I=1,4)/'~o1','~1+','Z','H+'/
      DATA ( NAMES(57,I),I=1,4)/'~o1','~1+','W+','h'/
      DATA ( NAMES(58,I),I=1,4)/'~o1','~1+','W+','H'/
      DATA ( NAMES(59,I),I=1,4)/'~o1','~1+','W+','H3'/
      DATA ( NAMES(60,I),I=1,4)/'~o1','~1+','n1','E1'/
      DATA ( NAMES(61,I),I=1,4)/'~o1','~1+','n2','E2'/
      DATA ( NAMES(62,I),I=1,4)/'~o1','~1+','n3','E3'/
      DATA ( NAMES(63,I),I=1,4)/'~o1','~1+','u','D'/
      DATA ( NAMES(64,I),I=1,4)/'~o1','~1+','u','S'/
      DATA ( NAMES(65,I),I=1,4)/'~o1','~1+','u','B'/
      DATA ( NAMES(66,I),I=1,4)/'~o1','~1+','D','c'/
      DATA ( NAMES(67,I),I=1,4)/'~o1','~1+','D','t'/
      DATA ( NAMES(68,I),I=1,4)/'~o1','~1+','c','S'/
      DATA ( NAMES(69,I),I=1,4)/'~o1','~1+','c','B'/
      DATA ( NAMES(70,I),I=1,4)/'~o1','~1+','S','t'/
      DATA ( NAMES(71,I),I=1,4)/'~o1','~1+','t','B'/
      DATA ( NAMES(72,I),I=1,4)/'~o1','~1+','h','H+'/
      DATA ( NAMES(73,I),I=1,4)/'~o1','~1+','H','H+'/
      DATA ( NAMES(74,I),I=1,4)/'~o1','~1+','H3','H+'/
      DATA ( NAMES(75,I),I=1,4)/'~o1','~e1','A','e1'/
      DATA ( NAMES(76,I),I=1,4)/'~o1','~e1','Z','e1'/
      DATA ( NAMES(77,I),I=1,4)/'~o1','~e1','W-','n1'/
      DATA ( NAMES(78,I),I=1,4)/'~o1','~e1','n1','H-'/
      DATA ( NAMES(79,I),I=1,4)/'~o1','~e1','e1','h'/
      DATA ( NAMES(80,I),I=1,4)/'~o1','~e1','e1','H'/
      DATA ( NAMES(81,I),I=1,4)/'~o1','~e1','e1','H3'/
      DATA ( NAMES(82,I),I=1,4)/'~o1','~e2','A','e2'/
      DATA ( NAMES(83,I),I=1,4)/'~o1','~e2','Z','e2'/
      DATA ( NAMES(84,I),I=1,4)/'~o1','~e2','W-','n2'/
      DATA ( NAMES(85,I),I=1,4)/'~o1','~e2','n2','H-'/
      DATA ( NAMES(86,I),I=1,4)/'~o1','~e2','e2','h'/
      DATA ( NAMES(87,I),I=1,4)/'~o1','~e2','e2','H'/
      DATA ( NAMES(88,I),I=1,4)/'~o1','~e2','e2','H3'/
      DATA ( NAMES(89,I),I=1,4)/'~o1','~e3','A','e3'/
      DATA ( NAMES(90,I),I=1,4)/'~o1','~e3','Z','e3'/
      DATA ( NAMES(91,I),I=1,4)/'~o1','~e3','W-','n3'/
      DATA ( NAMES(92,I),I=1,4)/'~o1','~e3','n3','H-'/
      DATA ( NAMES(93,I),I=1,4)/'~o1','~e3','e3','h'/
      DATA ( NAMES(94,I),I=1,4)/'~o1','~e3','e3','H'/
      DATA ( NAMES(95,I),I=1,4)/'~o1','~e3','e3','H3'/
      DATA ( NAMES(96,I),I=1,4)/'~o1','~n1','Z','n1'/
      DATA ( NAMES(97,I),I=1,4)/'~o1','~n1','W+','e1'/
      DATA ( NAMES(98,I),I=1,4)/'~o1','~n1','n1','h'/
      DATA ( NAMES(99,I),I=1,4)/'~o1','~n1','n1','H'/
      DATA ( NAMES(100,I),I=1,4)/'~o1','~n1','n1','H3'/
      DATA ( NAMES(101,I),I=1,4)/'~o1','~n1','e1','H+'/
      DATA ( NAMES(102,I),I=1,4)/'~o1','~n2','Z','n2'/
      DATA ( NAMES(103,I),I=1,4)/'~o1','~n2','W+','e2'/
      DATA ( NAMES(104,I),I=1,4)/'~o1','~n2','n2','h'/
      DATA ( NAMES(105,I),I=1,4)/'~o1','~n2','n2','H'/
      DATA ( NAMES(106,I),I=1,4)/'~o1','~n2','n2','H3'/
      DATA ( NAMES(107,I),I=1,4)/'~o1','~n2','e2','H+'/
      DATA ( NAMES(108,I),I=1,4)/'~o1','~n3','Z','n3'/
      DATA ( NAMES(109,I),I=1,4)/'~o1','~n3','W+','e3'/
      DATA ( NAMES(110,I),I=1,4)/'~o1','~n3','n3','h'/
      DATA ( NAMES(111,I),I=1,4)/'~o1','~n3','n3','H'/
      DATA ( NAMES(112,I),I=1,4)/'~o1','~n3','n3','H3'/
      DATA ( NAMES(113,I),I=1,4)/'~o1','~n3','e3','H+'/
      DATA ( NAMES(114,I),I=1,4)/'~o1','~u1','A','u'/
      DATA ( NAMES(115,I),I=1,4)/'~o1','~u1','Z','u'/
      DATA ( NAMES(116,I),I=1,4)/'~o1','~u1','W+','d'/
      DATA ( NAMES(117,I),I=1,4)/'~o1','~u1','W+','s'/
      DATA ( NAMES(118,I),I=1,4)/'~o1','~u1','W+','b'/
      DATA ( NAMES(119,I),I=1,4)/'~o1','~u1','G','u'/
      DATA ( NAMES(120,I),I=1,4)/'~o1','~u1','u','h'/
      DATA ( NAMES(121,I),I=1,4)/'~o1','~u1','u','H'/
      DATA ( NAMES(122,I),I=1,4)/'~o1','~u1','u','H3'/
      DATA ( NAMES(123,I),I=1,4)/'~o1','~u1','d','H+'/
      DATA ( NAMES(124,I),I=1,4)/'~o1','~u1','s','H+'/
      DATA ( NAMES(125,I),I=1,4)/'~o1','~u1','b','H+'/
      DATA ( NAMES(126,I),I=1,4)/'~o1','~d1','A','d'/
      DATA ( NAMES(127,I),I=1,4)/'~o1','~d1','Z','d'/
      DATA ( NAMES(128,I),I=1,4)/'~o1','~d1','W-','u'/
      DATA ( NAMES(129,I),I=1,4)/'~o1','~d1','W-','c'/
      DATA ( NAMES(130,I),I=1,4)/'~o1','~d1','W-','t'/
      DATA ( NAMES(131,I),I=1,4)/'~o1','~d1','G','d'/
      DATA ( NAMES(132,I),I=1,4)/'~o1','~d1','u','H-'/
      DATA ( NAMES(133,I),I=1,4)/'~o1','~d1','d','h'/
      DATA ( NAMES(134,I),I=1,4)/'~o1','~d1','d','H'/
      DATA ( NAMES(135,I),I=1,4)/'~o1','~d1','d','H3'/
      DATA ( NAMES(136,I),I=1,4)/'~o1','~d1','c','H-'/
      DATA ( NAMES(137,I),I=1,4)/'~o1','~d1','t','H-'/
      DATA ( NAMES(138,I),I=1,4)/'~o1','~c1','A','c'/
      DATA ( NAMES(139,I),I=1,4)/'~o1','~c1','Z','c'/
      DATA ( NAMES(140,I),I=1,4)/'~o1','~c1','W+','d'/
      DATA ( NAMES(141,I),I=1,4)/'~o1','~c1','W+','s'/
      DATA ( NAMES(142,I),I=1,4)/'~o1','~c1','W+','b'/
      DATA ( NAMES(143,I),I=1,4)/'~o1','~c1','G','c'/
      DATA ( NAMES(144,I),I=1,4)/'~o1','~c1','d','H+'/
      DATA ( NAMES(145,I),I=1,4)/'~o1','~c1','c','h'/
      DATA ( NAMES(146,I),I=1,4)/'~o1','~c1','c','H'/
      DATA ( NAMES(147,I),I=1,4)/'~o1','~c1','c','H3'/
      DATA ( NAMES(148,I),I=1,4)/'~o1','~c1','s','H+'/
      DATA ( NAMES(149,I),I=1,4)/'~o1','~c1','b','H+'/
      DATA ( NAMES(150,I),I=1,4)/'~o1','~s1','A','s'/
      DATA ( NAMES(151,I),I=1,4)/'~o1','~s1','Z','s'/
      DATA ( NAMES(152,I),I=1,4)/'~o1','~s1','W-','u'/
      DATA ( NAMES(153,I),I=1,4)/'~o1','~s1','W-','c'/
      DATA ( NAMES(154,I),I=1,4)/'~o1','~s1','W-','t'/
      DATA ( NAMES(155,I),I=1,4)/'~o1','~s1','G','s'/
      DATA ( NAMES(156,I),I=1,4)/'~o1','~s1','u','H-'/
      DATA ( NAMES(157,I),I=1,4)/'~o1','~s1','c','H-'/
      DATA ( NAMES(158,I),I=1,4)/'~o1','~s1','s','h'/
      DATA ( NAMES(159,I),I=1,4)/'~o1','~s1','s','H'/
      DATA ( NAMES(160,I),I=1,4)/'~o1','~s1','s','H3'/
      DATA ( NAMES(161,I),I=1,4)/'~o1','~s1','t','H-'/
      DATA ( NAMES(162,I),I=1,4)/'~o1','~t1','A','t'/
      DATA ( NAMES(163,I),I=1,4)/'~o1','~t1','Z','t'/
      DATA ( NAMES(164,I),I=1,4)/'~o1','~t1','W+','d'/
      DATA ( NAMES(165,I),I=1,4)/'~o1','~t1','W+','s'/
      DATA ( NAMES(166,I),I=1,4)/'~o1','~t1','W+','b'/
      DATA ( NAMES(167,I),I=1,4)/'~o1','~t1','G','t'/
      DATA ( NAMES(168,I),I=1,4)/'~o1','~t1','d','H+'/
      DATA ( NAMES(169,I),I=1,4)/'~o1','~t1','s','H+'/
      DATA ( NAMES(170,I),I=1,4)/'~o1','~t1','t','h'/
      DATA ( NAMES(171,I),I=1,4)/'~o1','~t1','t','H'/
      DATA ( NAMES(172,I),I=1,4)/'~o1','~t1','t','H3'/
      DATA ( NAMES(173,I),I=1,4)/'~o1','~t1','b','H+'/
      DATA ( NAMES(174,I),I=1,4)/'~o1','~b1','A','b'/
      DATA ( NAMES(175,I),I=1,4)/'~o1','~b1','Z','b'/
      DATA ( NAMES(176,I),I=1,4)/'~o1','~b1','W-','u'/
      DATA ( NAMES(177,I),I=1,4)/'~o1','~b1','W-','c'/
      DATA ( NAMES(178,I),I=1,4)/'~o1','~b1','W-','t'/
      DATA ( NAMES(179,I),I=1,4)/'~o1','~b1','G','b'/
      DATA ( NAMES(180,I),I=1,4)/'~o1','~b1','u','H-'/
      DATA ( NAMES(181,I),I=1,4)/'~o1','~b1','c','H-'/
      DATA ( NAMES(182,I),I=1,4)/'~o1','~b1','t','H-'/
      DATA ( NAMES(183,I),I=1,4)/'~o1','~b1','b','h'/
      DATA ( NAMES(184,I),I=1,4)/'~o1','~b1','b','H'/
      DATA ( NAMES(185,I),I=1,4)/'~o1','~b1','b','H3'/
      DATA ( NAMES(186,I),I=1,4)/'~o1','~g','u','U'/
      DATA ( NAMES(187,I),I=1,4)/'~o1','~g','d','D'/
      DATA ( NAMES(188,I),I=1,4)/'~o1','~g','c','C'/
      DATA ( NAMES(189,I),I=1,4)/'~o1','~g','s','S'/
      DATA ( NAMES(190,I),I=1,4)/'~o1','~g','t','T'/
      DATA ( NAMES(191,I),I=1,4)/'~o1','~g','b','B'/
      DATA ( NAMES(192,I),I=1,4)/'~o2','~o2','Z','Z'/
      DATA ( NAMES(193,I),I=1,4)/'~o2','~o2','Z','h'/
      DATA ( NAMES(194,I),I=1,4)/'~o2','~o2','Z','H'/
      DATA ( NAMES(195,I),I=1,4)/'~o2','~o2','Z','H3'/
      DATA ( NAMES(196,I),I=1,4)/'~o2','~o2','W+','W-'/
      DATA ( NAMES(197,I),I=1,4)/'~o2','~o2','W+','H-'/
      DATA ( NAMES(198,I),I=1,4)/'~o2','~o2','W-','H+'/
      DATA ( NAMES(199,I),I=1,4)/'~o2','~o2','n1','N1'/
      DATA ( NAMES(200,I),I=1,4)/'~o2','~o2','n2','N2'/
      DATA ( NAMES(201,I),I=1,4)/'~o2','~o2','n3','N3'/
      DATA ( NAMES(202,I),I=1,4)/'~o2','~o2','e1','E1'/
      DATA ( NAMES(203,I),I=1,4)/'~o2','~o2','e2','E2'/
      DATA ( NAMES(204,I),I=1,4)/'~o2','~o2','e3','E3'/
      DATA ( NAMES(205,I),I=1,4)/'~o2','~o2','u','U'/
      DATA ( NAMES(206,I),I=1,4)/'~o2','~o2','d','D'/
      DATA ( NAMES(207,I),I=1,4)/'~o2','~o2','c','C'/
      DATA ( NAMES(208,I),I=1,4)/'~o2','~o2','s','S'/
      DATA ( NAMES(209,I),I=1,4)/'~o2','~o2','t','T'/
      DATA ( NAMES(210,I),I=1,4)/'~o2','~o2','b','B'/
      DATA ( NAMES(211,I),I=1,4)/'~o2','~o2','h','h'/
      DATA ( NAMES(212,I),I=1,4)/'~o2','~o2','h','H'/
      DATA ( NAMES(213,I),I=1,4)/'~o2','~o2','h','H3'/
      DATA ( NAMES(214,I),I=1,4)/'~o2','~o2','H','H'/
      DATA ( NAMES(215,I),I=1,4)/'~o2','~o2','H','H3'/
      DATA ( NAMES(216,I),I=1,4)/'~o2','~o2','H3','H3'/
      DATA ( NAMES(217,I),I=1,4)/'~o2','~o2','H+','H-'/
      DATA ( NAMES(218,I),I=1,4)/'~o2','~1+','A','W+'/
      DATA ( NAMES(219,I),I=1,4)/'~o2','~1+','A','H+'/
      DATA ( NAMES(220,I),I=1,4)/'~o2','~1+','Z','W+'/
      DATA ( NAMES(221,I),I=1,4)/'~o2','~1+','Z','H+'/
      DATA ( NAMES(222,I),I=1,4)/'~o2','~1+','W+','h'/
      DATA ( NAMES(223,I),I=1,4)/'~o2','~1+','W+','H'/
      DATA ( NAMES(224,I),I=1,4)/'~o2','~1+','W+','H3'/
      DATA ( NAMES(225,I),I=1,4)/'~o2','~1+','n1','E1'/
      DATA ( NAMES(226,I),I=1,4)/'~o2','~1+','n2','E2'/
      DATA ( NAMES(227,I),I=1,4)/'~o2','~1+','n3','E3'/
      DATA ( NAMES(228,I),I=1,4)/'~o2','~1+','u','D'/
      DATA ( NAMES(229,I),I=1,4)/'~o2','~1+','u','S'/
      DATA ( NAMES(230,I),I=1,4)/'~o2','~1+','u','B'/
      DATA ( NAMES(231,I),I=1,4)/'~o2','~1+','D','c'/
      DATA ( NAMES(232,I),I=1,4)/'~o2','~1+','D','t'/
      DATA ( NAMES(233,I),I=1,4)/'~o2','~1+','c','S'/
      DATA ( NAMES(234,I),I=1,4)/'~o2','~1+','c','B'/
      DATA ( NAMES(235,I),I=1,4)/'~o2','~1+','S','t'/
      DATA ( NAMES(236,I),I=1,4)/'~o2','~1+','t','B'/
      DATA ( NAMES(237,I),I=1,4)/'~o2','~1+','h','H+'/
      DATA ( NAMES(238,I),I=1,4)/'~o2','~1+','H','H+'/
      DATA ( NAMES(239,I),I=1,4)/'~o2','~1+','H3','H+'/
      DATA ( NAMES(240,I),I=1,4)/'~o2','~e1','A','e1'/
      DATA ( NAMES(241,I),I=1,4)/'~o2','~e1','Z','e1'/
      DATA ( NAMES(242,I),I=1,4)/'~o2','~e1','W-','n1'/
      DATA ( NAMES(243,I),I=1,4)/'~o2','~e1','n1','H-'/
      DATA ( NAMES(244,I),I=1,4)/'~o2','~e1','e1','h'/
      DATA ( NAMES(245,I),I=1,4)/'~o2','~e1','e1','H'/
      DATA ( NAMES(246,I),I=1,4)/'~o2','~e1','e1','H3'/
      DATA ( NAMES(247,I),I=1,4)/'~o2','~e2','A','e2'/
      DATA ( NAMES(248,I),I=1,4)/'~o2','~e2','Z','e2'/
      DATA ( NAMES(249,I),I=1,4)/'~o2','~e2','W-','n2'/
      DATA ( NAMES(250,I),I=1,4)/'~o2','~e2','n2','H-'/
      DATA ( NAMES(251,I),I=1,4)/'~o2','~e2','e2','h'/
      DATA ( NAMES(252,I),I=1,4)/'~o2','~e2','e2','H'/
      DATA ( NAMES(253,I),I=1,4)/'~o2','~e2','e2','H3'/
      DATA ( NAMES(254,I),I=1,4)/'~o2','~e3','A','e3'/
      DATA ( NAMES(255,I),I=1,4)/'~o2','~e3','Z','e3'/
      DATA ( NAMES(256,I),I=1,4)/'~o2','~e3','W-','n3'/
      DATA ( NAMES(257,I),I=1,4)/'~o2','~e3','n3','H-'/
      DATA ( NAMES(258,I),I=1,4)/'~o2','~e3','e3','h'/
      DATA ( NAMES(259,I),I=1,4)/'~o2','~e3','e3','H'/
      DATA ( NAMES(260,I),I=1,4)/'~o2','~e3','e3','H3'/
      DATA ( NAMES(261,I),I=1,4)/'~o2','~n1','Z','n1'/
      DATA ( NAMES(262,I),I=1,4)/'~o2','~n1','W+','e1'/
      DATA ( NAMES(263,I),I=1,4)/'~o2','~n1','n1','h'/
      DATA ( NAMES(264,I),I=1,4)/'~o2','~n1','n1','H'/
      DATA ( NAMES(265,I),I=1,4)/'~o2','~n1','n1','H3'/
      DATA ( NAMES(266,I),I=1,4)/'~o2','~n1','e1','H+'/
      DATA ( NAMES(267,I),I=1,4)/'~o2','~n2','Z','n2'/
      DATA ( NAMES(268,I),I=1,4)/'~o2','~n2','W+','e2'/
      DATA ( NAMES(269,I),I=1,4)/'~o2','~n2','n2','h'/
      DATA ( NAMES(270,I),I=1,4)/'~o2','~n2','n2','H'/
      DATA ( NAMES(271,I),I=1,4)/'~o2','~n2','n2','H3'/
      DATA ( NAMES(272,I),I=1,4)/'~o2','~n2','e2','H+'/
      DATA ( NAMES(273,I),I=1,4)/'~o2','~n3','Z','n3'/
      DATA ( NAMES(274,I),I=1,4)/'~o2','~n3','W+','e3'/
      DATA ( NAMES(275,I),I=1,4)/'~o2','~n3','n3','h'/
      DATA ( NAMES(276,I),I=1,4)/'~o2','~n3','n3','H'/
      DATA ( NAMES(277,I),I=1,4)/'~o2','~n3','n3','H3'/
      DATA ( NAMES(278,I),I=1,4)/'~o2','~n3','e3','H+'/
      DATA ( NAMES(279,I),I=1,4)/'~o2','~u1','A','u'/
      DATA ( NAMES(280,I),I=1,4)/'~o2','~u1','Z','u'/
      DATA ( NAMES(281,I),I=1,4)/'~o2','~u1','W+','d'/
      DATA ( NAMES(282,I),I=1,4)/'~o2','~u1','W+','s'/
      DATA ( NAMES(283,I),I=1,4)/'~o2','~u1','W+','b'/
      DATA ( NAMES(284,I),I=1,4)/'~o2','~u1','G','u'/
      DATA ( NAMES(285,I),I=1,4)/'~o2','~u1','u','h'/
      DATA ( NAMES(286,I),I=1,4)/'~o2','~u1','u','H'/
      DATA ( NAMES(287,I),I=1,4)/'~o2','~u1','u','H3'/
      DATA ( NAMES(288,I),I=1,4)/'~o2','~u1','d','H+'/
      DATA ( NAMES(289,I),I=1,4)/'~o2','~u1','s','H+'/
      DATA ( NAMES(290,I),I=1,4)/'~o2','~u1','b','H+'/
      DATA ( NAMES(291,I),I=1,4)/'~o2','~d1','A','d'/
      DATA ( NAMES(292,I),I=1,4)/'~o2','~d1','Z','d'/
      DATA ( NAMES(293,I),I=1,4)/'~o2','~d1','W-','u'/
      DATA ( NAMES(294,I),I=1,4)/'~o2','~d1','W-','c'/
      DATA ( NAMES(295,I),I=1,4)/'~o2','~d1','W-','t'/
      DATA ( NAMES(296,I),I=1,4)/'~o2','~d1','G','d'/
      DATA ( NAMES(297,I),I=1,4)/'~o2','~d1','u','H-'/
      DATA ( NAMES(298,I),I=1,4)/'~o2','~d1','d','h'/
      DATA ( NAMES(299,I),I=1,4)/'~o2','~d1','d','H'/
      DATA ( NAMES(300,I),I=1,4)/'~o2','~d1','d','H3'/
      DATA ( NAMES(301,I),I=1,4)/'~o2','~d1','c','H-'/
      DATA ( NAMES(302,I),I=1,4)/'~o2','~d1','t','H-'/
      DATA ( NAMES(303,I),I=1,4)/'~o2','~c1','A','c'/
      DATA ( NAMES(304,I),I=1,4)/'~o2','~c1','Z','c'/
      DATA ( NAMES(305,I),I=1,4)/'~o2','~c1','W+','d'/
      DATA ( NAMES(306,I),I=1,4)/'~o2','~c1','W+','s'/
      DATA ( NAMES(307,I),I=1,4)/'~o2','~c1','W+','b'/
      DATA ( NAMES(308,I),I=1,4)/'~o2','~c1','G','c'/
      DATA ( NAMES(309,I),I=1,4)/'~o2','~c1','d','H+'/
      DATA ( NAMES(310,I),I=1,4)/'~o2','~c1','c','h'/
      DATA ( NAMES(311,I),I=1,4)/'~o2','~c1','c','H'/
      DATA ( NAMES(312,I),I=1,4)/'~o2','~c1','c','H3'/
      DATA ( NAMES(313,I),I=1,4)/'~o2','~c1','s','H+'/
      DATA ( NAMES(314,I),I=1,4)/'~o2','~c1','b','H+'/
      DATA ( NAMES(315,I),I=1,4)/'~o2','~s1','A','s'/
      DATA ( NAMES(316,I),I=1,4)/'~o2','~s1','Z','s'/
      DATA ( NAMES(317,I),I=1,4)/'~o2','~s1','W-','u'/
      DATA ( NAMES(318,I),I=1,4)/'~o2','~s1','W-','c'/
      DATA ( NAMES(319,I),I=1,4)/'~o2','~s1','W-','t'/
      DATA ( NAMES(320,I),I=1,4)/'~o2','~s1','G','s'/
      DATA ( NAMES(321,I),I=1,4)/'~o2','~s1','u','H-'/
      DATA ( NAMES(322,I),I=1,4)/'~o2','~s1','c','H-'/
      DATA ( NAMES(323,I),I=1,4)/'~o2','~s1','s','h'/
      DATA ( NAMES(324,I),I=1,4)/'~o2','~s1','s','H'/
      DATA ( NAMES(325,I),I=1,4)/'~o2','~s1','s','H3'/
      DATA ( NAMES(326,I),I=1,4)/'~o2','~s1','t','H-'/
      DATA ( NAMES(327,I),I=1,4)/'~o2','~t1','A','t'/
      DATA ( NAMES(328,I),I=1,4)/'~o2','~t1','Z','t'/
      DATA ( NAMES(329,I),I=1,4)/'~o2','~t1','W+','d'/
      DATA ( NAMES(330,I),I=1,4)/'~o2','~t1','W+','s'/
      DATA ( NAMES(331,I),I=1,4)/'~o2','~t1','W+','b'/
      DATA ( NAMES(332,I),I=1,4)/'~o2','~t1','G','t'/
      DATA ( NAMES(333,I),I=1,4)/'~o2','~t1','d','H+'/
      DATA ( NAMES(334,I),I=1,4)/'~o2','~t1','s','H+'/
      DATA ( NAMES(335,I),I=1,4)/'~o2','~t1','t','h'/
      DATA ( NAMES(336,I),I=1,4)/'~o2','~t1','t','H'/
      DATA ( NAMES(337,I),I=1,4)/'~o2','~t1','t','H3'/
      DATA ( NAMES(338,I),I=1,4)/'~o2','~t1','b','H+'/
      DATA ( NAMES(339,I),I=1,4)/'~o2','~b1','A','b'/
      DATA ( NAMES(340,I),I=1,4)/'~o2','~b1','Z','b'/
      DATA ( NAMES(341,I),I=1,4)/'~o2','~b1','W-','u'/
      DATA ( NAMES(342,I),I=1,4)/'~o2','~b1','W-','c'/
      DATA ( NAMES(343,I),I=1,4)/'~o2','~b1','W-','t'/
      DATA ( NAMES(344,I),I=1,4)/'~o2','~b1','G','b'/
      DATA ( NAMES(345,I),I=1,4)/'~o2','~b1','u','H-'/
      DATA ( NAMES(346,I),I=1,4)/'~o2','~b1','c','H-'/
      DATA ( NAMES(347,I),I=1,4)/'~o2','~b1','t','H-'/
      DATA ( NAMES(348,I),I=1,4)/'~o2','~b1','b','h'/
      DATA ( NAMES(349,I),I=1,4)/'~o2','~b1','b','H'/
      DATA ( NAMES(350,I),I=1,4)/'~o2','~b1','b','H3'/
      DATA ( NAMES(351,I),I=1,4)/'~o2','~g','u','U'/
      DATA ( NAMES(352,I),I=1,4)/'~o2','~g','d','D'/
      DATA ( NAMES(353,I),I=1,4)/'~o2','~g','c','C'/
      DATA ( NAMES(354,I),I=1,4)/'~o2','~g','s','S'/
      DATA ( NAMES(355,I),I=1,4)/'~o2','~g','t','T'/
      DATA ( NAMES(356,I),I=1,4)/'~o2','~g','b','B'/
      DATA ( NAMES(357,I),I=1,4)/'~1+','~1+','W+','W+'/
      DATA ( NAMES(358,I),I=1,4)/'~1+','~1+','W+','H+'/
      DATA ( NAMES(359,I),I=1,4)/'~1+','~1+','H+','H+'/
      DATA ( NAMES(360,I),I=1,4)/'~1+','~1-','A','A'/
      DATA ( NAMES(361,I),I=1,4)/'~1+','~1-','A','Z'/
      DATA ( NAMES(362,I),I=1,4)/'~1+','~1-','A','h'/
      DATA ( NAMES(363,I),I=1,4)/'~1+','~1-','A','H'/
      DATA ( NAMES(364,I),I=1,4)/'~1+','~1-','A','H3'/
      DATA ( NAMES(365,I),I=1,4)/'~1+','~1-','Z','Z'/
      DATA ( NAMES(366,I),I=1,4)/'~1+','~1-','Z','h'/
      DATA ( NAMES(367,I),I=1,4)/'~1+','~1-','Z','H'/
      DATA ( NAMES(368,I),I=1,4)/'~1+','~1-','Z','H3'/
      DATA ( NAMES(369,I),I=1,4)/'~1+','~1-','W+','W-'/
      DATA ( NAMES(370,I),I=1,4)/'~1+','~1-','W+','H-'/
      DATA ( NAMES(371,I),I=1,4)/'~1+','~1-','W-','H+'/
      DATA ( NAMES(372,I),I=1,4)/'~1+','~1-','n1','N1'/
      DATA ( NAMES(373,I),I=1,4)/'~1+','~1-','n2','N2'/
      DATA ( NAMES(374,I),I=1,4)/'~1+','~1-','n3','N3'/
      DATA ( NAMES(375,I),I=1,4)/'~1+','~1-','e1','E1'/
      DATA ( NAMES(376,I),I=1,4)/'~1+','~1-','e2','E2'/
      DATA ( NAMES(377,I),I=1,4)/'~1+','~1-','e3','E3'/
      DATA ( NAMES(378,I),I=1,4)/'~1+','~1-','u','U'/
      DATA ( NAMES(379,I),I=1,4)/'~1+','~1-','u','C'/
      DATA ( NAMES(380,I),I=1,4)/'~1+','~1-','u','T'/
      DATA ( NAMES(381,I),I=1,4)/'~1+','~1-','U','c'/
      DATA ( NAMES(382,I),I=1,4)/'~1+','~1-','U','t'/
      DATA ( NAMES(383,I),I=1,4)/'~1+','~1-','d','D'/
      DATA ( NAMES(384,I),I=1,4)/'~1+','~1-','d','S'/
      DATA ( NAMES(385,I),I=1,4)/'~1+','~1-','d','B'/
      DATA ( NAMES(386,I),I=1,4)/'~1+','~1-','D','s'/
      DATA ( NAMES(387,I),I=1,4)/'~1+','~1-','D','b'/
      DATA ( NAMES(388,I),I=1,4)/'~1+','~1-','c','C'/
      DATA ( NAMES(389,I),I=1,4)/'~1+','~1-','c','T'/
      DATA ( NAMES(390,I),I=1,4)/'~1+','~1-','C','t'/
      DATA ( NAMES(391,I),I=1,4)/'~1+','~1-','s','S'/
      DATA ( NAMES(392,I),I=1,4)/'~1+','~1-','s','B'/
      DATA ( NAMES(393,I),I=1,4)/'~1+','~1-','S','b'/
      DATA ( NAMES(394,I),I=1,4)/'~1+','~1-','t','T'/
      DATA ( NAMES(395,I),I=1,4)/'~1+','~1-','b','B'/
      DATA ( NAMES(396,I),I=1,4)/'~1+','~1-','h','h'/
      DATA ( NAMES(397,I),I=1,4)/'~1+','~1-','h','H'/
      DATA ( NAMES(398,I),I=1,4)/'~1+','~1-','h','H3'/
      DATA ( NAMES(399,I),I=1,4)/'~1+','~1-','H','H'/
      DATA ( NAMES(400,I),I=1,4)/'~1+','~1-','H','H3'/
      DATA ( NAMES(401,I),I=1,4)/'~1+','~1-','H3','H3'/
      DATA ( NAMES(402,I),I=1,4)/'~1+','~1-','H+','H-'/
      DATA ( NAMES(403,I),I=1,4)/'~1+','~e1','A','n1'/
      DATA ( NAMES(404,I),I=1,4)/'~1+','~e1','Z','n1'/
      DATA ( NAMES(405,I),I=1,4)/'~1+','~e1','W+','e1'/
      DATA ( NAMES(406,I),I=1,4)/'~1+','~e1','n1','h'/
      DATA ( NAMES(407,I),I=1,4)/'~1+','~e1','n1','H'/
      DATA ( NAMES(408,I),I=1,4)/'~1+','~e1','n1','H3'/
      DATA ( NAMES(409,I),I=1,4)/'~1+','~e1','e1','H+'/
      DATA ( NAMES(410,I),I=1,4)/'~1+','~E1','W+','E1'/
      DATA ( NAMES(411,I),I=1,4)/'~1+','~E1','E1','H+'/
      DATA ( NAMES(412,I),I=1,4)/'~1+','~e2','A','n2'/
      DATA ( NAMES(413,I),I=1,4)/'~1+','~e2','Z','n2'/
      DATA ( NAMES(414,I),I=1,4)/'~1+','~e2','W+','e2'/
      DATA ( NAMES(415,I),I=1,4)/'~1+','~e2','n2','h'/
      DATA ( NAMES(416,I),I=1,4)/'~1+','~e2','n2','H'/
      DATA ( NAMES(417,I),I=1,4)/'~1+','~e2','n2','H3'/
      DATA ( NAMES(418,I),I=1,4)/'~1+','~e2','e2','H+'/
      DATA ( NAMES(419,I),I=1,4)/'~1+','~E2','W+','E2'/
      DATA ( NAMES(420,I),I=1,4)/'~1+','~E2','E2','H+'/
      DATA ( NAMES(421,I),I=1,4)/'~1+','~e3','A','n3'/
      DATA ( NAMES(422,I),I=1,4)/'~1+','~e3','Z','n3'/
      DATA ( NAMES(423,I),I=1,4)/'~1+','~e3','W+','e3'/
      DATA ( NAMES(424,I),I=1,4)/'~1+','~e3','n3','h'/
      DATA ( NAMES(425,I),I=1,4)/'~1+','~e3','n3','H'/
      DATA ( NAMES(426,I),I=1,4)/'~1+','~e3','n3','H3'/
      DATA ( NAMES(427,I),I=1,4)/'~1+','~e3','e3','H+'/
      DATA ( NAMES(428,I),I=1,4)/'~1+','~E3','W+','E3'/
      DATA ( NAMES(429,I),I=1,4)/'~1+','~E3','E3','H+'/
      DATA ( NAMES(430,I),I=1,4)/'~1+','~n1','W+','n1'/
      DATA ( NAMES(431,I),I=1,4)/'~1+','~n1','n1','H+'/
      DATA ( NAMES(432,I),I=1,4)/'~1+','~N1','A','E1'/
      DATA ( NAMES(433,I),I=1,4)/'~1+','~N1','Z','E1'/
      DATA ( NAMES(434,I),I=1,4)/'~1+','~N1','W+','N1'/
      DATA ( NAMES(435,I),I=1,4)/'~1+','~N1','N1','H+'/
      DATA ( NAMES(436,I),I=1,4)/'~1+','~N1','E1','h'/
      DATA ( NAMES(437,I),I=1,4)/'~1+','~N1','E1','H'/
      DATA ( NAMES(438,I),I=1,4)/'~1+','~N1','E1','H3'/
      DATA ( NAMES(439,I),I=1,4)/'~1+','~n2','W+','n2'/
      DATA ( NAMES(440,I),I=1,4)/'~1+','~n2','n2','H+'/
      DATA ( NAMES(441,I),I=1,4)/'~1+','~N2','A','E2'/
      DATA ( NAMES(442,I),I=1,4)/'~1+','~N2','Z','E2'/
      DATA ( NAMES(443,I),I=1,4)/'~1+','~N2','W+','N2'/
      DATA ( NAMES(444,I),I=1,4)/'~1+','~N2','N2','H+'/
      DATA ( NAMES(445,I),I=1,4)/'~1+','~N2','E2','h'/
      DATA ( NAMES(446,I),I=1,4)/'~1+','~N2','E2','H'/
      DATA ( NAMES(447,I),I=1,4)/'~1+','~N2','E2','H3'/
      DATA ( NAMES(448,I),I=1,4)/'~1+','~n3','W+','n3'/
      DATA ( NAMES(449,I),I=1,4)/'~1+','~n3','n3','H+'/
      DATA ( NAMES(450,I),I=1,4)/'~1+','~N3','A','E3'/
      DATA ( NAMES(451,I),I=1,4)/'~1+','~N3','Z','E3'/
      DATA ( NAMES(452,I),I=1,4)/'~1+','~N3','W+','N3'/
      DATA ( NAMES(453,I),I=1,4)/'~1+','~N3','N3','H+'/
      DATA ( NAMES(454,I),I=1,4)/'~1+','~N3','E3','h'/
      DATA ( NAMES(455,I),I=1,4)/'~1+','~N3','E3','H'/
      DATA ( NAMES(456,I),I=1,4)/'~1+','~N3','E3','H3'/
      DATA ( NAMES(457,I),I=1,4)/'~1+','~u1','W+','u'/
      DATA ( NAMES(458,I),I=1,4)/'~1+','~u1','W+','c'/
      DATA ( NAMES(459,I),I=1,4)/'~1+','~u1','W+','t'/
      DATA ( NAMES(460,I),I=1,4)/'~1+','~u1','u','H+'/
      DATA ( NAMES(461,I),I=1,4)/'~1+','~u1','c','H+'/
      DATA ( NAMES(462,I),I=1,4)/'~1+','~u1','t','H+'/
      DATA ( NAMES(463,I),I=1,4)/'~1+','~U1','A','D'/
      DATA ( NAMES(464,I),I=1,4)/'~1+','~U1','A','S'/
      DATA ( NAMES(465,I),I=1,4)/'~1+','~U1','A','B'/
      DATA ( NAMES(466,I),I=1,4)/'~1+','~U1','Z','D'/
      DATA ( NAMES(467,I),I=1,4)/'~1+','~U1','Z','S'/
      DATA ( NAMES(468,I),I=1,4)/'~1+','~U1','Z','B'/
      DATA ( NAMES(469,I),I=1,4)/'~1+','~U1','W+','U'/
      DATA ( NAMES(470,I),I=1,4)/'~1+','~U1','W+','C'/
      DATA ( NAMES(471,I),I=1,4)/'~1+','~U1','W+','T'/
      DATA ( NAMES(472,I),I=1,4)/'~1+','~U1','G','D'/
      DATA ( NAMES(473,I),I=1,4)/'~1+','~U1','G','S'/
      DATA ( NAMES(474,I),I=1,4)/'~1+','~U1','G','B'/
      DATA ( NAMES(475,I),I=1,4)/'~1+','~U1','U','H+'/
      DATA ( NAMES(476,I),I=1,4)/'~1+','~U1','D','h'/
      DATA ( NAMES(477,I),I=1,4)/'~1+','~U1','D','H'/
      DATA ( NAMES(478,I),I=1,4)/'~1+','~U1','D','H3'/
      DATA ( NAMES(479,I),I=1,4)/'~1+','~U1','C','H+'/
      DATA ( NAMES(480,I),I=1,4)/'~1+','~U1','S','h'/
      DATA ( NAMES(481,I),I=1,4)/'~1+','~U1','S','H'/
      DATA ( NAMES(482,I),I=1,4)/'~1+','~U1','S','H3'/
      DATA ( NAMES(483,I),I=1,4)/'~1+','~U1','T','H+'/
      DATA ( NAMES(484,I),I=1,4)/'~1+','~U1','B','h'/
      DATA ( NAMES(485,I),I=1,4)/'~1+','~U1','B','H'/
      DATA ( NAMES(486,I),I=1,4)/'~1+','~U1','B','H3'/
      DATA ( NAMES(487,I),I=1,4)/'~1+','~d1','A','u'/
      DATA ( NAMES(488,I),I=1,4)/'~1+','~d1','A','c'/
      DATA ( NAMES(489,I),I=1,4)/'~1+','~d1','A','t'/
      DATA ( NAMES(490,I),I=1,4)/'~1+','~d1','Z','u'/
      DATA ( NAMES(491,I),I=1,4)/'~1+','~d1','Z','c'/
      DATA ( NAMES(492,I),I=1,4)/'~1+','~d1','Z','t'/
      DATA ( NAMES(493,I),I=1,4)/'~1+','~d1','W+','d'/
      DATA ( NAMES(494,I),I=1,4)/'~1+','~d1','W+','s'/
      DATA ( NAMES(495,I),I=1,4)/'~1+','~d1','W+','b'/
      DATA ( NAMES(496,I),I=1,4)/'~1+','~d1','G','u'/
      DATA ( NAMES(497,I),I=1,4)/'~1+','~d1','G','c'/
      DATA ( NAMES(498,I),I=1,4)/'~1+','~d1','G','t'/
      DATA ( NAMES(499,I),I=1,4)/'~1+','~d1','u','h'/
      DATA ( NAMES(500,I),I=1,4)/'~1+','~d1','u','H'/
      DATA ( NAMES(501,I),I=1,4)/'~1+','~d1','u','H3'/
      DATA ( NAMES(502,I),I=1,4)/'~1+','~d1','d','H+'/
      DATA ( NAMES(503,I),I=1,4)/'~1+','~d1','c','h'/
      DATA ( NAMES(504,I),I=1,4)/'~1+','~d1','c','H'/
      DATA ( NAMES(505,I),I=1,4)/'~1+','~d1','c','H3'/
      DATA ( NAMES(506,I),I=1,4)/'~1+','~d1','s','H+'/
      DATA ( NAMES(507,I),I=1,4)/'~1+','~d1','t','h'/
      DATA ( NAMES(508,I),I=1,4)/'~1+','~d1','t','H'/
      DATA ( NAMES(509,I),I=1,4)/'~1+','~d1','t','H3'/
      DATA ( NAMES(510,I),I=1,4)/'~1+','~d1','b','H+'/
      DATA ( NAMES(511,I),I=1,4)/'~1+','~D1','W+','D'/
      DATA ( NAMES(512,I),I=1,4)/'~1+','~D1','W+','S'/
      DATA ( NAMES(513,I),I=1,4)/'~1+','~D1','W+','B'/
      DATA ( NAMES(514,I),I=1,4)/'~1+','~D1','D','H+'/
      DATA ( NAMES(515,I),I=1,4)/'~1+','~D1','S','H+'/
      DATA ( NAMES(516,I),I=1,4)/'~1+','~D1','B','H+'/
      DATA ( NAMES(517,I),I=1,4)/'~1+','~c1','W+','u'/
      DATA ( NAMES(518,I),I=1,4)/'~1+','~c1','W+','c'/
      DATA ( NAMES(519,I),I=1,4)/'~1+','~c1','W+','t'/
      DATA ( NAMES(520,I),I=1,4)/'~1+','~c1','u','H+'/
      DATA ( NAMES(521,I),I=1,4)/'~1+','~c1','c','H+'/
      DATA ( NAMES(522,I),I=1,4)/'~1+','~c1','t','H+'/
      DATA ( NAMES(523,I),I=1,4)/'~1+','~C1','A','D'/
      DATA ( NAMES(524,I),I=1,4)/'~1+','~C1','A','S'/
      DATA ( NAMES(525,I),I=1,4)/'~1+','~C1','A','B'/
      DATA ( NAMES(526,I),I=1,4)/'~1+','~C1','Z','D'/
      DATA ( NAMES(527,I),I=1,4)/'~1+','~C1','Z','S'/
      DATA ( NAMES(528,I),I=1,4)/'~1+','~C1','Z','B'/
      DATA ( NAMES(529,I),I=1,4)/'~1+','~C1','W+','U'/
      DATA ( NAMES(530,I),I=1,4)/'~1+','~C1','W+','C'/
      DATA ( NAMES(531,I),I=1,4)/'~1+','~C1','W+','T'/
      DATA ( NAMES(532,I),I=1,4)/'~1+','~C1','G','D'/
      DATA ( NAMES(533,I),I=1,4)/'~1+','~C1','G','S'/
      DATA ( NAMES(534,I),I=1,4)/'~1+','~C1','G','B'/
      DATA ( NAMES(535,I),I=1,4)/'~1+','~C1','U','H+'/
      DATA ( NAMES(536,I),I=1,4)/'~1+','~C1','D','h'/
      DATA ( NAMES(537,I),I=1,4)/'~1+','~C1','D','H'/
      DATA ( NAMES(538,I),I=1,4)/'~1+','~C1','D','H3'/
      DATA ( NAMES(539,I),I=1,4)/'~1+','~C1','C','H+'/
      DATA ( NAMES(540,I),I=1,4)/'~1+','~C1','S','h'/
      DATA ( NAMES(541,I),I=1,4)/'~1+','~C1','S','H'/
      DATA ( NAMES(542,I),I=1,4)/'~1+','~C1','S','H3'/
      DATA ( NAMES(543,I),I=1,4)/'~1+','~C1','T','H+'/
      DATA ( NAMES(544,I),I=1,4)/'~1+','~C1','B','h'/
      DATA ( NAMES(545,I),I=1,4)/'~1+','~C1','B','H'/
      DATA ( NAMES(546,I),I=1,4)/'~1+','~C1','B','H3'/
      DATA ( NAMES(547,I),I=1,4)/'~1+','~s1','A','u'/
      DATA ( NAMES(548,I),I=1,4)/'~1+','~s1','A','c'/
      DATA ( NAMES(549,I),I=1,4)/'~1+','~s1','A','t'/
      DATA ( NAMES(550,I),I=1,4)/'~1+','~s1','Z','u'/
      DATA ( NAMES(551,I),I=1,4)/'~1+','~s1','Z','c'/
      DATA ( NAMES(552,I),I=1,4)/'~1+','~s1','Z','t'/
      DATA ( NAMES(553,I),I=1,4)/'~1+','~s1','W+','d'/
      DATA ( NAMES(554,I),I=1,4)/'~1+','~s1','W+','s'/
      DATA ( NAMES(555,I),I=1,4)/'~1+','~s1','W+','b'/
      DATA ( NAMES(556,I),I=1,4)/'~1+','~s1','G','u'/
      DATA ( NAMES(557,I),I=1,4)/'~1+','~s1','G','c'/
      DATA ( NAMES(558,I),I=1,4)/'~1+','~s1','G','t'/
      DATA ( NAMES(559,I),I=1,4)/'~1+','~s1','u','h'/
      DATA ( NAMES(560,I),I=1,4)/'~1+','~s1','u','H'/
      DATA ( NAMES(561,I),I=1,4)/'~1+','~s1','u','H3'/
      DATA ( NAMES(562,I),I=1,4)/'~1+','~s1','d','H+'/
      DATA ( NAMES(563,I),I=1,4)/'~1+','~s1','c','h'/
      DATA ( NAMES(564,I),I=1,4)/'~1+','~s1','c','H'/
      DATA ( NAMES(565,I),I=1,4)/'~1+','~s1','c','H3'/
      DATA ( NAMES(566,I),I=1,4)/'~1+','~s1','s','H+'/
      DATA ( NAMES(567,I),I=1,4)/'~1+','~s1','t','h'/
      DATA ( NAMES(568,I),I=1,4)/'~1+','~s1','t','H'/
      DATA ( NAMES(569,I),I=1,4)/'~1+','~s1','t','H3'/
      DATA ( NAMES(570,I),I=1,4)/'~1+','~s1','b','H+'/
      DATA ( NAMES(571,I),I=1,4)/'~1+','~S1','W+','D'/
      DATA ( NAMES(572,I),I=1,4)/'~1+','~S1','W+','S'/
      DATA ( NAMES(573,I),I=1,4)/'~1+','~S1','W+','B'/
      DATA ( NAMES(574,I),I=1,4)/'~1+','~S1','D','H+'/
      DATA ( NAMES(575,I),I=1,4)/'~1+','~S1','S','H+'/
      DATA ( NAMES(576,I),I=1,4)/'~1+','~S1','B','H+'/
      DATA ( NAMES(577,I),I=1,4)/'~1+','~t1','W+','u'/
      DATA ( NAMES(578,I),I=1,4)/'~1+','~t1','W+','c'/
      DATA ( NAMES(579,I),I=1,4)/'~1+','~t1','W+','t'/
      DATA ( NAMES(580,I),I=1,4)/'~1+','~t1','u','H+'/
      DATA ( NAMES(581,I),I=1,4)/'~1+','~t1','c','H+'/
      DATA ( NAMES(582,I),I=1,4)/'~1+','~t1','t','H+'/
      DATA ( NAMES(583,I),I=1,4)/'~1+','~T1','A','D'/
      DATA ( NAMES(584,I),I=1,4)/'~1+','~T1','A','S'/
      DATA ( NAMES(585,I),I=1,4)/'~1+','~T1','A','B'/
      DATA ( NAMES(586,I),I=1,4)/'~1+','~T1','Z','D'/
      DATA ( NAMES(587,I),I=1,4)/'~1+','~T1','Z','S'/
      DATA ( NAMES(588,I),I=1,4)/'~1+','~T1','Z','B'/
      DATA ( NAMES(589,I),I=1,4)/'~1+','~T1','W+','U'/
      DATA ( NAMES(590,I),I=1,4)/'~1+','~T1','W+','C'/
      DATA ( NAMES(591,I),I=1,4)/'~1+','~T1','W+','T'/
      DATA ( NAMES(592,I),I=1,4)/'~1+','~T1','G','D'/
      DATA ( NAMES(593,I),I=1,4)/'~1+','~T1','G','S'/
      DATA ( NAMES(594,I),I=1,4)/'~1+','~T1','G','B'/
      DATA ( NAMES(595,I),I=1,4)/'~1+','~T1','U','H+'/
      DATA ( NAMES(596,I),I=1,4)/'~1+','~T1','D','h'/
      DATA ( NAMES(597,I),I=1,4)/'~1+','~T1','D','H'/
      DATA ( NAMES(598,I),I=1,4)/'~1+','~T1','D','H3'/
      DATA ( NAMES(599,I),I=1,4)/'~1+','~T1','C','H+'/
      DATA ( NAMES(600,I),I=1,4)/'~1+','~T1','S','h'/
      DATA ( NAMES(601,I),I=1,4)/'~1+','~T1','S','H'/
      DATA ( NAMES(602,I),I=1,4)/'~1+','~T1','S','H3'/
      DATA ( NAMES(603,I),I=1,4)/'~1+','~T1','T','H+'/
      DATA ( NAMES(604,I),I=1,4)/'~1+','~T1','B','h'/
      DATA ( NAMES(605,I),I=1,4)/'~1+','~T1','B','H'/
      DATA ( NAMES(606,I),I=1,4)/'~1+','~T1','B','H3'/
      DATA ( NAMES(607,I),I=1,4)/'~1+','~b1','A','u'/
      DATA ( NAMES(608,I),I=1,4)/'~1+','~b1','A','c'/
      DATA ( NAMES(609,I),I=1,4)/'~1+','~b1','A','t'/
      DATA ( NAMES(610,I),I=1,4)/'~1+','~b1','Z','u'/
      DATA ( NAMES(611,I),I=1,4)/'~1+','~b1','Z','c'/
      DATA ( NAMES(612,I),I=1,4)/'~1+','~b1','Z','t'/
      DATA ( NAMES(613,I),I=1,4)/'~1+','~b1','W+','d'/
      DATA ( NAMES(614,I),I=1,4)/'~1+','~b1','W+','s'/
      DATA ( NAMES(615,I),I=1,4)/'~1+','~b1','W+','b'/
      DATA ( NAMES(616,I),I=1,4)/'~1+','~b1','G','u'/
      DATA ( NAMES(617,I),I=1,4)/'~1+','~b1','G','c'/
      DATA ( NAMES(618,I),I=1,4)/'~1+','~b1','G','t'/
      DATA ( NAMES(619,I),I=1,4)/'~1+','~b1','u','h'/
      DATA ( NAMES(620,I),I=1,4)/'~1+','~b1','u','H'/
      DATA ( NAMES(621,I),I=1,4)/'~1+','~b1','u','H3'/
      DATA ( NAMES(622,I),I=1,4)/'~1+','~b1','d','H+'/
      DATA ( NAMES(623,I),I=1,4)/'~1+','~b1','c','h'/
      DATA ( NAMES(624,I),I=1,4)/'~1+','~b1','c','H'/
      DATA ( NAMES(625,I),I=1,4)/'~1+','~b1','c','H3'/
      DATA ( NAMES(626,I),I=1,4)/'~1+','~b1','s','H+'/
      DATA ( NAMES(627,I),I=1,4)/'~1+','~b1','t','h'/
      DATA ( NAMES(628,I),I=1,4)/'~1+','~b1','t','H'/
      DATA ( NAMES(629,I),I=1,4)/'~1+','~b1','t','H3'/
      DATA ( NAMES(630,I),I=1,4)/'~1+','~b1','b','H+'/
      DATA ( NAMES(631,I),I=1,4)/'~1+','~B1','W+','D'/
      DATA ( NAMES(632,I),I=1,4)/'~1+','~B1','W+','S'/
      DATA ( NAMES(633,I),I=1,4)/'~1+','~B1','W+','B'/
      DATA ( NAMES(634,I),I=1,4)/'~1+','~B1','D','H+'/
      DATA ( NAMES(635,I),I=1,4)/'~1+','~B1','S','H+'/
      DATA ( NAMES(636,I),I=1,4)/'~1+','~B1','B','H+'/
      DATA ( NAMES(637,I),I=1,4)/'~1+','~g','u','D'/
      DATA ( NAMES(638,I),I=1,4)/'~1+','~g','u','S'/
      DATA ( NAMES(639,I),I=1,4)/'~1+','~g','u','B'/
      DATA ( NAMES(640,I),I=1,4)/'~1+','~g','D','c'/
      DATA ( NAMES(641,I),I=1,4)/'~1+','~g','D','t'/
      DATA ( NAMES(642,I),I=1,4)/'~1+','~g','c','S'/
      DATA ( NAMES(643,I),I=1,4)/'~1+','~g','c','B'/
      DATA ( NAMES(644,I),I=1,4)/'~1+','~g','S','t'/
      DATA ( NAMES(645,I),I=1,4)/'~1+','~g','t','B'/
      DATA ( NAMES(646,I),I=1,4)/'~e1','~e1','e1','e1'/
      DATA ( NAMES(647,I),I=1,4)/'~e1','~E1','A','A'/
      DATA ( NAMES(648,I),I=1,4)/'~e1','~E1','A','Z'/
      DATA ( NAMES(649,I),I=1,4)/'~e1','~E1','A','h'/
      DATA ( NAMES(650,I),I=1,4)/'~e1','~E1','A','H'/
      DATA ( NAMES(651,I),I=1,4)/'~e1','~E1','Z','Z'/
      DATA ( NAMES(652,I),I=1,4)/'~e1','~E1','Z','h'/
      DATA ( NAMES(653,I),I=1,4)/'~e1','~E1','Z','H'/
      DATA ( NAMES(654,I),I=1,4)/'~e1','~E1','Z','H3'/
      DATA ( NAMES(655,I),I=1,4)/'~e1','~E1','W+','W-'/
      DATA ( NAMES(656,I),I=1,4)/'~e1','~E1','W+','H-'/
      DATA ( NAMES(657,I),I=1,4)/'~e1','~E1','W-','H+'/
      DATA ( NAMES(658,I),I=1,4)/'~e1','~E1','n1','N1'/
      DATA ( NAMES(659,I),I=1,4)/'~e1','~E1','n2','N2'/
      DATA ( NAMES(660,I),I=1,4)/'~e1','~E1','n3','N3'/
      DATA ( NAMES(661,I),I=1,4)/'~e1','~E1','e1','E1'/
      DATA ( NAMES(662,I),I=1,4)/'~e1','~E1','e2','E2'/
      DATA ( NAMES(663,I),I=1,4)/'~e1','~E1','e3','E3'/
      DATA ( NAMES(664,I),I=1,4)/'~e1','~E1','u','U'/
      DATA ( NAMES(665,I),I=1,4)/'~e1','~E1','d','D'/
      DATA ( NAMES(666,I),I=1,4)/'~e1','~E1','c','C'/
      DATA ( NAMES(667,I),I=1,4)/'~e1','~E1','s','S'/
      DATA ( NAMES(668,I),I=1,4)/'~e1','~E1','t','T'/
      DATA ( NAMES(669,I),I=1,4)/'~e1','~E1','b','B'/
      DATA ( NAMES(670,I),I=1,4)/'~e1','~E1','h','h'/
      DATA ( NAMES(671,I),I=1,4)/'~e1','~E1','h','H'/
      DATA ( NAMES(672,I),I=1,4)/'~e1','~E1','h','H3'/
      DATA ( NAMES(673,I),I=1,4)/'~e1','~E1','H','H'/
      DATA ( NAMES(674,I),I=1,4)/'~e1','~E1','H','H3'/
      DATA ( NAMES(675,I),I=1,4)/'~e1','~E1','H3','H3'/
      DATA ( NAMES(676,I),I=1,4)/'~e1','~E1','H+','H-'/
      DATA ( NAMES(677,I),I=1,4)/'~e1','~e2','e1','e2'/
      DATA ( NAMES(678,I),I=1,4)/'~e1','~E2','n1','N2'/
      DATA ( NAMES(679,I),I=1,4)/'~e1','~E2','e1','E2'/
      DATA ( NAMES(680,I),I=1,4)/'~e1','~e3','e1','e3'/
      DATA ( NAMES(681,I),I=1,4)/'~e1','~E3','n1','N3'/
      DATA ( NAMES(682,I),I=1,4)/'~e1','~E3','e1','E3'/
      DATA ( NAMES(683,I),I=1,4)/'~e1','~n1','n1','e1'/
      DATA ( NAMES(684,I),I=1,4)/'~e1','~N1','A','W-'/
      DATA ( NAMES(685,I),I=1,4)/'~e1','~N1','A','H-'/
      DATA ( NAMES(686,I),I=1,4)/'~e1','~N1','Z','W-'/
      DATA ( NAMES(687,I),I=1,4)/'~e1','~N1','Z','H-'/
      DATA ( NAMES(688,I),I=1,4)/'~e1','~N1','W-','h'/
      DATA ( NAMES(689,I),I=1,4)/'~e1','~N1','W-','H'/
      DATA ( NAMES(690,I),I=1,4)/'~e1','~N1','W-','H3'/
      DATA ( NAMES(691,I),I=1,4)/'~e1','~N1','N1','e1'/
      DATA ( NAMES(692,I),I=1,4)/'~e1','~N1','N2','e2'/
      DATA ( NAMES(693,I),I=1,4)/'~e1','~N1','N3','e3'/
      DATA ( NAMES(694,I),I=1,4)/'~e1','~N1','U','d'/
      DATA ( NAMES(695,I),I=1,4)/'~e1','~N1','U','s'/
      DATA ( NAMES(696,I),I=1,4)/'~e1','~N1','U','b'/
      DATA ( NAMES(697,I),I=1,4)/'~e1','~N1','d','C'/
      DATA ( NAMES(698,I),I=1,4)/'~e1','~N1','d','T'/
      DATA ( NAMES(699,I),I=1,4)/'~e1','~N1','C','s'/
      DATA ( NAMES(700,I),I=1,4)/'~e1','~N1','C','b'/
      DATA ( NAMES(701,I),I=1,4)/'~e1','~N1','s','T'/
      DATA ( NAMES(702,I),I=1,4)/'~e1','~N1','T','b'/
      DATA ( NAMES(703,I),I=1,4)/'~e1','~N1','h','H-'/
      DATA ( NAMES(704,I),I=1,4)/'~e1','~N1','H','H-'/
      DATA ( NAMES(705,I),I=1,4)/'~e1','~N1','H3','H-'/
      DATA ( NAMES(706,I),I=1,4)/'~e1','~n2','n1','e2'/
      DATA ( NAMES(707,I),I=1,4)/'~e1','~n2','e1','n2'/
      DATA ( NAMES(708,I),I=1,4)/'~e1','~N2','e1','N2'/
      DATA ( NAMES(709,I),I=1,4)/'~e1','~n3','n1','e3'/
      DATA ( NAMES(710,I),I=1,4)/'~e1','~n3','e1','n3'/
      DATA ( NAMES(711,I),I=1,4)/'~e1','~N3','e1','N3'/
      DATA ( NAMES(712,I),I=1,4)/'~e1','~u1','n1','d'/
      DATA ( NAMES(713,I),I=1,4)/'~e1','~u1','n1','s'/
      DATA ( NAMES(714,I),I=1,4)/'~e1','~u1','n1','b'/
      DATA ( NAMES(715,I),I=1,4)/'~e1','~u1','e1','u'/
      DATA ( NAMES(716,I),I=1,4)/'~e1','~U1','e1','U'/
      DATA ( NAMES(717,I),I=1,4)/'~e1','~d1','e1','d'/
      DATA ( NAMES(718,I),I=1,4)/'~e1','~D1','n1','U'/
      DATA ( NAMES(719,I),I=1,4)/'~e1','~D1','n1','C'/
      DATA ( NAMES(720,I),I=1,4)/'~e1','~D1','n1','T'/
      DATA ( NAMES(721,I),I=1,4)/'~e1','~D1','e1','D'/
      DATA ( NAMES(722,I),I=1,4)/'~e1','~c1','n1','d'/
      DATA ( NAMES(723,I),I=1,4)/'~e1','~c1','n1','s'/
      DATA ( NAMES(724,I),I=1,4)/'~e1','~c1','n1','b'/
      DATA ( NAMES(725,I),I=1,4)/'~e1','~c1','e1','c'/
      DATA ( NAMES(726,I),I=1,4)/'~e1','~C1','e1','C'/
      DATA ( NAMES(727,I),I=1,4)/'~e1','~s1','e1','s'/
      DATA ( NAMES(728,I),I=1,4)/'~e1','~S1','n1','U'/
      DATA ( NAMES(729,I),I=1,4)/'~e1','~S1','n1','C'/
      DATA ( NAMES(730,I),I=1,4)/'~e1','~S1','n1','T'/
      DATA ( NAMES(731,I),I=1,4)/'~e1','~S1','e1','S'/
      DATA ( NAMES(732,I),I=1,4)/'~e1','~t1','n1','d'/
      DATA ( NAMES(733,I),I=1,4)/'~e1','~t1','n1','s'/
      DATA ( NAMES(734,I),I=1,4)/'~e1','~t1','n1','b'/
      DATA ( NAMES(735,I),I=1,4)/'~e1','~t1','e1','t'/
      DATA ( NAMES(736,I),I=1,4)/'~e1','~T1','e1','T'/
      DATA ( NAMES(737,I),I=1,4)/'~e1','~b1','e1','b'/
      DATA ( NAMES(738,I),I=1,4)/'~e1','~B1','n1','U'/
      DATA ( NAMES(739,I),I=1,4)/'~e1','~B1','n1','C'/
      DATA ( NAMES(740,I),I=1,4)/'~e1','~B1','n1','T'/
      DATA ( NAMES(741,I),I=1,4)/'~e1','~B1','e1','B'/
      DATA ( NAMES(742,I),I=1,4)/'~e2','~e2','e2','e2'/
      DATA ( NAMES(743,I),I=1,4)/'~e2','~E2','A','A'/
      DATA ( NAMES(744,I),I=1,4)/'~e2','~E2','A','Z'/
      DATA ( NAMES(745,I),I=1,4)/'~e2','~E2','A','h'/
      DATA ( NAMES(746,I),I=1,4)/'~e2','~E2','A','H'/
      DATA ( NAMES(747,I),I=1,4)/'~e2','~E2','Z','Z'/
      DATA ( NAMES(748,I),I=1,4)/'~e2','~E2','Z','h'/
      DATA ( NAMES(749,I),I=1,4)/'~e2','~E2','Z','H'/
      DATA ( NAMES(750,I),I=1,4)/'~e2','~E2','Z','H3'/
      DATA ( NAMES(751,I),I=1,4)/'~e2','~E2','W+','W-'/
      DATA ( NAMES(752,I),I=1,4)/'~e2','~E2','W+','H-'/
      DATA ( NAMES(753,I),I=1,4)/'~e2','~E2','W-','H+'/
      DATA ( NAMES(754,I),I=1,4)/'~e2','~E2','n1','N1'/
      DATA ( NAMES(755,I),I=1,4)/'~e2','~E2','n2','N2'/
      DATA ( NAMES(756,I),I=1,4)/'~e2','~E2','n3','N3'/
      DATA ( NAMES(757,I),I=1,4)/'~e2','~E2','e1','E1'/
      DATA ( NAMES(758,I),I=1,4)/'~e2','~E2','e2','E2'/
      DATA ( NAMES(759,I),I=1,4)/'~e2','~E2','e3','E3'/
      DATA ( NAMES(760,I),I=1,4)/'~e2','~E2','u','U'/
      DATA ( NAMES(761,I),I=1,4)/'~e2','~E2','d','D'/
      DATA ( NAMES(762,I),I=1,4)/'~e2','~E2','c','C'/
      DATA ( NAMES(763,I),I=1,4)/'~e2','~E2','s','S'/
      DATA ( NAMES(764,I),I=1,4)/'~e2','~E2','t','T'/
      DATA ( NAMES(765,I),I=1,4)/'~e2','~E2','b','B'/
      DATA ( NAMES(766,I),I=1,4)/'~e2','~E2','h','h'/
      DATA ( NAMES(767,I),I=1,4)/'~e2','~E2','h','H'/
      DATA ( NAMES(768,I),I=1,4)/'~e2','~E2','h','H3'/
      DATA ( NAMES(769,I),I=1,4)/'~e2','~E2','H','H'/
      DATA ( NAMES(770,I),I=1,4)/'~e2','~E2','H','H3'/
      DATA ( NAMES(771,I),I=1,4)/'~e2','~E2','H3','H3'/
      DATA ( NAMES(772,I),I=1,4)/'~e2','~E2','H+','H-'/
      DATA ( NAMES(773,I),I=1,4)/'~e2','~e3','e2','e3'/
      DATA ( NAMES(774,I),I=1,4)/'~e2','~E3','n2','N3'/
      DATA ( NAMES(775,I),I=1,4)/'~e2','~E3','e2','E3'/
      DATA ( NAMES(776,I),I=1,4)/'~e2','~n1','n1','e2'/
      DATA ( NAMES(777,I),I=1,4)/'~e2','~n1','e1','n2'/
      DATA ( NAMES(778,I),I=1,4)/'~e2','~N1','N1','e2'/
      DATA ( NAMES(779,I),I=1,4)/'~e2','~n2','n2','e2'/
      DATA ( NAMES(780,I),I=1,4)/'~e2','~N2','A','W-'/
      DATA ( NAMES(781,I),I=1,4)/'~e2','~N2','A','H-'/
      DATA ( NAMES(782,I),I=1,4)/'~e2','~N2','Z','W-'/
      DATA ( NAMES(783,I),I=1,4)/'~e2','~N2','Z','H-'/
      DATA ( NAMES(784,I),I=1,4)/'~e2','~N2','W-','h'/
      DATA ( NAMES(785,I),I=1,4)/'~e2','~N2','W-','H'/
      DATA ( NAMES(786,I),I=1,4)/'~e2','~N2','W-','H3'/
      DATA ( NAMES(787,I),I=1,4)/'~e2','~N2','N1','e1'/
      DATA ( NAMES(788,I),I=1,4)/'~e2','~N2','N2','e2'/
      DATA ( NAMES(789,I),I=1,4)/'~e2','~N2','N3','e3'/
      DATA ( NAMES(790,I),I=1,4)/'~e2','~N2','U','d'/
      DATA ( NAMES(791,I),I=1,4)/'~e2','~N2','U','s'/
      DATA ( NAMES(792,I),I=1,4)/'~e2','~N2','U','b'/
      DATA ( NAMES(793,I),I=1,4)/'~e2','~N2','d','C'/
      DATA ( NAMES(794,I),I=1,4)/'~e2','~N2','d','T'/
      DATA ( NAMES(795,I),I=1,4)/'~e2','~N2','C','s'/
      DATA ( NAMES(796,I),I=1,4)/'~e2','~N2','C','b'/
      DATA ( NAMES(797,I),I=1,4)/'~e2','~N2','s','T'/
      DATA ( NAMES(798,I),I=1,4)/'~e2','~N2','T','b'/
      DATA ( NAMES(799,I),I=1,4)/'~e2','~N2','h','H-'/
      DATA ( NAMES(800,I),I=1,4)/'~e2','~N2','H','H-'/
      DATA ( NAMES(801,I),I=1,4)/'~e2','~N2','H3','H-'/
      DATA ( NAMES(802,I),I=1,4)/'~e2','~n3','n2','e3'/
      DATA ( NAMES(803,I),I=1,4)/'~e2','~n3','e2','n3'/
      DATA ( NAMES(804,I),I=1,4)/'~e2','~N3','e2','N3'/
      DATA ( NAMES(805,I),I=1,4)/'~e2','~u1','n2','d'/
      DATA ( NAMES(806,I),I=1,4)/'~e2','~u1','n2','s'/
      DATA ( NAMES(807,I),I=1,4)/'~e2','~u1','n2','b'/
      DATA ( NAMES(808,I),I=1,4)/'~e2','~u1','e2','u'/
      DATA ( NAMES(809,I),I=1,4)/'~e2','~U1','e2','U'/
      DATA ( NAMES(810,I),I=1,4)/'~e2','~d1','e2','d'/
      DATA ( NAMES(811,I),I=1,4)/'~e2','~D1','n2','U'/
      DATA ( NAMES(812,I),I=1,4)/'~e2','~D1','n2','C'/
      DATA ( NAMES(813,I),I=1,4)/'~e2','~D1','n2','T'/
      DATA ( NAMES(814,I),I=1,4)/'~e2','~D1','e2','D'/
      DATA ( NAMES(815,I),I=1,4)/'~e2','~c1','n2','d'/
      DATA ( NAMES(816,I),I=1,4)/'~e2','~c1','n2','s'/
      DATA ( NAMES(817,I),I=1,4)/'~e2','~c1','n2','b'/
      DATA ( NAMES(818,I),I=1,4)/'~e2','~c1','e2','c'/
      DATA ( NAMES(819,I),I=1,4)/'~e2','~C1','e2','C'/
      DATA ( NAMES(820,I),I=1,4)/'~e2','~s1','e2','s'/
      DATA ( NAMES(821,I),I=1,4)/'~e2','~S1','n2','U'/
      DATA ( NAMES(822,I),I=1,4)/'~e2','~S1','n2','C'/
      DATA ( NAMES(823,I),I=1,4)/'~e2','~S1','n2','T'/
      DATA ( NAMES(824,I),I=1,4)/'~e2','~S1','e2','S'/
      DATA ( NAMES(825,I),I=1,4)/'~e2','~t1','n2','d'/
      DATA ( NAMES(826,I),I=1,4)/'~e2','~t1','n2','s'/
      DATA ( NAMES(827,I),I=1,4)/'~e2','~t1','n2','b'/
      DATA ( NAMES(828,I),I=1,4)/'~e2','~t1','e2','t'/
      DATA ( NAMES(829,I),I=1,4)/'~e2','~T1','e2','T'/
      DATA ( NAMES(830,I),I=1,4)/'~e2','~b1','e2','b'/
      DATA ( NAMES(831,I),I=1,4)/'~e2','~B1','n2','U'/
      DATA ( NAMES(832,I),I=1,4)/'~e2','~B1','n2','C'/
      DATA ( NAMES(833,I),I=1,4)/'~e2','~B1','n2','T'/
      DATA ( NAMES(834,I),I=1,4)/'~e2','~B1','e2','B'/
      DATA ( NAMES(835,I),I=1,4)/'~e3','~e3','e3','e3'/
      DATA ( NAMES(836,I),I=1,4)/'~e3','~E3','A','A'/
      DATA ( NAMES(837,I),I=1,4)/'~e3','~E3','A','Z'/
      DATA ( NAMES(838,I),I=1,4)/'~e3','~E3','A','h'/
      DATA ( NAMES(839,I),I=1,4)/'~e3','~E3','A','H'/
      DATA ( NAMES(840,I),I=1,4)/'~e3','~E3','Z','Z'/
      DATA ( NAMES(841,I),I=1,4)/'~e3','~E3','Z','h'/
      DATA ( NAMES(842,I),I=1,4)/'~e3','~E3','Z','H'/
      DATA ( NAMES(843,I),I=1,4)/'~e3','~E3','Z','H3'/
      DATA ( NAMES(844,I),I=1,4)/'~e3','~E3','W+','W-'/
      DATA ( NAMES(845,I),I=1,4)/'~e3','~E3','W+','H-'/
      DATA ( NAMES(846,I),I=1,4)/'~e3','~E3','W-','H+'/
      DATA ( NAMES(847,I),I=1,4)/'~e3','~E3','n1','N1'/
      DATA ( NAMES(848,I),I=1,4)/'~e3','~E3','n2','N2'/
      DATA ( NAMES(849,I),I=1,4)/'~e3','~E3','n3','N3'/
      DATA ( NAMES(850,I),I=1,4)/'~e3','~E3','e1','E1'/
      DATA ( NAMES(851,I),I=1,4)/'~e3','~E3','e2','E2'/
      DATA ( NAMES(852,I),I=1,4)/'~e3','~E3','e3','E3'/
      DATA ( NAMES(853,I),I=1,4)/'~e3','~E3','u','U'/
      DATA ( NAMES(854,I),I=1,4)/'~e3','~E3','d','D'/
      DATA ( NAMES(855,I),I=1,4)/'~e3','~E3','c','C'/
      DATA ( NAMES(856,I),I=1,4)/'~e3','~E3','s','S'/
      DATA ( NAMES(857,I),I=1,4)/'~e3','~E3','t','T'/
      DATA ( NAMES(858,I),I=1,4)/'~e3','~E3','b','B'/
      DATA ( NAMES(859,I),I=1,4)/'~e3','~E3','h','h'/
      DATA ( NAMES(860,I),I=1,4)/'~e3','~E3','h','H'/
      DATA ( NAMES(861,I),I=1,4)/'~e3','~E3','h','H3'/
      DATA ( NAMES(862,I),I=1,4)/'~e3','~E3','H','H'/
      DATA ( NAMES(863,I),I=1,4)/'~e3','~E3','H','H3'/
      DATA ( NAMES(864,I),I=1,4)/'~e3','~E3','H3','H3'/
      DATA ( NAMES(865,I),I=1,4)/'~e3','~E3','H+','H-'/
      DATA ( NAMES(866,I),I=1,4)/'~e3','~n1','n1','e3'/
      DATA ( NAMES(867,I),I=1,4)/'~e3','~n1','e1','n3'/
      DATA ( NAMES(868,I),I=1,4)/'~e3','~N1','N1','e3'/
      DATA ( NAMES(869,I),I=1,4)/'~e3','~n2','n2','e3'/
      DATA ( NAMES(870,I),I=1,4)/'~e3','~n2','e2','n3'/
      DATA ( NAMES(871,I),I=1,4)/'~e3','~N2','N2','e3'/
      DATA ( NAMES(872,I),I=1,4)/'~e3','~n3','n3','e3'/
      DATA ( NAMES(873,I),I=1,4)/'~e3','~N3','A','W-'/
      DATA ( NAMES(874,I),I=1,4)/'~e3','~N3','A','H-'/
      DATA ( NAMES(875,I),I=1,4)/'~e3','~N3','Z','W-'/
      DATA ( NAMES(876,I),I=1,4)/'~e3','~N3','Z','H-'/
      DATA ( NAMES(877,I),I=1,4)/'~e3','~N3','W-','h'/
      DATA ( NAMES(878,I),I=1,4)/'~e3','~N3','W-','H'/
      DATA ( NAMES(879,I),I=1,4)/'~e3','~N3','W-','H3'/
      DATA ( NAMES(880,I),I=1,4)/'~e3','~N3','N1','e1'/
      DATA ( NAMES(881,I),I=1,4)/'~e3','~N3','N2','e2'/
      DATA ( NAMES(882,I),I=1,4)/'~e3','~N3','N3','e3'/
      DATA ( NAMES(883,I),I=1,4)/'~e3','~N3','U','d'/
      DATA ( NAMES(884,I),I=1,4)/'~e3','~N3','U','s'/
      DATA ( NAMES(885,I),I=1,4)/'~e3','~N3','U','b'/
      DATA ( NAMES(886,I),I=1,4)/'~e3','~N3','d','C'/
      DATA ( NAMES(887,I),I=1,4)/'~e3','~N3','d','T'/
      DATA ( NAMES(888,I),I=1,4)/'~e3','~N3','C','s'/
      DATA ( NAMES(889,I),I=1,4)/'~e3','~N3','C','b'/
      DATA ( NAMES(890,I),I=1,4)/'~e3','~N3','s','T'/
      DATA ( NAMES(891,I),I=1,4)/'~e3','~N3','T','b'/
      DATA ( NAMES(892,I),I=1,4)/'~e3','~N3','h','H-'/
      DATA ( NAMES(893,I),I=1,4)/'~e3','~N3','H','H-'/
      DATA ( NAMES(894,I),I=1,4)/'~e3','~N3','H3','H-'/
      DATA ( NAMES(895,I),I=1,4)/'~e3','~u1','n3','d'/
      DATA ( NAMES(896,I),I=1,4)/'~e3','~u1','n3','s'/
      DATA ( NAMES(897,I),I=1,4)/'~e3','~u1','n3','b'/
      DATA ( NAMES(898,I),I=1,4)/'~e3','~u1','e3','u'/
      DATA ( NAMES(899,I),I=1,4)/'~e3','~U1','e3','U'/
      DATA ( NAMES(900,I),I=1,4)/'~e3','~d1','e3','d'/
      DATA ( NAMES(901,I),I=1,4)/'~e3','~D1','n3','U'/
      DATA ( NAMES(902,I),I=1,4)/'~e3','~D1','n3','C'/
      DATA ( NAMES(903,I),I=1,4)/'~e3','~D1','n3','T'/
      DATA ( NAMES(904,I),I=1,4)/'~e3','~D1','e3','D'/
      DATA ( NAMES(905,I),I=1,4)/'~e3','~c1','n3','d'/
      DATA ( NAMES(906,I),I=1,4)/'~e3','~c1','n3','s'/
      DATA ( NAMES(907,I),I=1,4)/'~e3','~c1','n3','b'/
      DATA ( NAMES(908,I),I=1,4)/'~e3','~c1','e3','c'/
      DATA ( NAMES(909,I),I=1,4)/'~e3','~C1','e3','C'/
      DATA ( NAMES(910,I),I=1,4)/'~e3','~s1','e3','s'/
      DATA ( NAMES(911,I),I=1,4)/'~e3','~S1','n3','U'/
      DATA ( NAMES(912,I),I=1,4)/'~e3','~S1','n3','C'/
      DATA ( NAMES(913,I),I=1,4)/'~e3','~S1','n3','T'/
      DATA ( NAMES(914,I),I=1,4)/'~e3','~S1','e3','S'/
      DATA ( NAMES(915,I),I=1,4)/'~e3','~t1','n3','d'/
      DATA ( NAMES(916,I),I=1,4)/'~e3','~t1','n3','s'/
      DATA ( NAMES(917,I),I=1,4)/'~e3','~t1','n3','b'/
      DATA ( NAMES(918,I),I=1,4)/'~e3','~t1','e3','t'/
      DATA ( NAMES(919,I),I=1,4)/'~e3','~T1','e3','T'/
      DATA ( NAMES(920,I),I=1,4)/'~e3','~b1','e3','b'/
      DATA ( NAMES(921,I),I=1,4)/'~e3','~B1','n3','U'/
      DATA ( NAMES(922,I),I=1,4)/'~e3','~B1','n3','C'/
      DATA ( NAMES(923,I),I=1,4)/'~e3','~B1','n3','T'/
      DATA ( NAMES(924,I),I=1,4)/'~e3','~B1','e3','B'/
      DATA ( NAMES(925,I),I=1,4)/'~n1','~n1','n1','n1'/
      DATA ( NAMES(926,I),I=1,4)/'~n1','~N1','Z','Z'/
      DATA ( NAMES(927,I),I=1,4)/'~n1','~N1','Z','h'/
      DATA ( NAMES(928,I),I=1,4)/'~n1','~N1','Z','H'/
      DATA ( NAMES(929,I),I=1,4)/'~n1','~N1','Z','H3'/
      DATA ( NAMES(930,I),I=1,4)/'~n1','~N1','W+','W-'/
      DATA ( NAMES(931,I),I=1,4)/'~n1','~N1','W+','H-'/
      DATA ( NAMES(932,I),I=1,4)/'~n1','~N1','W-','H+'/
      DATA ( NAMES(933,I),I=1,4)/'~n1','~N1','n1','N1'/
      DATA ( NAMES(934,I),I=1,4)/'~n1','~N1','n2','N2'/
      DATA ( NAMES(935,I),I=1,4)/'~n1','~N1','n3','N3'/
      DATA ( NAMES(936,I),I=1,4)/'~n1','~N1','e1','E1'/
      DATA ( NAMES(937,I),I=1,4)/'~n1','~N1','e2','E2'/
      DATA ( NAMES(938,I),I=1,4)/'~n1','~N1','e3','E3'/
      DATA ( NAMES(939,I),I=1,4)/'~n1','~N1','u','U'/
      DATA ( NAMES(940,I),I=1,4)/'~n1','~N1','d','D'/
      DATA ( NAMES(941,I),I=1,4)/'~n1','~N1','c','C'/
      DATA ( NAMES(942,I),I=1,4)/'~n1','~N1','s','S'/
      DATA ( NAMES(943,I),I=1,4)/'~n1','~N1','t','T'/
      DATA ( NAMES(944,I),I=1,4)/'~n1','~N1','b','B'/
      DATA ( NAMES(945,I),I=1,4)/'~n1','~N1','h','h'/
      DATA ( NAMES(946,I),I=1,4)/'~n1','~N1','h','H'/
      DATA ( NAMES(947,I),I=1,4)/'~n1','~N1','h','H3'/
      DATA ( NAMES(948,I),I=1,4)/'~n1','~N1','H','H'/
      DATA ( NAMES(949,I),I=1,4)/'~n1','~N1','H','H3'/
      DATA ( NAMES(950,I),I=1,4)/'~n1','~N1','H3','H3'/
      DATA ( NAMES(951,I),I=1,4)/'~n1','~N1','H+','H-'/
      DATA ( NAMES(952,I),I=1,4)/'~n1','~n2','n1','n2'/
      DATA ( NAMES(953,I),I=1,4)/'~n1','~N2','n1','N2'/
      DATA ( NAMES(954,I),I=1,4)/'~n1','~N2','e1','E2'/
      DATA ( NAMES(955,I),I=1,4)/'~n1','~n3','n1','n3'/
      DATA ( NAMES(956,I),I=1,4)/'~n1','~N3','n1','N3'/
      DATA ( NAMES(957,I),I=1,4)/'~n1','~N3','e1','E3'/
      DATA ( NAMES(958,I),I=1,4)/'~n1','~u1','n1','u'/
      DATA ( NAMES(959,I),I=1,4)/'~n1','~U1','n1','U'/
      DATA ( NAMES(960,I),I=1,4)/'~n1','~U1','e1','D'/
      DATA ( NAMES(961,I),I=1,4)/'~n1','~U1','e1','S'/
      DATA ( NAMES(962,I),I=1,4)/'~n1','~U1','e1','B'/
      DATA ( NAMES(963,I),I=1,4)/'~n1','~d1','n1','d'/
      DATA ( NAMES(964,I),I=1,4)/'~n1','~d1','e1','u'/
      DATA ( NAMES(965,I),I=1,4)/'~n1','~d1','e1','c'/
      DATA ( NAMES(966,I),I=1,4)/'~n1','~d1','e1','t'/
      DATA ( NAMES(967,I),I=1,4)/'~n1','~D1','n1','D'/
      DATA ( NAMES(968,I),I=1,4)/'~n1','~c1','n1','c'/
      DATA ( NAMES(969,I),I=1,4)/'~n1','~C1','n1','C'/
      DATA ( NAMES(970,I),I=1,4)/'~n1','~C1','e1','D'/
      DATA ( NAMES(971,I),I=1,4)/'~n1','~C1','e1','S'/
      DATA ( NAMES(972,I),I=1,4)/'~n1','~C1','e1','B'/
      DATA ( NAMES(973,I),I=1,4)/'~n1','~s1','n1','s'/
      DATA ( NAMES(974,I),I=1,4)/'~n1','~s1','e1','u'/
      DATA ( NAMES(975,I),I=1,4)/'~n1','~s1','e1','c'/
      DATA ( NAMES(976,I),I=1,4)/'~n1','~s1','e1','t'/
      DATA ( NAMES(977,I),I=1,4)/'~n1','~S1','n1','S'/
      DATA ( NAMES(978,I),I=1,4)/'~n1','~t1','n1','t'/
      DATA ( NAMES(979,I),I=1,4)/'~n1','~T1','n1','T'/
      DATA ( NAMES(980,I),I=1,4)/'~n1','~T1','e1','D'/
      DATA ( NAMES(981,I),I=1,4)/'~n1','~T1','e1','S'/
      DATA ( NAMES(982,I),I=1,4)/'~n1','~T1','e1','B'/
      DATA ( NAMES(983,I),I=1,4)/'~n1','~b1','n1','b'/
      DATA ( NAMES(984,I),I=1,4)/'~n1','~b1','e1','u'/
      DATA ( NAMES(985,I),I=1,4)/'~n1','~b1','e1','c'/
      DATA ( NAMES(986,I),I=1,4)/'~n1','~b1','e1','t'/
      DATA ( NAMES(987,I),I=1,4)/'~n1','~B1','n1','B'/
      DATA ( NAMES(988,I),I=1,4)/'~n2','~n2','n2','n2'/
      DATA ( NAMES(989,I),I=1,4)/'~n2','~N2','Z','Z'/
      DATA ( NAMES(990,I),I=1,4)/'~n2','~N2','Z','h'/
      DATA ( NAMES(991,I),I=1,4)/'~n2','~N2','Z','H'/
      DATA ( NAMES(992,I),I=1,4)/'~n2','~N2','Z','H3'/
      DATA ( NAMES(993,I),I=1,4)/'~n2','~N2','W+','W-'/
      DATA ( NAMES(994,I),I=1,4)/'~n2','~N2','W+','H-'/
      DATA ( NAMES(995,I),I=1,4)/'~n2','~N2','W-','H+'/
      DATA ( NAMES(996,I),I=1,4)/'~n2','~N2','n1','N1'/
      DATA ( NAMES(997,I),I=1,4)/'~n2','~N2','n2','N2'/
      DATA ( NAMES(998,I),I=1,4)/'~n2','~N2','n3','N3'/
      DATA ( NAMES(999,I),I=1,4)/'~n2','~N2','e1','E1'/
      DATA ( NAMES(1000,I),I=1,4)/'~n2','~N2','e2','E2'/
      DATA ( NAMES(1001,I),I=1,4)/'~n2','~N2','e3','E3'/
      DATA ( NAMES(1002,I),I=1,4)/'~n2','~N2','u','U'/
      DATA ( NAMES(1003,I),I=1,4)/'~n2','~N2','d','D'/
      DATA ( NAMES(1004,I),I=1,4)/'~n2','~N2','c','C'/
      DATA ( NAMES(1005,I),I=1,4)/'~n2','~N2','s','S'/
      DATA ( NAMES(1006,I),I=1,4)/'~n2','~N2','t','T'/
      DATA ( NAMES(1007,I),I=1,4)/'~n2','~N2','b','B'/
      DATA ( NAMES(1008,I),I=1,4)/'~n2','~N2','h','h'/
      DATA ( NAMES(1009,I),I=1,4)/'~n2','~N2','h','H'/
      DATA ( NAMES(1010,I),I=1,4)/'~n2','~N2','h','H3'/
      DATA ( NAMES(1011,I),I=1,4)/'~n2','~N2','H','H'/
      DATA ( NAMES(1012,I),I=1,4)/'~n2','~N2','H','H3'/
      DATA ( NAMES(1013,I),I=1,4)/'~n2','~N2','H3','H3'/
      DATA ( NAMES(1014,I),I=1,4)/'~n2','~N2','H+','H-'/
      DATA ( NAMES(1015,I),I=1,4)/'~n2','~n3','n2','n3'/
      DATA ( NAMES(1016,I),I=1,4)/'~n2','~N3','n2','N3'/
      DATA ( NAMES(1017,I),I=1,4)/'~n2','~N3','e2','E3'/
      DATA ( NAMES(1018,I),I=1,4)/'~n2','~u1','n2','u'/
      DATA ( NAMES(1019,I),I=1,4)/'~n2','~U1','n2','U'/
      DATA ( NAMES(1020,I),I=1,4)/'~n2','~U1','e2','D'/
      DATA ( NAMES(1021,I),I=1,4)/'~n2','~U1','e2','S'/
      DATA ( NAMES(1022,I),I=1,4)/'~n2','~U1','e2','B'/
      DATA ( NAMES(1023,I),I=1,4)/'~n2','~d1','n2','d'/
      DATA ( NAMES(1024,I),I=1,4)/'~n2','~d1','e2','u'/
      DATA ( NAMES(1025,I),I=1,4)/'~n2','~d1','e2','c'/
      DATA ( NAMES(1026,I),I=1,4)/'~n2','~d1','e2','t'/
      DATA ( NAMES(1027,I),I=1,4)/'~n2','~D1','n2','D'/
      DATA ( NAMES(1028,I),I=1,4)/'~n2','~c1','n2','c'/
      DATA ( NAMES(1029,I),I=1,4)/'~n2','~C1','n2','C'/
      DATA ( NAMES(1030,I),I=1,4)/'~n2','~C1','e2','D'/
      DATA ( NAMES(1031,I),I=1,4)/'~n2','~C1','e2','S'/
      DATA ( NAMES(1032,I),I=1,4)/'~n2','~C1','e2','B'/
      DATA ( NAMES(1033,I),I=1,4)/'~n2','~s1','n2','s'/
      DATA ( NAMES(1034,I),I=1,4)/'~n2','~s1','e2','u'/
      DATA ( NAMES(1035,I),I=1,4)/'~n2','~s1','e2','c'/
      DATA ( NAMES(1036,I),I=1,4)/'~n2','~s1','e2','t'/
      DATA ( NAMES(1037,I),I=1,4)/'~n2','~S1','n2','S'/
      DATA ( NAMES(1038,I),I=1,4)/'~n2','~t1','n2','t'/
      DATA ( NAMES(1039,I),I=1,4)/'~n2','~T1','n2','T'/
      DATA ( NAMES(1040,I),I=1,4)/'~n2','~T1','e2','D'/
      DATA ( NAMES(1041,I),I=1,4)/'~n2','~T1','e2','S'/
      DATA ( NAMES(1042,I),I=1,4)/'~n2','~T1','e2','B'/
      DATA ( NAMES(1043,I),I=1,4)/'~n2','~b1','n2','b'/
      DATA ( NAMES(1044,I),I=1,4)/'~n2','~b1','e2','u'/
      DATA ( NAMES(1045,I),I=1,4)/'~n2','~b1','e2','c'/
      DATA ( NAMES(1046,I),I=1,4)/'~n2','~b1','e2','t'/
      DATA ( NAMES(1047,I),I=1,4)/'~n2','~B1','n2','B'/
      DATA ( NAMES(1048,I),I=1,4)/'~n3','~n3','n3','n3'/
      DATA ( NAMES(1049,I),I=1,4)/'~n3','~N3','Z','Z'/
      DATA ( NAMES(1050,I),I=1,4)/'~n3','~N3','Z','h'/
      DATA ( NAMES(1051,I),I=1,4)/'~n3','~N3','Z','H'/
      DATA ( NAMES(1052,I),I=1,4)/'~n3','~N3','Z','H3'/
      DATA ( NAMES(1053,I),I=1,4)/'~n3','~N3','W+','W-'/
      DATA ( NAMES(1054,I),I=1,4)/'~n3','~N3','W+','H-'/
      DATA ( NAMES(1055,I),I=1,4)/'~n3','~N3','W-','H+'/
      DATA ( NAMES(1056,I),I=1,4)/'~n3','~N3','n1','N1'/
      DATA ( NAMES(1057,I),I=1,4)/'~n3','~N3','n2','N2'/
      DATA ( NAMES(1058,I),I=1,4)/'~n3','~N3','n3','N3'/
      DATA ( NAMES(1059,I),I=1,4)/'~n3','~N3','e1','E1'/
      DATA ( NAMES(1060,I),I=1,4)/'~n3','~N3','e2','E2'/
      DATA ( NAMES(1061,I),I=1,4)/'~n3','~N3','e3','E3'/
      DATA ( NAMES(1062,I),I=1,4)/'~n3','~N3','u','U'/
      DATA ( NAMES(1063,I),I=1,4)/'~n3','~N3','d','D'/
      DATA ( NAMES(1064,I),I=1,4)/'~n3','~N3','c','C'/
      DATA ( NAMES(1065,I),I=1,4)/'~n3','~N3','s','S'/
      DATA ( NAMES(1066,I),I=1,4)/'~n3','~N3','t','T'/
      DATA ( NAMES(1067,I),I=1,4)/'~n3','~N3','b','B'/
      DATA ( NAMES(1068,I),I=1,4)/'~n3','~N3','h','h'/
      DATA ( NAMES(1069,I),I=1,4)/'~n3','~N3','h','H'/
      DATA ( NAMES(1070,I),I=1,4)/'~n3','~N3','h','H3'/
      DATA ( NAMES(1071,I),I=1,4)/'~n3','~N3','H','H'/
      DATA ( NAMES(1072,I),I=1,4)/'~n3','~N3','H','H3'/
      DATA ( NAMES(1073,I),I=1,4)/'~n3','~N3','H3','H3'/
      DATA ( NAMES(1074,I),I=1,4)/'~n3','~N3','H+','H-'/
      DATA ( NAMES(1075,I),I=1,4)/'~n3','~u1','n3','u'/
      DATA ( NAMES(1076,I),I=1,4)/'~n3','~U1','n3','U'/
      DATA ( NAMES(1077,I),I=1,4)/'~n3','~U1','e3','D'/
      DATA ( NAMES(1078,I),I=1,4)/'~n3','~U1','e3','S'/
      DATA ( NAMES(1079,I),I=1,4)/'~n3','~U1','e3','B'/
      DATA ( NAMES(1080,I),I=1,4)/'~n3','~d1','n3','d'/
      DATA ( NAMES(1081,I),I=1,4)/'~n3','~d1','e3','u'/
      DATA ( NAMES(1082,I),I=1,4)/'~n3','~d1','e3','c'/
      DATA ( NAMES(1083,I),I=1,4)/'~n3','~d1','e3','t'/
      DATA ( NAMES(1084,I),I=1,4)/'~n3','~D1','n3','D'/
      DATA ( NAMES(1085,I),I=1,4)/'~n3','~c1','n3','c'/
      DATA ( NAMES(1086,I),I=1,4)/'~n3','~C1','n3','C'/
      DATA ( NAMES(1087,I),I=1,4)/'~n3','~C1','e3','D'/
      DATA ( NAMES(1088,I),I=1,4)/'~n3','~C1','e3','S'/
      DATA ( NAMES(1089,I),I=1,4)/'~n3','~C1','e3','B'/
      DATA ( NAMES(1090,I),I=1,4)/'~n3','~s1','n3','s'/
      DATA ( NAMES(1091,I),I=1,4)/'~n3','~s1','e3','u'/
      DATA ( NAMES(1092,I),I=1,4)/'~n3','~s1','e3','c'/
      DATA ( NAMES(1093,I),I=1,4)/'~n3','~s1','e3','t'/
      DATA ( NAMES(1094,I),I=1,4)/'~n3','~S1','n3','S'/
      DATA ( NAMES(1095,I),I=1,4)/'~n3','~t1','n3','t'/
      DATA ( NAMES(1096,I),I=1,4)/'~n3','~T1','n3','T'/
      DATA ( NAMES(1097,I),I=1,4)/'~n3','~T1','e3','D'/
      DATA ( NAMES(1098,I),I=1,4)/'~n3','~T1','e3','S'/
      DATA ( NAMES(1099,I),I=1,4)/'~n3','~T1','e3','B'/
      DATA ( NAMES(1100,I),I=1,4)/'~n3','~b1','n3','b'/
      DATA ( NAMES(1101,I),I=1,4)/'~n3','~b1','e3','u'/
      DATA ( NAMES(1102,I),I=1,4)/'~n3','~b1','e3','c'/
      DATA ( NAMES(1103,I),I=1,4)/'~n3','~b1','e3','t'/
      DATA ( NAMES(1104,I),I=1,4)/'~n3','~B1','n3','B'/
      DATA ( NAMES(1105,I),I=1,4)/'~u1','~u1','u','u'/
      DATA ( NAMES(1106,I),I=1,4)/'~u1','~U1','A','A'/
      DATA ( NAMES(1107,I),I=1,4)/'~u1','~U1','A','Z'/
      DATA ( NAMES(1108,I),I=1,4)/'~u1','~U1','A','G'/
      DATA ( NAMES(1109,I),I=1,4)/'~u1','~U1','A','h'/
      DATA ( NAMES(1110,I),I=1,4)/'~u1','~U1','A','H'/
      DATA ( NAMES(1111,I),I=1,4)/'~u1','~U1','Z','Z'/
      DATA ( NAMES(1112,I),I=1,4)/'~u1','~U1','Z','G'/
      DATA ( NAMES(1113,I),I=1,4)/'~u1','~U1','Z','h'/
      DATA ( NAMES(1114,I),I=1,4)/'~u1','~U1','Z','H'/
      DATA ( NAMES(1115,I),I=1,4)/'~u1','~U1','Z','H3'/
      DATA ( NAMES(1116,I),I=1,4)/'~u1','~U1','W+','W-'/
      DATA ( NAMES(1117,I),I=1,4)/'~u1','~U1','W+','H-'/
      DATA ( NAMES(1118,I),I=1,4)/'~u1','~U1','W-','H+'/
      DATA ( NAMES(1119,I),I=1,4)/'~u1','~U1','G','G'/
      DATA ( NAMES(1120,I),I=1,4)/'~u1','~U1','G','h'/
      DATA ( NAMES(1121,I),I=1,4)/'~u1','~U1','G','H'/
      DATA ( NAMES(1122,I),I=1,4)/'~u1','~U1','n1','N1'/
      DATA ( NAMES(1123,I),I=1,4)/'~u1','~U1','n2','N2'/
      DATA ( NAMES(1124,I),I=1,4)/'~u1','~U1','n3','N3'/
      DATA ( NAMES(1125,I),I=1,4)/'~u1','~U1','e1','E1'/
      DATA ( NAMES(1126,I),I=1,4)/'~u1','~U1','e2','E2'/
      DATA ( NAMES(1127,I),I=1,4)/'~u1','~U1','e3','E3'/
      DATA ( NAMES(1128,I),I=1,4)/'~u1','~U1','u','U'/
      DATA ( NAMES(1129,I),I=1,4)/'~u1','~U1','d','D'/
      DATA ( NAMES(1130,I),I=1,4)/'~u1','~U1','d','S'/
      DATA ( NAMES(1131,I),I=1,4)/'~u1','~U1','d','B'/
      DATA ( NAMES(1132,I),I=1,4)/'~u1','~U1','D','s'/
      DATA ( NAMES(1133,I),I=1,4)/'~u1','~U1','D','b'/
      DATA ( NAMES(1134,I),I=1,4)/'~u1','~U1','c','C'/
      DATA ( NAMES(1135,I),I=1,4)/'~u1','~U1','s','S'/
      DATA ( NAMES(1136,I),I=1,4)/'~u1','~U1','s','B'/
      DATA ( NAMES(1137,I),I=1,4)/'~u1','~U1','S','b'/
      DATA ( NAMES(1138,I),I=1,4)/'~u1','~U1','t','T'/
      DATA ( NAMES(1139,I),I=1,4)/'~u1','~U1','b','B'/
      DATA ( NAMES(1140,I),I=1,4)/'~u1','~U1','h','h'/
      DATA ( NAMES(1141,I),I=1,4)/'~u1','~U1','h','H'/
      DATA ( NAMES(1142,I),I=1,4)/'~u1','~U1','h','H3'/
      DATA ( NAMES(1143,I),I=1,4)/'~u1','~U1','H','H'/
      DATA ( NAMES(1144,I),I=1,4)/'~u1','~U1','H','H3'/
      DATA ( NAMES(1145,I),I=1,4)/'~u1','~U1','H3','H3'/
      DATA ( NAMES(1146,I),I=1,4)/'~u1','~U1','H+','H-'/
      DATA ( NAMES(1147,I),I=1,4)/'~u1','~d1','u','d'/
      DATA ( NAMES(1148,I),I=1,4)/'~u1','~d1','u','s'/
      DATA ( NAMES(1149,I),I=1,4)/'~u1','~d1','u','b'/
      DATA ( NAMES(1150,I),I=1,4)/'~u1','~d1','d','c'/
      DATA ( NAMES(1151,I),I=1,4)/'~u1','~d1','d','t'/
      DATA ( NAMES(1152,I),I=1,4)/'~u1','~d1','c','s'/
      DATA ( NAMES(1153,I),I=1,4)/'~u1','~d1','c','b'/
      DATA ( NAMES(1154,I),I=1,4)/'~u1','~d1','s','t'/
      DATA ( NAMES(1155,I),I=1,4)/'~u1','~d1','t','b'/
      DATA ( NAMES(1156,I),I=1,4)/'~u1','~D1','A','W+'/
      DATA ( NAMES(1157,I),I=1,4)/'~u1','~D1','A','H+'/
      DATA ( NAMES(1158,I),I=1,4)/'~u1','~D1','Z','W+'/
      DATA ( NAMES(1159,I),I=1,4)/'~u1','~D1','Z','H+'/
      DATA ( NAMES(1160,I),I=1,4)/'~u1','~D1','W+','G'/
      DATA ( NAMES(1161,I),I=1,4)/'~u1','~D1','W+','h'/
      DATA ( NAMES(1162,I),I=1,4)/'~u1','~D1','W+','H'/
      DATA ( NAMES(1163,I),I=1,4)/'~u1','~D1','W+','H3'/
      DATA ( NAMES(1164,I),I=1,4)/'~u1','~D1','G','H+'/
      DATA ( NAMES(1165,I),I=1,4)/'~u1','~D1','n1','E1'/
      DATA ( NAMES(1166,I),I=1,4)/'~u1','~D1','n2','E2'/
      DATA ( NAMES(1167,I),I=1,4)/'~u1','~D1','n3','E3'/
      DATA ( NAMES(1168,I),I=1,4)/'~u1','~D1','u','D'/
      DATA ( NAMES(1169,I),I=1,4)/'~u1','~D1','u','S'/
      DATA ( NAMES(1170,I),I=1,4)/'~u1','~D1','u','B'/
      DATA ( NAMES(1171,I),I=1,4)/'~u1','~D1','D','c'/
      DATA ( NAMES(1172,I),I=1,4)/'~u1','~D1','D','t'/
      DATA ( NAMES(1173,I),I=1,4)/'~u1','~D1','c','S'/
      DATA ( NAMES(1174,I),I=1,4)/'~u1','~D1','c','B'/
      DATA ( NAMES(1175,I),I=1,4)/'~u1','~D1','S','t'/
      DATA ( NAMES(1176,I),I=1,4)/'~u1','~D1','t','B'/
      DATA ( NAMES(1177,I),I=1,4)/'~u1','~D1','h','H+'/
      DATA ( NAMES(1178,I),I=1,4)/'~u1','~D1','H','H+'/
      DATA ( NAMES(1179,I),I=1,4)/'~u1','~D1','H3','H+'/
      DATA ( NAMES(1180,I),I=1,4)/'~u1','~c1','u','c'/
      DATA ( NAMES(1181,I),I=1,4)/'~u1','~C1','W+','W-'/
      DATA ( NAMES(1182,I),I=1,4)/'~u1','~C1','W+','H-'/
      DATA ( NAMES(1183,I),I=1,4)/'~u1','~C1','W-','H+'/
      DATA ( NAMES(1184,I),I=1,4)/'~u1','~C1','u','C'/
      DATA ( NAMES(1185,I),I=1,4)/'~u1','~C1','d','D'/
      DATA ( NAMES(1186,I),I=1,4)/'~u1','~C1','d','S'/
      DATA ( NAMES(1187,I),I=1,4)/'~u1','~C1','d','B'/
      DATA ( NAMES(1188,I),I=1,4)/'~u1','~C1','D','s'/
      DATA ( NAMES(1189,I),I=1,4)/'~u1','~C1','D','b'/
      DATA ( NAMES(1190,I),I=1,4)/'~u1','~C1','s','S'/
      DATA ( NAMES(1191,I),I=1,4)/'~u1','~C1','s','B'/
      DATA ( NAMES(1192,I),I=1,4)/'~u1','~C1','S','b'/
      DATA ( NAMES(1193,I),I=1,4)/'~u1','~C1','b','B'/
      DATA ( NAMES(1194,I),I=1,4)/'~u1','~C1','H+','H-'/
      DATA ( NAMES(1195,I),I=1,4)/'~u1','~s1','u','d'/
      DATA ( NAMES(1196,I),I=1,4)/'~u1','~s1','u','s'/
      DATA ( NAMES(1197,I),I=1,4)/'~u1','~s1','u','b'/
      DATA ( NAMES(1198,I),I=1,4)/'~u1','~s1','d','c'/
      DATA ( NAMES(1199,I),I=1,4)/'~u1','~s1','d','t'/
      DATA ( NAMES(1200,I),I=1,4)/'~u1','~s1','c','s'/
      DATA ( NAMES(1201,I),I=1,4)/'~u1','~s1','c','b'/
      DATA ( NAMES(1202,I),I=1,4)/'~u1','~s1','s','t'/
      DATA ( NAMES(1203,I),I=1,4)/'~u1','~s1','t','b'/
      DATA ( NAMES(1204,I),I=1,4)/'~u1','~S1','A','W+'/
      DATA ( NAMES(1205,I),I=1,4)/'~u1','~S1','A','H+'/
      DATA ( NAMES(1206,I),I=1,4)/'~u1','~S1','Z','W+'/
      DATA ( NAMES(1207,I),I=1,4)/'~u1','~S1','Z','H+'/
      DATA ( NAMES(1208,I),I=1,4)/'~u1','~S1','W+','G'/
      DATA ( NAMES(1209,I),I=1,4)/'~u1','~S1','W+','h'/
      DATA ( NAMES(1210,I),I=1,4)/'~u1','~S1','W+','H'/
      DATA ( NAMES(1211,I),I=1,4)/'~u1','~S1','W+','H3'/
      DATA ( NAMES(1212,I),I=1,4)/'~u1','~S1','G','H+'/
      DATA ( NAMES(1213,I),I=1,4)/'~u1','~S1','n1','E1'/
      DATA ( NAMES(1214,I),I=1,4)/'~u1','~S1','n2','E2'/
      DATA ( NAMES(1215,I),I=1,4)/'~u1','~S1','n3','E3'/
      DATA ( NAMES(1216,I),I=1,4)/'~u1','~S1','u','D'/
      DATA ( NAMES(1217,I),I=1,4)/'~u1','~S1','u','S'/
      DATA ( NAMES(1218,I),I=1,4)/'~u1','~S1','u','B'/
      DATA ( NAMES(1219,I),I=1,4)/'~u1','~S1','D','c'/
      DATA ( NAMES(1220,I),I=1,4)/'~u1','~S1','D','t'/
      DATA ( NAMES(1221,I),I=1,4)/'~u1','~S1','c','S'/
      DATA ( NAMES(1222,I),I=1,4)/'~u1','~S1','c','B'/
      DATA ( NAMES(1223,I),I=1,4)/'~u1','~S1','S','t'/
      DATA ( NAMES(1224,I),I=1,4)/'~u1','~S1','t','B'/
      DATA ( NAMES(1225,I),I=1,4)/'~u1','~S1','h','H+'/
      DATA ( NAMES(1226,I),I=1,4)/'~u1','~S1','H','H+'/
      DATA ( NAMES(1227,I),I=1,4)/'~u1','~S1','H3','H+'/
      DATA ( NAMES(1228,I),I=1,4)/'~u1','~t1','u','t'/
      DATA ( NAMES(1229,I),I=1,4)/'~u1','~T1','W+','W-'/
      DATA ( NAMES(1230,I),I=1,4)/'~u1','~T1','W+','H-'/
      DATA ( NAMES(1231,I),I=1,4)/'~u1','~T1','W-','H+'/
      DATA ( NAMES(1232,I),I=1,4)/'~u1','~T1','u','T'/
      DATA ( NAMES(1233,I),I=1,4)/'~u1','~T1','d','D'/
      DATA ( NAMES(1234,I),I=1,4)/'~u1','~T1','d','S'/
      DATA ( NAMES(1235,I),I=1,4)/'~u1','~T1','d','B'/
      DATA ( NAMES(1236,I),I=1,4)/'~u1','~T1','D','s'/
      DATA ( NAMES(1237,I),I=1,4)/'~u1','~T1','D','b'/
      DATA ( NAMES(1238,I),I=1,4)/'~u1','~T1','s','S'/
      DATA ( NAMES(1239,I),I=1,4)/'~u1','~T1','s','B'/
      DATA ( NAMES(1240,I),I=1,4)/'~u1','~T1','S','b'/
      DATA ( NAMES(1241,I),I=1,4)/'~u1','~T1','b','B'/
      DATA ( NAMES(1242,I),I=1,4)/'~u1','~T1','H+','H-'/
      DATA ( NAMES(1243,I),I=1,4)/'~u1','~b1','u','d'/
      DATA ( NAMES(1244,I),I=1,4)/'~u1','~b1','u','s'/
      DATA ( NAMES(1245,I),I=1,4)/'~u1','~b1','u','b'/
      DATA ( NAMES(1246,I),I=1,4)/'~u1','~b1','d','c'/
      DATA ( NAMES(1247,I),I=1,4)/'~u1','~b1','d','t'/
      DATA ( NAMES(1248,I),I=1,4)/'~u1','~b1','c','s'/
      DATA ( NAMES(1249,I),I=1,4)/'~u1','~b1','c','b'/
      DATA ( NAMES(1250,I),I=1,4)/'~u1','~b1','s','t'/
      DATA ( NAMES(1251,I),I=1,4)/'~u1','~b1','t','b'/
      DATA ( NAMES(1252,I),I=1,4)/'~u1','~B1','A','W+'/
      DATA ( NAMES(1253,I),I=1,4)/'~u1','~B1','A','H+'/
      DATA ( NAMES(1254,I),I=1,4)/'~u1','~B1','Z','W+'/
      DATA ( NAMES(1255,I),I=1,4)/'~u1','~B1','Z','H+'/
      DATA ( NAMES(1256,I),I=1,4)/'~u1','~B1','W+','G'/
      DATA ( NAMES(1257,I),I=1,4)/'~u1','~B1','W+','h'/
      DATA ( NAMES(1258,I),I=1,4)/'~u1','~B1','W+','H'/
      DATA ( NAMES(1259,I),I=1,4)/'~u1','~B1','W+','H3'/
      DATA ( NAMES(1260,I),I=1,4)/'~u1','~B1','G','H+'/
      DATA ( NAMES(1261,I),I=1,4)/'~u1','~B1','n1','E1'/
      DATA ( NAMES(1262,I),I=1,4)/'~u1','~B1','n2','E2'/
      DATA ( NAMES(1263,I),I=1,4)/'~u1','~B1','n3','E3'/
      DATA ( NAMES(1264,I),I=1,4)/'~u1','~B1','u','D'/
      DATA ( NAMES(1265,I),I=1,4)/'~u1','~B1','u','S'/
      DATA ( NAMES(1266,I),I=1,4)/'~u1','~B1','u','B'/
      DATA ( NAMES(1267,I),I=1,4)/'~u1','~B1','D','c'/
      DATA ( NAMES(1268,I),I=1,4)/'~u1','~B1','D','t'/
      DATA ( NAMES(1269,I),I=1,4)/'~u1','~B1','c','S'/
      DATA ( NAMES(1270,I),I=1,4)/'~u1','~B1','c','B'/
      DATA ( NAMES(1271,I),I=1,4)/'~u1','~B1','S','t'/
      DATA ( NAMES(1272,I),I=1,4)/'~u1','~B1','t','B'/
      DATA ( NAMES(1273,I),I=1,4)/'~u1','~B1','h','H+'/
      DATA ( NAMES(1274,I),I=1,4)/'~u1','~B1','H','H+'/
      DATA ( NAMES(1275,I),I=1,4)/'~u1','~B1','H3','H+'/
      DATA ( NAMES(1276,I),I=1,4)/'~u1','~g','A','u'/
      DATA ( NAMES(1277,I),I=1,4)/'~u1','~g','Z','u'/
      DATA ( NAMES(1278,I),I=1,4)/'~u1','~g','W+','d'/
      DATA ( NAMES(1279,I),I=1,4)/'~u1','~g','W+','s'/
      DATA ( NAMES(1280,I),I=1,4)/'~u1','~g','W+','b'/
      DATA ( NAMES(1281,I),I=1,4)/'~u1','~g','G','u'/
      DATA ( NAMES(1282,I),I=1,4)/'~u1','~g','u','h'/
      DATA ( NAMES(1283,I),I=1,4)/'~u1','~g','u','H'/
      DATA ( NAMES(1284,I),I=1,4)/'~u1','~g','d','H+'/
      DATA ( NAMES(1285,I),I=1,4)/'~u1','~g','s','H+'/
      DATA ( NAMES(1286,I),I=1,4)/'~u1','~g','b','H+'/
      DATA ( NAMES(1287,I),I=1,4)/'~d1','~d1','d','d'/
      DATA ( NAMES(1288,I),I=1,4)/'~d1','~D1','A','A'/
      DATA ( NAMES(1289,I),I=1,4)/'~d1','~D1','A','Z'/
      DATA ( NAMES(1290,I),I=1,4)/'~d1','~D1','A','G'/
      DATA ( NAMES(1291,I),I=1,4)/'~d1','~D1','A','h'/
      DATA ( NAMES(1292,I),I=1,4)/'~d1','~D1','A','H'/
      DATA ( NAMES(1293,I),I=1,4)/'~d1','~D1','Z','Z'/
      DATA ( NAMES(1294,I),I=1,4)/'~d1','~D1','Z','G'/
      DATA ( NAMES(1295,I),I=1,4)/'~d1','~D1','Z','h'/
      DATA ( NAMES(1296,I),I=1,4)/'~d1','~D1','Z','H'/
      DATA ( NAMES(1297,I),I=1,4)/'~d1','~D1','Z','H3'/
      DATA ( NAMES(1298,I),I=1,4)/'~d1','~D1','W+','W-'/
      DATA ( NAMES(1299,I),I=1,4)/'~d1','~D1','W+','H-'/
      DATA ( NAMES(1300,I),I=1,4)/'~d1','~D1','W-','H+'/
      DATA ( NAMES(1301,I),I=1,4)/'~d1','~D1','G','G'/
      DATA ( NAMES(1302,I),I=1,4)/'~d1','~D1','G','h'/
      DATA ( NAMES(1303,I),I=1,4)/'~d1','~D1','G','H'/
      DATA ( NAMES(1304,I),I=1,4)/'~d1','~D1','n1','N1'/
      DATA ( NAMES(1305,I),I=1,4)/'~d1','~D1','n2','N2'/
      DATA ( NAMES(1306,I),I=1,4)/'~d1','~D1','n3','N3'/
      DATA ( NAMES(1307,I),I=1,4)/'~d1','~D1','e1','E1'/
      DATA ( NAMES(1308,I),I=1,4)/'~d1','~D1','e2','E2'/
      DATA ( NAMES(1309,I),I=1,4)/'~d1','~D1','e3','E3'/
      DATA ( NAMES(1310,I),I=1,4)/'~d1','~D1','u','U'/
      DATA ( NAMES(1311,I),I=1,4)/'~d1','~D1','u','C'/
      DATA ( NAMES(1312,I),I=1,4)/'~d1','~D1','u','T'/
      DATA ( NAMES(1313,I),I=1,4)/'~d1','~D1','U','c'/
      DATA ( NAMES(1314,I),I=1,4)/'~d1','~D1','U','t'/
      DATA ( NAMES(1315,I),I=1,4)/'~d1','~D1','d','D'/
      DATA ( NAMES(1316,I),I=1,4)/'~d1','~D1','c','C'/
      DATA ( NAMES(1317,I),I=1,4)/'~d1','~D1','c','T'/
      DATA ( NAMES(1318,I),I=1,4)/'~d1','~D1','C','t'/
      DATA ( NAMES(1319,I),I=1,4)/'~d1','~D1','s','S'/
      DATA ( NAMES(1320,I),I=1,4)/'~d1','~D1','t','T'/
      DATA ( NAMES(1321,I),I=1,4)/'~d1','~D1','b','B'/
      DATA ( NAMES(1322,I),I=1,4)/'~d1','~D1','h','h'/
      DATA ( NAMES(1323,I),I=1,4)/'~d1','~D1','h','H'/
      DATA ( NAMES(1324,I),I=1,4)/'~d1','~D1','h','H3'/
      DATA ( NAMES(1325,I),I=1,4)/'~d1','~D1','H','H'/
      DATA ( NAMES(1326,I),I=1,4)/'~d1','~D1','H','H3'/
      DATA ( NAMES(1327,I),I=1,4)/'~d1','~D1','H3','H3'/
      DATA ( NAMES(1328,I),I=1,4)/'~d1','~D1','H+','H-'/
      DATA ( NAMES(1329,I),I=1,4)/'~d1','~c1','u','d'/
      DATA ( NAMES(1330,I),I=1,4)/'~d1','~c1','u','s'/
      DATA ( NAMES(1331,I),I=1,4)/'~d1','~c1','u','b'/
      DATA ( NAMES(1332,I),I=1,4)/'~d1','~c1','d','c'/
      DATA ( NAMES(1333,I),I=1,4)/'~d1','~c1','d','t'/
      DATA ( NAMES(1334,I),I=1,4)/'~d1','~c1','c','s'/
      DATA ( NAMES(1335,I),I=1,4)/'~d1','~c1','c','b'/
      DATA ( NAMES(1336,I),I=1,4)/'~d1','~c1','s','t'/
      DATA ( NAMES(1337,I),I=1,4)/'~d1','~c1','t','b'/
      DATA ( NAMES(1338,I),I=1,4)/'~d1','~C1','A','W-'/
      DATA ( NAMES(1339,I),I=1,4)/'~d1','~C1','A','H-'/
      DATA ( NAMES(1340,I),I=1,4)/'~d1','~C1','Z','W-'/
      DATA ( NAMES(1341,I),I=1,4)/'~d1','~C1','Z','H-'/
      DATA ( NAMES(1342,I),I=1,4)/'~d1','~C1','W-','G'/
      DATA ( NAMES(1343,I),I=1,4)/'~d1','~C1','W-','h'/
      DATA ( NAMES(1344,I),I=1,4)/'~d1','~C1','W-','H'/
      DATA ( NAMES(1345,I),I=1,4)/'~d1','~C1','W-','H3'/
      DATA ( NAMES(1346,I),I=1,4)/'~d1','~C1','G','H-'/
      DATA ( NAMES(1347,I),I=1,4)/'~d1','~C1','N1','e1'/
      DATA ( NAMES(1348,I),I=1,4)/'~d1','~C1','N2','e2'/
      DATA ( NAMES(1349,I),I=1,4)/'~d1','~C1','N3','e3'/
      DATA ( NAMES(1350,I),I=1,4)/'~d1','~C1','U','d'/
      DATA ( NAMES(1351,I),I=1,4)/'~d1','~C1','U','s'/
      DATA ( NAMES(1352,I),I=1,4)/'~d1','~C1','U','b'/
      DATA ( NAMES(1353,I),I=1,4)/'~d1','~C1','d','C'/
      DATA ( NAMES(1354,I),I=1,4)/'~d1','~C1','d','T'/
      DATA ( NAMES(1355,I),I=1,4)/'~d1','~C1','C','s'/
      DATA ( NAMES(1356,I),I=1,4)/'~d1','~C1','C','b'/
      DATA ( NAMES(1357,I),I=1,4)/'~d1','~C1','s','T'/
      DATA ( NAMES(1358,I),I=1,4)/'~d1','~C1','T','b'/
      DATA ( NAMES(1359,I),I=1,4)/'~d1','~C1','h','H-'/
      DATA ( NAMES(1360,I),I=1,4)/'~d1','~C1','H','H-'/
      DATA ( NAMES(1361,I),I=1,4)/'~d1','~C1','H3','H-'/
      DATA ( NAMES(1362,I),I=1,4)/'~d1','~s1','d','s'/
      DATA ( NAMES(1363,I),I=1,4)/'~d1','~S1','W+','W-'/
      DATA ( NAMES(1364,I),I=1,4)/'~d1','~S1','W+','H-'/
      DATA ( NAMES(1365,I),I=1,4)/'~d1','~S1','W-','H+'/
      DATA ( NAMES(1366,I),I=1,4)/'~d1','~S1','u','U'/
      DATA ( NAMES(1367,I),I=1,4)/'~d1','~S1','u','C'/
      DATA ( NAMES(1368,I),I=1,4)/'~d1','~S1','u','T'/
      DATA ( NAMES(1369,I),I=1,4)/'~d1','~S1','U','c'/
      DATA ( NAMES(1370,I),I=1,4)/'~d1','~S1','U','t'/
      DATA ( NAMES(1371,I),I=1,4)/'~d1','~S1','d','S'/
      DATA ( NAMES(1372,I),I=1,4)/'~d1','~S1','c','C'/
      DATA ( NAMES(1373,I),I=1,4)/'~d1','~S1','c','T'/
      DATA ( NAMES(1374,I),I=1,4)/'~d1','~S1','C','t'/
      DATA ( NAMES(1375,I),I=1,4)/'~d1','~S1','t','T'/
      DATA ( NAMES(1376,I),I=1,4)/'~d1','~S1','H+','H-'/
      DATA ( NAMES(1377,I),I=1,4)/'~d1','~t1','u','d'/
      DATA ( NAMES(1378,I),I=1,4)/'~d1','~t1','u','s'/
      DATA ( NAMES(1379,I),I=1,4)/'~d1','~t1','u','b'/
      DATA ( NAMES(1380,I),I=1,4)/'~d1','~t1','d','c'/
      DATA ( NAMES(1381,I),I=1,4)/'~d1','~t1','d','t'/
      DATA ( NAMES(1382,I),I=1,4)/'~d1','~t1','c','s'/
      DATA ( NAMES(1383,I),I=1,4)/'~d1','~t1','c','b'/
      DATA ( NAMES(1384,I),I=1,4)/'~d1','~t1','s','t'/
      DATA ( NAMES(1385,I),I=1,4)/'~d1','~t1','t','b'/
      DATA ( NAMES(1386,I),I=1,4)/'~d1','~T1','A','W-'/
      DATA ( NAMES(1387,I),I=1,4)/'~d1','~T1','A','H-'/
      DATA ( NAMES(1388,I),I=1,4)/'~d1','~T1','Z','W-'/
      DATA ( NAMES(1389,I),I=1,4)/'~d1','~T1','Z','H-'/
      DATA ( NAMES(1390,I),I=1,4)/'~d1','~T1','W-','G'/
      DATA ( NAMES(1391,I),I=1,4)/'~d1','~T1','W-','h'/
      DATA ( NAMES(1392,I),I=1,4)/'~d1','~T1','W-','H'/
      DATA ( NAMES(1393,I),I=1,4)/'~d1','~T1','W-','H3'/
      DATA ( NAMES(1394,I),I=1,4)/'~d1','~T1','G','H-'/
      DATA ( NAMES(1395,I),I=1,4)/'~d1','~T1','N1','e1'/
      DATA ( NAMES(1396,I),I=1,4)/'~d1','~T1','N2','e2'/
      DATA ( NAMES(1397,I),I=1,4)/'~d1','~T1','N3','e3'/
      DATA ( NAMES(1398,I),I=1,4)/'~d1','~T1','U','d'/
      DATA ( NAMES(1399,I),I=1,4)/'~d1','~T1','U','s'/
      DATA ( NAMES(1400,I),I=1,4)/'~d1','~T1','U','b'/
      DATA ( NAMES(1401,I),I=1,4)/'~d1','~T1','d','C'/
      DATA ( NAMES(1402,I),I=1,4)/'~d1','~T1','d','T'/
      DATA ( NAMES(1403,I),I=1,4)/'~d1','~T1','C','s'/
      DATA ( NAMES(1404,I),I=1,4)/'~d1','~T1','C','b'/
      DATA ( NAMES(1405,I),I=1,4)/'~d1','~T1','s','T'/
      DATA ( NAMES(1406,I),I=1,4)/'~d1','~T1','T','b'/
      DATA ( NAMES(1407,I),I=1,4)/'~d1','~T1','h','H-'/
      DATA ( NAMES(1408,I),I=1,4)/'~d1','~T1','H','H-'/
      DATA ( NAMES(1409,I),I=1,4)/'~d1','~T1','H3','H-'/
      DATA ( NAMES(1410,I),I=1,4)/'~d1','~b1','d','b'/
      DATA ( NAMES(1411,I),I=1,4)/'~d1','~B1','W+','W-'/
      DATA ( NAMES(1412,I),I=1,4)/'~d1','~B1','W+','H-'/
      DATA ( NAMES(1413,I),I=1,4)/'~d1','~B1','W-','H+'/
      DATA ( NAMES(1414,I),I=1,4)/'~d1','~B1','u','U'/
      DATA ( NAMES(1415,I),I=1,4)/'~d1','~B1','u','C'/
      DATA ( NAMES(1416,I),I=1,4)/'~d1','~B1','u','T'/
      DATA ( NAMES(1417,I),I=1,4)/'~d1','~B1','U','c'/
      DATA ( NAMES(1418,I),I=1,4)/'~d1','~B1','U','t'/
      DATA ( NAMES(1419,I),I=1,4)/'~d1','~B1','d','B'/
      DATA ( NAMES(1420,I),I=1,4)/'~d1','~B1','c','C'/
      DATA ( NAMES(1421,I),I=1,4)/'~d1','~B1','c','T'/
      DATA ( NAMES(1422,I),I=1,4)/'~d1','~B1','C','t'/
      DATA ( NAMES(1423,I),I=1,4)/'~d1','~B1','t','T'/
      DATA ( NAMES(1424,I),I=1,4)/'~d1','~B1','H+','H-'/
      DATA ( NAMES(1425,I),I=1,4)/'~d1','~g','A','d'/
      DATA ( NAMES(1426,I),I=1,4)/'~d1','~g','Z','d'/
      DATA ( NAMES(1427,I),I=1,4)/'~d1','~g','W-','u'/
      DATA ( NAMES(1428,I),I=1,4)/'~d1','~g','W-','c'/
      DATA ( NAMES(1429,I),I=1,4)/'~d1','~g','W-','t'/
      DATA ( NAMES(1430,I),I=1,4)/'~d1','~g','G','d'/
      DATA ( NAMES(1431,I),I=1,4)/'~d1','~g','u','H-'/
      DATA ( NAMES(1432,I),I=1,4)/'~d1','~g','d','h'/
      DATA ( NAMES(1433,I),I=1,4)/'~d1','~g','d','H'/
      DATA ( NAMES(1434,I),I=1,4)/'~d1','~g','c','H-'/
      DATA ( NAMES(1435,I),I=1,4)/'~d1','~g','t','H-'/
      DATA ( NAMES(1436,I),I=1,4)/'~c1','~c1','c','c'/
      DATA ( NAMES(1437,I),I=1,4)/'~c1','~C1','A','A'/
      DATA ( NAMES(1438,I),I=1,4)/'~c1','~C1','A','Z'/
      DATA ( NAMES(1439,I),I=1,4)/'~c1','~C1','A','G'/
      DATA ( NAMES(1440,I),I=1,4)/'~c1','~C1','A','h'/
      DATA ( NAMES(1441,I),I=1,4)/'~c1','~C1','A','H'/
      DATA ( NAMES(1442,I),I=1,4)/'~c1','~C1','Z','Z'/
      DATA ( NAMES(1443,I),I=1,4)/'~c1','~C1','Z','G'/
      DATA ( NAMES(1444,I),I=1,4)/'~c1','~C1','Z','h'/
      DATA ( NAMES(1445,I),I=1,4)/'~c1','~C1','Z','H'/
      DATA ( NAMES(1446,I),I=1,4)/'~c1','~C1','Z','H3'/
      DATA ( NAMES(1447,I),I=1,4)/'~c1','~C1','W+','W-'/
      DATA ( NAMES(1448,I),I=1,4)/'~c1','~C1','W+','H-'/
      DATA ( NAMES(1449,I),I=1,4)/'~c1','~C1','W-','H+'/
      DATA ( NAMES(1450,I),I=1,4)/'~c1','~C1','G','G'/
      DATA ( NAMES(1451,I),I=1,4)/'~c1','~C1','G','h'/
      DATA ( NAMES(1452,I),I=1,4)/'~c1','~C1','G','H'/
      DATA ( NAMES(1453,I),I=1,4)/'~c1','~C1','n1','N1'/
      DATA ( NAMES(1454,I),I=1,4)/'~c1','~C1','n2','N2'/
      DATA ( NAMES(1455,I),I=1,4)/'~c1','~C1','n3','N3'/
      DATA ( NAMES(1456,I),I=1,4)/'~c1','~C1','e1','E1'/
      DATA ( NAMES(1457,I),I=1,4)/'~c1','~C1','e2','E2'/
      DATA ( NAMES(1458,I),I=1,4)/'~c1','~C1','e3','E3'/
      DATA ( NAMES(1459,I),I=1,4)/'~c1','~C1','u','U'/
      DATA ( NAMES(1460,I),I=1,4)/'~c1','~C1','d','D'/
      DATA ( NAMES(1461,I),I=1,4)/'~c1','~C1','d','S'/
      DATA ( NAMES(1462,I),I=1,4)/'~c1','~C1','d','B'/
      DATA ( NAMES(1463,I),I=1,4)/'~c1','~C1','D','s'/
      DATA ( NAMES(1464,I),I=1,4)/'~c1','~C1','D','b'/
      DATA ( NAMES(1465,I),I=1,4)/'~c1','~C1','c','C'/
      DATA ( NAMES(1466,I),I=1,4)/'~c1','~C1','s','S'/
      DATA ( NAMES(1467,I),I=1,4)/'~c1','~C1','s','B'/
      DATA ( NAMES(1468,I),I=1,4)/'~c1','~C1','S','b'/
      DATA ( NAMES(1469,I),I=1,4)/'~c1','~C1','t','T'/
      DATA ( NAMES(1470,I),I=1,4)/'~c1','~C1','b','B'/
      DATA ( NAMES(1471,I),I=1,4)/'~c1','~C1','h','h'/
      DATA ( NAMES(1472,I),I=1,4)/'~c1','~C1','h','H'/
      DATA ( NAMES(1473,I),I=1,4)/'~c1','~C1','h','H3'/
      DATA ( NAMES(1474,I),I=1,4)/'~c1','~C1','H','H'/
      DATA ( NAMES(1475,I),I=1,4)/'~c1','~C1','H','H3'/
      DATA ( NAMES(1476,I),I=1,4)/'~c1','~C1','H3','H3'/
      DATA ( NAMES(1477,I),I=1,4)/'~c1','~C1','H+','H-'/
      DATA ( NAMES(1478,I),I=1,4)/'~c1','~s1','u','d'/
      DATA ( NAMES(1479,I),I=1,4)/'~c1','~s1','u','s'/
      DATA ( NAMES(1480,I),I=1,4)/'~c1','~s1','u','b'/
      DATA ( NAMES(1481,I),I=1,4)/'~c1','~s1','d','c'/
      DATA ( NAMES(1482,I),I=1,4)/'~c1','~s1','d','t'/
      DATA ( NAMES(1483,I),I=1,4)/'~c1','~s1','c','s'/
      DATA ( NAMES(1484,I),I=1,4)/'~c1','~s1','c','b'/
      DATA ( NAMES(1485,I),I=1,4)/'~c1','~s1','s','t'/
      DATA ( NAMES(1486,I),I=1,4)/'~c1','~s1','t','b'/
      DATA ( NAMES(1487,I),I=1,4)/'~c1','~S1','A','W+'/
      DATA ( NAMES(1488,I),I=1,4)/'~c1','~S1','A','H+'/
      DATA ( NAMES(1489,I),I=1,4)/'~c1','~S1','Z','W+'/
      DATA ( NAMES(1490,I),I=1,4)/'~c1','~S1','Z','H+'/
      DATA ( NAMES(1491,I),I=1,4)/'~c1','~S1','W+','G'/
      DATA ( NAMES(1492,I),I=1,4)/'~c1','~S1','W+','h'/
      DATA ( NAMES(1493,I),I=1,4)/'~c1','~S1','W+','H'/
      DATA ( NAMES(1494,I),I=1,4)/'~c1','~S1','W+','H3'/
      DATA ( NAMES(1495,I),I=1,4)/'~c1','~S1','G','H+'/
      DATA ( NAMES(1496,I),I=1,4)/'~c1','~S1','n1','E1'/
      DATA ( NAMES(1497,I),I=1,4)/'~c1','~S1','n2','E2'/
      DATA ( NAMES(1498,I),I=1,4)/'~c1','~S1','n3','E3'/
      DATA ( NAMES(1499,I),I=1,4)/'~c1','~S1','u','D'/
      DATA ( NAMES(1500,I),I=1,4)/'~c1','~S1','u','S'/
      DATA ( NAMES(1501,I),I=1,4)/'~c1','~S1','u','B'/
      DATA ( NAMES(1502,I),I=1,4)/'~c1','~S1','D','c'/
      DATA ( NAMES(1503,I),I=1,4)/'~c1','~S1','D','t'/
      DATA ( NAMES(1504,I),I=1,4)/'~c1','~S1','c','S'/
      DATA ( NAMES(1505,I),I=1,4)/'~c1','~S1','c','B'/
      DATA ( NAMES(1506,I),I=1,4)/'~c1','~S1','S','t'/
      DATA ( NAMES(1507,I),I=1,4)/'~c1','~S1','t','B'/
      DATA ( NAMES(1508,I),I=1,4)/'~c1','~S1','h','H+'/
      DATA ( NAMES(1509,I),I=1,4)/'~c1','~S1','H','H+'/
      DATA ( NAMES(1510,I),I=1,4)/'~c1','~S1','H3','H+'/
      DATA ( NAMES(1511,I),I=1,4)/'~c1','~t1','c','t'/
      DATA ( NAMES(1512,I),I=1,4)/'~c1','~T1','W+','W-'/
      DATA ( NAMES(1513,I),I=1,4)/'~c1','~T1','W+','H-'/
      DATA ( NAMES(1514,I),I=1,4)/'~c1','~T1','W-','H+'/
      DATA ( NAMES(1515,I),I=1,4)/'~c1','~T1','d','D'/
      DATA ( NAMES(1516,I),I=1,4)/'~c1','~T1','d','S'/
      DATA ( NAMES(1517,I),I=1,4)/'~c1','~T1','d','B'/
      DATA ( NAMES(1518,I),I=1,4)/'~c1','~T1','D','s'/
      DATA ( NAMES(1519,I),I=1,4)/'~c1','~T1','D','b'/
      DATA ( NAMES(1520,I),I=1,4)/'~c1','~T1','c','T'/
      DATA ( NAMES(1521,I),I=1,4)/'~c1','~T1','s','S'/
      DATA ( NAMES(1522,I),I=1,4)/'~c1','~T1','s','B'/
      DATA ( NAMES(1523,I),I=1,4)/'~c1','~T1','S','b'/
      DATA ( NAMES(1524,I),I=1,4)/'~c1','~T1','b','B'/
      DATA ( NAMES(1525,I),I=1,4)/'~c1','~T1','H+','H-'/
      DATA ( NAMES(1526,I),I=1,4)/'~c1','~b1','u','d'/
      DATA ( NAMES(1527,I),I=1,4)/'~c1','~b1','u','s'/
      DATA ( NAMES(1528,I),I=1,4)/'~c1','~b1','u','b'/
      DATA ( NAMES(1529,I),I=1,4)/'~c1','~b1','d','c'/
      DATA ( NAMES(1530,I),I=1,4)/'~c1','~b1','d','t'/
      DATA ( NAMES(1531,I),I=1,4)/'~c1','~b1','c','s'/
      DATA ( NAMES(1532,I),I=1,4)/'~c1','~b1','c','b'/
      DATA ( NAMES(1533,I),I=1,4)/'~c1','~b1','s','t'/
      DATA ( NAMES(1534,I),I=1,4)/'~c1','~b1','t','b'/
      DATA ( NAMES(1535,I),I=1,4)/'~c1','~B1','A','W+'/
      DATA ( NAMES(1536,I),I=1,4)/'~c1','~B1','A','H+'/
      DATA ( NAMES(1537,I),I=1,4)/'~c1','~B1','Z','W+'/
      DATA ( NAMES(1538,I),I=1,4)/'~c1','~B1','Z','H+'/
      DATA ( NAMES(1539,I),I=1,4)/'~c1','~B1','W+','G'/
      DATA ( NAMES(1540,I),I=1,4)/'~c1','~B1','W+','h'/
      DATA ( NAMES(1541,I),I=1,4)/'~c1','~B1','W+','H'/
      DATA ( NAMES(1542,I),I=1,4)/'~c1','~B1','W+','H3'/
      DATA ( NAMES(1543,I),I=1,4)/'~c1','~B1','G','H+'/
      DATA ( NAMES(1544,I),I=1,4)/'~c1','~B1','n1','E1'/
      DATA ( NAMES(1545,I),I=1,4)/'~c1','~B1','n2','E2'/
      DATA ( NAMES(1546,I),I=1,4)/'~c1','~B1','n3','E3'/
      DATA ( NAMES(1547,I),I=1,4)/'~c1','~B1','u','D'/
      DATA ( NAMES(1548,I),I=1,4)/'~c1','~B1','u','S'/
      DATA ( NAMES(1549,I),I=1,4)/'~c1','~B1','u','B'/
      DATA ( NAMES(1550,I),I=1,4)/'~c1','~B1','D','c'/
      DATA ( NAMES(1551,I),I=1,4)/'~c1','~B1','D','t'/
      DATA ( NAMES(1552,I),I=1,4)/'~c1','~B1','c','S'/
      DATA ( NAMES(1553,I),I=1,4)/'~c1','~B1','c','B'/
      DATA ( NAMES(1554,I),I=1,4)/'~c1','~B1','S','t'/
      DATA ( NAMES(1555,I),I=1,4)/'~c1','~B1','t','B'/
      DATA ( NAMES(1556,I),I=1,4)/'~c1','~B1','h','H+'/
      DATA ( NAMES(1557,I),I=1,4)/'~c1','~B1','H','H+'/
      DATA ( NAMES(1558,I),I=1,4)/'~c1','~B1','H3','H+'/
      DATA ( NAMES(1559,I),I=1,4)/'~c1','~g','A','c'/
      DATA ( NAMES(1560,I),I=1,4)/'~c1','~g','Z','c'/
      DATA ( NAMES(1561,I),I=1,4)/'~c1','~g','W+','d'/
      DATA ( NAMES(1562,I),I=1,4)/'~c1','~g','W+','s'/
      DATA ( NAMES(1563,I),I=1,4)/'~c1','~g','W+','b'/
      DATA ( NAMES(1564,I),I=1,4)/'~c1','~g','G','c'/
      DATA ( NAMES(1565,I),I=1,4)/'~c1','~g','d','H+'/
      DATA ( NAMES(1566,I),I=1,4)/'~c1','~g','c','h'/
      DATA ( NAMES(1567,I),I=1,4)/'~c1','~g','c','H'/
      DATA ( NAMES(1568,I),I=1,4)/'~c1','~g','c','H3'/
      DATA ( NAMES(1569,I),I=1,4)/'~c1','~g','s','H+'/
      DATA ( NAMES(1570,I),I=1,4)/'~c1','~g','b','H+'/
      DATA ( NAMES(1571,I),I=1,4)/'~s1','~s1','s','s'/
      DATA ( NAMES(1572,I),I=1,4)/'~s1','~S1','A','A'/
      DATA ( NAMES(1573,I),I=1,4)/'~s1','~S1','A','Z'/
      DATA ( NAMES(1574,I),I=1,4)/'~s1','~S1','A','G'/
      DATA ( NAMES(1575,I),I=1,4)/'~s1','~S1','A','h'/
      DATA ( NAMES(1576,I),I=1,4)/'~s1','~S1','A','H'/
      DATA ( NAMES(1577,I),I=1,4)/'~s1','~S1','Z','Z'/
      DATA ( NAMES(1578,I),I=1,4)/'~s1','~S1','Z','G'/
      DATA ( NAMES(1579,I),I=1,4)/'~s1','~S1','Z','h'/
      DATA ( NAMES(1580,I),I=1,4)/'~s1','~S1','Z','H'/
      DATA ( NAMES(1581,I),I=1,4)/'~s1','~S1','Z','H3'/
      DATA ( NAMES(1582,I),I=1,4)/'~s1','~S1','W+','W-'/
      DATA ( NAMES(1583,I),I=1,4)/'~s1','~S1','W+','H-'/
      DATA ( NAMES(1584,I),I=1,4)/'~s1','~S1','W-','H+'/
      DATA ( NAMES(1585,I),I=1,4)/'~s1','~S1','G','G'/
      DATA ( NAMES(1586,I),I=1,4)/'~s1','~S1','G','h'/
      DATA ( NAMES(1587,I),I=1,4)/'~s1','~S1','G','H'/
      DATA ( NAMES(1588,I),I=1,4)/'~s1','~S1','n1','N1'/
      DATA ( NAMES(1589,I),I=1,4)/'~s1','~S1','n2','N2'/
      DATA ( NAMES(1590,I),I=1,4)/'~s1','~S1','n3','N3'/
      DATA ( NAMES(1591,I),I=1,4)/'~s1','~S1','e1','E1'/
      DATA ( NAMES(1592,I),I=1,4)/'~s1','~S1','e2','E2'/
      DATA ( NAMES(1593,I),I=1,4)/'~s1','~S1','e3','E3'/
      DATA ( NAMES(1594,I),I=1,4)/'~s1','~S1','u','U'/
      DATA ( NAMES(1595,I),I=1,4)/'~s1','~S1','u','C'/
      DATA ( NAMES(1596,I),I=1,4)/'~s1','~S1','u','T'/
      DATA ( NAMES(1597,I),I=1,4)/'~s1','~S1','U','c'/
      DATA ( NAMES(1598,I),I=1,4)/'~s1','~S1','U','t'/
      DATA ( NAMES(1599,I),I=1,4)/'~s1','~S1','d','D'/
      DATA ( NAMES(1600,I),I=1,4)/'~s1','~S1','c','C'/
      DATA ( NAMES(1601,I),I=1,4)/'~s1','~S1','c','T'/
      DATA ( NAMES(1602,I),I=1,4)/'~s1','~S1','C','t'/
      DATA ( NAMES(1603,I),I=1,4)/'~s1','~S1','s','S'/
      DATA ( NAMES(1604,I),I=1,4)/'~s1','~S1','t','T'/
      DATA ( NAMES(1605,I),I=1,4)/'~s1','~S1','b','B'/
      DATA ( NAMES(1606,I),I=1,4)/'~s1','~S1','h','h'/
      DATA ( NAMES(1607,I),I=1,4)/'~s1','~S1','h','H'/
      DATA ( NAMES(1608,I),I=1,4)/'~s1','~S1','h','H3'/
      DATA ( NAMES(1609,I),I=1,4)/'~s1','~S1','H','H'/
      DATA ( NAMES(1610,I),I=1,4)/'~s1','~S1','H','H3'/
      DATA ( NAMES(1611,I),I=1,4)/'~s1','~S1','H3','H3'/
      DATA ( NAMES(1612,I),I=1,4)/'~s1','~S1','H+','H-'/
      DATA ( NAMES(1613,I),I=1,4)/'~s1','~t1','u','d'/
      DATA ( NAMES(1614,I),I=1,4)/'~s1','~t1','u','s'/
      DATA ( NAMES(1615,I),I=1,4)/'~s1','~t1','u','b'/
      DATA ( NAMES(1616,I),I=1,4)/'~s1','~t1','d','c'/
      DATA ( NAMES(1617,I),I=1,4)/'~s1','~t1','d','t'/
      DATA ( NAMES(1618,I),I=1,4)/'~s1','~t1','c','s'/
      DATA ( NAMES(1619,I),I=1,4)/'~s1','~t1','c','b'/
      DATA ( NAMES(1620,I),I=1,4)/'~s1','~t1','s','t'/
      DATA ( NAMES(1621,I),I=1,4)/'~s1','~t1','t','b'/
      DATA ( NAMES(1622,I),I=1,4)/'~s1','~T1','A','W-'/
      DATA ( NAMES(1623,I),I=1,4)/'~s1','~T1','A','H-'/
      DATA ( NAMES(1624,I),I=1,4)/'~s1','~T1','Z','W-'/
      DATA ( NAMES(1625,I),I=1,4)/'~s1','~T1','Z','H-'/
      DATA ( NAMES(1626,I),I=1,4)/'~s1','~T1','W-','G'/
      DATA ( NAMES(1627,I),I=1,4)/'~s1','~T1','W-','h'/
      DATA ( NAMES(1628,I),I=1,4)/'~s1','~T1','W-','H'/
      DATA ( NAMES(1629,I),I=1,4)/'~s1','~T1','W-','H3'/
      DATA ( NAMES(1630,I),I=1,4)/'~s1','~T1','G','H-'/
      DATA ( NAMES(1631,I),I=1,4)/'~s1','~T1','N1','e1'/
      DATA ( NAMES(1632,I),I=1,4)/'~s1','~T1','N2','e2'/
      DATA ( NAMES(1633,I),I=1,4)/'~s1','~T1','N3','e3'/
      DATA ( NAMES(1634,I),I=1,4)/'~s1','~T1','U','d'/
      DATA ( NAMES(1635,I),I=1,4)/'~s1','~T1','U','s'/
      DATA ( NAMES(1636,I),I=1,4)/'~s1','~T1','U','b'/
      DATA ( NAMES(1637,I),I=1,4)/'~s1','~T1','d','C'/
      DATA ( NAMES(1638,I),I=1,4)/'~s1','~T1','d','T'/
      DATA ( NAMES(1639,I),I=1,4)/'~s1','~T1','C','s'/
      DATA ( NAMES(1640,I),I=1,4)/'~s1','~T1','C','b'/
      DATA ( NAMES(1641,I),I=1,4)/'~s1','~T1','s','T'/
      DATA ( NAMES(1642,I),I=1,4)/'~s1','~T1','T','b'/
      DATA ( NAMES(1643,I),I=1,4)/'~s1','~T1','h','H-'/
      DATA ( NAMES(1644,I),I=1,4)/'~s1','~T1','H','H-'/
      DATA ( NAMES(1645,I),I=1,4)/'~s1','~T1','H3','H-'/
      DATA ( NAMES(1646,I),I=1,4)/'~s1','~b1','s','b'/
      DATA ( NAMES(1647,I),I=1,4)/'~s1','~B1','W+','W-'/
      DATA ( NAMES(1648,I),I=1,4)/'~s1','~B1','W+','H-'/
      DATA ( NAMES(1649,I),I=1,4)/'~s1','~B1','W-','H+'/
      DATA ( NAMES(1650,I),I=1,4)/'~s1','~B1','u','U'/
      DATA ( NAMES(1651,I),I=1,4)/'~s1','~B1','u','C'/
      DATA ( NAMES(1652,I),I=1,4)/'~s1','~B1','u','T'/
      DATA ( NAMES(1653,I),I=1,4)/'~s1','~B1','U','c'/
      DATA ( NAMES(1654,I),I=1,4)/'~s1','~B1','U','t'/
      DATA ( NAMES(1655,I),I=1,4)/'~s1','~B1','c','C'/
      DATA ( NAMES(1656,I),I=1,4)/'~s1','~B1','c','T'/
      DATA ( NAMES(1657,I),I=1,4)/'~s1','~B1','C','t'/
      DATA ( NAMES(1658,I),I=1,4)/'~s1','~B1','s','B'/
      DATA ( NAMES(1659,I),I=1,4)/'~s1','~B1','t','T'/
      DATA ( NAMES(1660,I),I=1,4)/'~s1','~B1','H+','H-'/
      DATA ( NAMES(1661,I),I=1,4)/'~s1','~g','A','s'/
      DATA ( NAMES(1662,I),I=1,4)/'~s1','~g','Z','s'/
      DATA ( NAMES(1663,I),I=1,4)/'~s1','~g','W-','u'/
      DATA ( NAMES(1664,I),I=1,4)/'~s1','~g','W-','c'/
      DATA ( NAMES(1665,I),I=1,4)/'~s1','~g','W-','t'/
      DATA ( NAMES(1666,I),I=1,4)/'~s1','~g','G','s'/
      DATA ( NAMES(1667,I),I=1,4)/'~s1','~g','u','H-'/
      DATA ( NAMES(1668,I),I=1,4)/'~s1','~g','c','H-'/
      DATA ( NAMES(1669,I),I=1,4)/'~s1','~g','s','h'/
      DATA ( NAMES(1670,I),I=1,4)/'~s1','~g','s','H'/
      DATA ( NAMES(1671,I),I=1,4)/'~s1','~g','s','H3'/
      DATA ( NAMES(1672,I),I=1,4)/'~s1','~g','t','H-'/
      DATA ( NAMES(1673,I),I=1,4)/'~t1','~t1','t','t'/
      DATA ( NAMES(1674,I),I=1,4)/'~t1','~T1','A','A'/
      DATA ( NAMES(1675,I),I=1,4)/'~t1','~T1','A','Z'/
      DATA ( NAMES(1676,I),I=1,4)/'~t1','~T1','A','G'/
      DATA ( NAMES(1677,I),I=1,4)/'~t1','~T1','A','h'/
      DATA ( NAMES(1678,I),I=1,4)/'~t1','~T1','A','H'/
      DATA ( NAMES(1679,I),I=1,4)/'~t1','~T1','Z','Z'/
      DATA ( NAMES(1680,I),I=1,4)/'~t1','~T1','Z','G'/
      DATA ( NAMES(1681,I),I=1,4)/'~t1','~T1','Z','h'/
      DATA ( NAMES(1682,I),I=1,4)/'~t1','~T1','Z','H'/
      DATA ( NAMES(1683,I),I=1,4)/'~t1','~T1','Z','H3'/
      DATA ( NAMES(1684,I),I=1,4)/'~t1','~T1','W+','W-'/
      DATA ( NAMES(1685,I),I=1,4)/'~t1','~T1','W+','H-'/
      DATA ( NAMES(1686,I),I=1,4)/'~t1','~T1','W-','H+'/
      DATA ( NAMES(1687,I),I=1,4)/'~t1','~T1','G','G'/
      DATA ( NAMES(1688,I),I=1,4)/'~t1','~T1','G','h'/
      DATA ( NAMES(1689,I),I=1,4)/'~t1','~T1','G','H'/
      DATA ( NAMES(1690,I),I=1,4)/'~t1','~T1','n1','N1'/
      DATA ( NAMES(1691,I),I=1,4)/'~t1','~T1','n2','N2'/
      DATA ( NAMES(1692,I),I=1,4)/'~t1','~T1','n3','N3'/
      DATA ( NAMES(1693,I),I=1,4)/'~t1','~T1','e1','E1'/
      DATA ( NAMES(1694,I),I=1,4)/'~t1','~T1','e2','E2'/
      DATA ( NAMES(1695,I),I=1,4)/'~t1','~T1','e3','E3'/
      DATA ( NAMES(1696,I),I=1,4)/'~t1','~T1','u','U'/
      DATA ( NAMES(1697,I),I=1,4)/'~t1','~T1','d','D'/
      DATA ( NAMES(1698,I),I=1,4)/'~t1','~T1','d','S'/
      DATA ( NAMES(1699,I),I=1,4)/'~t1','~T1','d','B'/
      DATA ( NAMES(1700,I),I=1,4)/'~t1','~T1','D','s'/
      DATA ( NAMES(1701,I),I=1,4)/'~t1','~T1','D','b'/
      DATA ( NAMES(1702,I),I=1,4)/'~t1','~T1','c','C'/
      DATA ( NAMES(1703,I),I=1,4)/'~t1','~T1','s','S'/
      DATA ( NAMES(1704,I),I=1,4)/'~t1','~T1','s','B'/
      DATA ( NAMES(1705,I),I=1,4)/'~t1','~T1','S','b'/
      DATA ( NAMES(1706,I),I=1,4)/'~t1','~T1','t','T'/
      DATA ( NAMES(1707,I),I=1,4)/'~t1','~T1','b','B'/
      DATA ( NAMES(1708,I),I=1,4)/'~t1','~T1','h','h'/
      DATA ( NAMES(1709,I),I=1,4)/'~t1','~T1','h','H'/
      DATA ( NAMES(1710,I),I=1,4)/'~t1','~T1','h','H3'/
      DATA ( NAMES(1711,I),I=1,4)/'~t1','~T1','H','H'/
      DATA ( NAMES(1712,I),I=1,4)/'~t1','~T1','H','H3'/
      DATA ( NAMES(1713,I),I=1,4)/'~t1','~T1','H3','H3'/
      DATA ( NAMES(1714,I),I=1,4)/'~t1','~T1','H+','H-'/
      DATA ( NAMES(1715,I),I=1,4)/'~t1','~b1','u','d'/
      DATA ( NAMES(1716,I),I=1,4)/'~t1','~b1','u','s'/
      DATA ( NAMES(1717,I),I=1,4)/'~t1','~b1','u','b'/
      DATA ( NAMES(1718,I),I=1,4)/'~t1','~b1','d','c'/
      DATA ( NAMES(1719,I),I=1,4)/'~t1','~b1','d','t'/
      DATA ( NAMES(1720,I),I=1,4)/'~t1','~b1','c','s'/
      DATA ( NAMES(1721,I),I=1,4)/'~t1','~b1','c','b'/
      DATA ( NAMES(1722,I),I=1,4)/'~t1','~b1','s','t'/
      DATA ( NAMES(1723,I),I=1,4)/'~t1','~b1','t','b'/
      DATA ( NAMES(1724,I),I=1,4)/'~t1','~B1','A','W+'/
      DATA ( NAMES(1725,I),I=1,4)/'~t1','~B1','A','H+'/
      DATA ( NAMES(1726,I),I=1,4)/'~t1','~B1','Z','W+'/
      DATA ( NAMES(1727,I),I=1,4)/'~t1','~B1','Z','H+'/
      DATA ( NAMES(1728,I),I=1,4)/'~t1','~B1','W+','G'/
      DATA ( NAMES(1729,I),I=1,4)/'~t1','~B1','W+','h'/
      DATA ( NAMES(1730,I),I=1,4)/'~t1','~B1','W+','H'/
      DATA ( NAMES(1731,I),I=1,4)/'~t1','~B1','W+','H3'/
      DATA ( NAMES(1732,I),I=1,4)/'~t1','~B1','G','H+'/
      DATA ( NAMES(1733,I),I=1,4)/'~t1','~B1','n1','E1'/
      DATA ( NAMES(1734,I),I=1,4)/'~t1','~B1','n2','E2'/
      DATA ( NAMES(1735,I),I=1,4)/'~t1','~B1','n3','E3'/
      DATA ( NAMES(1736,I),I=1,4)/'~t1','~B1','u','D'/
      DATA ( NAMES(1737,I),I=1,4)/'~t1','~B1','u','S'/
      DATA ( NAMES(1738,I),I=1,4)/'~t1','~B1','u','B'/
      DATA ( NAMES(1739,I),I=1,4)/'~t1','~B1','D','c'/
      DATA ( NAMES(1740,I),I=1,4)/'~t1','~B1','D','t'/
      DATA ( NAMES(1741,I),I=1,4)/'~t1','~B1','c','S'/
      DATA ( NAMES(1742,I),I=1,4)/'~t1','~B1','c','B'/
      DATA ( NAMES(1743,I),I=1,4)/'~t1','~B1','S','t'/
      DATA ( NAMES(1744,I),I=1,4)/'~t1','~B1','t','B'/
      DATA ( NAMES(1745,I),I=1,4)/'~t1','~B1','h','H+'/
      DATA ( NAMES(1746,I),I=1,4)/'~t1','~B1','H','H+'/
      DATA ( NAMES(1747,I),I=1,4)/'~t1','~B1','H3','H+'/
      DATA ( NAMES(1748,I),I=1,4)/'~t1','~g','A','t'/
      DATA ( NAMES(1749,I),I=1,4)/'~t1','~g','Z','t'/
      DATA ( NAMES(1750,I),I=1,4)/'~t1','~g','W+','d'/
      DATA ( NAMES(1751,I),I=1,4)/'~t1','~g','W+','s'/
      DATA ( NAMES(1752,I),I=1,4)/'~t1','~g','W+','b'/
      DATA ( NAMES(1753,I),I=1,4)/'~t1','~g','G','t'/
      DATA ( NAMES(1754,I),I=1,4)/'~t1','~g','d','H+'/
      DATA ( NAMES(1755,I),I=1,4)/'~t1','~g','s','H+'/
      DATA ( NAMES(1756,I),I=1,4)/'~t1','~g','t','h'/
      DATA ( NAMES(1757,I),I=1,4)/'~t1','~g','t','H'/
      DATA ( NAMES(1758,I),I=1,4)/'~t1','~g','t','H3'/
      DATA ( NAMES(1759,I),I=1,4)/'~t1','~g','b','H+'/
      DATA ( NAMES(1760,I),I=1,4)/'~b1','~b1','b','b'/
      DATA ( NAMES(1761,I),I=1,4)/'~b1','~B1','A','A'/
      DATA ( NAMES(1762,I),I=1,4)/'~b1','~B1','A','Z'/
      DATA ( NAMES(1763,I),I=1,4)/'~b1','~B1','A','G'/
      DATA ( NAMES(1764,I),I=1,4)/'~b1','~B1','A','h'/
      DATA ( NAMES(1765,I),I=1,4)/'~b1','~B1','A','H'/
      DATA ( NAMES(1766,I),I=1,4)/'~b1','~B1','Z','Z'/
      DATA ( NAMES(1767,I),I=1,4)/'~b1','~B1','Z','G'/
      DATA ( NAMES(1768,I),I=1,4)/'~b1','~B1','Z','h'/
      DATA ( NAMES(1769,I),I=1,4)/'~b1','~B1','Z','H'/
      DATA ( NAMES(1770,I),I=1,4)/'~b1','~B1','Z','H3'/
      DATA ( NAMES(1771,I),I=1,4)/'~b1','~B1','W+','W-'/
      DATA ( NAMES(1772,I),I=1,4)/'~b1','~B1','W+','H-'/
      DATA ( NAMES(1773,I),I=1,4)/'~b1','~B1','W-','H+'/
      DATA ( NAMES(1774,I),I=1,4)/'~b1','~B1','G','G'/
      DATA ( NAMES(1775,I),I=1,4)/'~b1','~B1','G','h'/
      DATA ( NAMES(1776,I),I=1,4)/'~b1','~B1','G','H'/
      DATA ( NAMES(1777,I),I=1,4)/'~b1','~B1','n1','N1'/
      DATA ( NAMES(1778,I),I=1,4)/'~b1','~B1','n2','N2'/
      DATA ( NAMES(1779,I),I=1,4)/'~b1','~B1','n3','N3'/
      DATA ( NAMES(1780,I),I=1,4)/'~b1','~B1','e1','E1'/
      DATA ( NAMES(1781,I),I=1,4)/'~b1','~B1','e2','E2'/
      DATA ( NAMES(1782,I),I=1,4)/'~b1','~B1','e3','E3'/
      DATA ( NAMES(1783,I),I=1,4)/'~b1','~B1','u','U'/
      DATA ( NAMES(1784,I),I=1,4)/'~b1','~B1','u','C'/
      DATA ( NAMES(1785,I),I=1,4)/'~b1','~B1','u','T'/
      DATA ( NAMES(1786,I),I=1,4)/'~b1','~B1','U','c'/
      DATA ( NAMES(1787,I),I=1,4)/'~b1','~B1','U','t'/
      DATA ( NAMES(1788,I),I=1,4)/'~b1','~B1','d','D'/
      DATA ( NAMES(1789,I),I=1,4)/'~b1','~B1','c','C'/
      DATA ( NAMES(1790,I),I=1,4)/'~b1','~B1','c','T'/
      DATA ( NAMES(1791,I),I=1,4)/'~b1','~B1','C','t'/
      DATA ( NAMES(1792,I),I=1,4)/'~b1','~B1','s','S'/
      DATA ( NAMES(1793,I),I=1,4)/'~b1','~B1','t','T'/
      DATA ( NAMES(1794,I),I=1,4)/'~b1','~B1','b','B'/
      DATA ( NAMES(1795,I),I=1,4)/'~b1','~B1','h','h'/
      DATA ( NAMES(1796,I),I=1,4)/'~b1','~B1','h','H'/
      DATA ( NAMES(1797,I),I=1,4)/'~b1','~B1','h','H3'/
      DATA ( NAMES(1798,I),I=1,4)/'~b1','~B1','H','H'/
      DATA ( NAMES(1799,I),I=1,4)/'~b1','~B1','H','H3'/
      DATA ( NAMES(1800,I),I=1,4)/'~b1','~B1','H3','H3'/
      DATA ( NAMES(1801,I),I=1,4)/'~b1','~B1','H+','H-'/
      DATA ( NAMES(1802,I),I=1,4)/'~b1','~g','A','b'/
      DATA ( NAMES(1803,I),I=1,4)/'~b1','~g','Z','b'/
      DATA ( NAMES(1804,I),I=1,4)/'~b1','~g','W-','u'/
      DATA ( NAMES(1805,I),I=1,4)/'~b1','~g','W-','c'/
      DATA ( NAMES(1806,I),I=1,4)/'~b1','~g','W-','t'/
      DATA ( NAMES(1807,I),I=1,4)/'~b1','~g','G','b'/
      DATA ( NAMES(1808,I),I=1,4)/'~b1','~g','u','H-'/
      DATA ( NAMES(1809,I),I=1,4)/'~b1','~g','c','H-'/
      DATA ( NAMES(1810,I),I=1,4)/'~b1','~g','t','H-'/
      DATA ( NAMES(1811,I),I=1,4)/'~b1','~g','b','h'/
      DATA ( NAMES(1812,I),I=1,4)/'~b1','~g','b','H'/
      DATA ( NAMES(1813,I),I=1,4)/'~b1','~g','b','H3'/
      DATA ( NAMES(1814,I),I=1,4)/'~g','~g','G','G'/
      DATA ( NAMES(1815,I),I=1,4)/'~g','~g','u','U'/
      DATA ( NAMES(1816,I),I=1,4)/'~g','~g','d','D'/
      DATA ( NAMES(1817,I),I=1,4)/'~g','~g','c','C'/
      DATA ( NAMES(1818,I),I=1,4)/'~g','~g','s','S'/
      DATA ( NAMES(1819,I),I=1,4)/'~g','~g','t','T'/
      DATA ( NAMES(1820,I),I=1,4)/'~g','~g','b','B'/
      PINF=NAMES(NSUB,NPRTCL)
      RETURN
      END

      FUNCTION NVAR()
         NVAR=146
      RETURN
      END

      FUNCTION NFUN()
         NFUN=1654
      RETURN
      END

      SUBROUTINE VINF(NUMVAR,NAME,VAL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION VAL
      COMMON/VARS/A(1800)
      CHARACTER*6 NAMES(146),NAME
      SAVE
      DATA NAMES/'EE','SW','s12','s23','s13','MZ','MH3','Mm','Mt','Mc','
     >Ms','Mtop','Mb','wtop','wZ','wW','sa','MU','Zp11','Zp12','Zp21','Z
     >p22','Zm11','Zm12','Zm21','Zm22','Zn11','Zn12','Zn13','Zn14','Zn21
     >','Zn22','Zn23','Zn24','Zn31','Zn32','Zn33','Zn34','Zn41','Zn42','
     >Zn43','Zn44','Atau','Zl33','Zl36','Zl63','Zl66','Zl11','Zl14','Zl4
     >1','Zl44','Zl22','Zl25','Zl52','Zl55','Atop','Abot','Zu33','Zu36',
     >'Zu63','Zu66','Zu11','Zu14','Zu41','Zu44','Zu22','Zu25','Zu52','Zu
     >55','Zd33','Zd36','Zd63','Zd66','Zd11','Zd14','Zd41','Zd44','Zd22'
     >,'Zd25','Zd52','Zd55','Mh','wh','MHH','wHh','wH3','MHc','wHc','MC1
     >','wC1','MC2','wC2','MNE1','wNE1','MNE2','wNE2','MNE3','wNE3','MNE
     >4','wNE4','wSG','wSe1','wSe2','wSmu1','wSmu2','MStau1','wStau1','M
     >Stau2','wStau2','wSne','wSnmu','MSntau','wSntau','wSu1','wSu2','wS
     >d1','wSd2','wSc1','wSc2','wSs1','wSs2','MStop1','wStop1','MStop2',
     >'wStop2','MSbot1','wSbot1','MSbot2','wSbot2','tb','MSG','MSe1','MS
     >e2','MSmu1','MSmu2','MSne','MSnmu','MSu1','MSu2','MSd1','MSd2','MS
     >c1','MSc2','MSs1','MSs2','GG'/
      IF (NUMVAR.GT.146) RETURN
      NAME=NAMES(NUMVAR)
      VAL=A(NUMVAR)
      RETURN
      END

      SUBROUTINE ASGN(NUMVAR,VALNEW)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION VALNEW
      COMMON/VARS/A(1800)
      SAVE
      IF((NUMVAR.LT.1).OR.(NUMVAR.GT.146)) RETURN
      A(NUMVAR)=VALNEW
      RETURN
      END

      SUBROUTINE PMAS(NSUB,NPRTCL,VAL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION NVALUE(1820,4)
      COMMON/VARS/A(1800)
      SAVE
      DATA ( NVALUE(1,I),I=1,4)/93,93,6,6/
      DATA ( NVALUE(2,I),I=1,4)/93,93,6,82/
      DATA ( NVALUE(3,I),I=1,4)/93,93,6,84/
      DATA ( NVALUE(4,I),I=1,4)/93,93,6,7/
      DATA ( NVALUE(5,I),I=1,4)/93,93,151,151/
      DATA ( NVALUE(6,I),I=1,4)/93,93,151,87/
      DATA ( NVALUE(7,I),I=1,4)/93,93,151,87/
      DATA ( NVALUE(8,I),I=1,4)/93,93,0,0/
      DATA ( NVALUE(9,I),I=1,4)/93,93,0,0/
      DATA ( NVALUE(10,I),I=1,4)/93,93,0,0/
      DATA ( NVALUE(11,I),I=1,4)/93,93,0,0/
      DATA ( NVALUE(12,I),I=1,4)/93,93,8,8/
      DATA ( NVALUE(13,I),I=1,4)/93,93,9,9/
      DATA ( NVALUE(14,I),I=1,4)/93,93,0,0/
      DATA ( NVALUE(15,I),I=1,4)/93,93,0,0/
      DATA ( NVALUE(16,I),I=1,4)/93,93,10,10/
      DATA ( NVALUE(17,I),I=1,4)/93,93,11,11/
      DATA ( NVALUE(18,I),I=1,4)/93,93,12,12/
      DATA ( NVALUE(19,I),I=1,4)/93,93,13,13/
      DATA ( NVALUE(20,I),I=1,4)/93,93,82,82/
      DATA ( NVALUE(21,I),I=1,4)/93,93,82,84/
      DATA ( NVALUE(22,I),I=1,4)/93,93,82,7/
      DATA ( NVALUE(23,I),I=1,4)/93,93,84,84/
      DATA ( NVALUE(24,I),I=1,4)/93,93,84,7/
      DATA ( NVALUE(25,I),I=1,4)/93,93,7,7/
      DATA ( NVALUE(26,I),I=1,4)/93,93,87,87/
      DATA ( NVALUE(27,I),I=1,4)/93,95,6,6/
      DATA ( NVALUE(28,I),I=1,4)/93,95,6,82/
      DATA ( NVALUE(29,I),I=1,4)/93,95,6,84/
      DATA ( NVALUE(30,I),I=1,4)/93,95,6,7/
      DATA ( NVALUE(31,I),I=1,4)/93,95,151,151/
      DATA ( NVALUE(32,I),I=1,4)/93,95,151,87/
      DATA ( NVALUE(33,I),I=1,4)/93,95,151,87/
      DATA ( NVALUE(34,I),I=1,4)/93,95,0,0/
      DATA ( NVALUE(35,I),I=1,4)/93,95,0,0/
      DATA ( NVALUE(36,I),I=1,4)/93,95,0,0/
      DATA ( NVALUE(37,I),I=1,4)/93,95,0,0/
      DATA ( NVALUE(38,I),I=1,4)/93,95,8,8/
      DATA ( NVALUE(39,I),I=1,4)/93,95,9,9/
      DATA ( NVALUE(40,I),I=1,4)/93,95,0,0/
      DATA ( NVALUE(41,I),I=1,4)/93,95,0,0/
      DATA ( NVALUE(42,I),I=1,4)/93,95,10,10/
      DATA ( NVALUE(43,I),I=1,4)/93,95,11,11/
      DATA ( NVALUE(44,I),I=1,4)/93,95,12,12/
      DATA ( NVALUE(45,I),I=1,4)/93,95,13,13/
      DATA ( NVALUE(46,I),I=1,4)/93,95,82,82/
      DATA ( NVALUE(47,I),I=1,4)/93,95,82,84/
      DATA ( NVALUE(48,I),I=1,4)/93,95,82,7/
      DATA ( NVALUE(49,I),I=1,4)/93,95,84,84/
      DATA ( NVALUE(50,I),I=1,4)/93,95,84,7/
      DATA ( NVALUE(51,I),I=1,4)/93,95,7,7/
      DATA ( NVALUE(52,I),I=1,4)/93,95,87,87/
      DATA ( NVALUE(53,I),I=1,4)/93,89,0,151/
      DATA ( NVALUE(54,I),I=1,4)/93,89,0,87/
      DATA ( NVALUE(55,I),I=1,4)/93,89,6,151/
      DATA ( NVALUE(56,I),I=1,4)/93,89,6,87/
      DATA ( NVALUE(57,I),I=1,4)/93,89,151,82/
      DATA ( NVALUE(58,I),I=1,4)/93,89,151,84/
      DATA ( NVALUE(59,I),I=1,4)/93,89,151,7/
      DATA ( NVALUE(60,I),I=1,4)/93,89,0,0/
      DATA ( NVALUE(61,I),I=1,4)/93,89,0,8/
      DATA ( NVALUE(62,I),I=1,4)/93,89,0,9/
      DATA ( NVALUE(63,I),I=1,4)/93,89,0,0/
      DATA ( NVALUE(64,I),I=1,4)/93,89,0,11/
      DATA ( NVALUE(65,I),I=1,4)/93,89,0,13/
      DATA ( NVALUE(66,I),I=1,4)/93,89,0,10/
      DATA ( NVALUE(67,I),I=1,4)/93,89,0,12/
      DATA ( NVALUE(68,I),I=1,4)/93,89,10,11/
      DATA ( NVALUE(69,I),I=1,4)/93,89,10,13/
      DATA ( NVALUE(70,I),I=1,4)/93,89,11,12/
      DATA ( NVALUE(71,I),I=1,4)/93,89,12,13/
      DATA ( NVALUE(72,I),I=1,4)/93,89,82,87/
      DATA ( NVALUE(73,I),I=1,4)/93,89,84,87/
      DATA ( NVALUE(74,I),I=1,4)/93,89,7,87/
      DATA ( NVALUE(75,I),I=1,4)/93,132,0,0/
      DATA ( NVALUE(76,I),I=1,4)/93,132,6,0/
      DATA ( NVALUE(77,I),I=1,4)/93,132,151,0/
      DATA ( NVALUE(78,I),I=1,4)/93,132,0,87/
      DATA ( NVALUE(79,I),I=1,4)/93,132,0,82/
      DATA ( NVALUE(80,I),I=1,4)/93,132,0,84/
      DATA ( NVALUE(81,I),I=1,4)/93,132,0,7/
      DATA ( NVALUE(82,I),I=1,4)/93,134,0,8/
      DATA ( NVALUE(83,I),I=1,4)/93,134,6,8/
      DATA ( NVALUE(84,I),I=1,4)/93,134,151,0/
      DATA ( NVALUE(85,I),I=1,4)/93,134,0,87/
      DATA ( NVALUE(86,I),I=1,4)/93,134,8,82/
      DATA ( NVALUE(87,I),I=1,4)/93,134,8,84/
      DATA ( NVALUE(88,I),I=1,4)/93,134,8,7/
      DATA ( NVALUE(89,I),I=1,4)/93,106,0,9/
      DATA ( NVALUE(90,I),I=1,4)/93,106,6,9/
      DATA ( NVALUE(91,I),I=1,4)/93,106,151,0/
      DATA ( NVALUE(92,I),I=1,4)/93,106,0,87/
      DATA ( NVALUE(93,I),I=1,4)/93,106,9,82/
      DATA ( NVALUE(94,I),I=1,4)/93,106,9,84/
      DATA ( NVALUE(95,I),I=1,4)/93,106,9,7/
      DATA ( NVALUE(96,I),I=1,4)/93,136,6,0/
      DATA ( NVALUE(97,I),I=1,4)/93,136,151,0/
      DATA ( NVALUE(98,I),I=1,4)/93,136,0,82/
      DATA ( NVALUE(99,I),I=1,4)/93,136,0,84/
      DATA ( NVALUE(100,I),I=1,4)/93,136,0,7/
      DATA ( NVALUE(101,I),I=1,4)/93,136,0,87/
      DATA ( NVALUE(102,I),I=1,4)/93,137,6,0/
      DATA ( NVALUE(103,I),I=1,4)/93,137,151,8/
      DATA ( NVALUE(104,I),I=1,4)/93,137,0,82/
      DATA ( NVALUE(105,I),I=1,4)/93,137,0,84/
      DATA ( NVALUE(106,I),I=1,4)/93,137,0,7/
      DATA ( NVALUE(107,I),I=1,4)/93,137,8,87/
      DATA ( NVALUE(108,I),I=1,4)/93,112,6,0/
      DATA ( NVALUE(109,I),I=1,4)/93,112,151,9/
      DATA ( NVALUE(110,I),I=1,4)/93,112,0,82/
      DATA ( NVALUE(111,I),I=1,4)/93,112,0,84/
      DATA ( NVALUE(112,I),I=1,4)/93,112,0,7/
      DATA ( NVALUE(113,I),I=1,4)/93,112,9,87/
      DATA ( NVALUE(114,I),I=1,4)/93,138,0,0/
      DATA ( NVALUE(115,I),I=1,4)/93,138,6,0/
      DATA ( NVALUE(116,I),I=1,4)/93,138,151,0/
      DATA ( NVALUE(117,I),I=1,4)/93,138,151,11/
      DATA ( NVALUE(118,I),I=1,4)/93,138,151,13/
      DATA ( NVALUE(119,I),I=1,4)/93,138,0,0/
      DATA ( NVALUE(120,I),I=1,4)/93,138,0,82/
      DATA ( NVALUE(121,I),I=1,4)/93,138,0,84/
      DATA ( NVALUE(122,I),I=1,4)/93,138,0,7/
      DATA ( NVALUE(123,I),I=1,4)/93,138,0,87/
      DATA ( NVALUE(124,I),I=1,4)/93,138,11,87/
      DATA ( NVALUE(125,I),I=1,4)/93,138,13,87/
      DATA ( NVALUE(126,I),I=1,4)/93,140,0,0/
      DATA ( NVALUE(127,I),I=1,4)/93,140,6,0/
      DATA ( NVALUE(128,I),I=1,4)/93,140,151,0/
      DATA ( NVALUE(129,I),I=1,4)/93,140,151,10/
      DATA ( NVALUE(130,I),I=1,4)/93,140,151,12/
      DATA ( NVALUE(131,I),I=1,4)/93,140,0,0/
      DATA ( NVALUE(132,I),I=1,4)/93,140,0,87/
      DATA ( NVALUE(133,I),I=1,4)/93,140,0,82/
      DATA ( NVALUE(134,I),I=1,4)/93,140,0,84/
      DATA ( NVALUE(135,I),I=1,4)/93,140,0,7/
      DATA ( NVALUE(136,I),I=1,4)/93,140,10,87/
      DATA ( NVALUE(137,I),I=1,4)/93,140,12,87/
      DATA ( NVALUE(138,I),I=1,4)/93,142,0,10/
      DATA ( NVALUE(139,I),I=1,4)/93,142,6,10/
      DATA ( NVALUE(140,I),I=1,4)/93,142,151,0/
      DATA ( NVALUE(141,I),I=1,4)/93,142,151,11/
      DATA ( NVALUE(142,I),I=1,4)/93,142,151,13/
      DATA ( NVALUE(143,I),I=1,4)/93,142,0,10/
      DATA ( NVALUE(144,I),I=1,4)/93,142,0,87/
      DATA ( NVALUE(145,I),I=1,4)/93,142,10,82/
      DATA ( NVALUE(146,I),I=1,4)/93,142,10,84/
      DATA ( NVALUE(147,I),I=1,4)/93,142,10,7/
      DATA ( NVALUE(148,I),I=1,4)/93,142,11,87/
      DATA ( NVALUE(149,I),I=1,4)/93,142,13,87/
      DATA ( NVALUE(150,I),I=1,4)/93,144,0,11/
      DATA ( NVALUE(151,I),I=1,4)/93,144,6,11/
      DATA ( NVALUE(152,I),I=1,4)/93,144,151,0/
      DATA ( NVALUE(153,I),I=1,4)/93,144,151,10/
      DATA ( NVALUE(154,I),I=1,4)/93,144,151,12/
      DATA ( NVALUE(155,I),I=1,4)/93,144,0,11/
      DATA ( NVALUE(156,I),I=1,4)/93,144,0,87/
      DATA ( NVALUE(157,I),I=1,4)/93,144,10,87/
      DATA ( NVALUE(158,I),I=1,4)/93,144,11,82/
      DATA ( NVALUE(159,I),I=1,4)/93,144,11,84/
      DATA ( NVALUE(160,I),I=1,4)/93,144,11,7/
      DATA ( NVALUE(161,I),I=1,4)/93,144,12,87/
      DATA ( NVALUE(162,I),I=1,4)/93,122,0,12/
      DATA ( NVALUE(163,I),I=1,4)/93,122,6,12/
      DATA ( NVALUE(164,I),I=1,4)/93,122,151,0/
      DATA ( NVALUE(165,I),I=1,4)/93,122,151,11/
      DATA ( NVALUE(166,I),I=1,4)/93,122,151,13/
      DATA ( NVALUE(167,I),I=1,4)/93,122,0,12/
      DATA ( NVALUE(168,I),I=1,4)/93,122,0,87/
      DATA ( NVALUE(169,I),I=1,4)/93,122,11,87/
      DATA ( NVALUE(170,I),I=1,4)/93,122,12,82/
      DATA ( NVALUE(171,I),I=1,4)/93,122,12,84/
      DATA ( NVALUE(172,I),I=1,4)/93,122,12,7/
      DATA ( NVALUE(173,I),I=1,4)/93,122,13,87/
      DATA ( NVALUE(174,I),I=1,4)/93,126,0,13/
      DATA ( NVALUE(175,I),I=1,4)/93,126,6,13/
      DATA ( NVALUE(176,I),I=1,4)/93,126,151,0/
      DATA ( NVALUE(177,I),I=1,4)/93,126,151,10/
      DATA ( NVALUE(178,I),I=1,4)/93,126,151,12/
      DATA ( NVALUE(179,I),I=1,4)/93,126,0,13/
      DATA ( NVALUE(180,I),I=1,4)/93,126,0,87/
      DATA ( NVALUE(181,I),I=1,4)/93,126,10,87/
      DATA ( NVALUE(182,I),I=1,4)/93,126,12,87/
      DATA ( NVALUE(183,I),I=1,4)/93,126,13,82/
      DATA ( NVALUE(184,I),I=1,4)/93,126,13,84/
      DATA ( NVALUE(185,I),I=1,4)/93,126,13,7/
      DATA ( NVALUE(186,I),I=1,4)/93,131,0,0/
      DATA ( NVALUE(187,I),I=1,4)/93,131,0,0/
      DATA ( NVALUE(188,I),I=1,4)/93,131,10,10/
      DATA ( NVALUE(189,I),I=1,4)/93,131,11,11/
      DATA ( NVALUE(190,I),I=1,4)/93,131,12,12/
      DATA ( NVALUE(191,I),I=1,4)/93,131,13,13/
      DATA ( NVALUE(192,I),I=1,4)/95,95,6,6/
      DATA ( NVALUE(193,I),I=1,4)/95,95,6,82/
      DATA ( NVALUE(194,I),I=1,4)/95,95,6,84/
      DATA ( NVALUE(195,I),I=1,4)/95,95,6,7/
      DATA ( NVALUE(196,I),I=1,4)/95,95,151,151/
      DATA ( NVALUE(197,I),I=1,4)/95,95,151,87/
      DATA ( NVALUE(198,I),I=1,4)/95,95,151,87/
      DATA ( NVALUE(199,I),I=1,4)/95,95,0,0/
      DATA ( NVALUE(200,I),I=1,4)/95,95,0,0/
      DATA ( NVALUE(201,I),I=1,4)/95,95,0,0/
      DATA ( NVALUE(202,I),I=1,4)/95,95,0,0/
      DATA ( NVALUE(203,I),I=1,4)/95,95,8,8/
      DATA ( NVALUE(204,I),I=1,4)/95,95,9,9/
      DATA ( NVALUE(205,I),I=1,4)/95,95,0,0/
      DATA ( NVALUE(206,I),I=1,4)/95,95,0,0/
      DATA ( NVALUE(207,I),I=1,4)/95,95,10,10/
      DATA ( NVALUE(208,I),I=1,4)/95,95,11,11/
      DATA ( NVALUE(209,I),I=1,4)/95,95,12,12/
      DATA ( NVALUE(210,I),I=1,4)/95,95,13,13/
      DATA ( NVALUE(211,I),I=1,4)/95,95,82,82/
      DATA ( NVALUE(212,I),I=1,4)/95,95,82,84/
      DATA ( NVALUE(213,I),I=1,4)/95,95,82,7/
      DATA ( NVALUE(214,I),I=1,4)/95,95,84,84/
      DATA ( NVALUE(215,I),I=1,4)/95,95,84,7/
      DATA ( NVALUE(216,I),I=1,4)/95,95,7,7/
      DATA ( NVALUE(217,I),I=1,4)/95,95,87,87/
      DATA ( NVALUE(218,I),I=1,4)/95,89,0,151/
      DATA ( NVALUE(219,I),I=1,4)/95,89,0,87/
      DATA ( NVALUE(220,I),I=1,4)/95,89,6,151/
      DATA ( NVALUE(221,I),I=1,4)/95,89,6,87/
      DATA ( NVALUE(222,I),I=1,4)/95,89,151,82/
      DATA ( NVALUE(223,I),I=1,4)/95,89,151,84/
      DATA ( NVALUE(224,I),I=1,4)/95,89,151,7/
      DATA ( NVALUE(225,I),I=1,4)/95,89,0,0/
      DATA ( NVALUE(226,I),I=1,4)/95,89,0,8/
      DATA ( NVALUE(227,I),I=1,4)/95,89,0,9/
      DATA ( NVALUE(228,I),I=1,4)/95,89,0,0/
      DATA ( NVALUE(229,I),I=1,4)/95,89,0,11/
      DATA ( NVALUE(230,I),I=1,4)/95,89,0,13/
      DATA ( NVALUE(231,I),I=1,4)/95,89,0,10/
      DATA ( NVALUE(232,I),I=1,4)/95,89,0,12/
      DATA ( NVALUE(233,I),I=1,4)/95,89,10,11/
      DATA ( NVALUE(234,I),I=1,4)/95,89,10,13/
      DATA ( NVALUE(235,I),I=1,4)/95,89,11,12/
      DATA ( NVALUE(236,I),I=1,4)/95,89,12,13/
      DATA ( NVALUE(237,I),I=1,4)/95,89,82,87/
      DATA ( NVALUE(238,I),I=1,4)/95,89,84,87/
      DATA ( NVALUE(239,I),I=1,4)/95,89,7,87/
      DATA ( NVALUE(240,I),I=1,4)/95,132,0,0/
      DATA ( NVALUE(241,I),I=1,4)/95,132,6,0/
      DATA ( NVALUE(242,I),I=1,4)/95,132,151,0/
      DATA ( NVALUE(243,I),I=1,4)/95,132,0,87/
      DATA ( NVALUE(244,I),I=1,4)/95,132,0,82/
      DATA ( NVALUE(245,I),I=1,4)/95,132,0,84/
      DATA ( NVALUE(246,I),I=1,4)/95,132,0,7/
      DATA ( NVALUE(247,I),I=1,4)/95,134,0,8/
      DATA ( NVALUE(248,I),I=1,4)/95,134,6,8/
      DATA ( NVALUE(249,I),I=1,4)/95,134,151,0/
      DATA ( NVALUE(250,I),I=1,4)/95,134,0,87/
      DATA ( NVALUE(251,I),I=1,4)/95,134,8,82/
      DATA ( NVALUE(252,I),I=1,4)/95,134,8,84/
      DATA ( NVALUE(253,I),I=1,4)/95,134,8,7/
      DATA ( NVALUE(254,I),I=1,4)/95,106,0,9/
      DATA ( NVALUE(255,I),I=1,4)/95,106,6,9/
      DATA ( NVALUE(256,I),I=1,4)/95,106,151,0/
      DATA ( NVALUE(257,I),I=1,4)/95,106,0,87/
      DATA ( NVALUE(258,I),I=1,4)/95,106,9,82/
      DATA ( NVALUE(259,I),I=1,4)/95,106,9,84/
      DATA ( NVALUE(260,I),I=1,4)/95,106,9,7/
      DATA ( NVALUE(261,I),I=1,4)/95,136,6,0/
      DATA ( NVALUE(262,I),I=1,4)/95,136,151,0/
      DATA ( NVALUE(263,I),I=1,4)/95,136,0,82/
      DATA ( NVALUE(264,I),I=1,4)/95,136,0,84/
      DATA ( NVALUE(265,I),I=1,4)/95,136,0,7/
      DATA ( NVALUE(266,I),I=1,4)/95,136,0,87/
      DATA ( NVALUE(267,I),I=1,4)/95,137,6,0/
      DATA ( NVALUE(268,I),I=1,4)/95,137,151,8/
      DATA ( NVALUE(269,I),I=1,4)/95,137,0,82/
      DATA ( NVALUE(270,I),I=1,4)/95,137,0,84/
      DATA ( NVALUE(271,I),I=1,4)/95,137,0,7/
      DATA ( NVALUE(272,I),I=1,4)/95,137,8,87/
      DATA ( NVALUE(273,I),I=1,4)/95,112,6,0/
      DATA ( NVALUE(274,I),I=1,4)/95,112,151,9/
      DATA ( NVALUE(275,I),I=1,4)/95,112,0,82/
      DATA ( NVALUE(276,I),I=1,4)/95,112,0,84/
      DATA ( NVALUE(277,I),I=1,4)/95,112,0,7/
      DATA ( NVALUE(278,I),I=1,4)/95,112,9,87/
      DATA ( NVALUE(279,I),I=1,4)/95,138,0,0/
      DATA ( NVALUE(280,I),I=1,4)/95,138,6,0/
      DATA ( NVALUE(281,I),I=1,4)/95,138,151,0/
      DATA ( NVALUE(282,I),I=1,4)/95,138,151,11/
      DATA ( NVALUE(283,I),I=1,4)/95,138,151,13/
      DATA ( NVALUE(284,I),I=1,4)/95,138,0,0/
      DATA ( NVALUE(285,I),I=1,4)/95,138,0,82/
      DATA ( NVALUE(286,I),I=1,4)/95,138,0,84/
      DATA ( NVALUE(287,I),I=1,4)/95,138,0,7/
      DATA ( NVALUE(288,I),I=1,4)/95,138,0,87/
      DATA ( NVALUE(289,I),I=1,4)/95,138,11,87/
      DATA ( NVALUE(290,I),I=1,4)/95,138,13,87/
      DATA ( NVALUE(291,I),I=1,4)/95,140,0,0/
      DATA ( NVALUE(292,I),I=1,4)/95,140,6,0/
      DATA ( NVALUE(293,I),I=1,4)/95,140,151,0/
      DATA ( NVALUE(294,I),I=1,4)/95,140,151,10/
      DATA ( NVALUE(295,I),I=1,4)/95,140,151,12/
      DATA ( NVALUE(296,I),I=1,4)/95,140,0,0/
      DATA ( NVALUE(297,I),I=1,4)/95,140,0,87/
      DATA ( NVALUE(298,I),I=1,4)/95,140,0,82/
      DATA ( NVALUE(299,I),I=1,4)/95,140,0,84/
      DATA ( NVALUE(300,I),I=1,4)/95,140,0,7/
      DATA ( NVALUE(301,I),I=1,4)/95,140,10,87/
      DATA ( NVALUE(302,I),I=1,4)/95,140,12,87/
      DATA ( NVALUE(303,I),I=1,4)/95,142,0,10/
      DATA ( NVALUE(304,I),I=1,4)/95,142,6,10/
      DATA ( NVALUE(305,I),I=1,4)/95,142,151,0/
      DATA ( NVALUE(306,I),I=1,4)/95,142,151,11/
      DATA ( NVALUE(307,I),I=1,4)/95,142,151,13/
      DATA ( NVALUE(308,I),I=1,4)/95,142,0,10/
      DATA ( NVALUE(309,I),I=1,4)/95,142,0,87/
      DATA ( NVALUE(310,I),I=1,4)/95,142,10,82/
      DATA ( NVALUE(311,I),I=1,4)/95,142,10,84/
      DATA ( NVALUE(312,I),I=1,4)/95,142,10,7/
      DATA ( NVALUE(313,I),I=1,4)/95,142,11,87/
      DATA ( NVALUE(314,I),I=1,4)/95,142,13,87/
      DATA ( NVALUE(315,I),I=1,4)/95,144,0,11/
      DATA ( NVALUE(316,I),I=1,4)/95,144,6,11/
      DATA ( NVALUE(317,I),I=1,4)/95,144,151,0/
      DATA ( NVALUE(318,I),I=1,4)/95,144,151,10/
      DATA ( NVALUE(319,I),I=1,4)/95,144,151,12/
      DATA ( NVALUE(320,I),I=1,4)/95,144,0,11/
      DATA ( NVALUE(321,I),I=1,4)/95,144,0,87/
      DATA ( NVALUE(322,I),I=1,4)/95,144,10,87/
      DATA ( NVALUE(323,I),I=1,4)/95,144,11,82/
      DATA ( NVALUE(324,I),I=1,4)/95,144,11,84/
      DATA ( NVALUE(325,I),I=1,4)/95,144,11,7/
      DATA ( NVALUE(326,I),I=1,4)/95,144,12,87/
      DATA ( NVALUE(327,I),I=1,4)/95,122,0,12/
      DATA ( NVALUE(328,I),I=1,4)/95,122,6,12/
      DATA ( NVALUE(329,I),I=1,4)/95,122,151,0/
      DATA ( NVALUE(330,I),I=1,4)/95,122,151,11/
      DATA ( NVALUE(331,I),I=1,4)/95,122,151,13/
      DATA ( NVALUE(332,I),I=1,4)/95,122,0,12/
      DATA ( NVALUE(333,I),I=1,4)/95,122,0,87/
      DATA ( NVALUE(334,I),I=1,4)/95,122,11,87/
      DATA ( NVALUE(335,I),I=1,4)/95,122,12,82/
      DATA ( NVALUE(336,I),I=1,4)/95,122,12,84/
      DATA ( NVALUE(337,I),I=1,4)/95,122,12,7/
      DATA ( NVALUE(338,I),I=1,4)/95,122,13,87/
      DATA ( NVALUE(339,I),I=1,4)/95,126,0,13/
      DATA ( NVALUE(340,I),I=1,4)/95,126,6,13/
      DATA ( NVALUE(341,I),I=1,4)/95,126,151,0/
      DATA ( NVALUE(342,I),I=1,4)/95,126,151,10/
      DATA ( NVALUE(343,I),I=1,4)/95,126,151,12/
      DATA ( NVALUE(344,I),I=1,4)/95,126,0,13/
      DATA ( NVALUE(345,I),I=1,4)/95,126,0,87/
      DATA ( NVALUE(346,I),I=1,4)/95,126,10,87/
      DATA ( NVALUE(347,I),I=1,4)/95,126,12,87/
      DATA ( NVALUE(348,I),I=1,4)/95,126,13,82/
      DATA ( NVALUE(349,I),I=1,4)/95,126,13,84/
      DATA ( NVALUE(350,I),I=1,4)/95,126,13,7/
      DATA ( NVALUE(351,I),I=1,4)/95,131,0,0/
      DATA ( NVALUE(352,I),I=1,4)/95,131,0,0/
      DATA ( NVALUE(353,I),I=1,4)/95,131,10,10/
      DATA ( NVALUE(354,I),I=1,4)/95,131,11,11/
      DATA ( NVALUE(355,I),I=1,4)/95,131,12,12/
      DATA ( NVALUE(356,I),I=1,4)/95,131,13,13/
      DATA ( NVALUE(357,I),I=1,4)/89,89,151,151/
      DATA ( NVALUE(358,I),I=1,4)/89,89,151,87/
      DATA ( NVALUE(359,I),I=1,4)/89,89,87,87/
      DATA ( NVALUE(360,I),I=1,4)/89,89,0,0/
      DATA ( NVALUE(361,I),I=1,4)/89,89,0,6/
      DATA ( NVALUE(362,I),I=1,4)/89,89,0,82/
      DATA ( NVALUE(363,I),I=1,4)/89,89,0,84/
      DATA ( NVALUE(364,I),I=1,4)/89,89,0,7/
      DATA ( NVALUE(365,I),I=1,4)/89,89,6,6/
      DATA ( NVALUE(366,I),I=1,4)/89,89,6,82/
      DATA ( NVALUE(367,I),I=1,4)/89,89,6,84/
      DATA ( NVALUE(368,I),I=1,4)/89,89,6,7/
      DATA ( NVALUE(369,I),I=1,4)/89,89,151,151/
      DATA ( NVALUE(370,I),I=1,4)/89,89,151,87/
      DATA ( NVALUE(371,I),I=1,4)/89,89,151,87/
      DATA ( NVALUE(372,I),I=1,4)/89,89,0,0/
      DATA ( NVALUE(373,I),I=1,4)/89,89,0,0/
      DATA ( NVALUE(374,I),I=1,4)/89,89,0,0/
      DATA ( NVALUE(375,I),I=1,4)/89,89,0,0/
      DATA ( NVALUE(376,I),I=1,4)/89,89,8,8/
      DATA ( NVALUE(377,I),I=1,4)/89,89,9,9/
      DATA ( NVALUE(378,I),I=1,4)/89,89,0,0/
      DATA ( NVALUE(379,I),I=1,4)/89,89,0,10/
      DATA ( NVALUE(380,I),I=1,4)/89,89,0,12/
      DATA ( NVALUE(381,I),I=1,4)/89,89,0,10/
      DATA ( NVALUE(382,I),I=1,4)/89,89,0,12/
      DATA ( NVALUE(383,I),I=1,4)/89,89,0,0/
      DATA ( NVALUE(384,I),I=1,4)/89,89,0,11/
      DATA ( NVALUE(385,I),I=1,4)/89,89,0,13/
      DATA ( NVALUE(386,I),I=1,4)/89,89,0,11/
      DATA ( NVALUE(387,I),I=1,4)/89,89,0,13/
      DATA ( NVALUE(388,I),I=1,4)/89,89,10,10/
      DATA ( NVALUE(389,I),I=1,4)/89,89,10,12/
      DATA ( NVALUE(390,I),I=1,4)/89,89,10,12/
      DATA ( NVALUE(391,I),I=1,4)/89,89,11,11/
      DATA ( NVALUE(392,I),I=1,4)/89,89,11,13/
      DATA ( NVALUE(393,I),I=1,4)/89,89,11,13/
      DATA ( NVALUE(394,I),I=1,4)/89,89,12,12/
      DATA ( NVALUE(395,I),I=1,4)/89,89,13,13/
      DATA ( NVALUE(396,I),I=1,4)/89,89,82,82/
      DATA ( NVALUE(397,I),I=1,4)/89,89,82,84/
      DATA ( NVALUE(398,I),I=1,4)/89,89,82,7/
      DATA ( NVALUE(399,I),I=1,4)/89,89,84,84/
      DATA ( NVALUE(400,I),I=1,4)/89,89,84,7/
      DATA ( NVALUE(401,I),I=1,4)/89,89,7,7/
      DATA ( NVALUE(402,I),I=1,4)/89,89,87,87/
      DATA ( NVALUE(403,I),I=1,4)/89,132,0,0/
      DATA ( NVALUE(404,I),I=1,4)/89,132,6,0/
      DATA ( NVALUE(405,I),I=1,4)/89,132,151,0/
      DATA ( NVALUE(406,I),I=1,4)/89,132,0,82/
      DATA ( NVALUE(407,I),I=1,4)/89,132,0,84/
      DATA ( NVALUE(408,I),I=1,4)/89,132,0,7/
      DATA ( NVALUE(409,I),I=1,4)/89,132,0,87/
      DATA ( NVALUE(410,I),I=1,4)/89,132,151,0/
      DATA ( NVALUE(411,I),I=1,4)/89,132,0,87/
      DATA ( NVALUE(412,I),I=1,4)/89,134,0,0/
      DATA ( NVALUE(413,I),I=1,4)/89,134,6,0/
      DATA ( NVALUE(414,I),I=1,4)/89,134,151,8/
      DATA ( NVALUE(415,I),I=1,4)/89,134,0,82/
      DATA ( NVALUE(416,I),I=1,4)/89,134,0,84/
      DATA ( NVALUE(417,I),I=1,4)/89,134,0,7/
      DATA ( NVALUE(418,I),I=1,4)/89,134,8,87/
      DATA ( NVALUE(419,I),I=1,4)/89,134,151,8/
      DATA ( NVALUE(420,I),I=1,4)/89,134,8,87/
      DATA ( NVALUE(421,I),I=1,4)/89,106,0,0/
      DATA ( NVALUE(422,I),I=1,4)/89,106,6,0/
      DATA ( NVALUE(423,I),I=1,4)/89,106,151,9/
      DATA ( NVALUE(424,I),I=1,4)/89,106,0,82/
      DATA ( NVALUE(425,I),I=1,4)/89,106,0,84/
      DATA ( NVALUE(426,I),I=1,4)/89,106,0,7/
      DATA ( NVALUE(427,I),I=1,4)/89,106,9,87/
      DATA ( NVALUE(428,I),I=1,4)/89,106,151,9/
      DATA ( NVALUE(429,I),I=1,4)/89,106,9,87/
      DATA ( NVALUE(430,I),I=1,4)/89,136,151,0/
      DATA ( NVALUE(431,I),I=1,4)/89,136,0,87/
      DATA ( NVALUE(432,I),I=1,4)/89,136,0,0/
      DATA ( NVALUE(433,I),I=1,4)/89,136,6,0/
      DATA ( NVALUE(434,I),I=1,4)/89,136,151,0/
      DATA ( NVALUE(435,I),I=1,4)/89,136,0,87/
      DATA ( NVALUE(436,I),I=1,4)/89,136,0,82/
      DATA ( NVALUE(437,I),I=1,4)/89,136,0,84/
      DATA ( NVALUE(438,I),I=1,4)/89,136,0,7/
      DATA ( NVALUE(439,I),I=1,4)/89,137,151,0/
      DATA ( NVALUE(440,I),I=1,4)/89,137,0,87/
      DATA ( NVALUE(441,I),I=1,4)/89,137,0,8/
      DATA ( NVALUE(442,I),I=1,4)/89,137,6,8/
      DATA ( NVALUE(443,I),I=1,4)/89,137,151,0/
      DATA ( NVALUE(444,I),I=1,4)/89,137,0,87/
      DATA ( NVALUE(445,I),I=1,4)/89,137,8,82/
      DATA ( NVALUE(446,I),I=1,4)/89,137,8,84/
      DATA ( NVALUE(447,I),I=1,4)/89,137,8,7/
      DATA ( NVALUE(448,I),I=1,4)/89,112,151,0/
      DATA ( NVALUE(449,I),I=1,4)/89,112,0,87/
      DATA ( NVALUE(450,I),I=1,4)/89,112,0,9/
      DATA ( NVALUE(451,I),I=1,4)/89,112,6,9/
      DATA ( NVALUE(452,I),I=1,4)/89,112,151,0/
      DATA ( NVALUE(453,I),I=1,4)/89,112,0,87/
      DATA ( NVALUE(454,I),I=1,4)/89,112,9,82/
      DATA ( NVALUE(455,I),I=1,4)/89,112,9,84/
      DATA ( NVALUE(456,I),I=1,4)/89,112,9,7/
      DATA ( NVALUE(457,I),I=1,4)/89,138,151,0/
      DATA ( NVALUE(458,I),I=1,4)/89,138,151,10/
      DATA ( NVALUE(459,I),I=1,4)/89,138,151,12/
      DATA ( NVALUE(460,I),I=1,4)/89,138,0,87/
      DATA ( NVALUE(461,I),I=1,4)/89,138,10,87/
      DATA ( NVALUE(462,I),I=1,4)/89,138,12,87/
      DATA ( NVALUE(463,I),I=1,4)/89,138,0,0/
      DATA ( NVALUE(464,I),I=1,4)/89,138,0,11/
      DATA ( NVALUE(465,I),I=1,4)/89,138,0,13/
      DATA ( NVALUE(466,I),I=1,4)/89,138,6,0/
      DATA ( NVALUE(467,I),I=1,4)/89,138,6,11/
      DATA ( NVALUE(468,I),I=1,4)/89,138,6,13/
      DATA ( NVALUE(469,I),I=1,4)/89,138,151,0/
      DATA ( NVALUE(470,I),I=1,4)/89,138,151,10/
      DATA ( NVALUE(471,I),I=1,4)/89,138,151,12/
      DATA ( NVALUE(472,I),I=1,4)/89,138,0,0/
      DATA ( NVALUE(473,I),I=1,4)/89,138,0,11/
      DATA ( NVALUE(474,I),I=1,4)/89,138,0,13/
      DATA ( NVALUE(475,I),I=1,4)/89,138,0,87/
      DATA ( NVALUE(476,I),I=1,4)/89,138,0,82/
      DATA ( NVALUE(477,I),I=1,4)/89,138,0,84/
      DATA ( NVALUE(478,I),I=1,4)/89,138,0,7/
      DATA ( NVALUE(479,I),I=1,4)/89,138,10,87/
      DATA ( NVALUE(480,I),I=1,4)/89,138,11,82/
      DATA ( NVALUE(481,I),I=1,4)/89,138,11,84/
      DATA ( NVALUE(482,I),I=1,4)/89,138,11,7/
      DATA ( NVALUE(483,I),I=1,4)/89,138,12,87/
      DATA ( NVALUE(484,I),I=1,4)/89,138,13,82/
      DATA ( NVALUE(485,I),I=1,4)/89,138,13,84/
      DATA ( NVALUE(486,I),I=1,4)/89,138,13,7/
      DATA ( NVALUE(487,I),I=1,4)/89,140,0,0/
      DATA ( NVALUE(488,I),I=1,4)/89,140,0,10/
      DATA ( NVALUE(489,I),I=1,4)/89,140,0,12/
      DATA ( NVALUE(490,I),I=1,4)/89,140,6,0/
      DATA ( NVALUE(491,I),I=1,4)/89,140,6,10/
      DATA ( NVALUE(492,I),I=1,4)/89,140,6,12/
      DATA ( NVALUE(493,I),I=1,4)/89,140,151,0/
      DATA ( NVALUE(494,I),I=1,4)/89,140,151,11/
      DATA ( NVALUE(495,I),I=1,4)/89,140,151,13/
      DATA ( NVALUE(496,I),I=1,4)/89,140,0,0/
      DATA ( NVALUE(497,I),I=1,4)/89,140,0,10/
      DATA ( NVALUE(498,I),I=1,4)/89,140,0,12/
      DATA ( NVALUE(499,I),I=1,4)/89,140,0,82/
      DATA ( NVALUE(500,I),I=1,4)/89,140,0,84/
      DATA ( NVALUE(501,I),I=1,4)/89,140,0,7/
      DATA ( NVALUE(502,I),I=1,4)/89,140,0,87/
      DATA ( NVALUE(503,I),I=1,4)/89,140,10,82/
      DATA ( NVALUE(504,I),I=1,4)/89,140,10,84/
      DATA ( NVALUE(505,I),I=1,4)/89,140,10,7/
      DATA ( NVALUE(506,I),I=1,4)/89,140,11,87/
      DATA ( NVALUE(507,I),I=1,4)/89,140,12,82/
      DATA ( NVALUE(508,I),I=1,4)/89,140,12,84/
      DATA ( NVALUE(509,I),I=1,4)/89,140,12,7/
      DATA ( NVALUE(510,I),I=1,4)/89,140,13,87/
      DATA ( NVALUE(511,I),I=1,4)/89,140,151,0/
      DATA ( NVALUE(512,I),I=1,4)/89,140,151,11/
      DATA ( NVALUE(513,I),I=1,4)/89,140,151,13/
      DATA ( NVALUE(514,I),I=1,4)/89,140,0,87/
      DATA ( NVALUE(515,I),I=1,4)/89,140,11,87/
      DATA ( NVALUE(516,I),I=1,4)/89,140,13,87/
      DATA ( NVALUE(517,I),I=1,4)/89,142,151,0/
      DATA ( NVALUE(518,I),I=1,4)/89,142,151,10/
      DATA ( NVALUE(519,I),I=1,4)/89,142,151,12/
      DATA ( NVALUE(520,I),I=1,4)/89,142,0,87/
      DATA ( NVALUE(521,I),I=1,4)/89,142,10,87/
      DATA ( NVALUE(522,I),I=1,4)/89,142,12,87/
      DATA ( NVALUE(523,I),I=1,4)/89,142,0,0/
      DATA ( NVALUE(524,I),I=1,4)/89,142,0,11/
      DATA ( NVALUE(525,I),I=1,4)/89,142,0,13/
      DATA ( NVALUE(526,I),I=1,4)/89,142,6,0/
      DATA ( NVALUE(527,I),I=1,4)/89,142,6,11/
      DATA ( NVALUE(528,I),I=1,4)/89,142,6,13/
      DATA ( NVALUE(529,I),I=1,4)/89,142,151,0/
      DATA ( NVALUE(530,I),I=1,4)/89,142,151,10/
      DATA ( NVALUE(531,I),I=1,4)/89,142,151,12/
      DATA ( NVALUE(532,I),I=1,4)/89,142,0,0/
      DATA ( NVALUE(533,I),I=1,4)/89,142,0,11/
      DATA ( NVALUE(534,I),I=1,4)/89,142,0,13/
      DATA ( NVALUE(535,I),I=1,4)/89,142,0,87/
      DATA ( NVALUE(536,I),I=1,4)/89,142,0,82/
      DATA ( NVALUE(537,I),I=1,4)/89,142,0,84/
      DATA ( NVALUE(538,I),I=1,4)/89,142,0,7/
      DATA ( NVALUE(539,I),I=1,4)/89,142,10,87/
      DATA ( NVALUE(540,I),I=1,4)/89,142,11,82/
      DATA ( NVALUE(541,I),I=1,4)/89,142,11,84/
      DATA ( NVALUE(542,I),I=1,4)/89,142,11,7/
      DATA ( NVALUE(543,I),I=1,4)/89,142,12,87/
      DATA ( NVALUE(544,I),I=1,4)/89,142,13,82/
      DATA ( NVALUE(545,I),I=1,4)/89,142,13,84/
      DATA ( NVALUE(546,I),I=1,4)/89,142,13,7/
      DATA ( NVALUE(547,I),I=1,4)/89,144,0,0/
      DATA ( NVALUE(548,I),I=1,4)/89,144,0,10/
      DATA ( NVALUE(549,I),I=1,4)/89,144,0,12/
      DATA ( NVALUE(550,I),I=1,4)/89,144,6,0/
      DATA ( NVALUE(551,I),I=1,4)/89,144,6,10/
      DATA ( NVALUE(552,I),I=1,4)/89,144,6,12/
      DATA ( NVALUE(553,I),I=1,4)/89,144,151,0/
      DATA ( NVALUE(554,I),I=1,4)/89,144,151,11/
      DATA ( NVALUE(555,I),I=1,4)/89,144,151,13/
      DATA ( NVALUE(556,I),I=1,4)/89,144,0,0/
      DATA ( NVALUE(557,I),I=1,4)/89,144,0,10/
      DATA ( NVALUE(558,I),I=1,4)/89,144,0,12/
      DATA ( NVALUE(559,I),I=1,4)/89,144,0,82/
      DATA ( NVALUE(560,I),I=1,4)/89,144,0,84/
      DATA ( NVALUE(561,I),I=1,4)/89,144,0,7/
      DATA ( NVALUE(562,I),I=1,4)/89,144,0,87/
      DATA ( NVALUE(563,I),I=1,4)/89,144,10,82/
      DATA ( NVALUE(564,I),I=1,4)/89,144,10,84/
      DATA ( NVALUE(565,I),I=1,4)/89,144,10,7/
      DATA ( NVALUE(566,I),I=1,4)/89,144,11,87/
      DATA ( NVALUE(567,I),I=1,4)/89,144,12,82/
      DATA ( NVALUE(568,I),I=1,4)/89,144,12,84/
      DATA ( NVALUE(569,I),I=1,4)/89,144,12,7/
      DATA ( NVALUE(570,I),I=1,4)/89,144,13,87/
      DATA ( NVALUE(571,I),I=1,4)/89,144,151,0/
      DATA ( NVALUE(572,I),I=1,4)/89,144,151,11/
      DATA ( NVALUE(573,I),I=1,4)/89,144,151,13/
      DATA ( NVALUE(574,I),I=1,4)/89,144,0,87/
      DATA ( NVALUE(575,I),I=1,4)/89,144,11,87/
      DATA ( NVALUE(576,I),I=1,4)/89,144,13,87/
      DATA ( NVALUE(577,I),I=1,4)/89,122,151,0/
      DATA ( NVALUE(578,I),I=1,4)/89,122,151,10/
      DATA ( NVALUE(579,I),I=1,4)/89,122,151,12/
      DATA ( NVALUE(580,I),I=1,4)/89,122,0,87/
      DATA ( NVALUE(581,I),I=1,4)/89,122,10,87/
      DATA ( NVALUE(582,I),I=1,4)/89,122,12,87/
      DATA ( NVALUE(583,I),I=1,4)/89,122,0,0/
      DATA ( NVALUE(584,I),I=1,4)/89,122,0,11/
      DATA ( NVALUE(585,I),I=1,4)/89,122,0,13/
      DATA ( NVALUE(586,I),I=1,4)/89,122,6,0/
      DATA ( NVALUE(587,I),I=1,4)/89,122,6,11/
      DATA ( NVALUE(588,I),I=1,4)/89,122,6,13/
      DATA ( NVALUE(589,I),I=1,4)/89,122,151,0/
      DATA ( NVALUE(590,I),I=1,4)/89,122,151,10/
      DATA ( NVALUE(591,I),I=1,4)/89,122,151,12/
      DATA ( NVALUE(592,I),I=1,4)/89,122,0,0/
      DATA ( NVALUE(593,I),I=1,4)/89,122,0,11/
      DATA ( NVALUE(594,I),I=1,4)/89,122,0,13/
      DATA ( NVALUE(595,I),I=1,4)/89,122,0,87/
      DATA ( NVALUE(596,I),I=1,4)/89,122,0,82/
      DATA ( NVALUE(597,I),I=1,4)/89,122,0,84/
      DATA ( NVALUE(598,I),I=1,4)/89,122,0,7/
      DATA ( NVALUE(599,I),I=1,4)/89,122,10,87/
      DATA ( NVALUE(600,I),I=1,4)/89,122,11,82/
      DATA ( NVALUE(601,I),I=1,4)/89,122,11,84/
      DATA ( NVALUE(602,I),I=1,4)/89,122,11,7/
      DATA ( NVALUE(603,I),I=1,4)/89,122,12,87/
      DATA ( NVALUE(604,I),I=1,4)/89,122,13,82/
      DATA ( NVALUE(605,I),I=1,4)/89,122,13,84/
      DATA ( NVALUE(606,I),I=1,4)/89,122,13,7/
      DATA ( NVALUE(607,I),I=1,4)/89,126,0,0/
      DATA ( NVALUE(608,I),I=1,4)/89,126,0,10/
      DATA ( NVALUE(609,I),I=1,4)/89,126,0,12/
      DATA ( NVALUE(610,I),I=1,4)/89,126,6,0/
      DATA ( NVALUE(611,I),I=1,4)/89,126,6,10/
      DATA ( NVALUE(612,I),I=1,4)/89,126,6,12/
      DATA ( NVALUE(613,I),I=1,4)/89,126,151,0/
      DATA ( NVALUE(614,I),I=1,4)/89,126,151,11/
      DATA ( NVALUE(615,I),I=1,4)/89,126,151,13/
      DATA ( NVALUE(616,I),I=1,4)/89,126,0,0/
      DATA ( NVALUE(617,I),I=1,4)/89,126,0,10/
      DATA ( NVALUE(618,I),I=1,4)/89,126,0,12/
      DATA ( NVALUE(619,I),I=1,4)/89,126,0,82/
      DATA ( NVALUE(620,I),I=1,4)/89,126,0,84/
      DATA ( NVALUE(621,I),I=1,4)/89,126,0,7/
      DATA ( NVALUE(622,I),I=1,4)/89,126,0,87/
      DATA ( NVALUE(623,I),I=1,4)/89,126,10,82/
      DATA ( NVALUE(624,I),I=1,4)/89,126,10,84/
      DATA ( NVALUE(625,I),I=1,4)/89,126,10,7/
      DATA ( NVALUE(626,I),I=1,4)/89,126,11,87/
      DATA ( NVALUE(627,I),I=1,4)/89,126,12,82/
      DATA ( NVALUE(628,I),I=1,4)/89,126,12,84/
      DATA ( NVALUE(629,I),I=1,4)/89,126,12,7/
      DATA ( NVALUE(630,I),I=1,4)/89,126,13,87/
      DATA ( NVALUE(631,I),I=1,4)/89,126,151,0/
      DATA ( NVALUE(632,I),I=1,4)/89,126,151,11/
      DATA ( NVALUE(633,I),I=1,4)/89,126,151,13/
      DATA ( NVALUE(634,I),I=1,4)/89,126,0,87/
      DATA ( NVALUE(635,I),I=1,4)/89,126,11,87/
      DATA ( NVALUE(636,I),I=1,4)/89,126,13,87/
      DATA ( NVALUE(637,I),I=1,4)/89,131,0,0/
      DATA ( NVALUE(638,I),I=1,4)/89,131,0,11/
      DATA ( NVALUE(639,I),I=1,4)/89,131,0,13/
      DATA ( NVALUE(640,I),I=1,4)/89,131,0,10/
      DATA ( NVALUE(641,I),I=1,4)/89,131,0,12/
      DATA ( NVALUE(642,I),I=1,4)/89,131,10,11/
      DATA ( NVALUE(643,I),I=1,4)/89,131,10,13/
      DATA ( NVALUE(644,I),I=1,4)/89,131,11,12/
      DATA ( NVALUE(645,I),I=1,4)/89,131,12,13/
      DATA ( NVALUE(646,I),I=1,4)/132,132,0,0/
      DATA ( NVALUE(647,I),I=1,4)/132,132,0,0/
      DATA ( NVALUE(648,I),I=1,4)/132,132,0,6/
      DATA ( NVALUE(649,I),I=1,4)/132,132,0,82/
      DATA ( NVALUE(650,I),I=1,4)/132,132,0,84/
      DATA ( NVALUE(651,I),I=1,4)/132,132,6,6/
      DATA ( NVALUE(652,I),I=1,4)/132,132,6,82/
      DATA ( NVALUE(653,I),I=1,4)/132,132,6,84/
      DATA ( NVALUE(654,I),I=1,4)/132,132,6,7/
      DATA ( NVALUE(655,I),I=1,4)/132,132,151,151/
      DATA ( NVALUE(656,I),I=1,4)/132,132,151,87/
      DATA ( NVALUE(657,I),I=1,4)/132,132,151,87/
      DATA ( NVALUE(658,I),I=1,4)/132,132,0,0/
      DATA ( NVALUE(659,I),I=1,4)/132,132,0,0/
      DATA ( NVALUE(660,I),I=1,4)/132,132,0,0/
      DATA ( NVALUE(661,I),I=1,4)/132,132,0,0/
      DATA ( NVALUE(662,I),I=1,4)/132,132,8,8/
      DATA ( NVALUE(663,I),I=1,4)/132,132,9,9/
      DATA ( NVALUE(664,I),I=1,4)/132,132,0,0/
      DATA ( NVALUE(665,I),I=1,4)/132,132,0,0/
      DATA ( NVALUE(666,I),I=1,4)/132,132,10,10/
      DATA ( NVALUE(667,I),I=1,4)/132,132,11,11/
      DATA ( NVALUE(668,I),I=1,4)/132,132,12,12/
      DATA ( NVALUE(669,I),I=1,4)/132,132,13,13/
      DATA ( NVALUE(670,I),I=1,4)/132,132,82,82/
      DATA ( NVALUE(671,I),I=1,4)/132,132,82,84/
      DATA ( NVALUE(672,I),I=1,4)/132,132,82,7/
      DATA ( NVALUE(673,I),I=1,4)/132,132,84,84/
      DATA ( NVALUE(674,I),I=1,4)/132,132,84,7/
      DATA ( NVALUE(675,I),I=1,4)/132,132,7,7/
      DATA ( NVALUE(676,I),I=1,4)/132,132,87,87/
      DATA ( NVALUE(677,I),I=1,4)/132,134,0,8/
      DATA ( NVALUE(678,I),I=1,4)/132,134,0,0/
      DATA ( NVALUE(679,I),I=1,4)/132,134,0,8/
      DATA ( NVALUE(680,I),I=1,4)/132,106,0,9/
      DATA ( NVALUE(681,I),I=1,4)/132,106,0,0/
      DATA ( NVALUE(682,I),I=1,4)/132,106,0,9/
      DATA ( NVALUE(683,I),I=1,4)/132,136,0,0/
      DATA ( NVALUE(684,I),I=1,4)/132,136,0,151/
      DATA ( NVALUE(685,I),I=1,4)/132,136,0,87/
      DATA ( NVALUE(686,I),I=1,4)/132,136,6,151/
      DATA ( NVALUE(687,I),I=1,4)/132,136,6,87/
      DATA ( NVALUE(688,I),I=1,4)/132,136,151,82/
      DATA ( NVALUE(689,I),I=1,4)/132,136,151,84/
      DATA ( NVALUE(690,I),I=1,4)/132,136,151,7/
      DATA ( NVALUE(691,I),I=1,4)/132,136,0,0/
      DATA ( NVALUE(692,I),I=1,4)/132,136,0,8/
      DATA ( NVALUE(693,I),I=1,4)/132,136,0,9/
      DATA ( NVALUE(694,I),I=1,4)/132,136,0,0/
      DATA ( NVALUE(695,I),I=1,4)/132,136,0,11/
      DATA ( NVALUE(696,I),I=1,4)/132,136,0,13/
      DATA ( NVALUE(697,I),I=1,4)/132,136,0,10/
      DATA ( NVALUE(698,I),I=1,4)/132,136,0,12/
      DATA ( NVALUE(699,I),I=1,4)/132,136,10,11/
      DATA ( NVALUE(700,I),I=1,4)/132,136,10,13/
      DATA ( NVALUE(701,I),I=1,4)/132,136,11,12/
      DATA ( NVALUE(702,I),I=1,4)/132,136,12,13/
      DATA ( NVALUE(703,I),I=1,4)/132,136,82,87/
      DATA ( NVALUE(704,I),I=1,4)/132,136,84,87/
      DATA ( NVALUE(705,I),I=1,4)/132,136,7,87/
      DATA ( NVALUE(706,I),I=1,4)/132,137,0,8/
      DATA ( NVALUE(707,I),I=1,4)/132,137,0,0/
      DATA ( NVALUE(708,I),I=1,4)/132,137,0,0/
      DATA ( NVALUE(709,I),I=1,4)/132,112,0,9/
      DATA ( NVALUE(710,I),I=1,4)/132,112,0,0/
      DATA ( NVALUE(711,I),I=1,4)/132,112,0,0/
      DATA ( NVALUE(712,I),I=1,4)/132,138,0,0/
      DATA ( NVALUE(713,I),I=1,4)/132,138,0,11/
      DATA ( NVALUE(714,I),I=1,4)/132,138,0,13/
      DATA ( NVALUE(715,I),I=1,4)/132,138,0,0/
      DATA ( NVALUE(716,I),I=1,4)/132,138,0,0/
      DATA ( NVALUE(717,I),I=1,4)/132,140,0,0/
      DATA ( NVALUE(718,I),I=1,4)/132,140,0,0/
      DATA ( NVALUE(719,I),I=1,4)/132,140,0,10/
      DATA ( NVALUE(720,I),I=1,4)/132,140,0,12/
      DATA ( NVALUE(721,I),I=1,4)/132,140,0,0/
      DATA ( NVALUE(722,I),I=1,4)/132,142,0,0/
      DATA ( NVALUE(723,I),I=1,4)/132,142,0,11/
      DATA ( NVALUE(724,I),I=1,4)/132,142,0,13/
      DATA ( NVALUE(725,I),I=1,4)/132,142,0,10/
      DATA ( NVALUE(726,I),I=1,4)/132,142,0,10/
      DATA ( NVALUE(727,I),I=1,4)/132,144,0,11/
      DATA ( NVALUE(728,I),I=1,4)/132,144,0,0/
      DATA ( NVALUE(729,I),I=1,4)/132,144,0,10/
      DATA ( NVALUE(730,I),I=1,4)/132,144,0,12/
      DATA ( NVALUE(731,I),I=1,4)/132,144,0,11/
      DATA ( NVALUE(732,I),I=1,4)/132,122,0,0/
      DATA ( NVALUE(733,I),I=1,4)/132,122,0,11/
      DATA ( NVALUE(734,I),I=1,4)/132,122,0,13/
      DATA ( NVALUE(735,I),I=1,4)/132,122,0,12/
      DATA ( NVALUE(736,I),I=1,4)/132,122,0,12/
      DATA ( NVALUE(737,I),I=1,4)/132,126,0,13/
      DATA ( NVALUE(738,I),I=1,4)/132,126,0,0/
      DATA ( NVALUE(739,I),I=1,4)/132,126,0,10/
      DATA ( NVALUE(740,I),I=1,4)/132,126,0,12/
      DATA ( NVALUE(741,I),I=1,4)/132,126,0,13/
      DATA ( NVALUE(742,I),I=1,4)/134,134,8,8/
      DATA ( NVALUE(743,I),I=1,4)/134,134,0,0/
      DATA ( NVALUE(744,I),I=1,4)/134,134,0,6/
      DATA ( NVALUE(745,I),I=1,4)/134,134,0,82/
      DATA ( NVALUE(746,I),I=1,4)/134,134,0,84/
      DATA ( NVALUE(747,I),I=1,4)/134,134,6,6/
      DATA ( NVALUE(748,I),I=1,4)/134,134,6,82/
      DATA ( NVALUE(749,I),I=1,4)/134,134,6,84/
      DATA ( NVALUE(750,I),I=1,4)/134,134,6,7/
      DATA ( NVALUE(751,I),I=1,4)/134,134,151,151/
      DATA ( NVALUE(752,I),I=1,4)/134,134,151,87/
      DATA ( NVALUE(753,I),I=1,4)/134,134,151,87/
      DATA ( NVALUE(754,I),I=1,4)/134,134,0,0/
      DATA ( NVALUE(755,I),I=1,4)/134,134,0,0/
      DATA ( NVALUE(756,I),I=1,4)/134,134,0,0/
      DATA ( NVALUE(757,I),I=1,4)/134,134,0,0/
      DATA ( NVALUE(758,I),I=1,4)/134,134,8,8/
      DATA ( NVALUE(759,I),I=1,4)/134,134,9,9/
      DATA ( NVALUE(760,I),I=1,4)/134,134,0,0/
      DATA ( NVALUE(761,I),I=1,4)/134,134,0,0/
      DATA ( NVALUE(762,I),I=1,4)/134,134,10,10/
      DATA ( NVALUE(763,I),I=1,4)/134,134,11,11/
      DATA ( NVALUE(764,I),I=1,4)/134,134,12,12/
      DATA ( NVALUE(765,I),I=1,4)/134,134,13,13/
      DATA ( NVALUE(766,I),I=1,4)/134,134,82,82/
      DATA ( NVALUE(767,I),I=1,4)/134,134,82,84/
      DATA ( NVALUE(768,I),I=1,4)/134,134,82,7/
      DATA ( NVALUE(769,I),I=1,4)/134,134,84,84/
      DATA ( NVALUE(770,I),I=1,4)/134,134,84,7/
      DATA ( NVALUE(771,I),I=1,4)/134,134,7,7/
      DATA ( NVALUE(772,I),I=1,4)/134,134,87,87/
      DATA ( NVALUE(773,I),I=1,4)/134,106,8,9/
      DATA ( NVALUE(774,I),I=1,4)/134,106,0,0/
      DATA ( NVALUE(775,I),I=1,4)/134,106,8,9/
      DATA ( NVALUE(776,I),I=1,4)/134,136,0,8/
      DATA ( NVALUE(777,I),I=1,4)/134,136,0,0/
      DATA ( NVALUE(778,I),I=1,4)/134,136,0,8/
      DATA ( NVALUE(779,I),I=1,4)/134,137,0,8/
      DATA ( NVALUE(780,I),I=1,4)/134,137,0,151/
      DATA ( NVALUE(781,I),I=1,4)/134,137,0,87/
      DATA ( NVALUE(782,I),I=1,4)/134,137,6,151/
      DATA ( NVALUE(783,I),I=1,4)/134,137,6,87/
      DATA ( NVALUE(784,I),I=1,4)/134,137,151,82/
      DATA ( NVALUE(785,I),I=1,4)/134,137,151,84/
      DATA ( NVALUE(786,I),I=1,4)/134,137,151,7/
      DATA ( NVALUE(787,I),I=1,4)/134,137,0,0/
      DATA ( NVALUE(788,I),I=1,4)/134,137,0,8/
      DATA ( NVALUE(789,I),I=1,4)/134,137,0,9/
      DATA ( NVALUE(790,I),I=1,4)/134,137,0,0/
      DATA ( NVALUE(791,I),I=1,4)/134,137,0,11/
      DATA ( NVALUE(792,I),I=1,4)/134,137,0,13/
      DATA ( NVALUE(793,I),I=1,4)/134,137,0,10/
      DATA ( NVALUE(794,I),I=1,4)/134,137,0,12/
      DATA ( NVALUE(795,I),I=1,4)/134,137,10,11/
      DATA ( NVALUE(796,I),I=1,4)/134,137,10,13/
      DATA ( NVALUE(797,I),I=1,4)/134,137,11,12/
      DATA ( NVALUE(798,I),I=1,4)/134,137,12,13/
      DATA ( NVALUE(799,I),I=1,4)/134,137,82,87/
      DATA ( NVALUE(800,I),I=1,4)/134,137,84,87/
      DATA ( NVALUE(801,I),I=1,4)/134,137,7,87/
      DATA ( NVALUE(802,I),I=1,4)/134,112,0,9/
      DATA ( NVALUE(803,I),I=1,4)/134,112,8,0/
      DATA ( NVALUE(804,I),I=1,4)/134,112,8,0/
      DATA ( NVALUE(805,I),I=1,4)/134,138,0,0/
      DATA ( NVALUE(806,I),I=1,4)/134,138,0,11/
      DATA ( NVALUE(807,I),I=1,4)/134,138,0,13/
      DATA ( NVALUE(808,I),I=1,4)/134,138,8,0/
      DATA ( NVALUE(809,I),I=1,4)/134,138,8,0/
      DATA ( NVALUE(810,I),I=1,4)/134,140,8,0/
      DATA ( NVALUE(811,I),I=1,4)/134,140,0,0/
      DATA ( NVALUE(812,I),I=1,4)/134,140,0,10/
      DATA ( NVALUE(813,I),I=1,4)/134,140,0,12/
      DATA ( NVALUE(814,I),I=1,4)/134,140,8,0/
      DATA ( NVALUE(815,I),I=1,4)/134,142,0,0/
      DATA ( NVALUE(816,I),I=1,4)/134,142,0,11/
      DATA ( NVALUE(817,I),I=1,4)/134,142,0,13/
      DATA ( NVALUE(818,I),I=1,4)/134,142,8,10/
      DATA ( NVALUE(819,I),I=1,4)/134,142,8,10/
      DATA ( NVALUE(820,I),I=1,4)/134,144,8,11/
      DATA ( NVALUE(821,I),I=1,4)/134,144,0,0/
      DATA ( NVALUE(822,I),I=1,4)/134,144,0,10/
      DATA ( NVALUE(823,I),I=1,4)/134,144,0,12/
      DATA ( NVALUE(824,I),I=1,4)/134,144,8,11/
      DATA ( NVALUE(825,I),I=1,4)/134,122,0,0/
      DATA ( NVALUE(826,I),I=1,4)/134,122,0,11/
      DATA ( NVALUE(827,I),I=1,4)/134,122,0,13/
      DATA ( NVALUE(828,I),I=1,4)/134,122,8,12/
      DATA ( NVALUE(829,I),I=1,4)/134,122,8,12/
      DATA ( NVALUE(830,I),I=1,4)/134,126,8,13/
      DATA ( NVALUE(831,I),I=1,4)/134,126,0,0/
      DATA ( NVALUE(832,I),I=1,4)/134,126,0,10/
      DATA ( NVALUE(833,I),I=1,4)/134,126,0,12/
      DATA ( NVALUE(834,I),I=1,4)/134,126,8,13/
      DATA ( NVALUE(835,I),I=1,4)/106,106,9,9/
      DATA ( NVALUE(836,I),I=1,4)/106,106,0,0/
      DATA ( NVALUE(837,I),I=1,4)/106,106,0,6/
      DATA ( NVALUE(838,I),I=1,4)/106,106,0,82/
      DATA ( NVALUE(839,I),I=1,4)/106,106,0,84/
      DATA ( NVALUE(840,I),I=1,4)/106,106,6,6/
      DATA ( NVALUE(841,I),I=1,4)/106,106,6,82/
      DATA ( NVALUE(842,I),I=1,4)/106,106,6,84/
      DATA ( NVALUE(843,I),I=1,4)/106,106,6,7/
      DATA ( NVALUE(844,I),I=1,4)/106,106,151,151/
      DATA ( NVALUE(845,I),I=1,4)/106,106,151,87/
      DATA ( NVALUE(846,I),I=1,4)/106,106,151,87/
      DATA ( NVALUE(847,I),I=1,4)/106,106,0,0/
      DATA ( NVALUE(848,I),I=1,4)/106,106,0,0/
      DATA ( NVALUE(849,I),I=1,4)/106,106,0,0/
      DATA ( NVALUE(850,I),I=1,4)/106,106,0,0/
      DATA ( NVALUE(851,I),I=1,4)/106,106,8,8/
      DATA ( NVALUE(852,I),I=1,4)/106,106,9,9/
      DATA ( NVALUE(853,I),I=1,4)/106,106,0,0/
      DATA ( NVALUE(854,I),I=1,4)/106,106,0,0/
      DATA ( NVALUE(855,I),I=1,4)/106,106,10,10/
      DATA ( NVALUE(856,I),I=1,4)/106,106,11,11/
      DATA ( NVALUE(857,I),I=1,4)/106,106,12,12/
      DATA ( NVALUE(858,I),I=1,4)/106,106,13,13/
      DATA ( NVALUE(859,I),I=1,4)/106,106,82,82/
      DATA ( NVALUE(860,I),I=1,4)/106,106,82,84/
      DATA ( NVALUE(861,I),I=1,4)/106,106,82,7/
      DATA ( NVALUE(862,I),I=1,4)/106,106,84,84/
      DATA ( NVALUE(863,I),I=1,4)/106,106,84,7/
      DATA ( NVALUE(864,I),I=1,4)/106,106,7,7/
      DATA ( NVALUE(865,I),I=1,4)/106,106,87,87/
      DATA ( NVALUE(866,I),I=1,4)/106,136,0,9/
      DATA ( NVALUE(867,I),I=1,4)/106,136,0,0/
      DATA ( NVALUE(868,I),I=1,4)/106,136,0,9/
      DATA ( NVALUE(869,I),I=1,4)/106,137,0,9/
      DATA ( NVALUE(870,I),I=1,4)/106,137,8,0/
      DATA ( NVALUE(871,I),I=1,4)/106,137,0,9/
      DATA ( NVALUE(872,I),I=1,4)/106,112,0,9/
      DATA ( NVALUE(873,I),I=1,4)/106,112,0,151/
      DATA ( NVALUE(874,I),I=1,4)/106,112,0,87/
      DATA ( NVALUE(875,I),I=1,4)/106,112,6,151/
      DATA ( NVALUE(876,I),I=1,4)/106,112,6,87/
      DATA ( NVALUE(877,I),I=1,4)/106,112,151,82/
      DATA ( NVALUE(878,I),I=1,4)/106,112,151,84/
      DATA ( NVALUE(879,I),I=1,4)/106,112,151,7/
      DATA ( NVALUE(880,I),I=1,4)/106,112,0,0/
      DATA ( NVALUE(881,I),I=1,4)/106,112,0,8/
      DATA ( NVALUE(882,I),I=1,4)/106,112,0,9/
      DATA ( NVALUE(883,I),I=1,4)/106,112,0,0/
      DATA ( NVALUE(884,I),I=1,4)/106,112,0,11/
      DATA ( NVALUE(885,I),I=1,4)/106,112,0,13/
      DATA ( NVALUE(886,I),I=1,4)/106,112,0,10/
      DATA ( NVALUE(887,I),I=1,4)/106,112,0,12/
      DATA ( NVALUE(888,I),I=1,4)/106,112,10,11/
      DATA ( NVALUE(889,I),I=1,4)/106,112,10,13/
      DATA ( NVALUE(890,I),I=1,4)/106,112,11,12/
      DATA ( NVALUE(891,I),I=1,4)/106,112,12,13/
      DATA ( NVALUE(892,I),I=1,4)/106,112,82,87/
      DATA ( NVALUE(893,I),I=1,4)/106,112,84,87/
      DATA ( NVALUE(894,I),I=1,4)/106,112,7,87/
      DATA ( NVALUE(895,I),I=1,4)/106,138,0,0/
      DATA ( NVALUE(896,I),I=1,4)/106,138,0,11/
      DATA ( NVALUE(897,I),I=1,4)/106,138,0,13/
      DATA ( NVALUE(898,I),I=1,4)/106,138,9,0/
      DATA ( NVALUE(899,I),I=1,4)/106,138,9,0/
      DATA ( NVALUE(900,I),I=1,4)/106,140,9,0/
      DATA ( NVALUE(901,I),I=1,4)/106,140,0,0/
      DATA ( NVALUE(902,I),I=1,4)/106,140,0,10/
      DATA ( NVALUE(903,I),I=1,4)/106,140,0,12/
      DATA ( NVALUE(904,I),I=1,4)/106,140,9,0/
      DATA ( NVALUE(905,I),I=1,4)/106,142,0,0/
      DATA ( NVALUE(906,I),I=1,4)/106,142,0,11/
      DATA ( NVALUE(907,I),I=1,4)/106,142,0,13/
      DATA ( NVALUE(908,I),I=1,4)/106,142,9,10/
      DATA ( NVALUE(909,I),I=1,4)/106,142,9,10/
      DATA ( NVALUE(910,I),I=1,4)/106,144,9,11/
      DATA ( NVALUE(911,I),I=1,4)/106,144,0,0/
      DATA ( NVALUE(912,I),I=1,4)/106,144,0,10/
      DATA ( NVALUE(913,I),I=1,4)/106,144,0,12/
      DATA ( NVALUE(914,I),I=1,4)/106,144,9,11/
      DATA ( NVALUE(915,I),I=1,4)/106,122,0,0/
      DATA ( NVALUE(916,I),I=1,4)/106,122,0,11/
      DATA ( NVALUE(917,I),I=1,4)/106,122,0,13/
      DATA ( NVALUE(918,I),I=1,4)/106,122,9,12/
      DATA ( NVALUE(919,I),I=1,4)/106,122,9,12/
      DATA ( NVALUE(920,I),I=1,4)/106,126,9,13/
      DATA ( NVALUE(921,I),I=1,4)/106,126,0,0/
      DATA ( NVALUE(922,I),I=1,4)/106,126,0,10/
      DATA ( NVALUE(923,I),I=1,4)/106,126,0,12/
      DATA ( NVALUE(924,I),I=1,4)/106,126,9,13/
      DATA ( NVALUE(925,I),I=1,4)/136,136,0,0/
      DATA ( NVALUE(926,I),I=1,4)/136,136,6,6/
      DATA ( NVALUE(927,I),I=1,4)/136,136,6,82/
      DATA ( NVALUE(928,I),I=1,4)/136,136,6,84/
      DATA ( NVALUE(929,I),I=1,4)/136,136,6,7/
      DATA ( NVALUE(930,I),I=1,4)/136,136,151,151/
      DATA ( NVALUE(931,I),I=1,4)/136,136,151,87/
      DATA ( NVALUE(932,I),I=1,4)/136,136,151,87/
      DATA ( NVALUE(933,I),I=1,4)/136,136,0,0/
      DATA ( NVALUE(934,I),I=1,4)/136,136,0,0/
      DATA ( NVALUE(935,I),I=1,4)/136,136,0,0/
      DATA ( NVALUE(936,I),I=1,4)/136,136,0,0/
      DATA ( NVALUE(937,I),I=1,4)/136,136,8,8/
      DATA ( NVALUE(938,I),I=1,4)/136,136,9,9/
      DATA ( NVALUE(939,I),I=1,4)/136,136,0,0/
      DATA ( NVALUE(940,I),I=1,4)/136,136,0,0/
      DATA ( NVALUE(941,I),I=1,4)/136,136,10,10/
      DATA ( NVALUE(942,I),I=1,4)/136,136,11,11/
      DATA ( NVALUE(943,I),I=1,4)/136,136,12,12/
      DATA ( NVALUE(944,I),I=1,4)/136,136,13,13/
      DATA ( NVALUE(945,I),I=1,4)/136,136,82,82/
      DATA ( NVALUE(946,I),I=1,4)/136,136,82,84/
      DATA ( NVALUE(947,I),I=1,4)/136,136,82,7/
      DATA ( NVALUE(948,I),I=1,4)/136,136,84,84/
      DATA ( NVALUE(949,I),I=1,4)/136,136,84,7/
      DATA ( NVALUE(950,I),I=1,4)/136,136,7,7/
      DATA ( NVALUE(951,I),I=1,4)/136,136,87,87/
      DATA ( NVALUE(952,I),I=1,4)/136,137,0,0/
      DATA ( NVALUE(953,I),I=1,4)/136,137,0,0/
      DATA ( NVALUE(954,I),I=1,4)/136,137,0,8/
      DATA ( NVALUE(955,I),I=1,4)/136,112,0,0/
      DATA ( NVALUE(956,I),I=1,4)/136,112,0,0/
      DATA ( NVALUE(957,I),I=1,4)/136,112,0,9/
      DATA ( NVALUE(958,I),I=1,4)/136,138,0,0/
      DATA ( NVALUE(959,I),I=1,4)/136,138,0,0/
      DATA ( NVALUE(960,I),I=1,4)/136,138,0,0/
      DATA ( NVALUE(961,I),I=1,4)/136,138,0,11/
      DATA ( NVALUE(962,I),I=1,4)/136,138,0,13/
      DATA ( NVALUE(963,I),I=1,4)/136,140,0,0/
      DATA ( NVALUE(964,I),I=1,4)/136,140,0,0/
      DATA ( NVALUE(965,I),I=1,4)/136,140,0,10/
      DATA ( NVALUE(966,I),I=1,4)/136,140,0,12/
      DATA ( NVALUE(967,I),I=1,4)/136,140,0,0/
      DATA ( NVALUE(968,I),I=1,4)/136,142,0,10/
      DATA ( NVALUE(969,I),I=1,4)/136,142,0,10/
      DATA ( NVALUE(970,I),I=1,4)/136,142,0,0/
      DATA ( NVALUE(971,I),I=1,4)/136,142,0,11/
      DATA ( NVALUE(972,I),I=1,4)/136,142,0,13/
      DATA ( NVALUE(973,I),I=1,4)/136,144,0,11/
      DATA ( NVALUE(974,I),I=1,4)/136,144,0,0/
      DATA ( NVALUE(975,I),I=1,4)/136,144,0,10/
      DATA ( NVALUE(976,I),I=1,4)/136,144,0,12/
      DATA ( NVALUE(977,I),I=1,4)/136,144,0,11/
      DATA ( NVALUE(978,I),I=1,4)/136,122,0,12/
      DATA ( NVALUE(979,I),I=1,4)/136,122,0,12/
      DATA ( NVALUE(980,I),I=1,4)/136,122,0,0/
      DATA ( NVALUE(981,I),I=1,4)/136,122,0,11/
      DATA ( NVALUE(982,I),I=1,4)/136,122,0,13/
      DATA ( NVALUE(983,I),I=1,4)/136,126,0,13/
      DATA ( NVALUE(984,I),I=1,4)/136,126,0,0/
      DATA ( NVALUE(985,I),I=1,4)/136,126,0,10/
      DATA ( NVALUE(986,I),I=1,4)/136,126,0,12/
      DATA ( NVALUE(987,I),I=1,4)/136,126,0,13/
      DATA ( NVALUE(988,I),I=1,4)/137,137,0,0/
      DATA ( NVALUE(989,I),I=1,4)/137,137,6,6/
      DATA ( NVALUE(990,I),I=1,4)/137,137,6,82/
      DATA ( NVALUE(991,I),I=1,4)/137,137,6,84/
      DATA ( NVALUE(992,I),I=1,4)/137,137,6,7/
      DATA ( NVALUE(993,I),I=1,4)/137,137,151,151/
      DATA ( NVALUE(994,I),I=1,4)/137,137,151,87/
      DATA ( NVALUE(995,I),I=1,4)/137,137,151,87/
      DATA ( NVALUE(996,I),I=1,4)/137,137,0,0/
      DATA ( NVALUE(997,I),I=1,4)/137,137,0,0/
      DATA ( NVALUE(998,I),I=1,4)/137,137,0,0/
      DATA ( NVALUE(999,I),I=1,4)/137,137,0,0/
      DATA ( NVALUE(1000,I),I=1,4)/137,137,8,8/
      DATA ( NVALUE(1001,I),I=1,4)/137,137,9,9/
      DATA ( NVALUE(1002,I),I=1,4)/137,137,0,0/
      DATA ( NVALUE(1003,I),I=1,4)/137,137,0,0/
      DATA ( NVALUE(1004,I),I=1,4)/137,137,10,10/
      DATA ( NVALUE(1005,I),I=1,4)/137,137,11,11/
      DATA ( NVALUE(1006,I),I=1,4)/137,137,12,12/
      DATA ( NVALUE(1007,I),I=1,4)/137,137,13,13/
      DATA ( NVALUE(1008,I),I=1,4)/137,137,82,82/
      DATA ( NVALUE(1009,I),I=1,4)/137,137,82,84/
      DATA ( NVALUE(1010,I),I=1,4)/137,137,82,7/
      DATA ( NVALUE(1011,I),I=1,4)/137,137,84,84/
      DATA ( NVALUE(1012,I),I=1,4)/137,137,84,7/
      DATA ( NVALUE(1013,I),I=1,4)/137,137,7,7/
      DATA ( NVALUE(1014,I),I=1,4)/137,137,87,87/
      DATA ( NVALUE(1015,I),I=1,4)/137,112,0,0/
      DATA ( NVALUE(1016,I),I=1,4)/137,112,0,0/
      DATA ( NVALUE(1017,I),I=1,4)/137,112,8,9/
      DATA ( NVALUE(1018,I),I=1,4)/137,138,0,0/
      DATA ( NVALUE(1019,I),I=1,4)/137,138,0,0/
      DATA ( NVALUE(1020,I),I=1,4)/137,138,8,0/
      DATA ( NVALUE(1021,I),I=1,4)/137,138,8,11/
      DATA ( NVALUE(1022,I),I=1,4)/137,138,8,13/
      DATA ( NVALUE(1023,I),I=1,4)/137,140,0,0/
      DATA ( NVALUE(1024,I),I=1,4)/137,140,8,0/
      DATA ( NVALUE(1025,I),I=1,4)/137,140,8,10/
      DATA ( NVALUE(1026,I),I=1,4)/137,140,8,12/
      DATA ( NVALUE(1027,I),I=1,4)/137,140,0,0/
      DATA ( NVALUE(1028,I),I=1,4)/137,142,0,10/
      DATA ( NVALUE(1029,I),I=1,4)/137,142,0,10/
      DATA ( NVALUE(1030,I),I=1,4)/137,142,8,0/
      DATA ( NVALUE(1031,I),I=1,4)/137,142,8,11/
      DATA ( NVALUE(1032,I),I=1,4)/137,142,8,13/
      DATA ( NVALUE(1033,I),I=1,4)/137,144,0,11/
      DATA ( NVALUE(1034,I),I=1,4)/137,144,8,0/
      DATA ( NVALUE(1035,I),I=1,4)/137,144,8,10/
      DATA ( NVALUE(1036,I),I=1,4)/137,144,8,12/
      DATA ( NVALUE(1037,I),I=1,4)/137,144,0,11/
      DATA ( NVALUE(1038,I),I=1,4)/137,122,0,12/
      DATA ( NVALUE(1039,I),I=1,4)/137,122,0,12/
      DATA ( NVALUE(1040,I),I=1,4)/137,122,8,0/
      DATA ( NVALUE(1041,I),I=1,4)/137,122,8,11/
      DATA ( NVALUE(1042,I),I=1,4)/137,122,8,13/
      DATA ( NVALUE(1043,I),I=1,4)/137,126,0,13/
      DATA ( NVALUE(1044,I),I=1,4)/137,126,8,0/
      DATA ( NVALUE(1045,I),I=1,4)/137,126,8,10/
      DATA ( NVALUE(1046,I),I=1,4)/137,126,8,12/
      DATA ( NVALUE(1047,I),I=1,4)/137,126,0,13/
      DATA ( NVALUE(1048,I),I=1,4)/112,112,0,0/
      DATA ( NVALUE(1049,I),I=1,4)/112,112,6,6/
      DATA ( NVALUE(1050,I),I=1,4)/112,112,6,82/
      DATA ( NVALUE(1051,I),I=1,4)/112,112,6,84/
      DATA ( NVALUE(1052,I),I=1,4)/112,112,6,7/
      DATA ( NVALUE(1053,I),I=1,4)/112,112,151,151/
      DATA ( NVALUE(1054,I),I=1,4)/112,112,151,87/
      DATA ( NVALUE(1055,I),I=1,4)/112,112,151,87/
      DATA ( NVALUE(1056,I),I=1,4)/112,112,0,0/
      DATA ( NVALUE(1057,I),I=1,4)/112,112,0,0/
      DATA ( NVALUE(1058,I),I=1,4)/112,112,0,0/
      DATA ( NVALUE(1059,I),I=1,4)/112,112,0,0/
      DATA ( NVALUE(1060,I),I=1,4)/112,112,8,8/
      DATA ( NVALUE(1061,I),I=1,4)/112,112,9,9/
      DATA ( NVALUE(1062,I),I=1,4)/112,112,0,0/
      DATA ( NVALUE(1063,I),I=1,4)/112,112,0,0/
      DATA ( NVALUE(1064,I),I=1,4)/112,112,10,10/
      DATA ( NVALUE(1065,I),I=1,4)/112,112,11,11/
      DATA ( NVALUE(1066,I),I=1,4)/112,112,12,12/
      DATA ( NVALUE(1067,I),I=1,4)/112,112,13,13/
      DATA ( NVALUE(1068,I),I=1,4)/112,112,82,82/
      DATA ( NVALUE(1069,I),I=1,4)/112,112,82,84/
      DATA ( NVALUE(1070,I),I=1,4)/112,112,82,7/
      DATA ( NVALUE(1071,I),I=1,4)/112,112,84,84/
      DATA ( NVALUE(1072,I),I=1,4)/112,112,84,7/
      DATA ( NVALUE(1073,I),I=1,4)/112,112,7,7/
      DATA ( NVALUE(1074,I),I=1,4)/112,112,87,87/
      DATA ( NVALUE(1075,I),I=1,4)/112,138,0,0/
      DATA ( NVALUE(1076,I),I=1,4)/112,138,0,0/
      DATA ( NVALUE(1077,I),I=1,4)/112,138,9,0/
      DATA ( NVALUE(1078,I),I=1,4)/112,138,9,11/
      DATA ( NVALUE(1079,I),I=1,4)/112,138,9,13/
      DATA ( NVALUE(1080,I),I=1,4)/112,140,0,0/
      DATA ( NVALUE(1081,I),I=1,4)/112,140,9,0/
      DATA ( NVALUE(1082,I),I=1,4)/112,140,9,10/
      DATA ( NVALUE(1083,I),I=1,4)/112,140,9,12/
      DATA ( NVALUE(1084,I),I=1,4)/112,140,0,0/
      DATA ( NVALUE(1085,I),I=1,4)/112,142,0,10/
      DATA ( NVALUE(1086,I),I=1,4)/112,142,0,10/
      DATA ( NVALUE(1087,I),I=1,4)/112,142,9,0/
      DATA ( NVALUE(1088,I),I=1,4)/112,142,9,11/
      DATA ( NVALUE(1089,I),I=1,4)/112,142,9,13/
      DATA ( NVALUE(1090,I),I=1,4)/112,144,0,11/
      DATA ( NVALUE(1091,I),I=1,4)/112,144,9,0/
      DATA ( NVALUE(1092,I),I=1,4)/112,144,9,10/
      DATA ( NVALUE(1093,I),I=1,4)/112,144,9,12/
      DATA ( NVALUE(1094,I),I=1,4)/112,144,0,11/
      DATA ( NVALUE(1095,I),I=1,4)/112,122,0,12/
      DATA ( NVALUE(1096,I),I=1,4)/112,122,0,12/
      DATA ( NVALUE(1097,I),I=1,4)/112,122,9,0/
      DATA ( NVALUE(1098,I),I=1,4)/112,122,9,11/
      DATA ( NVALUE(1099,I),I=1,4)/112,122,9,13/
      DATA ( NVALUE(1100,I),I=1,4)/112,126,0,13/
      DATA ( NVALUE(1101,I),I=1,4)/112,126,9,0/
      DATA ( NVALUE(1102,I),I=1,4)/112,126,9,10/
      DATA ( NVALUE(1103,I),I=1,4)/112,126,9,12/
      DATA ( NVALUE(1104,I),I=1,4)/112,126,0,13/
      DATA ( NVALUE(1105,I),I=1,4)/138,138,0,0/
      DATA ( NVALUE(1106,I),I=1,4)/138,138,0,0/
      DATA ( NVALUE(1107,I),I=1,4)/138,138,0,6/
      DATA ( NVALUE(1108,I),I=1,4)/138,138,0,0/
      DATA ( NVALUE(1109,I),I=1,4)/138,138,0,82/
      DATA ( NVALUE(1110,I),I=1,4)/138,138,0,84/
      DATA ( NVALUE(1111,I),I=1,4)/138,138,6,6/
      DATA ( NVALUE(1112,I),I=1,4)/138,138,6,0/
      DATA ( NVALUE(1113,I),I=1,4)/138,138,6,82/
      DATA ( NVALUE(1114,I),I=1,4)/138,138,6,84/
      DATA ( NVALUE(1115,I),I=1,4)/138,138,6,7/
      DATA ( NVALUE(1116,I),I=1,4)/138,138,151,151/
      DATA ( NVALUE(1117,I),I=1,4)/138,138,151,87/
      DATA ( NVALUE(1118,I),I=1,4)/138,138,151,87/
      DATA ( NVALUE(1119,I),I=1,4)/138,138,0,0/
      DATA ( NVALUE(1120,I),I=1,4)/138,138,0,82/
      DATA ( NVALUE(1121,I),I=1,4)/138,138,0,84/
      DATA ( NVALUE(1122,I),I=1,4)/138,138,0,0/
      DATA ( NVALUE(1123,I),I=1,4)/138,138,0,0/
      DATA ( NVALUE(1124,I),I=1,4)/138,138,0,0/
      DATA ( NVALUE(1125,I),I=1,4)/138,138,0,0/
      DATA ( NVALUE(1126,I),I=1,4)/138,138,8,8/
      DATA ( NVALUE(1127,I),I=1,4)/138,138,9,9/
      DATA ( NVALUE(1128,I),I=1,4)/138,138,0,0/
      DATA ( NVALUE(1129,I),I=1,4)/138,138,0,0/
      DATA ( NVALUE(1130,I),I=1,4)/138,138,0,11/
      DATA ( NVALUE(1131,I),I=1,4)/138,138,0,13/
      DATA ( NVALUE(1132,I),I=1,4)/138,138,0,11/
      DATA ( NVALUE(1133,I),I=1,4)/138,138,0,13/
      DATA ( NVALUE(1134,I),I=1,4)/138,138,10,10/
      DATA ( NVALUE(1135,I),I=1,4)/138,138,11,11/
      DATA ( NVALUE(1136,I),I=1,4)/138,138,11,13/
      DATA ( NVALUE(1137,I),I=1,4)/138,138,11,13/
      DATA ( NVALUE(1138,I),I=1,4)/138,138,12,12/
      DATA ( NVALUE(1139,I),I=1,4)/138,138,13,13/
      DATA ( NVALUE(1140,I),I=1,4)/138,138,82,82/
      DATA ( NVALUE(1141,I),I=1,4)/138,138,82,84/
      DATA ( NVALUE(1142,I),I=1,4)/138,138,82,7/
      DATA ( NVALUE(1143,I),I=1,4)/138,138,84,84/
      DATA ( NVALUE(1144,I),I=1,4)/138,138,84,7/
      DATA ( NVALUE(1145,I),I=1,4)/138,138,7,7/
      DATA ( NVALUE(1146,I),I=1,4)/138,138,87,87/
      DATA ( NVALUE(1147,I),I=1,4)/138,140,0,0/
      DATA ( NVALUE(1148,I),I=1,4)/138,140,0,11/
      DATA ( NVALUE(1149,I),I=1,4)/138,140,0,13/
      DATA ( NVALUE(1150,I),I=1,4)/138,140,0,10/
      DATA ( NVALUE(1151,I),I=1,4)/138,140,0,12/
      DATA ( NVALUE(1152,I),I=1,4)/138,140,10,11/
      DATA ( NVALUE(1153,I),I=1,4)/138,140,10,13/
      DATA ( NVALUE(1154,I),I=1,4)/138,140,11,12/
      DATA ( NVALUE(1155,I),I=1,4)/138,140,12,13/
      DATA ( NVALUE(1156,I),I=1,4)/138,140,0,151/
      DATA ( NVALUE(1157,I),I=1,4)/138,140,0,87/
      DATA ( NVALUE(1158,I),I=1,4)/138,140,6,151/
      DATA ( NVALUE(1159,I),I=1,4)/138,140,6,87/
      DATA ( NVALUE(1160,I),I=1,4)/138,140,151,0/
      DATA ( NVALUE(1161,I),I=1,4)/138,140,151,82/
      DATA ( NVALUE(1162,I),I=1,4)/138,140,151,84/
      DATA ( NVALUE(1163,I),I=1,4)/138,140,151,7/
      DATA ( NVALUE(1164,I),I=1,4)/138,140,0,87/
      DATA ( NVALUE(1165,I),I=1,4)/138,140,0,0/
      DATA ( NVALUE(1166,I),I=1,4)/138,140,0,8/
      DATA ( NVALUE(1167,I),I=1,4)/138,140,0,9/
      DATA ( NVALUE(1168,I),I=1,4)/138,140,0,0/
      DATA ( NVALUE(1169,I),I=1,4)/138,140,0,11/
      DATA ( NVALUE(1170,I),I=1,4)/138,140,0,13/
      DATA ( NVALUE(1171,I),I=1,4)/138,140,0,10/
      DATA ( NVALUE(1172,I),I=1,4)/138,140,0,12/
      DATA ( NVALUE(1173,I),I=1,4)/138,140,10,11/
      DATA ( NVALUE(1174,I),I=1,4)/138,140,10,13/
      DATA ( NVALUE(1175,I),I=1,4)/138,140,11,12/
      DATA ( NVALUE(1176,I),I=1,4)/138,140,12,13/
      DATA ( NVALUE(1177,I),I=1,4)/138,140,82,87/
      DATA ( NVALUE(1178,I),I=1,4)/138,140,84,87/
      DATA ( NVALUE(1179,I),I=1,4)/138,140,7,87/
      DATA ( NVALUE(1180,I),I=1,4)/138,142,0,10/
      DATA ( NVALUE(1181,I),I=1,4)/138,142,151,151/
      DATA ( NVALUE(1182,I),I=1,4)/138,142,151,87/
      DATA ( NVALUE(1183,I),I=1,4)/138,142,151,87/
      DATA ( NVALUE(1184,I),I=1,4)/138,142,0,10/
      DATA ( NVALUE(1185,I),I=1,4)/138,142,0,0/
      DATA ( NVALUE(1186,I),I=1,4)/138,142,0,11/
      DATA ( NVALUE(1187,I),I=1,4)/138,142,0,13/
      DATA ( NVALUE(1188,I),I=1,4)/138,142,0,11/
      DATA ( NVALUE(1189,I),I=1,4)/138,142,0,13/
      DATA ( NVALUE(1190,I),I=1,4)/138,142,11,11/
      DATA ( NVALUE(1191,I),I=1,4)/138,142,11,13/
      DATA ( NVALUE(1192,I),I=1,4)/138,142,11,13/
      DATA ( NVALUE(1193,I),I=1,4)/138,142,13,13/
      DATA ( NVALUE(1194,I),I=1,4)/138,142,87,87/
      DATA ( NVALUE(1195,I),I=1,4)/138,144,0,0/
      DATA ( NVALUE(1196,I),I=1,4)/138,144,0,11/
      DATA ( NVALUE(1197,I),I=1,4)/138,144,0,13/
      DATA ( NVALUE(1198,I),I=1,4)/138,144,0,10/
      DATA ( NVALUE(1199,I),I=1,4)/138,144,0,12/
      DATA ( NVALUE(1200,I),I=1,4)/138,144,10,11/
      DATA ( NVALUE(1201,I),I=1,4)/138,144,10,13/
      DATA ( NVALUE(1202,I),I=1,4)/138,144,11,12/
      DATA ( NVALUE(1203,I),I=1,4)/138,144,12,13/
      DATA ( NVALUE(1204,I),I=1,4)/138,144,0,151/
      DATA ( NVALUE(1205,I),I=1,4)/138,144,0,87/
      DATA ( NVALUE(1206,I),I=1,4)/138,144,6,151/
      DATA ( NVALUE(1207,I),I=1,4)/138,144,6,87/
      DATA ( NVALUE(1208,I),I=1,4)/138,144,151,0/
      DATA ( NVALUE(1209,I),I=1,4)/138,144,151,82/
      DATA ( NVALUE(1210,I),I=1,4)/138,144,151,84/
      DATA ( NVALUE(1211,I),I=1,4)/138,144,151,7/
      DATA ( NVALUE(1212,I),I=1,4)/138,144,0,87/
      DATA ( NVALUE(1213,I),I=1,4)/138,144,0,0/
      DATA ( NVALUE(1214,I),I=1,4)/138,144,0,8/
      DATA ( NVALUE(1215,I),I=1,4)/138,144,0,9/
      DATA ( NVALUE(1216,I),I=1,4)/138,144,0,0/
      DATA ( NVALUE(1217,I),I=1,4)/138,144,0,11/
      DATA ( NVALUE(1218,I),I=1,4)/138,144,0,13/
      DATA ( NVALUE(1219,I),I=1,4)/138,144,0,10/
      DATA ( NVALUE(1220,I),I=1,4)/138,144,0,12/
      DATA ( NVALUE(1221,I),I=1,4)/138,144,10,11/
      DATA ( NVALUE(1222,I),I=1,4)/138,144,10,13/
      DATA ( NVALUE(1223,I),I=1,4)/138,144,11,12/
      DATA ( NVALUE(1224,I),I=1,4)/138,144,12,13/
      DATA ( NVALUE(1225,I),I=1,4)/138,144,82,87/
      DATA ( NVALUE(1226,I),I=1,4)/138,144,84,87/
      DATA ( NVALUE(1227,I),I=1,4)/138,144,7,87/
      DATA ( NVALUE(1228,I),I=1,4)/138,122,0,12/
      DATA ( NVALUE(1229,I),I=1,4)/138,122,151,151/
      DATA ( NVALUE(1230,I),I=1,4)/138,122,151,87/
      DATA ( NVALUE(1231,I),I=1,4)/138,122,151,87/
      DATA ( NVALUE(1232,I),I=1,4)/138,122,0,12/
      DATA ( NVALUE(1233,I),I=1,4)/138,122,0,0/
      DATA ( NVALUE(1234,I),I=1,4)/138,122,0,11/
      DATA ( NVALUE(1235,I),I=1,4)/138,122,0,13/
      DATA ( NVALUE(1236,I),I=1,4)/138,122,0,11/
      DATA ( NVALUE(1237,I),I=1,4)/138,122,0,13/
      DATA ( NVALUE(1238,I),I=1,4)/138,122,11,11/
      DATA ( NVALUE(1239,I),I=1,4)/138,122,11,13/
      DATA ( NVALUE(1240,I),I=1,4)/138,122,11,13/
      DATA ( NVALUE(1241,I),I=1,4)/138,122,13,13/
      DATA ( NVALUE(1242,I),I=1,4)/138,122,87,87/
      DATA ( NVALUE(1243,I),I=1,4)/138,126,0,0/
      DATA ( NVALUE(1244,I),I=1,4)/138,126,0,11/
      DATA ( NVALUE(1245,I),I=1,4)/138,126,0,13/
      DATA ( NVALUE(1246,I),I=1,4)/138,126,0,10/
      DATA ( NVALUE(1247,I),I=1,4)/138,126,0,12/
      DATA ( NVALUE(1248,I),I=1,4)/138,126,10,11/
      DATA ( NVALUE(1249,I),I=1,4)/138,126,10,13/
      DATA ( NVALUE(1250,I),I=1,4)/138,126,11,12/
      DATA ( NVALUE(1251,I),I=1,4)/138,126,12,13/
      DATA ( NVALUE(1252,I),I=1,4)/138,126,0,151/
      DATA ( NVALUE(1253,I),I=1,4)/138,126,0,87/
      DATA ( NVALUE(1254,I),I=1,4)/138,126,6,151/
      DATA ( NVALUE(1255,I),I=1,4)/138,126,6,87/
      DATA ( NVALUE(1256,I),I=1,4)/138,126,151,0/
      DATA ( NVALUE(1257,I),I=1,4)/138,126,151,82/
      DATA ( NVALUE(1258,I),I=1,4)/138,126,151,84/
      DATA ( NVALUE(1259,I),I=1,4)/138,126,151,7/
      DATA ( NVALUE(1260,I),I=1,4)/138,126,0,87/
      DATA ( NVALUE(1261,I),I=1,4)/138,126,0,0/
      DATA ( NVALUE(1262,I),I=1,4)/138,126,0,8/
      DATA ( NVALUE(1263,I),I=1,4)/138,126,0,9/
      DATA ( NVALUE(1264,I),I=1,4)/138,126,0,0/
      DATA ( NVALUE(1265,I),I=1,4)/138,126,0,11/
      DATA ( NVALUE(1266,I),I=1,4)/138,126,0,13/
      DATA ( NVALUE(1267,I),I=1,4)/138,126,0,10/
      DATA ( NVALUE(1268,I),I=1,4)/138,126,0,12/
      DATA ( NVALUE(1269,I),I=1,4)/138,126,10,11/
      DATA ( NVALUE(1270,I),I=1,4)/138,126,10,13/
      DATA ( NVALUE(1271,I),I=1,4)/138,126,11,12/
      DATA ( NVALUE(1272,I),I=1,4)/138,126,12,13/
      DATA ( NVALUE(1273,I),I=1,4)/138,126,82,87/
      DATA ( NVALUE(1274,I),I=1,4)/138,126,84,87/
      DATA ( NVALUE(1275,I),I=1,4)/138,126,7,87/
      DATA ( NVALUE(1276,I),I=1,4)/138,131,0,0/
      DATA ( NVALUE(1277,I),I=1,4)/138,131,6,0/
      DATA ( NVALUE(1278,I),I=1,4)/138,131,151,0/
      DATA ( NVALUE(1279,I),I=1,4)/138,131,151,11/
      DATA ( NVALUE(1280,I),I=1,4)/138,131,151,13/
      DATA ( NVALUE(1281,I),I=1,4)/138,131,0,0/
      DATA ( NVALUE(1282,I),I=1,4)/138,131,0,82/
      DATA ( NVALUE(1283,I),I=1,4)/138,131,0,84/
      DATA ( NVALUE(1284,I),I=1,4)/138,131,0,87/
      DATA ( NVALUE(1285,I),I=1,4)/138,131,11,87/
      DATA ( NVALUE(1286,I),I=1,4)/138,131,13,87/
      DATA ( NVALUE(1287,I),I=1,4)/140,140,0,0/
      DATA ( NVALUE(1288,I),I=1,4)/140,140,0,0/
      DATA ( NVALUE(1289,I),I=1,4)/140,140,0,6/
      DATA ( NVALUE(1290,I),I=1,4)/140,140,0,0/
      DATA ( NVALUE(1291,I),I=1,4)/140,140,0,82/
      DATA ( NVALUE(1292,I),I=1,4)/140,140,0,84/
      DATA ( NVALUE(1293,I),I=1,4)/140,140,6,6/
      DATA ( NVALUE(1294,I),I=1,4)/140,140,6,0/
      DATA ( NVALUE(1295,I),I=1,4)/140,140,6,82/
      DATA ( NVALUE(1296,I),I=1,4)/140,140,6,84/
      DATA ( NVALUE(1297,I),I=1,4)/140,140,6,7/
      DATA ( NVALUE(1298,I),I=1,4)/140,140,151,151/
      DATA ( NVALUE(1299,I),I=1,4)/140,140,151,87/
      DATA ( NVALUE(1300,I),I=1,4)/140,140,151,87/
      DATA ( NVALUE(1301,I),I=1,4)/140,140,0,0/
      DATA ( NVALUE(1302,I),I=1,4)/140,140,0,82/
      DATA ( NVALUE(1303,I),I=1,4)/140,140,0,84/
      DATA ( NVALUE(1304,I),I=1,4)/140,140,0,0/
      DATA ( NVALUE(1305,I),I=1,4)/140,140,0,0/
      DATA ( NVALUE(1306,I),I=1,4)/140,140,0,0/
      DATA ( NVALUE(1307,I),I=1,4)/140,140,0,0/
      DATA ( NVALUE(1308,I),I=1,4)/140,140,8,8/
      DATA ( NVALUE(1309,I),I=1,4)/140,140,9,9/
      DATA ( NVALUE(1310,I),I=1,4)/140,140,0,0/
      DATA ( NVALUE(1311,I),I=1,4)/140,140,0,10/
      DATA ( NVALUE(1312,I),I=1,4)/140,140,0,12/
      DATA ( NVALUE(1313,I),I=1,4)/140,140,0,10/
      DATA ( NVALUE(1314,I),I=1,4)/140,140,0,12/
      DATA ( NVALUE(1315,I),I=1,4)/140,140,0,0/
      DATA ( NVALUE(1316,I),I=1,4)/140,140,10,10/
      DATA ( NVALUE(1317,I),I=1,4)/140,140,10,12/
      DATA ( NVALUE(1318,I),I=1,4)/140,140,10,12/
      DATA ( NVALUE(1319,I),I=1,4)/140,140,11,11/
      DATA ( NVALUE(1320,I),I=1,4)/140,140,12,12/
      DATA ( NVALUE(1321,I),I=1,4)/140,140,13,13/
      DATA ( NVALUE(1322,I),I=1,4)/140,140,82,82/
      DATA ( NVALUE(1323,I),I=1,4)/140,140,82,84/
      DATA ( NVALUE(1324,I),I=1,4)/140,140,82,7/
      DATA ( NVALUE(1325,I),I=1,4)/140,140,84,84/
      DATA ( NVALUE(1326,I),I=1,4)/140,140,84,7/
      DATA ( NVALUE(1327,I),I=1,4)/140,140,7,7/
      DATA ( NVALUE(1328,I),I=1,4)/140,140,87,87/
      DATA ( NVALUE(1329,I),I=1,4)/140,142,0,0/
      DATA ( NVALUE(1330,I),I=1,4)/140,142,0,11/
      DATA ( NVALUE(1331,I),I=1,4)/140,142,0,13/
      DATA ( NVALUE(1332,I),I=1,4)/140,142,0,10/
      DATA ( NVALUE(1333,I),I=1,4)/140,142,0,12/
      DATA ( NVALUE(1334,I),I=1,4)/140,142,10,11/
      DATA ( NVALUE(1335,I),I=1,4)/140,142,10,13/
      DATA ( NVALUE(1336,I),I=1,4)/140,142,11,12/
      DATA ( NVALUE(1337,I),I=1,4)/140,142,12,13/
      DATA ( NVALUE(1338,I),I=1,4)/140,142,0,151/
      DATA ( NVALUE(1339,I),I=1,4)/140,142,0,87/
      DATA ( NVALUE(1340,I),I=1,4)/140,142,6,151/
      DATA ( NVALUE(1341,I),I=1,4)/140,142,6,87/
      DATA ( NVALUE(1342,I),I=1,4)/140,142,151,0/
      DATA ( NVALUE(1343,I),I=1,4)/140,142,151,82/
      DATA ( NVALUE(1344,I),I=1,4)/140,142,151,84/
      DATA ( NVALUE(1345,I),I=1,4)/140,142,151,7/
      DATA ( NVALUE(1346,I),I=1,4)/140,142,0,87/
      DATA ( NVALUE(1347,I),I=1,4)/140,142,0,0/
      DATA ( NVALUE(1348,I),I=1,4)/140,142,0,8/
      DATA ( NVALUE(1349,I),I=1,4)/140,142,0,9/
      DATA ( NVALUE(1350,I),I=1,4)/140,142,0,0/
      DATA ( NVALUE(1351,I),I=1,4)/140,142,0,11/
      DATA ( NVALUE(1352,I),I=1,4)/140,142,0,13/
      DATA ( NVALUE(1353,I),I=1,4)/140,142,0,10/
      DATA ( NVALUE(1354,I),I=1,4)/140,142,0,12/
      DATA ( NVALUE(1355,I),I=1,4)/140,142,10,11/
      DATA ( NVALUE(1356,I),I=1,4)/140,142,10,13/
      DATA ( NVALUE(1357,I),I=1,4)/140,142,11,12/
      DATA ( NVALUE(1358,I),I=1,4)/140,142,12,13/
      DATA ( NVALUE(1359,I),I=1,4)/140,142,82,87/
      DATA ( NVALUE(1360,I),I=1,4)/140,142,84,87/
      DATA ( NVALUE(1361,I),I=1,4)/140,142,7,87/
      DATA ( NVALUE(1362,I),I=1,4)/140,144,0,11/
      DATA ( NVALUE(1363,I),I=1,4)/140,144,151,151/
      DATA ( NVALUE(1364,I),I=1,4)/140,144,151,87/
      DATA ( NVALUE(1365,I),I=1,4)/140,144,151,87/
      DATA ( NVALUE(1366,I),I=1,4)/140,144,0,0/
      DATA ( NVALUE(1367,I),I=1,4)/140,144,0,10/
      DATA ( NVALUE(1368,I),I=1,4)/140,144,0,12/
      DATA ( NVALUE(1369,I),I=1,4)/140,144,0,10/
      DATA ( NVALUE(1370,I),I=1,4)/140,144,0,12/
      DATA ( NVALUE(1371,I),I=1,4)/140,144,0,11/
      DATA ( NVALUE(1372,I),I=1,4)/140,144,10,10/
      DATA ( NVALUE(1373,I),I=1,4)/140,144,10,12/
      DATA ( NVALUE(1374,I),I=1,4)/140,144,10,12/
      DATA ( NVALUE(1375,I),I=1,4)/140,144,12,12/
      DATA ( NVALUE(1376,I),I=1,4)/140,144,87,87/
      DATA ( NVALUE(1377,I),I=1,4)/140,122,0,0/
      DATA ( NVALUE(1378,I),I=1,4)/140,122,0,11/
      DATA ( NVALUE(1379,I),I=1,4)/140,122,0,13/
      DATA ( NVALUE(1380,I),I=1,4)/140,122,0,10/
      DATA ( NVALUE(1381,I),I=1,4)/140,122,0,12/
      DATA ( NVALUE(1382,I),I=1,4)/140,122,10,11/
      DATA ( NVALUE(1383,I),I=1,4)/140,122,10,13/
      DATA ( NVALUE(1384,I),I=1,4)/140,122,11,12/
      DATA ( NVALUE(1385,I),I=1,4)/140,122,12,13/
      DATA ( NVALUE(1386,I),I=1,4)/140,122,0,151/
      DATA ( NVALUE(1387,I),I=1,4)/140,122,0,87/
      DATA ( NVALUE(1388,I),I=1,4)/140,122,6,151/
      DATA ( NVALUE(1389,I),I=1,4)/140,122,6,87/
      DATA ( NVALUE(1390,I),I=1,4)/140,122,151,0/
      DATA ( NVALUE(1391,I),I=1,4)/140,122,151,82/
      DATA ( NVALUE(1392,I),I=1,4)/140,122,151,84/
      DATA ( NVALUE(1393,I),I=1,4)/140,122,151,7/
      DATA ( NVALUE(1394,I),I=1,4)/140,122,0,87/
      DATA ( NVALUE(1395,I),I=1,4)/140,122,0,0/
      DATA ( NVALUE(1396,I),I=1,4)/140,122,0,8/
      DATA ( NVALUE(1397,I),I=1,4)/140,122,0,9/
      DATA ( NVALUE(1398,I),I=1,4)/140,122,0,0/
      DATA ( NVALUE(1399,I),I=1,4)/140,122,0,11/
      DATA ( NVALUE(1400,I),I=1,4)/140,122,0,13/
      DATA ( NVALUE(1401,I),I=1,4)/140,122,0,10/
      DATA ( NVALUE(1402,I),I=1,4)/140,122,0,12/
      DATA ( NVALUE(1403,I),I=1,4)/140,122,10,11/
      DATA ( NVALUE(1404,I),I=1,4)/140,122,10,13/
      DATA ( NVALUE(1405,I),I=1,4)/140,122,11,12/
      DATA ( NVALUE(1406,I),I=1,4)/140,122,12,13/
      DATA ( NVALUE(1407,I),I=1,4)/140,122,82,87/
      DATA ( NVALUE(1408,I),I=1,4)/140,122,84,87/
      DATA ( NVALUE(1409,I),I=1,4)/140,122,7,87/
      DATA ( NVALUE(1410,I),I=1,4)/140,126,0,13/
      DATA ( NVALUE(1411,I),I=1,4)/140,126,151,151/
      DATA ( NVALUE(1412,I),I=1,4)/140,126,151,87/
      DATA ( NVALUE(1413,I),I=1,4)/140,126,151,87/
      DATA ( NVALUE(1414,I),I=1,4)/140,126,0,0/
      DATA ( NVALUE(1415,I),I=1,4)/140,126,0,10/
      DATA ( NVALUE(1416,I),I=1,4)/140,126,0,12/
      DATA ( NVALUE(1417,I),I=1,4)/140,126,0,10/
      DATA ( NVALUE(1418,I),I=1,4)/140,126,0,12/
      DATA ( NVALUE(1419,I),I=1,4)/140,126,0,13/
      DATA ( NVALUE(1420,I),I=1,4)/140,126,10,10/
      DATA ( NVALUE(1421,I),I=1,4)/140,126,10,12/
      DATA ( NVALUE(1422,I),I=1,4)/140,126,10,12/
      DATA ( NVALUE(1423,I),I=1,4)/140,126,12,12/
      DATA ( NVALUE(1424,I),I=1,4)/140,126,87,87/
      DATA ( NVALUE(1425,I),I=1,4)/140,131,0,0/
      DATA ( NVALUE(1426,I),I=1,4)/140,131,6,0/
      DATA ( NVALUE(1427,I),I=1,4)/140,131,151,0/
      DATA ( NVALUE(1428,I),I=1,4)/140,131,151,10/
      DATA ( NVALUE(1429,I),I=1,4)/140,131,151,12/
      DATA ( NVALUE(1430,I),I=1,4)/140,131,0,0/
      DATA ( NVALUE(1431,I),I=1,4)/140,131,0,87/
      DATA ( NVALUE(1432,I),I=1,4)/140,131,0,82/
      DATA ( NVALUE(1433,I),I=1,4)/140,131,0,84/
      DATA ( NVALUE(1434,I),I=1,4)/140,131,10,87/
      DATA ( NVALUE(1435,I),I=1,4)/140,131,12,87/
      DATA ( NVALUE(1436,I),I=1,4)/142,142,10,10/
      DATA ( NVALUE(1437,I),I=1,4)/142,142,0,0/
      DATA ( NVALUE(1438,I),I=1,4)/142,142,0,6/
      DATA ( NVALUE(1439,I),I=1,4)/142,142,0,0/
      DATA ( NVALUE(1440,I),I=1,4)/142,142,0,82/
      DATA ( NVALUE(1441,I),I=1,4)/142,142,0,84/
      DATA ( NVALUE(1442,I),I=1,4)/142,142,6,6/
      DATA ( NVALUE(1443,I),I=1,4)/142,142,6,0/
      DATA ( NVALUE(1444,I),I=1,4)/142,142,6,82/
      DATA ( NVALUE(1445,I),I=1,4)/142,142,6,84/
      DATA ( NVALUE(1446,I),I=1,4)/142,142,6,7/
      DATA ( NVALUE(1447,I),I=1,4)/142,142,151,151/
      DATA ( NVALUE(1448,I),I=1,4)/142,142,151,87/
      DATA ( NVALUE(1449,I),I=1,4)/142,142,151,87/
      DATA ( NVALUE(1450,I),I=1,4)/142,142,0,0/
      DATA ( NVALUE(1451,I),I=1,4)/142,142,0,82/
      DATA ( NVALUE(1452,I),I=1,4)/142,142,0,84/
      DATA ( NVALUE(1453,I),I=1,4)/142,142,0,0/
      DATA ( NVALUE(1454,I),I=1,4)/142,142,0,0/
      DATA ( NVALUE(1455,I),I=1,4)/142,142,0,0/
      DATA ( NVALUE(1456,I),I=1,4)/142,142,0,0/
      DATA ( NVALUE(1457,I),I=1,4)/142,142,8,8/
      DATA ( NVALUE(1458,I),I=1,4)/142,142,9,9/
      DATA ( NVALUE(1459,I),I=1,4)/142,142,0,0/
      DATA ( NVALUE(1460,I),I=1,4)/142,142,0,0/
      DATA ( NVALUE(1461,I),I=1,4)/142,142,0,11/
      DATA ( NVALUE(1462,I),I=1,4)/142,142,0,13/
      DATA ( NVALUE(1463,I),I=1,4)/142,142,0,11/
      DATA ( NVALUE(1464,I),I=1,4)/142,142,0,13/
      DATA ( NVALUE(1465,I),I=1,4)/142,142,10,10/
      DATA ( NVALUE(1466,I),I=1,4)/142,142,11,11/
      DATA ( NVALUE(1467,I),I=1,4)/142,142,11,13/
      DATA ( NVALUE(1468,I),I=1,4)/142,142,11,13/
      DATA ( NVALUE(1469,I),I=1,4)/142,142,12,12/
      DATA ( NVALUE(1470,I),I=1,4)/142,142,13,13/
      DATA ( NVALUE(1471,I),I=1,4)/142,142,82,82/
      DATA ( NVALUE(1472,I),I=1,4)/142,142,82,84/
      DATA ( NVALUE(1473,I),I=1,4)/142,142,82,7/
      DATA ( NVALUE(1474,I),I=1,4)/142,142,84,84/
      DATA ( NVALUE(1475,I),I=1,4)/142,142,84,7/
      DATA ( NVALUE(1476,I),I=1,4)/142,142,7,7/
      DATA ( NVALUE(1477,I),I=1,4)/142,142,87,87/
      DATA ( NVALUE(1478,I),I=1,4)/142,144,0,0/
      DATA ( NVALUE(1479,I),I=1,4)/142,144,0,11/
      DATA ( NVALUE(1480,I),I=1,4)/142,144,0,13/
      DATA ( NVALUE(1481,I),I=1,4)/142,144,0,10/
      DATA ( NVALUE(1482,I),I=1,4)/142,144,0,12/
      DATA ( NVALUE(1483,I),I=1,4)/142,144,10,11/
      DATA ( NVALUE(1484,I),I=1,4)/142,144,10,13/
      DATA ( NVALUE(1485,I),I=1,4)/142,144,11,12/
      DATA ( NVALUE(1486,I),I=1,4)/142,144,12,13/
      DATA ( NVALUE(1487,I),I=1,4)/142,144,0,151/
      DATA ( NVALUE(1488,I),I=1,4)/142,144,0,87/
      DATA ( NVALUE(1489,I),I=1,4)/142,144,6,151/
      DATA ( NVALUE(1490,I),I=1,4)/142,144,6,87/
      DATA ( NVALUE(1491,I),I=1,4)/142,144,151,0/
      DATA ( NVALUE(1492,I),I=1,4)/142,144,151,82/
      DATA ( NVALUE(1493,I),I=1,4)/142,144,151,84/
      DATA ( NVALUE(1494,I),I=1,4)/142,144,151,7/
      DATA ( NVALUE(1495,I),I=1,4)/142,144,0,87/
      DATA ( NVALUE(1496,I),I=1,4)/142,144,0,0/
      DATA ( NVALUE(1497,I),I=1,4)/142,144,0,8/
      DATA ( NVALUE(1498,I),I=1,4)/142,144,0,9/
      DATA ( NVALUE(1499,I),I=1,4)/142,144,0,0/
      DATA ( NVALUE(1500,I),I=1,4)/142,144,0,11/
      DATA ( NVALUE(1501,I),I=1,4)/142,144,0,13/
      DATA ( NVALUE(1502,I),I=1,4)/142,144,0,10/
      DATA ( NVALUE(1503,I),I=1,4)/142,144,0,12/
      DATA ( NVALUE(1504,I),I=1,4)/142,144,10,11/
      DATA ( NVALUE(1505,I),I=1,4)/142,144,10,13/
      DATA ( NVALUE(1506,I),I=1,4)/142,144,11,12/
      DATA ( NVALUE(1507,I),I=1,4)/142,144,12,13/
      DATA ( NVALUE(1508,I),I=1,4)/142,144,82,87/
      DATA ( NVALUE(1509,I),I=1,4)/142,144,84,87/
      DATA ( NVALUE(1510,I),I=1,4)/142,144,7,87/
      DATA ( NVALUE(1511,I),I=1,4)/142,122,10,12/
      DATA ( NVALUE(1512,I),I=1,4)/142,122,151,151/
      DATA ( NVALUE(1513,I),I=1,4)/142,122,151,87/
      DATA ( NVALUE(1514,I),I=1,4)/142,122,151,87/
      DATA ( NVALUE(1515,I),I=1,4)/142,122,0,0/
      DATA ( NVALUE(1516,I),I=1,4)/142,122,0,11/
      DATA ( NVALUE(1517,I),I=1,4)/142,122,0,13/
      DATA ( NVALUE(1518,I),I=1,4)/142,122,0,11/
      DATA ( NVALUE(1519,I),I=1,4)/142,122,0,13/
      DATA ( NVALUE(1520,I),I=1,4)/142,122,10,12/
      DATA ( NVALUE(1521,I),I=1,4)/142,122,11,11/
      DATA ( NVALUE(1522,I),I=1,4)/142,122,11,13/
      DATA ( NVALUE(1523,I),I=1,4)/142,122,11,13/
      DATA ( NVALUE(1524,I),I=1,4)/142,122,13,13/
      DATA ( NVALUE(1525,I),I=1,4)/142,122,87,87/
      DATA ( NVALUE(1526,I),I=1,4)/142,126,0,0/
      DATA ( NVALUE(1527,I),I=1,4)/142,126,0,11/
      DATA ( NVALUE(1528,I),I=1,4)/142,126,0,13/
      DATA ( NVALUE(1529,I),I=1,4)/142,126,0,10/
      DATA ( NVALUE(1530,I),I=1,4)/142,126,0,12/
      DATA ( NVALUE(1531,I),I=1,4)/142,126,10,11/
      DATA ( NVALUE(1532,I),I=1,4)/142,126,10,13/
      DATA ( NVALUE(1533,I),I=1,4)/142,126,11,12/
      DATA ( NVALUE(1534,I),I=1,4)/142,126,12,13/
      DATA ( NVALUE(1535,I),I=1,4)/142,126,0,151/
      DATA ( NVALUE(1536,I),I=1,4)/142,126,0,87/
      DATA ( NVALUE(1537,I),I=1,4)/142,126,6,151/
      DATA ( NVALUE(1538,I),I=1,4)/142,126,6,87/
      DATA ( NVALUE(1539,I),I=1,4)/142,126,151,0/
      DATA ( NVALUE(1540,I),I=1,4)/142,126,151,82/
      DATA ( NVALUE(1541,I),I=1,4)/142,126,151,84/
      DATA ( NVALUE(1542,I),I=1,4)/142,126,151,7/
      DATA ( NVALUE(1543,I),I=1,4)/142,126,0,87/
      DATA ( NVALUE(1544,I),I=1,4)/142,126,0,0/
      DATA ( NVALUE(1545,I),I=1,4)/142,126,0,8/
      DATA ( NVALUE(1546,I),I=1,4)/142,126,0,9/
      DATA ( NVALUE(1547,I),I=1,4)/142,126,0,0/
      DATA ( NVALUE(1548,I),I=1,4)/142,126,0,11/
      DATA ( NVALUE(1549,I),I=1,4)/142,126,0,13/
      DATA ( NVALUE(1550,I),I=1,4)/142,126,0,10/
      DATA ( NVALUE(1551,I),I=1,4)/142,126,0,12/
      DATA ( NVALUE(1552,I),I=1,4)/142,126,10,11/
      DATA ( NVALUE(1553,I),I=1,4)/142,126,10,13/
      DATA ( NVALUE(1554,I),I=1,4)/142,126,11,12/
      DATA ( NVALUE(1555,I),I=1,4)/142,126,12,13/
      DATA ( NVALUE(1556,I),I=1,4)/142,126,82,87/
      DATA ( NVALUE(1557,I),I=1,4)/142,126,84,87/
      DATA ( NVALUE(1558,I),I=1,4)/142,126,7,87/
      DATA ( NVALUE(1559,I),I=1,4)/142,131,0,10/
      DATA ( NVALUE(1560,I),I=1,4)/142,131,6,10/
      DATA ( NVALUE(1561,I),I=1,4)/142,131,151,0/
      DATA ( NVALUE(1562,I),I=1,4)/142,131,151,11/
      DATA ( NVALUE(1563,I),I=1,4)/142,131,151,13/
      DATA ( NVALUE(1564,I),I=1,4)/142,131,0,10/
      DATA ( NVALUE(1565,I),I=1,4)/142,131,0,87/
      DATA ( NVALUE(1566,I),I=1,4)/142,131,10,82/
      DATA ( NVALUE(1567,I),I=1,4)/142,131,10,84/
      DATA ( NVALUE(1568,I),I=1,4)/142,131,10,7/
      DATA ( NVALUE(1569,I),I=1,4)/142,131,11,87/
      DATA ( NVALUE(1570,I),I=1,4)/142,131,13,87/
      DATA ( NVALUE(1571,I),I=1,4)/144,144,11,11/
      DATA ( NVALUE(1572,I),I=1,4)/144,144,0,0/
      DATA ( NVALUE(1573,I),I=1,4)/144,144,0,6/
      DATA ( NVALUE(1574,I),I=1,4)/144,144,0,0/
      DATA ( NVALUE(1575,I),I=1,4)/144,144,0,82/
      DATA ( NVALUE(1576,I),I=1,4)/144,144,0,84/
      DATA ( NVALUE(1577,I),I=1,4)/144,144,6,6/
      DATA ( NVALUE(1578,I),I=1,4)/144,144,6,0/
      DATA ( NVALUE(1579,I),I=1,4)/144,144,6,82/
      DATA ( NVALUE(1580,I),I=1,4)/144,144,6,84/
      DATA ( NVALUE(1581,I),I=1,4)/144,144,6,7/
      DATA ( NVALUE(1582,I),I=1,4)/144,144,151,151/
      DATA ( NVALUE(1583,I),I=1,4)/144,144,151,87/
      DATA ( NVALUE(1584,I),I=1,4)/144,144,151,87/
      DATA ( NVALUE(1585,I),I=1,4)/144,144,0,0/
      DATA ( NVALUE(1586,I),I=1,4)/144,144,0,82/
      DATA ( NVALUE(1587,I),I=1,4)/144,144,0,84/
      DATA ( NVALUE(1588,I),I=1,4)/144,144,0,0/
      DATA ( NVALUE(1589,I),I=1,4)/144,144,0,0/
      DATA ( NVALUE(1590,I),I=1,4)/144,144,0,0/
      DATA ( NVALUE(1591,I),I=1,4)/144,144,0,0/
      DATA ( NVALUE(1592,I),I=1,4)/144,144,8,8/
      DATA ( NVALUE(1593,I),I=1,4)/144,144,9,9/
      DATA ( NVALUE(1594,I),I=1,4)/144,144,0,0/
      DATA ( NVALUE(1595,I),I=1,4)/144,144,0,10/
      DATA ( NVALUE(1596,I),I=1,4)/144,144,0,12/
      DATA ( NVALUE(1597,I),I=1,4)/144,144,0,10/
      DATA ( NVALUE(1598,I),I=1,4)/144,144,0,12/
      DATA ( NVALUE(1599,I),I=1,4)/144,144,0,0/
      DATA ( NVALUE(1600,I),I=1,4)/144,144,10,10/
      DATA ( NVALUE(1601,I),I=1,4)/144,144,10,12/
      DATA ( NVALUE(1602,I),I=1,4)/144,144,10,12/
      DATA ( NVALUE(1603,I),I=1,4)/144,144,11,11/
      DATA ( NVALUE(1604,I),I=1,4)/144,144,12,12/
      DATA ( NVALUE(1605,I),I=1,4)/144,144,13,13/
      DATA ( NVALUE(1606,I),I=1,4)/144,144,82,82/
      DATA ( NVALUE(1607,I),I=1,4)/144,144,82,84/
      DATA ( NVALUE(1608,I),I=1,4)/144,144,82,7/
      DATA ( NVALUE(1609,I),I=1,4)/144,144,84,84/
      DATA ( NVALUE(1610,I),I=1,4)/144,144,84,7/
      DATA ( NVALUE(1611,I),I=1,4)/144,144,7,7/
      DATA ( NVALUE(1612,I),I=1,4)/144,144,87,87/
      DATA ( NVALUE(1613,I),I=1,4)/144,122,0,0/
      DATA ( NVALUE(1614,I),I=1,4)/144,122,0,11/
      DATA ( NVALUE(1615,I),I=1,4)/144,122,0,13/
      DATA ( NVALUE(1616,I),I=1,4)/144,122,0,10/
      DATA ( NVALUE(1617,I),I=1,4)/144,122,0,12/
      DATA ( NVALUE(1618,I),I=1,4)/144,122,10,11/
      DATA ( NVALUE(1619,I),I=1,4)/144,122,10,13/
      DATA ( NVALUE(1620,I),I=1,4)/144,122,11,12/
      DATA ( NVALUE(1621,I),I=1,4)/144,122,12,13/
      DATA ( NVALUE(1622,I),I=1,4)/144,122,0,151/
      DATA ( NVALUE(1623,I),I=1,4)/144,122,0,87/
      DATA ( NVALUE(1624,I),I=1,4)/144,122,6,151/
      DATA ( NVALUE(1625,I),I=1,4)/144,122,6,87/
      DATA ( NVALUE(1626,I),I=1,4)/144,122,151,0/
      DATA ( NVALUE(1627,I),I=1,4)/144,122,151,82/
      DATA ( NVALUE(1628,I),I=1,4)/144,122,151,84/
      DATA ( NVALUE(1629,I),I=1,4)/144,122,151,7/
      DATA ( NVALUE(1630,I),I=1,4)/144,122,0,87/
      DATA ( NVALUE(1631,I),I=1,4)/144,122,0,0/
      DATA ( NVALUE(1632,I),I=1,4)/144,122,0,8/
      DATA ( NVALUE(1633,I),I=1,4)/144,122,0,9/
      DATA ( NVALUE(1634,I),I=1,4)/144,122,0,0/
      DATA ( NVALUE(1635,I),I=1,4)/144,122,0,11/
      DATA ( NVALUE(1636,I),I=1,4)/144,122,0,13/
      DATA ( NVALUE(1637,I),I=1,4)/144,122,0,10/
      DATA ( NVALUE(1638,I),I=1,4)/144,122,0,12/
      DATA ( NVALUE(1639,I),I=1,4)/144,122,10,11/
      DATA ( NVALUE(1640,I),I=1,4)/144,122,10,13/
      DATA ( NVALUE(1641,I),I=1,4)/144,122,11,12/
      DATA ( NVALUE(1642,I),I=1,4)/144,122,12,13/
      DATA ( NVALUE(1643,I),I=1,4)/144,122,82,87/
      DATA ( NVALUE(1644,I),I=1,4)/144,122,84,87/
      DATA ( NVALUE(1645,I),I=1,4)/144,122,7,87/
      DATA ( NVALUE(1646,I),I=1,4)/144,126,11,13/
      DATA ( NVALUE(1647,I),I=1,4)/144,126,151,151/
      DATA ( NVALUE(1648,I),I=1,4)/144,126,151,87/
      DATA ( NVALUE(1649,I),I=1,4)/144,126,151,87/
      DATA ( NVALUE(1650,I),I=1,4)/144,126,0,0/
      DATA ( NVALUE(1651,I),I=1,4)/144,126,0,10/
      DATA ( NVALUE(1652,I),I=1,4)/144,126,0,12/
      DATA ( NVALUE(1653,I),I=1,4)/144,126,0,10/
      DATA ( NVALUE(1654,I),I=1,4)/144,126,0,12/
      DATA ( NVALUE(1655,I),I=1,4)/144,126,10,10/
      DATA ( NVALUE(1656,I),I=1,4)/144,126,10,12/
      DATA ( NVALUE(1657,I),I=1,4)/144,126,10,12/
      DATA ( NVALUE(1658,I),I=1,4)/144,126,11,13/
      DATA ( NVALUE(1659,I),I=1,4)/144,126,12,12/
      DATA ( NVALUE(1660,I),I=1,4)/144,126,87,87/
      DATA ( NVALUE(1661,I),I=1,4)/144,131,0,11/
      DATA ( NVALUE(1662,I),I=1,4)/144,131,6,11/
      DATA ( NVALUE(1663,I),I=1,4)/144,131,151,0/
      DATA ( NVALUE(1664,I),I=1,4)/144,131,151,10/
      DATA ( NVALUE(1665,I),I=1,4)/144,131,151,12/
      DATA ( NVALUE(1666,I),I=1,4)/144,131,0,11/
      DATA ( NVALUE(1667,I),I=1,4)/144,131,0,87/
      DATA ( NVALUE(1668,I),I=1,4)/144,131,10,87/
      DATA ( NVALUE(1669,I),I=1,4)/144,131,11,82/
      DATA ( NVALUE(1670,I),I=1,4)/144,131,11,84/
      DATA ( NVALUE(1671,I),I=1,4)/144,131,11,7/
      DATA ( NVALUE(1672,I),I=1,4)/144,131,12,87/
      DATA ( NVALUE(1673,I),I=1,4)/122,122,12,12/
      DATA ( NVALUE(1674,I),I=1,4)/122,122,0,0/
      DATA ( NVALUE(1675,I),I=1,4)/122,122,0,6/
      DATA ( NVALUE(1676,I),I=1,4)/122,122,0,0/
      DATA ( NVALUE(1677,I),I=1,4)/122,122,0,82/
      DATA ( NVALUE(1678,I),I=1,4)/122,122,0,84/
      DATA ( NVALUE(1679,I),I=1,4)/122,122,6,6/
      DATA ( NVALUE(1680,I),I=1,4)/122,122,6,0/
      DATA ( NVALUE(1681,I),I=1,4)/122,122,6,82/
      DATA ( NVALUE(1682,I),I=1,4)/122,122,6,84/
      DATA ( NVALUE(1683,I),I=1,4)/122,122,6,7/
      DATA ( NVALUE(1684,I),I=1,4)/122,122,151,151/
      DATA ( NVALUE(1685,I),I=1,4)/122,122,151,87/
      DATA ( NVALUE(1686,I),I=1,4)/122,122,151,87/
      DATA ( NVALUE(1687,I),I=1,4)/122,122,0,0/
      DATA ( NVALUE(1688,I),I=1,4)/122,122,0,82/
      DATA ( NVALUE(1689,I),I=1,4)/122,122,0,84/
      DATA ( NVALUE(1690,I),I=1,4)/122,122,0,0/
      DATA ( NVALUE(1691,I),I=1,4)/122,122,0,0/
      DATA ( NVALUE(1692,I),I=1,4)/122,122,0,0/
      DATA ( NVALUE(1693,I),I=1,4)/122,122,0,0/
      DATA ( NVALUE(1694,I),I=1,4)/122,122,8,8/
      DATA ( NVALUE(1695,I),I=1,4)/122,122,9,9/
      DATA ( NVALUE(1696,I),I=1,4)/122,122,0,0/
      DATA ( NVALUE(1697,I),I=1,4)/122,122,0,0/
      DATA ( NVALUE(1698,I),I=1,4)/122,122,0,11/
      DATA ( NVALUE(1699,I),I=1,4)/122,122,0,13/
      DATA ( NVALUE(1700,I),I=1,4)/122,122,0,11/
      DATA ( NVALUE(1701,I),I=1,4)/122,122,0,13/
      DATA ( NVALUE(1702,I),I=1,4)/122,122,10,10/
      DATA ( NVALUE(1703,I),I=1,4)/122,122,11,11/
      DATA ( NVALUE(1704,I),I=1,4)/122,122,11,13/
      DATA ( NVALUE(1705,I),I=1,4)/122,122,11,13/
      DATA ( NVALUE(1706,I),I=1,4)/122,122,12,12/
      DATA ( NVALUE(1707,I),I=1,4)/122,122,13,13/
      DATA ( NVALUE(1708,I),I=1,4)/122,122,82,82/
      DATA ( NVALUE(1709,I),I=1,4)/122,122,82,84/
      DATA ( NVALUE(1710,I),I=1,4)/122,122,82,7/
      DATA ( NVALUE(1711,I),I=1,4)/122,122,84,84/
      DATA ( NVALUE(1712,I),I=1,4)/122,122,84,7/
      DATA ( NVALUE(1713,I),I=1,4)/122,122,7,7/
      DATA ( NVALUE(1714,I),I=1,4)/122,122,87,87/
      DATA ( NVALUE(1715,I),I=1,4)/122,126,0,0/
      DATA ( NVALUE(1716,I),I=1,4)/122,126,0,11/
      DATA ( NVALUE(1717,I),I=1,4)/122,126,0,13/
      DATA ( NVALUE(1718,I),I=1,4)/122,126,0,10/
      DATA ( NVALUE(1719,I),I=1,4)/122,126,0,12/
      DATA ( NVALUE(1720,I),I=1,4)/122,126,10,11/
      DATA ( NVALUE(1721,I),I=1,4)/122,126,10,13/
      DATA ( NVALUE(1722,I),I=1,4)/122,126,11,12/
      DATA ( NVALUE(1723,I),I=1,4)/122,126,12,13/
      DATA ( NVALUE(1724,I),I=1,4)/122,126,0,151/
      DATA ( NVALUE(1725,I),I=1,4)/122,126,0,87/
      DATA ( NVALUE(1726,I),I=1,4)/122,126,6,151/
      DATA ( NVALUE(1727,I),I=1,4)/122,126,6,87/
      DATA ( NVALUE(1728,I),I=1,4)/122,126,151,0/
      DATA ( NVALUE(1729,I),I=1,4)/122,126,151,82/
      DATA ( NVALUE(1730,I),I=1,4)/122,126,151,84/
      DATA ( NVALUE(1731,I),I=1,4)/122,126,151,7/
      DATA ( NVALUE(1732,I),I=1,4)/122,126,0,87/
      DATA ( NVALUE(1733,I),I=1,4)/122,126,0,0/
      DATA ( NVALUE(1734,I),I=1,4)/122,126,0,8/
      DATA ( NVALUE(1735,I),I=1,4)/122,126,0,9/
      DATA ( NVALUE(1736,I),I=1,4)/122,126,0,0/
      DATA ( NVALUE(1737,I),I=1,4)/122,126,0,11/
      DATA ( NVALUE(1738,I),I=1,4)/122,126,0,13/
      DATA ( NVALUE(1739,I),I=1,4)/122,126,0,10/
      DATA ( NVALUE(1740,I),I=1,4)/122,126,0,12/
      DATA ( NVALUE(1741,I),I=1,4)/122,126,10,11/
      DATA ( NVALUE(1742,I),I=1,4)/122,126,10,13/
      DATA ( NVALUE(1743,I),I=1,4)/122,126,11,12/
      DATA ( NVALUE(1744,I),I=1,4)/122,126,12,13/
      DATA ( NVALUE(1745,I),I=1,4)/122,126,82,87/
      DATA ( NVALUE(1746,I),I=1,4)/122,126,84,87/
      DATA ( NVALUE(1747,I),I=1,4)/122,126,7,87/
      DATA ( NVALUE(1748,I),I=1,4)/122,131,0,12/
      DATA ( NVALUE(1749,I),I=1,4)/122,131,6,12/
      DATA ( NVALUE(1750,I),I=1,4)/122,131,151,0/
      DATA ( NVALUE(1751,I),I=1,4)/122,131,151,11/
      DATA ( NVALUE(1752,I),I=1,4)/122,131,151,13/
      DATA ( NVALUE(1753,I),I=1,4)/122,131,0,12/
      DATA ( NVALUE(1754,I),I=1,4)/122,131,0,87/
      DATA ( NVALUE(1755,I),I=1,4)/122,131,11,87/
      DATA ( NVALUE(1756,I),I=1,4)/122,131,12,82/
      DATA ( NVALUE(1757,I),I=1,4)/122,131,12,84/
      DATA ( NVALUE(1758,I),I=1,4)/122,131,12,7/
      DATA ( NVALUE(1759,I),I=1,4)/122,131,13,87/
      DATA ( NVALUE(1760,I),I=1,4)/126,126,13,13/
      DATA ( NVALUE(1761,I),I=1,4)/126,126,0,0/
      DATA ( NVALUE(1762,I),I=1,4)/126,126,0,6/
      DATA ( NVALUE(1763,I),I=1,4)/126,126,0,0/
      DATA ( NVALUE(1764,I),I=1,4)/126,126,0,82/
      DATA ( NVALUE(1765,I),I=1,4)/126,126,0,84/
      DATA ( NVALUE(1766,I),I=1,4)/126,126,6,6/
      DATA ( NVALUE(1767,I),I=1,4)/126,126,6,0/
      DATA ( NVALUE(1768,I),I=1,4)/126,126,6,82/
      DATA ( NVALUE(1769,I),I=1,4)/126,126,6,84/
      DATA ( NVALUE(1770,I),I=1,4)/126,126,6,7/
      DATA ( NVALUE(1771,I),I=1,4)/126,126,151,151/
      DATA ( NVALUE(1772,I),I=1,4)/126,126,151,87/
      DATA ( NVALUE(1773,I),I=1,4)/126,126,151,87/
      DATA ( NVALUE(1774,I),I=1,4)/126,126,0,0/
      DATA ( NVALUE(1775,I),I=1,4)/126,126,0,82/
      DATA ( NVALUE(1776,I),I=1,4)/126,126,0,84/
      DATA ( NVALUE(1777,I),I=1,4)/126,126,0,0/
      DATA ( NVALUE(1778,I),I=1,4)/126,126,0,0/
      DATA ( NVALUE(1779,I),I=1,4)/126,126,0,0/
      DATA ( NVALUE(1780,I),I=1,4)/126,126,0,0/
      DATA ( NVALUE(1781,I),I=1,4)/126,126,8,8/
      DATA ( NVALUE(1782,I),I=1,4)/126,126,9,9/
      DATA ( NVALUE(1783,I),I=1,4)/126,126,0,0/
      DATA ( NVALUE(1784,I),I=1,4)/126,126,0,10/
      DATA ( NVALUE(1785,I),I=1,4)/126,126,0,12/
      DATA ( NVALUE(1786,I),I=1,4)/126,126,0,10/
      DATA ( NVALUE(1787,I),I=1,4)/126,126,0,12/
      DATA ( NVALUE(1788,I),I=1,4)/126,126,0,0/
      DATA ( NVALUE(1789,I),I=1,4)/126,126,10,10/
      DATA ( NVALUE(1790,I),I=1,4)/126,126,10,12/
      DATA ( NVALUE(1791,I),I=1,4)/126,126,10,12/
      DATA ( NVALUE(1792,I),I=1,4)/126,126,11,11/
      DATA ( NVALUE(1793,I),I=1,4)/126,126,12,12/
      DATA ( NVALUE(1794,I),I=1,4)/126,126,13,13/
      DATA ( NVALUE(1795,I),I=1,4)/126,126,82,82/
      DATA ( NVALUE(1796,I),I=1,4)/126,126,82,84/
      DATA ( NVALUE(1797,I),I=1,4)/126,126,82,7/
      DATA ( NVALUE(1798,I),I=1,4)/126,126,84,84/
      DATA ( NVALUE(1799,I),I=1,4)/126,126,84,7/
      DATA ( NVALUE(1800,I),I=1,4)/126,126,7,7/
      DATA ( NVALUE(1801,I),I=1,4)/126,126,87,87/
      DATA ( NVALUE(1802,I),I=1,4)/126,131,0,13/
      DATA ( NVALUE(1803,I),I=1,4)/126,131,6,13/
      DATA ( NVALUE(1804,I),I=1,4)/126,131,151,0/
      DATA ( NVALUE(1805,I),I=1,4)/126,131,151,10/
      DATA ( NVALUE(1806,I),I=1,4)/126,131,151,12/
      DATA ( NVALUE(1807,I),I=1,4)/126,131,0,13/
      DATA ( NVALUE(1808,I),I=1,4)/126,131,0,87/
      DATA ( NVALUE(1809,I),I=1,4)/126,131,10,87/
      DATA ( NVALUE(1810,I),I=1,4)/126,131,12,87/
      DATA ( NVALUE(1811,I),I=1,4)/126,131,13,82/
      DATA ( NVALUE(1812,I),I=1,4)/126,131,13,84/
      DATA ( NVALUE(1813,I),I=1,4)/126,131,13,7/
      DATA ( NVALUE(1814,I),I=1,4)/131,131,0,0/
      DATA ( NVALUE(1815,I),I=1,4)/131,131,0,0/
      DATA ( NVALUE(1816,I),I=1,4)/131,131,0,0/
      DATA ( NVALUE(1817,I),I=1,4)/131,131,10,10/
      DATA ( NVALUE(1818,I),I=1,4)/131,131,11,11/
      DATA ( NVALUE(1819,I),I=1,4)/131,131,12,12/
      DATA ( NVALUE(1820,I),I=1,4)/131,131,13,13/
      N=NVALUE(NSUB,NPRTCL)
      IF (N.GT.146) CALL INIT
      IF (N.EQ.0)  THEN 
         VAL=0
      ELSE
         VAL=A(N)
      IF(VAL.LT.0) VAL=-VAL
      ENDIF
      RETURN
      END

      SUBROUTINE INIT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/LOGG/L(1820)
      LOGICAL RECALC 
      DIMENSION  AMEM(146)
      COMMON/VARS/A(1800)
      SAVE
      DATA RECALC/.TRUE./,P1MEM/0.D0/
      DO 1 I=1,146
         IF (A(I).NE.AMEM(I)) THEN
            RECALC=.TRUE.
            AMEM(I)=A(I)
         ENDIF
1     CONTINUE
      IF (RECALC) THEN
         DO 2 I=1,1820
2        L(I)=0
         A(147)=DSQRT(DFLOAT(2))
         A(148)=DSQRT(1-(A(2))**(2))
         A(149)=2*A(2)*A(148)
         A(150)=(A(148))**(2)-(A(2))**(2)
         A(151)=A(6)*A(148)
         A(152)=DSQRT(1-(A(3))**(2))
         A(153)=DSQRT(1-(A(4))**(2))
         A(154)=DSQRT(1-(A(5))**(2))
         A(155)=A(152)*A(154)
         A(156)=A(3)*A(154)
         A(157)=A(5)
         A(158)=-A(152)*A(4)*A(5)-A(3)*A(153)
         A(159)=A(152)*A(153)-A(3)*A(4)*A(5)
         A(160)=A(4)*A(154)
         A(161)=A(3)*A(4)-A(152)*A(153)*A(5)
         A(162)=-A(3)*A(153)*A(5)-A(152)*A(4)
         A(163)=A(153)*A(154)
         A(164)=A(130)/(DSQRT(1+(A(130))**(2)))
         A(165)=DSQRT(1-(A(164))**(2))
         A(166)=DSQRT(1-(A(17))**(2))
         A(167)=A(17)*A(165)+A(166)*A(164)
         A(168)=A(17)*A(165)-A(166)*A(164)
         A(169)=A(166)*A(165)-A(17)*A(164)
         A(170)=A(166)*A(165)+A(17)*A(164)
         A(171)=(A(165))**(2)-(A(164))**(2)
         A(172)=2*A(164)*A(165)
         A(173)=2*A(17)*A(166)
         A(174)=(A(166))**(2)-(A(17))**(2)
         A(175)=4*(A(2))**(2)-3
         A(176)=A(175)
         A(177)=(A(164))**(2)*A(13)+(A(165))**(2)*A(10)
         A(178)=A(177)
         A(179)=(A(165))**(2)*A(10)-(A(164))**(2)*A(13)
         A(180)=A(179)
         A(181)=(A(164))**(2)*A(13)+(A(165))**(2)*A(12)
         A(182)=A(181)
         A(183)=(A(165))**(2)*A(12)-(A(164))**(2)*A(13)
         A(184)=A(183)
         A(185)=A(165)*A(10)*A(147)*A(21)*A(68)+A(164)*A(13)*A(147)*A(2
     .5)*A(66)-A(172)*A(151)*A(19)*A(66)
         A(186)=A(185)
         A(187)=A(165)*A(10)*A(147)*A(21)*A(68)-A(164)*A(13)*A(147)*A(2
     .5)*A(66)-A(172)*A(151)*A(19)*A(66)
         A(188)=A(187)
         A(189)=A(165)*A(10)*A(147)*A(21)*A(69)+A(164)*A(13)*A(147)*A(2
     .5)*A(67)-A(172)*A(151)*A(19)*A(67)
         A(190)=A(189)
         A(191)=A(165)*A(10)*A(147)*A(21)*A(69)-A(164)*A(13)*A(147)*A(2
     .5)*A(67)-A(172)*A(151)*A(19)*A(67)
         A(192)=A(191)
         A(193)=A(165)*A(12)*A(147)*A(21)*A(60)+A(164)*A(13)*A(147)*A(2
     .5)*A(58)-A(172)*A(151)*A(19)*A(58)
         A(194)=A(193)
         A(195)=A(165)*A(12)*A(147)*A(21)*A(60)-A(164)*A(13)*A(147)*A(2
     .5)*A(58)-A(172)*A(151)*A(19)*A(58)
         A(196)=A(195)
         A(197)=A(165)*A(12)*A(147)*A(21)*A(61)+A(164)*A(13)*A(147)*A(2
     .5)*A(59)-A(172)*A(151)*A(19)*A(59)
         A(198)=A(197)
         A(199)=A(165)*A(12)*A(147)*A(21)*A(61)-A(164)*A(13)*A(147)*A(2
     .5)*A(59)-A(172)*A(151)*A(19)*A(59)
         A(200)=A(199)
         A(201)=A(13)*A(147)*A(25)-2*A(165)*A(151)*A(19)
         A(202)=A(201)
         A(203)=-A(13)*A(147)*A(25)-2*A(165)*A(151)*A(19)
         A(204)=A(203)
         A(205)=A(165)*A(10)*A(147)*A(22)*A(68)+A(164)*A(13)*A(147)*A(2
     .6)*A(66)-A(172)*A(151)*A(20)*A(66)
         A(206)=A(205)
         A(207)=A(165)*A(10)*A(147)*A(22)*A(68)-A(164)*A(13)*A(147)*A(2
     .6)*A(66)-A(172)*A(151)*A(20)*A(66)
         A(208)=A(207)
         A(209)=A(165)*A(12)*A(147)*A(22)*A(60)+A(164)*A(13)*A(147)*A(2
     .6)*A(58)-A(172)*A(151)*A(20)*A(58)
         A(210)=A(209)
         A(211)=A(165)*A(12)*A(147)*A(22)*A(60)-A(164)*A(13)*A(147)*A(2
     .6)*A(58)-A(172)*A(151)*A(20)*A(58)
         A(212)=A(211)
         A(213)=A(13)*A(147)*A(26)-2*A(165)*A(151)*A(20)
         A(214)=A(213)
         A(215)=-A(13)*A(147)*A(26)-2*A(165)*A(151)*A(20)
         A(216)=A(215)
         A(217)=3*A(148)*A(13)*A(70)*A(35)-3*A(148)*A(13)*A(72)*A(35)-A
     .(2)*A(165)*A(151)*A(70)*A(27)
         A(218)=3*A(148)*A(165)*A(151)*A(70)*A(31)+2*A(2)*A(165)*A(151)
     .*A(72)*A(27)
         A(219)=A(217)+A(218)
         A(220)=3*A(148)*A(165)*A(151)*A(70)*A(31)-2*A(2)*A(165)*A(151)
     .*A(72)*A(27)-A(2)*A(165)*A(151)*A(70)*A(27)
         A(221)=-3*A(148)*A(13)*A(70)*A(35)-3*A(148)*A(13)*A(72)*A(35)
         A(222)=A(220)+A(221)
         A(223)=3*A(148)*A(13)*A(71)*A(35)-3*A(148)*A(13)*A(73)*A(35)-A
     .(2)*A(165)*A(151)*A(71)*A(27)
         A(224)=3*A(148)*A(165)*A(151)*A(71)*A(31)+2*A(2)*A(165)*A(151)
     .*A(73)*A(27)
         A(225)=A(223)+A(224)
         A(226)=3*A(148)*A(165)*A(151)*A(71)*A(31)-2*A(2)*A(165)*A(151)
     .*A(73)*A(27)-A(2)*A(165)*A(151)*A(71)*A(27)
         A(227)=-3*A(148)*A(13)*A(71)*A(35)-3*A(148)*A(13)*A(73)*A(35)
         A(228)=A(226)+A(227)
         A(229)=3*A(148)*A(13)*A(70)*A(36)-3*A(148)*A(13)*A(72)*A(36)-A
     .(2)*A(165)*A(151)*A(70)*A(28)
         A(230)=3*A(148)*A(165)*A(151)*A(70)*A(32)+2*A(2)*A(165)*A(151)
     .*A(72)*A(28)
         A(231)=A(229)+A(230)
         A(232)=3*A(148)*A(165)*A(151)*A(70)*A(32)-2*A(2)*A(165)*A(151)
     .*A(72)*A(28)-A(2)*A(165)*A(151)*A(70)*A(28)
         A(233)=-3*A(148)*A(13)*A(70)*A(36)-3*A(148)*A(13)*A(72)*A(36)
         A(234)=A(232)+A(233)
         A(235)=3*A(148)*A(13)*A(71)*A(36)-3*A(148)*A(13)*A(73)*A(36)-A
     .(2)*A(165)*A(151)*A(71)*A(28)
         A(236)=3*A(148)*A(165)*A(151)*A(71)*A(32)+2*A(2)*A(165)*A(151)
     .*A(73)*A(28)
         A(237)=A(235)+A(236)
         A(238)=3*A(148)*A(165)*A(151)*A(71)*A(32)-2*A(2)*A(165)*A(151)
     .*A(73)*A(28)-A(2)*A(165)*A(151)*A(71)*A(28)
         A(239)=-3*A(148)*A(13)*A(71)*A(36)-3*A(148)*A(13)*A(73)*A(36)
         A(240)=A(238)+A(239)
         A(241)=3*A(148)*A(13)*A(70)*A(37)-3*A(148)*A(13)*A(72)*A(37)-A
     .(2)*A(165)*A(151)*A(70)*A(29)
         A(242)=3*A(148)*A(165)*A(151)*A(70)*A(33)+2*A(2)*A(165)*A(151)
     .*A(72)*A(29)
         A(243)=A(241)+A(242)
         A(244)=3*A(148)*A(165)*A(151)*A(70)*A(33)-2*A(2)*A(165)*A(151)
     .*A(72)*A(29)-A(2)*A(165)*A(151)*A(70)*A(29)
         A(245)=-3*A(148)*A(13)*A(70)*A(37)-3*A(148)*A(13)*A(72)*A(37)
         A(246)=A(244)+A(245)
         A(247)=3*A(148)*A(13)*A(70)*A(38)-3*A(148)*A(13)*A(72)*A(38)-A
     .(2)*A(165)*A(151)*A(70)*A(30)
         A(248)=3*A(148)*A(165)*A(151)*A(70)*A(34)+2*A(2)*A(165)*A(151)
     .*A(72)*A(30)
         A(249)=A(247)+A(248)
         A(250)=3*A(148)*A(165)*A(151)*A(70)*A(34)-2*A(2)*A(165)*A(151)
     .*A(72)*A(30)-A(2)*A(165)*A(151)*A(70)*A(30)
         A(251)=-3*A(148)*A(13)*A(70)*A(38)-3*A(148)*A(13)*A(72)*A(38)
         A(252)=A(250)+A(251)
         A(253)=(A(164))**(2)*A(13)-(A(165))**(2)*A(10)
         A(254)=A(253)
         A(255)=4*(A(2))**(2)-A(175)
         A(256)=A(255)
         A(257)=A(175)+4*(A(2))**(2)
         A(258)=A(257)
         A(259)=(A(164))**(2)*A(11)+(A(165))**(2)*A(10)
         A(260)=A(259)
         A(261)=(A(164))**(2)*A(11)-(A(165))**(2)*A(10)
         A(262)=A(261)
         A(263)=A(165)*A(10)*A(147)*A(70)*A(21)+A(164)*A(13)*A(147)*A(7
     .2)*A(25)+A(172)*A(151)*A(70)*A(23)
         A(264)=A(263)
         A(265)=A(172)*A(151)*A(70)*A(23)+A(164)*A(13)*A(147)*A(72)*A(2
     .5)-A(165)*A(10)*A(147)*A(70)*A(21)
         A(266)=A(265)
         A(267)=A(165)*A(10)*A(147)*A(71)*A(21)+A(164)*A(13)*A(147)*A(7
     .3)*A(25)+A(172)*A(151)*A(71)*A(23)
         A(268)=A(267)
         A(269)=A(172)*A(151)*A(71)*A(23)+A(164)*A(13)*A(147)*A(73)*A(2
     .5)-A(165)*A(10)*A(147)*A(71)*A(21)
         A(270)=A(269)
         A(271)=A(10)*A(147)*A(21)+2*A(164)*A(151)*A(23)
         A(272)=A(271)
         A(273)=2*A(164)*A(151)*A(23)-A(10)*A(147)*A(21)
         A(274)=A(273)
         A(275)=A(165)*A(10)*A(147)*A(78)*A(21)+A(164)*A(11)*A(147)*A(8
     .0)*A(25)+A(172)*A(151)*A(78)*A(23)
         A(276)=A(275)
         A(277)=A(172)*A(151)*A(78)*A(23)+A(164)*A(11)*A(147)*A(80)*A(2
     .5)-A(165)*A(10)*A(147)*A(78)*A(21)
         A(278)=A(277)
         A(279)=A(165)*A(10)*A(147)*A(79)*A(21)+A(164)*A(11)*A(147)*A(8
     .1)*A(25)+A(172)*A(151)*A(79)*A(23)
         A(280)=A(279)
         A(281)=A(172)*A(151)*A(79)*A(23)+A(164)*A(11)*A(147)*A(81)*A(2
     .5)-A(165)*A(10)*A(147)*A(79)*A(21)
         A(282)=A(281)
         A(283)=A(165)*A(10)*A(147)*A(70)*A(22)+A(164)*A(13)*A(147)*A(7
     .2)*A(26)+A(172)*A(151)*A(70)*A(24)
         A(284)=A(283)
         A(285)=A(172)*A(151)*A(70)*A(24)+A(164)*A(13)*A(147)*A(72)*A(2
     .6)-A(165)*A(10)*A(147)*A(70)*A(22)
         A(286)=A(285)
         A(287)=A(10)*A(147)*A(22)+2*A(164)*A(151)*A(24)
         A(288)=A(287)
         A(289)=2*A(164)*A(151)*A(24)-A(10)*A(147)*A(22)
         A(290)=A(289)
         A(291)=A(165)*A(10)*A(147)*A(78)*A(22)+A(164)*A(11)*A(147)*A(8
     .0)*A(26)+A(172)*A(151)*A(78)*A(24)
         A(292)=A(291)
         A(293)=A(172)*A(151)*A(78)*A(24)+A(164)*A(11)*A(147)*A(80)*A(2
     .6)-A(165)*A(10)*A(147)*A(78)*A(22)
         A(294)=A(293)
         A(295)=3*A(148)*A(10)*A(39)*A(66)-3*A(148)*A(10)*A(39)*A(68)-4
     .*A(2)*A(164)*A(151)*A(27)*A(68)
         A(296)=-3*A(148)*A(164)*A(151)*A(31)*A(66)-A(2)*A(164)*A(151)*
     .A(27)*A(66)
         A(297)=A(295)+A(296)
         A(298)=4*A(2)*A(164)*A(151)*A(27)*A(68)-A(2)*A(164)*A(151)*A(2
     .7)*A(66)-3*A(148)*A(164)*A(151)*A(31)*A(66)
         A(299)=-3*A(148)*A(10)*A(39)*A(66)-3*A(148)*A(10)*A(39)*A(68)
         A(300)=A(298)+A(299)
         A(301)=3*A(148)*A(10)*A(39)*A(67)-3*A(148)*A(10)*A(39)*A(69)-4
     .*A(2)*A(164)*A(151)*A(27)*A(69)
         A(302)=-3*A(148)*A(164)*A(151)*A(31)*A(67)-A(2)*A(164)*A(151)*
     .A(27)*A(67)
         A(303)=A(301)+A(302)
         A(304)=4*A(2)*A(164)*A(151)*A(27)*A(69)-A(2)*A(164)*A(151)*A(2
     .7)*A(67)-3*A(148)*A(164)*A(151)*A(31)*A(67)
         A(305)=-3*A(148)*A(10)*A(39)*A(67)-3*A(148)*A(10)*A(39)*A(69)
         A(306)=A(304)+A(305)
         A(307)=3*A(148)*A(10)*A(40)*A(66)-3*A(148)*A(10)*A(40)*A(68)-4
     .*A(2)*A(164)*A(151)*A(28)*A(68)
         A(308)=-3*A(148)*A(164)*A(151)*A(32)*A(66)-A(2)*A(164)*A(151)*
     .A(28)*A(66)
         A(309)=A(307)+A(308)
         A(310)=4*A(2)*A(164)*A(151)*A(28)*A(68)-A(2)*A(164)*A(151)*A(2
     .8)*A(66)-3*A(148)*A(164)*A(151)*A(32)*A(66)
         A(311)=-3*A(148)*A(10)*A(40)*A(66)-3*A(148)*A(10)*A(40)*A(68)
         A(312)=A(310)+A(311)
         A(313)=3*A(148)*A(10)*A(40)*A(67)-3*A(148)*A(10)*A(40)*A(69)-4
     .*A(2)*A(164)*A(151)*A(28)*A(69)
         A(314)=-3*A(148)*A(164)*A(151)*A(32)*A(67)-A(2)*A(164)*A(151)*
     .A(28)*A(67)
         A(315)=A(313)+A(314)
         A(316)=4*A(2)*A(164)*A(151)*A(28)*A(69)-A(2)*A(164)*A(151)*A(2
     .8)*A(67)-3*A(148)*A(164)*A(151)*A(32)*A(67)
         A(317)=-3*A(148)*A(10)*A(40)*A(67)-3*A(148)*A(10)*A(40)*A(69)
         A(318)=A(316)+A(317)
         A(319)=3*A(148)*A(10)*A(41)*A(66)-3*A(148)*A(10)*A(41)*A(68)-4
     .*A(2)*A(164)*A(151)*A(29)*A(68)
         A(320)=-3*A(148)*A(164)*A(151)*A(33)*A(66)-A(2)*A(164)*A(151)*
     .A(29)*A(66)
         A(321)=A(319)+A(320)
         A(322)=4*A(2)*A(164)*A(151)*A(29)*A(68)-A(2)*A(164)*A(151)*A(2
     .9)*A(66)-3*A(148)*A(164)*A(151)*A(33)*A(66)
         A(323)=-3*A(148)*A(10)*A(41)*A(66)-3*A(148)*A(10)*A(41)*A(68)
         A(324)=A(322)+A(323)
         A(325)=3*A(148)*A(10)*A(42)*A(66)-3*A(148)*A(10)*A(42)*A(68)-4
     .*A(2)*A(164)*A(151)*A(30)*A(68)
         A(326)=-3*A(148)*A(164)*A(151)*A(34)*A(66)-A(2)*A(164)*A(151)*
     .A(30)*A(66)
         A(327)=A(325)+A(326)
         A(328)=4*A(2)*A(164)*A(151)*A(30)*A(68)-A(2)*A(164)*A(151)*A(3
     .0)*A(66)-3*A(148)*A(164)*A(151)*A(34)*A(66)
         A(329)=-3*A(148)*A(10)*A(42)*A(66)-3*A(148)*A(10)*A(42)*A(68)
         A(330)=A(328)+A(329)
         A(331)=A(10)*A(147)*A(21)*A(68)-2*A(164)*A(151)*A(19)*A(66)
         A(332)=A(331)
         A(333)=A(10)*A(147)*A(21)*A(68)-2*A(164)*A(151)*A(19)*A(66)
         A(334)=A(333)
         A(335)=A(10)*A(147)*A(21)*A(69)-2*A(164)*A(151)*A(19)*A(67)
         A(336)=A(335)
         A(337)=A(10)*A(147)*A(21)*A(69)-2*A(164)*A(151)*A(19)*A(67)
         A(338)=A(337)
         A(339)=A(12)*A(147)*A(21)*A(60)-2*A(164)*A(151)*A(19)*A(58)
         A(340)=A(339)
         A(341)=A(12)*A(147)*A(21)*A(60)-2*A(164)*A(151)*A(19)*A(58)
         A(342)=A(341)
         A(343)=A(12)*A(147)*A(21)*A(61)-2*A(164)*A(151)*A(19)*A(59)
         A(344)=A(343)
         A(345)=A(12)*A(147)*A(21)*A(61)-2*A(164)*A(151)*A(19)*A(59)
         A(346)=A(345)
         A(347)=A(10)*A(147)*A(22)*A(68)-2*A(164)*A(151)*A(20)*A(66)
         A(348)=A(347)
         A(349)=A(10)*A(147)*A(22)*A(68)-2*A(164)*A(151)*A(20)*A(66)
         A(350)=A(349)
         A(351)=A(12)*A(147)*A(22)*A(60)-2*A(164)*A(151)*A(20)*A(58)
         A(352)=A(351)
         A(353)=A(12)*A(147)*A(22)*A(60)-2*A(164)*A(151)*A(20)*A(58)
         A(354)=A(353)
         A(355)=A(2)*A(74)*A(27)-3*A(148)*A(74)*A(31)-2*A(2)*A(76)*A(27
     .)
         A(356)=A(355)
         A(357)=2*A(2)*A(76)*A(27)-3*A(148)*A(74)*A(31)+A(2)*A(74)*A(27
     .)
         A(358)=A(357)
         A(359)=A(2)*A(75)*A(27)-3*A(148)*A(75)*A(31)-2*A(2)*A(77)*A(27
     .)
         A(360)=A(359)
         A(361)=2*A(2)*A(77)*A(27)-3*A(148)*A(75)*A(31)+A(2)*A(75)*A(27
     .)
         A(362)=A(361)
         A(363)=A(2)*A(74)*A(28)-3*A(148)*A(74)*A(32)-2*A(2)*A(76)*A(28
     .)
         A(364)=A(363)
         A(365)=2*A(2)*A(76)*A(28)-3*A(148)*A(74)*A(32)+A(2)*A(74)*A(28
     .)
         A(366)=A(365)
         A(367)=A(2)*A(75)*A(28)-3*A(148)*A(75)*A(32)-2*A(2)*A(77)*A(28
     .)
         A(368)=A(367)
         A(369)=2*A(2)*A(77)*A(28)-3*A(148)*A(75)*A(32)+A(2)*A(75)*A(28
     .)
         A(370)=A(369)
         A(371)=A(2)*A(74)*A(29)-3*A(148)*A(74)*A(33)-2*A(2)*A(76)*A(29
     .)
         A(372)=A(371)
         A(373)=2*A(2)*A(76)*A(29)-3*A(148)*A(74)*A(33)+A(2)*A(74)*A(29
     .)
         A(374)=A(373)
         A(375)=A(2)*A(74)*A(30)-3*A(148)*A(74)*A(34)-2*A(2)*A(76)*A(30
     .)
         A(376)=A(375)
         A(377)=2*A(2)*A(76)*A(30)-3*A(148)*A(74)*A(34)+A(2)*A(74)*A(30
     .)
         A(378)=A(377)
         A(379)=2*(A(2))**(2)+A(150)
         A(380)=A(379)
         A(381)=2*(A(2))**(2)-A(150)
         A(382)=A(381)
         A(383)=2*A(2)*A(50)*A(27)+A(2)*A(48)*A(27)+A(148)*A(48)*A(31)
         A(384)=A(383)
         A(385)=A(148)*A(48)*A(31)+A(2)*A(48)*A(27)-2*A(2)*A(50)*A(27)
         A(386)=A(385)
         A(387)=2*A(2)*A(51)*A(27)+A(2)*A(49)*A(27)+A(148)*A(49)*A(31)
         A(388)=A(387)
         A(389)=A(148)*A(49)*A(31)+A(2)*A(49)*A(27)-2*A(2)*A(51)*A(27)
         A(390)=A(389)
         A(391)=2*A(2)*A(50)*A(28)+A(2)*A(48)*A(28)+A(148)*A(48)*A(32)
         A(392)=A(391)
         A(393)=A(148)*A(48)*A(32)+A(2)*A(48)*A(28)-2*A(2)*A(50)*A(28)
         A(394)=A(393)
         A(395)=2*A(2)*A(51)*A(28)+A(2)*A(49)*A(28)+A(148)*A(49)*A(32)
         A(396)=A(395)
         A(397)=A(148)*A(49)*A(32)+A(2)*A(49)*A(28)-2*A(2)*A(51)*A(28)
         A(398)=A(397)
         A(399)=2*A(2)*A(50)*A(29)+A(2)*A(48)*A(29)+A(148)*A(48)*A(33)
         A(400)=A(399)
         A(401)=A(148)*A(48)*A(33)+A(2)*A(48)*A(29)-2*A(2)*A(50)*A(29)
         A(402)=A(401)
         A(403)=2*A(2)*A(50)*A(30)+A(2)*A(48)*A(30)+A(148)*A(48)*A(34)
         A(404)=A(403)
         A(405)=A(148)*A(48)*A(34)+A(2)*A(48)*A(30)-2*A(2)*A(50)*A(30)
         A(406)=A(405)
         A(407)=A(8)*A(147)*A(25)-2*A(165)*A(151)*A(19)
         A(408)=A(407)
         A(409)=-A(8)*A(147)*A(25)-2*A(165)*A(151)*A(19)
         A(410)=A(409)
         A(411)=A(8)*A(147)*A(26)-2*A(165)*A(151)*A(20)
         A(412)=A(411)
         A(413)=-A(8)*A(147)*A(26)-2*A(165)*A(151)*A(20)
         A(414)=A(413)
         A(415)=A(148)*A(8)*A(52)*A(35)-A(148)*A(8)*A(54)*A(35)+2*A(2)*
     .A(165)*A(151)*A(54)*A(27)
         A(416)=A(2)*A(165)*A(151)*A(52)*A(27)+A(148)*A(165)*A(151)*A(5
     .2)*A(31)
         A(417)=A(415)+A(416)
         A(418)=A(148)*A(165)*A(151)*A(52)*A(31)+A(2)*A(165)*A(151)*A(5
     .2)*A(27)-2*A(2)*A(165)*A(151)*A(54)*A(27)
         A(419)=-A(148)*A(8)*A(52)*A(35)-A(148)*A(8)*A(54)*A(35)
         A(420)=A(418)+A(419)
         A(421)=A(148)*A(8)*A(53)*A(35)-A(148)*A(8)*A(55)*A(35)+2*A(2)*
     .A(165)*A(151)*A(55)*A(27)
         A(422)=A(2)*A(165)*A(151)*A(53)*A(27)+A(148)*A(165)*A(151)*A(5
     .3)*A(31)
         A(423)=A(421)+A(422)
         A(424)=A(148)*A(165)*A(151)*A(53)*A(31)+A(2)*A(165)*A(151)*A(5
     .3)*A(27)-2*A(2)*A(165)*A(151)*A(55)*A(27)
         A(425)=-A(148)*A(8)*A(53)*A(35)-A(148)*A(8)*A(55)*A(35)
         A(426)=A(424)+A(425)
         A(427)=A(148)*A(8)*A(52)*A(36)-A(148)*A(8)*A(54)*A(36)+2*A(2)*
     .A(165)*A(151)*A(54)*A(28)
         A(428)=A(2)*A(165)*A(151)*A(52)*A(28)+A(148)*A(165)*A(151)*A(5
     .2)*A(32)
         A(429)=A(427)+A(428)
         A(430)=A(148)*A(165)*A(151)*A(52)*A(32)+A(2)*A(165)*A(151)*A(5
     .2)*A(28)-2*A(2)*A(165)*A(151)*A(54)*A(28)
         A(431)=-A(148)*A(8)*A(52)*A(36)-A(148)*A(8)*A(54)*A(36)
         A(432)=A(430)+A(431)
         A(433)=A(148)*A(8)*A(53)*A(36)-A(148)*A(8)*A(55)*A(36)+2*A(2)*
     .A(165)*A(151)*A(55)*A(28)
         A(434)=A(2)*A(165)*A(151)*A(53)*A(28)+A(148)*A(165)*A(151)*A(5
     .3)*A(32)
         A(435)=A(433)+A(434)
         A(436)=A(148)*A(165)*A(151)*A(53)*A(32)+A(2)*A(165)*A(151)*A(5
     .3)*A(28)-2*A(2)*A(165)*A(151)*A(55)*A(28)
         A(437)=-A(148)*A(8)*A(53)*A(36)-A(148)*A(8)*A(55)*A(36)
         A(438)=A(436)+A(437)
         A(439)=A(148)*A(8)*A(52)*A(37)-A(148)*A(8)*A(54)*A(37)+2*A(2)*
     .A(165)*A(151)*A(54)*A(29)
         A(440)=A(2)*A(165)*A(151)*A(52)*A(29)+A(148)*A(165)*A(151)*A(5
     .2)*A(33)
         A(441)=A(439)+A(440)
         A(442)=A(148)*A(165)*A(151)*A(52)*A(33)+A(2)*A(165)*A(151)*A(5
     .2)*A(29)-2*A(2)*A(165)*A(151)*A(54)*A(29)
         A(443)=-A(148)*A(8)*A(52)*A(37)-A(148)*A(8)*A(54)*A(37)
         A(444)=A(442)+A(443)
         A(445)=A(148)*A(8)*A(52)*A(38)-A(148)*A(8)*A(54)*A(38)+2*A(2)*
     .A(165)*A(151)*A(54)*A(30)
         A(446)=A(2)*A(165)*A(151)*A(52)*A(30)+A(148)*A(165)*A(151)*A(5
     .2)*A(34)
         A(447)=A(445)+A(446)
         A(448)=A(148)*A(165)*A(151)*A(52)*A(34)+A(2)*A(165)*A(151)*A(5
     .2)*A(30)-2*A(2)*A(165)*A(151)*A(54)*A(30)
         A(449)=-A(148)*A(8)*A(52)*A(38)-A(148)*A(8)*A(54)*A(38)
         A(450)=A(448)+A(449)
         A(451)=A(9)*A(147)*A(25)-2*A(165)*A(151)*A(19)
         A(452)=A(451)
         A(453)=-A(9)*A(147)*A(25)-2*A(165)*A(151)*A(19)
         A(454)=A(453)
         A(455)=A(9)*A(147)*A(26)-2*A(165)*A(151)*A(20)
         A(456)=A(455)
         A(457)=-A(9)*A(147)*A(26)-2*A(165)*A(151)*A(20)
         A(458)=A(457)
         A(459)=A(148)*A(9)*A(44)*A(35)-A(148)*A(9)*A(46)*A(35)+2*A(2)*
     .A(165)*A(151)*A(46)*A(27)
         A(460)=A(2)*A(165)*A(151)*A(44)*A(27)+A(148)*A(165)*A(151)*A(4
     .4)*A(31)
         A(461)=A(459)+A(460)
         A(462)=A(148)*A(165)*A(151)*A(44)*A(31)+A(2)*A(165)*A(151)*A(4
     .4)*A(27)-2*A(2)*A(165)*A(151)*A(46)*A(27)
         A(463)=-A(148)*A(9)*A(44)*A(35)-A(148)*A(9)*A(46)*A(35)
         A(464)=A(462)+A(463)
         A(465)=A(148)*A(9)*A(45)*A(35)-A(148)*A(9)*A(47)*A(35)+2*A(2)*
     .A(165)*A(151)*A(47)*A(27)
         A(466)=A(2)*A(165)*A(151)*A(45)*A(27)+A(148)*A(165)*A(151)*A(4
     .5)*A(31)
         A(467)=A(465)+A(466)
         A(468)=A(148)*A(165)*A(151)*A(45)*A(31)+A(2)*A(165)*A(151)*A(4
     .5)*A(27)-2*A(2)*A(165)*A(151)*A(47)*A(27)
         A(469)=-A(148)*A(9)*A(45)*A(35)-A(148)*A(9)*A(47)*A(35)
         A(470)=A(468)+A(469)
         A(471)=A(148)*A(9)*A(44)*A(36)-A(148)*A(9)*A(46)*A(36)+2*A(2)*
     .A(165)*A(151)*A(46)*A(28)
         A(472)=A(2)*A(165)*A(151)*A(44)*A(28)+A(148)*A(165)*A(151)*A(4
     .4)*A(32)
         A(473)=A(471)+A(472)
         A(474)=A(148)*A(165)*A(151)*A(44)*A(32)+A(2)*A(165)*A(151)*A(4
     .4)*A(28)-2*A(2)*A(165)*A(151)*A(46)*A(28)
         A(475)=-A(148)*A(9)*A(44)*A(36)-A(148)*A(9)*A(46)*A(36)
         A(476)=A(474)+A(475)
         A(477)=A(148)*A(9)*A(45)*A(36)-A(148)*A(9)*A(47)*A(36)+2*A(2)*
     .A(165)*A(151)*A(47)*A(28)
         A(478)=A(2)*A(165)*A(151)*A(45)*A(28)+A(148)*A(165)*A(151)*A(4
     .5)*A(32)
         A(479)=A(477)+A(478)
         A(480)=A(148)*A(165)*A(151)*A(45)*A(32)+A(2)*A(165)*A(151)*A(4
     .5)*A(28)-2*A(2)*A(165)*A(151)*A(47)*A(28)
         A(481)=-A(148)*A(9)*A(45)*A(36)-A(148)*A(9)*A(47)*A(36)
         A(482)=A(480)+A(481)
         A(483)=A(148)*A(9)*A(44)*A(37)-A(148)*A(9)*A(46)*A(37)+2*A(2)*
     .A(165)*A(151)*A(46)*A(29)
         A(484)=A(2)*A(165)*A(151)*A(44)*A(29)+A(148)*A(165)*A(151)*A(4
     .4)*A(33)
         A(485)=A(483)+A(484)
         A(486)=A(148)*A(165)*A(151)*A(44)*A(33)+A(2)*A(165)*A(151)*A(4
     .4)*A(29)-2*A(2)*A(165)*A(151)*A(46)*A(29)
         A(487)=-A(148)*A(9)*A(44)*A(37)-A(148)*A(9)*A(46)*A(37)
         A(488)=A(486)+A(487)
         A(489)=A(148)*A(9)*A(44)*A(38)-A(148)*A(9)*A(46)*A(38)+2*A(2)*
     .A(165)*A(151)*A(46)*A(30)
         A(490)=A(2)*A(165)*A(151)*A(44)*A(30)+A(148)*A(165)*A(151)*A(4
     .4)*A(34)
         A(491)=A(489)+A(490)
         A(492)=A(148)*A(165)*A(151)*A(44)*A(34)+A(2)*A(165)*A(151)*A(4
     .4)*A(30)-2*A(2)*A(165)*A(151)*A(46)*A(30)
         A(493)=-A(148)*A(9)*A(44)*A(38)-A(148)*A(9)*A(46)*A(38)
         A(494)=A(492)+A(493)
         A(495)=3*A(174)*A(168)+2*A(167)
         A(496)=-A(1)/((A(148))**(2))*A(151)/(A(2))/DFLOAT((2))*A(495)
         A(497)=A(150)*A(169)-A(172)*A(168)
         A(498)=-A(1)/((A(148))**(2))*A(151)/(A(2))/DFLOAT((2))*A(497)
         A(499)=3*A(173)*A(168)-A(169)
         A(500)=-A(1)/((A(148))**(2))*A(151)/(A(2))/DFLOAT((2))*A(499)
         A(501)=2*(A(2))**(2)*A(170)*A(165)*(A(151))**(2)*(A(72))**(2)+
     .3*A(170)*A(165)*(A(151))**(2)*(A(70))**(2)
         A(502)=6*(A(148))**(2)*A(17)*A(18)*A(13)*A(70)*A(72)-2*(A(2))*
     .*(2)*A(170)*A(165)*(A(151))**(2)*(A(70))**(2)
         A(503)=6*(A(148))**(2)*A(166)*A(57)*A(13)*A(70)*A(72)-6*(A(148
     .))**(2)*A(166)*(A(13))**(2)
         A(504)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((6
     .))*(A(501)+A(502)+A(503))
         A(505)=2*(A(2))**(2)*A(170)*A(165)*(A(151))**(2)*A(72)*A(73)+3
     .*A(170)*A(165)*(A(151))**(2)*A(70)*A(71)
         A(506)=3*(A(148))**(2)*A(17)*A(18)*A(13)*A(70)*A(73)-2*(A(2))*
     .*(2)*A(170)*A(165)*(A(151))**(2)*A(70)*A(71)
         A(507)=3*(A(148))**(2)*A(17)*A(18)*A(13)*A(71)*A(72)+3*(A(148)
     .)**(2)*A(166)*A(57)*A(13)*A(71)*A(72)
         A(508)=3*(A(148))**(2)*A(166)*A(57)*A(13)*A(70)*A(73)
         A(509)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((6
     .))*(A(505)+A(506)+A(507)+A(508))
         A(510)=3*(A(148))**(2)*A(17)*A(18)*A(13)*A(71)*A(72)-2*(A(2))*
     .*(2)*A(170)*A(165)*(A(151))**(2)*A(70)*A(71)
         A(511)=3*(A(148))**(2)*A(17)*A(18)*A(13)*A(70)*A(73)+3*(A(148)
     .)**(2)*A(166)*A(57)*A(13)*A(70)*A(73)
         A(512)=3*(A(148))**(2)*A(166)*A(57)*A(13)*A(71)*A(72)
         A(513)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((6
     .))*(A(505)+A(510)+A(511)+A(512))
         A(514)=A(175)*A(170)*A(164)*(A(151))**(2)*(A(66))**(2)-4*(A(2)
     .)**(2)*A(170)*A(164)*(A(151))**(2)*(A(68))**(2)
         A(515)=6*(A(148))**(2)*A(17)*(A(10))**(2)-6*(A(148))**(2)*A(16
     .6)*A(18)*A(10)*A(66)*A(68)
         A(516)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(164))/DFLOAT((6
     .))*(A(514)+A(515))
         A(517)=A(175)*A(170)*A(164)*(A(151))**(2)*A(66)*A(67)-4*(A(2))
     .**(2)*A(170)*A(164)*(A(151))**(2)*A(68)*A(69)
         A(518)=-3*(A(148))**(2)*A(166)*A(18)*A(10)*A(67)*A(68)-3*(A(14
     .8))**(2)*A(166)*A(18)*A(10)*A(66)*A(69)
         A(519)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(164))/DFLOAT((6
     .))*(A(517)+A(518))
         A(520)=-3*(A(148))**(2)*A(166)*A(18)*A(10)*A(66)*A(69)-3*(A(14
     .8))**(2)*A(166)*A(18)*A(10)*A(67)*A(68)
         A(521)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(164))/DFLOAT((6
     .))*(A(517)+A(520))
         A(522)=2*(A(2))**(2)*(A(76))**(2)+3*(A(74))**(2)
         A(523)=-2*(A(2))**(2)*(A(74))**(2)
         A(524)=A(1)/((A(148))**(2))*A(151)/(A(2))*A(170)/DFLOAT((6))*(
     .A(522)+A(523))
         A(525)=A(150)*(A(48))**(2)+2*(A(2))**(2)*(A(50))**(2)
         A(526)=A(1)/((A(148))**(2))*A(151)/(A(2))*A(170)/DFLOAT((2))*A
     .(525)
         A(527)=A(150)*A(170)*A(165)*(A(151))**(2)*(A(52))**(2)+2*(A(2)
     .)**(2)*A(170)*A(165)*(A(151))**(2)*(A(54))**(2)
         A(528)=2*(A(148))**(2)*A(17)*A(18)*A(8)*A(52)*A(54)-2*(A(148))
     .**(2)*A(166)*(A(8))**(2)
         A(529)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((2
     .))*(A(527)+A(528))
         A(530)=A(150)*A(170)*A(165)*(A(151))**(2)*A(52)*A(53)+2*(A(2))
     .**(2)*A(170)*A(165)*(A(151))**(2)*A(54)*A(55)
         A(531)=(A(148))**(2)*A(17)*A(18)*A(8)*A(52)*A(55)+(A(148))**(2
     .)*A(17)*A(18)*A(8)*A(53)*A(54)
         A(532)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((2
     .))*(A(530)+A(531))
         A(533)=A(150)*A(170)*A(165)*(A(151))**(2)*(A(44))**(2)+2*(A(2)
     .)**(2)*A(170)*A(165)*(A(151))**(2)*(A(46))**(2)
         A(534)=2*(A(148))**(2)*A(17)*A(18)*A(9)*A(44)*A(46)+2*(A(148))
     .**(2)*A(166)*A(43)*A(9)*A(44)*A(46)
         A(535)=-2*(A(148))**(2)*A(166)*(A(9))**(2)
         A(536)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((2
     .))*(A(533)+A(534)+A(535))
         A(537)=A(150)*A(170)*A(165)*(A(151))**(2)*A(44)*A(45)+2*(A(2))
     .**(2)*A(170)*A(165)*(A(151))**(2)*A(46)*A(47)
         A(538)=(A(148))**(2)*A(17)*A(18)*A(9)*A(44)*A(47)+(A(148))**(2
     .)*A(17)*A(18)*A(9)*A(45)*A(46)
         A(539)=(A(148))**(2)*A(166)*A(43)*A(9)*A(44)*A(47)+(A(148))**(
     .2)*A(166)*A(43)*A(9)*A(45)*A(46)
         A(540)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((2
     .))*(A(537)+A(538)+A(539))
         A(541)=(A(148))**(2)*A(17)*A(18)*A(8)*A(53)*A(54)+(A(148))**(2
     .)*A(17)*A(18)*A(8)*A(52)*A(55)
         A(542)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((2
     .))*(A(530)+A(541))
         A(543)=(A(148))**(2)*A(17)*A(18)*A(9)*A(45)*A(46)+(A(148))**(2
     .)*A(17)*A(18)*A(9)*A(44)*A(47)
         A(544)=(A(148))**(2)*A(166)*A(43)*A(9)*A(45)*A(46)+(A(148))**(
     .2)*A(166)*A(43)*A(9)*A(44)*A(47)
         A(545)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((2
     .))*(A(537)+A(543)+A(544))
         A(546)=2*(A(2))**(2)*A(170)*A(165)*(A(151))**(2)*(A(80))**(2)+
     .3*A(170)*A(165)*(A(151))**(2)*(A(78))**(2)
         A(547)=6*(A(148))**(2)*A(17)*A(18)*A(11)*A(78)*A(80)-2*(A(2))*
     .*(2)*A(170)*A(165)*(A(151))**(2)*(A(78))**(2)
         A(548)=-6*(A(148))**(2)*A(166)*(A(11))**(2)
         A(549)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((6
     .))*(A(546)+A(547)+A(548))
         A(550)=2*(A(2))**(2)*A(170)*A(165)*(A(151))**(2)*A(80)*A(81)+3
     .*A(170)*A(165)*(A(151))**(2)*A(78)*A(79)
         A(551)=3*(A(148))**(2)*A(17)*A(18)*A(11)*A(78)*A(81)-2*(A(2))*
     .*(2)*A(170)*A(165)*(A(151))**(2)*A(78)*A(79)
         A(552)=3*(A(148))**(2)*A(17)*A(18)*A(11)*A(79)*A(80)
         A(553)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((6
     .))*(A(550)+A(551)+A(552))
         A(554)=3*(A(148))**(2)*A(17)*A(18)*A(11)*A(79)*A(80)-2*(A(2))*
     .*(2)*A(170)*A(165)*(A(151))**(2)*A(78)*A(79)
         A(555)=3*(A(148))**(2)*A(17)*A(18)*A(11)*A(78)*A(81)
         A(556)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((6
     .))*(A(550)+A(554)+A(555))
         A(557)=A(175)*A(170)*A(164)*(A(151))**(2)*(A(58))**(2)-4*(A(2)
     .)**(2)*A(170)*A(164)*(A(151))**(2)*(A(60))**(2)
         A(558)=-6*(A(148))**(2)*A(17)*A(56)*A(12)*A(58)*A(60)-6*(A(148
     .))**(2)*A(166)*A(18)*A(12)*A(58)*A(60)
         A(559)=6*(A(148))**(2)*A(17)*(A(12))**(2)
         A(560)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(164))/DFLOAT((6
     .))*(A(557)+A(558)+A(559))
         A(561)=A(175)*A(170)*A(164)*(A(151))**(2)*A(58)*A(59)-4*(A(2))
     .**(2)*A(170)*A(164)*(A(151))**(2)*A(60)*A(61)
         A(562)=-3*(A(148))**(2)*A(166)*A(18)*A(12)*A(59)*A(60)-3*(A(14
     .8))**(2)*A(166)*A(18)*A(12)*A(58)*A(61)
         A(563)=-3*(A(148))**(2)*A(17)*A(56)*A(12)*A(58)*A(61)-3*(A(148
     .))**(2)*A(17)*A(56)*A(12)*A(59)*A(60)
         A(564)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(164))/DFLOAT((6
     .))*(A(561)+A(562)+A(563))
         A(565)=-3*(A(148))**(2)*A(166)*A(18)*A(12)*A(58)*A(61)-3*(A(14
     .8))**(2)*A(166)*A(18)*A(12)*A(59)*A(60)
         A(566)=-3*(A(148))**(2)*A(17)*A(56)*A(12)*A(59)*A(60)-3*(A(148
     .))**(2)*A(17)*A(56)*A(12)*A(58)*A(61)
         A(567)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(164))/DFLOAT((6
     .))*(A(561)+A(565)+A(566))
         A(568)=A(175)*(A(62))**(2)-4*(A(2))**(2)*(A(64))**(2)
         A(569)=A(1)/((A(148))**(2))*A(151)/(A(2))*A(170)/DFLOAT((6))*A
     .(568)
         A(570)=2*(A(148))**(2)*A(167)-A(171)*A(168)
         A(571)=-A(1)/((A(148))**(2))*A(151)/(A(2))/DFLOAT((2))*A(570)
         A(572)=4*(A(165))**(2)*(A(164))**(2)*(A(151))**(2)*A(160)*A(70
     .)*A(66)+A(172)*A(18)*A(13)*A(160)*A(72)*A(66)
         A(573)=-2*A(171)*A(13)*A(10)*A(160)*A(72)*A(68)-A(172)*A(18)*A
     .(10)*A(160)*A(70)*A(68)
         A(574)=-2*(A(164))**(2)*A(57)*A(13)*A(160)*A(72)*A(66)-2*(A(16
     .5))**(2)*(A(10))**(2)*A(160)*A(70)*A(66)
         A(575)=2*(A(164))**(2)*(A(13))**(2)*A(70)*A(66)
         A(576)=-A(1)/(A(151))/(A(2))*A(147)/(A(172))/DFLOAT((2))*(A(57
     .2)+A(573)+A(574)+A(575))
         A(577)=4*(A(165))**(2)*(A(164))**(2)*(A(151))**(2)*A(160)*A(71
     .)*A(66)+A(172)*A(18)*A(13)*A(160)*A(73)*A(66)
         A(578)=-2*A(171)*A(13)*A(10)*A(160)*A(73)*A(68)-A(172)*A(18)*A
     .(10)*A(160)*A(71)*A(68)
         A(579)=-2*(A(164))**(2)*A(57)*A(13)*A(160)*A(73)*A(66)-2*(A(16
     .5))**(2)*(A(10))**(2)*A(160)*A(71)*A(66)
         A(580)=2*(A(164))**(2)*(A(13))**(2)*A(71)*A(66)
         A(581)=-A(1)/(A(151))/(A(2))*A(147)/(A(172))/DFLOAT((2))*(A(57
     .7)+A(578)+A(579)+A(580))
         A(582)=2*A(165)*(A(164))**(2)*(A(151))**(2)*A(66)-A(164)*A(18)
     .*A(10)*A(68)
         A(583)=-A(165)*(A(10))**(2)*A(66)
         A(584)=-A(1)/(A(151))/(A(2))*A(147)*A(158)*A(74)/(A(164))/DFLO
     .AT((2))*(A(582)+A(583))
         A(585)=4*(A(165))**(2)*(A(164))**(2)*(A(151))**(2)*A(159)*A(78
     .)*A(66)+A(172)*A(18)*A(11)*A(159)*A(80)*A(66)
         A(586)=-2*A(171)*A(10)*A(11)*A(159)*A(80)*A(68)-A(172)*A(18)*A
     .(10)*A(159)*A(78)*A(68)
         A(587)=2*(A(164))**(2)*(A(11))**(2)*A(78)*A(66)-2*(A(165))**(2
     .)*(A(10))**(2)*A(159)*A(78)*A(66)
         A(588)=-A(1)/(A(151))/(A(2))*A(147)/(A(172))/DFLOAT((2))*(A(58
     .5)+A(586)+A(587))
         A(589)=4*(A(165))**(2)*(A(164))**(2)*(A(151))**(2)*A(159)*A(79
     .)*A(66)+A(172)*A(18)*A(11)*A(159)*A(81)*A(66)
         A(590)=-2*A(171)*A(10)*A(11)*A(159)*A(81)*A(68)-A(172)*A(18)*A
     .(10)*A(159)*A(79)*A(68)
         A(591)=2*(A(164))**(2)*(A(11))**(2)*A(79)*A(66)-2*(A(165))**(2
     .)*(A(10))**(2)*A(159)*A(79)*A(66)
         A(592)=-A(1)/(A(151))/(A(2))*A(147)/(A(172))/DFLOAT((2))*(A(58
     .9)+A(590)+A(591))
         A(593)=4*(A(165))**(2)*(A(164))**(2)*(A(151))**(2)*A(160)*A(70
     .)*A(67)+A(172)*A(18)*A(13)*A(160)*A(72)*A(67)
         A(594)=-2*A(171)*A(13)*A(10)*A(160)*A(72)*A(69)-A(172)*A(18)*A
     .(10)*A(160)*A(70)*A(69)
         A(595)=-2*(A(164))**(2)*A(57)*A(13)*A(160)*A(72)*A(67)-2*(A(16
     .5))**(2)*(A(10))**(2)*A(160)*A(70)*A(67)
         A(596)=2*(A(164))**(2)*(A(13))**(2)*A(70)*A(67)
         A(597)=-A(1)/(A(151))/(A(2))*A(147)/(A(172))/DFLOAT((2))*(A(59
     .3)+A(594)+A(595)+A(596))
         A(598)=2*A(165)*(A(164))**(2)*(A(151))**(2)*A(67)-A(164)*A(18)
     .*A(10)*A(69)
         A(599)=-A(165)*(A(10))**(2)*A(67)
         A(600)=-A(1)/(A(151))/(A(2))*A(147)*A(158)*A(74)/(A(164))/DFLO
     .AT((2))*(A(598)+A(599))
         A(601)=4*(A(165))**(2)*(A(164))**(2)*(A(151))**(2)*A(159)*A(78
     .)*A(67)+A(172)*A(18)*A(11)*A(159)*A(80)*A(67)
         A(602)=-2*A(171)*A(10)*A(11)*A(159)*A(80)*A(69)-A(172)*A(18)*A
     .(10)*A(159)*A(78)*A(69)
         A(603)=2*(A(164))**(2)*(A(11))**(2)*A(78)*A(67)-2*(A(165))**(2
     .)*(A(10))**(2)*A(159)*A(78)*A(67)
         A(604)=-A(1)/(A(151))/(A(2))*A(147)/(A(172))/DFLOAT((2))*(A(60
     .1)+A(602)+A(603))
         A(605)=2*(A(165))**(2)*A(164)*(A(151))**(2)*A(52)+A(165)*A(18)
     .*A(8)*A(54)
         A(606)=A(164)*(A(8))**(2)*A(52)
         A(607)=-A(1)/(A(151))/(A(2))*A(147)/(A(165))/DFLOAT((2))*(A(60
     .5)+A(606))
         A(608)=2*(A(165))**(2)*A(164)*(A(151))**(2)*A(53)+A(165)*A(18)
     .*A(8)*A(55)
         A(609)=A(164)*(A(8))**(2)*A(53)
         A(610)=-A(1)/(A(151))/(A(2))*A(147)/(A(165))/DFLOAT((2))*(A(60
     .8)+A(609))
         A(611)=2*(A(165))**(2)*A(164)*(A(151))**(2)*A(44)+A(165)*A(18)
     .*A(9)*A(46)
         A(612)=A(164)*(A(9))**(2)*A(44)-A(164)*A(43)*A(9)*A(46)
         A(613)=-A(1)/(A(151))/(A(2))*A(147)/(A(165))/DFLOAT((2))*(A(61
     .1)+A(612))
         A(614)=2*(A(165))**(2)*A(164)*(A(151))**(2)*A(45)+A(165)*A(18)
     .*A(9)*A(47)
         A(615)=A(164)*(A(9))**(2)*A(45)-A(164)*A(43)*A(9)*A(47)
         A(616)=-A(1)/(A(151))/(A(2))*A(147)/(A(165))/DFLOAT((2))*(A(61
     .4)+A(615))
         A(617)=4*(A(165))**(2)*(A(164))**(2)*(A(151))**(2)*A(163)*A(70
     .)*A(58)+A(172)*A(18)*A(13)*A(163)*A(72)*A(58)
         A(618)=-2*A(171)*A(13)*A(12)*A(163)*A(72)*A(60)-A(172)*A(18)*A
     .(12)*A(163)*A(70)*A(60)
         A(619)=2*(A(165))**(2)*A(56)*A(12)*A(163)*A(70)*A(60)-2*(A(165
     .))**(2)*(A(12))**(2)*A(163)*A(70)*A(58)
         A(620)=2*(A(164))**(2)*(A(13))**(2)*A(70)*A(58)-2*(A(164))**(2
     .)*A(57)*A(13)*A(163)*A(72)*A(58)
         A(621)=-A(1)/(A(151))/(A(2))*A(147)/(A(172))/DFLOAT((2))*(A(61
     .7)+A(618)+A(619)+A(620))
         A(622)=4*(A(165))**(2)*(A(164))**(2)*(A(151))**(2)*A(163)*A(71
     .)*A(58)+A(172)*A(18)*A(13)*A(163)*A(73)*A(58)
         A(623)=-2*A(171)*A(13)*A(12)*A(163)*A(73)*A(60)-A(172)*A(18)*A
     .(12)*A(163)*A(71)*A(60)
         A(624)=2*(A(165))**(2)*A(56)*A(12)*A(163)*A(71)*A(60)-2*(A(165
     .))**(2)*(A(12))**(2)*A(163)*A(71)*A(58)
         A(625)=2*(A(164))**(2)*(A(13))**(2)*A(71)*A(58)-2*(A(164))**(2
     .)*A(57)*A(13)*A(163)*A(73)*A(58)
         A(626)=-A(1)/(A(151))/(A(2))*A(147)/(A(172))/DFLOAT((2))*(A(62
     .2)+A(623)+A(624)+A(625))
         A(627)=2*A(165)*(A(164))**(2)*(A(151))**(2)*A(58)-A(164)*A(18)
     .*A(12)*A(60)
         A(628)=A(165)*A(56)*A(12)*A(60)-A(165)*(A(12))**(2)*A(58)
         A(629)=-A(1)/(A(151))/(A(2))*A(147)*A(161)*A(74)/(A(164))/DFLO
     .AT((2))*(A(627)+A(628))
         A(630)=4*(A(165))**(2)*(A(164))**(2)*(A(151))**(2)*A(162)*A(78
     .)*A(58)+A(172)*A(18)*A(11)*A(162)*A(80)*A(58)
         A(631)=-2*A(171)*A(11)*A(12)*A(162)*A(80)*A(60)-A(172)*A(18)*A
     .(12)*A(162)*A(78)*A(60)
         A(632)=2*(A(165))**(2)*A(56)*A(12)*A(162)*A(78)*A(60)-2*(A(165
     .))**(2)*(A(12))**(2)*A(162)*A(78)*A(58)
         A(633)=2*(A(164))**(2)*(A(11))**(2)*A(78)*A(58)
         A(634)=-A(1)/(A(151))/(A(2))*A(147)/(A(172))/DFLOAT((2))*(A(63
     .0)+A(631)+A(632)+A(633))
         A(635)=4*(A(165))**(2)*(A(164))**(2)*(A(151))**(2)*A(162)*A(79
     .)*A(58)+A(172)*A(18)*A(11)*A(162)*A(81)*A(58)
         A(636)=-2*A(171)*A(11)*A(12)*A(162)*A(81)*A(60)-A(172)*A(18)*A
     .(12)*A(162)*A(79)*A(60)
         A(637)=2*(A(165))**(2)*A(56)*A(12)*A(162)*A(79)*A(60)-2*(A(165
     .))**(2)*(A(12))**(2)*A(162)*A(79)*A(58)
         A(638)=2*(A(164))**(2)*(A(11))**(2)*A(79)*A(58)
         A(639)=-A(1)/(A(151))/(A(2))*A(147)/(A(172))/DFLOAT((2))*(A(63
     .5)+A(636)+A(637)+A(638))
         A(640)=4*(A(165))**(2)*(A(164))**(2)*(A(151))**(2)*A(163)*A(70
     .)*A(59)+A(172)*A(18)*A(13)*A(163)*A(72)*A(59)
         A(641)=-2*A(171)*A(13)*A(12)*A(163)*A(72)*A(61)-A(172)*A(18)*A
     .(12)*A(163)*A(70)*A(61)
         A(642)=2*(A(165))**(2)*A(56)*A(12)*A(163)*A(70)*A(61)-2*(A(165
     .))**(2)*(A(12))**(2)*A(163)*A(70)*A(59)
         A(643)=2*(A(164))**(2)*(A(13))**(2)*A(70)*A(59)-2*(A(164))**(2
     .)*A(57)*A(13)*A(163)*A(72)*A(59)
         A(644)=-A(1)/(A(151))/(A(2))*A(147)/(A(172))/DFLOAT((2))*(A(64
     .0)+A(641)+A(642)+A(643))
         A(645)=2*A(165)*(A(164))**(2)*(A(151))**(2)*A(59)-A(164)*A(18)
     .*A(12)*A(61)
         A(646)=A(165)*A(56)*A(12)*A(61)-A(165)*(A(12))**(2)*A(59)
         A(647)=-A(1)/(A(151))/(A(2))*A(147)*A(161)*A(74)/(A(164))/DFLO
     .AT((2))*(A(645)+A(646))
         A(648)=4*(A(165))**(2)*(A(164))**(2)*(A(151))**(2)*A(162)*A(78
     .)*A(59)+A(172)*A(18)*A(11)*A(162)*A(80)*A(59)
         A(649)=-2*A(171)*A(11)*A(12)*A(162)*A(80)*A(61)-A(172)*A(18)*A
     .(12)*A(162)*A(78)*A(61)
         A(650)=2*(A(165))**(2)*A(56)*A(12)*A(162)*A(78)*A(61)-2*(A(165
     .))**(2)*(A(12))**(2)*A(162)*A(78)*A(59)
         A(651)=2*(A(164))**(2)*(A(11))**(2)*A(78)*A(59)
         A(652)=-A(1)/(A(151))/(A(2))*A(147)/(A(172))/DFLOAT((2))*(A(64
     .8)+A(649)+A(650)+A(651))
         A(653)=2*(A(165))**(2)*A(164)*(A(151))**(2)*A(157)*A(70)+A(165
     .)*A(18)*A(13)*A(157)*A(72)
         A(654)=A(164)*(A(13))**(2)*A(70)-A(164)*A(57)*A(13)*A(157)*A(7
     .2)
         A(655)=-A(1)/(A(151))/(A(2))*A(147)*A(62)/(A(165))/DFLOAT((2))
     .*(A(653)+A(654))
         A(656)=2*(A(165))**(2)*A(164)*(A(151))**(2)*A(157)*A(71)+A(165
     .)*A(18)*A(13)*A(157)*A(73)
         A(657)=A(164)*(A(13))**(2)*A(71)-A(164)*A(57)*A(13)*A(157)*A(7
     .3)
         A(658)=-A(1)/(A(151))/(A(2))*A(147)*A(62)/(A(165))/DFLOAT((2))
     .*(A(656)+A(657))
         A(659)=2*(A(165))**(2)*A(164)*(A(151))**(2)*A(156)*A(78)+A(165
     .)*A(18)*A(11)*A(156)*A(80)
         A(660)=A(164)*(A(11))**(2)*A(78)
         A(661)=-A(1)/(A(151))/(A(2))*A(147)*A(62)/(A(165))/DFLOAT((2))
     .*(A(659)+A(660))
         A(662)=2*(A(165))**(2)*A(164)*(A(151))**(2)*A(156)*A(79)+A(165
     .)*A(18)*A(11)*A(156)*A(81)
         A(663)=A(164)*(A(11))**(2)*A(79)
         A(664)=-A(1)/(A(151))/(A(2))*A(147)*A(62)/(A(165))/DFLOAT((2))
     .*(A(662)+A(663))
         A(665)=A(165)*A(18)*A(70)*A(73)-A(165)*A(18)*A(71)*A(72)
         A(666)=A(164)*A(57)*A(70)*A(73)-A(164)*A(57)*A(71)*A(72)
         A(667)=-A(1)/(A(151))*A(13)/(A(2))/(A(165))/DFLOAT((2))*(A(665
     .)+A(666))
         A(668)=A(165)*A(18)*A(71)*A(72)-A(165)*A(18)*A(70)*A(73)
         A(669)=A(164)*A(57)*A(71)*A(72)-A(164)*A(57)*A(70)*A(73)
         A(670)=-A(1)/(A(151))*A(13)/(A(2))/(A(165))/DFLOAT((2))*(A(668
     .)+A(669))
         A(671)=A(66)*A(69)-A(67)*A(68)
         A(672)=-A(1)*A(18)/(A(151))*A(10)/(A(2))/DFLOAT((2))*A(671)
         A(673)=A(67)*A(68)-A(66)*A(69)
         A(674)=-A(1)*A(18)/(A(151))*A(10)/(A(2))/DFLOAT((2))*A(673)
         A(675)=A(52)*A(55)-A(53)*A(54)
         A(676)=-A(1)*A(18)/(A(151))*A(8)/(A(2))/DFLOAT((2))*A(675)
         A(677)=A(165)*A(18)*A(44)*A(47)-A(165)*A(18)*A(45)*A(46)
         A(678)=A(164)*A(43)*A(44)*A(47)-A(164)*A(43)*A(45)*A(46)
         A(679)=-A(1)/(A(151))*A(9)/(A(2))/(A(165))/DFLOAT((2))*(A(677)
     .+A(678))
         A(680)=A(53)*A(54)-A(52)*A(55)
         A(681)=-A(1)*A(18)/(A(151))*A(8)/(A(2))/DFLOAT((2))*A(680)
         A(682)=A(165)*A(18)*A(45)*A(46)-A(165)*A(18)*A(44)*A(47)
         A(683)=A(164)*A(43)*A(45)*A(46)-A(164)*A(43)*A(44)*A(47)
         A(684)=-A(1)/(A(151))*A(9)/(A(2))/(A(165))/DFLOAT((2))*(A(682)
     .+A(683))
         A(685)=A(78)*A(81)-A(79)*A(80)
         A(686)=-A(1)*A(18)/(A(151))*A(11)/(A(2))/DFLOAT((2))*A(685)
         A(687)=A(79)*A(80)-A(78)*A(81)
         A(688)=-A(1)*A(18)/(A(151))*A(11)/(A(2))/DFLOAT((2))*A(687)
         A(689)=A(164)*A(18)*A(58)*A(61)-A(164)*A(18)*A(59)*A(60)
         A(690)=A(165)*A(56)*A(58)*A(61)-A(165)*A(56)*A(59)*A(60)
         A(691)=-A(1)/(A(151))*A(12)/(A(2))/(A(164))/DFLOAT((2))*(A(689
     .)+A(690))
         A(692)=A(164)*A(18)*A(59)*A(60)-A(164)*A(18)*A(58)*A(61)
         A(693)=A(165)*A(56)*A(59)*A(60)-A(165)*A(56)*A(58)*A(61)
         A(694)=-A(1)/(A(151))*A(12)/(A(2))/(A(164))/DFLOAT((2))*(A(692
     .)+A(693))
         A(695)=A(2)*A(27)-A(148)*A(31)
         A(696)=A(695)
         A(697)=A(2)*A(27)-A(148)*A(31)
         A(698)=A(697)
         A(699)=A(2)*A(28)-A(148)*A(32)
         A(700)=A(699)
         A(701)=A(2)*A(28)-A(148)*A(32)
         A(702)=A(701)
         A(703)=A(2)*A(29)-A(148)*A(33)
         A(704)=A(703)
         A(705)=A(2)*A(29)-A(148)*A(33)
         A(706)=A(705)
         A(707)=A(2)*A(30)-A(148)*A(34)
         A(708)=A(707)
         A(709)=A(2)*A(30)-A(148)*A(34)
         A(710)=A(709)
         A(711)=A(8)*A(147)*A(54)*A(25)+2*A(165)*A(151)*A(52)*A(23)
         A(712)=A(711)
         A(713)=2*A(165)*A(151)*A(52)*A(23)+A(8)*A(147)*A(54)*A(25)
         A(714)=A(713)
         A(715)=A(8)*A(147)*A(55)*A(25)+2*A(165)*A(151)*A(53)*A(23)
         A(716)=A(715)
         A(717)=2*A(165)*A(151)*A(53)*A(23)+A(8)*A(147)*A(55)*A(25)
         A(718)=A(717)
         A(719)=A(8)*A(147)*A(54)*A(26)+2*A(165)*A(151)*A(52)*A(24)
         A(720)=A(719)
         A(721)=2*A(165)*A(151)*A(52)*A(24)+A(8)*A(147)*A(54)*A(26)
         A(722)=A(721)
         A(723)=A(9)*A(147)*A(46)*A(25)+2*A(165)*A(151)*A(44)*A(23)
         A(724)=A(723)
         A(725)=2*A(165)*A(151)*A(44)*A(23)+A(9)*A(147)*A(46)*A(25)
         A(726)=A(725)
         A(727)=A(9)*A(147)*A(47)*A(25)+2*A(165)*A(151)*A(45)*A(23)
         A(728)=A(727)
         A(729)=2*A(165)*A(151)*A(45)*A(23)+A(9)*A(147)*A(47)*A(25)
         A(730)=A(729)
         A(731)=A(9)*A(147)*A(46)*A(26)+2*A(165)*A(151)*A(44)*A(24)
         A(732)=A(731)
         A(733)=2*A(165)*A(151)*A(44)*A(24)+A(9)*A(147)*A(46)*A(26)
         A(734)=A(733)
         A(735)=(A(165))**(2)*A(10)-(A(164))**(2)*A(11)
         A(736)=A(735)
         A(737)=(A(164))**(2)*A(11)+(A(165))**(2)*A(12)
         A(738)=A(737)
         A(739)=(A(165))**(2)*A(12)-(A(164))**(2)*A(11)
         A(740)=A(739)
         A(741)=A(165)*A(10)*A(147)*A(21)*A(68)+A(164)*A(11)*A(147)*A(2
     .5)*A(66)-A(172)*A(151)*A(19)*A(66)
         A(742)=A(741)
         A(743)=A(165)*A(10)*A(147)*A(21)*A(68)-A(164)*A(11)*A(147)*A(2
     .5)*A(66)-A(172)*A(151)*A(19)*A(66)
         A(744)=A(743)
         A(745)=A(165)*A(10)*A(147)*A(21)*A(69)+A(164)*A(11)*A(147)*A(2
     .5)*A(67)-A(172)*A(151)*A(19)*A(67)
         A(746)=A(745)
         A(747)=A(165)*A(10)*A(147)*A(21)*A(69)-A(164)*A(11)*A(147)*A(2
     .5)*A(67)-A(172)*A(151)*A(19)*A(67)
         A(748)=A(747)
         A(749)=A(165)*A(12)*A(147)*A(21)*A(60)+A(164)*A(11)*A(147)*A(2
     .5)*A(58)-A(172)*A(151)*A(19)*A(58)
         A(750)=A(749)
         A(751)=A(165)*A(12)*A(147)*A(21)*A(60)-A(164)*A(11)*A(147)*A(2
     .5)*A(58)-A(172)*A(151)*A(19)*A(58)
         A(752)=A(751)
         A(753)=A(165)*A(12)*A(147)*A(21)*A(61)+A(164)*A(11)*A(147)*A(2
     .5)*A(59)-A(172)*A(151)*A(19)*A(59)
         A(754)=A(753)
         A(755)=A(165)*A(12)*A(147)*A(21)*A(61)-A(164)*A(11)*A(147)*A(2
     .5)*A(59)-A(172)*A(151)*A(19)*A(59)
         A(756)=A(755)
         A(757)=A(11)*A(147)*A(25)-2*A(165)*A(151)*A(19)
         A(758)=A(757)
         A(759)=-A(11)*A(147)*A(25)-2*A(165)*A(151)*A(19)
         A(760)=A(759)
         A(761)=A(165)*A(10)*A(147)*A(22)*A(68)+A(164)*A(11)*A(147)*A(2
     .6)*A(66)-A(172)*A(151)*A(20)*A(66)
         A(762)=A(761)
         A(763)=A(165)*A(10)*A(147)*A(22)*A(68)-A(164)*A(11)*A(147)*A(2
     .6)*A(66)-A(172)*A(151)*A(20)*A(66)
         A(764)=A(763)
         A(765)=A(165)*A(12)*A(147)*A(22)*A(60)+A(164)*A(11)*A(147)*A(2
     .6)*A(58)-A(172)*A(151)*A(20)*A(58)
         A(766)=A(765)
         A(767)=A(165)*A(12)*A(147)*A(22)*A(60)-A(164)*A(11)*A(147)*A(2
     .6)*A(58)-A(172)*A(151)*A(20)*A(58)
         A(768)=A(767)
         A(769)=A(11)*A(147)*A(26)-2*A(165)*A(151)*A(20)
         A(770)=A(769)
         A(771)=-A(11)*A(147)*A(26)-2*A(165)*A(151)*A(20)
         A(772)=A(771)
         A(773)=3*A(148)*A(11)*A(78)*A(35)-3*A(148)*A(11)*A(80)*A(35)-A
     .(2)*A(165)*A(151)*A(78)*A(27)
         A(774)=3*A(148)*A(165)*A(151)*A(78)*A(31)+2*A(2)*A(165)*A(151)
     .*A(80)*A(27)
         A(775)=A(773)+A(774)
         A(776)=3*A(148)*A(165)*A(151)*A(78)*A(31)-2*A(2)*A(165)*A(151)
     .*A(80)*A(27)-A(2)*A(165)*A(151)*A(78)*A(27)
         A(777)=-3*A(148)*A(11)*A(78)*A(35)-3*A(148)*A(11)*A(80)*A(35)
         A(778)=A(776)+A(777)
         A(779)=3*A(148)*A(11)*A(79)*A(35)-3*A(148)*A(11)*A(81)*A(35)-A
     .(2)*A(165)*A(151)*A(79)*A(27)
         A(780)=3*A(148)*A(165)*A(151)*A(79)*A(31)+2*A(2)*A(165)*A(151)
     .*A(81)*A(27)
         A(781)=A(779)+A(780)
         A(782)=3*A(148)*A(165)*A(151)*A(79)*A(31)-2*A(2)*A(165)*A(151)
     .*A(81)*A(27)-A(2)*A(165)*A(151)*A(79)*A(27)
         A(783)=-3*A(148)*A(11)*A(79)*A(35)-3*A(148)*A(11)*A(81)*A(35)
         A(784)=A(782)+A(783)
         A(785)=3*A(148)*A(11)*A(78)*A(36)-3*A(148)*A(11)*A(80)*A(36)-A
     .(2)*A(165)*A(151)*A(78)*A(28)
         A(786)=3*A(148)*A(165)*A(151)*A(78)*A(32)+2*A(2)*A(165)*A(151)
     .*A(80)*A(28)
         A(787)=A(785)+A(786)
         A(788)=3*A(148)*A(165)*A(151)*A(78)*A(32)-2*A(2)*A(165)*A(151)
     .*A(80)*A(28)-A(2)*A(165)*A(151)*A(78)*A(28)
         A(789)=-3*A(148)*A(11)*A(78)*A(36)-3*A(148)*A(11)*A(80)*A(36)
         A(790)=A(788)+A(789)
         A(791)=3*A(148)*A(11)*A(79)*A(36)-3*A(148)*A(11)*A(81)*A(36)-A
     .(2)*A(165)*A(151)*A(79)*A(28)
         A(792)=3*A(148)*A(165)*A(151)*A(79)*A(32)+2*A(2)*A(165)*A(151)
     .*A(81)*A(28)
         A(793)=A(791)+A(792)
         A(794)=3*A(148)*A(165)*A(151)*A(79)*A(32)-2*A(2)*A(165)*A(151)
     .*A(81)*A(28)-A(2)*A(165)*A(151)*A(79)*A(28)
         A(795)=-3*A(148)*A(11)*A(79)*A(36)-3*A(148)*A(11)*A(81)*A(36)
         A(796)=A(794)+A(795)
         A(797)=3*A(148)*A(11)*A(78)*A(37)-3*A(148)*A(11)*A(80)*A(37)-A
     .(2)*A(165)*A(151)*A(78)*A(29)
         A(798)=3*A(148)*A(165)*A(151)*A(78)*A(33)+2*A(2)*A(165)*A(151)
     .*A(80)*A(29)
         A(799)=A(797)+A(798)
         A(800)=3*A(148)*A(165)*A(151)*A(78)*A(33)-2*A(2)*A(165)*A(151)
     .*A(80)*A(29)-A(2)*A(165)*A(151)*A(78)*A(29)
         A(801)=-3*A(148)*A(11)*A(78)*A(37)-3*A(148)*A(11)*A(80)*A(37)
         A(802)=A(800)+A(801)
         A(803)=3*A(148)*A(11)*A(78)*A(38)-3*A(148)*A(11)*A(80)*A(38)-A
     .(2)*A(165)*A(151)*A(78)*A(30)
         A(804)=3*A(148)*A(165)*A(151)*A(78)*A(34)+2*A(2)*A(165)*A(151)
     .*A(80)*A(30)
         A(805)=A(803)+A(804)
         A(806)=3*A(148)*A(165)*A(151)*A(78)*A(34)-2*A(2)*A(165)*A(151)
     .*A(80)*A(30)-A(2)*A(165)*A(151)*A(78)*A(30)
         A(807)=-3*A(148)*A(11)*A(78)*A(38)-3*A(148)*A(11)*A(80)*A(38)
         A(808)=A(806)+A(807)
         A(809)=(A(164))**(2)*A(13)-(A(165))**(2)*A(12)
         A(810)=A(809)
         A(811)=(A(164))**(2)*A(11)-(A(165))**(2)*A(12)
         A(812)=A(811)
         A(813)=A(165)*A(12)*A(147)*A(70)*A(21)+A(164)*A(13)*A(147)*A(7
     .2)*A(25)+A(172)*A(151)*A(70)*A(23)
         A(814)=A(813)
         A(815)=A(172)*A(151)*A(70)*A(23)+A(164)*A(13)*A(147)*A(72)*A(2
     .5)-A(165)*A(12)*A(147)*A(70)*A(21)
         A(816)=A(815)
         A(817)=A(165)*A(12)*A(147)*A(71)*A(21)+A(164)*A(13)*A(147)*A(7
     .3)*A(25)+A(172)*A(151)*A(71)*A(23)
         A(818)=A(817)
         A(819)=A(172)*A(151)*A(71)*A(23)+A(164)*A(13)*A(147)*A(73)*A(2
     .5)-A(165)*A(12)*A(147)*A(71)*A(21)
         A(820)=A(819)
         A(821)=A(12)*A(147)*A(21)+2*A(164)*A(151)*A(23)
         A(822)=A(821)
         A(823)=2*A(164)*A(151)*A(23)-A(12)*A(147)*A(21)
         A(824)=A(823)
         A(825)=A(165)*A(12)*A(147)*A(78)*A(21)+A(164)*A(11)*A(147)*A(8
     .0)*A(25)+A(172)*A(151)*A(78)*A(23)
         A(826)=A(825)
         A(827)=A(172)*A(151)*A(78)*A(23)+A(164)*A(11)*A(147)*A(80)*A(2
     .5)-A(165)*A(12)*A(147)*A(78)*A(21)
         A(828)=A(827)
         A(829)=A(165)*A(12)*A(147)*A(79)*A(21)+A(164)*A(11)*A(147)*A(8
     .1)*A(25)+A(172)*A(151)*A(79)*A(23)
         A(830)=A(829)
         A(831)=A(172)*A(151)*A(79)*A(23)+A(164)*A(11)*A(147)*A(81)*A(2
     .5)-A(165)*A(12)*A(147)*A(79)*A(21)
         A(832)=A(831)
         A(833)=A(165)*A(12)*A(147)*A(70)*A(22)+A(164)*A(13)*A(147)*A(7
     .2)*A(26)+A(172)*A(151)*A(70)*A(24)
         A(834)=A(833)
         A(835)=A(172)*A(151)*A(70)*A(24)+A(164)*A(13)*A(147)*A(72)*A(2
     .6)-A(165)*A(12)*A(147)*A(70)*A(22)
         A(836)=A(835)
         A(837)=A(12)*A(147)*A(22)+2*A(164)*A(151)*A(24)
         A(838)=A(837)
         A(839)=2*A(164)*A(151)*A(24)-A(12)*A(147)*A(22)
         A(840)=A(839)
         A(841)=A(165)*A(12)*A(147)*A(78)*A(22)+A(164)*A(11)*A(147)*A(8
     .0)*A(26)+A(172)*A(151)*A(78)*A(24)
         A(842)=A(841)
         A(843)=A(172)*A(151)*A(78)*A(24)+A(164)*A(11)*A(147)*A(80)*A(2
     .6)-A(165)*A(12)*A(147)*A(78)*A(22)
         A(844)=A(843)
         A(845)=3*A(148)*A(12)*A(39)*A(58)-3*A(148)*A(12)*A(39)*A(60)-4
     .*A(2)*A(164)*A(151)*A(27)*A(60)
         A(846)=-3*A(148)*A(164)*A(151)*A(31)*A(58)-A(2)*A(164)*A(151)*
     .A(27)*A(58)
         A(847)=A(845)+A(846)
         A(848)=4*A(2)*A(164)*A(151)*A(27)*A(60)-A(2)*A(164)*A(151)*A(2
     .7)*A(58)-3*A(148)*A(164)*A(151)*A(31)*A(58)
         A(849)=-3*A(148)*A(12)*A(39)*A(58)-3*A(148)*A(12)*A(39)*A(60)
         A(850)=A(848)+A(849)
         A(851)=3*A(148)*A(12)*A(39)*A(59)-3*A(148)*A(12)*A(39)*A(61)-4
     .*A(2)*A(164)*A(151)*A(27)*A(61)
         A(852)=-3*A(148)*A(164)*A(151)*A(31)*A(59)-A(2)*A(164)*A(151)*
     .A(27)*A(59)
         A(853)=A(851)+A(852)
         A(854)=4*A(2)*A(164)*A(151)*A(27)*A(61)-A(2)*A(164)*A(151)*A(2
     .7)*A(59)-3*A(148)*A(164)*A(151)*A(31)*A(59)
         A(855)=-3*A(148)*A(12)*A(39)*A(59)-3*A(148)*A(12)*A(39)*A(61)
         A(856)=A(854)+A(855)
         A(857)=3*A(148)*A(12)*A(40)*A(58)-3*A(148)*A(12)*A(40)*A(60)-4
     .*A(2)*A(164)*A(151)*A(28)*A(60)
         A(858)=-3*A(148)*A(164)*A(151)*A(32)*A(58)-A(2)*A(164)*A(151)*
     .A(28)*A(58)
         A(859)=A(857)+A(858)
         A(860)=4*A(2)*A(164)*A(151)*A(28)*A(60)-A(2)*A(164)*A(151)*A(2
     .8)*A(58)-3*A(148)*A(164)*A(151)*A(32)*A(58)
         A(861)=-3*A(148)*A(12)*A(40)*A(58)-3*A(148)*A(12)*A(40)*A(60)
         A(862)=A(860)+A(861)
         A(863)=3*A(148)*A(12)*A(40)*A(59)-3*A(148)*A(12)*A(40)*A(61)-4
     .*A(2)*A(164)*A(151)*A(28)*A(61)
         A(864)=-3*A(148)*A(164)*A(151)*A(32)*A(59)-A(2)*A(164)*A(151)*
     .A(28)*A(59)
         A(865)=A(863)+A(864)
         A(866)=4*A(2)*A(164)*A(151)*A(28)*A(61)-A(2)*A(164)*A(151)*A(2
     .8)*A(59)-3*A(148)*A(164)*A(151)*A(32)*A(59)
         A(867)=-3*A(148)*A(12)*A(40)*A(59)-3*A(148)*A(12)*A(40)*A(61)
         A(868)=A(866)+A(867)
         A(869)=3*A(148)*A(12)*A(41)*A(58)-3*A(148)*A(12)*A(41)*A(60)-4
     .*A(2)*A(164)*A(151)*A(29)*A(60)
         A(870)=-3*A(148)*A(164)*A(151)*A(33)*A(58)-A(2)*A(164)*A(151)*
     .A(29)*A(58)
         A(871)=A(869)+A(870)
         A(872)=4*A(2)*A(164)*A(151)*A(29)*A(60)-A(2)*A(164)*A(151)*A(2
     .9)*A(58)-3*A(148)*A(164)*A(151)*A(33)*A(58)
         A(873)=-3*A(148)*A(12)*A(41)*A(58)-3*A(148)*A(12)*A(41)*A(60)
         A(874)=A(872)+A(873)
         A(875)=3*A(148)*A(12)*A(42)*A(58)-3*A(148)*A(12)*A(42)*A(60)-4
     .*A(2)*A(164)*A(151)*A(30)*A(60)
         A(876)=-3*A(148)*A(164)*A(151)*A(34)*A(58)-A(2)*A(164)*A(151)*
     .A(30)*A(58)
         A(877)=A(875)+A(876)
         A(878)=4*A(2)*A(164)*A(151)*A(30)*A(60)-A(2)*A(164)*A(151)*A(3
     .0)*A(58)-3*A(148)*A(164)*A(151)*A(34)*A(58)
         A(879)=-3*A(148)*A(12)*A(42)*A(58)-3*A(148)*A(12)*A(42)*A(60)
         A(880)=A(878)+A(879)
         A(881)=A(13)*A(147)*A(72)*A(25)+2*A(165)*A(151)*A(70)*A(23)
         A(882)=A(881)
         A(883)=2*A(165)*A(151)*A(70)*A(23)+A(13)*A(147)*A(72)*A(25)
         A(884)=A(883)
         A(885)=A(13)*A(147)*A(73)*A(25)+2*A(165)*A(151)*A(71)*A(23)
         A(886)=A(885)
         A(887)=2*A(165)*A(151)*A(71)*A(23)+A(13)*A(147)*A(73)*A(25)
         A(888)=A(887)
         A(889)=A(11)*A(147)*A(80)*A(25)+2*A(165)*A(151)*A(78)*A(23)
         A(890)=A(889)
         A(891)=2*A(165)*A(151)*A(78)*A(23)+A(11)*A(147)*A(80)*A(25)
         A(892)=A(891)
         A(893)=A(11)*A(147)*A(81)*A(25)+2*A(165)*A(151)*A(79)*A(23)
         A(894)=A(893)
         A(895)=2*A(165)*A(151)*A(79)*A(23)+A(11)*A(147)*A(81)*A(25)
         A(896)=A(895)
         A(897)=A(13)*A(147)*A(72)*A(26)+2*A(165)*A(151)*A(70)*A(24)
         A(898)=A(897)
         A(899)=2*A(165)*A(151)*A(70)*A(24)+A(13)*A(147)*A(72)*A(26)
         A(900)=A(899)
         A(901)=A(11)*A(147)*A(80)*A(26)+2*A(165)*A(151)*A(78)*A(24)
         A(902)=A(901)
         A(903)=2*A(165)*A(151)*A(78)*A(24)+A(11)*A(147)*A(80)*A(26)
         A(904)=A(903)
         A(905)=4*A(2)*A(27)*A(64)+A(2)*A(27)*A(62)+3*A(148)*A(31)*A(62
     .)
         A(906)=A(905)
         A(907)=3*A(148)*A(31)*A(62)+A(2)*A(27)*A(62)-4*A(2)*A(27)*A(64
     .)
         A(908)=A(907)
         A(909)=4*A(2)*A(27)*A(65)+A(2)*A(27)*A(63)+3*A(148)*A(31)*A(63
     .)
         A(910)=A(909)
         A(911)=3*A(148)*A(31)*A(63)+A(2)*A(27)*A(63)-4*A(2)*A(27)*A(65
     .)
         A(912)=A(911)
         A(913)=4*A(2)*A(28)*A(64)+A(2)*A(28)*A(62)+3*A(148)*A(32)*A(62
     .)
         A(914)=A(913)
         A(915)=3*A(148)*A(32)*A(62)+A(2)*A(28)*A(62)-4*A(2)*A(28)*A(64
     .)
         A(916)=A(915)
         A(917)=4*A(2)*A(28)*A(65)+A(2)*A(28)*A(63)+3*A(148)*A(32)*A(63
     .)
         A(918)=A(917)
         A(919)=3*A(148)*A(32)*A(63)+A(2)*A(28)*A(63)-4*A(2)*A(28)*A(65
     .)
         A(920)=A(919)
         A(921)=4*A(2)*A(29)*A(64)+A(2)*A(29)*A(62)+3*A(148)*A(33)*A(62
     .)
         A(922)=A(921)
         A(923)=3*A(148)*A(33)*A(62)+A(2)*A(29)*A(62)-4*A(2)*A(29)*A(64
     .)
         A(924)=A(923)
         A(925)=4*A(2)*A(30)*A(64)+A(2)*A(30)*A(62)+3*A(148)*A(34)*A(62
     .)
         A(926)=A(925)
         A(927)=3*A(148)*A(34)*A(62)+A(2)*A(30)*A(62)-4*A(2)*A(30)*A(64
     .)
         A(928)=A(927)
         A(929)=3*(A(70))**(2)-2*(A(2))**(2)
         A(930)=A(929)
         A(931)=2*(A(2))**(2)-3*(A(70))**(2)
         A(932)=A(931)
         A(933)=3*(A(66))**(2)-4*(A(2))**(2)
         A(934)=A(933)
         A(935)=4*(A(2))**(2)-3*(A(66))**(2)
         A(936)=A(935)
         A(937)=3*(A(74))**(2)-2*(A(2))**(2)
         A(938)=A(937)
         A(939)=2*(A(2))**(2)-3*(A(74))**(2)
         A(940)=A(939)
         A(941)=(A(48))**(2)-2*(A(2))**(2)
         A(942)=A(941)
         A(943)=2*(A(2))**(2)-(A(48))**(2)
         A(944)=A(943)
         A(945)=(A(52))**(2)-2*(A(2))**(2)
         A(946)=A(945)
         A(947)=2*(A(2))**(2)-(A(52))**(2)
         A(948)=A(947)
         A(949)=(A(44))**(2)-2*(A(2))**(2)
         A(950)=A(949)
         A(951)=2*(A(2))**(2)-(A(44))**(2)
         A(952)=A(951)
         A(953)=3*(A(78))**(2)-2*(A(2))**(2)
         A(954)=A(953)
         A(955)=2*(A(2))**(2)-3*(A(78))**(2)
         A(956)=A(955)
         A(957)=3*(A(58))**(2)-4*(A(2))**(2)
         A(958)=A(957)
         A(959)=4*(A(2))**(2)-3*(A(58))**(2)
         A(960)=A(959)
         A(961)=3*(A(62))**(2)-4*(A(2))**(2)
         A(962)=A(961)
         A(963)=4*(A(2))**(2)-3*(A(62))**(2)
         A(964)=A(963)
         A(965)=A(172)*A(151)*A(19)*A(66)+A(164)*A(13)*A(147)*A(25)*A(6
     .6)-A(165)*A(10)*A(147)*A(21)*A(68)
         A(966)=A(965)
         A(967)=A(172)*A(151)*A(19)*A(67)+A(164)*A(13)*A(147)*A(25)*A(6
     .7)-A(165)*A(10)*A(147)*A(21)*A(69)
         A(968)=A(967)
         A(969)=A(172)*A(151)*A(19)*A(58)+A(164)*A(13)*A(147)*A(25)*A(5
     .8)-A(165)*A(12)*A(147)*A(21)*A(60)
         A(970)=A(969)
         A(971)=A(172)*A(151)*A(19)*A(59)+A(164)*A(13)*A(147)*A(25)*A(5
     .9)-A(165)*A(12)*A(147)*A(21)*A(61)
         A(972)=A(971)
         A(973)=2*A(165)*A(151)*A(19)+A(13)*A(147)*A(25)
         A(974)=A(973)
         A(975)=A(172)*A(151)*A(20)*A(66)+A(164)*A(13)*A(147)*A(26)*A(6
     .6)-A(165)*A(10)*A(147)*A(22)*A(68)
         A(976)=A(975)
         A(977)=A(172)*A(151)*A(20)*A(58)+A(164)*A(13)*A(147)*A(26)*A(5
     .8)-A(165)*A(12)*A(147)*A(22)*A(60)
         A(978)=A(977)
         A(979)=2*A(165)*A(151)*A(20)+A(13)*A(147)*A(26)
         A(980)=A(979)
         A(981)=2*A(2)*A(165)*A(151)*A(72)*A(27)-3*A(148)*A(165)*A(151)
     .*A(70)*A(31)+A(2)*A(165)*A(151)*A(70)*A(27)
         A(982)=3*A(148)*A(13)*A(72)*A(35)+3*A(148)*A(13)*A(70)*A(35)
         A(983)=A(981)+A(982)
         A(984)=2*A(2)*A(165)*A(151)*A(73)*A(27)-3*A(148)*A(165)*A(151)
     .*A(71)*A(31)+A(2)*A(165)*A(151)*A(71)*A(27)
         A(985)=3*A(148)*A(13)*A(73)*A(35)+3*A(148)*A(13)*A(71)*A(35)
         A(986)=A(984)+A(985)
         A(987)=2*A(2)*A(165)*A(151)*A(72)*A(28)-3*A(148)*A(165)*A(151)
     .*A(70)*A(32)+A(2)*A(165)*A(151)*A(70)*A(28)
         A(988)=3*A(148)*A(13)*A(72)*A(36)+3*A(148)*A(13)*A(70)*A(36)
         A(989)=A(987)+A(988)
         A(990)=2*A(2)*A(165)*A(151)*A(73)*A(28)-3*A(148)*A(165)*A(151)
     .*A(71)*A(32)+A(2)*A(165)*A(151)*A(71)*A(28)
         A(991)=3*A(148)*A(13)*A(73)*A(36)+3*A(148)*A(13)*A(71)*A(36)
         A(992)=A(990)+A(991)
         A(993)=2*A(2)*A(165)*A(151)*A(72)*A(29)-3*A(148)*A(165)*A(151)
     .*A(70)*A(33)+A(2)*A(165)*A(151)*A(70)*A(29)
         A(994)=3*A(148)*A(13)*A(72)*A(37)+3*A(148)*A(13)*A(70)*A(37)
         A(995)=A(993)+A(994)
         A(996)=2*A(2)*A(165)*A(151)*A(72)*A(30)-3*A(148)*A(165)*A(151)
     .*A(70)*A(34)+A(2)*A(165)*A(151)*A(70)*A(30)
         A(997)=3*A(148)*A(13)*A(72)*A(38)+3*A(148)*A(13)*A(70)*A(38)
         A(998)=A(996)+A(997)
         A(999)=3*A(148)*A(164)*A(151)*A(31)*A(66)+A(2)*A(164)*A(151)*A
     .(27)*A(66)-4*A(2)*A(164)*A(151)*A(27)*A(68)
         A(1000)=3*A(148)*A(10)*A(39)*A(68)+3*A(148)*A(10)*A(39)*A(66)
         A(1001)=A(999)+A(1000)
         A(1002)=3*A(148)*A(164)*A(151)*A(31)*A(67)+A(2)*A(164)*A(151)*
     .A(27)*A(67)-4*A(2)*A(164)*A(151)*A(27)*A(69)
         A(1003)=3*A(148)*A(10)*A(39)*A(69)+3*A(148)*A(10)*A(39)*A(67)
         A(1004)=A(1002)+A(1003)
         A(1005)=3*A(148)*A(164)*A(151)*A(32)*A(66)+A(2)*A(164)*A(151)*
     .A(28)*A(66)-4*A(2)*A(164)*A(151)*A(28)*A(68)
         A(1006)=3*A(148)*A(10)*A(40)*A(68)+3*A(148)*A(10)*A(40)*A(66)
         A(1007)=A(1005)+A(1006)
         A(1008)=3*A(148)*A(164)*A(151)*A(32)*A(67)+A(2)*A(164)*A(151)*
     .A(28)*A(67)-4*A(2)*A(164)*A(151)*A(28)*A(69)
         A(1009)=3*A(148)*A(10)*A(40)*A(69)+3*A(148)*A(10)*A(40)*A(67)
         A(1010)=A(1008)+A(1009)
         A(1011)=3*A(148)*A(164)*A(151)*A(33)*A(66)+A(2)*A(164)*A(151)*
     .A(29)*A(66)-4*A(2)*A(164)*A(151)*A(29)*A(68)
         A(1012)=3*A(148)*A(10)*A(41)*A(68)+3*A(148)*A(10)*A(41)*A(66)
         A(1013)=A(1011)+A(1012)
         A(1014)=3*A(148)*A(164)*A(151)*A(34)*A(66)+A(2)*A(164)*A(151)*
     .A(30)*A(66)-4*A(2)*A(164)*A(151)*A(30)*A(68)
         A(1015)=3*A(148)*A(10)*A(42)*A(68)+3*A(148)*A(10)*A(42)*A(66)
         A(1016)=A(1014)+A(1015)
         A(1017)=2*A(164)*A(151)*A(19)*A(66)-A(10)*A(147)*A(21)*A(68)
         A(1018)=A(1017)
         A(1019)=2*A(164)*A(151)*A(19)*A(67)-A(10)*A(147)*A(21)*A(69)
         A(1020)=A(1019)
         A(1021)=2*A(164)*A(151)*A(19)*A(58)-A(12)*A(147)*A(21)*A(60)
         A(1022)=A(1021)
         A(1023)=2*A(164)*A(151)*A(19)*A(59)-A(12)*A(147)*A(21)*A(61)
         A(1024)=A(1023)
         A(1025)=2*A(164)*A(151)*A(20)*A(66)-A(10)*A(147)*A(22)*A(68)
         A(1026)=A(1025)
         A(1027)=2*A(164)*A(151)*A(20)*A(58)-A(12)*A(147)*A(22)*A(60)
         A(1028)=A(1027)
         A(1029)=3*A(148)*A(74)*A(31)-2*A(2)*A(76)*A(27)-A(2)*A(74)*A(2
     .7)
         A(1030)=A(1029)
         A(1031)=3*A(148)*A(75)*A(31)-2*A(2)*A(77)*A(27)-A(2)*A(75)*A(2
     .7)
         A(1032)=A(1031)
         A(1033)=3*A(148)*A(74)*A(32)-2*A(2)*A(76)*A(28)-A(2)*A(74)*A(2
     .8)
         A(1034)=A(1033)
         A(1035)=3*A(148)*A(75)*A(32)-2*A(2)*A(77)*A(28)-A(2)*A(75)*A(2
     .8)
         A(1036)=A(1035)
         A(1037)=3*A(148)*A(74)*A(33)-2*A(2)*A(76)*A(29)-A(2)*A(74)*A(2
     .9)
         A(1038)=A(1037)
         A(1039)=3*A(148)*A(74)*A(34)-2*A(2)*A(76)*A(30)-A(2)*A(74)*A(3
     .0)
         A(1040)=A(1039)
         A(1041)=2*A(2)*A(50)*A(27)-A(2)*A(48)*A(27)-A(148)*A(48)*A(31)
         A(1042)=A(1041)
         A(1043)=2*A(2)*A(51)*A(27)-A(2)*A(49)*A(27)-A(148)*A(49)*A(31)
         A(1044)=A(1043)
         A(1045)=2*A(2)*A(50)*A(28)-A(2)*A(48)*A(28)-A(148)*A(48)*A(32)
         A(1046)=A(1045)
         A(1047)=2*A(2)*A(51)*A(28)-A(2)*A(49)*A(28)-A(148)*A(49)*A(32)
         A(1048)=A(1047)
         A(1049)=2*A(2)*A(50)*A(29)-A(2)*A(48)*A(29)-A(148)*A(48)*A(33)
         A(1050)=A(1049)
         A(1051)=2*A(2)*A(50)*A(30)-A(2)*A(48)*A(30)-A(148)*A(48)*A(34)
         A(1052)=A(1051)
         A(1053)=2*A(165)*A(151)*A(19)+A(8)*A(147)*A(25)
         A(1054)=A(1053)
         A(1055)=2*A(165)*A(151)*A(20)+A(8)*A(147)*A(26)
         A(1056)=A(1055)
         A(1057)=2*A(2)*A(165)*A(151)*A(54)*A(27)-A(2)*A(165)*A(151)*A(
     .52)*A(27)-A(148)*A(165)*A(151)*A(52)*A(31)
         A(1058)=A(148)*A(8)*A(54)*A(35)+A(148)*A(8)*A(52)*A(35)
         A(1059)=A(1057)+A(1058)
         A(1060)=2*A(2)*A(165)*A(151)*A(55)*A(27)-A(2)*A(165)*A(151)*A(
     .53)*A(27)-A(148)*A(165)*A(151)*A(53)*A(31)
         A(1061)=A(148)*A(8)*A(55)*A(35)+A(148)*A(8)*A(53)*A(35)
         A(1062)=A(1060)+A(1061)
         A(1063)=2*A(2)*A(165)*A(151)*A(54)*A(28)-A(2)*A(165)*A(151)*A(
     .52)*A(28)-A(148)*A(165)*A(151)*A(52)*A(32)
         A(1064)=A(148)*A(8)*A(54)*A(36)+A(148)*A(8)*A(52)*A(36)
         A(1065)=A(1063)+A(1064)
         A(1066)=2*A(2)*A(165)*A(151)*A(55)*A(28)-A(2)*A(165)*A(151)*A(
     .53)*A(28)-A(148)*A(165)*A(151)*A(53)*A(32)
         A(1067)=A(148)*A(8)*A(55)*A(36)+A(148)*A(8)*A(53)*A(36)
         A(1068)=A(1066)+A(1067)
         A(1069)=2*A(2)*A(165)*A(151)*A(54)*A(29)-A(2)*A(165)*A(151)*A(
     .52)*A(29)-A(148)*A(165)*A(151)*A(52)*A(33)
         A(1070)=A(148)*A(8)*A(54)*A(37)+A(148)*A(8)*A(52)*A(37)
         A(1071)=A(1069)+A(1070)
         A(1072)=2*A(2)*A(165)*A(151)*A(54)*A(30)-A(2)*A(165)*A(151)*A(
     .52)*A(30)-A(148)*A(165)*A(151)*A(52)*A(34)
         A(1073)=A(148)*A(8)*A(54)*A(38)+A(148)*A(8)*A(52)*A(38)
         A(1074)=A(1072)+A(1073)
         A(1075)=2*A(165)*A(151)*A(19)+A(9)*A(147)*A(25)
         A(1076)=A(1075)
         A(1077)=2*A(165)*A(151)*A(20)+A(9)*A(147)*A(26)
         A(1078)=A(1077)
         A(1079)=2*A(2)*A(165)*A(151)*A(46)*A(27)-A(2)*A(165)*A(151)*A(
     .44)*A(27)-A(148)*A(165)*A(151)*A(44)*A(31)
         A(1080)=A(148)*A(9)*A(46)*A(35)+A(148)*A(9)*A(44)*A(35)
         A(1081)=A(1079)+A(1080)
         A(1082)=2*A(2)*A(165)*A(151)*A(47)*A(27)-A(2)*A(165)*A(151)*A(
     .45)*A(27)-A(148)*A(165)*A(151)*A(45)*A(31)
         A(1083)=A(148)*A(9)*A(47)*A(35)+A(148)*A(9)*A(45)*A(35)
         A(1084)=A(1082)+A(1083)
         A(1085)=2*A(2)*A(165)*A(151)*A(46)*A(28)-A(2)*A(165)*A(151)*A(
     .44)*A(28)-A(148)*A(165)*A(151)*A(44)*A(32)
         A(1086)=A(148)*A(9)*A(46)*A(36)+A(148)*A(9)*A(44)*A(36)
         A(1087)=A(1085)+A(1086)
         A(1088)=2*A(2)*A(165)*A(151)*A(47)*A(28)-A(2)*A(165)*A(151)*A(
     .45)*A(28)-A(148)*A(165)*A(151)*A(45)*A(32)
         A(1089)=A(148)*A(9)*A(47)*A(36)+A(148)*A(9)*A(45)*A(36)
         A(1090)=A(1088)+A(1089)
         A(1091)=2*A(2)*A(165)*A(151)*A(46)*A(29)-A(2)*A(165)*A(151)*A(
     .44)*A(29)-A(148)*A(165)*A(151)*A(44)*A(33)
         A(1092)=A(148)*A(9)*A(46)*A(37)+A(148)*A(9)*A(44)*A(37)
         A(1093)=A(1091)+A(1092)
         A(1094)=2*A(2)*A(165)*A(151)*A(46)*A(30)-A(2)*A(165)*A(151)*A(
     .44)*A(30)-A(148)*A(165)*A(151)*A(44)*A(34)
         A(1095)=A(148)*A(9)*A(46)*A(38)+A(148)*A(9)*A(44)*A(38)
         A(1096)=A(1094)+A(1095)
         A(1097)=2*(A(2))**(2)*A(165)*A(168)*(A(151))**(2)*(A(72))**(2)
     .+3*A(165)*A(168)*(A(151))**(2)*(A(70))**(2)
         A(1098)=-6*(A(148))**(2)*A(166)*A(18)*A(13)*A(70)*A(72)-2*(A(2
     .))**(2)*A(165)*A(168)*(A(151))**(2)*(A(70))**(2)
         A(1099)=6*(A(148))**(2)*A(17)*A(57)*A(13)*A(70)*A(72)-6*(A(148
     .))**(2)*A(17)*(A(13))**(2)
         A(1100)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((
     .6))*(A(1097)+A(1098)+A(1099))
         A(1101)=2*(A(2))**(2)*A(165)*A(168)*(A(151))**(2)*A(72)*A(73)+
     .3*A(165)*A(168)*(A(151))**(2)*A(70)*A(71)
         A(1102)=-3*(A(148))**(2)*A(166)*A(18)*A(13)*A(70)*A(73)-2*(A(2
     .))**(2)*A(165)*A(168)*(A(151))**(2)*A(70)*A(71)
         A(1103)=3*(A(148))**(2)*A(17)*A(57)*A(13)*A(71)*A(72)-3*(A(148
     .))**(2)*A(166)*A(18)*A(13)*A(71)*A(72)
         A(1104)=3*(A(148))**(2)*A(17)*A(57)*A(13)*A(70)*A(73)
         A(1105)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((
     .6))*(A(1101)+A(1102)+A(1103)+A(1104))
         A(1106)=-3*(A(148))**(2)*A(166)*A(18)*A(13)*A(71)*A(72)-2*(A(2
     .))**(2)*A(165)*A(168)*(A(151))**(2)*A(70)*A(71)
         A(1107)=3*(A(148))**(2)*A(17)*A(57)*A(13)*A(70)*A(73)-3*(A(148
     .))**(2)*A(166)*A(18)*A(13)*A(70)*A(73)
         A(1108)=3*(A(148))**(2)*A(17)*A(57)*A(13)*A(71)*A(72)
         A(1109)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((
     .6))*(A(1101)+A(1106)+A(1107)+A(1108))
         A(1110)=A(175)*A(168)*A(164)*(A(151))**(2)*(A(66))**(2)-4*(A(2
     .))**(2)*A(168)*A(164)*(A(151))**(2)*(A(68))**(2)
         A(1111)=-6*(A(148))**(2)*A(166)*(A(10))**(2)-6*(A(148))**(2)*A
     .(17)*A(18)*A(10)*A(66)*A(68)
         A(1112)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(164))/DFLOAT((
     .6))*(A(1110)+A(1111))
         A(1113)=A(175)*A(168)*A(164)*(A(151))**(2)*A(66)*A(67)-4*(A(2)
     .)**(2)*A(168)*A(164)*(A(151))**(2)*A(68)*A(69)
         A(1114)=-3*(A(148))**(2)*A(17)*A(18)*A(10)*A(67)*A(68)-3*(A(14
     .8))**(2)*A(17)*A(18)*A(10)*A(66)*A(69)
         A(1115)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(164))/DFLOAT((
     .6))*(A(1113)+A(1114))
         A(1116)=-3*(A(148))**(2)*A(17)*A(18)*A(10)*A(66)*A(69)-3*(A(14
     .8))**(2)*A(17)*A(18)*A(10)*A(67)*A(68)
         A(1117)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(164))/DFLOAT((
     .6))*(A(1113)+A(1116))
         A(1118)=A(1)/((A(148))**(2))*A(151)/(A(2))*A(168)/DFLOAT((6))*
     .(A(522)+A(523))
         A(1119)=A(1)/((A(148))**(2))*A(151)/(A(2))*A(168)/DFLOAT((2))*
     .A(525)
         A(1120)=A(150)*A(165)*A(168)*(A(151))**(2)*(A(52))**(2)+2*(A(2
     .))**(2)*A(165)*A(168)*(A(151))**(2)*(A(54))**(2)
         A(1121)=-2*(A(148))**(2)*A(17)*(A(8))**(2)-2*(A(148))**(2)*A(1
     .66)*A(18)*A(8)*A(52)*A(54)
         A(1122)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((
     .2))*(A(1120)+A(1121))
         A(1123)=A(150)*A(165)*A(168)*(A(151))**(2)*A(52)*A(53)+2*(A(2)
     .)**(2)*A(165)*A(168)*(A(151))**(2)*A(54)*A(55)
         A(1124)=-(A(148))**(2)*A(166)*A(18)*A(8)*A(53)*A(54)-(A(148))*
     .*(2)*A(166)*A(18)*A(8)*A(52)*A(55)
         A(1125)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((
     .2))*(A(1123)+A(1124))
         A(1126)=A(150)*A(165)*A(168)*(A(151))**(2)*(A(44))**(2)+2*(A(2
     .))**(2)*A(165)*A(168)*(A(151))**(2)*(A(46))**(2)
         A(1127)=2*(A(148))**(2)*A(17)*A(43)*A(9)*A(44)*A(46)-2*(A(148)
     .)**(2)*A(166)*A(18)*A(9)*A(44)*A(46)
         A(1128)=-2*(A(148))**(2)*A(17)*(A(9))**(2)
         A(1129)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((
     .2))*(A(1126)+A(1127)+A(1128))
         A(1130)=A(150)*A(165)*A(168)*(A(151))**(2)*A(44)*A(45)+2*(A(2)
     .)**(2)*A(165)*A(168)*(A(151))**(2)*A(46)*A(47)
         A(1131)=-(A(148))**(2)*A(166)*A(18)*A(9)*A(45)*A(46)-(A(148))*
     .*(2)*A(166)*A(18)*A(9)*A(44)*A(47)
         A(1132)=(A(148))**(2)*A(17)*A(43)*A(9)*A(44)*A(47)+(A(148))**(
     .2)*A(17)*A(43)*A(9)*A(45)*A(46)
         A(1133)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((
     .2))*(A(1130)+A(1131)+A(1132))
         A(1134)=-(A(148))**(2)*A(166)*A(18)*A(8)*A(52)*A(55)-(A(148))*
     .*(2)*A(166)*A(18)*A(8)*A(53)*A(54)
         A(1135)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((
     .2))*(A(1123)+A(1134))
         A(1136)=-(A(148))**(2)*A(166)*A(18)*A(9)*A(44)*A(47)-(A(148))*
     .*(2)*A(166)*A(18)*A(9)*A(45)*A(46)
         A(1137)=(A(148))**(2)*A(17)*A(43)*A(9)*A(45)*A(46)+(A(148))**(
     .2)*A(17)*A(43)*A(9)*A(44)*A(47)
         A(1138)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((
     .2))*(A(1130)+A(1136)+A(1137))
         A(1139)=2*(A(2))**(2)*A(165)*A(168)*(A(151))**(2)*(A(80))**(2)
     .+3*A(165)*A(168)*(A(151))**(2)*(A(78))**(2)
         A(1140)=-6*(A(148))**(2)*A(166)*A(18)*A(11)*A(78)*A(80)-2*(A(2
     .))**(2)*A(165)*A(168)*(A(151))**(2)*(A(78))**(2)
         A(1141)=-6*(A(148))**(2)*A(17)*(A(11))**(2)
         A(1142)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((
     .6))*(A(1139)+A(1140)+A(1141))
         A(1143)=2*(A(2))**(2)*A(165)*A(168)*(A(151))**(2)*A(80)*A(81)+
     .3*A(165)*A(168)*(A(151))**(2)*A(78)*A(79)
         A(1144)=-3*(A(148))**(2)*A(166)*A(18)*A(11)*A(78)*A(81)-2*(A(2
     .))**(2)*A(165)*A(168)*(A(151))**(2)*A(78)*A(79)
         A(1145)=-3*(A(148))**(2)*A(166)*A(18)*A(11)*A(79)*A(80)
         A(1146)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((
     .6))*(A(1143)+A(1144)+A(1145))
         A(1147)=-3*(A(148))**(2)*A(166)*A(18)*A(11)*A(79)*A(80)-2*(A(2
     .))**(2)*A(165)*A(168)*(A(151))**(2)*A(78)*A(79)
         A(1148)=-3*(A(148))**(2)*A(166)*A(18)*A(11)*A(78)*A(81)
         A(1149)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(165))/DFLOAT((
     .6))*(A(1143)+A(1147)+A(1148))
         A(1150)=A(175)*A(168)*A(164)*(A(151))**(2)*(A(58))**(2)-4*(A(2
     .))**(2)*A(168)*A(164)*(A(151))**(2)*(A(60))**(2)
         A(1151)=6*(A(148))**(2)*A(166)*A(56)*A(12)*A(58)*A(60)-6*(A(14
     .8))**(2)*A(17)*A(18)*A(12)*A(58)*A(60)
         A(1152)=-6*(A(148))**(2)*A(166)*(A(12))**(2)
         A(1153)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(164))/DFLOAT((
     .6))*(A(1150)+A(1151)+A(1152))
         A(1154)=A(175)*A(168)*A(164)*(A(151))**(2)*A(58)*A(59)-4*(A(2)
     .)**(2)*A(168)*A(164)*(A(151))**(2)*A(60)*A(61)
         A(1155)=-3*(A(148))**(2)*A(17)*A(18)*A(12)*A(59)*A(60)-3*(A(14
     .8))**(2)*A(17)*A(18)*A(12)*A(58)*A(61)
         A(1156)=3*(A(148))**(2)*A(166)*A(56)*A(12)*A(59)*A(60)+3*(A(14
     .8))**(2)*A(166)*A(56)*A(12)*A(58)*A(61)
         A(1157)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(164))/DFLOAT((
     .6))*(A(1154)+A(1155)+A(1156))
         A(1158)=-3*(A(148))**(2)*A(17)*A(18)*A(12)*A(58)*A(61)-3*(A(14
     .8))**(2)*A(17)*A(18)*A(12)*A(59)*A(60)
         A(1159)=3*(A(148))**(2)*A(166)*A(56)*A(12)*A(58)*A(61)+3*(A(14
     .8))**(2)*A(166)*A(56)*A(12)*A(59)*A(60)
         A(1160)=A(1)/((A(148))**(2))/(A(151))/(A(2))/(A(164))/DFLOAT((
     .6))*(A(1154)+A(1158)+A(1159))
         A(1161)=A(1)/((A(148))**(2))*A(151)/(A(2))*A(168)/DFLOAT((6))*
     .A(568)
         A(1162)=A(148)*A(31)-A(2)*A(27)
         A(1163)=A(1162)
         A(1164)=A(148)*A(32)-A(2)*A(28)
         A(1165)=A(1164)
         A(1166)=A(148)*A(33)-A(2)*A(29)
         A(1167)=A(1166)
         A(1168)=A(148)*A(34)-A(2)*A(30)
         A(1169)=A(1168)
         A(1170)=A(172)*A(151)*A(19)*A(66)+A(164)*A(11)*A(147)*A(25)*A(
     .66)-A(165)*A(10)*A(147)*A(21)*A(68)
         A(1171)=A(1170)
         A(1172)=A(172)*A(151)*A(19)*A(67)+A(164)*A(11)*A(147)*A(25)*A(
     .67)-A(165)*A(10)*A(147)*A(21)*A(69)
         A(1173)=A(1172)
         A(1174)=A(172)*A(151)*A(19)*A(58)+A(164)*A(11)*A(147)*A(25)*A(
     .58)-A(165)*A(12)*A(147)*A(21)*A(60)
         A(1175)=A(1174)
         A(1176)=A(172)*A(151)*A(19)*A(59)+A(164)*A(11)*A(147)*A(25)*A(
     .59)-A(165)*A(12)*A(147)*A(21)*A(61)
         A(1177)=A(1176)
         A(1178)=2*A(165)*A(151)*A(19)+A(11)*A(147)*A(25)
         A(1179)=A(1178)
         A(1180)=A(172)*A(151)*A(20)*A(66)+A(164)*A(11)*A(147)*A(26)*A(
     .66)-A(165)*A(10)*A(147)*A(22)*A(68)
         A(1181)=A(1180)
         A(1182)=A(172)*A(151)*A(20)*A(58)+A(164)*A(11)*A(147)*A(26)*A(
     .58)-A(165)*A(12)*A(147)*A(22)*A(60)
         A(1183)=A(1182)
         A(1184)=2*A(165)*A(151)*A(20)+A(11)*A(147)*A(26)
         A(1185)=A(1184)
         A(1186)=2*A(2)*A(165)*A(151)*A(80)*A(27)-3*A(148)*A(165)*A(151
     .)*A(78)*A(31)+A(2)*A(165)*A(151)*A(78)*A(27)
         A(1187)=3*A(148)*A(11)*A(80)*A(35)+3*A(148)*A(11)*A(78)*A(35)
         A(1188)=A(1186)+A(1187)
         A(1189)=2*A(2)*A(165)*A(151)*A(81)*A(27)-3*A(148)*A(165)*A(151
     .)*A(79)*A(31)+A(2)*A(165)*A(151)*A(79)*A(27)
         A(1190)=3*A(148)*A(11)*A(81)*A(35)+3*A(148)*A(11)*A(79)*A(35)
         A(1191)=A(1189)+A(1190)
         A(1192)=2*A(2)*A(165)*A(151)*A(80)*A(28)-3*A(148)*A(165)*A(151
     .)*A(78)*A(32)+A(2)*A(165)*A(151)*A(78)*A(28)
         A(1193)=3*A(148)*A(11)*A(80)*A(36)+3*A(148)*A(11)*A(78)*A(36)
         A(1194)=A(1192)+A(1193)
         A(1195)=2*A(2)*A(165)*A(151)*A(81)*A(28)-3*A(148)*A(165)*A(151
     .)*A(79)*A(32)+A(2)*A(165)*A(151)*A(79)*A(28)
         A(1196)=3*A(148)*A(11)*A(81)*A(36)+3*A(148)*A(11)*A(79)*A(36)
         A(1197)=A(1195)+A(1196)
         A(1198)=2*A(2)*A(165)*A(151)*A(80)*A(29)-3*A(148)*A(165)*A(151
     .)*A(78)*A(33)+A(2)*A(165)*A(151)*A(78)*A(29)
         A(1199)=3*A(148)*A(11)*A(80)*A(37)+3*A(148)*A(11)*A(78)*A(37)
         A(1200)=A(1198)+A(1199)
         A(1201)=2*A(2)*A(165)*A(151)*A(80)*A(30)-3*A(148)*A(165)*A(151
     .)*A(78)*A(34)+A(2)*A(165)*A(151)*A(78)*A(30)
         A(1202)=3*A(148)*A(11)*A(80)*A(38)+3*A(148)*A(11)*A(78)*A(38)
         A(1203)=A(1201)+A(1202)
         A(1204)=3*A(148)*A(164)*A(151)*A(31)*A(58)+A(2)*A(164)*A(151)*
     .A(27)*A(58)-4*A(2)*A(164)*A(151)*A(27)*A(60)
         A(1205)=3*A(148)*A(12)*A(39)*A(60)+3*A(148)*A(12)*A(39)*A(58)
         A(1206)=A(1204)+A(1205)
         A(1207)=3*A(148)*A(164)*A(151)*A(31)*A(59)+A(2)*A(164)*A(151)*
     .A(27)*A(59)-4*A(2)*A(164)*A(151)*A(27)*A(61)
         A(1208)=3*A(148)*A(12)*A(39)*A(61)+3*A(148)*A(12)*A(39)*A(59)
         A(1209)=A(1207)+A(1208)
         A(1210)=3*A(148)*A(164)*A(151)*A(32)*A(58)+A(2)*A(164)*A(151)*
     .A(28)*A(58)-4*A(2)*A(164)*A(151)*A(28)*A(60)
         A(1211)=3*A(148)*A(12)*A(40)*A(60)+3*A(148)*A(12)*A(40)*A(58)
         A(1212)=A(1210)+A(1211)
         A(1213)=3*A(148)*A(164)*A(151)*A(32)*A(59)+A(2)*A(164)*A(151)*
     .A(28)*A(59)-4*A(2)*A(164)*A(151)*A(28)*A(61)
         A(1214)=3*A(148)*A(12)*A(40)*A(61)+3*A(148)*A(12)*A(40)*A(59)
         A(1215)=A(1213)+A(1214)
         A(1216)=3*A(148)*A(164)*A(151)*A(33)*A(58)+A(2)*A(164)*A(151)*
     .A(29)*A(58)-4*A(2)*A(164)*A(151)*A(29)*A(60)
         A(1217)=3*A(148)*A(12)*A(41)*A(60)+3*A(148)*A(12)*A(41)*A(58)
         A(1218)=A(1216)+A(1217)
         A(1219)=3*A(148)*A(164)*A(151)*A(34)*A(58)+A(2)*A(164)*A(151)*
     .A(30)*A(58)-4*A(2)*A(164)*A(151)*A(30)*A(60)
         A(1220)=3*A(148)*A(12)*A(42)*A(60)+3*A(148)*A(12)*A(42)*A(58)
         A(1221)=A(1219)+A(1220)
         A(1222)=4*A(2)*A(27)*A(64)-A(2)*A(27)*A(62)-3*A(148)*A(31)*A(6
     .2)
         A(1223)=A(1222)
         A(1224)=4*A(2)*A(27)*A(65)-A(2)*A(27)*A(63)-3*A(148)*A(31)*A(6
     .3)
         A(1225)=A(1224)
         A(1226)=4*A(2)*A(28)*A(64)-A(2)*A(28)*A(62)-3*A(148)*A(32)*A(6
     .2)
         A(1227)=A(1226)
         A(1228)=4*A(2)*A(28)*A(65)-A(2)*A(28)*A(63)-3*A(148)*A(32)*A(6
     .3)
         A(1229)=A(1228)
         A(1230)=4*A(2)*A(29)*A(64)-A(2)*A(29)*A(62)-3*A(148)*A(33)*A(6
     .2)
         A(1231)=A(1230)
         A(1232)=4*A(2)*A(30)*A(64)-A(2)*A(30)*A(62)-3*A(148)*A(34)*A(6
     .2)
         A(1233)=A(1232)
         A(1234)=A(2)*A(164)*A(147)*A(25)*A(27)+A(148)*A(164)*A(147)*A(
     .25)*A(31)-2*A(148)*A(164)*A(23)*A(35)
         A(1235)=A(2)*A(165)*A(147)*A(27)*A(21)+2*A(148)*A(165)*A(39)*A
     .(19)+A(148)*A(165)*A(147)*A(31)*A(21)
         A(1236)=A(1234)+A(1235)
         A(1237)=-A(2)*A(165)*A(147)*A(27)*A(21)-2*A(148)*A(165)*A(39)*
     .A(19)-A(148)*A(165)*A(147)*A(31)*A(21)
         A(1238)=A(148)*A(164)*A(147)*A(25)*A(31)-2*A(148)*A(164)*A(23)
     .*A(35)+A(2)*A(164)*A(147)*A(25)*A(27)
         A(1239)=A(1237)+A(1238)
         A(1240)=A(147)*A(39)*A(21)+A(147)*A(25)*A(35)-2*A(31)*A(19)
         A(1241)=2*A(23)*A(31)
         A(1242)=A(1240)+A(1241)
         A(1243)=-A(147)*A(25)*A(35)-2*A(31)*A(19)-2*A(23)*A(31)
         A(1244)=A(147)*A(39)*A(21)
         A(1245)=A(1243)+A(1244)
         A(1246)=A(2)*A(164)*A(147)*A(25)*A(28)+A(148)*A(164)*A(147)*A(
     .25)*A(32)-2*A(148)*A(164)*A(23)*A(36)
         A(1247)=A(2)*A(165)*A(147)*A(28)*A(21)+2*A(148)*A(165)*A(40)*A
     .(19)+A(148)*A(165)*A(147)*A(32)*A(21)
         A(1248)=A(1246)+A(1247)
         A(1249)=-A(2)*A(165)*A(147)*A(28)*A(21)-2*A(148)*A(165)*A(40)*
     .A(19)-A(148)*A(165)*A(147)*A(32)*A(21)
         A(1250)=A(148)*A(164)*A(147)*A(25)*A(32)-2*A(148)*A(164)*A(23)
     .*A(36)+A(2)*A(164)*A(147)*A(25)*A(28)
         A(1251)=A(1249)+A(1250)
         A(1252)=A(147)*A(40)*A(21)+A(147)*A(25)*A(36)-2*A(32)*A(19)
         A(1253)=2*A(23)*A(32)
         A(1254)=A(1252)+A(1253)
         A(1255)=-A(147)*A(25)*A(36)-2*A(32)*A(19)-2*A(23)*A(32)
         A(1256)=A(147)*A(40)*A(21)
         A(1257)=A(1255)+A(1256)
         A(1258)=A(2)*A(164)*A(147)*A(25)*A(29)+A(148)*A(164)*A(147)*A(
     .25)*A(33)-2*A(148)*A(164)*A(23)*A(37)
         A(1259)=A(2)*A(165)*A(147)*A(29)*A(21)+2*A(148)*A(165)*A(41)*A
     .(19)+A(148)*A(165)*A(147)*A(33)*A(21)
         A(1260)=A(1258)+A(1259)
         A(1261)=-A(2)*A(165)*A(147)*A(29)*A(21)-2*A(148)*A(165)*A(41)*
     .A(19)-A(148)*A(165)*A(147)*A(33)*A(21)
         A(1262)=A(148)*A(164)*A(147)*A(25)*A(33)-2*A(148)*A(164)*A(23)
     .*A(37)+A(2)*A(164)*A(147)*A(25)*A(29)
         A(1263)=A(1261)+A(1262)
         A(1264)=A(147)*A(41)*A(21)+A(147)*A(25)*A(37)-2*A(33)*A(19)
         A(1265)=2*A(23)*A(33)
         A(1266)=A(1264)+A(1265)
         A(1267)=-A(147)*A(25)*A(37)-2*A(33)*A(19)-2*A(23)*A(33)
         A(1268)=A(147)*A(41)*A(21)
         A(1269)=A(1267)+A(1268)
         A(1270)=A(2)*A(164)*A(147)*A(25)*A(30)+A(148)*A(164)*A(147)*A(
     .25)*A(34)-2*A(148)*A(164)*A(23)*A(38)
         A(1271)=A(2)*A(165)*A(147)*A(30)*A(21)+2*A(148)*A(165)*A(42)*A
     .(19)+A(148)*A(165)*A(147)*A(34)*A(21)
         A(1272)=A(1270)+A(1271)
         A(1273)=-A(2)*A(165)*A(147)*A(30)*A(21)-2*A(148)*A(165)*A(42)*
     .A(19)-A(148)*A(165)*A(147)*A(34)*A(21)
         A(1274)=A(148)*A(164)*A(147)*A(25)*A(34)-2*A(148)*A(164)*A(23)
     .*A(38)+A(2)*A(164)*A(147)*A(25)*A(30)
         A(1275)=A(1273)+A(1274)
         A(1276)=A(147)*A(42)*A(21)+A(147)*A(25)*A(38)-2*A(34)*A(19)
         A(1277)=2*A(23)*A(34)
         A(1278)=A(1276)+A(1277)
         A(1279)=-A(147)*A(25)*A(38)-2*A(34)*A(19)-2*A(23)*A(34)
         A(1280)=A(147)*A(42)*A(21)
         A(1281)=A(1279)+A(1280)
         A(1282)=A(165)*A(10)*A(147)*A(70)*A(21)-A(164)*A(13)*A(147)*A(
     .72)*A(25)-A(172)*A(151)*A(70)*A(23)
         A(1283)=A(1282)
         A(1284)=A(165)*A(10)*A(147)*A(71)*A(21)-A(164)*A(13)*A(147)*A(
     .73)*A(25)-A(172)*A(151)*A(71)*A(23)
         A(1285)=A(1284)
         A(1286)=A(10)*A(147)*A(21)-2*A(164)*A(151)*A(23)
         A(1287)=A(1286)
         A(1288)=A(165)*A(10)*A(147)*A(78)*A(21)-A(164)*A(11)*A(147)*A(
     .80)*A(25)-A(172)*A(151)*A(78)*A(23)
         A(1289)=A(1288)
         A(1290)=A(165)*A(10)*A(147)*A(79)*A(21)-A(164)*A(11)*A(147)*A(
     .81)*A(25)-A(172)*A(151)*A(79)*A(23)
         A(1291)=A(1290)
         A(1292)=-A(8)*A(147)*A(54)*A(25)-2*A(165)*A(151)*A(52)*A(23)
         A(1293)=A(1292)
         A(1294)=-A(8)*A(147)*A(55)*A(25)-2*A(165)*A(151)*A(53)*A(23)
         A(1295)=A(1294)
         A(1296)=-A(9)*A(147)*A(46)*A(25)-2*A(165)*A(151)*A(44)*A(23)
         A(1297)=A(1296)
         A(1298)=-A(9)*A(147)*A(47)*A(25)-2*A(165)*A(151)*A(45)*A(23)
         A(1299)=A(1298)
         A(1300)=A(165)*A(12)*A(147)*A(70)*A(21)-A(164)*A(13)*A(147)*A(
     .72)*A(25)-A(172)*A(151)*A(70)*A(23)
         A(1301)=A(1300)
         A(1302)=A(165)*A(12)*A(147)*A(71)*A(21)-A(164)*A(13)*A(147)*A(
     .73)*A(25)-A(172)*A(151)*A(71)*A(23)
         A(1303)=A(1302)
         A(1304)=A(12)*A(147)*A(21)-2*A(164)*A(151)*A(23)
         A(1305)=A(1304)
         A(1306)=A(165)*A(12)*A(147)*A(78)*A(21)-A(164)*A(11)*A(147)*A(
     .80)*A(25)-A(172)*A(151)*A(78)*A(23)
         A(1307)=A(1306)
         A(1308)=A(165)*A(12)*A(147)*A(79)*A(21)-A(164)*A(11)*A(147)*A(
     .81)*A(25)-A(172)*A(151)*A(79)*A(23)
         A(1309)=A(1308)
         A(1310)=-A(13)*A(147)*A(72)*A(25)-2*A(165)*A(151)*A(70)*A(23)
         A(1311)=A(1310)
         A(1312)=-A(13)*A(147)*A(73)*A(25)-2*A(165)*A(151)*A(71)*A(23)
         A(1313)=A(1312)
         A(1314)=-A(11)*A(147)*A(80)*A(25)-2*A(165)*A(151)*A(78)*A(23)
         A(1315)=A(1314)
         A(1316)=-A(11)*A(147)*A(81)*A(25)-2*A(165)*A(151)*A(79)*A(23)
         A(1317)=A(1316)
         A(1318)=A(17)*A(23)*A(21)-A(166)*A(25)*A(19)
         A(1319)=A(1)/(A(2))*A(147)/DFLOAT((2))*A(1318)
         A(1320)=A(164)*A(25)*A(19)+A(165)*A(23)*A(21)
         A(1321)=-A(1)/(A(2))*A(147)/DFLOAT((2))*A(1320)
         A(1322)=(A(21))**(2)-(A(25))**(2)+2*(A(19))**(2)
         A(1323)=-2*(A(23))**(2)
         A(1324)=A(1322)+A(1323)
         A(1325)=-(A(25))**(2)-2*(A(19))**(2)-2*(A(23))**(2)
         A(1326)=4*(A(2))**(2)-(A(21))**(2)
         A(1327)=A(1325)+A(1326)
         A(1328)=A(166)*A(23)*A(21)+A(17)*A(25)*A(19)
         A(1329)=-A(1)/(A(2))*A(147)/DFLOAT((2))*A(1328)
         A(1330)=A(166)*A(26)*A(19)-A(166)*A(25)*A(20)-A(17)*A(24)*A(21
     .)
         A(1331)=A(17)*A(23)*A(22)
         A(1332)=A(1330)+A(1331)
         A(1333)=A(166)*A(25)*A(20)-A(17)*A(24)*A(21)-A(17)*A(23)*A(22)
         A(1334)=A(166)*A(26)*A(19)
         A(1335)=A(1333)+A(1334)
         A(1336)=A(164)*A(26)*A(19)+A(164)*A(25)*A(20)+A(165)*A(24)*A(2
     .1)
         A(1337)=A(165)*A(23)*A(22)
         A(1338)=A(1336)+A(1337)
         A(1339)=A(165)*A(24)*A(21)-A(165)*A(23)*A(22)-A(164)*A(25)*A(2
     .0)
         A(1340)=A(164)*A(26)*A(19)
         A(1341)=A(1339)+A(1340)
         A(1342)=A(21)*A(22)-A(25)*A(26)+2*A(19)*A(20)
         A(1343)=-2*A(23)*A(24)
         A(1344)=A(1342)+A(1343)
         A(1345)=-A(25)*A(26)-2*A(19)*A(20)-2*A(23)*A(24)
         A(1346)=-A(21)*A(22)
         A(1347)=A(1345)+A(1346)
         A(1348)=A(17)*A(26)*A(19)-A(17)*A(25)*A(20)+A(166)*A(24)*A(21)
         A(1349)=-A(166)*A(23)*A(22)
         A(1350)=A(1348)+A(1349)
         A(1351)=A(166)*A(23)*A(22)+A(166)*A(24)*A(21)+A(17)*A(25)*A(20
     .)
         A(1352)=A(17)*A(26)*A(19)
         A(1353)=A(1351)+A(1352)
         A(1354)=A(148)*A(165)*A(147)*A(31)*A(21)+2*A(148)*A(165)*A(39)
     .*A(19)+A(2)*A(165)*A(147)*A(27)*A(21)
         A(1355)=2*A(148)*A(164)*A(23)*A(35)-A(148)*A(164)*A(147)*A(25)
     .*A(31)-A(2)*A(164)*A(147)*A(25)*A(27)
         A(1356)=A(1354)+A(1355)
         A(1357)=2*A(23)*A(31)+2*A(31)*A(19)+A(147)*A(25)*A(35)
         A(1358)=-A(147)*A(39)*A(21)
         A(1359)=A(1357)+A(1358)
         A(1360)=A(148)*A(165)*A(147)*A(32)*A(21)+2*A(148)*A(165)*A(40)
     .*A(19)+A(2)*A(165)*A(147)*A(28)*A(21)
         A(1361)=2*A(148)*A(164)*A(23)*A(36)-A(148)*A(164)*A(147)*A(25)
     .*A(32)-A(2)*A(164)*A(147)*A(25)*A(28)
         A(1362)=A(1360)+A(1361)
         A(1363)=2*A(23)*A(32)+2*A(32)*A(19)+A(147)*A(25)*A(36)
         A(1364)=-A(147)*A(40)*A(21)
         A(1365)=A(1363)+A(1364)
         A(1366)=A(148)*A(165)*A(147)*A(33)*A(21)+2*A(148)*A(165)*A(41)
     .*A(19)+A(2)*A(165)*A(147)*A(29)*A(21)
         A(1367)=2*A(148)*A(164)*A(23)*A(37)-A(148)*A(164)*A(147)*A(25)
     .*A(33)-A(2)*A(164)*A(147)*A(25)*A(29)
         A(1368)=A(1366)+A(1367)
         A(1369)=2*A(23)*A(33)+2*A(33)*A(19)+A(147)*A(25)*A(37)
         A(1370)=-A(147)*A(41)*A(21)
         A(1371)=A(1369)+A(1370)
         A(1372)=A(148)*A(165)*A(147)*A(34)*A(21)+2*A(148)*A(165)*A(42)
     .*A(19)+A(2)*A(165)*A(147)*A(30)*A(21)
         A(1373)=2*A(148)*A(164)*A(23)*A(38)-A(148)*A(164)*A(147)*A(25)
     .*A(34)-A(2)*A(164)*A(147)*A(25)*A(30)
         A(1374)=A(1372)+A(1373)
         A(1375)=2*A(23)*A(34)+2*A(34)*A(19)+A(147)*A(25)*A(38)
         A(1376)=-A(147)*A(42)*A(21)
         A(1377)=A(1375)+A(1376)
         A(1378)=A(2)*A(164)*A(147)*A(26)*A(27)+A(148)*A(164)*A(147)*A(
     .26)*A(31)-2*A(148)*A(164)*A(24)*A(35)
         A(1379)=A(2)*A(165)*A(147)*A(27)*A(22)+2*A(148)*A(165)*A(39)*A
     .(20)+A(148)*A(165)*A(147)*A(31)*A(22)
         A(1380)=A(1378)+A(1379)
         A(1381)=-A(2)*A(165)*A(147)*A(27)*A(22)-2*A(148)*A(165)*A(39)*
     .A(20)-A(148)*A(165)*A(147)*A(31)*A(22)
         A(1382)=A(148)*A(164)*A(147)*A(26)*A(31)-2*A(148)*A(164)*A(24)
     .*A(35)+A(2)*A(164)*A(147)*A(26)*A(27)
         A(1383)=A(1381)+A(1382)
         A(1384)=A(147)*A(39)*A(22)+A(147)*A(26)*A(35)-2*A(31)*A(20)
         A(1385)=2*A(24)*A(31)
         A(1386)=A(1384)+A(1385)
         A(1387)=-A(147)*A(26)*A(35)-2*A(31)*A(20)-2*A(24)*A(31)
         A(1388)=A(147)*A(39)*A(22)
         A(1389)=A(1387)+A(1388)
         A(1390)=A(2)*A(164)*A(147)*A(26)*A(28)+A(148)*A(164)*A(147)*A(
     .26)*A(32)-2*A(148)*A(164)*A(24)*A(36)
         A(1391)=A(2)*A(165)*A(147)*A(28)*A(22)+2*A(148)*A(165)*A(40)*A
     .(20)+A(148)*A(165)*A(147)*A(32)*A(22)
         A(1392)=A(1390)+A(1391)
         A(1393)=-A(2)*A(165)*A(147)*A(28)*A(22)-2*A(148)*A(165)*A(40)*
     .A(20)-A(148)*A(165)*A(147)*A(32)*A(22)
         A(1394)=A(148)*A(164)*A(147)*A(26)*A(32)-2*A(148)*A(164)*A(24)
     .*A(36)+A(2)*A(164)*A(147)*A(26)*A(28)
         A(1395)=A(1393)+A(1394)
         A(1396)=A(147)*A(40)*A(22)+A(147)*A(26)*A(36)-2*A(32)*A(20)
         A(1397)=2*A(24)*A(32)
         A(1398)=A(1396)+A(1397)
         A(1399)=-A(147)*A(26)*A(36)-2*A(32)*A(20)-2*A(24)*A(32)
         A(1400)=A(147)*A(40)*A(22)
         A(1401)=A(1399)+A(1400)
         A(1402)=A(165)*A(10)*A(147)*A(70)*A(22)-A(164)*A(13)*A(147)*A(
     .72)*A(26)-A(172)*A(151)*A(70)*A(24)
         A(1403)=A(1402)
         A(1404)=A(10)*A(147)*A(22)-2*A(164)*A(151)*A(24)
         A(1405)=A(1404)
         A(1406)=A(165)*A(10)*A(147)*A(78)*A(22)-A(164)*A(11)*A(147)*A(
     .80)*A(26)-A(172)*A(151)*A(78)*A(24)
         A(1407)=A(1406)
         A(1408)=-A(8)*A(147)*A(54)*A(26)-2*A(165)*A(151)*A(52)*A(24)
         A(1409)=A(1408)
         A(1410)=-A(9)*A(147)*A(46)*A(26)-2*A(165)*A(151)*A(44)*A(24)
         A(1411)=A(1410)
         A(1412)=A(165)*A(12)*A(147)*A(70)*A(22)-A(164)*A(13)*A(147)*A(
     .72)*A(26)-A(172)*A(151)*A(70)*A(24)
         A(1413)=A(1412)
         A(1414)=A(12)*A(147)*A(22)-2*A(164)*A(151)*A(24)
         A(1415)=A(1414)
         A(1416)=A(165)*A(12)*A(147)*A(78)*A(22)-A(164)*A(11)*A(147)*A(
     .80)*A(26)-A(172)*A(151)*A(78)*A(24)
         A(1417)=A(1416)
         A(1418)=-A(13)*A(147)*A(72)*A(26)-2*A(165)*A(151)*A(70)*A(24)
         A(1419)=A(1418)
         A(1420)=-A(11)*A(147)*A(80)*A(26)-2*A(165)*A(151)*A(78)*A(24)
         A(1421)=A(1420)
         A(1422)=A(166)*A(25)*A(20)-A(166)*A(26)*A(19)-A(17)*A(23)*A(22
     .)
         A(1423)=A(17)*A(24)*A(21)
         A(1424)=A(1422)+A(1423)
         A(1425)=A(166)*A(26)*A(19)-A(17)*A(23)*A(22)-A(17)*A(24)*A(21)
         A(1426)=A(166)*A(25)*A(20)
         A(1427)=A(1425)+A(1426)
         A(1428)=A(164)*A(25)*A(20)+A(164)*A(26)*A(19)+A(165)*A(23)*A(2
     .2)
         A(1429)=A(165)*A(24)*A(21)
         A(1430)=A(1428)+A(1429)
         A(1431)=A(165)*A(23)*A(22)-A(165)*A(24)*A(21)-A(164)*A(26)*A(1
     .9)
         A(1432)=A(164)*A(25)*A(20)
         A(1433)=A(1431)+A(1432)
         A(1434)=A(17)*A(25)*A(20)-A(17)*A(26)*A(19)+A(166)*A(23)*A(22)
         A(1435)=-A(166)*A(24)*A(21)
         A(1436)=A(1434)+A(1435)
         A(1437)=A(166)*A(24)*A(21)+A(166)*A(23)*A(22)+A(17)*A(26)*A(19
     .)
         A(1438)=A(17)*A(25)*A(20)
         A(1439)=A(1437)+A(1438)
         A(1440)=A(148)*A(165)*A(147)*A(31)*A(22)+2*A(148)*A(165)*A(39)
     .*A(20)+A(2)*A(165)*A(147)*A(27)*A(22)
         A(1441)=2*A(148)*A(164)*A(24)*A(35)-A(148)*A(164)*A(147)*A(26)
     .*A(31)-A(2)*A(164)*A(147)*A(26)*A(27)
         A(1442)=A(1440)+A(1441)
         A(1443)=2*A(24)*A(31)+2*A(31)*A(20)+A(147)*A(26)*A(35)
         A(1444)=-A(147)*A(39)*A(22)
         A(1445)=A(1443)+A(1444)
         A(1446)=A(148)*A(165)*A(147)*A(32)*A(22)+2*A(148)*A(165)*A(40)
     .*A(20)+A(2)*A(165)*A(147)*A(28)*A(22)
         A(1447)=2*A(148)*A(164)*A(24)*A(36)-A(148)*A(164)*A(147)*A(26)
     .*A(32)-A(2)*A(164)*A(147)*A(26)*A(28)
         A(1448)=A(1446)+A(1447)
         A(1449)=2*A(24)*A(32)+2*A(32)*A(20)+A(147)*A(26)*A(36)
         A(1450)=-A(147)*A(40)*A(22)
         A(1451)=A(1449)+A(1450)
         A(1452)=A(148)*A(17)*A(31)*A(39)-A(2)*A(17)*A(27)*A(39)
         A(1453)=A(148)*A(166)*A(31)*A(35)-A(2)*A(166)*A(27)*A(35)
         A(1454)=-2*A(1)/(A(149))*(A(1452)+A(1453))
         A(1455)=A(2)*A(164)*A(27)*A(35)-A(148)*A(164)*A(31)*A(35)
         A(1456)=A(148)*A(165)*A(31)*A(39)-A(2)*A(165)*A(27)*A(39)
         A(1457)=2*A(1)/(A(149))*(A(1455)+A(1456))
         A(1458)=(A(35))**(2)-(A(39))**(2)
         A(1459)=A(1)/(A(149))*A(1458)
         A(1460)=A(148)*A(166)*A(31)*A(39)-A(2)*A(166)*A(27)*A(39)
         A(1461)=A(2)*A(17)*A(27)*A(35)-A(148)*A(17)*A(31)*A(35)
         A(1462)=2*A(1)/(A(149))*(A(1460)+A(1461))
         A(1463)=A(148)*A(17)*A(32)*A(39)+A(148)*A(17)*A(31)*A(40)
         A(1464)=-A(2)*A(17)*A(28)*A(39)-A(2)*A(17)*A(27)*A(40)
         A(1465)=A(148)*A(166)*A(32)*A(35)+A(148)*A(166)*A(31)*A(36)
         A(1466)=-A(2)*A(166)*A(28)*A(35)-A(2)*A(166)*A(27)*A(36)
         A(1467)=-A(1)/(A(149))*(A(1463)+A(1464)+A(1465)+A(1466))
         A(1468)=A(2)*A(164)*A(28)*A(35)+A(2)*A(164)*A(27)*A(36)
         A(1469)=-A(148)*A(164)*A(32)*A(35)-A(148)*A(164)*A(31)*A(36)
         A(1470)=-A(2)*A(165)*A(27)*A(40)-A(2)*A(165)*A(28)*A(39)
         A(1471)=A(148)*A(165)*A(31)*A(40)+A(148)*A(165)*A(32)*A(39)
         A(1472)=A(1)/(A(149))*(A(1468)+A(1469)+A(1470)+A(1471))
         A(1473)=A(39)*A(40)-A(35)*A(36)
         A(1474)=-A(1)/(A(149))*A(1473)
         A(1475)=A(148)*A(166)*A(32)*A(39)+A(148)*A(166)*A(31)*A(40)
         A(1476)=-A(2)*A(166)*A(28)*A(39)-A(2)*A(166)*A(27)*A(40)
         A(1477)=-A(148)*A(17)*A(31)*A(36)-A(148)*A(17)*A(32)*A(35)
         A(1478)=A(2)*A(17)*A(27)*A(36)+A(2)*A(17)*A(28)*A(35)
         A(1479)=A(1)/(A(149))*(A(1475)+A(1476)+A(1477)+A(1478))
         A(1480)=A(148)*A(17)*A(33)*A(39)+A(148)*A(17)*A(31)*A(41)
         A(1481)=-A(2)*A(17)*A(29)*A(39)-A(2)*A(17)*A(27)*A(41)
         A(1482)=A(148)*A(166)*A(33)*A(35)+A(148)*A(166)*A(31)*A(37)
         A(1483)=-A(2)*A(166)*A(29)*A(35)-A(2)*A(166)*A(27)*A(37)
         A(1484)=-A(1)/(A(149))*(A(1480)+A(1481)+A(1482)+A(1483))
         A(1485)=A(2)*A(164)*A(29)*A(35)+A(2)*A(164)*A(27)*A(37)
         A(1486)=-A(148)*A(164)*A(33)*A(35)-A(148)*A(164)*A(31)*A(37)
         A(1487)=-A(2)*A(165)*A(27)*A(41)-A(2)*A(165)*A(29)*A(39)
         A(1488)=A(148)*A(165)*A(31)*A(41)+A(148)*A(165)*A(33)*A(39)
         A(1489)=A(1)/(A(149))*(A(1485)+A(1486)+A(1487)+A(1488))
         A(1490)=A(39)*A(41)-A(35)*A(37)
         A(1491)=-A(1)/(A(149))*A(1490)
         A(1492)=A(148)*A(166)*A(33)*A(39)+A(148)*A(166)*A(31)*A(41)
         A(1493)=-A(2)*A(166)*A(29)*A(39)-A(2)*A(166)*A(27)*A(41)
         A(1494)=-A(148)*A(17)*A(31)*A(37)-A(148)*A(17)*A(33)*A(35)
         A(1495)=A(2)*A(17)*A(27)*A(37)+A(2)*A(17)*A(29)*A(35)
         A(1496)=A(1)/(A(149))*(A(1492)+A(1493)+A(1494)+A(1495))
         A(1497)=A(148)*A(17)*A(34)*A(39)+A(148)*A(17)*A(31)*A(42)
         A(1498)=-A(2)*A(17)*A(30)*A(39)-A(2)*A(17)*A(27)*A(42)
         A(1499)=A(148)*A(166)*A(34)*A(35)+A(148)*A(166)*A(31)*A(38)
         A(1500)=-A(2)*A(166)*A(30)*A(35)-A(2)*A(166)*A(27)*A(38)
         A(1501)=-A(1)/(A(149))*(A(1497)+A(1498)+A(1499)+A(1500))
         A(1502)=A(2)*A(164)*A(30)*A(35)+A(2)*A(164)*A(27)*A(38)
         A(1503)=-A(148)*A(164)*A(34)*A(35)-A(148)*A(164)*A(31)*A(38)
         A(1504)=-A(2)*A(165)*A(27)*A(42)-A(2)*A(165)*A(30)*A(39)
         A(1505)=A(148)*A(165)*A(31)*A(42)+A(148)*A(165)*A(34)*A(39)
         A(1506)=A(1)/(A(149))*(A(1502)+A(1503)+A(1504)+A(1505))
         A(1507)=A(39)*A(42)-A(35)*A(38)
         A(1508)=-A(1)/(A(149))*A(1507)
         A(1509)=A(148)*A(166)*A(34)*A(39)+A(148)*A(166)*A(31)*A(42)
         A(1510)=-A(2)*A(166)*A(30)*A(39)-A(2)*A(166)*A(27)*A(42)
         A(1511)=-A(148)*A(17)*A(31)*A(38)-A(148)*A(17)*A(34)*A(35)
         A(1512)=A(2)*A(17)*A(27)*A(38)+A(2)*A(17)*A(30)*A(35)
         A(1513)=A(1)/(A(149))*(A(1509)+A(1510)+A(1511)+A(1512))
         A(1514)=A(148)*A(17)*A(32)*A(40)-A(2)*A(17)*A(28)*A(40)
         A(1515)=A(148)*A(166)*A(32)*A(36)-A(2)*A(166)*A(28)*A(36)
         A(1516)=-2*A(1)/(A(149))*(A(1514)+A(1515))
         A(1517)=A(2)*A(164)*A(28)*A(36)-A(148)*A(164)*A(32)*A(36)
         A(1518)=A(148)*A(165)*A(32)*A(40)-A(2)*A(165)*A(28)*A(40)
         A(1519)=2*A(1)/(A(149))*(A(1517)+A(1518))
         A(1520)=(A(36))**(2)-(A(40))**(2)
         A(1521)=A(1)/(A(149))*A(1520)
         A(1522)=A(148)*A(166)*A(32)*A(40)-A(2)*A(166)*A(28)*A(40)
         A(1523)=A(2)*A(17)*A(28)*A(36)-A(148)*A(17)*A(32)*A(36)
         A(1524)=2*A(1)/(A(149))*(A(1522)+A(1523))
         A(1525)=A(148)*A(17)*A(33)*A(40)+A(148)*A(17)*A(32)*A(41)
         A(1526)=-A(2)*A(17)*A(29)*A(40)-A(2)*A(17)*A(28)*A(41)
         A(1527)=A(148)*A(166)*A(33)*A(36)+A(148)*A(166)*A(32)*A(37)
         A(1528)=-A(2)*A(166)*A(29)*A(36)-A(2)*A(166)*A(28)*A(37)
         A(1529)=-A(1)/(A(149))*(A(1525)+A(1526)+A(1527)+A(1528))
         A(1530)=A(2)*A(164)*A(29)*A(36)+A(2)*A(164)*A(28)*A(37)
         A(1531)=-A(148)*A(164)*A(33)*A(36)-A(148)*A(164)*A(32)*A(37)
         A(1532)=-A(2)*A(165)*A(28)*A(41)-A(2)*A(165)*A(29)*A(40)
         A(1533)=A(148)*A(165)*A(32)*A(41)+A(148)*A(165)*A(33)*A(40)
         A(1534)=A(1)/(A(149))*(A(1530)+A(1531)+A(1532)+A(1533))
         A(1535)=A(40)*A(41)-A(36)*A(37)
         A(1536)=-A(1)/(A(149))*A(1535)
         A(1537)=A(148)*A(166)*A(33)*A(40)+A(148)*A(166)*A(32)*A(41)
         A(1538)=-A(2)*A(166)*A(29)*A(40)-A(2)*A(166)*A(28)*A(41)
         A(1539)=-A(148)*A(17)*A(32)*A(37)-A(148)*A(17)*A(33)*A(36)
         A(1540)=A(2)*A(17)*A(28)*A(37)+A(2)*A(17)*A(29)*A(36)
         A(1541)=A(1)/(A(149))*(A(1537)+A(1538)+A(1539)+A(1540))
         A(1542)=A(148)*A(17)*A(34)*A(40)+A(148)*A(17)*A(32)*A(42)
         A(1543)=-A(2)*A(17)*A(30)*A(40)-A(2)*A(17)*A(28)*A(42)
         A(1544)=A(148)*A(166)*A(34)*A(36)+A(148)*A(166)*A(32)*A(38)
         A(1545)=-A(2)*A(166)*A(30)*A(36)-A(2)*A(166)*A(28)*A(38)
         A(1546)=-A(1)/(A(149))*(A(1542)+A(1543)+A(1544)+A(1545))
         A(1547)=A(2)*A(164)*A(30)*A(36)+A(2)*A(164)*A(28)*A(38)
         A(1548)=-A(148)*A(164)*A(34)*A(36)-A(148)*A(164)*A(32)*A(38)
         A(1549)=-A(2)*A(165)*A(28)*A(42)-A(2)*A(165)*A(30)*A(40)
         A(1550)=A(148)*A(165)*A(32)*A(42)+A(148)*A(165)*A(34)*A(40)
         A(1551)=A(1)/(A(149))*(A(1547)+A(1548)+A(1549)+A(1550))
         A(1552)=A(40)*A(42)-A(36)*A(38)
         A(1553)=-A(1)/(A(149))*A(1552)
         A(1554)=A(148)*A(166)*A(34)*A(40)+A(148)*A(166)*A(32)*A(42)
         A(1555)=-A(2)*A(166)*A(30)*A(40)-A(2)*A(166)*A(28)*A(42)
         A(1556)=-A(148)*A(17)*A(32)*A(38)-A(148)*A(17)*A(34)*A(36)
         A(1557)=A(2)*A(17)*A(28)*A(38)+A(2)*A(17)*A(30)*A(36)
         A(1558)=A(1)/(A(149))*(A(1554)+A(1555)+A(1556)+A(1557))
         A(1559)=2*(A(1))**(2)/(A(149))/DFLOAT((9))*A(929)
         A(1560)=4*(A(1))**(2)/(A(149))/DFLOAT((9))*A(933)
         A(1561)=2*(A(1))**(2)/(A(149))/DFLOAT((9))*A(937)
         A(1562)=2*(A(1))**(2)/(A(149))*A(941)
         A(1563)=2*(A(1))**(2)/(A(149))*A(945)
         A(1564)=2*(A(1))**(2)/(A(149))*A(949)
         A(1565)=2*(A(1))**(2)/(A(149))/DFLOAT((9))*A(953)
         A(1566)=4*(A(1))**(2)/(A(149))/DFLOAT((9))*A(957)
         A(1567)=4*(A(1))**(2)/(A(149))/DFLOAT((9))*A(961)
         A(1568)=2*(A(2))**(2)*A(174)*(A(165))**(2)*(A(151))**(2)*(A(72
     .))**(2)+3*A(174)*(A(165))**(2)*(A(151))**(2)*(A(70))**(2)
         A(1569)=-6*(A(148))**(2)*(A(166))**(2)*(A(13))**(2)-2*(A(2))**
     .(2)*A(174)*(A(165))**(2)*(A(151))**(2)*(A(70))**(2)
         A(1570)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))*
     .*(2))/DFLOAT((3))*(A(1568)+A(1569))
         A(1571)=A(175)*A(174)*(A(164))**(2)*(A(151))**(2)*(A(66))**(2)
     .-4*(A(2))**(2)*A(174)*(A(164))**(2)*(A(151))**(2)*(A(68))**(2)
         A(1572)=-6*(A(148))**(2)*(A(17))**(2)*(A(10))**(2)
         A(1573)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(164))*
     .*(2))/DFLOAT((3))*(A(1571)+A(1572))
         A(1574)=(A(1))**(2)/((A(149))**(2))*A(174)/DFLOAT((3))*(A(522)
     .+A(523))
         A(1575)=(A(1))**(2)/((A(149))**(2))*A(174)*A(525)
         A(1576)=A(150)*A(174)*(A(165))**(2)*(A(151))**(2)*(A(52))**(2)
     .+2*(A(2))**(2)*A(174)*(A(165))**(2)*(A(151))**(2)*(A(54))**(2)
         A(1577)=-2*(A(148))**(2)*(A(166))**(2)*(A(8))**(2)
         A(1578)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))*
     .*(2))*(A(1576)+A(1577))
         A(1579)=A(150)*A(174)*(A(165))**(2)*(A(151))**(2)*(A(44))**(2)
     .+2*(A(2))**(2)*A(174)*(A(165))**(2)*(A(151))**(2)*(A(46))**(2)
         A(1580)=-2*(A(148))**(2)*(A(166))**(2)*(A(9))**(2)
         A(1581)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))*
     .*(2))*(A(1579)+A(1580))
         A(1582)=2*(A(2))**(2)*A(174)*(A(165))**(2)*(A(151))**(2)*(A(80
     .))**(2)+3*A(174)*(A(165))**(2)*(A(151))**(2)*(A(78))**(2)
         A(1583)=-6*(A(148))**(2)*(A(166))**(2)*(A(11))**(2)-2*(A(2))**
     .(2)*A(174)*(A(165))**(2)*(A(151))**(2)*(A(78))**(2)
         A(1584)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))*
     .*(2))/DFLOAT((3))*(A(1582)+A(1583))
         A(1585)=A(175)*A(174)*(A(164))**(2)*(A(151))**(2)*(A(58))**(2)
     .-4*(A(2))**(2)*A(174)*(A(164))**(2)*(A(151))**(2)*(A(60))**(2)
         A(1586)=-6*(A(148))**(2)*(A(17))**(2)*(A(12))**(2)
         A(1587)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(164))*
     .*(2))/DFLOAT((3))*(A(1585)+A(1586))
         A(1588)=(A(1))**(2)/((A(149))**(2))*A(174)/DFLOAT((3))*A(568)
         A(1589)=(A(172))**(2)*A(168)*(A(151))**(2)*A(160)*A(70)*A(66)+
     .2*A(170)*A(172)*A(13)*A(10)*A(160)*A(72)*A(68)
         A(1590)=-4*A(166)*(A(164))**(3)*(A(13))**(2)*A(70)*A(66)-4*(A(
     .165))**(3)*A(17)*(A(10))**(2)*A(160)*A(70)*A(66)
         A(1591)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(1
     .72))**(2))/DFLOAT((4))*(A(1589)+A(1590))
         A(1592)=A(168)*(A(164))**(2)*(A(151))**(2)-A(165)*A(17)*(A(10)
     .)**(2)
         A(1593)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(158
     .)*A(74)*A(66)/((A(164))**(2))/DFLOAT((4))*A(1592)
         A(1594)=(A(172))**(2)*A(168)*(A(151))**(2)*A(159)*A(78)*A(66)+
     .2*A(170)*A(172)*A(10)*A(11)*A(159)*A(80)*A(68)
         A(1595)=-4*A(166)*(A(164))**(3)*(A(11))**(2)*A(78)*A(66)-4*(A(
     .165))**(3)*A(17)*(A(10))**(2)*A(159)*A(78)*A(66)
         A(1596)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(1
     .72))**(2))/DFLOAT((4))*(A(1594)+A(1595))
         A(1597)=(A(165))**(2)*A(168)*(A(151))**(2)-A(166)*A(164)*(A(8)
     .)**(2)
         A(1598)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(52)
     ./((A(165))**(2))/DFLOAT((4))*A(1597)
         A(1599)=(A(165))**(2)*A(168)*(A(151))**(2)-A(166)*A(164)*(A(9)
     .)**(2)
         A(1600)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(44)
     ./((A(165))**(2))/DFLOAT((4))*A(1599)
         A(1601)=(A(172))**(2)*A(168)*(A(151))**(2)*A(163)*A(70)*A(58)+
     .2*A(170)*A(172)*A(13)*A(12)*A(163)*A(72)*A(60)
         A(1602)=-4*A(166)*(A(164))**(3)*(A(13))**(2)*A(70)*A(58)-4*(A(
     .165))**(3)*A(17)*(A(12))**(2)*A(163)*A(70)*A(58)
         A(1603)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(1
     .72))**(2))/DFLOAT((4))*(A(1601)+A(1602))
         A(1604)=A(168)*(A(164))**(2)*(A(151))**(2)-A(165)*A(17)*(A(12)
     .)**(2)
         A(1605)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(161
     .)*A(74)*A(58)/((A(164))**(2))/DFLOAT((4))*A(1604)
         A(1606)=(A(172))**(2)*A(168)*(A(151))**(2)*A(162)*A(78)*A(58)+
     .2*A(170)*A(172)*A(11)*A(12)*A(162)*A(80)*A(60)
         A(1607)=-4*A(166)*(A(164))**(3)*(A(11))**(2)*A(78)*A(58)-4*(A(
     .165))**(3)*A(17)*(A(12))**(2)*A(162)*A(78)*A(58)
         A(1608)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(1
     .72))**(2))/DFLOAT((4))*(A(1606)+A(1607))
         A(1609)=(A(165))**(2)*A(168)*(A(151))**(2)*A(157)-A(166)*A(164
     .)*(A(13))**(2)
         A(1610)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(70)
     .*A(62)/((A(165))**(2))/DFLOAT((4))*A(1609)
         A(1611)=(A(165))**(2)*A(168)*(A(151))**(2)*A(156)-A(166)*A(164
     .)*(A(11))**(2)
         A(1612)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(78)
     .*A(62)/((A(165))**(2))/DFLOAT((4))*A(1611)
         A(1613)=2*(A(2))**(2)*(A(165))**(2)*(A(151))**(2)*(A(72))**(2)
     .+3*(A(165))**(2)*(A(151))**(2)*(A(70))**(2)
         A(1614)=-3*(A(148))**(2)*(A(13))**(2)-2*(A(2))**(2)*(A(165))**
     .(2)*(A(151))**(2)*(A(70))**(2)
         A(1615)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))*
     .*(2))*A(173)/DFLOAT((3))*(A(1613)+A(1614))
         A(1616)=A(175)*(A(164))**(2)*(A(151))**(2)*(A(66))**(2)-4*(A(2
     .))**(2)*(A(164))**(2)*(A(151))**(2)*(A(68))**(2)
         A(1617)=3*(A(148))**(2)*(A(10))**(2)
         A(1618)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))*A(173)/((A
     .(164))**(2))/DFLOAT((3))*(A(1616)+A(1617))
         A(1619)=2*(A(2))**(2)*(A(76))**(2)-2*(A(2))**(2)*(A(74))**(2)
         A(1620)=3*(A(74))**(2)
         A(1621)=(A(1))**(2)/((A(149))**(2))*A(173)/DFLOAT((3))*(A(1619
     .)+A(1620))
         A(1622)=(A(1))**(2)/((A(149))**(2))*A(173)*A(525)
         A(1623)=A(150)*(A(165))**(2)*(A(151))**(2)*(A(52))**(2)+2*(A(2
     .))**(2)*(A(165))**(2)*(A(151))**(2)*(A(54))**(2)
         A(1624)=-(A(148))**(2)*(A(8))**(2)
         A(1625)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))*
     .*(2))*A(173)*(A(1623)+A(1624))
         A(1626)=A(150)*(A(165))**(2)*(A(151))**(2)*(A(44))**(2)+2*(A(2
     .))**(2)*(A(165))**(2)*(A(151))**(2)*(A(46))**(2)
         A(1627)=-(A(148))**(2)*(A(9))**(2)
         A(1628)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))*
     .*(2))*A(173)*(A(1626)+A(1627))
         A(1629)=2*(A(2))**(2)*(A(165))**(2)*(A(151))**(2)*(A(80))**(2)
     .+3*(A(165))**(2)*(A(151))**(2)*(A(78))**(2)
         A(1630)=-3*(A(148))**(2)*(A(11))**(2)-2*(A(2))**(2)*(A(165))**
     .(2)*(A(151))**(2)*(A(78))**(2)
         A(1631)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))*
     .*(2))*A(173)/DFLOAT((3))*(A(1629)+A(1630))
         A(1632)=A(175)*(A(164))**(2)*(A(151))**(2)*(A(58))**(2)-4*(A(2
     .))**(2)*(A(164))**(2)*(A(151))**(2)*(A(60))**(2)
         A(1633)=3*(A(148))**(2)*(A(12))**(2)
         A(1634)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))*A(173)/((A
     .(164))**(2))/DFLOAT((3))*(A(1632)+A(1633))
         A(1635)=(A(1))**(2)/((A(149))**(2))*A(173)/DFLOAT((3))*A(568)
         A(1636)=2*(A(2))**(2)*A(171)*(A(172))**(2)*(A(151))**(2)*(A(72
     .))**(2)+24*(A(148))**(2)*(A(165))**(4)*(A(10))**(2)*(A(160))**(2)
     .*(A(70))**(2)
         A(1637)=24*(A(148))**(2)*(A(165))**(4)*(A(12))**(2)*(A(163))**
     .(2)*(A(70))**(2)+A(175)*A(171)*(A(172))**(2)*(A(151))**(2)*(A(70)
     .)**(2)
         A(1638)=24*(A(148))**(2)*(A(164))**(4)*(A(13))**(2)*(A(72))**(
     .2)
         A(1639)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(172))
     .**(2))/DFLOAT((3))*(A(1636)+A(1637)+A(1638))
         A(1640)=(A(10))**(2)*A(160)*A(158)+(A(12))**(2)*A(163)*A(161)
         A(1641)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(74)*A(70)
     ./((A(130))**(2))/DFLOAT((2))*A(1640)
         A(1642)=(A(10))**(2)*A(160)*A(159)+(A(12))**(2)*A(163)*A(162)
         A(1643)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(78)*A(70)
     ./((A(130))**(2))/DFLOAT((2))*A(1642)
         A(1644)=3*A(171)*(A(172))**(2)*(A(151))**(2)*(A(66))**(2)-2*(A
     .(2))**(2)*A(171)*(A(172))**(2)*(A(151))**(2)*(A(66))**(2)
         A(1645)=24*(A(148))**(2)*(A(165))**(4)*(A(10))**(2)*(A(68))**(
     .2)-4*(A(2))**(2)*A(171)*(A(172))**(2)*(A(151))**(2)*(A(68))**(2)
         A(1646)=24*(A(148))**(2)*(A(164))**(4)*(A(11))**(2)*(A(159))**
     .(2)*(A(66))**(2)+24*(A(148))**(2)*(A(164))**(4)*(A(13))**(2)*(A(1
     .60))**(2)*(A(66))**(2)
         A(1647)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(172))
     .**(2))/DFLOAT((3))*(A(1644)+A(1645)+A(1646))
         A(1648)=(A(11))**(2)*A(159)*A(162)+(A(13))**(2)*A(160)*A(163)
         A(1649)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(66)*A(58)
     .*(A(130))**(2)/DFLOAT((2))*A(1648)
         A(1650)=(A(11))**(2)*A(159)*A(156)+(A(13))**(2)*A(160)*A(157)
         A(1651)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(62)*A(66)
     .*(A(130))**(2)/DFLOAT((2))*A(1650)
         A(1652)=2*(A(2))**(2)*A(171)*(A(164))**(2)*(A(151))**(2)*(A(76
     .))**(2)+6*(A(148))**(2)*(A(165))**(2)*(A(10))**(2)*(A(158))**(2)*
     .(A(74))**(2)
         A(1653)=6*(A(148))**(2)*(A(165))**(2)*(A(12))**(2)*(A(161))**(
     .2)*(A(74))**(2)+A(175)*A(171)*(A(164))**(2)*(A(151))**(2)*(A(74))
     .**(2)
         A(1654)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(164))
     .**(2))/DFLOAT((3))*(A(1652)+A(1653))
         A(1655)=(A(10))**(2)*A(158)*A(159)+(A(12))**(2)*A(161)*A(162)
         A(1656)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(74)*A(78)
     ./((A(130))**(2))/DFLOAT((2))*A(1655)
         A(1657)=(A(48))**(2)-2*(A(2))**(2)*(A(50))**(2)
         A(1658)=(A(1))**(2)/((A(149))**(2))*A(171)*A(1657)
         A(1659)=A(171)*(A(165))**(2)*(A(151))**(2)*(A(52))**(2)-2*(A(2
     .))**(2)*A(171)*(A(165))**(2)*(A(151))**(2)*(A(54))**(2)
         A(1660)=-2*(A(148))**(2)*(A(164))**(2)*(A(8))**(2)*(A(54))**(2
     .)
         A(1661)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))*
     .*(2))*(A(1659)+A(1660))
         A(1662)=A(171)*(A(165))**(2)*(A(151))**(2)*(A(44))**(2)-2*(A(2
     .))**(2)*A(171)*(A(165))**(2)*(A(151))**(2)*(A(46))**(2)
         A(1663)=-2*(A(148))**(2)*(A(164))**(2)*(A(9))**(2)*(A(46))**(2
     .)
         A(1664)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))*
     .*(2))*(A(1662)+A(1663))
         A(1665)=A(150)*A(171)*(A(165))**(2)*(A(151))**(2)+2*(A(148))**
     .(2)*(A(164))**(2)*(A(8))**(2)
         A(1666)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))
     .**(2))*A(1665)
         A(1667)=A(150)*A(171)*(A(165))**(2)*(A(151))**(2)+2*(A(148))**
     .(2)*(A(164))**(2)*(A(9))**(2)
         A(1668)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))
     .**(2))*A(1667)
         A(1669)=2*(A(2))**(2)*A(171)*(A(172))**(2)*(A(151))**(2)*(A(80
     .))**(2)+24*(A(148))**(2)*(A(165))**(4)*(A(10))**(2)*(A(159))**(2)
     .*(A(78))**(2)
         A(1670)=24*(A(148))**(2)*(A(165))**(4)*(A(12))**(2)*(A(162))**
     .(2)*(A(78))**(2)+A(175)*A(171)*(A(172))**(2)*(A(151))**(2)*(A(78)
     .)**(2)
         A(1671)=24*(A(148))**(2)*(A(164))**(4)*(A(11))**(2)*(A(80))**(
     .2)
         A(1672)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(172))
     .**(2))/DFLOAT((3))*(A(1669)+A(1670)+A(1671))
         A(1673)=3*A(171)*(A(172))**(2)*(A(151))**(2)*(A(58))**(2)-2*(A
     .(2))**(2)*A(171)*(A(172))**(2)*(A(151))**(2)*(A(58))**(2)
         A(1674)=24*(A(148))**(2)*(A(165))**(4)*(A(12))**(2)*(A(60))**(
     .2)-4*(A(2))**(2)*A(171)*(A(172))**(2)*(A(151))**(2)*(A(60))**(2)
         A(1675)=24*(A(148))**(2)*(A(164))**(4)*(A(11))**(2)*(A(162))**
     .(2)*(A(58))**(2)+24*(A(148))**(2)*(A(164))**(4)*(A(13))**(2)*(A(1
     .63))**(2)*(A(58))**(2)
         A(1676)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(172))
     .**(2))/DFLOAT((3))*(A(1673)+A(1674)+A(1675))
         A(1677)=(A(11))**(2)*A(162)*A(156)+(A(13))**(2)*A(163)*A(157)
         A(1678)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(62)*A(58)
     .*(A(130))**(2)/DFLOAT((2))*A(1677)
         A(1679)=2*(A(2))**(2)*A(171)*(A(165))**(2)*(A(151))**(2)*(A(62
     .))**(2)-3*A(171)*(A(165))**(2)*(A(151))**(2)*(A(62))**(2)
         A(1680)=4*(A(2))**(2)*A(171)*(A(165))**(2)*(A(151))**(2)*(A(64
     .))**(2)-6*(A(148))**(2)*(A(164))**(2)*(A(11))**(2)*(A(156))**(2)*
     .(A(62))**(2)
         A(1681)=-6*(A(148))**(2)*(A(164))**(2)*(A(13))**(2)*(A(157))**
     .(2)*(A(62))**(2)
         A(1682)=(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))*
     .*(2))/DFLOAT((3))*(A(1679)+A(1680)+A(1681))
         A(1683)=A(171)*(A(172))**(2)*(A(151))**(2)*A(160)*A(70)*A(66)-
     .8*(A(165))**(2)*(A(164))**(2)*A(13)*A(10)*A(160)*A(72)*A(68)
         A(1684)=-4*(A(164))**(4)*(A(13))**(2)*A(70)*A(66)-4*(A(165))**
     .(4)*(A(10))**(2)*A(160)*A(70)*A(66)
         A(1685)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(1
     .72))**(2))/DFLOAT((4))*(A(1683)+A(1684))
         A(1686)=A(171)*(A(164))**(2)*(A(151))**(2)-(A(165))**(2)*(A(10
     .))**(2)
         A(1687)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(158
     .)*A(74)*A(66)/((A(164))**(2))/DFLOAT((4))*A(1686)
         A(1688)=A(171)*(A(172))**(2)*(A(151))**(2)*A(159)*A(78)*A(66)-
     .8*(A(165))**(2)*(A(164))**(2)*A(10)*A(11)*A(159)*A(80)*A(68)
         A(1689)=-4*(A(164))**(4)*(A(11))**(2)*A(78)*A(66)-4*(A(165))**
     .(4)*(A(10))**(2)*A(159)*A(78)*A(66)
         A(1690)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(1
     .72))**(2))/DFLOAT((4))*(A(1688)+A(1689))
         A(1691)=A(171)*(A(165))**(2)*(A(151))**(2)-(A(164))**(2)*(A(8)
     .)**(2)
         A(1692)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(52)
     ./((A(165))**(2))/DFLOAT((4))*A(1691)
         A(1693)=A(171)*(A(165))**(2)*(A(151))**(2)-(A(164))**(2)*(A(9)
     .)**(2)
         A(1694)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(44)
     ./((A(165))**(2))/DFLOAT((4))*A(1693)
         A(1695)=A(171)*(A(172))**(2)*(A(151))**(2)*A(163)*A(70)*A(58)-
     .8*(A(165))**(2)*(A(164))**(2)*A(13)*A(12)*A(163)*A(72)*A(60)
         A(1696)=-4*(A(164))**(4)*(A(13))**(2)*A(70)*A(58)-4*(A(165))**
     .(4)*(A(12))**(2)*A(163)*A(70)*A(58)
         A(1697)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(1
     .72))**(2))/DFLOAT((4))*(A(1695)+A(1696))
         A(1698)=A(171)*(A(164))**(2)*(A(151))**(2)-(A(165))**(2)*(A(12
     .))**(2)
         A(1699)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(161
     .)*A(74)*A(58)/((A(164))**(2))/DFLOAT((4))*A(1698)
         A(1700)=A(171)*(A(172))**(2)*(A(151))**(2)*A(162)*A(78)*A(58)-
     .8*(A(165))**(2)*(A(164))**(2)*A(11)*A(12)*A(162)*A(80)*A(60)
         A(1701)=-4*(A(164))**(4)*(A(11))**(2)*A(78)*A(58)-4*(A(165))**
     .(4)*(A(12))**(2)*A(162)*A(78)*A(58)
         A(1702)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(1
     .72))**(2))/DFLOAT((4))*(A(1700)+A(1701))
         A(1703)=A(171)*(A(165))**(2)*(A(151))**(2)*A(157)-(A(164))**(2
     .)*(A(13))**(2)
         A(1704)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(70)
     .*A(62)/((A(165))**(2))/DFLOAT((4))*A(1703)
         A(1705)=A(171)*(A(165))**(2)*(A(151))**(2)*A(156)-(A(164))**(2
     .)*(A(11))**(2)
         A(1706)=(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(78)
     .*A(62)/((A(165))**(2))/DFLOAT((4))*A(1705)
         A(1707)=A(170)*(A(172))**(2)*(A(151))**(2)*A(160)*A(70)*A(66)-
     .2*A(172)*A(168)*A(13)*A(10)*A(160)*A(72)*A(68)
         A(1708)=4*A(17)*(A(164))**(3)*(A(13))**(2)*A(70)*A(66)-4*A(166
     .)*(A(165))**(3)*(A(10))**(2)*A(160)*A(70)*A(66)
         A(1709)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(
     .172))**(2))/DFLOAT((4))*(A(1707)+A(1708))
         A(1710)=A(170)*(A(164))**(2)*(A(151))**(2)-A(166)*A(165)*(A(10
     .))**(2)
         A(1711)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(15
     .8)*A(74)*A(66)/((A(164))**(2))/DFLOAT((4))*A(1710)
         A(1712)=A(170)*(A(172))**(2)*(A(151))**(2)*A(159)*A(78)*A(66)-
     .2*A(172)*A(168)*A(10)*A(11)*A(159)*A(80)*A(68)
         A(1713)=4*A(17)*(A(164))**(3)*(A(11))**(2)*A(78)*A(66)-4*A(166
     .)*(A(165))**(3)*(A(10))**(2)*A(159)*A(78)*A(66)
         A(1714)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(
     .172))**(2))/DFLOAT((4))*(A(1712)+A(1713))
         A(1715)=A(170)*(A(165))**(2)*(A(151))**(2)+A(17)*A(164)*(A(8))
     .**(2)
         A(1716)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(52
     .)/((A(165))**(2))/DFLOAT((4))*A(1715)
         A(1717)=A(170)*(A(165))**(2)*(A(151))**(2)+A(17)*A(164)*(A(9))
     .**(2)
         A(1718)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(44
     .)/((A(165))**(2))/DFLOAT((4))*A(1717)
         A(1719)=A(170)*(A(172))**(2)*(A(151))**(2)*A(163)*A(70)*A(58)-
     .2*A(172)*A(168)*A(13)*A(12)*A(163)*A(72)*A(60)
         A(1720)=4*A(17)*(A(164))**(3)*(A(13))**(2)*A(70)*A(58)-4*A(166
     .)*(A(165))**(3)*(A(12))**(2)*A(163)*A(70)*A(58)
         A(1721)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(
     .172))**(2))/DFLOAT((4))*(A(1719)+A(1720))
         A(1722)=A(170)*(A(164))**(2)*(A(151))**(2)-A(166)*A(165)*(A(12
     .))**(2)
         A(1723)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(16
     .1)*A(74)*A(58)/((A(164))**(2))/DFLOAT((4))*A(1722)
         A(1724)=A(170)*(A(172))**(2)*(A(151))**(2)*A(162)*A(78)*A(58)-
     .2*A(172)*A(168)*A(11)*A(12)*A(162)*A(80)*A(60)
         A(1725)=4*A(17)*(A(164))**(3)*(A(11))**(2)*A(78)*A(58)-4*A(166
     .)*(A(165))**(3)*(A(12))**(2)*A(162)*A(78)*A(58)
         A(1726)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(
     .172))**(2))/DFLOAT((4))*(A(1724)+A(1725))
         A(1727)=A(170)*(A(165))**(2)*(A(151))**(2)*A(157)+A(17)*A(164)
     .*(A(13))**(2)
         A(1728)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(70
     .)*A(62)/((A(165))**(2))/DFLOAT((4))*A(1727)
         A(1729)=A(170)*(A(165))**(2)*(A(151))**(2)*A(156)+A(17)*A(164)
     .*(A(11))**(2)
         A(1730)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(78
     .)*A(62)/((A(165))**(2))/DFLOAT((4))*A(1729)
         A(1731)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(
     .172))**(2))/DFLOAT((4))*(A(1683)+A(1684))
         A(1732)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(
     .172))**(2))/DFLOAT((4))*(A(1695)+A(1696))
         A(1733)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(70
     .)*A(62)/((A(165))**(2))/DFLOAT((4))*A(1703)
         A(1734)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(15
     .8)*A(74)*A(66)/((A(164))**(2))/DFLOAT((4))*A(1686)
         A(1735)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(16
     .1)*A(74)*A(58)/((A(164))**(2))/DFLOAT((4))*A(1698)
         A(1736)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(52
     .)/((A(165))**(2))/DFLOAT((4))*A(1691)
         A(1737)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(44
     .)/((A(165))**(2))/DFLOAT((4))*A(1693)
         A(1738)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(
     .172))**(2))/DFLOAT((4))*(A(1688)+A(1689))
         A(1739)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)/((A(
     .172))**(2))/DFLOAT((4))*(A(1700)+A(1701))
         A(1740)=-(A(1))**(2)/((A(151))**(2))/((A(2))**(2))*A(147)*A(78
     .)*A(62)/((A(165))**(2))/DFLOAT((4))*A(1705)
         A(1741)=2*(A(2))**(2)*A(171)*(A(165))**(2)*(A(151))**(2)*(A(72
     .))**(2)-2*(A(2))**(2)*A(171)*(A(165))**(2)*(A(151))**(2)*(A(70))*
     .*(2)
         A(1742)=3*A(171)*(A(165))**(2)*(A(151))**(2)*(A(70))**(2)+6*(A
     .(148))**(2)*(A(164))**(2)*(A(13))**(2)
         A(1743)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))
     .**(2))/DFLOAT((3))*(A(1741)+A(1742))
         A(1744)=A(175)*A(171)*(A(164))**(2)*(A(151))**(2)*(A(66))**(2)
     .-4*(A(2))**(2)*A(171)*(A(164))**(2)*(A(151))**(2)*(A(68))**(2)
         A(1745)=6*(A(148))**(2)*(A(165))**(2)*(A(10))**(2)
         A(1746)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(164))
     .**(2))/DFLOAT((3))*(A(1744)+A(1745))
         A(1747)=-(A(1))**(2)/((A(149))**(2))*A(171)/DFLOAT((3))*(A(522
     .)+A(523))
         A(1748)=-(A(1))**(2)/((A(149))**(2))*A(171)*A(525)
         A(1749)=A(150)*A(171)*(A(165))**(2)*(A(151))**(2)*(A(52))**(2)
     .+2*(A(2))**(2)*A(171)*(A(165))**(2)*(A(151))**(2)*(A(54))**(2)
         A(1750)=2*(A(148))**(2)*(A(164))**(2)*(A(8))**(2)
         A(1751)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))
     .**(2))*(A(1749)+A(1750))
         A(1752)=A(150)*A(171)*(A(165))**(2)*(A(151))**(2)*(A(44))**(2)
     .+2*(A(2))**(2)*A(171)*(A(165))**(2)*(A(151))**(2)*(A(46))**(2)
         A(1753)=2*(A(148))**(2)*(A(164))**(2)*(A(9))**(2)
         A(1754)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))
     .**(2))*(A(1752)+A(1753))
         A(1755)=2*(A(2))**(2)*A(171)*(A(165))**(2)*(A(151))**(2)*(A(80
     .))**(2)-2*(A(2))**(2)*A(171)*(A(165))**(2)*(A(151))**(2)*(A(78))*
     .*(2)
         A(1756)=3*A(171)*(A(165))**(2)*(A(151))**(2)*(A(78))**(2)+6*(A
     .(148))**(2)*(A(164))**(2)*(A(11))**(2)
         A(1757)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))
     .**(2))/DFLOAT((3))*(A(1755)+A(1756))
         A(1758)=A(175)*A(171)*(A(164))**(2)*(A(151))**(2)*(A(58))**(2)
     .-4*(A(2))**(2)*A(171)*(A(164))**(2)*(A(151))**(2)*(A(60))**(2)
         A(1759)=6*(A(148))**(2)*(A(165))**(2)*(A(12))**(2)
         A(1760)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(164))
     .**(2))/DFLOAT((3))*(A(1758)+A(1759))
         A(1761)=4*(A(2))**(2)*(A(62))**(2)-3*(A(62))**(2)
         A(1762)=-4*(A(2))**(2)*(A(64))**(2)
         A(1763)=-(A(1))**(2)/((A(149))**(2))*A(171)/DFLOAT((3))*(A(176
     .1)+A(1762))
         A(1764)=3*A(175)*(A(70))**(2)-4*(A(2))**(4)
         A(1765)=-2*(A(1))**(2)/((A(149))**(2))/DFLOAT((9))*A(1764)
         A(1766)=24*(A(2))**(2)*(A(66))**(2)-9*(A(66))**(2)
         A(1767)=-16*(A(2))**(4)
         A(1768)=-2*(A(1))**(2)/((A(149))**(2))/DFLOAT((9))*(A(1766)+A(
     .1767))
         A(1769)=8*(A(2))**(2)-3
         A(1770)=3*A(175)*(A(74))**(2)-4*(A(2))**(4)
         A(1771)=-2*(A(1))**(2)/((A(149))**(2))/DFLOAT((9))*A(1770)
         A(1772)=4*(A(2))**(2)*(A(48))**(2)-(A(48))**(2)
         A(1773)=-4*(A(2))**(4)
         A(1774)=-2*(A(1))**(2)/((A(149))**(2))*(A(1772)+A(1773))
         A(1775)=4*(A(2))**(2)-1
         A(1776)=A(1775)*(A(52))**(2)-4*(A(2))**(4)
         A(1777)=-2*(A(1))**(2)/((A(149))**(2))*A(1776)
         A(1778)=A(1775)*(A(44))**(2)-4*(A(2))**(4)
         A(1779)=-2*(A(1))**(2)/((A(149))**(2))*A(1778)
         A(1780)=3*A(175)*(A(78))**(2)-4*(A(2))**(4)
         A(1781)=-2*(A(1))**(2)/((A(149))**(2))/DFLOAT((9))*A(1780)
         A(1782)=3*A(1769)*(A(58))**(2)-16*(A(2))**(4)
         A(1783)=-2*(A(1))**(2)/((A(149))**(2))/DFLOAT((9))*A(1782)
         A(1784)=3*A(1769)*(A(62))**(2)-16*(A(2))**(4)
         A(1785)=-2*(A(1))**(2)/((A(149))**(2))/DFLOAT((9))*A(1784)
         A(1786)=6*(A(148))**(2)*(A(17))**(2)*(A(13))**(2)-2*(A(2))**(2
     .)*A(174)*(A(165))**(2)*(A(151))**(2)*(A(70))**(2)
         A(1787)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))
     .**(2))/DFLOAT((3))*(A(1568)+A(1786))
         A(1788)=6*(A(148))**(2)*(A(166))**(2)*(A(10))**(2)
         A(1789)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(164))
     .**(2))/DFLOAT((3))*(A(1571)+A(1788))
         A(1790)=-(A(1))**(2)/((A(149))**(2))*A(174)/DFLOAT((3))*(A(522
     .)+A(523))
         A(1791)=-(A(1))**(2)/((A(149))**(2))*A(174)*A(525)
         A(1792)=2*(A(148))**(2)*(A(17))**(2)*(A(8))**(2)
         A(1793)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))
     .**(2))*(A(1576)+A(1792))
         A(1794)=2*(A(148))**(2)*(A(17))**(2)*(A(9))**(2)
         A(1795)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))
     .**(2))*(A(1579)+A(1794))
         A(1796)=6*(A(148))**(2)*(A(17))**(2)*(A(11))**(2)-2*(A(2))**(2
     .)*A(174)*(A(165))**(2)*(A(151))**(2)*(A(78))**(2)
         A(1797)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(165))
     .**(2))/DFLOAT((3))*(A(1582)+A(1796))
         A(1798)=6*(A(148))**(2)*(A(166))**(2)*(A(12))**(2)
         A(1799)=-(A(1))**(2)/((A(151))**(2))/((A(149))**(2))/((A(164))
     .**(2))/DFLOAT((3))*(A(1585)+A(1798))
         A(1800)=-(A(1))**(2)/((A(149))**(2))*A(174)/DFLOAT((3))*A(568)
      RECALC=.FALSE.
      ENDIF
      RETURN
      END

      SUBROUTINE VINI
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION SQRTS
      COMMON/VARS/A(1800)
      COMMON/SQS/SQRTS
      COMMON/LOGG/L(1820)
      LOGICAL GWIDTH,RWIDTH
      COMMON/WDTH/ GWIDTH,RWIDTH
      SAVE
      GWIDTH=.FALSE.
      RWIDTH=.FALSE.
      SQRTS=   0.000000D+00
      A(1)=   3.133300D-01
      A(2)=   4.740000D-01
      A(3)=   2.210000D-01
      A(4)=   4.000000D-02
      A(5)=   3.500000D-03
      A(6)=   9.118700D+01
      A(7)=   3.000000D+02
      A(8)=   1.057000D-01
      A(9)=   1.777000D+00
      A(10)=   1.300000D+00
      A(11)=   2.000000D-01
      A(12)=   1.750000D+02
      A(13)=   4.300000D+00
      A(14)=   1.590000D+00
      A(15)=   2.502000D+00
      A(16)=   2.094000D+00
      A(17)=   5.000000D-01
      A(18)=   1.000000D+03
      A(19)=   0.000000D+00
      A(20)=   0.000000D+00
      A(21)=   0.000000D+00
      A(22)=   0.000000D+00
      A(23)=   0.000000D+00
      A(24)=   0.000000D+00
      A(25)=   0.000000D+00
      A(26)=   0.000000D+00
      A(27)=   0.000000D+00
      A(28)=   0.000000D+00
      A(29)=   0.000000D+00
      A(30)=   0.000000D+00
      A(31)=   0.000000D+00
      A(32)=   0.000000D+00
      A(33)=   0.000000D+00
      A(34)=   0.000000D+00
      A(35)=   0.000000D+00
      A(36)=   0.000000D+00
      A(37)=   0.000000D+00
      A(38)=   0.000000D+00
      A(39)=   0.000000D+00
      A(40)=   0.000000D+00
      A(41)=   0.000000D+00
      A(42)=   0.000000D+00
      A(43)=   1.000000D+03
      A(44)=   0.000000D+00
      A(45)=   0.000000D+00
      A(46)=   0.000000D+00
      A(47)=   0.000000D+00
      A(48)=   0.000000D+00
      A(49)=   0.000000D+00
      A(50)=   0.000000D+00
      A(51)=   0.000000D+00
      A(52)=   0.000000D+00
      A(53)=   0.000000D+00
      A(54)=   0.000000D+00
      A(55)=   0.000000D+00
      A(56)=   1.000000D+03
      A(57)=   1.000000D+03
      A(58)=   0.000000D+00
      A(59)=   0.000000D+00
      A(60)=   0.000000D+00
      A(61)=   0.000000D+00
      A(62)=   0.000000D+00
      A(63)=   0.000000D+00
      A(64)=   0.000000D+00
      A(65)=   0.000000D+00
      A(66)=   0.000000D+00
      A(67)=   0.000000D+00
      A(68)=   0.000000D+00
      A(69)=   0.000000D+00
      A(70)=   0.000000D+00
      A(71)=   0.000000D+00
      A(72)=   0.000000D+00
      A(73)=   0.000000D+00
      A(74)=   0.000000D+00
      A(75)=   0.000000D+00
      A(76)=   0.000000D+00
      A(77)=   0.000000D+00
      A(78)=   0.000000D+00
      A(79)=   0.000000D+00
      A(80)=   0.000000D+00
      A(81)=   0.000000D+00
      A(82)=   1.000000D+02
      A(83)=   1.000000D+00
      A(84)=   1.000000D+02
      A(85)=   1.000000D+00
      A(86)=   1.000000D+00
      A(87)=   1.000000D+02
      A(88)=   1.000000D+00
      A(89)=   1.000000D+02
      A(90)=   1.000000D+00
      A(91)=   1.000000D+03
      A(92)=   1.000000D+00
      A(93)=   1.000000D+02
      A(94)=   1.000000D+00
      A(95)=   1.000000D+02
      A(96)=   1.000000D+00
      A(97)=   1.000000D+03
      A(98)=   1.000000D+00
      A(99)=   1.000000D+03
      A(100)=   1.000000D+00
      A(101)=   1.000000D+01
      A(102)=   1.000000D+00
      A(103)=   1.000000D+00
      A(104)=   1.000000D+00
      A(105)=   1.000000D+00
      A(106)=   1.000000D+02
      A(107)=   1.000000D+00
      A(108)=   1.000000D+03
      A(109)=   1.000000D+00
      A(110)=   1.000000D+00
      A(111)=   1.000000D+00
      A(112)=   1.000000D+03
      A(113)=   1.000000D+00
      A(114)=   1.000000D+00
      A(115)=   1.000000D+00
      A(116)=   1.000000D+00
      A(117)=   1.000000D+00
      A(118)=   1.000000D+00
      A(119)=   1.000000D+00
      A(120)=   1.000000D+00
      A(121)=   1.000000D+00
      A(122)=   1.000000D+02
      A(123)=   1.000000D+00
      A(124)=   1.000000D+03
      A(125)=   1.000000D+00
      A(126)=   1.000000D+02
      A(127)=   1.000000D+00
      A(128)=   1.000000D+03
      A(129)=   1.000000D+00
      A(130)=   1.000000D+01
      A(131)=   5.000000D+02
      A(132)=   1.000000D+02
      A(133)=   1.000000D+03
      A(134)=   1.000000D+02
      A(135)=   1.000000D+03
      A(136)=   1.000000D+03
      A(137)=   1.000000D+03
      A(138)=   1.000000D+03
      A(139)=   1.000000D+03
      A(140)=   1.000000D+03
      A(141)=   1.000000D+03
      A(142)=   1.000000D+03
      A(143)=   1.000000D+03
      A(144)=   1.000000D+03
      A(145)=   1.000000D+03
      A(146)=   1.117000D+00
      A(147)=   1.414214D+00
      A(148)=   8.805248D-01
      A(149)=   8.347376D-01
      A(150)=   5.506480D-01
      A(151)=   8.029242D+01
      A(152)=   9.752738D-01
      A(153)=   9.991997D-01
      A(154)=   9.999939D-01
      A(155)=   9.752678D-01
      A(156)=   2.209986D-01
      A(157)=   3.500000D-03
      A(158)=  -2.209597D-01
      A(159)=   9.744623D-01
      A(160)=   3.999975D-02
      A(161)=   5.429274D-03
      A(162)=  -3.978383D-02
      A(163)=   9.991936D-01
      A(164)=   9.950372D-01
      A(165)=   9.950372D-02
      A(166)=   8.660254D-01
      A(167)=   9.114793D-01
      A(168)=  -8.119756D-01
      A(169)=  -4.113458D-01
      A(170)=   5.836913D-01
      A(171)=  -9.801980D-01
      A(172)=   1.980198D-01
      A(173)=   8.660254D-01
      A(174)=   5.000000D-01
      A(175)=  -2.101296D+00
      A(176)=  -2.101296D+00
      A(177)=   4.270297D+00
      A(178)=   4.270297D+00
      A(179)=  -4.244554D+00
      A(180)=  -4.244554D+00
      A(181)=   5.990099D+00
      A(182)=   5.990099D+00
      A(183)=  -2.524752D+00
      A(184)=  -2.524752D+00
      A(185)=   0.000000D+00
      A(186)=   0.000000D+00
      A(187)=   0.000000D+00
      A(188)=   0.000000D+00
      A(189)=   0.000000D+00
      A(190)=   0.000000D+00
      A(191)=   0.000000D+00
      A(192)=   0.000000D+00
      A(193)=   0.000000D+00
      A(194)=   0.000000D+00
      A(195)=   0.000000D+00
      A(196)=   0.000000D+00
      A(197)=   0.000000D+00
      A(198)=   0.000000D+00
      A(199)=   0.000000D+00
      A(200)=   0.000000D+00
      A(201)=   0.000000D+00
      A(202)=   0.000000D+00
      A(203)=  -0.000000D+00
      A(204)=  -0.000000D+00
      A(205)=   0.000000D+00
      A(206)=   0.000000D+00
      A(207)=   0.000000D+00
      A(208)=   0.000000D+00
      A(209)=   0.000000D+00
      A(210)=   0.000000D+00
      A(211)=   0.000000D+00
      A(212)=   0.000000D+00
      A(213)=   0.000000D+00
      A(214)=   0.000000D+00
      A(215)=  -0.000000D+00
      A(216)=  -0.000000D+00
      A(217)=   0.000000D+00
      A(218)=   0.000000D+00
      A(219)=   0.000000D+00
      A(220)=   0.000000D+00
      A(221)=  -0.000000D+00
      A(222)=   0.000000D+00
      A(223)=   0.000000D+00
      A(224)=   0.000000D+00
      A(225)=   0.000000D+00
      A(226)=   0.000000D+00
      A(227)=  -0.000000D+00
      A(228)=   0.000000D+00
      A(229)=   0.000000D+00
      A(230)=   0.000000D+00
      A(231)=   0.000000D+00
      A(232)=   0.000000D+00
      A(233)=  -0.000000D+00
      A(234)=   0.000000D+00
      A(235)=   0.000000D+00
      A(236)=   0.000000D+00
      A(237)=   0.000000D+00
      A(238)=   0.000000D+00
      A(239)=  -0.000000D+00
      A(240)=   0.000000D+00
      A(241)=   0.000000D+00
      A(242)=   0.000000D+00
      A(243)=   0.000000D+00
      A(244)=   0.000000D+00
      A(245)=  -0.000000D+00
      A(246)=   0.000000D+00
      A(247)=   0.000000D+00
      A(248)=   0.000000D+00
      A(249)=   0.000000D+00
      A(250)=   0.000000D+00
      A(251)=  -0.000000D+00
      A(252)=   0.000000D+00
      A(253)=   4.244554D+00
      A(254)=   4.244554D+00
      A(255)=   3.000000D+00
      A(256)=   3.000000D+00
      A(257)=  -1.202592D+00
      A(258)=  -1.202592D+00
      A(259)=   2.108911D-01
      A(260)=   2.108911D-01
      A(261)=   1.851485D-01
      A(262)=   1.851485D-01
      A(263)=   0.000000D+00
      A(264)=   0.000000D+00
      A(265)=   0.000000D+00
      A(266)=   0.000000D+00
      A(267)=   0.000000D+00
      A(268)=   0.000000D+00
      A(269)=   0.000000D+00
      A(270)=   0.000000D+00
      A(271)=   0.000000D+00
      A(272)=   0.000000D+00
      A(273)=   0.000000D+00
      A(274)=   0.000000D+00
      A(275)=   0.000000D+00
      A(276)=   0.000000D+00
      A(277)=   0.000000D+00
      A(278)=   0.000000D+00
      A(279)=   0.000000D+00
      A(280)=   0.000000D+00
      A(281)=   0.000000D+00
      A(282)=   0.000000D+00
      A(283)=   0.000000D+00
      A(284)=   0.000000D+00
      A(285)=   0.000000D+00
      A(286)=   0.000000D+00
      A(287)=   0.000000D+00
      A(288)=   0.000000D+00
      A(289)=   0.000000D+00
      A(290)=   0.000000D+00
      A(291)=   0.000000D+00
      A(292)=   0.000000D+00
      A(293)=   0.000000D+00
      A(294)=   0.000000D+00
      A(295)=   0.000000D+00
      A(296)=  -0.000000D+00
      A(297)=   0.000000D+00
      A(298)=   0.000000D+00
      A(299)=  -0.000000D+00
      A(300)=   0.000000D+00
      A(301)=   0.000000D+00
      A(302)=  -0.000000D+00
      A(303)=   0.000000D+00
      A(304)=   0.000000D+00
      A(305)=  -0.000000D+00
      A(306)=   0.000000D+00
      A(307)=   0.000000D+00
      A(308)=  -0.000000D+00
      A(309)=   0.000000D+00
      A(310)=   0.000000D+00
      A(311)=  -0.000000D+00
      A(312)=   0.000000D+00
      A(313)=   0.000000D+00
      A(314)=  -0.000000D+00
      A(315)=   0.000000D+00
      A(316)=   0.000000D+00
      A(317)=  -0.000000D+00
      A(318)=   0.000000D+00
      A(319)=   0.000000D+00
      A(320)=  -0.000000D+00
      A(321)=   0.000000D+00
      A(322)=   0.000000D+00
      A(323)=  -0.000000D+00
      A(324)=   0.000000D+00
      A(325)=   0.000000D+00
      A(326)=  -0.000000D+00
      A(327)=   0.000000D+00
      A(328)=   0.000000D+00
      A(329)=  -0.000000D+00
      A(330)=   0.000000D+00
      A(331)=   0.000000D+00
      A(332)=   0.000000D+00
      A(333)=   0.000000D+00
      A(334)=   0.000000D+00
      A(335)=   0.000000D+00
      A(336)=   0.000000D+00
      A(337)=   0.000000D+00
      A(338)=   0.000000D+00
      A(339)=   0.000000D+00
      A(340)=   0.000000D+00
      A(341)=   0.000000D+00
      A(342)=   0.000000D+00
      A(343)=   0.000000D+00
      A(344)=   0.000000D+00
      A(345)=   0.000000D+00
      A(346)=   0.000000D+00
      A(347)=   0.000000D+00
      A(348)=   0.000000D+00
      A(349)=   0.000000D+00
      A(350)=   0.000000D+00
      A(351)=   0.000000D+00
      A(352)=   0.000000D+00
      A(353)=   0.000000D+00
      A(354)=   0.000000D+00
      A(355)=   0.000000D+00
      A(356)=   0.000000D+00
      A(357)=   0.000000D+00
      A(358)=   0.000000D+00
      A(359)=   0.000000D+00
      A(360)=   0.000000D+00
      A(361)=   0.000000D+00
      A(362)=   0.000000D+00
      A(363)=   0.000000D+00
      A(364)=   0.000000D+00
      A(365)=   0.000000D+00
      A(366)=   0.000000D+00
      A(367)=   0.000000D+00
      A(368)=   0.000000D+00
      A(369)=   0.000000D+00
      A(370)=   0.000000D+00
      A(371)=   0.000000D+00
      A(372)=   0.000000D+00
      A(373)=   0.000000D+00
      A(374)=   0.000000D+00
      A(375)=   0.000000D+00
      A(376)=   0.000000D+00
      A(377)=   0.000000D+00
      A(378)=   0.000000D+00
      A(379)=   1.000000D-00
      A(380)=   1.000000D-00
      A(381)=  -1.012960D-01
      A(382)=  -1.012960D-01
      A(383)=   0.000000D+00
      A(384)=   0.000000D+00
      A(385)=   0.000000D+00
      A(386)=   0.000000D+00
      A(387)=   0.000000D+00
      A(388)=   0.000000D+00
      A(389)=   0.000000D+00
      A(390)=   0.000000D+00
      A(391)=   0.000000D+00
      A(392)=   0.000000D+00
      A(393)=   0.000000D+00
      A(394)=   0.000000D+00
      A(395)=   0.000000D+00
      A(396)=   0.000000D+00
      A(397)=   0.000000D+00
      A(398)=   0.000000D+00
      A(399)=   0.000000D+00
      A(400)=   0.000000D+00
      A(401)=   0.000000D+00
      A(402)=   0.000000D+00
      A(403)=   0.000000D+00
      A(404)=   0.000000D+00
      A(405)=   0.000000D+00
      A(406)=   0.000000D+00
      A(407)=   0.000000D+00
      A(408)=   0.000000D+00
      A(409)=  -0.000000D+00
      A(410)=  -0.000000D+00
      A(411)=   0.000000D+00
      A(412)=   0.000000D+00
      A(413)=  -0.000000D+00
      A(414)=  -0.000000D+00
      A(415)=   0.000000D+00
      A(416)=   0.000000D+00
      A(417)=   0.000000D+00
      A(418)=   0.000000D+00
      A(419)=  -0.000000D+00
      A(420)=   0.000000D+00
      A(421)=   0.000000D+00
      A(422)=   0.000000D+00
      A(423)=   0.000000D+00
      A(424)=   0.000000D+00
      A(425)=  -0.000000D+00
      A(426)=   0.000000D+00
      A(427)=   0.000000D+00
      A(428)=   0.000000D+00
      A(429)=   0.000000D+00
      A(430)=   0.000000D+00
      A(431)=  -0.000000D+00
      A(432)=   0.000000D+00
      A(433)=   0.000000D+00
      A(434)=   0.000000D+00
      A(435)=   0.000000D+00
      A(436)=   0.000000D+00
      A(437)=  -0.000000D+00
      A(438)=   0.000000D+00
      A(439)=   0.000000D+00
      A(440)=   0.000000D+00
      A(441)=   0.000000D+00
      A(442)=   0.000000D+00
      A(443)=  -0.000000D+00
      A(444)=   0.000000D+00
      A(445)=   0.000000D+00
      A(446)=   0.000000D+00
      A(447)=   0.000000D+00
      A(448)=   0.000000D+00
      A(449)=  -0.000000D+00
      A(450)=   0.000000D+00
      A(451)=   0.000000D+00
      A(452)=   0.000000D+00
      A(453)=  -0.000000D+00
      A(454)=  -0.000000D+00
      A(455)=   0.000000D+00
      A(456)=   0.000000D+00
      A(457)=  -0.000000D+00
      A(458)=  -0.000000D+00
      A(459)=   0.000000D+00
      A(460)=   0.000000D+00
      A(461)=   0.000000D+00
      A(462)=   0.000000D+00
      A(463)=  -0.000000D+00
      A(464)=   0.000000D+00
      A(465)=   0.000000D+00
      A(466)=   0.000000D+00
      A(467)=   0.000000D+00
      A(468)=   0.000000D+00
      A(469)=  -0.000000D+00
      A(470)=   0.000000D+00
      A(471)=   0.000000D+00
      A(472)=   0.000000D+00
      A(473)=   0.000000D+00
      A(474)=   0.000000D+00
      A(475)=  -0.000000D+00
      A(476)=   0.000000D+00
      A(477)=   0.000000D+00
      A(478)=   0.000000D+00
      A(479)=   0.000000D+00
      A(480)=   0.000000D+00
      A(481)=  -0.000000D+00
      A(482)=   0.000000D+00
      A(483)=   0.000000D+00
      A(484)=   0.000000D+00
      A(485)=   0.000000D+00
      A(486)=   0.000000D+00
      A(487)=  -0.000000D+00
      A(488)=   0.000000D+00
      A(489)=   0.000000D+00
      A(490)=   0.000000D+00
      A(491)=   0.000000D+00
      A(492)=   0.000000D+00
      A(493)=  -0.000000D+00
      A(494)=   0.000000D+00
      A(495)=   6.049953D-01
      A(496)=  -2.070794D+01
      A(497)=  -6.571952D-02
      A(498)=   2.249465D+00
      A(499)=  -1.698229D+00
      A(500)=   5.812743D+01
      A(501)=   0.000000D+00
      A(502)=   0.000000D+00
      A(503)=  -7.449069D+01
      A(504)=  -1.324882D+00
      A(505)=   0.000000D+00
      A(506)=   0.000000D+00
      A(507)=   0.000000D+00
      A(508)=   0.000000D+00
      A(509)=   0.000000D+00
      A(510)=   0.000000D+00
      A(511)=   0.000000D+00
      A(512)=   0.000000D+00
      A(513)=   0.000000D+00
      A(514)=  -0.000000D+00
      A(515)=   3.930893D+00
      A(516)=   6.991438D-03
      A(517)=  -0.000000D+00
      A(518)=  -0.000000D+00
      A(519)=  -0.000000D+00
      A(520)=  -0.000000D+00
      A(521)=  -0.000000D+00
      A(522)=   0.000000D+00
      A(523)=  -0.000000D+00
      A(524)=   0.000000D+00
      A(525)=   0.000000D+00
      A(526)=   0.000000D+00
      A(527)=   0.000000D+00
      A(528)=  -1.500354D-02
      A(529)=  -8.005536D-04
      A(530)=   0.000000D+00
      A(531)=   0.000000D+00
      A(532)=   0.000000D+00
      A(533)=   0.000000D+00
      A(534)=   0.000000D+00
      A(535)=  -4.240516D+00
      A(536)=  -2.262639D-01
      A(537)=   0.000000D+00
      A(538)=   0.000000D+00
      A(539)=   0.000000D+00
      A(540)=   0.000000D+00
      A(541)=   0.000000D+00
      A(542)=   0.000000D+00
      A(543)=   0.000000D+00
      A(544)=   0.000000D+00
      A(545)=   0.000000D+00
      A(546)=   0.000000D+00
      A(547)=   0.000000D+00
      A(548)=  -1.611481D-01
      A(549)=  -2.866160D-03
      A(550)=   0.000000D+00
      A(551)=   0.000000D+00
      A(552)=   0.000000D+00
      A(553)=   0.000000D+00
      A(554)=   0.000000D+00
      A(555)=   0.000000D+00
      A(556)=   0.000000D+00
      A(557)=  -0.000000D+00
      A(558)=  -0.000000D+00
      A(559)=   7.123289D+04
      A(560)=   1.266940D+02
      A(561)=  -0.000000D+00
      A(562)=  -0.000000D+00
      A(563)=  -0.000000D+00
      A(564)=  -0.000000D+00
      A(565)=  -0.000000D+00
      A(566)=  -0.000000D+00
      A(567)=  -0.000000D+00
      A(568)=  -0.000000D+00
      A(569)=  -0.000000D+00
      A(570)=   6.174867D-01
      A(571)=  -2.113550D+01
      A(572)=   0.000000D+00
      A(573)=   0.000000D+00
      A(574)=  -0.000000D+00
      A(575)=   0.000000D+00
      A(576)=  -0.000000D+00
      A(577)=   0.000000D+00
      A(578)=   0.000000D+00
      A(579)=  -0.000000D+00
      A(580)=   0.000000D+00
      A(581)=  -0.000000D+00
      A(582)=   0.000000D+00
      A(583)=  -0.000000D+00
      A(584)=   0.000000D+00
      A(585)=   0.000000D+00
      A(586)=   0.000000D+00
      A(587)=   0.000000D+00
      A(588)=  -0.000000D+00
      A(589)=   0.000000D+00
      A(590)=   0.000000D+00
      A(591)=   0.000000D+00
      A(592)=  -0.000000D+00
      A(593)=   0.000000D+00
      A(594)=   0.000000D+00
      A(595)=  -0.000000D+00
      A(596)=   0.000000D+00
      A(597)=  -0.000000D+00
      A(598)=   0.000000D+00
      A(599)=  -0.000000D+00
      A(600)=   0.000000D+00
      A(601)=   0.000000D+00
      A(602)=   0.000000D+00
      A(603)=   0.000000D+00
      A(604)=  -0.000000D+00
      A(605)=   0.000000D+00
      A(606)=   0.000000D+00
      A(607)=  -0.000000D+00
      A(608)=   0.000000D+00
      A(609)=   0.000000D+00
      A(610)=  -0.000000D+00
      A(611)=   0.000000D+00
      A(612)=   0.000000D+00
      A(613)=  -0.000000D+00
      A(614)=   0.000000D+00
      A(615)=   0.000000D+00
      A(616)=  -0.000000D+00
      A(617)=   0.000000D+00
      A(618)=   0.000000D+00
      A(619)=   0.000000D+00
      A(620)=   0.000000D+00
      A(621)=  -0.000000D+00
      A(622)=   0.000000D+00
      A(623)=   0.000000D+00
      A(624)=   0.000000D+00
      A(625)=   0.000000D+00
      A(626)=  -0.000000D+00
      A(627)=   0.000000D+00
      A(628)=   0.000000D+00
      A(629)=  -0.000000D+00
      A(630)=  -0.000000D+00
      A(631)=   0.000000D+00
      A(632)=   0.000000D+00
      A(633)=   0.000000D+00
      A(634)=  -0.000000D+00
      A(635)=  -0.000000D+00
      A(636)=   0.000000D+00
      A(637)=   0.000000D+00
      A(638)=   0.000000D+00
      A(639)=  -0.000000D+00
      A(640)=   0.000000D+00
      A(641)=   0.000000D+00
      A(642)=   0.000000D+00
      A(643)=   0.000000D+00
      A(644)=  -0.000000D+00
      A(645)=   0.000000D+00
      A(646)=   0.000000D+00
      A(647)=  -0.000000D+00
      A(648)=  -0.000000D+00
      A(649)=   0.000000D+00
      A(650)=   0.000000D+00
      A(651)=   0.000000D+00
      A(652)=  -0.000000D+00
      A(653)=   0.000000D+00
      A(654)=   0.000000D+00
      A(655)=  -0.000000D+00
      A(656)=   0.000000D+00
      A(657)=   0.000000D+00
      A(658)=  -0.000000D+00
      A(659)=   0.000000D+00
      A(660)=   0.000000D+00
      A(661)=  -0.000000D+00
      A(662)=   0.000000D+00
      A(663)=   0.000000D+00
      A(664)=  -0.000000D+00
      A(665)=   0.000000D+00
      A(666)=   0.000000D+00
      A(667)=  -0.000000D+00
      A(668)=   0.000000D+00
      A(669)=   0.000000D+00
      A(670)=  -0.000000D+00
      A(671)=   0.000000D+00
      A(672)=  -0.000000D+00
      A(673)=   0.000000D+00
      A(674)=  -0.000000D+00
      A(675)=   0.000000D+00
      A(676)=  -0.000000D+00
      A(677)=   0.000000D+00
      A(678)=   0.000000D+00
      A(679)=  -0.000000D+00
      A(680)=   0.000000D+00
      A(681)=  -0.000000D+00
      A(682)=   0.000000D+00
      A(683)=   0.000000D+00
      A(684)=  -0.000000D+00
      A(685)=   0.000000D+00
      A(686)=  -0.000000D+00
      A(687)=   0.000000D+00
      A(688)=  -0.000000D+00
      A(689)=   0.000000D+00
      A(690)=   0.000000D+00
      A(691)=  -0.000000D+00
      A(692)=   0.000000D+00
      A(693)=   0.000000D+00
      A(694)=  -0.000000D+00
      A(695)=   0.000000D+00
      A(696)=   0.000000D+00
      A(697)=   0.000000D+00
      A(698)=   0.000000D+00
      A(699)=   0.000000D+00
      A(700)=   0.000000D+00
      A(701)=   0.000000D+00
      A(702)=   0.000000D+00
      A(703)=   0.000000D+00
      A(704)=   0.000000D+00
      A(705)=   0.000000D+00
      A(706)=   0.000000D+00
      A(707)=   0.000000D+00
      A(708)=   0.000000D+00
      A(709)=   0.000000D+00
      A(710)=   0.000000D+00
      A(711)=   0.000000D+00
      A(712)=   0.000000D+00
      A(713)=   0.000000D+00
      A(714)=   0.000000D+00
      A(715)=   0.000000D+00
      A(716)=   0.000000D+00
      A(717)=   0.000000D+00
      A(718)=   0.000000D+00
      A(719)=   0.000000D+00
      A(720)=   0.000000D+00
      A(721)=   0.000000D+00
      A(722)=   0.000000D+00
      A(723)=   0.000000D+00
      A(724)=   0.000000D+00
      A(725)=   0.000000D+00
      A(726)=   0.000000D+00
      A(727)=   0.000000D+00
      A(728)=   0.000000D+00
      A(729)=   0.000000D+00
      A(730)=   0.000000D+00
      A(731)=   0.000000D+00
      A(732)=   0.000000D+00
      A(733)=   0.000000D+00
      A(734)=   0.000000D+00
      A(735)=  -1.851485D-01
      A(736)=  -1.851485D-01
      A(737)=   1.930693D+00
      A(738)=   1.930693D+00
      A(739)=   1.534653D+00
      A(740)=   1.534653D+00
      A(741)=   0.000000D+00
      A(742)=   0.000000D+00
      A(743)=   0.000000D+00
      A(744)=   0.000000D+00
      A(745)=   0.000000D+00
      A(746)=   0.000000D+00
      A(747)=   0.000000D+00
      A(748)=   0.000000D+00
      A(749)=   0.000000D+00
      A(750)=   0.000000D+00
      A(751)=   0.000000D+00
      A(752)=   0.000000D+00
      A(753)=   0.000000D+00
      A(754)=   0.000000D+00
      A(755)=   0.000000D+00
      A(756)=   0.000000D+00
      A(757)=   0.000000D+00
      A(758)=   0.000000D+00
      A(759)=  -0.000000D+00
      A(760)=  -0.000000D+00
      A(761)=   0.000000D+00
      A(762)=   0.000000D+00
      A(763)=   0.000000D+00
      A(764)=   0.000000D+00
      A(765)=   0.000000D+00
      A(766)=   0.000000D+00
      A(767)=   0.000000D+00
      A(768)=   0.000000D+00
      A(769)=   0.000000D+00
      A(770)=   0.000000D+00
      A(771)=  -0.000000D+00
      A(772)=  -0.000000D+00
      A(773)=   0.000000D+00
      A(774)=   0.000000D+00
      A(775)=   0.000000D+00
      A(776)=   0.000000D+00
      A(777)=  -0.000000D+00
      A(778)=   0.000000D+00
      A(779)=   0.000000D+00
      A(780)=   0.000000D+00
      A(781)=   0.000000D+00
      A(782)=   0.000000D+00
      A(783)=  -0.000000D+00
      A(784)=   0.000000D+00
      A(785)=   0.000000D+00
      A(786)=   0.000000D+00
      A(787)=   0.000000D+00
      A(788)=   0.000000D+00
      A(789)=  -0.000000D+00
      A(790)=   0.000000D+00
      A(791)=   0.000000D+00
      A(792)=   0.000000D+00
      A(793)=   0.000000D+00
      A(794)=   0.000000D+00
      A(795)=  -0.000000D+00
      A(796)=   0.000000D+00
      A(797)=   0.000000D+00
      A(798)=   0.000000D+00
      A(799)=   0.000000D+00
      A(800)=   0.000000D+00
      A(801)=  -0.000000D+00
      A(802)=   0.000000D+00
      A(803)=   0.000000D+00
      A(804)=   0.000000D+00
      A(805)=   0.000000D+00
      A(806)=   0.000000D+00
      A(807)=  -0.000000D+00
      A(808)=   0.000000D+00
      A(809)=   2.524752D+00
      A(810)=   2.524752D+00
      A(811)=  -1.534653D+00
      A(812)=  -1.534653D+00
      A(813)=   0.000000D+00
      A(814)=   0.000000D+00
      A(815)=   0.000000D+00
      A(816)=   0.000000D+00
      A(817)=   0.000000D+00
      A(818)=   0.000000D+00
      A(819)=   0.000000D+00
      A(820)=   0.000000D+00
      A(821)=   0.000000D+00
      A(822)=   0.000000D+00
      A(823)=   0.000000D+00
      A(824)=   0.000000D+00
      A(825)=   0.000000D+00
      A(826)=   0.000000D+00
      A(827)=   0.000000D+00
      A(828)=   0.000000D+00
      A(829)=   0.000000D+00
      A(830)=   0.000000D+00
      A(831)=   0.000000D+00
      A(832)=   0.000000D+00
      A(833)=   0.000000D+00
      A(834)=   0.000000D+00
      A(835)=   0.000000D+00
      A(836)=   0.000000D+00
      A(837)=   0.000000D+00
      A(838)=   0.000000D+00
      A(839)=   0.000000D+00
      A(840)=   0.000000D+00
      A(841)=   0.000000D+00
      A(842)=   0.000000D+00
      A(843)=   0.000000D+00
      A(844)=   0.000000D+00
      A(845)=   0.000000D+00
      A(846)=  -0.000000D+00
      A(847)=   0.000000D+00
      A(848)=   0.000000D+00
      A(849)=  -0.000000D+00
      A(850)=   0.000000D+00
      A(851)=   0.000000D+00
      A(852)=  -0.000000D+00
      A(853)=   0.000000D+00
      A(854)=   0.000000D+00
      A(855)=  -0.000000D+00
      A(856)=   0.000000D+00
      A(857)=   0.000000D+00
      A(858)=  -0.000000D+00
      A(859)=   0.000000D+00
      A(860)=   0.000000D+00
      A(861)=  -0.000000D+00
      A(862)=   0.000000D+00
      A(863)=   0.000000D+00
      A(864)=  -0.000000D+00
      A(865)=   0.000000D+00
      A(866)=   0.000000D+00
      A(867)=  -0.000000D+00
      A(868)=   0.000000D+00
      A(869)=   0.000000D+00
      A(870)=  -0.000000D+00
      A(871)=   0.000000D+00
      A(872)=   0.000000D+00
      A(873)=  -0.000000D+00
      A(874)=   0.000000D+00
      A(875)=   0.000000D+00
      A(876)=  -0.000000D+00
      A(877)=   0.000000D+00
      A(878)=   0.000000D+00
      A(879)=  -0.000000D+00
      A(880)=   0.000000D+00
      A(881)=   0.000000D+00
      A(882)=   0.000000D+00
      A(883)=   0.000000D+00
      A(884)=   0.000000D+00
      A(885)=   0.000000D+00
      A(886)=   0.000000D+00
      A(887)=   0.000000D+00
      A(888)=   0.000000D+00
      A(889)=   0.000000D+00
      A(890)=   0.000000D+00
      A(891)=   0.000000D+00
      A(892)=   0.000000D+00
      A(893)=   0.000000D+00
      A(894)=   0.000000D+00
      A(895)=   0.000000D+00
      A(896)=   0.000000D+00
      A(897)=   0.000000D+00
      A(898)=   0.000000D+00
      A(899)=   0.000000D+00
      A(900)=   0.000000D+00
      A(901)=   0.000000D+00
      A(902)=   0.000000D+00
      A(903)=   0.000000D+00
      A(904)=   0.000000D+00
      A(905)=   0.000000D+00
      A(906)=   0.000000D+00
      A(907)=   0.000000D+00
      A(908)=   0.000000D+00
      A(909)=   0.000000D+00
      A(910)=   0.000000D+00
      A(911)=   0.000000D+00
      A(912)=   0.000000D+00
      A(913)=   0.000000D+00
      A(914)=   0.000000D+00
      A(915)=   0.000000D+00
      A(916)=   0.000000D+00
      A(917)=   0.000000D+00
      A(918)=   0.000000D+00
      A(919)=   0.000000D+00
      A(920)=   0.000000D+00
      A(921)=   0.000000D+00
      A(922)=   0.000000D+00
      A(923)=   0.000000D+00
      A(924)=   0.000000D+00
      A(925)=   0.000000D+00
      A(926)=   0.000000D+00
      A(927)=   0.000000D+00
      A(928)=   0.000000D+00
      A(929)=  -4.493520D-01
      A(930)=  -4.493520D-01
      A(931)=   4.493520D-01
      A(932)=   4.493520D-01
      A(933)=  -8.987040D-01
      A(934)=  -8.987040D-01
      A(935)=   8.987040D-01
      A(936)=   8.987040D-01
      A(937)=  -4.493520D-01
      A(938)=  -4.493520D-01
      A(939)=   4.493520D-01
      A(940)=   4.493520D-01
      A(941)=  -4.493520D-01
      A(942)=  -4.493520D-01
      A(943)=   4.493520D-01
      A(944)=   4.493520D-01
      A(945)=  -4.493520D-01
      A(946)=  -4.493520D-01
      A(947)=   4.493520D-01
      A(948)=   4.493520D-01
      A(949)=  -4.493520D-01
      A(950)=  -4.493520D-01
      A(951)=   4.493520D-01
      A(952)=   4.493520D-01
      A(953)=  -4.493520D-01
      A(954)=  -4.493520D-01
      A(955)=   4.493520D-01
      A(956)=   4.493520D-01
      A(957)=  -8.987040D-01
      A(958)=  -8.987040D-01
      A(959)=   8.987040D-01
      A(960)=   8.987040D-01
      A(961)=  -8.987040D-01
      A(962)=  -8.987040D-01
      A(963)=   8.987040D-01
      A(964)=   8.987040D-01
      A(965)=   0.000000D+00
      A(966)=   0.000000D+00
      A(967)=   0.000000D+00
      A(968)=   0.000000D+00
      A(969)=   0.000000D+00
      A(970)=   0.000000D+00
      A(971)=   0.000000D+00
      A(972)=   0.000000D+00
      A(973)=   0.000000D+00
      A(974)=   0.000000D+00
      A(975)=   0.000000D+00
      A(976)=   0.000000D+00
      A(977)=   0.000000D+00
      A(978)=   0.000000D+00
      A(979)=   0.000000D+00
      A(980)=   0.000000D+00
      A(981)=   0.000000D+00
      A(982)=   0.000000D+00
      A(983)=   0.000000D+00
      A(984)=   0.000000D+00
      A(985)=   0.000000D+00
      A(986)=   0.000000D+00
      A(987)=   0.000000D+00
      A(988)=   0.000000D+00
      A(989)=   0.000000D+00
      A(990)=   0.000000D+00
      A(991)=   0.000000D+00
      A(992)=   0.000000D+00
      A(993)=   0.000000D+00
      A(994)=   0.000000D+00
      A(995)=   0.000000D+00
      A(996)=   0.000000D+00
      A(997)=   0.000000D+00
      A(998)=   0.000000D+00
      A(999)=   0.000000D+00
      A(1000)=   0.000000D+00
      A(1001)=   0.000000D+00
      A(1002)=   0.000000D+00
      A(1003)=   0.000000D+00
      A(1004)=   0.000000D+00
      A(1005)=   0.000000D+00
      A(1006)=   0.000000D+00
      A(1007)=   0.000000D+00
      A(1008)=   0.000000D+00
      A(1009)=   0.000000D+00
      A(1010)=   0.000000D+00
      A(1011)=   0.000000D+00
      A(1012)=   0.000000D+00
      A(1013)=   0.000000D+00
      A(1014)=   0.000000D+00
      A(1015)=   0.000000D+00
      A(1016)=   0.000000D+00
      A(1017)=   0.000000D+00
      A(1018)=   0.000000D+00
      A(1019)=   0.000000D+00
      A(1020)=   0.000000D+00
      A(1021)=   0.000000D+00
      A(1022)=   0.000000D+00
      A(1023)=   0.000000D+00
      A(1024)=   0.000000D+00
      A(1025)=   0.000000D+00
      A(1026)=   0.000000D+00
      A(1027)=   0.000000D+00
      A(1028)=   0.000000D+00
      A(1029)=   0.000000D+00
      A(1030)=   0.000000D+00
      A(1031)=   0.000000D+00
      A(1032)=   0.000000D+00
      A(1033)=   0.000000D+00
      A(1034)=   0.000000D+00
      A(1035)=   0.000000D+00
      A(1036)=   0.000000D+00
      A(1037)=   0.000000D+00
      A(1038)=   0.000000D+00
      A(1039)=   0.000000D+00
      A(1040)=   0.000000D+00
      A(1041)=   0.000000D+00
      A(1042)=   0.000000D+00
      A(1043)=   0.000000D+00
      A(1044)=   0.000000D+00
      A(1045)=   0.000000D+00
      A(1046)=   0.000000D+00
      A(1047)=   0.000000D+00
      A(1048)=   0.000000D+00
      A(1049)=   0.000000D+00
      A(1050)=   0.000000D+00
      A(1051)=   0.000000D+00
      A(1052)=   0.000000D+00
      A(1053)=   0.000000D+00
      A(1054)=   0.000000D+00
      A(1055)=   0.000000D+00
      A(1056)=   0.000000D+00
      A(1057)=   0.000000D+00
      A(1058)=   0.000000D+00
      A(1059)=   0.000000D+00
      A(1060)=   0.000000D+00
      A(1061)=   0.000000D+00
      A(1062)=   0.000000D+00
      A(1063)=   0.000000D+00
      A(1064)=   0.000000D+00
      A(1065)=   0.000000D+00
      A(1066)=   0.000000D+00
      A(1067)=   0.000000D+00
      A(1068)=   0.000000D+00
      A(1069)=   0.000000D+00
      A(1070)=   0.000000D+00
      A(1071)=   0.000000D+00
      A(1072)=   0.000000D+00
      A(1073)=   0.000000D+00
      A(1074)=   0.000000D+00
      A(1075)=   0.000000D+00
      A(1076)=   0.000000D+00
      A(1077)=   0.000000D+00
      A(1078)=   0.000000D+00
      A(1079)=   0.000000D+00
      A(1080)=   0.000000D+00
      A(1081)=   0.000000D+00
      A(1082)=   0.000000D+00
      A(1083)=   0.000000D+00
      A(1084)=   0.000000D+00
      A(1085)=   0.000000D+00
      A(1086)=   0.000000D+00
      A(1087)=   0.000000D+00
      A(1088)=   0.000000D+00
      A(1089)=   0.000000D+00
      A(1090)=   0.000000D+00
      A(1091)=   0.000000D+00
      A(1092)=   0.000000D+00
      A(1093)=   0.000000D+00
      A(1094)=   0.000000D+00
      A(1095)=   0.000000D+00
      A(1096)=   0.000000D+00
      A(1097)=  -0.000000D+00
      A(1098)=   0.000000D+00
      A(1099)=  -4.300722D+01
      A(1100)=  -7.649212D-01
      A(1101)=  -0.000000D+00
      A(1102)=   0.000000D+00
      A(1103)=   0.000000D+00
      A(1104)=   0.000000D+00
      A(1105)=   0.000000D+00
      A(1106)=   0.000000D+00
      A(1107)=   0.000000D+00
      A(1108)=   0.000000D+00
      A(1109)=   0.000000D+00
      A(1110)=   0.000000D+00
      A(1111)=  -6.808506D+00
      A(1112)=  -1.210953D-02
      A(1113)=   0.000000D+00
      A(1114)=  -0.000000D+00
      A(1115)=   0.000000D+00
      A(1116)=  -0.000000D+00
      A(1117)=   0.000000D+00
      A(1118)=  -0.000000D+00
      A(1119)=  -0.000000D+00
      A(1120)=  -0.000000D+00
      A(1121)=  -8.662300D-03
      A(1122)=  -4.621998D-04
      A(1123)=  -0.000000D+00
      A(1124)=  -0.000000D+00
      A(1125)=  -0.000000D+00
      A(1126)=  -0.000000D+00
      A(1127)=   0.000000D+00
      A(1128)=  -2.448263D+00
      A(1129)=  -1.306335D-01
      A(1130)=  -0.000000D+00
      A(1131)=  -0.000000D+00
      A(1132)=   0.000000D+00
      A(1133)=   0.000000D+00
      A(1134)=  -0.000000D+00
      A(1135)=  -0.000000D+00
      A(1136)=  -0.000000D+00
      A(1137)=   0.000000D+00
      A(1138)=   0.000000D+00
      A(1139)=  -0.000000D+00
      A(1140)=   0.000000D+00
      A(1141)=  -9.303888D-02
      A(1142)=  -1.654778D-03
      A(1143)=  -0.000000D+00
      A(1144)=   0.000000D+00
      A(1145)=  -0.000000D+00
      A(1146)=   0.000000D+00
      A(1147)=   0.000000D+00
      A(1148)=  -0.000000D+00
      A(1149)=   0.000000D+00
      A(1150)=   0.000000D+00
      A(1151)=   0.000000D+00
      A(1152)=  -1.233790D+05
      A(1153)=  -2.194404D+02
      A(1154)=   0.000000D+00
      A(1155)=  -0.000000D+00
      A(1156)=   0.000000D+00
      A(1157)=   0.000000D+00
      A(1158)=  -0.000000D+00
      A(1159)=   0.000000D+00
      A(1160)=   0.000000D+00
      A(1161)=   0.000000D+00
      A(1162)=   0.000000D+00
      A(1163)=   0.000000D+00
      A(1164)=   0.000000D+00
      A(1165)=   0.000000D+00
      A(1166)=   0.000000D+00
      A(1167)=   0.000000D+00
      A(1168)=   0.000000D+00
      A(1169)=   0.000000D+00
      A(1170)=   0.000000D+00
      A(1171)=   0.000000D+00
      A(1172)=   0.000000D+00
      A(1173)=   0.000000D+00
      A(1174)=   0.000000D+00
      A(1175)=   0.000000D+00
      A(1176)=   0.000000D+00
      A(1177)=   0.000000D+00
      A(1178)=   0.000000D+00
      A(1179)=   0.000000D+00
      A(1180)=   0.000000D+00
      A(1181)=   0.000000D+00
      A(1182)=   0.000000D+00
      A(1183)=   0.000000D+00
      A(1184)=   0.000000D+00
      A(1185)=   0.000000D+00
      A(1186)=   0.000000D+00
      A(1187)=   0.000000D+00
      A(1188)=   0.000000D+00
      A(1189)=   0.000000D+00
      A(1190)=   0.000000D+00
      A(1191)=   0.000000D+00
      A(1192)=   0.000000D+00
      A(1193)=   0.000000D+00
      A(1194)=   0.000000D+00
      A(1195)=   0.000000D+00
      A(1196)=   0.000000D+00
      A(1197)=   0.000000D+00
      A(1198)=   0.000000D+00
      A(1199)=   0.000000D+00
      A(1200)=   0.000000D+00
      A(1201)=   0.000000D+00
      A(1202)=   0.000000D+00
      A(1203)=   0.000000D+00
      A(1204)=   0.000000D+00
      A(1205)=   0.000000D+00
      A(1206)=   0.000000D+00
      A(1207)=   0.000000D+00
      A(1208)=   0.000000D+00
      A(1209)=   0.000000D+00
      A(1210)=   0.000000D+00
      A(1211)=   0.000000D+00
      A(1212)=   0.000000D+00
      A(1213)=   0.000000D+00
      A(1214)=   0.000000D+00
      A(1215)=   0.000000D+00
      A(1216)=   0.000000D+00
      A(1217)=   0.000000D+00
      A(1218)=   0.000000D+00
      A(1219)=   0.000000D+00
      A(1220)=   0.000000D+00
      A(1221)=   0.000000D+00
      A(1222)=   0.000000D+00
      A(1223)=   0.000000D+00
      A(1224)=   0.000000D+00
      A(1225)=   0.000000D+00
      A(1226)=   0.000000D+00
      A(1227)=   0.000000D+00
      A(1228)=   0.000000D+00
      A(1229)=   0.000000D+00
      A(1230)=   0.000000D+00
      A(1231)=   0.000000D+00
      A(1232)=   0.000000D+00
      A(1233)=   0.000000D+00
      A(1234)=   0.000000D+00
      A(1235)=   0.000000D+00
      A(1236)=   0.000000D+00
      A(1237)=  -0.000000D+00
      A(1238)=   0.000000D+00
      A(1239)=   0.000000D+00
      A(1240)=   0.000000D+00
      A(1241)=   0.000000D+00
      A(1242)=   0.000000D+00
      A(1243)=  -0.000000D+00
      A(1244)=   0.000000D+00
      A(1245)=   0.000000D+00
      A(1246)=   0.000000D+00
      A(1247)=   0.000000D+00
      A(1248)=   0.000000D+00
      A(1249)=  -0.000000D+00
      A(1250)=   0.000000D+00
      A(1251)=   0.000000D+00
      A(1252)=   0.000000D+00
      A(1253)=   0.000000D+00
      A(1254)=   0.000000D+00
      A(1255)=  -0.000000D+00
      A(1256)=   0.000000D+00
      A(1257)=   0.000000D+00
      A(1258)=   0.000000D+00
      A(1259)=   0.000000D+00
      A(1260)=   0.000000D+00
      A(1261)=  -0.000000D+00
      A(1262)=   0.000000D+00
      A(1263)=   0.000000D+00
      A(1264)=   0.000000D+00
      A(1265)=   0.000000D+00
      A(1266)=   0.000000D+00
      A(1267)=  -0.000000D+00
      A(1268)=   0.000000D+00
      A(1269)=   0.000000D+00
      A(1270)=   0.000000D+00
      A(1271)=   0.000000D+00
      A(1272)=   0.000000D+00
      A(1273)=  -0.000000D+00
      A(1274)=   0.000000D+00
      A(1275)=   0.000000D+00
      A(1276)=   0.000000D+00
      A(1277)=   0.000000D+00
      A(1278)=   0.000000D+00
      A(1279)=  -0.000000D+00
      A(1280)=   0.000000D+00
      A(1281)=   0.000000D+00
      A(1282)=   0.000000D+00
      A(1283)=   0.000000D+00
      A(1284)=   0.000000D+00
      A(1285)=   0.000000D+00
      A(1286)=   0.000000D+00
      A(1287)=   0.000000D+00
      A(1288)=   0.000000D+00
      A(1289)=   0.000000D+00
      A(1290)=   0.000000D+00
      A(1291)=   0.000000D+00
      A(1292)=  -0.000000D+00
      A(1293)=  -0.000000D+00
      A(1294)=  -0.000000D+00
      A(1295)=  -0.000000D+00
      A(1296)=  -0.000000D+00
      A(1297)=  -0.000000D+00
      A(1298)=  -0.000000D+00
      A(1299)=  -0.000000D+00
      A(1300)=   0.000000D+00
      A(1301)=   0.000000D+00
      A(1302)=   0.000000D+00
      A(1303)=   0.000000D+00
      A(1304)=   0.000000D+00
      A(1305)=   0.000000D+00
      A(1306)=   0.000000D+00
      A(1307)=   0.000000D+00
      A(1308)=   0.000000D+00
      A(1309)=   0.000000D+00
      A(1310)=  -0.000000D+00
      A(1311)=  -0.000000D+00
      A(1312)=  -0.000000D+00
      A(1313)=  -0.000000D+00
      A(1314)=  -0.000000D+00
      A(1315)=  -0.000000D+00
      A(1316)=  -0.000000D+00
      A(1317)=  -0.000000D+00
      A(1318)=   0.000000D+00
      A(1319)=   0.000000D+00
      A(1320)=   0.000000D+00
      A(1321)=  -0.000000D+00
      A(1322)=   0.000000D+00
      A(1323)=  -0.000000D+00
      A(1324)=   0.000000D+00
      A(1325)=  -0.000000D+00
      A(1326)=   8.987040D-01
      A(1327)=   8.987040D-01
      A(1328)=   0.000000D+00
      A(1329)=  -0.000000D+00
      A(1330)=   0.000000D+00
      A(1331)=   0.000000D+00
      A(1332)=   0.000000D+00
      A(1333)=   0.000000D+00
      A(1334)=   0.000000D+00
      A(1335)=   0.000000D+00
      A(1336)=   0.000000D+00
      A(1337)=   0.000000D+00
      A(1338)=   0.000000D+00
      A(1339)=   0.000000D+00
      A(1340)=   0.000000D+00
      A(1341)=   0.000000D+00
      A(1342)=   0.000000D+00
      A(1343)=  -0.000000D+00
      A(1344)=   0.000000D+00
      A(1345)=  -0.000000D+00
      A(1346)=  -0.000000D+00
      A(1347)=  -0.000000D+00
      A(1348)=   0.000000D+00
      A(1349)=  -0.000000D+00
      A(1350)=   0.000000D+00
      A(1351)=   0.000000D+00
      A(1352)=   0.000000D+00
      A(1353)=   0.000000D+00
      A(1354)=   0.000000D+00
      A(1355)=   0.000000D+00
      A(1356)=   0.000000D+00
      A(1357)=   0.000000D+00
      A(1358)=  -0.000000D+00
      A(1359)=   0.000000D+00
      A(1360)=   0.000000D+00
      A(1361)=   0.000000D+00
      A(1362)=   0.000000D+00
      A(1363)=   0.000000D+00
      A(1364)=  -0.000000D+00
      A(1365)=   0.000000D+00
      A(1366)=   0.000000D+00
      A(1367)=   0.000000D+00
      A(1368)=   0.000000D+00
      A(1369)=   0.000000D+00
      A(1370)=  -0.000000D+00
      A(1371)=   0.000000D+00
      A(1372)=   0.000000D+00
      A(1373)=   0.000000D+00
      A(1374)=   0.000000D+00
      A(1375)=   0.000000D+00
      A(1376)=  -0.000000D+00
      A(1377)=   0.000000D+00
      A(1378)=   0.000000D+00
      A(1379)=   0.000000D+00
      A(1380)=   0.000000D+00
      A(1381)=  -0.000000D+00
      A(1382)=   0.000000D+00
      A(1383)=   0.000000D+00
      A(1384)=   0.000000D+00
      A(1385)=   0.000000D+00
      A(1386)=   0.000000D+00
      A(1387)=  -0.000000D+00
      A(1388)=   0.000000D+00
      A(1389)=   0.000000D+00
      A(1390)=   0.000000D+00
      A(1391)=   0.000000D+00
      A(1392)=   0.000000D+00
      A(1393)=  -0.000000D+00
      A(1394)=   0.000000D+00
      A(1395)=   0.000000D+00
      A(1396)=   0.000000D+00
      A(1397)=   0.000000D+00
      A(1398)=   0.000000D+00
      A(1399)=  -0.000000D+00
      A(1400)=   0.000000D+00
      A(1401)=   0.000000D+00
      A(1402)=   0.000000D+00
      A(1403)=   0.000000D+00
      A(1404)=   0.000000D+00
      A(1405)=   0.000000D+00
      A(1406)=   0.000000D+00
      A(1407)=   0.000000D+00
      A(1408)=  -0.000000D+00
      A(1409)=  -0.000000D+00
      A(1410)=  -0.000000D+00
      A(1411)=  -0.000000D+00
      A(1412)=   0.000000D+00
      A(1413)=   0.000000D+00
      A(1414)=   0.000000D+00
      A(1415)=   0.000000D+00
      A(1416)=   0.000000D+00
      A(1417)=   0.000000D+00
      A(1418)=  -0.000000D+00
      A(1419)=  -0.000000D+00
      A(1420)=  -0.000000D+00
      A(1421)=  -0.000000D+00
      A(1422)=   0.000000D+00
      A(1423)=   0.000000D+00
      A(1424)=   0.000000D+00
      A(1425)=   0.000000D+00
      A(1426)=   0.000000D+00
      A(1427)=   0.000000D+00
      A(1428)=   0.000000D+00
      A(1429)=   0.000000D+00
      A(1430)=   0.000000D+00
      A(1431)=   0.000000D+00
      A(1432)=   0.000000D+00
      A(1433)=   0.000000D+00
      A(1434)=   0.000000D+00
      A(1435)=  -0.000000D+00
      A(1436)=   0.000000D+00
      A(1437)=   0.000000D+00
      A(1438)=   0.000000D+00
      A(1439)=   0.000000D+00
      A(1440)=   0.000000D+00
      A(1441)=   0.000000D+00
      A(1442)=   0.000000D+00
      A(1443)=   0.000000D+00
      A(1444)=  -0.000000D+00
      A(1445)=   0.000000D+00
      A(1446)=   0.000000D+00
      A(1447)=   0.000000D+00
      A(1448)=   0.000000D+00
      A(1449)=   0.000000D+00
      A(1450)=  -0.000000D+00
      A(1451)=   0.000000D+00
      A(1452)=   0.000000D+00
      A(1453)=   0.000000D+00
      A(1454)=  -0.000000D+00
      A(1455)=   0.000000D+00
      A(1456)=   0.000000D+00
      A(1457)=   0.000000D+00
      A(1458)=   0.000000D+00
      A(1459)=   0.000000D+00
      A(1460)=   0.000000D+00
      A(1461)=   0.000000D+00
      A(1462)=   0.000000D+00
      A(1463)=   0.000000D+00
      A(1464)=  -0.000000D+00
      A(1465)=   0.000000D+00
      A(1466)=  -0.000000D+00
      A(1467)=  -0.000000D+00
      A(1468)=   0.000000D+00
      A(1469)=  -0.000000D+00
      A(1470)=  -0.000000D+00
      A(1471)=   0.000000D+00
      A(1472)=   0.000000D+00
      A(1473)=   0.000000D+00
      A(1474)=  -0.000000D+00
      A(1475)=   0.000000D+00
      A(1476)=  -0.000000D+00
      A(1477)=  -0.000000D+00
      A(1478)=   0.000000D+00
      A(1479)=   0.000000D+00
      A(1480)=   0.000000D+00
      A(1481)=  -0.000000D+00
      A(1482)=   0.000000D+00
      A(1483)=  -0.000000D+00
      A(1484)=  -0.000000D+00
      A(1485)=   0.000000D+00
      A(1486)=  -0.000000D+00
      A(1487)=  -0.000000D+00
      A(1488)=   0.000000D+00
      A(1489)=   0.000000D+00
      A(1490)=   0.000000D+00
      A(1491)=  -0.000000D+00
      A(1492)=   0.000000D+00
      A(1493)=  -0.000000D+00
      A(1494)=  -0.000000D+00
      A(1495)=   0.000000D+00
      A(1496)=   0.000000D+00
      A(1497)=   0.000000D+00
      A(1498)=  -0.000000D+00
      A(1499)=   0.000000D+00
      A(1500)=  -0.000000D+00
      A(1501)=  -0.000000D+00
      A(1502)=   0.000000D+00
      A(1503)=  -0.000000D+00
      A(1504)=  -0.000000D+00
      A(1505)=   0.000000D+00
      A(1506)=   0.000000D+00
      A(1507)=   0.000000D+00
      A(1508)=  -0.000000D+00
      A(1509)=   0.000000D+00
      A(1510)=  -0.000000D+00
      A(1511)=  -0.000000D+00
      A(1512)=   0.000000D+00
      A(1513)=   0.000000D+00
      A(1514)=   0.000000D+00
      A(1515)=   0.000000D+00
      A(1516)=  -0.000000D+00
      A(1517)=   0.000000D+00
      A(1518)=   0.000000D+00
      A(1519)=   0.000000D+00
      A(1520)=   0.000000D+00
      A(1521)=   0.000000D+00
      A(1522)=   0.000000D+00
      A(1523)=   0.000000D+00
      A(1524)=   0.000000D+00
      A(1525)=   0.000000D+00
      A(1526)=  -0.000000D+00
      A(1527)=   0.000000D+00
      A(1528)=  -0.000000D+00
      A(1529)=  -0.000000D+00
      A(1530)=   0.000000D+00
      A(1531)=  -0.000000D+00
      A(1532)=  -0.000000D+00
      A(1533)=   0.000000D+00
      A(1534)=   0.000000D+00
      A(1535)=   0.000000D+00
      A(1536)=  -0.000000D+00
      A(1537)=   0.000000D+00
      A(1538)=  -0.000000D+00
      A(1539)=  -0.000000D+00
      A(1540)=   0.000000D+00
      A(1541)=   0.000000D+00
      A(1542)=   0.000000D+00
      A(1543)=  -0.000000D+00
      A(1544)=   0.000000D+00
      A(1545)=  -0.000000D+00
      A(1546)=  -0.000000D+00
      A(1547)=   0.000000D+00
      A(1548)=  -0.000000D+00
      A(1549)=  -0.000000D+00
      A(1550)=   0.000000D+00
      A(1551)=   0.000000D+00
      A(1552)=   0.000000D+00
      A(1553)=  -0.000000D+00
      A(1554)=   0.000000D+00
      A(1555)=  -0.000000D+00
      A(1556)=  -0.000000D+00
      A(1557)=   0.000000D+00
      A(1558)=   0.000000D+00
      A(1559)=  -1.174433D-02
      A(1560)=  -4.697731D-02
      A(1561)=  -1.174433D-02
      A(1562)=  -1.056990D-01
      A(1563)=  -1.056990D-01
      A(1564)=  -1.056990D-01
      A(1565)=  -1.174433D-02
      A(1566)=  -4.697731D-02
      A(1567)=  -4.697731D-02
      A(1568)=   0.000000D+00
      A(1569)=  -6.451083D+01
      A(1570)=  -4.746656D-02
      A(1571)=  -0.000000D+00
      A(1572)=  -1.965446D+00
      A(1573)=  -1.446160D-05
      A(1574)=   0.000000D+00
      A(1575)=   0.000000D+00
      A(1576)=   0.000000D+00
      A(1577)=  -1.299345D-02
      A(1578)=  -2.868143D-05
      A(1579)=   0.000000D+00
      A(1580)=  -3.672395D+00
      A(1581)=  -8.106356D-03
      A(1582)=   0.000000D+00
      A(1583)=  -1.395583D-01
      A(1584)=  -1.026859D-04
      A(1585)=  -0.000000D+00
      A(1586)=  -3.561645D+04
      A(1587)=  -2.620630D-01
      A(1588)=  -0.000000D+00
      A(1589)=   0.000000D+00
      A(1590)=  -0.000000D+00
      A(1591)=   0.000000D+00
      A(1592)=  -5.182959D+03
      A(1593)=   0.000000D+00
      A(1594)=   0.000000D+00
      A(1595)=  -0.000000D+00
      A(1596)=   0.000000D+00
      A(1597)=  -5.183837D+01
      A(1598)=  -0.000000D+00
      A(1599)=  -5.454985D+01
      A(1600)=  -0.000000D+00
      A(1601)=   0.000000D+00
      A(1602)=  -0.000000D+00
      A(1603)=   0.000000D+00
      A(1604)=  -6.706525D+03
      A(1605)=  -0.000000D+00
      A(1606)=   0.000000D+00
      A(1607)=   0.000000D+00
      A(1608)=   0.000000D+00
      A(1609)=  -1.611474D+01
      A(1610)=  -0.000000D+00
      A(1611)=  -1.148855D+01
      A(1612)=  -0.000000D+00
      A(1613)=   0.000000D+00
      A(1614)=  -4.300722D+01
      A(1615)=  -2.740483D-02
      A(1616)=  -0.000000D+00
      A(1617)=   3.930893D+00
      A(1618)=   2.504822D-05
      A(1619)=   0.000000D+00
      A(1620)=   0.000000D+00
      A(1621)=   0.000000D+00
      A(1622)=   0.000000D+00
      A(1623)=   0.000000D+00
      A(1624)=  -8.662300D-03
      A(1625)=  -1.655923D-05
      A(1626)=   0.000000D+00
      A(1627)=  -2.448263D+00
      A(1628)=  -4.680207D-03
      A(1629)=   0.000000D+00
      A(1630)=  -9.303888D-02
      A(1631)=  -5.928573D-05
      A(1632)=  -0.000000D+00
      A(1633)=   7.123289D+04
      A(1634)=   4.539064D-01
      A(1635)=  -0.000000D+00
      A(1636)=   0.000000D+00
      A(1637)=   0.000000D+00
      A(1638)=   0.000000D+00
      A(1639)=  -0.000000D+00
      A(1640)=   1.661225D+02
      A(1641)=  -0.000000D+00
      A(1642)=  -1.217331D+03
      A(1643)=   0.000000D+00
      A(1644)=   0.000000D+00
      A(1645)=   0.000000D+00
      A(1646)=   0.000000D+00
      A(1647)=  -0.000000D+00
      A(1648)=   7.374483D-01
      A(1649)=  -0.000000D+00
      A(1650)=   1.120278D-02
      A(1651)=  -0.000000D+00
      A(1652)=   0.000000D+00
      A(1653)=   0.000000D+00
      A(1654)=  -0.000000D+00
      A(1655)=  -6.978803D+00
      A(1656)=   0.000000D+00
      A(1657)=   0.000000D+00
      A(1658)=  -0.000000D+00
      A(1659)=   0.000000D+00
      A(1660)=  -0.000000D+00
      A(1661)=   0.000000D+00
      A(1662)=   0.000000D+00
      A(1663)=  -0.000000D+00
      A(1664)=   0.000000D+00
      A(1665)=  -3.443494D+01
      A(1666)=   7.601086D-02
      A(1667)=  -2.960405D+01
      A(1668)=   6.534727D-02
      A(1669)=   0.000000D+00
      A(1670)=   0.000000D+00
      A(1671)=   0.000000D+00
      A(1672)=  -0.000000D+00
      A(1673)=   0.000000D+00
      A(1674)=   0.000000D+00
      A(1675)=   0.000000D+00
      A(1676)=  -0.000000D+00
      A(1677)=   6.431112D-02
      A(1678)=  -0.000000D+00
      A(1679)=   0.000000D+00
      A(1680)=  -0.000000D+00
      A(1681)=  -0.000000D+00
      A(1682)=   0.000000D+00
      A(1683)=  -0.000000D+00
      A(1684)=  -0.000000D+00
      A(1685)=  -0.000000D+00
      A(1686)=  -6.256662D+03
      A(1687)=   0.000000D+00
      A(1688)=  -0.000000D+00
      A(1689)=  -0.000000D+00
      A(1690)=  -0.000000D+00
      A(1691)=  -6.257751D+01
      A(1692)=  -0.000000D+00
      A(1693)=  -6.569292D+01
      A(1694)=  -0.000000D+00
      A(1695)=  -0.000000D+00
      A(1696)=  -0.000000D+00
      A(1697)=  -0.000000D+00
      A(1698)=  -6.559863D+03
      A(1699)=  -0.000000D+00
      A(1700)=   0.000000D+00
      A(1701)=   0.000000D+00
      A(1702)=   0.000000D+00
      A(1703)=  -1.852591D+01
      A(1704)=  -0.000000D+00
      A(1705)=  -1.386671D+01
      A(1706)=  -0.000000D+00
      A(1707)=   0.000000D+00
      A(1708)=   0.000000D+00
      A(1709)=  -0.000000D+00
      A(1710)=   3.725581D+03
      A(1711)=   0.000000D+00
      A(1712)=   0.000000D+00
      A(1713)=   0.000000D+00
      A(1714)=  -0.000000D+00
      A(1715)=   3.726282D+01
      A(1716)=  -0.000000D+00
      A(1717)=   3.882829D+01
      A(1718)=  -0.000000D+00
      A(1719)=   0.000000D+00
      A(1720)=   0.000000D+00
      A(1721)=  -0.000000D+00
      A(1722)=   1.086686D+03
      A(1723)=  -0.000000D+00
      A(1724)=  -0.000000D+00
      A(1725)=   0.000000D+00
      A(1726)=  -0.000000D+00
      A(1727)=   9.329519D+00
      A(1728)=  -0.000000D+00
      A(1729)=   8.253706D+00
      A(1730)=  -0.000000D+00
      A(1731)=   0.000000D+00
      A(1732)=   0.000000D+00
      A(1733)=   0.000000D+00
      A(1734)=  -0.000000D+00
      A(1735)=   0.000000D+00
      A(1736)=   0.000000D+00
      A(1737)=   0.000000D+00
      A(1738)=   0.000000D+00
      A(1739)=  -0.000000D+00
      A(1740)=   0.000000D+00
      A(1741)=   0.000000D+00
      A(1742)=   8.516282D+01
      A(1743)=  -6.266212D-02
      A(1744)=   0.000000D+00
      A(1745)=   7.783946D-02
      A(1746)=  -5.727366D-07
      A(1747)=   0.000000D+00
      A(1748)=   0.000000D+00
      A(1749)=  -0.000000D+00
      A(1750)=   1.715307D-02
      A(1751)=  -3.786327D-05
      A(1752)=  -0.000000D+00
      A(1753)=   4.848046D+00
      A(1754)=  -1.070146D-02
      A(1755)=   0.000000D+00
      A(1756)=   1.842354D-01
      A(1757)=  -1.355589D-04
      A(1758)=   0.000000D+00
      A(1759)=   1.410552D+03
      A(1760)=  -1.037873D-02
      A(1761)=   0.000000D+00
      A(1762)=  -0.000000D+00
      A(1763)=   0.000000D+00
      A(1764)=  -2.019172D-01
      A(1765)=   6.322152D-03
      A(1766)=   0.000000D+00
      A(1767)=  -8.076689D-01
      A(1768)=   2.528861D-02
      A(1769)=  -1.202592D+00
      A(1770)=  -2.019172D-01
      A(1771)=   6.322152D-03
      A(1772)=   0.000000D+00
      A(1773)=  -2.019172D-01
      A(1774)=   5.689936D-02
      A(1775)=  -1.012960D-01
      A(1776)=  -2.019172D-01
      A(1777)=   5.689936D-02
      A(1778)=  -2.019172D-01
      A(1779)=   5.689936D-02
      A(1780)=  -2.019172D-01
      A(1781)=   6.322152D-03
      A(1782)=  -8.076689D-01
      A(1783)=   2.528861D-02
      A(1784)=  -8.076689D-01
      A(1785)=   2.528861D-02
      A(1786)=   2.150361D+01
      A(1787)=  -1.582219D-02
      A(1788)=   5.896339D+00
      A(1789)=  -4.338479D-05
      A(1790)=  -0.000000D+00
      A(1791)=  -0.000000D+00
      A(1792)=   4.331150D-03
      A(1793)=  -9.560477D-06
      A(1794)=   1.224132D+00
      A(1795)=  -2.702119D-03
      A(1796)=   4.651944D-02
      A(1797)=  -3.422863D-05
      A(1798)=   1.068493D+05
      A(1799)=  -7.861889D-01
      A(1800)=   0.000000D+00
      RETURN
      END

      SUBROUTINE CPTH(PATH,D_SLASH,F_SLASH)
      CHARACTER*60 PATH
      CHARACTER*1 D_SLASH,F_SLASH
      PATH='/home/belyaev/comphep/v_33_sasha_tchw'
      D_SLASH='/'
      F_SLASH='/'
      RETURN
      END

