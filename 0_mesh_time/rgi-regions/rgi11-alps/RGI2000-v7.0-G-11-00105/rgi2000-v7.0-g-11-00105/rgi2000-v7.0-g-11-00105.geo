// This a a geo file created using the python script Contour2geo.py // 
Mesh.Algorithm=5; 
// To controle the element size, one can directly modify the lc value in the geo file // 
lc = 50. ; 
// Mesh size near the boundary from prescribed value  //
Mesh.CharacteristicLengthFromCurvature = 0; 
Mesh.CharacteristicLengthFromPoints = 1; 
// Give a backgroung field with uniform value for the mesh size  // 
Mesh.CharacteristicLengthExtendFromBoundary = 0; 
Field[1] = MathEval; 
Field[1].F = Sprintf("%g",lc); 
Background Field = 1; 
Point(1) = { 289214.8125, 4972519.0, 0.0, lc}; 
Point(2) = { 289314.8125, 4972519.0, 0.0, lc}; 
Point(3) = { 289314.8125, 4972494.0, 0.0, lc}; 
Point(4) = { 289364.8125, 4972494.0, 0.0, lc}; 
Point(5) = { 289364.8125, 4972469.0, 0.0, lc}; 
Point(6) = { 289389.8125, 4972469.0, 0.0, lc}; 
Point(7) = { 289389.8125, 4972444.0, 0.0, lc}; 
Point(8) = { 289414.8125, 4972444.0, 0.0, lc}; 
Point(9) = { 289414.8125, 4972394.0, 0.0, lc}; 
Point(10) = { 289439.8125, 4972394.0, 0.0, lc}; 
Point(11) = { 289439.8125, 4972369.0, 0.0, lc}; 
Point(12) = { 289464.8125, 4972369.0, 0.0, lc}; 
Point(13) = { 289464.8125, 4972344.0, 0.0, lc}; 
Point(14) = { 289489.8125, 4972344.0, 0.0, lc}; 
Point(15) = { 289489.8125, 4972294.0, 0.0, lc}; 
Point(16) = { 289514.8125, 4972294.0, 0.0, lc}; 
Point(17) = { 289514.8125, 4972269.0, 0.0, lc}; 
Point(18) = { 289539.8125, 4972269.0, 0.0, lc}; 
Point(19) = { 289539.8125, 4972219.0, 0.0, lc}; 
Point(20) = { 289564.8125, 4972219.0, 0.0, lc}; 
Point(21) = { 289564.8125, 4972194.0, 0.0, lc}; 
Point(22) = { 289589.8125, 4972194.0, 0.0, lc}; 
Point(23) = { 289589.8125, 4972119.0, 0.0, lc}; 
Point(24) = { 289614.8125, 4972119.0, 0.0, lc}; 
Point(25) = { 289614.8125, 4972044.0, 0.0, lc}; 
Point(26) = { 289639.8125, 4972044.0, 0.0, lc}; 
Point(27) = { 289639.8125, 4971894.0, 0.0, lc}; 
Point(28) = { 289664.8125, 4971894.0, 0.0, lc}; 
Point(29) = { 289664.8125, 4971394.0, 0.0, lc}; 
Point(30) = { 289689.8125, 4971394.0, 0.0, lc}; 
Point(31) = { 289689.8125, 4971319.0, 0.0, lc}; 
Point(32) = { 289714.8125, 4971319.0, 0.0, lc}; 
Point(33) = { 289714.8125, 4971294.0, 0.0, lc}; 
Point(34) = { 289739.8125, 4971294.0, 0.0, lc}; 
Point(35) = { 289739.8125, 4971244.0, 0.0, lc}; 
Point(36) = { 289764.8125, 4971244.0, 0.0, lc}; 
Point(37) = { 289764.8125, 4971194.0, 0.0, lc}; 
Point(38) = { 289789.8125, 4971194.0, 0.0, lc}; 
Point(39) = { 289789.8125, 4971144.0, 0.0, lc}; 
Point(40) = { 289839.8125, 4971144.0, 0.0, lc}; 
Point(41) = { 289839.8125, 4971119.0, 0.0, lc}; 
Point(42) = { 289864.8125, 4971119.0, 0.0, lc}; 
Point(43) = { 289864.8125, 4971094.0, 0.0, lc}; 
Point(44) = { 289914.8125, 4971094.0, 0.0, lc}; 
Point(45) = { 289914.8125, 4971069.0, 0.0, lc}; 
Point(46) = { 289939.8125, 4971069.0, 0.0, lc}; 
Point(47) = { 289939.8125, 4971019.0, 0.0, lc}; 
Point(48) = { 289989.8125, 4971019.0, 0.0, lc}; 
Point(49) = { 289989.8125, 4970994.0, 0.0, lc}; 
Point(50) = { 290064.8125, 4970994.0, 0.0, lc}; 
Point(51) = { 290064.8125, 4970969.0, 0.0, lc}; 
Point(52) = { 290189.8125, 4970969.0, 0.0, lc}; 
Point(53) = { 290189.8125, 4970944.0, 0.0, lc}; 
Point(54) = { 290214.8125, 4970944.0, 0.0, lc}; 
Point(55) = { 290214.8125, 4970919.0, 0.0, lc}; 
Point(56) = { 290239.8125, 4970919.0, 0.0, lc}; 
Point(57) = { 290239.8125, 4970894.0, 0.0, lc}; 
Point(58) = { 290264.8125, 4970894.0, 0.0, lc}; 
Point(59) = { 290264.8125, 4970869.0, 0.0, lc}; 
Point(60) = { 290289.8125, 4970869.0, 0.0, lc}; 
Point(61) = { 290289.8125, 4970844.0, 0.0, lc}; 
Point(62) = { 290339.8125, 4970844.0, 0.0, lc}; 
Point(63) = { 290339.8125, 4970819.0, 0.0, lc}; 
Point(64) = { 290364.8125, 4970819.0, 0.0, lc}; 
Point(65) = { 290364.8125, 4970794.0, 0.0, lc}; 
Point(66) = { 290414.8125, 4970794.0, 0.0, lc}; 
Point(67) = { 290414.8125, 4970769.0, 0.0, lc}; 
Point(68) = { 290439.8125, 4970769.0, 0.0, lc}; 
Point(69) = { 290439.8125, 4970744.0, 0.0, lc}; 
Point(70) = { 290489.8125, 4970744.0, 0.0, lc}; 
Point(71) = { 290489.8125, 4970694.0, 0.0, lc}; 
Point(72) = { 290589.8125, 4970694.0, 0.0, lc}; 
Point(73) = { 290589.8125, 4970719.0, 0.0, lc}; 
Point(74) = { 290564.8125, 4970719.0, 0.0, lc}; 
Point(75) = { 290564.8125, 4970744.0, 0.0, lc}; 
Point(76) = { 290514.8125, 4970744.0, 0.0, lc}; 
Point(77) = { 290514.8125, 4970769.0, 0.0, lc}; 
Point(78) = { 290489.8125, 4970769.0, 0.0, lc}; 
Point(79) = { 290489.8125, 4970819.0, 0.0, lc}; 
Point(80) = { 290464.8125, 4970819.0, 0.0, lc}; 
Point(81) = { 290464.8125, 4970869.0, 0.0, lc}; 
Point(82) = { 290489.8125, 4970869.0, 0.0, lc}; 
Point(83) = { 290489.8125, 4970894.0, 0.0, lc}; 
Point(84) = { 290614.8125, 4970894.0, 0.0, lc}; 
Point(85) = { 290614.8125, 4970869.0, 0.0, lc}; 
Point(86) = { 290639.8125, 4970869.0, 0.0, lc}; 
Point(87) = { 290639.8125, 4970844.0, 0.0, lc}; 
Point(88) = { 290664.8125, 4970844.0, 0.0, lc}; 
Point(89) = { 290664.8125, 4970794.0, 0.0, lc}; 
Point(90) = { 290639.8125, 4970794.0, 0.0, lc}; 
Point(91) = { 290639.8125, 4970769.0, 0.0, lc}; 
Point(92) = { 290664.8125, 4970769.0, 0.0, lc}; 
Point(93) = { 290664.8125, 4970744.0, 0.0, lc}; 
Point(94) = { 290639.8125, 4970744.0, 0.0, lc}; 
Point(95) = { 290639.8125, 4970719.0, 0.0, lc}; 
Point(96) = { 290764.8125, 4970719.0, 0.0, lc}; 
Point(97) = { 290764.8125, 4970694.0, 0.0, lc}; 
Point(98) = { 290814.8125, 4970694.0, 0.0, lc}; 
Point(99) = { 290814.8125, 4970619.0, 0.0, lc}; 
Point(100) = { 290739.8125, 4970619.0, 0.0, lc}; 
Point(101) = { 290739.8125, 4970469.0, 0.0, lc}; 
Point(102) = { 290764.8125, 4970469.0, 0.0, lc}; 
Point(103) = { 290764.8125, 4970419.0, 0.0, lc}; 
Point(104) = { 290839.8125, 4970419.0, 0.0, lc}; 
Point(105) = { 290839.8125, 4970394.0, 0.0, lc}; 
Point(106) = { 290914.8125, 4970394.0, 0.0, lc}; 
Point(107) = { 290914.8125, 4970269.0, 0.0, lc}; 
Point(108) = { 290889.8125, 4970269.0, 0.0, lc}; 
Point(109) = { 290889.8125, 4970094.0, 0.0, lc}; 
Point(110) = { 290839.8125, 4970094.0, 0.0, lc}; 
Point(111) = { 290839.8125, 4970069.0, 0.0, lc}; 
Point(112) = { 290814.8125, 4970069.0, 0.0, lc}; 
Point(113) = { 290814.8125, 4970044.0, 0.0, lc}; 
Point(114) = { 290764.8125, 4970044.0, 0.0, lc}; 
Point(115) = { 290764.8125, 4970069.0, 0.0, lc}; 
Point(116) = { 290689.8125, 4970069.0, 0.0, lc}; 
Point(117) = { 290689.8125, 4970044.0, 0.0, lc}; 
Point(118) = { 290639.8125, 4970044.0, 0.0, lc}; 
Point(119) = { 290639.8125, 4970019.0, 0.0, lc}; 
Point(120) = { 290564.8125, 4970019.0, 0.0, lc}; 
Point(121) = { 290564.8125, 4969994.0, 0.0, lc}; 
Point(122) = { 290539.8125, 4969994.0, 0.0, lc}; 
Point(123) = { 290539.8125, 4969969.0, 0.0, lc}; 
Point(124) = { 290464.8125, 4969969.0, 0.0, lc}; 
Point(125) = { 290464.8125, 4970019.0, 0.0, lc}; 
Point(126) = { 290439.8125, 4970019.0, 0.0, lc}; 
Point(127) = { 290439.8125, 4969944.0, 0.0, lc}; 
Point(128) = { 290314.8125, 4969944.0, 0.0, lc}; 
Point(129) = { 290314.8125, 4969844.0, 0.0, lc}; 
Point(130) = { 290289.8125, 4969844.0, 0.0, lc}; 
Point(131) = { 290289.8125, 4969794.0, 0.0, lc}; 
Point(132) = { 290189.8125, 4969794.0, 0.0, lc}; 
Point(133) = { 290189.8125, 4969819.0, 0.0, lc}; 
Point(134) = { 290164.8125, 4969819.0, 0.0, lc}; 
Point(135) = { 290164.8125, 4969769.0, 0.0, lc}; 
Point(136) = { 290139.8125, 4969769.0, 0.0, lc}; 
Point(137) = { 290139.8125, 4969719.0, 0.0, lc}; 
Point(138) = { 290089.8125, 4969719.0, 0.0, lc}; 
Point(139) = { 290089.8125, 4969694.0, 0.0, lc}; 
Point(140) = { 290014.8125, 4969694.0, 0.0, lc}; 
Point(141) = { 290014.8125, 4969719.0, 0.0, lc}; 
Point(142) = { 289989.8125, 4969719.0, 0.0, lc}; 
Point(143) = { 289989.8125, 4969644.0, 0.0, lc}; 
Point(144) = { 289914.8125, 4969644.0, 0.0, lc}; 
Point(145) = { 289914.8125, 4969669.0, 0.0, lc}; 
Point(146) = { 289889.8125, 4969669.0, 0.0, lc}; 
Point(147) = { 289889.8125, 4969719.0, 0.0, lc}; 
Point(148) = { 289864.8125, 4969719.0, 0.0, lc}; 
Point(149) = { 289864.8125, 4969619.0, 0.0, lc}; 
Point(150) = { 289839.8125, 4969619.0, 0.0, lc}; 
Point(151) = { 289839.8125, 4969594.0, 0.0, lc}; 
Point(152) = { 289764.8125, 4969594.0, 0.0, lc}; 
Point(153) = { 289764.8125, 4969644.0, 0.0, lc}; 
Point(154) = { 289614.8125, 4969644.0, 0.0, lc}; 
Point(155) = { 289614.8125, 4969669.0, 0.0, lc}; 
Point(156) = { 289589.8125, 4969669.0, 0.0, lc}; 
Point(157) = { 289589.8125, 4969744.0, 0.0, lc}; 
Point(158) = { 289614.8125, 4969744.0, 0.0, lc}; 
Point(159) = { 289614.8125, 4969769.0, 0.0, lc}; 
Point(160) = { 289639.8125, 4969769.0, 0.0, lc}; 
Point(161) = { 289639.8125, 4969794.0, 0.0, lc}; 
Point(162) = { 289589.8125, 4969794.0, 0.0, lc}; 
Point(163) = { 289589.8125, 4969869.0, 0.0, lc}; 
Point(164) = { 289564.8125, 4969869.0, 0.0, lc}; 
Point(165) = { 289564.8125, 4969819.0, 0.0, lc}; 
Point(166) = { 289439.8125, 4969819.0, 0.0, lc}; 
Point(167) = { 289439.8125, 4969894.0, 0.0, lc}; 
Point(168) = { 289464.8125, 4969894.0, 0.0, lc}; 
Point(169) = { 289464.8125, 4969994.0, 0.0, lc}; 
Point(170) = { 289439.8125, 4969994.0, 0.0, lc}; 
Point(171) = { 289439.8125, 4969944.0, 0.0, lc}; 
Point(172) = { 289414.8125, 4969944.0, 0.0, lc}; 
Point(173) = { 289414.8125, 4969919.0, 0.0, lc}; 
Point(174) = { 289339.8125, 4969919.0, 0.0, lc}; 
Point(175) = { 289339.8125, 4969894.0, 0.0, lc}; 
Point(176) = { 289314.8125, 4969894.0, 0.0, lc}; 
Point(177) = { 289314.8125, 4969844.0, 0.0, lc}; 
Point(178) = { 289289.8125, 4969844.0, 0.0, lc}; 
Point(179) = { 289289.8125, 4969869.0, 0.0, lc}; 
Point(180) = { 289264.8125, 4969869.0, 0.0, lc}; 
Point(181) = { 289264.8125, 4969919.0, 0.0, lc}; 
Point(182) = { 289239.8125, 4969919.0, 0.0, lc}; 
Point(183) = { 289239.8125, 4970019.0, 0.0, lc}; 
Point(184) = { 289289.8125, 4970019.0, 0.0, lc}; 
Point(185) = { 289289.8125, 4969994.0, 0.0, lc}; 
Point(186) = { 289314.8125, 4969994.0, 0.0, lc}; 
Point(187) = { 289314.8125, 4969944.0, 0.0, lc}; 
Point(188) = { 289339.8125, 4969944.0, 0.0, lc}; 
Point(189) = { 289339.8125, 4970019.0, 0.0, lc}; 
Point(190) = { 289314.8125, 4970019.0, 0.0, lc}; 
Point(191) = { 289314.8125, 4970069.0, 0.0, lc}; 
Point(192) = { 289289.8125, 4970069.0, 0.0, lc}; 
Point(193) = { 289289.8125, 4970144.0, 0.0, lc}; 
Point(194) = { 289189.8125, 4970144.0, 0.0, lc}; 
Point(195) = { 289189.8125, 4970119.0, 0.0, lc}; 
Point(196) = { 289139.8125, 4970119.0, 0.0, lc}; 
Point(197) = { 289139.8125, 4970044.0, 0.0, lc}; 
Point(198) = { 289164.8125, 4970044.0, 0.0, lc}; 
Point(199) = { 289164.8125, 4969969.0, 0.0, lc}; 
Point(200) = { 289139.8125, 4969969.0, 0.0, lc}; 
Point(201) = { 289139.8125, 4969919.0, 0.0, lc}; 
Point(202) = { 289114.8125, 4969919.0, 0.0, lc}; 
Point(203) = { 289114.8125, 4969944.0, 0.0, lc}; 
Point(204) = { 289014.8125, 4969944.0, 0.0, lc}; 
Point(205) = { 289014.8125, 4969969.0, 0.0, lc}; 
Point(206) = { 288989.8125, 4969969.0, 0.0, lc}; 
Point(207) = { 288989.8125, 4970019.0, 0.0, lc}; 
Point(208) = { 288964.8125, 4970019.0, 0.0, lc}; 
Point(209) = { 288964.8125, 4970044.0, 0.0, lc}; 
Point(210) = { 288939.8125, 4970044.0, 0.0, lc}; 
Point(211) = { 288939.8125, 4970069.0, 0.0, lc}; 
Point(212) = { 288889.8125, 4970069.0, 0.0, lc}; 
Point(213) = { 288889.8125, 4970094.0, 0.0, lc}; 
Point(214) = { 288939.8125, 4970094.0, 0.0, lc}; 
Point(215) = { 288939.8125, 4970169.0, 0.0, lc}; 
Point(216) = { 288964.8125, 4970169.0, 0.0, lc}; 
Point(217) = { 288964.8125, 4970244.0, 0.0, lc}; 
Point(218) = { 289014.8125, 4970244.0, 0.0, lc}; 
Point(219) = { 289014.8125, 4970269.0, 0.0, lc}; 
Point(220) = { 289039.8125, 4970269.0, 0.0, lc}; 
Point(221) = { 289039.8125, 4970244.0, 0.0, lc}; 
Point(222) = { 289089.8125, 4970244.0, 0.0, lc}; 
Point(223) = { 289089.8125, 4970344.0, 0.0, lc}; 
Point(224) = { 289114.8125, 4970344.0, 0.0, lc}; 
Point(225) = { 289114.8125, 4970369.0, 0.0, lc}; 
Point(226) = { 289139.8125, 4970369.0, 0.0, lc}; 
Point(227) = { 289139.8125, 4970394.0, 0.0, lc}; 
Point(228) = { 289189.8125, 4970394.0, 0.0, lc}; 
Point(229) = { 289189.8125, 4970444.0, 0.0, lc}; 
Point(230) = { 289089.8125, 4970444.0, 0.0, lc}; 
Point(231) = { 289089.8125, 4970519.0, 0.0, lc}; 
Point(232) = { 289039.8125, 4970519.0, 0.0, lc}; 
Point(233) = { 289039.8125, 4970494.0, 0.0, lc}; 
Point(234) = { 289014.8125, 4970494.0, 0.0, lc}; 
Point(235) = { 289014.8125, 4970469.0, 0.0, lc}; 
Point(236) = { 288964.8125, 4970469.0, 0.0, lc}; 
Point(237) = { 288964.8125, 4970319.0, 0.0, lc}; 
Point(238) = { 288939.8125, 4970319.0, 0.0, lc}; 
Point(239) = { 288939.8125, 4970244.0, 0.0, lc}; 
Point(240) = { 288864.8125, 4970244.0, 0.0, lc}; 
Point(241) = { 288864.8125, 4970169.0, 0.0, lc}; 
Point(242) = { 288839.8125, 4970169.0, 0.0, lc}; 
Point(243) = { 288839.8125, 4970144.0, 0.0, lc}; 
Point(244) = { 288814.8125, 4970144.0, 0.0, lc}; 
Point(245) = { 288814.8125, 4970094.0, 0.0, lc}; 
Point(246) = { 288739.8125, 4970094.0, 0.0, lc}; 
Point(247) = { 288739.8125, 4970169.0, 0.0, lc}; 
Point(248) = { 288664.8125, 4970169.0, 0.0, lc}; 
Point(249) = { 288664.8125, 4970119.0, 0.0, lc}; 
Point(250) = { 288689.8125, 4970119.0, 0.0, lc}; 
Point(251) = { 288689.8125, 4970094.0, 0.0, lc}; 
Point(252) = { 288664.8125, 4970094.0, 0.0, lc}; 
Point(253) = { 288664.8125, 4970069.0, 0.0, lc}; 
Point(254) = { 288564.8125, 4970069.0, 0.0, lc}; 
Point(255) = { 288564.8125, 4970044.0, 0.0, lc}; 
Point(256) = { 288514.8125, 4970044.0, 0.0, lc}; 
Point(257) = { 288514.8125, 4970069.0, 0.0, lc}; 
Point(258) = { 288489.8125, 4970069.0, 0.0, lc}; 
Point(259) = { 288489.8125, 4970169.0, 0.0, lc}; 
Point(260) = { 288514.8125, 4970169.0, 0.0, lc}; 
Point(261) = { 288514.8125, 4970219.0, 0.0, lc}; 
Point(262) = { 288364.8125, 4970219.0, 0.0, lc}; 
Point(263) = { 288364.8125, 4970244.0, 0.0, lc}; 
Point(264) = { 288289.8125, 4970244.0, 0.0, lc}; 
Point(265) = { 288289.8125, 4970269.0, 0.0, lc}; 
Point(266) = { 288239.8125, 4970269.0, 0.0, lc}; 
Point(267) = { 288239.8125, 4970369.0, 0.0, lc}; 
Point(268) = { 288264.8125, 4970369.0, 0.0, lc}; 
Point(269) = { 288264.8125, 4970394.0, 0.0, lc}; 
Point(270) = { 288289.8125, 4970394.0, 0.0, lc}; 
Point(271) = { 288289.8125, 4970419.0, 0.0, lc}; 
Point(272) = { 288364.8125, 4970419.0, 0.0, lc}; 
Point(273) = { 288364.8125, 4970469.0, 0.0, lc}; 
Point(274) = { 288389.8125, 4970469.0, 0.0, lc}; 
Point(275) = { 288389.8125, 4970494.0, 0.0, lc}; 
Point(276) = { 288439.8125, 4970494.0, 0.0, lc}; 
Point(277) = { 288439.8125, 4970519.0, 0.0, lc}; 
Point(278) = { 288489.8125, 4970519.0, 0.0, lc}; 
Point(279) = { 288489.8125, 4970569.0, 0.0, lc}; 
Point(280) = { 288514.8125, 4970569.0, 0.0, lc}; 
Point(281) = { 288514.8125, 4970619.0, 0.0, lc}; 
Point(282) = { 288589.8125, 4970619.0, 0.0, lc}; 
Point(283) = { 288589.8125, 4970644.0, 0.0, lc}; 
Point(284) = { 288689.8125, 4970644.0, 0.0, lc}; 
Point(285) = { 288689.8125, 4970669.0, 0.0, lc}; 
Point(286) = { 288739.8125, 4970669.0, 0.0, lc}; 
Point(287) = { 288739.8125, 4970694.0, 0.0, lc}; 
Point(288) = { 288789.8125, 4970694.0, 0.0, lc}; 
Point(289) = { 288789.8125, 4970719.0, 0.0, lc}; 
Point(290) = { 288814.8125, 4970719.0, 0.0, lc}; 
Point(291) = { 288814.8125, 4970769.0, 0.0, lc}; 
Point(292) = { 288839.8125, 4970769.0, 0.0, lc}; 
Point(293) = { 288839.8125, 4970794.0, 0.0, lc}; 
Point(294) = { 288864.8125, 4970794.0, 0.0, lc}; 
Point(295) = { 288864.8125, 4970869.0, 0.0, lc}; 
Point(296) = { 288889.8125, 4970869.0, 0.0, lc}; 
Point(297) = { 288889.8125, 4970919.0, 0.0, lc}; 
Point(298) = { 288964.8125, 4970919.0, 0.0, lc}; 
Point(299) = { 288964.8125, 4970994.0, 0.0, lc}; 
Point(300) = { 288989.8125, 4970994.0, 0.0, lc}; 
Point(301) = { 288989.8125, 4971044.0, 0.0, lc}; 
Point(302) = { 289014.8125, 4971044.0, 0.0, lc}; 
Point(303) = { 289014.8125, 4971119.0, 0.0, lc}; 
Point(304) = { 289064.8125, 4971119.0, 0.0, lc}; 
Point(305) = { 289064.8125, 4971144.0, 0.0, lc}; 
Point(306) = { 289089.8125, 4971144.0, 0.0, lc}; 
Point(307) = { 289089.8125, 4971244.0, 0.0, lc}; 
Point(308) = { 289114.8125, 4971244.0, 0.0, lc}; 
Point(309) = { 289114.8125, 4971294.0, 0.0, lc}; 
Point(310) = { 289139.8125, 4971294.0, 0.0, lc}; 
Point(311) = { 289139.8125, 4971344.0, 0.0, lc}; 
Point(312) = { 289164.8125, 4971344.0, 0.0, lc}; 
Point(313) = { 289164.8125, 4971369.0, 0.0, lc}; 
Point(314) = { 289189.8125, 4971369.0, 0.0, lc}; 
Point(315) = { 289189.8125, 4971444.0, 0.0, lc}; 
Point(316) = { 289214.8125, 4971444.0, 0.0, lc}; 
Point(317) = { 289214.8125, 4971494.0, 0.0, lc}; 
Point(318) = { 289239.8125, 4971494.0, 0.0, lc}; 
Point(319) = { 289239.8125, 4971569.0, 0.0, lc}; 
Point(320) = { 289264.8125, 4971569.0, 0.0, lc}; 
Point(321) = { 289264.8125, 4971644.0, 0.0, lc}; 
Point(322) = { 289289.8125, 4971644.0, 0.0, lc}; 
Point(323) = { 289289.8125, 4971744.0, 0.0, lc}; 
Point(324) = { 289314.8125, 4971744.0, 0.0, lc}; 
Point(325) = { 289314.8125, 4971944.0, 0.0, lc}; 
Point(326) = { 289339.8125, 4971944.0, 0.0, lc}; 
Point(327) = { 289339.8125, 4972069.0, 0.0, lc}; 
Point(328) = { 289314.8125, 4972069.0, 0.0, lc}; 
Point(329) = { 289314.8125, 4972169.0, 0.0, lc}; 
Point(330) = { 289289.8125, 4972169.0, 0.0, lc}; 
Point(331) = { 289289.8125, 4972219.0, 0.0, lc}; 
Point(332) = { 289264.8125, 4972219.0, 0.0, lc}; 
Point(333) = { 289264.8125, 4972269.0, 0.0, lc}; 
Point(334) = { 289239.8125, 4972269.0, 0.0, lc}; 
Point(335) = { 289239.8125, 4972344.0, 0.0, lc}; 
Point(336) = { 289214.8125, 4972344.0, 0.0, lc}; 
Line(1) = {1,2}; 
Line(2) = {2,3}; 
Line(3) = {3,4}; 
Line(4) = {4,5}; 
Line(5) = {5,6}; 
Line(6) = {6,7}; 
Line(7) = {7,8}; 
Line(8) = {8,9}; 
Line(9) = {9,10}; 
Line(10) = {10,11}; 
Line(11) = {11,12}; 
Line(12) = {12,13}; 
Line(13) = {13,14}; 
Line(14) = {14,15}; 
Line(15) = {15,16}; 
Line(16) = {16,17}; 
Line(17) = {17,18}; 
Line(18) = {18,19}; 
Line(19) = {19,20}; 
Line(20) = {20,21}; 
Line(21) = {21,22}; 
Line(22) = {22,23}; 
Line(23) = {23,24}; 
Line(24) = {24,25}; 
Line(25) = {25,26}; 
Line(26) = {26,27}; 
Line(27) = {27,28}; 
Line(28) = {28,29}; 
Line(29) = {29,30}; 
Line(30) = {30,31}; 
Line(31) = {31,32}; 
Line(32) = {32,33}; 
Line(33) = {33,34}; 
Line(34) = {34,35}; 
Line(35) = {35,36}; 
Line(36) = {36,37}; 
Line(37) = {37,38}; 
Line(38) = {38,39}; 
Line(39) = {39,40}; 
Line(40) = {40,41}; 
Line(41) = {41,42}; 
Line(42) = {42,43}; 
Line(43) = {43,44}; 
Line(44) = {44,45}; 
Line(45) = {45,46}; 
Line(46) = {46,47}; 
Line(47) = {47,48}; 
Line(48) = {48,49}; 
Line(49) = {49,50}; 
Line(50) = {50,51}; 
Line(51) = {51,52}; 
Line(52) = {52,53}; 
Line(53) = {53,54}; 
Line(54) = {54,55}; 
Line(55) = {55,56}; 
Line(56) = {56,57}; 
Line(57) = {57,58}; 
Line(58) = {58,59}; 
Line(59) = {59,60}; 
Line(60) = {60,61}; 
Line(61) = {61,62}; 
Line(62) = {62,63}; 
Line(63) = {63,64}; 
Line(64) = {64,65}; 
Line(65) = {65,66}; 
Line(66) = {66,67}; 
Line(67) = {67,68}; 
Line(68) = {68,69}; 
Line(69) = {69,70}; 
Line(70) = {70,71}; 
Line(71) = {71,72}; 
Line(72) = {72,73}; 
Line(73) = {73,74}; 
Line(74) = {74,75}; 
Line(75) = {75,76}; 
Line(76) = {76,77}; 
Line(77) = {77,78}; 
Line(78) = {78,79}; 
Line(79) = {79,80}; 
Line(80) = {80,81}; 
Line(81) = {81,82}; 
Line(82) = {82,83}; 
Line(83) = {83,84}; 
Line(84) = {84,85}; 
Line(85) = {85,86}; 
Line(86) = {86,87}; 
Line(87) = {87,88}; 
Line(88) = {88,89}; 
Line(89) = {89,90}; 
Line(90) = {90,91}; 
Line(91) = {91,92}; 
Line(92) = {92,93}; 
Line(93) = {93,94}; 
Line(94) = {94,95}; 
Line(95) = {95,96}; 
Line(96) = {96,97}; 
Line(97) = {97,98}; 
Line(98) = {98,99}; 
Line(99) = {99,100}; 
Line(100) = {100,101}; 
Line(101) = {101,102}; 
Line(102) = {102,103}; 
Line(103) = {103,104}; 
Line(104) = {104,105}; 
Line(105) = {105,106}; 
Line(106) = {106,107}; 
Line(107) = {107,108}; 
Line(108) = {108,109}; 
Line(109) = {109,110}; 
Line(110) = {110,111}; 
Line(111) = {111,112}; 
Line(112) = {112,113}; 
Line(113) = {113,114}; 
Line(114) = {114,115}; 
Line(115) = {115,116}; 
Line(116) = {116,117}; 
Line(117) = {117,118}; 
Line(118) = {118,119}; 
Line(119) = {119,120}; 
Line(120) = {120,121}; 
Line(121) = {121,122}; 
Line(122) = {122,123}; 
Line(123) = {123,124}; 
Line(124) = {124,125}; 
Line(125) = {125,126}; 
Line(126) = {126,127}; 
Line(127) = {127,128}; 
Line(128) = {128,129}; 
Line(129) = {129,130}; 
Line(130) = {130,131}; 
Line(131) = {131,132}; 
Line(132) = {132,133}; 
Line(133) = {133,134}; 
Line(134) = {134,135}; 
Line(135) = {135,136}; 
Line(136) = {136,137}; 
Line(137) = {137,138}; 
Line(138) = {138,139}; 
Line(139) = {139,140}; 
Line(140) = {140,141}; 
Line(141) = {141,142}; 
Line(142) = {142,143}; 
Line(143) = {143,144}; 
Line(144) = {144,145}; 
Line(145) = {145,146}; 
Line(146) = {146,147}; 
Line(147) = {147,148}; 
Line(148) = {148,149}; 
Line(149) = {149,150}; 
Line(150) = {150,151}; 
Line(151) = {151,152}; 
Line(152) = {152,153}; 
Line(153) = {153,154}; 
Line(154) = {154,155}; 
Line(155) = {155,156}; 
Line(156) = {156,157}; 
Line(157) = {157,158}; 
Line(158) = {158,159}; 
Line(159) = {159,160}; 
Line(160) = {160,161}; 
Line(161) = {161,162}; 
Line(162) = {162,163}; 
Line(163) = {163,164}; 
Line(164) = {164,165}; 
Line(165) = {165,166}; 
Line(166) = {166,167}; 
Line(167) = {167,168}; 
Line(168) = {168,169}; 
Line(169) = {169,170}; 
Line(170) = {170,171}; 
Line(171) = {171,172}; 
Line(172) = {172,173}; 
Line(173) = {173,174}; 
Line(174) = {174,175}; 
Line(175) = {175,176}; 
Line(176) = {176,177}; 
Line(177) = {177,178}; 
Line(178) = {178,179}; 
Line(179) = {179,180}; 
Line(180) = {180,181}; 
Line(181) = {181,182}; 
Line(182) = {182,183}; 
Line(183) = {183,184}; 
Line(184) = {184,185}; 
Line(185) = {185,186}; 
Line(186) = {186,187}; 
Line(187) = {187,188}; 
Line(188) = {188,189}; 
Line(189) = {189,190}; 
Line(190) = {190,191}; 
Line(191) = {191,192}; 
Line(192) = {192,193}; 
Line(193) = {193,194}; 
Line(194) = {194,195}; 
Line(195) = {195,196}; 
Line(196) = {196,197}; 
Line(197) = {197,198}; 
Line(198) = {198,199}; 
Line(199) = {199,200}; 
Line(200) = {200,201}; 
Line(201) = {201,202}; 
Line(202) = {202,203}; 
Line(203) = {203,204}; 
Line(204) = {204,205}; 
Line(205) = {205,206}; 
Line(206) = {206,207}; 
Line(207) = {207,208}; 
Line(208) = {208,209}; 
Line(209) = {209,210}; 
Line(210) = {210,211}; 
Line(211) = {211,212}; 
Line(212) = {212,213}; 
Line(213) = {213,214}; 
Line(214) = {214,215}; 
Line(215) = {215,216}; 
Line(216) = {216,217}; 
Line(217) = {217,218}; 
Line(218) = {218,219}; 
Line(219) = {219,220}; 
Line(220) = {220,221}; 
Line(221) = {221,222}; 
Line(222) = {222,223}; 
Line(223) = {223,224}; 
Line(224) = {224,225}; 
Line(225) = {225,226}; 
Line(226) = {226,227}; 
Line(227) = {227,228}; 
Line(228) = {228,229}; 
Line(229) = {229,230}; 
Line(230) = {230,231}; 
Line(231) = {231,232}; 
Line(232) = {232,233}; 
Line(233) = {233,234}; 
Line(234) = {234,235}; 
Line(235) = {235,236}; 
Line(236) = {236,237}; 
Line(237) = {237,238}; 
Line(238) = {238,239}; 
Line(239) = {239,240}; 
Line(240) = {240,241}; 
Line(241) = {241,242}; 
Line(242) = {242,243}; 
Line(243) = {243,244}; 
Line(244) = {244,245}; 
Line(245) = {245,246}; 
Line(246) = {246,247}; 
Line(247) = {247,248}; 
Line(248) = {248,249}; 
Line(249) = {249,250}; 
Line(250) = {250,251}; 
Line(251) = {251,252}; 
Line(252) = {252,253}; 
Line(253) = {253,254}; 
Line(254) = {254,255}; 
Line(255) = {255,256}; 
Line(256) = {256,257}; 
Line(257) = {257,258}; 
Line(258) = {258,259}; 
Line(259) = {259,260}; 
Line(260) = {260,261}; 
Line(261) = {261,262}; 
Line(262) = {262,263}; 
Line(263) = {263,264}; 
Line(264) = {264,265}; 
Line(265) = {265,266}; 
Line(266) = {266,267}; 
Line(267) = {267,268}; 
Line(268) = {268,269}; 
Line(269) = {269,270}; 
Line(270) = {270,271}; 
Line(271) = {271,272}; 
Line(272) = {272,273}; 
Line(273) = {273,274}; 
Line(274) = {274,275}; 
Line(275) = {275,276}; 
Line(276) = {276,277}; 
Line(277) = {277,278}; 
Line(278) = {278,279}; 
Line(279) = {279,280}; 
Line(280) = {280,281}; 
Line(281) = {281,282}; 
Line(282) = {282,283}; 
Line(283) = {283,284}; 
Line(284) = {284,285}; 
Line(285) = {285,286}; 
Line(286) = {286,287}; 
Line(287) = {287,288}; 
Line(288) = {288,289}; 
Line(289) = {289,290}; 
Line(290) = {290,291}; 
Line(291) = {291,292}; 
Line(292) = {292,293}; 
Line(293) = {293,294}; 
Line(294) = {294,295}; 
Line(295) = {295,296}; 
Line(296) = {296,297}; 
Line(297) = {297,298}; 
Line(298) = {298,299}; 
Line(299) = {299,300}; 
Line(300) = {300,301}; 
Line(301) = {301,302}; 
Line(302) = {302,303}; 
Line(303) = {303,304}; 
Line(304) = {304,305}; 
Line(305) = {305,306}; 
Line(306) = {306,307}; 
Line(307) = {307,308}; 
Line(308) = {308,309}; 
Line(309) = {309,310}; 
Line(310) = {310,311}; 
Line(311) = {311,312}; 
Line(312) = {312,313}; 
Line(313) = {313,314}; 
Line(314) = {314,315}; 
Line(315) = {315,316}; 
Line(316) = {316,317}; 
Line(317) = {317,318}; 
Line(318) = {318,319}; 
Line(319) = {319,320}; 
Line(320) = {320,321}; 
Line(321) = {321,322}; 
Line(322) = {322,323}; 
Line(323) = {323,324}; 
Line(324) = {324,325}; 
Line(325) = {325,326}; 
Line(326) = {326,327}; 
Line(327) = {327,328}; 
Line(328) = {328,329}; 
Line(329) = {329,330}; 
Line(330) = {330,331}; 
Line(331) = {331,332}; 
Line(332) = {332,333}; 
Line(333) = {333,334}; 
Line(334) = {334,335}; 
Line(335) = {335,336}; 
Line(336) = {336,1}; 
Curve Loop(1) = {1:336}; 
Plane Surface(1) = {1}; 
Physical Curve(1) = {1:336}; 
Physical Surface(1) = {1}; 
