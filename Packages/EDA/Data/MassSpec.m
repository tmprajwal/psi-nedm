BeginPackage["EDA`Data`MassSpec`"]

MassSpecData::usage =
	"MassSpecData is mass spectrometer data for two isotopes,
	taken by Dr. Alex Young, Dept. of Chemistry, Univ. of Toronto
	(1995, unpublished). The format is {amu,ions}, where amu
	is the mass, in atomic mass units, and ions is the number of
	ions."

MassSpecData = {{325.99, 24633.01743457931}, {325.992, 33319.53289060652}, 
   {325.994, 39068.88754532911}, {325.996, 42304.66450411614}, 
   {325.998, 43450.44687233666}, {326.0000000000001, 42929.81775535972}, 
   {326.0020000000001, 41166.36025855441}, 
   {326.0040000000001, 38583.65748728973}, 
   {326.0060000000001, 35605.2925469348}, 
   {326.0080000000001, 32654.84854285864}, 
   {326.0100000000001, 30155.90858043032}, 
   {326.0120000000001, 28299.7481555767}, 
   {326.0140000000001, 26218.67105932304}, 
   {326.0160000000001, 39230.14909238108}, 
   {326.0180000000001, 57752.60043494894}, 
   {326.0200000000002, 58762.97507343223}, 
   {326.0220000000002, 58274.82912623829}, 
   {326.0240000000002, 59646.75602726315}, 
   {326.0260000000001, 58558.83959612421}, 
   {326.0280000000002, 50655.09102447967}, 
   {326.0300000000002, 43294.12254952472}, 
   {326.0320000000002, 52167.82671334649}, 
   {326.0340000000002, 68986.2686482609}, 
   {326.0360000000002, 89540.8631669397}, 
   {326.0380000000002, 109623.0250820544}, 
   {326.0400000000003, 109040.6236248281}, 
   {326.0420000000002, 98579.9048415447}, 
   {326.0440000000003, 86864.2584119499}, 
   {326.0460000000003, 76524.13620759326}, 
   {326.0480000000002, 70609.05743882197}, 
   {326.0500000000003, 70839.98914691589}, 
   {326.0520000000003, 80549.5564334141}, 
   {326.0540000000003, 96061.7387435428}, 
   {326.0560000000003, 112267.8791063423}, 
   {326.0580000000003, 124002.661311289}, 
   {326.0600000000004, 127767.2390967893}, 
   {326.0620000000004, 127969.7227361879}, 
   {326.0640000000003, 151753.0707478645}, 
   {326.0660000000004, 180532.914031558}, 
   {326.0680000000003, 197442.8740053732}, 
   {326.0700000000004, 209769.89503558}, 
   {326.0720000000004, 219501.8130805668}, 
   {326.0740000000004, 228626.4640987216}, 
   {326.0760000000004, 238353.7776312799}, 
   {326.0780000000004, 252087.289913772}, 
   {326.0800000000005, 272545.7170329585}, 
   {326.0820000000005, 313145.726478736}, 
   {326.0840000000004, 362360.2373675203}, 
   {326.0860000000004, 417536.2748444469}, 
   {326.0880000000005, 476020.8640546514}, 
   {326.0900000000005, 541364.928058827}, 
   {326.0920000000005, 586620.7217477136}, 
   {326.0940000000005, 582629.7166987997}, 
   {326.0960000000005, 640293.8955084962}, 
   {326.0980000000005, 716574.9469645944}, 
   {326.1000000000005, 712955.866145096}, 
   {326.1020000000006, 753126.6086829765}, 
   {326.1040000000006, 882644.486635944}, 
   {326.1060000000005, 1.109997060401784*10^6}, 
   {326.1080000000005, 1.246810118401686*10^6}, 
   {326.1100000000006, 1.410249078527456*10^6}, 
   {326.1120000000006, 1.735831122256345*10^6}, 
   {326.1140000000006, 2.013577125857998*10^6}, 
   {326.1160000000006, 2.141751003175358*10^6}, 
   {326.1180000000006, 2.1298763199945*10^6}, 
   {326.1200000000007, 2.433616918448254*10^6}, 
   {326.1220000000006, 2.912441433520677*10^6}, 
   {326.1240000000007, 3.388255766512854*10^6}, 
   {326.1260000000006, 3.774048871023693*10^6}, 
   {326.1280000000006, 4.061861357924298*10^6}, 
   {326.1300000000007, 4.255599343266439*10^6}, 
   {326.1320000000007, 4.423322781343827*10^6}, 
   {326.1340000000007, 4.596404551160511*10^6}, 
   {326.1360000000007, 4.885880440416875*10^6}, 
   {326.1380000000007, 5.578470094117199*10^6}, 
   {326.1400000000008, 6.227559222177579*10^6}, 
   {326.1420000000007, 6.6245842554421*10^6}, 
   {326.1440000000007, 6.666627243115469*10^6}, 
   {326.1460000000008, 6.54443212064129*10^6}, 
   {326.1480000000007, 6.413414137646139*10^6}, 
   {326.1500000000008, 6.42381147313873*10^6}, 
   {326.1520000000008, 6.919042663105583*10^6}, 
   {326.1540000000008, 7.422314511049228*10^6}, 
   {326.1560000000008, 7.224524248959417*10^6}, 
   {326.1580000000008, 7.073205043875447*10^6}, 
   {326.1600000000008, 7.090246240368322*10^6}, 
   {326.1620000000009, 7.025144567892811*10^6}, 
   {326.1640000000008, 6.887174501328889*10^6}, 
   {326.1660000000008, 6.654872194223759*10^6}, 
   {326.1680000000008, 6.30845988387208*10^6}, 
   {326.1700000000009, 5.799186891427532*10^6}, 
   {326.1720000000009, 5.383642386759777*10^6}, 
   {326.1740000000009, 5.3873087408988*10^6}, 
   {326.1760000000009, 5.24487529741527*10^6}, 
   {326.1780000000009, 4.850394416806682*10^6}, 
   {326.1800000000009, 4.281416340070239*10^6}, 
   {326.1820000000009, 3.459054139988248*10^6}, 
   {326.1840000000009, 2.870443143264258*10^6}, 
   {326.1860000000009, 2.778725958929529*10^6}, 
   {326.1880000000009, 2.464291241311586*10^6}, 
   {326.190000000001, 2.03670136717552*10^6}, 
   {326.192000000001, 1.714747045767676*10^6}, 
   {326.194000000001, 1.459530262124535*10^6}, 
   {326.196000000001, 1.284239681952259*10^6}, 
   {326.198000000001, 1.172581689275903*10^6}, 
   {326.200000000001, 1.069408033512128*10^6}, 
   {326.202000000001, 907257.413935769}, 
   {326.204000000001, 667750.2166373352}, 
   {326.206000000001, 517443.3366764845}, 
   {326.208000000001, 434534.6395949092}, 
   {326.2100000000011, 395860.3813228682}, 
   {326.2120000000011, 385517.3550928665}, 
   {326.2140000000011, 385486.3632890583}, 
   {326.2160000000011, 386105.4748189074}, 
   {326.218000000001, 359211.405523025}, 
   {326.2200000000011, 315751.2107051792}, 
   {326.2220000000011, 260960.4869821443}, 
   {326.2240000000011, 199848.559631605}, 
   {326.2260000000011, 154945.0004140997}, 
   {326.2280000000011, 129347.9767556867}, 
   {326.2300000000012, 109299.1146861372}, 
   {326.2320000000012, 94202.8857563402}, 
   {326.2340000000012, 83463.7615171848}, 
   {326.2360000000012, 76486.21351956027}, 
   {326.2380000000012, 72674.71331435556}, 
   {326.2400000000012, 70027.01153293663}, 
   {326.2420000000012, 61179.25614888111}, 
   {326.2440000000012, 53261.73323968891}, 
   {326.2460000000012, 46228.26596248317}, 
   {326.2480000000012, 40032.67747438696}, 
   {326.2500000000013, 34628.79093252339}, 
   {326.2520000000013, 29970.42949401556}, 
   {326.2540000000013, 26011.41631598657}, 
   {326.2560000000013, 22705.57455555953}, 
   {326.2580000000013, 20006.72736985755}, 
   {326.2600000000013, 17868.69791600371}, 
   {326.2620000000013, 16245.30935112113}, 
   {326.2640000000013, 15090.38483233289}, 
   {326.2660000000013, 14357.74751676212}, 
   {326.2680000000013, 13491.03980631931}, 
   {326.2700000000014, 13344.65255736098}, 
   {326.2720000000014, 14124.10043519118}, 
   {326.2740000000014, 17752.49644030457}, 
   {326.2760000000013, 22962.07690926122}, 
   {326.2780000000014, 29119.64488776361}, 
   {326.2800000000014, 35798.96119543905}, 
   {326.2820000000014, 42573.78665191486}, 
   {326.2840000000014, 49017.88207681834}, 
   {326.2860000000014, 54705.00828977679}, 
   {326.2880000000014, 57946.78074170835}, 
   {326.2900000000015, 58870.60841157278}, 
   {326.2920000000014, 58989.74579349867}, 
   {326.2940000000015, 58375.30192153415}, 
   {326.2960000000015, 57170.61593622496}, 
   {326.2980000000014, 56944.68152839445}, 
   {326.3000000000015, 56640.35213491318}, 
   {326.3020000000015, 56259.08164483955}, 
   {326.3040000000015, 55802.32394723192}, 
   {326.3060000000015, 55271.53293114865}, 
   {326.3080000000015, 54668.16248564817}, 
   {326.3100000000016, 53993.66649978882}, 
   {326.3120000000016, 53249.49886262899}, 
   {326.3140000000015, 52437.11346322707}, 
   {326.3160000000016, 51557.96419064143}, 
   {326.3180000000015, 50613.50493393044}, 
   {326.3200000000016, 49605.1895821525}, 
   {326.3220000000016, 48534.47202436597}, 
   {326.3240000000016, 47402.80614962925}, 
   {326.3260000000016, 46211.6458470007}, 
   {326.3280000000016, 44962.4450055387}, 
   {326.3300000000016, 43656.65751430165}, 
   {326.3320000000017, 42295.73726234788}, 
   {326.3340000000016, 40881.13813873585}, 
   {326.3360000000016, 39414.31403252386}, 
   {326.3380000000017, 37896.71883277035}, 
   {326.3400000000017, 36329.80642853366}, 
   {326.3420000000017, 34715.03070887217}, 
   {326.3440000000017, 33053.84556284429}, 
   {326.3460000000017, 31347.70487950838}, 
   {326.3480000000017, 29598.0625479228}, 
   {326.3500000000017, 27273.24368085493}, 
   {326.3520000000017, 24394.48749040972}, 
   {326.3540000000018, 21787.36651227403}, 
   {326.3560000000017, 19492.98530460853}, 
   {326.3580000000017, 17552.4484255739}, 
   {326.3600000000018, 16006.86043333083}, 
   {326.3620000000018, 14897.32588603999}, 
   {326.3640000000018, 14243.99510126607}, 
   {326.3660000000018, 13923.09600548733}, 
   {326.3680000000018, 15849.29118317942}, 
   {326.3700000000019, 20483.02116829896}, 
   {326.3720000000018, 27027.61160906766}, 
   {326.3740000000018, 35010.86985440044}, 
   {326.3760000000018, 43308.39811518709}, 
   {326.3780000000018, 46381.19583974755}, 
   {326.3800000000019, 48450.31734290763}, 
   {326.3820000000019, 49600.61956178553}, 
   {326.3840000000019, 49916.95943349942}, 
   {326.3860000000019, 49484.19389516748}, 
   {326.3880000000019, 48387.17988390791}, 
   {326.390000000002, 46710.77433683886}, 
   {326.3920000000019, 44539.83419107856}, 
   {326.3940000000019, 41959.21638374515}, 
   {326.3960000000019, 39053.77785195682}, 
   {326.3980000000019, 35908.37553283178}, 
   {326.400000000002, 32607.8663634882}, 
   {326.402000000002, 29237.10728104425}, 
   {326.404000000002, 27461.90914955704}, 
   {326.406000000002, 26076.37236323604}, 
   {326.408000000002, 24701.1908227761}, 
   {326.410000000002, 23345.51713237966}, 
   {326.4120000000021, 22018.50389624918}, 
   {326.414000000002, 20729.30371858709}, 
   {326.416000000002, 19487.06920359586}, 
   {326.418000000002, 18300.95295547793}, 
   {326.4200000000021, 17180.10757843575}, 
   {326.4220000000021, 16133.68567667176}, 
   {326.4240000000021, 15170.83985438842}, 
   {326.4260000000021, 14300.72271578817}, 
   {326.4280000000021, 13085.42154947313}, 
   {326.4300000000021, 11987.5349361753}, 
   {326.4320000000021, 11048.96448901685}, 
   {326.4340000000021, 10298.70421340814}, 
   {326.4360000000021, 9765.74811475951}, 
   {326.4380000000021, 9479.0901984813}, 
   {326.4400000000022, 9467.72446998387}, 
   {326.4420000000022, 9760.64493467757}, 
   {326.4440000000022, 10386.84559797274}, 
   {326.4460000000022, 11375.32046527973}, 
   {326.4480000000022, 12755.06354200888}, 
   {326.4500000000022, 14942.90310955909}, 
   {326.4520000000022, 19765.91857777008}, 
   {326.4540000000022, 24563.88666975971}, 
   {326.4560000000022, 29173.60807601755}, 
   {326.4580000000022, 33431.88348703313}, 
   {326.4600000000023, 37175.51359329604}, 
   {326.4620000000023, 40241.29908529581}, 
   {326.4640000000023, 42466.040653522}, 
   {326.4660000000023, 39592.58266917155}, 
   {326.4680000000022, 34588.36981220787}, 
   {326.4700000000023, 29316.45161997513}, 
   {326.4720000000023, 24223.00431458361}, 
   {326.4740000000023, 19754.20411814356}, 
   {326.4760000000023, 16356.22725276527}, 
   {326.4780000000023, 14475.24994055901}, 
   {326.4800000000024, 17104.56278119642}, 
   {326.4820000000024, 26170.50001923291}, 
   {326.4840000000023, 36220.7950165342}, 
   {326.4860000000024, 46685.80224559955}, 
   {326.4880000000024, 56995.87617892819}, 
   {326.4900000000024, 66581.37128901935}, 
   {326.4920000000024, 70232.03864036289}, 
   {326.4940000000024, 65617.73436761277}, 
   {326.4960000000024, 59106.35783151045}, 
   {326.4980000000024, 51329.32009392678}, 
   {326.5000000000024, 42918.03221673267}, 
   {326.5020000000025, 34503.90526179901}, 
   {326.5040000000025, 26718.35029099667}, 
   {326.5060000000024, 20192.77836619658}, 
   {326.5080000000025, 15558.6005492696}, 
   {326.5100000000025, 14045.88693656196}, 
   {326.5120000000025, 14337.47290315595}, 
   {326.5140000000025, 15537.93072668616}, 
   {326.5160000000025, 17456.30408811893}, 
   {326.5180000000025, 19931.88935495529}, 
   {326.5200000000026, 22803.98289469628}, 
   {326.5220000000025, 25911.88107484295}, 
   {326.5240000000026, 28771.49332762977}, 
   {326.5260000000025, 29766.08499047953}, 
   {326.5280000000025, 30453.51730306389}, 
   {326.5300000000026, 30865.50930555084}, 
   {326.5320000000026, 31033.78003810838}, 
   {326.5340000000026, 30990.04854090456}, 
   {326.5360000000026, 30766.03385410736}, 
   {326.5380000000026, 30393.45501788479}, 
   {326.5400000000027, 29904.03107240487}, 
   {326.5420000000026, 29329.4810578356}, 
   {326.5440000000026, 28701.524014345}, 
   {326.5460000000027, 28620.89951322651}, 
   {326.5480000000026, 28672.56328058091}, 
   {326.5500000000027, 28720.85475295239}, 
   {326.5520000000027, 28763.2118770384}, 
   {326.5540000000027, 28797.07259953638}, 
   {326.5560000000027, 28819.87486714378}, 
   {326.5580000000027, 28829.05662655804}, 
   {326.5600000000028, 28822.05582447659}, 
   {326.5620000000028, 28796.3104075969}, 
   {326.5640000000027, 28749.25832261639}, 
   {326.5660000000027, 28678.33751623252}, 
   {326.5680000000027, 28580.98593514272}, 
   {326.5700000000028, 27996.1456618251}, 
   {326.5720000000028, 27343.49710669627}, 
   {326.5740000000028, 26641.44349482627}, 
   {326.5760000000028, 25897.79097284474}, 
   {326.5780000000028, 25120.34568738129}, 
   {326.5800000000028, 24316.91378506557}, 
   {326.5820000000029, 23495.3014125272}, 
   {326.5840000000028, 22663.31471639581}, 
   {326.5860000000028, 21828.75984330102}, 
   {326.5880000000028, 20999.44293987249}, 
   {326.5900000000029, 20183.17015273982}, 
   {326.5920000000029, 19387.74762853266}, 
   {326.5940000000029, 18620.98151388062}, 
   {326.5960000000029, 17890.67795541335}, 
   {326.5980000000029, 17204.64309976047}, 
   {326.6000000000029, 16570.68309355161}, 
   {326.6020000000029, 15996.60408341641}, 
   {326.604000000003, 15490.21221598448}, 
   {326.6060000000029, 15059.31363788548}, 
   {326.6080000000029, 14711.71449574901}, 
   {326.610000000003, 14455.22093620471}, 
   {326.612000000003, 14297.63910588222}, 
   {326.614000000003, 14271.58515020572}, 
   {326.616000000003, 14277.88305864634}, 
   {326.618000000003, 14277.88305864634}, 
   {326.6200000000031, 14277.88305864634}, 
   {326.622000000003, 14277.88305864634}, 
   {326.624000000003, 14277.88305864634}, 
   {326.626000000003, 14277.88305864634}, 
   {326.628000000003, 14277.88305864634}, 
   {326.6300000000031, 14277.88305864634}, 
   {326.6320000000031, 14277.88305864634}, 
   {326.6340000000031, 14277.88305864634}, 
   {326.6360000000031, 14277.88305864634}, 
   {326.6380000000031, 14277.88305864634}, 
   {326.6400000000031, 14277.88305864634}, 
   {326.6420000000031, 14277.88305864634}, 
   {326.6440000000031, 14277.88305864634}, 
   {326.6460000000031, 14277.88305864634}, 
   {326.6480000000031, 14210.26626215953}, 
   {326.6500000000032, 12624.55534830406}, 
   {326.6520000000032, 11167.36442302156}, 
   {326.6540000000032, 9889.68571771221}, 
   {326.6560000000032, 8842.51146377619}, 
   {326.6580000000032, 8076.83389261367}, 
   {326.6600000000032, 7643.645235624827}, 
   {326.6620000000032, 7593.937724209842}, 
   {326.6640000000032, 7978.703589768891}, 
   {326.6660000000032, 8848.93506370215}, 
   {326.6680000000032, 10255.6243774098}, 
   {326.6700000000033, 12249.76376229201}, 
   {326.6720000000033, 15445.01042132044}, 
   {326.6740000000033, 21243.43687432185}, 
   {326.6760000000032, 27279.75760449524}, 
   {326.6780000000033, 33417.98643819574}, 
   {326.6800000000033, 39522.13720177855}, 
   {326.6820000000033, 45456.22372159878}, 
   {326.6840000000033, 51084.25982401162}, 
   {326.6860000000033, 56270.25933537219}, 
   {326.6880000000033, 56530.49492514154}, 
   {326.6900000000034, 54765.46667085765}, 
   {326.6920000000034, 52100.75173149564}, 
   {326.6940000000034, 48713.1395741241}, 
   {326.6960000000034, 44779.41966581161}, 
   {326.6980000000033, 40476.38147362681}, 
   {326.7000000000034, 35980.81446463827}, 
   {326.7020000000034, 31469.50810591462}, 
   {326.7040000000034, 27119.25186452445}, 
   {326.7060000000034, 23106.83520753636}, 
   {326.7080000000034, 19609.04760201897}, 
   {326.7100000000035, 16802.67851504084}, 
   {326.7120000000035, 14864.51741367062}, 
   {326.7140000000035, 14226.45086972581}, 
   {326.7160000000035, 14277.88305864634}, 
   {326.7180000000034, 14277.88305864634}, 
   {326.7200000000035, 14277.88305864634}, 
   {326.7220000000035, 14277.88305864634}, 
   {326.7240000000035, 14277.88305864634}, 
   {326.7260000000035, 14277.88305864634}, 
   {326.7280000000035, 14277.88305864634}, 
   {326.7300000000036, 14277.88305864634}, 
   {326.7320000000036, 14277.88305864634}, 
   {326.7340000000035, 14277.88305864634}, 
   {326.7360000000036, 14277.88305864634}, 
   {326.7380000000036, 14277.88305864634}, 
   {326.7400000000036, 14277.88305864634}, 
   {326.7420000000036, 14277.88305864634}, 
   {326.7440000000036, 14277.88305864634}, 
   {326.7460000000036, 14277.88305864634}, 
   {326.7480000000036, 14277.88305864634}, 
   {326.7500000000036, 14277.88305864634}, 
   {326.7520000000037, 14277.88305864634}, 
   {326.7540000000037, 14277.88305864634}, 
   {326.7560000000036, 14277.88305864634}, 
   {326.7580000000037, 14277.88305864634}, 
   {326.7600000000037, 14277.88305864634}, 
   {326.7620000000037, 14277.88305864634}, 
   {326.7640000000037, 14277.88305864634}, 
   {326.7660000000037, 14277.88305864634}, 
   {326.7680000000037, 14277.88305864634}, 
   {326.7700000000038, 14277.88305864634}, 
   {326.7720000000037, 14277.88305864634}, 
   {326.7740000000038, 14277.88305864634}, 
   {326.7760000000037, 14277.88305864634}, 
   {326.7780000000037, 14277.88305864634}, 
   {326.7800000000038, 14277.88305864634}, 
   {326.7820000000038, 14277.88305864634}, 
   {326.7840000000038, 14277.88305864634}, 
   {326.7860000000038, 14277.88305864634}, 
   {326.7880000000038, 14277.88305864634}, 
   {326.7900000000039, 14277.88305864634}, 
   {326.7920000000038, 14277.88305864634}, 
   {326.7940000000038, 14277.88305864634}, 
   {326.7960000000039, 14277.88305864634}, 
   {326.7980000000038, 14277.88305864634}, 
   {326.8000000000039, 14277.88305864634}, 
   {326.8020000000039, 14277.88305864634}, 
   {326.8040000000039, 14277.88305864634}, 
   {326.8060000000039, 14277.88305864634}, 
   {326.8080000000039, 14277.88305864634}, 
   {326.8100000000039, 14277.88305864634}, 
   {326.812000000004, 14277.88305864634}, 
   {326.8140000000039, 14277.88305864634}, 
   {326.8160000000039, 14277.88305864634}, 
   {326.8180000000039, 14277.88305864634}, 
   {326.820000000004, 14277.88305864634}, 
   {326.822000000004, 14277.88305864634}, 
   {326.824000000004, 14277.88305864634}, 
   {326.826000000004, 14277.88305864634}, 
   {326.828000000004, 14277.88305864634}, 
   {326.830000000004, 14277.88305864634}, 
   {326.832000000004, 14277.88305864634}, 
   {326.834000000004, 14277.88305864634}, 
   {326.836000000004, 14277.88305864634}, 
   {326.838000000004, 14277.88305864634}, 
   {326.8400000000041, 14277.88305864634}, 
   {326.8420000000041, 14277.88305864634}, 
   {326.8440000000041, 14277.88305864634}, 
   {326.8460000000041, 14277.88305864634}, 
   {326.8480000000041, 14277.88305864634}, 
   {326.8500000000041, 14277.88305864634}, 
   {326.8520000000041, 14277.88305864634}, 
   {326.8540000000041, 14277.88305864634}, 
   {326.8560000000041, 14277.88305864634}, 
   {326.8580000000041, 14277.88305864634}, 
   {326.8600000000042, 14277.88305864634}, 
   {326.8620000000042, 14275.08577793055}, 
   {326.8640000000042, 14281.4357583784}, 
   {326.8660000000042, 14367.55678292861}, 
   {326.8680000000041, 14514.93910421883}, 
   {326.8700000000043, 14719.73974354574}, 
   {326.8720000000042, 14978.11572220603}, 
   {326.8740000000042, 15286.2240614964}, 
   {326.8760000000042, 15640.22178271353}, 
   {326.8780000000042, 16036.26590715413}, 
   {326.8800000000043, 16470.51345611488}, 
   {326.8820000000043, 16939.12145089246}, 
   {326.8840000000043, 17438.24691278358}, 
   {326.8860000000043, 17964.04686308492}, 
   {326.8880000000043, 18512.67832309318}, 
   {326.8900000000043, 19080.29831410505}, 
   {326.8920000000043, 19663.06385741722}, 
   {326.8940000000043, 20257.13197432637}, 
   {326.8960000000043, 20858.65968612921}, 
   {326.8980000000043, 21463.80401412242}, 
   {326.9000000000044, 22068.7219796027}, 
   {326.9020000000044, 22669.57060386673}, 
   {326.9040000000044, 23262.50690821121}, 
   {326.9060000000044, 23843.68791393283}, 
   {326.9080000000044, 24409.27064232828}, 
   {326.9100000000044, 24955.41211469425}, 
   {326.9120000000044, 25478.26935232743}, 
   {326.9140000000044, 25973.99937652452}, 
   {326.9160000000044, 26438.75920858221}, 
   {326.9180000000044, 26868.70586979718}, 
   {326.9200000000045, 27259.99638146614}, 
   {326.9220000000045, 27608.78776488577}, 
   {326.9240000000045, 27911.23704135275}, 
   {326.9260000000044, 28163.50123216379}, 
   {326.9280000000045, 28361.73735861558}, 
   {326.9300000000045, 28502.1024420048}, 
   {326.9320000000045, 28579.2103185094}, 
   {326.9340000000045, 28583.71495824675}, 
   {326.9360000000045, 28549.28394869083}, 
   {326.9380000000045, 28461.04419516556}, 
   {326.9400000000046, 28327.40128207836}, 
   {326.9420000000045, 28150.6965530493}, 
   {326.9440000000046, 27933.27135169844}, 
   {326.9460000000046, 27677.46702164584}, 
   {326.9480000000045, 27385.62490651158}, 
   {326.9500000000046, 27060.0863499157}, 
   {326.9520000000046, 26703.19269547828}, 
   {326.9540000000046, 26317.28528681937}, 
   {326.9560000000046, 25904.70546755906}, 
   {326.9580000000046, 25467.79458131739}, 
   {326.9600000000047, 25008.89397171444}, 
   {326.9620000000047, 24530.34498237025}, 
   {326.9640000000046, 24034.48895690491}, 
   {326.9660000000047, 23523.66723893847}, 
   {326.9680000000046, 23000.221172091}, 
   {326.9700000000047, 22466.49209998257}, 
   {326.9720000000047, 21924.82136623323}, 
   {326.9740000000047, 21377.55031446305}, 
   {326.9760000000047, 20827.0202882921}, 
   {326.9780000000047, 20275.57263134043}, 
   {326.9800000000048, 19725.54868722812}, 
   {326.9820000000048, 19179.28979957522}, 
   {326.9840000000047, 18639.13731200181}, 
   {326.9860000000047, 18107.43256812794}, 
   {326.9880000000048, 17586.51691157367}, 
   {326.9900000000048, 17078.73168595908}, 
   {326.9920000000048, 16586.41823490423}, 
   {326.9940000000048, 16111.91790202918}, 
   {326.9960000000048, 15657.57203095399}, 
   {326.9980000000048, 15225.72196529873}, 
   {327.0000000000048, 14818.70904868347}, 
   {327.0020000000048, 14438.87462472826}, 
   {327.0040000000049, 14236.80218444179}, 
   {327.0060000000048, 14172.02444355391}, 
   {327.0080000000048, 14119.27559448167}, 
   {327.0100000000049, 14077.92767949065}, 
   {327.0120000000049, 14047.35274084645}, 
   {327.0140000000049, 14026.92282081464}, 
   {327.0160000000049, 14016.00996166081}, 
   {327.0180000000049, 14013.98620565055}, 
   {327.020000000005, 14020.22359504944}, 
   {327.0220000000049, 14034.09417212307}, 
   {327.0240000000049, 14054.96997913703}, 
   {327.0260000000049, 14082.22305835689}, 
   {327.0280000000049, 14115.22545204825}, 
   {327.030000000005, 14153.3492024767}, 
   {327.032000000005, 14195.96635190781}, 
   {327.034000000005, 14242.44894260717}, 
   {327.036000000005, 13918.77913252576}, 
   {327.038000000005, 12603.37805345146}, 
   {327.0400000000051, 11238.16660377719}, 
   {327.042000000005, 9838.92966014917}, 
   {327.044000000005, 8421.45209921365}, 
   {327.046000000005, 7001.518797616852}, 
   {327.048000000005, 5594.914632005013}, 
   {327.0500000000051, 4217.424479024368}, 
   {327.0520000000051, 2884.833215321138}, 
   {327.0540000000051, 1612.925717541548}, 
   {327.0560000000051, 417.4868623318471}, 
   {327.0580000000051, -685.6984736617305}, 
   {327.0600000000051, -1680.845413792973}, 
   {327.0620000000052, -2552.169081415639}, 
   {327.0640000000051, -3283.884599883488}, 
   {327.0660000000051, -3860.207092550307}, 
   {327.0680000000051, -4265.351682769855}, 
   {327.0700000000052, -4483.533493895892}, 
   {327.0720000000052, -4498.967649282197}, 
   {327.0740000000052, -4295.869272282536}, 
   {327.0760000000052, -3858.453486250677}, 
   {327.0780000000052, -3170.935414540392}, 
   {327.0800000000052, -2217.530180505441}, 
   {327.0820000000052, -982.452907499603}, 
   {327.0840000000052, 550.0812811233628}, 
   {327.0860000000052, 2395.857262009682}, 
   {327.0880000000052, 4570.659911805583}, 
   {327.0900000000053, 7090.274107157311}, 
   {327.0920000000053, 9970.48472471109}, 
   {327.0940000000053, 13227.07664111314}, 
   {327.0960000000053, 19700.11628021205}, 
   {327.0980000000053, 26656.96882098628}, 
   {327.1000000000053, 32569.01187075921}, 
   {327.1020000000053, 37298.87701372127}, 
   {327.1040000000053, 40709.19583406291}, 
   {327.1060000000053, 42662.59991597456}, 
   {327.1080000000053, 38003.0022893519}, 
   {327.1100000000054, 32117.89784095947}, 
   {327.1120000000054, 27935.18164599512}, 
   {327.1140000000054, 26885.97105382029}, 
   {327.1160000000054, 34194.86737775398}, 
   {327.1180000000053, 50917.15776601443}, 
   {327.1200000000054, 65386.53733420704}, 
   {327.1220000000054, 72083.29176757032}, 
   {327.1240000000054, 71590.95084159153}, 
   {327.1260000000054, 71527.11569236196}, 
   {327.1280000000054, 70643.1683807678}, 
   {327.1300000000055, 67763.67784214226}, 
   {327.1320000000055, 64193.62736455592}, 
   {327.1340000000054, 60690.56382203326}, 
   {327.1360000000055, 58012.03408859882}, 
   {327.1380000000055, 56915.58503827706}, 
   {327.1400000000055, 60297.51905724938}, 
   {327.1420000000055, 67920.49748773245}, 
   {327.1440000000055, 77413.61939309498}, 
   {327.1460000000055, 88545.699324659}, 
   {327.1480000000055, 101157.158192648}, 
   {327.1500000000056, 115606.2476438677}, 
   {327.1520000000056, 130322.3813706639}, 
   {327.1540000000056, 144279.8099410181}, 
   {327.1560000000055, 154424.5900292777}, 
   {327.1580000000056, 163709.198496375}, 
   {327.1600000000056, 172849.3190266803}, 
   {327.1620000000056, 182560.6353045643}, 
   {327.1640000000056, 195524.8652819837}, 
   {327.1660000000056, 211249.8181929447}, 
   {327.1680000000056, 228925.4052107823}, 
   {327.1700000000057, 248621.4699339942}, 
   {327.1720000000057, 270407.8559610777}, 
   {327.1740000000057, 294354.4068905301}, 
   {327.1760000000056, 322466.3309264695}, 
   {327.1780000000056, 351799.8150080239}, 
   {327.1800000000057, 380654.4684264247}, 
   {327.1820000000057, 406864.2420573227}, 
   {327.1840000000057, 431244.2135964878}, 
   {327.1860000000057, 454240.9168088342}, 
   {327.1880000000057, 458964.1288692458}, 
   {327.1900000000058, 459046.3720980647}, 
   {327.1920000000057, 463538.4281193367}, 
   {327.1940000000058, 478815.9635530623}, 
   {327.1960000000058, 511254.6450192414}, 
   {327.1980000000057, 567230.1391378747}, 
   {327.2000000000058, 657780.5406797222}, 
   {327.2020000000058, 771987.1178236555}, 
   {327.2040000000058, 906946.803044422}, 
   {327.2060000000058, 1.040182228602529*10^6}, 
   {327.2080000000058, 1.148288289564812*10^6}, 
   {327.2100000000059, 1.230574280877097*10^6}, 
   {327.2120000000059, 1.265387362934558*10^6}, 
   {327.2140000000058, 1.310260908540603*10^6}, 
   {327.2160000000059, 1.411963220087171*10^6}, 
   {327.2180000000058, 1.625535347522879*10^6}, 
   {327.2200000000059, 1.894576580279939*10^6}, 
   {327.2220000000059, 2.16171847263033*10^6}, 
   {327.2240000000059, 2.273980602179915*10^6}, 
   {327.2260000000059, 2.324752014832996*10^6}, 
   {327.2280000000059, 2.370515938402942*10^6}, 
   {327.2300000000059, 2.463254722656685*10^6}, 
   {327.232000000006, 2.567591371129184*10^6}, 
   {327.2340000000059, 2.845646565633306*10^6}, 
   {327.2360000000059, 3.454383287780957*10^6}, 
   {327.238000000006, 3.773052238188907*10^6}, 
   {327.240000000006, 3.8700117748315*10^6}, 
   {327.242000000006, 3.964059092257352*10^6}, 
   {327.244000000006, 4.114351051421027*10^6}, 
   {327.246000000006, 4.236901270368364*10^6}, 
   {327.248000000006, 4.234418052453922*10^6}, 
   {327.250000000006, 4.033217819200666*10^6}, 
   {327.252000000006, 4.06886444931379*10^6}, 
   {327.2540000000061, 4.520254718652933*10^6}, 
   {327.256000000006, 4.390532272293141*10^6}, 
   {327.258000000006, 4.098342987021379*10^6}, 
   {327.2600000000061, 3.983765783828454*10^6}, 
   {327.2620000000061, 3.964802455623492*10^6}, 
   {327.2640000000061, 3.996552684709525*10^6}, 
   {327.2660000000061, 4.024758671442628*10^6}, 
   {327.2680000000061, 3.939383356595675*10^6}, 
   {327.2700000000062, 3.555910243793355*10^6}, 
   {327.2720000000061, 3.023037554672013*10^6}, 
   {327.2740000000061, 2.811502984007526*10^6}, 
   {327.2760000000061, 2.747096786463223*10^6}, 
   {327.2780000000061, 2.721903430263561*10^6}, 
   {327.2800000000062, 2.692544247554736*10^6}, 
   {327.2820000000062, 2.582761962865438*10^6}, 
   {327.2840000000062, 2.39400034525562*10^6}, 
   {327.2860000000062, 2.155863739984125*10^6}, 
   {327.2880000000062, 1.917275720834709*10^6}, 
   {327.2900000000062, 1.70161479229352*10^6}, 
   {327.2920000000062, 1.503568940536544*10^6}, 
   {327.2940000000062, 1.311425295106994*10^6}, 
   {327.2960000000062, 1.181553597722989*10^6}, 
   {327.2980000000062, 1.17985327367214*10^6}, 
   {327.3000000000063, 1.133619122462222*10^6}, 
   {327.3020000000063, 1.005549761266834*10^6}, 
   {327.3040000000063, 841880.409548775}, 
   {327.3060000000063, 649329.3734099998}, 
   {327.3080000000063, 518683.3242471516}, 
   {327.3100000000063, 519838.7801753978}, 
   {327.3120000000063, 511252.9426547436}, 
   {327.3140000000063, 473475.2143463788}, 
   {327.3160000000063, 426268.8553932568}, 
   {327.3180000000063, 397527.1804200134}, 
   {327.3200000000064, 374934.9574492611}, 
   {327.3220000000064, 346755.8586445816}, 
   {327.3240000000064, 307247.5219930449}, 
   {327.3260000000064, 240637.0510764002}, 
   {327.3280000000064, 165014.4563800752}, 
   {327.3300000000064, 147229.8242406335}, 
   {327.3320000000064, 154738.8794020693}, 
   {327.3340000000064, 144322.4265463048}, 
   {327.3360000000064, 130404.7088398123}, 
   {327.3380000000064, 115195.6707662091}, 
   {327.3400000000065, 100905.2568091125}, 
   {327.3420000000065, 89743.4114521397}, 
   {327.3440000000065, 85749.7039369309}, 
   {327.3460000000065, 85787.6176742222}, 
   {327.3480000000065, 79578.45321818927}, 
   {327.3500000000065, 69444.10412205575}, 
   {327.3520000000065, 56615.22582299275}, 
   {327.3540000000065, 42240.83013295685}, 
   {327.3560000000065, 27469.92886390469}, 
   {327.3580000000065, 14137.53736140822}, 
   {327.3600000000066, 13171.38220616753}, 
   {327.3620000000066, 14394.96526160624}, 
   {327.3640000000066, 17444.33684596984}, 
   {327.3660000000066, 21955.54727750377}, 
   {327.3680000000065, 27564.6468744535}, 
   {327.3700000000066, 33907.68595506449}, 
   {327.3720000000066, 40620.7148375822}, 
   {327.3740000000066, 40122.80825289301}, 
   {327.3760000000066, 35231.7767826367}, 
   {327.3780000000066, 29754.12123744483}, 
   {327.3800000000067, 24047.72315771201}, 
   {327.3820000000067, 18470.46408383289}, 
   {327.3840000000066, 13380.22555620203}, 
   {327.3860000000067, 9134.88911521407}, 
   {327.3880000000067, 6092.336301263601}, 
   {327.3900000000067, 4610.448654745225}};

EndPackage[]
