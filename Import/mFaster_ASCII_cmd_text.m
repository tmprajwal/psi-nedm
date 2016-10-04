#!/usr/local/bin/wolframscript
Print["Working Directory:",curDir=SetDirectory["/home/prajwal/Dropbox/nEDM/psi-nedm/Import"]];
Print["Meta file Directory:",metaDir="/home/prajwal/nEDM/RawData"];
Print["Rawdata (faster) Directory:",fasDir="/home/prajwal/nEDM/RawData"];
Print["Rawdata (ASCII) Directory:",AscDir="/home/prajwal/Dropbox/nEDM/rawdata"];
datFile={{9497,"2015/01/06"},{9498,"2015/01/09"},{9499,"2015/01/09"},{9500,"2015/01/09"},{9501,"2015/01/09"},{9504,"2015/01/14"},{9505,"2015/01/19"},{9506,"2015/01/19"},{9507,"2015/01/21"},{9508,"2015/01/22"},{9511,"2015/01/23"},{9512,"2015/01/26"},{9513,"2015/01/27"},{9514,"2015/01/28"},{9515,"2015/01/30"},{9516,"2015/02/06"},{9517,"2015/02/06"},{9518,"2015/02/09"},{9519,"2015/02/09"},{9520,"2015/02/10"},{9521,"2015/02/10"},{9522,"2015/02/10"},{9523,"2015/02/11"},{9524,"2015/02/11"},{9525,"2015/02/11"},{9526,"2015/02/11"},{9527,"2015/02/11"},{9529,"2015/02/11"},{9530,"2015/02/11"},{9531,"2015/02/11"},{9532,"2015/02/11"},{9533,"2015/02/11"},{9534,"2015/02/11"},{9535,"2015/02/11"},{9536,"2015/02/11"},{9537,"2015/02/11"},{9538,"2015/02/11"},{9539,"2015/02/12"},{9540,"2015/02/12"},{9541,"2015/02/12"},{9542,"2015/02/12"},{9543,"2015/02/12"},{9544,"2015/02/12"},{9545,"2015/02/12"},{9546,"2015/02/13"},{9547,"2015/02/13"},{9548,"2015/02/13"},{9549,"2015/02/13"},{9550,"2015/02/13"},{9551,"2015/02/16"},{9552,"2015/02/16"},{9553,"2015/02/16"},{9555,"2015/02/16"},{9557,"2015/02/16"},{9558,"2015/02/16"},{9559,"2015/02/16"},{9560,"2015/02/16"},{9561,"2015/02/16"},{9562,"2015/02/16"},{9563,"2015/02/16"},{9564,"2015/02/16"},{9565,"2015/02/16"},{9566,"2015/02/16"},{9567,"2015/02/16"},{9568,"2015/02/16"},{9569,"2015/02/17"},{9571,"2015/02/17"},{9572,"2015/02/17"},{9573,"2015/02/18"},{9574,"2015/02/18"},{9575,"2015/02/18"},{9576,"2015/02/18"},{9577,"2015/02/18"},{9578,"2015/02/18"},{9579,"2015/02/19"},{9580,"2015/02/20"},{9581,"2015/02/20"},{9582,"2015/02/20"},{9583,"2015/02/20"},{9584,"2015/02/20"},{9585,"2015/02/21"},{9586,"2015/02/21"},{9588,"2015/02/23"},{9589,"2015/02/24"},{9590,"2015/02/25"},{9591,"2015/02/25"},{9592,"2015/02/25"},{9593,"2015/02/25"},{9594,"2015/03/02"},{9595,"2015/03/02"},{9726,"2015/05/26"},{9727,"2015/05/26"},{9728,"2015/05/27"},{9729,"2015/05/27"},{9732,"2015/05/27"},{9733,"2015/05/27"},{9734,"2015/05/27"},{9736,"2015/05/29"},{9737,"2015/05/29"},{9738,"2015/05/29"},{9739,"2015/05/29"},{9740,"2015/05/29"},{9741,"2015/05/29"},{9742,"2015/05/30"},{9743,"2015/05/30"},{9744,"2015/05/31"},{9745,"2015/06/01"},{9746,"2015/06/01"},{9747,"2015/06/01"},{9748,"2015/06/01"},{9749,"2015/06/01"},{9750,"2015/06/01"},{9751,"2015/06/01"},{9752,"2015/06/01"},{9753,"2015/06/01"},{9754,"2015/06/01"},{9755,"2015/06/01"},{9756,"2015/06/01"},{9757,"2015/06/01"},{9758,"2015/06/02"},{9759,"2015/06/02"},{9760,"2015/06/02"},{9761,"2015/06/02"},{9762,"2015/06/02"},{9763,"2015/06/02"},{9764,"2015/06/02"},{9765,"2015/06/02"},{9766,"2015/06/02"},{9767,"2015/06/02"},{9768,"2015/06/02"},{9769,"2015/06/02"},{9770,"2015/06/02"},{9779,"2015/06/03"},{9781,"2015/06/03"},{9782,"2015/06/03"},{9783,"2015/06/03"},{9784,"2015/06/03"},{9785,"2015/06/03"},{9786,"2015/06/03"},{9787,"2015/06/03"},{9788,"2015/06/04"},{9789,"2015/06/05"},{9790,"2015/06/05"},{9791,"2015/06/08"},{9792,"2015/06/08"},{9793,"2015/06/08"},{9794,"2015/06/08"},{9795,"2015/06/08"},{9799,"2015/06/08"},{9800,"2015/06/09"},{9801,"2015/06/10"},{9802,"2015/06/10"},{9803,"2015/06/10"},{9804,"2015/06/10"},{9805,"2015/06/12"},{9806,"2015/06/12"},{9807,"2015/06/12"},{9808,"2015/06/12"},{9809,"2015/06/15"},{9810,"2015/06/15"},{9812,"2015/06/15"},{9813,"2015/06/15"},{9814,"2015/06/15"},{9815,"2015/06/15"},{9816,"2015/06/15"},{9817,"2015/06/16"},{9818,"2015/06/16"},{9819,"2015/06/16"},{9820,"2015/06/17"},{9821,"2015/06/17"},{9822,"2015/06/18"},{9823,"2015/06/18"},{9824,"2015/06/18"},{9825,"2015/06/18"},{9826,"2015/06/19"},{9827,"2015/06/19"},{9828,"2015/06/19"},{9829,"2015/06/19"},{9830,"2015/06/22"},{9831,"2015/06/22"},{9832,"2015/06/22"},{9833,"2015/06/23"},{9834,"2015/06/24"},{9835,"2015/06/24"},{9836,"2015/06/26"},{9837,"2015/06/26"},{9838,"2015/06/26"},{9839,"2015/06/26"},{9840,"2015/06/26"},{9842,"2015/06/26"},{9843,"2015/06/26"},{9844,"2015/06/29"},{9845,"2015/06/29"},{9846,"2015/06/30"},{9849,"2015/06/30"},{9850,"2015/06/30"},{9851,"2015/07/01"},{9852,"2015/07/02"},{9853,"2015/07/02"},{9854,"2015/07/02"},{9855,"2015/07/02"},{9856,"2015/07/03"},{9857,"2015/07/03"},{9858,"2015/07/03"},{9860,"2015/07/03"},{9861,"2015/07/03"},{9862,"2015/07/03"},{9863,"2015/07/03"},{9864,"2015/07/03"},{9865,"2015/07/03"},{9866,"2015/07/03"},{9867,"2015/07/03"},{9868,"2015/07/04"},{9869,"2015/07/06"},{9870,"2015/07/08"},{9871,"2015/07/08"},{9872,"2015/07/08"},{9873,"2015/07/08"},{9874,"2015/07/09"},{9875,"2015/07/10"},{9876,"2015/07/10"},{9877,"2015/07/10"},{9878,"2015/07/10"},{9879,"2015/07/10"},{9882,"2015/07/10"},{9883,"2015/07/10"},{9884,"2015/07/11"},{9885,"2015/07/13"},{9886,"2015/07/14"},{9887,"2015/07/14"},{9888,"2015/07/14"},{9889,"2015/07/15"},{9890,"2015/07/15"},{9891,"2015/07/15"},{9893,"2015/07/16"},{9894,"2015/07/16"},{9895,"2015/07/16"},{9897,"2015/07/16"},{9898,"2015/07/16"},{9899,"2015/07/16"},{9900,"2015/07/16"},{9901,"2015/07/16"},{9904,"2015/07/16"},{9905,"2015/07/16"},{9906,"2015/07/16"},{9907,"2015/07/17"},{9908,"2015/07/17"},{9909,"2015/07/17"},{9912,"2015/07/17"},{9913,"2015/07/17"},{9914,"2015/07/18"},{9915,"2015/07/18"},{9916,"2015/07/19"},{9917,"2015/07/19"},{9918,"2015/07/19"},{9919,"2015/07/19"},{9920,"2015/07/20"},{9921,"2015/07/20"},{9922,"2015/07/20"},{9923,"2015/07/20"},{9924,"2015/07/20"},{9925,"2015/07/20"},{9926,"2015/07/20"},{9927,"2015/07/20"},{9928,"2015/07/20"},{9929,"2015/07/20"},{9930,"2015/07/20"},{9931,"2015/07/21"},{9932,"2015/07/21"},{9933,"2015/07/21"},{9934,"2015/07/21"},{9935,"2015/07/21"},{9936,"2015/07/21"},{9937,"2015/07/21"},{9938,"2015/07/21"},{9939,"2015/07/22"},{9940,"2015/07/22"},{9941,"2015/07/22"},{9943,"2015/07/22"},{9944,"2015/07/22"},{9945,"2015/07/22"},{9946,"2015/07/22"},{9947,"2015/07/22"},{9948,"2015/07/22"},{9949,"2015/07/22"},{9950,"2015/07/22"},{9951,"2015/07/22"},{9952,"2015/07/23"},{9953,"2015/07/23"},{9955,"2015/07/23"},{9956,"2015/07/23"},{9957,"2015/07/23"},{9958,"2015/07/23"},{9959,"2015/07/23"},{9960,"2015/07/23"},{9961,"2015/07/23"},{9962,"2015/07/23"},{9963,"2015/07/23"},{9964,"2015/07/23"},{9965,"2015/07/23"},{9966,"2015/07/23"},{9967,"2015/07/23"},{9968,"2015/07/23"},{9969,"2015/07/23"},{9970,"2015/07/23"},{9971,"2015/07/23"},{9972,"2015/07/23"},{9973,"2015/07/24"},{9974,"2015/07/24"},{9975,"2015/07/24"},{9976,"2015/07/24"},{9977,"2015/07/24"},{9978,"2015/07/24"},{9980,"2015/07/24"},{9981,"2015/07/24"},{9982,"2015/07/24"},{9983,"2015/07/24"},{9984,"2015/07/24"},{9986,"2015/07/24"},{9987,"2015/07/25"},{9988,"2015/07/25"},{9989,"2015/07/25"},{9990,"2015/07/25"},{9992,"2015/07/25"},{9993,"2015/07/25"},{9994,"2015/07/25"},{9995,"2015/07/25"},{9997,"2015/07/25"},{9998,"2015/07/25"},{9999,"2015/07/25"},{10000,"2015/07/26"},{10001,"2015/07/26"},{10002,"2015/07/26"},{10003,"2015/07/26"},{10004,"2015/07/26"},{10005,"2015/07/26"},{10006,"2015/07/26"},{10007,"2015/07/26"},{10008,"2015/07/26"},{10009,"2015/07/26"},{10010,"2015/07/26"},{10011,"2015/07/26"},{10012,"2015/07/26"},{10013,"2015/07/26"},{10014,"2015/07/26"},{10015,"2015/07/26"},{10016,"2015/07/26"},{10017,"2015/07/26"},{10018,"2015/07/26"},{10019,"2015/07/26"},{10021,"2015/07/27"},{10022,"2015/07/27"},{10023,"2015/07/27"},{10024,"2015/07/27"},{10025,"2015/07/27"},{10026,"2015/07/27"},{10027,"2015/07/27"},{10028,"2015/07/27"},{10029,"2015/07/27"},{10030,"2015/07/27"},{10031,"2015/07/27"},{10032,"2015/07/27"},{10033,"2015/07/28"},{10034,"2015/07/28"},{10035,"2015/07/28"},{10036,"2015/07/28"},{10037,"2015/07/28"},{10038,"2015/07/29"},{10039,"2015/07/29"},{10040,"2015/07/29"},{10041,"2015/07/29"},{10042,"2015/07/29"},{10043,"2015/07/29"},{10044,"2015/07/29"},{10045,"2015/07/29"},{10046,"2015/07/29"},{10047,"2015/07/29"},{10048,"2015/07/30"},{10049,"2015/07/30"},{10050,"2015/07/30"},{10051,"2015/07/30"},{10052,"2015/07/30"},{10053,"2015/07/31"},{10054,"2015/07/31"},{10055,"2015/07/31"},{10056,"2015/07/31"},{10057,"2015/07/31"},{10059,"2015/08/03"},{10060,"2015/08/03"},{10061,"2015/08/03"},{10062,"2015/08/03"},{10063,"2015/08/03"},{10064,"2015/08/03"},{10065,"2015/08/03"},{10066,"2015/08/04"},{10067,"2015/08/04"},{10069,"2015/08/04"},{10070,"2015/08/04"},{10071,"2015/08/04"},{10072,"2015/08/05"},{10073,"2015/08/05"},{10074,"2015/08/05"},{10075,"2015/08/05"},{10079,"2015/08/05"},{10080,"2015/08/05"},{10081,"2015/08/05"},{10082,"2015/08/05"},{10083,"2015/08/05"},{10084,"2015/08/05"},{10085,"2015/08/06"},{10086,"2015/08/06"},{10087,"2015/08/06"},{10088,"2015/08/06"},{10089,"2015/08/07"},{10090,"2015/08/07"},{10091,"2015/08/07"},{10092,"2015/08/07"},{10093,"2015/08/07"},{10094,"2015/08/08"},{10095,"2015/08/09"},{10096,"2015/08/11"},{10097,"2015/08/11"},{10098,"2015/08/11"},{10099,"2015/08/11"},{10100,"2015/08/11"},{10101,"2015/08/11"},{10102,"2015/08/11"},{10103,"2015/08/11"},{10104,"2015/08/11"},{10106,"2015/08/12"},{10107,"2015/08/12"},{10108,"2015/08/13"},{10109,"2015/08/13"},{10110,"2015/08/13"},{10111,"2015/08/13"},{10112,"2015/08/13"},{10113,"2015/08/13"},{10114,"2015/08/13"},{10115,"2015/08/13"},{10116,"2015/08/13"},{10117,"2015/08/13"},{10118,"2015/08/13"},{10119,"2015/08/14"},{10120,"2015/08/16"},{10121,"2015/08/16"},{10122,"2015/08/16"},{10123,"2015/08/16"},{10124,"2015/08/17"},{10125,"2015/08/19"},{10126,"2015/08/19"},{10127,"2015/08/20"},{10128,"2015/08/20"},{10129,"2015/08/20"},{10130,"2015/08/20"},{10131,"2015/08/20"},{10132,"2015/08/20"},{10133,"2015/08/20"},{10134,"2015/08/20"},{10135,"2015/08/21"},{10136,"2015/08/23"},{10137,"2015/08/23"},{10138,"2015/08/24"},{10139,"2015/08/25"},{10140,"2015/08/25"},{10141,"2015/08/26"},{10142,"2015/08/26"},{10143,"2015/08/26"},{10144,"2015/08/26"},{10145,"2015/08/26"},{10146,"2015/08/26"},{10147,"2015/08/26"},{10148,"2015/08/27"},{10149,"2015/08/27"},{10150,"2015/08/27"},{10151,"2015/08/27"},{10152,"2015/08/27"},{10153,"2015/08/27"},{10154,"2015/08/27"},{10155,"2015/08/27"},{10156,"2015/08/27"},{10157,"2015/08/27"},{10158,"2015/08/27"},{10160,"2015/08/27"},{10161,"2015/08/27"},{10162,"2015/08/28"},{10163,"2015/08/28"},{10164,"2015/08/29"},{10165,"2015/08/29"},{10166,"2015/08/29"},{10168,"2015/08/31"},{10169,"2015/08/31"},{10170,"2015/09/01"},{10171,"2015/09/01"},{10173,"2015/09/01"},{10174,"2015/09/01"},{10176,"2015/09/01"},{10177,"2015/09/01"},{10178,"2015/09/02"},{10179,"2015/09/03"},{10180,"2015/09/03"},{10181,"2015/09/03"},{10182,"2015/09/03"},{10183,"2015/09/03"},{10184,"2015/09/03"},{10185,"2015/09/03"},{10186,"2015/09/03"},{10187,"2015/09/03"},{10188,"2015/09/04"},{10189,"2015/09/04"},{10190,"2015/09/05"},{10191,"2015/09/05"},{10192,"2015/09/07"},{10193,"2015/09/07"},{10194,"2015/09/07"},{10195,"2015/09/07"},{10196,"2015/09/08"},{10197,"2015/09/08"},{10198,"2015/09/08"},{10199,"2015/09/08"},{10200,"2015/09/10"},{10202,"2015/09/10"},{10203,"2015/09/10"},{10204,"2015/09/10"},{10205,"2015/09/10"},{10206,"2015/09/10"},{10207,"2015/09/10"},{10208,"2015/09/13"},{10209,"2015/09/13"},{10210,"2015/09/13"},{10211,"2015/09/15"},{10212,"2015/09/15"},{10213,"2015/09/15"},{10214,"2015/09/15"},{10215,"2015/09/15"},{10216,"2015/09/15"},{10217,"2015/09/15"},{10219,"2015/09/15"},{10220,"2015/09/18"},{10221,"2015/09/18"},{10222,"2015/09/18"},{10223,"2015/09/18"},{10224,"2015/09/18"},{10225,"2015/09/20"},{10226,"2015/09/20"},{10227,"2015/09/20"},{10228,"2015/09/22"},{10229,"2015/09/22"},{10230,"2015/09/22"},{10231,"2015/09/22"},{10232,"2015/09/23"},{10233,"2015/09/23"},{10234,"2015/09/23"},{10235,"2015/09/23"},{10236,"2015/09/23"},{10237,"2015/09/23"},{10238,"2015/09/23"},{10239,"2015/09/23"},{10240,"2015/09/25"},{10241,"2015/09/25"},{10242,"2015/09/25"},{10243,"2015/09/25"},{10244,"2015/09/25"},{10246,"2015/09/25"},{10247,"2015/09/28"},{10248,"2015/09/28"},{10249,"2015/09/28"},{10250,"2015/09/28"},{10251,"2015/09/29"},{10252,"2015/09/30"},{10253,"2015/09/30"},{10254,"2015/09/30"},{10258,"2015/09/30"},{10260,"2015/09/30"},{10261,"2015/09/30"},{10262,"2015/10/02"},{10263,"2015/10/03"},{10264,"2015/10/03"},{10265,"2015/10/04"},{10267,"2015/10/05"},{10268,"2015/10/05"},{10269,"2015/10/05"},{10270,"2015/10/05"},{10271,"2015/10/06"},{10272,"2015/10/07"},{10273,"2015/10/07"},{10274,"2015/10/09"},{10275,"2015/10/09"},{10276,"2015/10/09"},{10277,"2015/10/10"},{10278,"2015/10/10"},{10279,"2015/10/10"},{10280,"2015/10/10"},{10281,"2015/10/10"},{10284,"2015/10/11"},{10285,"2015/10/12"},{10286,"2015/10/12"},{10287,"2015/10/13"},{10288,"2015/10/13"},{10289,"2015/10/14"},{10291,"2015/10/15"},{10292,"2015/10/16"},{10293,"2015/10/16"},{10295,"2015/10/16"},{10296,"2015/10/16"},{10297,"2015/10/16"},{10298,"2015/10/18"},{10300,"2015/10/19"},{10301,"2015/10/19"},{10302,"2015/10/19"},{10305,"2015/10/20"},{10306,"2015/10/20"},{10307,"2015/10/20"},{10308,"2015/10/20"},{10310,"2015/10/20"},{10311,"2015/10/21"},{10312,"2015/10/21"},{10313,"2015/10/21"},{10315,"2015/10/21"},{10317,"2015/10/22"},{10318,"2015/10/22"},{10319,"2015/10/23"},{10320,"2015/10/23"},{10321,"2015/10/23"},{10322,"2015/10/23"},{10323,"2015/10/23"},{10324,"2015/10/23"},{10325,"2015/10/24"},{10326,"2015/10/24"},{10328,"2015/10/25"},{10330,"2015/10/25"},{10331,"2015/10/26"},{10332,"2015/10/26"},{10333,"2015/10/26"},{10334,"2015/10/27"},{10335,"2015/10/27"},{10336,"2015/10/28"},{10337,"2015/10/28"},{10338,"2015/10/29"},{10339,"2015/10/29"},{10340,"2015/10/30"},{10341,"2015/10/30"},{10342,"2015/10/30"},{10343,"2015/10/30"},{10345,"2015/10/30"},{10347,"2015/11/02"},{10348,"2015/11/02"},{10349,"2015/11/02"},{10350,"2015/11/02"},{10351,"2015/11/02"},{10352,"2015/11/02"},{10353,"2015/11/02"},{10354,"2015/11/03"},{10355,"2015/11/03"},{10357,"2015/11/03"},{10358,"2015/11/04"},{10359,"2015/11/05"},{10361,"2015/11/05"},{10362,"2015/11/05"},{10363,"2015/11/05"},{10364,"2015/11/05"},{10365,"2015/11/05"},{10366,"2015/11/05"},{10367,"2015/11/05"},{10368,"2015/11/05"},{10369,"2015/11/05"},{10370,"2015/11/05"},{10371,"2015/11/05"},{10372,"2015/11/05"},{10373,"2015/11/05"},{10374,"2015/11/05"},{10375,"2015/11/05"},{10376,"2015/11/05"},{10377,"2015/11/06"},{10379,"2015/11/06"},{10380,"2015/11/06"},{10381,"2015/11/06"},{10382,"2015/11/06"},{10383,"2015/11/06"},{10384,"2015/11/07"},{10385,"2015/11/07"},{10386,"2015/11/08"},{10387,"2015/11/10"},{10388,"2015/11/10"},{10389,"2015/11/10"},{10390,"2015/11/10"},{10391,"2015/11/11"},{10392,"2015/11/13"},{10393,"2015/11/13"},{10394,"2015/11/13"},{10396,"2015/11/13"},{10397,"2015/11/13"},{10398,"2015/11/13"},{10399,"2015/11/13"},{10400,"2015/11/13"},{10401,"2015/11/15"},{10402,"2015/11/15"},{10403,"2015/11/15"},{10404,"2015/11/15"},{10405,"2015/11/16"},{10407,"2015/11/16"},{10408,"2015/11/16"},{10409,"2015/11/16"},{10410,"2015/11/16"},{10411,"2015/11/16"},{10412,"2015/11/16"},{10413,"2015/11/16"},{10414,"2015/11/17"},{10415,"2015/11/17"},{10416,"2015/11/18"},{10417,"2015/11/18"},{10418,"2015/11/20"},{10419,"2015/11/20"},{10420,"2015/11/20"},{10421,"2015/11/22"},{10422,"2015/11/23"},{10423,"2015/11/23"},{10424,"2015/11/23"},{10425,"2015/11/23"},{10426,"2015/11/23"},{10427,"2015/11/23"},{10428,"2015/11/23"},{10429,"2015/11/23"},{10430,"2015/11/23"},{10431,"2015/11/23"},{10432,"2015/11/23"},{10433,"2015/11/23"},{10435,"2015/11/23"},{10436,"2015/11/23"},{10439,"2015/11/23"},{10440,"2015/11/24"},{10441,"2015/11/24"},{10442,"2015/11/26"},{10443,"2015/11/26"},{10444,"2015/11/26"},{10445,"2015/11/26"},{10446,"2015/11/27"},{10448,"2015/11/27"},{10450,"2015/11/27"},{10451,"2015/11/27"},{10453,"2015/11/28"},{10454,"2015/11/30"},{10455,"2015/11/30"},{10456,"2015/11/30"},{10457,"2015/11/30"},{10458,"2015/12/01"},{10459,"2015/12/01"},{10460,"2015/12/01"},{10461,"2015/12/01"},{10462,"2015/12/01"},{10463,"2015/12/01"},{10464,"2015/12/03"},{10465,"2015/12/03"},{10466,"2015/12/04"},{10467,"2015/12/04"},{10468,"2015/12/04"},{10469,"2015/12/04"},{10470,"2015/12/05"},{10471,"2015/12/05"},{10472,"2015/12/06"},{10473,"2015/12/07"},{10474,"2015/12/07"},{10475,"2015/12/07"},{10476,"2015/12/07"},{10481,"2015/12/08"},{10482,"2015/12/08"},{10483,"2015/12/08"},{10485,"2015/12/10"},{10487,"2015/12/10"},{10488,"2015/12/10"},{10489,"2015/12/10"},{10490,"2015/12/10"},{10491,"2015/12/10"},{10492,"2015/12/10"},{10493,"2015/12/10"},{10494,"2015/12/10"},{10495,"2015/12/10"},{10496,"2015/12/10"},{10497,"2015/12/10"},{10498,"2015/12/14"},{10499,"2015/12/14"},{10500,"2015/12/14"},{10501,"2015/12/14"},{10502,"2015/12/14"},{10503,"2015/12/14"},{10504,"2015/12/14"},{10505,"2015/12/14"},{10506,"2015/12/14"},{10507,"2015/12/14"},{10508,"2015/12/14"},{10509,"2015/12/14"},{10510,"2015/12/14"},{10512,"2015/12/14"},{10513,"2015/12/14"},{10514,"2015/12/14"},{10515,"2015/12/15"},{10517,"2015/12/15"},{10518,"2015/12/15"},{10519,"2015/12/15"},{10520,"2015/12/15"},{10521,"2015/12/15"},{10522,"2015/12/15"},{10523,"2015/12/15"},{10524,"2015/12/15"},{10525,"2015/12/15"},{10526,"2015/12/15"},{10527,"2015/12/15"},{10528,"2015/12/15"},{10530,"2015/12/15"},{10531,"2015/12/15"},{10532,"2015/12/16"},{10533,"2015/12/16"},{10534,"2015/12/16"},{10535,"2015/12/16"},{10536,"2015/12/16"},{10537,"2015/12/16"},{10538,"2015/12/16"},{10539,"2015/12/16"},{10540,"2015/12/16"},{10541,"2015/12/16"},{10542,"2015/12/17"},{10543,"2015/12/17"},{10544,"2015/12/17"},{10545,"2015/12/17"},{10546,"2015/12/17"},{10547,"2015/12/17"},{10548,"2015/12/17"},{10550,"2015/12/17"},{10551,"2015/12/17"},{10552,"2015/12/17"},{10553,"2015/12/17"},{10555,"2015/12/17"},{10556,"2015/12/17"},{10557,"2015/12/17"},{10558,"2015/12/17"},{10559,"2015/12/17"},{10560,"2015/12/17"},{10561,"2015/12/17"},{10562,"2015/12/18"},{10563,"2015/12/18"},{10564,"2015/12/18"},{10565,"2015/12/18"},{10566,"2015/12/18"},{10567,"2015/12/18"},{10569,"2015/12/18"},{10570,"2015/12/18"},{10571,"2015/12/18"}};
metaStructure={Word,Real,Real,Word,Real,Real,Word,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Word,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Word,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Word,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real,Real};
runNum=Round[ToExpression[$ScriptCommandLine[[2]]]];
mmddyy=datFile[[Flatten[Position[datFile,runNum]][[1]]]][[2]];
(*Creting Directory*)
If[DirectoryQ[StringJoin[AscDir,"/",IntegerString[runNum,10,6]]],
Print[StringJoin["Folder: ",AscDir,"/",IntegerString[runNum,10,6]," exists.\n"]],
Print[StringJoin["Folder: ",AscDir,"/",IntegerString[runNum,10,6]," doesnt exists. Creating...\n"]];
Run[StringJoin["mkdir ",AscDir,"/",IntegerString[runNum,10,6]]];
];
(*Creatng Meta file*)
If[FileExistsQ[StringJoin[AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_Meta.edm"]],
Print[StringJoin["Folder: ",AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_Meta.edm"," exists.\n"]],
Print[StringJoin["Folder: ",AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_Meta.edm"," doesnt exists. Creating...\n"]];
Run[StringJoin["cp ",metaDir,"/",mmddyy,"/",IntegerString[runNum,10,6],"_Meta.edm ",AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_Meta.edm"]];
Run[StringJoin["chmod +rw ",AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_Meta.edm"]];
Run[StringJoin["sed '1,5d' < ",AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_Meta.edm > ",AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_Meta3.edm"]];
];
maxCy=ReadList[StringJoin[AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_Meta3.edm"],metaStructure][[-1]][[2]];
(*Creating ASCII faster files*)
For[i=1,i<=maxCy,i++,
Run[StringJoin["faster_disfast ",fasDir,"/",mmddyy,"/",IntegerString[runNum,10,6],"_UCNdet_",IntegerString[i,10,3],"_0001.fast -t 43 > ",AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_UCNdet_",IntegerString[i,10,3],"_0001.fast.ascii"]];
Run[StringJoin["sed -i -e '1,3d' -e 's/ns//' -e 's/q1=//' -e 's/q2=//' -e 's/q3=//' -e 's/mV.ns//' -e 's/mV.ns//' -e 's/mV.ns//' ",AscDir,"/",IntegerString[runNum,10,6],"/",IntegerString[runNum,10,6],"_UCNdet_",IntegerString[i,10,3],"_0001.fast.ascii"]];
Print["Run #",runNum,"with ",i," cycles, completed.\n"];
];
Print["Run #",runNum," with ",i-1," cycles, completed.\n"];
(*EOF*)
