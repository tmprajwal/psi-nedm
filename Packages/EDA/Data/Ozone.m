BeginPackage["EDA`Data`Ozone`"]

OzoneData::usage =
	"OzoneData is data on ozone levels in the atmosphere on December 25,
	1988. The data was taken by the Total Ozone Measurement Spectrometer
	(TOMS) aboard the Nimbus 7 satellite. The 60 rows of data correspond
	to latitudes 50 degrees south to 10 degrees north in one degree bins.
	The 60 columns correspond to longitudes of 180 degrees west to 105 degrees
	west in 1.25 degree bins. The units of the data are Dobson units."

OzoneData = {{290, 289, 296, 296, 293, 301, 295, 295, 295, 296, 296,
	298, 298, 301, 300, 
   299, 301, 305, 305, 302, 298, 300, 303, 303, 303, 304, 305, 309, 310, 
   317, 323, 330, 332, 339, 352, 361, 364, 361, 359, 355, 352, 341, 339, 
   345, 348, 353, 361, 366, 364, 365, 368, 354, 347, 346, 337, 329, 329, 
   331, 329, 333}, {294, 295, 297, 297, 296, 296, 291, 293, 295, 296, 296, 
   297, 299, 298, 300, 301, 300, 305, 310, 306, 303, 301, 299, 300, 301, 
   307, 309, 313, 315, 324, 327, 333, 340, 344, 350, 353, 353, 353, 348, 
   344, 338, 336, 335, 348, 349, 355, 361, 367, 366, 363, 366, 350, 344, 
   341, 336, 332, 330, 331, 334, 330}, 
  {296, 299, 301, 297, 294, 295, 293, 294, 296, 297, 297, 300, 298, 299, 
   298, 299, 299, 299, 307, 310, 306, 306, 307, 310, 308, 317, 319, 325, 
   327, 327, 326, 331, 331, 336, 341, 344, 346, 343, 332, 332, 331, 332, 
   342, 346, 349, 357, 358, 357, 358, 355, 352, 343, 340, 343, 332, 328, 
   328, 335, 331, 331}, {301, 300, 298, 295, 295, 296, 294, 294, 295, 296, 
   294, 296, 296, 298, 299, 301, 301, 300, 305, 309, 309, 310, 313, 316, 
   315, 319, 316, 317, 316, 319, 323, 323, 326, 326, 334, 329, 331, 326, 
   331, 329, 333, 333, 337, 343, 348, 353, 354, 351, 351, 358, 345, 341, 
   341, 336, 327, 326, 329, 328, 328, 327}, 
  {301, 301, 297, 296, 298, 295, 293, 293, 291, 291, 297, 308, 301, 299, 
   303, 310, 309, 310, 309, 310, 306, 309, 309, 308, 307, 308, 309, 310, 
   310, 313, 314, 315, 318, 319, 320, 323, 323, 326, 326, 333, 334, 332, 
   339, 341, 343, 344, 348, 347, 344, 349, 345, 341, 337, 325, 327, 328, 
   325, 322, 322, 322}, {299, 301, 301, 298, 296, 295, 293, 291, 294, 296, 
   301, 303, 302, 304, 310, 317, 314, 316, 315, 310, 303, 306, 307, 306, 
   308, 307, 308, 310, 312, 311, 314, 314, 316, 319, 319, 320, 323, 325, 
   328, 331, 333, 335, 337, 336, 339, 340, 340, 340, 342, 342, 343, 333, 
   324, 325, 323, 322, 316, 319, 318, 319}, 
  {302, 305, 307, 302, 297, 294, 292, 298, 306, 302, 303, 300, 302, 302, 
   304, 309, 314, 310, 308, 312, 308, 308, 310, 308, 308, 310, 309, 307, 
   309, 310, 312, 314, 316, 317, 320, 322, 323, 325, 327, 332, 335, 341, 
   340, 343, 343, 344, 343, 343, 340, 338, 336, 323, 323, 323, 322, 314, 
   312, 313, 315, 320}, {304, 307, 312, 302, 299, 297, 296, 305, 309, 309, 
   308, 307, 305, 307, 309, 310, 308, 309, 310, 313, 313, 313, 313, 311, 
   311, 311, 313, 313, 314, 314, 315, 316, 317, 317, 320, 321, 324, 325, 
   329, 329, 332, 333, 338, 335, 335, 336, 341, 338, 335, 330, 321, 321, 
   316, 317, 309, 310, 312, 310, 314, 313}, 
  {304, 305, 308, 305, 301, 300, 302, 305, 303, 306, 309, 309, 309, 311, 
   313, 312, 312, 312, 314, 0, 316, 318, 0, 315, 316, 312, 313, 313, 314, 
   314, 316, 318, 318, 322, 325, 327, 326, 330, 330, 331, 330, 330, 332, 
   328, 327, 327, 327, 316, 322, 319, 318, 316, 310, 309, 308, 310, 309, 
   313, 312, 314}, {306, 306, 308, 308, 305, 309, 309, 309, 306, 314, 314, 
   316, 318, 322, 322, 318, 320, 322, 322, 325, 0, 324, 0, 323, 0, 0, 0, 0, 
   0, 0, 322, 328, 328, 325, 325, 330, 325, 326, 324, 324, 325, 327, 326, 
   322, 318, 323, 317, 309, 313, 308, 308, 312, 308, 307, 310, 310, 310, 
   313, 310, 311}, {303, 304, 309, 307, 311, 306, 307, 312, 313, 307, 308, 
   317, 319, 318, 322, 319, 318, 324, 327, 329, 332, 330, 325, 0, 0, 0, 0, 
   0, 0, 0, 0, 0, 0, 0, 0, 321, 314, 317, 317, 316, 319, 321, 315, 318, 314, 
   312, 307, 301, 298, 306, 305, 305, 305, 306, 306, 308, 308, 310, 312, 313}\
   , {309, 305, 309, 305, 311, 312, 309, 306, 305, 307, 304, 311, 316, 321, 
   325, 320, 321, 321, 322, 0, 324, 0, 324, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
   0, 0, 0, 317, 316, 315, 314, 317, 315, 310, 307, 301, 303, 301, 297, 300, 
   301, 301, 304, 304, 304, 306, 305, 308, 310, 313}, 
  {303, 301, 302, 310, 308, 307, 307, 307, 305, 304, 312, 315, 314, 320, 
   318, 318, 319, 323, 323, 330, 328, 332, 337, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
   0, 0, 0, 0, 0, 313, 313, 0, 311, 310, 306, 302, 299, 301, 300, 295, 298, 
   297, 301, 305, 305, 305, 305, 306, 308, 309, 307, 304}, 
  {302, 300, 298, 301, 303, 304, 307, 308, 307, 306, 309, 309, 312, 320, 
   319, 326, 331, 336, 0, 332, 331, 331, 333, 334, 326, 320, 318, 322, 318, 
   313, 0, 0, 0, 0, 0, 0, 310, 314, 309, 306, 301, 302, 297, 299, 299, 300, 
   297, 297, 298, 299, 302, 302, 302, 302, 305, 305, 306, 304, 303, 303}, 
  {300, 299, 297, 295, 297, 300, 303, 311, 310, 309, 311, 318, 319, 323, 
   319, 324, 325, 323, 325, 320, 329, 322, 323, 321, 312, 308, 307, 310, 
   310, 304, 306, 304, 307, 300, 0, 0, 312, 0, 305, 306, 296, 294, 295, 295, 
   295, 295, 294, 293, 295, 298, 300, 298, 301, 300, 302, 301, 300, 297, 
   297, 298}, {297, 296, 295, 294, 295, 298, 304, 310, 314, 310, 313, 312, 
   313, 314, 317, 323, 323, 323, 326, 331, 324, 321, 317, 315, 313, 305, 
   308, 306, 304, 299, 301, 299, 296, 301, 296, 295, 303, 0, 300, 300, 292, 
   289, 291, 290, 291, 288, 291, 292, 294, 297, 298, 301, 301, 298, 298, 
   297, 300, 297, 293, 294}, {289, 292, 293, 294, 293, 293, 295, 297, 298, 
   298, 307, 309, 312, 314, 318, 319, 314, 318, 315, 318, 329, 324, 313, 
   304, 304, 303, 305, 302, 302, 298, 291, 289, 292, 291, 292, 293, 300, 
   297, 299, 295, 292, 288, 285, 285, 287, 290, 288, 290, 293, 295, 295, 
   298, 301, 297, 297, 294, 294, 288, 285, 292}, 
  {295, 295, 294, 290, 288, 290, 289, 295, 291, 294, 298, 300, 311, 310, 
   316, 318, 315, 314, 316, 324, 324, 319, 308, 304, 302, 302, 300, 296, 
   298, 296, 292, 290, 285, 284, 281, 288, 292, 293, 296, 292, 287, 286, 
   285, 288, 287, 290, 291, 287, 290, 294, 293, 294, 292, 295, 294, 292, 
   284, 287, 282, 289}, {294, 294, 293, 284, 286, 288, 292, 291, 293, 293, 
   295, 297, 300, 302, 311, 312, 312, 314, 320, 322, 319, 311, 306, 303, 
   300, 299, 295, 294, 292, 293, 291, 284, 278, 280, 286, 290, 290, 0, 290, 
   285, 285, 285, 287, 284, 286, 292, 281, 283, 288, 290, 289, 290, 289, 
   293, 290, 282, 283, 286, 289, 289}, 
  {296, 289, 290, 286, 285, 281, 284, 285, 286, 292, 292, 296, 298, 299, 
   303, 308, 313, 314, 317, 319, 317, 307, 302, 299, 296, 293, 292, 291, 
   290, 291, 282, 280, 278, 282, 286, 288, 290, 286, 291, 284, 285, 284, 
   284, 281, 282, 280, 280, 279, 284, 287, 289, 291, 292, 287, 282, 284, 
   281, 285, 288, 288}, {302, 297, 289, 285, 285, 283, 280, 280, 283, 289, 
   291, 292, 292, 298, 297, 303, 310, 0, 312, 312, 305, 300, 297, 295, 293, 
   293, 289, 288, 288, 284, 276, 275, 280, 285, 288, 290, 290, 282, 285, 
   284, 282, 281, 280, 280, 281, 279, 279, 281, 286, 287, 287, 289, 286, 
   285, 284, 285, 286, 284, 287, 284}, 
  {299, 293, 298, 288, 287, 287, 282, 283, 281, 281, 283, 290, 293, 300, 
   298, 298, 303, 299, 302, 302, 295, 295, 293, 294, 292, 289, 285, 288, 
   285, 276, 275, 280, 284, 286, 285, 285, 285, 282, 281, 282, 278, 278, 
   279, 280, 281, 279, 278, 281, 284, 288, 287, 284, 281, 282, 280, 285, 
   286, 286, 286, 282}, {293, 293, 290, 287, 287, 287, 286, 284, 283, 284, 
   279, 283, 288, 295, 297, 298, 298, 297, 294, 293, 293, 292, 290, 288, 
   288, 279, 280, 276, 275, 276, 279, 284, 284, 280, 284, 279, 280, 281, 
   278, 281, 280, 280, 280, 282, 280, 279, 280, 281, 283, 282, 283, 280, 
   279, 280, 287, 287, 287, 288, 284, 278}, 
  {285, 284, 287, 288, 286, 288, 287, 285, 284, 277, 279, 281, 283, 288, 
   289, 293, 292, 292, 286, 291, 290, 291, 288, 283, 279, 278, 273, 274, 
   274, 279, 281, 284, 283, 278, 278, 279, 280, 279, 277, 279, 278, 280, 
   280, 280, 281, 277, 278, 280, 284, 284, 281, 282, 285, 287, 288, 288, 
   288, 287, 283, 278}, {281, 283, 286, 285, 286, 290, 288, 285, 282, 272, 
   272, 273, 279, 282, 283, 285, 285, 286, 291, 293, 286, 281, 284, 279, 
   277, 277, 275, 277, 277, 281, 283, 279, 278, 281, 280, 281, 280, 280, 
   277, 276, 276, 277, 277, 284, 276, 275, 280, 283, 283, 284, 282, 284, 
   287, 289, 288, 288, 287, 287, 283, 277}, 
  {281, 272, 279, 278, 281, 284, 281, 278, 273, 273, 275, 271, 276, 278, 
   274, 279, 282, 280, 293, 284, 280, 282, 281, 280, 276, 276, 278, 276, 
   279, 280, 275, 277, 279, 279, 278, 279, 0, 278, 277, 278, 279, 278, 277, 
   278, 274, 274, 279, 284, 284, 281, 283, 286, 290, 289, 287, 286, 285, 
   282, 282, 277}, {278, 272, 278, 280, 279, 281, 278, 274, 268, 268, 271, 
   271, 273, 274, 274, 0, 281, 281, 285, 281, 283, 281, 277, 275, 274, 277, 
   274, 276, 277, 277, 275, 275, 277, 277, 278, 281, 277, 279, 277, 277, 
   276, 274, 275, 272, 277, 278, 280, 282, 280, 283, 287, 290, 292, 289, 
   286, 284, 283, 279, 276, 279}, 
  {269, 274, 279, 281, 282, 277, 271, 272, 266, 269, 272, 274, 276, 278, 
   278, 277, 278, 279, 278, 274, 276, 271, 275, 273, 275, 275, 274, 274, 
   276, 275, 276, 276, 276, 277, 279, 280, 278, 278, 277, 277, 276, 276, 
   277, 277, 278, 281, 281, 283, 284, 287, 288, 291, 290, 285, 283, 281, 
   283, 278, 277, 277}, {280, 279, 277, 282, 279, 275, 275, 272, 270, 269, 
   271, 276, 275, 277, 278, 276, 278, 277, 273, 273, 270, 272, 266, 270, 
   274, 273, 273, 275, 276, 274, 276, 277, 278, 280, 281, 278, 279, 280, 
   277, 278, 278, 278, 278, 278, 279, 282, 284, 285, 286, 286, 287, 289, 
   286, 284, 283, 282, 283, 278, 278, 284}, 
  {277, 272, 267, 274, 266, 271, 268, 276, 268, 268, 271, 271, 271, 272, 
   275, 271, 272, 275, 275, 272, 270, 269, 267, 273, 275, 273, 274, 273, 
   274, 274, 275, 278, 280, 279, 281, 281, 280, 281, 280, 280, 281, 280, 
   282, 282, 283, 286, 286, 287, 285, 286, 287, 283, 283, 283, 283, 280, 
   283, 277, 280, 282}, {271, 271, 266, 264, 265, 265, 267, 270, 266, 269, 
   265, 270, 270, 274, 278, 273, 270, 275, 276, 271, 270, 267, 266, 274, 
   275, 273, 273, 271, 272, 275, 276, 279, 280, 281, 281, 285, 282, 282, 
   283, 284, 284, 284, 286, 287, 287, 285, 285, 286, 287, 287, 286, 285, 
   282, 282, 281, 279, 278, 277, 278, 279}, 
  {277, 268, 262, 267, 266, 263, 262, 263, 265, 273, 266, 271, 270, 275, 
   278, 273, 269, 273, 273, 271, 267, 266, 268, 273, 273, 272, 272, 272, 
   271, 275, 276, 279, 281, 0, 283, 285, 283, 284, 284, 285, 285, 286, 287, 
   287, 286, 288, 294, 292, 291, 286, 284, 281, 281, 279, 279, 279, 278, 
   277, 275, 280}, {278, 272, 263, 267, 268, 261, 260, 264, 270, 272, 268, 
   268, 271, 276, 276, 271, 272, 269, 271, 267, 265, 265, 270, 273, 273, 
   273, 272, 271, 271, 274, 279, 280, 282, 284, 284, 286, 283, 285, 285, 
   286, 286, 287, 287, 288, 293, 296, 292, 285, 285, 284, 282, 280, 283, 
   280, 278, 279, 279, 279, 278, 277}, 
  {279, 266, 262, 264, 262, 261, 262, 261, 262, 263, 264, 265, 270, 273, 
   273, 273, 270, 269, 269, 268, 268, 268, 271, 273, 274, 274, 271, 270, 
   272, 279, 278, 283, 282, 283, 286, 285, 286, 287, 289, 291, 291, 292, 
   290, 292, 289, 287, 286, 280, 281, 281, 280, 281, 281, 281, 280, 280, 
   280, 278, 281, 280}, {275, 263, 263, 262, 259, 258, 257, 261, 262, 261, 
   264, 261, 265, 271, 271, 0, 270, 270, 268, 267, 267, 268, 272, 273, 274, 
   273, 273, 272, 275, 277, 281, 281, 281, 284, 288, 290, 287, 287, 288, 
   291, 288, 287, 286, 283, 281, 281, 281, 280, 281, 282, 280, 279, 278, 
   279, 279, 281, 278, 281, 281, 281}, 
  {266, 258, 261, 261, 261, 257, 260, 261, 262, 261, 262, 262, 265, 270, 
   270, 270, 274, 272, 269, 267, 267, 269, 271, 273, 275, 272, 275, 277, 
   275, 279, 280, 279, 0, 286, 288, 285, 284, 285, 287, 283, 284, 281, 278, 
   281, 281, 278, 277, 279, 277, 280, 274, 277, 278, 279, 0, 280, 278, 283, 
   282, 279}, {256, 257, 257, 255, 261, 258, 260, 267, 265, 263, 261, 266, 
   0, 271, 267, 271, 272, 267, 269, 263, 268, 267, 269, 269, 273, 270, 274, 
   276, 275, 277, 283, 281, 286, 284, 285, 288, 287, 285, 282, 277, 278, 
   277, 276, 277, 276, 275, 275, 276, 276, 275, 274, 275, 279, 280, 281, 
   281, 275, 279, 283, 279}, {254, 257, 256, 261, 255, 262, 266, 264, 261, 
   259, 264, 265, 0, 271, 267, 270, 269, 266, 271, 261, 267, 267, 266, 270, 
   271, 272, 269, 272, 275, 277, 274, 281, 285, 286, 285, 281, 287, 287, 
   279, 275, 276, 274, 272, 273, 274, 273, 275, 273, 272, 273, 272, 272, 
   275, 278, 279, 0, 274, 274, 274, 274}, 
  {252, 253, 255, 256, 260, 263, 260, 260, 256, 257, 263, 268, 268, 0, 263, 
   260, 267, 264, 262, 262, 266, 267, 269, 270, 273, 268, 272, 271, 275, 
   272, 273, 284, 282, 284, 0, 281, 284, 280, 277, 275, 275, 273, 271, 268, 
   269, 271, 271, 271, 272, 273, 273, 273, 273, 275, 275, 273, 273, 275, 
   274, 273}, {250, 251, 257, 256, 254, 255, 255, 251, 258, 259, 256, 261, 
   262, 0, 261, 261, 259, 262, 261, 263, 263, 263, 265, 269, 271, 270, 273, 
   276, 272, 274, 281, 278, 281, 283, 284, 284, 281, 277, 275, 274, 269, 
   269, 267, 269, 267, 271, 270, 272, 273, 271, 274, 274, 273, 0, 273, 271, 
   269, 271, 273, 270}, {252, 251, 256, 255, 255, 256, 257, 254, 257, 255, 
   256, 0, 259, 258, 256, 259, 255, 257, 257, 259, 260, 259, 262, 265, 268, 
   266, 271, 274, 273, 273, 278, 279, 0, 280, 280, 279, 276, 275, 271, 270, 
   269, 267, 267, 266, 269, 269, 267, 270, 271, 270, 270, 271, 273, 273, 
   271, 269, 266, 268, 265, 268}, 
  {249, 253, 254, 255, 254, 256, 255, 256, 259, 257, 257, 258, 259, 257, 
   257, 256, 255, 255, 255, 254, 256, 256, 256, 259, 267, 264, 276, 268, 
   269, 272, 275, 270, 273, 276, 278, 269, 272, 269, 270, 269, 269, 266, 
   264, 266, 266, 265, 266, 266, 269, 266, 270, 271, 269, 271, 268, 266, 
   264, 267, 265, 265}, {248, 251, 251, 251, 254, 254, 254, 257, 258, 259, 
   259, 260, 259, 257, 257, 256, 256, 254, 253, 254, 256, 257, 259, 262, 
   263, 263, 271, 268, 270, 269, 269, 269, 272, 272, 272, 267, 269, 268, 
   265, 263, 264, 262, 262, 262, 264, 263, 264, 267, 267, 268, 269, 270, 
   271, 269, 0, 265, 265, 264, 263, 263}, 
  {249, 250, 250, 250, 251, 251, 253, 255, 255, 259, 258, 261, 260, 258, 
   258, 257, 255, 255, 255, 255, 253, 256, 259, 263, 261, 264, 262, 264, 
   271, 271, 272, 272, 269, 0, 270, 265, 266, 264, 263, 261, 260, 260, 260, 
   258, 258, 260, 260, 263, 265, 264, 268, 269, 0, 266, 0, 266, 263, 263, 
   261, 259}, {249, 249, 248, 249, 250, 251, 253, 254, 255, 259, 258, 260, 
   0, 258, 258, 257, 256, 256, 256, 256, 258, 260, 261, 263, 264, 266, 265, 
   268, 267, 270, 268, 0, 269, 0, 269, 267, 266, 267, 265, 263, 261, 261, 
   259, 258, 258, 256, 258, 260, 264, 264, 264, 266, 266, 265, 264, 261, 
   262, 259, 260, 258}, {249, 248, 248, 248, 250, 250, 251, 253, 256, 257, 
   0, 260, 259, 258, 257, 258, 256, 256, 257, 258, 259, 260, 263, 263, 265, 
   263, 265, 265, 265, 268, 266, 266, 267, 267, 267, 267, 264, 267, 264, 
   262, 259, 260, 259, 259, 261, 258, 260, 262, 263, 264, 261, 265, 264, 
   261, 261, 261, 257, 257, 259, 257}, 
  {248, 247, 248, 248, 250, 249, 252, 253, 254, 256, 259, 259, 257, 259, 
   258, 257, 256, 255, 256, 257, 258, 259, 261, 262, 263, 263, 262, 262, 
   263, 264, 263, 265, 265, 265, 265, 266, 265, 264, 262, 261, 259, 258, 
   259, 259, 260, 261, 262, 262, 261, 261, 261, 262, 264, 262, 260, 258, 
   258, 256, 258, 257}, {248, 247, 248, 247, 247, 251, 251, 254, 253, 255, 
   258, 258, 257, 257, 258, 256, 257, 254, 254, 255, 255, 256, 259, 259, 
   259, 260, 261, 261, 262, 263, 264, 263, 262, 263, 262, 263, 261, 259, 
   258, 257, 257, 258, 257, 258, 259, 260, 259, 259, 258, 262, 259, 261, 
   261, 263, 259, 258, 259, 257, 256, 255}, 
  {246, 247, 246, 246, 246, 248, 252, 253, 252, 255, 256, 257, 255, 256, 
   254, 255, 254, 252, 252, 252, 252, 253, 255, 256, 256, 258, 259, 260, 
   262, 260, 262, 261, 262, 259, 260, 257, 258, 256, 256, 256, 254, 254, 
   254, 257, 257, 256, 256, 259, 260, 261, 260, 0, 260, 0, 256, 258, 258, 
   257, 253, 253}, {245, 246, 245, 246, 244, 246, 249, 250, 254, 254, 255, 
   256, 254, 254, 254, 253, 251, 251, 250, 251, 251, 253, 255, 256, 255, 
   256, 255, 259, 260, 259, 0, 260, 0, 259, 258, 256, 255, 256, 255, 254, 
   254, 255, 254, 256, 255, 256, 255, 254, 259, 259, 260, 261, 260, 264, 
   261, 258, 253, 254, 254, 251}, 
  {245, 246, 246, 247, 244, 247, 251, 251, 252, 0, 254, 0, 253, 250, 251, 
   250, 249, 247, 249, 249, 250, 251, 252, 253, 253, 254, 256, 258, 258, 
   260, 260, 259, 258, 258, 256, 255, 255, 255, 257, 254, 255, 256, 254, 
   251, 255, 254, 256, 256, 255, 262, 258, 259, 259, 259, 258, 258, 253, 
   254, 252, 252}, {241, 246, 245, 247, 244, 247, 250, 251, 251, 253, 253, 
   252, 252, 250, 249, 248, 247, 247, 248, 247, 248, 248, 250, 250, 251, 
   251, 253, 256, 256, 258, 258, 260, 258, 257, 256, 256, 255, 253, 254, 
   252, 254, 253, 252, 253, 254, 255, 255, 259, 258, 258, 258, 257, 259, 
   257, 256, 256, 255, 257, 249, 251}, 
  {245, 246, 246, 245, 243, 247, 249, 251, 250, 250, 252, 251, 250, 251, 
   250, 248, 246, 246, 245, 247, 247, 248, 248, 249, 251, 251, 253, 253, 
   254, 256, 258, 259, 258, 257, 254, 254, 257, 255, 255, 255, 252, 252, 
   256, 261, 254, 254, 254, 257, 257, 259, 258, 259, 256, 256, 258, 255, 
   253, 257, 250, 251}, {247, 246, 245, 247, 246, 248, 247, 252, 251, 251, 
   253, 251, 250, 250, 248, 249, 247, 245, 245, 247, 246, 247, 248, 249, 
   248, 250, 250, 253, 254, 254, 259, 260, 255, 257, 257, 256, 256, 256, 
   256, 253, 254, 260, 260, 263, 262, 259, 256, 258, 260, 258, 0, 259, 0, 
   257, 256, 253, 257, 258, 253, 250}, 
  {244, 243, 244, 253, 257, 250, 251, 248, 250, 256, 253, 249, 248, 249, 
   250, 249, 245, 247, 249, 249, 250, 249, 250, 250, 247, 248, 250, 252, 
   253, 0, 256, 0, 256, 257, 257, 255, 255, 258, 256, 254, 252, 253, 258, 
   256, 256, 255, 255, 261, 261, 260, 258, 259, 259, 260, 257, 256, 256, 
   251, 253, 251}, {241, 243, 242, 244, 243, 247, 256, 251, 0, 256, 248, 
   249, 250, 255, 259, 258, 252, 252, 250, 251, 251, 253, 252, 251, 253, 
   252, 252, 254, 253, 258, 255, 256, 256, 255, 253, 257, 253, 253, 253, 
   254, 253, 253, 254, 257, 255, 258, 258, 257, 260, 257, 258, 259, 258, 
   259, 256, 253, 258, 249, 251, 256}, 
  {242, 244, 244, 242, 245, 246, 246, 249, 255, 253, 249, 253, 255, 250, 
   252, 249, 255, 250, 251, 251, 256, 256, 254, 255, 254, 255, 256, 264, 
   260, 259, 259, 259, 257, 255, 255, 252, 262, 256, 255, 250, 256, 255, 
   256, 257, 256, 258, 256, 255, 257, 257, 257, 259, 259, 258, 259, 259, 
   256, 249, 250, 252}, {245, 243, 244, 244, 247, 251, 250, 254, 253, 254, 
   251, 253, 253, 253, 253, 254, 256, 253, 261, 262, 255, 261, 259, 265, 
   266, 267, 266, 266, 263, 259, 257, 255, 263, 264, 262, 263, 263, 266, 
   265, 261, 264, 258, 255, 259, 262, 258, 257, 259, 260, 255, 260, 258, 
   259, 258, 256, 256, 257, 253, 252, 250}, 
  {248, 247, 249, 248, 251, 253, 252, 255, 256, 256, 254, 253, 255, 254, 
   255, 256, 256, 253, 267, 263, 265, 267, 268, 266, 266, 266, 266, 268, 
   268, 260, 0, 260, 265, 264, 267, 264, 259, 259, 257, 258, 258, 258, 256, 
   256, 260, 260, 263, 261, 255, 256, 262, 0, 257, 253, 257, 258, 258, 257, 
   257, 255}, {249, 251, 252, 251, 252, 254, 255, 257, 256, 0, 255, 254, 256, 
   257, 256, 257, 257, 257, 261, 260, 262, 263, 262, 261, 268, 267, 267, 264, 
   266, 266, 0, 265, 266, 267, 266, 258, 255, 261, 266, 264, 259, 259, 257, 
   258, 260, 261, 258, 267, 259, 262, 261, 256, 255, 258, 259, 259, 259, 257, 
   257, 256}};

EndPackage[]