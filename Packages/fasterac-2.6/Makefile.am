
SUBDIRS              = lib src include man

confexecdir	      	 = ${bindir}
confexec_SCRIPTS     = fasterac_config.sh  \
                       fasterac_config.csh \
											 fasterac_examples_copy

pkgconfigdir         = ${libdir}/pkgconfig
pkgconfig_DATA       = libfasterac.pc

dataExampledir            = ${prefix}/share/fasterac/data
dist_dataExample_DATA     = data/*.fast

readerExampledir          = ${prefix}/share/fasterac/examples/data_reader
dist_readerExample_DATA   = examples/data_reader/README   \
														examples/data_reader/Makefile \
														examples/data_reader/*.c
dist_readerExample_SCRIPTS= examples/data_reader/TEST.sh

rootTreeExampledir        = ${prefix}/share/fasterac/examples/faster_to_root
dist_rootTreeExample_DATA = examples/faster_to_root/Makefile         \
														examples/faster_to_root/group2tree.C     \
														examples/faster_to_root/group2tree_bis.C \
														examples/faster_to_root/regroup2tree.C   \
													  examples/faster_to_root/rootlogon.C
dist_rootTreeExample_SCRIPTS = examples/faster_to_root/TEST.sh

miscExampledir            = ${prefix}/share/fasterac/examples/misc
dist_miscExample_DATA     = examples/misc/Makefile        \
														examples/misc/*.c
dist_miscExample_SCRIPTS  = examples/misc/TEST.sh

oscilloExampledir         = ${prefix}/share/fasterac/examples/oscillo
dist_oscilloExample_DATA  = examples/oscillo/Makefile     \
														examples/oscillo/*.c

oscirootExampledir        = ${prefix}/share/fasterac/examples/osciroot
dist_oscirootExample_DATA = examples/osciroot/README    \
												 	  examples/osciroot/*.h       \
												 	  examples/osciroot/*.C
dist_oscirootExample_SCRIPTS = examples/osciroot/clean.sh \
                               examples/osciroot/TEST.sh

libsrcdir                 = ${prefix}/share/fasterac/src/lib
dist_libsrc_DATA          = lib/fasterac.c     \
														lib/fast_data.c    \
														lib/farray.c       \
														lib/utils.c        \
														lib/qdc_caras.c    \
														lib/adc_caras.c    \
														lib/rf_caras.c     \
														lib/electrometer.c \
														lib/scaler.c       \
														lib/qtdc.c

incsrcdir                 = ${prefix}/share/fasterac/src/lib/fasterac
dist_incsrc_DATA          = include/fasterac/fasterac.h     \
														include/fasterac/fast_data.h    \
														include/fasterac/farray.h       \
														include/fasterac/utils.h        \
														include/fasterac/qdc_caras.h    \
														include/fasterac/adc_caras.h    \
														include/fasterac/rf_caras.h     \
														include/fasterac/electrometer.h \
														include/fasterac/scaler.h       \
														include/fasterac/sampler.h      \
														include/fasterac/qtdc.h

binsrcdir                 = ${prefix}/share/fasterac/src/prog
dist_binsrc_DATA          = src/faster_disfast.c        \
														src/faster_file_is_sorted.c \
														src/faster_file_sort.c      \
														src/faster_file_ungroup.c

EXTRA_DIST = saved_svn_version.txt

