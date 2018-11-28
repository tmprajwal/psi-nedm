
bin_PROGRAMS           = faster_disfast          \
												 faster_file_display     \
												 faster_file_is_sorted   \
												 faster_file_sort        \
												 faster_file_ungroup

cflags  = -I../include
#ldadd   = ../lib/libfasterac.a
ldadd   = -L../lib -lfasterac

faster_disfast_SOURCES        = faster_disfast.c
faster_disfast_CFLAGS         = $(cflags)
faster_disfast_LDADD          = $(ldadd)

faster_file_display_SOURCES   = faster_disfast.c
faster_file_display_CFLAGS    = $(cflags)
faster_file_display_LDADD     = $(ldadd)

faster_file_is_sorted_SOURCES = faster_file_is_sorted.c
faster_file_is_sorted_CFLAGS  = $(cflags)
faster_file_is_sorted_LDADD   = $(ldadd)

faster_file_sort_SOURCES      = faster_file_sort.c
faster_file_sort_CFLAGS       = $(cflags)
faster_file_sort_LDADD        = $(ldadd)

faster_file_ungroup_SOURCES   = faster_file_ungroup.c
faster_file_ungroup_CFLAGS    = $(cflags)
faster_file_ungroup_LDADD     = $(ldadd)

