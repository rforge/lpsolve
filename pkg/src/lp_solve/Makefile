PKG_CPPFLAGS=$(CPPFLAGS) -I../../inst/include -DYY_NEVER_INTERACTIVE \
             -DPARSER_LP -DINVERSE_ACTIVE=INVERSE_LUSOL \
             -DRoleIsExternalInvEngine

LP_SOLVE_SOURCES=colamd.c lp_MDO.c lp_mipbb.c lp_rlp.c mmio.c commonlib.c \
                 lp_MPS.c lp_params.c lp_scale.c myblas.c lp_SOS.c \
                 lp_presolve.c lp_simplex.c yacc_read.c ini.c lp_crash.c \
                 lp_price.c lp_utils.c lp_Hash.c lp_lib.c lp_pricePSE.c \
                 lp_wlp.c lp_LUSOL.c lp_matrix.c lp_report.c lusol.c

LP_SOLVE_OBJECTS=$(LP_SOLVE_SOURCES:.c=.o)

all: libmylib.so

libmylib.so: $(LP_SOLVE_OBJECTS)
	$(SHLIB_LD) $(LDFLAGS) $(LP_SOLVE_OBJECTS) -o libmylib${SHLIB_EXT}

.c.o:
	$(CC) $(CFLAGS) $(PKG_CPPFLAGS) -c $< -o $@

clean:
	rm -rf $(LP_SOLVE_OBJECTS) libmylib${SHLIB_EXT}



