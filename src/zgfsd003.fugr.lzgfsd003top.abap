FUNCTION-POOL zgfsd003.                     "MESSAGE-ID ..

* INCLUDE LZGFSD003D...                      " Local class definition

TABLES: zsds078, zsds079, zsde0035, zsds081.

DATA gv_ucomm_9000 TYPE sy-ucomm.

DATA gv_erro.

CONTROLS tc_alv TYPE TABLEVIEW USING SCREEN 9000.
DATA gt_alv_9000 TYPE TABLE OF zsds079.

DATA gv_edit_9000 TYPE c.
DATA gv_fatu_9000 TYPE c.
DATA gv_tc_alv_lines LIKE sy-loopc.
DATA gv_exibe_travas TYPE c.
DATA check_dt TYPE sy-subrc.

" screen 7000 -------

CONTROLS tc_alv_7 TYPE TABLEVIEW USING SCREEN 7000.
CONTROLS tc_alv_trv_7000 TYPE TABLEVIEW USING SCREEN 7000.
DATA gt_alv_7000 TYPE TABLE OF zsds081.
DATA gv_ucomm_7000 TYPE sy-ucomm.

DATA gt_desab TYPE zsdc082.
DATA gv_dtprev_pag_a TYPE sy-datum.
DATA gv_taxa_a TYPE kurrf.
DATA gv_dtvenc TYPE sy-datum.
