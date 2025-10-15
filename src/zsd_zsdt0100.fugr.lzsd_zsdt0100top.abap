FUNCTION-POOL zsd_zsdt0100.                 "MESSAGE-ID ..

* INCLUDE LZSD_ZSDT0100D...                  " Local class definition

TABLES: zsds_tela_calculo_aprovacao, zsds_tela_calculo_alv.

DATA gv_ucomm TYPE sy-ucomm.

CONTROLS grd_9000 TYPE TABLEVIEW USING SCREEN 9000.

data gt_saldos TYPE TABLE of zsds_tela_calculo_alv.

data gv_error.
