*&---------------------------------------------------------------------*
*& Include ZFIR0103_TOP
*&---------------------------------------------------------------------*

DATA container_main       TYPE REF TO cl_gui_custom_container.
DATA painel_control       TYPE REF TO cl_gui_splitter_container.
DATA painel1              TYPE REF TO cl_gui_container.
DATA painel2              TYPE REF TO cl_gui_container.

DATA lr_column            TYPE REF TO cl_salv_column.
DATA lr_columns_TB        TYPE REF TO cl_salv_columns_table.
DATA lr_column_TB         TYPE REF TO cl_salv_column_table.
DATA lr_columns           TYPE REF TO cl_salv_columns.
DATA lr_functions         TYPE REF TO cl_salv_functions.
DATA lr_selections        TYPE REF TO cl_salv_selections.
DATA lv_key               TYPE salv_s_layout_key.
DATA lex_not_found        TYPE REF TO cx_salv_not_found.
DATA gr_table             TYPE REF TO cl_salv_table.
DATA lr_display_settings  TYPE REF TO cl_salv_display_settings.
DATA l_title              TYPE lvc_title.
DATA ls_api               TYPE REF TO if_salv_gui_om_extend_grid_api.
DATA ls_edit              TYPE REF TO if_salv_gui_om_edit_restricted.
DATA lr_layout            TYPE REF TO cl_salv_layout.
DATA icon_status          TYPE string.
DATA lt_rows              TYPE TABLE OF lvc_s_row.  "TYPE TABLE OF lvc_s_row "TYPE salv_t_row.
DATA qtd_rows             TYPE i.
DATA ls_selected_row      TYPE lvc_s_row.

DATA lr_column2            TYPE REF TO cl_salv_column.
DATA lr_columns_TB2        TYPE REF TO cl_salv_columns_table.
DATA lr_column_TB2         TYPE REF TO cl_salv_column_table.
DATA lr_columns2           TYPE REF TO cl_salv_columns.
DATA lr_functions2         TYPE REF TO cl_salv_functions.
DATA lr_selections2        TYPE REF TO cl_salv_selections.
DATA lv_key2               TYPE salv_s_layout_key.
DATA lex_not_found2        TYPE REF TO cx_salv_not_found.
DATA gr_table2             TYPE REF TO cl_salv_table.
DATA lr_display_settings2  TYPE REF TO cl_salv_display_settings.
DATA l_title2              TYPE lvc_title.
DATA ls_api2               TYPE REF TO if_salv_gui_om_extend_grid_api.
DATA ls_edit2              TYPE REF TO if_salv_gui_om_edit_restricted.
DATA lr_layout2            TYPE REF TO cl_salv_layout.
DATA icon_status2         TYPE string.
DATA lt_rows2              TYPE TABLE OF lvc_s_row.  "TYPE TABLE OF lvc_s_row "TYPE salv_t_row.
DATA qtd_rows2             TYPE i.
DATA ls_selected_row2      TYPE lvc_s_row.
DATA: dtini TYPE sy-datum.
DATA: dtfim TYPE sy-datum.



TYPES: BEGIN OF ty_saida,
         obj_key TYPE zib_contabil-obj_key,
         xblnr   TYPE zib_contabil-xblnr,
         belnr   TYPE belnr_d,
         seqitem TYPE zib_contabil-seqitem,
         gsber   TYPE zib_contabil-gsber,
         bukrs   TYPE zib_contabil-bukrs,
         bldat   TYPE zib_contabil-bldat,
         budat   TYPE zib_contabil-budat,
         gjahr   TYPE zib_contabil-gjahr,
         status  TYPE string,
       END OF ty_saida.

TYPES: BEGIN OF ty_saida2,
         obj_key       TYPE zib_contabil-obj_key,
         xblnr         TYPE zib_contabil-xblnr,
         belnr         TYPE belnr_d,
         seqitem       TYPE zib_contabil-seqitem,
         bschl         TYPE zib_contabil-bschl,
         gsber         TYPE zib_contabil-gsber,
         bukrs         TYPE zib_contabil-bukrs,
         bldat         TYPE zib_contabil-bldat,
         budat         TYPE zib_contabil-budat,
         gjahr         TYPE zib_contabil-gjahr,
         monat         TYPE zib_contabil-monat,
         blart         TYPE zib_contabil-blart,
         hkont         TYPE zib_contabil-hkont,
         wrbtr         TYPE zib_contabil-wrbtr,
         waers         TYPE zib_contabil-waers,
         sgtxt         TYPE zib_contabil-sgtxt,
         kostl         TYPE zib_contabil-prctr,
         prctr         TYPE zib_contabil-prctr,
         waers_i       TYPE zib_contabil-waers_i,
         dmbtr         TYPE zib_contabil-dmbtr,
         waers_f       TYPE zib_contabil-waers_f,
         dmbe2         TYPE zib_contabil-dmbe2,
         rg_atualizado TYPE zib_contabil-rg_atualizado,
         rldnr         TYPE zib_contabil-rldnr,
         status        TYPE string,
       END OF ty_saida2.

TYPES: BEGIN OF ty_acdoca,
         belnr  TYPE acdoca-belnr,
         bschl  TYPE acdoca-bschl,
         tsl    TYPE acdoca-tsl,
         hsl    TYPE acdoca-hsl,
         ksl    TYPE acdoca-ksl,
         rhcur  TYPE acdoca-rhcur,
         rbusa  TYPE acdoca-rbusa,
         rbukrs TYPE acdoca-rbukrs,
         bldat  TYPE acdoca-bldat,
         budat  TYPE acdoca-budat,
         gjahr  TYPE acdoca-gjahr,
         poper  TYPE acdoca-poper,
         blart  TYPE acdoca-blart,
         racct  TYPE acdoca-racct,
         rwcur  TYPE acdoca-rwcur,
         sgtxt  TYPE acdoca-sgtxt,
         rcntr  TYPE acdoca-rcntr,
         prctr  TYPE acdoca-prctr,
         rkcur  TYPE acdoca-rkcur,
       END OF ty_acdoca.


TYPES: BEGIN OF ty_validacao_obj_key,
         obj_key   TYPE zib_contabil_chv-obj_key,
         existe(3) TYPE c,
       END OF ty_validacao_obj_key.

TYPES: BEGIN OF ty_selecao,
         rbukrs      TYPE acdoca-rbukrs,
         gjahr       TYPE acdoca-gjahr,
         belnr       TYPE acdoca-belnr,
         obj_key(20) TYPE c,
       END OF ty_selecao.

DATA: it_selecao TYPE STANDARD TABLE OF ty_selecao INITIAL SIZE 0.

DATA: it_obj_key TYPE STANDARD TABLE OF ty_validacao_obj_key WITH HEADER LINE. "Sim Encontrado nas ZIB's

DATA: it_acdoca TYPE STANDARD TABLE OF acdoca INITIAL SIZE 0.
DATA: lt_zib_contabil TYPE zib_contabil.
DATA: lt_zib_contabil_CHV TYPE zib_contabil_chv.
DATA: lt_zib_contabil_ERR TYPE zib_contabil_err.
DATA: it_saida TYPE STANDARD TABLE OF ty_saida WITH HEADER LINE.
DATA: it_saida2 TYPE STANDARD TABLE OF ty_saida2 WITH HEADER LINE.
DATA: lv_message TYPE string.
DATA: it_message TYPE TABLE OF string.
