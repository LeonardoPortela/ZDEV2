FUNCTION-POOL z_fi MESSAGE-ID z_fi.

TYPES: BEGIN OF ty_estrat,
         nivel_aprov  TYPE numc4,
         branch       TYPE j_1bbranc_,
         usnam        TYPE usnam,
         nome         TYPE char80,
         departamento TYPE char20.
TYPES: END OF ty_estrat.

TYPES: BEGIN OF ty_logaprov.
         INCLUDE STRUCTURE zfiwrt0018.
         TYPES:   nome TYPE char80.
TYPES: END OF ty_logaprov.

"FI - ZFI0064 - Transação Parametros US #149772 - WPP -->>>
TYPES: BEGIN OF ty_saida_tmp,
           bukrs     TYPE bsad-bukrs,
           gsber     TYPE bsad-gsber,
           kunnr     TYPE bsad-kunnr,
           name1     TYPE kna1-name1,
           tipo      TYPE c LENGTH 13,
           belnr     TYPE bsad-belnr,
           augbl     TYPE bsad-augbl,
           budat     TYPE bsad-budat,
           augdt     TYPE bsad-augdt,
           vbel2     TYPE bsad-vbel2,
           auart     TYPE vbak-auart,
           vbeln     TYPE bsad-vbeln,
           nr_sol    TYPE zsdt0053-nro_sol_ov,
           tp_venda  TYPE zsdt0051-tp_venda,
           dmbtr     TYPE bsad-dmbtr,
           dmbe2     TYPE bsad-dmbe2,
           tx_camb   TYPE zlest0061-tax_dolar,
           banco_liq TYPE skat-txt50,
           zfbdt     TYPE bsad-zfbdt,
           butxt     TYPE t001-butxt,
           belnr_bx  TYPE bsad-belnr,
           budat_bx  TYPE bsad-budat,
           augdt_bx  TYPE bsad-augdt,
           dmbtr_bx  TYPE bsad-dmbtr,
           dmbe2_bx  TYPE bsad-dmbe2,
           vbel2_bx  TYPE bsad-vbel2,
           vbeln_bx  TYPE bsad-vbeln,
           matnr     TYPE mara-matnr,
           maktx     TYPE makt-maktx,
           buzei     TYPE bsad-buzei,
           charg     TYPE vbap-charg,
           waers     TYPE bsad-waers,
           tpsim     TYPE char2,
           spart     TYPE vbak-spart,
           jr_ds     TYPE c,
           matkl     TYPE mara-matkl,
           ktokd     TYPE kna1-ktokd,
         END OF ty_saida_tmp.
"FI - ZFI0064 - Transação Parametros US #149772 - WPP <<---

DATA: vg_banco_fornecedor TYPE zbanco_empresa,
      vg_valor_movimento  TYPE p,
      it_zfit0010         TYPE TABLE OF zfit0010 INITIAL SIZE 0 WITH HEADER LINE,
      wa_zfit0011         TYPE zfit0011,
      it_zfit0011         TYPE TABLE OF zfit0011 INITIAL SIZE 0 WITH HEADER LINE,
      wa_t012             TYPE t012,
      it_t012             TYPE TABLE OF t012 INITIAL SIZE 0 WITH HEADER LINE,
      it_0007             TYPE TABLE OF zfiwrt0007,
      wa_0007             TYPE zfiwrt0007,
      it_user             TYPE TABLE OF user_addr,
      wa_user             TYPE user_addr,
      it_0018             TYPE TABLE OF zfiwrt0018,
      wa_0018             TYPE zfiwrt0018,
      it_estrat           TYPE TABLE OF ty_estrat,
      wa_estrat           TYPE ty_estrat,
      it_logaprov         TYPE TABLE OF ty_logaprov,
      wa_logaprov         TYPE ty_logaprov,
      gt_saida_tmp        type table of ty_saida_tmp WITH HEADER LINE."FI - ZFI0064 - Transação Parametros US #149772 - WPP  <<---

"FI - ZFI0064 - Transação Parametros US #149772 - WPP --->>
 RANGES: r_zterm_troca_acerto FOR bsad-zterm,
         r_zterm_ajuste_finan FOR bsad-zterm.
"FI - ZFI0064 - Transação Parametros US #149772 - WPP <<---

DATA: c_u    TYPE c LENGTH 1 VALUE 'U',
      c_0001 TYPE c LENGTH 4 VALUE '0001'.

DATA: g_container        TYPE scrfname VALUE 'CONTAINER',
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_grid             TYPE REF TO cl_gui_alv_grid,
      g_grid2            TYPE REF TO cl_gui_alv_grid,
      w_tool             TYPE stb_button,
      ok_code            TYPE sy-ucomm,
      l_ok               TYPE c,
      t_fieldcatalog     TYPE lvc_t_fcat, "Fieldcatalog
      t_exctab           TYPE slis_t_extab,
      w_exctab           TYPE slis_extab,
      w_item_layout      TYPE lvc_s_laci,
      w_layout           TYPE lvc_s_layo,
      ls_fieldcatalog    TYPE lvc_s_fcat,
      ls_exclude         TYPE ui_func,
      pt_exclude         TYPE ui_functions,
      t_del_rows         TYPE lvc_t_row,
      w_del_rows         TYPE lvc_s_row,
      t_sel_cols         TYPE lvc_t_col,
      w_sel_cols         TYPE lvc_s_col,
      l_row_id           TYPE lvc_s_row,
      l_column_id        TYPE lvc_s_col,
      l_stable           TYPE lvc_s_stbl,
      t_fcat_lvc         TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE,
      t_fcat_kkb         TYPE kkblo_t_fieldcat.
