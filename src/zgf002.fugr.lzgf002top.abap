FUNCTION-POOL zgf002.                       "MESSAGE-ID ..
DEFINE calcula.
  PERFORM calcula TABLES te_esq_calc
                    USING  &1 &2 &3
                    CHANGING &4.

END-OF-DEFINITION.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis, kkblo.

TYPES: BEGIN OF ty_saida,
         kunnr TYPE bsid-kunnr,
         belnr TYPE bsid-belnr,
         bldat TYPE bsid-bldat,
         budat TYPE bsid-budat,
         blart TYPE bsid-blart,
         zfbdt TYPE bsid-zfbdt,
         vbel2 TYPE bsid-vbel2,
         vbeln TYPE bsid-vbeln,
         dmbtr TYPE bsid-dmbtr,
         dmbe2 TYPE bsid-dmbe2,
         sgtxt TYPE bsid-sgtxt,
       END OF ty_saida,

       BEGIN OF ty_ovs,
         vbeln TYPE vbak-vbeln,
       END OF ty_ovs,

       BEGIN OF ty_saida_exec,
         nro_sol_ov TYPE zsdt0053-nro_sol_ov,
         posnr      TYPE zsdt0053-posnr,
         zmeng      TYPE zsdt0053-zmeng,
         valdt      TYPE zsdt0053-valdt,
         vlrtot     TYPE zsdt0053-vlrtot,
         vbeln      TYPE vbak-vbeln,
         msg(255),
       END OF ty_saida_exec,

       BEGIN OF ty_saldo,
         nro_sol_ov TYPE zsdt0053-nro_sol_ov,
         vbeln      TYPE vbfa-vbeln,
         werks      TYPE zsdt0053-werks,
         zmeng      TYPE zsdt0053-zmeng,
         total      TYPE zsdt0053-zmeng,
         saldo      TYPE zsdt0053-zmeng,
       END OF ty_saldo.



TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
*        t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid VALUE sy-repid,
      t_top        TYPE slis_t_listheader,
      wg_bukrs     TYPE bsid-bukrs,
      wg_kunnr     TYPE kna1-kunnr,
      tg_esq_calc  TYPE TABLE OF zsds006 WITH HEADER LINE,
      result       TYPE bsid-dmbtr,
      wg_bdc       TYPE bdcdata,
      tg_bdc       TYPE TABLE OF bdcdata,
      tg_msg       TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      wg_msg       TYPE bdcmsgcoll,
      tg_saldo     TYPE TABLE OF ty_saldo,
      wg_saldo     TYPE ty_saldo,
      wl_col_pos   TYPE lvc_colpos.
*        t_sort       TYPE slis_t_sortinfo_alv WITH HEADER LINE.

** Criação de tabela dinamica
DATA: t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      lt_fieldcat    TYPE kkblo_t_fieldcat.

DATA: lr_value_descr TYPE REF TO cl_abap_elemdescr,
      component      TYPE cl_abap_structdescr=>component,
      lt_components  TYPE cl_abap_structdescr=>component_table.

DATA: lt_struc TYPE REF TO cl_abap_structdescr,
      lt_tab   TYPE REF TO cl_abap_tabledescr.

DATA: container         TYPE REF TO cl_gui_custom_container,
      display_mode      TYPE xfeld,
      editor            TYPE REF TO cl_gui_textedit,
      longtext_tab      TYPE catsxt_longtext_itab,
      obg_EDITCONTAINER TYPE REF TO cl_gui_custom_container,
      obg_editor        TYPE REF TO cl_gui_textedit.
*          TITLE        TYPE SYTITLE                   .

FIELD-SYMBOLS: <fs_table> TYPE any.

TYPES: BEGIN OF Ty_CODE,
         line(255),
       END OF Ty_CODE.
DATA: tg_tp_venda TYPE zsdt0057-param_espec.  "*-Equalização RISE x PRD - 19.07.2023 - JT
DATA: tg_code_show TYPE TABLE OF Ty_CODE WITH HEADER LINE.
