FUNCTION-POOL ZFI_ADM.                      "MESSAGE-ID ..

* INCLUDE LZFI_ADMD...                       " Local class definition
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_RUN_TIME_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ABAP_FALSE  text
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_saida,
         bukrs       TYPE zfit0080-bukrs,
         cd_grupo    TYPE zfit0105-cd_grupo,
         ds_grupo    TYPE zfit0105-descricao,
         cod_flx     TYPE zfit0077-cod_flx,
         desc_flx    TYPE zfit0077-desc_flx,
         ent_sai     TYPE c,
         rmvct       TYPE zfit0080-rmvct,
         sld_ini_fim TYPE c,
         day_01      TYPE zfit0080-dmbtr,
         day_02      TYPE zfit0080-dmbtr,
         day_03      TYPE zfit0080-dmbtr,
         day_04      TYPE zfit0080-dmbtr,
         day_05      TYPE zfit0080-dmbtr,
         day_06      TYPE zfit0080-dmbtr,
         day_07      TYPE zfit0080-dmbtr,
         day_08      TYPE zfit0080-dmbtr,
         day_09      TYPE zfit0080-dmbtr,
         day_10      TYPE zfit0080-dmbtr,
         day_11      TYPE zfit0080-dmbtr,
         day_12      TYPE zfit0080-dmbtr,
         day_13      TYPE zfit0080-dmbtr,
         day_14      TYPE zfit0080-dmbtr,
         day_15      TYPE zfit0080-dmbtr,
         day_16      TYPE zfit0080-dmbtr,
         day_17      TYPE zfit0080-dmbtr,
         day_18      TYPE zfit0080-dmbtr,
         day_19      TYPE zfit0080-dmbtr,
         day_20      TYPE zfit0080-dmbtr,
         day_21      TYPE zfit0080-dmbtr,
         day_22      TYPE zfit0080-dmbtr,
         day_23      TYPE zfit0080-dmbtr,
         day_24      TYPE zfit0080-dmbtr,
         day_25      TYPE zfit0080-dmbtr,
         day_26      TYPE zfit0080-dmbtr,
         day_27      TYPE zfit0080-dmbtr,
         day_28      TYPE zfit0080-dmbtr,
         day_29      TYPE zfit0080-dmbtr,
         day_30      TYPE zfit0080-dmbtr,
         day_31      TYPE zfit0080-dmbtr,
         tot_month   TYPE zfit0080-dmbtr,
         rowcolor(4) TYPE c,
       END OF ty_saida.

DATA: it_alv_saida TYPE TABLE OF ty_saida.


DATA: lr_data_line       TYPE REF TO data,
      lr_data            TYPE REF TO data,
      lr_data_descr      TYPE REF TO cl_abap_datadescr,
      lr_data_line_descr TYPE REF TO cl_abap_datadescr.


FIELD-SYMBOLS:
  <lt_data>      TYPE ANY TABLE,
  <lt_data_line> TYPE ANY TABLE,
  <ls_data>      TYPE any,
  <ls_data_line> TYPE any.

FIELD-SYMBOLS: <t_data>      TYPE ANY TABLE,
               <t_data_line> TYPE ANY TABLE,
               <w_data>      TYPE any,
               <w_data_line> TYPE any.

DATA: l_data            TYPE REF TO data,
      l_data_line       TYPE REF TO data,
      l_tabix           TYPE sy-tabix,
      l_data_descr      TYPE REF TO cl_abap_datadescr,
      l_data_line_descr TYPE REF TO cl_abap_datadescr.


DATA: P_BUKRS TYPE RANGE OF BUKRS.


FORM f_prepare_run_time_info  USING p_display TYPE c.

  IF <t_data> IS ASSIGNED.
    CLEAR: <t_data>[].
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR: <t_data_line>[].
  ENDIF.

  IF <t_data> IS ASSIGNED.
    CLEAR: <t_data>.
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR: <t_data_line>.
  ENDIF.

  FREE: l_data,  l_data_line, l_data_descr,  l_data_line_descr.

  cl_salv_bs_runtime_info=>set( EXPORTING display  = p_display
    metadata = abap_false
  data     = abap_true ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_RUNTIME_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_runtime_info.
  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
      IMPORTING r_data_descr  = l_data_descr
        r_data_line_descr = l_data_line_descr ).

      CHECK ( l_data_descr IS NOT INITIAL ) OR ( l_data_line_descr IS  NOT INITIAL ).

      CREATE DATA l_data      TYPE HANDLE  l_data_descr.
      CREATE DATA l_data_line TYPE HANDLE  l_data_line_descr.

      ASSIGN l_data->* TO <t_data>.
      ASSIGN l_data_line->* TO <t_data_line>.

      cl_salv_bs_runtime_info=>get_data( IMPORTING t_data  = <t_data>
        t_data_line = <t_data_line> ).
    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  ASSIGN l_data->*        TO <w_data>.
  ASSIGN l_data_line->*   TO <w_data_line>.
ENDFORM.
