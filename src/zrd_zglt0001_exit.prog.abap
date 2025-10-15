*&---------------------------------------------------------------------*
*& Report  ZRD_ZGLT0102_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0001_exit.

FORM f_exit_zglt0001_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt0001 TYPE zglt0001.

  CLEAR: wl_zglt0001.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0001.

  IF wl_zglt0001-criador IS INITIAL.
    wl_zglt0001-criador   = sy-uname.
  ENDIF.
  wl_zglt0001-modificador   = sy-uname.
  wl_zglt0001-data   = sy-datum.
  wl_zglt0001-hora   = sy-uzeit.

  MOVE-CORRESPONDING wl_zglt0001 TO p_registro_manter.

ENDFORM.


FORM f_exit_zglt0001_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zglt0001        TYPE zglt0001,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zglt0001.
  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0001.


  MOVE-CORRESPONDING wl_zglt0001 TO p_registro_manter.

  CLEAR: p_error.

ENDFORM.


FORM f_exit_zglt0001_0003 CHANGING p_saida TYPE any.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zglt0001        TYPE zglt0001,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zglt0001.
  MOVE-CORRESPONDING p_saida TO wl_zglt0001.

  MOVE-CORRESPONDING wl_zglt0001 TO p_saida.

ENDFORM.

FORM f_exit_zglt0001_0004 CHANGING p_saida TYPE any.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zglt0001_out    TYPE zglt0001_out,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zglt0001_out.
  MOVE-CORRESPONDING p_saida TO wl_zglt0001_out.

  IF wl_zglt0001_out-cfop IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_CFOBR_INPUT'
      EXPORTING
        input  = wl_zglt0001_out-cfop
      IMPORTING
        output = wl_zglt0001_out-cfop.

    SELECT cfotxt
          UP TO 1 ROWS
           FROM j_1bagnt
           INTO wl_zglt0001_out-cfotxt
           WHERE spras EQ 'P'
           AND   cfop EQ wl_zglt0001_out-cfop.
    ENDSELECT.
  ENDIF.

  MOVE-CORRESPONDING wl_zglt0001_out TO p_saida.

ENDFORM.

FORM f_exit_zglt0001_0005 CHANGING p_registro_manter TYPE any.


  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zglt0001_out    TYPE zglt0001_out,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zglt0001_out.
  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0001_out.

  IF wl_zglt0001_out-cfop IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_CFOBR_INPUT'
      EXPORTING
        input  = wl_zglt0001_out-cfop
      IMPORTING
        output = wl_zglt0001_out-cfop.

    SELECT cfotxt
          UP TO 1 ROWS
           FROM j_1bagnt
           INTO wl_zglt0001_out-cfotxt
           WHERE spras EQ 'P'
           AND   cfop EQ wl_zglt0001_out-cfop.
    ENDSELECT.
  ENDIF.


  MOVE-CORRESPONDING wl_zglt0001_out TO p_registro_manter.

ENDFORM.
FORM f_exit_zglt0001_0008 CHANGING p_col_pos
                                p_ref_tabname
                                p_ref_fieldname
                                p_tabname
                                p_field
                                p_scrtext_l
                                p_outputlen
                                p_edit
                                p_sum
                                p_emphasize
                                p_just
                                p_hotspot
                                p_f4
                                p_check.

  IF p_ref_tabname = 'ZGLT0001_OUT'. "= 'ZSDT0343 T2'.
    p_outputlen = 20.
  ENDIF.

  IF p_ref_tabname = 'ZGLT0001_OUT'
   AND   p_field       = 'CFOTXT'.
    p_outputlen = 50.
  ENDIF.

  IF p_ref_tabname = 'ZGLT0001_OUT'
   AND   p_field       = 'TP_M'.
    p_outputlen = 20.
    p_scrtext_l = 'Tipo Mercado'.
  ENDIF.

ENDFORM.
FORM  f_exit_zglt0001_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zglt0001'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.
