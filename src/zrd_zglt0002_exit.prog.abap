*&---------------------------------------------------------------------*
*& Report  ZRD_ZGLT0102_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0002_exit.

FORM f_exit_zglt0002_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt0002 TYPE zglt0002.

  CLEAR: wl_zglt0002.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0002.

  IF wl_zglt0002-criador IS INITIAL.
    wl_zglt0002-criador   = sy-uname.
  ENDIF.
  wl_zglt0002-modificador   = sy-uname.
  wl_zglt0002-data   = sy-datum.
  wl_zglt0002-hora   = sy-uzeit.

  MOVE-CORRESPONDING wl_zglt0002 TO p_registro_manter.

ENDFORM.


FORM f_exit_zglt0002_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zglt0002        TYPE zglt0002,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zglt0002.
  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0002.


  MOVE-CORRESPONDING wl_zglt0002 TO p_registro_manter.

  CLEAR: p_error.

ENDFORM.


FORM f_exit_zglt0002_0003 CHANGING p_saida TYPE any.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zglt0002        TYPE zglt0002,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zglt0002.
  MOVE-CORRESPONDING p_saida TO wl_zglt0002.

  MOVE-CORRESPONDING wl_zglt0002 TO p_saida.

ENDFORM.

FORM f_exit_zglt0002_0004 CHANGING p_saida TYPE any.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zglt0002_out    TYPE zglt0002_out,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zglt0002_out.
  MOVE-CORRESPONDING p_saida TO wl_zglt0002_out.

  IF wl_zglt0002_out-tipo IS NOT INITIAL.

*    CALL FUNCTION 'CONVERSION_EXIT_CFOBR_INPUT'
*      EXPORTING
*        input  = wl_zglt0002_out-cfop
*      IMPORTING
*        output = wl_zglt0002_out-cfop.

    SELECT SINGLE name1
           FROM t880
           INTO wl_zglt0002_out-descr
           WHERE rcomp EQ wl_zglt0002_out-tipo.
*             AND langu EQ 'P'.
  ENDIF.

  MOVE-CORRESPONDING wl_zglt0002_out TO p_saida.

ENDFORM.

FORM f_exit_zglt0002_0005 CHANGING p_registro_manter TYPE any.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zglt0002_out    TYPE zglt0002_out,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zglt0002_out.
  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0002_out.

  IF wl_zglt0002_out-tipo IS NOT INITIAL.

*    CALL FUNCTION 'CONVERSION_EXIT_CFOBR_INPUT'
*      EXPORTING
*        input  = wl_zglt0002_out-cfop
*      IMPORTING
*        output = wl_zglt0002_out-cfop.

    SELECT single name1
           FROM t880
           INTO wl_zglt0002_out-descr
           WHERE rcomp EQ wl_zglt0002_out-tipo.
*             AND langu EQ 'P'.
  ENDIF.

  MOVE-CORRESPONDING wl_zglt0002_out TO p_registro_manter.

ENDFORM.
FORM f_exit_zglt0002_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZGLT0002_OUT'. "= 'ZSDT0343 T2'.
    p_outputlen = 20.
  ENDIF.

  IF p_ref_tabname = 'ZGLT0002_OUT'
   AND   p_field       = 'DESCR'.
    p_outputlen = 50.
    p_scrtext_l = 'Descrição'.
  ENDIF.

ENDFORM.
FORM  f_exit_zglt0002_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zglt0002'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.
