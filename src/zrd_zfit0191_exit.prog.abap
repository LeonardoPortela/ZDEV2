*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0150_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0191_exit.

FORM f_exit_zfit0191_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0191 TYPE zfit0191.

  wl_zfit0191-usuario  = sy-uname.
  wl_zfit0191-data     = sy-datum.
  wl_zfit0191-hora     = sy-uzeit.

  MOVE-CORRESPONDING wl_zfit0191 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0191_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zfit0191 TYPE zfit0191,
        vl_message  TYPE c LENGTH 200,
        lv_cpf      TYPE zde_stcd2,
        lv_answer.

  CLEAR: wl_zfit0191.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0191.

  IF wl_zfit0191-ebeln   IS INITIAL.
    MESSAGE 'Informar Pedido!' TYPE 'S' DISPLAY LIKE 'E'.
    p_error = abap_true.
    EXIT.
  ENDIF.

*------------------------------
*---- valida empresa
*------------------------------
  SELECT ebeln
    INTO @DATA(l_ebeln)
    FROM ekko
      UP TO 1 ROWS
   WHERE ebeln = @wl_zfit0191-ebeln.
  ENDSELECT.

  IF sy-subrc <> 0.
    MESSAGE 'Pedido de Compras não Localizado!' TYPE 'S' DISPLAY LIKE 'E'.
    p_error = abap_true.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zfit0191_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0191 TYPE zfit0191,
        vl_message  TYPE c LENGTH 200.

  CLEAR: wl_zfit0191.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0191.

  wl_zfit0191-usuario  = sy-uname.
  wl_zfit0191-data     = sy-datum.
  wl_zfit0191-hora     = sy-uzeit.

  MOVE-CORRESPONDING wl_zfit0191 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0191_0004 CHANGING p_saida TYPE any.
ENDFORM.

FORM f_exit_zfit0191_0005 CHANGING p_saida TYPE any.
ENDFORM.

FORM f_exit_zfit0191_0014 USING p_saida TYPE any CHANGING p_break.
ENDFORM.

FORM f_exit_zfit0191_0016 USING p_ucomm
                          CHANGING p_registro_manter p_saida.
ENDFORM.

FORM f_edit_text USING wl_thead TYPE thead.
ENDFORM.

FORM f_create_text USING wl_thead.
ENDFORM.

FORM f_exit_zfit0191_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZFIT0191' AND
     p_field       = 'EBELN'.
    p_scrtext_l    = 'Pedido de Compras'.
    p_outputlen    = 20.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0191' AND
     p_field       = 'USUARIO'.
    p_scrtext_l    = 'Usuário'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0191' AND
     p_field       = 'DATA'.
    p_scrtext_l    = 'Data'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0191' AND
     p_field       = 'HORA'.
    p_scrtext_l    = 'Hora'.
    p_outputlen    = 15.
  ENDIF.

ENDFORM.
