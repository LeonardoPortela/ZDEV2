*&---------------------------------------------------------------------*
*& Report  ZRD_zmmt0154_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0154_exit.



FORM f_exit_zmmt0154_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zmmt0154 TYPE zmmt0154.

  CLEAR: wl_zmmt0154.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0154.

  IF wl_zmmt0154-bsart IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Tipo de pedido deve ser infomado!' TYPE 'S'.
    EXIT.
  ELSE.
    SELECT SINGLE * FROM t161 INTO @DATA(_wl_t161) WHERE bsart EQ @wl_zmmt0154-bsart.
    IF sy-subrc NE 0.

      p_error = abap_true.
      MESSAGE 'Tipo de pedido não encontrado' TYPE 'S'.
      EXIT.
    ENDIF.

  ENDIF.

  IF wl_zmmt0154-matkl IS INITIAL AND wl_zmmt0154-matnr IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Material ou Grupo de material deve ser infomado!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zmmt0154-matkl IS NOT INITIAL AND wl_zmmt0154-matnr IS NOT INITIAL.
    p_error = abap_true.
    MESSAGE 'Informar apenas Material ou grupo de mercadoia!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zmmt0154-direcao IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Direção deve ser informado!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zmmt0154-direcao ne '1' AND wl_zmmt0154-direcao NE '2'.
    p_error = abap_true.
    MESSAGE 'Somente Direção 1 e 2 Permitida!' TYPE 'S'.
    EXIT.
  ENDIF.

* US #154447 - MMSILVA - 02.04.2025 - Inicio
  IF wl_zmmt0154-mtorg IS NOT INITIAL.
    DATA: lt_dd07v TYPE TABLE OF dd07v.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = 'J_1BMATORG'
      TABLES
        values_tab = lt_dd07v
      EXCEPTIONS
        OTHERS     = 1.

    READ TABLE lt_dd07v INTO DATA(_wl_dd07v) WITH KEY domvalue_l = wl_zmmt0154-mtorg.

    IF sy-subrc IS NOT INITIAL.
      p_error = abap_true.
      MESSAGE 'Origem do material não encontrado!' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.
* US #154447 - MMSILVA - 02.04.2025 - Fim

*-CS2025000249-19.05.2025-#175013-JT-inicio
  IF wl_zmmt0154-direcao IS NOT INITIAL.
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = 'J_1BDIRECT'
      TABLES
        values_tab = lt_dd07v
      EXCEPTIONS
        OTHERS     = 1.

    READ TABLE lt_dd07v INTO _wl_dd07v WITH KEY domvalue_l = wl_zmmt0154-direcao.

    IF sy-subrc IS NOT INITIAL.
      p_error = abap_true.
      MESSAGE 'Direção Informada Incorreta!' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.
*-CS2025000249-19.05.2025-#175013-JT-fim

  MOVE-CORRESPONDING wl_zmmt0154 TO p_registro_manter.

  CLEAR: p_error.

ENDFORM.


FORM f_exit_zmmt0154_0003 CHANGING p_saida TYPE any.

  DATA: wl_zmmt0154_out TYPE zmmt0154_out.

  CLEAR: wl_zmmt0154_out.

  MOVE-CORRESPONDING p_saida TO wl_zmmt0154_out.

  MOVE-CORRESPONDING wl_zmmt0154_out TO p_saida.


ENDFORM.

FORM f_exit_zmmt0154_0004 CHANGING p_saida TYPE any.

  DATA: wl_zmmt0154_out TYPE zmmt0154_out.

  CLEAR: wl_zmmt0154_out.

  MOVE-CORRESPONDING p_saida TO wl_zmmt0154_out.
  CLEAR p_saida.

  SELECT SINGLE * FROM t161t INTO @DATA(_wl_t161t) WHERE bsart EQ @wl_zmmt0154_out-bsart AND spras = 'P'.
  IF sy-subrc EQ 0.
    wl_zmmt0154_out-desc_bsart = _wl_t161t-batxt.

  ENDIF.

  MOVE-CORRESPONDING wl_zmmt0154_out TO p_saida.




ENDFORM.

FORM f_exit_zmmt0154_0005 CHANGING p_registro_manter TYPE any.
  DATA: wl_zmmt0154_out TYPE zmmt0154_out.
  DATA: wl_t001 TYPE  t001,
        wl_lfa1 TYPE  lfa1.

  CLEAR: wl_zmmt0154_out, wl_t001, wl_lfa1.
  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0154_out.

  MOVE-CORRESPONDING wl_zmmt0154_out TO p_registro_manter.

ENDFORM.

*FORM  f_exit_zmmt0154_0009 TABLES it_excl_toolbar
*                           USING p_db_tab.
*
*  IF p_db_tab = 'ZMMT0154'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
*ENDFORM.

*-CS2025000249-19.05.2025-#175013-JT-inicio
FORM f_exit_zmmt0154_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZMMT0154_OUT' AND
     p_field       = 'DIRECAO'.
    p_outputlen    = 10.
    p_scrtext_l    = 'Direção'.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0154_OUT' AND
     p_field       = 'ICMS'.
    p_outputlen    = 06.
    p_check        = abap_true.
    p_scrtext_l    = 'ICMS?'.
  ENDIF.

ENDFORM.

FORM f_exit_zmmt0154_0017 USING p_tipo.

  CASE p_tipo.
    WHEN '0001'.
      PERFORM f4_val_param_direcao USING '<FS_WA_REGISTRO_MANTER>-DIRECAO'.
  ENDCASE.

ENDFORM.

FORM f4_val_param_direcao USING p_cod  TYPE help_info-dynprofld.

  TYPES: BEGIN OF ty_dados,
           direcao TYPE j_1bdirect,
           descr   TYPE char60,
         END OF ty_dados.

  DATA: t_dd07v   TYPE TABLE OF dd07v,
        t_dados   TYPE TABLE OF ty_dados,
        t_mapping TYPE STANDARD TABLE OF dselc,
        s_mapping TYPE dselc,
        t_ret     TYPE TABLE OF ddshretval,
        v_rc      TYPE sy-subrc.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname   = 'J_1BDIRECT'
      text      = 'X'
      langu     = sy-langu
*     BYPASS_BUFFER        = ' '
    IMPORTING
      rc        = v_rc
    TABLES
      dd07v_tab = t_dd07v.

  LOOP AT t_dd07v INTO DATA(ls_dd07v).
    APPEND VALUE #( direcao = ls_dd07v-domvalue_l descr = ls_dd07v-ddtext ) TO t_dados.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DIRECAO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = p_cod
      window_title    = 'Direção'
      value_org       = 'S'
    TABLES
      value_tab       = t_dados
      return_tab      = t_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.
*-CS2025000249-19.05.2025-#175013-JT-fim
