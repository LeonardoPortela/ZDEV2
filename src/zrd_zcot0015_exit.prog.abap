*&---------------------------------------------------------------------*
*& Report  ZRD_ZCOT0015-_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zcot0015_exit.

FORM f_exit_zcot0015_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zcot0015 TYPE zcot0015.

  CLEAR: wl_zcot0015.

  MOVE-CORRESPONDING p_registro_manter TO wl_zcot0015.

  IF wl_zcot0015-usuario IS INITIAL.
    wl_zcot0015-data      = sy-datum.
    wl_zcot0015-hora      = sy-uzeit.
    wl_zcot0015-usuario   = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zcot0015 TO p_registro_manter.

ENDFORM.

FORM f_exit_zcot0015_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.


  DATA: wl_zcot0015        TYPE zcot0015.

  CLEAR: wl_zcot0015.
  MOVE-CORRESPONDING p_registro_manter TO wl_zcot0015.

  CLEAR: p_error.

  IF wl_zcot0015-wrttp IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo categoria de valor obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zcot0015-brg_cstg_intdg EQ abap_true AND wl_zcot0015-brg_os_material EQ abap_true AND wl_zcot0015-nao_integra EQ abap_true.
    p_error = abap_true.
    MESSAGE 'Não permitido marcar o checkbox nos 3 campos!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zcot0015-brg_cstg_intdg EQ abap_true AND wl_zcot0015-brg_os_material EQ abap_true.
    p_error = abap_true.
    MESSAGE 'Não permitido marcar o checkbox nos 2 campos!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zcot0015-brg_cstg_intdg EQ abap_true AND wl_zcot0015-nao_integra EQ abap_true.
    p_error = abap_true.
    MESSAGE 'Não permitido marcar o checkbox nos 2 campos!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zcot0015-brg_os_material EQ abap_true AND wl_zcot0015-nao_integra EQ abap_true.
    p_error = abap_true.
    MESSAGE 'Não permitido marcar o checkbox nos 2 campos!' TYPE 'S'.
    EXIT.
  ENDIF.


  IF wl_zcot0015-brg_cstg_intdg EQ abap_false AND wl_zcot0015-brg_os_material EQ abap_false AND wl_zcot0015-nao_integra EQ abap_false.
    p_error = abap_true.
    MESSAGE 'Não permitido realizar o cadastro e nao marcar nenhum checkbox!' TYPE 'S'.
    EXIT.
  ENDIF.


ENDFORM.


FORM f_exit_zcot0015_0003 CHANGING p_saida TYPE any.

*  DATA: wl_zfit0209 TYPE zfit0209.
*
*  CLEAR: wl_zfit0209.
*
*  MOVE-CORRESPONDING p_saida TO wl_zfit0209.
*
*  MOVE-CORRESPONDING wl_zfit0209 TO p_saida.


ENDFORM.

FORM f_exit_zcot0015_0004 CHANGING p_saida TYPE any.


*  DATA: wl_zfit0209_out    TYPE zfit0209_out.
*
*
*  CLEAR: wl_zfit0209_out.
*  MOVE-CORRESPONDING p_saida TO wl_zfit0209_out.
*
*  IF wl_zfit0209_out-tipo_valor EQ '01'.
*    wl_zfit0209_out-desc_tipo_valor = 'Valor Principal'.
*  ELSEIF wl_zfit0209_out-tipo_valor EQ '02'.
*    wl_zfit0209_out-desc_tipo_valor = 'Valor 13º'.
*  ENDIF.
*
*  IF wl_zfit0209_out-lgart IS NOT INITIAL.
*    SELECT SINGLE lgtxt
*           FROM t512t
*           INTO wl_zfit0209_out-desc_lgart
*           WHERE sprsl EQ 'P'
*           AND   molga EQ '37'
*           AND   lgart EQ wl_zfit0209_out-lgart.
*  ENDIF.
*
*  MOVE-CORRESPONDING wl_zfit0209_out TO p_saida.

ENDFORM.

FORM f_exit_zcot0015_0005 CHANGING p_registro_manter TYPE any.



  DATA: wl_zfit0209_out    TYPE zfit0209_out.


*  CLEAR: wl_zfit0209_out.
*  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0209_out.
*
*  IF wl_zfit0209_out-tipo_valor EQ '01'.
*    wl_zfit0209_out-desc_tipo_valor = 'Valor Principal'.
*  ELSEIF wl_zfit0209_out-tipo_valor EQ '02'.
*    wl_zfit0209_out-desc_tipo_valor = 'Valor 13º'.
*  ENDIF.
*
*  IF wl_zfit0209_out-lgart IS NOT INITIAL.
*    SELECT SINGLE lgtxt
*           FROM t512t
*           INTO wl_zfit0209_out-desc_lgart
*           WHERE sprsl EQ 'P'
*           AND   molga EQ '37'
*           AND   lgart EQ wl_zfit0209_out-lgart.
*  ENDIF.
*
*  MOVE-CORRESPONDING wl_zfit0209_out TO p_registro_manter.



ENDFORM.

FORM  f_exit_zcot0015_0009 TABLES it_excl_toolbar
                           USING  p_db_tab.

*  IF p_db_tab = 'zfit0209'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.
