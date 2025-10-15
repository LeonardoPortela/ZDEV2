*&---------------------------------------------------------------------*
*& Report ZRD_ZPPT_CONTROLE_EXIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zppt_controle_exit.


*&---------------------------------------------------------------------*
*&  "Set atributos iniciais ao incluir um novo registro
*&---------------------------------------------------------------------*
FORM f_exit_zppt_controle_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zppt_controle TYPE zppt_controle.

  CLEAR: wl_zppt_controle.

  wl_zppt_controle-us_registro_cri = sy-uname.
  wl_zppt_controle-dt_criacao      = sy-datum.
  wl_zppt_controle-dt_registro     = sy-datum.
  wl_zppt_controle-us_registro     = sy-uname.

  MOVE-CORRESPONDING wl_zppt_controle TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Validações antes da gravação do registro
*&---------------------------------------------------------------------*
FORM f_exit_zppt_controle_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

* Declaração de Estruturas
  DATA: wl_zppt_controle TYPE zppt_controle.
  CLEAR: wl_zppt_controle.

  MOVE-CORRESPONDING p_registro_manter TO wl_zppt_controle.

  IF sy-ucomm EQ 'NOVO'.
    SELECT MAX( cod_alt )
           INTO @DATA(vl_cod_alt)
           FROM zppt_controle
           WHERE werks EQ @wl_zppt_controle-werks
            AND aprovador EQ @wl_zppt_controle-aprovador.

    wl_zppt_controle-cod_alt = vl_cod_alt + 1.

    MOVE-CORRESPONDING wl_zppt_controle TO p_registro_manter.
  ENDIF.

* Busca Centros
  IF wl_zppt_controle-werks IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Centro é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zppt_controle-aprovador IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Aprovador é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zppt_controle-eqtyp IS INITIAL AND
     wl_zppt_controle-mptyp IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Favor, informar pelo menos um tipo' TYPE 'S'.
    EXIT.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Set atributos antes de gravar o registro
*&---------------------------------------------------------------------*
FORM f_exit_zppt_controle_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zppt_controle TYPE zppt_controle.

  CLEAR: wl_zppt_controle.

  MOVE-CORRESPONDING p_registro_manter TO wl_zppt_controle.

  wl_zppt_controle-dt_registro     = sy-datum.
  wl_zppt_controle-us_registro     = sy-uname.

  MOVE-CORRESPONDING wl_zppt_controle TO p_registro_manter.

ENDFORM.
FORM f_exit_zppt_controle_0017 USING p_tipo.

  DATA: it_t001w TYPE TABLE OF t001w.

  IF p_tipo = '0001'.

    FREE:it_t001w.
    SELECT  *
      FROM t001w
      INTO CORRESPONDING FIELDS OF TABLE it_t001w.

    CHECK it_t001w IS NOT INITIAL.

    SORT it_t001w BY werks.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        pvalkey          = ' '
        retfield         = 'WERKS'
        dynpprog         = sy-repid
        dynpnr           = sy-dynnr
        dynprofield      = '<FS_WA_REGISTRO_MANTER>-WERKS'
        callback_program = sy-repid
        value_org        = 'S'
      TABLES
        value_tab        = it_t001w
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.
  ENDIF.
ENDFORM.
