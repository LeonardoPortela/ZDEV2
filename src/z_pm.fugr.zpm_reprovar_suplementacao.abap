FUNCTION zpm_reprovar_suplementacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ORDEM) TYPE  AUFNR
*"     REFERENCE(MOTIVO) TYPE  STRING OPTIONAL
*"     REFERENCE(USUARIO) TYPE  XUBNAME OPTIONAL
*"     REFERENCE(APP) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     VALUE(OK) TYPE  CHAR01
*"     VALUE(MSG_RETORNO) TYPE  STRING
*"     VALUE(OBSERVACAO) TYPE  STRING
*"----------------------------------------------------------------------

  DATA: ordens             TYPE TABLE OF zpmr0003,
        tl_fields          TYPE TABLE OF sval,
        lv_return,
        tl_texto           TYPE catsxt_longtext_itab,
        vl_observacao(500),
        msg                TYPE bapi_msg,
        vl_valor_estimado  TYPE aufuser4,
        cont               TYPE i,
        lv_ordem           TYPE aufnr,
        lv_usuario         TYPE xubname.

  IF usuario IS NOT INITIAL.
    lv_usuario = usuario.
  ELSE.
    lv_usuario = sy-uname.
  ENDIF.

  ok = abap_false.

  lv_ordem = |{ ordem ALPHA = IN }|.

*  APPEND VALUE #(
*                   TABNAME    = 'VBAP'
*                   FIELDNAME  = 'ZMENG'
*                   FIELD_OBL  = 'X'
*                   FIELDTEXT  = 'Custo Estimado'
*                 ) TO TL_FIELDS.
*
*  CALL FUNCTION 'POPUP_GET_VALUES_USER_HELP'
*    EXPORTING
*      POPUP_TITLE               = 'Informe o Custo da Suplementação'
*      PROGRAMNAME               = SY-CPROG
*      NO_CHECK_FOR_FIXED_VALUES = ABAP_TRUE
*    IMPORTING
*      RETURNCODE                = LV_RETURN
*    TABLES
*      FIELDS                    = TL_FIELDS
*    EXCEPTIONS
*      ERROR_IN_FIELDS           = 1
*      OTHERS                    = 2.

  SELECT SINGLE vlr_estimado
    FROM zpmr0006
    INTO @DATA(vl_valor)
    WHERE aufnr = @lv_ordem
      AND status = 'P'.

  IF sy-ucomm EQ 'FURT'.
    vl_valor_estimado = tl_fields[ 1 ]-value.
  ELSE.
    SELECT SINGLE vlr_estimado FROM zpmr0006 INTO vl_valor_estimado WHERE aufnr = lv_ordem AND status = 'P'.
  ENDIF.

  IF motivo IS INITIAL.
    tl_texto = VALUE #( ( 'Suplementação Rejeitada' ) ).

    IF app IS INITIAL.

      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title = 'Observação'
        CHANGING
          ch_text  = tl_texto.

      IF sy-ucomm NE 'CX_CONT'.
        FREE: tl_texto.
      ENDIF.

    ENDIF.

    LOOP AT tl_texto INTO DATA(wa).
      vl_observacao = |{ vl_observacao } { wa }|.
    ENDLOOP.

  ELSE.
    vl_observacao = motivo.
  ENDIF.

  observacao = vl_observacao.

  DATA(tamanho_string) = strlen( vl_observacao ).

  IF tamanho_string >= 200.
    IF motivo IS INITIAL.
      MESSAGE |Quantidade de caracteres { tamanho_string } Ultrapassa o Valor Permitido "200"!| TYPE 'I'.
    ELSE.
      msg_retorno = |Quantidade de caracteres { tamanho_string } Ultrapassa o Valor Permitido "200"!|.
    ENDIF.
    EXIT.
  ENDIF.

  UPDATE zpmr0006 SET status         = 'R'
                      responsavel    = lv_usuario
                      dt_modificacao = sy-datum
                      nivel_aprovado = '0000000000'
                      obs_reprov     = vl_observacao
                WHERE aufnr          = lv_ordem
                  AND status         = 'P'.

  msg = |Suplementação Rejeitada com o Custo Inicial de { vl_valor }!|.
  msg_retorno = msg.

  CALL FUNCTION 'Z_GRAVA_LOG_PM'
    EXPORTING
      i_tp_msg   = 'S'
      i_mensagem = msg
      i_tcode    = sy-tcode.

  ok = abap_true.

ENDFUNCTION.
