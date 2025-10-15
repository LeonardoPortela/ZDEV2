FUNCTION zpm_reprovar_permit.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ORDEM) TYPE  AUFNR
*"     REFERENCE(MOTIVO) TYPE  STRING OPTIONAL
*"     REFERENCE(USUARIO) TYPE  XUBNAME OPTIONAL
*"  EXPORTING
*"     REFERENCE(OK) TYPE  CHAR01
*"     REFERENCE(MSG_RETORNO) TYPE  STRING
*"     REFERENCE(OBSERVACAO) TYPE  STRING
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_ih,
           objnr   TYPE j_objnr,
           counter TYPE i_count,
         END OF ty_ih.

  DATA: ordens             TYPE TABLE OF zpmr0003,
        tl_texto           TYPE catsxt_longtext_itab,
        vl_observacao(500),
        msg                TYPE bapi_msg,
        gt_gnsvb           TYPE TABLE OF gnsvb,
        gt_ihgns           TYPE TABLE OF ty_ih,
        gw_order_header    TYPE alm_me_order_header,
        gw_user_data       TYPE alm_me_user_data,
        gw_user_profile    TYPE alm_me_c010prf,
        lv_usuario         TYPE xubname.

  IF usuario IS NOT INITIAL.
    lv_usuario = usuario.
  ELSE.
    lv_usuario = lv_usuario.
  ENDIF.

  ok = abap_false.

  CALL FUNCTION 'ALM_ME_ORDER_GETDETAIL'
    EXPORTING
      orderid       = ordem
      resource      = abap_true
      userdata      = gw_user_data
      order_profile = gw_user_profile
    IMPORTING
      order_header  = gw_order_header
    EXCEPTIONS
      read_error    = 1.

  CHECK gw_order_header IS NOT INITIAL.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gt_ihgns
    FROM ihgns
    WHERE objnr EQ gw_order_header-object_no
    AND geniakt EQ abap_false.

  IF sy-subrc IS NOT INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE gt_ihgns
      FROM ihsg
      WHERE objnr EQ gw_order_header-object_no.
  ENDIF.

  CHECK gt_ihgns IS NOT INITIAL.

*   Reprova as Permissão concedida anteriormente
  gt_gnsvb = VALUE #( FOR ls IN  gt_ihgns
                        ( CORRESPONDING #( ls ) )
                        (
                          objnr    = ls-objnr
                          counter  = ls-counter
                          geniakt  = abap_true
                          geniname = lv_usuario
                          genidate = sy-datum
                          genitime = sy-uzeit
                          aktiv    = '1'    " Ação de Reprovação da Ordem
                         )
                    ).

  DELETE gt_gnsvb WHERE aktiv NE '1'.

  CALL FUNCTION 'PM_SP_ISSUE_POST' IN UPDATE TASK
    EXPORTING
      i_belgnstab = gt_gnsvb[].

  IF motivo IS INITIAL.
    tl_texto = VALUE #( ( 'Ordem Rejeitada' ) ).

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title = 'Observação'
      CHANGING
        ch_text  = tl_texto.

    IF sy-ucomm NE 'CX_CONT'.
      FREE: tl_texto.
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
  ENDIF.

  DATA(_log) =
    VALUE zpmr0006( aufnr          = gw_order_header-orderid
                    status         = 'R'
                    obs_reprov     = vl_observacao
                    vlr_estimado   = gw_order_header-estimated_costs
                    responsavel    = lv_usuario
                    dt_modificacao = sy-datum
                  ).

  MODIFY zpmr0006 FROM _log.
  COMMIT WORK.
  WAIT UP TO 2 SECONDS.

  msg = |Ordem Rejeitada com o Custo de { gw_order_header-estimated_costs }!|.
  msg_retorno = msg.

  CALL FUNCTION 'Z_GRAVA_LOG_PM'
    EXPORTING
      i_tp_msg   = 'S'
      i_mensagem = msg
      i_tcode    = sy-tcode.

  COMMIT WORK.
  WAIT UP TO 2 SECONDS.
  ok = abap_true.

ENDFUNCTION.
