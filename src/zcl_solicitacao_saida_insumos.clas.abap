class ZCL_SOLICITACAO_SAIDA_INSUMOS definition
  public
  final
  create public .

public section.

  class-methods CANCELAR
    importing
      !IV_NRO_SOL type ZDE_NRO_SOL
      !IV_VBELN type VBELN_VA
      !IV_POSNR type POSNR_VA
      !IV_NAO_VALIDAR_BLOQUEIO type FLAG optional
      !IV_POPUP_TO_CONFIRM type FLAG default SPACE
    exporting
      !EV_ERRO type FLAG
    returning
      value(RV_MESSAGE) type STRING .
  class-methods SHOW_MESSAGE
    importing
      !IV_MESSAGE type STRING
      !IV_MSGTY type SYMSGTY default 'I' .
protected section.
private section.

  class-methods POPUP_TO_CONFIRM
    importing
      !IV_MESSAGE type STRING
    returning
      value(RV_RET) type FLAG .
  class-methods STRING_TO_CHAR
    importing
      !IV_STRING type STRING
    exporting
      !EV_MSG01 type SYST_MSGV
      !EV_MSG02 type SYST_MSGV
      !EV_MSG03 type SYST_MSGV
      !EV_MSG04 type SYST_MSGV .
ENDCLASS.



CLASS ZCL_SOLICITACAO_SAIDA_INSUMOS IMPLEMENTATION.


  METHOD cancelar.

    DATA lv_param TYPE string.
    DATA lv_msg TYPE string.

    CLEAR rv_message.

    IF iv_popup_to_confirm = abap_true.

      DATA(lv_ret) = popup_to_confirm( 'Confirmar cancelamento solicitação' ).

      IF lv_ret <> '1'.
        ev_erro = abap_true.
      ENDIF.

    ENDIF.


    lv_param =  `Núm.Sol: ` && |{ iv_nro_sol ALPHA = OUT }| &&
                ` OV: ` &&  |{ iv_vbeln ALPHA = OUT }| &&
                ` Item: ` &&  |{ iv_posnr ALPHA = OUT }| && `: `.

    CONDENSE lv_param.

    SELECT SINGLE nro_sol, bloqueio
          INTO @DATA(_zsdt0082)
          FROM zsdt0082
         WHERE nro_sol  = @iv_nro_sol
           AND vbeln    = @iv_vbeln
           AND posnr    = @iv_posnr
           AND status  <> '3'
           AND seq      = '001'.

    IF sy-subrc = 0 AND _zsdt0082-bloqueio = abap_true AND iv_nao_validar_bloqueio = abap_false.  "*-US192801-06.10.2025-#192801-JT

      MESSAGE s024(sd) WITH 'Solicitacao está Pendente na Distribuicao!!'
        INTO lv_msg.

      rv_message = lv_param && lv_msg.

      EXIT.

    ENDIF.

    lv_msg = NEW zcl_util_sd( )->cancelar_ordem_vendas(
     EXPORTING
       i_nro_sol   = iv_nro_sol
       i_vbeln     = iv_vbeln
       i_posnr     = iv_posnr
   ).

    CHECK lv_msg IS NOT INITIAL.

    rv_message = lv_param && lv_msg.

  ENDMETHOD.


  METHOD popup_to_confirm.

*    DATA lv_texto TYPE c LENGTH 120.
*    DATA lv_var TYPE c LENGTH 30.
*
*    string_to_char(
*      EXPORTING
*        iv_string = iv_message
*      IMPORTING
*          ev_msg01 = DATA(lv_msg01)
*          ev_msg02 = DATA(lv_msg02)
*          ev_msg03 = DATA(lv_msg03)
*          ev_msg04 = DATA(lv_msg04) ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'Confirmar execução'
        text_question  = iv_message
      IMPORTING
        answer         = rv_ret
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      rv_ret = 'A'.
    ENDIF.

  ENDMETHOD.


  METHOD show_message.

    string_to_char(
      EXPORTING
        iv_string = iv_message
      IMPORTING
          ev_msg01 = DATA(lv_msgv01)
          ev_msg02 = DATA(lv_msgv02)
          ev_msg03 = DATA(lv_msgv03)
          ev_msg04 = DATA(lv_msgv04) ).

    CASE iv_msgty.
      WHEN 'S' OR 'E' OR 'A'.
        MESSAGE s016(ds) WITH lv_msgv01 lv_msgv02 lv_msgv03 lv_msgv04 DISPLAY LIKE iv_msgty.
      WHEN OTHERS.
        MESSAGE ID 'DS' TYPE iv_msgty NUMBER 016 WITH lv_msgv01 lv_msgv02 lv_msgv03 lv_msgv04.
    ENDCASE.


  ENDMETHOD.


  METHOD string_to_char.

    DATA lv_texto(4000).
    DATA lv_num TYPE c.
    DATA lv_field TYPE c LENGTH 30.

    DATA lt_trtexts TYPE trtexts.

    lv_texto = iv_string.

    CALL FUNCTION 'TR_SPLIT_TEXT'
      EXPORTING
        iv_text  = lv_texto
        iv_len   = 40
      IMPORTING
        et_lines = lt_trtexts.

    DO 4 TIMES.

      READ TABLE lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>) INDEX sy-index.

      CHECK sy-subrc EQ 0.

      lv_num = sy-index.

      lv_field = 'EV_MSG0' && lv_num.

      ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_ret>).

      <fs_ret> = <fs_line>.

    ENDDO.


  ENDMETHOD.
ENDCLASS.
