FUNCTION Z_VISUALIZA_MSG_TELA.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_POPUP) TYPE  CHAR01 DEFAULT 'X'
*"  TABLES
*"      IT_RETORNO STRUCTURE  BAPIRET2
*"--------------------------------------------------------------------

  DATA: wa_retorno   TYPE bapiret2,
        wa_mensagens TYPE ty_mensagens,
        vg_msgno     TYPE sy-msgno.

  clear: it_mensagens[].

  LOOP AT it_retorno INTO wa_retorno.

    MOVE-CORRESPONDING wa_retorno TO wa_mensagens.
    MOVE: wa_mensagens-number TO vg_msgno.

    IF wa_retorno-message IS INITIAL.
      MESSAGE ID wa_mensagens-id
         TYPE wa_mensagens-type
       NUMBER vg_msgno
         WITH wa_mensagens-message_v1 wa_mensagens-message_v2 wa_mensagens-message_v3 wa_mensagens-message_v4
         INTO wa_mensagens-texto.
    ELSE.
      wa_mensagens-texto = wa_retorno-message.
    ENDIF.

    CASE wa_mensagens-type.
      WHEN c_e.
        wa_mensagens-icons = icon_led_red.
      WHEN c_s.
        wa_mensagens-icons = icon_led_green.
      WHEN c_w.
        wa_mensagens-icons = icon_led_yellow.
    ENDCASE.

    APPEND wa_mensagens TO it_mensagens.

  ENDLOOP.

  usa_popup = p_popup.

  IF NOT p_popup IS INITIAL.
    CALL SCREEN 9999 STARTING AT 03 03 ENDING AT 120 25.
  ELSE.
    CALL SCREEN 9998.
  ENDIF.

ENDFUNCTION.
