*----------------------------------------------------------------------*
***INCLUDE LZGSD_CARGUERO_TROCA_NOTAO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  FREE: it_status,
        l_naotem_doc_age.

  IF wa_zsdt0001-docs_enviado_carguero = abap_false AND
     l_naotem_doc                      = abap_false.
    FREE: l_mensagem.

    SELECT SINGLE *
      INTO @DATA(w_zsdt0280)
      FROM zsdt0280
     WHERE lifnr = @l_lifnr.

    IF sy-subrc = 0.
      READ TABLE it_file_local INTO wa_file_local WITH KEY tipo_doc = '98'.
      IF sy-subrc <> 0.
        l_naotem_doc_age = abap_true.
        l_mensagem       = 'Necessário anexar PDF de agendamento!'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF wa_zsdt0001-docs_enviado_carguero = abap_true.
    APPEND 'ENVIAR' TO it_status.
  ENDIF.

  IF wa_zsdt0001-docs_enviado_carguero = abap_false.
    APPEND 'REMOVER' TO it_status.
  ENDIF.

  IF l_naotem_doc     = abap_true OR
     l_naotem_doc_age = abap_true.
    APPEND 'ENVIAR' TO it_status.
  ENDIF.

  SET PF-STATUS 'TROCA_NOTA' EXCLUDING it_status.
  SET TITLEBAR 'TROCA_NOTA'.

  LOOP AT SCREEN.
    IF wa_zsdt0001-docs_enviado_carguero = abap_true.
      IF screen-name(8) = 'BOT_FILE' OR
         screen-name    = 'BOTAO_ANEXAR'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-name(8) = 'BOT_FILE' OR
         screen-name    = 'BOTAO_ANEXAR'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name(8) = 'BOT_FILE'.
      l_campo = 'L_FILE' && screen-name+8(2).
      ASSIGN (l_campo) TO <f_campo>.
      IF sy-subrc = 0.
        IF <f_campo> IS INITIAL.
          screen-input  = '0'.
          screen-active = '0'.
          MODIFY SCREEN.
        ELSE.
          READ TABLE it_file_local INTO wa_file_local WITH KEY filename = <f_campo>.
          IF sy-subrc = 0.
            IF wa_file_local-bloq = abap_true.
              screen-input  = '0'.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.

    WHEN 'ANEXAR'.
      PERFORM f_anexar_arquivos.

    WHEN 'ENVIAR'.
*------------------------------------
*-----popup de confirmacao
*------------------------------------
      CALL SCREEN 0101 STARTING AT  64 07
                         ENDING AT 144 13.
      LEAVE TO SCREEN 0.

    WHEN 'REMOVER'.
      PERFORM f_remove_carguero.
      l_abandona_tela = abap_true.
      LEAVE TO SCREEN 0.

    WHEN 'CANCELAR'.
      l_abandona_tela = abap_true.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      l_abandona_tela = abap_true.
      LEAVE TO SCREEN 0.

    WHEN '&CANCEL'.
      l_abandona_tela = abap_true.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      IF ok_code(4) = 'DELF'.
        PERFORM f_elimina_arquivos USING ok_code.
      ENDIF.

  ENDCASE.

  CLEAR ok_code.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  SET PF-STATUS 'TROCA_NOTA2' EXCLUDING it_status.
  SET TITLEBAR 'TROCA_NOTA2'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  CASE ok_code2.

    WHEN 'VER_DOCS'.
      PERFORM f_visualiza_arquivos.

    WHEN 'CONF_ENVIO'.
      PERFORM f_envia_carguero.

      l_abandona_tela = abap_true.
      LEAVE TO SCREEN 0.

    WHEN 'VOLTAR'.
      l_abandona_tela = abap_false.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      l_abandona_tela = abap_false.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CLEAR ok_code2.

ENDMODULE.
