*----------------------------------------------------------------------*
***INCLUDE ZSDR0060_5522.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_5522  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5522 OUTPUT.

  FREE: t_observ, t_text.

  l_id     = 'Z001'.
  l_object = 'ZOBSERVAC'.
  CONCATENATE wa_5522-nro_sol wa_5522-seq_cam  wa_5522-seq
              wa_5522-filial_resp
         INTO l_name_text.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = l_id
      language                = sy-langu
      name                    = l_name_text
      object                  = l_object
    TABLES
      lines                   = t_observ
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF t_observ[] IS INITIAL.
    w_text = wa_5522-observacao+0(72).
    APPEND w_text TO t_text.
    w_text = wa_5522-observacao+72(72).
    APPEND w_text TO t_text.
    w_text = wa_5522-observacao+144(72).
    APPEND w_text TO t_text.
    w_text = wa_5522-observacao+216(39).
    APPEND w_text TO t_text.
  ELSE.
    LOOP AT t_observ INTO w_observ.
      w_text = w_observ-tdline.
      APPEND w_text    TO t_text.
    ENDLOOP.
  ENDIF.

  CREATE OBJECT zeditor
    EXPORTING
      im_title        = ' '
      im_display_mode = ' '
      im_longtext_tab = t_text.

  IF zeditor->container IS INITIAL.
    CALL METHOD zeditor->start.
  ENDIF.

  SET PF-STATUS 'PF5522'.
  SET TITLEBAR  'T5522'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5522_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5522_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5522  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5522 INPUT.

  CASE sy-ucomm.
    WHEN 'OK'.

      IF wa_5522-adiantamento IS INITIAL.
        MESSAGE 'É Obrigatório o Preenchimento do campo Adiantamento!' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      t_text = zeditor->get_text( ).

      CALL METHOD zeditor->free.
      CLEAR zeditor.

      READ TABLE t_text INTO w_text INDEX 1.
      wa_5522-observacao = w_text.

      FREE: t_observ.

      LOOP AT t_text   INTO w_text.
        w_observ-tdformat = '*'.
        w_observ-tdline   = w_text.
        APPEND w_observ  TO t_observ.
      ENDLOOP.

      CONCATENATE wa_5522-nro_sol wa_5522-seq_cam  wa_5522-seq
                  wa_5522-filial_resp
             INTO l_name_text.

      w_header-tdobject = 'ZOBSERVAC'.
      w_header-tdname   = l_name_text.
      w_header-tdid     = 'Z001'.
      w_header-tdspras  = sy-langu.

      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          client          = sy-mandt
          header          = w_header
*         INSERT          = ' '
          savemode_direct = 'X'
        TABLES
          lines           = t_observ
        EXCEPTIONS
          id              = 1
          language        = 2
          name            = 3
          object          = 4
          OTHERS          = 5.

      UPDATE zsdt0138 SET adiantamento = wa_5522-adiantamento
                            observacao = wa_5522-observacao
      WHERE nro_sol     EQ wa_5522-nro_sol
        AND seq_cam     EQ wa_5522-seq_cam
        AND seq         EQ wa_5522-seq
        AND filial_resp EQ wa_5522-filial_resp.

      CHECK sy-subrc IS INITIAL.

      LOOP AT it_caminhao_5620 ASSIGNING FIELD-SYMBOL(<caminhao>)
        WHERE nro_sol     EQ wa_5522-nro_sol AND
              seq_cam     EQ wa_5522-seq_cam AND
              seq         EQ wa_5522-seq     AND
              filial_resp EQ wa_5522-filial_resp.
        <caminhao>-adiantamento = wa_5522-adiantamento.
        <caminhao>-observacao = wa_5522-observacao.
      ENDLOOP.

      LOOP AT it_caminhao_5520 ASSIGNING FIELD-SYMBOL(<caminhao_5520>)" Rubenilson - 10.09.24 - 140377
        WHERE nro_sol     EQ wa_5522-nro_sol AND
              seq_cam     EQ wa_5522-seq_cam AND
              seq         EQ wa_5522-seq     AND
              filial_resp EQ wa_5522-filial_resp.
        <caminhao_5520>-adiantamento = wa_5522-adiantamento. " Rubenilson - 10.09.24 - 140377
        <caminhao_5520>-observacao = wa_5522-observacao." Rubenilson - 10.09.24 - 140377
      ENDLOOP.

      CALL FUNCTION 'Z_SD_AUTORIZACAO_EMBARQUE'
        EXPORTING
          nro_sol     = wa_5522-nro_sol
          seq_cam     = wa_5522-seq_cam
          seq         = wa_5522-seq
          frete       = wa_5522-frete
*-------CS2019001896 - 11.01.2021 - inicio
          filial_resp = wa_5522-filial_resp
*-------CS2019001896 - 11.01.2021 - inicio
          pto_coleta  = <caminhao>-pto_col.

      LEAVE TO SCREEN 0.

    WHEN 'CAN'.

      CALL METHOD zeditor->free.
      CLEAR zeditor.

      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      BREAK-POINT.
  ENDCASE.

ENDMODULE.
