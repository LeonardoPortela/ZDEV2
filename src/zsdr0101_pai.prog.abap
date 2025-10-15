*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_PAI
*&---------------------------------------------------------------------*


MODULE user_command_0100 INPUT.

  DATA: it_zsdt0006 TYPE TABLE OF zsdt0006,
        wl_zsdt0006 TYPE zsdt0006.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.

      CALL METHOD obj_alv_0100->check_changed_data.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Deseja realmente salvar os registros?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = var_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK var_answer EQ '1'.

      CLEAR: it_zsdt0006[].

      LOOP AT it_saida_0100 INTO wa_saida_0100 WHERE ck_modify EQ abap_true.
        CLEAR: wl_zsdt0006.
        MOVE-CORRESPONDING wa_saida_0100 TO wl_zsdt0006.

        IF wl_zsdt0006-auart IS INITIAL.
          MESSAGE 'Tp.OV é um campo obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        APPEND wl_zsdt0006 TO it_zsdt0006.
      ENDLOOP.

      MODIFY zsdt0006 FROM TABLE it_zsdt0006.

      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE 'Registros salvos com sucesso!' TYPE 'S'.
        PERFORM: f_selecionar_dados,
                 f_processa_dados.
        LEAVE TO SCREEN 0100.
      ELSE.
        MESSAGE 'Houve um erro ao salvar os registros!' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN 'OBS_BRANCH'.

      CLEAR: it_sel_rows[], wa_sel_rows.

      CALL METHOD obj_alv_0100->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows.

      IF it_sel_rows[] IS INITIAL.
        MESSAGE 'Selecione uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF lines( it_sel_rows ) NE 1.
        MESSAGE 'Selecione apenas uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
      CHECK sy-subrc = 0.

      READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.
      CHECK sy-subrc = 0.

      PERFORM f_call_screen_0120.

    WHEN 'CAD_OBS'.

      CLEAR: it_sel_rows[], wa_sel_rows.

**      CALL METHOD obj_alv_0100->get_selected_rows
**        IMPORTING
**          et_index_rows = it_sel_rows.
**
**      IF it_sel_rows[] IS INITIAL.
**        MESSAGE 'Selecione uma linha!' TYPE 'S'.
**        EXIT.
**      ENDIF.
**
**      IF lines( it_sel_rows ) NE 1.
**        MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
**        EXIT.
**      ENDIF.
**
**      READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
**      CHECK sy-subrc = 0.
**
**      READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.
**      CHECK sy-subrc = 0.

      PERFORM f_call_screen_0130.

    WHEN 'PARAM_OBS'.

      CLEAR: it_sel_rows[], wa_sel_rows.

      CALL METHOD obj_alv_0100->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows.

      IF it_sel_rows[] IS INITIAL.
        MESSAGE 'Selecione uma linha!' TYPE 'S'  DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF lines( it_sel_rows ) NE 1.
        MESSAGE 'Selecione apenas uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
      CHECK sy-subrc = 0.

      READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.
      CHECK sy-subrc = 0.

      PERFORM f_call_screen_0140.

    WHEN 'PAR_SEMOV'.

      CLEAR: it_sel_rows[], wa_sel_rows.

      CALL METHOD obj_alv_0100->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows.

      IF it_sel_rows[] IS NOT INITIAL.
        MESSAGE 'Desmarque todas as linhas selecionadas na lista abaixo.' TYPE 'S'  DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      PERFORM f_call_screen_0150.

    WHEN 'EXEC'.
      PERFORM: f_selecionar_dados,
               f_processa_dados,
               f_refresh_alv USING '0100'.
  ENDCASE.

ENDMODULE.

MODULE user_command_0110 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE user_command_0120 INPUT.

  DATA: it_zsdt0294 TYPE TABLE OF zsdt0294,
        wl_zsdt0294 TYPE zsdt0294.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      CALL METHOD obj_alv_0120->check_changed_data.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Deseja realmente salvar os registros?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = var_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK var_answer EQ '1'.

      CLEAR: it_zsdt0294[].

      LOOP AT it_saida_0120 INTO wa_saida_0120 WHERE ck_modify EQ abap_true.
        CLEAR: wl_zsdt0294.
        MOVE-CORRESPONDING wa_saida_0120 TO wl_zsdt0294.

        IF wl_zsdt0294-auart IS INITIAL.
          MESSAGE 'Tp.OV é um campo obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF wl_zsdt0294-branch IS INITIAL.
          MESSAGE 'Filial é um campo obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        wl_zsdt0294-dt_registro = sy-datum.
        wl_zsdt0294-hr_registro = sy-uzeit.
        wl_zsdt0294-us_registro = sy-uname.

        APPEND wl_zsdt0294 TO it_zsdt0294.
      ENDLOOP.

      MODIFY zsdt0294 FROM TABLE it_zsdt0294.

      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE 'Registros salvos com sucesso!' TYPE 'S' .
        PERFORM: f_selecionar_dados,
                 f_processa_dados.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao salvar os registros!' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0130  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0130 INPUT.



  DATA: it_zsdt0287 TYPE TABLE OF zsdt0287,
        wl_zsdt0287 TYPE zsdt0287,
        w_0287      TYPE zsdt0287.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      CALL METHOD obj_alv_0130->check_changed_data.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Deseja realmente salvar os registros?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = var_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK var_answer EQ '1'.

      CLEAR: it_zsdt0287[].

      LOOP AT it_saida_0130 INTO wa_saida_0130 WHERE ck_modify EQ abap_true.
        CLEAR: wl_zsdt0287.
        MOVE-CORRESPONDING wa_saida_0130 TO wl_zsdt0287.

*        IF wl_zsdt0287-ID_OBS IS INITIAL.
*          MESSAGE 'Tp.OV é um campo obrigatório!' TYPE 'S'.
*          RETURN.
*        ENDIF.

        IF wl_zsdt0287-observ IS INITIAL.
          MESSAGE 'Informar Uma Observação!' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        wl_zsdt0287-dt_registro = sy-datum.
        wl_zsdt0287-hr_registro = sy-uzeit.
        wl_zsdt0287-us_registro = sy-uname.

        APPEND wl_zsdt0287 TO it_zsdt0287.
      ENDLOOP.

*-BUG 65582-17.05.2022-JT-inicio
*     DELETE it_zsdt0287 WHERE id_obs IS INITIAL. "Deletar ID_OBS em branco
*-BUG 65582-17.05.2022-JT-fim

      PERFORM f_grava_zsdt0287 TABLES it_zsdt0287
                                USING w_0287.

*     MODIFY zsdt0287 FROM TABLE it_zsdt0287.

      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE 'Registros salvos com sucesso!' TYPE 'S'.
        PERFORM: f_selecionar_dados,
                 f_processa_dados.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao salvar os registros!' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0140  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0140 INPUT.

  "DATA: it_zsdt0287 TYPE TABLE OF ZSDT0287,
  "     wl_zsdt0287 TYPE ZSDT0287.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      CALL METHOD obj_alv_0140->check_changed_data.

      CHECK l_erro = abap_false.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Deseja realmente salvar os registros?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = var_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK var_answer EQ '1'.

      CLEAR: tg_zsdt0294[].

      LOOP AT it_saida_0140 INTO wa_saida_0140 WHERE ck_modify EQ abap_true.
        CLEAR: wl_zsdt0294 .
        MOVE-CORRESPONDING wa_saida_0140 TO wl_zsdt0294 .

*        IF  wl_zsdt0294-branch IS INITIAL.
*          MESSAGE 'Filial é um campo obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
*          RETURN.
*        ENDIF.

*       IF  wl_zsdt0294-matnr IS INITIAL.
*          MESSAGE 'Material é um campo obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
*          RETURN.
*        ENDIF.

        IF wl_zsdt0294-id_obs IS INITIAL.
          MESSAGE 'Informar Uma Observação!' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.

        ELSE.
*-CS2022000324-25.08.2022-#84903-JT-inicio
          IF wl_zsdt0294-matnr IS NOT INITIAL AND wl_zsdt0294-matkl IS NOT INITIAL.
            MESSAGE 'Informe apenas Grupo de Mercadoria ou Cod. Material' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
*-CS2022000324-25.08.2022-#84903-JT-fim

          SELECT * FROM zsdt0287
            INTO TABLE @DATA(xtl_zsdt0287)
            WHERE id_obs EQ @wl_zsdt0294-id_obs
            AND cancelado NE 'X'.

          IF ( sy-subrc IS NOT INITIAL ).
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH |O ID { wl_zsdt0294-id_obs } informado na OV |
                                                   |{ wl_zsdt0294-auart } e filial { wl_zsdt0294-branch } está inativo!|.
            RETURN.

          ELSE.
            wl_zsdt0294-dt_registro = sy-datum.
            wl_zsdt0294-hr_registro = sy-uzeit.
            wl_zsdt0294-us_registro = sy-uname.

            APPEND wl_zsdt0294 TO tg_zsdt0294.
          ENDIF.
        ENDIF.
      ENDLOOP.

      MODIFY zsdt0294 FROM TABLE tg_zsdt0294.
      " UPDATE  zsdt0294 FROM TABLE tg_zsdt0294.
      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE 'Registros salvos com sucesso!' TYPE 'S' .
        PERFORM: f_selecionar_dados,
                 f_processa_dados.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao salvar os registros!' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.

MODULE user_command_0150 INPUT.

  "DATA: it_zsdt0287 TYPE TABLE OF ZSDT0287,
  "     wl_zsdt0287 TYPE ZSDT0287.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      CALL METHOD obj_alv_0150->check_changed_data.

      CHECK l_erro = abap_false.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Deseja realmente salvar os registros?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = var_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK var_answer EQ '1'.

      CLEAR: tg_zsdt0294[].

      LOOP AT it_saida_0150 INTO wa_saida_0150 WHERE ck_modify EQ abap_true.
        CLEAR: wl_zsdt0294 .
        MOVE-CORRESPONDING wa_saida_0150 TO wl_zsdt0294 .

        IF  wl_zsdt0294-branch IS INITIAL AND
            wl_zsdt0294-matnr  IS INITIAL AND
            wl_zsdt0294-matkl  IS INITIAL.   "*-CS2022000324-25.08.2022-#84903-JT-inicio
          MESSAGE 'Filial ou Material ou Grupo de Material devem ser preenchidos!' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

*-CS2022000324-25.08.2022-#84903-JT-inicio
        IF wl_zsdt0294-matnr IS NOT INITIAL AND wl_zsdt0294-matkl IS NOT INITIAL.
          MESSAGE 'Informe apenas Grupo de Mercadoria ou Cod. Material' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
*-CS2022000324-25.08.2022-#84903-JT-fim

        SELECT branch
          INTO @DATA(l_branch)
          FROM zsdt0294
            UP TO 1 ROWS
         WHERE auart          = @abap_off
           AND branch         = @wa_saida_0150-branch
           AND id_obs         = @wa_saida_0150-id_obs
           AND matkl          = @wa_saida_0150-matkl
           AND matnr          = @wa_saida_0150-matnr
           AND inf_add_prod   = @wa_saida_0150-inf_add_prod "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP
           AND cancelado      = @abap_off.
        ENDSELECT.

        IF sy-subrc = 0.
          MESSAGE 'Esta combinação já está cadastrada!' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

*       IF  wl_zsdt0294-matnr IS INITIAL.
*          MESSAGE 'Material é um campo obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
*          RETURN.
*        ENDIF.

        "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP -->>>
*        IF wl_zsdt0294-id_obs IS INITIAL.
*          MESSAGE 'Informar Uma Observação!' TYPE 'S' DISPLAY LIKE 'E'.
*          RETURN.
*        ENDIF.

        CASE wa_saida_0150-inf_add_prod.
          WHEN abap_true.

            IF wl_zsdt0294-id_obs IS INITIAL AND wl_zsdt0294-tx_item_material is INITIAL.
              MESSAGE 'Nenhuma configuração de observações definida para o item!' TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.

          WHEN abap_false.

            IF wl_zsdt0294-id_obs IS INITIAL.
              MESSAGE 'Informar um Id. Observação!' TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.

            if wl_zsdt0294-tx_item_material is not INITIAL.
              MESSAGE '"Texto Item Material" só pode ser definido se a configuração for de "Informação Adicional Produto"!' TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
            endif.

        ENDCASE.
        "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP <<---


        wl_zsdt0294-dt_registro = sy-datum.
        wl_zsdt0294-hr_registro = sy-uzeit.
        wl_zsdt0294-us_registro = sy-uname.

        APPEND wl_zsdt0294 TO tg_zsdt0294.
      ENDLOOP.

      MODIFY zsdt0294 FROM TABLE tg_zsdt0294.
      " UPDATE  zsdt0294 FROM TABLE tg_zsdt0294.
      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE 'Registros salvos com sucesso!' TYPE 'S' .
        PERFORM: f_selecionar_dados,
                 f_processa_dados.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao salvar os registros!' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
