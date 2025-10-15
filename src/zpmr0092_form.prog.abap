
MODULE user_command_0100 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'T0100'.
  CREATE OBJECT lo_report.
  "lo_report->get_data( ).
  lo_report->generate_output( ).
  lo_report->set_refresh( ).

ENDMODULE.

FORM action_process.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'SAVE'. "GRAVAR

  ENDCASE.
ENDFORM.

FORM gravar.

*      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*        EXPORTING
*          functioncode           = '=ENT'
*        EXCEPTIONS
*          function_not_supported = 1
*          OTHERS                 = 2.

  "lo_report->on_user_command( e_salv_function = 'SAVE' ).

  IF it_saida IS NOT INITIAL.

    TYPE-POOLS: esp1.

    DATA: lt_tab TYPE esp1_message_tab_type.
    DATA: ls_tab TYPE esp1_message_wa_type.
    DATA: it_erro TYPE STANDARD TABLE OF string INITIAL SIZE 0.
    DATA: wa_erro TYPE string.
    DATA: linha TYPE i.

    FREE:lt_tab.

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_changed>).
      CLEAR: linha.
      linha = sy-tabix.

      IF <fs_changed>-bukrs IS NOT INITIAL.

      ELSE.
        CLEAR: wa_erro.
        wa_erro = |"Empresa" não pode ser vazio!|.
        APPEND wa_erro TO it_erro.
        CLEAR: wa_erro.
      ENDIF.

      IF <fs_changed>-centro_desp IS NOT INITIAL.

      ELSE.
        CLEAR: wa_erro.
        wa_erro = |"Centro" não pode ser vazio!|.
        APPEND wa_erro TO it_erro.
        CLEAR: wa_erro.
      ENDIF.


      IF <fs_changed>-valor_de IS NOT INITIAL.

      ELSE.
        CLEAR: wa_erro.
        wa_erro = |"Valor de" não pode ser vazio!|.
        APPEND wa_erro TO it_erro.
        CLEAR: wa_erro.
      ENDIF.

      IF <fs_changed>-valor_ate IS NOT INITIAL.

      ELSE.
        CLEAR: wa_erro.
        wa_erro = |"Valor até" não pode ser vazio!|.
        APPEND wa_erro TO it_erro.
        CLEAR: wa_erro.
      ENDIF.

      IF <fs_changed>-waers IS NOT INITIAL.

      ELSE.
        CLEAR: wa_erro.
        wa_erro = |"Moeda" não pode ser vazio!|.
        APPEND wa_erro TO it_erro.
        CLEAR: wa_erro.
      ENDIF.

      IF <fs_changed>-permit IS NOT INITIAL.

      ELSE.
        CLEAR: wa_erro.
        wa_erro = |"Permit" não pode ser vazio!|.
        APPEND wa_erro TO it_erro.
        CLEAR: wa_erro.
      ENDIF.

      IF <fs_changed>-nivel IS NOT INITIAL.

      ELSE.
        CLEAR: wa_erro.
        wa_erro = |"Nível" não pode ser vazio!|.
        APPEND wa_erro TO it_erro.
        CLEAR: wa_erro.
      ENDIF.


      IF <fs_changed>-supl_orc IS NOT INITIAL.

      ELSE.
        CLEAR: wa_erro.
        wa_erro = |"Supl. Orçamento" não pode ser vazio!|.
        APPEND wa_erro TO it_erro.
        CLEAR: wa_erro.
      ENDIF.

      IF <fs_changed>-aprovador IS NOT INITIAL.
        CONDENSE <fs_changed>-aprovador NO-GAPS.
        SELECT SINGLE * FROM usr21 WHERE bname = @<fs_changed>-aprovador INTO @DATA(ls_aprova).
        IF sy-subrc = 0.
          <fs_changed>-aprovador = ls_aprova-bname.
        ELSE.
          CLEAR: wa_erro.
          wa_erro = |"Aprovador" { <fs_changed>-aprovador },não é valido!|.
          APPEND wa_erro TO it_erro.
          CLEAR: wa_erro.
          CLEAR: <fs_changed>-aprovador.
        ENDIF.
      ELSE.
        wa_erro = |"Aprovador" não pode ser vazio!|.
        APPEND wa_erro TO it_erro.
        CLEAR: wa_erro.
      ENDIF.


      IF <fs_changed>-usua_subst IS NOT INITIAL.
        CONDENSE <fs_changed>-usua_subst NO-GAPS.
        SELECT SINGLE * FROM usr21 WHERE bname = @<fs_changed>-usua_subst INTO @DATA(ls_subst).
        IF sy-subrc = 0.
          <fs_changed>-usua_subst = ls_subst-bname.
        ELSE.
          CLEAR: wa_erro.
          wa_erro = |"Usuário substituto" { <fs_changed>-usua_subst },não é valido!|.
          APPEND wa_erro TO it_erro.
          CLEAR: wa_erro.
          CLEAR: <fs_changed>-usua_subst.
        ENDIF.
      ENDIF.


      IF <fs_changed>-data_lim IS INITIAL AND <fs_changed>-usua_subst IS INITIAL AND <fs_changed>-data_lim IS INITIAL.


      ELSE.

        IF <fs_changed>-usua_subst IS  NOT INITIAL AND <fs_changed>-data_lim IS INITIAL.

          CLEAR: wa_erro.
          wa_erro = |"Data Limite" não pode ser vazia se "Aprovador Subst." existir!|.
          APPEND wa_erro TO it_erro.
          CLEAR: wa_erro.

        ELSEIF <fs_changed>-usua_subst IS  INITIAL AND <fs_changed>-data_lim IS NOT INITIAL.
          CLEAR: wa_erro.
          wa_erro = |"Aprovador Subst." não pode ser vazia se "Data Limite" existir!|.
          APPEND wa_erro TO it_erro.
          CLEAR: wa_erro.

        ELSEIF <fs_changed>-data_lim IS NOT INITIAL AND <fs_changed>-usua_subst IS NOT INITIAL AND <fs_changed>-data_lim < sy-datum.
          CLEAR: wa_erro.
          wa_erro = |"Data Limite" pode estar no passado!|.
          APPEND wa_erro TO it_erro.
          CLEAR: wa_erro.

        ENDIF.

      ENDIF.


      IF it_erro IS NOT INITIAL.

        LOOP AT it_erro ASSIGNING FIELD-SYMBOL(<fs_erro>).
          ls_tab-msgid  = 'E4'.
          ls_tab-msgno  = '000'.
          ls_tab-msgty  = 'E'.
          ls_tab-msgv1  = |Linha { linha } - { <fs_erro> }|.
          ls_tab-lineno = sy-tabix.
          APPEND ls_tab TO lt_tab.
          CLEAR: ls_tab.
        ENDLOOP.

      ENDIF.

    ENDLOOP.


    IF lt_tab IS NOT INITIAL.
      SORT lt_tab.

      DELETE ADJACENT DUPLICATES FROM lt_tab.

      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_tab.
    ELSE.

      LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
        MOVE-CORRESPONDING <fs_saida> TO wa_zpmr0002.
        APPEND wa_zpmr0002 TO it_zpmr0002.
        CLEAR:wa_zpmr0002.
      ENDLOOP.
      CASE p_tipo.
        WHEN 'E'.

          IF it_zpmr0002 IS NOT INITIAL.
            LOOP AT it_zpmr0002 INTO wa_zpmr0002.

              "FF #185560 - inicio
              "=== Nova validação: verificando se o usuário já está na ZPMR0007 ===
              DATA(lv_exists_007) = abap_false.

              PERFORM check_user_in_zpmr0007 USING wa_zpmr0002-aprovador CHANGING lv_exists_007.

              IF lv_exists_007 = abap_true.
                CLEAR: ls_tab.
                ls_tab-msgid  = 'E4'.
                ls_tab-msgno  = '000'.
                ls_tab-msgty  = 'E'.
                ls_tab-msgv1  = |{ wa_zpmr0002-aprovador } já cadastrado na ZPM0027|.
                ls_tab-lineno = sy-tabix.
                APPEND ls_tab TO lt_tab.
                CLEAR: ls_tab.
                CONTINUE. "não grava este registro
              ENDIF.
              "===============================================================
              "FF #185560 - fim

              UPDATE zpmr0002 FROM wa_zpmr0002.
              COMMIT WORK.
            ENDLOOP.
          ELSE.
            MESSAGE 'Não é possivel Inserir ou Editar no Tipo Edição!' TYPE 'I'.
          ENDIF.

        WHEN 'I' OR 'C'.
          IF it_zpmr0002 IS NOT INITIAL.
            LOOP AT it_zpmr0002 INTO wa_zpmr0002.

              "FF #185560 - inicio


              "=== Nova validação: verificando se o usuário já está na ZPMR0007 ===
              lv_exists_007 = abap_false.

              PERFORM check_user_in_zpmr0007 USING wa_zpmr0002-aprovador CHANGING lv_exists_007.

              IF lv_exists_007 = abap_true.
                CLEAR: ls_tab.
                ls_tab-msgid  = 'E4'.
                ls_tab-msgno  = '000'.
                ls_tab-msgty  = 'E'.
                ls_tab-msgv1  = |Usuário { wa_zpmr0002-aprovador } já cadastrado na ZPM0027|.
                ls_tab-lineno = sy-tabix.
                APPEND ls_tab TO lt_tab.
                CLEAR: ls_tab.
                CONTINUE. "não grava este registro
              ENDIF.
              "===============================================================
              "FF #185560 - fim


              INSERT zpmr0002 FROM wa_zpmr0002.
              COMMIT WORK.
            ENDLOOP.
          ELSE.
            MESSAGE 'Não é possivel Editar no Tipo Inserir/Copiar!' TYPE 'I'.
          ENDIF.
      ENDCASE.
      "FF #185560 - inicio
      IF lt_tab IS NOT INITIAL.
        SORT lt_tab.
        DELETE ADJACENT DUPLICATES FROM lt_tab.

        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = lt_tab.
      ENDIF.
      "FF #185560 - fim
      FREE: it_saida.
      CLEAR:wa_saida.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

ENDFORM.

FORM check_user_in_zpmr0007 USING iv_user TYPE usnam
                            CHANGING cv_exists TYPE abap_bool.

  SELECT SINGLE usnam FROM zpmr0007 WHERE usnam = @iv_user INTO @DATA(dummy).
  cv_exists = xsdbool( sy-subrc = 0 ).

ENDFORM.
