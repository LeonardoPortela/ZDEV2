
MODULE user_command_0100 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'T0100'.

  CREATE OBJECT lo_report.
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

  ENDCASE.
ENDFORM.

FORM gravar.

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

*      IF <fs_changed>-bukrs IS NOT INITIAL.
*
*      ELSE.
*        CLEAR: wa_erro.
*        wa_erro = |"Empresa" não pode ser vazio!|.
*        APPEND wa_erro TO it_erro.
*        CLEAR: wa_erro.
*      ENDIF.
*
*      IF <fs_changed>-centro_desp IS NOT INITIAL.
*
*      ELSE.
*        CLEAR: wa_erro.
*        wa_erro = |"Centro" não pode ser vazio!|.
*        APPEND wa_erro TO it_erro.
*        CLEAR: wa_erro.
*      ENDIF.
*
*
*      IF <fs_changed>-valor_de IS NOT INITIAL.
*
*      ELSE.
*        CLEAR: wa_erro.
*        wa_erro = |"Valor de" não pode ser vazio!|.
*        APPEND wa_erro TO it_erro.
*        CLEAR: wa_erro.
*      ENDIF.
*
*      IF <fs_changed>-valor_ate IS NOT INITIAL.
*
*      ELSE.
*        CLEAR: wa_erro.
*        wa_erro = |"Valor até" não pode ser vazio!|.
*        APPEND wa_erro TO it_erro.
*        CLEAR: wa_erro.
*      ENDIF.
*
*      IF <fs_changed>-waers IS NOT INITIAL.
*
*      ELSE.
*        CLEAR: wa_erro.
*        wa_erro = |"Moeda" não pode ser vazio!|.
*        APPEND wa_erro TO it_erro.
*        CLEAR: wa_erro.
*      ENDIF.
*
*      IF <fs_changed>-permit IS NOT INITIAL.
*
*      ELSE.
*        CLEAR: wa_erro.
*        wa_erro = |"Permit" não pode ser vazio!|.
*        APPEND wa_erro TO it_erro.
*        CLEAR: wa_erro.
*      ENDIF.
*
*      IF <fs_changed>-nivel IS NOT INITIAL.
*
*      ELSE.
*        CLEAR: wa_erro.
*        wa_erro = |"Nível" não pode ser vazio!|.
*        APPEND wa_erro TO it_erro.
*        CLEAR: wa_erro.
*      ENDIF.
*
*
*      IF <fs_changed>-kostl IS NOT INITIAL.
*        CONDENSE <fs_changed>-kostl NO-GAPS.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = <fs_changed>-kostl
*          IMPORTING
*            output = <fs_changed>-kostl.
*
*        SELECT SINGLE * FROM csks
*        WHERE kokrs = 'MAGI'
*        AND datbi >= @sy-datum
*        AND bukrs = @<fs_changed>-bukrs
*        AND gsber = @<fs_changed>-centro_desp
*        AND kostl = @<fs_changed>-kostl
*        INTO @DATA(ls_csks).
*
*        IF sy-subrc = 0.
*          <fs_changed>-kostl = ls_csks-kostl.
*          SELECT SINGLE ktext FROM cskt WHERE kostl = @<fs_changed>-kostl AND spras = 'P' INTO @<fs_changed>-ktext.
*        ELSE.
*          CLEAR: wa_erro.
*          wa_erro = |"Centro de Custo" informado { <fs_changed>-kostl },não é valido!|.
*          APPEND wa_erro TO it_erro.
*          CLEAR: wa_erro.
*          CLEAR: <fs_changed>-kostl.
*        ENDIF.
*      ELSE.
*        CLEAR: wa_erro.
*        wa_erro = |"Centro de Custo" não pode ser vazio!|.
*        APPEND wa_erro TO it_erro.
*        CLEAR: wa_erro.
*      ENDIF.
*
*      IF <fs_changed>-aprovador IS NOT INITIAL.
*        CONDENSE <fs_changed>-aprovador NO-GAPS.
*        SELECT SINGLE * FROM usr21 WHERE bname = @<fs_changed>-aprovador INTO @DATA(ls_aprova).
*        IF sy-subrc = 0.
*          <fs_changed>-aprovador = ls_aprova-bname.
*        ELSE.
*          CLEAR: wa_erro.
*          wa_erro = |"Aprovador" { <fs_changed>-aprovador },não é valido!|.
*          APPEND wa_erro TO it_erro.
*          CLEAR: wa_erro.
*          CLEAR: <fs_changed>-aprovador.
*        ENDIF.
*      ELSE.
*        wa_erro = |"Aprovador" não pode ser vazio!|.
*        APPEND wa_erro TO it_erro.
*        CLEAR: wa_erro.
*      ENDIF.
*
*
*      IF <fs_changed>-usua_subst IS NOT INITIAL.
*        CONDENSE <fs_changed>-usua_subst NO-GAPS.
*        SELECT SINGLE * FROM usr21 WHERE bname = @<fs_changed>-usua_subst INTO @DATA(ls_subst).
*        IF sy-subrc = 0.
*          <fs_changed>-usua_subst = ls_subst-bname.
*        ELSE.
*          CLEAR: wa_erro.
*          wa_erro = |"Usuério substituto" { <fs_changed>-usua_subst },não é valido!|.
*          APPEND wa_erro TO it_erro.
*          CLEAR: wa_erro.
*          CLEAR: <fs_changed>-usua_subst.
*        ENDIF.
*      ENDIF.
*
*
*      IF <fs_changed>-usua_subst IS INITIAL AND <fs_changed>-data_lim IS INITIAL.
*
*      ELSE.
*
*        IF <fs_changed>-usua_subst IS  NOT INITIAL AND <fs_changed>-data_lim IS INITIAL.
*
*          CLEAR: wa_erro.
*          wa_erro = |"Data Limite" não pode ser vazia  se "Aprpovador Subst." existir!|.
*          APPEND wa_erro TO it_erro.
*          CLEAR: wa_erro.
*
*        ELSEIF <fs_changed>-usua_subst IS  INITIAL AND <fs_changed>-data_lim IS NOT INITIAL.
*          CLEAR: wa_erro.
*          wa_erro = |"Aprpovador Subst." não pode ser vazia se "Data Limite" existir!|.
*          APPEND wa_erro TO it_erro.
*          CLEAR: wa_erro.
*
*         elseIF <fs_changed>-data_lim IS not INITIAL and <fs_changed>-usua_subst IS not INITIAL AND <fs_changed>-data_lim < sy-datum.
*          CLEAR: wa_erro.
*          wa_erro = |"Data Limite" pode estar no passado!|.
*          APPEND wa_erro TO it_erro.
*          CLEAR: wa_erro.
*
*        ENDIF.
*
*      ENDIF.
*
*
*      IF it_erro IS NOT INITIAL.
*
*        LOOP AT it_erro ASSIGNING FIELD-SYMBOL(<fs_erro>).
*          ls_tab-msgid  = 'E4'.
*          ls_tab-msgno  = '000'.
*          ls_tab-msgty  = 'E'.
*          ls_tab-msgv1  = |Linha { linha } - { <fs_erro> }|.
*          ls_tab-lineno = sy-tabix.
*          APPEND ls_tab TO lt_tab.
*          CLEAR: ls_tab.
*        ENDLOOP.
*
*      ENDIF.

    ENDLOOP.


    IF lt_tab IS NOT INITIAL.
      SORT lt_tab.

      DELETE ADJACENT DUPLICATES FROM lt_tab.
      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_tab.
    ELSE.

      LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
        MOVE-CORRESPONDING <fs_saida> TO wa_znotaimpad.
        APPEND WA_znotaimpad TO it_znotaimpad.
        CLEAR:WA_znotaimpad.
      ENDLOOP.


      IF IT_znotaimpad IS NOT INITIAL.
        LOOP AT IT_znotaimpad INTO WA_znotaimpad WHERE nr_adicao IS NOT INITIAL.
          "UPDATE znotaimpad FROM WA_znotaimpad.
          MODIFY znota_import_ad FROM WA_znotaimpad.
          COMMIT WORK.
        ENDLOOP.
      ELSE.
        MESSAGE 'Não é possivel Inserir ou Editar no Tipo Edição!' TYPE 'I'.
      ENDIF.

      FREE: it_saida.
      CLEAR:wa_saida.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

ENDFORM.
