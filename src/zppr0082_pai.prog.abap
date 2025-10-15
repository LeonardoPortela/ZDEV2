*&---------------------------------------------------------------------*
*&  Include           ZPPR0028_PAI
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.

  GET CURSOR FIELD w_cursor_field.

  IF w_cursor_field = 'ZEPM_APONTA_CAT_NOTAS-URTXT' OR
     w_cursor_field = 'ZEPM_APONTA_CAT_NOTAS-FETXT'.
    IF sy-ucomm = 'ENTER'.
      CLEAR sy-ucomm.
    ENDIF.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'DEL'.
      PERFORM elim_linha.
    WHEN 'V_ENC_NOTA'.
      PERFORM hab_enc_nota.
    WHEN 'V_ENC_ORDEM'.
      PERFORM hab_enc_ordem.
    WHEN 'REFRESH'.
*      LEAVE TO CURRENT TRANSACTION.

      PERFORM unlock_order.

      CALL SCREEN '0100'.
    WHEN 'BACK'.
      CLEAR obj_main->acao.
      PERFORM confirm_sair.
    WHEN 'CLEAR'.
      CLEAR obj_main->acao.
      PERFORM confirm_sair.
    WHEN 'EXIT'.
      CLEAR obj_main->acao.
      PERFORM confirm_sair.
    WHEN 'APONTAR' OR 'SAVE'.
      CLEAR vg_erro.
      PERFORM verifica_campos_vazios CHANGING v_tipo_ordem.
      IF vg_erro IS INITIAL.

        IF sy-ucomm = 'SAVE'.
*-US 145467-22-07-2024-#145467-RJF-Início
*          DATA: lv_nao_atualizar,
*                lv_gravar_ini,
*                lv_gravar_fim.
*          PERFORM validar_dados_avaria CHANGING lv_nao_atualizar
*                                                lv_gravar_ini
*                                                lv_gravar_fim
*                                                wa_viqmel.
*          IF lv_nao_atualizar IS INITIAL.

          obj_main->set_avaria( ).
*            EXPORTING i_grava   = lv_nao_atualizar
*                                            i_ini     = lv_gravar_ini
*                                            i_fim     = lv_gravar_fim
*                                            iw_viqmel = wa_viqmel ).
*          ENDIF.
*-US 145467-22-07-2024-#145467-RJF-fim

          DATA(lv_hr00) = 'HR00'. "Ordem para apontamento de horas improdutivas
          IF v_tipo_ordem <> lv_hr00.

            obj_main->set_save_nota( ).                     "#96115  FF
          ENDIF.
        ENDIF.

        obj_main->set_apontar( ).

        IF NOT line_exists(  gt_return_save[ type = 'E' ] ).

*-US 145467-22-07-2024-#145467-RJF-Início
          IF v_nota IS NOT INITIAL AND wa_viqmel-msaus IS NOT INITIAL.
            PERFORM shdb_modif_notif USING v_nota.
          ENDIF.
*-US 145467-22-07-2024-#145467-RJF-fim

          MESSAGE i185(/pwsati/zlfm). "Registro gravado com sucesso
          SET CURSOR FIELD 'V_ORDEM'.
          CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
            EXPORTING
              functioncode           = '=ENTER' "ENTER
            EXCEPTIONS
              function_not_supported = 1
              OTHERS                 = 2.

          PERFORM unlock_order.
          FREE: it_aponta. "*-US 145467-21-08-2024-#145467-RJF
          CALL SCREEN '0100'.
        ELSE.

          READ TABLE gt_return_save INTO DATA(wa_return_save) INDEX 1.
          MESSAGE i000(o0) WITH wa_return_save-message.
          CLEAR gt_return_save[].
        ENDIF.
      ENDIF.

    WHEN 'ENCORD'.
      obj_main->set_encerra_ordem( ).
    WHEN 'ENCONO'.
      obj_main->set_encerra_ordem_nota( ).
    WHEN 'EDIT'.
      obj_main->acao = sy-ucomm.
      obj_main->set_edit_nota( ).
*    WHEN 'SAVE'.
*      CLEAR obj_main->acao.
*      obj_main->set_save_nota( ).
    WHEN 'EXE'.
      GET CURSOR FIELD w_cursor_field.
      PERFORM check_bloq_ordem USING v_ordem CHANGING sy-subrc.
      IF sy-subrc = 0.
        v_ordem = |{ v_ordem ALPHA = IN }|.
        obj_main->set_estrutura( v_ordem ).
      ENDIF.
    WHEN 'ENTER'.
      PERFORM check_bloq_ordem USING v_ordem CHANGING sy-subrc.
      IF sy-subrc = 0.
        v_ordem = |{ v_ordem ALPHA = IN }|.
        CLEAR: v_nota, v_nota_descr, zepm_aponta_cat_notas.
        obj_main->set_estrutura( v_ordem ).
      ENDIF.
    WHEN 'DOUBLE'.
      PERFORM exibir_ordem_nota.
    WHEN 'ZPM0003'.
      PERFORM chamar_zpm0003.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_ORDEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_ordem INPUT.
  v_ordem = |{ v_ordem ALPHA = IN }|.
  obj_main->set_estrutura( v_ordem ).
  IF sy-ucomm <> 'VOLTAR'.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'TL001'.
  SET TITLEBAR 'TL002'.

*  IF zaponta-afrud >= 1.
*    zaponta-duration_normal_unit = 'H'.
*  ELSE.
*    zaponta-duration_normal_unit = 'MIN'.
*  ENDIF.

  IF it_aufk[] IS NOT INITIAL.
    IF sy-ucomm EQ 'V_NOVO'.
*      SET CURSOR FIELD 'ZAPONTA-ACTIVITY'.
      SET CURSOR FIELD 'ZAPONTA-PERNR'.
    ELSEIF sy-ucomm EQ 'V_CONFIRMA'.
*      SET CURSOR FIELD 'ZAPONTA-ACTIVITY'.
      SET CURSOR FIELD 'ZAPONTA-PERNR'.
    ELSEIF sy-ucomm EQ 'ENTER'.
      IF zaponta-pernr IS NOT INITIAL.
        IF it_empregado-sname IS INITIAL.
          SET CURSOR FIELD 'ZAPONTA-PERNR'.
        ELSEIF zaponta-grund IS NOT INITIAL.
          IF it_selec_parada-grdtx IS INITIAL.
            SET CURSOR FIELD 'ZAPONTA-GRUND'.
          ELSE.
            CASE w_cursor_field.
              WHEN 'ZAPONTA-AUFNR'.
*                SET CURSOR FIELD 'ZAPONTA-ACTIVITY'.
*              WHEN 'ZAPONTA-ACTIVITY'.
                SET CURSOR FIELD 'ZAPONTA-PERNR'.
              WHEN 'ZAPONTA-PERNR'.
                SET CURSOR FIELD 'ZAPONTA-ISDD'.
              WHEN 'ZAPONTA-ISDD'.
                IF zaponta-isdd > sy-datum.
                  SET CURSOR FIELD 'ZAPONTA-ISDD'.
                ELSE.
                  SET CURSOR FIELD 'ZAPONTA-ISDZ'.
                ENDIF.
              WHEN 'ZAPONTA-ISDZ'.
                IF zaponta-isdd = sy-datum AND zaponta-isdz > sy-uzeit.
                  SET CURSOR FIELD 'ZAPONTA-ISDZ'.
                ELSE.
                  SET CURSOR FIELD 'ZAPONTA-IEDD'.
                ENDIF.
              WHEN 'ZAPONTA-IEDD'.
                IF zaponta-iedd > sy-datum.
                  SET CURSOR FIELD 'ZAPONTA-IEDD'.
                ELSE.
                  SET CURSOR FIELD 'ZAPONTA-IEDZ'.
                ENDIF.
              WHEN 'ZAPONTA-IEDZ'.
                IF soma_total > zaponta-einzh OR zaponta-isdd = sy-datum AND zaponta-isdz > sy-uzeit OR zaponta-iedd = sy-datum AND zaponta-iedz > sy-uzeit.
                  SET CURSOR FIELD 'ZAPONTA-IEDZ'.
                ELSE.
*                  SET CURSOR FIELD 'ZAPONTA-GRUND'.
*                  IF soma_total IS NOT INITIAL.
*                    zaponta-afrud = soma_total.
*                  ENDIF.
                ENDIF.
              WHEN 'ZAPONTA-GRUND'.
                SET CURSOR FIELD 'ZAPONTA-AFRUD'.
              WHEN 'ZAPONTA-AFRUD'.
                SET CURSOR FIELD 'ZAPONTA-DURATION_NORMAL_UNIT'.
              WHEN 'ZAPONTA-DURATION_NORMAL_UNIT'.
                SET CURSOR FIELD 'V_CONFIRMA'.
                PERFORM validar_dados.
            ENDCASE.
          ENDIF.
        ELSE.
          CASE w_cursor_field.
*            WHEN 'ZAPONTA-ACTIVITY'.
***              SET CURSOR FIELD 'ZAPONTA-PERNR'.

            WHEN 'ZAPONTA-PERNR'.
              SET CURSOR FIELD 'ZAPONTA-BUDAT'.
            WHEN   'ZAPONTA-BUDAT'.
              SET CURSOR FIELD 'ZAPONTA-MNCOD'.
            WHEN   'ZAPONTA-MNCOD'.
              SET CURSOR FIELD 'ZAPONTA-ISDD'.
            WHEN 'ZAPONTA-ISDD'.
              IF zaponta-isdd > sy-datum.
                SET CURSOR FIELD 'ZAPONTA-ISDD'.
              ELSE.
                SET CURSOR FIELD 'ZAPONTA-ISDZ'.
              ENDIF.
            WHEN 'ZAPONTA-ISDZ'.
              IF zaponta-isdd = sy-datum AND zaponta-isdz > sy-uzeit.
                SET CURSOR FIELD 'ZAPONTA-ISDZ'.
              ELSE.
                SET CURSOR FIELD 'ZAPONTA-IEDD'.
              ENDIF.
            WHEN 'ZAPONTA-IEDD'.
              IF zaponta-iedd > sy-datum.
                SET CURSOR FIELD 'ZAPONTA-IEDD'.
              ELSE.
                SET CURSOR FIELD 'ZAPONTA-IEDZ'.
              ENDIF.
            WHEN 'ZAPONTA-IEDZ'.
              IF soma_total > zaponta-einzh OR zaponta-isdd = sy-datum AND zaponta-isdz > sy-uzeit OR zaponta-iedd = sy-datum AND zaponta-iedz > sy-uzeit.
                SET CURSOR FIELD 'ZAPONTA-IEDZ'.
              ELSE.
*                SET CURSOR FIELD 'ZAPONTA-GRUND'.
*                IF soma_total IS NOT INITIAL.
*                  zaponta-afrud = soma_total.
*                ENDIF.

                SET CURSOR FIELD 'V_CONFIRMA'. "Botão verde da tela de apontamento.
                PERFORM validar_dados.
                PERFORM limpar_campos. "Hora, Ação e Trab. Real

              ENDIF.
            WHEN 'ZAPONTA-GRUND'.
              SET CURSOR FIELD 'ZAPONTA-AFRUD'.
            WHEN 'ZAPONTA-AFRUD'.
              SET CURSOR FIELD 'ZAPONTA-DURATION_NORMAL_UNIT'.
*            WHEN 'ZAPONTA-DURATION_NORMAL_UNIT'.
*              SET CURSOR FIELD 'V_CONFIRMA'.
*              PERFORM validar_dados.
          ENDCASE.
        ENDIF.
      ELSE.
        CASE w_cursor_field.
*          WHEN 'ZAPONTA-ACTIVITY'.
**            SET CURSOR FIELD 'ZAPONTA-PERNR'.
          WHEN 'ZAPONTA-PERNR'.
            SET CURSOR FIELD 'ZAPONTA-BUDAT'.
          WHEN   'ZAPONTA-BUDAT'.
            SET CURSOR FIELD 'ZAPONTA-MNCOD'.
          WHEN   'ZAPONTA-MNCOD'.
            SET CURSOR FIELD 'ZAPONTA-ISDD'.
          WHEN 'ZAPONTA-ISDD'.
            IF zaponta-isdd > sy-datum.
              SET CURSOR FIELD 'ZAPONTA-ISDD'.
            ELSE.
              SET CURSOR FIELD 'ZAPONTA-ISDZ'.
            ENDIF.
          WHEN 'ZAPONTA-ISDZ'.
            IF zaponta-isdd = sy-datum AND zaponta-isdz > sy-uzeit.
              SET CURSOR FIELD 'ZAPONTA-ISDZ'.
            ELSE.
              SET CURSOR FIELD 'ZAPONTA-IEDD'.
            ENDIF.
          WHEN 'ZAPONTA-IEDD'.
            IF zaponta-iedd > sy-datum.
              SET CURSOR FIELD 'ZAPONTA-IEDD'.
            ELSE.
              SET CURSOR FIELD 'ZAPONTA-IEDZ'.
            ENDIF.
          WHEN 'ZAPONTA-IEDZ'.
            IF soma_total > zaponta-einzh OR zaponta-isdd = sy-datum AND zaponta-isdz > sy-uzeit OR zaponta-iedd = sy-datum AND zaponta-iedz > sy-uzeit.
              SET CURSOR FIELD 'ZAPONTA-IEDZ'.
            ELSE.
*              SET CURSOR FIELD 'ZAPONTA-GRUND'.
*              IF soma_total IS NOT INITIAL.
*                zaponta-afrud = soma_total.
*              ENDIF.

              SET CURSOR FIELD 'V_CONFIRMA'. "Botão verde da tela de apontamento.
              PERFORM validar_dados.
              PERFORM limpar_campos. "Hora, Ação e Trab. Real

            ENDIF.
          WHEN 'ZAPONTA-GRUND'.
            SET CURSOR FIELD 'ZAPONTA-AFRUD'.
          WHEN 'ZAPONTA-AFRUD'.
            SET CURSOR FIELD 'ZAPONTA-DURATION_NORMAL_UNIT'.
*          WHEN 'ZAPONTA-IEDZ'.
*            SET CURSOR FIELD 'V_CONFIRMA'.
*            PERFORM validar_dados.
        ENDCASE.
      ENDIF.
    ELSEIF sy-ucomm EQ 'BACK'.
      LEAVE TO SCREEN 0.
    ELSEIF sy-ucomm EQ 'CLEAR'.
      LEAVE TO SCREEN 0.
    ELSE.
      CASE w_cursor_field.
*        WHEN 'ZAPONTA-ACTIVITY'.
**            SET CURSOR FIELD 'ZAPONTA-PERNR'.

*        WHEN 'ZAPONTA-PERNR'.
*          SET CURSOR FIELD 'ZAPONTA-ISDD'.
        WHEN 'ZAPONTA-PERNR'.
          SET CURSOR FIELD 'ZAPONTA-BUDAT'.
        WHEN   'ZAPONTA-BUDAT'.
          SET CURSOR FIELD 'ZAPONTA-MNCOD'.
        WHEN   'ZAPONTA-MNCOD'.
          SET CURSOR FIELD 'ZAPONTA-ISDD'.

        WHEN 'ZAPONTA-ISDD'.
          IF zaponta-isdd > sy-datum.
            SET CURSOR FIELD 'ZAPONTA-ISDD'.
          ELSE.
            SET CURSOR FIELD 'ZAPONTA-ISDZ'.
          ENDIF.
        WHEN 'ZAPONTA-ISDZ'.
          IF zaponta-isdd = sy-datum AND zaponta-isdz > sy-uzeit.
            SET CURSOR FIELD 'ZAPONTA-ISDZ'.
          ELSE.
            SET CURSOR FIELD 'ZAPONTA-IEDD'.
          ENDIF.
        WHEN 'ZAPONTA-IEDD'.
          IF zaponta-iedd > sy-datum.
            SET CURSOR FIELD 'ZAPONTA-IEDD'.
          ELSE.
            SET CURSOR FIELD 'ZAPONTA-IEDZ'.
          ENDIF.
        WHEN 'ZAPONTA-IEDZ'.
          IF soma_total > zaponta-einzh OR zaponta-isdd = sy-datum AND zaponta-isdz > sy-uzeit OR zaponta-iedd = sy-datum AND zaponta-iedz > sy-uzeit.
            SET CURSOR FIELD 'ZAPONTA-IEDZ'.
          ELSE.
*            SET CURSOR FIELD 'ZAPONTA-GRUND'.
*            IF soma_total IS NOT INITIAL.
*              zaponta-afrud = soma_total.
*            ENDIF.

            SET CURSOR FIELD 'V_CONFIRMA'. "Botão verde da tela de apontamento.
            PERFORM validar_dados.
            PERFORM limpar_campos. "Hora, Ação e Trab. Real

          ENDIF.
        WHEN 'ZAPONTA-GRUND'.
          SET CURSOR FIELD 'ZAPONTA-AFRUD'.
        WHEN 'ZAPONTA-AFRUD'.
          SET CURSOR FIELD 'ZAPONTA-DURATION_NORMAL_UNIT'.
        WHEN 'ZAPONTA-DURATION_NORMAL_UNIT'.
          SET CURSOR FIELD 'V_CONFIRMA'.
          PERFORM validar_dados.
      ENDCASE.
    ENDIF.
    IF zaponta-work_cntr IS NOT INITIAL.
      IF zaponta-ktext IS INITIAL.
        SET CURSOR FIELD 'ZAPONTA-WORK_CNTR'.
      ENDIF.
    ENDIF.

    IF zaponta-afrud IS NOT INITIAL.
      IF zaponta-afrud > soma_total.
        SET CURSOR FIELD 'ZAPONTA-AFRUD'.
      ENDIF.
    ENDIF.

    IF zaponta-isdd IS NOT INITIAL AND zaponta-iedd IS NOT INITIAL.
      IF zaponta-isdd > zaponta-iedd.
        SET CURSOR FIELD 'ZAPONTA-ISDD'.
      ENDIF.
    ENDIF.

    IF zaponta-isdd = zaponta-iedd AND zaponta-iedz < zaponta-isdz.
      SET CURSOR FIELD 'ZAPONTA-IEDZ'.
    ENDIF.

  ELSE.

    IF sy-ucomm = '' AND w_cursor_field = 'ZAPONTA-MNCOD'.
      SET CURSOR FIELD 'ZAPONTA-ISDD'.
    ELSE.
      SET CURSOR FIELD 'ZAPONTA-PERNR'.
    ENDIF.

  ENDIF.
ENDMODULE.

MODULE user_command_0001 INPUT.

  DATA: c_apontamento(11) VALUE 'APONTAMENTO'.
  CLEAR it_zpmt0012[].

  CASE sy-ucomm.
    WHEN 'BTN_CANCEL'.
      v_ucomm = sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_OK' OR 'ENTER'.
      IF ztpm_d_usuario-cpf_nr+3(1) <> '.'.
        WRITE ztpm_d_usuario-cpf_nr TO ztpm_d_usuario-cpf_nr USING EDIT MASK '___.___.___-__'.
      ENDIF.
      SELECT SINGLE *
        FROM ztpm_d_usuario
        INTO @DATA(w_ztpm_d_usuario)
        WHERE cpf_nr = @ztpm_d_usuario-cpf_nr AND
              login = @ztpm_d_usuario-login.
      IF sy-subrc <> 0.
        MESSAGE 'Usuário ou senha inválidos' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR ztpm_d_usuario-login.
*        v_ucomm = 'BTN_CANCEL'.
*        LEAVE TO SCREEN 0.
      ELSE.

        SELECT *
          FROM zpmt0012
          INTO TABLE it_zpmt0012
          WHERE pernr = w_ztpm_d_usuario-pernr.

**  Begin of    #96115  FF  07.03.2023
        READ TABLE it_zpmt0012 WITH KEY transacao = c_apontamento TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          MESSAGE 'Usuário sem permissão para fazer apontamento' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
** End of FF  07.03.2023

        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TC_APONTA'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc_aponta_mark INPUT.
  DATA: g_tc_aponta_wa2 LIKE LINE OF it_aponta.
  IF tc_aponta-line_sel_mode = 1
  AND zaponta-marc = 'X'.
    LOOP AT it_aponta INTO g_tc_aponta_wa2
      WHERE marc = 'X'.
      g_tc_aponta_wa2-marc = ''.
      MODIFY it_aponta
        FROM g_tc_aponta_wa2
        TRANSPORTING marc.
    ENDLOOP.
  ENDIF.
  MODIFY it_aponta
    FROM zaponta
    INDEX tc_aponta-current_line
    TRANSPORTING marc.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC_APONTA'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_aponta_user_command INPUT.
  enter = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_APONTA'
                              'IT_APONTA'
                              'MARC'
                     CHANGING enter.
  sy-ucomm = enter.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  HELP_FIELD_ORTGRP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_field_ortgrp INPUT.

*  PERFORM f4_val_ortgrp USING 'ZEPM_APONTA_CAT_NOTAS-OTGRP'
*                              'ZEPM_APONTA_CAT_NOTAS-OTEIL'
*                              'ZEPM_APONTA_CAT_NOTAS-TXTCDOT'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CODE_PF4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE code_pf4_oteil INPUT.

  SET CURSOR FIELD 'ZEPM_APONTA_CAT_NOTAS-OTEIL'.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENTER' "ENTER
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.


  DATA(lv_cat) = 'OTKAT'."Parte Objeto
  PERFORM code_pf4 USING lv_cat.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CODE_PF4_FEGRP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE code_pf4_fecod INPUT.

  SET CURSOR FIELD 'ZEPM_APONTA_CAT_NOTAS-FECOD'.

  CLEAR sy-ucomm. "Limpando para não chamar a tela de apontamento.
  lv_cat = 'FEKAT'."Sintoma Dano
  PERFORM code_pf4 USING lv_cat.

ENDMODULE.

MODULE code_pf4_urcod INPUT.

  SET CURSOR FIELD 'ZEPM_APONTA_CAT_NOTAS-URCOD'.

  CLEAR sy-ucomm. "Limpando para não chamar a tela de apontamento.
  lv_cat = 'URKAT'."Causa
  PERFORM code_pf4 USING lv_cat.

ENDMODULE.

MODULE code_pf4_mncod INPUT.

  SET CURSOR FIELD 'ZAPONTA-MNCOD'.

  CLEAR sy-ucomm. "Limpando para não chamar a tela de apontamento.
  lv_cat = 'MFKAT'."Códigos de ação
  PERFORM code_pf4 USING lv_cat.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LIMPAR_CAMPOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE limpar_campos INPUT.

*&-------------------------------AOENNING.
  IF sy-ucomm NE 'SAVE'.
    CALL METHOD g_editor->delete_text. "Limpa o texto longo.
  ENDIF.
*&-------------------------------AOENNING.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_DADOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atualiza_dados INPUT.

**  Begin of    #96115  FF  31.07.2023
  CHECK sy-ucomm <> 'SAVE'.

  DATA: number        TYPE  bapi_alm_order_header_e-orderid,
        lt_operations TYPE TABLE OF  bapi_alm_order_operation_e,
        lt_return	    TYPE TABLE OF  bapiret2.

  IF sy-ucomm <> 'BACK' AND
     sy-ucomm <> 'CLEAR'.

    PERFORM verifica_campos_vazios CHANGING v_tipo_ordem.
    IF vg_erro IS NOT INITIAL.
      sy-ucomm = 'SAVE'. "Apenas para forçar uma mensagem de erro

    ENDIF.
    CHECK vg_erro IS INITIAL.
  ENDIF.

  number = |{ v_ordem ALPHA = IN }|.

  CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
    EXPORTING
      number        = number
    TABLES
      et_operations = lt_operations
      return        = lt_return.

  DELETE lt_operations WHERE complete = 'X'.
  READ TABLE lt_operations INTO DATA(wa_op) WITH KEY activity = v_vornr.
  IF sy-subrc = 0.
    zaponta-description = v_op_descrip = wa_op-description.
  ENDIF.
**  End of    #96115  FF  31.07.2023

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CASE sy-ucomm.
    WHEN 'VOLTAR'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.
*-US 145467-22-07-2024-#145467-RJF-Início
*&---------------------------------------------------------------------*
*& Form validar_dados_avaria
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM validar_dados_avaria CHANGING p_nao_atualizar TYPE c
*                                   p_gravar_ini    TYPE c
*                                   p_gravar_fim    TYPE c
*                                   wa_viqmel       TYPE viqmel.
*
*  SELECT * "ausbs, ausvn " fim e ini
*    UP TO 1 ROWS
*  INTO @wa_viqmel
*  FROM viqmel
*  WHERE qmnum EQ @v_nota.
*  ENDSELECT.

*  IF sy-subrc IS INITIAL.
*    IF ( ( wa_viqmel-ausbs NE '' AND wa_viqmel-ausbs NE '00000000' ) " fim
*    AND ( wa_viqmel-ausvn NE '' AND wa_viqmel-ausvn NE '00000000' ) ).
*      MESSAGE i001(00) WITH 'Períodos avaria' ' possui valores cadastrados na nota!'.
*      p_nao_atualizar = abap_true.
*
*    ENDIF.
*
*    CHECK p_nao_atualizar IS INITIAL.
*    IF ( wa_viqmel-ausbs NE '' AND wa_viqmel-ausbs NE '00000000' ).
*      MESSAGE i001(00) WITH 'Período avaria final' ' possui valor cadastrado na nota!'.
*      IF wa_viqmel-ausvn EQ '' OR wa_viqmel-ausvn EQ '00000000'.
*        p_gravar_ini = abap_true.
*      ENDIF.
*    ENDIF.
*    CHECK p_gravar_fim IS INITIAL.
*    IF ( wa_viqmel-ausvn NE '' AND wa_viqmel-ausvn NE '00000000' ).
*      MESSAGE i001(00) WITH 'Período inicial' ' possui valor cadastrado na nota!'.
*      IF wa_viqmel-ausbs EQ '' OR wa_viqmel-ausbs EQ '00000000'.
*        p_gravar_fim = abap_true.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form shdb_modif_notif
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> V_NOTA
*&---------------------------------------------------------------------*
FORM shdb_modif_notif  USING p_nota.

  CLEAR: wa_bdcdata, ti_bdcdata, it_msg.
  FREE: ti_bdcdata, ti_bdcdata[], it_msg[].

  PERFORM f_bdcc_data USING:

'SAPLIQS0'  '100'   'X' ''  '',
''  ''  ' ' 'BDC_CURSOR'    'RIWO00-QMNUM',
''  ''  ' ' 'BDC_OKCODE'    '/00',
''  ''  ' ' 'RIWO00-QMNUM'  p_nota,
'SAPLIQS0'  '7200'  'X' ''  '',
''  ''  ' ' 'BDC_OKCODE'    '/00',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                1050SCREEN_1',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                1051NOTIF_TYPE',
*''  ''  ' ' 'VIQMEL-QMTXT'  'Ordem de remonta',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLQM07                                3000ACTION-BOX',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7235SUB_GROUP_10',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7212CUSTOM_SCREEN',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7322SUBSCREEN_1',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIWO1                                0100OBJEKT',
*''  ''  ' ' 'BDC_CURSOR'    'RIWO1-TPLNR',
*''  ''  ' ' 'RIWO1-TPLNR'   '',
*''  ''  ' ' 'RIWO1-EQUNR'   '901812',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7710SUBSCREEN_2',
*''  ''  ' ' 'VIQMEL-QMGRP'  'F0000010',
*''  ''  ' ' 'VIQMEL-QMCOD'  '20',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7326SUBSCREEN_3',
*''  ''  ' ' 'VIQMEL-INGRP'  'ABS',
*''  ''  ' ' 'VIQMEL-IWERK'  '189',
*''  ''  ' ' 'RIWO00-GEWRK'  'OFICINA',
*''  ''  ' ' 'RIWO00-SWERK'  '189',
*''  ''  ' ' 'VIQMEL-QMNAM'  'FFONSECA',
*''  ''  ' ' 'VIQMEL-QMDAT'  '11.04.2024',
*''  ''  ' ' 'VIQMEL-MZEIT'  '0,5341087962  96296',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIPAR                                0400INTPAR',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIPAR                                0450VERA',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7328SUBSCREEN_4',
''  ''  ' ' 'BDC_CURSOR'    'VIQMEL-AUZTB',
*''  ''  ' ' 'VIQMEL-AUSVN'  '11.04.2024',
*''  ''  ' ' 'VIQMEL-AUZTV'  '0,534166666666667',
*''  ''  ' ' 'VIQMEL-AUZTB'  '0',
''  ''  ' ' 'VIQMEL-MSAUS'  ' ',
*''  ''  ' ' 'VIQMEL-MAUEH'  'H',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7324SUBSCREEN_5',
'SAPLIQS0'  '7200'  'X' ''  '',
''  ''  ' ' 'BDC_OKCODE'    '/00',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                1050SCREEN_1',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                1051NOTIF_TYPE',
*''  ''  ' ' 'VIQMEL-QMTXT'  'Ordem de remonta',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLQM07                                3000ACTION-BOX',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7235SUB_GROUP_10',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7212CUSTOM_SCREEN',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7322SUBSCREEN_1',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIWO1                                0100OBJEKT',
''  ''  ' ' 'BDC_CURSOR'    'RIWO1-TPLNR',
''  ''  ' ' 'RIWO1-TPLNR'   '',
*''  ''  ' ' 'RIWO1-EQUNR'   '901812',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7710SUBSCREEN_2',
*''  ''  ' ' 'VIQMEL-QMGRP'  'F0000010',
*''  ''  ' ' 'VIQMEL-QMCOD'  '20',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7326SUBSCREEN_3',
*''  ''  ' ' 'VIQMEL-INGRP'  'ABS',
*''  ''  ' ' 'VIQMEL-IWERK'  '189',
*''  ''  ' ' 'RIWO00-GEWRK'  'OFICINA',
*''  ''  ' ' 'RIWO00-SWERK'  '189',
*''  ''  ' ' 'VIQMEL-QMNAM'  'FFONSECA',
*''  ''  ' ' 'VIQMEL-QMDAT'  '11.04.2024',
*''  ''  ' ' 'VIQMEL-MZEIT'  '0,534108796296296',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIPAR                                0400INTPAR',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIPAR                                0450VERA',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7328SUBSCREEN_4',
''  ''  ' ' 'BDC_CURSOR'    'VIQMEL-AUZTB',
*''  ''  ' ' 'VIQMEL-AUSVN'  '11.04.2024',
*''  ''  ' ' 'VIQMEL-AUZTV'  '0,534166666666667',
*''  ''  ' ' 'VIQMEL-AUZTB'  '0',
''  ''  ' ' 'VIQMEL-MSAUS'  'X',
*''  ''  ' ' 'VIQMEL-MAUEH'  'H',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7324SUBSCREEN_5',
'SAPLIQS0'  '7200'  'X' ''  '',
''  ''  ' ' 'BDC_OKCODE'    '=BUCH',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                1050SCREEN_1',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                1051NOTIF_TYPE',
*''  ''  ' ' 'BDC_CURSOR'    'VIQMEL-QMTXT',
*''  ''  ' ' 'VIQMEL-QMTXT'  'Ordem de remonta',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLQM07                                3000ACTION-BOX',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7235SUB_GROUP_10',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7212CUSTOM_SCREEN',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7322SUBSCREEN_1',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIWO1                                0100OBJEKT',
*''  ''  ' ' 'RIWO1-TPLNR'   'MAGI.001.0189.FROT.FR005',
*''  ''  ' ' 'RIWO1-EQUNR'   '901812',
*''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7710SUBSCREEN_2',
*''  ''  ' ' 'VIQMEL-QMGRP'  'F0000010',
*''  ''  ' ' 'VIQMEL-QMCOD'  '20',
*''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7326SUBSCREEN_3',
*''  ''  ' ' 'VIQMEL-INGRP'  'ABS',
*''  ''  ' ' 'VIQMEL-IWERK'  '189',
*''  ''  ' ' 'RIWO00-GEWRK'  'OFICINA',
*''  ''  ' ' 'RIWO00-SWERK'  '189',
*''  ''  ' ' 'VIQMEL-QMNAM'  'FFONSECA',
*''  ''  ' ' 'VIQMEL-QMDAT'  '11.04.2024',
*''  ''  ' ' 'VIQMEL-MZEIT'  '0,534108796296296',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIPAR                                0400INTPAR',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIPAR                                0450VERA',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7328SUBSCREEN_4',
''  ''  ' ' 'BDC_CURSOR'    'VIQMEL-AUZTB',
*''  ''  ' ' 'VIQMEL-AUSVN'  '11.04.2024',
*''  ''  ' ' 'VIQMEL-AUZTV'  '0,534166666666667',
''  ''  ' ' 'VIQMEL-MSAUS'  'X',
*''  ''  ' ' 'VIQMEL-AUZTB'  '0',
*''  ''  ' ' 'VIQMEL-MAUEH'  'H',
''  ''  ' ' 'BDC_SUBSCR'    'SAPLIQS0                                7324SUBSCREEN_5'.


  CLEAR p_erro.
  PERFORM f_call_transaction USING 'IW22' CHANGING p_erro.

ENDFORM.
"*-US 145467-22-07-2024-#145467-RJF-fim
*&---------------------------------------------------------------------*
*& Form f_call_transaction
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- P_ERRO
*&---------------------------------------------------------------------*
FORM f_call_transaction  USING p_trans CHANGING p_erro.

  DATA:
    wl_mode(1),
    wa_opcoes  TYPE ctu_params,
    ls_msg     TYPE string,
    ls_messtab LIKE LINE OF it_msg.

  CLEAR wl_mode.
  CLEAR it_msg.
  wl_mode = 'E'.

  CLEAR wa_opcoes.
  wa_opcoes-dismode  = 'N'.
  wa_opcoes-updmode  = 'A'.
  wa_opcoes-cattmode = ' '.
  wa_opcoes-racommit = ' '.
  wa_opcoes-nobinpt  = ' '.
  wa_opcoes-nobiend  = 'X'.

  CALL TRANSACTION p_trans USING ti_bdcdata
*                           MODE wl_mode
                   OPTIONS FROM wa_opcoes
                           MESSAGES INTO it_msg.

  IF line_exists( it_msg[ msgtyp = 'A' ] ).
    p_erro = abap_true.
  ELSE.
    IF line_exists( it_msg[ msgtyp = 'E' ] ).
      p_erro = abap_true.
    ENDIF.
  ENDIF.

  IF p_erro IS NOT INITIAL.

    READ TABLE it_msg INTO ls_messtab INDEX 1.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = ls_messtab-msgid          "Messg class
        msgnr               = ls_messtab-msgnr          "Messg No.
        msgv1               = ls_messtab-msgv1
        msgv2               = ls_messtab-msgv2
        msgv3               = ls_messtab-msgv3
        msgv4               = ls_messtab-msgv4
      IMPORTING
        message_text_output = ls_msg.

    IF ls_msg IS NOT INITIAL.
      MESSAGE ls_msg TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WHEN  text
*      -->P_OTHERS  text
*----------------------------------------------------------------------*
FORM f_bdcc_data  USING p_program p_dynpro p_start p_fnam p_fval.

  APPEND VALUE #(
                program   = p_program
                dynpro    = p_dynpro
                dynbegin  = p_start
                fnam      = p_fnam
                fval      = p_fval
  ) TO ti_bdcdata.
ENDFORM.
