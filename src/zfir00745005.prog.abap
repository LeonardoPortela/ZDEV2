*&---------------------------------------------------------------------*
*&  Include           ZFIR00745005
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_5005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5005 OUTPUT.

  SET PF-STATUS 'DIALOG'.
  SET TITLEBAR '5005'.

  TYPE-POOLS: vrm.

  DATA: lista5005 TYPE vrm_values,
        valor5005 LIKE LINE OF lista5005,
        linha_03  TYPE i.

  "Criando opções na listbox WA_TRANS-TRANSF_APROV
  valor5005-key = 'D'.
  valor5005-text = 'Definitiva'.
  APPEND valor5005 TO lista5005.

  valor5005-key = 'P'.
  valor5005-text = 'Provisória'.
  APPEND valor5005 TO lista5005.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'WA_TRANS-TRANSF_APROV'
      values = lista5005.

  CLEAR: valor5005, lista5005.

  "Criando opções na listbox WA_TRANS-ESTRATEGIA
  valor5005-key = '1'.
  valor5005-text = 'ZIMP - Impostos'.
  APPEND valor5005 TO lista5005.

  valor5005-key = '2'.
  valor5005-text = 'ZG - Lançamentos Manuais'.
  APPEND valor5005 TO lista5005.

  valor5005-key = '3'.
  valor5005-text = 'Adiantamentos'.
  APPEND valor5005 TO lista5005.

  valor5005-key = '4'.
  valor5005-text = 'Pagamentos Internacionais'.
  APPEND valor5005 TO lista5005.

  valor5005-key = '5'.
  valor5005-text = 'SD - O.V Lançamentos'.
  APPEND valor5005 TO lista5005.

  valor5005-key = '6'.
  valor5005-text = 'LES - Preço Frete'.
  APPEND valor5005 TO lista5005.

  valor5005-key = '7'.
  valor5005-text = 'SD - Solicição OV'.
  APPEND valor5005 TO lista5005.

  valor5005-key = '8'.
  valor5005-text = 'Todas'.
  APPEND valor5005 TO lista5005.

  valor5005-key = '9'.
  valor5005-text = 'LIM - Limite de Crédito'.
  APPEND valor5005 TO lista5005.

  valor5005-key = '10'.
  valor5005-text = 'MM - Variação Câmbio'.
  APPEND valor5005 TO lista5005.

  valor5005-key = '11'.
  valor5005-text = 'Isenção de Juros'.
  APPEND valor5005 TO lista5005.

  valor5005-key = '12'."150184 CS2024000781 Aprovações ZNFW - PSA
  valor5005-text = 'Operações ZNFW'.
  APPEND valor5005 TO lista5005.

  " 08.05.2025 - 174338 - RAMON -->
  valor5005-key = '13'.
  valor5005-text = 'Questi.Jurídico - Insumos'.
  APPEND valor5005 TO lista5005.
  " 08.05.2025 - 174338 - RAMON --<

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'WA_TRANS-ESTRATEGIA'
      values = lista5005.

  CLEAR: lista5005, valor5005.

  DESCRIBE TABLE it_selected_rows LINES linha_03.

  IF linha_03 < 1.
    MESSAGE 'Selecione uma linha.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0.
  ELSEIF linha_03 > 1.
    MESSAGE 'Selecione apenas uma linha.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0.
  ELSE.
    READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
    IF i_main_tab-pressed_tab = c_main_tab-tab1.
      READ TABLE it_saida_zimp INTO wa_saida_zimp INDEX wa_selected_rows-index.
      wa_trans-aprovador_de = wa_saida_zimp-aprovador.
      CLEAR: wa_saida_zimp.
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab2.
      READ TABLE it_saida_zglt INTO wa_saida_zglt INDEX wa_selected_rows-index.
      wa_trans-aprovador_de = wa_saida_zglt-aprovador.
      CLEAR: wa_saida_zglt.
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab3.
      READ TABLE it_saida_zinv INTO wa_saida_zinv INDEX wa_selected_rows-index.
      wa_trans-aprovador_de = wa_saida_zinv-aprovador.
      CLEAR: wa_saida_zinv.
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab4.
      READ TABLE it_saida_zadto INTO wa_saida_zadto INDEX wa_selected_rows-index.
      wa_trans-aprovador_de = wa_saida_zadto-aprovador.
      CLEAR: wa_saida_zadto.
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab5.
      READ TABLE it_saida_zov INTO wa_saida_zov INDEX wa_selected_rows-index.
      wa_trans-aprovador_de = wa_saida_zov-aprovador.
      CLEAR: wa_saida_zov.
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab6.
      READ TABLE it_saida_lim INTO wa_saida_lim INDEX wa_selected_rows-index.
      wa_trans-aprovador_de = wa_saida_lim-aprovador.
      CLEAR: wa_saida_lim.
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab7.
      READ TABLE it_saida_zfre INTO wa_saida_zfre INDEX wa_selected_rows-index.
      wa_trans-aprovador_de = wa_saida_zfre-aprovador.
      CLEAR: wa_saida_zfre.
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab8.
      READ TABLE it_saida_zsolov INTO wa_saida_zsolov INDEX wa_selected_rows-index.
      wa_trans-aprovador_de = wa_saida_zsolov-aprovador.
      CLEAR: wa_saida_zsolov.
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab9.
      READ TABLE it_saida_var_camb INTO wa_saida_var_camb INDEX wa_selected_rows-index.
      wa_trans-aprovador_de = wa_saida_var_camb-aprovador.
      CLEAR: wa_saida_zsolov.
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab11.
      READ TABLE it_saida_isencao INTO wa_saida_isencao INDEX wa_selected_rows-index.
      wa_trans-aprovador_de = wa_saida_isencao-aprovador.
      CLEAR: wa_saida_isencao.
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab12.
      READ TABLE it_saida_operznfw INTO wa_saida_operznfw INDEX wa_selected_rows-index.
      wa_trans-aprovador_de = wa_saida_operznfw-aprovador.
      CLEAR: wa_saida_operznfw.

      " 08.05.2025 - 174338 - RAMON -->
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab13.
      READ TABLE it_saida_checklist INTO wa_saida_checklist INDEX wa_selected_rows-index.
      wa_trans-aprovador_de = wa_saida_checklist-aprovador.
      CLEAR: wa_saida_checklist.
      " 08.05.2025 - 174338 - RAMON --<

    ENDIF.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5005 INPUT.

  CASE sy-ucomm.
    WHEN 'TRA'.
      CASE wa_trans-transf_aprov.
        WHEN 'D'.
          CLEAR wa_trans-dt_val_para.
          wa_trans-dt_val_para = '99991231'.
        WHEN 'P'.
          CLEAR wa_trans-dt_val_para.
      ENDCASE.
    WHEN OTHERS.
      "DO NOTHING
  ENDCASE.

  IF sy-ucomm = 'SAVE'.

    IF wa_trans-aprovador_de      IS INITIAL
      OR wa_trans-aprovador_para  IS INITIAL
      OR wa_trans-transf_aprov    IS INITIAL
      OR wa_trans-dt_val_de       IS INITIAL
      OR wa_trans-dt_val_para     IS INITIAL
      OR wa_trans-motivo          IS INITIAL
      OR wa_trans-estrategia      IS INITIAL.
      MESSAGE i000(z01) WITH 'Todos os campos são de preenchimento obrigatório.'.
      LEAVE TO SCREEN 5005.
    ENDIF.

    IF wa_trans-dt_val_de < sy-datum
       OR wa_trans-dt_val_para < sy-datum.
      MESSAGE i000(z01) WITH 'Não é possível salvar uma Estratégia com' 'data retroativa.'.
      LEAVE TO SCREEN 5005.
    ENDIF.

    vl_uzeit = sy-uzeit.

*---> CS1005354 /  IR101904
    EXPORT wa_trans-estrategia TO MEMORY ID 'waestrat'.
*<--- CS1005354 /  IR101904

    CASE wa_trans-estrategia.
      WHEN '1'.
        PERFORM check_trans.
        PERFORM trans_zimp.
        PERFORM preenche_data.
      WHEN '2'.
        PERFORM check_trans.
        PERFORM trans_zglt.
        PERFORM preenche_data.
      WHEN '3'.
        PERFORM check_trans.
        PERFORM trans_zadto.
        PERFORM preenche_data.
      WHEN '4'.
        PERFORM check_trans.
        PERFORM trans_zinv.
        PERFORM preenche_data.
      WHEN '5'.
        PERFORM check_trans.
        PERFORM trans_zov.
        PERFORM preenche_data.
      WHEN '6'.
        PERFORM check_trans.
        PERFORM trans_zfre.
        PERFORM preenche_data.
      WHEN '7'.
        PERFORM check_trans.
        PERFORM trans_zsolov.
        PERFORM preenche_data.
      WHEN '8'.                 "Todos
        PERFORM check_trans.
        PERFORM trans_zimp.     "1
        PERFORM trans_zglt.     "2
        PERFORM trans_zadto.    "3
        PERFORM trans_zinv.     "4
        PERFORM trans_zov.      "5
        PERFORM trans_zfre.     "6
        PERFORM trans_zsolov.   "7
        PERFORM trans_lim.      "9
        PERFORM trans_varcambio.
        PERFORM trans_isencao.
        PERFORM preenche_data.
      WHEN '9'. "limite credito
        PERFORM check_trans.
        PERFORM trans_lim.
        PERFORM preenche_data.
      WHEN '10'.
        PERFORM check_trans.
        PERFORM trans_varcambio.
        PERFORM preenche_data.
      WHEN '11'.
        PERFORM check_trans.
        PERFORM trans_isencao.
        PERFORM preenche_data.
      WHEN '12'."150184 CS2024000781 Aprovações ZNFW - PSA
        PERFORM check_trans.
        PERFORM trans_operznfw.
        PERFORM preenche_data.

        " 08.05.2025 - 174338 - RAMON -->
      WHEN '13'.
        PERFORM check_trans.
        PERFORM trans_checklist.
        PERFORM preenche_data.
        " 08.05.2025 - 174338 - RAMON --<


      WHEN OTHERS.
        "DO NOTHING
    ENDCASE.

    CLEAR: wa_trans, vl_uzeit.
    LEAVE TO SCREEN 0.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5005_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5005_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  TRANS_ZIMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM trans_zimp .

  DATA: it_saida_zimp_trans TYPE STANDARD TABLE OF t_zimp_aprovador,
        cont_loop           TYPE i.

  LOOP AT it_saida_zimp ASSIGNING <wa_saida_zimp>.
    cont_loop = sy-tabix.
    IF <wa_saida_zimp>-aprovador = wa_trans-aprovador_de
      AND wa_trans-dt_val_de >= <wa_saida_zimp>-dt_val_de
      AND wa_trans-dt_val_para <= <wa_saida_zimp>-dt_val_ate.
      "Não passa looping nas estratégias com início e fim no mesmo dia da transferência e no passado
      IF wa_trans-dt_val_de EQ wa_trans-dt_val_para
        AND <wa_saida_zimp>-dt_val_ate EQ <wa_saida_zimp>-dt_val_de
        AND wa_trans-dt_val_de  EQ <wa_saida_zimp>-dt_val_de.
        IF <wa_saida_zimp>-hr_val_ate < sy-uzeit
          AND <wa_saida_zimp>-hr_val_de < sy-uzeit.
          CONTINUE.
        ENDIF.
      ENDIF.
      "Não passa nas estratégias terminadas mas ainda na tela
      IF <wa_saida_zimp>-dt_val_ate EQ sy-datum AND <wa_saida_zimp>-hr_val_ate LT sy-uzeit.
        CONTINUE.
      ENDIF.
      "criando registro do novo aprovador
      wa_saida_zimp-bukrs         = <wa_saida_zimp>-bukrs.
      wa_saida_zimp-bukrs_ate     = <wa_saida_zimp>-bukrs_ate.
      wa_saida_zimp-dep_resp      = <wa_saida_zimp>-dep_resp.
      wa_saida_zimp-dep_resp_desc = <wa_saida_zimp>-dep_resp_desc.
      wa_saida_zimp-waers         = <wa_saida_zimp>-waers.
      wa_saida_zimp-nivel         = <wa_saida_zimp>-nivel.
      wa_saida_zimp-aprovador     = wa_trans-aprovador_para.
      wa_saida_zimp-valor_de      = <wa_saida_zimp>-valor_de.
      wa_saida_zimp-valor_ate     = <wa_saida_zimp>-valor_ate.
      "WA_SAIDA_ZIMP-DT_VAL_DE = WA_TRANS-DT_VAL_DE.
      IF vl_uzeit EQ '235959'.
        wa_saida_zimp-dt_val_de     = wa_trans-dt_val_de + 1.
      ELSE.
        wa_saida_zimp-dt_val_de     = wa_trans-dt_val_de.
      ENDIF.
      IF wa_saida_zimp-dt_val_de EQ sy-datum.
        wa_saida_zimp-hr_val_de     = vl_uzeit + '000001'.
      ELSE.
        wa_saida_zimp-hr_val_de     = '000000'.
      ENDIF.
      wa_saida_zimp-dt_val_ate    = wa_trans-dt_val_para.
      wa_saida_zimp-hr_val_ate    = '235959'.
      wa_saida_zimp-motivo        = wa_trans-motivo.
      wa_saida_zimp-transf_aprov  = wa_trans-transf_aprov.
      wa_saida_zimp-ck_ant        = abap_true.
      APPEND wa_saida_zimp TO it_saida_zimp_trans.
      CLEAR wa_saida_zimp.
      "cria registro para o periodo anterior do aprovador de se a transferencia for provisória
      IF wa_trans-transf_aprov = 'P'.
        wa_saida_zimp-bukrs         = <wa_saida_zimp>-bukrs.
        wa_saida_zimp-bukrs_ate     = <wa_saida_zimp>-bukrs_ate.
        wa_saida_zimp-dep_resp      = <wa_saida_zimp>-dep_resp.
        wa_saida_zimp-dep_resp_desc = <wa_saida_zimp>-dep_resp_desc.
        wa_saida_zimp-waers         = <wa_saida_zimp>-waers.
        wa_saida_zimp-nivel         = <wa_saida_zimp>-nivel.
        wa_saida_zimp-aprovador     = <wa_saida_zimp>-aprovador.
        wa_saida_zimp-valor_de      = <wa_saida_zimp>-valor_de.
        wa_saida_zimp-valor_ate     = <wa_saida_zimp>-valor_ate.
        wa_saida_zimp-dt_val_de     = <wa_saida_zimp>-dt_val_de.
        wa_saida_zimp-hr_val_de     = <wa_saida_zimp>-hr_val_de.
        IF wa_trans-dt_val_de EQ sy-datum.
          wa_saida_zimp-dt_val_ate    = wa_trans-dt_val_de.
          wa_saida_zimp-hr_val_ate    = vl_uzeit.
        ELSE.
          wa_saida_zimp-hr_val_ate    = '235959'.
          wa_saida_zimp-dt_val_ate    = wa_trans-dt_val_de - 1.
        ENDIF.
        "WA_SAIDA_ZIMP-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
        wa_saida_zimp-motivo        = <wa_saida_zimp>-motivo.
        wa_saida_zimp-transf_aprov  = <wa_saida_zimp>-transf_aprov.
        wa_saida_zimp-ck_ant = 'X'.
        APPEND wa_saida_zimp TO it_saida_zimp_trans.
        CLEAR wa_saida_zimp.
      ENDIF.
      "modificando o registro do aprovador antigo para o periodo futuro
      IF wa_trans-transf_aprov = 'P'.
        IF <wa_saida_zimp>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zimp>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zimp>-hr_val_ate = '235959'.
          <wa_saida_zimp>-dt_val_de = wa_trans-dt_val_para.
          <wa_saida_zimp>-hr_val_de = '235959'.
*          <WA_SAIDA_ZIMP>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*          <WA_SAIDA_ZIMP>-CK_ANT = 'X'.
        ELSE.
          <wa_saida_zimp>-dt_val_de = wa_trans-dt_val_para + 1.
          <wa_saida_zimp>-hr_val_de = '000000'.
        ENDIF.
      ELSE.
        IF wa_trans-dt_val_de EQ sy-datum.
          <wa_saida_zimp>-dt_val_ate = wa_trans-dt_val_de.
          <wa_saida_zimp>-hr_val_ate = vl_uzeit.
        ELSE.
          <wa_saida_zimp>-dt_val_ate = wa_trans-dt_val_de - 1.
          <wa_saida_zimp>-hr_val_ate = '235959'.
        ENDIF.
*        <WA_SAIDA_ZIMP>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*        <WA_SAIDA_ZIMP>-CK_ANT = 'X'.
      ENDIF.
      "<WA_SAIDA_ZIMP>-TRANSF_APROV = WA_TRANS-TRANSF_APROV.
      "<WA_SAIDA_ZIMP>-MOTIVO = WA_TRANS-MOTIVO.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida_zimp_trans INTO wa_saida_zimp.
    IF wa_trans-motivo IS NOT INITIAL.
      wa_saida_zimp-motivo = wa_trans-motivo.
    ENDIF.

    APPEND wa_saida_zimp TO it_saida_zimp.
    CLEAR wa_saida_zimp.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRANS_ZGLT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM trans_zglt .

  DATA: it_saida_zglt_trans TYPE STANDARD TABLE OF t_zglt037,
        cont_loop           TYPE i.

  LOOP AT it_saida_zglt ASSIGNING <wa_saida_zglt>.
    cont_loop = sy-tabix.
    IF <wa_saida_zglt>-aprovador = wa_trans-aprovador_de
      AND wa_trans-dt_val_de >= <wa_saida_zglt>-dt_val_de
      AND wa_trans-dt_val_para <= <wa_saida_zglt>-dt_val_ate.
      "Não passa looping nas estratégias com início e fim no mesmo dia da transferência e no passado
      IF wa_trans-dt_val_de EQ wa_trans-dt_val_para
        AND <wa_saida_zglt>-dt_val_ate EQ <wa_saida_zglt>-dt_val_de
        AND wa_trans-dt_val_de  EQ <wa_saida_zglt>-dt_val_de.
        IF <wa_saida_zglt>-hr_val_ate < sy-uzeit
          AND <wa_saida_zglt>-hr_val_de < sy-uzeit.
          CONTINUE.
        ENDIF.
      ENDIF.
      "Não passa nas estratégias terminadas mas ainda na tela
      IF <wa_saida_zglt>-dt_val_ate EQ sy-datum AND <wa_saida_zglt>-hr_val_ate LT sy-uzeit.
        CONTINUE.
      ENDIF.
      "criando registro do novo aprovador
      wa_saida_zglt-bukrs         = <wa_saida_zglt>-bukrs.
      wa_saida_zglt-bukrs_ate     = <wa_saida_zglt>-bukrs_ate.
      wa_saida_zglt-dep_resp      = <wa_saida_zglt>-dep_resp.
      wa_saida_zglt-dep_resp_desc = <wa_saida_zglt>-dep_resp_desc.
      wa_saida_zglt-pgt_forn      = <wa_saida_zglt>-pgt_forn.
      wa_saida_zglt-waers         = <wa_saida_zglt>-waers.
      wa_saida_zglt-nivel         = <wa_saida_zglt>-nivel.
      wa_saida_zglt-aprovador     = wa_trans-aprovador_para.
      wa_saida_zglt-valor_de      = <wa_saida_zglt>-valor_de.
      wa_saida_zglt-valor_ate     = <wa_saida_zglt>-valor_ate.
      IF vl_uzeit EQ '235959'.
        wa_saida_zglt-dt_val_de     = wa_trans-dt_val_de + 1.
      ELSE.
        wa_saida_zglt-dt_val_de     = wa_trans-dt_val_de.
      ENDIF.
      IF wa_saida_zglt-dt_val_de EQ sy-datum.
        wa_saida_zglt-hr_val_de     = vl_uzeit + '000001'.
      ELSE.
        wa_saida_zglt-hr_val_de     = '000000'.
      ENDIF.
      wa_saida_zglt-dt_val_ate    = wa_trans-dt_val_para.
      wa_saida_zglt-hr_val_ate    = '235959'.
      wa_saida_zglt-motivo        = wa_trans-motivo.
      wa_saida_zglt-transf_aprov  = wa_trans-transf_aprov.
      wa_saida_zglt-ck_ant = abap_true.
      APPEND wa_saida_zglt TO it_saida_zglt_trans.
      CLEAR wa_saida_zglt.
      "cria registro para o periodo anterior do aprovador de se a transferencia for provisória
      IF wa_trans-transf_aprov = 'P'.
        wa_saida_zglt-bukrs         = <wa_saida_zglt>-bukrs.
        wa_saida_zglt-bukrs_ate     = <wa_saida_zglt>-bukrs_ate.
        wa_saida_zglt-dep_resp      = <wa_saida_zglt>-dep_resp.
        wa_saida_zglt-dep_resp_desc = <wa_saida_zglt>-dep_resp_desc.
        wa_saida_zglt-pgt_forn      = <wa_saida_zglt>-pgt_forn.
        wa_saida_zglt-waers         = <wa_saida_zglt>-waers.
        wa_saida_zglt-nivel         = <wa_saida_zglt>-nivel.
        wa_saida_zglt-aprovador     = <wa_saida_zglt>-aprovador.
        wa_saida_zglt-valor_de      = <wa_saida_zglt>-valor_de.
        wa_saida_zglt-valor_ate     = <wa_saida_zglt>-valor_ate.
        wa_saida_zglt-dt_val_de     = <wa_saida_zglt>-dt_val_de.
        wa_saida_zglt-hr_val_de     = <wa_saida_zglt>-hr_val_de.
        wa_saida_zglt-dt_val_ate    = wa_trans-dt_val_de - 1.
        IF wa_trans-dt_val_de EQ sy-datum.
          wa_saida_zglt-dt_val_ate    = wa_trans-dt_val_de.
          wa_saida_zglt-hr_val_ate    = vl_uzeit.
        ELSE.
          wa_saida_zglt-hr_val_ate    = '235959'.
          wa_saida_zglt-dt_val_ate    = wa_trans-dt_val_de - 1.
        ENDIF.
        wa_saida_zglt-motivo        = <wa_saida_zglt>-motivo.
        wa_saida_zglt-transf_aprov  = <wa_saida_zglt>-transf_aprov.
        wa_saida_zglt-ck_ant        = 'X'.
        APPEND wa_saida_zglt TO it_saida_zglt_trans.
        CLEAR wa_saida_zglt.
      ENDIF.
      "modificando o registro do aprovador antigo para o periodo futuro
      IF wa_trans-transf_aprov = 'P'.
        IF <wa_saida_zglt>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zglt>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zglt>-hr_val_ate = '235959'.
          <wa_saida_zglt>-dt_val_de = wa_trans-dt_val_para.
          <wa_saida_zglt>-hr_val_de = '235959'.
          "<WA_SAIDA_ZGLT>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
          "<WA_SAIDA_ZGLT>-CK_ANT = 'X'.
        ELSE.
          <wa_saida_zglt>-dt_val_de = wa_trans-dt_val_para + 1.
          <wa_saida_zglt>-hr_val_de = '000000'.
        ENDIF.
      ELSE.
        IF wa_trans-dt_val_de EQ sy-datum.
          <wa_saida_zglt>-dt_val_ate = wa_trans-dt_val_de.
          <wa_saida_zglt>-hr_val_ate = vl_uzeit.
        ELSE.
          <wa_saida_zglt>-dt_val_ate = wa_trans-dt_val_de - 1.
          <wa_saida_zglt>-hr_val_ate = '235959'.
        ENDIF.
*        <WA_SAIDA_ZGLT>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*        <WA_SAIDA_ZGLT>-CK_ANT = 'X'.
      ENDIF.
      "<WA_SAIDA_ZGLT>-TRANSF_APROV = WA_TRANS-TRANSF_APROV.
      "<WA_SAIDA_ZGLT>-MOTIVO = WA_TRANS-MOTIVO.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida_zglt_trans INTO wa_saida_zglt.
    IF wa_trans-motivo IS NOT INITIAL.
      wa_saida_zglt-motivo = wa_trans-motivo.
    ENDIF.

    APPEND wa_saida_zglt TO it_saida_zglt.
    CLEAR: wa_saida_zglt.
  ENDLOOP.

ENDFORM.

FORM trans_zov .

  DATA: it_saida_zov_trans TYPE STANDARD TABLE OF t_zsdt0141,
        cont_loop          TYPE i.

  LOOP AT it_saida_zov ASSIGNING <wa_saida_zov>.
    cont_loop = sy-tabix.
    IF <wa_saida_zov>-aprovador = wa_trans-aprovador_de
      AND wa_trans-dt_val_de >= <wa_saida_zov>-dt_val_de
      AND wa_trans-dt_val_para <= <wa_saida_zov>-dt_val_ate.
      "Não passa looping nas estratégias com início e fim no mesmo dia da transferência e no passado
      IF wa_trans-dt_val_de EQ wa_trans-dt_val_para
        AND <wa_saida_zov>-dt_val_ate EQ <wa_saida_zov>-dt_val_de
        AND wa_trans-dt_val_de  EQ <wa_saida_zov>-dt_val_de.
        IF <wa_saida_zov>-hr_val_ate < sy-uzeit
          AND <wa_saida_zov>-hr_val_de < sy-uzeit.
          CONTINUE.
        ENDIF.
      ENDIF.
      "Não passa nas estratégias terminadas mas ainda na tela
      IF <wa_saida_zov>-dt_val_ate EQ sy-datum AND <wa_saida_zov>-hr_val_ate LT sy-uzeit.
        CONTINUE.
      ENDIF.
      "criando registro do novo aprovador
      wa_saida_zov-bukrs         = <wa_saida_zov>-bukrs.
      wa_saida_zov-bukrs_ate     = <wa_saida_zov>-bukrs_ate.
      wa_saida_zov-vkbur         = <wa_saida_zov>-vkbur.
      wa_saida_zov-vkbur_ate     = <wa_saida_zov>-vkbur_ate.
      wa_saida_zov-waers         = <wa_saida_zov>-waers.
      wa_saida_zov-nivel         = <wa_saida_zov>-nivel.
      wa_saida_zov-aprovador     = wa_trans-aprovador_para.
      wa_saida_zov-valor_de      = <wa_saida_zov>-valor_de.
      wa_saida_zov-valor_ate     = <wa_saida_zov>-valor_ate.
      IF vl_uzeit EQ '235959'.
        wa_saida_zov-dt_val_de     = wa_trans-dt_val_de + 1.
      ELSE.
        wa_saida_zov-dt_val_de     = wa_trans-dt_val_de.
      ENDIF.
      IF wa_saida_zov-dt_val_de EQ sy-datum.
        wa_saida_zov-hr_val_de     = vl_uzeit + '000001'.
      ELSE.
        wa_saida_zov-hr_val_de     = '000000'.
      ENDIF.
      wa_saida_zov-dt_val_ate    = wa_trans-dt_val_para.
      wa_saida_zov-hr_val_ate    = '235959'.
      wa_saida_zov-motivo        = wa_trans-motivo.
      wa_saida_zov-transf_aprov  = wa_trans-transf_aprov.
      wa_saida_zov-ck_ant = abap_true.
      APPEND wa_saida_zov TO it_saida_zov_trans.
      CLEAR wa_saida_zov.
      "cria registro para o periodo anterior do aprovador de se a transferencia for provisória
      IF wa_trans-transf_aprov = 'P'.
        wa_saida_zov-bukrs         = <wa_saida_zov>-bukrs.
        wa_saida_zov-bukrs_ate     = <wa_saida_zov>-bukrs_ate.
        wa_saida_zov-vkbur         = <wa_saida_zov>-vkbur.
        wa_saida_zov-vkbur_ate     = <wa_saida_zov>-vkbur_ate.
        wa_saida_zov-waers         = <wa_saida_zov>-waers.
        wa_saida_zov-nivel         = <wa_saida_zov>-nivel.
        wa_saida_zov-aprovador     = <wa_saida_zov>-aprovador.
        wa_saida_zov-valor_de      = <wa_saida_zov>-valor_de.
        wa_saida_zov-valor_ate     = <wa_saida_zov>-valor_ate.
        wa_saida_zov-dt_val_de     = <wa_saida_zov>-dt_val_de.
        wa_saida_zov-hr_val_de     = <wa_saida_zov>-hr_val_de.
        wa_saida_zov-dt_val_ate    = wa_trans-dt_val_de - 1.
        IF wa_trans-dt_val_de EQ sy-datum.
          wa_saida_zov-dt_val_ate    = wa_trans-dt_val_de.
          wa_saida_zov-hr_val_ate    = vl_uzeit.
        ELSE.
          wa_saida_zov-hr_val_ate    = '235959'.
          wa_saida_zov-dt_val_ate    = wa_trans-dt_val_de - 1.
        ENDIF.
        wa_saida_zov-motivo        = <wa_saida_zov>-motivo.
        wa_saida_zov-transf_aprov  = <wa_saida_zov>-transf_aprov.
        wa_saida_zov-ck_ant        = 'X'.
        APPEND wa_saida_zov TO it_saida_zov_trans.
        CLEAR wa_saida_zov.
      ENDIF.
      "modificando o registro do aprovador antigo para o periodo futuro
      IF wa_trans-transf_aprov = 'P'.
        IF <wa_saida_zov>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zov>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zov>-hr_val_ate = '235959'.
          <wa_saida_zov>-dt_val_de = wa_trans-dt_val_para.
          <wa_saida_zov>-hr_val_de = '235959'.
          "<WA_SAIDA_ZOV>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
          "<WA_SAIDA_ZOV>-CK_ANT = 'X'.
        ELSE.
          <wa_saida_zov>-dt_val_de = wa_trans-dt_val_para + 1.
          <wa_saida_zov>-hr_val_de = '000000'.
        ENDIF.
      ELSE.
        IF wa_trans-dt_val_de EQ sy-datum.
          <wa_saida_zov>-dt_val_ate = wa_trans-dt_val_de.
          <wa_saida_zov>-hr_val_ate = vl_uzeit.
        ELSE.
          <wa_saida_zov>-dt_val_ate = wa_trans-dt_val_de - 1.
          <wa_saida_zov>-hr_val_ate = '235959'.
        ENDIF.
*        <WA_SAIDA_ZOV>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*        <WA_SAIDA_ZOV>-CK_ANT = 'X'.
      ENDIF.
      "<WA_SAIDA_ZOV>-TRANSF_APROV = WA_TRANS-TRANSF_APROV.
      "<WA_SAIDA_ZOV>-MOTIVO = WA_TRANS-MOTIVO.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida_zov_trans INTO wa_saida_zov.
    IF wa_trans-motivo IS NOT INITIAL.
      wa_saida_zov-motivo = wa_trans-motivo.
    ENDIF.

    APPEND wa_saida_zov TO it_saida_zov.
    CLEAR: wa_saida_zov.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRANS_ZADTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM trans_zadto .

  DATA: it_saida_zadto_trans TYPE STANDARD TABLE OF t_zadto_aprovador,
        cont_loop            TYPE i.

  LOOP AT it_saida_zadto ASSIGNING <wa_saida_zadto>.
    cont_loop = sy-tabix.
    IF <wa_saida_zadto>-aprovador = wa_trans-aprovador_de
      AND wa_trans-dt_val_de >= <wa_saida_zadto>-dt_val_de
      AND wa_trans-dt_val_para <= <wa_saida_zadto>-dt_val_ate.
      "Não passa looping nas estratégias com início e fim no mesmo dia da transferência e no passado
      IF wa_trans-dt_val_de EQ wa_trans-dt_val_para
        AND <wa_saida_zadto>-dt_val_ate EQ <wa_saida_zadto>-dt_val_de
        AND wa_trans-dt_val_de  EQ <wa_saida_zadto>-dt_val_de.
        IF <wa_saida_zadto>-hr_val_ate < sy-uzeit
          AND <wa_saida_zadto>-hr_val_de < sy-uzeit.
          CONTINUE.
        ENDIF.
      ENDIF.
      "Não passa nas estratégias terminadas mas ainda na tela
      IF <wa_saida_zadto>-dt_val_ate EQ sy-datum AND <wa_saida_zadto>-hr_val_ate LT sy-uzeit.
        CONTINUE.
      ENDIF.
      "criando registro do novo aprovador
      wa_saida_zadto-bukrs          = <wa_saida_zadto>-bukrs.
      wa_saida_zadto-bukrs_ate      = <wa_saida_zadto>-bukrs_ate.
      wa_saida_zadto-dep_resp       = <wa_saida_zadto>-dep_resp.
      wa_saida_zadto-dep_resp_desc  = <wa_saida_zadto>-dep_resp_desc.
      wa_saida_zadto-waers          = <wa_saida_zadto>-waers.
      wa_saida_zadto-nivel          = <wa_saida_zadto>-nivel.
      wa_saida_zadto-aprovador      = wa_trans-aprovador_para.
      wa_saida_zadto-valor_de       = <wa_saida_zadto>-valor_de.
      wa_saida_zadto-valor_ate      = <wa_saida_zadto>-valor_ate.
      IF vl_uzeit EQ '235959'.
        wa_saida_zadto-dt_val_de     = wa_trans-dt_val_de + 1.
      ELSE.
        wa_saida_zadto-dt_val_de     = wa_trans-dt_val_de.
      ENDIF.
      IF wa_saida_zadto-dt_val_de EQ sy-datum.
        wa_saida_zadto-hr_val_de     = vl_uzeit + '000001'.
      ELSE.
        wa_saida_zadto-hr_val_de     = '000000'.
      ENDIF.
      wa_saida_zadto-dt_val_ate     = wa_trans-dt_val_para.
      wa_saida_zadto-hr_val_ate    = '235959'.
      wa_saida_zadto-motivo         = wa_trans-motivo.
      wa_saida_zadto-transf_aprov   = wa_trans-transf_aprov.
      wa_saida_zadto-ck_ant        = 'X'.
      APPEND wa_saida_zadto TO it_saida_zadto_trans.
      CLEAR wa_saida_zadto.
      "cria registro para o periodo anterior do aprovador de se a transferencia for provisória
      IF wa_trans-transf_aprov = 'P'.
        wa_saida_zadto-bukrs          = <wa_saida_zadto>-bukrs.
        wa_saida_zadto-bukrs_ate      = <wa_saida_zadto>-bukrs_ate.
        wa_saida_zadto-dep_resp       = <wa_saida_zadto>-dep_resp.
        wa_saida_zadto-dep_resp_desc  = <wa_saida_zadto>-dep_resp_desc.
        wa_saida_zadto-waers          = <wa_saida_zadto>-waers.
        wa_saida_zadto-nivel          = <wa_saida_zadto>-nivel.
        wa_saida_zadto-aprovador      = <wa_saida_zadto>-aprovador.
        wa_saida_zadto-valor_de       = <wa_saida_zadto>-valor_de.
        wa_saida_zadto-valor_ate      = <wa_saida_zadto>-valor_ate.
        wa_saida_zadto-dt_val_de      = <wa_saida_zadto>-dt_val_de.
        wa_saida_zadto-hr_val_de      = <wa_saida_zadto>-hr_val_de.
        "WA_SAIDA_ZADTO-DT_VAL_ATE     = WA_TRANS-DT_VAL_DE - 1.
        IF wa_trans-dt_val_de EQ sy-datum.
          wa_saida_zadto-dt_val_ate    = wa_trans-dt_val_de.
          wa_saida_zadto-hr_val_ate    = vl_uzeit.
        ELSE.
          wa_saida_zadto-hr_val_ate    = '235959'.
          wa_saida_zadto-dt_val_ate    = wa_trans-dt_val_de - 1.
        ENDIF.
        wa_saida_zadto-motivo        = <wa_saida_zadto>-motivo.
        wa_saida_zadto-transf_aprov  = <wa_saida_zadto>-transf_aprov.
        wa_saida_zadto-ck_ant = 'X'.
        APPEND wa_saida_zadto TO it_saida_zadto_trans.
        CLEAR wa_saida_zadto.
      ENDIF.
      "modificando o registro do aprovador antigo para o periodo futuro
      IF wa_trans-transf_aprov = 'P'.
        IF <wa_saida_zadto>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zadto>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zadto>-hr_val_ate = '235959'.
          <wa_saida_zadto>-dt_val_de = wa_trans-dt_val_para.
          <wa_saida_zadto>-hr_val_de = '235959'.
*          <WA_SAIDA_ZADTO>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*          <WA_SAIDA_ZADTO>-CK_ANT = 'X'.
        ELSE.
          <wa_saida_zadto>-dt_val_de = wa_trans-dt_val_para + 1.
          <wa_saida_zadto>-hr_val_de = '000000'.
        ENDIF.
      ELSE.
        IF wa_trans-dt_val_de EQ sy-datum.
          <wa_saida_zadto>-dt_val_ate = wa_trans-dt_val_de.
          <wa_saida_zadto>-hr_val_ate = vl_uzeit.
        ELSE.
          <wa_saida_zadto>-dt_val_ate = wa_trans-dt_val_de - 1.
          <wa_saida_zadto>-hr_val_ate = '235959'.
        ENDIF.
*        <WA_SAIDA_ZADTO>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*        <WA_SAIDA_ZADTO>-CK_ANT = 'X'.
      ENDIF.
      "<WA_SAIDA_ZADTO>-TRANSF_APROV = WA_TRANS-TRANSF_APROV.
      "<WA_SAIDA_ZADTO>-MOTIVO = WA_TRANS-MOTIVO.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida_zadto_trans INTO wa_saida_zadto.
    IF wa_trans-motivo IS NOT INITIAL.
      wa_saida_zadto-motivo = wa_trans-motivo.
    ENDIF.

    APPEND wa_saida_zadto TO it_saida_zadto.
    CLEAR: wa_saida_zadto.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRANS_ZINV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM trans_zinv .

  DATA: it_saida_zinv_trans TYPE STANDARD TABLE OF t_zinv_aprovador,
        cont_loop           TYPE i.

  LOOP AT it_saida_zinv ASSIGNING <wa_saida_zinv>.
    cont_loop = sy-tabix.
    IF <wa_saida_zinv>-aprovador = wa_trans-aprovador_de
      AND wa_trans-dt_val_de >= <wa_saida_zinv>-dt_val_de
      AND wa_trans-dt_val_para <= <wa_saida_zinv>-dt_val_ate.
      "Não passa looping nas estratégias com início e fim no mesmo dia da transferência e no passado
      IF wa_trans-dt_val_de EQ wa_trans-dt_val_para
        AND <wa_saida_zinv>-dt_val_ate EQ <wa_saida_zinv>-dt_val_de
        AND wa_trans-dt_val_de  EQ <wa_saida_zinv>-dt_val_de.
        IF <wa_saida_zinv>-hr_val_ate < sy-uzeit
          AND <wa_saida_zinv>-hr_val_de < sy-uzeit.
          CONTINUE.
        ENDIF.
      ENDIF.
      "Não passa nas estratégias terminadas mas ainda na tela
      IF <wa_saida_zinv>-dt_val_ate EQ sy-datum AND <wa_saida_zinv>-hr_val_ate LT sy-uzeit.
        CONTINUE.
      ENDIF.
      "criando registro do novo aprovador
      wa_saida_zinv-bukrs = <wa_saida_zinv>-bukrs.
      wa_saida_zinv-bukrs_ate = <wa_saida_zinv>-bukrs_ate.
      wa_saida_zinv-tipo = <wa_saida_zinv>-tipo.
      wa_saida_zinv-tipo_desc = <wa_saida_zinv>-tipo_desc.
      wa_saida_zinv-waers = <wa_saida_zinv>-waers.
      wa_saida_zinv-tp_operacao = <wa_saida_zinv>-tp_operacao.
      wa_saida_zinv-ds_operacao = <wa_saida_zinv>-ds_operacao.
      wa_saida_zinv-matnr = <wa_saida_zinv>-matnr.
      wa_saida_zinv-maktx = <wa_saida_zinv>-maktx.
      wa_saida_zinv-nivel = <wa_saida_zinv>-nivel.
      wa_saida_zinv-aprovador = wa_trans-aprovador_para.
      wa_saida_zinv-valor_de = <wa_saida_zinv>-valor_de.
      wa_saida_zinv-valor_ate = <wa_saida_zinv>-valor_ate.
      IF vl_uzeit EQ '235959'.
        wa_saida_zinv-dt_val_de     = wa_trans-dt_val_de + 1.
      ELSE.
        wa_saida_zinv-dt_val_de     = wa_trans-dt_val_de.
      ENDIF.
      IF wa_saida_zinv-dt_val_de EQ sy-datum.
        wa_saida_zinv-hr_val_de     = vl_uzeit + '000001'.
      ELSE.
        wa_saida_zinv-hr_val_de     = '000000'.
      ENDIF.
      wa_saida_zinv-dt_val_ate    = wa_trans-dt_val_para.
      wa_saida_zinv-hr_val_ate    = '235959'.
      wa_saida_zinv-motivo        = wa_trans-motivo.
      wa_saida_zinv-transf_aprov  = wa_trans-transf_aprov.
      wa_saida_zinv-ck_ant        = abap_true.
      APPEND wa_saida_zinv TO it_saida_zinv_trans.
      CLEAR wa_saida_zinv.
      "cria registro para o periodo anterior do aprovador de se a transferencia for provisória
      IF wa_trans-transf_aprov = 'P'.
        wa_saida_zinv-bukrs           = <wa_saida_zinv>-bukrs.
        wa_saida_zinv-bukrs_ate       = <wa_saida_zinv>-bukrs_ate.
        wa_saida_zinv-tipo            = <wa_saida_zinv>-tipo.
        wa_saida_zinv-tipo_desc       = <wa_saida_zinv>-tipo_desc.
        wa_saida_zinv-waers           = <wa_saida_zinv>-waers.
        wa_saida_zinv-tp_operacao     = <wa_saida_zinv>-tp_operacao.
        wa_saida_zinv-ds_operacao     = <wa_saida_zinv>-ds_operacao.
        wa_saida_zinv-matnr           = <wa_saida_zinv>-matnr.
        wa_saida_zinv-maktx           = <wa_saida_zinv>-maktx.
        wa_saida_zinv-nivel           = <wa_saida_zinv>-nivel.
        wa_saida_zinv-aprovador       = <wa_saida_zinv>-aprovador.
        wa_saida_zinv-valor_de        = <wa_saida_zinv>-valor_de.
        wa_saida_zinv-valor_ate       = <wa_saida_zinv>-valor_ate.
        wa_saida_zinv-dt_val_de       = <wa_saida_zinv>-dt_val_de.
        wa_saida_zinv-hr_val_de       = <wa_saida_zinv>-hr_val_de.
*        WA_SAIDA_ZINV-DT_VAL_ATE      = WA_TRANS-DT_VAL_DE - 1.
        IF wa_trans-dt_val_de EQ sy-datum.
          wa_saida_zinv-dt_val_ate    = wa_trans-dt_val_de.
          wa_saida_zinv-hr_val_ate    = vl_uzeit.
        ELSE.
          wa_saida_zinv-hr_val_ate    = '235959'.
          wa_saida_zinv-dt_val_ate    = wa_trans-dt_val_de - 1.
        ENDIF.
        wa_saida_zinv-motivo        = <wa_saida_zinv>-motivo.
        wa_saida_zinv-transf_aprov  = <wa_saida_zinv>-transf_aprov.
        wa_saida_zinv-ck_ant        = 'X'.
        APPEND wa_saida_zinv TO it_saida_zinv_trans.
        CLEAR wa_saida_zinv.
      ENDIF.
      "modificando o registro do aprovador antigo para o periodo futuro
      IF wa_trans-transf_aprov = 'P'.
        IF <wa_saida_zinv>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zinv>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zinv>-hr_val_ate = '235959'.
          <wa_saida_zinv>-dt_val_de = wa_trans-dt_val_para.
          <wa_saida_zinv>-hr_val_de = '235959'.
*          <WA_SAIDA_ZINV>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*          <WA_SAIDA_ZINV>-CK_ANT = 'X'.
        ELSE.
          <wa_saida_zinv>-dt_val_de = wa_trans-dt_val_para + 1.
          <wa_saida_zinv>-hr_val_de = '000000'.
        ENDIF.
      ELSE.
        IF wa_trans-dt_val_de EQ sy-datum.
          <wa_saida_zinv>-dt_val_ate = wa_trans-dt_val_de.
          <wa_saida_zinv>-hr_val_ate = vl_uzeit.
        ELSE.
          <wa_saida_zinv>-dt_val_ate = wa_trans-dt_val_de - 1.
          <wa_saida_zinv>-hr_val_ate = '235959'.
        ENDIF.
*        <WA_SAIDA_ZINV>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*        <WA_SAIDA_ZINV>-CK_ANT = 'X'.
      ENDIF.
      "<WA_SAIDA_ZINV>-TRANSF_APROV = WA_TRANS-TRANSF_APROV.
      "<WA_SAIDA_ZINV>-MOTIVO = WA_TRANS-MOTIVO.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida_zinv_trans INTO wa_saida_zinv.
    IF wa_trans-motivo IS NOT INITIAL.
      wa_saida_zinv-motivo = wa_trans-motivo.
    ENDIF.
    APPEND wa_saida_zinv TO it_saida_zinv.
    CLEAR: wa_saida_zinv.
  ENDLOOP.

ENDFORM.

FORM trans_zfre .

  DATA: it_saida_zfre_trans TYPE STANDARD TABLE OF t_zlest0156,
        cont_loop           TYPE i.

  LOOP AT it_saida_zfre ASSIGNING <wa_saida_zfre>.
    cont_loop = sy-tabix.
    IF <wa_saida_zfre>-aprovador = wa_trans-aprovador_de
      AND wa_trans-dt_val_de   >= <wa_saida_zfre>-dt_val_de
      AND wa_trans-dt_val_para <= <wa_saida_zfre>-dt_val_ate.
      "Não passa looping nas estratégias com início e fim no mesmo dia da transferência e no passado
      IF wa_trans-dt_val_de EQ wa_trans-dt_val_para
        AND <wa_saida_zfre>-dt_val_ate EQ <wa_saida_zfre>-dt_val_de
        AND wa_trans-dt_val_de  EQ <wa_saida_zfre>-dt_val_de.
        IF <wa_saida_zfre>-hr_val_ate < sy-uzeit
          AND <wa_saida_zfre>-hr_val_de < sy-uzeit.
          CONTINUE.
        ENDIF.
      ENDIF.
      "Não passa nas estratégias terminadas mas ainda na tela
      IF <wa_saida_zfre>-dt_val_ate EQ sy-datum AND <wa_saida_zfre>-hr_val_ate LT sy-uzeit.
        CONTINUE.
      ENDIF.
      "criando registro do novo aprovador
      wa_saida_zfre-bukrs = <wa_saida_zfre>-bukrs.
      wa_saida_zfre-bukrs_ate = <wa_saida_zfre>-bukrs_ate.
      wa_saida_zfre-nivel = <wa_saida_zfre>-nivel.
      wa_saida_zfre-aprovador = wa_trans-aprovador_para.
      wa_saida_zfre-valor_de = <wa_saida_zfre>-valor_de.
      wa_saida_zfre-valor_ate = <wa_saida_zfre>-valor_ate.
      IF vl_uzeit EQ '235959'.
        wa_saida_zfre-dt_val_de     = wa_trans-dt_val_de + 1.
      ELSE.
        wa_saida_zfre-dt_val_de     = wa_trans-dt_val_de.
      ENDIF.
      IF wa_saida_zfre-dt_val_de EQ sy-datum.
        wa_saida_zfre-hr_val_de     = vl_uzeit + '000001'.
      ELSE.
        wa_saida_zfre-hr_val_de     = '000000'.
      ENDIF.
      wa_saida_zfre-dt_val_ate    = wa_trans-dt_val_para.
      wa_saida_zfre-hr_val_ate    = '235959'.
      wa_saida_zfre-motivo        = wa_trans-motivo.
      wa_saida_zfre-transf_aprov  = wa_trans-transf_aprov.
      wa_saida_zfre-ck_ant        = abap_true.
      APPEND wa_saida_zfre TO it_saida_zfre_trans.
      CLEAR wa_saida_zfre.
      "cria registro para o periodo anterior do aprovador de se a transferencia for provisória
      IF wa_trans-transf_aprov = 'P'.
        wa_saida_zfre-bukrs           = <wa_saida_zfre>-bukrs.
        wa_saida_zfre-bukrs_ate       = <wa_saida_zfre>-bukrs_ate.
        wa_saida_zfre-nivel           = <wa_saida_zfre>-nivel.
        wa_saida_zfre-aprovador       = <wa_saida_zfre>-aprovador.
        wa_saida_zfre-valor_de        = <wa_saida_zfre>-valor_de.
        wa_saida_zfre-valor_ate       = <wa_saida_zfre>-valor_ate.
        wa_saida_zfre-dt_val_de       = <wa_saida_zfre>-dt_val_de.
        wa_saida_zfre-hr_val_de       = <wa_saida_zfre>-hr_val_de.
        IF wa_trans-dt_val_de EQ sy-datum.
          wa_saida_zfre-dt_val_ate    = wa_trans-dt_val_de.
          wa_saida_zfre-hr_val_ate    = vl_uzeit.
        ELSE.
          wa_saida_zfre-hr_val_ate    = '235959'.
          wa_saida_zfre-dt_val_ate    = wa_trans-dt_val_de - 1.
        ENDIF.
        wa_saida_zfre-motivo        = <wa_saida_zfre>-motivo.
        wa_saida_zfre-transf_aprov  = <wa_saida_zfre>-transf_aprov.
        wa_saida_zfre-ck_ant        = 'X'.
        APPEND wa_saida_zfre TO it_saida_zfre_trans.
        CLEAR wa_saida_zfre.
      ENDIF.
      "modificando o registro do aprovador antigo para o periodo futuro
      IF wa_trans-transf_aprov = 'P'.
        IF <wa_saida_zfre>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zfre>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zfre>-hr_val_ate = '235959'.
          <wa_saida_zfre>-dt_val_de = wa_trans-dt_val_para.
          <wa_saida_zfre>-hr_val_de = '235959'.
*          <WA_SAIDA_ZINV>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*          <WA_SAIDA_ZINV>-CK_ANT = 'X'.
        ELSE.
          <wa_saida_zfre>-dt_val_de = wa_trans-dt_val_para + 1.
          <wa_saida_zfre>-hr_val_de = '000000'.
        ENDIF.
      ELSE.
        IF wa_trans-dt_val_de EQ sy-datum.
          <wa_saida_zfre>-dt_val_ate = wa_trans-dt_val_de.
          <wa_saida_zfre>-hr_val_ate = vl_uzeit.
        ELSE.
          <wa_saida_zfre>-dt_val_ate = wa_trans-dt_val_de - 1.
          <wa_saida_zfre>-hr_val_ate = '235959'.
        ENDIF.
*        <WA_SAIDA_ZINV>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*        <WA_SAIDA_ZINV>-CK_ANT = 'X'.
      ENDIF.
      "<WA_SAIDA_ZINV>-TRANSF_APROV = WA_TRANS-TRANSF_APROV.
      "<WA_SAIDA_ZINV>-MOTIVO = WA_TRANS-MOTIVO.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida_zfre_trans INTO wa_saida_zfre.
    IF wa_trans-motivo IS NOT INITIAL.
      wa_saida_zfre-motivo = wa_trans-motivo.
    ENDIF.
    APPEND wa_saida_zfre TO it_saida_zfre.
    CLEAR: wa_saida_zfre.
  ENDLOOP.

ENDFORM.

FORM trans_zsolov.

  DATA: it_saida_zsolov_trans TYPE STANDARD TABLE OF t_zsdt0161,
        cont_loop             TYPE i.

  LOOP AT it_saida_zsolov ASSIGNING <wa_saida_zsolov>.
    cont_loop = sy-tabix.
    IF <wa_saida_zsolov>-aprovador = wa_trans-aprovador_de
      AND wa_trans-dt_val_de >= <wa_saida_zsolov>-dt_val_de
      AND wa_trans-dt_val_para <= <wa_saida_zsolov>-dt_val_ate.
      "Não passa looping nas estratégias com início e fim no mesmo dia da transferência e no passado
      IF wa_trans-dt_val_de EQ wa_trans-dt_val_para
        AND <wa_saida_zsolov>-dt_val_ate EQ <wa_saida_zsolov>-dt_val_de
        AND wa_trans-dt_val_de  EQ <wa_saida_zsolov>-dt_val_de.
        IF <wa_saida_zsolov>-hr_val_ate < sy-uzeit
          AND <wa_saida_zsolov>-hr_val_de < sy-uzeit.
          CONTINUE.
        ENDIF.
      ENDIF.
      "Não passa nas estratégias terminadas mas ainda na tela
      IF <wa_saida_zsolov>-dt_val_ate EQ sy-datum AND <wa_saida_zsolov>-hr_val_ate LT sy-uzeit.
        CONTINUE.
      ENDIF.
      "criando registro do novo aprovador
      wa_saida_zsolov-bukrs         = <wa_saida_zsolov>-bukrs.
      wa_saida_zsolov-bukrs_ate     = <wa_saida_zsolov>-bukrs_ate.
      wa_saida_zsolov-vkbur         = <wa_saida_zsolov>-vkbur.
      wa_saida_zsolov-vkbur_ate     = <wa_saida_zsolov>-vkbur_ate.
      wa_saida_zsolov-tp_venda      = <wa_saida_zsolov>-tp_venda.
      wa_saida_zsolov-tp_venda_ate  = <wa_saida_zsolov>-tp_venda_ate.
      wa_saida_zsolov-waers         = <wa_saida_zsolov>-waers.
      wa_saida_zsolov-nivel         = <wa_saida_zsolov>-nivel.
      wa_saida_zsolov-aprovador     = wa_trans-aprovador_para.
      wa_saida_zsolov-valor_de      = <wa_saida_zsolov>-valor_de.
      wa_saida_zsolov-valor_ate     = <wa_saida_zsolov>-valor_ate.

      IF vl_uzeit EQ '235959'.
        wa_saida_zsolov-dt_val_de     = wa_trans-dt_val_de + 1.
      ELSE.
        wa_saida_zsolov-dt_val_de     = wa_trans-dt_val_de.
      ENDIF.
      IF wa_saida_zsolov-dt_val_de EQ sy-datum.
        wa_saida_zsolov-hr_val_de     = vl_uzeit + '000001'.
      ELSE.
        wa_saida_zsolov-hr_val_de     = '000000'.
      ENDIF.
      wa_saida_zsolov-dt_val_ate    = wa_trans-dt_val_para.
      wa_saida_zsolov-hr_val_ate    = '235959'.
      wa_saida_zsolov-motivo        = wa_trans-motivo.
      wa_saida_zsolov-transf_aprov  = wa_trans-transf_aprov.
      wa_saida_zsolov-ck_ant        = abap_true.
      APPEND wa_saida_zsolov TO it_saida_zsolov_trans.
      CLEAR wa_saida_zsolov.

      "cria registro para o periodo anterior do aprovador de se a transferencia for provisória
      IF wa_trans-transf_aprov = 'P'.
        wa_saida_zsolov-bukrs           = <wa_saida_zsolov>-bukrs.
        wa_saida_zsolov-bukrs_ate       = <wa_saida_zsolov>-bukrs_ate.
        wa_saida_zsolov-vkbur           = <wa_saida_zsolov>-vkbur.
        wa_saida_zsolov-vkbur_ate       = <wa_saida_zsolov>-vkbur_ate.
        wa_saida_zsolov-tp_venda      = <wa_saida_zsolov>-tp_venda.
        wa_saida_zsolov-tp_venda_ate  = <wa_saida_zsolov>-tp_venda_ate.
        wa_saida_zsolov-waers           = <wa_saida_zsolov>-waers.
        wa_saida_zsolov-nivel           = <wa_saida_zsolov>-nivel.
        wa_saida_zsolov-aprovador       = <wa_saida_zsolov>-aprovador.
        wa_saida_zsolov-valor_de        = <wa_saida_zsolov>-valor_de.
        wa_saida_zsolov-valor_ate       = <wa_saida_zsolov>-valor_ate.
        wa_saida_zsolov-dt_val_de       = <wa_saida_zsolov>-dt_val_de.
        wa_saida_zsolov-hr_val_de       = <wa_saida_zsolov>-hr_val_de.
        IF wa_trans-dt_val_de EQ sy-datum.
          wa_saida_zsolov-dt_val_ate    = wa_trans-dt_val_de.
          wa_saida_zsolov-hr_val_ate    = vl_uzeit.
        ELSE.
          wa_saida_zsolov-hr_val_ate    = '235959'.
          wa_saida_zsolov-dt_val_ate    = wa_trans-dt_val_de - 1.
        ENDIF.
        wa_saida_zsolov-motivo        = <wa_saida_zsolov>-motivo.
        wa_saida_zsolov-transf_aprov  = <wa_saida_zsolov>-transf_aprov.
        wa_saida_zsolov-ck_ant        = 'X'.
        APPEND wa_saida_zsolov TO it_saida_zsolov_trans.
        CLEAR wa_saida_zsolov.
      ENDIF.
      "modificando o registro do aprovador antigo para o periodo futuro
      IF wa_trans-transf_aprov = 'P'.
        IF <wa_saida_zsolov>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zsolov>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_zsolov>-hr_val_ate = '235959'.
          <wa_saida_zsolov>-dt_val_de = wa_trans-dt_val_para.
          <wa_saida_zsolov>-hr_val_de = '235959'.
*          <WA_SAIDA_ZINV>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*          <WA_SAIDA_ZINV>-CK_ANT = 'X'.
        ELSE.
          <wa_saida_zsolov>-dt_val_de = wa_trans-dt_val_para + 1.
          <wa_saida_zsolov>-hr_val_de = '000000'.
        ENDIF.
      ELSE.
        IF wa_trans-dt_val_de EQ sy-datum.
          <wa_saida_zsolov>-dt_val_ate = wa_trans-dt_val_de.
          <wa_saida_zsolov>-hr_val_ate = vl_uzeit.
        ELSE.
          <wa_saida_zsolov>-dt_val_ate = wa_trans-dt_val_de - 1.
          <wa_saida_zsolov>-hr_val_ate = '235959'.
        ENDIF.
*        <WA_SAIDA_ZINV>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*        <WA_SAIDA_ZINV>-CK_ANT = 'X'.
      ENDIF.
      "<WA_SAIDA_ZINV>-TRANSF_APROV = WA_TRANS-TRANSF_APROV.
      "<WA_SAIDA_ZINV>-MOTIVO = WA_TRANS-MOTIVO.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida_zsolov_trans INTO wa_saida_zsolov.
    IF wa_trans-motivo IS NOT INITIAL.
      wa_saida_zsolov-motivo = wa_trans-motivo.
    ENDIF.
    APPEND wa_saida_zsolov TO it_saida_zsolov.
    CLEAR: wa_saida_zsolov.
  ENDLOOP.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  CHECK_TRANS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_trans .

  DATA tem_registro TYPE char1.

  CASE wa_trans-estrategia.
    WHEN '1'.
      LOOP AT it_saida_zimp ASSIGNING <wa_saida_zimp>.
        IF <wa_saida_zimp>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZIMP>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZIMP>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zimp>-dt_val_de. "AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZIMP>-DT_VAL_ATE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.
    WHEN '2'.
      LOOP AT it_saida_zglt ASSIGNING <wa_saida_zglt>.
        IF <wa_saida_zglt>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
          IF wa_trans-dt_val_de >= <wa_saida_zglt>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.
    WHEN '3'.
      LOOP AT it_saida_zadto ASSIGNING <wa_saida_zadto>.
        IF <wa_saida_zadto>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZADTO>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZADTO>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zadto>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.
    WHEN '4'.
      LOOP AT it_saida_zinv ASSIGNING <wa_saida_zinv>.
        IF <wa_saida_zinv>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZINV>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZINV>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zinv>-dt_val_de." AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.
    WHEN '5'.
      LOOP AT it_saida_zov ASSIGNING <wa_saida_zov>.
        IF <wa_saida_zov>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZOV>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZOV>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zov>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.

    WHEN '6'.
      LOOP AT it_saida_zfre ASSIGNING <wa_saida_zfre>.
        IF <wa_saida_zfre>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZOV>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZOV>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zfre>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.

    WHEN '7'.
      LOOP AT it_saida_zsolov ASSIGNING <wa_saida_zsolov>.
        IF <wa_saida_zsolov>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZOV>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZOV>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zsolov>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.

    WHEN '9'.
      LOOP AT it_saida_lim ASSIGNING <wa_saida_lim>.
        IF <wa_saida_lim>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZINV>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZINV>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_lim>-dt_val_de." AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.

    WHEN '10'.
      LOOP AT it_saida_var_camb ASSIGNING <wa_saida_varcabio>.
        IF <wa_saida_varcabio>-aprovador = wa_trans-aprovador_de.
          IF wa_trans-dt_val_de >= <wa_saida_varcabio>-dt_val_de." AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.

    WHEN '11'.
      LOOP AT it_saida_isencao ASSIGNING FIELD-SYMBOL(<wa_isencao>).
        IF <wa_isencao>-aprovador = wa_trans-aprovador_de.
          IF wa_trans-dt_val_de >= <wa_isencao>-dt_val_de.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.
    WHEN '12'."150184 CS2024000781 Aprovações ZNFW - PSA
      LOOP AT it_saida_operznfw ASSIGNING FIELD-SYMBOL(<wa_operznfw>).
        IF <wa_operznfw>-aprovador = wa_trans-aprovador_de.
          IF wa_trans-dt_val_de >= <wa_operznfw>-dt_val_de.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.

      " 08.05.2025 - 174338 - RAMON -->
    WHEN '13'."150184 CS2024000781 Aprovações ZNFW - PSA
      LOOP AT it_saida_checklist ASSIGNING FIELD-SYMBOL(<wa_checklist>).
        IF <wa_checklist>-aprovador = wa_trans-aprovador_de.
          IF wa_trans-dt_val_de >= <wa_checklist>-dt_val_de.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.
      " 08.05.2025 - 174338 - RAMON -->

    WHEN OTHERS.
      LOOP AT it_saida_zimp ASSIGNING <wa_saida_zimp>.
        IF <wa_saida_zimp>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZIMP>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZIMP>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zimp>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      LOOP AT it_saida_zglt ASSIGNING <wa_saida_zglt>.
        IF <wa_saida_zglt>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZGLT>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZGLT>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zglt>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      LOOP AT it_saida_zadto ASSIGNING <wa_saida_zadto>.
        IF <wa_saida_zadto>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZADTO>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZADTO>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zadto>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      LOOP AT it_saida_zinv ASSIGNING <wa_saida_zinv>.
        IF <wa_saida_zinv>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZINV>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZINV>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zinv>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      LOOP AT it_saida_zov ASSIGNING <wa_saida_zov>.
        IF <wa_saida_zov>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZOV>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZOV>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zov>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      LOOP AT it_saida_zfre ASSIGNING <wa_saida_zfre>.
        IF <wa_saida_zfre>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZOV>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZOV>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zfre>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      LOOP AT it_saida_zsolov ASSIGNING <wa_saida_zsolov>.
        IF <wa_saida_zsolov>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZOV>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZOV>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_zsolov>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.

      LOOP AT it_saida_var_camb ASSIGNING <wa_saida_varcabio>.
        IF <wa_saida_varcabio>-aprovador = wa_trans-aprovador_de.
          "verificando se a janela de data informada para a transferência está contida no período de aprovação do aprovador de
*          IF WA_TRANS-DT_VAL_DE >= <WA_SAIDA_ZOV>-DT_VAL_DE AND WA_TRANS-DT_VAL_PARA <= <WA_SAIDA_ZOV>-DT_VAL_ATE.
*            TEM_REGISTRO = 'X'.
*          ENDIF.
          IF wa_trans-dt_val_de >= <wa_saida_varcabio>-dt_val_de. " AND WA_TRANS-DT_VAL_PARA >= WA_TRANS-DT_VAL_DE.
            tem_registro = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tem_registro NE 'X'.
        MESSAGE 'O Aprovador delegante não possui permissão para o período selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 5005.
      ENDIF.
      CLEAR tem_registro.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRANS_LIM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trans_lim .
  DATA: it_saida_lim_trans TYPE STANDARD TABLE OF t_zsdt0152,
        cont_loop          TYPE i.

  LOOP AT it_saida_lim ASSIGNING <wa_saida_lim>.
    cont_loop = sy-tabix.
    IF <wa_saida_lim>-aprovador = wa_trans-aprovador_de
      AND wa_trans-dt_val_de >= <wa_saida_lim>-dt_val_de
      AND wa_trans-dt_val_para <= <wa_saida_lim>-dt_val_ate.
      "Não passa looping nas estratégias com início e fim no mesmo dia da transferência e no passado
      IF wa_trans-dt_val_de EQ wa_trans-dt_val_para
        AND <wa_saida_lim>-dt_val_ate EQ <wa_saida_lim>-dt_val_de
        AND wa_trans-dt_val_de  EQ <wa_saida_lim>-dt_val_de.
        IF <wa_saida_lim>-hr_val_ate < sy-uzeit
          AND <wa_saida_lim>-hr_val_de < sy-uzeit.
          CONTINUE.
        ENDIF.
      ENDIF.
      "Não passa nas estratégias terminadas mas ainda na tela
      IF <wa_saida_lim>-dt_val_ate EQ sy-datum AND <wa_saida_lim>-hr_val_ate LT sy-uzeit.
        CONTINUE.
      ENDIF.
      "criando registro do novo aprovador
      wa_saida_lim-vkorg         = <wa_saida_lim>-vkorg.
      wa_saida_lim-werks         = <wa_saida_lim>-werks.
      wa_saida_lim-werks_ate     = <wa_saida_lim>-werks_ate.
      wa_saida_lim-waers         = <wa_saida_lim>-waers.
      wa_saida_lim-nivel         = <wa_saida_lim>-nivel.
      wa_saida_lim-aprovador     = wa_trans-aprovador_para.
      wa_saida_lim-valor_de      = <wa_saida_lim>-valor_de.
      wa_saida_lim-valor_ate     = <wa_saida_lim>-valor_ate.
      "WA_SAIDA_LIM-DT_VAL_DE = WA_TRANS-DT_VAL_DE.
      IF vl_uzeit EQ '235959'.
        wa_saida_lim-dt_val_de     = wa_trans-dt_val_de + 1.
      ELSE.
        wa_saida_lim-dt_val_de     = wa_trans-dt_val_de.
      ENDIF.
      IF wa_saida_lim-dt_val_de EQ sy-datum.
        wa_saida_lim-hr_val_de     = vl_uzeit + '000001'.
      ELSE.
        wa_saida_lim-hr_val_de     = '000000'.
      ENDIF.
      wa_saida_lim-dt_val_ate    = wa_trans-dt_val_para.
      wa_saida_lim-hr_val_ate    = '235959'.
      wa_saida_lim-motivo        = wa_trans-motivo.
      wa_saida_lim-transf_aprov  = wa_trans-transf_aprov.
      wa_saida_lim-ck_ant        = abap_true.
      APPEND wa_saida_lim TO it_saida_lim_trans.
      CLEAR wa_saida_lim.
      "cria registro para o periodo anterior do aprovador de se a transferencia for provisória
      IF wa_trans-transf_aprov = 'P'.
        wa_saida_lim-vkorg         = <wa_saida_lim>-vkorg.
        wa_saida_lim-werks         = <wa_saida_lim>-werks.
        wa_saida_lim-werks_ate     = <wa_saida_lim>-werks_ate.
        wa_saida_lim-waers         = <wa_saida_lim>-waers.
        wa_saida_lim-nivel         = <wa_saida_lim>-nivel.
        wa_saida_lim-aprovador     = <wa_saida_lim>-aprovador.
        wa_saida_lim-valor_de      = <wa_saida_lim>-valor_de.
        wa_saida_lim-valor_ate     = <wa_saida_lim>-valor_ate.
        wa_saida_lim-dt_val_de     = <wa_saida_lim>-dt_val_de.
        wa_saida_lim-hr_val_de     = <wa_saida_lim>-hr_val_de.
        IF wa_trans-dt_val_de EQ sy-datum.
          wa_saida_lim-dt_val_ate    = wa_trans-dt_val_de.
          wa_saida_lim-hr_val_ate    = vl_uzeit.
        ELSE.
          wa_saida_lim-hr_val_ate    = '235959'.
          wa_saida_lim-dt_val_ate    = wa_trans-dt_val_de - 1.
        ENDIF.
        "WA_SAIDA_LIM-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
        wa_saida_lim-motivo        = <wa_saida_lim>-motivo.
        wa_saida_lim-transf_aprov  = <wa_saida_lim>-transf_aprov.
        wa_saida_lim-ck_ant = 'X'.
        APPEND wa_saida_lim TO it_saida_lim_trans.
        CLEAR wa_saida_lim.
      ENDIF.
      "modificando o registro do aprovador antigo para o periodo futuro
      IF wa_trans-transf_aprov = 'P'.
        IF <wa_saida_lim>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_lim>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_lim>-hr_val_ate = '235959'.
          <wa_saida_lim>-dt_val_de = wa_trans-dt_val_para.
          <wa_saida_lim>-hr_val_de = '235959'.
*          <WA_SAIDA_LIM>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*          <WA_SAIDA_LIM>-CK_ANT = 'X'.
        ELSE.
          <wa_saida_lim>-dt_val_de = wa_trans-dt_val_para + 1.
          <wa_saida_lim>-hr_val_de = '000000'.
        ENDIF.
      ELSE.
        IF wa_trans-dt_val_de EQ sy-datum.
          <wa_saida_lim>-dt_val_ate = wa_trans-dt_val_de.
          <wa_saida_lim>-hr_val_ate = vl_uzeit.
        ELSE.
          <wa_saida_lim>-dt_val_ate = wa_trans-dt_val_de - 1.
          <wa_saida_lim>-hr_val_ate = '235959'.
        ENDIF.
*        <WA_SAIDA_LIM>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*        <WA_SAIDA_LIM>-CK_ANT = 'X'.
      ENDIF.
      "<WA_SAIDA_LIM>-TRANSF_APROV = WA_TRANS-TRANSF_APROV.
      "<WA_SAIDA_LIM>-MOTIVO = WA_TRANS-MOTIVO.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida_lim_trans INTO wa_saida_lim.
    IF wa_trans-motivo IS NOT INITIAL.
      wa_saida_lim-motivo = wa_trans-motivo.
    ENDIF.

    APPEND wa_saida_lim TO it_saida_lim.
    CLEAR wa_saida_lim.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRANS_VARCAMBIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trans_varcambio .
  DATA: it_saida_varcambio TYPE STANDARD TABLE OF t_zmmt0150,
        cont_loop          TYPE i.

  LOOP AT it_saida_var_camb ASSIGNING <wa_saida_varcabio>.
    cont_loop = sy-tabix.
    IF <wa_saida_varcabio>-aprovador = wa_trans-aprovador_de
      AND wa_trans-dt_val_de   >= <wa_saida_varcabio>-dt_val_de
      AND wa_trans-dt_val_para <= <wa_saida_varcabio>-dt_val_ate.
      "Não passa looping nas estratégias com início e fim no mesmo dia da transferência e no passado
      IF wa_trans-dt_val_de EQ wa_trans-dt_val_para
        AND <wa_saida_varcabio>-dt_val_ate EQ <wa_saida_varcabio>-dt_val_de
        AND wa_trans-dt_val_de  EQ <wa_saida_varcabio>-dt_val_de.
        IF <wa_saida_varcabio>-hr_val_ate < sy-uzeit
          AND <wa_saida_varcabio>-hr_val_de < sy-uzeit.
          CONTINUE.
        ENDIF.
      ENDIF.
      "Não passa nas estratégias terminadas mas ainda na tela
      IF <wa_saida_varcabio>-dt_val_ate EQ sy-datum AND <wa_saida_varcabio>-hr_val_ate LT sy-uzeit.
        CONTINUE.
      ENDIF.
      "criando registro do novo aprovador
      wa_saida_var_camb-bukrs = <wa_saida_varcabio>-bukrs.
      wa_saida_var_camb-bukrs_ate = <wa_saida_varcabio>-bukrs_ate.
      wa_saida_var_camb-nivel = <wa_saida_varcabio>-nivel.
      wa_saida_var_camb-aprovador = wa_trans-aprovador_para.
      wa_saida_var_camb-valor_de = <wa_saida_varcabio>-valor_de.
      wa_saida_var_camb-valor_ate = <wa_saida_varcabio>-valor_ate.
      IF vl_uzeit EQ '235959'.
        wa_saida_var_camb-dt_val_de     = wa_trans-dt_val_de + 1.
      ELSE.
        wa_saida_var_camb-dt_val_de     = wa_trans-dt_val_de.
      ENDIF.
      IF wa_saida_var_camb-dt_val_de EQ sy-datum.
        wa_saida_var_camb-hr_val_de     = vl_uzeit + '000001'.
      ELSE.
        wa_saida_var_camb-hr_val_de     = '000000'.
      ENDIF.
      wa_saida_var_camb-dt_val_ate    = wa_trans-dt_val_para.
      wa_saida_var_camb-hr_val_ate    = '235959'.
      wa_saida_var_camb-motivo        = wa_trans-motivo.
      wa_saida_var_camb-transf_aprov  = wa_trans-transf_aprov.
      wa_saida_var_camb-ck_ant        = abap_true.
      APPEND wa_saida_var_camb TO it_saida_varcambio.
      CLEAR wa_saida_var_camb.
      "cria registro para o periodo anterior do aprovador de se a transferencia for provisória
      IF wa_trans-transf_aprov = 'P'.
        wa_saida_var_camb-bukrs           = <wa_saida_varcabio>-bukrs.
        wa_saida_var_camb-bukrs_ate       = <wa_saida_varcabio>-bukrs_ate.
        wa_saida_var_camb-nivel           = <wa_saida_varcabio>-nivel.
        wa_saida_var_camb-aprovador       = <wa_saida_varcabio>-aprovador.
        wa_saida_var_camb-valor_de        = <wa_saida_varcabio>-valor_de.
        wa_saida_var_camb-valor_ate       = <wa_saida_varcabio>-valor_ate.
        wa_saida_var_camb-dt_val_de       = <wa_saida_varcabio>-dt_val_de.
        wa_saida_var_camb-hr_val_de       = <wa_saida_varcabio>-hr_val_de.
        IF wa_trans-dt_val_de EQ sy-datum.
          wa_saida_var_camb-dt_val_ate    = wa_trans-dt_val_de.
          wa_saida_var_camb-hr_val_ate    = vl_uzeit.
        ELSE.
          wa_saida_var_camb-hr_val_ate    = '235959'.
          wa_saida_var_camb-dt_val_ate    = wa_trans-dt_val_de - 1.
        ENDIF.
        wa_saida_var_camb-motivo        = <wa_saida_varcabio>-motivo.
        wa_saida_var_camb-transf_aprov  = <wa_saida_varcabio>-transf_aprov.
        wa_saida_var_camb-ck_ant        = 'X'.
        APPEND wa_saida_var_camb TO it_saida_varcambio.
        CLEAR wa_saida_var_camb.
      ENDIF.
      "modificando o registro do aprovador antigo para o periodo futuro
      IF wa_trans-transf_aprov = 'P'.
        IF <wa_saida_varcabio>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_varcabio>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_varcabio>-hr_val_ate = '235959'.
          <wa_saida_varcabio>-dt_val_de = wa_trans-dt_val_para.
          <wa_saida_varcabio>-hr_val_de = '235959'.
*          <WA_SAIDA_ZINV>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*          <WA_SAIDA_ZINV>-CK_ANT = 'X'.
        ELSE.
          <wa_saida_varcabio>-dt_val_de = wa_trans-dt_val_para + 1.
          <wa_saida_varcabio>-hr_val_de = '000000'.
        ENDIF.
      ELSE.
        IF wa_trans-dt_val_de EQ sy-datum.
          <wa_saida_varcabio>-dt_val_ate = wa_trans-dt_val_de.
          <wa_saida_varcabio>-hr_val_ate = vl_uzeit.
        ELSE.
          <wa_saida_varcabio>-dt_val_ate = wa_trans-dt_val_de - 1.
          <wa_saida_varcabio>-hr_val_ate = '235959'.
        ENDIF.
*        <WA_SAIDA_ZINV>-DT_VAL_ATE = WA_TRANS-DT_VAL_DE - 1.
*        <WA_SAIDA_ZINV>-CK_ANT = 'X'.
      ENDIF.
      "<WA_SAIDA_ZINV>-TRANSF_APROV = WA_TRANS-TRANSF_APROV.
      "<WA_SAIDA_ZINV>-MOTIVO = WA_TRANS-MOTIVO.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida_varcambio INTO wa_saida_var_camb.
    IF wa_trans-motivo IS NOT INITIAL.
      wa_saida_var_camb-motivo = wa_trans-motivo.
    ENDIF.
    APPEND wa_saida_var_camb TO it_saida_var_camb.
    CLEAR: wa_saida_var_camb.
  ENDLOOP.
ENDFORM.

FORM trans_isencao.

  DATA: it_saida_isencao_var TYPE STANDARD TABLE OF t_zsdt0336,
        cont_loop            TYPE i.

  LOOP AT it_saida_isencao ASSIGNING <wa_saida_isencao>.
    cont_loop = sy-tabix.
    IF <wa_saida_isencao>-aprovador = wa_trans-aprovador_de
      AND wa_trans-dt_val_de   >= <wa_saida_isencao>-dt_val_de
      AND wa_trans-dt_val_para <= <wa_saida_isencao>-dt_val_ate.
      "Não passa looping nas estratégias com início e fim no mesmo dia da transferência e no passado
      IF wa_trans-dt_val_de EQ wa_trans-dt_val_para
        AND <wa_saida_isencao>-dt_val_ate EQ <wa_saida_isencao>-dt_val_de
        AND wa_trans-dt_val_de  EQ <wa_saida_isencao>-dt_val_de.
        IF <wa_saida_isencao>-hr_val_ate < sy-uzeit
          AND <wa_saida_isencao>-hr_val_de < sy-uzeit.
          CONTINUE.
        ENDIF.
      ENDIF.
      "Não passa nas estratégias terminadas mas ainda na tela
      IF <wa_saida_isencao>-dt_val_ate EQ sy-datum AND <wa_saida_isencao>-hr_val_ate LT sy-uzeit.
        CONTINUE.
      ENDIF.
      "criando registro do novo aprovador
      wa_saida_isencao-bukrs          = <wa_saida_isencao>-bukrs.
      wa_saida_isencao-bukrs_ate      = <wa_saida_isencao>-bukrs_ate.
      wa_saida_isencao-vkbur          = <wa_saida_isencao>-vkbur.
      wa_saida_isencao-vkbur_ate      = <wa_saida_isencao>-vkbur_ate.
*      wa_saida_isencao-waers          = <wa_saida_isencao>-waers. "Bug 189279 - SMC 12-09-2025
      wa_saida_isencao-tp_negocio_de  = <wa_saida_isencao>-tp_negocio_de.
      wa_saida_isencao-tp_negocio_ate = <wa_saida_isencao>-tp_negocio_ate.
      wa_saida_isencao-nivel          = <wa_saida_isencao>-nivel.
      wa_saida_isencao-aprovador      = wa_trans-aprovador_para.
      wa_saida_isencao-valor_de       = <wa_saida_isencao>-valor_de.
      wa_saida_isencao-valor_ate      = <wa_saida_isencao>-valor_ate.
      IF vl_uzeit EQ '235959'.
        wa_saida_isencao-dt_val_de     = wa_trans-dt_val_de + 1.
      ELSE.
        wa_saida_isencao-dt_val_de     = wa_trans-dt_val_de.
      ENDIF.
      IF wa_saida_isencao-dt_val_de EQ sy-datum.
        wa_saida_isencao-hr_val_de     = vl_uzeit + '000001'.
      ELSE.
        wa_saida_isencao-hr_val_de     = '000000'.
      ENDIF.
      wa_saida_isencao-dt_val_ate    = wa_trans-dt_val_para.
      wa_saida_isencao-hr_val_ate    = '235959'.
      wa_saida_isencao-motivo        = wa_trans-motivo.
      wa_saida_isencao-transf_aprov  = wa_trans-transf_aprov.
      wa_saida_isencao-ck_ant        = abap_true.
      APPEND wa_saida_isencao TO it_saida_isencao_var.
      CLEAR wa_saida_isencao.
      "cria registro para o periodo anterior do aprovador de se a transferencia for provisória
      IF wa_trans-transf_aprov = 'P'.
        wa_saida_isencao-bukrs           = <wa_saida_isencao>-bukrs.
        wa_saida_isencao-bukrs_ate       = <wa_saida_isencao>-bukrs_ate.
        wa_saida_isencao-vkbur          = <wa_saida_isencao>-vkbur.
        wa_saida_isencao-vkbur_ate      = <wa_saida_isencao>-vkbur_ate.
        wa_saida_isencao-tp_negocio_de  = <wa_saida_isencao>-tp_negocio_de.
        wa_saida_isencao-tp_negocio_ate = <wa_saida_isencao>-tp_negocio_ate.
*        wa_saida_isencao-waers          = <wa_saida_isencao>-waers."Bug 189279 - SMC 12-09-2025
        wa_saida_isencao-nivel           = <wa_saida_isencao>-nivel.
        wa_saida_isencao-aprovador       = <wa_saida_isencao>-aprovador.
        wa_saida_isencao-valor_de        = <wa_saida_isencao>-valor_de.
        wa_saida_isencao-valor_ate       = <wa_saida_isencao>-valor_ate.
        wa_saida_isencao-dt_val_de       = <wa_saida_isencao>-dt_val_de.
        wa_saida_isencao-hr_val_de       = <wa_saida_isencao>-hr_val_de.
        IF wa_trans-dt_val_de EQ sy-datum.
          wa_saida_isencao-dt_val_ate    = wa_trans-dt_val_de.
          wa_saida_isencao-hr_val_ate    = vl_uzeit.
        ELSE.
          wa_saida_isencao-hr_val_ate    = '235959'.
          wa_saida_isencao-dt_val_ate    = wa_trans-dt_val_de - 1.
        ENDIF.
        wa_saida_isencao-motivo        = <wa_saida_isencao>-motivo.
        wa_saida_isencao-transf_aprov  = <wa_saida_isencao>-transf_aprov.
        wa_saida_isencao-ck_ant        = 'X'.
        APPEND wa_saida_isencao TO it_saida_isencao_var .
        CLEAR wa_saida_isencao.
      ENDIF.
      "modificando o registro do aprovador antigo para o periodo futuro
      IF wa_trans-transf_aprov = 'P'.
        IF <wa_saida_isencao>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_isencao>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_isencao>-hr_val_ate = '235959'.
          <wa_saida_isencao>-dt_val_de = wa_trans-dt_val_para.
          <wa_saida_isencao>-hr_val_de = '235959'.
        ELSE.
          <wa_saida_isencao>-dt_val_de = wa_trans-dt_val_para + 1.
          <wa_saida_isencao>-hr_val_de = '000000'.
        ENDIF.
      ELSE.
        IF wa_trans-dt_val_de EQ sy-datum.
          <wa_saida_isencao>-dt_val_ate = wa_trans-dt_val_de.
          <wa_saida_isencao>-hr_val_ate = vl_uzeit.
        ELSE.
          <wa_saida_isencao>-dt_val_ate = wa_trans-dt_val_de - 1.
          <wa_saida_isencao>-hr_val_ate = '235959'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida_isencao_var INTO wa_saida_isencao .
    IF wa_trans-motivo IS NOT INITIAL.
      wa_saida_isencao-motivo = wa_trans-motivo.
    ENDIF.
    APPEND wa_saida_isencao TO it_saida_isencao.
    CLEAR: wa_saida_isencao.
  ENDLOOP.

ENDFORM.

FORM trans_operznfw.

  DATA: it_saida_operznfw_trans TYPE STANDARD TABLE OF t_zfiwrt0033,
        cont_loop               TYPE i.

  LOOP AT it_saida_operznfw ASSIGNING <wa_saida_operznfw>.
    cont_loop = sy-tabix.
    IF <wa_saida_operznfw>-aprovador = wa_trans-aprovador_de
      AND wa_trans-dt_val_de   >= <wa_saida_operznfw>-dt_val_de
      AND wa_trans-dt_val_para <= <wa_saida_operznfw>-dt_val_ate.
      "Não passa looping nas estratégias com início e fim no mesmo dia da transferência e no passado
      IF wa_trans-dt_val_de EQ wa_trans-dt_val_para
        AND <wa_saida_operznfw>-dt_val_ate EQ <wa_saida_operznfw>-dt_val_de
        AND wa_trans-dt_val_de  EQ <wa_saida_operznfw>-dt_val_de.
        IF <wa_saida_operznfw>-hr_val_ate < sy-uzeit
          AND <wa_saida_operznfw>-hr_val_de < sy-uzeit.
          CONTINUE.
        ENDIF.
      ENDIF.
      "Não passa nas estratégias terminadas mas ainda na tela
      IF <wa_saida_operznfw>-dt_val_ate EQ sy-datum AND <wa_saida_operznfw>-hr_val_ate LT sy-uzeit.
        CONTINUE.
      ENDIF.
      "criando registro do novo aprovador
      wa_saida_operznfw-dep_resp          = <wa_saida_operznfw>-dep_resp.
      wa_saida_operznfw-nivel          = <wa_saida_operznfw>-nivel.
      wa_saida_operznfw-aprovador      = wa_trans-aprovador_para.
      IF vl_uzeit EQ '235959'.
        wa_saida_operznfw-dt_val_de     = wa_trans-dt_val_de + 1.
      ELSE.
        wa_saida_operznfw-dt_val_de     = wa_trans-dt_val_de.
      ENDIF.
      IF wa_saida_operznfw-dt_val_de EQ sy-datum.
        wa_saida_operznfw-hr_val_de     = vl_uzeit + '000001'.
      ELSE.
        wa_saida_operznfw-hr_val_de     = '000000'.
      ENDIF.
      wa_saida_operznfw-dt_val_ate    = wa_trans-dt_val_para.
      wa_saida_operznfw-hr_val_ate    = '235959'.
      wa_saida_operznfw-motivo        = wa_trans-motivo.
      wa_saida_operznfw-transf_aprov  = wa_trans-transf_aprov.
      wa_saida_operznfw-ck_ant        = abap_true.
      APPEND wa_saida_operznfw TO it_saida_operznfw_trans.
*      APPEND wa_saida_operznfw TO it_saida_operznfw.
      CLEAR wa_saida_operznfw.
      "cria registro para o periodo anterior do aprovador de se a transferencia for provisória
      IF wa_trans-transf_aprov = 'P'.
        wa_saida_operznfw-dep_resp           = <wa_saida_operznfw>-dep_resp.
        wa_saida_operznfw-nivel           = <wa_saida_operznfw>-nivel.
        wa_saida_operznfw-aprovador       = <wa_saida_operznfw>-aprovador.
        wa_saida_operznfw-dt_val_de       = <wa_saida_operznfw>-dt_val_de.
        wa_saida_operznfw-hr_val_de       = <wa_saida_operznfw>-hr_val_de.
        IF wa_trans-dt_val_de EQ sy-datum.
          wa_saida_operznfw-dt_val_ate    = wa_trans-dt_val_de.
          wa_saida_operznfw-hr_val_ate    = vl_uzeit.
        ELSE.
          wa_saida_operznfw-hr_val_ate    = '235959'.
          wa_saida_operznfw-dt_val_ate    = wa_trans-dt_val_de - 1.
        ENDIF.
        wa_saida_operznfw-motivo        = <wa_saida_operznfw>-motivo.
        wa_saida_operznfw-transf_aprov  = <wa_saida_operznfw>-transf_aprov.
        wa_saida_operznfw-ck_ant        = 'X'.
        APPEND wa_saida_operznfw TO it_saida_operznfw_trans.
        CLEAR wa_saida_operznfw.
      ENDIF.
      "modificando o registro do aprovador antigo para o periodo futuro
      IF wa_trans-transf_aprov = 'P'.
        IF <wa_saida_operznfw>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_operznfw>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_operznfw>-hr_val_ate = '235959'.
          <wa_saida_operznfw>-dt_val_de = wa_trans-dt_val_para.
          <wa_saida_operznfw>-hr_val_de = '235959'.
        ELSE.
          <wa_saida_operznfw>-dt_val_de = wa_trans-dt_val_para + 1.
          <wa_saida_operznfw>-hr_val_de = '000000'.
        ENDIF.
      ELSE.
        IF wa_trans-dt_val_de EQ sy-datum.
          <wa_saida_operznfw>-dt_val_ate = wa_trans-dt_val_de.
          <wa_saida_operznfw>-hr_val_ate = vl_uzeit.
        ELSE.
          <wa_saida_operznfw>-dt_val_ate = wa_trans-dt_val_de - 1.
          <wa_saida_operznfw>-hr_val_ate = '235959'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida_operznfw_trans INTO wa_saida_operznfw .
    IF wa_trans-motivo IS NOT INITIAL.
      wa_saida_operznfw-motivo = wa_trans-motivo.
    ENDIF.
    APPEND wa_saida_operznfw TO it_saida_operznfw.
    CLEAR: wa_saida_operznfw.
  ENDLOOP.


ENDFORM.
FORM trans_checklist.

  DATA: it_saida_checklist_var TYPE STANDARD TABLE OF t_zsdt0385,
        cont_loop              TYPE i.

  LOOP AT it_saida_checklist ASSIGNING <wa_saida_checklist>.
    cont_loop = sy-tabix.
    IF <wa_saida_checklist>-aprovador = wa_trans-aprovador_de
      AND wa_trans-dt_val_de   >= <wa_saida_checklist>-dt_val_de
      AND wa_trans-dt_val_para <= <wa_saida_checklist>-dt_val_ate.
      "Não passa looping nas estratégias com início e fim no mesmo dia da transferência e no passado
      IF wa_trans-dt_val_de EQ wa_trans-dt_val_para
        AND <wa_saida_checklist>-dt_val_ate EQ <wa_saida_checklist>-dt_val_de
        AND wa_trans-dt_val_de  EQ <wa_saida_checklist>-dt_val_de.
        IF <wa_saida_checklist>-hr_val_ate < sy-uzeit
          AND <wa_saida_checklist>-hr_val_de < sy-uzeit.
          CONTINUE.
        ENDIF.
      ENDIF.
      "Não passa nas estratégias terminadas mas ainda na tela
      IF <wa_saida_checklist>-dt_val_ate EQ sy-datum AND <wa_saida_checklist>-hr_val_ate LT sy-uzeit.
        CONTINUE.
      ENDIF.
      "criando registro do novo aprovador
      wa_saida_checklist-bukrs          = <wa_saida_checklist>-bukrs.
      wa_saida_checklist-bukrs_ate      = <wa_saida_checklist>-bukrs_ate.
      wa_saida_checklist-vkbur          = <wa_saida_checklist>-vkbur.
      wa_saida_checklist-vkbur_ate      = <wa_saida_checklist>-vkbur_ate.
      "wa_saida_checklist-waers          = <wa_saida_checklist>-waers.
      "wa_saida_checklist-tp_negocio_de  = <wa_saida_checklist>-tp_negocio_de.
      "wa_saida_checklist-tp_negocio_ate = <wa_saida_checklist>-tp_negocio_ate.
      wa_saida_checklist-nivel          = <wa_saida_checklist>-nivel.
      wa_saida_checklist-aprovador      = wa_trans-aprovador_para.
      "wa_saida_checklist-valor_de       = <wa_saida_checklist>-valor_de.
      "wa_saida_checklist-valor_ate      = <wa_saida_checklist>-valor_ate.
      IF vl_uzeit EQ '235959'.
        wa_saida_checklist-dt_val_de     = wa_trans-dt_val_de + 1.
      ELSE.
        wa_saida_checklist-dt_val_de     = wa_trans-dt_val_de.
      ENDIF.
      IF wa_saida_checklist-dt_val_de EQ sy-datum.
        wa_saida_checklist-hr_val_de     = vl_uzeit + '000001'.
      ELSE.
        wa_saida_checklist-hr_val_de     = '000000'.
      ENDIF.
      wa_saida_checklist-dt_val_ate    = wa_trans-dt_val_para.
      wa_saida_checklist-hr_val_ate    = '235959'.
      wa_saida_checklist-motivo        = wa_trans-motivo.
      wa_saida_checklist-transf_aprov  = wa_trans-transf_aprov.
      wa_saida_checklist-ck_ant        = abap_true.
      APPEND wa_saida_checklist TO it_saida_checklist_var.
      CLEAR wa_saida_checklist.
      "cria registro para o periodo anterior do aprovador de se a transferencia for provisória
      IF wa_trans-transf_aprov = 'P'.
        wa_saida_checklist-bukrs           = <wa_saida_checklist>-bukrs.
        wa_saida_checklist-bukrs_ate       = <wa_saida_checklist>-bukrs_ate.
        wa_saida_checklist-vkbur          = <wa_saida_checklist>-vkbur.
        wa_saida_checklist-vkbur_ate      = <wa_saida_checklist>-vkbur_ate.
        "wa_saida_checklist-tp_negocio_de  = <wa_saida_checklist>-tp_negocio_de.
        "wa_saida_checklist-tp_negocio_ate = <wa_saida_checklist>-tp_negocio_ate.
        "wa_saida_checklist-waers          = <wa_saida_checklist>-waers.
        wa_saida_checklist-nivel           = <wa_saida_checklist>-nivel.
        wa_saida_checklist-aprovador       = <wa_saida_checklist>-aprovador.
        "wa_saida_checklist-valor_de        = <wa_saida_checklist>-valor_de.
        "wa_saida_checklist-valor_ate       = <wa_saida_checklist>-valor_ate.
        wa_saida_checklist-dt_val_de       = <wa_saida_checklist>-dt_val_de.
        wa_saida_checklist-hr_val_de       = <wa_saida_checklist>-hr_val_de.
        IF wa_trans-dt_val_de EQ sy-datum.
          wa_saida_checklist-dt_val_ate    = wa_trans-dt_val_de.
          wa_saida_checklist-hr_val_ate    = vl_uzeit.
        ELSE.
          wa_saida_checklist-hr_val_ate    = '235959'.
          wa_saida_checklist-dt_val_ate    = wa_trans-dt_val_de - 1.
        ENDIF.
        wa_saida_checklist-motivo        = <wa_saida_checklist>-motivo.
        wa_saida_checklist-transf_aprov  = <wa_saida_checklist>-transf_aprov.
        wa_saida_checklist-ck_ant        = 'X'.
        APPEND wa_saida_checklist TO it_saida_checklist_var .
        CLEAR wa_saida_checklist.
      ENDIF.
      "modificando o registro do aprovador antigo para o periodo futuro
      IF wa_trans-transf_aprov = 'P'.
        IF <wa_saida_checklist>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_checklist>-dt_val_ate = wa_trans-dt_val_para.
          <wa_saida_checklist>-hr_val_ate = '235959'.
          <wa_saida_checklist>-dt_val_de = wa_trans-dt_val_para.
          <wa_saida_checklist>-hr_val_de = '235959'.
        ELSE.
          <wa_saida_checklist>-dt_val_de = wa_trans-dt_val_para + 1.
          <wa_saida_checklist>-hr_val_de = '000000'.
        ENDIF.
      ELSE.
        IF wa_trans-dt_val_de EQ sy-datum.
          <wa_saida_checklist>-dt_val_ate = wa_trans-dt_val_de.
          <wa_saida_checklist>-hr_val_ate = vl_uzeit.
        ELSE.
          <wa_saida_checklist>-dt_val_ate = wa_trans-dt_val_de - 1.
          <wa_saida_checklist>-hr_val_ate = '235959'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT it_saida_checklist_var INTO wa_saida_checklist .
    IF wa_trans-motivo IS NOT INITIAL.
      wa_saida_checklist-motivo = wa_trans-motivo.
    ENDIF.
    APPEND wa_saida_checklist TO it_saida_checklist.
    CLEAR: wa_saida_checklist.
  ENDLOOP.

ENDFORM.
