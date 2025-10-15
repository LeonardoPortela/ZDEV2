*&---------------------------------------------------------------------*
*&  Include           ZFIR00749000
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*
*
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  CONTROLS:  main_tab TYPE TABSTRIP.                    "Tabstrip declaration
*  DATA: CK_DISP TYPE CHAR1.                              "Check Primeira apresentação da ALV

  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITLE'.

  IF ck_disp IS INITIAL.
    PERFORM select_data.
    PERFORM sort_data.
  ENDIF.

  main_tab-activetab = i_main_tab-pressed_tab.

  CASE i_main_tab-pressed_tab.
    WHEN c_main_tab-tab1.
      PERFORM create_object USING 'CC_9100'.            "Creating Object
      PERFORM create_fieldcat USING 'ZIMP_APROVADOR'.   "Building the field catalog
      PERFORM fieldcat_layout USING 'ZIMP_APROVADOR'.   "Building the fieldcat layout
      i_main_tab-subscreen = '9100'.
      PERFORM display_output USING it_saida_zimp.       "Displaying data
    WHEN c_main_tab-tab2.
      PERFORM create_object USING 'CC_9200'.            "Creating Object
      PERFORM create_fieldcat USING 'ZGLT037'.          "Building the field catalog
      PERFORM fieldcat_layout USING 'ZGLT037'.          "Building the fieldcat layout
      i_main_tab-subscreen = '9200'.
      PERFORM display_output USING it_saida_zglt.       "Displaying data
    WHEN c_main_tab-tab3.
      PERFORM create_object USING 'CC_9400'.            "Creating Object
      PERFORM create_fieldcat USING 'ZINV_APROVADOR'.   "Building the field catalog
      PERFORM fieldcat_layout USING 'ZINV_APROVADOR'.   "Building the fieldcat layout
      i_main_tab-subscreen = '9400'.
      PERFORM display_output USING it_saida_zinv.       "Displaying data
    WHEN c_main_tab-tab4.
      PERFORM create_object USING 'CC_9300'.            "Creating Object
      PERFORM create_fieldcat USING 'ZADTO_APROVADOR'.  "Building the field catalog
      PERFORM fieldcat_layout USING 'ZADTO_APROVADOR'.  "Building the fieldcat layout
      i_main_tab-subscreen = '9300'.
      PERFORM display_output USING it_saida_zadto.      "Displaying data
    WHEN c_main_tab-tab5.
      PERFORM create_object USING 'CC_9500'.            "Creating Object
      PERFORM create_fieldcat USING 'ZSDT0141'.  "Building the field catalog
      PERFORM fieldcat_layout USING 'ZSDT0141'.  "Building the fieldcat layout
      i_main_tab-subscreen = '9500'.
      PERFORM display_output USING it_saida_zov.      "Displaying data
    WHEN c_main_tab-tab6.
      PERFORM create_object USING 'CC_9600'.            "Creating Object
      PERFORM create_fieldcat USING 'ZSDT0152'.  "Building the field catalog
      PERFORM fieldcat_layout USING 'ZSDT0152'.  "Building the fieldcat layout
      i_main_tab-subscreen = '9600'.
      PERFORM display_output USING it_saida_lim.      "Displaying data
    WHEN c_main_tab-tab7.
      PERFORM create_object USING 'CC_9700'.            "Creating Object
      PERFORM create_fieldcat USING 'ZLEST0156'.  "Building the field catalog
      PERFORM fieldcat_layout USING 'ZLEST0156'.  "Building the fieldcat layout
      i_main_tab-subscreen = '9700'.
      PERFORM display_output USING it_saida_zfre.      "Displaying data
    WHEN c_main_tab-tab8.
      PERFORM create_object USING 'CC_9800'.            "Creating Object
      PERFORM create_fieldcat USING 'ZSDT0161'.  "Building the field catalog
      PERFORM fieldcat_layout USING 'ZSDT0161'.  "Building the fieldcat layout
      i_main_tab-subscreen = '9800'.
      PERFORM display_output USING it_saida_zsolov.      "Displaying data
    WHEN c_main_tab-tab9.
      PERFORM create_object USING 'CC_9900'.            "Creating Object
      PERFORM create_fieldcat USING 'ZMMT0150'.  "Building the field catalog
      PERFORM fieldcat_layout USING 'ZMMT0150'.  "Building the fieldcat layout
      i_main_tab-subscreen = '9900'.
      PERFORM display_output USING it_saida_var_camb.      "Displaying data
    WHEN c_main_tab-tab11.
      PERFORM create_object USING 'CC_9901'.            "Creating Object
      PERFORM create_fieldcat USING 'ZSDT0336'.         "Building the field catalog
      PERFORM fieldcat_layout USING 'ZSDT0336'.         "Building the fieldcat layout
      i_main_tab-subscreen = '9901'.
      PERFORM display_output USING it_saida_isencao.    "Displaying data
    WHEN c_main_tab-tab12. "150184 CS2024000781 Aprovações ZNFW - PSA
      PERFORM create_object USING 'CC_9902'.            "Creating Object
      PERFORM create_fieldcat USING 'ZFIWRT0033'.         "Building the field catalog
      PERFORM fieldcat_layout USING 'ZFIWRT0033'.         "Building the fieldcat layout
      i_main_tab-subscreen = '9902'.
      PERFORM display_output USING it_saida_operznfw.    "Displaying data

      " 06.05.2025 - 174338 - RAMON -->
    WHEN c_main_tab-tab13. "150184 CS2024000781 Aprovações ZNFW - PSA
      PERFORM create_object USING 'CC_9903'.            "Creating Object
      PERFORM create_fieldcat USING 'ZSDT0385'.         "Building the field catalog
      PERFORM fieldcat_layout USING 'ZSDT0385'.         "Building the fieldcat layout
      i_main_tab-subscreen = '9903'.
      PERFORM display_output USING it_saida_checklist.    "Displaying data
      " 06.05.2025 - 174338 - RAMON --<

  ENDCASE.

  CREATE OBJECT event_handler.
  SET HANDLER event_handler->data_changed_finished FOR o_grid.

ENDMODULE.                                              " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       PAI
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE ok_code.

    WHEN 'SALVAR'.
*---> CS1005354 /  IR101904
      DATA: v_salvar(1) TYPE c.
      CLEAR v_salvar.
      IMPORT wa_trans-estrategia TO v_salvar FROM MEMORY ID 'waestrat'.
      FREE MEMORY ID 'waestrat'.

      IF v_salvar EQ '8'.
        PERFORM add_hora_novos_zimp.
        PERFORM consistencia_zimp.

        PERFORM add_hora_novos_zglt.
        PERFORM consistencia_zglt.

        PERFORM add_hora_novos_zinv.
        PERFORM consistencia_zinv.

        PERFORM add_hora_novos_zadto.
        PERFORM consistencia_zadto.

        PERFORM add_hora_novos_zov.
        PERFORM consistencia_zov.

        PERFORM add_hora_novos_lim.
        PERFORM consistencia_lim.

        PERFORM add_hora_novos_fre.
        PERFORM consistencia_zfre.

        PERFORM add_hora_novos_zsolov.
        PERFORM consistencia_zsolv.

        PERFORM add_hora_novos_var_camb.
        PERFORM consistencia_var_camb.

        PERFORM add_hora_novos_operznfw. "150184 CS2024000781 Aprovações ZNFW - PSA
        PERFORM consistencia_operznfw. "150184 CS2024000781 Aprovações ZNFW - PSA

        " 06.05.2025 - 174338 - RAMON -->
        PERFORM add_hora_novos_checklist.
        PERFORM consistencia_checklist.
        " 06.05.2025 - 174338 - RAMON --<


        EXPORT '0' TO MEMORY ID 'waestrat'.
        CLEAR ok_code.
        CLEAR v_salvar.
        PERFORM select_data.
        PERFORM sort_data.


      ELSE.
*<--- CS1005354 /  IR101904
        IF i_main_tab-pressed_tab = c_main_tab-tab1.
          PERFORM add_hora_novos_zimp.
          PERFORM consistencia_zimp.
        ELSEIF i_main_tab-pressed_tab = c_main_tab-tab2.
          PERFORM add_hora_novos_zglt.
          PERFORM consistencia_zglt.
        ELSEIF i_main_tab-pressed_tab = c_main_tab-tab3.
          PERFORM add_hora_novos_zinv.
          PERFORM consistencia_zinv.
        ELSEIF i_main_tab-pressed_tab = c_main_tab-tab4.
          PERFORM add_hora_novos_zadto.
          PERFORM consistencia_zadto.
        ELSEIF i_main_tab-pressed_tab = c_main_tab-tab5.
          PERFORM add_hora_novos_zov.
          PERFORM consistencia_zov.
        ELSEIF i_main_tab-pressed_tab = c_main_tab-tab6.
          PERFORM add_hora_novos_lim.
          PERFORM consistencia_lim.
        ELSEIF i_main_tab-pressed_tab = c_main_tab-tab7.
          PERFORM add_hora_novos_fre.
          PERFORM consistencia_zfre.
        ELSEIF i_main_tab-pressed_tab = c_main_tab-tab8.
          PERFORM add_hora_novos_zsolov.
          PERFORM consistencia_zsolv.
        ELSEIF i_main_tab-pressed_tab = c_main_tab-tab9.
          PERFORM add_hora_novos_var_camb.
          PERFORM consistencia_var_camb.
        ELSEIF i_main_tab-pressed_tab = c_main_tab-tab11.
          PERFORM add_hora_novos_isencao.
          PERFORM consistencia_isencao.
        ELSEIF i_main_tab-pressed_tab = c_main_tab-tab12. "150184 CS2024000781 Aprovações ZNFW - PSA
          PERFORM add_hora_novos_operznfw.
          PERFORM consistencia_operznfw.

          " 06.05.2025 - 174338 - RAMON -->
        ELSEIF i_main_tab-pressed_tab = c_main_tab-tab13.
          PERFORM add_hora_novos_checklist.
          PERFORM consistencia_checklist.
          " 06.05.2025 - 174338 - RAMON --<

        ENDIF.
      ENDIF.
      CLEAR ok_code.

    WHEN 'CANCEL' OR 'EXIT' OR 'BACK'.
      PERFORM free_objects.
      SET SCREEN '0'.
      CLEAR ok_code.

    WHEN  'CANCELAR'.
      IF it_saida_zimp IS NOT INITIAL
        OR it_saida_zglt IS NOT INITIAL
        OR it_saida_zadto IS NOT INITIAL
        OR it_saida_zinv IS NOT INITIAL
        OR it_saida_zov IS NOT INITIAL
        OR it_saida_lim IS NOT INITIAL
        OR it_saida_zfre IS NOT INITIAL
        OR it_saida_zsolov IS NOT INITIAL
        OR it_saida_var_camb IS NOT INITIAL
        OR it_saida_operznfw IS NOT INITIAL  "150184 CS2024000781 Aprovações ZNFW - PSA
        " 06.05.2025 - 174338 - RAMON -->
        OR it_saida_checklist IS NOT INITIAL.
        " 06.05.2025 - 174338 - RAMON --<

        PERFORM free_objects.
        PERFORM select_data.
        CLEAR ok_code.

      ENDIF.
    WHEN 'EXIBIR'.
      PERFORM free_objects.
      PERFORM select_data.
      PERFORM sort_data.
      CLEAR ok_code.

    WHEN 'TRANSF'.
      CLEAR: wa_trans.
      IF o_grid IS NOT INITIAL.
        CALL METHOD o_grid->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.
      ENDIF.
      CALL SCREEN 5005 STARTING AT 20 5.
      CLEAR ok_code.

    WHEN 'TERM'.

      PERFORM terminar_estrategia.
      CLEAR ok_code.




    WHEN c_main_tab-tab1 OR c_main_tab-tab2 OR c_main_tab-tab3 OR c_main_tab-tab4 OR c_main_tab-tab5 OR  c_main_tab-tab6 OR c_main_tab-tab7 OR c_main_tab-tab8
      OR c_main_tab-tab9 OR c_main_tab-tab11 OR c_main_tab-tab12 "150184 CS2024000781 Aprovações ZNFW - PSA
      OR c_main_tab-tab13." 06.05.2025 - 174338 - RAMON
      PERFORM free_objects.
  ENDCASE.



ENDMODULE.                                                  " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&  Module  MAIN_TAB_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
*   This is used to catch the pressed tab
*----------------------------------------------------------------------*
MODULE main_tab_active_tab_get INPUT.
  CASE ok_code.
    WHEN c_main_tab-tab1.
      i_main_tab-pressed_tab = c_main_tab-tab1.
      i_main_tab-subscreen = '9100'.
    WHEN c_main_tab-tab2.
      i_main_tab-pressed_tab = c_main_tab-tab2.
      i_main_tab-subscreen = '9200'.
    WHEN c_main_tab-tab3.
      i_main_tab-pressed_tab = c_main_tab-tab3.
      i_main_tab-subscreen = '9400'.
    WHEN c_main_tab-tab4.
      i_main_tab-pressed_tab = c_main_tab-tab4.
      i_main_tab-subscreen = '9300'.
    WHEN c_main_tab-tab5.
      i_main_tab-pressed_tab = c_main_tab-tab5.
      i_main_tab-subscreen = '9500'.
    WHEN c_main_tab-tab6.
      i_main_tab-pressed_tab = c_main_tab-tab6.
      i_main_tab-subscreen = '9600'.
    WHEN c_main_tab-tab7.
      i_main_tab-pressed_tab = c_main_tab-tab7.
      i_main_tab-subscreen = '9700'.
    WHEN c_main_tab-tab8.
      i_main_tab-pressed_tab = c_main_tab-tab8.
      i_main_tab-subscreen = '9800'.
    WHEN c_main_tab-tab9.
      i_main_tab-pressed_tab = c_main_tab-tab9.
      i_main_tab-subscreen = '9900'.
    WHEN c_main_tab-tab11.
      i_main_tab-pressed_tab = c_main_tab-tab11.
      i_main_tab-subscreen = '9901'.
    WHEN c_main_tab-tab12.
      i_main_tab-pressed_tab = c_main_tab-tab12. "150184 CS2024000781 Aprovações ZNFW - PSA
      i_main_tab-subscreen = '9902'.

      " 06.05.2025 - 174338 - RAMON -->
    WHEN c_main_tab-tab13.
      i_main_tab-pressed_tab = c_main_tab-tab13.
      i_main_tab-subscreen = '9903'.
      " 06.05.2025 - 174338 - RAMON --<

    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.
ENDMODULE.

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD data_changed_finished.

    PERFORM preenche_data.
    wa_stable-row = abap_true.
    wa_stable-col = abap_true.

    CALL METHOD o_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_data .
  IF v_salvar EQ '8'.
    EXIT.
  ENDIF.
  LOOP AT p_bukrs.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_bukrs-low
      IMPORTING
        output = p_bukrs-low.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_bukrs-high
      IMPORTING
        output = p_bukrs-high.

    MODIFY p_bukrs.

  ENDLOOP.

  LOOP AT  p_depto.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_depto-low
      IMPORTING
        output = p_depto-low.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_depto-high
      IMPORTING
        output = p_depto-high.

    MODIFY p_depto.

  ENDLOOP.

  "seleção da zimp
  IF p_bukrs-low IS INITIAL.
    SELECT * FROM zimp_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_zimp
      WHERE dep_resp  IN p_depto
      AND aprovador   IN p_aprov
      AND waers       IN p_moeda
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ELSE.
    SELECT * FROM zimp_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_zimp
      WHERE dep_resp  IN p_depto
      AND aprovador   IN p_aprov
      AND waers       IN p_moeda
      AND ( ( bukrs LE p_bukrs-low AND bukrs_ate GE p_bukrs-low ) OR ( bukrs_ate LE p_bukrs-high ) )
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ENDIF.
  DELETE it_saida_zimp WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  "seleção da zglt
  IF p_bukrs IS INITIAL.
    SELECT * FROM zglt037 INTO CORRESPONDING FIELDS OF TABLE it_saida_zglt
      WHERE dep_resp IN p_depto
      AND aprovador IN p_aprov
      AND waers IN p_moeda
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel
      AND pgt_forn    IN p_forn.
  ELSE.
    SELECT * FROM zglt037 INTO CORRESPONDING FIELDS OF TABLE it_saida_zglt
      WHERE dep_resp IN p_depto
      AND aprovador IN p_aprov
      AND waers IN p_moeda
      AND ( ( bukrs LE p_bukrs-low AND bukrs_ate GE p_bukrs-low ) OR ( bukrs_ate LE p_bukrs-high ) )
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel
      AND pgt_forn    IN p_forn.
  ENDIF.
  DELETE it_saida_zglt WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  "seleção da zov
  IF p_bukrs IS INITIAL.
    SELECT * FROM zsdt0141 INTO CORRESPONDING FIELDS OF TABLE it_saida_zov
    WHERE aprovador IN p_aprov
      AND waers IN p_moeda
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ELSE.
    SELECT * FROM zsdt0141 INTO CORRESPONDING FIELDS OF TABLE it_saida_zov
      WHERE aprovador IN p_aprov
      AND waers IN p_moeda
      AND ( ( bukrs LE p_bukrs-low AND bukrs_ate GE p_bukrs-low ) OR ( bukrs_ate LE p_bukrs-high ) )
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ENDIF.
  DELETE it_saida_zov WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  "seleção da limite lim ALRS
  SELECT * FROM zsdt0152 INTO CORRESPONDING FIELDS OF TABLE it_saida_lim
  WHERE aprovador IN p_aprov
    AND dt_val_de   LE sy-datum
    AND dt_val_ate  GE sy-datum
    AND nivel       IN p_nivel.

  DELETE it_saida_lim WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.


  "seleção da zadto
  IF p_bukrs IS INITIAL.
    SELECT * FROM zadto_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_zadto
      WHERE dep_resp IN p_depto
      AND aprovador IN p_aprov
      AND waers IN p_moeda
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ELSE.
    SELECT * FROM zadto_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_zadto
      WHERE dep_resp IN p_depto
      AND aprovador IN p_aprov
      AND waers IN p_moeda
      AND ( ( bukrs LE p_bukrs-low AND bukrs_ate GE p_bukrs-low ) OR ( bukrs_ate LE p_bukrs-high ) )
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ENDIF.

  DELETE it_saida_zadto WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  "seleção da zinv
  IF p_bukrs IS INITIAL.
    SELECT * FROM zinv_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_zinv
      WHERE aprovador IN p_aprov
      AND waers IN p_moeda
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND matnr IN p_matnr
      AND tp_operacao IN p_opera
      AND tipo IN p_tppgt
      AND nivel       IN p_nivel.
  ELSE.
    SELECT * FROM zinv_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_zinv
      WHERE aprovador IN p_aprov
      AND waers IN p_moeda
      AND ( ( bukrs LE p_bukrs-low AND bukrs_ate GE p_bukrs-low ) OR ( bukrs_ate LE p_bukrs-high ) )
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND matnr IN p_matnr
      AND tp_operacao IN p_opera
      AND tipo IN p_tppgt
      AND nivel       IN p_nivel.
  ENDIF.

  DELETE it_saida_zinv WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  "seleção da zfre
  IF p_bukrs IS INITIAL.
    SELECT * FROM zlest0156 INTO CORRESPONDING FIELDS OF TABLE it_saida_zfre
      WHERE aprovador IN p_aprov
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ELSE.
    SELECT * FROM zlest0156 INTO CORRESPONDING FIELDS OF TABLE it_saida_zfre
      WHERE aprovador IN p_aprov
      AND ( ( bukrs LE p_bukrs-low AND bukrs_ate GE p_bukrs-low ) OR ( bukrs_ate LE p_bukrs-high ) )
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ENDIF.
  DELETE it_saida_zfre WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  "seleção da ZSOLOV
  IF p_bukrs IS INITIAL.
    SELECT * FROM zsdt0161 INTO CORRESPONDING FIELDS OF TABLE it_saida_zsolov
      WHERE aprovador IN p_aprov
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ELSE.
    SELECT * FROM zsdt0161 INTO CORRESPONDING FIELDS OF TABLE it_saida_zsolov
      WHERE aprovador IN p_aprov
      AND ( ( bukrs LE p_bukrs-low AND bukrs_ate GE p_bukrs-low ) OR ( bukrs_ate LE p_bukrs-high ) )
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ENDIF.

  DELETE it_saida_zsolov WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  "seleção da Varição Cambial
  IF p_bukrs IS INITIAL.
    SELECT * FROM zmmt0150 INTO CORRESPONDING FIELDS OF TABLE it_saida_var_camb
      WHERE aprovador IN p_aprov
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ELSE.
    SELECT * FROM zmmt0150 INTO CORRESPONDING FIELDS OF TABLE it_saida_var_camb
      WHERE aprovador IN p_aprov
      AND ( ( bukrs LE p_bukrs-low AND bukrs_ate GE p_bukrs-low ) OR ( bukrs_ate LE p_bukrs-high ) )
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ENDIF.
  DELETE it_saida_var_camb WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  "seleção da ZSOLOV
  IF p_bukrs IS INITIAL.
    SELECT * FROM zsdt0336 INTO CORRESPONDING FIELDS OF TABLE it_saida_isencao
      WHERE aprovador IN p_aprov
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ELSE.
    SELECT * FROM zsdt0336 INTO CORRESPONDING FIELDS OF TABLE it_saida_isencao
      WHERE aprovador IN p_aprov
      AND ( ( bukrs LE p_bukrs-low AND bukrs_ate GE p_bukrs-low ) OR ( bukrs_ate LE p_bukrs-high ) )
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ENDIF.

  DELETE it_saida_isencao WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  "150184 CS2024000781 Aprovações ZNFW - PSA / Começo da Seleção
  IF p_depto IS INITIAL.
    SELECT * FROM zfiwrt0033 INTO CORRESPONDING FIELDS OF TABLE it_saida_operznfw
      WHERE aprovador IN p_aprov
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ELSE.
    SELECT * FROM zfiwrt0033 INTO CORRESPONDING FIELDS OF TABLE it_saida_operznfw
      WHERE aprovador IN p_aprov
      AND dt_val_de   LE sy-datum
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.

  ENDIF.

  DELETE it_saida_operznfw WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  " 07.05.2025 - 174338 - RAMON -->
  IF p_bukrs IS INITIAL.
    SELECT * FROM zsdt0385 INTO CORRESPONDING FIELDS OF TABLE it_saida_checklist
      WHERE aprovador IN p_aprov
      AND ( ( dt_val_de LE sy-datum ) OR ( dt_val_de GE sy-datum AND transf_aprov EQ 'D' ) )
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ELSE.
    SELECT * FROM zsdt0385 INTO CORRESPONDING FIELDS OF TABLE it_saida_checklist
      WHERE aprovador IN p_aprov
      AND ( ( bukrs LE p_bukrs-low AND bukrs_ate GE p_bukrs-low ) OR ( bukrs_ate LE p_bukrs-high ) )
      AND ( ( dt_val_de LE sy-datum ) OR ( dt_val_de GE sy-datum AND transf_aprov EQ 'D' ) )
      AND dt_val_ate  GE sy-datum
      AND nivel       IN p_nivel.
  ENDIF.

  DELETE it_saida_checklist WHERE dt_val_ate <= sy-datum AND hr_val_ate <= sy-uzeit.

  " 07.05.2025 - 174338 - RAMON --<


  PERFORM marcar_antigos.
  PERFORM edit_alv_zimp.    "/Torna os campos da ALV não modificáveis, desta forma apenas linhas novas terão campos modificáveis
  PERFORM edit_alv_zglt.    "/Torna os campos da ALV não modificáveis, desta forma apenas linhas novas terão campos modificáveis
  PERFORM edit_alv_zov.     "/Torna os campos da ALV não modificáveis, desta forma apenas linhas novas terão campos modificáveis
  PERFORM edit_alv_zadto.   "/Torna os campos da ALV não modificáveis, desta forma apenas linhas novas terão campos modificáveis
  PERFORM edit_alv_zinv.    "/Torna os campos da ALV não modificáveis, desta forma apenas linhas novas terão campos modificáveis
  PERFORM edit_alv_zfre.    "/Torna os campos da ALV não modificáveis, desta forma apenas linhas novas terão campos modificáveis
  PERFORM edit_alv_zsolov.  "/Torna os campos da ALV não modificáveis, desta forma apenas linhas novas terão campos modificáveis
  PERFORM edit_alv_lim.     "/Torna os campos da ALV não modificáveis, desta forma apenas linhas novas terão campos modificáveis
  PERFORM edit_alv_var_camb."/Torna os campos da ALV não modificáveis, desta forma apenas linhas novas terão campos modificáveis
  PERFORM edit_alv_izencao.
  PERFORM edit_alv_operznfw. "150184 CS2024000781 Aprovações ZNFW - PSA

  " 06.05.2025 - 174338 - RAMON -->
  PERFORM edit_alv_checklist.
  " 06.05.2025 - 174338 - RAMON --<

  PERFORM select_complementar.
  PERFORM preenche_data.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM preenche_data .


  DATA: it_zimp_cad_depto TYPE STANDARD TABLE OF zimp_cad_depto,
        it_tvbur          TYPE STANDARD TABLE OF tvbur,
        it_usr21          TYPE STANDARD TABLE OF usr21,
        it_adrp           TYPE STANDARD TABLE OF adrp,
        it_zfit0043       TYPE STANDARD TABLE OF zfit0043,
        it_makt           TYPE STANDARD TABLE OF makt,
        it_zlest0156      TYPE STANDARD TABLE OF zlest0156,
        wa_zimp_cad_depto TYPE zimp_cad_depto,
        wa_usr21          TYPE usr21,
        wa_adrp           TYPE adrp,
        wa_zfit0043       TYPE zfit0043,
        wa_makt           TYPE makt,
        wa_zlest0156      TYPE zlest0156.

  FIELD-SYMBOLS: <wa_zimp_aprovador>  TYPE t_zimp_aprovador,
                 <wa_zglt037>         TYPE t_zglt037,
                 <wa_zsdt0141>        TYPE t_zsdt0141,
                 <wa_zsdt0152>        TYPE t_zsdt0152,
                 <wa_zadto_aprovador> TYPE t_zadto_aprovador,
                 <wa_zinv_aprovador>  TYPE t_zinv_aprovador,
                 <wa_zlest0156>       TYPE t_zlest0156,
                 <wa_zsdt0161>        TYPE t_zsdt0161,
                 <wa_zmmt0150>        TYPE t_zmmt0150,
                 <wa_zsdt0336>        TYPE t_zsdt0336,
                 <wa_zfiwrt0033>      TYPE t_zfiwrt0033, "150184 CS2024000781 Aprovações ZNFW - PSA
                 <wa_zsdt0385>        TYPE t_zsdt0385.

  IF                   it_saida_zimp IS NOT INITIAL.

    SELECT * FROM zimp_cad_depto INTO TABLE it_zimp_cad_depto
      FOR ALL ENTRIES IN it_saida_zimp
      WHERE dep_resp = it_saida_zimp-dep_resp.

    SELECT * FROM usr21 INTO TABLE it_usr21
      FOR ALL ENTRIES IN it_saida_zimp
      WHERE bname = it_saida_zimp-aprovador.

    IF it_usr21 IS NOT INITIAL.

      SELECT * FROM adrp INTO TABLE it_adrp
        FOR ALL ENTRIES IN it_usr21
        WHERE persnumber = it_usr21-persnumber.

    ENDIF.

  ENDIF.

  LOOP AT it_saida_zimp ASSIGNING <wa_zimp_aprovador>.
    "ADICIONA DESCRIÇÃO DO DEPARTAMENTO NA TABELA INTERNA
    READ TABLE it_zimp_cad_depto INTO wa_zimp_cad_depto WITH KEY dep_resp = <wa_zimp_aprovador>-dep_resp.
    IF sy-subrc EQ 0.
      <wa_zimp_aprovador>-dep_resp_desc = wa_zimp_cad_depto-dep_resp_desc.
    ELSE.
      CLEAR <wa_zimp_aprovador>-dep_resp_desc.
    ENDIF.
    "ADICIONA NOME E SOBRENOME DO APROVADOR NA TABELA INTERNA
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = <wa_zimp_aprovador>-aprovador.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        <wa_zimp_aprovador>-name = wa_adrp-name_first && '&' && wa_adrp-name_last.
        REPLACE '&' WITH space INTO <wa_zimp_aprovador>-name.
      ELSE.
        CLEAR <wa_zimp_aprovador>-name.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR:  it_zimp_cad_depto,
          it_usr21,
          it_adrp,
          wa_zimp_cad_depto,
          wa_usr21,
          wa_adrp.

  IF it_saida_zglt IS NOT INITIAL.

    SELECT * FROM zimp_cad_depto INTO TABLE it_zimp_cad_depto
      FOR ALL ENTRIES IN it_saida_zglt
      WHERE dep_resp = it_saida_zglt-dep_resp.

    SELECT * FROM usr21 INTO TABLE it_usr21
      FOR ALL ENTRIES IN it_saida_zglt
      WHERE bname = it_saida_zglt-aprovador.

    IF it_usr21 IS NOT INITIAL.

      SELECT * FROM adrp INTO TABLE it_adrp
        FOR ALL ENTRIES IN it_usr21
        WHERE persnumber = it_usr21-persnumber.

    ENDIF.

  ENDIF.

  LOOP AT it_saida_zglt ASSIGNING <wa_zglt037>.
    "ADICIONA DESCRIÇÃO DO DEPARTAMENTO NA TABELA INTERNA
    READ TABLE it_zimp_cad_depto INTO wa_zimp_cad_depto WITH KEY dep_resp = <wa_zglt037>-dep_resp.
    IF sy-subrc EQ 0.
      <wa_zglt037>-dep_resp_desc = wa_zimp_cad_depto-dep_resp_desc.
    ELSE.
      CLEAR <wa_zglt037>-dep_resp_desc.
    ENDIF.
    "ADICIONA NOME E SOBRENOME DO APROVADOR NA TABELA INTERNA
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = <wa_zglt037>-aprovador.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        <wa_zglt037>-name = wa_adrp-name_first && '&' && wa_adrp-name_last.
        REPLACE '&' WITH space INTO <wa_zglt037>-name.
      ELSE.
        CLEAR <wa_zglt037>-name.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR:  it_zimp_cad_depto,
          it_usr21,
          it_adrp,
          wa_zimp_cad_depto,
          wa_usr21,
          wa_adrp.

  IF it_saida_zov IS NOT INITIAL.

    SELECT * FROM usr21 INTO TABLE it_usr21
      FOR ALL ENTRIES IN it_saida_zov
      WHERE bname = it_saida_zov-aprovador.

    IF it_usr21 IS NOT INITIAL.

      SELECT * FROM adrp INTO TABLE it_adrp
        FOR ALL ENTRIES IN it_usr21
        WHERE persnumber = it_usr21-persnumber.

    ENDIF.

  ENDIF.

  LOOP AT it_saida_zov ASSIGNING <wa_zsdt0141>.
    "ADICIONA NOME E SOBRENOME DO APROVADOR NA TABELA INTERNA
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = <wa_zsdt0141>-aprovador.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        <wa_zsdt0141>-name = wa_adrp-name_first && '&' && wa_adrp-name_last.
        REPLACE '&' WITH space INTO <wa_zsdt0141>-name.
      ELSE.
        CLEAR <wa_zsdt0141>-name.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "ALRS
  IF it_saida_lim IS NOT INITIAL.

    SELECT * FROM usr21 INTO TABLE it_usr21
      FOR ALL ENTRIES IN it_saida_lim
      WHERE bname = it_saida_lim-aprovador.

    IF it_usr21 IS NOT INITIAL.

      SELECT * FROM adrp INTO TABLE it_adrp
        FOR ALL ENTRIES IN it_usr21
        WHERE persnumber = it_usr21-persnumber.

    ENDIF.

  ENDIF.

  LOOP AT it_saida_lim ASSIGNING <wa_zsdt0152>.
    "ADICIONA NOME E SOBRENOME DO APROVADOR NA TABELA INTERNA
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = <wa_zsdt0152>-aprovador.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        <wa_zsdt0152>-name = wa_adrp-name_first && '&' && wa_adrp-name_last.
        REPLACE '&' WITH space INTO <wa_zsdt0152>-name.
      ELSE.
        CLEAR <wa_zsdt0152>-name.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR:  it_usr21,
          it_adrp,
          wa_usr21,
          wa_adrp.

  IF it_saida_zadto IS NOT INITIAL.

    SELECT * FROM zimp_cad_depto INTO TABLE it_zimp_cad_depto
      FOR ALL ENTRIES IN it_saida_zadto
      WHERE dep_resp = it_saida_zadto-dep_resp.

    SELECT * FROM usr21 INTO TABLE it_usr21
      FOR ALL ENTRIES IN it_saida_zadto
      WHERE bname = it_saida_zadto-aprovador.

    IF it_usr21 IS NOT INITIAL.

      SELECT * FROM adrp INTO TABLE it_adrp
        FOR ALL ENTRIES IN it_usr21
        WHERE persnumber = it_usr21-persnumber.

    ENDIF.

  ENDIF.

  LOOP AT it_saida_zadto ASSIGNING <wa_zadto_aprovador>.
    "ADICIONA DESCRIÇÃO DO DEPARTAMENTO NA TABELA INTERNA
    READ TABLE it_zimp_cad_depto INTO wa_zimp_cad_depto WITH KEY dep_resp = <wa_zadto_aprovador>-dep_resp.
    IF sy-subrc EQ 0.
      <wa_zadto_aprovador>-dep_resp_desc = wa_zimp_cad_depto-dep_resp_desc.
    ELSE.
      CLEAR <wa_zadto_aprovador>-dep_resp_desc.
    ENDIF.
    "ADICIONA NOME E SOBRENOME DO APROVADOR NA TABELA INTERNA
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = <wa_zadto_aprovador>-aprovador.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        <wa_zadto_aprovador>-name = wa_adrp-name_first && '&' && wa_adrp-name_last.
        REPLACE '&' WITH space INTO <wa_zadto_aprovador>-name.
      ELSE.
        CLEAR <wa_zadto_aprovador>-name.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR:  it_zimp_cad_depto,
          it_usr21,
          it_adrp,
          wa_zimp_cad_depto,
          wa_usr21,
          wa_adrp.

  IF it_saida_zinv IS NOT INITIAL.

    SELECT * FROM makt INTO TABLE it_makt
      FOR ALL ENTRIES IN it_saida_zinv
      WHERE matnr = it_saida_zinv-matnr.

    SELECT * FROM   zfit0043 INTO TABLE it_zfit0043
      FOR ALL ENTRIES IN it_saida_zinv
      WHERE tp_operacao = it_saida_zinv-tp_operacao
      AND spras = sy-langu.

    SELECT * FROM usr21 INTO TABLE it_usr21
      FOR ALL ENTRIES IN it_saida_zinv
      WHERE bname = it_saida_zinv-aprovador.

    IF it_usr21 IS NOT INITIAL.

      SELECT * FROM adrp INTO TABLE it_adrp
        FOR ALL ENTRIES IN it_usr21
        WHERE persnumber = it_usr21-persnumber.

    ENDIF.

  ENDIF.

  LOOP AT it_saida_zinv ASSIGNING <wa_zinv_aprovador>.
    "ADICIONA DESCRIÇÃO DA OPERAÇÃO NA TABELA INTERNA
    READ TABLE it_zfit0043 INTO wa_zfit0043 WITH KEY tp_operacao = <wa_zinv_aprovador>-tp_operacao.
    IF sy-subrc EQ 0.
      <wa_zinv_aprovador>-ds_operacao = wa_zfit0043-ds_operacao.
    ELSE.
      CLEAR <wa_zinv_aprovador>-ds_operacao.
    ENDIF.
    "ADICIONA NOME E SOBRENOME DO APROVADOR NA TABELA INTERNA
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = <wa_zinv_aprovador>-aprovador.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        <wa_zinv_aprovador>-name = wa_adrp-name_first && '&' && wa_adrp-name_last.
        REPLACE '&' WITH space INTO <wa_zinv_aprovador>-name.
      ELSE.
        CLEAR <wa_zinv_aprovador>-name.
      ENDIF.
    ENDIF.
    "ADICIONA O TIPO DE PAGAMENTO NA TABELA INTERNA
    IF <wa_zinv_aprovador>-tipo = '01'.
      <wa_zinv_aprovador>-tipo_desc = 'INVOICE - Terceiros'.
    ELSEIF <wa_zinv_aprovador>-tipo = '02'.
      <wa_zinv_aprovador>-tipo_desc = 'Performance'.
    ELSEIF <wa_zinv_aprovador>-tipo = '03'.
      <wa_zinv_aprovador>-tipo_desc = 'Adiantamento'.
    ELSEIF <wa_zinv_aprovador>-tipo = '04'.
      <wa_zinv_aprovador>-tipo_desc = 'INVOICE - Grupo'.
    ELSE.
      <wa_zinv_aprovador>-tipo_desc = ''.
    ENDIF.
    "ADICIONA O NOME DO MATERIAL NA TABLE INTERNA
    READ TABLE it_makt INTO wa_makt WITH KEY matnr = <wa_zinv_aprovador>-matnr.
    IF sy-subrc EQ 0.
      <wa_zinv_aprovador>-maktx = wa_makt-maktx.
    ELSE.
      CLEAR <wa_zinv_aprovador>-maktx.
    ENDIF.
  ENDLOOP.

  IF it_saida_zfre IS NOT INITIAL.

    SELECT * FROM usr21 INTO TABLE it_usr21
      FOR ALL ENTRIES IN it_saida_zfre
      WHERE bname = it_saida_zfre-aprovador.

    IF it_usr21 IS NOT INITIAL.

      SELECT * FROM adrp INTO TABLE it_adrp
        FOR ALL ENTRIES IN it_usr21
        WHERE persnumber = it_usr21-persnumber.

    ENDIF.

  ENDIF.

  LOOP AT it_saida_zfre ASSIGNING <wa_zlest0156>.
    "ADICIONA NOME E SOBRENOME DO APROVADOR NA TABELA INTERNA
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = <wa_zlest0156>-aprovador.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        <wa_zlest0156>-name = wa_adrp-name_first && '&' && wa_adrp-name_last.
        REPLACE '&' WITH space INTO <wa_zlest0156>-name.
      ELSE.
        CLEAR <wa_zlest0156>-name.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR:  it_usr21,
          it_adrp,
          wa_usr21,
          wa_adrp.

  IF it_saida_zsolov IS NOT INITIAL.

    SELECT * FROM usr21 INTO TABLE it_usr21
      FOR ALL ENTRIES IN it_saida_zsolov
      WHERE bname = it_saida_zsolov-aprovador.

    IF it_usr21 IS NOT INITIAL.

      SELECT * FROM adrp INTO TABLE it_adrp
        FOR ALL ENTRIES IN it_usr21
        WHERE persnumber = it_usr21-persnumber.

    ENDIF.

  ENDIF.

  LOOP AT it_saida_zsolov ASSIGNING <wa_zsdt0161>.
    "ADICIONA NOME E SOBRENOME DO APROVADOR NA TABELA INTERNA
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = <wa_zsdt0161>-aprovador.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        <wa_zsdt0161>-name = wa_adrp-name_first && '&' && wa_adrp-name_last.
        REPLACE '&' WITH space INTO <wa_zsdt0161>-name.
      ELSE.
        CLEAR <wa_zsdt0161>-name.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR:  it_usr21,
          it_adrp,
          wa_usr21,
          wa_adrp.

  IF it_saida_var_camb IS NOT INITIAL.

    SELECT * FROM usr21 INTO TABLE it_usr21
      FOR ALL ENTRIES IN it_saida_var_camb
      WHERE bname = it_saida_var_camb-aprovador.

    IF it_usr21 IS NOT INITIAL.

      SELECT * FROM adrp INTO TABLE it_adrp
        FOR ALL ENTRIES IN it_usr21
        WHERE persnumber = it_usr21-persnumber.

    ENDIF.

  ENDIF.

  LOOP AT it_saida_var_camb ASSIGNING <wa_zmmt0150>.
    "ADICIONA NOME E SOBRENOME DO APROVADOR NA TABELA INTERNA
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = <wa_zmmt0150>-aprovador.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        <wa_zmmt0150>-name = wa_adrp-name_first && '&' && wa_adrp-name_last.
        REPLACE '&' WITH space INTO <wa_zmmt0150>-name.
      ELSE.
        CLEAR <wa_zmmt0150>-name.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR:  it_usr21,
          it_adrp,
          wa_usr21,
          wa_adrp.

  IF it_saida_isencao IS NOT INITIAL.

    SELECT * FROM usr21 INTO TABLE it_usr21
      FOR ALL ENTRIES IN it_saida_isencao
      WHERE bname = it_saida_isencao-aprovador.

    IF it_usr21 IS NOT INITIAL.

      SELECT * FROM adrp INTO TABLE it_adrp
        FOR ALL ENTRIES IN it_usr21
        WHERE persnumber = it_usr21-persnumber.

    ENDIF.

  ENDIF.

  LOOP AT it_saida_isencao ASSIGNING <wa_zsdt0336>.
    "ADICIONA NOME E SOBRENOME DO APROVADOR NA TABELA INTERNA
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = <wa_zsdt0336>-aprovador.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        <wa_zsdt0336>-name = wa_adrp-name_first && '&' && wa_adrp-name_last.
        REPLACE '&' WITH space INTO <wa_zsdt0336>-name.
      ELSE.
        CLEAR <wa_zsdt0336>-name.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR:  it_usr21,
          it_adrp,
          wa_usr21,
          wa_adrp.

  IF it_saida_operznfw IS NOT INITIAL. "150184 CS2024000781 Aprovações ZNFW - PSA

    SELECT * FROM usr21 INTO TABLE it_usr21
      FOR ALL ENTRIES IN it_saida_operznfw
      WHERE bname = it_saida_operznfw-aprovador.

    IF it_usr21 IS NOT INITIAL.

      SELECT * FROM adrp INTO TABLE it_adrp
        FOR ALL ENTRIES IN it_usr21
        WHERE persnumber = it_usr21-persnumber.

    ENDIF.

  ENDIF.

  LOOP AT it_saida_operznfw ASSIGNING <wa_zfiwrt0033>. "150184 CS2024000781 Aprovações ZNFW - PSA
    "ADICIONA NOME E SOBRENOME DO APROVADOR NA TABELA INTERNA
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = <wa_zfiwrt0033>-aprovador.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        <wa_zfiwrt0033>-name = wa_adrp-name_first && '&' && wa_adrp-name_last.
        REPLACE '&' WITH space INTO <wa_zfiwrt0033>-name.
      ELSE.
        CLEAR <wa_zfiwrt0033>-name.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR:  it_usr21,
          it_adrp,
          wa_usr21,
          wa_adrp.

  " 07.05.2025 - 174338 - RAMON -->
  IF it_saida_checklist IS NOT INITIAL.

    SELECT * FROM usr21 INTO TABLE it_usr21
      FOR ALL ENTRIES IN it_saida_checklist
      WHERE bname = it_saida_checklist-aprovador.

    IF it_usr21 IS NOT INITIAL.

      SELECT * FROM adrp INTO TABLE it_adrp
        FOR ALL ENTRIES IN it_usr21
        WHERE persnumber = it_usr21-persnumber.

    ENDIF.

  ENDIF.

  LOOP AT it_saida_checklist ASSIGNING <wa_zsdt0385>.
    "ADICIONA NOME E SOBRENOME DO APROVADOR NA TABELA INTERNA
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = <wa_zsdt0385>-aprovador.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        <wa_zsdt0385>-name = wa_adrp-name_first && '&' && wa_adrp-name_last.
        REPLACE '&' WITH space INTO <wa_zsdt0385>-name.
      ELSE.
        CLEAR <wa_zsdt0385>-name.
      ENDIF.

    ENDIF.

  ENDLOOP.

  CLEAR:  it_usr21,
          it_adrp,
          wa_usr21,
          wa_adrp.
  " 07.05.2025 - 174338 - RAMON --<


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SOTR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sort_data .

  SORT it_saida_zimp    BY bukrs       ASCENDING
                           bukrs_ate   ASCENDING
                           dep_resp    ASCENDING
                           waers       ASCENDING
                           nivel       ASCENDING.

  SORT it_saida_zglt    BY bukrs       ASCENDING
                           bukrs_ate   ASCENDING
                           dep_resp    ASCENDING
                           pgt_forn    ASCENDING
                           waers       ASCENDING
                           nivel       ASCENDING.

  SORT it_saida_zov     BY bukrs       ASCENDING
                           bukrs_ate   ASCENDING
                           vkbur       ASCENDING
                           vkbur_ate   ASCENDING
                           waers       ASCENDING
                           nivel       ASCENDING.

  SORT it_saida_zadto   BY bukrs      ASCENDING
                          bukrs_ate   ASCENDING
                          dep_resp    ASCENDING
                          waers       ASCENDING
                          nivel       ASCENDING.

  SORT it_saida_zinv    BY bukrs      ASCENDING
                          bukrs_ate   ASCENDING
                          tipo        ASCENDING
                          waers       ASCENDING
                          tp_operacao ASCENDING
                          matnr       ASCENDING
                          nivel       ASCENDING.

  SORT it_saida_lim     BY vkorg      ASCENDING
                          werks       ASCENDING
                          nivel       ASCENDING.

  SORT it_saida_zfre    BY bukrs      ASCENDING
                           bukrs_ate  ASCENDING
                           nivel      ASCENDING.

  SORT it_saida_zsolov  BY bukrs        ASCENDING
                           bukrs_ate    ASCENDING
                           vkbur        ASCENDING
                           vkbur_ate    ASCENDING
                           tp_venda     ASCENDING
                           tp_venda_ate ASCENDING
                           nivel        ASCENDING.

  SORT it_saida_var_camb  BY bukrs        ASCENDING
                             bukrs_ate    ASCENDING
                             nivel        ASCENDING.

  SORT it_saida_isencao BY bukrs          ASCENDING
                           bukrs_ate      ASCENDING
                           vkbur          ASCENDING
                           vkbur_ate      ASCENDING
                           tp_negocio_de  ASCENDING
                           tp_negocio_ate ASCENDING
                           nivel          ASCENDING.

  SORT it_saida_operznfw BY dep_resp     ASCENDING "150184 CS2024000781 Aprovações ZNFW - PSA
                         nivel          ASCENDING.

  " 06.05.2025 - 174338 - RAMON -->
  SORT it_saida_checklist BY bukrs     ASCENDING
                             bukrs_ate ASCENDING
                             vkbur     ASCENDING
                             vkbur_ate ASCENDING
                             nivel     ASCENDING.
  " 06.05.2025 - 174338 - RAMON --<

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECT_COMPLEMENTAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_complementar .

  "seleção complementar da zimp
  SELECT * FROM zimp_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_zimp_c.
  DELETE it_saida_zimp_c WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  LOOP AT it_saida_zimp_c INTO wa_saida_zimp_c.
    READ TABLE it_saida_zimp INTO wa_saida_zimp WITH KEY bukrs      = wa_saida_zimp_c-bukrs
                                                         bukrs_ate  = wa_saida_zimp_c-bukrs_ate
                                                         dep_resp   = wa_saida_zimp_c-dep_resp
                                                         waers      = wa_saida_zimp_c-waers
                                                         nivel      = wa_saida_zimp_c-nivel
                                                         aprovador  = wa_saida_zimp_c-aprovador
                                                         dt_val_de  = wa_saida_zimp_c-dt_val_de
                                                         dt_val_ate = wa_saida_zimp_c-dt_val_ate.

    IF sy-subrc EQ 0.
      DELETE TABLE it_saida_zimp_c FROM wa_saida_zimp_c.
    ENDIF.
    CLEAR: wa_saida_zimp.
  ENDLOOP.

  "seleção complementar da zglt
  SELECT * FROM zglt037 INTO CORRESPONDING FIELDS OF TABLE it_saida_zglt_c.
  DELETE it_saida_zglt_c WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.
  LOOP AT it_saida_zglt_c INTO wa_saida_zglt_c.
    READ TABLE it_saida_zglt INTO wa_saida_zglt WITH KEY bukrs      = wa_saida_zglt_c-bukrs
                                                         bukrs_ate  = wa_saida_zglt_c-bukrs_ate
                                                         dep_resp   = wa_saida_zglt_c-dep_resp
                                                         pgt_forn   = wa_saida_zglt_c-pgt_forn
                                                         waers      = wa_saida_zglt_c-waers
                                                         nivel      = wa_saida_zglt_c-nivel
                                                         aprovador  = wa_saida_zglt_c-aprovador
                                                         dt_val_de  = wa_saida_zglt_c-dt_val_de
                                                         dt_val_ate = wa_saida_zglt_c-dt_val_ate.

    IF sy-subrc EQ 0.
      DELETE TABLE it_saida_zglt_c FROM wa_saida_zglt_c.
    ENDIF.
    CLEAR: wa_saida_zglt.
  ENDLOOP.

  "seleção complementar da zov
  SELECT * FROM zsdt0141 INTO CORRESPONDING FIELDS OF TABLE it_saida_zov_c.

  LOOP AT it_saida_zov_c INTO wa_saida_zov_c.
    READ TABLE it_saida_zov INTO wa_saida_zov WITH KEY bukrs      = wa_saida_zov_c-bukrs
                                                       bukrs_ate  = wa_saida_zov_c-bukrs_ate
                                                       vkbur      = wa_saida_zov_c-vkbur
                                                       vkbur_ate  = wa_saida_zov_c-vkbur_ate
                                                       waers      = wa_saida_zov_c-waers
                                                       nivel      = wa_saida_zov_c-nivel
                                                       aprovador  = wa_saida_zov_c-aprovador
                                                       dt_val_de  = wa_saida_zov_c-dt_val_de
                                                       dt_val_ate = wa_saida_zov_c-dt_val_ate.

    IF sy-subrc EQ 0.
      DELETE TABLE it_saida_zov_c FROM wa_saida_zov_c.
    ENDIF.
    CLEAR: wa_saida_zov.
  ENDLOOP.

  "seleção complementar da zadto
  SELECT * FROM zadto_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_zadto_c.
  DELETE it_saida_zadto_c WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.
  "
  LOOP AT it_saida_zadto_c INTO wa_saida_zadto_c.
    READ TABLE it_saida_zadto INTO wa_saida_zadto WITH KEY bukrs      = wa_saida_zadto_c-bukrs
                                                         bukrs_ate  = wa_saida_zadto_c-bukrs_ate
                                                         dep_resp   = wa_saida_zadto_c-dep_resp
                                                         waers      = wa_saida_zadto_c-waers
                                                         nivel      = wa_saida_zadto_c-nivel
                                                         aprovador  = wa_saida_zadto_c-aprovador
                                                         dt_val_de  = wa_saida_zadto_c-dt_val_de
                                                         dt_val_ate = wa_saida_zadto_c-dt_val_ate.

    IF sy-subrc EQ 0.
      DELETE TABLE it_saida_zadto_c FROM wa_saida_zadto_c.
    ENDIF.
    CLEAR: wa_saida_zadto.
  ENDLOOP.
  "seleção complementar da zinv
  SELECT * FROM zinv_aprovador INTO CORRESPONDING FIELDS OF TABLE it_saida_zinv_c.
  DELETE it_saida_zinv_c WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  LOOP AT it_saida_zinv_c INTO wa_saida_zinv_c.
    READ TABLE it_saida_zinv INTO wa_saida_zinv WITH KEY bukrs      = wa_saida_zinv_c-bukrs
                                                         bukrs_ate  = wa_saida_zinv_c-bukrs_ate
                                                         tipo = wa_saida_zinv_c-tipo
                                                         waers      = wa_saida_zinv_c-waers
                                                         tp_operacao = wa_saida_zinv_c-tp_operacao
                                                         matnr = wa_saida_zinv_c-matnr
                                                         nivel      = wa_saida_zinv_c-nivel
                                                         aprovador  = wa_saida_zinv_c-aprovador
                                                         dt_val_de  = wa_saida_zinv_c-dt_val_de
                                                         dt_val_ate = wa_saida_zinv_c-dt_val_ate.

    IF sy-subrc EQ 0.
      DELETE TABLE it_saida_zinv_c FROM wa_saida_zinv_c.
    ENDIF.
    CLEAR: wa_saida_zinv.
  ENDLOOP.

  "seleção complementar da ZFRE
  SELECT * FROM zlest0156 INTO CORRESPONDING FIELDS OF TABLE it_saida_zfre_c.

  LOOP AT it_saida_zfre_c INTO wa_saida_zfre_c.
    READ TABLE it_saida_zfre INTO wa_saida_zfre WITH KEY bukrs      = wa_saida_zfre_c-bukrs
                                                         bukrs_ate  = wa_saida_zfre_c-bukrs_ate
                                                         nivel      = wa_saida_zfre_c-nivel
                                                         aprovador  = wa_saida_zfre_c-aprovador
                                                         dt_val_de  = wa_saida_zfre_c-dt_val_de
                                                         dt_val_ate = wa_saida_zfre_c-dt_val_ate.

    IF sy-subrc EQ 0.
      DELETE TABLE it_saida_zfre_c FROM wa_saida_zfre_c.
    ENDIF.
    CLEAR: wa_saida_zfre.
  ENDLOOP.

  "seleção complementar da ZSOLOV
  SELECT * FROM zsdt0161 INTO CORRESPONDING FIELDS OF TABLE it_saida_zsolov_c.
  DELETE it_saida_zsolov_c WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  LOOP AT it_saida_zsolov_c INTO wa_saida_zsolov_c.
    READ TABLE it_saida_zsolov INTO wa_saida_zsolov WITH KEY bukrs      = wa_saida_zsolov_c-bukrs
                                                             bukrs_ate  = wa_saida_zsolov_c-bukrs_ate
                                                             nivel      = wa_saida_zsolov_c-nivel
                                                             aprovador  = wa_saida_zsolov_c-aprovador
                                                             dt_val_de  = wa_saida_zsolov_c-dt_val_de
                                                             dt_val_ate = wa_saida_zsolov_c-dt_val_ate.

    IF sy-subrc EQ 0.
      DELETE TABLE it_saida_zsolov_c FROM wa_saida_zsolov_c.
    ENDIF.
    CLEAR: wa_saida_zsolov.
  ENDLOOP.

  "seleção complementar da zsdt0152
  SELECT * FROM zsdt0152 INTO CORRESPONDING FIELDS OF TABLE it_saida_lim_c.

  LOOP AT it_saida_lim_c INTO wa_saida_lim_c.
    READ TABLE it_saida_lim INTO wa_saida_lim WITH KEY vkorg      = wa_saida_lim_c-vkorg
                                                       werks      = wa_saida_lim_c-werks
                                                       werks_ate  = wa_saida_lim_c-werks_ate
                                                       nivel      = wa_saida_lim_c-nivel
                                                       aprovador  = wa_saida_lim_c-aprovador
                                                       waers      = wa_saida_lim_c-waers
                                                       dt_val_de  = wa_saida_lim_c-dt_val_de
                                                       dt_val_ate = wa_saida_lim_c-dt_val_ate.

    IF sy-subrc EQ 0.
      DELETE TABLE it_saida_lim_c FROM wa_saida_lim_c.
    ENDIF.
    CLEAR: wa_saida_lim.
  ENDLOOP.

  "seleção complementar da Variação Cambio
  SELECT * FROM zmmt0150 INTO CORRESPONDING FIELDS OF TABLE it_saida_var_camb_c.
  DELETE it_saida_var_camb_c WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  LOOP AT it_saida_var_camb_c INTO wa_saida_var_camb_c.
    READ TABLE it_saida_var_camb INTO wa_saida_var_camb WITH KEY bukrs  = wa_saida_var_camb_c-bukrs
                                                             bukrs_ate  = wa_saida_var_camb_c-bukrs_ate
                                                             nivel      = wa_saida_var_camb_c-nivel
                                                             aprovador  = wa_saida_var_camb_c-aprovador
                                                             dt_val_de  = wa_saida_var_camb_c-dt_val_de
                                                             dt_val_ate = wa_saida_var_camb_c-dt_val_ate.

    IF sy-subrc EQ 0.
      DELETE TABLE it_saida_var_camb_c FROM wa_saida_var_camb_c.
    ENDIF.
    CLEAR: wa_saida_var_camb.
  ENDLOOP.

  "seleção complementar da isencao
  SELECT * FROM zsdt0336 INTO CORRESPONDING FIELDS OF TABLE it_saida_isencao_c.
  DELETE it_saida_isencao_c WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  LOOP AT it_saida_isencao_c INTO wa_saida_isencao_c.
    READ TABLE it_saida_isencao INTO wa_saida_isencao WITH KEY bukrs    = wa_saida_isencao_c-bukrs
                                                             bukrs_ate  = wa_saida_isencao_c-bukrs_ate
                                                             nivel      = wa_saida_isencao_c-nivel
                                                             aprovador  = wa_saida_isencao_c-aprovador
                                                             dt_val_de  = wa_saida_isencao_c-dt_val_de
                                                             dt_val_ate = wa_saida_isencao_c-dt_val_ate.

    IF sy-subrc EQ 0.
      DELETE TABLE it_saida_isencao_c FROM wa_saida_isencao_c.
    ENDIF.
    CLEAR: wa_saida_isencao.
  ENDLOOP.

  "seleção Operação ZNFW
  SELECT * FROM zfiwrt0033 INTO CORRESPONDING FIELDS OF TABLE it_saida_operznfw_c. "150184 CS2024000781 Aprovações ZNFW - PSA
  "PSA aqui esta limpando a tela!
  DELETE it_saida_operznfw_c WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  LOOP AT it_saida_operznfw_c INTO wa_saida_operznfw_c.
    READ TABLE it_saida_operznfw INTO wa_saida_operznfw WITH KEY dep_resp     = wa_saida_operznfw_c-dep_resp
                                                             nivel            = wa_saida_operznfw_c-nivel
                                                             aprovador        = wa_saida_operznfw_c-aprovador
                                                             dt_val_de        = wa_saida_operznfw_c-dt_val_de
                                                             dt_val_ate       = wa_saida_operznfw_c-dt_val_ate.
    IF sy-subrc EQ 0.
      DELETE TABLE it_saida_operznfw_c FROM wa_saida_operznfw_c.
    ENDIF.
    CLEAR: wa_saida_operznfw.
  ENDLOOP.

  " 07.05.2025 - 174338 - RAMON -->
  SELECT * FROM zsdt0385 INTO CORRESPONDING FIELDS OF TABLE it_saida_checklist_c.
  DELETE it_saida_checklist_c WHERE dt_val_ate = sy-datum AND hr_val_ate LT sy-uzeit.

  LOOP AT it_saida_checklist_c INTO wa_saida_checklist_c.
    READ TABLE it_saida_checklist INTO wa_saida_checklist WITH KEY bukrs    = wa_saida_checklist_c-bukrs
                                                             bukrs_ate  = wa_saida_checklist_c-bukrs_ate
                                                             nivel      = wa_saida_checklist_c-nivel
                                                             aprovador  = wa_saida_checklist_c-aprovador
                                                             dt_val_de  = wa_saida_checklist_c-dt_val_de
                                                             dt_val_ate = wa_saida_checklist_c-dt_val_ate.

    IF sy-subrc EQ 0.
      DELETE TABLE it_saida_checklist_c FROM wa_saida_checklist_c.
    ENDIF.
    CLEAR: wa_saida_checklist.
  ENDLOOP.

  " 07.05.2025 - 174338 - RAMON --<
  PERFORM marcar_antigos_complementar.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDIT_ALV_ZIMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM edit_alv_zimp.

  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_zimp INTO wa_saida_zimp.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_zimp-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_zimp-celltab.
    MODIFY it_saida_zimp FROM wa_saida_zimp INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_zimp.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDIT_ALV_ZGLT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM edit_alv_zglt.

  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_zglt INTO wa_saida_zglt.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_zglt-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_zglt-celltab.
    MODIFY it_saida_zglt FROM wa_saida_zglt INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_zglt.

ENDFORM.

FORM edit_alv_zov.

  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_zov INTO wa_saida_zov.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_zov-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_zov-celltab.
    MODIFY it_saida_zov FROM wa_saida_zov INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_zov.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDIT_ALV_ZADTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM edit_alv_zadto.

  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_zadto INTO wa_saida_zadto.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_zadto-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_zadto-celltab.
    MODIFY it_saida_zadto FROM wa_saida_zadto INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_zadto.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDIT_ALV_ZINV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM edit_alv_zinv.

  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_zinv INTO wa_saida_zinv.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_zinv-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_zinv-celltab.
    MODIFY it_saida_zinv FROM wa_saida_zinv INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_zinv.

ENDFORM.

FORM edit_alv_zfre.

  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_zfre INTO wa_saida_zfre.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_zfre-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_zfre-celltab.
    MODIFY it_saida_zfre FROM wa_saida_zfre INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_zfre.

ENDFORM.

FORM edit_alv_zsolov.

  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_zsolov INTO wa_saida_zsolov.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_zsolov-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_zsolov-celltab.
    MODIFY it_saida_zsolov FROM wa_saida_zsolov INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_zsolov.

ENDFORM.

FORM edit_alv_isencao.

  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_isencao INTO wa_saida_isencao.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_isencao-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_isencao-celltab.
    MODIFY it_saida_isencao FROM wa_saida_isencao INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_isencao.

ENDFORM.

FORM edit_alv_operznfw. "150184 CS2024000781 Aprovações ZNFW - PSA

  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_operznfw INTO wa_saida_operznfw.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_operznfw-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_operznfw-celltab.
    MODIFY it_saida_operznfw FROM wa_saida_operznfw INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_isencao.

ENDFORM.


FORM fill_celltab USING VALUE(p_mode)
                  CHANGING pt_celltab TYPE lvc_t_styl.

  DATA: ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4.

  IF p_mode EQ 'RW'.
    l_mode = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.                                "p_mode eq 'RO'
    l_mode = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  ls_celltab-fieldname = 'VKORG'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'WERKS'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'WERKS_ATE'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'BUKRS'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'BUKRS_ATE'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'DEP_RESP'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'VKBUR'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'VKBUR_ATE'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'APROVADOR'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'PGT_FORN'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'NIVEL'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'WAERS'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'MOTIVO'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'VALOR_DE'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'VALOR_ATE'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'DT_VAL_DE'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'DT_VAL_ATE'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'TIPO'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'TP_OPERACAO'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'MATNR'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'TRANSF_APROV'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'TP_VENDA'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'TP_VENDA_ATE'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'TP_NEGOCIO_DE'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.
  ls_celltab-fieldname = 'TP_NEGOCIO_ATE'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.

  ls_celltab-fieldname = 'DEP_RESP'."150184 CS2024000781 Aprovações ZNFW - PSA
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.

ENDFORM.                               " FILL_CELLTAB
*&---------------------------------------------------------------------*
*&      Form  MARCAR_ANTIGOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM marcar_antigos .

  LOOP AT it_saida_zimp INTO wa_saida_zimp.
    wa_saida_zimp-ck_ant = 'X'.
    MODIFY it_saida_zimp FROM wa_saida_zimp INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_zglt INTO wa_saida_zglt.
    wa_saida_zglt-ck_ant = 'X'.
    MODIFY it_saida_zglt FROM wa_saida_zglt INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_zov INTO wa_saida_zov.
    wa_saida_zov-ck_ant = 'X'.
    MODIFY it_saida_zov FROM wa_saida_zov INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_zinv INTO wa_saida_zinv.
    wa_saida_zinv-ck_ant = 'X'.
    MODIFY it_saida_zinv FROM wa_saida_zinv INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_zadto INTO wa_saida_zadto.
    wa_saida_zadto-ck_ant = 'X'.
    MODIFY it_saida_zadto FROM wa_saida_zadto INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_zfre INTO wa_saida_zfre.
    wa_saida_zfre-ck_ant = 'X'.
    MODIFY it_saida_zfre FROM wa_saida_zfre INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_zsolov INTO wa_saida_zsolov.
    wa_saida_zsolov-ck_ant = 'X'.
    MODIFY it_saida_zsolov FROM wa_saida_zsolov INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_var_camb  INTO wa_saida_var_camb.
    wa_saida_var_camb-ck_ant = 'X'.
    MODIFY it_saida_var_camb FROM wa_saida_var_camb INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_lim INTO wa_saida_lim.
    wa_saida_lim-ck_ant = 'X'.
    MODIFY it_saida_lim FROM wa_saida_lim INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_isencao INTO wa_saida_isencao.
    wa_saida_isencao-ck_ant = 'X'.
    MODIFY it_saida_isencao FROM wa_saida_isencao INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_operznfw INTO wa_saida_operznfw."150184 CS2024000781 Aprovações ZNFW - PSA
    wa_saida_operznfw-ck_ant = 'X'.
    MODIFY it_saida_operznfw FROM wa_saida_operznfw INDEX sy-tabix.
  ENDLOOP.

  " 08.05.2025 - 174338 - RAMON -->
  LOOP AT it_saida_checklist INTO wa_saida_checklist.
    wa_saida_checklist-ck_ant = 'X'.
    MODIFY it_saida_checklist FROM wa_saida_checklist INDEX sy-tabix.
  ENDLOOP.
  " 08.05.2025 - 174338 - RAMON --<

  CLEAR: wa_saida_lim, wa_saida_zadto, wa_saida_zinv, wa_saida_zglt, wa_saida_zimp, wa_saida_zfre, wa_saida_zfre, wa_saida_isencao,wa_saida_operznfw, wa_saida_checklist.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MARCAR_ANTIGOS_COMPLEMENTAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM marcar_antigos_complementar .

  LOOP AT it_saida_zimp_c INTO wa_saida_zimp_c.
    wa_saida_zimp_c-ck_ant = 'X'.
    MODIFY it_saida_zimp_c FROM wa_saida_zimp_c INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_zglt_c INTO wa_saida_zglt_c.
    wa_saida_zglt_c-ck_ant = 'X'.
    MODIFY it_saida_zglt_c FROM wa_saida_zglt_c INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_zinv_c INTO wa_saida_zinv_c.
    wa_saida_zinv_c-ck_ant = 'X'.
    MODIFY it_saida_zinv_c FROM wa_saida_zinv_c INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_zadto_c INTO wa_saida_zadto_c.
    wa_saida_zadto_c-ck_ant = 'X'.
    MODIFY it_saida_zadto_c FROM wa_saida_zadto_c INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_zfre_c INTO wa_saida_zfre_c.
    wa_saida_zfre_c-ck_ant = 'X'.
    MODIFY it_saida_zfre_c FROM wa_saida_zfre_c INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_zsolov_c INTO wa_saida_zsolov_c.
    wa_saida_zsolov_c-ck_ant = 'X'.
    MODIFY it_saida_zsolov_c FROM wa_saida_zsolov_c INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_lim_c INTO wa_saida_lim_c.
    wa_saida_lim_c-ck_ant = 'X'.
    MODIFY it_saida_lim_c FROM wa_saida_lim_c INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_var_camb_c INTO wa_saida_var_camb_c.
    wa_saida_var_camb_c-ck_ant = 'X'.
    MODIFY it_saida_var_camb_c FROM wa_saida_var_camb_c INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_isencao_c INTO wa_saida_isencao_c.
    wa_saida_isencao_c-ck_ant = 'X'.
    MODIFY it_saida_isencao_c FROM wa_saida_isencao_c INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_saida_operznfw_c INTO wa_saida_operznfw_c.
    wa_saida_operznfw_c-ck_ant = 'X'.
    MODIFY it_saida_operznfw_c FROM wa_saida_operznfw_c INDEX sy-tabix.
  ENDLOOP.

  " 07.05.2025 - 174338 - RAMON -->
  LOOP AT it_saida_checklist_c INTO wa_saida_checklist_c.
    wa_saida_checklist_c-ck_ant = 'X'.
    MODIFY it_saida_checklist_c FROM wa_saida_checklist_c INDEX sy-tabix.
  ENDLOOP.
  " 07.05.2025 - 174338 - RAMON --<

  CLEAR: wa_saida_lim_c, wa_saida_zadto_c, wa_saida_zinv_c, wa_saida_zglt_c, wa_saida_zimp_c, wa_saida_zfre_c, wa_saida_zsolov_c,
         wa_saida_var_camb_c,wa_saida_isencao_c,wa_saida_operznfw_c,wa_saida_checklist_c. " 07.05.2025 - 174338 - RAMON

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_HORA_NOVOS_ZIMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_hora_novos_zimp.

  DATA: vl_uzeit TYPE sy-uzeit.

  CLEAR: wa_saida_zimp.
  vl_uzeit = sy-uzeit.

  LOOP AT it_saida_zimp INTO wa_saida_zimp WHERE ck_ant NE abap_true.
    wa_saida_zimp-hr_val_de = vl_uzeit.
    wa_saida_zimp-hr_val_ate = '235959'.
    MODIFY it_saida_zimp FROM wa_saida_zimp INDEX sy-tabix.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_HORA_NOVOS_ZGLT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_hora_novos_zglt.

  DATA: vl_uzeit TYPE sy-uzeit.

  CLEAR: wa_saida_zglt.
  vl_uzeit = sy-uzeit.

  LOOP AT it_saida_zglt INTO wa_saida_zglt WHERE ck_ant NE abap_true.
    wa_saida_zglt-hr_val_de = vl_uzeit.
    wa_saida_zglt-hr_val_ate = '235959'.
    MODIFY it_saida_zglt FROM wa_saida_zglt INDEX sy-tabix.
  ENDLOOP.

ENDFORM.

FORM add_hora_novos_zov.

  DATA: vl_uzeit TYPE sy-uzeit.

  CLEAR: wa_saida_zov.
  vl_uzeit = sy-uzeit.

  LOOP AT it_saida_zov INTO wa_saida_zov WHERE ck_ant NE abap_true.
    wa_saida_zov-hr_val_de = vl_uzeit.
    wa_saida_zov-hr_val_ate = '235959'.
    MODIFY it_saida_zov FROM wa_saida_zov INDEX sy-tabix.
  ENDLOOP.

ENDFORM.


FORM add_hora_novos_lim.

  DATA: vl_uzeit TYPE sy-uzeit.

  CLEAR: wa_saida_lim.
  vl_uzeit = sy-uzeit.

  LOOP AT it_saida_lim INTO wa_saida_lim WHERE ck_ant NE abap_true.
    wa_saida_lim-hr_val_de = vl_uzeit.
    wa_saida_lim-hr_val_ate = '235959'.
    MODIFY it_saida_lim FROM wa_saida_lim INDEX sy-tabix.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_HORA_NOVOS_ZADTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_hora_novos_zadto.

  DATA: vl_uzeit TYPE sy-uzeit.

  CLEAR: wa_saida_zadto.
  vl_uzeit = sy-uzeit.

  LOOP AT it_saida_zadto INTO wa_saida_zadto WHERE ck_ant NE abap_true.
    wa_saida_zadto-hr_val_de = vl_uzeit.
    wa_saida_zadto-hr_val_ate = '235959'.
    MODIFY it_saida_zadto FROM wa_saida_zadto INDEX sy-tabix.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_HORA_NOVOS_ZINV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_hora_novos_zinv.

  DATA: vl_uzeit TYPE sy-uzeit.

  CLEAR: wa_saida_zinv.
  vl_uzeit = sy-uzeit.

  LOOP AT it_saida_zinv INTO wa_saida_zinv WHERE ck_ant NE abap_true.
    wa_saida_zinv-hr_val_de = vl_uzeit.
    wa_saida_zinv-hr_val_ate = '235959'.
    MODIFY it_saida_zinv FROM wa_saida_zinv INDEX sy-tabix.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ADD_HORA_NOVOS_FRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_hora_novos_fre .
  DATA: vl_uzeit TYPE sy-uzeit.

  CLEAR: wa_saida_zfre.
  vl_uzeit = sy-uzeit.

  LOOP AT it_saida_zfre INTO wa_saida_zfre WHERE ck_ant NE abap_true.
    wa_saida_zfre-hr_val_de = vl_uzeit.
    wa_saida_zfre-hr_val_ate = '235959'.
    MODIFY it_saida_zfre FROM wa_saida_zfre INDEX sy-tabix.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_HORA_NOVOS_ZSOLOV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_hora_novos_zsolov.
  DATA: vl_uzeit TYPE sy-uzeit.

  CLEAR: wa_saida_zsolov.
  vl_uzeit = sy-uzeit.

  LOOP AT it_saida_zsolov INTO wa_saida_zsolov WHERE ck_ant NE abap_true.
    wa_saida_zsolov-hr_val_de = vl_uzeit.
    wa_saida_zsolov-hr_val_ate = '235959'.
    MODIFY it_saida_zsolov FROM wa_saida_zsolov INDEX sy-tabix.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_HORA_NOVOS_VAR_CAMB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_hora_novos_var_camb.
  DATA: vl_uzeit TYPE sy-uzeit.

  CLEAR: wa_saida_var_camb.
  vl_uzeit = sy-uzeit.

  LOOP AT it_saida_var_camb INTO wa_saida_var_camb WHERE ck_ant NE abap_true.
    wa_saida_var_camb-hr_val_de = vl_uzeit.
    wa_saida_var_camb-hr_val_ate = '235959'.
    MODIFY it_saida_var_camb FROM wa_saida_var_camb INDEX sy-tabix.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TERMINAR_ESTRATEGIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM terminar_estrategia .

  DATA: vl_lines                    TYPE i,
        vl_uzeit                    TYPE sy-uzeit,
        pop_up_msg                  TYPE string,
        ans                         TYPE char1,
        wa_selected_rows_term       TYPE lvc_s_row,
        wa_saida_zimp_term          TYPE t_zimp_aprovador,
        wa_saida_zglt_term          TYPE t_zglt037,
        wa_saida_zov_term           TYPE t_zsdt0141,
        wa_saida_lim_term           TYPE t_zsdt0152,
        wa_saida_zinv_term          TYPE t_zinv_aprovador,
        wa_saida_zadto_term         TYPE t_zadto_aprovador,
        wa_saida_zfre_term          TYPE t_zlest0156,
        wa_saida_zsolov_term        TYPE t_zsdt0161,
        wa_saida_var_camb_term      TYPE t_zmmt0150,
        it_zimp_aprovador_term_aux  TYPE STANDARD TABLE OF zimp_aprovador,
        wa_zimp_aprovador_term_aux  TYPE zimp_aprovador,
        it_zglt037_term_aux         TYPE STANDARD TABLE OF zglt037,
        wa_zglt037_term_aux         TYPE zglt037,
        it_zsdt0141_term_aux        TYPE STANDARD TABLE OF zsdt0141,
        wa_zsdt0141_term_aux        TYPE zsdt0141,

        it_zsdt0152_term_aux        TYPE STANDARD TABLE OF zsdt0152,
        wa_zsdt0152_term_aux        TYPE zsdt0152,

        it_zinv_aprovador_term_aux  TYPE STANDARD TABLE OF zinv_aprovador,
        wa_zinv_aprovador_term_aux  TYPE zinv_aprovador,
        it_zadto_aprovador_term_aux TYPE STANDARD TABLE OF zadto_aprovador,
        wa_zadto_aprovador_term_aux TYPE zadto_aprovador,
        it_zfre_term_aux            TYPE STANDARD TABLE OF zlest0156,
        wa_zfre_term_aux            TYPE zlest0156,
        it_zsolov_term_aux          TYPE STANDARD TABLE OF zsdt0161,
        wa_zsolov_term_aux          TYPE zsdt0161,
        it_var_camb_term_aux        TYPE STANDARD TABLE OF zmmt0150,
        wa_var_camb_term_aux        TYPE zmmt0150,
        it_operznfw_term_aux        TYPE STANDARD TABLE OF zfiwrt0033,
        wa_operznfw_term_aux        TYPE zfiwrt0033,

        " 09.05.2025 - 174338 - RAMON -->
        it_zsdt0385_term_aux        TYPE STANDARD TABLE OF zsdt0385,
        wa_zsdt0385_term_aux        TYPE zsdt0385,
        wa_saida_checklist_term     TYPE t_zsdt0385.
  " 09.05.2025 - 174338 - RAMON --<

  IF                          o_grid IS NOT INITIAL.
    CALL METHOD o_grid->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_rows.
  ENDIF.

  DESCRIBE TABLE it_selected_rows LINES vl_lines.

  IF vl_lines EQ 0.
    MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 9000.
  ELSEIF vl_lines NE 1.
    MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 9000.
  ENDIF.

  pop_up_msg = 'Você tem certeza que deseja terminar a Estratégia selecionada?'.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Terminar Estratégia'
      text_question         = pop_up_msg
      text_button_1         = 'Sim' "(002)
      text_button_2         = 'Não' "(005)
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = ans.

  IF ans = 1.

    vl_uzeit = sy-uzeit.
    READ TABLE it_selected_rows INTO wa_selected_rows_term INDEX 1.

    IF i_main_tab-pressed_tab = c_main_tab-tab1.

      READ TABLE it_saida_zimp INTO wa_saida_zimp_term INDEX wa_selected_rows_term-index.

      SELECT *
        FROM zimp_aprovador
        INTO TABLE it_zimp_aprovador_term_aux
        WHERE bukrs EQ wa_saida_zimp_term-bukrs
          AND bukrs_ate EQ wa_saida_zimp_term-bukrs_ate
          AND dep_resp EQ wa_saida_zimp_term-dep_resp
          AND waers EQ wa_saida_zimp_term-waers
          AND dt_val_ate GE sy-datum.

      LOOP AT it_zimp_aprovador_term_aux INTO wa_zimp_aprovador_term_aux.
        IF wa_zimp_aprovador_term_aux-dt_val_de GT sy-datum.
          DELETE zimp_aprovador FROM wa_zimp_aprovador_term_aux.
        ELSEIF wa_zimp_aprovador_term_aux-dt_val_de EQ sy-datum
                 AND wa_zimp_aprovador_term_aux-hr_val_de  GT sy-uzeit.
          DELETE zimp_aprovador FROM wa_zimp_aprovador_term_aux.
        ELSEIF ( wa_zimp_aprovador_term_aux-dt_val_de      LT sy-datum
                 AND wa_zimp_aprovador_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zimp_aprovador_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zimp_aprovador_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zimp_aprovador_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zimp_aprovador_term_aux-dt_val_de      LT sy-datum
                 AND wa_zimp_aprovador_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zimp_aprovador_term_aux-hr_val_ate GT sy-uzeit ) OR
               ( wa_zimp_aprovador_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zimp_aprovador_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zimp_aprovador_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zimp_aprovador_term_aux-hr_val_ate GT sy-uzeit ).
          DELETE zimp_aprovador FROM wa_zimp_aprovador_term_aux.
          wa_zimp_aprovador_term_aux-dt_val_ate = sy-datum.
          wa_zimp_aprovador_term_aux-hr_val_ate = vl_uzeit.
          wa_zimp_aprovador_term_aux-data_atual = sy-datum.
          wa_zimp_aprovador_term_aux-hora_atual = vl_uzeit.
          wa_zimp_aprovador_term_aux-usuario = sy-uname.
          MODIFY zimp_aprovador FROM wa_zimp_aprovador_term_aux.
        ENDIF.
      ENDLOOP.

    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab2.

      READ TABLE it_saida_zglt INTO wa_saida_zglt_term INDEX wa_selected_rows_term-index.

      SELECT *
        FROM zglt037
        INTO TABLE it_zglt037_term_aux
        WHERE bukrs EQ wa_saida_zglt_term-bukrs
          AND bukrs_ate EQ wa_saida_zglt_term-bukrs_ate
          AND dep_resp EQ wa_saida_zglt_term-dep_resp
          AND pgt_forn EQ wa_saida_zglt_term-pgt_forn
          AND waers EQ wa_saida_zglt_term-waers
          AND dt_val_ate GE sy-datum.

      LOOP AT it_zglt037_term_aux INTO wa_zglt037_term_aux.
        IF wa_zglt037_term_aux-dt_val_de GT sy-datum.
          DELETE zglt037 FROM wa_zglt037_term_aux.
        ELSEIF wa_zglt037_term_aux-dt_val_de EQ sy-datum
                 AND wa_zglt037_term_aux-hr_val_de  GT sy-uzeit.
          DELETE zglt037 FROM wa_zglt037_term_aux.
        ELSEIF ( wa_zglt037_term_aux-dt_val_de      LT sy-datum
                 AND wa_zglt037_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zglt037_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zglt037_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zglt037_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zglt037_term_aux-dt_val_de      LT sy-datum
                 AND wa_zglt037_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zglt037_term_aux-hr_val_ate GT sy-uzeit ) OR
               ( wa_zglt037_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zglt037_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zglt037_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zglt037_term_aux-hr_val_ate GT sy-uzeit ).
          DELETE zglt037 FROM wa_zglt037_term_aux.
          wa_zglt037_term_aux-dt_val_ate = sy-datum.
          wa_zglt037_term_aux-hr_val_ate = vl_uzeit.
          wa_zglt037_term_aux-data_atual = sy-datum.
          wa_zglt037_term_aux-hora_atual = vl_uzeit.
          wa_zglt037_term_aux-usuario = sy-uname.
          MODIFY zglt037 FROM wa_zglt037_term_aux.
        ENDIF.
      ENDLOOP.

    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab3.

      READ TABLE it_saida_zinv INTO wa_saida_zinv_term INDEX wa_selected_rows_term-index.

      SELECT *
        FROM zinv_aprovador
        INTO TABLE it_zinv_aprovador_term_aux
        WHERE bukrs EQ wa_saida_zinv_term-bukrs
          AND bukrs_ate EQ wa_saida_zinv_term-bukrs_ate
          AND tipo EQ wa_saida_zinv_term-tipo
          AND waers EQ wa_saida_zinv_term-waers
          AND tp_operacao EQ wa_saida_zinv_term-tp_operacao
          AND matnr EQ wa_saida_zinv_term-matnr
          AND dt_val_ate GE sy-datum.

      LOOP AT it_zinv_aprovador_term_aux INTO wa_zinv_aprovador_term_aux.
        IF wa_zinv_aprovador_term_aux-dt_val_de GT sy-datum.
          DELETE zinv_aprovador FROM wa_zinv_aprovador_term_aux.
        ELSEIF wa_zinv_aprovador_term_aux-dt_val_de EQ sy-datum
                 AND wa_zinv_aprovador_term_aux-hr_val_de  GT sy-uzeit.
          DELETE zinv_aprovador FROM wa_zinv_aprovador_term_aux.
        ELSEIF ( wa_zinv_aprovador_term_aux-dt_val_de      LT sy-datum
                 AND wa_zinv_aprovador_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zinv_aprovador_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zinv_aprovador_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zinv_aprovador_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zinv_aprovador_term_aux-dt_val_de      LT sy-datum
                 AND wa_zinv_aprovador_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zinv_aprovador_term_aux-hr_val_ate GT sy-uzeit ) OR
               ( wa_zinv_aprovador_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zinv_aprovador_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zinv_aprovador_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zinv_aprovador_term_aux-hr_val_ate GT sy-uzeit ).
          DELETE zinv_aprovador FROM wa_zinv_aprovador_term_aux.
          wa_zinv_aprovador_term_aux-dt_val_ate = sy-datum.
          wa_zinv_aprovador_term_aux-hr_val_ate = vl_uzeit.
          wa_zinv_aprovador_term_aux-data_atual = sy-datum.
          wa_zinv_aprovador_term_aux-hora_atual = vl_uzeit.
          wa_zinv_aprovador_term_aux-usuario = sy-uname.
          MODIFY zinv_aprovador FROM wa_zinv_aprovador_term_aux.
        ENDIF.
      ENDLOOP.

    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab4.

      READ TABLE it_saida_zadto INTO wa_saida_zadto_term INDEX wa_selected_rows_term-index.

      SELECT *
        FROM zadto_aprovador
        INTO TABLE it_zadto_aprovador_term_aux
        WHERE bukrs EQ wa_saida_zadto_term-bukrs
          AND bukrs_ate EQ wa_saida_zadto_term-bukrs_ate
          AND dep_resp EQ wa_saida_zadto_term-dep_resp
          AND waers EQ wa_saida_zadto_term-waers
          AND dt_val_ate GE sy-datum.

      LOOP AT it_zadto_aprovador_term_aux INTO wa_zadto_aprovador_term_aux.
        IF wa_zadto_aprovador_term_aux-dt_val_de GT sy-datum.
          DELETE zadto_aprovador FROM wa_zadto_aprovador_term_aux.
        ELSEIF wa_zadto_aprovador_term_aux-dt_val_de EQ sy-datum
                 AND wa_zadto_aprovador_term_aux-hr_val_de  GT sy-uzeit.
          DELETE zadto_aprovador FROM wa_zadto_aprovador_term_aux.
        ELSEIF ( wa_zadto_aprovador_term_aux-dt_val_de      LT sy-datum
                 AND wa_zadto_aprovador_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zadto_aprovador_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zadto_aprovador_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zadto_aprovador_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zadto_aprovador_term_aux-dt_val_de      LT sy-datum
                 AND wa_zadto_aprovador_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zadto_aprovador_term_aux-hr_val_ate GT sy-uzeit ) OR
               ( wa_zadto_aprovador_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zadto_aprovador_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zadto_aprovador_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zadto_aprovador_term_aux-hr_val_ate GT sy-uzeit ).
          DELETE zadto_aprovador FROM wa_zadto_aprovador_term_aux.
          wa_zadto_aprovador_term_aux-dt_val_ate = sy-datum.
          wa_zadto_aprovador_term_aux-hr_val_ate = vl_uzeit.
          wa_zadto_aprovador_term_aux-data_atual = sy-datum.
          wa_zadto_aprovador_term_aux-hora_atual = vl_uzeit.
          wa_zadto_aprovador_term_aux-usuario = sy-uname.
          MODIFY zadto_aprovador FROM wa_zadto_aprovador_term_aux.
        ENDIF.
      ENDLOOP.

    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab5.

      READ TABLE it_saida_zov INTO wa_saida_zov_term INDEX wa_selected_rows_term-index.

      SELECT *
        FROM zsdt0141
        INTO TABLE it_zsdt0141_term_aux
        WHERE bukrs     EQ wa_saida_zov_term-bukrs
          AND bukrs_ate EQ wa_saida_zov_term-bukrs_ate
          AND vkbur     EQ wa_saida_zov_term-vkbur
          AND vkbur_ate EQ wa_saida_zov_term-vkbur_ate
          AND waers EQ wa_saida_zov_term-waers
          AND dt_val_ate GE sy-datum.

      LOOP AT it_zsdt0141_term_aux INTO wa_zsdt0141_term_aux.
        IF wa_zsdt0141_term_aux-dt_val_de GT sy-datum.
          DELETE zsdt0141 FROM wa_zsdt0141_term_aux.
        ELSEIF wa_zsdt0141_term_aux-dt_val_de EQ sy-datum
                 AND wa_zsdt0141_term_aux-hr_val_de  GT sy-uzeit.
          DELETE zsdt0141 FROM wa_zsdt0141_term_aux.
        ELSEIF ( wa_zsdt0141_term_aux-dt_val_de      LT sy-datum
                 AND wa_zsdt0141_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zsdt0141_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zsdt0141_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zsdt0141_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zsdt0141_term_aux-dt_val_de      LT sy-datum
                 AND wa_zsdt0141_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zsdt0141_term_aux-hr_val_ate GT sy-uzeit ) OR
               ( wa_zsdt0141_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zsdt0141_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zsdt0141_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zsdt0141_term_aux-hr_val_ate GT sy-uzeit ).
          DELETE zsdt0141 FROM wa_zsdt0141_term_aux.
          wa_zsdt0141_term_aux-dt_val_ate = sy-datum.
          wa_zsdt0141_term_aux-hr_val_ate = vl_uzeit.
          wa_zsdt0141_term_aux-data_atual = sy-datum.
          wa_zsdt0141_term_aux-hora_atual = vl_uzeit.
          wa_zsdt0141_term_aux-usuario = sy-uname.
          MODIFY zsdt0141 FROM wa_zsdt0141_term_aux.
        ENDIF.
      ENDLOOP.
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab6.

      READ TABLE it_saida_lim INTO wa_saida_lim_term INDEX wa_selected_rows_term-index.

      SELECT *
        FROM zsdt0152
        INTO TABLE it_zsdt0152_term_aux
        WHERE vkorg     EQ wa_saida_lim_term-vkorg
          AND werks     EQ wa_saida_lim_term-werks
          AND dt_val_ate GE sy-datum.

      LOOP AT it_zsdt0152_term_aux INTO wa_zsdt0152_term_aux.
        IF wa_zsdt0152_term_aux-dt_val_de GT sy-datum.
          DELETE zsdt0152 FROM wa_zsdt0152_term_aux.
        ELSEIF wa_zsdt0152_term_aux-dt_val_de EQ sy-datum
                 AND wa_zsdt0152_term_aux-hr_val_de  GT sy-uzeit.
          DELETE zsdt0152 FROM wa_zsdt0141_term_aux.
        ELSEIF ( wa_zsdt0152_term_aux-dt_val_de      LT sy-datum
                 AND wa_zsdt0152_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zsdt0152_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zsdt0152_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zsdt0152_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zsdt0152_term_aux-dt_val_de      LT sy-datum
                 AND wa_zsdt0152_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zsdt0152_term_aux-hr_val_ate GT sy-uzeit ) OR
               ( wa_zsdt0152_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zsdt0152_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zsdt0152_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zsdt0152_term_aux-hr_val_ate GT sy-uzeit ).
          DELETE zsdt0152 FROM wa_zsdt0152_term_aux.
          wa_zsdt0152_term_aux-dt_val_ate = sy-datum.
          wa_zsdt0152_term_aux-hr_val_ate = vl_uzeit.
          wa_zsdt0152_term_aux-data_atual = sy-datum.
          wa_zsdt0152_term_aux-hora_atual = vl_uzeit.
          wa_zsdt0152_term_aux-usuario = sy-uname.
          MODIFY zsdt0152 FROM wa_zsdt0152_term_aux.
        ENDIF.
      ENDLOOP.

    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab7.

      READ TABLE it_saida_zfre INTO wa_saida_zfre_term INDEX wa_selected_rows_term-index.

      SELECT *
        FROM zlest0156
        INTO TABLE it_zfre_term_aux
        WHERE dt_val_ate GE sy-datum.

      LOOP AT it_zfre_term_aux INTO wa_zfre_term_aux.
        IF wa_zfre_term_aux-dt_val_de GT sy-datum.
          DELETE zlest0156 FROM wa_zfre_term_aux.
        ELSEIF wa_zfre_term_aux-dt_val_de EQ sy-datum
                 AND wa_zfre_term_aux-hr_val_de  GT sy-uzeit.
          DELETE zlest0156 FROM wa_zfre_term_aux.
        ELSEIF ( wa_zfre_term_aux-dt_val_de      LT sy-datum
                 AND wa_zfre_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zfre_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zfre_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zfre_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zfre_term_aux-dt_val_de      LT sy-datum
                 AND wa_zfre_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zfre_term_aux-hr_val_ate GT sy-uzeit ) OR
               ( wa_zfre_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zfre_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zfre_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zfre_term_aux-hr_val_ate GT sy-uzeit ).
          DELETE zlest0156 FROM wa_zfre_term_aux.
          wa_zfre_term_aux-dt_val_ate = sy-datum.
          wa_zfre_term_aux-hr_val_ate = vl_uzeit.
          wa_zfre_term_aux-data_atual = sy-datum.
          wa_zfre_term_aux-hora_atual = vl_uzeit.
          wa_zfre_term_aux-usuario = sy-uname.
          MODIFY zlest0156 FROM wa_zfre_term_aux.
        ENDIF.
      ENDLOOP.

    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab8.

      READ TABLE it_saida_zsolov INTO wa_saida_zsolov_term INDEX wa_selected_rows_term-index.

      SELECT *
        FROM zsdt0161
        INTO TABLE it_zsolov_term_aux
        WHERE bukrs         EQ wa_saida_zsolov_term-bukrs
          AND bukrs_ate     EQ wa_saida_zsolov_term-bukrs_ate
          AND vkbur         EQ wa_saida_zsolov_term-vkbur
          AND vkbur_ate     EQ wa_saida_zsolov_term-vkbur_ate
          AND tp_venda      EQ wa_saida_zsolov_term-tp_venda
          AND tp_venda_ate  EQ wa_saida_zsolov_term-tp_venda_ate
          AND dt_val_ate    GE sy-datum.

      LOOP AT it_zsolov_term_aux INTO wa_zsolov_term_aux.
        IF wa_zsolov_term_aux-dt_val_de GT sy-datum.
          DELETE zsdt0161 FROM wa_zsolov_term_aux.
        ELSEIF wa_zsolov_term_aux-dt_val_de EQ sy-datum
                 AND wa_zsolov_term_aux-hr_val_de  GT sy-uzeit.
          DELETE zsdt0161 FROM wa_zsolov_term_aux.
        ELSEIF ( wa_zsolov_term_aux-dt_val_de      LT sy-datum
                 AND wa_zsolov_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zsolov_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zsolov_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zsolov_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zsolov_term_aux-dt_val_de      LT sy-datum
                 AND wa_zsolov_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zsolov_term_aux-hr_val_ate GT sy-uzeit ) OR
               ( wa_zsolov_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zsolov_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zsolov_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zsolov_term_aux-hr_val_ate GT sy-uzeit ).
          DELETE zsdt0161 FROM wa_zsolov_term_aux.
          wa_zsolov_term_aux-dt_val_ate = sy-datum.
          wa_zsolov_term_aux-hr_val_ate = vl_uzeit.
          wa_zsolov_term_aux-data_atual = sy-datum.
          wa_zsolov_term_aux-hora_atual = vl_uzeit.
          wa_zsolov_term_aux-usuario = sy-uname.
          MODIFY zsdt0161 FROM wa_zsolov_term_aux.
        ENDIF.
      ENDLOOP.

    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab9.

      READ TABLE it_saida_var_camb INTO wa_saida_var_camb_term INDEX wa_selected_rows_term-index.

      SELECT *
        FROM zmmt0150
        INTO TABLE it_var_camb_term_aux
        WHERE bukrs         EQ wa_saida_var_camb_term-bukrs
          AND bukrs_ate     EQ wa_saida_var_camb_term-bukrs_ate
          AND dt_val_ate    GE sy-datum.

      LOOP AT it_var_camb_term_aux INTO wa_var_camb_term_aux.
        IF wa_var_camb_term_aux-dt_val_de GT sy-datum.
          DELETE zmmt0150 FROM wa_var_camb_term_aux.
        ELSEIF wa_var_camb_term_aux-dt_val_de EQ sy-datum
                 AND wa_var_camb_term_aux-hr_val_de  GT sy-uzeit.
          DELETE zmmt0150 FROM wa_var_camb_term_aux.
        ELSEIF ( wa_var_camb_term_aux-dt_val_de      LT sy-datum
                 AND wa_var_camb_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_var_camb_term_aux-dt_val_de      EQ sy-datum
                 AND wa_var_camb_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_var_camb_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_var_camb_term_aux-dt_val_de      LT sy-datum
                 AND wa_var_camb_term_aux-dt_val_ate EQ sy-datum
                 AND wa_var_camb_term_aux-hr_val_ate GT sy-uzeit ) OR
               ( wa_var_camb_term_aux-dt_val_de      EQ sy-datum
                 AND wa_var_camb_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_var_camb_term_aux-dt_val_ate EQ sy-datum
                 AND wa_var_camb_term_aux-hr_val_ate GT sy-uzeit ).
          DELETE zmmt0150 FROM wa_var_camb_term_aux.
          wa_var_camb_term_aux-dt_val_ate = sy-datum.
          wa_var_camb_term_aux-hr_val_ate = vl_uzeit.
          wa_var_camb_term_aux-data_atual = sy-datum.
          wa_var_camb_term_aux-hora_atual = vl_uzeit.
          wa_var_camb_term_aux-usuario = sy-uname.
          MODIFY zmmt0150 FROM wa_var_camb_term_aux.
        ENDIF.
      ENDLOOP.

    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab12.

*      READ TABLE it_saida_var_camb INTO wa_saida_var_camb_term INDEX wa_selected_rows_term-index.
*
*      SELECT *
*        FROM zmmt0150
*        INTO TABLE it_var_camb_term_aux
*        WHERE bukrs         EQ wa_saida_var_camb_term-bukrs
*          AND bukrs_ate     EQ wa_saida_var_camb_term-bukrs_ate
*          AND dt_val_ate    GE sy-datum.
*
*      LOOP AT it_var_camb_term_aux INTO wa_var_camb_term_aux.
*        IF wa_var_camb_term_aux-dt_val_de GT sy-datum.
*          DELETE zmmt0150 FROM wa_var_camb_term_aux.
*        ELSEIF wa_var_camb_term_aux-dt_val_de EQ sy-datum
*                 AND wa_var_camb_term_aux-hr_val_de  GT sy-uzeit.
*          DELETE zmmt0150 FROM wa_var_camb_term_aux.
*        ELSEIF ( wa_var_camb_term_aux-dt_val_de      LT sy-datum
*                 AND wa_var_camb_term_aux-dt_val_ate GT sy-datum ) OR
*               ( wa_var_camb_term_aux-dt_val_de      EQ sy-datum
*                 AND wa_var_camb_term_aux-hr_val_de  LT sy-uzeit
*                 AND wa_var_camb_term_aux-dt_val_ate GT sy-datum ) OR
*               ( wa_var_camb_term_aux-dt_val_de      LT sy-datum
*                 AND wa_var_camb_term_aux-dt_val_ate EQ sy-datum
*                 AND wa_var_camb_term_aux-hr_val_ate GT sy-uzeit ) OR
*               ( wa_var_camb_term_aux-dt_val_de      EQ sy-datum
*                 AND wa_var_camb_term_aux-hr_val_de  LT sy-uzeit
*                 AND wa_var_camb_term_aux-dt_val_ate EQ sy-datum
*                 AND wa_var_camb_term_aux-hr_val_ate GT sy-uzeit ).
*          DELETE zmmt0150 FROM wa_var_camb_term_aux.
*          wa_var_camb_term_aux-dt_val_ate = sy-datum.
*          wa_var_camb_term_aux-hr_val_ate = vl_uzeit.
*          wa_var_camb_term_aux-data_atual = sy-datum.
*          wa_var_camb_term_aux-hora_atual = vl_uzeit.
*          wa_var_camb_term_aux-usuario = sy-uname.
*          MODIFY zmmt0150 FROM wa_var_camb_term_aux.
*        ENDIF.
*      ENDLOOP.

      " 09.05.2025 - 174338 - RAMON -->
    ELSEIF i_main_tab-pressed_tab = c_main_tab-tab13.

      READ TABLE it_saida_checklist INTO wa_saida_checklist_term INDEX wa_selected_rows_term-index.

      SELECT *
        FROM zsdt0385
        INTO TABLE it_zsdt0385_term_aux
        WHERE bukrs     EQ wa_saida_checklist_term-bukrs
          AND bukrs_ate EQ wa_saida_checklist_term-bukrs_ate
          AND vkbur     EQ wa_saida_checklist_term-vkbur
          AND vkbur_ate EQ wa_saida_checklist_term-vkbur_ate
          "AND waers EQ wa_saida_checklist_term-waers
          AND dt_val_ate GE sy-datum.

      LOOP AT it_zsdt0385_term_aux INTO wa_zsdt0385_term_aux.
        IF wa_zsdt0385_term_aux-dt_val_de GT sy-datum.
          DELETE zsdt0385 FROM wa_zsdt0385_term_aux.
        ELSEIF wa_zsdt0385_term_aux-dt_val_de EQ sy-datum
                 AND wa_zsdt0385_term_aux-hr_val_de  GT sy-uzeit.
          DELETE zsdt0385 FROM wa_zsdt0385_term_aux.
        ELSEIF ( wa_zsdt0385_term_aux-dt_val_de      LT sy-datum
                 AND wa_zsdt0385_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zsdt0385_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zsdt0385_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zsdt0385_term_aux-dt_val_ate GT sy-datum ) OR
               ( wa_zsdt0385_term_aux-dt_val_de      LT sy-datum
                 AND wa_zsdt0385_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zsdt0385_term_aux-hr_val_ate GT sy-uzeit ) OR
               ( wa_zsdt0385_term_aux-dt_val_de      EQ sy-datum
                 AND wa_zsdt0385_term_aux-hr_val_de  LT sy-uzeit
                 AND wa_zsdt0385_term_aux-dt_val_ate EQ sy-datum
                 AND wa_zsdt0385_term_aux-hr_val_ate GT sy-uzeit ).
          DELETE zsdt0385 FROM wa_zsdt0385_term_aux.
          wa_zsdt0385_term_aux-dt_val_ate = sy-datum.
          wa_zsdt0385_term_aux-hr_val_ate = vl_uzeit.
          wa_zsdt0385_term_aux-data_atual = sy-datum.
          wa_zsdt0385_term_aux-hora_atual = vl_uzeit.
          wa_zsdt0385_term_aux-usuario = sy-uname.
          MODIFY zsdt0385 FROM wa_zsdt0385_term_aux.
        ENDIF.
      ENDLOOP.
      " 09.05.2025 - 174338 - RAMON --<


    ENDIF.
  ENDIF.

  PERFORM free_objects.
  PERFORM select_data.
  PERFORM sort_data.

  CLEAR: it_selected_rows.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EDIT_ALV_LIM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM edit_alv_lim .
  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_lim INTO wa_saida_lim.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_lim-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_lim-celltab.
    MODIFY it_saida_lim FROM wa_saida_lim INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_lim.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EDIT_ALV_VAR_CAMB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM edit_alv_var_camb.
  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_var_camb INTO wa_saida_var_camb.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_var_camb-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_var_camb-celltab.
    MODIFY it_saida_var_camb FROM wa_saida_var_camb INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_var_camb.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form edit_alv_izencao
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM edit_alv_izencao .

  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_isencao INTO wa_saida_isencao.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_isencao-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_isencao-celltab.
    MODIFY it_saida_isencao FROM wa_saida_isencao INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_isencao.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_hora_novos_isencao
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM add_hora_novos_isencao .
  DATA: vl_uzeit TYPE sy-uzeit.

  CLEAR: wa_saida_isencao.
  vl_uzeit = sy-uzeit.

  LOOP AT it_saida_isencao INTO wa_saida_isencao WHERE ck_ant NE abap_true.
    wa_saida_isencao-hr_val_de = vl_uzeit.
    wa_saida_isencao-hr_val_ate = '235959'.
    MODIFY it_saida_isencao FROM wa_saida_isencao INDEX sy-tabix.
  ENDLOOP.
ENDFORM.

FORM add_hora_novos_operznfw .
  DATA: vl_uzeit TYPE sy-uzeit.

  CLEAR: wa_saida_operznfw.
  vl_uzeit = sy-uzeit.

  LOOP AT it_saida_operznfw INTO wa_saida_operznfw WHERE ck_ant = abap_false.
    IF wa_saida_operznfw-ck_ant = abap_false.
      wa_saida_operznfw-hr_val_de = vl_uzeit.
      wa_saida_operznfw-hr_val_ate = '235959'.
      MODIFY it_saida_operznfw FROM wa_saida_operznfw INDEX sy-tabix.
    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form edit_alv_checklist
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM edit_alv_checklist .

  DATA: l_index    TYPE i,
        lt_celltab TYPE lvc_t_styl.

  LOOP AT it_saida_checklist INTO wa_saida_checklist.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    PERFORM fill_celltab USING 'RO'
                      CHANGING lt_celltab.

    CLEAR wa_saida_checklist-celltab.
    INSERT LINES OF lt_celltab INTO TABLE wa_saida_checklist-celltab.
    MODIFY it_saida_checklist FROM wa_saida_checklist INDEX l_index.
  ENDLOOP.

  CLEAR wa_saida_checklist.


ENDFORM.
