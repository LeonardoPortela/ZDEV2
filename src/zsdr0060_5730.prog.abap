*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5730
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_solicitacao_5730,
         check      TYPE char1,
         matnr      TYPE mara-matnr,
         maktx      TYPE makt-maktx,
         meins      TYPE vbap-meins,
         kunnr      TYPE vbpa-kunnr,
         name1      TYPE kna1-name1,
         inco1      TYPE vbkd-inco1,
         valor_brl  TYPE komv-kwert,
         moeda      TYPE vbak-waerk,
         cor(4)     TYPE c.
         INCLUDE      STRUCTURE zsdt0082.
TYPES: cellstyles TYPE lvc_t_styl.
TYPES: END OF ty_solicitacao_5730.

DATA: it_sol_5730     TYPE STANDARD TABLE OF ty_solicitacao_5730.

DATA: g_custom_container_5730  TYPE REF TO cl_gui_custom_container,
      ctl_alv1_5730            TYPE REF TO cl_gui_alv_grid,
      gs_layout_5730           TYPE lvc_s_layo,
      it_fieldcatalog_sol_5730 TYPE lvc_t_fcat,
      it_exclude_5730          TYPE ui_functions,
      it_stable_5730           TYPE lvc_s_stbl.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5730 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:

      toolbar_5730 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command_5730 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_5730 IMPLEMENTATION.

  METHOD toolbar_5730.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'DEL'.
    wa_tool-icon     = '@18@'.
    wa_tool-quickinfo = 'Eliminar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

  METHOD user_command_5730.

    IF e_ucomm = 'DEL'.

      DATA: it_sel_rows_5730 TYPE lvc_t_row,
            wa_sel_rows_5730 TYPE lvc_s_row,
            wa_sol_5730      TYPE ty_solicitacao_5730,
            vl_cont          TYPE i,
            chave            TYPE zde_chave_sol.

      CALL METHOD ctl_alv1_5730->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows_5730.

      LOOP AT it_sol_5730 INTO wa_sol_5730.
        READ TABLE it_sel_rows_5730 INTO wa_sel_rows_5730 WITH KEY index = sy-tabix."INDEX SY-TABIX.
        IF sy-subrc IS INITIAL.
          wa_sol_5730-name1 = 'DELETE'.
          MODIFY it_sol_5730 FROM wa_sol_5730 INDEX wa_sel_rows_5730-index.

          CONCATENATE wa_sol_5730-nro_sol wa_sol_5730-seq wa_sol_5730-vbeln wa_sol_5730-posnr INTO chave.

          CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
            EXPORTING
              chave = chave.

        ENDIF.
      ENDLOOP.

      DELETE it_sol_5730 WHERE name1 EQ 'DELETE'.

      vg_subt_lote = '5730'.
      LEAVE TO SCREEN 5000.

    ENDIF.
  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION


*-CS2021000218-30.08.2022-#893743-JT-inicio
*&---------------------------------------------------------------------*
*&      Module  STATUS_5730  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5730_trata_cpf OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'WA_HEADER_CARGAD-CPF_RTC'.
*-CS2021000218-05.10.2022-#91290-JT-inicio
      IF it_sol_5730[] IS INITIAL.
        screen-input             = '0'.
        wa_header_cargad-cpf_rtc = abap_off.
        wa_header_cargad-cod_tr  = abap_off.
        wa_header_cargad-name2   = abap_off.
*-CS2021000218-05.10.2022-#91290-JT-inicio
      ELSEIF w_rtc_terceiro = abap_true.
        screen-input             = '0'.
        wa_header_cargad-cpf_rtc = abap_off.
      ELSE.
        screen-input             = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.
*-CS2021000218-30.08.2022-#893743-JT-fim

*&---------------------------------------------------------------------*
*&      Module  STATUS_5730  OUTPUT
*&---------------------------------------------------------------------*

MODULE status_5730 OUTPUT.

*-CS2021000218-05.10.2022-#91290-JT-inicio
  LOOP AT SCREEN.
    IF screen-group1 = 'AA1'.
      IF it_sol_5730[] IS INITIAL.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*-CS2021000218-05.10.2022-#91290-JT-fim

  IF  sy-ucomm EQ 'NOVACARGA'.
    PERFORM clear_5730.
  ENDIF.

  IF g_custom_container_5730 IS INITIAL.

    CREATE OBJECT g_custom_container_5730
      EXPORTING
        container_name              = 'CONTAINER5730'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.


    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_sol_5730 USING:
          01 'NRO_SOL'       'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
          02 'VBELN'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ordem',
          03 'POSNR'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item',
          04 'AUART'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tipo OV',
          05 'INCO1'         'VBKD'      ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Incoterm',
          06 'VKORG'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Org. Vendas',
          07 'SPART'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Setor Atv',
          08 'WERKS'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Centro Forn',
          09 'MATNR'         'MARA'      ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Material',
          10 'MAKTX'         'MAKT'      ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Descrição',
          11 'QTE_LIB'       ' '         ' '     ' '  ' '  ' '   'X'   ' '   'X'   ' '   'Qtd. Distrib.',
          12 'MEINS'         'VBAP'      ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.',
          13 'NR_ROT'        'ZSDT0132'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Roteiro'.

    gs_layout_5730-sel_mode   = 'A'.
    gs_layout_5730-stylefname = 'CELLSTYLES'.
    gs_layout_5730-cwidth_opt = 'X'.
    gs_layout_5730-info_fname = 'COR'.
    gs_layout_5730-smalltitle = 'X'.
    gs_layout_5730-grid_title = 'Ordens com Solicitação de Entrega'.

    CREATE OBJECT ctl_alv1_5730
      EXPORTING
        i_parent = g_custom_container_5730. "DG_PARENT_1_5730.           "ALV Solicitações

    PERFORM excluir_botoes CHANGING it_exclude_5730.
*    PERFORM REGISTRAR_F4_5730.

    SET HANDLER:
      lcl_event_handler_5730=>toolbar_5730 FOR ctl_alv1_5730,
      lcl_event_handler_5730=>user_command_5730 FOR ctl_alv1_5730.

    CALL METHOD ctl_alv1_5730->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5730
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5730
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_sol_5730
        it_outtab            = it_sol_5730.

    CALL METHOD ctl_alv1_5730->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ELSE.

    it_stable_5730-row = 'X'.
    it_stable_5730-col = 'X'.

    CALL METHOD ctl_alv1_5730->refresh_table_display
      EXPORTING
        is_stable = it_stable_5730.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5730  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5730 INPUT.

  ctl_alv1_5730->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'F85730'.
      CALL SCREEN 5731 STARTING AT 5 5 ENDING AT 160 30.
    WHEN 'SAVE'.
      PERFORM salva_carga_5730.
    WHEN 'ENTER'.
      PERFORM completa_motorista_5730.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SEQARCH HELP
*&---------------------------------------------------------------------*
MODULE z_help_cpf_rtc_5730.

  PERFORM f_search_cpf_rtc USING '5730'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SALVA_LOTE
*&---------------------------------------------------------------------*
FORM salva_carga_5730.

  DATA: wa_zsdt0139   TYPE zsdt0139,
        wa_zsdt0140   TYPE zsdt0140,
        wa_zsdt0082   TYPE zsdt0082,
        wa_zsdt0259   TYPE zsdt0259,
        wa_zsdt0266   TYPE zsdt0266,
        wa_sol_5730   TYPE ty_solicitacao_5730,
        vl_check      TYPE char1,
        vl_nao_obriga TYPE c.

*-CS2021000218-08.12.2022-#97470 -JT-inicio
  FREE: vl_nao_obriga.

  LOOP AT it_sol_5730 INTO wa_sol_5730.
    SELECT revenda
      INTO @DATA(l_revenda)
      FROM zsdt0216
        UP TO 1 ROWS
     WHERE bukrs           = @wa_sol_5730-vkorg
       AND kunnr           = @wa_sol_5730-kunnr
       AND setor_atividade = 'A'
       AND revenda         = 'S'
       AND tipo            = 'C'.
    ENDSELECT.

    IF sy-subrc = 0.
      vl_nao_obriga = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.
*-CS2021000218-08.12.2022-#97470 -JT-fim

  "Check cabeçalho em branco
  IF ( wa_header_cargad-cod_ce IS INITIAL AND vl_nao_obriga = abap_false ). "*-CS2021000218-08.12.2022-#97470 -JT
* IF ( wa_header_cargad-cod_ce IS INITIAL ). " OR
*      wa_header_cargad-cod_tr IS INITIAL ). "*-CS2021000218-05.10.2022-#91290-JT-inicio
    MESSAGE TEXT-013 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
    EXIT.
  ENDIF.

  "Check lote sem solicitação
  IF it_sol_5730 IS INITIAL.
    MESSAGE TEXT-014 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
    EXIT.
  ENDIF.

*-CS2021000218-05.10.2022-#91290-JT-inicio
*-validar cod transportadora
  READ TABLE it_sol_5730 INTO wa_sol_5730 INDEX 1.
  IF sy-subrc = 0.
    IF wa_header_cargad-cod_tr IS INITIAL.
      IF wa_sol_5730-inco1 <> 'FOB'.
        MESSAGE TEXT-200 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
        EXIT.
      ENDIF.
    ELSE.
      SELECT SINGLE ktokk
               INTO @DATA(l_ktokk)
               FROM lfa1
              WHERE lifnr = @wa_header_cargad-cod_tr.
      IF sy-subrc <> 0.
        MESSAGE TEXT-202 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
        EXIT.
      ENDIF.
      IF wa_sol_5730-inco1 = 'FOB'.
        IF l_ktokk = 'ZFIC'.
          MESSAGE TEXT-201 TYPE 'S' DISPLAY LIKE 'E'.
          vl_check = abap_true.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*-CS2021000218-05.10.2022-#91290-JT-fim

  "Check se Carga com Solicitações CIF estão com Motorista e Placas inseridas e se placas são válidas
  READ TABLE it_sol_5730 INTO wa_sol_5730 INDEX 1.
  IF sy-subrc IS INITIAL.
    IF wa_sol_5730-inco1 EQ 'CIF'.
      IF wa_header_cargad-cod_mt    IS INITIAL OR
         wa_header_cargad-placa_cav IS INITIAL.
        MESSAGE TEXT-127 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
        EXIT.
      ELSE.
        PERFORM valida_placa_defensivos USING wa_header_cargad-placa_cav '0'
                                       CHANGING vl_check.

        IF wa_header_cargad-placa_car1 IS NOT INITIAL.
          PERFORM valida_placa_defensivos USING wa_header_cargad-placa_car1 '1'
                                          CHANGING vl_check.
        ENDIF.

        IF wa_header_cargad-placa_car2 IS NOT INITIAL.
          PERFORM valida_placa_defensivos USING wa_header_cargad-placa_car2 '1'
                                          CHANGING vl_check.
        ENDIF.

*-CS2019001891 - JT - 04.02.2021 - inicio
        IF wa_header_cargad-placa_car3 IS NOT INITIAL.
          PERFORM valida_placa_defensivos USING wa_header_cargad-placa_car3 '1'
                                          CHANGING vl_check.
        ENDIF.
*-CS2019001891 - JT - 04.02.2021 - fim

      ENDIF.
    ELSE.
      CLEAR: wa_header_cargad-cod_mt,
             wa_header_cargad-placa_cav,
             wa_header_cargad-placa_car1,
             wa_header_cargad-placa_car2,
             wa_header_cargad-placa_car3.
    ENDIF.
  ENDIF.

  CHECK vl_check = abap_off.

*-CS2021000218-30.08.2022-#893743-JT-inicio
  IF w_rtc_proprio = abap_true.
    wa_header_cargad-tipo_rtc = 'P'.
  ELSE.
    wa_header_cargad-tipo_rtc = 'T'.
    wa_header_cargad-cpf_rtc  = abap_off.
  ENDIF.
*-CS2021000218-30.08.2022-#893743-JT-fim

*-CS2021000218-05.10.2022-#91290-JT-inicio
*-monta range centro
  FREE: r_werks.
  LOOP AT it_sol_5730 INTO wa_sol_5730.
    r_werks-sign   = 'I'.
    r_werks-option = 'EQ'.
    r_werks-low    = wa_sol_5730-werks.
    APPEND r_werks.
  ENDLOOP.
  IF r_werks[] IS INITIAL.
    r_werks-sign   = 'I'.
    r_werks-option = 'EQ'.
    r_werks-low    = '9999'.
    APPEND r_werks.
  ENDIF.
  SORT r_werks BY low.
  DELETE ADJACENT DUPLICATES FROM r_werks
                        COMPARING low.
*-CS2021000218-05.10.2022-#91290-JT-fim


  IF l_revenda <> 'S'. "Alexandre Rimini - CS1097281 - Verificar cliente revenda. - 26.05.2023
*-CS2019001891 - JT - 04.02.2021 - inicio
    IF wa_header_cargad-cpf_rtc  IS NOT INITIAL   AND
       wa_header_cargad-cpf_rtc  <> '00000000000' AND
       wa_header_cargad-tipo_rtc  = 'P'.
      SELECT SINGLE *
               INTO wa_zsdt0259
               FROM zsdt0259
              WHERE cpf = wa_header_cargad-cpf_rtc.

      IF sy-subrc <> 0.
        MESSAGE TEXT-160 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
        EXIT.
      ELSEIF wa_zsdt0259-status = 'N'.
        MESSAGE TEXT-161 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
        EXIT.
      ELSE.
        SELECT SINGLE *
                 INTO wa_zsdt0266
                 FROM zsdt0266
                WHERE cpf     = wa_header_cargad-cpf_rtc
                  AND werks  IN r_werks.  "*-CS2021000218-05.10.2022-#91290-JT-fim
        IF sy-subrc <> 0.
          MESSAGE TEXT-162 TYPE 'S' DISPLAY LIKE 'E'.
          vl_check = abap_true.
          EXIT.
        ENDIF.
      ENDIF.
    ELSE.
      IF wa_header_cargad-tipo_rtc = 'P'.
        MESSAGE TEXT-180 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF. " Fim alteracao Alexandre - 26.05.2023
*-CS2019001891 - JT - 04.02.2021 - fim

  IF vl_check IS INITIAL.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSD_INS_CD'
      IMPORTING
        number                  = wa_zsdt0139-nro_cgd
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    wa_zsdt0139-cod_ce     = wa_header_cargad-cod_ce.
    wa_zsdt0139-cod_tr     = wa_header_cargad-cod_tr.
    wa_zsdt0139-cod_mt     = wa_header_cargad-cod_mt.
    wa_zsdt0139-cod_ar     = wa_header_cargad-cod_ar.
    wa_zsdt0139-placa_cav  = wa_header_cargad-placa_cav.
    wa_zsdt0139-placa_car1 = wa_header_cargad-placa_car1.
    wa_zsdt0139-placa_car2 = wa_header_cargad-placa_car2.
    wa_zsdt0139-placa_car3 = wa_header_cargad-placa_car3.
    wa_zsdt0139-cpf_rtc    = wa_header_cargad-cpf_rtc.
    wa_zsdt0139-tipo_rtc   = wa_header_cargad-tipo_rtc. "*-CS2021000218-30.08.2022-#893743-JT-inicio
    wa_zsdt0139-status     = 1. "0.                     "*-CS2021000218-08.12.2022-#97470 -JT
    wa_zsdt0139-usnam      = sy-uname.
    wa_zsdt0139-data_atual = sy-datum.
    wa_zsdt0139-hora_atual = sy-uzeit.
    MODIFY zsdt0139 FROM wa_zsdt0139.

    LOOP AT it_sol_5730 INTO wa_sol_5730.

      wa_zsdt0140-nro_cgd = wa_zsdt0139-nro_cgd.
      wa_zsdt0140-nro_sol = wa_sol_5730-nro_sol.
      wa_zsdt0140-seq = wa_sol_5730-seq.
      wa_zsdt0140-inco1 = wa_sol_5730-inco1.
      wa_zsdt0140-kunnr = wa_sol_5730-kunnr.
      wa_zsdt0140-matnr = wa_sol_5730-matnr.
      wa_zsdt0140-vbeln = wa_sol_5730-vbeln.
      wa_zsdt0140-posnr = wa_sol_5730-posnr.
      wa_zsdt0140-meins = wa_sol_5730-meins.
      wa_zsdt0140-status = wa_sol_5730-status.
      wa_zsdt0140-usnam = sy-uname.
      wa_zsdt0140-data_atual = sy-datum.
      wa_zsdt0140-hora_atual = sy-uzeit.
      MODIFY zsdt0140 FROM wa_zsdt0140.

      SELECT SINGLE *
        FROM zsdt0082
        INTO wa_zsdt0082
        WHERE nro_sol = wa_sol_5730-nro_sol
          AND seq     = wa_sol_5730-seq
          AND vbeln   = wa_sol_5730-vbeln
          AND posnr   = wa_sol_5730-posnr.

      IF sy-subrc IS INITIAL.
        wa_zsdt0082-status = '5'.
      ENDIF.

      MODIFY zsdt0082 FROM wa_zsdt0082.

    ENDLOOP.

    PERFORM desbloqueia_sol_5730.
    PERFORM clear_5730.

    MESSAGE s000(z_fi) WITH 'Carga' wa_zsdt0139-nro_cgd 'salvo com sucesso' .

    CLEAR: wa_zsdt0082.
    PERFORM clear_5720.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_5730
*&---------------------------------------------------------------------*
FORM clear_5730 .
  CLEAR: it_sol_5730, wa_header_cargad.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_MOTORISTA_5730
*&---------------------------------------------------------------------*
FORM completa_motorista_5730 .

  DATA: wa_lfa1 TYPE lfa1.

  SELECT SINGLE *
    FROM lfa1
    INTO  wa_lfa1
    WHERE lifnr EQ wa_header_cargad-cod_ce.

  wa_header_cargad-name1 = wa_lfa1-name1.

  CLEAR: wa_lfa1.

  SELECT SINGLE *
    FROM lfa1
    INTO  wa_lfa1
    WHERE lifnr EQ wa_header_cargad-cod_tr.

  wa_header_cargad-name2 = wa_lfa1-name1.

  CLEAR: wa_lfa1.

  SELECT SINGLE *
    FROM lfa1
    INTO  wa_lfa1
    WHERE lifnr EQ wa_header_cargad-cod_mt.

  wa_header_cargad-name3 = wa_lfa1-name1.

  CLEAR: wa_lfa1.

  SELECT SINGLE *
    FROM lfa1
    INTO  wa_lfa1
    WHERE lifnr EQ wa_header_cargad-cod_ar.

  wa_header_cargad-name4 = wa_lfa1-name1.

*-CS2019001891 - JT - 04.02.2021 - inicio
  CLEAR wa_header_cargad-nome_rtc.

  SELECT SINGLE nome
    FROM zsdt0259
    INTO wa_header_cargad-nome_rtc
   WHERE cpf EQ wa_header_cargad-cpf_rtc.
*-CS2019001891 - JT - 04.02.2021 - fim

  l_cpf_rtc = wa_header_cargad-cpf_rtc.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DESBLOQUEIA_SOL_5730
*&---------------------------------------------------------------------*
FORM desbloqueia_sol_5730.

  DATA: wa_sol_5730 TYPE ty_solicitacao_5730,
        chave       TYPE zde_chave_sol.


  LOOP AT it_sol_5730 INTO wa_sol_5730.

    CONCATENATE wa_sol_5730-nro_sol wa_sol_5730-seq wa_sol_5730-vbeln wa_sol_5730-posnr INTO chave.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        chave = chave.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VALIDA_PLACA_DEFENSIVOS
*&---------------------------------------------------------------------*
FORM valida_placa_defensivos USING VALUE(p_placa)
                                   VALUE(p_tpveiculo)
                                CHANGING vl_check TYPE char1.

  DATA: wa_zlest0002 TYPE zlest0002,
        vl_renavam   TYPE zlest0002-cd_renavam.

  SELECT SINGLE *
        FROM zlest0002
        INTO wa_zlest0002
        WHERE pc_veiculo EQ p_placa
          AND tp_veiculo EQ p_tpveiculo.

  IF wa_zlest0002 IS NOT INITIAL.
    IF wa_zlest0002-qt_eixo = 0.
      MESSAGE TEXT-080 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
    ELSE.
      vl_renavam = strlen( wa_zlest0002-cd_renavam ).
      IF vl_renavam NOT BETWEEN 9 AND 11.
        MESSAGE TEXT-081 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE TEXT-082 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
  ENDIF.

ENDFORM.
