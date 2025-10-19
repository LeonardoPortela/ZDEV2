*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5740
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_sol_5740,
         check      TYPE char1,
         matnr      TYPE mara-matnr,
         maktx      TYPE makt-maktx,
         meins      TYPE vbap-meins,
         kunnr      TYPE vbpa-kunnr,
         name1      TYPE kna1-name1,
         inco1      TYPE vbkd-inco1,
         cor(4)     TYPE c.
         INCLUDE      STRUCTURE zsdt0082.
         TYPES: cellstyles TYPE lvc_t_styl.
TYPES: END OF ty_sol_5740.

DATA: it_sol_5740     TYPE STANDARD TABLE OF ty_sol_5740.

DATA: g_custom_container_5740  TYPE REF TO cl_gui_custom_container,
      ctl_alv1_5740            TYPE REF TO cl_gui_alv_grid,
      gs_layout_5740           TYPE lvc_s_layo,
      it_fieldcatalog_sol_5740 TYPE lvc_t_fcat,
      it_exclude_5740          TYPE ui_functions,
      it_stable_5740           TYPE lvc_s_stbl.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5740 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      toolbar_5740 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command_5740 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_5740 IMPLEMENTATION.

  METHOD toolbar_5740.

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

  METHOD user_command_5740.

    IF e_ucomm = 'DEL'.

      DATA: it_sel_rows_5740 TYPE lvc_t_row,
            wa_sel_rows_5740 TYPE lvc_s_row,
            wa_sol_5740      TYPE ty_sol_5740,
            vl_cont          TYPE i,
            chave            TYPE zde_chave_sol.

      CALL METHOD ctl_alv1_5740->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows_5740.

      LOOP AT it_sol_5740 INTO wa_sol_5740.
        READ TABLE it_sel_rows_5740 INTO wa_sel_rows_5740 WITH KEY index = sy-tabix."INDEX SY-TABIX.
        IF sy-subrc IS INITIAL.
          wa_sol_5740-name1 = 'DELETE'.
          MODIFY it_sol_5740 FROM wa_sol_5740 INDEX wa_sel_rows_5740-index.

          CONCATENATE wa_sol_5740-nro_sol wa_sol_5740-seq wa_sol_5740-vbeln wa_sol_5740-posnr INTO chave.

          CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
            EXPORTING
              chave = chave.

        ENDIF.
      ENDLOOP.

      DELETE it_sol_5740 WHERE name1 EQ 'DELETE'.

      vg_subt_lote = '5740'.
      LEAVE TO SCREEN 5000.

    ENDIF.
  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*-CS2021000218-30.08.2022-#893743-JT-inicio
*&---------------------------------------------------------------------*
*&      Module  STATUS_5730  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5740_trata_cpf OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'WA_HEADER_CARGAD-CPF_RTC'.
*-CS2021000218-05.10.2022-#91290-JT-inicio
      IF it_sol_5740[] IS INITIAL.
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
        wa_header_cargad-cpf_rtc = l_cpf_rtc.
      ENDIF.
      MODIFY SCREEN.

      CLEAR wa_header_cargad-nome_rtc.
      SELECT SINGLE nome
        FROM zsdt0259
        INTO wa_header_cargad-nome_rtc
       WHERE cpf EQ wa_header_cargad-cpf_rtc.
    ENDIF.
  ENDLOOP.

ENDMODULE.
*-CS2021000218-30.08.2022-#893743-JT-fim

*&---------------------------------------------------------------------*
*&      Module  STATUS_5740  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5740 OUTPUT.

*-CS2021000218-05.10.2022-#91290-JT-inicio
  LOOP AT SCREEN.
    IF screen-group2 = 'AA1'.
      IF it_sol_5740[] IS INITIAL.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*-CS2021000218-05.10.2022-#91290-JT-fim

  IF g_custom_container_5740 IS INITIAL.

    CREATE OBJECT g_custom_container_5740
      EXPORTING
        container_name              = 'CONTAINER5740'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_sol_5740 USING:
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

    gs_layout_5740-sel_mode   = 'A'.
    gs_layout_5740-stylefname = 'CELLSTYLES'.
    gs_layout_5740-cwidth_opt = 'X'.
    gs_layout_5740-info_fname = 'COR'.
    gs_layout_5740-smalltitle = 'X'.
    gs_layout_5740-grid_title = 'Ordens com Solicitação de Entrega'.

    CREATE OBJECT ctl_alv1_5740
      EXPORTING
        i_parent = g_custom_container_5740.           "ALV Lote

    PERFORM excluir_botoes CHANGING it_exclude_5740.

    SET HANDLER:
      lcl_event_handler_5740=>toolbar_5740 FOR ctl_alv1_5740,
      lcl_event_handler_5740=>user_command_5740 FOR ctl_alv1_5740.

    CALL METHOD ctl_alv1_5740->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5740
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5740
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_sol_5740
        it_outtab            = it_sol_5740.

    CALL METHOD ctl_alv1_5740->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ELSE.

    it_stable_5740-row = 'X'.
    it_stable_5740-col = 'X'.

    CALL METHOD ctl_alv1_5740->refresh_table_display
      EXPORTING
        is_stable = it_stable_5740.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5740  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5740 INPUT.

  ctl_alv1_5740->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'F85740'.
      CALL SCREEN 5741 STARTING AT 5 5 ENDING AT 160 30.
    WHEN 'SAVE'.
      PERFORM salva_carga_5740.
    WHEN 'DELETECG'.
      PERFORM delete_carga.
    WHEN 'ENTER'.
      PERFORM completa_motorista_5740.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SEQARCH HELP
*&---------------------------------------------------------------------*
MODULE z_help_cpf_rtc_5740.

  PERFORM f_search_cpf_rtc USING '5740'.

ENDMODULE.

**&---------------------------------------------------------------------*
**&      Form  SALVA_LOTE
**&---------------------------------------------------------------------*
FORM salva_carga_5740.

  DATA: it_zsdt0140_ex TYPE STANDARD TABLE OF zsdt0140,
        wa_zsdt0139    TYPE zsdt0139,
        wa_zsdt0140    TYPE zsdt0140,
        wa_zsdt0082    TYPE zsdt0082,
        wa_zsdt0259    TYPE zsdt0259,
        wa_zsdt0266    TYPE zsdt0266,
        wa_sol_5740    TYPE ty_sol_5740,
        vl_check       TYPE char1,
        vl_nao_obriga  TYPE c.

*-CS2021000218-08.12.2022-#97470 -JT-inicio
  FREE: vl_nao_obriga.

  LOOP AT it_sol_5740 INTO wa_sol_5740.
    SELECT revenda
      INTO @DATA(l_revenda)
      FROM zsdt0216
        UP TO 1 ROWS
     WHERE bukrs           = @wa_sol_5740-vkorg
       AND kunnr           = @wa_sol_5740-kunnr
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

*-CS2021000218-08.12.2022-#97470 -JT-inicio
*-salvar carga ja aprovada
  vl_status = '1'.
*-CS2021000218-08.12.2022-#97470 -JT-fim

  "Check cabeçalho em branco
  IF ( wa_header_cargad-cod_ce IS INITIAL AND vl_nao_obriga = abap_false ). "*-CS2021000218-08.12.2022-#97470 -JT
* IF ( wa_header_cargad-cod_ce IS INITIAL ). " OR
*      wa_header_cargad-cod_tr IS INITIAL ). "*-CS2021000218-05.10.2022-#91290-JT-inicio
    MESSAGE text-013 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
    EXIT.
  ENDIF.

  "Check lote sem solicitação
  IF it_sol_5740 IS INITIAL.
    MESSAGE text-014 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
    EXIT.
  ENDIF.

*-CS2021000218-05.10.2022-#91290-JT-inicio
*-validar cod transportadora
  READ TABLE it_sol_5740 INTO wa_sol_5740 INDEX 1.
  IF sy-subrc = 0.
    IF wa_header_cargad-cod_tr IS INITIAL.
      IF wa_sol_5740-inco1 <> 'FOB'.
        MESSAGE text-200 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
        EXIT.
      ENDIF.
    ELSE.
      SELECT SINGLE ktokk
              INTO @DATA(l_ktokk)
              FROM lfa1
             WHERE lifnr = @wa_header_cargad-cod_tr.
      IF sy-subrc <> 0.
        MESSAGE text-202 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
        EXIT.
      ENDIF.
      IF wa_sol_5740-inco1 = 'FOB'.
        IF l_ktokk = 'ZFIC'.
          MESSAGE text-201 TYPE 'S' DISPLAY LIKE 'E'.
          vl_check = abap_true.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*-CS2021000218-05.10.2022-#91290-JT-fim

  "Check se Carga com Solicitações CIF estão com Motorista e Placas inseridas e se placas são válidas
  READ TABLE it_sol_5740 INTO wa_sol_5740 INDEX 1.
  IF sy-subrc IS INITIAL.
    IF wa_sol_5740-inco1 EQ 'CIF'.
      IF wa_header_cargad-cod_mt    IS INITIAL OR
         wa_header_cargad-placa_cav IS INITIAL.
        MESSAGE text-127 TYPE 'S' DISPLAY LIKE 'E'.
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
  LOOP AT it_sol_5740 INTO wa_sol_5740.
    r_werks-sign   = 'I'.
    r_werks-option = 'EQ'.
    r_werks-low    = wa_sol_5740-werks.
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

*-CS2019001891 - JT - 04.02.2021 - inicio
  IF wa_header_cargad-cpf_rtc  IS NOT INITIAL   AND
     wa_header_cargad-cpf_rtc  <> '00000000000' AND
     wa_header_cargad-tipo_rtc  = 'P'.
    SELECT SINGLE *
             INTO wa_zsdt0259
             FROM zsdt0259
            WHERE cpf = wa_header_cargad-cpf_rtc.

    IF sy-subrc <> 0.
      MESSAGE text-160 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
      EXIT.
    ELSEIF wa_zsdt0259-status = 'N'.
      MESSAGE text-161 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
      EXIT.
    ELSE.
      SELECT SINGLE *
               INTO wa_zsdt0266
               FROM zsdt0266
              WHERE cpf     = wa_header_cargad-cpf_rtc
                AND werks  IN r_werks.  "*-CS2021000218-05.10.2022-#91290-JT-fim
      IF sy-subrc <> 0.
        MESSAGE text-162 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
        EXIT.
      ENDIF.
    ENDIF.
  ELSE.
    IF wa_header_cargad-tipo_rtc = 'P'.
      MESSAGE text-180 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
      EXIT.
    ENDIF.
  ENDIF.
*-CS2019001891 - JT - 04.02.2021 - fim

  IF vl_check IS INITIAL.

*    WA_ZSDT0139-NRO_CGD    = VG_LOTE_EDITAR.
*    WA_ZSDT0139-COD_CE     = WA_HEADER_CARGAD-COD_CE.
*    WA_ZSDT0139-COD_TR     = WA_HEADER_CARGAD-COD_TR.
*    WA_ZSDT0139-STATUS     = VL_STATUS.
*    WA_ZSDT0139-USNAM      = SY-UNAME.
*    WA_ZSDT0139-DATA_ATUAL = SY-DATUM.
*    WA_ZSDT0139-HORA_ATUAL = SY-UZEIT.
*    MODIFY ZSDT0139 FROM WA_ZSDT0139.

    UPDATE zsdt0139 SET cod_ce  = wa_header_cargad-cod_ce
                        cod_tr  = wa_header_cargad-cod_tr
                         cod_mt = wa_header_cargad-cod_mt
                         cod_ar = wa_header_cargad-cod_ar
                      placa_cav = wa_header_cargad-placa_cav
                     placa_car1 = wa_header_cargad-placa_car1
                     placa_car2 = wa_header_cargad-placa_car2
*-CS2019001891 - JT - 04.02.2021 - inicio
                     placa_car3 = wa_header_cargad-placa_car3
                        cpf_rtc = wa_header_cargad-cpf_rtc
*-CS2019001891 - JT - 04.02.2021 - fim
                       tipo_rtc = wa_header_cargad-tipo_rtc "*-CS2021000218-30.08.2022-#893743-JT
                        status  = vl_status
                  WHERE nro_cgd = vg_lote_editar.

    LOOP AT it_sol_5740 INTO wa_sol_5740.

      wa_zsdt0140-nro_cgd = vg_lote_editar."WA_ZSDT0139-NRO_CGD.
      wa_zsdt0140-nro_sol = wa_sol_5740-nro_sol.
      wa_zsdt0140-seq = wa_sol_5740-seq.
      wa_zsdt0140-inco1 = wa_sol_5740-inco1.
      wa_zsdt0140-kunnr = wa_sol_5740-kunnr.
      wa_zsdt0140-vbeln = wa_sol_5740-vbeln.
      wa_zsdt0140-posnr = wa_sol_5740-posnr.
      wa_zsdt0140-matnr = wa_sol_5740-matnr.
      wa_zsdt0140-meins = wa_sol_5740-meins.
      wa_zsdt0140-status = wa_sol_5740-status.
      wa_zsdt0140-usnam = sy-uname.
      wa_zsdt0140-data_atual = sy-datum.
      wa_zsdt0140-hora_atual = sy-uzeit.
      MODIFY zsdt0140 FROM wa_zsdt0140.

      SELECT SINGLE *
        FROM zsdt0082
        INTO wa_zsdt0082
        WHERE nro_sol = wa_sol_5740-nro_sol
          AND seq     = wa_sol_5740-seq
          AND vbeln   = wa_sol_5740-vbeln
          AND posnr   = wa_sol_5740-posnr.

      IF sy-subrc IS INITIAL.
        wa_zsdt0082-status = '5'.
      ENDIF.

      MODIFY zsdt0082 FROM wa_zsdt0082.

    ENDLOOP.

    SELECT *
      FROM zsdt0140
      INTO TABLE it_zsdt0140_ex
      WHERE nro_cgd EQ vg_lote_editar
        AND status NE 'X'.

    LOOP AT it_zsdt0140_ex INTO wa_zsdt0140.
      READ TABLE it_sol_5740 INTO wa_sol_5740 WITH KEY nro_sol = wa_zsdt0140-nro_sol
                                                           seq = wa_zsdt0140-seq.
      IF sy-subrc IS NOT INITIAL AND wa_zsdt0140-status NE 'X'.

        wa_zsdt0140-status = 'X'.
        wa_zsdt0140-user_canc = sy-uname.
        wa_zsdt0140-dt_canc = sy-datum.
        wa_zsdt0140-hr_can = sy-uzeit.
        MODIFY zsdt0140 FROM wa_zsdt0140.

        SELECT SINGLE *
          FROM zsdt0082
          INTO wa_zsdt0082
          WHERE nro_sol = wa_zsdt0140-nro_sol
            AND seq     = wa_zsdt0140-seq.

        wa_zsdt0082-status = '2'.
        MODIFY zsdt0082 FROM wa_zsdt0082.
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
      EXPORTING
        chave = vg_lote_editar.

    PERFORM desbloqueia_sol_5740.
    PERFORM clear_5740.
    MESSAGE s000(z_fi) WITH 'Carga' vg_lote_editar 'salva com sucesso' .
    CLEAR: wa_zsdt0082.
    PERFORM clear_5720.
    vg_subt_lote = '5720'.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_5740
*&---------------------------------------------------------------------*
FORM clear_5740 .
  CLEAR: it_sol_5740, wa_header_cargad.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_HEADER_5740
*&---------------------------------------------------------------------*
FORM completa_header_5740 .

  DATA: wa_zsdt0139 TYPE zsdt0139,
        it_zsdt0140 TYPE STANDARD TABLE OF zsdt0140,
        wa_zsdt0140 TYPE zsdt0140,
        wa_sol_5740 TYPE ty_sol_5740,
        it_lfa1     TYPE STANDARD TABLE OF lfa1,
        wa_lfa1     TYPE lfa1,
        it_kna1     TYPE STANDARD TABLE OF kna1,
        wa_kna1     TYPE kna1,
        it_makt     TYPE STANDARD TABLE OF makt,
        wa_makt     TYPE makt,
        it_vbpa     TYPE STANDARD TABLE OF vbpa,
        wa_vbpa     TYPE vbpa,
        it_vbap     TYPE STANDARD TABLE OF vbap,
        wa_vbap     TYPE vbap,
        vl_cont     TYPE i.

  DATA: ls_style TYPE lvc_s_styl,
        wl_name  TYPE thead-tdname,
        tg_texto TYPE STANDARD TABLE OF tline.

  SELECT SINGLE *
    FROM zsdt0139
    INTO wa_zsdt0139
    WHERE nro_cgd EQ vg_lote_editar.

  IF wa_zsdt0139 IS NOT INITIAL.

    SELECT *
      FROM lfa1
      INTO TABLE it_lfa1
      WHERE lifnr EQ wa_zsdt0139-cod_ce.

    SELECT *
      FROM lfa1
      APPENDING TABLE it_lfa1
      WHERE lifnr EQ wa_zsdt0139-cod_tr.

    SELECT *
      FROM lfa1
      APPENDING TABLE it_lfa1
      WHERE lifnr EQ wa_zsdt0139-cod_mt.

    SELECT *
      FROM lfa1
      APPENDING TABLE it_lfa1
      WHERE lifnr EQ wa_zsdt0139-cod_ar.

  ENDIF.

  SELECT *
      FROM zsdt0140
      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0140
      WHERE nro_cgd EQ vg_lote_editar
        AND status NE 'X'.

  IF it_zsdt0140 IS NOT INITIAL.



    SELECT *
      FROM zsdt0082
      INTO CORRESPONDING FIELDS OF TABLE it_sol_5740
      FOR ALL ENTRIES IN it_zsdt0140
      WHERE nro_sol EQ it_zsdt0140-nro_sol
        AND seq     EQ it_zsdt0140-seq.

    IF it_sol_5740 IS NOT INITIAL.

      SELECT *
        FROM vbap
        INTO TABLE it_vbap
        FOR ALL ENTRIES IN it_sol_5740
        WHERE vbeln EQ it_sol_5740-vbeln
          AND posnr EQ it_sol_5740-posnr.

      SELECT *
        FROM vbpa
        INTO TABLE it_vbpa
        FOR ALL ENTRIES IN it_sol_5740
        WHERE vbeln EQ it_sol_5740-vbeln
          AND parvw EQ 'AG'.

      IF it_vbap IS NOT INITIAL.

        SELECT *
                FROM makt
                INTO TABLE it_makt
                FOR ALL ENTRIES IN it_vbap
                WHERE matnr EQ it_vbap-matnr.

      ENDIF.

      IF it_vbpa IS NOT INITIAL.

        SELECT *
              FROM kna1
              INTO TABLE it_kna1
              FOR ALL ENTRIES IN it_vbpa
              WHERE kunnr EQ it_vbpa-kunnr.

      ENDIF.

    ENDIF.

  ENDIF.

  "Atualiza Cabeçalho
  CLEAR: vl_status, l_cpf_rtc.

  wa_header_cargad-cod_ce     = wa_zsdt0139-cod_ce.
  wa_header_cargad-cod_tr     = wa_zsdt0139-cod_tr.
  wa_header_cargad-cod_mt     = wa_zsdt0139-cod_mt.
  wa_header_cargad-cod_ar     = wa_zsdt0139-cod_ar.
  wa_header_cargad-placa_cav  = wa_zsdt0139-placa_cav.
  wa_header_cargad-placa_car1 = wa_zsdt0139-placa_car1.
  wa_header_cargad-placa_car2 = wa_zsdt0139-placa_car2.
*-CS2019001891 - JT - 04.02.2021 - inicio
  wa_header_cargad-placa_car3 = wa_zsdt0139-placa_car3.
  wa_header_cargad-cpf_rtc    = wa_zsdt0139-cpf_rtc.
*-CS2019001891 - JT - 04.02.2021 - fim

*-CS2021000218-30.08.2022-#893743-JT-inicio
  l_cpf_rtc = wa_zsdt0139-cpf_rtc.
  IF wa_zsdt0139-tipo_rtc = 'T'.
    w_rtc_proprio             = abap_false.
    w_rtc_terceiro            = abap_true.
    wa_header_cargad-tipo_rtc = 'T'.
  ELSE.
    w_rtc_proprio             = abap_true.
    w_rtc_terceiro            = abap_false.
    wa_header_cargad-tipo_rtc = 'P'.
  ENDIF.
*-CS2021000218-30.08.2022-#893743-JT-fim

  READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_header_cargad-cod_ce.
  IF sy-subrc IS INITIAL.
    wa_header_cargad-name1 = wa_lfa1-name1.
  ENDIF.

  READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_header_cargad-cod_tr.
  IF sy-subrc IS INITIAL.
    wa_header_cargad-name2 = wa_lfa1-name1.
  ENDIF.

  READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_header_cargad-cod_mt.
  IF sy-subrc IS INITIAL.
    wa_header_cargad-name3 = wa_lfa1-name1.
  ENDIF.

  READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_header_cargad-cod_ar.
  IF sy-subrc IS INITIAL.
    wa_header_cargad-name4 = wa_lfa1-name1.
  ENDIF.

*-CS2019001891 - JT - 04.02.2021 - inicio
  CLEAR wa_header_cargad-nome_rtc.

  SELECT SINGLE nome
    FROM zsdt0259
    INTO wa_header_cargad-nome_rtc
   WHERE cpf EQ wa_header_cargad-cpf_rtc.
*-CS2019001891 - JT - 04.02.2021 - fim

  vl_status = wa_zsdt0139-status.

  "Atualiza Solicitações
  LOOP AT it_sol_5740 INTO wa_sol_5740.
    vl_cont = vl_cont + 1.

    READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_sol_5740-vbeln
                                             posnr = wa_sol_5740-posnr.
    IF sy-subrc IS INITIAL.
      wa_sol_5740-matnr = wa_vbap-matnr.
      wa_sol_5740-meins = wa_vbap-meins.
      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_sol_5740-matnr.
      IF sy-subrc IS INITIAL.
        wa_sol_5740-maktx = wa_makt-maktx.
      ENDIF.
    ENDIF.

    READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_sol_5740-vbeln.
    IF sy-subrc IS INITIAL.
      wa_sol_5740-kunnr = wa_vbpa-kunnr.
      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_sol_5740-kunnr.
      IF sy-subrc IS INITIAL.
        wa_sol_5740-name1 = wa_kna1-name1.
      ENDIF.
    ENDIF.

    READ TABLE it_zsdt0140 INTO wa_zsdt0140 WITH KEY nro_sol = wa_sol_5740-nro_sol
                                                         seq = wa_sol_5740-seq.
    IF sy-subrc IS INITIAL.
      wa_sol_5740-inco1 = wa_zsdt0140-inco1.
    ENDIF.

    MODIFY it_sol_5740 FROM wa_sol_5740 INDEX vl_cont.
    CLEAR: vl_qtd_usada.

  ENDLOOP.

  CLEAR: vl_cont.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETE_LOTE
*&---------------------------------------------------------------------*
FORM delete_carga.

  DATA: it_zsdt0139 TYPE STANDARD TABLE OF zsdt0139,
        it_zsdt0140 TYPE STANDARD TABLE OF zsdt0140,
        wa_zsdt0082 TYPE zsdt0082,
        wa_zsdt0139 TYPE zsdt0139,
        wa_zsdt0140 TYPE zsdt0140,
        answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Excluir Lote'
      text_question         = 'A Carga será excluída. Prosseguir?'
      text_button_1         = 'Sim'(023)
      text_button_2         = 'Não'(024)
      default_button        = '1'
      display_cancel_button = ' '
    IMPORTING
      answer                = answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF answer = '1'.

    SELECT *
      FROM zsdt0139
      INTO TABLE it_zsdt0139
      WHERE nro_cgd EQ vg_lote_editar.

    SELECT *
      FROM zsdt0140
      INTO TABLE it_zsdt0140
      WHERE nro_cgd EQ vg_lote_editar.

    LOOP AT it_zsdt0139 INTO wa_zsdt0139.
      wa_zsdt0139-user_canc = sy-uname.
      wa_zsdt0139-dt_canc   = sy-datum.
      wa_zsdt0139-hr_can    = sy-uzeit.
      wa_zsdt0139-status    = 'X'.
      MODIFY zsdt0139 FROM wa_zsdt0139.
    ENDLOOP.

    LOOP AT it_zsdt0140 INTO wa_zsdt0140.

      wa_zsdt0140-user_canc = sy-uname.
      wa_zsdt0140-dt_canc   = sy-datum.
      wa_zsdt0140-hr_can    = sy-uzeit.
      wa_zsdt0140-status    = 'X'.
      MODIFY zsdt0140 FROM wa_zsdt0140.

      SELECT SINGLE *
          FROM zsdt0082
          INTO wa_zsdt0082
          WHERE nro_sol = wa_zsdt0140-nro_sol
            AND seq     = wa_zsdt0140-seq.

      IF sy-subrc IS INITIAL.
        wa_zsdt0082-status = '2'.
        MODIFY zsdt0082 FROM wa_zsdt0082.
      ENDIF.

    ENDLOOP.

    PERFORM desbloqueia_sol_5740.
    PERFORM clear_5740.
    CLEAR: vg_lote_editar.
    MESSAGE text-069 TYPE 'S'.
    PERFORM clear_5720.
    vg_subt_lote = '5720'.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_MOTORISTA_5740
*&---------------------------------------------------------------------*
FORM completa_motorista_5740 .

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
*&      Form  DESBLOQUEIA_SOL_5740
*&---------------------------------------------------------------------*
FORM desbloqueia_sol_5740.

  DATA: wa_sol_5740 TYPE ty_sol_5740,
        chave       TYPE zde_chave_sol.


  LOOP AT it_sol_5740 INTO wa_sol_5740.

    CONCATENATE wa_sol_5740-nro_sol wa_sol_5740-seq wa_sol_5740-vbeln wa_sol_5740-posnr INTO chave.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        chave = chave.

  ENDLOOP.

ENDFORM.
