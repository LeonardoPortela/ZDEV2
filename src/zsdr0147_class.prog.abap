*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed       FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_f4         FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells sender,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no sender,

      toolbar               FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

    DATA: l_ukurs     TYPE ukurs_curr,
          l_gdatu     TYPE gdatu_inv,
          l_vlr_usd   TYPE zlest0061-vlr_usd,
          l_vlr_brl   TYPE zlest0061-vlr_brl,
          l_tax_dolar TYPE zlest0061-tax_dolar,
          l_stcd_str  TYPE c LENGTH 9,
          l_stcd_conc TYPE c LENGTH 4,
          l_tabix2    TYPE sy-tabix,
          l_value     TYPE lvc_value,
          l_popup     TYPE char1,
          l_resp      TYPE char1,
*---> 01/06/2023 - Migração S4 - JS
*         l_texto1    TYPE npdok-t35,
*         l_texto2    TYPE npdok-t35.
          l_texto1    TYPE char35,
          l_texto2    TYPE char35.
*<--- 01/06/2023 - Migração S4 - JS
    CLEAR: obj_zcl_util_sd.
    CREATE OBJECT obj_zcl_util_sd.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cellsx) WHERE fieldname = 'NAVIO'.
      l_value = wa_good_cellsx-value.
      READ TABLE t_saida INTO DATA(w_saida_selx) INDEX wa_good_cellsx-row_id.
      LOOP AT t_saida  INTO w_saida WHERE id_seq = w_saida_selx-id_seq.
        w_saida-navio        = l_value.
        MODIFY t_saida    FROM w_saida INDEX sy-tabix.
      ENDLOOP.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cellsy) WHERE fieldname = 'LOCAL_OPERACAO'.
      l_value = wa_good_cellsy-value.
      READ TABLE t_saida INTO DATA(w_saida_sely) INDEX wa_good_cellsy-row_id.
      LOOP AT t_saida  INTO w_saida WHERE id_seq = w_saida_sely-id_seq.
        w_saida-local_operacao = l_value.
        MODIFY t_saida      FROM w_saida INDEX sy-tabix.
      ENDLOOP.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells1) WHERE fieldname = 'DT_FATURA'.
      l_value = wa_good_cells1-value.
      READ TABLE t_saida INTO DATA(w_saida_sel1) INDEX wa_good_cells1-row_id.
      LOOP AT t_saida  INTO w_saida WHERE id_seq = w_saida_sel1-id_seq.
        w_saida-dt_fatura    = l_value.
        w_saida-waerk        = abap_off.
        w_saida-emp_fat_serv = abap_off.
        w_saida-tax_dolar    = 0.
        w_saida-vlr_brl      = 0.
        w_saida-vlr_usd      = 0.
        w_saida-netpr        = 0.
        MODIFY t_saida    FROM w_saida INDEX sy-tabix.
      ENDLOOP.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells2) WHERE fieldname = 'AUART'.
      l_value = wa_good_cells2-value.
      READ TABLE t_saida INTO DATA(w_saida_sel2) INDEX wa_good_cells2-row_id.
      LOOP AT t_saida  INTO w_saida WHERE id_seq = w_saida_sel2-id_seq.
        w_saida-auart        = l_value.
        w_saida-waerk        = abap_off.
        w_saida-emp_fat_serv = abap_off.
        w_saida-tax_dolar    = 0.
        w_saida-vlr_brl      = 0.
        w_saida-vlr_usd      = 0.
        w_saida-netpr        = 0.
        MODIFY t_saida    FROM w_saida INDEX sy-tabix.
      ENDLOOP.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells) WHERE fieldname = 'WAERK'.

      CLEAR: w_saida, w_mara, w_zlest0055.

      l_value = wa_good_cells-value.

      READ TABLE t_saida INTO DATA(w_saida_sel) INDEX wa_good_cells-row_id.

      LOOP AT t_saida  INTO w_saida WHERE id_seq = w_saida_sel-id_seq.
        w_saida-dt_fatura = w_saida_sel-dt_fatura.
        w_saida-auart     = w_saida_sel-auart.
        w_saida-waerk     = l_value.
        MODIFY t_saida FROM w_saida INDEX sy-tabix.
      ENDLOOP.

      FREE: l_erro, l_popup.

      LOOP AT t_saida INTO w_saida WHERE id_seq = w_saida_sel-id_seq.

        l_tabix = sy-tabix.

        CASE wa_good_cells-fieldname.

          WHEN 'WAERK'.
            IF     w_saida-dt_fatura IS INITIAL.
              l_erro = abap_true.
              MESSAGE s024(sd) WITH 'Favor informar a Data da Fatura!' DISPLAY LIKE 'W'.
              EXIT.
            ELSEIF w_saida-auart IS INITIAL.
              l_erro = abap_true.
              MESSAGE s024(sd) WITH 'Favor informar Tp OV!' DISPLAY LIKE 'W'.
              EXIT.
            ENDIF.

            SELECT SINGLE *
              FROM mara
              INTO w_mara
             WHERE matnr EQ w_saida-matnr.

            w_saida-kunnr   = w_saida-kunnr_ori.

            DATA(cl_codigo) = |{ w_saida-cl_codigo ALPHA = IN }|.

            SELECT SINGLE * FROM lfa1 INTO @DATA(wl_lfa1) WHERE lifnr EQ @cl_codigo.
            SELECT SINGLE * FROM kna1 INTO @DATA(wl_kna1) WHERE kunnr EQ @wl_lfa1-kunnr.

            IF sy-subrc NE 0.
              SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ cl_codigo.
            ENDIF.

            IF sy-subrc EQ 0.
              DATA(l_kunnr_ped) = wl_kna1-kunnr.

              CONCATENATE wl_kna1-stcd1(8) '%' INTO l_stcd_str.

              SELECT *
                FROM kna1
                INTO TABLE @DATA(tl_kna1)
               WHERE stcd1 LIKE @l_stcd_str.

              CHECK NOT tl_kna1[] IS INITIAL.

              LOOP AT tl_kna1 INTO DATA(wkna1).

                l_tabix2 = sy-tabix.

                CLEAR: l_ukurs, l_gdatu, l_stcd_conc.

                CONCATENATE wkna1-stcd1+8(1) wkna1-stcd1+9(1) wkna1-stcd1+10(1)  wkna1-stcd1+11(1) INTO l_stcd_conc.

                IF ( l_stcd_conc NE '0001' ).
                  DELETE tl_kna1 INDEX l_tabix2.
                ELSE.

                  IF l_popup = abap_false.
                    IF w_saida-bukrs_fat IS INITIAL.
                      SELECT *
                        FROM zsdt0307
                        INTO TABLE @DATA(t_0307)
                       WHERE emp_pedido  EQ @w_saida-bukrs.
                    ELSE.
                      SELECT *
                        FROM zsdt0307
                        INTO TABLE t_0307
                       WHERE emp_pedido   EQ w_saida-bukrs
                         AND emp_fat_serv EQ w_saida-bukrs_fat.
                    ENDIF.

                    IF sy-subrc <> 0.
                      FREE: t_0307.
                    ENDIF.

                    IF lines( t_0307 ) > 1.
                      FREE: t_lista.
                      LOOP AT t_0307 INTO DATA(w_0307).
                        DATA(l_linha) = |{ w_0307-emp_pedido } / { w_0307-emp_fat_serv } / { w_0307-centro_fat_serv }|.
                        APPEND VALUE #( selflag = abap_off varoption = l_linha inactive = abap_off ) TO t_lista.
                      ENDLOOP.

                      l_texto1 = 'Há mais de uma Empresa Fat.Servico'.
                      l_texto2 = 'para a Empresa Pedido:' && w_saida-bukrs.
*---> Migração S4 - 19.07.2023 - MIGNOW
*                      call function 'ISH_POPUP_TO_DECIDE_LIST'
                      CALL FUNCTION 'Z_POPUP_TO_DECIDE_LIST'
*<--- Migração S4 - 19.07.2023 - MIGNOW
                        EXPORTING
                          cursorline  = 1
                          start_col   = 70
                          start_row   = 05
                          show_row    = 12
                          textline1   = l_texto1
                          textline2   = l_texto2
                          titel       = 'Fatura Serviço - Parametros Pedido'
                          coltitle    = 'Empr.Pedido / Empr.Fat.Serv / Centro Fat.Serv'
                        IMPORTING
                          answer      = l_resp
                        TABLES
                          im_ex_liste = t_lista
                        EXCEPTIONS
                          no_entries  = 1
                          OTHERS      = 2.

                      IF l_resp <> 'S'.
                        FREE t_0307.
                      ELSE.
                        READ TABLE t_lista INTO DATA(w_lista) WITH KEY selflag = abap_true.
                        DELETE t_0307 WHERE emp_fat_serv <> w_lista-varoption+7(4).
                        l_popup = abap_true.
                      ENDIF.
                    ELSEIF lines( t_0307 ) = 1.
                      l_popup = abap_true.
                    ENDIF.
                  ENDIF.

                  IF t_0307[] IS INITIAL.
                    l_erro        = abap_true.
                    MESSAGE s024(sd) WITH 'Empresa Fatura Serviço não encontrado. '
                                          'Favor realize o parametro na transação ZSDT0158 !'
                                     DISPLAY LIKE 'W'.
                    EXIT.
                  ENDIF.

                  READ TABLE t_0307 INTO w_0307 INDEX 1.

                  CLEAR w_zlest0055.

                  FREE: r_matkl.
                  t_matkl-sign   = 'I'.
                  t_matkl-option = 'CP'.
                  t_matkl-low    = '*' && w_saida-matkl && '*'.
                  APPEND t_matkl TO r_matkl.

                  SELECT SINGLE *
                    FROM zlest0055
                    INTO w_zlest0055
                   WHERE kunnr   EQ l_kunnr_ped
                     AND auart   EQ w_saida-auart
                     AND matkl   IN r_matkl
                     AND dt_fim  GE w_saida-dt_fatura
                     AND waerk   EQ wa_good_cells-value
                     AND status  EQ '1'
                     AND vkorg   EQ w_0307-emp_fat_serv.

                  IF sy-subrc <> 0.
                    SELECT SINGLE *
                      FROM zlest0055
                      INTO w_zlest0055
                     WHERE kunnr   EQ wkna1-kunnr
                       AND auart   EQ w_saida-auart
                       AND matkl   IN r_matkl
                       AND dt_fim  GE w_saida-dt_fatura
                       AND waerk   EQ wa_good_cells-value
                       AND status  EQ '1'
                       AND vkorg   EQ w_0307-emp_fat_serv.
                  ENDIF.

                  CASE  w_zlest0055-waerk.
                    WHEN 'BRL'.
                      l_vlr_brl = ( ( w_saida-menge / 1000 ) *  w_zlest0055-netpr  ).
                      l_gdatu   =  w_saida-dt_fatura.

                      obj_zcl_util_sd->set_data(    EXPORTING i_data  = l_gdatu ).
                      obj_zcl_util_sd->set_kurst(   EXPORTING i_kurst = w_zlest0055-kurst ).
                      obj_zcl_util_sd->set_waerk(   EXPORTING i_waerk = w_zlest0055-waerk ).
                      obj_zcl_util_sd->set_tcurr(   EXPORTING i_tcurr = 'USD' ).
                      obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = l_ukurs ).

                      IF l_ukurs IS NOT INITIAL.
                        w_saida-tax_dolar    = ( l_ukurs * -1 ).
                        l_vlr_usd            = ( l_vlr_brl /  ( l_ukurs * -1 ) ).
                        w_saida-vlr_brl      = l_vlr_brl.
                        w_saida-vlr_usd      = l_vlr_usd.
                        w_saida-waerk_fatura = l_ukurs.

                      ELSE.
                        l_erro = abap_true.
                        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Taxa do câmbio não cadastrada.'.
                        EXIT.
                      ENDIF.

                    WHEN 'USD'.
                      l_vlr_usd = ( ( w_saida-menge / 1000 ) *  w_zlest0055-netpr  ).
                      l_gdatu   =     w_saida-dt_fatura.

                      obj_zcl_util_sd->set_data(    EXPORTING i_data  = l_gdatu ).
                      obj_zcl_util_sd->set_kurst(   EXPORTING i_kurst = w_zlest0055-kurst ).
                      obj_zcl_util_sd->set_waerk(   EXPORTING i_waerk = w_zlest0055-waerk ).
                      obj_zcl_util_sd->set_tcurr(   EXPORTING i_tcurr = 'BRL' ).
                      obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = l_ukurs ).

                      IF l_ukurs IS NOT INITIAL.
                        l_vlr_brl            = l_vlr_usd * l_ukurs.
                        w_saida-tax_dolar    = l_ukurs.
                        w_saida-vlr_brl      = l_vlr_brl.
                        w_saida-vlr_usd      = l_vlr_usd.
                        w_saida-waerk_fatura = l_ukurs.
                      ELSE.
                        l_erro        = abap_true.
                        MESSAGE s024(sd) WITH 'Taxa do câmbio não cadastrada.' DISPLAY LIKE 'E'.
                        EXIT.
                      ENDIF.

                    WHEN OTHERS.
                      w_saida-tax_dolar    = 0.
                      w_saida-vlr_brl      = 0.
                      w_saida-vlr_usd      = 0.

                  ENDCASE.

                  w_saida-netpr        =  w_zlest0055-netpr.
                  w_saida-emp_fat_serv =  w_0307-emp_fat_serv.
                  w_saida-kunnr        =  COND #( WHEN w_zlest0055-kunnr IS NOT INITIAL THEN w_zlest0055-kunnr
                                                                                        ELSE w_saida-kunnr_ori ).
                  w_saida-waerk_fatura =  w_zlest0055-waerk_fatura.
                  MODIFY t_saida    FROM w_saida INDEX l_tabix.
                ENDIF.
              ENDLOOP.

              IF l_erro = abap_true.
                EXIT.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      IF l_erro = abap_true.
        l_value = abap_off.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = wa_good_cells-row_id
            i_fieldname = 'WAERK'
            i_value     = l_value.

        LOOP AT t_saida     INTO w_saida WHERE id_seq = w_saida_sel-id_seq.
          w_saida-waerk        = abap_off.
          w_saida-tax_dolar    = 0.
          w_saida-vlr_brl      = 0.
          w_saida-vlr_usd      = 0.
          w_saida-waerk_fatura = abap_off.
          MODIFY t_saida    FROM w_saida INDEX sy-tabix.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

  METHOD user_command.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

    IF lines( t_rows ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows.
    ENDIF.

  ENDMETHOD.

  METHOD on_f4.

    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE dd03l-fieldname,
             s(1)      TYPE c,
           END OF ty_field.

    TYPES: BEGIN OF ty_value,
             tabname     TYPE dd03l-tabname,
             fieldname   TYPE  dd03l-fieldname,
             char79(100) TYPE c,
           END OF  ty_value.

    DATA: BEGIN OF wl_auart,
            field(50),
          END OF wl_auart.

    DATA: BEGIN OF wl_waerk,
            field(50),
          END OF wl_waerk.

    DATA: tl_auart    LIKE TABLE OF wl_auart,
          tl_waerk    LIKE TABLE OF wl_waerk,
          tl_field    TYPE TABLE OF ty_field,
          wl_field    TYPE ty_field,
          tl_value    TYPE TABLE OF ty_value,
          wl_value    TYPE ty_value,
          wl_char(20),
          wl_index    TYPE sy-tabix.

    IF e_fieldname = 'AUART'.

      SELECT *
        FROM tvakt INTO TABLE @DATA(it_tvakt)
        WHERE spras EQ @sy-langu.

      LOOP AT it_tvakt INTO DATA(wa_tvakt) .
        MOVE: wa_tvakt-auart TO wl_auart-field.
        APPEND wl_auart TO tl_auart.

        MOVE: wa_tvakt-bezei TO wl_auart-field.
        APPEND wl_auart TO tl_auart.
      ENDLOOP.

      wl_field-tabname   = 'TVAKT'.
      wl_field-fieldname = 'AUART'.
      wl_field-s         = 'X'.
      APPEND wl_field TO tl_field.

      wl_field-tabname   = 'TVAKT'.
      wl_field-fieldname = 'BEZEI'.
      wl_field-s         = ' '.
      APPEND wl_field TO tl_field.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
          fieldname                 = 'TVAKT'
          tabname                   = 'AUART'
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_auart
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      IF sy-subrc IS INITIAL.
        READ TABLE it_tvakt INTO wa_tvakt INDEX wl_index.
        IF es_row_no-row_id GT 0.
          READ TABLE t_saida INTO w_saida  INDEX es_row_no-row_id.
          IF sy-subrc IS INITIAL.
            LOOP AT t_saida  INTO w_saida WHERE id_seq = w_saida-id_seq.
              MOVE: wa_tvakt-auart TO w_saida-auart.
              MODIFY t_saida FROM w_saida INDEX sy-tabix.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSEIF  e_fieldname = 'WAERK'.

      SELECT *
        FROM tcurt INTO TABLE @DATA(it_tcurt)
        WHERE spras EQ @sy-langu.

      LOOP AT it_tcurt INTO DATA(wa_tcurt) .
        MOVE: wa_tcurt-waers TO wl_waerk-field.
        APPEND wl_waerk TO tl_waerk.

        MOVE: wa_tcurt-ltext TO wl_waerk-field.
        APPEND wl_waerk TO tl_waerk.
      ENDLOOP.

      wl_field-tabname   = 'TCURT'.
      wl_field-fieldname = 'WAERS'.
      wl_field-s         = 'X'.
      APPEND wl_field TO tl_field.

      wl_field-tabname   = 'TCURT'.
      wl_field-fieldname = 'LTEXT'.
      wl_field-s         = ' '.
      APPEND wl_field TO tl_field.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
          fieldname                 = 'TCURT'
          tabname                   = 'WAERS'
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_waerk
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      IF sy-subrc IS INITIAL.
        READ TABLE it_tcurt INTO wa_tcurt INDEX wl_index.
        IF es_row_no-row_id GT 0.
          READ TABLE t_saida INTO w_saida  INDEX es_row_no-row_id.
          IF sy-subrc IS INITIAL.
            LOOP AT t_saida  INTO w_saida WHERE id_seq = w_saida-id_seq.
              MOVE: wa_tcurt-waers TO w_saida-waerk.
              MODIFY t_saida FROM w_saida INDEX sy-tabix.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD on_hotspot_click.

    READ TABLE t_saida INTO w_saida INDEX e_row_id-index.

    CASE e_column_id.
      WHEN 'NR_OV'.
        IF w_saida-nr_ov IS NOT INITIAL.
*----------------------------
*-------- exportar TCODE
*----------------------------
          DATA: xtcode   TYPE tcode.
          xtcode = 'ZSDT0158'.
          EXPORT xtcode FROM xtcode TO MEMORY ID 'ZSDR0147'.

          SET PARAMETER ID 'AUN' FIELD w_saida-nr_ov.
          CALL TRANSACTION 'VA02' AND SKIP FIRST SCREEN.

          FREE MEMORY ID 'ZSDR0147'.

          PERFORM f_selecao_dados    USING 0
                                  CHANGING l_erro.
          IF l_erro = abap_false.
            PERFORM f_processa_dados USING abap_true.
          ENDIF.
        ENDIF.
      WHEN 'FATURA'.
        IF w_saida-fatura IS NOT INITIAL.
          SET PARAMETER ID 'VF' FIELD w_saida-fatura.
          CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'DOCNUM'.
        IF w_saida-docnum IS NOT INITIAL.
          SET PARAMETER ID 'JEF' FIELD w_saida-docnum.
          CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
        ENDIF.
    ENDCASE.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD toolbar.

    DATA wa_tool TYPE stb_button.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
