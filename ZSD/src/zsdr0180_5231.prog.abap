*----------------------------------------------------------------------*
***INCLUDE ZSDR0060_5231.
*----------------------------------------------------------------------*

*DATA: G_CUSTOM_CONTAINER_5231 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
*      CTL_ALV1_5231           TYPE REF TO CL_GUI_ALV_GRID,
*      IT_FIELDCATALOG_5231    TYPE LVC_T_FCAT,
*      IT_EXCLUDE_5231         TYPE UI_FUNCTIONS.

DATA r_lote TYPE RANGE OF zsdt0131-nro_lote.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5231  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5231 OUTPUT.
  SET PF-STATUS 'T_5231'.
  SET TITLEBAR 'T_5231'.

  PERFORM alv_tela_carga_5231.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5231  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5231 INPUT.
  CASE sy-ucomm.
    WHEN 'REMOVE'.
      LEAVE TO SCREEN 0.
    WHEN 'ADD'.
      PERFORM add_carga_5231.

      PERFORM seleciona_carga_5230.
      PERFORM completa_dados_carga_5230.
      PERFORM bloqueia_linhas_5230.
      PERFORM alv_tela_carga_5230.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

FORM seleciona_carga_5231.

  SELECT *
      FROM zsdt0129
      INTO CORRESPONDING FIELDS OF TABLE it_lote_5231
      WHERE inco1      IN c_inco1
        AND nro_lote   IN c_nlote
        AND data_atual IN c_datab
        AND status     EQ '1'
        AND EXISTS ( SELECT *
                       FROM zsdt0131
                      WHERE zsdt0131~nro_lote   EQ zsdt0129~nro_lote
                        AND zsdt0131~vkorg      IN c_vkorg
                        AND zsdt0131~vkbur      IN c_vkbur
                        AND zsdt0131~spart      EQ c_spart
                        AND zsdt0131~kunnr      IN c_kunnr ).

  IF it_lote_5231 IS NOT INITIAL.

    SELECT *
      FROM lfa1
      INTO TABLE @DATA(it_lfa1_5231)
      FOR ALL ENTRIES IN @it_lote_5231
      WHERE lifnr EQ @it_lote_5231-motorista.

    SELECT *
      FROM zsdt0133
      INTO TABLE @DATA(it_zsdt0133_5231)
      FOR ALL ENTRIES IN @it_lote_5231
      WHERE nro_cg EQ @it_lote_5231-nro_cg.

    SELECT *
      FROM zsdt0131
      INTO TABLE @DATA(it_zsdt0131_5231)
      FOR ALL ENTRIES IN @it_lote_5231
      WHERE nro_lote EQ @it_lote_5231-nro_lote
        AND status NE @abap_true.

    IF it_zsdt0131_5231 IS NOT INITIAL.

      SELECT *
        FROM t001w
        INTO TABLE @DATA(it_t001w_5231)
        FOR ALL ENTRIES IN @it_zsdt0131_5231
        WHERE werks EQ @it_zsdt0131_5231-vkbur.

    ENDIF.

    IF it_zsdt0133_5231 IS NOT INITIAL.

      SELECT *
        FROM lfa1
        INTO TABLE @DATA(it_lfa1_t_5231)
        FOR ALL ENTRIES IN @it_zsdt0133_5231
        WHERE lifnr EQ @it_zsdt0133_5231-cod_transportadora.

    ENDIF.

  ENDIF.

  PERFORM completa_dados_carga_5231 TABLES: it_lfa1_5231
                                          it_zsdt0133_5231
                                          it_zsdt0131_5231
                                          it_t001w_5231
                                          it_lfa1_t_5231.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_DADOS_CARGA_5231
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->T_LFA1_5231  text
*      -->T_ZSDT0133_5231  text
*      -->T_ZSDT0131_5231  text
*      -->T_T001W_5231  text
*      -->T_LFA1_T_5231  text
*----------------------------------------------------------------------*
FORM completa_dados_carga_5231  TABLES   t_lfa1_5231 STRUCTURE lfa1
                                         t_zsdt0133_5231 STRUCTURE zsdt0133
                                         t_zsdt0131_5231 STRUCTURE zsdt0131
                                         t_t001w_5231 STRUCTURE t001w
                                         t_lfa1_t_5231 STRUCTURE lfa1.

  LOOP AT it_lote_5231 ASSIGNING FIELD-SYMBOL(<f_lote_5231>).

    vl_cont = vl_cont + 1.

    TRY .
        DATA(w_lfa1) = t_lfa1_5231[ lifnr = <f_lote_5231>-motorista ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR w_lfa1.
    ENDTRY.

    <f_lote_5231>-name1 = w_lfa1-name1.
    <f_lote_5231>-stcd2 = w_lfa1-stcd2.
    <f_lote_5231>-telf1 = w_lfa1-telf1.

    TRY .
        DATA(w_zsdt0133) = t_zsdt0133_5231[ nro_cg = <f_lote_5231>-nro_cg ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR w_zsdt0133.
    ENDTRY.

    <f_lote_5231>-icone2 =
    SWITCH #( wa_zsdt0133-status
                WHEN 2 THEN '@5D@'
                WHEN 3 THEN '@FD@'
                WHEN 4 THEN '@4A@'
                WHEN 5 THEN '@96@'
                WHEN 6 THEN '@0Q@'
            ).

    TRY .
        wa_lfa1 = t_lfa1_t_5231[ lifnr = wa_zsdt0133-cod_transportadora ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_lfa1.
    ENDTRY.

    <f_lote_5231>-name1_t = wa_lfa1-name1.
    <f_lote_5231>-telf1_t = wa_lfa1-telf1.


    <f_lote_5231>-ctg_transp_d =
    SWITCH #( <f_lote_5231>-ctg_transp
                 WHEN 'A' THEN 'AVULSO'
                 WHEN 'B' THEN '27'
                 WHEN 'C' THEN '32'
                 WHEN 'D' THEN '37'
                 WHEN 'E' THEN '50'
            ).


    <f_lote_5231>-icone =
    SWITCH #( <f_lote_5231>-status
                WHEN 0 THEN '@5C@'
                WHEN 1 THEN '@5B@'
                WHEN 2 THEN '@7E@'
                WHEN 3 THEN '@5D@'
                WHEN 4 THEN '@7Q@'
            ).

    TRY .
        DATA(w_zsdt0131) = t_zsdt0131_5231[ nro_lote = <f_lote_5231>-nro_lote ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR w_zsdt0131.
    ENDTRY.

    TRY .
        DATA(w_t001w) = t_t001w_5231[ werks = w_zsdt0131-vkbur ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR w_t001w.
    ENDTRY.

    <f_lote_5231>-name2 = w_t001w-name1.

  ENDLOOP.

ENDFORM.

FORM alv_tela_carga_5231.

  IF g_custom_container_5231 IS INITIAL.

    CREATE OBJECT g_custom_container_5231
      EXPORTING
        container_name              = 'CONTAINER5231'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_5231 USING:
          01 'ICONE'          ' '         ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Status Lote',
          02 'NRO_LOTE'       ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Lote',
*          02 'ICONE2'         ' '         ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Status Carga',
*          03 'NRO_CG'         ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Carga',
          04 'INCO1'          ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tp Frete',
          05 'MARCA'          ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Marca',
          05 'NAME2'          ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Esc. Vendas',
          06 'CTG_TRANSP_D'   ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ctg Transporte',
          07 'QTD_TOTAL_KG'   ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qnt Total Kg'.
*          08 'DT_ENTREGA'     ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Dt. Entrega',
*          09 'PLACA_CAV'      ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Cavalo',
*          10 'PLACA_CAR1'     ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Carreta I',
*          11 'PLACA_CAR2'     ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Carreta II',
*          12 'MOTORISTA'      'ZSDT0129'  ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Motorista',
*          13 'NAME1'          ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Motorista',
*          14 'STCD2'          ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'CPF Mot.',
*          15 'TELF1'          ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Fone Mot.',
*          16 'NAME1_T'        ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Transportadora',
*          17 'TELF1_T'        ' '         ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Fone Transp.'.

    DATA(gs_layout_5231) =
    VALUE lvc_s_layo(
        sel_mode   = 'A'
        stylefname = 'CELLSTYLES'
        cwidth_opt = 'X'
    ).

    PERFORM excluir_botoes CHANGING it_exclude_5231.

    CREATE OBJECT ctl_alv1_5231
      EXPORTING
        i_parent = g_custom_container_5231.           "ALV Lote

*    SET HANDLER:
*      LCL_EVENT_HANDLER_5231=>HANDLE_HOTSPOT_CLICK_5231 FOR CTL_ALV1_POP_5231,
*      LCL_EVENT_HANDLER_5231=>HANDLE_BUTTON_CLICK_5231 FOR CTL_ALV1_POP_5231.

    CALL METHOD ctl_alv1_5231->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5231
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5231
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_5231
        it_outtab            = it_lote_5231.

    CALL METHOD ctl_alv1_5231->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ELSE.

    CALL METHOD ctl_alv1_5231->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_CARGA_5231
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_carga_5231 .

  CLEAR: wa_zsdt0133.

  CHECK it_sol_click_5230 IS NOT INITIAL.

  DATA(vl_nro_cg) = it_sol_click_5230[ 1 ]-nro_cg.

  CALL METHOD ctl_alv1_5231->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  TRY .
      DATA(w_lote_5231) = it_lote_5231[ it_selected_rows[ 1 ]-index ].
    CATCH cx_sy_itab_line_not_found.
      CLEAR w_lote_5231.
      EXIT.
  ENDTRY.


  READ TABLE it_carga_5230 INTO wa_carga_5230 WITH KEY nro_cg = vl_nro_cg.

  IF sy-subrc IS INITIAL.

    SELECT SINGLE *
      FROM zsdt0133
      INTO wa_zsdt0133
      WHERE nro_cg EQ wa_carga_5230-nro_cg.

    IF wa_zsdt0133-status NE wa_carga_5230-status.
      MESSAGE text-096 TYPE 'S' DISPLAY LIKE 'E'.
    ELSEIF wa_carga_5230-status GE 6.
      MESSAGE text-070 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    SELECT *
      FROM zsdt0129
      INTO TABLE @DATA(t_0129)
      WHERE nro_lote EQ @w_lote_5231-nro_lote.

    SELECT 'I' AS sign,
          'EQ' AS option,
          nro_lote AS low,
          nro_lote AS high
    FROM zsdt0129
    INTO TABLE @r_lote
    WHERE nro_cg EQ @vl_nro_cg.

    APPEND VALUE #(
        sign = 'I'
        option = 'EQ'
        low = w_lote_5231-nro_lote
        high = w_lote_5231-nro_lote
     )  TO r_lote.

    IF sy-subrc IS INITIAL.
      SELECT SUM( qtd_emkg )
        FROM zsdt0131
        INTO @DATA(vl_qtd_emkg)
        WHERE nro_lote IN @r_lote
        AND   status   NE 'X'.    "<<RIM-SKM-IR112129
    ENDIF.

    PERFORM valida_dados_5231 TABLES r_lote.

    CHECK vl_check IS INITIAL.

    IF vl_qtd_emkg > 50000.
      MESSAGE text-033 TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    DATA(vl_ctg_transp) =
    COND #(
            WHEN vl_qtd_emkg LE 23000 THEN 'A'
            WHEN vl_qtd_emkg LE 27000 THEN 'B'
            WHEN vl_qtd_emkg LE 32000 THEN 'C'
            WHEN vl_qtd_emkg LE 37000 THEN 'D'
            WHEN vl_qtd_emkg LE 50000 THEN 'E'
          ).

    UPDATE zsdt0129
      SET nro_cg = vl_nro_cg
          status = 2
          dt_entrega = w_lote_5231-dt_entrega
    WHERE nro_lote EQ w_lote_5231-nro_lote.

    UPDATE zsdt0133
    SET qtd_total_kg = vl_qtd_emkg
        ctg_transp   = vl_ctg_transp
      WHERE nro_cg EQ vl_nro_cg.

    COMMIT WORK.

    FREE it_sol_click_5230.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALIDA_DADOS_5231
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_dados_5231 TABLES r_lote.

  CLEAR: vl_check.

  SELECT c~*
      FROM zsdt0129 AS a
      INNER JOIN zsdt0131 AS b ON b~nro_lote EQ a~nro_lote
      INNER JOIN zsdt0132 AS c ON c~nr_rot EQ b~cod_loc_emb
        INTO TABLE @DATA(t_0132)
          WHERE a~nro_lote IN @r_lote.

  IF sy-subrc IS INITIAL.
    DATA(t_0132_aux) = t_0132.
    SORT t_0132_aux BY transp_resp.
    DELETE ADJACENT DUPLICATES FROM t_0132_aux COMPARING transp_resp.
    IF lines( t_0132_aux ) > 1.
      MESSAGE text-151 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true. EXIT.
    ENDIF.

    t_0132_aux = t_0132.
    SORT t_0132_aux BY armazem.
    DELETE ADJACENT DUPLICATES FROM t_0132_aux COMPARING armazem.
    IF lines( t_0132_aux ) > 1.
      MESSAGE text-152 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true. EXIT.
    ENDIF.

    t_0132_aux = t_0132.
    SORT t_0132_aux BY transportadora.
    DELETE ADJACENT DUPLICATES FROM t_0132_aux COMPARING transportadora.
    IF lines( t_0132_aux ) > 1.
      MESSAGE text-153 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true. EXIT.
    ENDIF.

  ENDIF.

ENDFORM.
