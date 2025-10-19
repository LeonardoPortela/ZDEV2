*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5131
*&---------------------------------------------------------------------*

DATA: it_sol_pop_5141   TYPE STANDARD TABLE OF ty_sol_5140,
      it_zsdt0131_5140  TYPE STANDARD TABLE OF zsdt0131,
      vl_qtd_usada_5141 TYPE mara-meins.

DATA: g_custom_container_pop_5141 TYPE REF TO cl_gui_custom_container,
      ctl_alv1_pop_5141           TYPE REF TO cl_gui_alv_grid,
      gs_layout_pop_5141          TYPE lvc_s_layo,
      it_fieldcatalog_pop_5141    TYPE lvc_t_fcat,
      it_exclude_pop_5141         TYPE ui_functions.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5141  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5141 OUTPUT.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T5141'.

  PERFORM pesquisa_pop_5141.
  PERFORM mostra_pop_5141.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5141  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5141 INPUT.

  DATA: vl_lines_5141 TYPE i.

  ctl_alv1_pop_5141->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'SALVAR'.

      PERFORM desbloqueia_sol_pop_5141 USING ' ' ' '.
      DELETE it_sol_pop_5141 WHERE check NE abap_true OR cor EQ 'C600'.

      DESCRIBE TABLE it_sol_pop_5141 LINES vl_lines_5141.

      IF vl_lines_5141 EQ 0.
        MESSAGE TEXT-101 TYPE 'S' DISPLAY LIKE 'E'.
        PERFORM desbloqueia_sol_pop_5141 USING 'X' 'X'.
      ELSE.
        APPEND LINES OF it_sol_pop_5141 TO it_sol_5140.
        PERFORM busca_clientes_pop_5141.
        PERFORM atualiza_header_5140.
      ENDIF.

      CALL METHOD ctl_alv1_5140->get_frontend_layout
        IMPORTING
          es_layout = gs_layout_5140_alv1.

      gs_layout_5140_alv1-cwidth_opt = abap_true.

      CALL METHOD ctl_alv1_5140->set_frontend_layout
        EXPORTING
          is_layout = gs_layout_5140_alv1.

      CALL METHOD ctl_alv2_5140->get_frontend_layout
        IMPORTING
          es_layout = gs_layout_5140_alv2.

      gs_layout_5140_alv2-cwidth_opt = abap_true.

      CALL METHOD ctl_alv2_5140->set_frontend_layout
        EXPORTING
          is_layout = gs_layout_5140_alv2.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      PERFORM desbloqueia_sol_pop_5141 USING 'X' ' '.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_POP_5141
*&---------------------------------------------------------------------*
FORM pesquisa_pop_5141.

  DATA: it_vbkd         TYPE STANDARD TABLE OF vbkd,
        it_makt         TYPE STANDARD TABLE OF makt,
        it_vbpa         TYPE STANDARD TABLE OF vbpa,
        it_kna1         TYPE STANDARD TABLE OF kna1,
        wa_vbkd         TYPE vbkd,
        wa_makt         TYPE makt,
        wa_vbpa         TYPE vbpa,
        wa_kna1         TYPE kna1,
        vl_cont         TYPE i,
        wa_sol_pop_5141 TYPE ty_sol_5140,
        wa_zsdt0131     TYPE zsdt0131,
        vl_lifnr        TYPE zsdt0132-lifnr,
        vl_cod_loc_emb  TYPE zsdt0131-cod_loc_emb,
        vl_local_embarq TYPE zsdt0131-local_embarq,
        chave           TYPE zde_chave_sol.

*  IF P_FILIAL IS NOT INITIAL.
*
*    SELECT VBAP~MATNR
*             MARA~MEINS
*             MARA~WRKST
*             ZSDT0082~MANDT
*             ZSDT0082~NRO_SOL
*             ZSDT0082~SEQ
*             ZSDT0082~VBELN
*             ZSDT0082~POSNR
*             ZSDT0082~SEQ_LIB
*             ZSDT0082~VKORG
*             ZSDT0082~SPART
*             ZSDT0082~VKGRP
*             ZSDT0082~VKBUR
*             ZSDT0082~AUART
*             VBAP~WERKS
*             ZSDT0082~QTE_SOL
*             ZSDT0082~DT_LIBER
*             ZSDT0082~USUARIO_LIB
*             ZSDT0082~QTE_LIB
*             ZSDT0082~STATUS
*             ZSDT0082~DT_ENTREGA
*             MARA~BRGEW
*             ZSDT0082~NR_ROT
*        INTO CORRESPONDING FIELDS OF TABLE IT_SOL_POP_5141
*        FROM ZSDT0082
*        INNER JOIN VBAP ON ZSDT0082~VBELN = VBAP~VBELN AND
*                           ZSDT0082~POSNR = VBAP~POSNR
*        INNER JOIN MARA ON MARA~MATNR = VBAP~MATNR
*        INNER JOIN VBAK ON VBAK~VBELN = VBAP~VBELN
*          WHERE ZSDT0082~VKBUR  IN P_VKBUR
*            AND ZSDT0082~VKORG  IN P_VKORG
*            AND ZSDT0082~SPART  EQ P_SPART
*            AND VBAK~KUNNR IN P_KUNNR
*            AND ZSDT0082~SEQ    NE 1
*            AND MARA~WRKST      EQ WA_HEADER_LOTE-WRKST
*            AND ( ZSDT0082~STATUS EQ 2 OR
*                  ZSDT0082~STATUS EQ 5 ).
*
*  ELSEIF P_CORPLT IS NOT INITIAL OR
*         P_CORPCG IS NOT INITIAL OR
*         P_CORPPT IS NOT INITIAL.
*
*    SELECT VBAP~MATNR
*           MARA~MEINS
*           MARA~WRKST
*           ZSDT0082~MANDT
*           ZSDT0082~NRO_SOL
*           ZSDT0082~SEQ
*           ZSDT0082~VBELN
*           ZSDT0082~POSNR
*           ZSDT0082~SEQ_LIB
*           ZSDT0082~VKORG
*           ZSDT0082~SPART
*           ZSDT0082~VKGRP
*           ZSDT0082~VKBUR
*           ZSDT0082~AUART
*           VBAP~WERKS
*           ZSDT0082~QTE_SOL
*           ZSDT0082~DT_LIBER
*           ZSDT0082~USUARIO_LIB
*           ZSDT0082~QTE_LIB
*           ZSDT0082~STATUS
*           ZSDT0082~DT_ENTREGA
*           MARA~BRGEW
*           ZSDT0082~NR_ROT
*      INTO CORRESPONDING FIELDS OF TABLE IT_SOL_POP_5141
*      FROM ZSDT0082
*      INNER JOIN VBAP ON ZSDT0082~VBELN = VBAP~VBELN AND
*                         ZSDT0082~POSNR = VBAP~POSNR
*      INNER JOIN MARA ON MARA~MATNR = VBAP~MATNR
*      INNER JOIN VBAK ON VBAK~VBELN = VBAP~VBELN
*        WHERE ZSDT0082~VKBUR  IN C_VKBUR
*          AND ZSDT0082~VKORG  IN C_VKORG
*          AND ZSDT0082~SPART  EQ C_SPART
*          AND VBAK~KUNNR IN C_KUNNR
*          AND ZSDT0082~SEQ    NE 1
*          AND MARA~WRKST      EQ WA_HEADER_LOTE-WRKST
*          AND ( ZSDT0082~STATUS EQ 2 OR
*                ZSDT0082~STATUS EQ 5 ).
*
*  ENDIF.

  IF it_sol_pop_5141 IS NOT INITIAL.

    SELECT *
      FROM vbkd
      INTO TABLE it_vbkd
      FOR ALL ENTRIES IN it_sol_pop_5141
      WHERE vbeln EQ it_sol_pop_5141-vbeln.

    SELECT *
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_sol_pop_5141
      WHERE matnr EQ it_sol_pop_5141-matnr.

    SELECT *
      FROM zsdt0131
      INTO TABLE it_zsdt0131_5140
      FOR ALL ENTRIES IN it_sol_pop_5141
      WHERE nro_sol EQ it_sol_pop_5141-nro_sol
        AND seq     EQ it_sol_pop_5141-seq
        AND status  NE 'X'.

    SELECT *
      FROM vbpa
      INTO TABLE it_vbpa
      FOR ALL ENTRIES IN it_sol_pop_5141
      WHERE vbeln EQ it_sol_pop_5141-vbeln
        AND parvw EQ 'AG'.

    SELECT *
      FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_vbpa
      WHERE kunnr EQ it_vbpa-kunnr.

    PERFORM busca_local_embarque_5141 USING vl_lifnr
                                          vl_cod_loc_emb
                                          vl_local_embarq.

  ENDIF.


  LOOP AT it_sol_pop_5141 INTO wa_sol_pop_5141.

    vl_cont = vl_cont + 1.

    READ TABLE it_sol_5140 WITH KEY nro_sol = wa_sol_pop_5141-nro_sol
                                    seq     = wa_sol_pop_5141-seq TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      wa_sol_pop_5141-inco1 = '999'.        "Para eliminar o que já está na tela 5140
    ENDIF.

    IF wa_sol_pop_5141-inco1 NE '999'..

      READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_sol_pop_5141-vbeln.
      IF sy-subrc IS INITIAL.
        wa_sol_pop_5141-inco1 = wa_vbkd-inco1.
      ENDIF.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_sol_pop_5141-matnr.
      IF sy-subrc IS INITIAL.
        wa_sol_pop_5141-maktx = wa_makt-maktx.
      ENDIF.

      READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_sol_pop_5141-vbeln.
      IF sy-subrc IS INITIAL.
        wa_sol_pop_5141-kunnr = wa_vbpa-kunnr.
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr.
        IF sy-subrc IS INITIAL.
          wa_sol_pop_5141-name1 = wa_kna1-name1.
        ENDIF.
      ENDIF.

      LOOP AT it_zsdt0131_5140 INTO wa_zsdt0131
      WHERE nro_sol EQ wa_sol_pop_5141-nro_sol.
        vl_qtd_usada = vl_qtd_usada + wa_zsdt0131-qtd_vinc.
      ENDLOOP.

      wa_sol_pop_5141-lifnr = vl_lifnr.
      wa_sol_pop_5141-cod_loc_emb = vl_cod_loc_emb.
      wa_sol_pop_5141-local_embarq = vl_local_embarq.

      wa_sol_pop_5141-saldo = wa_sol_pop_5141-qte_lib - vl_qtd_usada.
      wa_sol_pop_5141-qtd_vinc = wa_sol_pop_5141-saldo.

      IF wa_sol_pop_5141-saldo LE 0.
        wa_sol_pop_5141-inco1 = '999'.
      ENDIF.

      CONCATENATE wa_sol_pop_5141-nro_sol wa_sol_pop_5141-seq wa_sol_pop_5141-vbeln wa_sol_pop_5141-posnr INTO chave.

      CALL FUNCTION 'ZENQUEUE_SD_SOL_INSUMOS'
        EXPORTING
          chave          = chave
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        wa_sol_pop_5141-cor = 'C600'.
      ENDIF.

      CLEAR: vl_qtd_usada.

    ENDIF.

    MODIFY it_sol_pop_5141 FROM wa_sol_pop_5141 INDEX vl_cont.

  ENDLOOP.

  DELETE it_sol_pop_5141 WHERE inco1 NE wa_header_lote-inco1.
  CLEAR: vl_cont.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP_5141
*&---------------------------------------------------------------------*
FORM mostra_pop_5141.

  IF g_custom_container_pop_5141 IS INITIAL.

    CREATE OBJECT g_custom_container_pop_5141
      EXPORTING
        container_name              = 'CONTAINER5141'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_pop_5141 USING:
          01 'CHECK'          ''         ' '  'X' ' '  ' '   'X'   'X'   ' '   ' '   'Nro Lote',
          01 'NRO_SOL'        'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
          02 'VBELN'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Ordem',
          03 'POSNR'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Item',
          04 'AUART'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Tipo OV',
          05 'VKBUR'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Esc. Vendas',
          06 'KUNNR'          'VBPA'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Cliente',
          07 'NAME1'          'KNA1'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Cliente',
          08 'VKORG'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Org. Vendas',
          09 'SPART'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Setor Atv.',
          10 'WERKS'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Centro Forn',
          11 'MATNR'          'MARA'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Material',
          12 'MAKTX'          'MAKT'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Descrição',
          13 'SALDO'          ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Saldo à Formar Lote',
          14 'MEINS'          'VBAP'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'U.M.'.

    gs_layout_pop_5141-sel_mode   = 'A'.
    gs_layout_pop_5141-stylefname = 'CELLSTYLES'.
    gs_layout_pop_5141-cwidth_opt = 'X'.
    gs_layout_pop_5141-info_fname = 'COR'.

    PERFORM excluir_botoes CHANGING it_exclude_pop_5141.

    CREATE OBJECT ctl_alv1_pop_5141
      EXPORTING
        i_parent = g_custom_container_pop_5141.           "ALV Lote

    CALL METHOD ctl_alv1_pop_5141->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_pop_5141
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_pop_5141
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_pop_5141
        it_outtab            = it_sol_pop_5141.

  ELSE.
    CALL METHOD ctl_alv1_pop_5141->refresh_table_display
      EXPORTING
        is_stable = _stable.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5141_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5141_exit INPUT.
  PERFORM desbloqueia_sol_pop_5141 USING 'X' ' '.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_CLIENTES_POP_5141
*&---------------------------------------------------------------------*
FORM busca_clientes_pop_5141.

  DATA: wa_clientes_pop_5141 TYPE ty_cliente_5140,
        wa_zsdt0082_pop_5141 TYPE ty_sol_5140.

  DATA: ls_style TYPE lvc_s_styl,
        wl_name  TYPE thead-tdname,
        tg_texto TYPE STANDARD TABLE OF tline.

  LOOP AT it_sol_pop_5141 INTO wa_zsdt0082_pop_5141.

    wa_clientes_pop_5141-nro_sol = wa_zsdt0082_pop_5141-nro_sol.
    wa_clientes_pop_5141-seq     = wa_zsdt0082_pop_5141-seq.
    wa_clientes_pop_5141-kunnr   = wa_zsdt0082_pop_5141-kunnr.
    wa_clientes_pop_5141-name1   = wa_zsdt0082_pop_5141-name1.
    wa_clientes_pop_5141-nr_rot1 = wa_zsdt0082_pop_5141-nr_rot.
    wa_clientes_pop_5141-nr_rot2 = wa_zsdt0082_pop_5141-nr_rot.

    ls_style-fieldname = 'OBS'.
    ls_style-style = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_style TO wa_clientes_pop_5141-cellstyles.

    CONCATENATE wa_zsdt0082_pop_5141-nro_sol '001' INTO wl_name.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'OBSE'
        language                = sy-langu
        name                    = wl_name
        object                  = 'ZTEXTO'
      TABLES
        lines                   = tg_texto
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF tg_texto IS INITIAL.
      wa_clientes_pop_5141-obs = '@1F@'.
    ELSE.
      wa_clientes_pop_5141-obs = '@1E@'.
    ENDIF.

    APPEND wa_clientes_pop_5141 TO it_cliente_5140.
    CLEAR: wa_clientes_pop_5141.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_LOCAL_EMBARQUE_5141
*&---------------------------------------------------------------------*
FORM busca_local_embarque_5141 USING vl_lifnr TYPE zsdt0132-lifnr
                                     vl_cod_loc_emb  TYPE zsdt0131-cod_loc_emb
                                     vl_local_embarq TYPE zsdt0131-local_embarq. .

  DATA: it_zsdt0132_5141 TYPE STANDARD TABLE OF zsdt0132,
        wa_zsdt0132_5141 TYPE zsdt0132,
        vl_lines         TYPE i.

  SELECT *
    FROM zsdt0132
    INTO TABLE it_zsdt0132_5141
    WHERE marca EQ wa_header_lote-wrkst
    AND status NE 'I'.

  DESCRIBE TABLE it_zsdt0132_5141 LINES vl_lines.

  IF vl_lines NE 1.

    CLEAR: vl_lifnr, vl_cod_loc_emb, vl_local_embarq.

  ELSE.

    READ TABLE it_zsdt0132_5141 INTO wa_zsdt0132_5141 INDEX 1.

    vl_lifnr = wa_zsdt0132_5141-lifnr.
    vl_cod_loc_emb = wa_zsdt0132_5141-nr_rot.
    vl_local_embarq = wa_zsdt0132_5141-rot_desc.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DESBLOQUEIA_SOL_POP_5741
*&---------------------------------------------------------------------*
FORM desbloqueia_sol_pop_5141 USING VALUE(p_01)
                                    VALUE(p_02).

  DATA: wa_sol_pop_5141 TYPE ty_sol_5140,
        chave_exit_5141 TYPE zde_chave_sol.

  LOOP AT it_sol_pop_5141 INTO wa_sol_pop_5141 WHERE cor IS INITIAL
                                                 AND ( check EQ p_01 OR
                                                       check EQ p_02 ) .

    CONCATENATE wa_sol_pop_5141-nro_sol wa_sol_pop_5141-seq wa_sol_pop_5141-vbeln wa_sol_pop_5141-posnr INTO chave_exit_5141.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        chave = chave_exit_5141.

  ENDLOOP.

ENDFORM.
