*&---------------------------------------------------------------------*
*& Report  ZSDR0098
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0098.

TABLES: zsdt0116,  vbak.


TYPES: BEGIN OF ty_saida,
         status           TYPE c LENGTH 20,
         vbeln            TYPE vbak-vbeln,
         seq              TYPE zsdt0116-seq,
         doc_41           TYPE zsdt0041-doc_simulacao,
         kunnr            TYPE vbak-kunnr,
         name1            TYPE kna1-name1,
         vkbur            TYPE vbak-vkbur,
         posnr            TYPE vbap-posnr,
         matnr            TYPE vbap-matnr,
         arktx            TYPE vbap-arktx,
         wrkst            TYPE mara-wrkst,
         meins            TYPE vbap-meins,
         kwmeng           TYPE vbap-kwmeng,
         kwert            TYPE konv-kwert,
         netwr            TYPE vbak-netwr, " 28.05.2025 - RAMON - 174345
         netwr_seq        TYPE vbak-netwr, " 28.05.2025 - RAMON - 174345
         waerk            TYPE vbak-waerk,
         justif           TYPE zsdt0116-just_workflow,
         just_icon        TYPE c LENGTH 4,
*#140787 -  ITSOUZA - 29.05.2024 09:37:14 - Inicio
         status_desc      TYPE char25,
         data_solicitante TYPE zsdt0116-data_solicitante,
         hora_solicitante TYPE zsdt0116-hora_solicitante,
         user_solicitante TYPE zsdt0116-user_solicitante,
         dt_apv           TYPE zsdt0116-dt_apv,
         hr_apv           TYPE zsdt0116-hr_apv,
         user_apv         TYPE zsdt0116-user_apv,
         dt_rej           TYPE zsdt0116-dt_apv,
         hr_rej           TYPE zsdt0116-hr_apv,
         user_rej         TYPE zsdt0116-user_apv,
         aprov_por_ref    TYPE zsdt0116-aprov_por_ref,
         seq_ref          TYPE zsdt0116-seq_ref,
         vbeln_ref        TYPE zsdt0116-vbeln_ref,
         posnr_ref        TYPE zsdt0116-posnr_ref,
*#140787 -  ITSOUZA - 29.05.2024 09:37:14 - Fim
       END OF ty_saida.


TYPES: BEGIN OF ty_hist,
         bukrs               TYPE zsdt0142-bukrs,
         vbeln               TYPE zsdt0142-vbeln,
         seq                 TYPE zsdt0142-seq, " 21.07.2025 - RAMON - 174345
         nivel               TYPE zsdt0142-nivel,
         aprovador           TYPE zsdt0142-aprovador,
         valor_de            TYPE zsdt0142-valor_de,
         valor_ate           TYPE zsdt0142-valor_ate,
         data_atual          TYPE zsdt0142-data_atual,
         hora_atual          TYPE zsdt0142-hora_atual,
         usuario             TYPE zsdt0142-usuario,
         user_apv            TYPE zsdt0116-user_apv,
         dt_apv              TYPE zsdt0116-dt_apv,
         hr_apv              TYPE zsdt0116-hr_apv,
         status_workflow(10) TYPE c,
       END OF ty_hist.



DATA: it_saida         TYPE TABLE OF ty_saida,
      wa_saida         TYPE ty_saida,
      it_zsdt0116      TYPE TABLE OF zsdt0116,
      wa_zsdt0116      TYPE zsdt0116,

      " 28.05.2025 - RAMON - 174345 -->
      it_acumulado     TYPE TABLE OF zsd_in_est_limite_ov_01,
      " 28.05.2025 - RAMON - 174345 --<

      it_zsdt0116_aux  TYPE TABLE OF zsdt0116,
      wa_zsdt0116_aux  TYPE zsdt0116,
      it_zsdt0116_aux2 TYPE TABLE OF zsdt0116,
      wa_zsdt0116_aux2 TYPE zsdt0116,
      it_z0116         TYPE TABLE OF zsdt0116,
      wa_z0116         TYPE zsdt0116,
      it_vbak          TYPE TABLE OF vbak,
      wa_vbak          TYPE vbak,
      it_vbap          TYPE TABLE OF vbap,
      wa_vbap          TYPE vbap,
      it_vbep          TYPE TABLE OF vbep,
      wa_vbep          TYPE vbep,
      it_mara          TYPE TABLE OF mara,
      wa_mara          TYPE mara,
      it_zsdt0041      TYPE TABLE OF zsdt0041,
      wa_zsdt0041      TYPE zsdt0041,
      it_zsdt0090      TYPE TABLE OF zsdt0090,
      wa_zsdt0090      TYPE zsdt0090,
      it_kna1          TYPE TABLE OF kna1,
      wa_kna1          TYPE kna1,
      it_zsdt0142      TYPE TABLE OF zsdt0142,
      wa_zsdt0142      TYPE zsdt0142,
      it_estrat        TYPE TABLE OF zsd_estrategia_ov,
      wa_estrat        TYPE zsd_estrategia_ov,
      it_hist          TYPE TABLE OF ty_hist,
      wa_hist          TYPE ty_hist.

DATA: it_fieldcatalog TYPE lvc_t_fcat,
      wa_fieldcatalog TYPE lvc_s_fcat,
      wa_stable       TYPE lvc_s_stbl.

DATA: g_custom_container   TYPE REF TO cl_gui_custom_container,
      g_custom_container02 TYPE REF TO cl_gui_custom_container,
      g_custom_container03 TYPE REF TO cl_gui_custom_container,
      g_custom_container04 TYPE REF TO cl_gui_custom_container,


      dg_splitter_1        TYPE REF TO cl_gui_splitter_container,
      dg_parent_1          TYPE REF TO cl_gui_container,
      dg_splitter_2        TYPE REF TO cl_gui_splitter_container,
      dg_parent_2          TYPE REF TO cl_gui_container,
      dg_parent_2a         TYPE REF TO cl_gui_container,
      dg_parent_alv        TYPE REF TO cl_gui_container,
      gs_layout            TYPE lvc_s_layo,
      gs_variant           TYPE disvariant,
      it_exclude_fcode     TYPE ui_functions,
      wa_exclude_fcode     LIKE LINE OF it_exclude_fcode,
      dg_dyndoc_id         TYPE REF TO cl_dd_document,
      ctl_alv              TYPE REF TO cl_gui_alv_grid,
      ctl_alv02            TYPE REF TO cl_gui_alv_grid,
      ctl_alv03            TYPE REF TO cl_gui_alv_grid,

      ctl_alv04            TYPE REF TO cl_gui_alv_grid,

      table_element        TYPE REF TO cl_dd_table_element,
      column               TYPE REF TO cl_dd_area,
      table_element2       TYPE REF TO cl_dd_table_element,
      column_1             TYPE REF TO cl_dd_area,
      column_2             TYPE REF TO cl_dd_area,
      dg_html_cntrl        TYPE REF TO cl_gui_html_viewer.


DATA: it_fcat_h   TYPE lvc_t_fcat,
      it_fcat_est TYPE lvc_t_fcat,
      wa_fcat     TYPE lvc_s_fcat,
      wa_layout   TYPE lvc_s_layo,
      it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.


DATA: vrej    TYPE c,
      tstatus TYPE RANGE OF zsdt0116-status_workflow,
      wstatus LIKE LINE OF  tstatus,
      pare    TYPE n LENGTH 3. "#140787 -  ITSOUZA

*#140787 -  ITSOUZA - 29.05.2024 09:43:24 - Inicio
CONSTANTS: c_aprovado     TYPE char25 VALUE 'Aprovado',
           c_rejeitado    TYPE char25 VALUE 'Rejeitada',
           c_aprovado_ref TYPE char25 VALUE 'Aprovado por referência',
           c_pendente     TYPE char25 VALUE 'Pendente Aprovação'.
*#140787 -  ITSOUZA - 29.05.2024 09:43:24 - Fim


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:  p_bukrs FOR  vbak-vkorg OBLIGATORY NO INTERVALS,
                   p_vkbur FOR  vbak-vkbur OBLIGATORY NO INTERVALS,
                   p_vbeln FOR  zsdt0116-vbeln,
                   p_data  FOR  zsdt0116-dt_apv.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
  PARAMETERS: ap  RADIOBUTTON GROUP b1 USER-COMMAND ov_child,
              pa  RADIOBUTTON GROUP b1,
              rej RADIOBUTTON GROUP b1,
              td  RADIOBUTTON GROUP b1,
              ovc AS CHECKBOX DEFAULT 'X' MODIF ID ovc.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  IF pa EQ 'X' OR rej EQ 'X' .
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'OVC'.
          screen-invisible = 1.
          MODIFY SCREEN.
          CLEAR ovc.
      ENDCASE.
    ENDLOOP.
  ELSE.
    ovc = abap_true.
  ENDIF.

CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_hotspot_click.

    IF e_column_id = 'JUST_ICON'.

      READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_order>)
       INDEX es_row_no-row_id.

      IF sy-subrc EQ 0.

        DATA lv_text TYPE string.

        lv_text = <fs_order>-justif.

        CHECK lv_text IS NOT INITIAL.

        CALL FUNCTION 'Z_CAIXA_TEXTO'
          EXPORTING
            iv_texto = lv_text
            iv_title = 'Justificativa'
            iv_limit = 1000
            iv_edit  = abap_false.

      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

INITIALIZATION.

START-OF-SELECTION.
  IF pa EQ 'X' OR rej EQ 'X'.
    CLEAR ovc.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'M_MATE_BUK' ID 'BUKRS' FIELD p_bukrs-low.
  IF sy-subrc IS INITIAL.
    PERFORM z_validar_esc_venda CHANGING sy-subrc.

    IF sy-subrc IS INITIAL.
      PERFORM seleciona_dados.

      PERFORM trata_dados_2.

      "PERFORM trata_dados.

      IF it_saida IS INITIAL.
        MESSAGE 'Nenhum dado encontrado' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ELSE.
        PERFORM alv.
      ENDIF.
    ENDIF.

  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Usuário sem Permissão para Empresa ' p_bukrs-low.
    EXIT.
  ENDIF.

END-OF-SELECTION.


FORM z_validar_esc_venda CHANGING c_erro TYPE sy-subrc.
  DATA: it_0060 TYPE TABLE OF zsdt0060 WITH HEADER LINE,
        l_erro  TYPE i.

  CLEAR c_erro.

  SELECT * FROM zsdt0060 INTO TABLE it_0060
    WHERE usnam    EQ sy-uname
    AND   programa EQ 'ZSDR016'
  AND   vkbur    IN p_vkbur.

  IF it_0060[] IS NOT INITIAL.
    LOOP AT p_vkbur.
      READ TABLE it_0060 WITH KEY vkbur = p_vkbur-low.
      IF sy-subrc IS NOT INITIAL.
        ADD 1 TO l_erro.
      ENDIF.
    ENDLOOP.

  ELSE.
    ADD 1 TO l_erro.
  ENDIF.

  CHECK l_erro IS NOT  INITIAL.
  MESSAGE 'Usuário sem Permissão para o Escritório de Vendas informado.' TYPE 'I'.
  c_erro =  l_erro.

ENDFORM.


FORM seleciona_dados.

  IF ap = 'X'.
    wstatus-sign   = 'I'.
    wstatus-option = 'EQ'.
    wstatus-low    = 'A'.
    wstatus-high   = space.
    APPEND wstatus TO tstatus.
  ELSEIF pa  = 'X'.
    wstatus-sign   = 'I'.
    wstatus-option = 'EQ'.
    wstatus-low    = 'L'.
    wstatus-high   = space.
    APPEND wstatus TO tstatus.
  ELSEIF rej = 'X'.
    wstatus-sign   = 'I'.
    wstatus-option = 'EQ'.
    wstatus-low    = 'R'.
    wstatus-high   = space.
    APPEND wstatus TO tstatus.
  ELSEIF td = 'X'.
    wstatus-sign   = 'I'.
    wstatus-option = 'EQ'.
    wstatus-low    = 'A'.
    APPEND wstatus TO tstatus.
    wstatus-low    = 'L'.
    APPEND wstatus TO tstatus.
    wstatus-low    = 'R'.
    APPEND wstatus TO tstatus.
  ENDIF.

  IF p_vbeln IS NOT INITIAL AND p_data IS NOT INITIAL.

    SELECT * FROM zsdt0116 INTO TABLE it_zsdt0116
      WHERE status EQ ' '
      AND   status_workflow IN  tstatus
      AND   vbeln  IN p_vbeln
      AND   dt_apv IN p_data.

  ELSEIF  p_vbeln IS INITIAL AND p_data IS NOT INITIAL.

    SELECT * FROM zsdt0116 INTO TABLE it_zsdt0116
      WHERE status EQ ' '
      AND   status_workflow IN  tstatus
    AND   dt_apv IN p_data.

  ELSEIF  p_vbeln IS NOT INITIAL AND p_data IS INITIAL.

    SELECT * FROM zsdt0116 INTO TABLE it_zsdt0116
      WHERE status EQ ' '
      AND   status_workflow IN  tstatus
    AND   vbeln  IN p_vbeln.
  ELSE.
    SELECT * FROM zsdt0116 INTO TABLE it_zsdt0116
      WHERE status EQ ' '
    AND   status_workflow IN  tstatus.
  ENDIF.


  IF it_zsdt0116[] IS NOT INITIAL.

*#140787 -  ITSOUZA - 28.05.2024 22:25:31 - Inicio
    IF ovc EQ abap_true.
*      SELECT * FROM zsdt0116 APPENDING TABLE it_zsdt0116
*        FOR ALL ENTRIES IN it_zsdt0116
*        WHERE aprov_por_ref = 'X'
*          AND seq_ref       = it_zsdt0116-seq
*          AND vbeln_ref     = it_zsdt0116-vbeln
*          AND posnr_ref     = it_zsdt0116-posnr.

      pare = 0.

      SELECT * FROM zsdt0116 INTO TABLE it_zsdt0116_aux
       FOR ALL ENTRIES IN it_zsdt0116
       WHERE aprov_por_ref = 'X'
         AND seq_ref       = it_zsdt0116-seq
         AND vbeln_ref     = it_zsdt0116-vbeln
         AND posnr_ref     = it_zsdt0116-posnr.

      IF sy-subrc IS INITIAL.
        WHILE pare IS INITIAL.
          pare = 4.

          LOOP AT it_zsdt0116_aux INTO DATA(wa_zsdt0116_aux).
            APPEND wa_zsdt0116_aux TO it_zsdt0116[].
          ENDLOOP.

          SELECT * FROM zsdt0116 APPENDING TABLE it_zsdt0116_aux2
            FOR ALL ENTRIES IN it_zsdt0116_aux
            WHERE aprov_por_ref = 'X'
              AND seq_ref       = it_zsdt0116_aux-seq
              AND vbeln_ref     = it_zsdt0116_aux-vbeln
              AND posnr_ref     = it_zsdt0116_aux-posnr.

          IF sy-subrc IS INITIAL.
            FREE it_zsdt0116_aux.
            MOVE-CORRESPONDING it_zsdt0116_aux2[] TO it_zsdt0116_aux[].
            FREE it_zsdt0116_aux2.
            pare = 0.
          ENDIF.
        ENDWHILE.
      ENDIF.


    ENDIF.
*#140787 -  ITSOUZA - 28.05.2024 22:25:31 - Fim

    " 23.07.2025 - RAMON - 183689 -->
    SELECT * FROM zsdt0142 INTO TABLE it_zsdt0142
      FOR ALL ENTRIES IN it_zsdt0116
            WHERE vbeln = it_zsdt0116-vbeln.
    "AND seq = it_zsdt0116-seq.
    " 23.07.2025 - RAMON - 183689 --<

    " 28.05.2025 - RAMON - 174345 -->
    SELECT * FROM zsd_in_est_limite_ov_01
      INTO TABLE @it_acumulado
        FOR ALL ENTRIES IN @it_zsdt0116
            WHERE vbeln = @it_zsdt0116-vbeln.
    " 28.05.2025 - RAMON - 174345 --<

    SELECT * FROM vbak INTO TABLE it_vbak
      FOR ALL ENTRIES IN it_zsdt0116
      WHERE vbeln EQ it_zsdt0116-vbeln
      AND   vkorg IN p_bukrs
    AND   vkbur IN p_vkbur.


    IF it_vbak[] IS NOT INITIAL.

      SELECT v~* FROM vbap AS v
        INNER JOIN zsdt0116 AS z ON v~vbeln = z~vbeln "AND v~posnr = z~posnr
        INTO TABLE @DATA(it_aux)
        FOR ALL ENTRIES IN @it_vbak
        WHERE v~vbeln EQ @it_vbak-vbeln.
      "AND   v~posnr EQ z~posnr. " 02.07.2025 - RAMON CORREÇÃO SELEÇÃO ITENS,

      MOVE-CORRESPONDING  it_aux TO it_vbap.


      IF it_vbap[] IS NOT INITIAL.

        SELECT * FROM vbep INTO TABLE it_vbep
          FOR ALL ENTRIES IN it_vbap
        WHERE vbeln EQ it_vbap-vbeln
          AND posnr EQ it_vbap-posnr.


        SELECT * FROM mara INTO TABLE it_mara
          FOR ALL ENTRIES IN it_vbap
        WHERE matnr EQ it_vbap-matnr.
      ENDIF.

      SELECT * FROM zsdt0041 INTO TABLE it_zsdt0041
        FOR ALL ENTRIES IN it_vbak
      WHERE vbeln EQ it_vbak-vbeln.

      SELECT * FROM zsdt0090 INTO TABLE it_zsdt0090
        FOR ALL ENTRIES IN it_vbak
      WHERE vbeln EQ it_vbak-vbeln.

      SELECT * FROM kna1  INTO TABLE it_kna1
        FOR ALL ENTRIES IN it_vbak
      WHERE kunnr EQ it_vbak-kunnr.

    ENDIF.
  ENDIF.

ENDFORM.


FORM trata_dados.

  DATA lv_vlr TYPE netwr_ak.
  DATA lv_posnr TYPE posnr_va.
  DATA lv_novo(1).

  SORT it_vbak BY vbeln ASCENDING .

  LOOP AT it_vbak INTO wa_vbak.

    LOOP AT it_vbap INTO wa_vbap WHERE vbeln = wa_vbak-vbeln.

      CLEAR lv_vlr.

      " 02.07.2025 - RAMON - 183689 -->
      IF line_exists( it_zsdt0116[ vbeln = wa_vbak-vbeln posnr = '000000' ] ).

        lv_posnr = '000000'.
        lv_novo = abap_true.

        LOOP AT it_zsdt0116 INTO wa_zsdt0116
            WHERE vbeln = wa_vbap-vbeln
              AND saldo_origem = 'Z'
              AND posnr = lv_posnr.

          IF wa_vbak-waerk <> 'USD'.
            ADD wa_zsdt0116-vlr_liberado_moeda TO lv_vlr.
          ELSE.
            ADD wa_zsdt0116-vlr_liberado TO lv_vlr.
          ENDIF.

        ENDLOOP.

        wa_zsdt0116-vlr_liberado = lv_vlr.

      ELSE.
        lv_posnr = wa_vbap-posnr.
        lv_novo = abap_false.

        READ TABLE it_zsdt0116 INTO wa_zsdt0116 WITH KEY vbeln = wa_vbap-vbeln
                                         posnr = lv_posnr.


      ENDIF.
      " 02.07.2025 - RAMON - 183689 --<

      IF sy-subrc = 0.

        IF wa_zsdt0116-posnr IS NOT INITIAL.

          READ TABLE it_zsdt0142 INTO DATA(ls_0142)
            WITH KEY vbeln = wa_vbap-vbeln.

        ELSE.

          READ TABLE it_zsdt0142 INTO ls_0142
            WITH KEY vbeln = wa_vbap-vbeln
                     seq = wa_zsdt0116-seq.
        ENDIF.

        IF wa_zsdt0116-aprov_por_ref IS NOT INITIAL.
          wa_zsdt0116-status_workflow = 'A'.
        ENDIF.
        CASE wa_zsdt0116-status_workflow.
          WHEN 'A'.
            wa_saida-status = icon_green_light.
            wa_saida-status_desc = COND #( WHEN wa_zsdt0116-aprov_por_ref IS NOT INITIAL THEN c_aprovado_ref
                                           ELSE c_aprovado ).
            wa_saida-dt_apv   = wa_zsdt0116-dt_apv.
            wa_saida-hr_apv   = wa_zsdt0116-hr_apv.
            wa_saida-user_apv = wa_zsdt0116-user_apv.
          WHEN 'L'.
            wa_saida-status = icon_yellow_light.
            wa_saida-status_desc = c_pendente.
          WHEN 'R'.
            wa_saida-status = icon_red_light.
            wa_saida-status_desc = c_rejeitado.
            wa_saida-dt_rej   = wa_zsdt0116-dt_apv.
            wa_saida-hr_rej   = wa_zsdt0116-hr_apv.
            wa_saida-user_rej = wa_zsdt0116-user_apv.
          WHEN ''.
            wa_saida-dt_apv   = wa_zsdt0116-dt_apv.
            wa_saida-hr_apv   = wa_zsdt0116-hr_apv.
            wa_saida-user_apv = wa_zsdt0116-user_apv.
        ENDCASE.

        wa_saida-vbeln = wa_vbak-vbeln.
        wa_saida-kunnr = |{ wa_vbak-kunnr ALPHA = OUT }|.
        wa_saida-vkbur = wa_vbak-vkbur.
        wa_saida-waerk = wa_vbak-waerk.

        wa_saida-posnr  = wa_vbap-posnr.
        wa_saida-matnr  = |{ wa_vbap-matnr ALPHA = OUT }|.
        wa_saida-arktx  = wa_vbap-arktx.
        wa_saida-meins  = wa_vbap-meins.
        wa_saida-kwmeng = wa_vbap-kwmeng.

        wa_saida-justif = wa_zsdt0116-just_workflow.

        IF wa_saida-justif IS NOT INITIAL.
          wa_saida-just_icon = icon_display_more.
        ELSE.
          wa_saida-just_icon = icon_enter_more.
        ENDIF.


*#140787 -  ITSOUZA - 29.05.2024 09:48:38 - Inicio
        wa_saida-data_solicitante = wa_zsdt0116-data_solicitante.
        wa_saida-hora_solicitante = wa_zsdt0116-hora_solicitante.
        wa_saida-user_solicitante = wa_zsdt0116-user_solicitante.
        wa_saida-aprov_por_ref    = wa_zsdt0116-aprov_por_ref.
        wa_saida-seq_ref          = wa_zsdt0116-seq_ref      .
        wa_saida-vbeln_ref        = wa_zsdt0116-vbeln_ref    .
        wa_saida-posnr_ref        = wa_zsdt0116-posnr_ref    .
*#140787 -  ITSOUZA - 29.05.2024 09:48:38 - Fim

*#140787 - 03.07.2024 - PQ
*        TRY.
*
*            cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
*              EXPORTING it_selection_attribute = VALUE #(
*             ( fieldname = 'KNUMV' value = wa_vbak-knumv )
*             ( fieldname = 'KPOSN' value = wa_vbap-posnr )
*             ( fieldname = 'KSCHL' value = 'PR00' )
*             )
*              IMPORTING et_prc_element_classic_format = DATA(etl327c8r7304) ).
*            wa_saida-kwert = etl327c8r7304[ 1 ]-kwert.
*          CATCH cx_prc_result cx_sy_itab_line_not_found .
*            sy-subrc = 4.
*        ENDTRY.



        wa_saida-kwert = wa_vbap-netwr + wa_vbap-mwsbp.

        " 28.05.2025 - RAMON - 174345 -->
        DATA(ls_acumulado) = VALUE #( it_acumulado[ vbeln = wa_saida-vbeln ] DEFAULT '' ).

***        wa_saida-netwr = ls_acumulado-valor_acumulado_apr + ls_acumulado-valor_acumulado_sap + ls_acumulado-valor_acumulado_opus.
        "wa_saida-netwr = ls_0142-vlr_foto_acum + ls_acumulado-valor_acumulado_sap + ls_acumulado-valor_acumulado_opus .

        wa_saida-netwr = ls_acumulado-valor_acumulado_apr + ls_acumulado-valor_acumulado_sap + ls_acumulado-valor_acumulado_opus .

        wa_saida-netwr_seq = ls_0142-vlr_foto_acum.

        IF lv_novo = abap_true.
          wa_saida-kwert = wa_zsdt0116-vlr_liberado.
        ENDIF.

        " 28.05.2025 - RAMON - 174345 --<

        READ TABLE it_vbep INTO wa_vbep WITH KEY vbeln = wa_vbap-vbeln
                                                 posnr = wa_vbap-posnr.

        "tratativa para deixar zerado o valor e quantidade quando o item for resultado de uma sobre de transaferencia
        IF wa_vbep-lifsp EQ '12'.
          wa_saida-kwmeng = '0'.
          wa_saida-kwert = '0'.
        ENDIF.


        TRY.
            wa_saida-name1 =  it_kna1[ kunnr = wa_vbak-kunnr  ]-name1.
          CATCH cx_sy_itab_line_not_found.
            wa_saida-name1 = ''.
        ENDTRY.

        TRY.
            wa_saida-wrkst = |{  it_mara[  matnr = it_vbap[ vbeln = wa_vbak-vbeln  ]-matnr ]-wrkst ALPHA = OUT  }|.
          CATCH cx_sy_itab_line_not_found.
            wa_saida-wrkst = ''.
        ENDTRY.

        TRY.
            wa_saida-doc_41 =  |{  it_zsdt0041[  vbeln = wa_vbap-vbeln ]-doc_simulacao ALPHA = OUT  }|.
          CATCH cx_sy_itab_line_not_found.
            wa_saida-doc_41 = ''.
        ENDTRY.

        IF wa_saida-doc_41 IS INITIAL.
          TRY.
              wa_saida-doc_41 =  |{ it_zsdt0090[ vbeln = wa_vbap-vbeln ]-doc_simulacao ALPHA = OUT }|.
            CATCH cx_sy_itab_line_not_found.
              wa_saida-wrkst = ''.
          ENDTRY.
        ENDIF.

        IF NOT wa_saida-doc_41 IS INITIAL.
          APPEND wa_saida TO it_saida.
        ENDIF.
      ENDIF.

      CLEAR: wa_saida, wa_zsdt0116, wa_vbap, wa_vbep.
    ENDLOOP.

    CLEAR wa_vbak.
  ENDLOOP.
ENDFORM.
FORM trata_dados_2.

  DATA lv_vlr TYPE netwr_ak.
  DATA lv_posnr TYPE posnr_va.
  DATA lv_novo(1).

  SORT it_vbak BY vbeln ASCENDING .

  LOOP AT it_zsdt0116 ASSIGNING FIELD-SYMBOL(<fs_0116>).

    " 01.09.2025 - RAMON -->
    IF <fs_0116>-posnr IS INITIAL.
      CHECK <fs_0116>-saldo_origem = 'Z'. " <---zsdt0117
    ENDIF.
    " 01.09.2025 - RAMON --<

    "LOOP AT it_vbak INTO wa_vbak.

    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = <fs_0116>-vbeln.

    CHECK sy-subrc EQ 0. " 04.09.2025

    LOOP AT it_vbap INTO wa_vbap WHERE vbeln = wa_vbak-vbeln.

      CLEAR lv_vlr.

      IF wa_vbak-waerk <> 'USD'.
        ADD <fs_0116>-vlr_liberado_moeda TO lv_vlr.
      ELSE.
        ADD <fs_0116>-vlr_liberado TO lv_vlr.
      ENDIF.

    ENDLOOP.

    MOVE-CORRESPONDING <fs_0116> TO wa_zsdt0116.
    <fs_0116>-vlr_liberado = lv_vlr.


*      " 02.07.2025 - RAMON - 183689 -->
*      IF line_exists( it_zsdt0116[ vbeln = wa_vbak-vbeln posnr = '000000' ] ).
*
*        lv_posnr = '000000'.
*        lv_novo = abap_true.
*
*        LOOP AT it_zsdt0116 INTO wa_zsdt0116
*            WHERE vbeln = wa_vbap-vbeln
*              AND saldo_origem = 'Z'
*              AND posnr = lv_posnr.
*
*          IF wa_vbak-waerk <> 'USD'.
*            ADD wa_zsdt0116-vlr_liberado_moeda TO lv_vlr.
*          ELSE.
*            ADD wa_zsdt0116-vlr_liberado TO lv_vlr.
*          ENDIF.
*
*        ENDLOOP.
*
*        wa_zsdt0116-vlr_liberado = lv_vlr.
*
*      ELSE.
*        lv_posnr = wa_vbap-posnr.
*        lv_novo = abap_false.
*
*        READ TABLE it_zsdt0116 INTO wa_zsdt0116 WITH KEY vbeln = wa_vbap-vbeln
*                                         posnr = lv_posnr.
*
*
*      ENDIF.
    " 02.07.2025 - RAMON - 183689 --<

    "IF sy-subrc = 0.

    READ TABLE it_zsdt0142 INTO DATA(ls_0142)
      WITH KEY vbeln = wa_vbap-vbeln
               seq = wa_zsdt0116-seq.

    IF wa_zsdt0116-aprov_por_ref IS NOT INITIAL.
      wa_zsdt0116-status_workflow = 'A'.
    ENDIF.
    CASE wa_zsdt0116-status_workflow.
      WHEN 'A'.
        wa_saida-status = icon_green_light.
        wa_saida-status_desc = COND #( WHEN wa_zsdt0116-aprov_por_ref IS NOT INITIAL THEN c_aprovado_ref
                                       ELSE c_aprovado ).
        wa_saida-dt_apv   = wa_zsdt0116-dt_apv.
        wa_saida-hr_apv   = wa_zsdt0116-hr_apv.
        wa_saida-user_apv = wa_zsdt0116-user_apv.
      WHEN 'L'.
        wa_saida-status = icon_yellow_light.
        wa_saida-status_desc = c_pendente.
      WHEN 'R'.
        wa_saida-status = icon_red_light.
        wa_saida-status_desc = c_rejeitado.
        wa_saida-dt_rej   = wa_zsdt0116-dt_apv.
        wa_saida-hr_rej   = wa_zsdt0116-hr_apv.
        wa_saida-user_rej = wa_zsdt0116-user_apv.
      WHEN ''.
        wa_saida-dt_apv   = wa_zsdt0116-dt_apv.
        wa_saida-hr_apv   = wa_zsdt0116-hr_apv.
        wa_saida-user_apv = wa_zsdt0116-user_apv.
    ENDCASE.

    wa_saida-seq = wa_zsdt0116-seq.
    wa_saida-vbeln = wa_vbak-vbeln.
    wa_saida-kunnr = |{ wa_vbak-kunnr ALPHA = OUT }|.
    wa_saida-vkbur = wa_vbak-vkbur.
    wa_saida-waerk = wa_vbak-waerk.

    wa_saida-posnr  = wa_vbap-posnr.
    wa_saida-matnr  = |{ wa_vbap-matnr ALPHA = OUT }|.
    wa_saida-arktx  = wa_vbap-arktx.
    wa_saida-meins  = wa_vbap-meins.
    wa_saida-kwmeng = wa_vbap-kwmeng.

    wa_saida-justif = wa_zsdt0116-just_workflow.

    IF wa_saida-justif IS NOT INITIAL.
      wa_saida-just_icon = icon_display_more.
    ELSE.
      wa_saida-just_icon = icon_enter_more.
    ENDIF.

*#140787 -  ITSOUZA - 29.05.2024 09:48:38 - Inicio
    wa_saida-data_solicitante = wa_zsdt0116-data_solicitante.
    wa_saida-hora_solicitante = wa_zsdt0116-hora_solicitante.
    wa_saida-user_solicitante = wa_zsdt0116-user_solicitante.
    wa_saida-aprov_por_ref    = wa_zsdt0116-aprov_por_ref.
    wa_saida-seq_ref          = wa_zsdt0116-seq_ref      .
    wa_saida-vbeln_ref        = wa_zsdt0116-vbeln_ref    .
    wa_saida-posnr_ref        = wa_zsdt0116-posnr_ref    .
*#140787 -  ITSOUZA - 29.05.2024 09:48:38 - Fim

*#140787 - 03.07.2024 - PQ
*        TRY.
*
*            cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
*              EXPORTING it_selection_attribute = VALUE #(
*             ( fieldname = 'KNUMV' value = wa_vbak-knumv )
*             ( fieldname = 'KPOSN' value = wa_vbap-posnr )
*             ( fieldname = 'KSCHL' value = 'PR00' )
*             )
*              IMPORTING et_prc_element_classic_format = DATA(etl327c8r7304) ).
*            wa_saida-kwert = etl327c8r7304[ 1 ]-kwert.
*          CATCH cx_prc_result cx_sy_itab_line_not_found .
*            sy-subrc = 4.
*        ENDTRY.



    "wa_saida-kwert = wa_vbap-netwr + wa_vbap-mwsbp.
    wa_saida-kwert = lv_vlr.

    " 28.05.2025 - RAMON - 174345 -->
    DATA(ls_acumulado) = VALUE #( it_acumulado[ vbeln = wa_saida-vbeln ] DEFAULT '' ).

***        wa_saida-netwr = ls_acumulado-valor_acumulado_apr + ls_acumulado-valor_acumulado_sap + ls_acumulado-valor_acumulado_opus.
    "wa_saida-netwr = ls_0142-vlr_foto_acum + ls_acumulado-valor_acumulado_sap + ls_acumulado-valor_acumulado_opus.

    wa_saida-netwr = ls_acumulado-valor_acumulado_apr + ls_acumulado-valor_acumulado_sap + ls_acumulado-valor_acumulado_opus.

    wa_saida-netwr_seq = ls_0142-vlr_foto_acum.

    IF lv_novo = abap_true.
      wa_saida-kwert = wa_zsdt0116-vlr_liberado.
    ENDIF.

    " 28.05.2025 - RAMON - 174345 --<

    READ TABLE it_vbep INTO wa_vbep WITH KEY vbeln = wa_vbap-vbeln
                                             posnr = wa_vbap-posnr.

    "tratativa para deixar zerado o valor e quantidade quando o item for resultado de uma sobre de transaferencia
    IF wa_vbep-lifsp EQ '12'.
      wa_saida-kwmeng = '0'.
      wa_saida-kwert = '0'.
    ENDIF.


    TRY.
        wa_saida-name1 =  it_kna1[ kunnr = wa_vbak-kunnr  ]-name1.
      CATCH cx_sy_itab_line_not_found.
        wa_saida-name1 = ''.
    ENDTRY.

    TRY.
        wa_saida-wrkst = |{  it_mara[  matnr = it_vbap[ vbeln = wa_vbak-vbeln  ]-matnr ]-wrkst ALPHA = OUT  }|.
      CATCH cx_sy_itab_line_not_found.
        wa_saida-wrkst = ''.
    ENDTRY.

    TRY.
        wa_saida-doc_41 =  |{  it_zsdt0041[  vbeln = wa_vbap-vbeln ]-doc_simulacao ALPHA = OUT  }|.
      CATCH cx_sy_itab_line_not_found.
        wa_saida-doc_41 = ''.
    ENDTRY.

    IF wa_saida-doc_41 IS INITIAL.
      TRY.
          wa_saida-doc_41 =  |{ it_zsdt0090[ vbeln = wa_vbap-vbeln ]-doc_simulacao ALPHA = OUT }|.
        CATCH cx_sy_itab_line_not_found.
          wa_saida-wrkst = ''.
      ENDTRY.
    ENDIF.

    IF NOT wa_saida-doc_41 IS INITIAL.
      APPEND wa_saida TO it_saida.
    ENDIF.
    "ENDIF.

    CLEAR: wa_saida, wa_zsdt0116, wa_vbap, wa_vbep.
  ENDLOOP.

ENDFORM.

FORM alv.

  PERFORM preenche_cat USING:

        'STATUS'        'Status'                   '05'      ''   ''     ''   ''   'X',
        'STATUS_DESC'   'Desc. Status'             '25'      ''   ''     ''   ''   '',
        'VBELN'         'Ordem de Venda'           '10'      ''   ''     ''   ''   '',
        'SEQ'           'Sequencia'                '10'      ''   ''     ''   ''   '',
        'DOC_41'        'Simulador Venda'          '08'      ''   ''     ''   ''   '',
        'KUNNR'         'Cliente'                  '07'      ''   ''     ''   ''   '',
        'NAME1'         'Nome'                     '30'      ''   ''     ''   ''   '',
        'VKBUR'         'Escr.Venda'               '04'      ''   ''     ''   ''   '',
        'POSNR'         'Item'                     '03'      ''   ''     ''   ''   '',
        'MATNR'         'Material'                 '07'      ''   ''     ''   ''   '',
        'ARKTX'         'Desc Material'            '35'      ''   ''     ''   ''   '',
        'WRKST'         'Marca'                    '15'      ''   ''     ''   ''   '',
        'MEINS'         'Unid'                     '04'      ''   ''     ''   ''   '',
        'KWMENG'        'Qtd. Ordem'               '13'      ''   ''     ''   ''   '',
        'KWERT'         'Valor Total'              '13'      ''   ''     ''   ''   '',
        'WAERK'         'Moeda'                    '05'      ''   ''     ''   ''   '',
        'NETWR'         'Total.Acumu.USD'          '13'      ''   ''     ''   ''   ''," 28.05.2025 - RAMON - 174345
        'NETWR_SEQ'     'Vlr.Foto.Acum'            '13'      ''   ''     ''   ''   ''," 23.07.2025 - RAMON - 174345

        " 04.09.2025 -->
        "'JUSTIF'        'Justificativa Workflow'   '120'     ''   ''     ''   ''   '',
        'JUST_ICON'     'Justificativa'            '3'       ''   ''     ''   ''   'X'.
  " 04.09.2025 --<

*#140787 -  ITSOUZA - 29.05.2024 10:30:58 - Inicio
  PERFORM preenche_cat USING:
        'DATA_SOLICITANTE'     'Data workflow'                   '10'     ''   ''     ''   ''   '',
        'HORA_SOLICITANTE'     'Hora workflow'                   '10'     ''   ''     ''   ''   '',
        'USER_SOLICITANTE'     'Usuário Solicitante Workflow'    '15'     ''   ''     ''   ''   ''.

  IF ap EQ abap_true OR td EQ abap_true.
    PERFORM preenche_cat USING:
*        'APROV_POR_REF'        'Aprovado por referência'         '01'     ''   ''     ''   ''   '',
*        'SEQ_REF'              'Ref. Sequencial'                 '10'     ''   ''     ''   ''   '',
        'VBELN_REF'            'OV Referência'                   '10'     ''   ''     ''   ''   '',
        'POSNR_REF'            'Item Referência'                 '06'     ''   ''     ''   ''   ''.
  ENDIF.

  IF ap EQ abap_true.
    PERFORM preenche_cat USING:
    'DT_APV'        'Data Final Aprovação'               '10'     ''   ''     ''   ''   '',
    'HR_APV'        'Hora Aprovação'                     '10'     ''   ''     ''   ''   '',
    'USER_APV'      'Usuário Final Aprovação'            '15'     ''   ''     ''   ''   ''.
  ELSEIF rej EQ abap_true.
    PERFORM preenche_cat USING:
    'DT_REJ'        'Data Rejeição'                      '10'     ''   ''     ''   ''   '',
    'HR_REJ'        'Hora Rejeição'                      '10'     ''   ''     ''   ''   '',
    'USER_REJ'      'Usuário Rejeição'                   '15'     ''   ''     ''   ''   ''.
  ELSEIF td EQ abap_true.
    PERFORM preenche_cat USING:
    'DT_APV'        'Data Final Aprovação'               '10'     ''   ''     ''   ''   '',
    'HR_APV'        'Hora Aprovação'                     '10'     ''   ''     ''   ''   '',
    'USER_APV'      'Usuário Final Aprovação'            '15'     ''   ''     ''   ''   '',
    'DT_REJ'        'Data Rejeição'                      '10'     ''   ''     ''   ''   '',
    'HR_REJ'        'Hora Rejeição'                      '10'     ''   ''     ''   ''   '',
    'USER_REJ'      'Usuário Rejeição'                   '15'     ''   ''     ''   ''   ''.
  ENDIF.
*#140787 -  ITSOUZA - 29.05.2024 10:30:58 - Fim

  " 04.09.2025 -->
  READ TABLE it_fieldcatalog ASSIGNING FIELD-SYMBOL(<fs_field>)
    WITH KEY fieldname = 'JUST_ICON'.

  IF sy-subrc EQ 0.
    <fs_field>-hotspot = abap_true.
  ENDIF.

  " 04.09.2025 --<


  CALL SCREEN 0100.

ENDFORM.


FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_icon).

  wa_fieldcatalog-fieldname = p_campo.
  wa_fieldcatalog-coltext   = p_desc.
  wa_fieldcatalog-scrtext_l = p_desc.
  wa_fieldcatalog-scrtext_m = p_desc.
  wa_fieldcatalog-scrtext_s = p_desc.


  wa_fieldcatalog-outputlen = p_tam.
  wa_fieldcatalog-hotspot   = p_hot.
  wa_fieldcatalog-no_zero   = p_zero.
  wa_fieldcatalog-do_sum    = p_sum.
  wa_fieldcatalog-just      = p_just.
  wa_fieldcatalog-icon      = p_icon.


  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.

FORM fill_gs_variant.

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: data_ini(10)            TYPE c,
        data_fim(10)            TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        vl_cont                 TYPE i,
        vl_gtext                TYPE tgsbt-gtext.

  DATA: vl_butxt LIKE t001-butxt.

  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'TITULO'.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 1.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 16.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 0.

    PERFORM fill_gs_variant.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR ctl_alv." 04.09.2025

    "SET HANDLER: LCL_EVENT_RECEIVER=>ZM_HANDLE_HOTSPOT_REPORT FOR CTL_ALV.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        "IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_saida.

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        sap_align = 'LEFT'
        sap_style = cl_dd_document=>heading.

    p_text = TEXT-002.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.

    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD table_element2->add_column
      IMPORTING
        column = column_2.

    CALL METHOD table_element2->set_column_style
      EXPORTING
        col_no       = 2
        sap_align    = 'LEFT'
        sap_fontsize = cl_dd_document=>medium.

    CLEAR: p_text_table.

    LOOP AT p_bukrs.
      IF p_bukrs-option NE 'EQ' AND p_bukrs-option NE 'BT'.
        sdydo_text_element = 'Empresa: Multiplas Seleções'.
        EXIT.
      ELSEIF p_bukrs-option EQ 'BT'.

        SELECT SINGLE butxt FROM t001  INTO vl_butxt
          WHERE bukrs EQ p_bukrs-low
        AND spras EQ sy-langu.

        CONCATENATE 'Empresa:' p_bukrs-low vl_butxt '-' INTO sdydo_text_element SEPARATED BY space.
        CLEAR: vl_butxt.

        SELECT SINGLE butxt  FROM t001  INTO vl_butxt
          WHERE bukrs EQ p_bukrs-low
        AND spras EQ sy-langu.

        CONCATENATE sdydo_text_element p_bukrs-high vl_butxt INTO sdydo_text_element SEPARATED BY space.

        EXIT.
      ELSE.
        vl_cont = vl_cont + 1.
        IF vl_cont GT 1.
          sdydo_text_element = 'Empresa: Multiplas Seleções'.
        ELSE.
          SELECT SINGLE butxt  FROM t001 INTO vl_butxt
          WHERE bukrs EQ p_bukrs-low
          AND spras EQ sy-langu.
          CONCATENATE 'Empresa:' p_bukrs-low vl_butxt INTO sdydo_text_element SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.

    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, vl_butxt, sdydo_text_element.

    LOOP AT p_vkbur.
      IF p_vkbur-option NE 'EQ' AND p_vkbur-option NE 'BT'.
        sdydo_text_element = 'Esc.Vendas: Multiplas Seleções'.
        EXIT.
      ELSEIF p_vkbur-option EQ 'BT'.

        SELECT SINGLE * FROM t001w INTO @DATA(w_t001w)
          WHERE werks EQ @p_vkbur-low.

        CONCATENATE 'Esc. Vendas:' p_vkbur-low  w_t001w-name1 '-' INTO sdydo_text_element SEPARATED BY space.
        CLEAR: w_t001w.

        SELECT SINGLE * FROM t001w INTO w_t001w
          WHERE werks EQ p_vkbur-high.

        CONCATENATE sdydo_text_element p_vkbur-high  w_t001w-name1 INTO sdydo_text_element SEPARATED BY space.
        EXIT.
      ELSE.
        vl_cont = vl_cont + 1.
        IF vl_cont GT 1.
          sdydo_text_element = 'Esc.Vendas: Multiplas Seleções'.
        ELSE.
          SELECT SINGLE * FROM t001w INTO w_t001w
            WHERE werks EQ p_vkbur-low.

          CONCATENATE 'Esc.Vendas:' p_vkbur-low  w_t001w-name1 INTO sdydo_text_element SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, w_t001w, sdydo_text_element.

    IF p_vbeln IS NOT INITIAL.
      LOOP AT p_vbeln.
        IF p_vbeln-option NE 'EQ' AND p_vbeln-option NE 'BT'.
          sdydo_text_element = 'Ordem de Venda: Multiplas Seleções'.
          EXIT.
        ELSEIF p_vbeln-option EQ 'BT'.
          CONCATENATE 'Ordem de Venda:' p_vbeln-low  '-' p_vbeln-high INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Ordem de Venda: Multiplas Seleções'.
          ELSE.

            CONCATENATE 'Ordem de Venda:' p_vbeln-low  INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.

    IF p_data IS NOT INITIAL.
      LOOP AT p_data.
        CONCATENATE  p_data-low+6(2)  '.' p_data-low+4(2)  '.' p_data-low(4)   INTO data_ini.
        CONCATENATE  p_data-high+6(2) '.' p_data-high+4(2) '.' p_data-high(4)  INTO data_fim.

        CONCATENATE 'Período:' data_ini  data_fim INTO sdydo_text_element SEPARATED BY space.
        APPEND sdydo_text_element TO p_text_table.
      ENDLOOP.
    ENDIF.

    IF  ap = 'X'.
      sdydo_text_element = 'Aprovado'.
      APPEND sdydo_text_element TO p_text_table.
    ELSEIF  pa = 'X'.
      sdydo_text_element = 'Pendente de Aprovação'.
      APPEND sdydo_text_element TO p_text_table.
    ELSEIF rej = 'X'.
      sdydo_text_element = 'Rejeitadas'.
      APPEND sdydo_text_element TO p_text_table.
    ELSEIF  td  = 'X'.
      sdydo_text_element = 'Todas'.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.


    CALL METHOD column_2->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_2
      EXCEPTIONS
        html_display_error = 1.

  ELSE.
    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&LOG'  .
      PERFORM z_log_aprovacao.
    WHEN '&EST'.
      PERFORM z_estr_aprovacao.

      " 28.05.2025 - RAMON - 174345 -->
    WHEN '&REINI'.
      PERFORM z_reini_aprovacao.
      " 28.05.2025 - RAMON - 174345 --<

  ENDCASE.

ENDMODULE.


FORM z_log_aprovacao.

  DATA: tl_rows TYPE lvc_t_row,
        wa_rows TYPE lvc_s_row.


  FREE: tl_rows[], wa_rows, it_hist, it_z0116, it_zsdt0142.

  CALL METHOD ctl_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF tl_rows[] IS INITIAL.
    MESSAGE 'Favor selecione uma linha !' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE: tl_rows INTO wa_rows INDEX 1,
              it_saida INTO wa_saida INDEX wa_rows-index.

  CLEAR vrej.

  IF wa_saida-status = icon_red_light.

    vrej = 'X'.

    SELECT * FROM zsdt0116 INTO TABLE it_z0116
    WHERE vbeln EQ  wa_saida-vbeln.

    LOOP AT it_z0116 INTO wa_z0116.
      wa_hist-user_apv        =  wa_z0116-user_apv.
      wa_hist-dt_apv          =  wa_z0116-dt_apv.
      wa_hist-hr_apv          =  wa_z0116-hr_apv.
      wa_hist-status_workflow = 'Rejeitado'.
      APPEND wa_hist TO it_hist.
      CLEAR: wa_z0116,  wa_hist.
    ENDLOOP.

    CALL SCREEN 0104 STARTING AT 020 3
                       ENDING AT 100 10.
  ELSE.

    vrej = ' '.

    SELECT * FROM zsdt0142 INTO TABLE it_zsdt0142
    WHERE vbeln EQ wa_saida-vbeln.

    LOOP AT it_zsdt0142 INTO wa_zsdt0142.
      MOVE-CORRESPONDING wa_zsdt0142 TO wa_hist.
      APPEND wa_hist TO it_hist.
    ENDLOOP.

    " 21.07.2025 - RAMON - 174345 -->
    SORT it_hist BY vbeln seq ASCENDING.
    " 21.07.2025 - RAMON - 174345 --<

    CALL SCREEN 0102 STARTING AT 020 3
                       ENDING AT 125 12.
  ENDIF.
ENDFORM.

FORM z_estr_aprovacao.
  DATA: v_msg    TYPE char50,
        t_ordens TYPE TABLE OF zsd_ord_vendas_est,
        t_estra  TYPE TABLE OF zsd_estrategia_ov,
        t_itens  TYPE TABLE OF zsd_itens_ov_est.


  FREE: it_sel_rows[], wa_sel_rows,  t_ordens, t_estra,  t_itens, it_estrat.

  CALL METHOD ctl_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows[] IS INITIAL.
    MESSAGE 'Favor selecione uma linha !' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE: it_sel_rows INTO wa_sel_rows INDEX 1,
              it_saida INTO wa_saida INDEX wa_sel_rows-index.

  IF sy-subrc = 0 AND wa_saida-vbeln IS NOT INITIAL.

    CALL FUNCTION 'Z_OV_ESTRATEGIA_LISTA'
      EXPORTING
        i_usuario    = sy-uname
        i_vbeln      = wa_saida-vbeln
        i_visualizar = abap_true
      IMPORTING
        e_msg        = v_msg
      TABLES
        t_ordens     = t_ordens
        t_estra      = t_estra
        t_itens      = t_itens.

    LOOP AT t_estra INTO DATA(wa_estra) WHERE seq = wa_saida-seq.
      MOVE-CORRESPONDING wa_estra TO wa_estrat.
      APPEND wa_estrat TO it_estrat.
    ENDLOOP.

    SORT it_estrat BY vbeln seq ASCENDING.

    CALL SCREEN 0103 STARTING AT 030 3
                       ENDING AT 125 12.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.
  SET PF-STATUS 'STATUS_01'.
  SET TITLEBAR 'TITULO_01'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.
  CASE sy-ucomm.
    WHEN '&SAIR'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PBO_02  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_02 OUTPUT.

  DATA: it_filter   TYPE lvc_t_filt,
        wa_filter   TYPE lvc_s_filt,
        it_function TYPE ui_functions,
        wa_function LIKE it_function WITH HEADER LINE.

  wa_layout-zebra      = ''.
  wa_layout-no_toolbar = 'X'.
  wa_layout-no_rowmark = 'X'.
  wa_stable-row        = 'X'.

  FREE: it_fcat_h, wa_fcat.

  DEFINE alv.
    wa_fcat-hotspot   = &1.
    wa_fcat-ref_table = &2.
    wa_fcat-ref_field = &3.
    wa_fcat-tabname   = &4.
    wa_fcat-fieldname = &5.
    wa_fcat-scrtext_l = &6.
    wa_fcat-scrtext_m = &6.
    wa_fcat-no_zero   = &7.
    wa_fcat-outputlen = &8.
    wa_fcat-edit      = &9.

    APPEND wa_fcat TO it_fcat_h.
    CLEAR wa_fcat.
  END-OF-DEFINITION.

  IF vrej = 'X'.

    alv:
    ''       ''     'SEQ'                'IT_HIST'   'SEQ'               'Sequencial'   ' '     '12'    '',
    ''       ''     'USER_APV'           'IT_HIST'   'USER_APV'          'Usuário'      ' '     '12'    '',
    ''       ''     'DT_APV'             'IT_HIST'   'DT_APV'            'Data'         ' '     '10'    '',
    ''       ''     'HR_APV'             'IT_HIST'   'HR_APV'            'Hora'         ' '     '08'    '',
    ''       ''     'STATUS_WORKFLOW'    'IT_HIST'   'STATUS_WORKFLOW'   'Status'       ' '     '15'    ''.


    IF g_custom_container04 IS INITIAL.

      CREATE OBJECT g_custom_container04
        EXPORTING
          container_name = 'CONTAINER04'.

      CREATE OBJECT ctl_alv04
        EXPORTING
          i_parent = g_custom_container04.


      REFRESH it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_delete_row.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_insert_row.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_move_row.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_paste.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_undo.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_append_row.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_copy.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_copy_row.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_cut.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_refresh.
      APPEND wa_function TO it_function.

      wa_layout-no_toolbar = space.
      wa_layout-stylefname = 'STYLE2'.
      wa_layout-grid_title = 'Log de Ação'.
      wa_layout-no_toolbar = 'X'.

      CALL METHOD ctl_alv04->set_table_for_first_display
        EXPORTING
          is_layout            = wa_layout
          it_toolbar_excluding = it_function
        CHANGING
          it_filter            = it_filter
          it_fieldcatalog      = it_fcat_h[]
          it_outtab            = it_hist[].
    ELSE.
      CALL METHOD ctl_alv04->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.



  ELSE.

    alv:
    ''       'ZSDT0142'     'BUKRS'        'IT_HIST'   'BUKRS'       'Empresa'            ' '     '07'    '',
    ''       'ZSDT0142'     'VBELN'        'IT_HIST'   'VBELN'       'Ordem de Venda'     ' '     '12'    '',
    ''       'ZSDT0142'     'SEQ'          'IT_HIST'   'SEQ'         'Sequencia'          ' '     '10'    '',
    ''       'ZSDT0142'     'NIVEL'        'IT_HIST'   'NIVEL'       'Nivel'              ' '     '04'    '',
    ''       'ZSDT0142'     'APROVADOR'    'IT_HIST'   'APROVADOR'   'Aprovador'          ' '     '12'    '',
    ''       'ZSDT0142'     'VALOR_DE'     'IT_HIST'   'VALOR_DE'    'Valor de'           ' '     '10'    '',
    ''       'ZSDT0142'     'VALOR_ATE'    'IT_HIST'   'VALOR_ATE'   'Valor Até'          ' '     '10'    '',
    ''       'ZSDT0142'     'DATA_ATUAL'   'IT_HIST'   'DATA_ATUAL'  'Data'               ' '     '10'    '',
    ''       'ZSDT0142'     'HORA_ATUAL'   'IT_HIST'   'HORA_ATUAL'  'Hora'               ' '     '08'    '',
    ''       'ZSDT0142'     'USUARIO'      'IT_HIST'   'USUARIO'     'Usuário'            ' '     '12'    ''.


    IF g_custom_container02 IS INITIAL.

      CREATE OBJECT g_custom_container02
        EXPORTING
          container_name = 'CONTAINER02'.

      CREATE OBJECT ctl_alv02
        EXPORTING
          i_parent = g_custom_container02.


      REFRESH it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_delete_row.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_insert_row.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_move_row.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_paste.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_undo.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_append_row.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_copy.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_copy_row.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_loc_cut.
      APPEND wa_function TO it_function.
      wa_function =  cl_gui_alv_grid=>mc_fc_refresh.
      APPEND wa_function TO it_function.

      wa_layout-no_toolbar = space.
      wa_layout-stylefname = 'STYLE2'.
      wa_layout-grid_title = 'Log de Ação'.
      wa_layout-no_toolbar = 'X'.

      CALL METHOD ctl_alv02->set_table_for_first_display
        EXPORTING
          is_layout            = wa_layout
          it_toolbar_excluding = it_function
        CHANGING
          it_filter            = it_filter
          it_fieldcatalog      = it_fcat_h[]
          it_outtab            = it_hist[].
    ELSE.
      CALL METHOD ctl_alv02->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PBO_03  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_03 OUTPUT.

  wa_layout-zebra      = ''.
  wa_layout-no_toolbar = 'X'.
  wa_layout-no_rowmark = 'X'.
  wa_stable-row        = 'X'.

  FREE: it_fcat_est, wa_fcat.

  DEFINE alv.
    wa_fcat-hotspot   = &1.
    wa_fcat-ref_table = &2.
    wa_fcat-ref_field = &3.
    wa_fcat-tabname   = &4.
    wa_fcat-fieldname = &5.
    wa_fcat-scrtext_l = &6.
    wa_fcat-scrtext_m = &6.
    wa_fcat-no_zero   = &7.
    wa_fcat-outputlen = &8.
    wa_fcat-edit      = &9.

    APPEND wa_fcat TO it_fcat_est.
    CLEAR wa_fcat.
  END-OF-DEFINITION.

  alv:
  ''   'ZGLT037'  'SEQ'          'IT_ESTRAT'  'SEQ'        'Sequencial'     ' '    '10'    ' ',
  ''   'ZGLT037'  'VALOR_DE'     'IT_ESTRAT'  'VALOR_DE'   'Valor de'       ' '    '10'    ' ',
  ''   'ZGLT037'  'VALOR_ATE'    'IT_ESTRAT'  'VALOR_ATE'  'Valor Até'      ' '    '10'    ' ',
  ''   'ZGLT037'  'VALOR_DE'     'IT_ESTRAT'  'VLR_FOTO_ACUM'  'Vlr.Foto.Acum'      ' '    '10'    ' ',
  ''   'ZGLT037'  'APROVADOR'    'IT_ESTRAT'  'APROVADOR'  'Aprovador'      ' '    '12'    ' ',
  ''   ''         'ESTADO'       'IT_ESTRAT'  'ESTADO'     'Estado'         ' '    '10'    ' ',
  ''   ''         'OPCOES'       'IT_ESTRAT'  'OPCOES'     'Opções Liber.'  ' '    '12'    ' '.


  IF g_custom_container03 IS INITIAL.

    CREATE OBJECT g_custom_container03
      EXPORTING
        container_name = 'CONTAINER03'.

    CREATE OBJECT ctl_alv03
      EXPORTING
        i_parent = g_custom_container03.


    REFRESH it_function.
    wa_function =  cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_function TO it_function.
    wa_function =  cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_function TO it_function.
    wa_function =  cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_function TO it_function.
    wa_function =  cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_function TO it_function.
    wa_function =  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_function TO it_function.
    wa_function =  cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_function TO it_function.
    wa_function =  cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_function TO it_function.
    wa_function =  cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_function TO it_function.
    wa_function =  cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_function TO it_function.
    wa_function =  cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_function TO it_function.
    wa_function =  cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_function TO it_function.

    wa_layout-no_toolbar = space.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = 'Estratégia de Aprovação'.
    wa_layout-no_toolbar = 'X'.

    CALL METHOD ctl_alv03->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = it_function
      CHANGING
        it_filter            = it_filter
        it_fieldcatalog      = it_fcat_est[]
        it_outtab            = it_estrat[].
  ELSE.
    CALL METHOD ctl_alv03->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103 INPUT.

  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0103 OUTPUT.
  SET PF-STATUS 'STATUS_02'.
  SET TITLEBAR 'TITULO_02'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form z_reini_aprovacao
*&---------------------------------------------------------------------*
FORM z_reini_aprovacao .

  DATA lv_ret.

  DATA: tl_rows TYPE lvc_t_row,
        wa_rows TYPE lvc_s_row.


  FREE: tl_rows[], wa_rows, it_hist, it_z0116, it_zsdt0142.

  CALL METHOD ctl_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF tl_rows[] IS INITIAL.
    MESSAGE 'Favor selecione uma linha !' TYPE 'I'.
    EXIT.
  ENDIF.

  PERFORM f_popup_to_confirm USING 'Confirmar reinicio da estratégia?' CHANGING lv_ret.

  CHECK lv_ret = '1'.

  LOOP AT tl_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).

    READ TABLE it_saida INTO wa_saida INDEX <fs_rows>-index.

    CHECK sy-subrc EQ 0.

    DELETE FROM zsdt0142 WHERE vbeln = wa_saida-vbeln.
    "DELETE FROM zsdt0116 WHERE vbeln = wa_saida-vbeln.

    DELETE it_saida INDEX <fs_rows>-index.

*    UPDATE zsdt0116
*      SET status   = abap_true
*          user_can = sy-uname
*          dt_can   = sy-datum
*          hora_can = sy-uzeit
*    WHERE vbeln EQ wa_saida-vbeln.

  ENDLOOP.

  COMMIT WORK AND WAIT.

  CALL METHOD ctl_alv->refresh_table_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM f_popup_to_confirm USING p_question TYPE c
                     CHANGING p_answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = sy-title
      text_question  = p_question
    IMPORTING
      answer         = p_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

ENDFORM.                    " F_POPUP_TO_CONFIRM
