*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0401 .
*----------------------------------------------------------------------*

TABLES: zib_cte_dist_gmi_alv.

DATA: ck_confirma_linha TYPE c LENGTH 1.

DATA: editor       TYPE REF TO cl_gui_textedit,
      container    TYPE REF TO cl_gui_custom_container,
      longtext_tab TYPE catsxt_longtext_itab,
      it_texto     TYPE TABLE OF j_1bnfftx WITH HEADER LINE,
      vl_id	       TYPE thead-tdid,
      vl_language  TYPE thead-tdspras,
      vl_object    TYPE thead-tdobject,
      vl_name      TYPE thead-tdname,
      tl_lines     TYPE TABLE OF tline,
      wa_lines     TYPE tline.

*&--------------------------------------------------------------------&*
*& Classes Locais                                                     &*
*&--------------------------------------------------------------------&*

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_0401 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: event_handler_0401  TYPE REF TO lcl_event_handler_0401.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_0401 IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click_0401 USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA: ctl_con_0401       TYPE REF TO cl_gui_custom_container,
      ctl_alv_0401       TYPE REF TO cl_gui_alv_grid,
      gs_lay_0401        TYPE lvc_s_layo,
      gs_var_0401        TYPE disvariant,
      gs_scroll_col_0401 TYPE lvc_s_col,
      gs_scroll_row_0401 TYPE lvc_s_roid,
      it_catalog_0401    TYPE lvc_t_fcat.

DATA: it_exclude_0401 TYPE ui_functions,
      wa_exclude_0401 LIKE LINE OF it_exclude_fcode.

DATA: it_0401_vt TYPE TABLE OF zib_cte_dist_gmi_alv WITH HEADER LINE,
      it_cte_gmi TYPE TABLE OF zib_cte_dist_gmi WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_0301
*&---------------------------------------------------------------------*
FORM handle_hotspot_click_0401
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_0401_vt INDEX row_id.
  lc_index = row_id.

  CASE fieldname.
    WHEN 'IC_EDITAR'.
      PERFORM editar_0401_vt.
      LEAVE TO SCREEN 0401.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Module  STAUS_0401  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE staus_0401 OUTPUT.

  DATA: it_comando_0401 TYPE TABLE OF syucomm.

  IF zib_cte_dist_ter-ck_finalizado EQ abap_true.
    APPEND 'APROVAR' TO it_comando_0401.
    APPEND 'SALVAR'  TO it_comando_0401.
  ELSE.
    CLEAR: it_comando_0401.
  ENDIF.

  SET PF-STATUS 'PFAPROVACAO' EXCLUDING it_comando_0401.
  SET TITLEBAR 'TLAPROVA'.

  IF ctl_con_0401 IS INITIAL.

    CREATE OBJECT ctl_con_0401
      EXPORTING
        container_name = 'ALV_ITENS_NOTA'.

    CREATE OBJECT ctl_alv_0401
      EXPORTING
        i_parent = ctl_con_0401.

    PERFORM fill_it_fieldcatalog_0401.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0401.
*   Set layout parameters for ALV grid

    gs_lay_0401-sel_mode   = space.
    gs_lay_0401-zebra      = abap_true.

    CALL METHOD ctl_alv_0401->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0401
        is_variant           = gs_var_0401
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_0401
      CHANGING
        it_fieldcatalog      = it_catalog_0401
        it_outtab            = it_0401_vt[].

    CALL METHOD ctl_alv_0401->refresh_table_display.

    CREATE OBJECT event_handler_0401.
    SET HANDLER event_handler_0401->handle_hotspot_click FOR ctl_alv_0401.

  ELSE.
    CALL METHOD ctl_alv_0401->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_0401->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0401
      es_row_no   = gs_scroll_row_0401.

ENDMODULE.                 " STAUS_0401  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0401
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0401 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0401> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZIB_CTE_DIST_GMI_ALV'
    CHANGING
      ct_fieldcat      = it_catalog_0401.


  LOOP AT it_catalog_0401 ASSIGNING <fs_cat_0401>.
    CASE <fs_cat_0401>-fieldname.
      WHEN 'IC_EDITAR'.
        <fs_cat_0401>-col_pos = 1.
    ENDCASE.
  ENDLOOP.

  lc_col_pos = 2.

  LOOP AT it_catalog_0401 ASSIGNING <fs_cat_0401>.
    IF <fs_cat_0401>-col_pos IS INITIAL.
      <fs_cat_0401>-col_pos = lc_col_pos.
      ADD 1 TO lc_col_pos.
    ENDIF.
    <fs_cat_0401>-tabname = 'IT_0401_VT'.
    CASE <fs_cat_0401>-fieldname.
      WHEN 'IC_EDITAR'.
        <fs_cat_0401>-key     = abap_true.
        <fs_cat_0401>-hotspot = abap_true.
        <fs_cat_0401>-just    = 'C'.
      WHEN 'QTD_FARDOS' OR 'QT_CARGA_ORG' OR 'QT_CARGA_CHEGADA' OR 'ZVLR_FRETE'.
        <fs_cat_0401>-do_sum  = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0401

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0401
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0401.
  gs_var_0401-report      = sy-repid.
  gs_var_0401-handle      = '0401'.
  gs_var_0401-log_group   = abap_false.
  gs_var_0401-username    = abap_false.
  gs_var_0401-variant     = abap_false.
  gs_var_0401-text        = abap_false.
  gs_var_0401-dependvars  = abap_false.
ENDFORM.                    " FILL_GS_VARIANT_0401

*&---------------------------------------------------------------------*
*&      Form  POPULA_INFORMACOES_ALGODAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM popula_informacoes_algodao.

  DATA: lc_vbfa      TYPE vbfa,
        lc_lips      TYPE lips,
        lc_zsdt0066  TYPE zsdt0066,
        lc_zsdt0053  TYPE zsdt0053,
        lc_j_1bnfnad TYPE j_1bnfnad,
        lc_j_1bnfdoc TYPE j_1bnfdoc,
        lc_j_1bnflin TYPE j_1bnflin,
        wa_active    TYPE j_1bnfe_active,
        lc_lfa1      TYPE lfa1.

  CLEAR: it_0401_vt[], it_cte_gmi[], lc_zsdt0066, lc_zsdt0053, lc_vbfa.

  SELECT * INTO TABLE it_cte_gmi
    FROM zib_cte_dist_gmi
   WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte.

  SORT it_cte_gmi BY docnum_nfe itmnum_nfe.

  LOOP AT it_cte_nit INTO wa_cte_nit.

    READ TABLE it_cte_n55 INTO wa_cte_n55 WITH KEY docnum_nfe = wa_cte_nit-docnum.

    SELECT SINGLE * INTO lc_j_1bnfdoc
      FROM j_1bnfdoc
     WHERE docnum EQ wa_cte_nit-docnum.

    SELECT SINGLE * INTO lc_j_1bnflin
      FROM j_1bnflin
     WHERE docnum EQ wa_cte_nit-docnum
       AND itmnum EQ wa_cte_nit-itmnum.

    it_0401_vt-bukrs    = lc_j_1bnfdoc-bukrs.
    it_0401_vt-branch   = lc_j_1bnfdoc-branch.
    it_0401_vt-nfenum   = lc_j_1bnfdoc-nfenum.
    it_0401_vt-safra    = lc_j_1bnflin-charg.
    it_0401_vt-maktx    = lc_j_1bnflin-maktx.
    it_0401_vt-vbeln_vf = wa_cte_n55-vbeln_vf.

    IF it_0401_vt-terminal_entrega IS INITIAL.
      "Busca Terminal
      SELECT SINGLE * INTO lc_j_1bnfnad
        FROM j_1bnfnad
       WHERE docnum EQ wa_cte_nit-docnum
         AND parvw  EQ 'Z1'.

      IF sy-subrc IS INITIAL.
        it_0401_vt-terminal_entrega = lc_j_1bnfnad-parid.
      ENDIF.
    ENDIF.

    IF it_0401_vt-terminal_entrega IS NOT INITIAL.
      SELECT SINGLE * INTO lc_lfa1 FROM lfa1
       WHERE lifnr EQ it_0401_vt-terminal_entrega.

      IF sy-subrc IS INITIAL.
        it_0401_vt-name1 = lc_lfa1-name1.
        it_0401_vt-ort01 = lc_lfa1-ort01.
        it_0401_vt-regio = lc_lfa1-regio.
      ENDIF.
    ENDIF.

    "Buscar Remessa da Saída
    IF lc_j_1bnfdoc-form IS INITIAL.
      wa_active-regio     = wa_cte_n55-n55_chave_acesso(2).
      wa_active-nfyear    = wa_cte_n55-n55_chave_acesso+2(2).
      wa_active-nfmonth   = wa_cte_n55-n55_chave_acesso+4(2).
      wa_active-stcd1     = wa_cte_n55-n55_chave_acesso+6(14).
      wa_active-model     = wa_cte_n55-n55_chave_acesso+20(2).
      wa_active-serie     = wa_cte_n55-n55_chave_acesso+22(3).
      wa_active-nfnum9    = wa_cte_n55-n55_chave_acesso+25(9).
      wa_active-docnum9   = wa_cte_n55-n55_chave_acesso+34(9).
      wa_active-cdv       = wa_cte_n55-n55_chave_acesso+43(1).

      SELECT SINGLE * INTO wa_active
        FROM j_1bnfe_active AS a
       WHERE regio    EQ wa_active-regio
         AND nfyear   EQ wa_active-nfyear
         AND nfmonth  EQ wa_active-nfmonth
         AND stcd1    EQ wa_active-stcd1
         AND model    EQ wa_active-model
         AND serie    EQ wa_active-serie
         AND nfnum9   EQ wa_active-nfnum9
         AND docnum9  EQ wa_active-docnum9
         AND cdv      EQ wa_active-cdv
         AND form     NE space
         AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ 'X' ).

      IF sy-subrc IS NOT INITIAL.
        wa_active-docnum = lc_j_1bnfdoc-docnum.
      ENDIF.
    ELSE.
      wa_active-docnum = lc_j_1bnfdoc-docnum.
    ENDIF.

    " Data de Chegada """""""""""""""""""""""""""""""""""""""
    SELECT SINGLE * INTO @DATA(wa_0039) FROM zlest0039 WHERE docnum EQ @wa_active-docnum.
    IF sy-subrc IS INITIAL.
      IF wa_0039-pontotransb IS INITIAL.
        IF ( wa_0039-pesochegada IS NOT INITIAL ) AND ( wa_0039-datachegada IS NOT INITIAL ).
          it_0401_vt-dt_chegada = wa_0039-datachegada.
        ENDIF.
      ELSE.
        IF ( wa_0039-pesotransb IS NOT INITIAL ) AND ( wa_0039-datatransb IS NOT INITIAL ).
          it_0401_vt-dt_chegada = wa_0039-datatransb.
        ENDIF.
      ENDIF.
    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""

    it_0401_vt-docnum_saida = wa_active-docnum.

    READ TABLE it_cte_gmi WITH KEY docnum_nfe = wa_cte_nit-docnum
                                   itmnum_nfe = wa_cte_nit-itmnum BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING it_cte_gmi TO it_0401_vt.
    ELSE.

      it_0401_vt-cd_chave_cte	    = zib_cte_dist_ter-cd_chave_cte.
      it_0401_vt-docnum_nfe	      = wa_cte_nit-docnum.
      it_0401_vt-itmnum_nfe	      = wa_cte_nit-itmnum.
      it_0401_vt-qt_carga_org     = zib_cte_dist_ter-qt_carga_cte.
      it_0401_vt-qt_carga_chegada = zib_cte_dist_ter-qt_carga_cte.
      it_0401_vt-dt_saida         = zib_cte_dist_ter-dt_emissao.
      it_0401_vt-zvlr_frete       = wa_cte_nit-zvlr_frete.

      IF wa_cte_n55-vbeln_vl IS NOT INITIAL.

        it_0401_vt-vbeln_vl  = wa_cte_n55-vbeln_vl.

        SELECT SINGLE * INTO lc_vbfa
          FROM vbfa
         WHERE vbeln   = wa_cte_n55-vbeln_vl
           AND vbtyp_n = 'J'
           AND vbtyp_v = 'C'.

        IF sy-subrc IS INITIAL.
          SELECT SINGLE * INTO lc_lips FROM lips WHERE vbeln EQ lc_vbfa-vbeln
                                                   AND posnr EQ lc_vbfa-posnn.
          IF sy-subrc IS INITIAL.
            it_0401_vt-qtd_fardos = lc_lips-volum.
            it_0401_vt-und_voleh  = lc_lips-voleh.
          ENDIF.

          it_0401_vt-vbeln_va = lc_vbfa-vbelv.

          SELECT SINGLE * INTO lc_zsdt0066
            FROM zsdt0066
           WHERE vbeln EQ lc_vbfa-vbelv.

          IF sy-subrc IS NOT INITIAL.
            SELECT SINGLE * INTO lc_zsdt0053
              FROM zsdt0053
             WHERE vbeln EQ lc_vbfa-vbelv.
            IF sy-subrc IS INITIAL.
              it_0401_vt-instrucao     = lc_zsdt0066-instrucao.
              it_0401_vt-nro_sol_ov    = lc_zsdt0066-nro_sol_ov.
              it_0401_vt-nro_sol_posnr = lc_zsdt0066-posnr.
            ENDIF.
          ELSE.
            it_0401_vt-instrucao     = lc_zsdt0066-instrucao.
            it_0401_vt-nro_sol_ov    = lc_zsdt0066-nro_sol_ov.
            it_0401_vt-nro_sol_posnr = lc_zsdt0066-posnr.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    IF zib_cte_dist_ter-ck_finalizado EQ abap_false.
      IF it_0401_vt-ck_autorizado EQ abap_false.
        it_0401_vt-ic_editar = icon_change_number.
      ELSE.
        it_0401_vt-ic_editar = icon_set_state.
      ENDIF.
    ELSE.
      it_0401_vt-ic_editar = icon_complete.
    ENDIF.
    APPEND it_0401_vt.
  ENDLOOP.

ENDFORM.                    " POPULA_INFORMACOES_ALGODAO

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0401  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0401 INPUT.
  CASE ok_code.
    WHEN ok_aprovar.
      PERFORM salvar_informacoes_algodao.
      IF sy-subrc IS INITIAL.
        PERFORM autorizar_informacoes_algodao.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN ok_salvar.
      PERFORM salvar_informacoes_algodao.
      IF sy-subrc IS INITIAL.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0401  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT0401  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit0401 INPUT.
  ck_confirma_linha = abap_false.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_EXIT0401  INPUT

*&---------------------------------------------------------------------*
*&      Form  SALVAR_INFORMACOES_ALGODAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM salvar_informacoes_algodao .

  PERFORM validar_dados_algodao USING sy-subrc.

  IF sy-subrc IS INITIAL.
    CLEAR: it_cte_gmi[].
    DELETE FROM zib_cte_dist_gmi WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte.
    LOOP AT it_0401_vt.
      it_0401_vt-dt_autorizacao  = sy-datum.
      it_0401_vt-hr_autorizacao  = sy-uzeit.
      it_0401_vt-ds_name_usuario = sy-uname.
      it_0401_vt-ck_autorizado   = abap_false.
      MODIFY it_0401_vt INDEX sy-tabix TRANSPORTING dt_autorizacao hr_autorizacao ds_name_usuario ck_autorizado.
      MOVE-CORRESPONDING it_0401_vt TO it_cte_gmi.
      APPEND it_cte_gmi.
    ENDLOOP.

    "Grava Autorização
    MODIFY zib_cte_dist_gmi FROM TABLE it_cte_gmi.
    "Grava Autorização dos Itens de Nota Fiscal
    UPDATE zib_cte_dist_nit SET ck_autorizado = abap_false WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte.

    UPDATE zib_cte_dist_ter SET ck_autorizado = abap_false WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte.

    COMMIT WORK.

    MESSAGE s059.
  ENDIF.

ENDFORM.                    " SALVAR_INFORMACOES_ALGODAO

*&---------------------------------------------------------------------*
*&      Form  AUTORIZAR_INFORMACOES_ALGODAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM autorizar_informacoes_algodao .

*  DATA: LC_IT_NIT TYPE TABLE OF ZIB_CTE_DIST_NIT WITH HEADER LINE.
*
*  SELECT * INTO TABLE LC_IT_NIT
*    FROM ZIB_CTE_DIST_NIT
*   WHERE CD_CHAVE_CTE EQ ZIB_CTE_DIST_TER-CD_CHAVE_CTE.
*
*  SORT LC_IT_NIT BY DOCNUM ITMNUM.

  DELETE FROM zib_cte_dist_gmi WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte.
  LOOP AT it_0401_vt.
    it_0401_vt-dt_autorizacao  = sy-datum.
    it_0401_vt-hr_autorizacao  = sy-uzeit.
    it_0401_vt-ds_name_usuario = sy-uname.
    it_0401_vt-ck_autorizado   = abap_true.
    MODIFY it_0401_vt INDEX sy-tabix TRANSPORTING dt_autorizacao hr_autorizacao ds_name_usuario ck_autorizado.
    MOVE-CORRESPONDING it_0401_vt TO it_cte_gmi.
    APPEND it_cte_gmi.

*    READ TABLE LC_IT_NIT WITH KEY DOCNUM = IT_0401_VT-DOCNUM_NFE ITMNUM = IT_0401_VT-ITMNUM_NFE BINARY SEARCH.
*    IF SY-SUBRC IS INITIAL.
*      LC_IT_NIT-CK_AUTORIZADO    = ABAP_TRUE.
*      LC_IT_NIT-PESO_ORIGEM_APRO = IT_0401_VT-QT_CARGA_ORG.
*      LC_IT_NIT-PESO_CHEGADA_APR = IT_0401_VT-QT_CARGA_CHEGADA.
*      LC_IT_NIT-PESO_DIFERE_APRO = IT_0401_VT-QT_CARGA_ORG - IT_0401_VT-QT_CARGA_CHEGADA.
*      MODIFY LC_IT_NIT INDEX SY-TABIX TRANSPORTING CK_AUTORIZADO PESO_ORIGEM_APRO PESO_CHEGADA_APR PESO_DIFERE_APRO.
*    ENDIF.
  ENDLOOP.

  MODIFY zib_cte_dist_gmi FROM TABLE it_cte_gmi.

  UPDATE zib_cte_dist_ter SET ck_autorizado = abap_true WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte.

*  MODIFY ZIB_CTE_DIST_NIT FROM TABLE LC_IT_NIT.
  COMMIT WORK.

  MESSAGE s105.

ENDFORM.                    " AUTORIZAR_INFORMACOES_ALGODAO

*&---------------------------------------------------------------------*
*&      Form  EDITAR_0401_VT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM editar_0401_vt .

  MOVE-CORRESPONDING it_0401_vt TO zib_cte_dist_gmi_alv.
  ck_confirma_linha = abap_false.
  CALL SCREEN 0402 STARTING AT 30 10.
  IF ck_confirma_linha EQ abap_true.
    READ TABLE it_0401_vt WITH KEY docnum_nfe = zib_cte_dist_gmi_alv-docnum_nfe
                                   itmnum_nfe = zib_cte_dist_gmi_alv-itmnum_nfe.

    it_0401_vt-ic_editar        = icon_change_number.
    it_0401_vt-dt_chegada       = zib_cte_dist_gmi_alv-dt_chegada.
    it_0401_vt-zvlr_frete       = zib_cte_dist_gmi_alv-zvlr_frete.
    it_0401_vt-qt_carga_chegada = zib_cte_dist_gmi_alv-qt_carga_chegada.
    it_0401_vt-qt_carga_org     = zib_cte_dist_gmi_alv-qt_carga_org.
    it_0401_vt-qtd_fardos       = zib_cte_dist_gmi_alv-qtd_fardos.
    it_0401_vt-instrucao        = zib_cte_dist_gmi_alv-instrucao.

    MODIFY it_0401_vt INDEX sy-tabix
    TRANSPORTING instrucao qtd_fardos zvlr_frete qt_carga_org qt_carga_chegada dt_chegada ic_editar.
  ENDIF.

ENDFORM.                    " EDITAR_0401_VT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0402  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0402 OUTPUT.

  DATA: it_comando_0402 TYPE TABLE OF syucomm,
        lc_lfa1         TYPE lfa1.

  IF zib_cte_dist_ter-ck_finalizado EQ abap_true.
    APPEND 'ENTER' TO it_comando_0402.
  ELSE.
    CLEAR: it_comando_0402.
  ENDIF.

  SET PF-STATUS 'PFNOYES' EXCLUDING it_comando_0402.
  SET TITLEBAR 'TLAPROVA'.

  CLEAR: it_texto[], longtext_tab.

  IF ( editor IS INITIAL ).
    CREATE OBJECT container
      EXPORTING
        container_name              = 'LONGTEXT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc IS INITIAL.

    CREATE OBJECT editor
      EXPORTING
        parent                 = container
        wordwrap_mode          = '2'
        wordwrap_position      = '70'
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.

    CHECK sy-subrc IS INITIAL.
  ENDIF.

  IF editor IS NOT INITIAL.

    CALL METHOD editor->delete_text.

    IF zib_cte_dist_gmi_alv-docnum_saida IS NOT INITIAL.

      SELECT * INTO TABLE it_texto
        FROM j_1bnfftx
       WHERE docnum EQ zib_cte_dist_gmi_alv-docnum_saida.

      SORT it_texto BY seqnum.

      LOOP AT it_texto.
        APPEND it_texto-message TO longtext_tab.
      ENDLOOP.

    ENDIF.

    IF zib_cte_dist_gmi_alv-vbeln_vf IS NOT INITIAL.

      CLEAR: tl_lines.

      vl_id       = '0002'.
      vl_language = sy-langu.
      vl_object   = 'VBBK'.
      vl_name     = zib_cte_dist_gmi_alv-vbeln_vf.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id       = vl_id
          language = vl_language
          name     = vl_name
          object   = vl_object
        TABLES
          lines    = tl_lines
        EXCEPTIONS
          OTHERS   = 1.

      IF sy-subrc IS INITIAL.
        LOOP AT tl_lines INTO wa_lines.
          APPEND wa_lines-tdline TO longtext_tab.
        ENDLOOP.
      ENDIF.

    ENDIF.

    CALL METHOD editor->set_text_as_r3table
      EXPORTING
        table           = longtext_tab
      EXCEPTIONS
        error_dp        = 1
        error_dp_create = 2
        OTHERS          = 3.

    CALL METHOD editor->set_readonly_mode
      EXPORTING
        readonly_mode = editor->true.

  ENDIF.

  "Bloquear Grupo
  IF zib_cte_dist_ter-ck_finalizado EQ abap_true.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'A1'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF zib_cte_dist_gmi_alv-terminal_entrega IS NOT INITIAL.
    SELECT SINGLE * INTO lc_lfa1 FROM lfa1
     WHERE lifnr EQ it_0401_vt-terminal_entrega.

    IF sy-subrc IS INITIAL.
      zib_cte_dist_gmi_alv-name1 = lc_lfa1-name1.
      zib_cte_dist_gmi_alv-ort01 = lc_lfa1-ort01.
      zib_cte_dist_gmi_alv-regio = lc_lfa1-regio.
    ENDIF.
  ENDIF.

ENDMODULE.                 " STATUS_0402  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0402  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0402 INPUT.
  CASE ok_code.
    WHEN ok_enter.
      ck_confirma_linha = abap_true.
      LEAVE TO SCREEN 0.
      CLEAR ok_code.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0402  INPUT

*&---------------------------------------------------------------------*
*&      Form  VALIDAR_DADOS_ALGODAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM validar_dados_algodao CHANGING lc_subrc.

  DATA: lc_qt_carga_org TYPE zde_qt_carga_cte,
        lc_zvlr_frete   TYPE zde_vlr_frete.

  lc_subrc = 0.

  lc_qt_carga_org = 0.
  lc_zvlr_frete   = 0.

  LOOP AT it_0401_vt.
    ADD it_0401_vt-qt_carga_org TO lc_qt_carga_org.
    ADD it_0401_vt-zvlr_frete   TO lc_zvlr_frete.
  ENDLOOP.

  IF lc_qt_carga_org NE zib_cte_dist_ter-qt_carga_cte.
    MESSAGE s108.
    lc_subrc = 1.
  ENDIF.

  IF lc_zvlr_frete NE zib_cte_dist_ter-zvlr_frete.
    MESSAGE s109.
    lc_subrc = 1.
  ENDIF.

ENDFORM.                    " VALIDAR_DADOS_ALGODAO
