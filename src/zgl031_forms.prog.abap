*&---------------------------------------------------------------------*
*&  Include           ZGL031_FORMS
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2JVI |12/05/2025 |Ajuste Campo Competencia para  &*
*&                                    |range para os tipos 17, 29, 21 &*
*&                                    |e '22'                         &*
*&                                    |Chamado: 164255.               &*
*&--------------------------------------------------------------------&*
*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER
*&---------------------------------------------------------------------*
FORM build_header CHANGING p_header TYPE treev_hhdr.
  p_header-heading = 'Controle de Apropriações'.
  p_header-tooltip = 'Controle de Apropriações'.
  p_header-width   = 20.
ENDFORM.                    "BUILD_HEADER

*&---------------------------------------------------------------------*
*&      Form  CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_hierarchy TABLES it_node_key TYPE lvc_t_nkey.
  DATA: p_relat_key TYPE lvc_nkey,
        p_pai_key   TYPE lvc_nkey,
        p_filho_key TYPE lvc_nkey,
        l_node_text TYPE lvc_value.

  wl_menu_tree-node_pai   = 'Seguros Cadastro' .
  wl_menu_tree-node_filho = 'Contas a Pagar e Receber'.
  APPEND wl_menu_tree TO gt_menu_tree.

  wl_menu_tree-node_filho = 'Bens Assegurados'.
  APPEND wl_menu_tree TO gt_menu_tree.

  wl_menu_tree-node_pai   = 'Seguros Geração' .
  wl_menu_tree-node_filho = 'Gerar Apropriação'.
  APPEND wl_menu_tree TO gt_menu_tree.

  wl_menu_tree-node_filho = 'Gerar C.Pagar/Receber'.
  APPEND wl_menu_tree TO gt_menu_tree.

  wl_menu_tree-node_filho = 'Gerar Ajuste Apropriação'.
  APPEND wl_menu_tree TO gt_menu_tree.

*  WL_MENU_TREE-NODE_PAI   = 'Relatórios' .
*  WL_MENU_TREE-NODE_FILHO = 'Apropriações Lançadas'.
*  APPEND WL_MENU_TREE TO GT_MENU_TREE.

  LOOP AT gt_menu_tree INTO wl_menu_tree.
    IF NOT wl_menu_tree IS INITIAL.

      ON CHANGE OF wl_menu_tree-node_pai.
        PERFORM add_pai USING space
                              wl_menu_tree-node_pai
                              CHANGING p_pai_key.

        it_node_key = p_pai_key.
        APPEND it_node_key.

      ENDON.

      ON CHANGE OF wl_menu_tree-node_filho.
        PERFORM add_filho USING p_pai_key
                                wl_menu_tree-node_filho
                                CHANGING p_filho_key.
      ENDON.

    ENDIF.
  ENDLOOP.
ENDFORM.                    "CREATE_HIERARCHY

*&---------------------------------------------------------------------*
*&      Form  ADD_PAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RELAT_KEY  text
*      -->P_NODE_PAI   text
*      -->P_PAI_KEY    text
*----------------------------------------------------------------------*
FORM add_pai USING    p_relat_key TYPE lvc_nkey
                      p_node_pai  TYPE char50
             CHANGING p_pai_key   TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value.

  l_node_text = p_node_pai.

  CALL METHOD obj_alv_tree->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
    IMPORTING
      e_new_node_key   = p_pai_key.

ENDFORM.                    "ADD_PAI

*&---------------------------------------------------------------------*
*&      Form  ADD_FILHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PAI_KEY     text
*      -->P_NODE_FILHO  text
*      -->P_FILHO_KEY   text
*----------------------------------------------------------------------*
FORM add_filho USING  p_pai_key    TYPE lvc_nkey
                      p_node_filho TYPE char50
             CHANGING p_filho_key  TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value.

  l_node_text = p_node_filho.

  CALL METHOD obj_alv_tree->add_node
    EXPORTING
      i_relat_node_key = p_pai_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
    IMPORTING
      e_new_node_key   = p_filho_key.

ENDFORM.                    "ADD_FILHO

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM alv_preenche_cat       USING: p_pos
                                   p_campo         TYPE c
                                   p_desc          TYPE c
                                   p_tam           TYPE c
                                   p_hot           TYPE c
                                   p_zero          TYPE c
                                   p_sum           TYPE c
                                   p_icon          TYPE c
                                   p_edit          TYPE c
                                   p_check         TYPE c
                                   p_ref_tabname   LIKE dd02d-tabname
                                   p_ref_fieldname LIKE dd03d-fieldname
                                   p_tabname       LIKE dd02d-tabname
                                   p_f4            TYPE ddf4avail "Match code
                                   p_drdn_field    TYPE int4     "Dropdown in alv
                                   no_out          TYPE c.

  wl_fcat-col_pos    = p_pos.
  wl_fcat-fieldname  = p_campo.
  wl_fcat-scrtext_l  = p_desc.
  wl_fcat-scrtext_m  = p_desc.
  wl_fcat-scrtext_s  = p_desc.
  wl_fcat-hotspot    = p_hot.
  wl_fcat-no_zero    = p_zero.
  wl_fcat-do_sum     = p_sum.
  wl_fcat-outputlen  = p_tam.
  wl_fcat-icon       = p_icon.
  wl_fcat-edit       = p_edit.
  wl_fcat-checkbox   = p_check.
  wl_fcat-ref_table  = p_ref_tabname.
  wl_fcat-ref_field  = p_ref_fieldname.
  wl_fcat-tabname    = p_ref_tabname.
  wl_fcat-f4availabl = p_f4.
  wl_fcat-drdn_hndl  = p_drdn_field.
  wl_fcat-no_out     = no_out.

  CASE p_campo.
    WHEN 'ST_BAIXA'.
      wl_fcat-just    = 'C'.
    WHEN 'AUFNR'.
      "      WL_FCAT-REF_TABLE = 'AFIH'.
      "      WL_FCAT-REF_FIELD = 'AUFNR'.
  ENDCASE.

  IF screen_item = c_screen_0120.
    APPEND wl_fcat TO gt_fcat_0120.
  ELSEIF screen_item = c_screen_0130.
    APPEND wl_fcat TO gt_fcat_0130.
  ELSEIF screen_item = c_screen_0132.
    APPEND wl_fcat TO gt_fcat_0132.
  ELSEIF screen_principal = c_screen_0150.
    APPEND wl_fcat TO gt_fcat_0150.
  ELSEIF screen_principal = c_screen_0160.
    APPEND wl_fcat TO gt_fcat_0160.
  ELSEIF screen_principal = c_screen_0170.
    APPEND wl_fcat TO gt_fcat_0170.
  ENDIF.
  CLEAR wl_fcat.
ENDFORM.                    "ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  SET_DROPDOWN_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_dropdown_table.
  DATA: lt_dropdown TYPE lvc_t_drop,
        ls_dropdown TYPE lvc_s_drop.

  IF lt_dropdown IS INITIAL.
    ls_dropdown-handle = '1'.
    ls_dropdown-value  = 'Sim'.
    APPEND ls_dropdown TO lt_dropdown.

    ls_dropdown-handle = '1'.
    ls_dropdown-value  = 'Não'.
    APPEND ls_dropdown TO lt_dropdown.

    CALL METHOD obj_alv_0130->set_drop_down_table
      EXPORTING
        it_drop_down = lt_dropdown.
  ENDIF.
ENDFORM.                    "SET_DROPDOWN_TABLE

*&---------------------------------------------------------------------*
*&      Form  REGISTER_F4_FOR_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM register_f4_for_fields.
  DATA wl_f4 TYPE lvc_t_f4 WITH HEADER LINE.

  CASE screen_item.
    WHEN '0120'.
      wl_f4-fieldname = 'BCO_EMPRESA'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'BCO_PARCEIRO'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'BLOQ_PGTO'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'FILIAL'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'FORMA_PGTO'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'NRO_DOCUMENTO'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'PAIS_PGTO'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      CALL METHOD obj_alv_0120->register_f4_for_fields
        EXPORTING
          it_f4 = wl_f4[].

    WHEN '0130'.

      wl_f4-fieldname = 'CENTRO_CUSTO'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'CHASSI'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'FILIAL'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'IMOBILIZADO'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'MERCADORIA'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

*      WL_F4-FIELDNAME = 'NR_SERIE'.
*      WL_F4-REGISTER  = 'X' .
*      WL_F4-GETBEFORE = 'X' .
*      APPEND WL_F4.

      wl_f4-fieldname = 'UF'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      wl_f4-fieldname = 'VORNR'.
      wl_f4-register  = 'X' .
      wl_f4-getbefore = 'X' .
      APPEND wl_f4.

      CALL METHOD obj_alv_0130->register_f4_for_fields
        EXPORTING
          it_f4 = wl_f4[].
  ENDCASE.
ENDFORM.                    "REGISTER_F4_FOR_FIELDS

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->WL_EXC_BUTTON  text
*----------------------------------------------------------------------*
FORM exclude_tb_functions CHANGING wl_exc_button TYPE ui_functions.
  APPEND cl_gui_alv_grid=>mc_fc_refresh           TO wl_exc_button.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO wl_exc_button.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO wl_exc_button.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO wl_exc_button.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO wl_exc_button.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO wl_exc_button.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO wl_exc_button.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO wl_exc_button.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO wl_exc_button.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO wl_exc_button.
  APPEND cl_gui_alv_grid=>mc_fc_check             TO wl_exc_button.
ENDFORM.                    "EXCLUDE_TB_FUNCTIONS

*&---------------------------------------------------------------------*
*&      Form  TOP_PAGE_0150
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_page_0150.

  DATA: gt_header TYPE slis_t_listheader,
        lt_header TYPE slis_listheader.

  lt_header-typ  = 'H'.
  lt_header-info = 'Apropriação Seguros - Lançamentos'.
  APPEND lt_header TO gt_header.
  CLEAR lt_header.

  lt_header-typ  = 'S'.
  lt_header-key = 'Empresa: '.
  CONCATENATE wl_cabecalho_0150-bukrs '-' wl_cabecalho_0150-descr_bukrs
  INTO lt_header-info SEPARATED BY space.
  APPEND lt_header TO gt_header.
  CLEAR lt_header.

  lt_header-typ = 'S'.
  lt_header-key = 'Data Apropriação: '.
  CONCATENATE wl_cabecalho_0150-dt_aprop_de 'até' wl_cabecalho_0150-dt_aprop_ate
  INTO lt_header-info SEPARATED BY space.
  APPEND lt_header TO gt_header.
  CLEAR lt_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_header.
*     I_LOGO                   =
*     I_END_OF_LIST_GRID       =

ENDFORM.                    "TOP_PAGE_0150

*&---------------------------------------------------------------------*
*&      Form  F_DATE_INCREMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_date_increment CHANGING i_date TYPE sy-datum.

* INCREMENTA O MÊS, E SE CHEGAR NO MÊS 12 INCREMENTA O ANO.

  IF ( i_date+4(2) = 12 ).
    ADD 1 TO i_date(4).
    i_date+4(2) = 01.
  ELSE.
    ADD 1 TO i_date+4(2).
  ENDIF.

  i_date+6(2) = 01. "Atribuí a data o dia 01.

ENDFORM.                    "F_DATE_INCREMENT

*&---------------------------------------------------------------------*
*&      Form  F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_START    text
*      -->L_NAME     text
*      -->L_VALUE    text
*----------------------------------------------------------------------*
FORM f_preencher_dynpro USING l_start TYPE c
                              l_name  TYPE c
                              l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
        l_name  TO wl_bdc-program,
        l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
        l_name  TO wl_bdc-fnam,
        l_value TO wl_bdc-fval.
  ENDIF.

  APPEND wl_bdc TO gt_bdc.
  CLEAR wl_bdc.
ENDFORM.                    "F_PREENCHER_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS_IMOBILIZADO
*&---------------------------------------------------------------------*
FORM f_seleciona_dados_imobilizado USING p_value
                                CHANGING p_field
                                         p_imobilizado
                                         p_subnumero
                                         p_chassi
                                         p_nrserie
                                         p_descrbem
                                         p_centrocusto
                                         p_estado "Ex: 'MT'
                                         p_filial.
  DATA: lv_fieldname TYPE string,
        lw_zaa001    TYPE zaa001,
        vl_invnr     TYPE anla-invnr,
        vl_anln1     TYPE anla-anln1,
        vl_sernr     TYPE anla-sernr,
        vl_deakt     TYPE anla-deakt.

  CHECK ( p_value NE space ).
  CLEAR: wl_anla, wl_anlz.

  MOVE p_field TO lv_fieldname.
  TRANSLATE lv_fieldname TO LOWER CASE.

  CASE p_field.
    WHEN 'CHASSI'.

      vl_invnr = p_value.

      SELECT SINGLE *
        FROM anla
        INTO wl_anla
       WHERE bukrs EQ wl_cabecalho_0110-bukrs
         AND invnr EQ vl_invnr
         AND deakt EQ vl_deakt.

    WHEN 'IMOBILIZADO'.

      vl_anln1 = p_value.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vl_anln1
        IMPORTING
          output = vl_anln1.

      SELECT SINGLE *
        FROM anla
        INTO wl_anla
       WHERE bukrs EQ wl_cabecalho_0110-bukrs
         AND anln1 EQ vl_anln1
         AND deakt EQ vl_deakt
         AND anln2 EQ '0000'.

    WHEN 'NR_SERIE'.

      vl_sernr = p_value.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vl_sernr
        IMPORTING
          output = vl_sernr.

      SELECT SINGLE *
        FROM anla
        INTO wl_anla
       WHERE bukrs EQ wl_cabecalho_0110-bukrs
         AND sernr EQ vl_sernr
         AND deakt EQ vl_deakt.

  ENDCASE.

  SELECT SINGLE *
    FROM anlz
    INTO wl_anlz
   WHERE bukrs = wl_anla-bukrs
     AND anln1 = wl_anla-anln1
     AND anln2 = wl_anla-anln2
     AND adatu <= sy-datum
     AND bdatu >= sy-datum.

*  SELECT SINGLE COD_REGI
*    FROM ZAA001
*    INTO P_ESTADO
*   WHERE BUKRS = WL_ANLA-BUKRS
*     AND ANLN1 = WL_ANLA-ANLN1.

  IF ( sy-subrc IS NOT INITIAL ).
    CLEAR: wl_anla, wl_anlz.
    MESSAGE s836(sd) WITH TEXT-e09 lv_fieldname DISPLAY LIKE 'E'.
  ELSE.
    IF ( wl_anla-bukrs NE wl_cabecalho_0110-bukrs ).
      CLEAR: wl_anla, wl_anlz.
      MESSAGE s836(sd) WITH TEXT-e15 DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

  p_imobilizado = wl_anla-anln1.
  p_subnumero   = wl_anla-anln2.
  p_chassi      = wl_anla-invnr.
  p_nrserie     = wl_anla-sernr.
  p_descrbem    = wl_anla-txt50.
  p_centrocusto = wl_anlz-kostl.
  p_filial      = wl_anlz-gsber.

ENDFORM.                    "F_SELECIONA_DADOS_IMOBILIZADO
*&---------------------------------------------------------------------*
*&      Form  IMPORT_BENS_ASSEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_import_bens_asseg .

  DATA: vl_fieldname       TYPE lvc_s_modi-fieldname,
        vl_value           TYPE lvc_s_modi-value,
        vl_chassi_aux      TYPE ty_saida_0130-chassi,
        vl_imobilizado_aux TYPE ty_saida_0130-imobilizado,
        vl_txt_imob        TYPE itex132,
        vl_break_import    TYPE c,
        vl_lines_return    TYPE i,
        vl_index           TYPE sy-tabix.

  DATA: wl_saida_0130_aux TYPE ty_saida_0130.

  DATA: go_utils         TYPE REF TO zutils,
        r_operacao       TYPE REF TO z_tipo_operacao,
        r_seguro_geracao TYPE REF TO z_seguro_geracao,
        r_insert_row_alv TYPE REF TO z_insert_row_alv.

  CREATE OBJECT: go_utils, r_operacao, r_seguro_geracao, r_insert_row_alv.

  DATA: gt_planilha LIKE STANDARD TABLE OF alsmex_tabline,
        wl_planilha LIKE alsmex_tabline.

  REFRESH gt_msg_return.

  IF ( header_status = abap_true ). "Verifica se o cabeçalho foi preenchido.


    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = TEXT-i39.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_file
        i_begin_col             = 1
        i_begin_row             = 2
        i_end_col               = 55
        i_end_row               = 10000
      TABLES
        intern                  = gt_planilha
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.

  ELSE.
    MESSAGE s836(sd) WITH TEXT-e17 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: vl_break_import.

  vl_index = 0.

  LOOP AT gt_planilha INTO wl_planilha.
    AT NEW row.

      vl_lines_return = lines( gt_msg_return ).

      go_utils->z_validar_info_alv_0130( i_valida_tot = '' ). "Em seguida valida a tela do Bens Assegurados.

      IF ( vl_lines_return <> lines( gt_msg_return ) ) OR
         ( vl_break_import IS NOT INITIAL ).
        EXIT.
      ELSE.
        r_insert_row_alv->insert_row_tela_0130( EXPORTING append_table = '' ).
      ENDIF.

    ENDAT.

    IF wl_planilha-value(1) = space.
      SHIFT wl_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE wl_planilha-col.
      WHEN 1.
        wl_saida_0130-chassi   = wl_planilha-value.
      WHEN 2.
        wl_saida_0130-nr_serie = wl_planilha-value.
      WHEN 3.
        wl_saida_0130-imobilizado = wl_planilha-value.
      WHEN 4.
        PERFORM tratar_campo CHANGING wl_planilha-value.
        TRY.
            wl_saida_0130-vlr_premio_usd = wl_planilha-value.
          CATCH cx_sy_conversion_no_number.
            DATA(lva_erro) = |Verificar arquivo! Caracter não numérico: { wl_planilha-value }|.
          CATCH cx_sy_conversion_overflow.
            lva_erro = |Verificar arquivo! Tamanho do campo: { wl_planilha-value }|.
        ENDTRY.

      WHEN 5.
        PERFORM tratar_campo CHANGING wl_planilha-value.
        TRY.
            wl_saida_0130-vlr_premio_brl = wl_planilha-value.
          CATCH cx_sy_conversion_no_number.
            lva_erro = |Verificar arquivo! Caracter não numérico: { wl_planilha-value }|.
          CATCH cx_sy_conversion_overflow.
            lva_erro = |Verificar arquivo! Tamanho do campo: { wl_planilha-value }|.
        ENDTRY.

      WHEN 6.
        IF ( wl_planilha-value EQ 'X' ).
          wl_saida_0130-clau_benef = 'SIM'.
        ENDIF.
      WHEN 7.
        wl_saida_0130-banco          = wl_planilha-value.
      WHEN 8.
        wl_saida_0130-filial         = wl_planilha-value.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_saida_0130-filial
          IMPORTING
            output = wl_saida_0130-filial.

      WHEN 9.
        wl_saida_0130-centro_custo   = wl_planilha-value.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_saida_0130-centro_custo
          IMPORTING
            output = wl_saida_0130-centro_custo.

      WHEN 10.
        wl_saida_0130-descr_bens     = wl_planilha-value.

      WHEN 11.
        PERFORM tratar_campo CHANGING wl_planilha-value.
        TRY.
            wl_saida_0130-vlr_risco_usd = wl_planilha-value.
          CATCH cx_sy_conversion_no_number.
            lva_erro = |Verificar arquivo! Caracter não numérico: { wl_planilha-value }|.
          CATCH cx_sy_conversion_overflow.
            lva_erro = |Verificar arquivo! Tamanho do campo: { wl_planilha-value }|.
        ENDTRY.
      WHEN 12.
        PERFORM tratar_campo CHANGING wl_planilha-value.
        wl_saida_0130-vlr_risco_brl = wl_planilha-value.
      WHEN 13.
        wl_saida_0130-aufnr = wl_planilha-value.

    ENDCASE.

    IF ( lva_erro IS NOT INITIAL ).
      MESSAGE lva_erro TYPE 'I'.
      EXIT.
    ENDIF.

    AT END OF row.

      ADD 1 TO vl_index.

      IF ( wl_saida_0130-chassi      IS NOT INITIAL ) OR
         ( wl_saida_0130-nr_serie    IS NOT INITIAL ) OR
         ( wl_saida_0130-imobilizado IS NOT INITIAL ) OR
         ( wl_saida_0130-descr_bens  IS NOT INITIAL ).

        CLEAR: vl_fieldname, vl_value, wl_saida_0130_aux.

        IF wl_saida_0130-chassi IS NOT INITIAL.
          vl_fieldname = 'CHASSI'.
          vl_value     = wl_saida_0130-chassi.
          CONCATENATE 'CHASSI: ' wl_saida_0130-chassi INTO vl_txt_imob SEPARATED BY space.
        ELSEIF wl_saida_0130-nr_serie IS NOT INITIAL.
          vl_fieldname = 'NR_SERIE'.
          vl_value     = wl_saida_0130-nr_serie.
          CONCATENATE 'Série: ' wl_saida_0130-nr_serie INTO vl_txt_imob SEPARATED BY space.
        ELSEIF wl_saida_0130-imobilizado IS NOT INITIAL.
          vl_fieldname = 'IMOBILIZADO'.
          vl_value     = wl_saida_0130-imobilizado.
          CONCATENATE 'Imobilizado: ' wl_saida_0130-imobilizado INTO vl_txt_imob SEPARATED BY space.
        ENDIF.

        IF ( wl_saida_0130-vlr_premio_usd EQ 0   ) AND
           ( wl_saida_0130-vlr_premio_brl > 0    ) AND
           ( abs( wl_cabecalho_0110-wkurs ) >  0 ).
          wl_saida_0130-vlr_premio_usd = wl_saida_0130-vlr_premio_brl / abs( wl_cabecalho_0110-wkurs ).
        ENDIF.

        IF ( wl_saida_0130-vlr_premio_usd > 0    ) AND
           ( wl_saida_0130-vlr_premio_brl EQ 0   ) AND
           ( abs( wl_cabecalho_0110-wkurs ) >  0 ).
          wl_saida_0130-vlr_premio_brl =  wl_saida_0130-vlr_premio_usd * abs( wl_cabecalho_0110-wkurs ).
        ENDIF.

        IF ( wl_saida_0130-vlr_risco_usd EQ 0   ) AND
           ( wl_saida_0130-vlr_risco_brl > 0    ) AND
           ( abs( wl_cabecalho_0110-wkurs ) >  0 ).
          wl_saida_0130-vlr_risco_usd = wl_saida_0130-vlr_risco_brl / abs( wl_cabecalho_0110-wkurs ).
        ENDIF.

        IF ( wl_saida_0130-vlr_risco_usd > 0    ) AND
           ( wl_saida_0130-vlr_risco_brl EQ 0   ) AND
           ( abs( wl_cabecalho_0110-wkurs ) >  0 ).
          wl_saida_0130-vlr_risco_brl =  wl_saida_0130-vlr_risco_usd * abs( wl_cabecalho_0110-wkurs ).
        ENDIF.


        IF ( vl_value IS NOT INITIAL ).
          PERFORM f_seleciona_dados_imobilizado USING vl_value
                                             CHANGING vl_fieldname
                                                      wl_saida_0130-imobilizado
                                                      wl_saida_0130-subnumero
                                                      wl_saida_0130-chassi
                                                      wl_saida_0130-nr_serie
                                                      wl_saida_0130-descr_bens
                                                      wl_saida_0130-centro_custo
                                                      wl_saida_0130-uf
                                                      wl_saida_0130-filial.
        ENDIF.

        IF ( wl_saida_0130-chassi      IS NOT INITIAL ) OR
           ( wl_saida_0130-nr_serie    IS NOT INITIAL ) OR
           ( wl_saida_0130-imobilizado IS NOT INITIAL ).

          READ TABLE gt_saida_0130 INTO wl_saida_0130_aux
            WITH KEY chassi      = wl_saida_0130-chassi
                     nr_serie    = wl_saida_0130-nr_serie
                     imobilizado = wl_saida_0130-imobilizado.

          IF sy-subrc NE 0.
            APPEND wl_saida_0130 TO gt_saida_0130.
          ELSE.
            go_utils->z_criar_mensagem_erro( msg_type = 'I'
                                             text1 = TEXT-s02
                                             text2 = vl_txt_imob
                                             field = 'IMOBILIZADO'
                                             index = vl_index
                                             aba   = '0130' ).
          ENDIF.

        ELSEIF ( wl_saida_0130-descr_bens IS NOT INITIAL ).
          APPEND wl_saida_0130 TO gt_saida_0130.
        ELSE.

          go_utils->z_criar_mensagem_erro(  msg_type = 'E'
                                            text1 = TEXT-e31
                                            text2 = vl_txt_imob
                                            field = 'IMOBILIZADO'
                                            index = vl_index
                                            aba   = '0130' ).
          "VL_BREAK_IMPORT = 'X'.

        ENDIF.

      ENDIF.

    ENDAT.

  ENDLOOP.

  CALL METHOD obj_alv_0130->refresh_table_display
    EXPORTING
      is_stable = wl_stable.

  IF vl_index > 0.
    MESSAGE s836(sd) WITH TEXT-s01 DISPLAY LIKE 'S'.
    LEAVE TO SCREEN 0.
  ELSE.
    MESSAGE s836(sd) WITH TEXT-i41 DISPLAY LIKE 'I'.
  ENDIF.

ENDFORM.

FORM tratar_campo CHANGING v_value.
  REPLACE '.' WITH ' ' INTO v_value.
  REPLACE ',' WITH '.' INTO v_value.
  REPLACE '-' WITH ' ' INTO v_value.

  CONDENSE v_value NO-GAPS.
ENDFORM.                    "
*&---------------------------------------------------------------------*
*&      Form  VALIDA_COMPETENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_CABECALHO_0150_COMPETENCIA  text
*      <--P_RETURN_STATUS  text
*----------------------------------------------------------------------*
FORM valida_competencia  USING    p_competencia
                         CHANGING p_return_status
                                  p_dt_ini
                                  p_dt_fim.

  DATA: vl_mes         TYPE i,
        vl_ano         TYPE i,
        vl_dt_ini(8)   TYPE c,
        vl_dt_fim(8)   TYPE c,
        vl_dt_low      TYPE sy-datum,
        vl_dt_high_in  TYPE sy-datum,
        vl_dt_high_out TYPE sy-datum.

**<<<------"164255 - NMS - INI------>>>
*  vl_mes = p_competencia(02).
*  vl_ano = p_competencia+2(04).
  vl_mes = p_competencia+4(02).
  vl_ano = p_competencia(04).
**<<<------"164255 - NMS - FIM------>>>
  IF ( vl_mes = 0 ) OR ( vl_mes > 12 ).
    p_return_status = 'X'.
    MESSAGE s836(sd) WITH TEXT-e29 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF ( vl_ano = 0 ).
    p_return_status = 'X'.
    MESSAGE s836(sd) WITH TEXT-e30 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
**<<<------"164255 - NMS - INI------>>>
*  CONCATENATE p_competencia+2(4) p_competencia(2) '01' INTO vl_dt_ini.
*
*  CONCATENATE p_competencia+2(4) p_competencia(2) '01' INTO vl_dt_fim.
*
*  vl_dt_low     = vl_dt_ini.
*  vl_dt_high_in = vl_dt_fim.
  CONCATENATE p_competencia(4) p_competencia+4(02) '01' INTO vl_dt_ini.
  vl_dt_high_in = vl_dt_low = vl_dt_fim = vl_dt_ini.
**<<<------"164255 - NMS - FIM------>>>
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = vl_dt_high_in
    IMPORTING
      last_day_of_month = vl_dt_high_out.

  p_dt_ini  = vl_dt_low.
  p_dt_fim  = vl_dt_high_out.

ENDFORM.

FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  IF p_outputlen IS INITIAL.
    wa_estrutura-outputlen     = x_contador.
  ELSE.
    wa_estrutura-outputlen     =  p_outputlen.
  ENDIF.

  APPEND wa_estrutura TO it_estrutura.

ENDFORM.                    " montar_estrutura

FORM montar_layout_log_ccusto.

  REFRESH it_estrutura[].
  PERFORM montar_estrutura USING:
     1  ''   ''            'GT_ZGLT079' 'DT_ATUAL'    'Data.Alteração'     '14',
     1  ''   ''            'GT_ZGLT079' 'HR_ATUAL'    'Hora'               '10',
     1  ''   ''            'GT_ZGLT079' 'USNAME'      'Usuário'            '10',
     1  ''   ''            'GT_ZGLT079' 'OLD_WERKS'   'Divisão Anterior'   '16',
     1  ''   ''            'GT_ZGLT079' 'OLD_KOSTL'   'C.Custo Anterior'   '16',
     1  ''   ''            'GT_ZGLT079' 'NEW_WERKS'   'Divisão Nova'       '12',
     1  ''   ''            'GT_ZGLT079' 'NEW_KOSTL'   'C.Custo Novo'       '12'.

ENDFORM.                    " MONTAR_LAYOUT

FORM load_seq_lcto USING p_seq_lcto TYPE zglt050-seq_lcto.

  CHECK p_seq_lcto IS NOT INITIAL.

  PERFORM f_action_search.
  wl_cabecalho_0110-seq_lcto = p_seq_lcto.
  PERFORM f_action_enter.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ACTION_SEARCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_action_search .

  CLEAR: gt_fields,
         wl_cabecalho_0110,
         gt_saida_0120,
         gt_saida_0130,
         gt_editor.

  CALL METHOD obj_custom_editor->set_text_as_r3table
    EXPORTING
      table = gt_editor.

  op_modo = c_search.

  go_utils->z_tratar_campos( name      = space
                             group1    = 'NU1'
                             group2    = space
                             value     = '1'
                             invisible = '0' ).

  go_utils->z_tratar_campos( name      = space
                             group1    = 'GR1'
                             group2    = space
                             value     = '0'
                             invisible = '0' ).
  custom_mode = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ACTION_ENTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_action_enter .

  DATA: r_tipo_operacao TYPE REF TO z_tipo_operacao,
        r_event_click   TYPE REF TO lcl_event_handler,
        r_insert_row    TYPE REF TO z_insert_row_alv.

  CREATE OBJECT: r_tipo_operacao,
                 r_insert_row,
                 go_utils.

  op_modo = c_enter.

  IF ( wl_cabecalho_0110-seq_lcto IS INITIAL ).

    go_utils->z_validar_cabecalho_0110( ).

    IF ( wl_cabecalho_0110-seq_parc <> lines( gt_saida_0120 ) ).
      r_insert_row->insert_row_tela_0120( ).
    ENDIF.

    CHECK ( return_status IS INITIAL
      AND   gt_msg_return IS INITIAL ).


  ELSEIF ( wl_cabecalho_0110-seq_parc <> lines( gt_saida_0120 ) ).
    r_insert_row->insert_row_tela_0120( ).
  ELSE.
    r_tipo_operacao->z_pesquisar_registros( ).
  ENDIF.

ENDFORM.

FORM f_monta_range  USING p_screen.

  CLEAR: r_bukrs[], r_seq_lcto[], r_seq_tipo[].

  CASE p_screen.
    WHEN '0150'.

      IF wl_cabecalho_0150-bukrs IS NOT INITIAL.
        r_bukrs-sign    = 'I'.
        r_bukrs-option  = 'EQ'.
        r_bukrs-low     = wl_cabecalho_0150-bukrs.
        r_bukrs-high    = wl_cabecalho_0150-bukrs.
        APPEND r_bukrs.
      ENDIF.

      IF r_seq_150[] IS NOT INITIAL.
        r_seq_lcto[] = r_seq_150[].
      ELSEIF wl_cabecalho_0150-seq_lcto IS NOT INITIAL.
        r_seq_lcto-sign    = 'I'.
        r_seq_lcto-option  = 'EQ'.
        r_seq_lcto-low     = wl_cabecalho_0150-seq_lcto.
        r_seq_lcto-high    = wl_cabecalho_0150-seq_lcto.
        APPEND r_seq_lcto.
      ENDIF.

      IF wl_cabecalho_0150-seq_tipo IS NOT INITIAL.
        r_seq_tipo-sign    = 'I'.
        r_seq_tipo-option  = 'EQ'.
        r_seq_tipo-low     = wl_cabecalho_0150-seq_tipo.
        r_seq_tipo-high    = wl_cabecalho_0150-seq_tipo.
        APPEND r_seq_tipo.
      ENDIF.

    WHEN '0160'.
      DATA: wl_r_bukrs LIKE LINE OF r_bukrs_160[].
      DATA: wl_r_seq   LIKE LINE OF r_seq_160[].
      DATA: wl_r_tipo  LIKE LINE OF r_tipo_160[].

*------------------------------BUKRS
      IF r_bukrs_160[] IS NOT INITIAL.
        LOOP AT r_bukrs_160[] INTO wl_r_bukrs.
          r_bukrs-sign    = 'I'.
          r_bukrs-option  = 'EQ'.
          r_bukrs-low     = wl_r_bukrs-low.
          APPEND r_bukrs.
        ENDLOOP.
      ELSEIF r_bukrs_160[] IS INITIAL AND wl_cabecalho_0160-bukrs IS NOT INITIAL.
        r_bukrs-sign    = 'I'.
        r_bukrs-option  = 'EQ'.
        r_bukrs-low     = wl_cabecalho_0160-bukrs.
        r_bukrs-high    = wl_cabecalho_0160-bukrs.
        APPEND r_bukrs.
*      ELSEIF R_BUKRS_160[] IS INITIAL AND WL_CABECALHO_0160-BUKRS IS INITIAL.
*        R_BUKRS-SIGN    = 'I'.
*        R_BUKRS-OPTION  = 'EQ'.
*        R_BUKRS-LOW     = '0000'.
*        R_BUKRS-HIGH    = '9999'.
*        APPEND R_BUKRS.
      ENDIF.
*-------------------------------SEQ_LCTO
      IF r_seq_160[] IS NOT INITIAL.
        LOOP AT r_seq_160[] INTO wl_r_seq.
          r_seq_lcto-sign    = 'I'.
          r_seq_lcto-option  = 'EQ'.
          r_seq_lcto-low     = wl_r_seq-low.
          APPEND r_bukrs.
        ENDLOOP.
      ELSEIF r_seq_160[] IS INITIAL AND wl_cabecalho_0160-seq_lcto IS NOT INITIAL.
        r_seq_lcto-sign    = 'I'.
        r_seq_lcto-option  = 'EQ'.
        r_seq_lcto-low     = wl_cabecalho_0160-seq_lcto.
        r_seq_lcto-high    = wl_cabecalho_0160-seq_lcto.
        APPEND r_seq_lcto.
*      ELSEIF R_TIPO_160[] IS INITIAL AND WL_CABECALHO_0160-SEQ_LCTO IS INITIAL.
*        R_SEQ_LCTO-SIGN    = 'I'.
*        R_SEQ_LCTO-OPTION  = 'EQ'.
*        R_SEQ_LCTO-LOW     = '*'.
*        APPEND R_SEQ_LCTO.
      ENDIF.
*-------------------------------SEQ_TIPO
      IF r_tipo_160[] IS NOT INITIAL.
        LOOP AT r_tipo_160 INTO wl_r_tipo.
          r_seq_tipo-sign    = 'I'.
          r_seq_tipo-option  = 'EQ'.
          r_seq_tipo-low     = wl_r_tipo-low.
          APPEND r_seq_tipo.
        ENDLOOP.
      ELSEIF r_tipo_160[] IS INITIAL AND wl_cabecalho_0160-seq_tipo IS NOT INITIAL.
        r_seq_tipo-sign    = 'I'.
        r_seq_tipo-option  = 'EQ'.
        r_seq_tipo-low     = wl_cabecalho_0160-seq_tipo.
        r_seq_tipo-high    = wl_cabecalho_0160-seq_tipo.
        APPEND r_seq_tipo.
*      ELSEIF R_TIPO_160[] IS INITIAL AND WL_CABECALHO_0160-SEQ_TIPO IS INITIAL.
*        R_SEQ_TIPO-SIGN    = 'I'.
*        R_SEQ_TIPO-OPTION  = 'EQ'.
*        R_SEQ_TIPO-LOW     = '000'.
*        R_SEQ_TIPO-HIGH    = '999'.
*        APPEND R_SEQ_TIPO.
      ENDIF.

    WHEN '0170'.

      IF wl_cabecalho_0170-bukrs IS NOT INITIAL.
        r_bukrs-sign    = 'I'.
        r_bukrs-option  = 'EQ'.
        r_bukrs-low     = wl_cabecalho_0170-bukrs.
        r_bukrs-high    = wl_cabecalho_0170-bukrs.
        APPEND r_bukrs.
      ENDIF.

      IF wl_cabecalho_0170-seq_lcto IS NOT INITIAL.
        r_seq_lcto-sign    = 'I'.
        r_seq_lcto-option  = 'EQ'.
        r_seq_lcto-low     = wl_cabecalho_0170-seq_lcto.
        r_seq_lcto-high    = wl_cabecalho_0170-seq_lcto.
        APPEND r_seq_lcto.
      ENDIF.

      IF wl_cabecalho_0170-seq_tipo IS NOT INITIAL.
        r_seq_tipo-sign    = 'I'.
        r_seq_tipo-option  = 'EQ'.
        r_seq_tipo-low     = wl_cabecalho_0170-seq_tipo.
        r_seq_tipo-high    = wl_cabecalho_0170-seq_tipo.
        APPEND r_seq_tipo.
      ENDIF.


  ENDCASE.


ENDFORM.

FORM f_check_filial USING p_bukrs
                          p_branch.

  DATA: vl_bukrs  TYPE j_1bbranch-bukrs,
        vs_t001   TYPE t001,
        vs_tgsb   TYPE tgsb,
        vl_branch TYPE j_1bbranch-branch.

  IF ( p_bukrs  IS INITIAL ) OR
     ( p_branch IS INITIAL ).
    sy-subrc = 1.
    EXIT.
  ENDIF.

  vl_bukrs  = p_bukrs.
  vl_branch = p_branch.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vl_bukrs
    IMPORTING
      output = vl_bukrs.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vl_branch
    IMPORTING
      output = vl_branch.

  p_bukrs  = vl_bukrs.
  p_branch = vl_branch.

  SELECT SINGLE *
    FROM j_1bbranch INTO @DATA(_wl_j_1bbranch)
   WHERE bukrs  = @vl_bukrs
     AND branch = @vl_branch.

  IF sy-subrc NE 0. " Caso não encontre

    CONCATENATE vl_bukrs '-' vl_branch INTO DATA(_emp_filial).

    "Verificar set
    SELECT SINGLE *
      FROM setleaf INTO @DATA(_wl_setleaf)
     WHERE setname = 'MAGGI_ZGL0016_DIV'
       AND valfrom = @_emp_filial.

  ENDIF.



ENDFORM.

FORM f_check_authority  USING p_bukrs
                              p_msg
                     CHANGING p_ok.

  p_ok = abap_false.

  IF p_bukrs IS INITIAL.
    IF p_msg IS NOT INITIAL.
      MESSAGE 'Empresa não informada!' TYPE 'S'.
    ENDIF.
    EXIT.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZGL047' ID 'BUKRS' FIELD p_bukrs.

  IF sy-subrc <> 0.

    IF p_msg IS NOT INITIAL.
      SELECT SINGLE *
        FROM user_addrp INTO @DATA(wl_user)
       WHERE bname = @sy-uname.

      IF ( sy-subrc = 0 ) AND ( wl_user-name_first IS NOT INITIAL ).
        MESSAGE | { wl_user-name_first }, seu perfil está sem acesso as apólices da empresa selecionada! | TYPE 'S'.
      ELSE.
        MESSAGE | Perfil do usuário sem acesso as apólices da empresa selecionada! | TYPE 'S'.
      ENDIF.
    ENDIF.

    RETURN.
  ENDIF.

  p_ok = abap_true.

ENDFORM.

FORM f_anexo_apolice.

  DATA: vl_obj_key    TYPE sibflporb-instid,
        vl_lines      TYPE i,
        anexos        TYPE TABLE OF bdn_con,
        vl_ip_mode    TYPE sgs_rwmod,
        vl_ip_service TYPE sgs_srvnam,
        wa_bor        TYPE borident,
        anexo_obj     TYPE REF TO cl_gos_manager.

  DATA: vl_display_mode TYPE xfeld,
        wl_header       TYPE thead,
        wl_name         TYPE thead-tdname,
        it_texto        TYPE STANDARD TABLE OF tline,
        wa_texto        TYPE tline,
        tl_texto        TYPE catsxt_longtext_itab,
        wl_texto        TYPE LINE OF catsxt_longtext_itab.

  CHECK wl_cabecalho_0110-seq_lcto IS NOT INITIAL.

  IF op_modo = c_change.
    MESSAGE s836(sd) WITH TEXT-e64 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CREATE OBJECT anexo_obj TYPE cl_gos_manager.

  vl_ip_mode     = 'E'.

  wa_bor-objtype = 'ZGL047'.
  wa_bor-objkey  = wl_cabecalho_0110-seq_lcto.
  vl_obj_key     = wl_cabecalho_0110-seq_lcto.

  PERFORM f_get_anexos TABLES anexos
                        USING vl_obj_key.

  IF lines( anexos[] ) > 0.
    vl_ip_service  = 'VIEW_ATTA'.
  ELSE.
    vl_ip_service = 'PCATTA_CREA'.
  ENDIF.

  anexo_obj->set_rw_mode( ip_mode = vl_ip_mode ).

  anexo_obj->start_service_direct(
    EXPORTING
      ip_service         = vl_ip_service
      is_object          = wa_bor
    EXCEPTIONS
      no_object          = 1
      object_invalid     = 2
      execution_failed   = 3
      OTHERS             = 4 ).

  COMMIT WORK.

ENDFORM.

FORM f_get_anexos TABLES p_anexos STRUCTURE bdn_con
                   USING p_obj_key.

  DATA: v_class TYPE bapibds01-classname.
  CLEAR: p_anexos[].

  v_class = 'ZGL047'.

  CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
    EXPORTING
      classname          = v_class
      objkey             = p_obj_key
      client             = sy-mandt
    TABLES
      gos_connections    = p_anexos
    EXCEPTIONS
      no_objects_found   = 1
      internal_error     = 2
      internal_gos_error = 3
      OTHERS             = 4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AUTORIZACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM autorizacao .

  DATA: _param TYPE  ustyp_t_parameters.

* // Pega os Parametros do Usuario
  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = _param
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.

  DELETE _param WHERE parid NE 'ZGL047_CAD'.

  IF _param[] IS NOT INITIAL.
*   "// Pega as informações do parametro
    autorizado = _param[ 1 ]-parva.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P  text
*      <--P_OK  text
*----------------------------------------------------------------------*
FORM zf_valida_campos  CHANGING p_ok.

*----------------------------------------------------------------------
  " Valida Preenchimento da SEq. no caso de prorroga~ção
*----------------------------------------------------------------------
  IF wl_cabecalho_0110-ref_seq_lcto IS INITIAL AND wl_cabecalho_0110-tp_opr = 'P'.
    p_ok = abap_true.
    MESSAGE s000(zles) WITH 'Informe Seq. Lançamento'
          DISPLAY LIKE 'E'.
  ENDIF.

*----------------------------------------------------------------------
  " Valida Limite de data
*----------------------------------------------------------------------
  IF wl_cabecalho_0110-vig_de > wl_cabecalho_0110-vig_ate.
    p_ok = abap_true.
    MESSAGE s000(zles) WITH 'Limite da vigência inferior do contrato,' ' maior que o limite superior'
          DISPLAY LIKE 'E'.
  ENDIF.

*----------------------------------------------------------------------
* Validar códgio da corretora
*----------------------------------------------------------------------
*  IF wl_cabecalho_0110-cod_corretora IS NOT INITIAL.
*
*    DATA: l_corretora TYPE lfa1-lifnr.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = wl_cabecalho_0110-cod_corretora
*      IMPORTING
*        output = l_corretora.
*
*    SELECT SINGLE lifnr FROM lfa1
*      INTO @DATA(l_lifnr)
*      WHERE lifnr = @l_corretora.
*
*    IF sy-subrc IS NOT INITIAL.
*      p_ok = abap_true.
*      MESSAGE s000(zles) WITH 'Código da corretora não encontrado!'
*      DISPLAY LIKE 'E'.
*    ENDIF.
*
*  ENDIF.

*----------------------------------------------------------------------
* Empresa Filial
*----------------------------------------------------------------------

  IF gt_saida_0120[] IS NOT INITIAL.

    LOOP AT gt_saida_0120 INTO DATA(w_saida_0120).

      PERFORM f_check_filial USING wl_cabecalho_0110-bukrs
                                   w_saida_0120-filial.

      IF sy-subrc IS NOT INITIAL.
        p_ok = abap_true.
        MESSAGE s000(zles) WITH 'Filial ' w_saida_0120-filial 'não encontrada para empresa informada!'
      DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ACTION_ENTER_PRORROG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_action_enter_prorrog .

  DATA: r_tipo_operacao TYPE REF TO z_tipo_operacao,
        r_event_click   TYPE REF TO lcl_event_handler,
        r_insert_row    TYPE REF TO z_insert_row_alv.

  CREATE OBJECT: r_tipo_operacao,
                 r_insert_row,
                 go_utils.

  op_modo = c_enter.

  IF ( wl_cabecalho_0110-ref_seq_lcto IS INITIAL ).

    go_utils->z_validar_cabecalho_0110( ).

    IF ( wl_cabecalho_0110-seq_parc <> lines( gt_saida_0120 ) ).
      r_insert_row->insert_row_tela_0120( ).
    ENDIF.

    CHECK ( return_status IS INITIAL
      AND   gt_msg_return IS INITIAL ).


  ELSEIF ( wl_cabecalho_0110-seq_parc <> lines( gt_saida_0120 ) ).
    r_insert_row->insert_row_tela_0120( ).
  ELSE.
    r_tipo_operacao->z_pesquisar_registros_prorrog( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MOSTRAR_LOG_ERRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_OBJKEY  text
*----------------------------------------------------------------------*
FORM zf_mostrar_log_erro  USING   p_lva_chave_zib.

  TYPES: BEGIN OF ty_zib_err,
           obj_key        TYPE zib_contabil_err-obj_key,
           dt_atualizacao TYPE zib_contabil_err-dt_atualizacao,
           hr_atualizacao TYPE zib_contabil_err-hr_atualizacao,
           message        TYPE zib_contabil_err-message,
         END OF ty_zib_err.

  DATA: lit_zib_err TYPE TABLE OF ty_zib_err.

  IF ( p_lva_chave_zib IS NOT INITIAL ).

    SELECT obj_key, dt_atualizacao, hr_atualizacao, message
       FROM zib_contabil_err INTO TABLE @lit_zib_err
      WHERE obj_key = @p_lva_chave_zib.

    IF ( sy-subrc = 0 ).

      cl_demo_output=>new(
        )->begin_section( `ZIB_CONTABIL_ERR:`
        )->write_text( |Log de Erros de processamento contábeis: \n|
        ")->WRITE_DATA( SY-DATUM
        )->write_data( lit_zib_err[]
        )->end_section(
        )->display( ).

    ENDIF.

  ENDIF.

ENDFORM.

"USER STORY 163032 - MMSILVA - 09.01.2025 - Inicio
FORM f_submit_rsvtprot.

  TYPES: BEGIN OF ty_gt_saida_0150,
           dt_inic_vigenc TYPE zglt068-dt_in_vig,
           seq_lcto       TYPE zglt050-seq_lcto,
         END OF ty_gt_saida_0150.

  TYPES: BEGIN OF ty_submit_rsvtprot,
           tlog_logdate     TYPE dats,
           tlog_logtime     TYPE tims,
           tlog_username    TYPE c LENGTH 12,
           seq_lcto         TYPE n LENGTH 10,
           tlog_optype_text TYPE c LENGTH 20,
           seq_tipo         TYPE n LENGTH 3,
         END OF ty_submit_rsvtprot.

  DATA: lo_data            TYPE REF TO data,
        wa_gt_saida_0150   TYPE ty_gt_saida_0150,
        it_gt_saida_0150   TYPE TABLE OF ty_gt_saida_0150,
        it_submit_rsvtprot TYPE TABLE OF ty_submit_rsvtprot,
        wa_submit_rsvtprot TYPE ty_submit_rsvtprot.

  FIELD-SYMBOLS: <postab>         TYPE ANY TABLE,
                 <wa_pos>         TYPE any,
                 <fs>             TYPE any,
                 <fs_inic_vigenc> TYPE any,
                 <fs_seq_lcto>    TYPE any.

  cl_salv_bs_runtime_info=>set(
    EXPORTING
      display  = abap_false
      metadata = abap_false
      data     = abap_true ).

  ASSIGN ('DT_INIC_VIGENC') TO <fs_inic_vigenc>.

  SUBMIT rsvtprot
      WITH cusobj   EQ 'ZGLT050'
      WITH tabfirst EQ 'X'
      WITH alv_grid EQ 'X'
      WITH objfirst EQ ''
      WITH ign_unch EQ ''
      WITH addtblgs EQ ''
      WITH dbeg     EQ '00000000'
      WITH dend     EQ sy-datum
      WITH tbeg     EQ '000000'
      WITH tend     EQ sy-uzeit AND RETURN.

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
       IMPORTING
         r_data = lo_data ).

      ASSIGN lo_data->* TO <postab>.
    CATCH cx_salv_bs_sc_runtime_info.
      MESSAGE 'Unable to retrieve ALV data' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  CHECK <postab> IS ASSIGNED.

  MOVE-CORRESPONDING <postab> TO it_submit_rsvtprot.

  DELETE it_submit_rsvtprot WHERE seq_lcto NE wl_cabecalho_0110-seq_lcto.

  SORT it_submit_rsvtprot DESCENDING BY tlog_logdate tlog_logtime.

  PERFORM montar_layout_log_hist.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat           = it_estrutura[]
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 100
      i_screen_end_line     = 13
    TABLES
      t_outtab              = it_submit_rsvtprot.

ENDFORM.

FORM montar_layout_log_hist.
  REFRESH it_estrutura[].
  PERFORM montar_estrutura USING:
     1  ''   ''            'it_submit_rsvtprot' 'TLOG_LOGDATE'     'Data Alteração' '14',
     2  ''   ''            'it_submit_rsvtprot' 'TLOG_LOGTIME'     'Hora Alteração' '10',
     3  ''   ''            'it_submit_rsvtprot' 'TLOG_USERNAME'    'Usuário'        '10',
     4  ''   ''            'it_submit_rsvtprot' 'SEQ_LCTO'         'Seq. Lcto'      '16',
     5  ''   ''            'it_submit_rsvtprot' 'TLOG_OPTYPE_TEXT' 'Status'         '16',
     6  ''   ''            'it_submit_rsvtprot' 'SEQ_TIPO'         'Tipo'           '12'.
ENDFORM.
"USER STORY 163032 - MMSILVA - 09.01.2025 - Fim
**<<<------"164255 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form zf_fill_tipo
*&---------------------------------------------------------------------*
*& Valida se campo alterado para ajuste de tela campo competência
*&---------------------------------------------------------------------*
FORM zf_fill_tipo.

  CHECK wl_cabecalho_0150-seq_tipo NE 17 AND
        wl_cabecalho_0150-seq_tipo NE 29 AND
        wl_cabecalho_0150-seq_tipo NE 21 AND
        wl_cabecalho_0150-seq_tipo NE 22.

  LOOP AT SCREEN.
    IF screen-group1 EQ 'ATE'.
      screen-input     = 0.
      screen-invisible = 1.

    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_muda_data_aprop_massa
*&---------------------------------------------------------------------*
*& Altera Data de Apropriação em massa
*&---------------------------------------------------------------------*
FORM zf_muda_data_aprop_massa.

  DATA: tl_fields TYPE TABLE OF sval.

  DATA: el_fields TYPE          sval.

  DATA: vl_resp   TYPE          c.

  REFRESH tl_fields.
  el_fields-tabname   = 'ZGLT073'.
  el_fields-fieldname = 'DT_APROPR'.
  el_fields-field_obl = abap_on.
  el_fields-fieldtext = 'Dt Apropriação Massa'.
  APPEND el_fields TO tl_fields.
  CLEAR el_fields.
* Função de exibição de Popup.
  CLEAR vl_resp.
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = TEXT-t01 "Altera Dt. Aprop. Massa
      start_column    = 20
      start_row       = 10
    IMPORTING
      returncode      = vl_resp
    TABLES
      fields          = tl_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF NOT sy-subrc IS INITIAL .
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ELSE.
    IF vl_resp EQ 'A'.
* Ação cancelada pelo usuário
      MESSAGE s060(01) DISPLAY LIKE 'E'.
      EXIT.

    ELSE.
      READ TABLE tl_fields INTO el_fields INDEX 1.
      CLEAR wl_saida_0150.
      wl_saida_0150-dt_apropr = el_fields-value.
      MODIFY gt_saida_0150 FROM wl_saida_0150 TRANSPORTING dt_apropr WHERE status   EQ '@EB@'
                                                                       AND doc_lcto IS INITIAL.

    ENDIF.

  ENDIF.
*** Método de atualização de dados na Tela
    CALL METHOD obj_alv_0150->refresh_table_display
      EXPORTING
        is_stable = wl_stable.

  CALL METHOD cl_gui_cfw=>flush.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENT'
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

ENDFORM.
**<<<------"164255 - NMS - FIM------>>>
