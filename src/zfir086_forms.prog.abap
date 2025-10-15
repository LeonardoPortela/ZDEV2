*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_FORMS
*&---------------------------------------------------------------------*

**********************************************************************
* selecao de dados
**********************************************************************
FORM f_selecao_dados.

  FREE: t_carta, t_jnad, t_lfa1.

*-------------------------------------------
* selecao carta
*-------------------------------------------
  SELECT zcarta_correcao~*,
         j_1bnfdoc~bukrs
    FROM zcarta_correcao
   INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = zcarta_correcao~docnum
    INTO TABLE @t_carta
   WHERE zcarta_correcao~docnum     IN @s_docnum
     AND zcarta_correcao~dt_authcod IN @s_dtauth
     AND j_1bnfdoc~bukrs            IN @s_bukrs
     AND j_1bnfdoc~branch           IN @s_branch.

  IF p_check = abap_true.
    DELETE t_carta WHERE novo_terminal    IS INITIAL
                     AND novo_agente      IS INITIAL
                     AND novo_loc_coleta  IS INITIAL
                     AND novo_loc_entrega IS INITIAL.
  ENDIF.

  CHECK t_carta[] IS NOT INITIAL.

*-------------------------------------------
* selecao parceiros
*-------------------------------------------
  SELECT *
    FROM j_1bnfnad
    INTO TABLE t_jnad
     FOR ALL ENTRIES IN t_carta
   WHERE docnum = t_carta-docnum.

*-------------------------------------------
* selecao fornecedores
*-------------------------------------------
  t_carta_aux[] = t_carta[].
  DELETE t_carta_aux WHERE novo_terminal IS INITIAL.

  IF t_carta_aux[] IS NOT INITIAL.
    SELECT *
      FROM lfa1
      INTO TABLE t_lfa1
       FOR ALL ENTRIES IN t_carta_aux
     WHERE lifnr = t_carta_aux-novo_terminal.
  ENDIF.

*-------------------------------------------
* selecao fornecedores
*-------------------------------------------
  t_carta_aux[] = t_carta[].
  DELETE t_carta_aux WHERE novo_agente IS INITIAL.

  IF t_carta_aux[] IS NOT INITIAL.
    SELECT *
      FROM lfa1
      APPENDING TABLE t_lfa1
       FOR ALL ENTRIES IN t_carta_aux
     WHERE lifnr = t_carta_aux-novo_agente.
  ENDIF.

*-------------------------------------------
* selecao fornecedores
*-------------------------------------------
  t_carta_aux[] = t_carta[].
  DELETE t_carta_aux WHERE novo_loc_coleta IS INITIAL.

  IF t_carta_aux[] IS NOT INITIAL.
    SELECT *
      FROM lfa1
      APPENDING TABLE t_lfa1
       FOR ALL ENTRIES IN t_carta_aux
     WHERE lifnr = t_carta_aux-novo_loc_coleta.
  ENDIF.

*-------------------------------------------
* selecao fornecedores
*-------------------------------------------
  t_carta_aux[] = t_carta[].
  DELETE t_carta_aux WHERE novo_loc_entrega IS INITIAL.

  IF t_carta_aux[] IS NOT INITIAL.
    SELECT *
      FROM lfa1
      APPENDING TABLE t_lfa1
       FOR ALL ENTRIES IN t_carta_aux
     WHERE lifnr = t_carta_aux-novo_loc_entrega.
  ENDIF.

  SORT t_carta BY docnum dt_authcod.

  SORT t_jnad  BY docnum parvw.

  SORT t_lfa1  BY lifnr.
  DELETE ADJACENT DUPLICATES FROM t_lfa1
                        COMPARING lifnr.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_dados.

  FREE: t_alv.

  LOOP AT t_carta INTO w_carta.

    FREE: w_alv,
          l_parceiro.

    w_alv-bukrs           = w_carta-bukrs.
    w_alv-docnum          = w_carta-docnum.
    w_alv-tp_authcod      = w_carta-tp_authcod.
    w_alv-dt_authcod      = w_carta-dt_authcod.
    w_alv-werks_o         = w_carta-werks_o.
    w_alv-lgort_o         = w_carta-lgort_o.
    w_alv-werks_d         = w_carta-werks_d.
    w_alv-lgort_d         = w_carta-lgort_d.
    w_alv-doc_material    = w_carta-doc_material.
    w_alv-ano_material    = w_carta-ano_material.
    w_alv-msg_correc1     = w_carta-msg_correc1.

    CASE w_carta-tp_transf.
      WHEN '1'.
        w_alv-tp_transf   = 'Depósito'.
      WHEN '2'.
        w_alv-tp_transf   = 'Centro'.
      WHEN OTHERS.
        w_alv-tp_transf   = 'Sem Transf.'.
    ENDCASE.

    IF w_carta-novo_terminal IS NOT INITIAL.
      PERFORM f_ler_parceiro USING w_carta-novo_terminal
                                   'Z1'.
      l_parceiro          = abap_true.
      APPEND w_alv       TO t_alv.
    ENDIF.

    IF w_carta-novo_loc_coleta IS NOT INITIAL.
      PERFORM f_ler_parceiro USING w_carta-novo_loc_coleta
                                   'PC'.
      l_parceiro          = abap_true.
      APPEND w_alv       TO t_alv.
    ENDIF.

    IF w_carta-novo_loc_entrega IS NOT INITIAL.
      PERFORM f_ler_parceiro USING w_carta-novo_loc_entrega
                                   'LR'.
      l_parceiro          = abap_true.
      APPEND w_alv       TO t_alv.
    ENDIF.

    IF w_carta-novo_agente IS NOT INITIAL.
      PERFORM f_ler_parceiro USING w_carta-novo_agente
                                   'SP'.
      l_parceiro          = abap_true.
      APPEND w_alv       TO t_alv.
    ENDIF.

    IF l_parceiro = abap_false.
      APPEND w_alv       TO t_alv.
    ENDIF.

  ENDLOOP.

  SORT t_alv BY docnum dt_authcod.

ENDFORM.

**********************************************************************
* ler parceiro
**********************************************************************
FORM f_ler_parceiro   USING p_lifnr
                            p_parvw.

  CLEAR: w_jnad, w_lfa1.

  READ TABLE t_jnad INTO w_jnad WITH KEY docnum = w_carta-docnum
                                         parvw  = p_parvw
                                BINARY SEARCH.
  READ TABLE t_lfa1 INTO w_lfa1 WITH KEY lifnr  = p_lifnr
                                BINARY SEARCH.

  CASE p_parvw.
    WHEN 'Z1'.
      w_alv-tip_troca = 'Terminal'.
    WHEN 'PC'.
      w_alv-tip_troca = 'Loc.Coleta'.
    WHEN 'LR'.
      w_alv-tip_troca = 'Loc.Entrega'.
    WHEN 'SP'.
      w_alv-tip_troca = 'Ag.Frete'.
  ENDCASE.

  w_alv-parc_orig     = w_jnad-parid.
  w_alv-name1_orig    = w_jnad-name1.
  w_alv-parc_corr     = p_lifnr.
  w_alv-name1_corr    = w_lfa1-name1.

ENDFORM.

**********************************************************************
* alv saida
**********************************************************************
FORM f_alv_saida.

  CALL SCREEN 100.

ENDFORM.

**********************************************************************
* gerar arquivos
**********************************************************************
FORM f_gerar_arquivos_pdf_xml USING p_ucomm.

  DATA: t_docs TYPE TABLE OF zsds0041,
        w_docs TYPE zsds0041,
        l_type TYPE z_type_file.

  l_type = p_ucomm.

  FREE: t_docs.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_selectedrow.

  IF t_selectedrow[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'E'.
    EXIT.
  ENDIF.

  LOOP AT t_selectedrow INTO w_selectedrow.
    CLEAR w_alv.
    READ TABLE t_alv INTO w_alv INDEX w_selectedrow-index.

    w_docs-docnum  = w_alv-docnum.
    APPEND w_docs TO t_docs.
  ENDLOOP.

*------------------------------
*-Executa processo
*------------------------------
  CALL FUNCTION 'Z_GRC_DOWNLOAD_XML_PDF'
    EXPORTING
      i_type_file     = l_type
    TABLES
      t_documentos    = t_docs
    EXCEPTIONS
      error_open_file = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Houve erro ao gerar o Arquivo!' DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE s024(sd) WITH 'Processo finalizado.'.
  ENDIF.

ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

  PERFORM f_funcoes.
  PERFORM f_fieldcatalog.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
* w_layout-info_fname   = 'LINE_COLOR'.
  w_layout-zebra        = abap_false.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_false.
  w_layout-sel_mode     = 'A'.
* w_layout-stylefname   = 'CELLSTYLES'.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

*  IF g_custom_container IS INITIAL.
*    CREATE OBJECT g_custom_container
*      EXPORTING
*        container_name              = 'CONTAINER'
*      EXCEPTIONS
*        cntl_error                  = 1
*        cntl_system_error           = 2
*        create_error                = 3
*        lifetime_error              = 4
*        lifetime_dynpro_dynpro_link = 5
*        OTHERS                      = 6.

  IF g_grid IS INITIAL. " AND  g_custom_container IS NOT INITIAL.
*      CREATE OBJECT g_grid
*        EXPORTING
*          i_parent          = g_custom_container
*        EXCEPTIONS
*          error_cntl_create = 1
*          error_cntl_init   = 2
*          error_cntl_link   = 3
*          error_dp_create   = 4
*          OTHERS            = 5.

    CREATE OBJECT obj_dyndoc_id
      EXPORTING
        no_margins = 'X'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'HEADER'.

    PERFORM f_alv_header .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = g_custom_container
      EXCEPTIONS
        html_display_error = 1.

    "Grafico 1
    CALL METHOD cl_gui_cfw=>flush.

    CREATE OBJECT: g_custom_container
       EXPORTING
         container_name = 'CC_IMG',
         picture
       EXPORTING
         parent = g_custom_container.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = cl_container_95.
*    ENDIF.

    SET HANDLER lcl_event_handler=>on_hotspot_click  FOR g_grid.
    SET HANDLER lcl_event_handler=>on_data_changed  FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_alv[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
*   CALL METHOD g_grid->set_ready_for_input
*     EXPORTING
*       i_ready_for_input = 1.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

**********************************************************************
*  cabecalho
**********************************************************************
FORM f_alv_header .

  DATA: wl_data1(10),
        wl_data2(10),
        wl_hora(8),
        wl_linha(60),
        wl_text TYPE sdydo_text_element.

  DATA: wa_t001       TYPE t001,
        wa_j_1bbranch TYPE j_1bbranch.

  wl_linha = 'Consulta Cartas de Correções Emitidas'.

  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.

  IF s_bukrs[] IS NOT INITIAL.
    READ TABLE s_bukrs INDEX 1.
    CONCATENATE  'Empresa.........:' s_bukrs-low
            INTO wl_linha SEPARATED BY space.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.
  ENDIF.

  IF s_branch[]  IS NOT INITIAL.
    READ TABLE s_branch INDEX 1.
    CONCATENATE  'Filial................:' s_branch-low
           INTO wl_linha SEPARATED BY space.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.
  ENDIF.

  IF s_docnum[] IS NOT INITIAL.
    READ TABLE s_docnum INDEX 1.

    IF s_docnum-high IS NOT INITIAL.
      CONCATENATE  'Nr.Documento.:' s_docnum-low 'a' s_docnum-high
             INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE  'Nr.Documento.:' s_docnum-low
             INTO wl_linha SEPARATED BY space.
    ENDIF.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.
  ENDIF.

  IF s_dtauth[] IS NOT INITIAL.
    READ TABLE s_dtauth INDEX 1.

    wl_data1 = s_dtauth-low+6(2)  && '.' && s_dtauth-low+4(2)  && '.' && s_dtauth-low(4).
    wl_data2 = s_dtauth-high+6(2) && '.' && s_dtauth-high+4(2) && '.' && s_dtauth-high(4).

    IF s_dtauth-high IS NOT INITIAL.
      CONCATENATE  'Período...........:' wl_data1 'a' wl_data2
             INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE  'Período...........:' wl_data1
             INTO wl_linha SEPARATED BY space.
    ENDIF.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.
  ENDIF.
*
  IF p_check IS NOT INITIAL.
    CONCATENATE  '[X] Somente alteração Parceiros' ''
            INTO wl_linha SEPARATED BY space.
  ELSE.
    CONCATENATE  '[ ] Somente alteração Parceiros' ''
            INTO wl_linha SEPARATED BY space.
  ENDIF.

  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_fontsize = cl_dd_area=>list_normal.

ENDFORM.                    " ZF_ALV_HEADER

**********************************************************************
*  imagen
**********************************************************************
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM

**********************************************************************
*  barra tarefas
**********************************************************************
FORM f_funcoes.

  FREE: t_function.

  w_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_check.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND w_function TO t_function.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
    01  ''      ''       'T_ALV' 'DOCNUM'        'Nr.Documento'            '12'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' 'X',
    02  ''      ''       'T_ALV' 'TP_AUTHCOD'    'Status Doc'              '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    03  ''      ''       'T_ALV' 'TIP_TROCA'     'Tipo Troca'              '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    04  'LFA1'  'LIFNR'  'T_ALV' 'PARC_ORIG'     'Parceiro Orig'           '14'  ' '    ' ' ' ' 'X'  'X' ' ' ' ' ' ' ' ' ' ',
    05  ''      ''       'T_ALV' 'NAME1_ORIG'    'Nome Orig'               '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    06  'LFA1'  'LIFNR'  'T_ALV' 'PARC_CORR'     'Parceiro Correção'       '18'  ' '    ' ' ' ' 'X'  'X' ' ' ' ' ' ' ' ' ' ',
    07  ''      ''       'T_ALV' 'NAME1_CORR'    'Nome Correção'           '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    08  ''      ''       'T_ALV' 'DT_AUTHCOD'    'Data Autorização'        '16'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    09  ''      ''       'T_ALV' 'TP_TRANSF'     'Transferência'           '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    10  ''      ''       'T_ALV' 'WERKS_O'       'Centro Origem'           '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    11  ''      ''       'T_ALV' 'LGORT_O'       'Dep.Origem'              '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    12  ''      ''       'T_ALV' 'WERKS_D'       'Centro Destino'          '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    13  ''      ''       'T_ALV' 'LGORT_D'       'Dep.Destino'             '13'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    14  ''      ''       'T_ALV' 'DOC_MATERIAL'  'Doc.Material'            '13'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
    15  ''      ''       'T_ALV' 'ANO_MATERIAL'  'Ano'                     '05'  ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
    16  ''      ''       'T_ALV' 'MSG_CORREC1'   'Mensagem1'               '200' ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).                                    "16

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
  w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-scrtext_s   = p_scrtext_l.
  w_fieldcat-scrtext_m   = p_scrtext_l.
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

**********************************************************************
**********************************************************************
*&---------------------------------------------------------------------*
*&      Form  FM_DOWNLOAD_CARTA_CORREÇÃO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_download_carta_correcao .
  DATA: t_docs          TYPE TABLE OF zsds0041,
        w_docs          TYPE zsds0041,
        e_link_pdf      TYPE string,
        e_pdf           TYPE xstring,
        tamanho         TYPE i,
        lt_pdf          TYPE TABLE OF char80,
        selected_folder	TYPE string,
        l_type          TYPE z_type_file.

*  l_type = p_ucomm.
  DATA: zcl_cce TYPE REF TO zcl_cce.

  FREE: t_docs.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_selectedrow.

  IF t_selectedrow[] IS INITIAL.
    MESSAGE 'Selecione a linha!' TYPE 'E'.
    EXIT.
  ENDIF.

  cl_gui_frontend_services=>directory_browse(
        EXPORTING
          window_title         = 'Pasta para salvar arquivos XML'
        CHANGING
          selected_folder      = selected_folder
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4  ).


    CHECK selected_folder IS NOT INITIAL.

*  READ TABLE t_selectedrow INTO w_selectedrow INDEX 1.
  CLEAR: w_selectedrow.
  LOOP AT t_selectedrow INTO w_selectedrow.

    READ TABLE t_alv INTO w_alv INDEX w_selectedrow-index. "

    SELECT SINGLE *
           FROM zcarta_correcao INTO @DATA(wl_zcarta_correcao)
          WHERE docnum EQ @w_alv-docnum
            AND id_cc  EQ ( SELECT MAX( id_cc ) FROM zcarta_correcao WHERE docnum EQ @w_alv-docnum ).

    TRY.
     e_link_pdf = wl_zcarta_correcao-ds_url.
        zcl_arquivo=>get_file_uri_get_(
          EXPORTING
            i_uri          = e_link_pdf
          IMPORTING
            e_texto_2      = e_pdf
            e_code         = DATA(e_code)
            e_reason       = DATA(e_reason)
        ).

        IF e_code NE '200'.
*          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

        ELSE.
          DATA(filename) =  selected_folder && '\' && wl_zcarta_correcao-docnum && wl_zcarta_correcao-id_cc && '-' && sy-datum && '.PDF'.
          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer        = e_pdf
            IMPORTING
              output_length = tamanho
            TABLES
              binary_tab    = lt_pdf.



          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer        = e_pdf
            IMPORTING
              output_length = tamanho
            TABLES
              binary_tab    = lt_pdf.

          cl_gui_frontend_services=>gui_download(
                EXPORTING
                  bin_filesize              = tamanho
                  filename                  = filename
                   filetype                  = 'BIN'
                CHANGING
                  data_tab                  = lt_pdf
                EXCEPTIONS
                  file_write_error          = 1
                  no_batch                  = 2
                  gui_refuse_filetransfer   = 3
                  invalid_type              = 4
                  no_authority              = 5
                  unknown_error             = 6
                  header_not_allowed        = 7
                  separator_not_allowed     = 8
                  filesize_not_allowed      = 9
                  header_too_long           = 10
                  dp_error_create           = 11
                  dp_error_send             = 12
                  dp_error_write            = 13
                  unknown_dp_error          = 14
                  access_denied             = 15
                  dp_out_of_memory          = 16
                  disk_full                 = 17
                  dp_timeout                = 18
                  file_not_found            = 19
                  dataprovider_exception    = 20
                  control_flush_error       = 21
                  not_supported_by_gui      = 22
                  error_no_gui              = 23
                  OTHERS                    = 24
              ).

          IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.
      CATCH zcx_arquivo.

    ENDTRY.
    CLEAR: w_selectedrow, wl_zcarta_correcao, e_code, e_reason, e_pdf, tamanho, filename, lt_pdf, e_pdf, e_link_pdf, w_alv.
    FREE: lt_pdf.
  ENDLOOP.



*  FREE zcl_cce.
*  CREATE OBJECT zcl_cce
*    EXPORTING
*      i_docnum = wl_zcarta_correcao-docnum
*      i_id_cc    = wl_zcarta_correcao-id_cc.
*
*  zcl_cce->imprimir( ).
*  IF sy-subrc <> 0.
*    MESSAGE s024(sd) WITH 'Houve erro ao gerar o Arquivo!' DISPLAY LIKE 'E'.
*  ELSE.
*    MESSAGE s024(sd) WITH 'Processo finalizado.'.
*  ENDIF.
*
*  clear: wl_zcarta_correcao.
ENDFORM.
