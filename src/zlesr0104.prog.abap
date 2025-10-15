*&---------------------------------------------------------------------*
*& Report  ZLESR0104
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0104 MESSAGE-ID ztipfrete.

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click  FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id e_row_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: it_consultas    TYPE TABLE OF zlest0139 WITH HEADER LINE,
      dg_splitter     TYPE REF TO cl_gui_splitter_container,
      dg_parent_html  TYPE REF TO cl_gui_container,
      dg_splitter_2   TYPE REF TO cl_gui_splitter_container,
      dg_parent_html1 TYPE REF TO cl_gui_container,
      dg_parent_html2 TYPE REF TO cl_gui_container,
      picture         TYPE REF TO cl_gui_picture,
      dg_parent_alv   TYPE REF TO cl_gui_container,
      g_alv           TYPE REF TO cl_gui_alv_grid,
      gs_layout       TYPE lvc_s_layo,
      gs_variant      TYPE disvariant,
      it_fieldcatalog TYPE lvc_t_fcat,
      dg_dyndoc_id    TYPE REF TO cl_dd_document,
      table_element   TYPE REF TO cl_dd_table_element,
      column          TYPE REF TO cl_dd_area,
      table_element2  TYPE REF TO cl_dd_table_element,
      column_1        TYPE REF TO cl_dd_area,
      column_2        TYPE REF TO cl_dd_area,
      dg_html_cntrl   TYPE REF TO cl_gui_html_viewer.

DATA: event_handler      TYPE REF TO lcl_event_handler.

DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

DATA: gs_scroll_col TYPE lvc_s_col,
      gs_scroll_row TYPE lvc_s_roid.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_double_click.
    PERFORM handle_double_click USING e_row.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE TEXT-001.
  PARAMETERS: pbukrs  TYPE bukrs OBLIGATORY,
              pbranch TYPE j_1bbranc_ OBLIGATORY,
              pltadm  TYPE zpfe_nr_lote_adm.
SELECTION-SCREEN END OF BLOCK bl01.

SELECTION-SCREEN BEGIN OF BLOCK bl02 WITH FRAME TITLE TEXT-002.
  PARAMETERS: pprocon1 RADIOBUTTON GROUP rad1 DEFAULT 'X',
              pprocon2 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK bl02.

START-OF-SELECTION.

  DATA: lc_filtro  TYPE zde_zlest0139_filtro,
        lc_retorno TYPE zde_zlest0139_t,
        e_lotes	   TYPE zpfe_numero_lote_t.

  CASE pprocon1.
    WHEN abap_true.

      DATA: arquivos TYPE REF TO zcl_tip_frete_aq_cobranca.

      CREATE OBJECT arquivos.
      CLEAR: lc_filtro.
      lc_filtro-bukrs         = VALUE #( ( sign = 'I' option = 'EQ' low = pbukrs   high = pbukrs   ) ).
      lc_filtro-branch[]      = VALUE #( ( sign = 'I' option = 'EQ' low = pbranch  high = pbranch  ) ).
      IF pltadm IS NOT INITIAL.
        lc_filtro-nr_lote_adm[] = VALUE #( ( sign = 'I' option = 'EQ' low = pltadm   high = pltadm   ) ).
      ENDIF.

      DATA(e_pesquisou) = arquivos->zif_pesquisa~pesquisar( EXPORTING i_filtros   = lc_filtro IMPORTING e_registros = lc_retorno ).

      "CHECK E_PESQUISOU EQ ABAP_TRUE.

      MOVE lc_retorno[] TO it_consultas[].

      CALL SCREEN 0100.

      CLEAR: arquivos.

    WHEN abap_false.
      DATA: obj_consulta TYPE REF TO zcl_webservice_tipcard,
            e_msg	       TYPE char255.

      CREATE OBJECT obj_consulta.

      obj_consulta->consultar_arquivo_cobranca(
        EXPORTING
          i_bukrs                   = pbukrs
          i_branch                  = pbranch
          i_fatura                  = pltadm
        IMPORTING
          e_msg                     = e_msg
          r_linkarquivo             = DATA(r_linkarquivo)
          r_linkarquivo_pedagio     = DATA(r_linkarquivo_ped)
          r_content_arquivo         = DATA(r_content_arquivo)     "*-US 130492-08.04.2024-JT
          r_content_arquivo_pedagio = DATA(r_content_arquivo_ped) "*-US 130492-08.04.2024-JT
        EXCEPTIONS
          zwebservice   = 1
          OTHERS        = 2 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
      ELSE.
        TRY .
            zcl_webservice_tipcard=>processar_arquivo_cobranca(
              EXPORTING
              i_filename                 = r_linkarquivo
              i_filename_pedagio         = r_linkarquivo_ped
              i_content_filename         = r_content_arquivo      "*-US 130492-08.04.2024-JT
              i_content_filename_pedagio = r_content_arquivo_ped  "*-US 130492-08.04.2024-JT
              i_bukrs                    = pbukrs
              i_branch                   = pbranch
              IMPORTING
              e_lotes = e_lotes ).

            LOOP AT e_lotes INTO DATA(wa_lote).
              CALL FUNCTION 'Z_PFE_GERA_FINAN'
                EXPORTING
                  p_nm_lote       = wa_lote
                EXCEPTIONS
                  sem_lote        = 1
                  gerando_finam   = 2
                  concluido_finan = 3
                  zdiasvencimento = 4
                  data_vencimento = 5
                  gerado_finam    = 6
                  OTHERS          = 7.

              IF sy-subrc IS NOT INITIAL.
                MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
                CONTINUE.
              ENDIF.

              SELECT SINGLE * INTO @DATA(wa_zpfe_lote) FROM zpfe_lote WHERE nm_lote EQ @wa_lote AND tp_plano_administradora EQ @zcl_ciot=>st_tp_plano_pre_pago.

              IF sy-subrc IS NOT INITIAL.
                CONTINUE.
              ENDIF.

              SELECT * INTO TABLE @DATA(it_zpfe_lote_item)
                FROM zpfe_lote_item
               WHERE nm_lote EQ @wa_zpfe_lote-nm_lote.

              LOOP AT it_zpfe_lote_item INTO DATA(wa_zpfe_lote_item).

                wa_zpfe_lote_item-vl_conferido    = wa_zpfe_lote_item-vl_transacao.
                wa_zpfe_lote_item-vl_pago_lote    = wa_zpfe_lote_item-vl_transacao.
                wa_zpfe_lote_item-peso_chegada    = wa_zpfe_lote_item-peso_importado.
                wa_zpfe_lote_item-ck_conferido    = 'X'.
                wa_zpfe_lote_item-ds_usuario_conf = sy-uname.
                wa_zpfe_lote_item-dt_baixa        = wa_zpfe_lote-dt_posicao.
                MODIFY zpfe_lote_item FROM wa_zpfe_lote_item.

                CALL FUNCTION 'Z_PFE_GERA_CONTAB'
                  EXPORTING
                    p_nm_lote        = wa_zpfe_lote_item-nm_lote
                    p_nm_lote_item   = wa_zpfe_lote_item-nm_lote_item
                  TABLES
                    it_lote_item     = it_zpfe_lote_item
                  EXCEPTIONS
                    sem_lote         = 1
                    sem_lote_item    = 2
                    gerando_contb    = 3
                    concluido_contb  = 4
                    gerado_contb     = 5
                    conta_deb_cred   = 6
                    sem_proprietario = 7
                    nao_conferido    = 8
                    error            = 9
                    sem_data_baixa   = 10
                    OTHERS           = 11.

                IF sy-subrc IS NOT INITIAL.
                  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
                ENDIF.

              ENDLOOP.

            ENDLOOP.

          CATCH zcx_cadastro     INTO DATA(cx_cadastro).
            cx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          CATCH zcx_erro_arquivo INTO DATA(cx_erro_arquivo).
            cx_erro_arquivo->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        ENDTRY.
      ENDIF.
      CLEAR: obj_consulta.
  ENDCASE.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: url(255)                TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF dg_splitter IS INITIAL.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_html
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html1.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 40.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_html2.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_html2.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CALL METHOD dg_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 16.

    CREATE OBJECT g_alv
      EXPORTING
        i_parent = dg_parent_alv.

    PERFORM fill_it_fieldcatalog.

    PERFORM fill_gs_variant.

    gs_layout-sel_mode   = 'A'.

    CALL METHOD g_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_consultas[].

    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_double_click  FOR g_alv.
    SET HANDLER event_handler->handle_hotspot_click FOR g_alv.

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
        sap_align = 'CENTER'
        sap_style = cl_dd_document=>heading.

    p_text = TEXT-008.

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

    sdydo_text_element = 'Empresa'.
    APPEND sdydo_text_element TO p_text_table.

    sdydo_text_element = 'Local de Negócio'.
    APPEND sdydo_text_element TO p_text_table.

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

*PBUKRS	Sociedad
    SELECT SINGLE butxt INTO @DATA(lc_butxt)
      FROM t001
     WHERE bukrs EQ @pbukrs.

    CONCATENATE pbukrs '-' lc_butxt INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.

*PBUKRS	Sociedad
    SELECT SINGLE name INTO @DATA(lc_name)
      FROM j_1bbranch
     WHERE branch EQ @pbranch.

    CONCATENATE pbranch '-' lc_name INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.

    CALL METHOD column_2->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html1.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_html1
      EXCEPTIONS
        html_display_error = 1.
  ENDIF.

  CALL METHOD g_alv->refresh_table_display.

  CALL METHOD g_alv->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col
      es_row_no   = gs_scroll_row.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog .

  DATA: lc_col_pos  TYPE lvc_colpos.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZLEST0139'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.

  LOOP AT it_fieldcatalog ASSIGNING FIELD-SYMBOL(<fs_field>).
    IF <fs_field>-fieldname EQ 'NR_LOTE_ADM' OR
       <fs_field>-fieldname EQ 'DS_FILENAME' OR
       <fs_field>-fieldname EQ 'DS_FILENAME_PED'.
      <fs_field>-hotspot = abap_true.
    ENDIF.
    IF <fs_field>-fieldname EQ 'DS_FILENAME' OR
       <fs_field>-fieldname EQ 'DS_FILENAME_PED'.
      <fs_field>-outputlen = 100.
    ENDIF.

    IF <fs_field>-fieldname EQ 'DS_FILENAME'.
      <fs_field>-scrtext_l = 'Arquivo Cobrança'.
      <fs_field>-scrtext_m = 'Arquivo Cobrança'.
      <fs_field>-scrtext_s = 'Arq.Cob.'.
    ENDIF.

    IF <fs_field>-fieldname EQ 'DS_FILENAME_PED'.
      <fs_field>-scrtext_l = 'Arquivo Pedágio'.
      <fs_field>-scrtext_m = 'Arquivo Pedágio'.
      <fs_field>-scrtext_s = 'Arq.Ped.'.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0190   text
*      <--P_URL  text
*----------------------------------------------------------------------*
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.
  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

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

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM handle_double_click  USING p_row TYPE lvc_s_row.

  DATA: lc_row TYPE lvc_t_row.

  IF p_row-rowtype IS INITIAL.

    APPEND p_row TO lc_row.

    CALL METHOD g_alv->set_selected_rows
      EXPORTING
        it_index_rows = lc_row.

    READ TABLE it_consultas INDEX p_row-index INTO DATA(wa_consultas).

    TRY .
        arquivos->zif_cadastro~set_registro( i_id_registro = wa_consultas-cd_importacao ).
        arquivos->visualizar_texto_arquivo( ).
      CATCH zcx_cadastro INTO DATA(cx_cadastro).
        cx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_ID  text
*      -->P_E_ROW_ID  text
*----------------------------------------------------------------------*
FORM handle_hotspot_click
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_consultas INDEX row_id INTO DATA(wa_consultas).

  CASE fieldname.
    WHEN 'NR_LOTE_ADM'.
      TRY.
          arquivos->zif_cadastro~set_registro( i_id_registro = wa_consultas-cd_importacao ).
          arquivos->visualizar_log_arquivo( ).
        CATCH zcx_cadastro INTO DATA(cx_cadastro).
          cx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH cx_salv_msg INTO DATA(cx_salv_msg).
      ENDTRY.
    WHEN 'DS_FILENAME'.
      TRY.
          arquivos->zif_cadastro~set_registro( i_id_registro = wa_consultas-cd_importacao ).
          arquivos->visualizar_texto_arquivo( i_arquivo = 'C' ).
        CATCH zcx_cadastro INTO cx_cadastro.
          cx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.
    WHEN 'DS_FILENAME_PED'.
      TRY.
          arquivos->zif_cadastro~set_registro( i_id_registro = wa_consultas-cd_importacao ).
          arquivos->visualizar_texto_arquivo( i_arquivo = 'P' ).
        CATCH zcx_cadastro INTO cx_cadastro.
          cx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.
  ENDCASE.

ENDFORM.
