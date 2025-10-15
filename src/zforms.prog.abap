*&---------------------------------------------------------------------*
*&  Include           ZFORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.


  FREE it_fcat.

  IF rb_cad IS INITIAL.
    PERFORM alv_preenche_cat USING:

*   'IWERK           ' 'Centro                  '  '60 ' ''  '' ''  ' ' ' '  '' '' '' ' ',

     'STATUS_PROC      ' 'Status integração       '  '60 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'CLIENTE          ' 'Cliente                 '  '60 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'AUFNR            ' 'Ordem                   '  '60 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'NUMEROAMOSTRA    ' 'Amostra                 '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'SITUACAO         ' 'Situação                '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'EARTX            ' 'Categoria               '  '20 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'FROTA            ' 'Equipamento             '  '20 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'EQKTX            ' 'Desc eqpto              '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
*   'MODELO           ' 'Modelo                  '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'HR_KM            ' 'Hr/coleta               '  '20 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'HORASOLEO        ' 'Hr/óleo                 '  '20 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'FABRICANTEOLEO   ' 'Fab óleo                '  '20 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'VISCOSIDADE      ' 'Tipo                    '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'OLEOTROCADO      ' 'Trocado?                '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'VOLUMEADICIONADO ' 'Volume                  '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'NOME             ' 'Compartimento           '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'DATACOLETA       ' 'Data coleta             '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'DATAFINALIZACAO  ' 'Data fim                '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'L_AVALIACAO        ' 'Avaliação             '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'STATUSAMOSTRA    ' 'Status amostra          '  '15 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'L_LAUDO          ' 'Laudo téc               '  '15 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
     'AVALIACAO        ' 'Detalhe avaliação       '  '255' ''  '' ''  ' ' ' '  '' '' '' ' '.

  ELSE.
    PERFORM alv_preenche_cat USING:
  'CODIGOEXTERNO                     ' 'Nº equipamento                            '  '60 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'NUMEROAMOSTRA                     ' 'Nº Amostra                                '  '60 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'IDOBRATIPOCOLETAAVULSA            ' 'Empresa                                   '  '60 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'IDCOMPARTIMENTO                   ' 'Compartimento óleo                        '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'IDVISCOSIDADE                     ' 'ID Viscosidade                            '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'OLEOTROCADO                       ' 'Óleo trocado? (Sim) (Não)                 '  '20 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'DATACOLETA                        ' 'Data de criação do registro               '  '20 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'IDFABRICANTEOLEO                  ' 'ID Fabricantes do óleo                    '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'DETALHESOLEO                      ' 'Texto breve de material                   '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'HORASEQUIPAMENTO                  ' 'Posição do contador                       '  '20 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'HORASOLEO                         ' 'Diferença de posições do numerador        '  '20 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'VOLUMEADICIONADO                  ' 'Diferença de posições do numerador        '  '20 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'COMENTARIO                        ' 'Texto para o doc.medição                  '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'USUARIO                           ' 'Nome do responsável que adicionou o objeto'  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'ERDAT                             ' 'Data de criação do registro               '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'AUFNR                             ' 'Nº ordem                                  '  '30 ' ''  '' ''  ' ' ' '  '' '' '' ' '.
  ENDIF.

  DATA: q_linha TYPE char8.
  IF obj_dados->lw_ztpm_amost IS NOT INITIAL.
    DESCRIBE TABLE obj_dados->lw_ztpm_amost LINES q_linha.
  ENDIF.

  IF t_zpmt0053 IS NOT INITIAL.
    DESCRIBE TABLE t_zpmt0053 LINES q_linha.
  ENDIF.

  IF ( obj_custom_0110 IS INITIAL ).
    CREATE OBJECT obj_custom_0110
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

*    CREATE OBJECT OBJ_ALV_0110
*      EXPORTING
*        I_PARENT          = OBJ_CUSTOM_0110
*      EXCEPTIONS
*        ERROR_CNTL_CREATE = 1
*        ERROR_CNTL_INIT   = 2
*        ERROR_CNTL_LINK   = 3
*        ERROR_DP_CREATE   = 4
*        OTHERS            = 5.

    PERFORM cabecario USING obj_custom_0110.

    CREATE OBJECT obj_alv_0110
      EXPORTING
        i_parent = dg_parent_alv.
  ENDIF.


  wa_layout = VALUE #( ctab_fname    = 'CELL_COLOR'
                       excp_conds    = 'X'
                       zebra         = 'X'
                       sel_mode      = 'A'
                       cwidth_opt    = 'X'     "  Otimizar colunas na tela
*                            GRID_TITLE = 'Total de informações:  ' && Q_LINHA
                       totals_bef    = ' ' ).


  SET HANDLER:
 lcl_event_handler=>on_double_click  FOR obj_alv_0110.

  IF rb_cad IS INITIAL.
    CALL METHOD obj_alv_0110->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        it_toolbar_excluding          = gt_exc_button
        i_save                        = 'A'
      CHANGING
        it_fieldcatalog               = it_fcat
        it_outtab                     = obj_dados->lw_ztpm_amost
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.

    CALL METHOD obj_alv_0110->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        it_toolbar_excluding          = gt_exc_button
        i_save                        = 'A'
      CHANGING
        it_fieldcatalog               = it_fcat
        it_outtab                     = t_zpmt0053
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ENDIF.

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
      "SAP_ALIGN = 'CENTER'
      sap_style = cl_dd_document=>heading.

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

  PERFORM cabecario_alv USING q_linha.

  CALL METHOD column_1->add_text
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

*      PERFORM AJUSTA_TOTAIS.


  CALL METHOD obj_alv_0110->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD obj_alv_0110->refresh_table_display
    EXPORTING
      is_stable = wa_stable.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'CONEXAO'.
      PERFORM fm_exec_api.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'SET_0100'.
  SET TITLEBAR 'TIT_0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ENDMODULE  text
*----------------------------------------------------------------------*
FORM alv_preenche_cat    USING: p_campo         TYPE c
                                p_desc          TYPE c
                                p_tam           TYPE c
                                p_hot           TYPE c
                                p_zero          TYPE c
                                p_sum           TYPE c
                                p_edit          TYPE c
                                p_check         TYPE c
                                p_ref_tabname   LIKE dd02d-tabname
                                p_ref_fieldname LIKE dd03d-fieldname
                                p_tabname       LIKE dd02d-tabname
                                p_no_out        TYPE c.

  DATA: wl_fcat TYPE lvc_s_fcat.
  CLEAR: wa_layout, wl_fcat.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-edit      = p_edit.
  wl_fcat-checkbox  = p_check.
  wl_fcat-ref_table = p_ref_tabname.
  wl_fcat-ref_field = p_ref_fieldname.
  wl_fcat-tabname   = p_ref_tabname.
  wl_fcat-no_out    = p_no_out.
  APPEND wl_fcat TO it_fcat.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'PF-0200'.
  SET TITLEBAR 'TIT-0200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'GRAF'.
*      PERFORM GRAF.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PB0_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pb0_0200 OUTPUT.

  IF o_docking IS NOT BOUND.

    CREATE OBJECT o_docking
      EXPORTING
        repid                       = sy-repid
        dynnr                       = sy-dynnr
        extension                   = 2000 " Tela Full " Tamanho da Tela
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

*&---------------------------------------------------------------------*
    CREATE OBJECT o_splitter
      EXPORTING
        link_dynnr        = sy-dynnr    " Screen Number
        link_repid        = sy-repid   " Report Name
*       shellstyle        =     " Window Style
*       left              =     " Left-aligned
        top               = 20    " top
*       width             =     " NPlWidth
*       height            =     " Hght
*       metric            = CNTL_METRIC_DYNPRO    " Metric
*       align             = 15    " Alignment
        parent            = o_docking    " Parent Container
        rows              = 3   " Number of Rows to be displayed
        columns           = 1  " Number of Columns to be Displayed
*       no_autodef_progid_dynnr =     " Don't Autodefined Progid and Dynnr?
*       name              =     " Name
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
  ENDIF.


*Definição das medidas dos containers
  PERFORM f_define_container_extension
*         Row column Height Width
    USING '1' '1'    '20'   '10'.

*&---------------------------------------------------------------------*
* Container Header ( 1,1 )
*&---------------------------------------------------------------------*
  PERFORM f_create_container
*         Row Column  Container
    USING '1' '1'     o_container_3.

  PERFORM f_define_container_cabecario.

*&---------------------------------------------------------------------*
* Container Header ( 2,1 )
*&---------------------------------------------------------------------*
  PERFORM f_create_container
*         Row Column  Container
    USING '2' '1'     o_container_1.

  PERFORM f_define_container_header.

*&---------------------------------------------------------------------*
* Container Item ( 3,1 )
*&---------------------------------------------------------------------*
  PERFORM f_create_container
*         Row Column  Container
    USING '3' '1'     o_container_2.
  PERFORM f_define_container_item.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT_SINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ENDMODULE  text
*----------------------------------------------------------------------*
FORM alv_preenche_cat_sint  USING: p_campo         TYPE c
                                p_desc          TYPE c
                                p_tam           TYPE c
                                p_hot           TYPE c
                                p_zero          TYPE c
                                p_sum           TYPE c
                                p_edit          TYPE c
                                p_check         TYPE c
                                p_ref_tabname   LIKE dd02d-tabname
                                p_ref_fieldname LIKE dd03d-fieldname
                                p_tabname       LIKE dd02d-tabname
                                p_no_out        TYPE c.

  DATA: ws_fcat TYPE lvc_s_fcat.
  CLEAR: wa_layout, ws_fcat.
  ws_fcat-fieldname = p_campo.
  ws_fcat-scrtext_l = p_desc.
  ws_fcat-scrtext_m = p_desc.
  ws_fcat-scrtext_s = p_desc.
  ws_fcat-hotspot   = p_hot.
  ws_fcat-no_zero   = p_zero.
  ws_fcat-outputlen = p_tam.
  ws_fcat-edit      = p_edit.
  ws_fcat-checkbox  = p_check.
  ws_fcat-ref_table = p_ref_tabname.
  ws_fcat-ref_field = p_ref_fieldname.
  ws_fcat-tabname   = p_ref_tabname.
  ws_fcat-no_out    = p_no_out.
  APPEND ws_fcat TO it_fcat_sint.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GRAF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM graf .

* DATA: 2D_COL VALUE '2'.

  DATA w_dados TYPE ztpm_amost_graf.

  DATA(title) = 'Status das amostras'.

  DATA: BEGIN OF tyear OCCURS 1,
          C(20) TYPE c,
        END OF tyear.

  DATA: BEGIN OF opts OCCURS 1,
          C(80) TYPE c,
        END OF opts.

  DATA: BEGIN OF data  OCCURS 1,
          text(25),
          total    TYPE p,
        END OF data.


  CLEAR data.

  UNASSIGN <fs_line>.
  CREATE DATA t_new_line LIKE LINE OF obj_dados->lt_ztpm_amost_sint.
  ASSIGN t_new_line->* TO <fs_line>.

  LOOP AT obj_dados->lt_ztpm_amost_graf INTO DATA(_lt_ztpm_amost_sint).
    MOVE-CORRESPONDING _lt_ztpm_amost_sint TO <fs_line>.

    LOOP AT  it_fcat_sint INTO DATA(_fcat_sint).
      ASSIGN COMPONENT _fcat_sint-fieldname  OF STRUCTURE <fs_line> TO <fs_campo>.
      IF _fcat_sint-fieldname NE 'TOTAL'.
        data-text  = _fcat_sint-fieldname.
        data-total = CONV #( <fs_campo> ).
        APPEND data.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  WRITE 'P2TYPE = PI' TO opts-c. APPEND opts.
  MOVE  'Amostra'     TO tyear-c. APPEND tyear.

  CALL FUNCTION 'GRAPH_MATRIX_2D'
    EXPORTING
      titl       = title
      valt       = 'DM'
      ncol       = '1'
      mail_allow = 'X'
      set_focus  = 'X'
    TABLES
      data       = data
      opts       = opts
      tcol       = tyear.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  PB0_0200_1  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pb0_0200_1 OUTPUT.





ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT_EQUI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1089   text
*      -->P_1090   text
*      -->P_1091   text
*      -->P_1092   text
*      -->P_1093   text
*      -->P_1094   text
*      -->P_1095   text
*      -->P_1096   text
*      -->P_1097   text
*      -->P_1098   text
*      -->P_1099   text
*      -->P_1100   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat_equi  USING: p_campo         TYPE c
                                p_desc          TYPE c
                                p_tam           TYPE c
                                p_hot           TYPE c
                                p_zero          TYPE c
                                p_sum           TYPE c
                                p_edit          TYPE c
                                p_check         TYPE c
                                p_ref_tabname   LIKE dd02d-tabname
                                p_ref_fieldname LIKE dd03d-fieldname
                                p_tabname       LIKE dd02d-tabname
                                p_no_out        TYPE c.

  DATA: wt_fcat TYPE lvc_s_fcat.
  CLEAR: wa_layout, wt_fcat.
  wt_fcat-fieldname = p_campo.
  wt_fcat-scrtext_l = p_desc.
  wt_fcat-scrtext_m = p_desc.
  wt_fcat-scrtext_s = p_desc.
  wt_fcat-hotspot   = p_hot.
  wt_fcat-no_zero   = p_zero.
  wt_fcat-outputlen = p_tam.
  wt_fcat-edit      = p_edit.
  wt_fcat-checkbox  = p_check.
  wt_fcat-ref_table = p_ref_tabname.
  wt_fcat-ref_field = p_ref_fieldname.
  wt_fcat-tabname   = p_ref_tabname.
  wt_fcat-no_out    = p_no_out.
  APPEND wt_fcat TO it_fcat_equi.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONTAINER_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM container_1 .

  PERFORM alv_preenche_cat_sint USING:
  'TOTAL      ' 'Total Amostra    '  '15 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'NORMAL     ' 'Normal           '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'ATENCAO    ' 'Atenção          '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'ANORMAL    ' 'Anormal          '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'CRITICO    ' 'Critico          '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' '.


  IF ( obj_custom_0200 IS INITIAL ).
    CREATE OBJECT obj_custom_0200
      EXPORTING
        container_name              = 'CONTAINER_SINT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0200
      EXPORTING
        i_parent          = obj_custom_0200
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

  ENDIF.

  wa_layout = VALUE #( ctab_fname    = 'CELL_COLOR'
                     excp_conds    = 'X'
                     zebra         = 'X'
                     sel_mode      = 'A'
                     cwidth_opt    = ' '     "  Otimizar colunas na tela
                     totals_bef    = ' ' ).

  CALL METHOD obj_alv_0200->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      it_toolbar_excluding          = gw_exc_button
      i_save                        = 'A'
    CHANGING
      it_fieldcatalog               = it_fcat_sint
      it_outtab                     = obj_dados->lt_ztpm_amost_sint
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.



  CALL METHOD obj_alv_0200->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD obj_alv_0200->refresh_table_display
    EXPORTING
      is_stable = ws_stable.

  PERFORM container_2.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONTAINER_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM container_2 .
  PERFORM alv_preenche_cat_equi USING:
  'EARTX      ' 'Categoria        '  '15 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'CRITICO    ' 'Critica          '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'ANORMAL    ' 'Anormal          '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'ATENCAO    ' 'Atenção          '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  'NORMAL     ' 'Normal           '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' '.


  IF ( obj_custom_0201 IS INITIAL ).
    CREATE OBJECT obj_custom_0201
      EXPORTING
        container_name              = 'CONTAINER_EQUI'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0201
      EXPORTING
        i_parent          = obj_custom_0201
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

  ENDIF.

  wa_layout = VALUE #( ctab_fname    = 'CELL_COLOR'
                     excp_conds    = 'X'
                     zebra         = 'X'
                     sel_mode      = 'A'
                     cwidth_opt    = ' '     "  Otimizar colunas na tela
                     totals_bef    = ' ' ).

  CALL METHOD obj_alv_0200->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      it_toolbar_excluding          = gs_exc_button
      i_save                        = 'A'
    CHANGING
      it_fieldcatalog               = it_fcat_equi
      it_outtab                     = obj_dados->lt_ztpm_amost_equi
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.



  CALL METHOD obj_alv_0201->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD obj_alv_0201->refresh_table_display
    EXPORTING
      is_stable = wt_stable.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DEFINE_CONTAINER_EXTENSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0666   text
*      -->P_0667   text
*      -->P_0668   text
*      -->P_0669   text
*----------------------------------------------------------------------*
FORM f_define_container_extension USING row    TYPE i
                                        column TYPE i
                                        height TYPE i
                                        width  TYPE i.

* Altura
  IF row IS NOT INITIAL
    AND height IS NOT INITIAL.

    o_splitter->set_row_height(
      EXPORTING
        id     = row
        height = height
        ).
  ENDIF.

* Largura
  IF column IS NOT INITIAL
    AND width IS NOT INITIAL.

    o_splitter->set_column_width(
      EXPORTING
        id    = column
        width = width
    ).
  ENDIF.


ENDFORM.                    " F_DEFINE_CONTAINER_EXTENSION
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0673   text
*      -->P_0674   text
*      -->P_O_CONTAINER_1  text
*----------------------------------------------------------------------*
FORM f_create_container  USING row       TYPE i
                               col       TYPE i
                               container TYPE REF TO cl_gui_container.


* Aqui é feito a criação do container com referencia a uma das partições.
  o_splitter->get_container(
    EXPORTING
      row    = row
      column = col
    RECEIVING
      container = container
      ).


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DEFINE_CONTAINER_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_define_container_header .


  IF o_docking IS BOUND
    AND o_alv_1 IS NOT BOUND.

* Cria o ALV OO
    CREATE OBJECT o_alv_1
      EXPORTING
        i_parent          = o_container_1   " Parent Container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.


*    PERFORM CABECARIO USING O_ALV_1.

    FREE obj_alv_0110.
    CREATE OBJECT obj_alv_0110
      EXPORTING
        i_parent = dg_parent_alv.

    FREE it_fcat_sint.
    PERFORM alv_preenche_cat_sint USING:
  ' TOTAL      ' 'Total Amostra    '  '15 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' CRITICO    ' 'Critico          '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' ATENCAO    ' 'Atenção          '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' NORMAL     ' 'Normal           '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' ANORMAL    ' 'Anormal          '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' '.



    SET HANDLER:
   lcl_event_handler=>set_toolbar      FOR o_alv_1,
   lcl_event_handler=>get_ucomm        FOR o_alv_1.

    lst_layout-zebra      = 'X'.
*    LST_LAYOUT-CWIDTH_OPT = 'X'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].

    PERFORM exclude.

* Exibe o ALV OO
    o_alv_1->set_table_for_first_display(
      EXPORTING
        is_layout                     =  lst_layout   " Layout
        it_toolbar_excluding          = it_exclude_fcode
     CHANGING
        it_outtab                     =  obj_dados->lt_ztpm_amost_sint        " Output Table
        it_fieldcatalog               =  it_fcat_sint   " Field Catalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4
    ).

  ELSE.

* Atualiza o ALV OO
    o_alv_1->refresh_table_display(
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2
    ).

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DEFINE_CONTAINER_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_define_container_item .
  DATA:
    lst_layout TYPE lvc_s_layo.

  IF o_docking IS BOUND
      AND o_alv_2 IS NOT BOUND.

    CREATE OBJECT o_alv_2
      EXPORTING
        i_parent          = o_container_2   " Parent Container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.


    FREE it_fcat_equi.
    PERFORM alv_preenche_cat_equi USING:
  ' EARTX      ' 'Categoria        '  '15 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' CRITICO    ' 'Critico          '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' P_CRITICO  ' '%                '  '05 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' ATENCAO    ' 'Atenção          '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' P_ATENCAO  ' '%                '  '05 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' NORMAL     ' 'Normal           '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' P_NORMAL   ' '%                '  '05 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' ANORMAL    ' 'Anormal          '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' P_ANORMAL  ' '%                '  '05 ' ''  '' ''  ' ' ' '  '' '' '' ' ',
  ' TOTAL      ' 'Total            '  '10 ' ''  '' ''  ' ' ' '  '' '' '' ' '.


    lst_layout-zebra      = 'X'.
*    LST_LAYOUT-CWIDTH_OPT = 'X'.

*    SET HANDLER:
* LCL_EVENT_HANDLER=>SET_TOOLBAR      FOR O_ALV_2,
* LCL_EVENT_HANDLER=>GET_UCOMM        FOR O_ALV_2.

    PERFORM exclude_alv2.


    o_alv_2->set_table_for_first_display(
      EXPORTING
        is_layout                     =  lst_layout   " Layout
        it_toolbar_excluding          = it_exclude_alv2
      CHANGING
        it_outtab                     = obj_dados->lt_ztpm_amost_equi    " Output Table
        it_fieldcatalog               = it_fcat_equi    " Field Catalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4
    ).

  ELSE.
    o_alv_2->refresh_table_display(
  EXCEPTIONS
    finished       = 1
    OTHERS         = 2
).
  ENDIF.

ENDFORM.

AT SELECTION-SCREEN OUTPUT.

  SELECT id cliente
  FROM ztpm_l_amost
  INTO TABLE it_ztpm_l_amost.

  IF it_ztpm_l_amost IS NOT INITIAL.
    SORT it_ztpm_l_amost BY id.
    DELETE ADJACENT DUPLICATES FROM it_ztpm_l_amost COMPARING id.

    CLEAR value.
    FREE lista.
    LOOP AT it_ztpm_l_amost INTO DATA(w_ztpm_l_amost).
      value-key   = w_ztpm_l_amost-id.
      value-text  = w_ztpm_l_amost-cliente.
      APPEND value TO lista.
    ENDLOOP.

    name = 'P_CLASS'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name
        values = lista.
  ENDIF.


  SELECT id_comp compartimento
  FROM ztpm_r_amost
  INTO TABLE it_ztpm_r_amost.

  IF it_ztpm_r_amost IS NOT INITIAL.
    SORT it_ztpm_r_amost BY compartimento.
    DELETE ADJACENT DUPLICATES FROM it_ztpm_r_amost COMPARING compartimento.


    LOOP AT it_ztpm_r_amost INTO DATA(w_ztpm_r_amost).
      value_comp-key   = w_ztpm_r_amost-id_comp.
      value_comp-text  = w_ztpm_r_amost-compartimento.
      APPEND value_comp TO lista_comp.
    ENDLOOP.

    name_comp = 'P_COMPT'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name_comp
        values = lista_comp.
  ENDIF.


  LOOP AT SCREEN.
    CASE screen-name.
      WHEN '%_P_DATA_%_APP_%-TEXT'
        OR '%_P_DATA_%_APP_%-OPTI_PUSH'
        OR 'P_DATA-LOW'
        OR '%_P_DATA_%_APP_%-TO_TEXT'
        OR 'P_DATA-HIGH'
        OR '%_P_DATA_%_APP_%-VALU_PUSH'.



        IF rb_sint IS NOT INITIAL OR rb_anal IS NOT INITIAL AND p_agu IS INITIAL AND p_fin IS NOT INITIAL.
          screen-active = '1'.         "show parameters     "n921165
        ELSE.                                               "n921165
          screen-active = '0'.         "Hide parameters     "n921165
          CLEAR p_data[].
        ENDIF.
        MODIFY SCREEN.


      WHEN 'P_AGU' OR 'P_FIN' OR '%B009000_BLOCK_1000' OR '%B009000_BLOCK_1000'.

        IF rb_sint IS NOT INITIAL OR rb_cad IS NOT INITIAL.
          screen-active = '0'.         "show parameters     "n921165
        ELSE.                                               "n921165
          screen-active = '1'.         "Hide parameters     "n921165
        ENDIF.
        MODIFY SCREEN.

      WHEN '%B009010_BLOCK_1000'
          OR '%_BLOCK_1000010'
          OR '%F010014_1000'
          OR '%B003025_BLOCK_1000'
          OR '%F006029_1000'
          OR 'P_CLASS'
          OR '%B004032_BLOCK_1000'
          OR 'RB_ANAL'
          OR '%F005036_1000'
          OR '%F008039_1000'
          OR 'RB_SINT'
          OR '%_17SNS0000002289_%_%_%_%_%_%_'.


        IF rb_cad IS NOT INITIAL .
          screen-active = '0'.         "show parameters     "n921165
        ELSE.                                               "n921165
          screen-active = '1'.         "Hide parameters     "n921165
        ENDIF.
        MODIFY SCREEN.


      WHEN 'P_DATE-LOW'
        OR 'P_DATE-HIGH'
        OR '%_P_DATE_%_APP_%-TEXT'
        OR '%_P_DATE_%_APP_%-OPTI_PUSH'
        OR '%_P_DATE_%_APP_%-TO_TEXT'
        OR '%_P_DATE_%_APP_%-VALU_PUSH'.
        IF rb_cad IS NOT INITIAL .
          screen-active = '1'.         "show parameters     "n921165
        ELSE.                                               "n921165
          screen-active = '0'.         "Hide parameters     "n921165
        ENDIF.
        MODIFY SCREEN.

    ENDCASE.
  ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude .

*   Excluir Buttons Toolbar
  FREE: it_exclude_fcode.

  wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_graph.
  APPEND wa_exclude_fcode TO it_exclude_fcode.

ENDFORM.

FORM exclude_alv2 .

*   Excluir Buttons Toolbar
  FREE: it_exclude_alv2.

  wa_exclude_alv2 = cl_gui_alv_grid=>mc_fc_graph.
  APPEND wa_exclude_alv2 TO it_exclude_alv2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CABECARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cabecario USING obj_custom_0110.

  CREATE OBJECT dg_splitter_1
    EXPORTING
      parent  = obj_custom_0110
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
      columns = 2.

  CALL METHOD dg_splitter_2->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = dg_parent_2.

  CALL METHOD dg_splitter_2->get_container
    EXPORTING
      row       = 1
      column    = 2
    RECEIVING
      container = dg_parent_2a.

  CALL METHOD dg_splitter_1->set_row_height
    EXPORTING
      id     = 1
      height = 20.

  CALL METHOD dg_splitter_2->set_column_width
    EXPORTING
      id    = 1
      width = 65.

  CREATE OBJECT picture
    EXPORTING
      parent = dg_parent_2a.

  PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

  CALL METHOD picture->load_picture_from_url
    EXPORTING
      url = url.

  CALL METHOD picture->set_display_mode
    EXPORTING
      display_mode = picture->display_mode_fit_center.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2216   text
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
*&      Form  CABECARIO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cabecario_alv USING q_linha.

  IF p_class IS NOT INITIAL.

    SELECT SINGLE cliente
    FROM ztpm_l_amost
    INTO vl_butxt
      WHERE id EQ p_class.

    CONCATENATE 'CLIENTE:' vl_butxt INTO sdydo_text_element SEPARATED BY space.
    CLEAR: vl_butxt.

    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_butxt, sdydo_text_element.

  ELSE.

    CONCATENATE 'EMPRESA:' 'MULTIPLAS SELEÇÕES' INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_butxt, sdydo_text_element.
  ENDIF.


  IF p_data IS NOT INITIAL OR p_date IS NOT INITIAL.
    IF p_date IS NOT INITIAL.
      APPEND LINES OF p_date TO p_date.
    ENDIF.
    DATA: data  TYPE char10,
          data_ TYPE char10.

    LOOP AT p_data.
      data = p_data-low+6(2) && '.' && p_data-low+4(2) && '.' &&  p_data-low(4).
      data_ = p_data-high+6(2) && '.' && p_data-high+4(2) && '.' &&  p_data-high(4).
      IF p_data-option NE 'EQ' AND p_data-option NE 'BT'.
        sdydo_text_element = 'Centro: Multiplas Seleções'.
        EXIT.
      ELSEIF p_data-option EQ 'BT'.
        CONCATENATE 'PERIODO:' data 'a' data_ INTO sdydo_text_element SEPARATED BY space.
        EXIT.
      ELSE.
        vl_cont = vl_cont + 1.
        IF vl_cont GT 1.
          sdydo_text_element = 'Centro: Multiplas Seleções'.
        ELSE.
          CONCATENATE 'PERIODO:' data INTO sdydo_text_element SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, sdydo_text_element.
  ELSE.
    sdydo_text_element = 'PERIODO:'.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.
  CLEAR: vl_cont, sdydo_text_element.


  IF q_linha IS NOT INITIAL.

    DATA: icon_amost TYPE char8.
    icon_amost = icon_batch.

*    SDYDO_TEXT_ELEMENT =  Q_LINHA && ICON_AMOST.

    CONCATENATE 'TOTAL DE AMOSTRAS:' q_linha INTO sdydo_text_element SEPARATED BY space.

    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_butxt, sdydo_text_element.
  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DEFINE_CONTAINER_CABECARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_define_container_cabecario .

  DATA table_element  TYPE REF TO cl_dd_table_element.
  DATA table_texts     TYPE sdydo_text_table.
  DATA table_text     LIKE LINE OF table_texts.
  DATA column         TYPE REF TO cl_dd_area.

  CLEAR vl_butxt.

  o_splitter->set_column_width( id = 1 width = 100 ).
  o_splitter->set_border( cl_gui_cfw=>false ).

  IF ( document IS NOT BOUND ).
    CREATE OBJECT document.
  ELSE.
    document->initialize_document( ).
  ENDIF.

  "//Build title text
  document->add_text( text = 'Parâmetros de seleção:' sap_style = cl_dd_area=>heading ).
  document->new_line(  ). document->underline( ).

  document->add_table(
    EXPORTING
      no_of_columns     = 2
      border            = '0'
    IMPORTING
      table             = DATA(_doctable1)
  ).

  _doctable1->add_column( EXPORTING width = '30%' IMPORTING column = DATA(_column_key) ).
  _doctable1->add_column( IMPORTING column = DATA(_column_value) ).
  _column_key->add_text( text = 'CLIENTE:' sap_fontsize = cl_dd_area=>large sap_emphasis = cl_dd_area=>emphasis ).

  IF p_class IS NOT INITIAL.

    SELECT SINGLE cliente
    FROM ztpm_l_amost
    INTO vl_butxt
      WHERE id EQ p_class.

    _column_value->add_text( text = CONV #( vl_butxt ) ).
  ENDIF.
  _doctable1->new_row( ).

  IF p_data IS NOT INITIAL.
    DATA: data  TYPE char10,
          data_ TYPE char10.

    data  = p_data-low+6(2) && '.' && p_data-low+4(2) && '.' &&  p_data-low(4).
    data_ = p_data-high+6(2) && '.' && p_data-high+4(2) && '.' &&  p_data-high(4).

    _column_key->add_text( text = 'PERIODO:' sap_fontsize = cl_dd_area=>large sap_emphasis = cl_dd_area=>emphasis ).
    CONCATENATE data 'a' data_ INTO DATA(_data) SEPARATED BY space.
    _column_value->add_text( text = CONV #( _data ) ).
  ENDIF.

  _doctable1->new_row( ).

  document->merge_document( ).
  document->display_document( parent = o_container_3 ).




ENDFORM.


*INITIALIZATION.
*  GS_VARIANT_C-REPORT      = SY-REPID.
*  variant-report = sy-repid.
*  def_variant-report = sy-repid.
*
** Verificar se existe uma variante default
*  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
*    EXPORTING
*      i_save     = 'A'
*    CHANGING
*      cs_variant = def_variant
*    EXCEPTIONS
*      not_found  = 2.
*
*  IF sy-subrc = 0.
*    p_varia = def_variant-variant.
*  ENDIF.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
**
*  DATA: vg_repid   LIKE sy-repid,
*        vg_variant TYPE disvariant.
*
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.
*  PERFORM f_f4_variant.
*&---------------------------------------------------------------------*
*&      Form  F_F4_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_f4_variant .

*  vg_repid          = sy-repid.
*  variante-report = vg_repid.
*
*  IF ( NOT p_varia IS INITIAL ).
*    vg_variant-variant = p_varia.
*
*  ENDIF.
*  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
*    EXPORTING
*      is_variant    = variante
*      i_save        = 'A'
*    IMPORTING
*      es_variant    = variante
*    EXCEPTIONS
*      not_found     = 1
*      program_error = 2
*      OTHERS        = 3.
*
*  IF ( sy-subrc NE 0 ).
*    MESSAGE s000(z01) WITH 'Não existe variante'.
*    STOP.
*  ELSE.
*    MOVE variante-variant TO p_varia.
*    MOVE variante-variant TO gs_variant_c-variant.
*  ENDIF.


*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_EXEC_API
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_exec_api.
  DATA: vg_seq TYPE string.
* "// Criando o Objeto da Classe
  DATA(obj_create) = NEW zcl_pm_ordem( ).
**"// Processa start JOB para consultar resultados amastra.
  CLEAR: vg_seq.
  vg_seq = sy-datum && sy-uzeit.
  data(JOB) = 'JOB_ZPMR0039_' && vg_seq.
  CALL METHOD obj_create->call_report
    EXPORTING
      i_sequen = CONV #( vg_seq )
      i_report = 'ZPMR0039'.
*   "// Finaliza o processo em caso de dados OffLine

  MESSAGE s024(sd) WITH 'JOB-->' JOB 'iniciado para consultar analise amostras'.
  EXIT.

ENDFORM.
