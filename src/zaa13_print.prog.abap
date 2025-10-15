*&---------------------------------------------------------------------*
*&  Include           ZAA13_PRINT
*&---------------------------------------------------------------------*

MODULE criar_alv_0001 OUTPUT.

  IF sy-ucomm EQ 'ENVIAR'.
    PERFORM busca_zaa007.
  ENDIF.

  IF ( tg_saida[] IS INITIAL ).
    MESSAGE 'Sem informações para os parâmetros informados!' TYPE 'I'.
    EXIT.
  ELSE.
    SORT tg_saida[] BY bukrs werks anln1.
    PERFORM criar_catalog_0001.
    PERFORM criar_alv_0001.
  ENDIF.

ENDMODULE.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      user_command_0100 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD user_command_0100.

    DATA: it_selected_rows TYPE lvc_t_row,
          wa_selected_rows TYPE lvc_s_row.

    IF ( sy-ucomm = 'ANEXO' ).

    ENDIF.

  ENDMETHOD.

  METHOD handle_button_click.
    DATA: anexo_obj     TYPE REF TO cl_gos_manager,
          vl_ip_service TYPE sgs_srvnam,
          wa_bor        TYPE borident,
          vl_obj_key    TYPE sibflporb-instid,
          tl_anexos     TYPE TABLE OF bdn_con,
          ip_mode       TYPE sgs_rwmod.

    DATA(vl_ano) = sy-datum+0(4).

    READ TABLE tg_saida[] ASSIGNING <wg_saida> INDEX es_row_no-row_id.

    CASE es_col_id.
      WHEN 'ANEXO'.

        CREATE OBJECT anexo_obj TYPE cl_gos_manager.

        IF ( <wg_saida>-anexo EQ '@1F@' ).        "Sem anexo

          IF ( <wg_saida>-edicao IS INITIAL ).
            MESSAGE 'Editar a linha para anexar arquivos!' TYPE 'I'.
            EXIT.
          ELSE.
            vl_ip_service = 'PCATTA_CREA'.
            ip_mode = 'E'.
          ENDIF.
        ELSE.
          IF ( <wg_saida>-edicao IS INITIAL ).
            vl_ip_service = 'VIEW_ATTA'.
            ip_mode = 'R'.
          ELSE.
            vl_ip_service = 'VIEW_ATTA'.
            ip_mode = 'E'.
          ENDIF.
        ENDIF.

        wa_bor-objkey   = |ZAA18{ <wg_saida>-anln1 }{ <wg_saida>-werks }{ vl_ano }|.
        wa_bor-objtype  = 'ZAA13'.
        anexo_obj->set_rw_mode( ip_mode = ip_mode ).
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
        "VERIFICANDO SE EXISTEM ANEXOS PARA O OBJETO:
        vl_obj_key = |ZAA18{ <wg_saida>-anln1 }{ <wg_saida>-werks }{ vl_ano }|.
        "128405 CS2023000909 Melhorias ZAA19 - Transação de baixa - PSA
*        CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
*          EXPORTING
*            classname          = 'ZAA13'
*            objkey             = vl_obj_key
*            client             = sy-mandt
*          TABLES
*            gos_connections    = tl_anexos
*          EXCEPTIONS
*            no_objects_found   = 1
*            internal_error     = 2
*            internal_gos_error = 3
*            OTHERS             = 4.
        SELECT SINGLE * FROM srgbtbrel WHERE instid_a = @vl_obj_key AND typeid_a = 'ZAA13' AND reltype = 'ATTA' INTO @DATA(WA_srgbtbrel).

        IF sy-subrc = 0.
          "IF ( tl_anexos[] IS NOT INITIAL ).
          <wg_saida>-anexo = '@1E@'.
          <wg_saida>-check_anexo = 'X'.
        ELSE.
          <wg_saida>-anexo = '@1F@'.
          <wg_saida>-check_anexo = ''.
        ENDIF.

        CALL METHOD wa_alv_0001->refresh_table_display.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      zm_handle_hotspot_report
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "lcl_event_compras DEFINITION

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_hotspot_report.

    PERFORM z_hotspot_report USING  e_row_id e_column_id es_row_no.

  ENDMETHOD.
ENDCLASS.


FORM criar_catalog_0001.
  REFRESH: it_fcat[].

  PERFORM alv_catalog_0001  USING :

'TG_SAIDA'  'BUKRS'             'Empresa'               '06'    ''  ''  ''      ''  'C'   'X' 'BUKRS'           'ZAA007',
'TG_SAIDA'  'WERKS'             'Filial'                '04'    ''  ''  ''      ''  'C'   'X' 'WERKS'           'ZAA007',
'TG_SAIDA'  'ANLN1'             'Imobilizado'           '08'    'X' ''  ''      ''  'C'   'X' 'ANLN1'           'ZAA007',
'TG_SAIDA'  'ANLN2'             'Sub nº'                '06'    ''  ''  ''      ''  'C'   'X' 'ANLN2'           'ZAA007',
'TG_SAIDA'  'TXT50'             'Denominação Imob.'     '15'    ''  ''  ''      ''  'C'   'X' 'TXT50'           'ZAA007',
'TG_SAIDA'  'TXA50'             'Denominãção Imob.2'    '10'    ''  ''  ''      ''  'C'   'X' 'TXA50'           'ZAA007',
'TG_SAIDA'  'KOSTL'             'Centro Custo'          '10'    ''  ''  ''      ''  'C'   'X' 'KOSTL'           'ZAA007',
'TG_SAIDA'  'ZUGDT'             'Data Aquisição'        '10'    ''  ''  'C700'  ''  'C'   'X' 'ZUGDT'           'ZAA007',
*** RMNI - CS1066933 - Alterar nomenclaturas - 29/03/2023 - Inicio                            '' '',
*'TG_SAIDA'  'MOTIVO'            'Motivo Baixa'          '15'    ''  ''  'C700'  ''  'C' 	'X' '' '',
'TG_SAIDA'  'MOTIVO'            'Observação CSC Contábil' '15'    ''  ''  'C700'  '' 'C'  'X' 'MOTIVO'          'ZAA007',
*** RMNI - CS1066933 - Alterar nomenclaturas - 29/03/2023 - Fim                               '' '',
'TG_SAIDA'  'VLR_AQ_BRL'        'Vlr.Aquisição BRL'     '10'    ''  'X' ''      ''  'C'   'X' 'VLR_AQ_BRL'      'ZAA007',
'TG_SAIDA'  'DEPREC_BRL'        'Depreciação Acum.BRL'  '10'    ''  'X' ''      ''  'C'   'X' 'DEPREC_BRL'      'ZAA007',
'TG_SAIDA'  'VLR_CONTABIL_BRL'  'Valor Contábil BRL'    '10'    ''  'X' ''      ''  'C'   'X' 'VLR_CONTABIL_BRL' 'ZAA007',
'TG_SAIDA'  'VLR_AQ_USD'        'Vlr.Aquisição USD'     '10'    ''  'X' ''      ''  'C'   'X' 'VLR_AQ_USD'      'ZAA007',
'TG_SAIDA'  'DEPREC_USD'        'Depreciação Acum.USD'  '10'    ''  'X' ''      ''  'C'   'X' 'DEPREC_USD'      'ZAA007',
'TG_SAIDA'  'VLR_CONTABIL_USD'  'Valor Contábil USD'    '10'    ''  'X' ''      ''  'C'   'X' 'LR_CONTABIL_USD'  'ZAA007',
*** RMNI - CS1066933 - Alterar nomenclaturas - 24/03/2023 - Inicio                            '' '',
*'TG_SAIDA'  'ESTADO_BEM'        'Estado do bem'         '11'    ''  ''  'C100'  '' 'C'   'X' '' '',
'TG_SAIDA'  'ESTADO_BEM'        'Motivo Baixa'          '11'    ''  ''  'C100'  ''  'C'   'X' 'ESTADO_BEM'      'ZAA007',
*** RMNI - CS1066933 - Alterar nomenclaturas - 24/03/2023 - Fim	                              '' '',
'TG_SAIDA'  'RESPONSAVEL'       'Responsável Aval.'     '12'    ''  ''  'C100'  ''  'C'   'X' 'RESPONSAVEL'     'ZAA007',
'TG_SAIDA'  'ANEXO'             'Anexo'                 '05'    ''  ''  'C100'  ''  'C'   'X' ''                '',
'TG_SAIDA'  'OBS_CONTROLLER'    'Obs.Controller'        '20'    ''  ''  'C500'  ''  'C'   'X' 'OBS_CONTROLLER' 	'ZAA007',
'TG_SAIDA'  'SOLICITANTE'       'Solicitante'           '12'    ''  ''  ''      ''  'C'   'X' 'SOLICITANTE'     'ZAA007'.


ENDFORM.

FORM alv_catalog_0001  USING   p_table    TYPE c
                               p_campo    TYPE c
                               p_desc     TYPE c
                               p_tam      TYPE c
                               p_zero     TYPE c
                               p_sum      TYPE c
                               p_cor      TYPE c
                               p_convexit TYPE c
                               p_just     TYPE c
                               p_opt      TYPE c
                               p_REF_FIELD      TYPE c
                               p_REF_TABLE      TYPE c.

  DATA: p_hot   TYPE c,
        wl_fcat TYPE lvc_s_fcat,
        marca   TYPE c VALUE 'X'.

  CASE p_campo.
    WHEN 'ANLN1'.
      wl_fcat-hotspot = 'X'.
    WHEN 'RESPONSAVEL'.
      wl_fcat-edit = marca.
    WHEN 'ESTADO_BEM'.
      wl_fcat-edit = 'X'.
      wl_fcat-drdn_hndl = '2'.
      wl_fcat-outputlen = 12.
      wl_fcat-checktable = '!'.
  ENDCASE.

  wl_fcat-tabname   = p_table.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum   =  p_sum.
  wl_fcat-emphasize = p_cor.
  wl_fcat-convexit  = p_convexit.
  wl_fcat-just      = p_just.
  wl_fcat-col_opt   = p_opt.
  wl_fcat-ref_field   = p_ref_field.
  wl_fcat-ref_table   = p_ref_table.

  APPEND wl_fcat TO it_fcat.

ENDFORM.

FORM criar_alv_0001.

  DATA: wa_event_0001    TYPE REF TO lcl_event_receiver.

  DATA: dg_splitter_1 TYPE REF TO cl_gui_splitter_container,
        dg_parent_alv TYPE REF TO cl_gui_container,
        dg_dyndoc_id  TYPE REF TO cl_dd_document.

  IF wa_container_0001 IS INITIAL.

    CREATE OBJECT wa_container_0001
      EXPORTING
        container_name = 'CONTAINER_0100'.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = wa_container_0001
        rows    = 1
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 18.

    CREATE OBJECT wa_alv_0001
      EXPORTING
        i_parent = dg_parent_alv.

    IF wa_event_0001 IS INITIAL.
      CREATE OBJECT wa_event_0001.
      SET HANDLER: wa_event_0001->zm_handle_hotspot_report FOR wa_alv_0001.
    ENDIF.

    wa_layout-cwidth_opt = ' '.
    wa_layout-sel_mode   = 'A'.
    wa_layout-stylefname = 'CELLSTYLES'.

    PERFORM set_drdwn_table.

    CALL METHOD wa_alv_0001->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        is_variant           = gs_variant_c
        i_save               = 'A'
        it_toolbar_excluding = tl_function
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = tg_saida[].

    CALL METHOD wa_alv_0001->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_receiver=>zm_handle_hotspot_report FOR wa_alv_0001.
    SET HANDLER: lcl_event_handler=>handle_button_click FOR wa_alv_0001.

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

  ELSE.
    CALL METHOD wa_alv_0001->refresh_table_display.
  ENDIF.
ENDFORM.                    " CRIAR_ALV_0001


*&---------------------------------------------------------------------*
*&      Form  SET_DRDWN_TABLE
*&---------------------------------------------------------------------*
FORM set_drdwn_table.
  DATA: lt_dropdown TYPE lvc_t_drop,
        ls_dropdown TYPE lvc_s_drop.

  DATA: lt_dral TYPE lvc_t_dral,
        ls_dral TYPE lvc_s_dral.

* Listbox - Handle 2 - Estado do Bem
  ls_dropdown-handle = '2'.
  ls_dropdown-value  = 'Avaria'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '2'.
  ls_dropdown-value  = 'Furto/Extravio'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '2'.
  ls_dropdown-value  = 'Sucata'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '2'.
*** RMNI - CS1066933 - Alterar nomenclaturas - 24/03/2023 - Inicio
*  LS_DROPDOWN-VALUE  = 'Morte'.
  ls_dropdown-value  = 'Perda'.
*** RMNI - CS1066933 - Alterar nomenclaturas - 24/03/2023 - Fim
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '2'.
  ls_dropdown-value  = 'Estoque'.
  APPEND ls_dropdown TO lt_dropdown.
*  LS_DROPDOWN-HANDLE = '2'.
*  LS_DROPDOWN-VALUE  = 'Ruim'.
*  APPEND LS_DROPDOWN TO LT_DROPDOWN.

  wa_alv_0001->set_drop_down_table(
   it_drop_down = lt_dropdown ).

ENDFORM.              "FORM SET_DRDWN_TABLE
