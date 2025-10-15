*----------------------------------------------------------------------*
***INCLUDE ZLESR0082_0500 .
*----------------------------------------------------------------------*

TABLES: zlest0101, zde_zlest0107.

DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      it_zlest0107_alv     TYPE TABLE OF zde_zlest0107 WITH HEADER LINE,
      it_zlest0107_sel     TYPE TABLE OF zde_zlest0107 WITH HEADER LINE.

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_0501 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_0501 DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_0501 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_0501 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: ty_toolbar   TYPE stb_button.

    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_positive.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = TEXT-004.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_negative.
    ty_toolbar-function  = 'EXC'.
    ty_toolbar-quickinfo = TEXT-005.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_trend_up.
    ty_toolbar-function  = 'UP'.
    ty_toolbar-quickinfo = TEXT-002.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_trend_down.
    ty_toolbar-function  = 'DOWN'.
    ty_toolbar-quickinfo = TEXT-003.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    FIELD-SYMBOLS: <fs_0107> TYPE zde_zlest0107.

    DATA: p_parid          TYPE j_1bparid,
          wa_aux           LIKE LINE OF it_zlest0107_alv,
          wa_aux2          LIKE LINE OF it_zlest0107_alv,
          pos_item         TYPE zde_id_rota_item,
          pos_back         TYPE zde_id_rota_item,
          qtd_linhas       TYPE i,
          it_zlest0107_aux TYPE TABLE OF zde_zlest0107,
          it_zlest0107_nao TYPE TABLE OF zde_zlest0107.

    FIELD-SYMBOLS: <fsmov> TYPE zde_mov_fornecedor.

    PERFORM popula_selecao_0501.

    CASE e_ucomm.
      WHEN 'ADD'.

        pos_item = 1.

        LOOP AT it_zlest0107_alv INTO wa_aux.
          ADD 1 TO pos_item.
        ENDLOOP.

        CLEAR: zde_zlest0107.
        zde_zlest0107-id_rota_item = pos_item.
        zde_zlest0107-country      = zlest0101-country.
        CALL SCREEN 0504 STARTING AT 15 01.

      WHEN 'EXC'.
        IF it_zlest0107_sel[] IS INITIAL.
          MESSAGE s101.
          RETURN.
        ENDIF.
        LOOP AT it_zlest0107_sel INTO wa_aux.
          DELETE it_zlest0107_alv WHERE id_rota EQ wa_aux-id_rota AND id_rota_item EQ wa_aux-id_rota_item.
        ENDLOOP.
      WHEN 'UP'.

        IF it_zlest0107_sel[] IS INITIAL.
          MESSAGE s101.
          RETURN.
        ENDIF.

        DESCRIBE TABLE it_zlest0107_alv LINES qtd_linhas.

        "Separa Não Selecionados.
        LOOP AT it_zlest0107_alv INTO wa_aux.
          READ TABLE it_zlest0107_sel INTO wa_aux2 WITH KEY id_rota_item = wa_aux-id_rota_item.
          IF sy-subrc IS NOT INITIAL.
            APPEND wa_aux TO it_zlest0107_nao.
          ENDIF.
        ENDLOOP.

        LOOP AT it_zlest0107_sel ASSIGNING <fs_0107>.
          ADD -1 TO <fs_0107>-id_rota_item.
        ENDLOOP.

        "IT_ZLEST0107_SEL - Selecionados
        "IT_ZLEST0107_NAO - Não Selecionados
        pos_item = 1.

        DO qtd_linhas TIMES.

          READ TABLE it_zlest0107_sel INTO wa_aux WITH KEY id_rota_item = pos_item.
          IF sy-subrc IS INITIAL.
            wa_aux-id_rota_item = pos_item.
            APPEND wa_aux TO it_zlest0107_aux.
          ENDIF.

          READ TABLE it_zlest0107_nao INTO wa_aux WITH KEY id_rota_item = pos_item.
          IF sy-subrc IS INITIAL.
            pos_back = pos_item.
            wa_aux-id_rota_item = pos_back.
            APPEND wa_aux TO it_zlest0107_aux.
          ENDIF.

          ADD 1 TO pos_item.
        ENDDO.

        CLEAR: it_zlest0107_alv[].

        pos_item = 1.
        LOOP AT it_zlest0107_aux INTO wa_aux.
          wa_aux-id_rota_item = pos_item.
          APPEND wa_aux TO it_zlest0107_alv.
          ADD 1 TO pos_item.
        ENDLOOP.

      WHEN 'DOWN'.

        IF it_zlest0107_sel[] IS INITIAL.
          MESSAGE s101.
          RETURN.
        ENDIF.

        DESCRIBE TABLE it_zlest0107_alv LINES qtd_linhas.

        "Separa Não Selecionados.
        LOOP AT it_zlest0107_alv INTO wa_aux.
          READ TABLE it_zlest0107_sel INTO wa_aux2 WITH KEY id_rota_item = wa_aux-id_rota_item.
          IF sy-subrc IS NOT INITIAL.
            APPEND wa_aux TO it_zlest0107_nao.
          ENDIF.
        ENDLOOP.

        "IT_ZLEST0107_SEL - Selecionados
        "IT_ZLEST0107_NAO - Não Selecionados
        pos_item = 1.

        DO qtd_linhas TIMES.
          READ TABLE it_zlest0107_sel INTO wa_aux WITH KEY id_rota_item = pos_item.
          IF sy-subrc IS INITIAL.
            ADD 1 TO wa_aux-id_rota_item.
            APPEND wa_aux TO it_zlest0107_aux.
          ENDIF.

          READ TABLE it_zlest0107_nao INTO wa_aux WITH KEY id_rota_item = pos_item.

          IF sy-subrc IS INITIAL.
            pos_back = pos_item.
            READ TABLE it_zlest0107_aux INTO wa_aux2 WITH KEY id_rota_item = pos_item.
            WHILE sy-subrc IS INITIAL.
              ADD -1 TO pos_back.
              READ TABLE it_zlest0107_aux INTO wa_aux2 WITH KEY id_rota_item = pos_back.
            ENDWHILE.
            wa_aux-id_rota_item = pos_back.
            APPEND wa_aux TO it_zlest0107_aux.
          ENDIF.
          ADD 1 TO pos_item.
        ENDDO.

        CLEAR: it_zlest0107_alv[].

        pos_item = 1.
        DO qtd_linhas TIMES.
          READ TABLE it_zlest0107_aux INTO wa_aux WITH KEY id_rota_item = pos_item.
          APPEND wa_aux TO it_zlest0107_alv.
          ADD 1 TO pos_item.
        ENDDO.

    ENDCASE.

    LEAVE TO SCREEN 0501.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_0302 IMPLEMENTATION

DATA: ctl_alv_0501       TYPE REF TO cl_gui_alv_grid,
      obg_toolbar_0501   TYPE REF TO lcl_alv_toolbar_0501,
      ctl_con_0501       TYPE REF TO cl_gui_custom_container,
      gs_lay_0501        TYPE lvc_s_layo,
      gs_var_0501        TYPE disvariant,
      gs_scroll_col_0501 TYPE lvc_s_col,
      gs_scroll_row_0501 TYPE lvc_s_roid,
      it_catalog_0501    TYPE lvc_t_fcat,
      wa_stable_0501     TYPE lvc_s_stbl.

DATA: it_selected_0501 TYPE lvc_t_row,
      wa_selected_0501 TYPE lvc_s_row.

*&--------------------------------------------------------------------&*
*& Classes Locais                                                     &*
*&--------------------------------------------------------------------&*

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_0500 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: event_handler_0500      TYPE REF TO lcl_event_handler_0500.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_0500 IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM handle_double_click_0500 USING e_row.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


DATA: ctl_alv_0500       TYPE REF TO cl_gui_alv_grid,
      ctl_con_0500       TYPE REF TO cl_gui_custom_container,
      gs_lay_0500        TYPE lvc_s_layo,
      gs_var_0500        TYPE disvariant,
      gs_scroll_col_0500 TYPE lvc_s_col,
      gs_scroll_row_0500 TYPE lvc_s_roid,
      it_catalog_0500    TYPE lvc_t_fcat.

DATA: it_exclude_0500   TYPE ui_functions,
      wa_exclude_0500   LIKE LINE OF it_exclude_0500,
      it_exclude_05xx   TYPE ui_functions,
      wa_exclude_05xx   LIKE LINE OF it_exclude_05xx,
      it_0500           TYPE TABLE OF zde_zlest0101_alv WITH HEADER LINE,
      it_0500_sel       TYPE TABLE OF zde_zlest0101_alv WITH HEADER LINE,
      wa_0500_sel       TYPE zde_zlest0101_alv,
      lc_tx_cid_origem  TYPE text60,
      lc_tx_cid_destino	TYPE text60,
      lc_butxt          TYPE butxt,
      lc_branch_name    TYPE name1,
      it_pracas         TYPE TABLE OF zlest0102 WITH HEADER LINE,
      it_cidades        TYPE TABLE OF zlest0107 WITH HEADER LINE.

DATA: ctl_alv_0503       TYPE REF TO cl_gui_alv_grid,
      ctl_con_0503       TYPE REF TO cl_gui_custom_container,
      gs_lay_0503        TYPE lvc_s_layo,
      gs_var_0503        TYPE disvariant,
      gs_scroll_col_0503 TYPE lvc_s_col,
      gs_scroll_row_0503 TYPE lvc_s_roid,
      it_catalog_0503    TYPE lvc_t_fcat.

DATA: it_exclude_0503 TYPE ui_functions,
      wa_exclude_0503 LIKE LINE OF it_exclude_0503.

DATA: ctl_alv_0501b       TYPE REF TO cl_gui_alv_grid,
      ctl_con_0501b       TYPE REF TO cl_gui_custom_container,
      gs_lay_0501b        TYPE lvc_s_layo,
      gs_var_0501b        TYPE disvariant,
      gs_scroll_col_0501b TYPE lvc_s_col,
      gs_scroll_row_0501b TYPE lvc_s_roid,
      it_catalog_0501b    TYPE lvc_t_fcat,
      wa_stable_0501b     TYPE lvc_s_stbl.

DATA: it_selected_0501b TYPE lvc_t_row,
      wa_selected_0501b TYPE lvc_s_row.

CONSTANTS: ok_ref_rota   TYPE sy-ucomm VALUE 'REF_ROTA',
           ok_new_rota   TYPE sy-ucomm VALUE 'NEW_ROTA',
           ok_del_rota   TYPE sy-ucomm VALUE 'DEL_ROTA',
           ok_cns_rota   TYPE sy-ucomm VALUE 'CNS_ROTA',
           ok_sol_rota   TYPE sy-ucomm VALUE 'SOL_ROTA',
           ok_atu_rota   TYPE sy-ucomm VALUE 'ATU_ROTA',
           ok_def_priord TYPE sy-ucomm VALUE 'DEF_PRIORD',
           ok_cg_itinera TYPE sy-ucomm VALUE 'CG_ITINERA',
           ok_psq_pracas TYPE sy-ucomm VALUE 'PSQ_PRACAS'.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.
  DATA: q TYPE i.
  FIELD-SYMBOLS: <cat> TYPE lvc_s_fcat.

  "Somente em Debugg""""""""""""""""""""""""""""" Para Carga
  wa_exclude_05xx = ok_cg_itinera.
  APPEND wa_exclude_05xx TO it_exclude_05xx.
  """"""""""""""""""""""""""""""""""""""""""""""" Para Carga

  "05  Cadastro de Rotas - Inserir
  IF ck_autorizacao_05 EQ abap_false.
    APPEND ok_new_rota TO it_exclude_05xx.
  ENDIF.

  "06  Cadastro de Rotas - Deletar
  IF ck_autorizacao_06 EQ abap_false.
    APPEND ok_del_rota TO it_exclude_05xx.
  ENDIF.

  "07 Cadastro de Rotas - Solicitar Rota Administradora
  IF ck_autorizacao_07 EQ abap_false.
    APPEND ok_sol_rota TO it_exclude_05xx.
  ENDIF.

  "08 Cadastro de Rotas - Consutar Rota Administradora
  IF ck_autorizacao_08 EQ abap_false.
    APPEND ok_cns_rota TO it_exclude_05xx.
  ENDIF.

  "09 Cadastro de Rotas - Atualizar Rota Administradora (Carga)
  IF ck_autorizacao_09 EQ abap_false.
    APPEND ok_atu_rota TO it_exclude_05xx.
  ENDIF.

  "10 Cadastro de Rotas - Marcar Rota Prioritária
  IF ck_autorizacao_10 EQ abap_false.
    APPEND ok_def_priord TO it_exclude_05xx.
  ENDIF.

  SET PF-STATUS 'PF0500' EXCLUDING it_exclude_05xx.
  SET TITLEBAR 'TB0500'.

  IF ctl_con_0500 IS INITIAL.

    PERFORM consultar_rotas.

    CREATE OBJECT ctl_con_0500
      EXPORTING
        container_name = 'TL0500'.

    CREATE OBJECT ctl_alv_0500
      EXPORTING
        i_parent = ctl_con_0500.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZDE_ZLEST0101_ALV'
      CHANGING
        ct_fieldcat      = it_catalog_0500.

    LOOP AT it_catalog_0500 ASSIGNING <cat>.
      IF <cat>-fieldname EQ 'TX_CID_ORIGEM' OR
         <cat>-fieldname EQ 'TX_CID_DESTINO'.
        <cat>-outputlen = 30.
      ENDIF.
      IF <cat>-fieldname(7) = 'VL_EIXO'.
        q = strlen( <cat>-fieldname ).
        q = q - 7.
        CONCATENATE 'Qtd.Eixo' <cat>-fieldname+7(q) INTO <cat>-scrtext_l.
        CONCATENATE 'Qtd.Eixo' <cat>-fieldname+7(q) INTO <cat>-scrtext_m.
        CONCATENATE 'Qtd.Eixo' <cat>-fieldname+7(q) INTO <cat>-scrtext_s.
        <cat>-outputlen = 10.
      ENDIF.

*** Inicio - Rubenilson - 19.12.2024 - US160867
      IF <cat>-fieldname(10) = 'VL_TAGEIXO'.
        q = strlen( <cat>-fieldname ).
        q = q - 10.
        CONCATENATE 'Qtd.Eixo' <cat>-fieldname+10(q) INTO <cat>-scrtext_l.
        CONCATENATE 'Qtd.TagEixo' <cat>-fieldname+10(q) INTO <cat>-scrtext_m.
        CONCATENATE 'Qtd.TagEixo' <cat>-fieldname+10(q) INTO <cat>-scrtext_s.
        <cat>-outputlen = 10.
      ENDIF.
*** Fim - Rubenilson - 19.12.2024 - US160867
    ENDLOOP.

    gs_lay_0500-sel_mode   = 'A'.
    gs_lay_0500-zebra      = abap_true.

    CALL METHOD ctl_alv_0500->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0500
        is_variant           = gs_var_0500
        i_default            = space
        it_toolbar_excluding = it_exclude_0500
      CHANGING
        it_fieldcatalog      = it_catalog_0500
        it_outtab            = it_0500[].

    CALL METHOD ctl_alv_0500->refresh_table_display.

    CREATE OBJECT event_handler_0500.
    SET HANDLER event_handler_0500->handle_double_click FOR ctl_alv_0500.

  ELSE.
    CALL METHOD ctl_alv_0500->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_0500->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0500
      es_row_no   = gs_scroll_row_0500.

  "  'TL0500'
ENDMODULE.                 " STATUS_0500  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0500_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.
  CASE ok_code.
    WHEN ok_ref_rota.
      PERFORM consultar_rotas.
    WHEN ok_new_rota.
      PERFORM nova_rota.
    WHEN ok_del_rota.
      PERFORM dele_rota.
    WHEN ok_cns_rota.
      PERFORM consultar_rota_adm.
    WHEN ok_sol_rota.
      PERFORM solicitar_rota_adm.
    WHEN ok_atu_rota.
      PERFORM atualizar_rota_adm.
    WHEN ok_def_priord.
      PERFORM def_prioritaria_rota_adm.
    WHEN ok_cg_itinera.
      PERFORM carga_itinerario_pedagio.
    WHEN ok_psq_pracas.
      PERFORM visualizar_pracas.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0500  INPUT

*&---------------------------------------------------------------------*
*&      Form  CONSULTAR_ROTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM consultar_rotas .
  DATA: it_rotas      TYPE TABLE OF zlest0101 WITH HEADER LINE,
        it_cidades    TYPE TABLE OF j_1btxjurt WITH HEADER LINE,
        it_t001       TYPE TABLE OF t001 WITH HEADER LINE,
        it_j_1bbranch TYPE TABLE OF j_1bbranch WITH HEADER LINE,
        wa_0500       TYPE zde_zlest0101_alv.

  CLEAR: it_0500[].

  SELECT * INTO TABLE it_rotas
    FROM zlest0101.

  IF it_rotas[] IS NOT INITIAL.

    SELECT * INTO TABLE it_cidades
      FROM j_1btxjurt
       FOR ALL ENTRIES IN it_rotas
     WHERE spras      EQ sy-langu
       AND country    EQ it_rotas-country
       AND taxjurcode EQ it_rotas-cd_cid_origem.

    SELECT * APPENDING TABLE it_cidades
      FROM j_1btxjurt
       FOR ALL ENTRIES IN it_rotas
     WHERE spras      EQ sy-langu
       AND country    EQ it_rotas-country
       AND taxjurcode EQ it_rotas-cd_cid_destino.

    SELECT * INTO TABLE it_t001
      FROM t001
       FOR ALL ENTRIES IN it_rotas
     WHERE bukrs EQ it_rotas-bukrs.

    SELECT * INTO TABLE it_j_1bbranch
      FROM j_1bbranch
     FOR ALL ENTRIES IN it_rotas
     WHERE bukrs  EQ it_rotas-bukrs
       AND branch EQ it_rotas-branch.
  ENDIF.

  SORT it_cidades BY country taxjurcode.
  SORT it_t001 BY bukrs.
  SORT it_j_1bbranch BY bukrs branch.

  LOOP AT it_rotas.
    CLEAR: wa_0500.
    MOVE-CORRESPONDING it_rotas TO wa_0500.

    READ TABLE it_t001
    WITH KEY bukrs = it_rotas-bukrs BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_0500-butxt = it_t001-butxt.
    ENDIF.

    READ TABLE it_j_1bbranch
    WITH KEY bukrs = it_rotas-bukrs
         branch = it_rotas-branch BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_0500-branch_name = it_j_1bbranch-name.
    ENDIF.

    READ TABLE it_cidades
    WITH KEY country    = it_rotas-country
             taxjurcode = it_rotas-cd_cid_origem BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_0500-tx_cid_origem = it_cidades-text.
    ENDIF.

    READ TABLE it_cidades
    WITH KEY country    = it_rotas-country
             taxjurcode = it_rotas-cd_cid_destino BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_0500-tx_cid_destino = it_cidades-text.
    ENDIF.

    APPEND wa_0500 TO it_0500.
  ENDLOOP.

ENDFORM.                    " CONSULTAR_ROTAS


*&---------------------------------------------------------------------*
*&      Form  NOVA_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nova_rota .

  CLEAR: zlest0101, it_zlest0107_alv[].
  zlest0101-country = 'BR'.

  CALL SCREEN 0501 STARTING AT 15 01.
  PERFORM consultar_rotas.

ENDFORM.                    " NOVA_ROTA

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0501_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0501_exit INPUT.
  CLEAR: zlest0101.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0501_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0501  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0501 INPUT.
  CASE ok_code.
    WHEN 'SALVAR'.
      PERFORM salvar_rota.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0501  INPUT

*&---------------------------------------------------------------------*
*&      Form  SALVAR_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_rota .

  DATA: wa_zlest0101 TYPE zlest0101,
        wa_zlest0107 TYPE zlest0107,
        cd_id_rota   TYPE zde_id_rota.

  CHECK lc_butxt IS NOT INITIAL.
  CHECK lc_branch_name IS NOT INITIAL.
  CHECK lc_tx_cid_origem IS NOT INITIAL.
  CHECK lc_tx_cid_destino IS NOT INITIAL.

  SELECT SINGLE * INTO wa_zlest0101
    FROM zlest0101
   WHERE bukrs          EQ zlest0101-bukrs
     AND branch         EQ zlest0101-branch
     AND country        EQ zlest0101-country
     AND cd_cid_origem  EQ zlest0101-cd_cid_origem
     AND cd_cid_destino EQ zlest0101-cd_cid_destino
     AND tp_rota_perc   EQ zlest0101-tp_rota_perc.

  IF sy-subrc IS INITIAL.
    MESSAGE i083 WITH zlest0101-cd_cid_origem zlest0101-cd_cid_destino zlest0101-tp_rota_perc.
    EXIT.
  ENDIF.

*  SELECT SINGLE * INTO WA_ZLEST0101
*    FROM ZLEST0101
*   WHERE BUKRS          EQ ZLEST0101-BUKRS
*     AND BRANCH         EQ ZLEST0101-BRANCH
*     AND COUNTRY        EQ ZLEST0101-COUNTRY
*     AND CD_CID_ORIGEM  EQ ZLEST0101-CD_CID_DESTINO
*     AND CD_CID_DESTINO EQ ZLEST0101-CD_CID_ORIGEM
*     AND TP_ROTA_PERC   EQ ZLEST0101-TP_ROTA_PERC.
*
*  IF SY-SUBRC IS INITIAL.
*    MESSAGE I083 WITH ZLEST0101-CD_CID_DESTINO ZLEST0101-CD_CID_ORIGEM ZLEST0101-TP_ROTA_PERC.
*    EXIT.
*  ENDIF.

  SELECT MAX( id_rota ) INTO cd_id_rota
    FROM zlest0101.

  IF cd_id_rota IS INITIAL OR cd_id_rota EQ 0.
    MOVE 1 TO cd_id_rota.
  ELSE.
    ADD 1 TO cd_id_rota.
  ENDIF.

  zlest0101-id_rota = cd_id_rota.
  MODIFY zlest0101.

  LOOP AT it_zlest0107_alv.
    MOVE-CORRESPONDING it_zlest0107_alv TO wa_zlest0107.
    wa_zlest0107-id_rota = cd_id_rota.
    MODIFY zlest0107 FROM wa_zlest0107.
  ENDLOOP.
  COMMIT WORK.

  MESSAGE s084 WITH zlest0101-cd_cid_destino zlest0101-cd_cid_origem zlest0101-tp_rota_perc.

  LEAVE TO SCREEN 0.
ENDFORM.                    " SALVAR_ROTA

*&---------------------------------------------------------------------*
*&      Module  LIMPAR_CIDADES  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE limpar_cidades INPUT.
  PERFORM limpar_cidades.
ENDMODULE.                 " LIMPAR_CIDADES  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0501  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0501 OUTPUT.

  DATA: fs_sort_0501 TYPE lvc_s_sort,
        gt_sort_0501 TYPE lvc_t_sort.

  SET PF-STATUS 'PF0501'.
  SET TITLEBAR 'TB0501'.

  PERFORM limpar_cidades.

  IF zlest0101-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt INTO lc_butxt
      FROM t001
     WHERE bukrs EQ zlest0101-bukrs.

    IF zlest0101-branch IS NOT INITIAL.
      SELECT SINGLE name INTO lc_branch_name
        FROM j_1bbranch
       WHERE bukrs  EQ zlest0101-bukrs
         AND branch EQ zlest0101-branch.
    ENDIF.
  ENDIF.

  IF zlest0101-country IS NOT INITIAL AND zlest0101-cd_cid_origem IS NOT INITIAL.
    SELECT SINGLE text INTO lc_tx_cid_origem
      FROM j_1btxjurt
     WHERE spras      EQ sy-langu
       AND country    EQ zlest0101-country
       AND taxjurcode EQ zlest0101-cd_cid_origem.
  ENDIF.

  IF zlest0101-country IS NOT INITIAL AND zlest0101-cd_cid_destino IS NOT INITIAL.
    SELECT SINGLE text INTO lc_tx_cid_destino
      FROM j_1btxjurt
     WHERE spras      EQ sy-langu
       AND country    EQ zlest0101-country
       AND taxjurcode EQ zlest0101-cd_cid_destino.
  ENDIF.

  IF ctl_con_0501 IS INITIAL.

    CREATE OBJECT ctl_con_0501
      EXPORTING
        container_name = 'ALV_CIDADES'.

    CREATE OBJECT ctl_alv_0501
      EXPORTING
        i_parent = ctl_con_0501.

    PERFORM fill_it_fieldcatalog_0501.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0501.
*   Set layout parameters for ALV grid

    CREATE OBJECT obg_toolbar_0501
      EXPORTING
        io_alv_grid = ctl_alv_0501.

    SET HANDLER obg_toolbar_0501->on_toolbar FOR ctl_alv_0501.
    SET HANDLER obg_toolbar_0501->handle_user_command FOR ctl_alv_0501.

    CLEAR: gs_lay_0501.
    gs_lay_0501-zebra = 'X'.

    CALL METHOD ctl_alv_0501->set_table_for_first_display
      EXPORTING
        is_layout       = gs_lay_0501
        is_variant      = gs_var_0501
      CHANGING
        it_fieldcatalog = it_catalog_0501
        it_outtab       = it_zlest0107_alv[]
        it_sort         = gt_sort_0501[].

    CALL METHOD ctl_alv_0501->refresh_table_display.

  ELSE.
    wa_stable_0501-row = abap_true.
    wa_stable_0501-col = abap_true.

    CALL METHOD ctl_alv_0501->refresh_table_display
      EXPORTING
        is_stable = wa_stable_0501.
  ENDIF.

  CALL METHOD ctl_alv_0501->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0501
      es_row_no   = gs_scroll_row_0501.

ENDMODULE.                 " STATUS_0501  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_CIDADES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_cidades .
  CLEAR: lc_butxt,
         lc_branch_name,
         lc_tx_cid_origem,
         lc_tx_cid_destino.
ENDFORM.                    " LIMPAR_CIDADES

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info INPUT.

  CALL METHOD ctl_alv_0500->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0500
      es_row_no   = gs_scroll_row_0500.
ENDMODULE.                 " GET_SCROLL_INFO  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows INPUT.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CLEAR it_selected_rows.

  CALL METHOD ctl_alv_0500->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  CLEAR it_0500_sel[].

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_0500 INDEX wa_selected_rows-index.
    MOVE-CORRESPONDING it_0500 TO it_0500_sel.
    APPEND it_0500_sel.
  ENDLOOP.

ENDMODULE.                 " GET_SELECTED_ROWS  INPUT

*&---------------------------------------------------------------------*
*&      Form  DELE_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dele_rota .

  IF it_0500_sel[] IS INITIAL.
    MESSAGE s085.
    RETURN.
  ENDIF.

  READ TABLE it_0500_sel INDEX 1.

  IF it_0500_sel-id_rota_adm IS NOT INITIAL.
    MESSAGE s086.
    RETURN.
  ELSE.
    DELETE FROM zlest0107 WHERE id_rota EQ it_0500_sel-id_rota.
    DELETE FROM zlest0101 WHERE id_rota EQ it_0500_sel-id_rota.
    COMMIT WORK.
  ENDIF.

  PERFORM consultar_rotas.

ENDFORM.                    " DELE_ROTA

*&---------------------------------------------------------------------*
*&      Form  SOLICITAR_ROTA_ADM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM solicitar_rota_adm .

  DATA: tipcard       TYPE REF TO zcl_webservice_tipcard,
        e_msg	        TYPE char255,
        e_id_rota_adm	TYPE zde_id_rota_adm.

  IF it_0500_sel[] IS INITIAL.
    MESSAGE s085.
    RETURN.
  ENDIF.

  CREATE OBJECT tipcard.

  LOOP AT it_0500_sel.

    CALL METHOD tipcard->solicita_rota
      EXPORTING
        i_rota        = it_0500_sel-id_rota
        i_bukrs       = it_0500_sel-bukrs
        i_branch      = it_0500_sel-branch
      IMPORTING
        e_msg         = e_msg
        e_id_rota_adm = e_id_rota_adm
      EXCEPTIONS
        zwebservice   = 1
        OTHERS        = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE s089 WITH it_0500_sel-id_rota e_id_rota_adm.
    ENDIF.

  ENDLOOP.

  PERFORM consultar_rotas.

ENDFORM.                    " SOLICITAR_ROTA_ADM

*&---------------------------------------------------------------------*
*&      Form  CONSULTAR_ROTA_ADM
*&---------------------------------------------------------------------*
FORM consultar_rota_adm .

  DATA: tipcard  TYPE REF TO zcl_webservice_tipcard,
        e_msg	   TYPE char255,
        i_xml    TYPE string,
        e_rotas	 TYPE	zlest0101_t,
        e_pracas TYPE zlest0102_t.

  IF it_0500_sel[] IS INITIAL.
    MESSAGE s085.
    RETURN.
  ENDIF.

  CREATE OBJECT tipcard.

  LOOP AT it_0500_sel.

    IF it_0500_sel-id_rota_adm IS INITIAL.
      MESSAGE w091.
      CONTINUE.
    ENDIF.

    CALL METHOD tipcard->consultar_rota
      EXPORTING
        i_rota      = it_0500_sel-id_rota
        i_bukrs     = it_0500_sel-bukrs
        i_branch    = it_0500_sel-branch
      IMPORTING
        e_msg       = e_msg
      EXCEPTIONS
        zwebservice = 1
        OTHERS      = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE e_msg TYPE 'S'.
    ENDIF.

  ENDLOOP.

  PERFORM consultar_rotas.

ENDFORM.                    " CONSULTAR_ROTA_ADM

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_ROTA_ADM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualizar_rota_adm .

  DATA: tipcard TYPE REF TO zcl_webservice_tipcard,
        e_msg	  TYPE char255,
        i_xml   TYPE string.

  DATA: it_zlest0101 TYPE TABLE OF zlest0101 WITH HEADER LINE.

  CREATE OBJECT tipcard.

*  CONCATENATE I_XML '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' INTO I_XML.
*  CONCATENATE I_XML '<retornoAtualizaRota>' INTO I_XML.
*  CONCATENATE I_XML '    <msg>Sucesso</msg>' INTO I_XML.
*  CONCATENATE I_XML '    <rotas>' INTO I_XML.
*  CONCATENATE I_XML '        <rota>' INTO I_XML.
*  CONCATENATE I_XML '            <cnpjContratante>77294254001670</cnpjContratante>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoRota>523</codigoRota>' INTO I_XML.
*  CONCATENATE I_XML '           <codigoCidadeOrigem>5107602</codigoCidadeOrigem>' INTO I_XML.
*  CONCATENATE I_XML '           <codigoCidadeDestino>5106372</codigoCidadeDestino>' INTO I_XML.
*  CONCATENATE I_XML '           <retornoOrigem>0</retornoOrigem>' INTO I_XML.
*  CONCATENATE I_XML '       </rota>' INTO I_XML.
*  CONCATENATE I_XML '       <rota>' INTO I_XML.
*  CONCATENATE I_XML '           <cnpjContratante>77294254001670</cnpjContratante>' INTO I_XML.
*  CONCATENATE I_XML '           <codigoRota>1002</codigoRota>' INTO I_XML.
*  CONCATENATE I_XML '           <codigoCidadeOrigem>2915353</codigoCidadeOrigem>' INTO I_XML.
*  CONCATENATE I_XML '           <codigoCidadeDestino>3550308</codigoCidadeDestino>' INTO I_XML.
*  CONCATENATE I_XML '           <retornoOrigem>0</retornoOrigem>' INTO I_XML.
*  CONCATENATE I_XML '           <pracasPedagio>' INTO I_XML.
*  CONCATENATE I_XML '               <praca>' INTO I_XML.
*  CONCATENATE I_XML '                   <codigoPraca>886</codigoPraca>' INTO I_XML.
*  CONCATENATE I_XML '                   <nomePraca>Vitória da Conquista</nomePraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo1>340</eixo1>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo2>680</eixo2>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo3>1020</eixo3>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo4>1360</eixo4>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo5>1700</eixo5>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo6>2040</eixo6>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo7>2380</eixo7>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo8>2720</eixo8>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo9>3060</eixo9>' INTO I_XML.
*  CONCATENATE I_XML '                </praca>' INTO I_XML.
*  CONCATENATE I_XML '                <praca>' INTO I_XML.
*  CONCATENATE I_XML '                    <codigoPraca>772</codigoPraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <nomePraca>Itatiaiaçú</nomePraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo1>160</eixo1>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo2>320</eixo2>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo3>480</eixo3>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo4>640</eixo4>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo5>800</eixo5>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo6>960</eixo6>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo7>1120</eixo7>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo8>1280</eixo8>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo9>1440</eixo9>' INTO I_XML.
*  CONCATENATE I_XML '                </praca>' INTO I_XML.
*  CONCATENATE I_XML '                <praca>' INTO I_XML.
*  CONCATENATE I_XML '                    <codigoPraca>771</codigoPraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <nomePraca>Carmópolis de Minas</nomePraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo1>160</eixo1>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo2>320</eixo2>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo3>480</eixo3>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo4>640</eixo4>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo5>800</eixo5>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo6>960</eixo6>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo7>1120</eixo7>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo8>1280</eixo8>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo9>1440</eixo9>' INTO I_XML.
*  CONCATENATE I_XML '                </praca>' INTO I_XML.
*  CONCATENATE I_XML '                <praca>' INTO I_XML.
*  CONCATENATE I_XML '                    <codigoPraca>770</codigoPraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <nomePraca>Santo Antonio do Amparo</nomePraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo1>160</eixo1>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo2>320</eixo2>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo3>480</eixo3>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo4>640</eixo4>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo5>800</eixo5>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo6>960</eixo6>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo7>1120</eixo7>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo8>1280</eixo8>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo9>1440</eixo9>' INTO I_XML.
*  CONCATENATE I_XML '                </praca>' INTO I_XML.
*  CONCATENATE I_XML '                <praca>' INTO I_XML.
*  CONCATENATE I_XML '                    <codigoPraca>769</codigoPraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <nomePraca>Carmo da Cachoeira</nomePraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo1>160</eixo1>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo2>320</eixo2>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo3>480</eixo3>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo4>640</eixo4>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo5>800</eixo5>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo6>960</eixo6>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo7>1120</eixo7>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo8>1280</eixo8>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo9>1440</eixo9>' INTO I_XML.
*  CONCATENATE I_XML '                </praca>' INTO I_XML.
*  CONCATENATE I_XML '                <praca>' INTO I_XML.
*  CONCATENATE I_XML '                    <codigoPraca>768</codigoPraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <nomePraca>São Gonçalo do Sapucaí</nomePraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo1>160</eixo1>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo2>320</eixo2>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo3>480</eixo3>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo4>640</eixo4>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo5>800</eixo5>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo6>960</eixo6>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo7>1120</eixo7>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo8>1280</eixo8>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo9>1440</eixo9>' INTO I_XML.
*  CONCATENATE I_XML '                </praca>' INTO I_XML.
*  CONCATENATE I_XML '                <praca>' INTO I_XML.
*  CONCATENATE I_XML '                    <codigoPraca>767</codigoPraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <nomePraca>Cambuí</nomePraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo1>160</eixo1>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo2>320</eixo2>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo3>480</eixo3>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo4>640</eixo4>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo5>800</eixo5>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo6>960</eixo6>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo7>1120</eixo7>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo8>1280</eixo8>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo9>1440</eixo9>' INTO I_XML.
*  CONCATENATE I_XML '                </praca>' INTO I_XML.
*  CONCATENATE I_XML '                <praca>' INTO I_XML.
*  CONCATENATE I_XML '                    <codigoPraca>766</codigoPraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <nomePraca>Vargem</nomePraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo1>160</eixo1>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo2>320</eixo2>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo3>480</eixo3>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo4>640</eixo4>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo5>800</eixo5>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo6>960</eixo6>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo7>1120</eixo7>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo8>1280</eixo8>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo9>1440</eixo9>' INTO I_XML.
*  CONCATENATE I_XML '                </praca>' INTO I_XML.
*  CONCATENATE I_XML '                <praca>' INTO I_XML.
*  CONCATENATE I_XML '                    <codigoPraca>765</codigoPraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <nomePraca>Mairiporã</nomePraca>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo1>160</eixo1>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo2>320</eixo2>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo3>480</eixo3>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo4>640</eixo4>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo5>800</eixo5>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo6>960</eixo6>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo7>1120</eixo7>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo8>1280</eixo8>' INTO I_XML.
*  CONCATENATE I_XML '                    <eixo9>1440</eixo9>' INTO I_XML.
*  CONCATENATE I_XML '                </praca>' INTO I_XML.
*  CONCATENATE I_XML '            </pracasPedagio>' INTO I_XML.
*  CONCATENATE I_XML '        </rota>' INTO I_XML.
*  CONCATENATE I_XML '        <rota>' INTO I_XML.
*  CONCATENATE I_XML '            <cnpjContratante>77294254001670</cnpjContratante>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoRota>1003</codigoRota>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoCidadeDestino>5106372</codigoCidadeDestino>' INTO I_XML.
*  CONCATENATE I_XML '            <retornoOrigem>0</retornoOrigem>' INTO I_XML.
*  CONCATENATE I_XML '        </rota>' INTO I_XML.
*  CONCATENATE I_XML '        <rota>' INTO I_XML.
*  CONCATENATE I_XML '            <cnpjContratante>77294254001670</cnpjContratante>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoRota>1004</codigoRota>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoCidadeDestino>5106372</codigoCidadeDestino>' INTO I_XML.
*  CONCATENATE I_XML '            <retornoOrigem>0</retornoOrigem>' INTO I_XML.
*  CONCATENATE I_XML '        </rota>' INTO I_XML.
*  CONCATENATE I_XML '        <rota>' INTO I_XML.
*  CONCATENATE I_XML '            <cnpjContratante>77294254001670</cnpjContratante>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoRota>1005</codigoRota>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoCidadeDestino>5106372</codigoCidadeDestino>' INTO I_XML.
*  CONCATENATE I_XML '            <retornoOrigem>0</retornoOrigem>' INTO I_XML.
*  CONCATENATE I_XML '        </rota>' INTO I_XML.
*  CONCATENATE I_XML '        <rota>' INTO I_XML.
*  CONCATENATE I_XML '            <cnpjContratante>77294254001670</cnpjContratante>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoRota>1006</codigoRota>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>' INTO I_XML.
*  CONCATENATE I_XML '            <codigoCidadeDestino>5106372</codigoCidadeDestino>' INTO I_XML.
*  CONCATENATE I_XML '            <retornoOrigem>0</retornoOrigem>' INTO I_XML.
*  CONCATENATE I_XML '        </rota>' INTO I_XML.
*  CONCATENATE I_XML '    </rotas>' INTO I_XML.
*  CONCATENATE I_XML '</retornoAtualizaRota>' INTO I_XML.

  IF sy-batch EQ abap_true.
    SELECT * INTO TABLE it_zlest0101
      FROM zlest0101.

    CHECK it_zlest0101[] IS NOT INITIAL.
    SORT it_zlest0101 BY bukrs branch.
    DELETE ADJACENT DUPLICATES FROM it_zlest0101 COMPARING bukrs branch.

    LOOP AT it_zlest0101.
      CALL METHOD tipcard->atualizar_rota
        EXPORTING
          i_bukrs     = it_zlest0101-bukrs
          i_branch    = it_zlest0101-branch
        IMPORTING
          e_msg       = e_msg
        EXCEPTIONS
          zwebservice = 1
          OTHERS      = 2.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE e_msg TYPE 'S'.
      ENDIF.
    ENDLOOP.

  ELSE.
    CLEAR: zlest0101.
    CALL SCREEN 0502 STARTING AT 15 01.

    IF zlest0101-bukrs IS NOT INITIAL AND zlest0101-branch IS NOT INITIAL.
      CALL METHOD tipcard->atualizar_rota
        EXPORTING
          i_bukrs     = zlest0101-bukrs
          i_branch    = zlest0101-branch
        IMPORTING
          e_msg       = e_msg
        EXCEPTIONS
          zwebservice = 1
          OTHERS      = 2.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE e_msg TYPE 'S'.
      ENDIF.
    ENDIF.

  ENDIF.

*  CALL METHOD TIPCARD->LER_XML_ATUALIZAR_ROTA
*    EXPORTING
*      I_XML    = I_XML
*    IMPORTING
*      E_MSG    = E_MSG.
*
*  IF SY-SUBRC IS NOT INITIAL.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ELSE.
*    MESSAGE E_MSG TYPE 'S'.
*  ENDIF.

  PERFORM consultar_rotas.

ENDFORM.                    " ATUALIZAR_ROTA_ADM

*&---------------------------------------------------------------------*
*&      Module  STATUS_0502  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0502 OUTPUT.

  SET PF-STATUS 'PF0502'.
  SET TITLEBAR 'TB0502'.

  IF zlest0101-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt INTO lc_butxt
      FROM t001
     WHERE bukrs EQ zlest0101-bukrs.

    IF zlest0101-branch IS NOT INITIAL.
      SELECT SINGLE name INTO lc_branch_name
        FROM j_1bbranch
       WHERE bukrs  EQ zlest0101-bukrs
         AND branch EQ zlest0101-branch.
    ENDIF.
  ENDIF.

ENDMODULE.                 " STATUS_0502  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0502  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0502 INPUT.

  CASE ok_code.
    WHEN 'ATUALIZAR'.
      CHECK lc_butxt IS NOT INITIAL.
      CHECK lc_branch_name IS NOT INITIAL.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0502  INPUT

*&---------------------------------------------------------------------*
*&      Form  DEF_PRIORITARIA_ROTA_ADM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM def_prioritaria_rota_adm .

  IF it_0500_sel[] IS INITIAL.
    MESSAGE s085.
    RETURN.
  ENDIF.

  READ TABLE it_0500_sel INDEX 1.

  IF it_0500_sel-id_rota_adm IS INITIAL.
    MESSAGE s091.
    RETURN.
  ELSE.
    UPDATE zlest0101 SET prioridade = 'X'
     WHERE id_rota EQ it_0500_sel-id_rota.

    UPDATE zlest0101 SET prioridade = ' '
     WHERE id_rota        NE it_0500_sel-id_rota
       AND bukrs          EQ it_0500_sel-bukrs
       AND branch         EQ it_0500_sel-branch
       AND country        EQ it_0500_sel-country
       AND cd_cid_origem  EQ it_0500_sel-cd_cid_origem
       AND cd_cid_destino EQ it_0500_sel-cd_cid_destino
       AND tp_rota_perc   EQ it_0500_sel-tp_rota_perc.

    UPDATE zlest0084 SET prioridade = 'X'
     WHERE id_rota EQ it_0500_sel-id_rota_adm.

    UPDATE zlest0084 SET prioridade = ' '
     WHERE id_rota        NE it_0500_sel-id_rota_adm
       AND bukrs          EQ it_0500_sel-bukrs
       AND branch         EQ it_0500_sel-branch
       AND munic_origem   EQ it_0500_sel-cd_cid_origem+3(7)
       AND munic_destino  EQ it_0500_sel-cd_cid_destino+3(7).

    COMMIT WORK.
    MESSAGE s090 WITH it_0500_sel-id_rota.
  ENDIF.

  PERFORM consultar_rotas.

ENDFORM.                    " DEF_PRIORITARIA_ROTA_ADM

*&---------------------------------------------------------------------*
*&      Form  CARGA_ITINERARIO_PEDAGIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carga_itinerario_pedagio .

  DATA: it_zlest0027 TYPE TABLE OF zlest0027 WITH HEADER LINE,
        it_tvro      TYPE TABLE OF tvro WITH HEADER LINE.

  FIELD-SYMBOLS: <f0027> TYPE zlest0027.

  SELECT * INTO TABLE it_zlest0027
    FROM zlest0027
   WHERE ck_pedagio EQ abap_true.

  CHECK it_zlest0027[] IS INITIAL.

  SELECT * INTO TABLE it_zlest0027
    FROM zlest0027.

  LOOP AT it_zlest0027 ASSIGNING <f0027>.
    <f0027>-ck_pedagio = abap_true.
  ENDLOOP.

  SELECT * INTO TABLE it_tvro
    FROM tvro AS o
   WHERE NOT EXISTS ( SELECT * FROM zlest0027 AS z WHERE z~route EQ o~route ).
  "ZLEST0027

  LOOP AT it_tvro.
    it_zlest0027-route      = it_tvro-route.
    it_zlest0027-ck_pedagio = abap_false.
    APPEND it_zlest0027.
  ENDLOOP.

  MODIFY zlest0027 FROM TABLE it_zlest0027.
  COMMIT WORK.

ENDFORM.                    " CARGA_ITINERARIO_PEDAGIO

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK_0500
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM handle_double_click_0500  USING p_row TYPE lvc_s_row.

  DATA: lc_row TYPE lvc_t_row.

  IF p_row-rowtype IS INITIAL.

    APPEND p_row TO lc_row.

    CALL METHOD ctl_alv_0500->set_selected_rows
      EXPORTING
        it_index_rows = lc_row.

    READ TABLE it_0500 INDEX p_row-index INTO wa_0500_sel.

    PERFORM mostra_pracas_rota USING wa_0500_sel-id_rota wa_0500_sel-bukrs wa_0500_sel-branch.

  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK_0500

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_PRACAS_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mostra_pracas_rota  USING  p_id_rota TYPE zde_id_rota
                                p_empresa TYPE bukrs
                                p_filial  TYPE j_1bbranc_.

  CLEAR: zlest0101, it_zlest0107_alv[].

  SELECT SINGLE * INTO zlest0101
    FROM zlest0101
   WHERE id_rota EQ p_id_rota
     AND bukrs   EQ p_empresa
     AND branch  EQ p_filial.

  SELECT * INTO TABLE it_pracas
    FROM zlest0102
   WHERE id_rota EQ p_id_rota
     AND bukrs   EQ p_empresa
     AND branch  EQ p_filial
     AND st_praca EQ 'X'.

  SELECT * INTO TABLE it_cidades
    FROM zlest0107
   WHERE id_rota EQ p_id_rota
     AND bukrs   EQ p_empresa
     AND branch  EQ p_filial.

  LOOP AT it_cidades.
    MOVE-CORRESPONDING it_cidades TO it_zlest0107_alv.
    SELECT SINGLE text INTO it_zlest0107_alv-text
      FROM j_1btxjurt
     WHERE spras      EQ sy-langu
       AND country    EQ it_zlest0107_alv-country
       AND taxjurcode EQ it_zlest0107_alv-cd_cidade.

    APPEND it_zlest0107_alv.
  ENDLOOP.

  IF zlest0101 IS NOT INITIAL.
    CALL SCREEN 0503 STARTING AT 10 10.
  ELSE.
    MESSAGE s093.
  ENDIF.

ENDFORM.                    " MOSTRA_PRACAS_ROTA

*&---------------------------------------------------------------------*
*&      Module  STATUS_0503  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0503 OUTPUT.

  SET PF-STATUS 'PF0503'.
  SET TITLEBAR 'TB0503'.

  PERFORM limpar_cidades.

  IF zlest0101-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt INTO lc_butxt
      FROM t001
     WHERE bukrs EQ zlest0101-bukrs.

    IF zlest0101-branch IS NOT INITIAL.
      SELECT SINGLE name INTO lc_branch_name
        FROM j_1bbranch
       WHERE bukrs  EQ zlest0101-bukrs
         AND branch EQ zlest0101-branch.
    ENDIF.
  ENDIF.

  IF zlest0101-country IS NOT INITIAL AND zlest0101-cd_cid_origem IS NOT INITIAL.
    SELECT SINGLE text INTO lc_tx_cid_origem
      FROM j_1btxjurt
     WHERE spras      EQ sy-langu
       AND country    EQ zlest0101-country
       AND taxjurcode EQ zlest0101-cd_cid_origem.
  ENDIF.

  IF zlest0101-country IS NOT INITIAL AND zlest0101-cd_cid_destino IS NOT INITIAL.
    SELECT SINGLE text INTO lc_tx_cid_destino
      FROM j_1btxjurt
     WHERE spras      EQ sy-langu
       AND country    EQ zlest0101-country
       AND taxjurcode EQ zlest0101-cd_cid_destino.
  ENDIF.

  IF ctl_con_0503 IS INITIAL.

    CREATE OBJECT ctl_con_0503
      EXPORTING
        container_name = 'ALV_0503'.

    CREATE OBJECT ctl_alv_0503
      EXPORTING
        i_parent = ctl_con_0503.

    PERFORM fill_it_fieldcatalog_0503.

    PERFORM fill_gs_variant_0503.
    gs_lay_0503-sel_mode   = space.
    gs_lay_0503-zebra      = abap_true.
    gs_lay_0503-grid_title = 'Praças de Pedágio'.
    gs_lay_0503-no_toolbar = 'X'.

    CALL METHOD ctl_alv_0503->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0503
        is_variant           = gs_var_0503
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_0503
      CHANGING
        it_fieldcatalog      = it_catalog_0503
        it_outtab            = it_pracas[].

    CALL METHOD ctl_alv_0503->refresh_table_display.
  ELSE.
    CALL METHOD ctl_alv_0503->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_0503->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0503
      es_row_no   = gs_scroll_row_0503.

  IF ctl_con_0501b IS INITIAL.

    CREATE OBJECT ctl_con_0501b
      EXPORTING
        container_name = 'ALV_CIDADESB'.

    CREATE OBJECT ctl_alv_0501b
      EXPORTING
        i_parent = ctl_con_0501b.

    PERFORM fill_it_fieldcatalog_0501b.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0501b.
*   Set layout parameters for ALV grid

    CLEAR: gs_lay_0501b.
    gs_lay_0501b-zebra      = 'X'.
    gs_lay_0501b-no_toolbar = 'X'.
    gs_lay_0501b-grid_title = 'Cidades do Percurso'.

    CALL METHOD ctl_alv_0501b->set_table_for_first_display
      EXPORTING
        is_layout       = gs_lay_0501b
        is_variant      = gs_var_0501b
      CHANGING
        it_fieldcatalog = it_catalog_0501b
        it_outtab       = it_zlest0107_alv[].

    CALL METHOD ctl_alv_0501b->refresh_table_display.

  ELSE.
    wa_stable_0501b-row = abap_true.
    wa_stable_0501b-col = abap_true.

    CALL METHOD ctl_alv_0501b->refresh_table_display
      EXPORTING
        is_stable = wa_stable_0501b.
  ENDIF.

  CALL METHOD ctl_alv_0501b->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0501b
      es_row_no   = gs_scroll_row_0501b.

ENDMODULE.                 " STATUS_0503  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0503  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0503 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0503  INPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0503
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0503 .

  gs_var_0503-report      = sy-repid.
  gs_var_0503-handle      = '0503'.
  gs_var_0503-log_group   = abap_false.
  gs_var_0503-username    = abap_false.
  gs_var_0503-variant     = abap_false.
  gs_var_0503-text        = abap_false.
  gs_var_0503-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_0503

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0503
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0503 .

  FIELD-SYMBOLS: <fs_0503> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZLEST0102'
    CHANGING
      ct_fieldcat      = it_catalog_0503.

  LOOP AT it_catalog_0503 ASSIGNING <fs_0503>.

    CASE <fs_0503>-fieldname(7).
      WHEN 'VL_EIXO'.
        q = strlen( <fs_0503>-fieldname ).
        q = q - 7.
        CONCATENATE 'Qtd.Eixo' <fs_0503>-fieldname+7(q) INTO <fs_0503>-scrtext_l.
        CONCATENATE 'Qtd.Eixo' <fs_0503>-fieldname+7(q) INTO <fs_0503>-scrtext_m.
        CONCATENATE 'Qtd.Eixo' <fs_0503>-fieldname+7(q) INTO <fs_0503>-scrtext_s.
        CONCATENATE 'Qtd.Eixo' <fs_0503>-fieldname+7(q) INTO <fs_0503>-coltext.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0503

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_PRACAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM visualizar_pracas .

  IF it_0500_sel[] IS INITIAL.
    MESSAGE s085.
    RETURN.
  ENDIF.

  READ TABLE it_0500_sel INDEX 1.

  PERFORM mostra_pracas_rota USING it_0500_sel-id_rota it_0500_sel-bukrs it_0500_sel-branch.

ENDFORM.                    " VISUALIZAR_PRACAS

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0501
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0501 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0501> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZLEST0107'
    CHANGING
      ct_fieldcat      = it_catalog_0501.

  lc_col_pos = 1.

  DELETE it_catalog_0501 WHERE fieldname EQ 'MANDT'.
  DELETE it_catalog_0501 WHERE fieldname EQ 'ID_ROTA'.

  LOOP AT it_catalog_0501 ASSIGNING <fs_cat_0501>.
    <fs_cat_0501>-col_pos = lc_col_pos.
    <fs_cat_0501>-tabname   = 'ZDE_ZLEST0107'.
    <fs_cat_0501>-fieldname = <fs_cat_0501>-fieldname.
    ADD 1 TO lc_col_pos.
    CASE <fs_cat_0501>-fieldname.
      WHEN 'TEXT'.
        <fs_cat_0501>-outputlen = 40.
*      WHEN 'SGTXT'.
*        <FS_CAT_0304>-EDIT      = ABAP_TRUE.
*        <FS_CAT_0304>-OUTPUTLEN = 30.
*      WHEN 'DESCRICAO'.
*        <FS_CAT_0304>-OUTPUTLEN = 30.
*      WHEN 'FCURR' OR 'TCURR' OR 'SAKNR' OR 'UKURS' OR 'UKURS_VARIACAO'.
*        <FS_CAT_0304>-EDIT      = ABAP_FALSE.
*      WHEN 'BUKRS' OR 'KOART' OR 'PARID' OR 'AKONT' OR 'UMSKZ' OR 'SKONT' OR 'TXT50' OR 'TXT50_VARIACAO' OR 'DMBE2_VARIACAOC'.
*        <FS_CAT_0304>-EDIT      = ABAP_FALSE.
*        <FS_CAT_0304>-NO_OUT    = ABAP_TRUE.
*      WHEN 'BSCHL' .
*        <FS_CAT_0304>-EDIT      = ABAP_FALSE.
*        <FS_CAT_0304>-NO_OUT    = ABAP_TRUE.
*        <FS_CAT_0304>-SCRTEXT_L = 'Chave Outras'.
*      WHEN 'BSCHL_BANCO'.
*        <FS_CAT_0304>-EDIT      = ABAP_FALSE.
*        <FS_CAT_0304>-NO_OUT    = ABAP_TRUE.
*        <FS_CAT_0304>-SCRTEXT_L = 'Chave Banco'.
*      WHEN 'BSCHL_VARIACAO'.
*        <FS_CAT_0304>-EDIT      = ABAP_FALSE.
*        <FS_CAT_0304>-NO_OUT    = ABAP_TRUE.
*        <FS_CAT_0304>-SCRTEXT_L = 'Chave Variação Outras'.
*      WHEN 'BSCHL_VARIACAOB'.
*        <FS_CAT_0304>-EDIT      = ABAP_FALSE.
*        <FS_CAT_0304>-NO_OUT    = ABAP_TRUE.
*        <FS_CAT_0304>-SCRTEXT_L = 'Chave Variação Banco'.
*      WHEN 'DMBTR'.
*        <FS_CAT_0304>-DO_SUM    = ABAP_TRUE.
*        <FS_CAT_0304>-OUTPUTLEN = 15.
*        <FS_CAT_0304>-EDIT      = ABAP_TRUE.
*      WHEN 'DMBE2' OR 'DMBE2_VARIACAO'.
*        <FS_CAT_0304>-DO_SUM    = ABAP_TRUE.
*        <FS_CAT_0304>-OUTPUTLEN = 15.
*        <FS_CAT_0304>-EDIT      = ABAP_FALSE.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0501

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0501
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0501 .

  gs_var_0501-report      = sy-repid.
  gs_var_0501-handle      = '0501'.
  gs_var_0501-log_group   = abap_false.
  gs_var_0501-username    = abap_false.
  gs_var_0501-variant     = abap_false.
  gs_var_0501-text        = abap_false.
  gs_var_0501-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_0501

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_0501  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info_0501 INPUT.

  CALL METHOD ctl_alv_0501->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0501
      es_row_no   = gs_scroll_row_0501.

ENDMODULE.                 " GET_SCROLL_INFO_0501  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_0501  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows_0501 INPUT.
  PERFORM popula_selecao_0501.
ENDMODULE.                 " GET_SELECTED_ROWS_0501  INPUT

*&---------------------------------------------------------------------*
*&      Form  POPULA_SELECAO_0501
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM popula_selecao_0501 .

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CLEAR it_selected_rows.

  CALL METHOD ctl_alv_0501->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  CLEAR it_zlest0107_sel[].

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_zlest0107_alv INDEX wa_selected_rows-index.
    MOVE-CORRESPONDING it_zlest0107_alv TO it_zlest0107_sel.
    APPEND it_zlest0107_sel.
  ENDLOOP.

ENDFORM.                    " POPULA_SELECAO_0501

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0504_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0504_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0504_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0504  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0504 INPUT.
  CASE ok_code.
    WHEN 'SALVAR'.
      CHECK zde_zlest0107-text IS NOT INITIAL.
      APPEND zde_zlest0107 TO it_zlest0107_alv.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                 " USER_COMMAND_0504  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0504  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0504 OUTPUT.

  SET PF-STATUS 'PF0501'.
  SET TITLEBAR 'TB0504'.

  CLEAR: zde_zlest0107-text.

  IF zde_zlest0107-cd_cidade IS NOT INITIAL AND zde_zlest0107-country IS NOT INITIAL.
    SELECT SINGLE text INTO zde_zlest0107-text
      FROM j_1btxjurt
     WHERE spras      EQ sy-langu
       AND country    EQ zde_zlest0107-country
       AND taxjurcode EQ zde_zlest0107-cd_cidade.
  ENDIF.

ENDMODULE.                 " STATUS_0504  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  LIMPAR_CIDADES_0504  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE limpar_cidades_0504 INPUT.
  CLEAR: zde_zlest0107-text.
ENDMODULE.                 " LIMPAR_CIDADES_0504  INPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0501B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0501b .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0501b> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZLEST0107'
    CHANGING
      ct_fieldcat      = it_catalog_0501b.

  lc_col_pos = 1.

  DELETE it_catalog_0501b WHERE fieldname EQ 'MANDT'.
  DELETE it_catalog_0501b WHERE fieldname EQ 'ID_ROTA'.

  LOOP AT it_catalog_0501b ASSIGNING <fs_cat_0501b>.
    <fs_cat_0501b>-col_pos = lc_col_pos.
    <fs_cat_0501b>-tabname   = 'ZDE_ZLEST0107'.
    <fs_cat_0501b>-fieldname = <fs_cat_0501b>-fieldname.
    ADD 1 TO lc_col_pos.
    CASE <fs_cat_0501b>-fieldname.
      WHEN 'TEXT'.
        <fs_cat_0501b>-outputlen = 40.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0501B

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0501B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0501b .

  gs_var_0501b-report      = sy-repid.
  gs_var_0501b-handle      = '0503'.
  gs_var_0501b-log_group   = abap_false.
  gs_var_0501b-username    = abap_false.
  gs_var_0501b-variant     = abap_false.
  gs_var_0501b-text        = abap_false.
  gs_var_0501b-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_0501B
