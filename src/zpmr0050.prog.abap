*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Autor......: Rogério Filipsick                                       *
* Data.......: 19/08/2019                                              *
* Descrição  : Exibir faturas de combustível                           *
* Transação..: ZPM0065                                                 *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
REPORT zpmr0050.

*----------------------------------------------------------------------*
* Tabelas -------------------------------------------------------------*
*----------------------------------------------------------------------*
TABLES: zpmt0024, zpmt0030.

*----------------------------------------------------------------------*
* Classes -------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS cl_gui_column_tree DEFINITION LOAD.
CLASS cl_gui_cfw         DEFINITION LOAD.

*----------------------------------------------------------------------*
* Tipos ---------------------------------------------------------------*
*----------------------------------------------------------------------*
TYPE-POOLS: abap.

TYPES: BEGIN OF ty_s_zpmt0029.
         INCLUDE TYPE zpmt0029 AS data2.
TYPES:   fatura TYPE zde_fatura.
TYPES: nkey TYPE lvc_nkey.
TYPES: parent_key TYPE lvc_nkey.
TYPES: END OF ty_s_zpmt0029.
TYPES: ty_t_zpmt0029 TYPE STANDARD TABLE OF ty_s_zpmt0029
                      WITH DEFAULT KEY.

TYPES: BEGIN OF ty_report.
         INCLUDE TYPE zpme0039.
TYPES: END OF ty_report.

TYPES: BEGIN OF ty_no,
         node_key TYPE lvc_nkey,
         fatura   TYPE zde_fatura,
         cnpj	    TYPE zde_stcd1,
       END OF ty_no.

*----------------------------------------------------------------------*
* Tabelas internas ----------------------------------------------------*
*----------------------------------------------------------------------*
DATA: t_zpmt0029       TYPE ty_t_zpmt0029,
      t_zpmt0030       TYPE TABLE OF zpmt0030,
      t_zpmt0026       TYPE TABLE OF zpmt0026,
      t_zpmt0032       TYPE TABLE OF zpmt0032,
      t_zpmt0025       TYPE TABLE OF zpmt0025,
      t_zpmt0024       TYPE TABLE OF zpmt0024,
      t_report         TYPE TABLE OF ty_report,
      t_zpmt0029_2     TYPE ty_t_zpmt0029,
      ls_stable        TYPE lvc_s_stbl,
      t_fcat           TYPE lvc_t_fcat,
      t_fcat2          TYPE lvc_t_fcat,
      t_no             TYPE TABLE OF ty_no,
      t_erro           TYPE TABLE OF zpme0050,

      gob_gui_alv_grid TYPE REF TO cl_gui_alv_grid,
      lines            TYPE sy-tabix,
      wa_selected_rows TYPE lvc_s_row,
      it_selected_rows TYPE lvc_t_row.

DATA: it_fcat          TYPE lvc_t_fcat.
*      gob_gui_alv_grid TYPE REF TO cl_gui_alv_grid.

*----------------------------------------------------------------------*
* Variaveis -----------------------------------------------------------*
*----------------------------------------------------------------------*
DATA:
  v_no_unico    TYPE char10,
  v_okcode      TYPE ui_func,
  v_node_key    TYPE lvc_nkey, "CHAR12,
  v_repid       TYPE syst-repid,
  v_ucomm       TYPE sy-ucomm,
  gs_layout     TYPE lvc_s_layo,
  gs_layout2    TYPE lvc_s_layo,
  gt_exc_button TYPE ui_functions,
  gs_toolbar    TYPE stb_button,
  gs_variant    TYPE disvariant,
  go_docking    TYPE REF TO cl_gui_docking_container,
  go_tree       TYPE REF TO cl_gui_alv_tree.

*----------------------------------------------------------------------*
*       CLASS lcl_eventhandler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_eventhandler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:

      handle_node_double_click
        FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING node_key,

      handle_item_double_click
        FOR EVENT item_double_click OF cl_gui_alv_tree
        IMPORTING node_key
                  fieldname.

ENDCLASS.                    "lcl_eventhandler DEFINITION

*---------------------------------------------------------------------
*
* Classes locais (Definição)
*
*---------------------------------------------------------------------
*
CLASS lcl_grid_event DEFINITION.
* seção publica
  PUBLIC SECTION.
*...Barra de Ferramentas
    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

*...Barra de Ferramentas
    METHODS handle_toolbar_300
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.
*...User Command
    METHODS handle_command_grid
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
*...User Command
    METHODS handle_command_grid_300
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.



ENDCLASS. "LCL_GRID_EVENT DEFINITION
*---------------------------------------------------------------------
*
* Classes locais (Implementação)
*
*---------------------------------------------------------------------
*
CLASS lcl_grid_event IMPLEMENTATION.
  METHOD handle_toolbar.
*...Barra de Ferramentas
    PERFORM f_toolbar_grid CHANGING e_object.
  ENDMETHOD. "handle_toolba

  METHOD handle_toolbar_300.
*...Barra de Ferramentas
    PERFORM f_toolbar_grid_300 CHANGING e_object.
  ENDMETHOD. "handle_toolba

  METHOD handle_command_grid.
*...Rotinas do botão Z da barra de ferramentas do ALV
    v_ucomm = e_ucomm.
    PERFORM f_command USING e_ucomm.
  ENDMETHOD. "handle_command_grid

  METHOD handle_command_grid_300.
*...Rotinas do botão Z da barra de ferramentas do ALV
    v_ucomm = e_ucomm.
    PERFORM f_command_300 USING e_ucomm.
  ENDMETHOD. "handle_command_grid

  METHOD on_double_click.

    DATA: lc_binario TYPE xstring,
          lt_pdf     TYPE TABLE OF char80.

    DATA: placa TYPE char7.
    DATA: text TYPE string.

    CLEAR:  text ,
            placa.

    CHECK e_row-rowtype IS INITIAL.


    TRY .
        DATA(wa_saida) = t_report[ e_row ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    DATA: cont TYPE char1.
    CASE  e_column.
      WHEN 'ICON'.
*        DATA(it_erro) = t_erro.
*        LOOP AT it_erro ASSIGNING FIELD-SYMBOL(<w_erro>) WHERE fatura        EQ wa_saida-fatura
*                                                          AND cnpj          EQ wa_saida-cnpj
*                                                          AND dt_cupom_fisc EQ wa_saida-dt_cupom_fisc
*                                                          AND hr_cupom_fisc EQ wa_saida-hr_cupom_fisc
*                                                          AND placa         EQ wa_saida-placa
*                                                          AND qtde          EQ wa_saida-qtde
*                                                          AND cod_material  EQ wa_saida-cod_material.
*
*          <w_erro>-check = abap_true.

*          IF <W_ERRO>-AVALIACAO IS NOT INITIAL.
*            CALL FUNCTION 'STATUS_TEXT_EDIT'
*              EXPORTING
*                FLG_USER_STAT    = YX
*                OBJNR            = <W_ORDEM>-OBJNR             "1695763
*                SPRAS            = SY-LANGU
*              IMPORTING
*                LINE             = <W_ORDEM>-STTXT             "1695763
*                USER_LINE        = <W_ORDEM>-ASTTX             "1695763
*              EXCEPTIONS
*                OBJECT_NOT_FOUND = 1
*                OTHERS           = 2.
*
*            IF SY-SUBRC = 0.
*              <W_ORDEM>-STATUS = <W_ORDEM>-STTXT.
*              <W_ORDEM>-STATUS = <W_ORDEM>-STATUS(4).
*            ENDIF.
*          ENDIF.
*        ENDLOOP.

*        DELETE it_erro WHERE check NE abap_true.
*
*        IF ( it_erro IS NOT INITIAL ).
*          DATA: linha_selecionada TYPE slis_selfield.
*          DATA: _exit             TYPE c.
*          DATA(tl_fieldcat) = VALUE slis_t_fieldcat_alv(
*
*          ( fieldname = 'FATURA            '        seltext_m = 'Fatura          '  outputlen = '15' )
*          ( fieldname = 'CNPJ              '        seltext_m = 'CNPJ            '  outputlen = '18' )
*          ( fieldname = 'DT_CUPOM_FISC     '        seltext_m = 'Data Abastec    '  outputlen = '10' )
*          ( fieldname = 'HR_CUPOM_FISC     '        seltext_m = 'Hora Abastec    '  outputlen = '10' )
*          ( fieldname = 'PLACA             '        seltext_m = 'Placa Veiculo   '  outputlen = '07' )
*          ( fieldname = 'COD_MATERIAL      '        seltext_m = 'Cod material    '  outputlen = '15' )
*          ( fieldname = 'QTDE              '        seltext_m = 'Qtde            '  outputlen = '10' )
*          ( fieldname = 'AVALIACAO         '        seltext_m = 'Analise erro    '  outputlen = '72' ) ).
*
*          CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
*            EXPORTING
*              i_title     = 'Pendencias a serem corrigidas'
*              i_selection = 'X'
*              i_tabname   = 'T_ERRO'
*              i_zebra     = 'X'
*              it_fieldcat = tl_fieldcat
*            IMPORTING
*              es_selfield = linha_selecionada
*              e_exit      = _exit
*            TABLES
*              t_outtab    = it_erro.
*
**          IF PLACA EQ <W_ERRO>-PLACA.
**            DATA(AVALIACAO) =  |{ <W_ERRO>-AVALIACAO } , { TEXT }| .
**          ENDIF.
**
**          TEXT  = AVALIACAO.
**          PLACA = <W_ERRO>-PLACA.
*
**
**        IF SY-SUBRC EQ 0.
**          ZCL_STRING=>SHOW_TEXTO( EXPORTING I_STRING = AVALIACAO I_TITULO = 'lOG DE AVALIAÇÃO' ).
*        ENDIF.

      WHEN 'PLACA'.

        DATA: gw_veiculo TYPE equz.

        SELECT SINGLE b~equnr
           FROM equz AS a
           INNER JOIN equi AS b ON b~equnr EQ a~equnr
           INNER JOIN fleet AS c ON c~objnr EQ b~objnr
             INTO @DATA(equnr)
               WHERE c~license_num EQ @wa_saida-placa
                 AND a~datbi EQ '99991231'.

        SET PARAMETER ID 'EQN' FIELD equnr.
        CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.
    ENDCASE.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

ENDCLASS. "LCL_GRID_EVENT IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_eventhandler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_eventhandler IMPLEMENTATION.

  METHOD handle_node_double_click.

  ENDMETHOD.                    "handle_node_double_click

  METHOD handle_item_double_click.

    CLEAR: v_node_key.
    v_node_key = node_key.
    CONDENSE v_node_key.
*    V_NODE_KEY = V_NODE_KEY - 1.
    CONDENSE v_node_key.

    DELETE t_zpmt0029 WHERE fatura = space.

    IF t_zpmt0029[] IS NOT INITIAL.
      CALL SCREEN '0200'.
    ENDIF.

*    IF V_UCOMM = 'APROVAR' OR V_UCOMM = 'REPROVAR'.
*
*      CALL METHOD GO_TREE->DELETE_SUBTREE
*        EXPORTING
*          I_NODE_KEY                = NODE_KEY
*          I_UPDATE_PARENTS_EXPANDER = ABAP_TRUE.
*
*      CALL METHOD CL_GUI_CFW=>FLUSH.
*    ENDIF.

  ENDMETHOD.                    "handle_item_double_click

ENDCLASS.                    "lcl_eventhandler IMPLEMENTATION

DATA: lcl_alv           TYPE REF TO cl_gui_alv_grid,
      lcl_container_alv TYPE REF TO cl_gui_custom_container,
      lcl_event_300     TYPE REF TO lcl_grid_event,
      lcl_event         TYPE REF TO lcl_grid_event.

DATA:
  dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
  dg_parent_1        TYPE REF TO cl_gui_container,
  dg_parent_alv      TYPE REF TO cl_gui_container,
  obj_custom         TYPE REF TO cl_gui_custom_container,
  ctl_alv            TYPE REF TO cl_gui_alv_grid,
  dg_dyndoc_id       TYPE REF TO cl_dd_document,
  table_element      TYPE REF TO cl_dd_table_element,
  column             TYPE REF TO cl_dd_area,
  g_custom_container TYPE REF TO cl_gui_custom_container,
  picture            TYPE REF TO cl_gui_picture,
  url(255)           TYPE c.

DATA:
  table_element2          TYPE REF TO cl_dd_table_element,
  sdydo_text_element(255),
  p_text_table            TYPE sdydo_text_table,
  p_text                  TYPE sdydo_text_element,
  dg_splitter_2           TYPE REF TO cl_gui_splitter_container,
  dg_parent_2a            TYPE REF TO cl_gui_container,
  dg_html_cntrl           TYPE REF TO cl_gui_html_viewer,
  dg_parent_2             TYPE REF TO cl_gui_container,
  column_1                TYPE REF TO cl_dd_area.
*----------------------------------------------------------------------*
* Tela de seleção -----------------------------------------------------*
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(22) TEXT-003.
*SELECTION-SCREEN POSITION 2.
    PARAMETERS: r_r1 RADIOBUTTON GROUP 1 DEFAULT 'X' USER-COMMAND abc.

    SELECTION-SCREEN COMMENT 32(10) TEXT-004.
*SELECTION-SCREEN POSITION 20.
    PARAMETERS: r_r2 RADIOBUTTON GROUP 1.

    SELECTION-SCREEN COMMENT 50(20) TEXT-013.
*SELECTION-SCREEN POSITION 79.
    PARAMETERS: r_r3 RADIOBUTTON GROUP 1.

    SELECTION-SCREEN COMMENT 79(30) TEXT-014.
*SELECTION-SCREEN POSITION 79.
    PARAMETERS: r_r4 RADIOBUTTON GROUP 1.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_centro FOR zpmt0024-centro  MODIF ID i,
                  s_cnpj   FOR zpmt0024-cnpj MODIF ID i,
                  s_fat    FOR zpmt0024-fatura MODIF ID i,
                  s_dt     FOR zpmt0024-dt_fatura MODIF ID i.
SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-015.
  SELECT-OPTIONS: s_pedido FOR zpmt0030-pedido MODIF ID t.
SELECTION-SCREEN END OF BLOCK b3.

*---------------------------------------------------------------------*
* SELECTION-SCREEN --------------------------------------------------*
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  IF r_r4 IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 = 'T'.
        screen-active = 0.
      ENDIF.

      IF screen-group1 = 'I'.
        screen-active = 1.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = 'T'.
        screen-active = 1.
      ENDIF.

      IF screen-group1 = 'I'.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

*----------------------------------------------------------------------*
* START-OF-SELECTION --------------------------------------------------*
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF r_r4 IS INITIAL.
    IF s_centro IS INITIAL.
      MESSAGE 'Informe o centro' TYPE 'I'.
    ELSE.
      CALL SCREEN '0100'.
    ENDIF.

  ELSE.
    PERFORM fm_selec_pedido.
    CALL SCREEN '0300'.
  ENDIF.


END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  INIT_CONTROLS
*&---------------------------------------------------------------------*
FORM f_init_controls .

* create Hierarchy-header
  DATA ls_hierarchy_header TYPE treev_hhdr.
  DATA: nodes TYPE lvc_t_nkey.
* Create docking container

  IF sy-ucomm = 'REFRESH'.
    CALL METHOD go_tree->free( ).
    CLEAR: sy-ucomm.
  ENDIF.

  IF ( go_docking IS NOT BOUND ).
    CREATE OBJECT go_docking
      EXPORTING
        parent = cl_gui_container=>screen0
        ratio  = 90
      EXCEPTIONS
        OTHERS = 6.

  ENDIF.
* create tree control
  CREATE OBJECT go_tree
    EXPORTING
      parent                      = go_docking
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
      item_selection              = 'X'  " required for double-click event on item
      no_html_header              = ''
      no_toolbar                  = ''
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.

  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.

  PERFORM f_build_hierarchy_header CHANGING ls_hierarchy_header.

  PERFORM f_build_fieldcatalog.

  PERFORM f_set_layout_and_variant.

* create emty tree-control
  CALL METHOD go_tree->set_table_for_first_display
    EXPORTING
      i_structure_name    = 'ZPMT0029'
      i_default           = 'X'
      is_hierarchy_header = ls_hierarchy_header
    CHANGING
      it_outtab           = t_zpmt0029
      it_fieldcatalog     = t_fcat.

* create hierarchy
  PERFORM f_create_hierarchy.

  IF t_zpmt0029[] IS INITIAL.
    MESSAGE s000(z_les) WITH TEXT-011 DISPLAY LIKE 'S'.
    LEAVE TO SCREEN 0.
  ENDIF.

* register events
  PERFORM f_register_events.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_build_fieldcatalog .


  FIELD-SYMBOLS: <fs_fcat> TYPE lvc_s_fcat.

  REFRESH: t_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZPMT0029'
      i_client_never_display = 'X'
    CHANGING
      ct_fieldcat            = t_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

*  IF r_r2 EQ abap_false.
*    LOOP AT t_fcat ASSIGNING <fs_fcat>.
*      CASE <fs_fcat>-fieldname.
*        WHEN 'DOCNUM'.
*          <fs_fcat>-no_out = 'X'.
*        WHEN 'MIRO'.
*          <fs_fcat>-no_out = 'X'.
*        WHEN 'NMR_NFE'.
*          <fs_fcat>-no_out = 'X'.
*        WHEN 'PREV_PAG_MIRO'.
*          <fs_fcat>-no_out = 'X'.
*      ENDCASE.
*    ENDLOOP.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_HIERARCHY_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_HIERARCHY_HEADER  text
*----------------------------------------------------------------------*
FORM f_build_hierarchy_header  CHANGING p_hierarchy_header TYPE treev_hhdr.

  p_hierarchy_header-heading = 'Tipo Documento'.            "#EC NOTEXT
  p_hierarchy_header-tooltip =
                         'Tipo Documento'.                  "#EC NOTEXT
  p_hierarchy_header-width = 30.
  p_hierarchy_header-width_pix = ''.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_hierarchy.

* add data to tree
  DATA: ld_fatura      TYPE lvc_nkey,
        ld_fatura_item TYPE lvc_nkey,
        ld_item_key    TYPE lvc_nkey,
        ld_cnpj        TYPE lvc_nkey,
        ld_cnpj_key    TYPE lvc_nkey.
  DATA: it_node_key    TYPE lvc_t_nkey.

* get data
  IF r_r1 = abap_true.

    SELECT *
      FROM zpmt0032
      INTO CORRESPONDING FIELDS OF TABLE t_zpmt0029
     WHERE centro       IN s_centro
       AND cnpj         IN s_cnpj
       AND fatura       IN s_fat
       AND dt_fatura    IN s_dt
       AND cod_status    = space.

  ELSEIF  r_r2 = abap_true.
    DATA: r_fatura TYPE RANGE OF zpmt0032-fatura.

    SELECT *
       FROM zpmt0032
       INTO CORRESPONDING FIELDS OF TABLE t_zpmt0029
      WHERE centro       IN s_centro
        AND cnpj         IN s_cnpj
        AND fatura       IN s_fat
        AND dt_fatura    IN s_dt
        AND cod_status   NOT IN (space, '999').

    SELECT *
     FROM zpmt0030
     INTO TABLE @DATA(t_zpmt0030)
      FOR ALL ENTRIES IN   @t_zpmt0029
      WHERE fatura  EQ @t_zpmt0029-fatura
       AND  pedido  EQ @t_zpmt0029-pedido.
    "AND  conf_rateio NE @space.

    IF t_zpmt0030 IS NOT INITIAL.

*** CS2019001802 - Inicio
*      LOOP AT t_zpmt0030 ASSIGNING FIELD-SYMBOL(<w_zpmt0030>).
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = <w_zpmt0030>-fatura ) TO r_fatura.
*      ENDLOOP.
*
*      DELETE t_zpmt0029 WHERE fatura IN r_fatura.

*** CS2019001802 - Inicio

      LOOP AT t_zpmt0029 INTO DATA(w_zpmt0029).
        DATA(idx) = sy-tabix.

        READ TABLE t_zpmt0030 INTO DATA(w_zpmt0030) WITH KEY fatura = w_zpmt0029-fatura
                                                             pedido = w_zpmt0029-pedido.
        IF sy-subrc = 0.

          w_zpmt0029-migo    = w_zpmt0030-mblnr.
          w_zpmt0029-miro    = w_zpmt0030-belnr.
          w_zpmt0029-nfe     = w_zpmt0030-nfe.

          SELECT *
            FROM rbkp
            INTO TABLE @DATA(t_rbkp)
            WHERE belnr EQ @w_zpmt0029-miro.

          READ TABLE t_rbkp INTO DATA(w_rbkp) INDEX 1.

          IF sy-subrc EQ 0.

            DATA(zbelnr) = w_rbkp-belnr && w_rbkp-gjahr.

            SELECT SINGLE *
              FROM bkpf
              INTO @DATA(w_bkpf)
              WHERE awkey EQ @zbelnr.

            IF w_bkpf IS NOT INITIAL.
              SELECT SINGLE *
                FROM bsak
                INTO @DATA(w_bsak)
                WHERE belnr EQ @w_bkpf-belnr.

              IF sy-subrc EQ 0.
                w_zpmt0029-prev_pag_miro = w_bsak-zfbdt. "Data base para cálculo do vencimento.
              ENDIF.
            ENDIF.
          ENDIF.

          MODIFY t_zpmt0029 INDEX idx FROM w_zpmt0029.
        ENDIF.
      ENDLOOP.
    ENDIF.
*** CS2019001802 - Fim

  ELSEIF  r_r3 = abap_true.
    SELECT *
       FROM zpmt0032
       INTO CORRESPONDING FIELDS OF TABLE t_zpmt0029
      WHERE centro       IN s_centro
        AND cnpj         IN s_cnpj
        AND fatura       IN s_fat
        AND dt_fatura    IN s_dt
        AND cod_status   EQ '3'.  "NOT IN (space, '999'). *** CS2019001802



  ENDIF.


  CLEAR: v_no_unico.
  SORT t_zpmt0029 BY cnpj fatura.
*  DELETE ADJACENT DUPLICATES FROM T_ZPMT0029 COMPARING FATURA.
  MOVE-CORRESPONDING t_zpmt0029[] TO t_zpmt0029_2[].

  LOOP AT t_zpmt0029_2 ASSIGNING FIELD-SYMBOL(<zpmt0029>).
    PERFORM f_add_customer_line USING  <zpmt0029>-data2
                                       ''
                            CHANGING ld_cnpj.

    IF v_no_unico = 1.
      IF ld_cnpj IS NOT INITIAL.
        APPEND ld_cnpj TO it_node_key.
      ENDIF.
    ENDIF.

***************************************
    ON CHANGE OF <zpmt0029>-cnpj.

      PERFORM f_add_cnpj USING <zpmt0029>-data2
                               <zpmt0029>-cnpj
                               ld_cnpj
                      CHANGING ld_item_key.

    ENDON.
***************************************


***************************************
    ON CHANGE OF <zpmt0029>-cnpj.
      IF ld_fatura IS INITIAL.
        ld_fatura = ld_cnpj.
      ELSE.
        ld_fatura = 1.
      ENDIF.

      PERFORM f_add_fatura USING <zpmt0029>-data2
                                 ld_fatura
                        CHANGING ld_fatura_item.

      IF ld_fatura_item  IS NOT INITIAL.
        APPEND ld_fatura_item TO it_node_key.
      ENDIF.
    ENDON.

    PERFORM f_add_item_line    USING <zpmt0029>-data2
                                    <zpmt0029>-fatura
                                     ld_fatura_item
                           CHANGING ld_item_key.
***************************************

    APPEND INITIAL LINE TO t_no ASSIGNING FIELD-SYMBOL(<fs_no>).
    <fs_no>-node_key = ld_item_key.
    CONDENSE <fs_no>-node_key NO-GAPS.
    <fs_no>-fatura   = <zpmt0029>-fatura.
    <fs_no>-cnpj     = <zpmt0029>-cnpj.

  ENDLOOP.

*" calculate totals
  CALL METHOD go_tree->update_calculations.

* this method must be called to send the data to the frontend
  CALL METHOD go_tree->frontend_update.

  DELETE ADJACENT DUPLICATES FROM it_node_key COMPARING ALL FIELDS.
  CALL METHOD go_tree->expand_nodes( it_node_key = it_node_key ).

* adjust column_width
  CALL METHOD go_tree->column_optimize.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ADD_CUSTOMER_LINE
*&---------------------------------------------------------------------*
FORM f_add_customer_line  USING us_data      TYPE ty_s_zpmt0029-data2
                              ud_relat_key TYPE lvc_nkey
                     CHANGING cd_node_key  TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = go_tree->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensifd_critical.
  APPEND ls_item_layout TO lt_item_layout.

* add node
  IF v_no_unico = space.
    l_node_text =  'Cnpj'.

    DATA: ls_node TYPE lvc_s_layn.

    CALL METHOD go_tree->add_node
      EXPORTING
        i_relat_node_key = ud_relat_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = l_node_text
*       IS_OUTTAB_LINE   = US_DATA
        is_node_layout   = ls_node
        it_item_layout   = lt_item_layout
      IMPORTING
        e_new_node_key   = cd_node_key.
  ENDIF.

*  V_NO_UNICO = ABAP_TRUE.
  v_no_unico = v_no_unico + 1.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_register_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_register_events.
* define the events which will be passed to the backend
  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.

* define the events which will be passed to the backend
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND l_event TO lt_events.

  CALL METHOD go_tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.

* set Handler
  SET HANDLER:
    lcl_eventhandler=>handle_node_double_click FOR go_tree,
    lcl_eventhandler=>handle_item_double_click FOR go_tree.

ENDFORM.                               " register_events

*&---------------------------------------------------------------------*
*&      Form  F_SET_LAYOUT_AND_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_set_layout_and_variant .

  CLEAR: gs_layout,
         gs_variant.

  gs_variant-report = syst-repid.
  gs_variant-handle = 'TREE'.

ENDFORM.                    " SET_LAYOUT_AND_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_ADD_ITEM_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUTTAB_DATA  text
*      -->P_LD_FATURA_KEY  text
*      <--P_LD_ITEM_KEY  text
*----------------------------------------------------------------------*
FORM f_add_item_line  USING    us_data TYPE ty_s_zpmt0029-data2
                             fatura  TYPE zde_fatura
                             ud_relat_key TYPE lvc_nkey
                    CHANGING cd_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value.
  DATA: ls_node TYPE lvc_s_layn.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = go_tree->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensifd_critical.
  APPEND ls_item_layout TO lt_item_layout.

* add node
  l_node_text       =  fatura.
  ls_node-n_image   = space.
  ls_node-exp_image = space.

  CALL METHOD go_tree->add_node
    EXPORTING
      i_relat_node_key = ud_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = us_data
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = cd_node_key.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'PF_0100'.
  SET TITLEBAR 'TITULO_0100'.

  PERFORM f_init_controls.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'CANC' OR 'EXIT'.
*      FREE: T_ZPMT0029.
*      PERFORM CONFIRM_SAIR.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'PF_0200'.
  SET TITLEBAR 'TITULO_0200'.

  PERFORM f_seleciona_zpmt0026.

  IF t_report[] IS INITIAL.
    MESSAGE s000(z_les) WITH TEXT-011 DISPLAY LIKE 'S'.
    LEAVE TO SCREEN 0.
  ENDIF.

  PERFORM f_exibe_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      FREE t_report[].
      LEAVE TO SCREEN 0.
    WHEN 'CANC' OR 'EXIT'.
      LEAVE TO TRANSACTION 'ZPM0065'.

*    WHEN 'EXECUTE'.
*
*      CHECK t_report IS NOT INITIAL.
*      zcl_exc_apont_med=>check_erro(
*        EXPORTING
*          t_report =     t_report " Exibir as informações da fatura
*        IMPORTING
*          t_erro   =     t_erro " Estrutura para retornar o erro de odometros frota propria.
*      ).
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_ZPMT0026
*&---------------------------------------------------------------------*
FORM f_seleciona_zpmt0026.
  DATA: wa_value        TYPE imrg,
        vl_cont         TYPE rihimrg-pyeac,
        v_medano        TYPE dec_16_02_s,
        placa           TYPE license_num,
        med_ant         TYPE dec_16_02_s,
        wa_data_general TYPE bapi_itob,
        wa_return       TYPE bapiret2,
        i_ordem         TYPE aufnr,
        gw_veiculo      TYPE equz.


  FREE: t_report, t_erro.
  IF t_zpmt0029[] IS NOT INITIAL.

*    READ TABLE T_ZPMT0029 ASSIGNING FIELD-SYMBOL(<ZPMT0029>)
*                                        INDEX V_NODE_KEY.

    READ TABLE t_no ASSIGNING FIELD-SYMBOL(<zpmt0029>)
                                  WITH KEY node_key = v_node_key.
    IF <zpmt0029> IS ASSIGNED.

      CLEAR: t_zpmt0032.
      SELECT *
        FROM zpmt0032
        INTO TABLE t_zpmt0032
       WHERE fatura EQ <zpmt0029>-fatura
        AND cnpj EQ <zpmt0029>-cnpj.

      CLEAR: t_zpmt0026.
      SELECT *
        FROM zpmt0026
        INTO TABLE t_zpmt0026
        FOR ALL ENTRIES IN t_zpmt0032
       WHERE fatura EQ t_zpmt0032-fatura
         AND   cnpj EQ <zpmt0029>-cnpj.

      IF sy-subrc IS INITIAL.

        CLEAR: t_zpmt0024.
        SELECT *
          FROM zpmt0024
          INTO TABLE t_zpmt0024
           FOR ALL ENTRIES IN t_zpmt0026
         WHERE fatura     EQ t_zpmt0026-fatura
           AND  cnpj      EQ t_zpmt0026-cnpj
           AND cupom_fisc EQ t_zpmt0026-cupom_fisc.

        SORT t_zpmt0024 ASCENDING BY placa dt_cupom_fisc hr_cupom_fisc .
        SORT t_zpmt0026 ASCENDING BY cod_material.

        LOOP AT t_zpmt0024 ASSIGNING FIELD-SYMBOL(<zpmt0024>).
          LOOP AT t_zpmt0026 ASSIGNING FIELD-SYMBOL(<zpmt0026>) WHERE fatura       EQ <zpmt0024>-fatura
                                                                AND    cnpj        EQ <zpmt0024>-cnpj
                                                                AND    cupom_fisc  EQ <zpmt0024>-cupom_fisc.

            APPEND INITIAL LINE TO t_report ASSIGNING FIELD-SYMBOL(<fs_report>).
*          READ TABLE T_ZPMT0024 ASSIGNING FIELD-SYMBOL(<ZPMT0024>)
*                                              WITH KEY FATURA       = <ZPMT0026>-FATURA
*                                                       CNPJ         = <ZPMT0026>-CNPJ
*                                                       CUPOM_FISC   = <ZPMT0026>-CUPOM_FISC.
*          IF SY-SUBRC IS INITIAL.

*            zcl_exc_apont_med=>get_equipamento(
*              EXPORTING
*                i_placa = <zpmt0024>-placa " Placa de veículo
*              IMPORTING
*                e_equnr = DATA(equipamento)    " Nº equipamento
*            ).
*
*            zcl_exc_apont_med=>select_pont_medicao(
*                EXPORTING
*                i_equnr = equipamento
*                RECEIVING
*                t_dimpt = DATA(t_dimpt)
*            ).

            "Verificando se material foi atribuido ao ponto de medição.
*            LOOP AT t_dimpt ASSIGNING FIELD-SYMBOL(<w_dimpt>).
*              CASE <w_dimpt>-atnam.
*                WHEN 'COMBUSTIVEL'.
*                  IF <w_dimpt>-locas IS INITIAL.
*                    IF <zpmt0026>-cod_material EQ '000000000000000007'.
*                      APPEND VALUE #( avaliacao = 'Veiculo não possue material 184924 atribuido ao ponto de medição de combustivel.'
*                                      fatura        = <zpmt0024>-fatura
*                                      cnpj          = <zpmt0024>-cnpj
*                                      dt_cupom_fisc = <zpmt0024>-dt_cupom_fisc
*                                      hr_cupom_fisc = <zpmt0024>-hr_cupom_fisc
*                                      placa         = <zpmt0024>-placa
*                                      cod_material  = <zpmt0026>-cod_material
*                                      qtde          = <zpmt0026>-qtde
*                                     ) TO t_erro.
*
*                      <fs_report>-icon          = icon_annotation.
*                    ENDIF.
*                  ENDIF.
*              ENDCASE.
*            ENDLOOP.

*            DATA(it_dimpt) = t_dimpt.
*            SORT it_dimpt BY atnam.
*            DELETE it_dimpt WHERE atnam NE 'COMBUSTIVEL'.
*            DELETE it_dimpt WHERE inact EQ abap_true.
*
*            IF it_dimpt IS INITIAL.
*              IF <zpmt0026>-cod_material EQ '000000000000000007'.
*                APPEND VALUE #( avaliacao     = 'Veiculo não possue pontos de medição de combustivel cadastrado, favor cadastrar.'
*                                    fatura        = <zpmt0024>-fatura
*                                    cnpj          = <zpmt0024>-cnpj
*                                    dt_cupom_fisc = <zpmt0024>-dt_cupom_fisc
*                                    hr_cupom_fisc = <zpmt0024>-hr_cupom_fisc
*                                    placa         = <zpmt0024>-placa
*                                    cod_material  = <zpmt0026>-cod_material
*                                    qtde          = <zpmt0026>-qtde
*                                    ) TO t_erro.
*                <fs_report>-icon          = icon_annotation.
*              ENDIF.
*            ENDIF.
*
*
*
*            SORT t_dimpt BY atnam.
*            DELETE t_dimpt WHERE atnam NE 'ODOMETRO'.
*            DELETE t_dimpt WHERE indtr EQ abap_true.
*            DELETE t_dimpt WHERE inact EQ abap_true.

*            CLEAR: v_medano, wa_value, vl_cont.
*            IF placa NE <zpmt0024>-placa.
*
*              IF t_dimpt IS NOT INITIAL.
*                READ TABLE t_dimpt ASSIGNING <w_dimpt> INDEX 1.
*
***  VERIFICAR SE EXISTE ALGUMA MEDIÇÃO VÁLIDA PARA O PONTO A SER APONTADO..
*                IF <w_dimpt>-atnam EQ 'ODOMETRO' AND <w_dimpt>-indtr NE abap_true.
*                  CALL FUNCTION 'MEASUREM_DOCUM_READ_LAST'
*                    EXPORTING
*                      buffer_bypass  = ' '
*                      dyfield        = ' '
*                      offset_date    = sy-datum
*                      offset_time    = sy-uzeit
*                      point          = <w_dimpt>-point
*                    IMPORTING
*                      imrg_wa        = wa_value
*                    EXCEPTIONS
*                      imrg_not_found = 1
*                      OTHERS         = 2.
*
*                  IF sy-subrc IS INITIAL.
*** Calcula posição do contador
*                    PERFORM zf_convert USING wa_value-recdu wa_value-readg 'X' vl_cont.
*                    REPLACE ALL OCCURRENCES OF ',' IN vl_cont WITH '.' .
*                    MOVE vl_cont TO v_medano.
*                  ENDIF.
*                ENDIF.
*              ELSE.
*                IF <zpmt0026>-cod_material EQ '000000000000000007'.
*                  APPEND VALUE #( avaliacao     = 'Veiculo não possue pontos de medição odometro cadastrado, favor cadastrar.'
*                                  fatura        = <zpmt0024>-fatura
*                                  cnpj          = <zpmt0024>-cnpj
*                                  dt_cupom_fisc = <zpmt0024>-dt_cupom_fisc
*                                  hr_cupom_fisc = <zpmt0024>-hr_cupom_fisc
*                                  placa         = <zpmt0024>-placa
*                                  cod_material  = <zpmt0026>-cod_material
*                                  qtde          = <zpmt0026>-qtde
*                                  ) TO t_erro.
*
*                  <fs_report>-icon          = icon_annotation.
*                ENDIF.
*              ENDIF.
*            ENDIF.

*            IF <zpmt0026>-cod_material EQ '000000000000000007'.
*              IF v_medano IS NOT INITIAL AND placa NE <zpmt0024>-placa.
*                <fs_report>-odometro_ant  = v_medano.
*              ELSE.
*                IF placa EQ <zpmt0024>-placa.
*                  <fs_report>-odometro_ant  = med_ant.
*                ENDIF.
*              ENDIF.
*
*              CONDENSE <zpmt0024>-odometro.
*              <fs_report>-odometro      = <zpmt0024>-odometro.
*              <fs_report>-diferenca     = <fs_report>-odometro - <fs_report>-odometro_ant.
*              med_ant = <fs_report>-odometro.
*              placa = <zpmt0024>-placa.
*
*            ENDIF.

            <fs_report>-fatura        = <zpmt0026>-fatura.
            <fs_report>-cod_material  = <zpmt0026>-cod_material.
            <fs_report>-desc_material = <zpmt0026>-desc_material.
            <fs_report>-qtde          = <zpmt0026>-qtde.
            <fs_report>-vlr_unt       = <zpmt0026>-vlr_unt.
            <fs_report>-vlr_total     = <zpmt0026>-vlr_total.
            <fs_report>-cnpj          = <zpmt0026>-cnpj.


            "Check placa veiculo.
            CLEAR: gw_veiculo, i_ordem .
            SELECT SINGLE *
              FROM fleet AS a
              INNER JOIN equi AS b ON b~objnr EQ a~objnr
              INNER JOIN equz AS c ON c~equnr EQ b~equnr
                INTO CORRESPONDING FIELDS OF gw_veiculo
                  WHERE a~license_num EQ <zpmt0024>-placa
                  AND b~eqtyp EQ 'A'
                  AND c~datbi EQ '99991231'.

            IF gw_veiculo IS NOT INITIAL.

              "Seleciona material Diesel no SET
              SELECT SINGLE *
              FROM setleaf
              INTO @DATA(i_data)
              WHERE setname EQ 'MAGGI_ZPM0065_DIESEL'.


              "Seleciona os dados do veiculo.
              CLEAR: wa_data_general.
              CALL FUNCTION 'BAPI_EQUI_GETDETAIL' "#EC CI_USAGE_OK[2438131]
                EXPORTING
                  equipment        = gw_veiculo-equnr
                IMPORTING
                  data_general_exp = wa_data_general
                  return           = wa_return.

*              DATA(cod_material) = |{ <zpmt0026>-cod_material ALPHA = OUT }|.
*              DATA(cod_material_set) = |{ i_data-valfrom ALPHA = OUT }|.

              IF <zpmt0026>-cod_material EQ i_data-valfrom.
                i_ordem = wa_data_general-standorder.
              ELSE.
                i_ordem = wa_data_general-settlorder.
              ENDIF.
            ENDIF.

            IF i_ordem IS NOT INITIAL.
              "Check ordem.
              SELECT SINGLE * FROM aufk INTO @DATA(w_aufk) WHERE aufnr EQ  @i_ordem.
              IF sy-subrc EQ 0.
                READ TABLE t_zpmt0032 INTO DATA(w_zpmt0032) WITH KEY fatura = <zpmt0024>-fatura cnpj = <zpmt0024>-cnpj.
                IF sy-subrc EQ 0.
                  IF w_zpmt0032-centro NE w_aufk-werks.
                    i_ordem = ''.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

            <zpmt0026>-aufnr = i_ordem.

            <fs_report>-dt_fatura     = <zpmt0024>-dt_fatura.
            <fs_report>-hr_fatura     = <zpmt0024>-hr_fatura.
            <fs_report>-chave_nfe     = <zpmt0024>-chave_nfe.
            <fs_report>-cupom_fisc    = <zpmt0024>-cupom_fisc.
            <fs_report>-dt_cupom_fisc = <zpmt0024>-dt_cupom_fisc.
            <fs_report>-hr_cupom_fisc = <zpmt0024>-hr_cupom_fisc.
            <fs_report>-dt_exportacao = <zpmt0024>-dt_exportacao.
            <fs_report>-hr_exportacao = <zpmt0024>-hr_exportacao.
            <fs_report>-placa         = <zpmt0024>-placa.
            <fs_report>-ordem         = <zpmt0026>-aufnr.
            <fs_report>-status_proc   = <zpmt0024>-status_proc.
            <fs_report>-odometro      = <zpmt0024>-odometro.
*          ENDIF.
            CLEAR: w_aufk, i_data.
          ENDLOOP.
        ENDLOOP.

      ENDIF.
*      FREE: T_ZPMT0024, T_ZPMT0025, T_ZPMT0026.
    ENDIF.
  ENDIF.

ENDFORM.


FORM f_command USING p_ucomm TYPE sy-ucomm.


  CASE v_ucomm.
    WHEN 'APROVAR'. "Botão Aprovar

*      CHECK t_report IS NOT INITIAL.
*      zcl_exc_apont_med=>check_erro(
*        EXPORTING
*          t_report =     t_report " Exibir as informações da fatura
*        IMPORTING
*          t_erro   =     t_erro " Estrutura para retornar o erro de odometros frota propria.
*      ).

*      IF t_erro IS INITIAL.
      PERFORM f_aprova.
*      ELSE.
*        MESSAGE 'Não foi possivel aprovar  a fatura porque existem erros a serem corrigidos, favor verificar' TYPE 'I' DISPLAY LIKE 'E'.
*        EXIT..
*      ENDIF.
    WHEN 'REPROVAR'. "Botão Reprovar
      PERFORM f_reprova.
    WHEN 'MARCA'.
      PERFORM f_marca_desmarca USING 'M'.
    WHEN 'DESMARCA'.
      PERFORM f_marca_desmarca USING 'D'.

*    WHEN 'EXECUTE'.
*      CHECK t_report IS NOT INITIAL.
*      zcl_exc_apont_med=>check_erro(
*        EXPORTING
*          t_report =     t_report " Exibir as informações da fatura
*        IMPORTING
*          t_erro   =     t_erro " Estrutura para retornar o erro de odometros frota propria.
*      ).

  ENDCASE.

* Refresh no Relatório
  PERFORM f_atualiza_alv.

ENDFORM. " F_command
*&--------------------------------------------------------------------
*& Form F_toolbar_grid
*&--------------------------------------------------------------------
* Barra de botões do alv
*---------------------------------------------------------------------
FORM f_toolbar_grid CHANGING p_object TYPE REF TO cl_alv_event_toolbar_set.

  CLEAR gs_toolbar.
  MOVE: 'MARCA' TO gs_toolbar-function,
  icon_select_all TO gs_toolbar-icon,
  TEXT-006 TO gs_toolbar-text,
  space TO gs_toolbar-disabled.
  APPEND gs_toolbar TO p_object->mt_toolbar.

  CLEAR gs_toolbar.
  MOVE: 'DESMARCA' TO gs_toolbar-function ,
  icon_deselect_all TO gs_toolbar-icon ,
  TEXT-007 TO gs_toolbar-text ,
  space TO gs_toolbar-disabled .
  APPEND gs_toolbar TO p_object->mt_toolbar.

  CLEAR gs_toolbar.
  MOVE: 'APROVAR' TO gs_toolbar-function,
  icon_okay TO gs_toolbar-icon,
  TEXT-008 TO gs_toolbar-text,
  space TO gs_toolbar-disabled.
  APPEND gs_toolbar TO p_object->mt_toolbar.

  CLEAR gs_toolbar.
  MOVE: 'REPROVAR' TO gs_toolbar-function ,
  icon_cancel TO gs_toolbar-icon ,
  TEXT-009 TO gs_toolbar-text ,
  space TO gs_toolbar-disabled .
  APPEND gs_toolbar TO p_object->mt_toolbar.


*  DELETE p_object->mt_toolbar WHERE function <> 'APROVAR'
*                                AND function <> 'REPROVAR'
*                                AND function <> 'MARCA'
*                                AND function <> 'DESMARCA'
*                                AND function <> '&MB_SUM'
*                                AND function <> '&MB_FILTER'
*                                AND function <> '&MB_EXPORT'
*                                AND function <> '&SORT_ASC'
*                                AND function <> '&SORT_DSC'
*                                AND function <> '&PRINT_BACK'.


ENDFORM. " F_toolbar_grid
*&---------------------------------------------------------------------*
*&      Form  F_APROVA
*&---------------------------------------------------------------------*
FORM f_aprova .

  DATA: gs_zpmt0025  TYPE zpmt0025.
  DATA: item       TYPE p DECIMALS 2,
        cont_preco TYPE p DECIMALS 2,
        cont_vlr   TYPE p DECIMALS 2.


  LOOP AT t_zpmt0032 ASSIGNING FIELD-SYMBOL(<fs_zpmt0032>).
    <fs_zpmt0032>-cod_status  = '1'.
    <fs_zpmt0032>-status_proc  = 'Aguardando pedido'.
  ENDLOOP.


  LOOP AT t_zpmt0024 ASSIGNING FIELD-SYMBOL(<fs_zpmt0024>).
    <fs_zpmt0024>-cod_status  = '1'.
    <fs_zpmt0024>-status_proc  = 'Aguardando pedido'.
  ENDLOOP.



  IF <fs_zpmt0024> IS ASSIGNED.
    DELETE t_zpmt0029 WHERE fatura = <fs_zpmt0024>-fatura
                        AND cnpj   = <fs_zpmt0024>-cnpj.
  ENDIF.

  SORT t_zpmt0026 BY cod_material ASCENDING.
  DATA(gt_zpmt0026) = t_zpmt0026.

****  Separando a quantidade de materiais.
  DELETE ADJACENT DUPLICATES FROM gt_zpmt0026 COMPARING fatura cod_material.

  CLEAR: cont_vlr, item.
  LOOP AT gt_zpmt0026 ASSIGNING FIELD-SYMBOL(<fw_zpmt0026>).
    CLEAR: <fw_zpmt0026>-qtde, <fw_zpmt0026>-vlr_unt, <fw_zpmt0026>-vlr_total, cont_vlr, cont_preco.

    LOOP AT t_zpmt0026 ASSIGNING FIELD-SYMBOL(<fs_zpmt0026>) WHERE cod_material EQ <fw_zpmt0026>-cod_material.
      ADD <fs_zpmt0026>-qtde TO <fw_zpmt0026>-qtde.
      ADD <fs_zpmt0026>-vlr_unt TO cont_preco.
      ADD 1 TO cont_vlr.

    ENDLOOP.

    ADD 10 TO item.
    <fw_zpmt0026>-vlr_unt = cont_preco / cont_vlr.
    <fw_zpmt0026>-vlr_total = <fw_zpmt0026>-qtde * <fw_zpmt0026>-vlr_unt.

    READ TABLE t_zpmt0024 INTO DATA(_zpmt0024) WITH KEY fatura = <fw_zpmt0026>-fatura
                                                        cnpj   = <fw_zpmt0026>-cnpj.

    READ TABLE t_zpmt0032 INTO DATA(_zpmt0032) WITH  KEY fatura = _zpmt0024-fatura cnpj = _zpmt0024-cnpj.
*    Verificando ordem para centro no SET 'MAGGI_ZPM0065_ORDEM'
    zcl_webservic_protheus=>get_ordem( EXPORTING werks = _zpmt0032-centro IMPORTING aufnr = DATA(w_ordem) ).

*   Selecionando centro de custo da ordem
    zcl_webservic_protheus=>get_c_c_equip( EXPORTING aufnr = w_ordem IMPORTING kostl = DATA(w_kostl) ).

    APPEND VALUE #(
     item          = item
     cnpj          = _zpmt0032-cnpj
     lifnr         = _zpmt0032-lifnr
     empresa       = _zpmt0032-empresa
     dt_fatura     = _zpmt0032-dt_fatura
     hr_fatura     = _zpmt0032-hr_fatura
     cliente       = _zpmt0032-cliente
     cnpj_cliente  = _zpmt0032-cnpj_cliente
     centro        = _zpmt0032-centro
     ordem         =  w_ordem
     activity      = '0010'
     werks         = _zpmt0032-centro
     mandt         = sy-mandt
     fatura        = <fw_zpmt0026>-fatura
     cod_material  = <fw_zpmt0026>-cod_material
     desc_material = <fw_zpmt0026>-desc_material
     qtde          = <fw_zpmt0026>-qtde
     und           = <fw_zpmt0026>-und
     vlr_unt       = <fw_zpmt0026>-vlr_unt
     vlr_total     = <fw_zpmt0026>-vlr_total
     bsart         = 'PCSF'
     ekgrp         = 'F99'
     acctasscat    = 'F'
     gl_account    = ' '
     costcenter    = w_kostl
     co_area       = 'MAGI'
*     CREAT_DATE    = SY-DATUM
     cod_status    = '1'
     status_proc   = 'Aguardando pedido' ) TO t_zpmt0025.

**<<-------Chamado User Story 108613 / User Story 108613 / AOENNING / Chamado cancelado apos retorno de homologação keyuser.
*    DATA: wa_requisition_items  TYPE bapiebanc,
*          it_requisition_items  TYPE STANDARD TABLE OF bapiebanc,
*          it_return             TYPE STANDARD TABLE OF bapireturn,
*          wa_return             TYPE bapireturn,
*          it_ACCOUNT_ASSIGNMENT TYPE STANDARD TABLE OF bapiebkn,
*          wa_ACCOUNT_ASSIGNMENT TYPE bapiebkn,
*          w_number              LIKE bapiebanc-preq_no,
*          c_e                   TYPE c VALUE 'I',
*          c_x                   TYPE c VALUE 'X'.


*    DATA aux_codmat    TYPE matnr18.
*    DATA aux_matnr    TYPE matnr18.
*    DATA aux_MATKL    TYPE maRA-matkl.
*    DATA aux_SAKNR    TYPE zmmt0039-saknr.
*    PACK <fw_zpmt0026>-cod_material TO aux_codmat.
*    UNPACK aux_codmat TO aux_codmat.
*
*
*    SELECT SINGLE matnr FROM zpmt0034 WHERE cod_material = @aux_codmat INTO @aux_matnr.
*    SELECT SINGLE maktx FROM makt WHERE matnr = @aux_matnr INTO @wa_requisition_items-short_text.
*    SELECT SINGLE matkl FROM maRA WHERE matnr = @aux_matnr INTO @aux_MATKL.
*    SELECT SINGLE saknr FROM zmmt0039 WHERE matkl = @aux_MATKL INTO @aux_SAKNR.
*
*
*    wa_requisition_items-doc_type   = 'RCS'.                   "Tipo de requisição de compra (P/ Agro sempre NB)
*    wa_requisition_items-preq_item  = item.      "N Item
*    wa_requisition_items-material   = aux_matnr.       "N Material
*    "wa_requisition_items-short_text = <fw_zpmt0026>-desc_material.        "Texto Breve Material
*    "wa_requisition_items-store_loc  = gv_deposito.       "Depósito
*    wa_requisition_items-quantity   = <fw_zpmt0026>-qtde.
*    wa_requisition_items-pur_group  = 'F99'.       "Grupo de Comprador
*    wa_requisition_items-plant      = _zpmt0032-centro.             "Centro
*    "wa_requisition_items-trackingno =  gv_acompanha .
*    wa_requisition_items-preq_name  = sy-uname.
*    wa_requisition_items-deliv_date = sy-datum.               "Data da remessa
*    wa_requisition_items-del_datcat = 1.                      "Tipo de data da remessa
**      wa_requisition_items-prio_urg   = gv_un.
*    "wa_requisition_items-mrp_contr = gv_planejador.
*    wa_requisition_items-c_amt_bapi = <fw_zpmt0026>-vlr_total.
*    wa_requisition_items-price_unit = <fw_zpmt0026>-vlr_unt.
*    wa_requisition_items-acctasscat = 'F'.
*
*    APPEND wa_requisition_items TO it_requisition_items.
*    CLEAR: aux_codmat,aux_matnr.
*
*    wa_ACCOUNT_ASSIGNMENT-preq_item = item.
*    wa_ACCOUNT_ASSIGNMENT-created_on = sy-datum.
*    wa_ACCOUNT_ASSIGNMENT-created_by = sy-uname.
*    wa_ACCOUNT_ASSIGNMENT-g_l_acct = aux_SAKNR.
*    wa_ACCOUNT_ASSIGNMENT-bus_area = _zpmt0032-centro.
*    wa_ACCOUNT_ASSIGNMENT-cost_ctr = w_kostl.
*    wa_ACCOUNT_ASSIGNMENT-order_no = w_ordem.
*
*
*    APPEND wa_ACCOUNT_ASSIGNMENT TO it_ACCOUNT_ASSIGNMENT.


*    CLEAR: _zpmt0032,w_kostl.", w_ordem,wa_ACCOUNT_ASSIGNMENT,aux_SAKNR, wa_requisition_items.
**<<-------Chamado User Story 108613 / User Story 108613 / AOENNING / Chamado cancelado apos retorno de homologação keyuser.
  ENDLOOP.

**<<-------Chamado User Story 108613 / User Story 108613 / AOENNING / Chamado cancelado apos retorno de homologação keyuser.
*  CALL FUNCTION 'BAPI_REQUISITION_CREATE'
*    IMPORTING
*      number                         = w_number
*    TABLES
*      requisition_items              = it_requisition_items
*      requisition_account_assignment = it_ACCOUNT_ASSIGNMENT
*      return                         = it_return
*      "extensionin        = lt_extensionin
*    .
*
*  READ TABLE it_return INTO wa_return WITH KEY type = c_e.
*  IF sy-subrc EQ 0.
*
*    "Commit
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = c_x.

    "gravar no campo novo w_number

*    LOOP AT t_zpmt0032 ASSIGNING FIELD-SYMBOL(<_add_BANFN_32>).
*      <_add_BANFN_32>-banfn = w_number.
*    ENDLOOP.
*
*    LOOP AT t_zpmt0025 ASSIGNING FIELD-SYMBOL(<_add_BANFN_25>).
*      <_add_BANFN_25>-banfn = w_number.
*    ENDLOOP.
**<<-------Chamado User Story 108613 / User Story 108613 / AOENNING / Chamado cancelado apos retorno de homologação keyuser.

    MODIFY zpmt0024 FROM TABLE t_zpmt0024.
    MODIFY zpmt0025 FROM TABLE t_zpmt0025.
    MODIFY zpmt0026 FROM TABLE t_zpmt0026.
    MODIFY zpmt0032 FROM TABLE t_zpmt0032.
    COMMIT WORK.
**<<-------Chamado User Story 108613 / User Story 108613 / AOENNING / Chamado cancelado apos retorno de homologação keyuser.
*    CLEAR: w_number.
*    CLEAR: it_requisition_items[],it_ACCOUNT_ASSIGNMENT[], it_return[].
**<<-------Chamado User Story 108613 / User Story 108613 / AOENNING / Chamado cancelado apos retorno de homologação keyuser.
    MESSAGE s000(zwrm001) DISPLAY LIKE 'S' WITH 'Fatura aprovada com sucesso!'.

    sy-ucomm = 'REFRESH'.
    LEAVE TO SCREEN 0100.

**<<-------Chamado User Story 108613 / User Story 108613 / AOENNING / Chamado cancelado apos retorno de homologação keyuser.
*  ELSE.
*    MESSAGE e000(zwrm001) DISPLAY LIKE 'S' WITH wa_return-message wa_return-message_v1 wa_return-message_v2 wa_return-message_v3.
*    CLEAR: w_number.
*    CLEAR: it_requisition_items[],it_ACCOUNT_ASSIGNMENT[], it_return[].
*  ENDIF.

*  CLEAR: w_number.
*  CLEAR: it_requisition_items[],it_ACCOUNT_ASSIGNMENT[], it_return[].
**<<-------Chamado User Story 108613 / User Story 108613 / AOENNING / Chamado cancelado apos retorno de homologação keyuser.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REPROVA
*&---------------------------------------------------------------------*
FORM f_reprova .

  DATA: t_ch_text TYPE TABLE OF txline,
        l_flag    TYPE c,
        l_texto   TYPE string.

  LOOP AT t_report ASSIGNING FIELD-SYMBOL(<fs_report>).
    IF <fs_report>-flag = abap_true.
      l_flag = abap_true.
    ENDIF.
  ENDLOOP.

  IF l_flag = space.
    MESSAGE s000(z_les) WITH TEXT-005 DISPLAY LIKE 'E'.
  ELSE.

    DELETE t_report WHERE flag NE abap_true.

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title        = 'Inserir a observação'
        im_start_column = 50
        im_start_row    = 5
      CHANGING
        ch_text         = t_ch_text.

    IF t_ch_text[] IS INITIAL.
      MESSAGE s000(z_les) WITH TEXT-010 DISPLAY LIKE 'E'.
    ELSEIF t_ch_text[] IS NOT INITIAL.

      LOOP AT t_ch_text ASSIGNING FIELD-SYMBOL(<fs_ch_text>).
        CONCATENATE l_texto <fs_ch_text>
        INTO l_texto SEPARATED BY space.
      ENDLOOP.

      LOOP AT t_report ASSIGNING <fs_report>.
        LOOP AT t_zpmt0032 ASSIGNING FIELD-SYMBOL(<fs_zpmt0032>) WHERE fatura EQ <fs_report>-fatura.
          <fs_zpmt0032>-observ = 'cancelada'.
          <fs_zpmt0032>-cod_status = '999'.
          <fs_zpmt0032>-status_proc = 'Erro'.
        ENDLOOP.
        LOOP AT t_zpmt0024 ASSIGNING FIELD-SYMBOL(<fs_zpmt0024>) WHERE fatura EQ <fs_report>-fatura AND cupom_fisc EQ <fs_report>-cupom_fisc.
          <fs_zpmt0024>-observ = l_texto.
          <fs_zpmt0024>-cod_status = '999'.
          <fs_zpmt0024>-status_proc = 'Erro'.
        ENDLOOP.
      ENDLOOP.

      MODIFY zpmt0024 FROM TABLE t_zpmt0024.
      MODIFY zpmt0032 FROM TABLE t_zpmt0032.
      COMMIT WORK.

*      MESSAGE 'Fatura reprovada com sucesso!' TYPE 'S' DISPLAY LIKE 'S'.
      MESSAGE s000(zwrm001) DISPLAY LIKE 'S' WITH 'Fatura reprovada com sucesso!'.
      LEAVE TO SCREEN 0.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MARCA_DESMARCA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_marca_desmarca USING p_condicao TYPE c.

  IF p_condicao = 'M'.
    LOOP AT t_report ASSIGNING FIELD-SYMBOL(<fs_report_m>).
      <fs_report_m>-flag = abap_true.
    ENDLOOP.
  ELSEIF p_condicao = 'D'.
    LOOP AT t_report ASSIGNING FIELD-SYMBOL(<fs_report_d>).
      CLEAR: <fs_report_d>-flag.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ALV
*&---------------------------------------------------------------------*
FORM f_atualiza_alv .

  DATA: l_stable       TYPE lvc_s_stbl, "Estrutura para refresh do ALV
        l_soft_refresh TYPE c. "Campo para refresh do ALV

*...Fixa posição da linha no ALV
  l_stable-row = abap_true.
  l_stable-col = abap_true.
  l_soft_refresh = abap_true.

*...Atualiza o ALV
  CALL METHOD ctl_alv->refresh_table_display
    EXPORTING
      is_stable      = l_stable
      i_soft_refresh = l_soft_refresh
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
*       ALV
*----------------------------------------------------------------------*
FORM f_exibe_alv.

  DATA: gs_variant  TYPE disvariant.
  gs_variant-report      = sy-repid.

*  DATA:
*    P_TEXT                  TYPE SDYDO_TEXT_ELEMENT,
*    SDYDO_TEXT_ELEMENT(255),
*    P_TEXT_TABLE            TYPE SDYDO_TEXT_TABLE.

  FREE: t_fcat2.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZPME0039'
      i_client_never_display = 'X'
    CHANGING
      ct_fieldcat            = t_fcat2
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT t_fcat2 ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
    IF <fs_fieldcat>-fieldname = 'QTDE'.
      <fs_fieldcat>-do_sum = abap_true.
    ENDIF.
  ENDLOOP.

  READ TABLE t_fcat2 ASSIGNING FIELD-SYMBOL(<fs_fcat>) INDEX 1.
  <fs_fcat>-checkbox  = abap_true.
*  <FS_FCAT>-NO_SUM    = ABAP_TRUE.
  <fs_fcat>-edit      = abap_true.
  <fs_fcat>-outputlen = 2.

  gs_layout2-zebra = abap_true.       "Código Zebrado
  gs_layout2-no_rowmark = abap_true. "Exclui barra standard de flag a esquerda
  gs_layout2-cwidth_opt = abap_true. "Ajusta tamanho na coluna
  gs_layout2-box_fname = abap_true. "

  IF ctl_alv IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'ALV'
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
        height = 25.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 75.

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

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent          = dg_parent_alv
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF r_r1 IS NOT INITIAL.
      CREATE OBJECT lcl_event.
*      SET HANDLER LCL_EVENT-> FOR CTL_ALV.
      SET HANDLER lcl_event->handle_toolbar FOR ctl_alv.
      SET HANDLER lcl_event->handle_command_grid FOR ctl_alv.
      SET HANDLER lcl_event->on_double_click  FOR ctl_alv.

    ENDIF.
  ENDIF.


  CALL METHOD ctl_alv->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout2
      i_save          = 'A'
      is_variant      = gs_variant
    CHANGING
      it_fieldcatalog = t_fcat2
      it_outtab       = t_report[].

***Cabecalho.
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

  PERFORM cabecario.

*  ------------------
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

  CALL METHOD ctl_alv->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2169   text
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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CABECARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cabecario .
  DATA: w_cnpj TYPE char18.

  READ TABLE t_zpmt0032 ASSIGNING FIELD-SYMBOL(<_fatura>) INDEX 1.

  w_cnpj = <_fatura>-cnpj(02) &&'.'&& <_fatura>-cnpj+02(03) && '.' && <_fatura>-cnpj+5(03) &&'/' && <_fatura>-cnpj+8(04) && '-' && <_fatura>-cnpj+12(02).
  CONCATENATE 'CNPJ:' w_cnpj INTO sdydo_text_element SEPARATED BY space.
  APPEND sdydo_text_element TO p_text_table.
  CLEAR: sdydo_text_element.

  CONCATENATE 'Empresa:' <_fatura>-empresa INTO sdydo_text_element SEPARATED BY space.
  APPEND sdydo_text_element TO p_text_table.
  CLEAR: sdydo_text_element.

  CONCATENATE 'Cod fornecedor:' <_fatura>-lifnr INTO sdydo_text_element SEPARATED BY space.
  APPEND sdydo_text_element TO p_text_table.
  CLEAR: sdydo_text_element.

  CONCATENATE 'Fatura:' <_fatura>-fatura INTO sdydo_text_element SEPARATED BY space.
  APPEND sdydo_text_element TO p_text_table.
  CLEAR: sdydo_text_element.

  CONCATENATE 'Cliente:' <_fatura>-cliente INTO sdydo_text_element SEPARATED BY space.
  APPEND sdydo_text_element TO p_text_table.
  CLEAR: sdydo_text_element.

  CONCATENATE 'Centro:' <_fatura>-centro INTO sdydo_text_element SEPARATED BY space.
  APPEND sdydo_text_element TO p_text_table.
  CLEAR: sdydo_text_element.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_SAIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirm_sair .

  DATA: p_respo TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING        "TITLEBAR = 'Confirmar'
      text_question         = 'Sair da transação sem salvar as alterações?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      display_cancel_button = ' '
    IMPORTING
      answer                = p_respo.

  IF p_respo = 1.
    LEAVE TO TRANSACTION 'ZPM0065'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ADD_CNPJ
*&---------------------------------------------------------------------*
FORM f_add_cnpj  USING  us_data      TYPE ty_s_zpmt0029-data2
                        cnpj         TYPE zde_stcd1
                        ud_relat_key TYPE lvc_nkey
               CHANGING cd_node_key  TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value.
  DATA: ls_node TYPE lvc_s_layn.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = go_tree->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensifd_critical.
  APPEND ls_item_layout TO lt_item_layout.

* add node
  l_node_text       = cnpj.
  ls_node-n_image   = space.
  ls_node-exp_image = space.

  CALL METHOD go_tree->add_node
    EXPORTING
      i_relat_node_key = ud_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
*     IS_OUTTAB_LINE   = US_DATA
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = cd_node_key.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ADD_FATURA
*&---------------------------------------------------------------------*
FORM f_add_fatura USING us_data      TYPE ty_s_zpmt0029-data2
                        ud_relat_key TYPE lvc_nkey
               CHANGING cd_node_key  TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value.
  DATA: ls_node TYPE lvc_s_layn.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = go_tree->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensifd_critical.
  APPEND ls_item_layout TO lt_item_layout.

* add node
  l_node_text =  'Fatura'.

  CALL METHOD go_tree->add_node
    EXPORTING
      i_relat_node_key = ud_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
*     IS_OUTTAB_LINE   = US_DATA
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = cd_node_key.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CONVERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_VALUE_RECDU  text
*      -->P_WA_VALUE_READG  text
*      -->P_1543   text
*      -->P_VL_CONT  text
*----------------------------------------------------------------------*
FORM zf_convert USING f_unit   TYPE c
                     f_input  LIKE impt-pyear
                     f_indik  LIKE impt-pyeari
                     f_flstr  LIKE rihimrg-pyeac.

  CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
    EXPORTING
      char_unit       = f_unit
      decimals        = 2
      exponent        = 0
      fltp_value_si   = f_input
      indicator_value = f_indik
    IMPORTING
      char_value      = f_flstr
    EXCEPTIONS
      no_unit_given   = 01.
ENDFORM.                    " F_CONVERT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'PF0300'.
  SET TITLEBAR 'TB0300'.

  PERFORM fm_criar_objetos.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos .

  DATA: lva_data(22) TYPE c,
        w_layout     TYPE lvc_s_layo.

  PERFORM fm_cria_fieldcat.


  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'Pedidos sem MIGO e MIRO'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.


*    CREATE OBJECT event_receiver.
*    SET HANDLER event_receiver->hotspot_click  FOR gob_gui_alv_grid.
*    SET HANDLER event_receiver->get_ucomm  FOR gob_gui_alv_grid.

    w_layout-cwidth_opt = abap_true.
    w_layout-zebra      = 'X'.
    w_layout-sel_mode   = 'A'.
    w_layout-col_opt    = abap_true.

    CREATE OBJECT lcl_event_300.
    SET HANDLER lcl_event_300->handle_toolbar_300 FOR gob_gui_alv_grid.
    SET HANDLER lcl_event_300->handle_command_grid_300 FOR gob_gui_alv_grid.



    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
      CHANGING
        it_outtab                     = t_zpmt0030
        it_fieldcatalog               = it_fcat
*       IT_SORT                       =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat .

  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.
  it_fcat = VALUE lit_fieldcat_aux(
( fieldname ='FATURA        '               coltext = 'Nº de um documento de faturamento     '    col_opt = 'X' no_zero = '' hotspot = '' )
( fieldname ='CHAVE_NFE     '               coltext = 'Chave de Documento Fiscal Eletrônico  '    col_opt = 'X' no_zero = '' )
( fieldname ='PEDIDO        '               coltext = 'Nº do documento de compras            '    col_opt = 'X' no_zero = '' )
( fieldname ='LIFNR         '               coltext = 'Nº conta do fornecedor                '    col_opt = 'X' no_zero = '' )
( fieldname ='NFE           '               coltext = 'Número NF-e                           '    col_opt = 'X' no_zero = '' )
( fieldname ='MBLNR         '               coltext = 'Nº documento de material              '    col_opt = 'X' no_zero = '' )
( fieldname ='BELNR         '               coltext = 'Nº de um documento de faturamento     '    col_opt = 'X' no_zero = '' )
( fieldname ='DOCNUM_NFE    '               coltext = 'Nº documento                          '    col_opt = 'X' no_zero = '' )
( fieldname ='CONF_PGT      '               coltext = 'Confirmação pagamento da NFe.         '    col_opt = 'X' no_zero = '' hotspot = '' )
( fieldname ='CONF_RATEIO   '               coltext = 'Confirmação se rateio                 '    col_opt = 'X' no_zero = '' )
( fieldname ='CANC          '               coltext = 'Cancelamento                          '    col_opt = 'X' no_zero = '' )
( fieldname ='DT_ATUAL      '               coltext = 'Campo do tipo DATS                    '    col_opt = 'X' no_zero = '' )
( fieldname ='HR_ATUAL      '               coltext = 'Campo da ctg.TIMS                     '    col_opt = 'X' no_zero = '' )
( fieldname ='USUARIO       '               coltext = 'Nome do usuário                       '    col_opt = 'X' no_zero = '' )
).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SELEC_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_selec_pedido .

  SELECT *
  FROM zpmt0030
  INTO CORRESPONDING FIELDS OF TABLE t_zpmt0030
  WHERE pedido IN s_pedido
  AND  mblnr  EQ space
  AND belnr EQ space
  AND canc EQ space
  ORDER BY pedido.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCELAR'.
*      PERFORM fm_cancelar_chave_fatura.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_CANCELAR_CHAVE_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cancelar_chave_fatura .


  CALL METHOD gob_gui_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  DESCRIBE TABLE it_selected_rows LINES lines.

  IF ( lines IS INITIAL ).
    MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.

  ELSE.
    LOOP AT it_selected_rows INTO wa_selected_rows.
      READ TABLE t_zpmt0030 INTO DATA(wa_zpmt0030) INDEX wa_selected_rows-index.
      IF sy-subrc EQ 0.
        wa_zpmt0030-canc = abap_true.

        "Alterar status tabela.
        MODIFY zpmt0030 FROM wa_zpmt0030.
        COMMIT WORK.

      ENDIF.
      CLEAR: wa_zpmt0030.
    ENDLOOP.
  ENDIF.

  PERFORM fm_selec_pedido.

  CALL METHOD gob_gui_alv_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TOOLBAR_GRID_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM f_toolbar_grid_300  CHANGING p_object TYPE REF TO cl_alv_event_toolbar_set..
  CLEAR gs_toolbar.
  MOVE: 'CANCELAR_NFE' TO gs_toolbar-function,
  icon_bw_process_cancel TO gs_toolbar-icon,
  TEXT-016 TO gs_toolbar-text,
  space TO gs_toolbar-disabled.
  APPEND gs_toolbar TO p_object->mt_toolbar.

  CLEAR gs_toolbar.
  MOVE: 'EXECUTAR_MIGO_MIRO' TO gs_toolbar-function,
  icon_execute_object TO gs_toolbar-icon,
  TEXT-017 TO gs_toolbar-text,
  space TO gs_toolbar-disabled.
  APPEND gs_toolbar TO p_object->mt_toolbar.

  CLEAR gs_toolbar.
  MOVE: 'ATUALIZAR_STATUS_PEDIDO' TO gs_toolbar-function,
  icon_refresh TO gs_toolbar-icon,
  TEXT-018 TO gs_toolbar-text,
  space TO gs_toolbar-disabled.
  APPEND gs_toolbar TO p_object->mt_toolbar.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COMMAND_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM f_command_300  USING    p_e_ucomm.

  CASE v_ucomm.
    WHEN 'CANCELAR_NFE'. "Botão Aprovar
      PERFORM fm_cancelar_chave_fatura.

    WHEN 'EXECUTAR_MIGO_MIRO'. "Start do processamento migo e miro.
      PERFORM fm_executar_migo_miro.
    WHEN 'ATUALIZAR_STATUS_PEDIDO'. "Start do processamento migo e miro.
      PERFORM fm_atual_status_pedido.

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_EXECUTAR_MIGO_MIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_executar_migo_miro.

***criar migo e miro para pedidos fatura de combustivel pendentes.
  TRY .
      zcl_webservic_protheus=>get_nfe( ).
    CATCH zcx_cadastro INTO DATA(ex_cadastro).

  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_atual_status_pedido
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_atual_status_pedido.

  DATA:tl_fields TYPE TABLE OF sval WITH HEADER LINE,
       v_ebeln   TYPE ekko-ebeln,
       lv_return TYPE vbpok-charg.


  "Preenche campo do popup.
  CLEAR tl_fields.
  tl_fields-tabname    = 'EKKO'.
  tl_fields-fieldname  = 'EBELN'.
  tl_fields-fieldtext  = 'Nº Pedido'.
  tl_fields-value      =  ''.
  tl_fields-comp_tab   = 'EKKO'.
  tl_fields-comp_field = 'EBELN'.
  tl_fields-field_obl  = 'X'.
*  it_tab-novaluehlp = ''.
  APPEND tl_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Informe o pedido'
    IMPORTING
      returncode      = lv_return
    TABLES
      fields          = tl_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF sy-subrc IS INITIAL.
    v_ebeln  = tl_fields-value.
  ENDIF.

  SELECT SINGLE * FROM zpmt0032
    INTO @DATA(ws_zpmt0032)
    WHERE pedido EQ @v_ebeln.
  IF  sy-subrc EQ 0.
    zcl_webservic_protheus=>get_cons_fatura(
      EXPORTING
        fatura    = ws_zpmt0032-fatura
        cnpj      = ws_zpmt0032-cnpj
      RECEIVING
        e_returng = DATA(e_fatura) ).
  ELSE.
    MESSAGE 'Pedido não localizado' TYPE 'I'.
  ENDIF.

  CLEAR: ws_zpmt0032.



  PERFORM fm_selec_pedido.

  CALL METHOD gob_gui_alv_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

ENDFORM.
