*******************************************************************************************
*            	                     +------------------+                                  	*
*                                  | Grupo | WAYON    |                                   *
*                                  +------------------+                                   *
*-----------------------------------------------------------------------------------------*
*** Dados do Programa                                                                     *
*-----------------------------------------------------------------------------------------*
* Nome            : ZFIR0085
* Título          : Painel de Processamento de Estorno de ICMS - Decreto 64213/19         *
* Autor           : Jaime Tassoni                                                         *
* Data            : 01.09.2020                                                            *
*-----------------------------------------------------------------------------------------*
*** Histórico das modificações                                                            *
*-----------------------------------------------------------------------------------------*
* Seq | Data         | Autor              | COD/ Descrição Da Modificação                 *
*-----------------------------------------------------------------------------------------*
*                                                                                         *
*******************************************************************************************
REPORT zfir0085 MESSAGE-ID fb.

*******************************************************************************************
* tabelas
*******************************************************************************************
TABLES: reguh.

*******************************************************************************************
* classes / btree
*******************************************************************************************
CLASS cl_gui_column_tree     DEFINITION LOAD.
CLASS cl_gui_cfw             DEFINITION LOAD.

DATA tree1 TYPE REF TO cl_hrpayna_gui_alv_tree. "cl_gui_alv_tree.
DATA mr_toolbar              TYPE REF TO cl_gui_toolbar.

DATA: v_docking   TYPE REF TO cl_gui_docking_container,
      v_splitter  TYPE REF TO cl_gui_splitter_container,
      v_container TYPE REF TO cl_gui_container,
      v_grid      TYPE REF TO cl_gui_alv_grid.

*******************************************************************************************
* types
*******************************************************************************************
TYPES: BEGIN OF ty_reguh,
         laufd TYPE reguh-laufd,
         laufi TYPE reguh-laufi,
         xvorl TYPE reguh-xvorl,
         zbukr TYPE reguh-zbukr,
         lifnr TYPE reguh-lifnr,
         kunnr TYPE reguh-kunnr,
         empfg TYPE reguh-empfg,
         vblnr TYPE reguh-vblnr,
         znme1 TYPE reguh-znme1,
         rbetr TYPE reguh-rbetr,
         valut TYPE reguh-valut,
         hbkid TYPE reguh-hbkid,
         zaldt TYPE reguh-zaldt.
TYPES: END   OF ty_reguh.

TYPES: BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         stblg TYPE bkpf-stblg.
TYPES: END   OF ty_bkpf.

TYPES: BEGIN OF ty_alv.
         INCLUDE STRUCTURE zfis0010.
       TYPES: END   OF ty_alv.

TYPES: BEGIN OF ty_node_chave,
         level1 TYPE lvc_nkey,
         level2 TYPE lvc_nkey,
         linha  TYPE sy-tabix,
         stblg  TYPE bkpf-stblg,
         erro   TYPE char1,
         chave  TYPE char40.
TYPES: END   OF ty_node_chave.

TYPES: BEGIN OF ty_chave,
         line      TYPE sy-tabix,
         index_key TYPE lvc_nkey.
TYPES: END   OF ty_chave.

TYPES: BEGIN OF ty_icon,
         id   TYPE icon-id,
         name TYPE icon-name.
TYPES: END   OF ty_icon.

*******************************************************************************************
* tabelas / works
*******************************************************************************************
DATA: t_fieldcatalog  TYPE lvc_t_fcat, "Fieldcatalog
      t_checked_items TYPE lvc_t_chit,
      w_checked_items TYPE LINE OF  lvc_t_chit,
      t_node_key      TYPE lvc_t_nkey,  "Saves top node key for expand nodes
      t_node_key_aux  TYPE lvc_t_nkey,  "Saves top node key for expand nodes
      t_node_chave    TYPE TABLE OF ty_node_chave,
      t_node_key_sel  TYPE lvc_t_nkey,  "Saves top node key for expand nodes
      t_node_key_det  TYPE lvc_t_nkey,  "Saves top node key for expand nodes
      w_node_key      TYPE lvc_nkey,
      w_node_chave    TYPE ty_node_chave,
      w_node_key_sel  TYPE lvc_nkey,
      w_node_key_det  TYPE lvc_nkey,
*
      t_chave         TYPE TABLE OF ty_chave,
      t_reguh         TYPE TABLE OF ty_reguh,
      t_bkpf          TYPE TABLE OF ty_bkpf,
      t_zfit0167      TYPE TABLE OF zfit0167,
      t_alv           TYPE TABLE OF ty_alv,
      t_exctab        TYPE slis_t_extab,
      t_icon          TYPE TABLE OF ty_icon,
      gt_alv          TYPE TABLE OF ty_alv,
*
      w_chave         TYPE ty_chave,
      w_reguh         TYPE ty_reguh,
      w_zfit0167      TYPE zfit0167,
      w_bkpf          TYPE ty_bkpf,

      w_exctab        TYPE slis_extab,
      w_alv           TYPE ty_alv,
      w_icon          TYPE ty_icon,
      w_item_layout   TYPE lvc_s_laci.

*******************************************************************************************
* data
*******************************************************************************************
DATA: ok_code     TYPE sy-ucomm,           "OK-Code
      l_tabix     TYPE sy-tabix,          "OK-Code
      l_chave     TYPE char40,
      l_chave_ant TYPE char40,
      l_timestamp TYPE timestampl,
      l_seq(3)    TYPE n,
      l_field     TYPE lvc_fname,
      l_icon_name TYPE icon-name,
      l_erro      TYPE c,
      l_vblnr     TYPE char10,
      l_lista     TYPE c,
      l_subrc     TYPE sy-subrc,
      l_name      TYPE tbtcjob-jobname,
      l_count     TYPE tbtcjob-jobcount,
      l_user      TYPE sy-uname         VALUE 'JOBADM',
      l_logsys    TYPE logsys,
      l_dest      TYPE char10           VALUE 'NONE',
      l_taskname  TYPE char10.

*******************************************************************************************
* includes
*******************************************************************************************
INCLUDE <icon>.
INCLUDE zfi0085_toolbar_event_receiver.
INCLUDE zfi0085_tree_event_receiver.

DATA: toolbar_event_receiver TYPE REF TO lcl_toolbar_event_receiver.

************************************************************************
* Ranges
************************************************************************
RANGES:
      r_name                  FOR icon-name,
      r_data                  FOR sy-datum.

*******************************************************************************************
* select
*******************************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS    : p_zbukr LIKE reguh-zbukr   OBLIGATORY,
                p_laufd LIKE reguh-laufd   OBLIGATORY,
                p_laufi LIKE reguh-laufi   OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK b1.

*******************************************************************************************
* inicio
*******************************************************************************************
START-OF-SELECTION.

*-Selecao dos dados
  PERFORM f_selecao.

  IF t_reguh[] IS INITIAL.
    MESSAGE s000(fb) WITH text-110.
    STOP.
  ENDIF.

*-Montar saida
  PERFORM f_monta_saida.

  IF t_alv[] IS INITIAL.
    MESSAGE s000(fb) WITH text-110.
    STOP.
  ENDIF.

*-Alv
  CALL SCREEN 100.

*******************************************************************************************
* Selecao
*******************************************************************************************
FORM f_selecao.

  FREE: t_reguh,
        t_bkpf.

*-Montar Range
*-Montar Range
  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_COMPLETE'.
  APPEND r_name.

*-Montar Range
  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_DEFECT'.
  APPEND r_name.

*-Montar Range
  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_GENERATE'.
  APPEND r_name.

*-Montar Range
  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_WORKFLOW_PROCESS'.
  APPEND r_name.

*-Seleciona tabela Icones
  SELECT id
         name
    FROM icon
    INTO TABLE t_icon
   WHERE name IN r_name.

*---notas fiscais
  SELECT laufd laufi xvorl zbukr
         lifnr kunnr empfg vblnr
         znme1 rbetr valut hbkid
         zaldt
    FROM reguh
    INTO TABLE t_reguh
   WHERE laufd     = p_laufd
     AND laufi     = p_laufi
     AND xvorl     = abap_off
     AND zbukr     = p_zbukr.

* DELETE t_reguh WHERE xvorl = abap_true.

  LOOP AT t_reguh INTO w_reguh.
    l_tabix = sy-tabix.

    PACK w_reguh-vblnr TO l_vblnr.
    CONDENSE l_vblnr.
    IF l_vblnr(1) <> '2'.
      DELETE t_reguh INDEX l_tabix.
    ENDIF.
  ENDLOOP.

  CHECK t_reguh[] IS NOT INITIAL.

*-------------------------------------
*-selecao movimento icms
*-------------------------------------
  SELECT bukrs belnr
         gjahr stblg
    FROM bkpf
    INTO TABLE t_bkpf
     FOR ALL ENTRIES IN t_reguh
   WHERE bukrs  = t_reguh-zbukr
     AND belnr  = t_reguh-vblnr
     AND gjahr  = t_reguh-zaldt(4).

  SELECT *
    FROM zfit0167
    INTO TABLE t_zfit0167
     FOR ALL ENTRIES IN t_reguh
   WHERE laufd  = t_reguh-laufd
     AND laufi  = t_reguh-laufi
     AND zbukr  = t_reguh-zbukr
     AND vblnr  = t_reguh-vblnr.

*-------------------------------------
*-sort tables
*-------------------------------------
  SORT t_bkpf     BY bukrs belnr gjahr.
  SORT t_zfit0167 BY laufd laufi zbukr vblnr.
  SORT t_icon     BY name.

ENDFORM.

*******************************************************************************************
* MOnta saida
*******************************************************************************************
FORM f_monta_saida.

  FREE: t_alv.

  LOOP AT t_reguh INTO w_reguh.

    FREE: w_alv,
          w_bkpf,
          l_icon_name,
          l_lista,
          w_icon,
          w_zfit0167.

    READ TABLE t_bkpf INTO w_bkpf WITH KEY bukrs = w_reguh-zbukr
                                           belnr = w_reguh-vblnr
                                           gjahr = w_reguh-zaldt(4)
                                  BINARY SEARCH.

    READ TABLE t_zfit0167 INTO w_zfit0167
                          WITH KEY laufd = w_reguh-laufd
                                   laufi = w_reguh-laufi
                                   zbukr = w_reguh-zbukr
                                   vblnr = w_reguh-vblnr
                          BINARY SEARCH.

    IF w_bkpf-stblg IS INITIAL.
      MOVE w_zfit0167-belnr_fb08 TO w_bkpf-stblg.
    ENDIF.

    MOVE-CORRESPONDING w_reguh   TO w_alv.
    MOVE w_bkpf-stblg            TO w_alv-stblg.
    MOVE w_zfit0167-erro         TO w_alv-erro.
    MOVE w_zfit0167-message      TO w_alv-message.

    CONCATENATE w_reguh-zbukr
                w_reguh-laufd
                w_reguh-laufi
           INTO w_alv-chave.

    IF     w_bkpf-stblg IS NOT INITIAL.
      l_icon_name = 'ICON_COMPLETE'.
    ELSEIF w_zfit0167-erro = 'P'.
      l_icon_name = 'ICON_WORKFLOW_PROCESS'.
    ELSEIF w_zfit0167-erro = 'E'.
      l_icon_name = 'ICON_DEFECT'.
    ELSEIF w_zfit0167-erro = 'S'.
      l_icon_name = 'ICON_COMPLETE'.
    ELSE.
      l_icon_name = 'ICON_GENERATE'.
    ENDIF.

    READ TABLE t_icon INTO w_icon WITH KEY name = l_icon_name
                                  BINARY SEARCH.
    w_alv-status = w_icon-id.

    APPEND w_alv                 TO t_alv.
  ENDLOOP.

ENDFORM.

*******************************************************************************************
* Module  PBO  OUTPUT
*******************************************************************************************
MODULE status_0100 OUTPUT.

  FREE: t_exctab.

  SET PF-STATUS 'ZFIR0085' EXCLUDING t_exctab.
  SET TITLEBAR  'ZFIR0085'.

  IF tree1 IS INITIAL.
    PERFORM init_tree.
    PERFORM init_custom.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.                             " PBO  OUTPUT

*******************************************************************************************
* Module  PAI  INPUT
*******************************************************************************************
MODULE user_command_0100 INPUT.

  CASE ok_code.

    WHEN '&REFRESH'.
      PERFORM f_selecao.
      PERFORM f_monta_saida.
      CALL METHOD cl_gui_cfw=>dispatch.
      CALL METHOD tree1->delete_all_nodes.
      PERFORM create_hierarchy USING '1'.

    WHEN '&FBRA'.
      PERFORM f_selec_linhas.
      CALL METHOD cl_gui_cfw=>dispatch.
      CALL METHOD tree1->delete_all_nodes.
      PERFORM create_hierarchy USING '1'.

      IF t_alv[] IS INITIAL.
        MESSAGE s000(fb) WITH text-200.
        EXIT.
      ENDIF.

      PERFORM f_executa_fbra.

    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      PERFORM exit_program.

    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.

  ENDCASE.

  CLEAR ok_code.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.                             " PAI  INPUT

*******************************************************************************************
* Form  SELECIONA LINHAS
*******************************************************************************************
FORM f_executa_fbra.

  LOOP AT t_alv INTO w_alv WHERE marca = abap_on.

    l_tabix    = sy-tabix.
    l_taskname = l_tabix.

    CHECK w_alv-stblg IS INITIAL.

*---atualiza status
    w_zfit0167-mandt       = sy-mandt.
    w_zfit0167-laufd       = w_alv-laufd.
    w_zfit0167-laufi       = w_alv-laufi.
    w_zfit0167-zbukr       = w_alv-zbukr.
    w_zfit0167-vblnr       = w_alv-vblnr.
    w_zfit0167-erro        = 'P'.
    MODIFY zfit0167     FROM w_zfit0167.

*---------------------------------
*-- EXECUTA
*---------------------------------
    CALL FUNCTION 'ZFIMF_ESTORNA_PAGAMENTO'
      DESTINATION l_dest
      STARTING NEW TASK l_taskname
      EXPORTING
        i_zbukr               = w_alv-zbukr
        i_laufd               = w_alv-laufd
        i_laufi               = w_alv-laufi
        i_vblnr               = w_alv-vblnr
        i_zaldt               = w_alv-zaldt
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2.

    COMMIT WORK AND WAIT.

  ENDLOOP.

ENDFORM.

*******************************************************************************************
* Form  SELECIONA LINHAS
*******************************************************************************************
FORM f_selec_linhas.

  FREE: t_checked_items,
        t_node_key,
        t_node_key_sel.

*-checa linhas selecionadas
  CALL METHOD tree1->get_checked_items(  "tree1
    IMPORTING
      et_checked_items = t_checked_items ).

  DELETE t_checked_items WHERE fieldname <> '&Hierarchy'.

  IF t_checked_items[] IS INITIAL.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = 'DUMMY'.  " triggers PAI of the screen
  ENDIF.

*-------------------------------
* recupera linhas marcadas
*-------------------------------
  LOOP AT t_checked_items INTO w_checked_items.
    LOOP AT t_node_chave INTO w_node_chave WHERE level2 = w_checked_items-nodekey.
      READ TABLE t_alv INTO w_alv INDEX w_node_chave-linha.
      IF sy-subrc = 0.
        IF w_alv-stblg IS NOT INITIAL.
          CONTINUE.
        ENDIF.
        l_tabix     = sy-tabix.
        l_icon_name = 'ICON_WORKFLOW_PROCESS'.
        READ TABLE t_icon INTO w_icon WITH KEY name = l_icon_name
                                      BINARY SEARCH.
        w_alv-marca  = abap_true.
        w_alv-status = w_icon-id.
        MODIFY t_alv FROM w_alv INDEX l_tabix.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM build_fieldcatalog.

* get fieldcatalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZFIS0010'
    CHANGING
      ct_fieldcat      = t_fieldcatalog.

  SORT t_fieldcatalog BY scrtext_l.

* change fieldcatalog
  DATA: ls_fieldcatalog TYPE lvc_s_fcat.

  LOOP AT t_fieldcatalog INTO ls_fieldcatalog.
    CASE ls_fieldcatalog-fieldname.
      WHEN 'STATUS'.
        ls_fieldcatalog-col_pos   = 1.
        ls_fieldcatalog-icon      = abap_true.
        ls_fieldcatalog-outputlen = 5.
      WHEN 'VBLNR'.
        ls_fieldcatalog-col_pos   = 2.
        ls_fieldcatalog-outputlen = 25.
        ls_fieldcatalog-hotspot   = abap_true.
        ls_fieldcatalog-coltext   = text-100.
      WHEN 'STBLG'.
        ls_fieldcatalog-col_pos   = 3.
        ls_fieldcatalog-outputlen = 25.
        ls_fieldcatalog-hotspot   = abap_true.
        ls_fieldcatalog-coltext   = text-101.
      WHEN 'RBETR'.
        ls_fieldcatalog-col_pos   = 4.
        ls_fieldcatalog-outputlen = 30.
        ls_fieldcatalog-do_sum    = abap_true.
        ls_fieldcatalog-coltext   = text-106.
      WHEN 'ZALDT'.
        ls_fieldcatalog-col_pos   = 5.
        ls_fieldcatalog-outputlen = 28.
        ls_fieldcatalog-coltext   = text-102.
      WHEN 'VALUT'.
        ls_fieldcatalog-col_pos   = 6.
        ls_fieldcatalog-outputlen = 28.
        ls_fieldcatalog-coltext   = text-103.
      WHEN 'LIFNR'.
        ls_fieldcatalog-col_pos   = 7.
        ls_fieldcatalog-outputlen = 20.
        ls_fieldcatalog-coltext   = text-104.
      WHEN 'ZNME1'.
        ls_fieldcatalog-col_pos   = 8.
        ls_fieldcatalog-outputlen = 57.
        ls_fieldcatalog-coltext   = text-105.
      WHEN 'MESSAGE'.
        ls_fieldcatalog-col_pos   = 10.
        ls_fieldcatalog-outputlen = 150.
        ls_fieldcatalog-coltext   = text-107.
      WHEN OTHERS.
        ls_fieldcatalog-no_out = abap_true.
    ENDCASE.
    MODIFY t_fieldcatalog FROM ls_fieldcatalog.
  ENDLOOP.
ENDFORM.                               " build_fieldcatalog
*&---------------------------------------------------------------------*
*&      Form  build_hierarchy_header
*&---------------------------------------------------------------------*
*       build hierarchy-header-information
*----------------------------------------------------------------------*
*      -->P_L_HIERARCHY_HEADER  strucxture for hierarchy-header
*----------------------------------------------------------------------*
FORM build_hierarchy_header CHANGING
                               p_hierarchy_header TYPE treev_hhdr.

  p_hierarchy_header-heading = 'Empresa / Dt.Proposta / Identificador'. "#EC NOTEXT
* p_hierarchy_header-tooltip = 'This is the Hierarchy Header !'.  "#EC NOTEXT
  p_hierarchy_header-width = 45.
  p_hierarchy_header-width_pix = ''.

ENDFORM.                               " build_hierarchy_header
*&---------------------------------------------------------------------*
*&      Form  exit_program
*&---------------------------------------------------------------------*
*       free object and leave program
*----------------------------------------------------------------------*
FORM exit_program.

  CALL METHOD tree1->free.
  LEAVE TO SCREEN 0.

ENDFORM.                               " exit_program

*&---------------------------------------------------------------------*
*&      Form  build_header
*&---------------------------------------------------------------------*
FORM build_comment USING
      pt_list_commentary TYPE slis_t_listheader
      p_logo             TYPE sdydo_value.

* LIST HEADING LINE: TYPE H
*  CLEAR ls_line.
*  ls_line-typ  = 'H'.
** LS_LINE-KEY:  NOT USED FOR THIS TYPE
*  ls_line-info = text-070.
*  APPEND ls_line TO pt_list_commentary.

*  CLEAR ls_line.
*  ls_line-typ   = 'S'.
*  ls_line-key   = 'Empresa:'.
*  ls_line-info  = p_zbukr.
*  APPEND ls_line TO pt_list_commentary.
*
*  CLEAR ls_line.
*  ls_line-typ   = 'S'.
*  ls_line-key   = 'Data de Execução:'.
*  ls_line-info  = p_laufd.
*  APPEND ls_line TO pt_list_commentary.
*
*  CLEAR ls_line.
*  ls_line-typ   = 'S'.
*  ls_line-key   = 'Identificação:'.
*  ls_line-info  = p_laufi.
*  APPEND ls_line TO pt_list_commentary.
*
*  ls_line-key  = 'time'.
*  ls_line-info = '2.00 pm'.                                 "#EC NOTEXT
*  APPEND ls_line TO pt_list_commentary.
** ACTION LINE: TYPE A
*  CLEAR ls_line.
*  ls_line-typ  = 'A'.
** LS_LINE-KEY:  NOT USED FOR THIS TYPE
*  ls_line-info = 'actual data'.                             "#EC NOTEXT
*  APPEND ls_line TO pt_list_commentary.
*
*   p_logo = 'ZLOGO_COMPANY'.
ENDFORM.                    "build_comment

*&---------------------------------------------------------------------*
*&      Form  create_hierarchy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_hierarchy USING p_evento.

  DATA: l_carrid_key TYPE lvc_nkey,
        l_connid_key TYPE lvc_nkey,
        l_last_key   TYPE lvc_nkey,
        l_last_key2  TYPE lvc_nkey,
        l_last_key3  TYPE lvc_nkey.

  FREE: t_chave,
        t_node_key,
        t_node_key_det,
        t_node_chave,
        l_chave_ant.

  SORT t_alv BY chave.

  LOOP AT t_alv INTO w_alv.
    l_tabix = sy-tabix.

    IF w_alv-chave <> l_chave_ant.
      PERFORM add_carrid_line USING    w_alv
                                       ''
                              CHANGING l_carrid_key.
    ENDIF.

    PERFORM add_complete_line USING  w_alv
                                     l_carrid_key
                            CHANGING l_connid_key.

*-----------
    w_node_chave-level1     = l_carrid_key.
    w_node_chave-level2     = l_connid_key.
    w_node_chave-linha      = l_tabix.
    w_node_chave-chave      = l_chave.
    w_node_chave-stblg      = w_alv-stblg.
    w_node_chave-erro       = w_alv-erro.
    APPEND w_node_chave    TO t_node_chave.
*-----------

    APPEND w_chave         TO t_chave.

    l_chave_ant = w_alv-chave.

  ENDLOOP.

* calculate totals
  CALL METHOD tree1->update_calculations.

* this method must be called to send the data to the frontend
  CALL METHOD tree1->frontend_update.

*------------------------------------------------
* traz todos marcados
*------------------------------------------------
  CLEAR w_item_layout.
  w_item_layout-chosen   = abap_on.
  w_item_layout-u_chosen = abap_on.

  LOOP AT t_node_key INTO w_node_key.
    LOOP AT t_node_chave INTO w_node_chave WHERE level1 = w_node_key.
      IF w_node_chave-stblg IS NOT INITIAL OR
         w_node_chave-erro  = 'P'.
        CONTINUE.
      ENDIF.

      CALL METHOD tree1->change_item
        EXPORTING
          i_node_key     = w_node_chave-level2 "w_node_key   "<-- loop at the nodes and repeat for each one with a checkbox
          i_fieldname    = tree1->c_hierarchy_column_name
          i_data         = ' ' "w_node_chave-chave
          is_item_layout = w_item_layout
        EXCEPTIONS
          node_not_found = 1
          OTHERS         = 2.
    ENDLOOP.
  ENDLOOP.

*------------------------------------------------
*-Expandir nos selecionados
*------------------------------------------------
  IF p_evento = '1'.
    CALL METHOD tree1->expand_nodes
      EXPORTING
        it_node_key = t_node_key.

    SORT t_node_key     ASCENDING.
    READ TABLE t_node_key     INTO w_node_key INDEX 1.
  ELSE.
    CALL METHOD tree1->expand_nodes
      EXPORTING
        it_node_key = t_node_key_aux.

    SORT t_node_key_aux ASCENDING.
    READ TABLE t_node_key_aux INTO w_node_key INDEX 1.
  ENDIF.

  IF sy-subrc = 0.
    CALL METHOD tree1->set_top_node
      EXPORTING
        i_node_key = w_node_key.
  ENDIF.

ENDFORM.                               " create_hierarchy

*&---------------------------------------------------------------------*
*&      Form  add_carrid_line
*&---------------------------------------------------------------------*
*       add hierarchy-level 1 to tree
*----------------------------------------------------------------------*
*      -->P_LS_SFLIGHT  sflight
*      -->P_RELEATKEY   relatkey
*     <-->p_node_key    new node-key
*----------------------------------------------------------------------*
FORM add_carrid_line USING     ps_alv         TYPE zfis0010
                               p_relat_key    TYPE lvc_nkey
                     CHANGING  p_node_key     TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_alv      TYPE zfis0010.
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.
  DATA: ls_node          TYPE lvc_s_layn.
  DATA: l_data(10)       TYPE c.

* set item-layout
*  CLEAR ls_item_layout.
*  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
*  ls_item_layout-class     = cl_gui_column_tree=>item_class_checkbox.
** ls_item_layout-style     = cl_gui_column_tree=>style_intensifd_critical.
*  ls_item_layout-editable  = abap_true.
*  APPEND ls_item_layout TO lt_item_layout.

* add node
  CONCATENATE w_alv-laufd+6(2) '.' w_alv-laufd+4(2) '.' w_alv-laufd(4)
         INTO l_data.

  CONCATENATE w_alv-zbukr '/'
              l_data      '/'
              w_alv-laufi
         INTO l_chave
         SEPARATED BY space.

  l_node_text       = l_chave.
  ls_node-n_image   = space.
  ls_node-exp_image = space.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_alv
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

  w_node_key          = p_node_key.
  APPEND w_node_key  TO t_node_key.

ENDFORM.                               " add_carrid_line

*&---------------------------------------------------------------------*
*&      Form  add_cmplete_line
*&---------------------------------------------------------------------*
*       add hierarchy-level 3 to tree
*----------------------------------------------------------------------*
*      -->P_LS_SFLIGHT  sflight
*      -->P_RELEATKEY   relatkey
*     <-->p_node_key    new node-key
*----------------------------------------------------------------------*
FORM add_complete_line USING   ps_alv        TYPE zfis0010
                               p_relat_key   TYPE lvc_nkey
                     CHANGING  p_node_key    TYPE lvc_nkey.

  DATA: l_node_text        TYPE lvc_value.
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.
  DATA: ls_node            TYPE lvc_s_layn.

* set item-layout
  CLEAR ls_item_layout.
  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
  ls_item_layout-class     = cl_gui_column_tree=>item_class_checkbox.
  ls_item_layout-editable  = abap_false.
* IF w_alv-stblg IS NOT INITIAL OR w_alv-erro = 'P'.
*   ls_item_layout-editable = abap_false.
* ELSE.
*   ls_item_layout-editable = abap_true.
* ENDIF.
  APPEND ls_item_layout TO lt_item_layout.

  ls_item_layout-class = cl_gui_column_tree=>item_class_link. "This is the same style like HOTSPOT
  APPEND ls_item_layout TO lt_item_layout.
  ls_item_layout-class = cl_gui_column_tree=>eventid_link_click.
  APPEND ls_item_layout TO lt_item_layout.
  ls_item_layout-class = cl_gui_column_tree=>eventid_item_double_click.
  APPEND ls_item_layout TO lt_item_layout.

  ls_node-n_image   = space.
  ls_node-exp_image = space.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      is_outtab_line   = ps_alv
      i_node_text      = l_node_text
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

  w_chave-line        = l_tabix.
  w_chave-index_key   = p_node_key.

  w_node_key_det          = p_node_key.
  APPEND w_node_key_det  TO t_node_key_det.

ENDFORM.                               " add_complete_line

*&---------------------------------------------------------------------*
*&      Form  register_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_events.
* define the events which will be passed to the backend
  DATA: lt_events        TYPE cntl_simple_events,
        l_event          TYPE cntl_simple_event,
        l_event_receiver TYPE REF TO lcl_tree_event_receiver. " lcl_tree_event_receiver.

* define the events which will be passed to the backend
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_checkbox_change.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_header_context_men_req.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_header_click.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_keypress.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_link_click.  "Link click for Sintetico (Call Display Doc Txn)
  l_event-appl_event = 'X'.
  APPEND l_event TO lt_events.

  CALL METHOD tree1->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.

  CREATE OBJECT l_event_receiver ." exporting 'X'.
  SET HANDLER l_event_receiver->handle_link_click FOR tree1."g_alv_tree.

* set Handler
  CREATE OBJECT l_event_receiver.
  SET HANDLER l_event_receiver->handle_node_ctmenu_request
                                                        FOR tree1.
  SET HANDLER l_event_receiver->handle_node_ctmenu_selected
                                                        FOR tree1.
  SET HANDLER l_event_receiver->handle_item_ctmenu_request
                                                        FOR tree1.
  SET HANDLER l_event_receiver->handle_item_ctmenu_selected
                                                        FOR tree1.
ENDFORM.                               " register_events
*&---------------------------------------------------------------------*
*&      Form  change_toolbar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_toolbar.

* get toolbar control
  CALL METHOD tree1->get_toolbar_object
    IMPORTING
      er_toolbar = mr_toolbar.

  CHECK NOT mr_toolbar IS INITIAL.

* add seperator to toolbar
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = ''
      icon      = ''
      butn_type = cntb_btype_sep
      text      = ''
      quickinfo = 'This is a Seperator'.                    "#EC NOTEXT

* add Standard Button to toolbar (for Delete Subtree)
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = '&OPENTREE'
      icon      = '@4H@'
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = 'Open All'.                               "#EC NOTEXT

* add Standard Button to toolbar (for Delete Subtree)
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = '&CLOSETREE'
      icon      = '@4I@'
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = 'Close All'.                              "#EC NOTEXT

* add Standard Button to toolbar (for Delete Subtree)
*  CALL METHOD mr_toolbar->add_button
*    EXPORTING
*      fcode     = '&SELALL'
*      icon      = '@4B@'
*      butn_type = cntb_btype_button
*      text      = ''
*      quickinfo = 'Sel All'.                                "#EC NOTEXT

* add Standard Button to toolbar (for Delete Subtree)
*  CALL METHOD mr_toolbar->add_button
*    EXPORTING
*      fcode     = '&DELALL'
*      icon      = '@4D@'
*      butn_type = cntb_btype_button
*      text      = ''
*      quickinfo = 'Del All'.                                "#EC NOTEXT
*
** add seperator to toolbar
*  CALL METHOD mr_toolbar->add_button
*    EXPORTING
*      fcode     = ''
*      icon      = ''
*      butn_type = cntb_btype_sep
*      text      = ''
*      quickinfo = 'This is a Seperator'.                    "#EC NOTEXT
*
** add Standard Button to toolbar (for Delete Subtree)
*  CALL METHOD mr_toolbar->add_button
*    EXPORTING
*      fcode     = '&EXCEL'
*      icon      = '@J2@'
*      butn_type = cntb_btype_button
*      text      = ''
*      quickinfo = 'Excel'.                                  "#EC NOTEXT
*
*** add Dropdown Button to toolbar (for Insert Line)
*  CALL METHOD mr_toolbar->add_button
*    EXPORTING
*      fcode     = '&PREV_TREE'
*      icon      = '@4I@'
*      butn_type = cntb_btype_button "cntb_btype_dropdown
*      text      = ''
*      quickinfo = 'Prev Tree'.                              "#EC NOTEXT

* set event-handler for toolbar-control
  CREATE OBJECT toolbar_event_receiver.
  SET HANDLER toolbar_event_receiver->on_function_selected
                                                      FOR mr_toolbar.
  SET HANDLER toolbar_event_receiver->on_toolbar_dropdown
                                                      FOR mr_toolbar.

ENDFORM.                               " change_toolbar
*&---------------------------------------------------------------------*
*&      Form  init_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree.

* create fieldcatalog for structure sflight
  PERFORM build_fieldcatalog.

* create container for alv-tree
  DATA: l_tree_container_name(30) TYPE c,
        l_custom_container        TYPE REF TO cl_gui_custom_container.
  l_tree_container_name = 'TREE1'.

  IF sy-batch IS INITIAL.
    CREATE OBJECT l_custom_container
      EXPORTING
        container_name              = l_tree_container_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc <> 0.
      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.
  ENDIF.

* create tree control
  CREATE OBJECT tree1
    EXPORTING
      parent                      = l_custom_container
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
      item_selection              = abap_true               "YI3K118558
      no_html_header              = 'X'
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

* create Hierarchy-header
  DATA l_hierarchy_header TYPE treev_hhdr.
  PERFORM build_hierarchy_header CHANGING l_hierarchy_header.

* create info-table for html-header
  DATA: lt_list_commentary TYPE slis_t_listheader,
        l_logo             TYPE sdydo_value.
  PERFORM build_comment USING
                 lt_list_commentary
                 l_logo.

* repid for saving variants
  DATA: ls_variant TYPE disvariant.
  ls_variant-report = sy-repid.

* create emty tree-control
  CALL METHOD tree1->set_table_for_first_display
    EXPORTING
      is_hierarchy_header = l_hierarchy_header
      it_list_commentary  = lt_list_commentary
      i_logo              = l_logo
      i_background_id     = 'ALV_BACKGROUND'
      i_save              = 'A'
      is_variant          = ls_variant
    CHANGING
      it_outtab           = gt_alv
      it_fieldcatalog     = t_fieldcatalog.

* create hierarchy
  PERFORM create_hierarchy USING '1'.

* add own functioncodes to the toolbar
  PERFORM change_toolbar.

* register events
  PERFORM register_events.                                  "YI3K118558

* adjust column_width
*  CALL METHOD tree1->column_optimize.

ENDFORM.                    " init_tree

*&---------------------------------------------------------------------*
*&      Form  init_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_custom.

  DATA:  w_layout  TYPE lvc_s_layo.
  DATA: t_fcat     TYPE lvc_t_fcat,
        w_fcat     TYPE lvc_s_fcat,
        t_sort     TYPE lvc_t_sort,
        ls_celltab TYPE lvc_s_styl,
        lt_celltab TYPE lvc_t_styl,
        l_data(10) TYPE c.
  DATA: ls_variant     TYPE disvariant.

  TYPES: BEGIN OF ty_saida,
           texto1(20)  TYPE c,
           texto2(20)  TYPE c,
           icon        TYPE icon-id,
           texto3(100) TYPE c,
           celltab     TYPE lvc_t_styl.
  TYPES: END OF ty_saida.

  DATA: t_saida TYPE TABLE OF ty_saida.
  DATA: w_saida TYPE ty_saida.

  ls_variant-report = sy-repid.

  ls_celltab-style = '00000121'.
  APPEND ls_celltab TO lt_celltab.

  CONCATENATE p_laufd+6(2) '.' p_laufd+4(2) '.' p_laufd(4)
         INTO l_data.

  w_layout-cwidth_opt = ' '.
  w_layout-zebra      = ' '.
  w_layout-no_toolbar = 'X'.
  w_layout-no_hgridln = 'X'.
  w_layout-no_vgridln = 'X'.
  w_layout-no_headers = 'X'.
  w_layout-stylefname = 'CELLTAB'.

  w_fcat-fieldname = 'TEXTO1'.
  w_fcat-outputlen = 18.
  APPEND w_fcat   TO t_fcat.
  w_fcat-fieldname = 'TEXTO2'.
  w_fcat-outputlen = 93.
  APPEND w_fcat   TO t_fcat.
  w_fcat-fieldname = 'ICON'.
  w_fcat-outputlen = 5.
  APPEND w_fcat   TO t_fcat.
  w_fcat-fieldname = 'TEXTO3'.
  w_fcat-outputlen = 160.
  APPEND w_fcat   TO t_fcat.

  CLEAR w_saida.
  w_saida-celltab = lt_celltab.
  w_saida-texto3  = 'Legenda'.
  APPEND w_saida TO t_saida.

  READ TABLE t_icon INTO w_icon WITH KEY name = 'ICON_GENERATE'.

  CLEAR w_saida.
  w_saida-celltab   = lt_celltab.
  w_saida-texto1 = 'Empresa:'.
  w_saida-texto2 = p_zbukr.
  w_saida-icon   = w_icon-id.
  w_saida-texto3 = 'Gerar Estorno via Transação FBRA'.
  APPEND w_saida TO t_saida.

  READ TABLE t_icon INTO w_icon WITH KEY name = 'ICON_WORKFLOW_PROCESS'.

  CLEAR w_saida.
  w_saida-celltab   = lt_celltab.
  w_saida-texto1 = 'Data de Execução:'.
  w_saida-texto2 = l_data.
  w_saida-icon   = w_icon-id.
  w_saida-texto3 = 'Em Processamento'.
  APPEND w_saida TO t_saida.

  READ TABLE t_icon INTO w_icon WITH KEY name = 'ICON_COMPLETE'.

  CLEAR w_saida.
  w_saida-celltab   = lt_celltab.
  w_saida-texto1 = 'Identificação:'.
  w_saida-texto2 = p_laufi.
  w_saida-icon   = w_icon-id.
  w_saida-texto3 = 'Estornos Finalizados com Sucesso'.
  APPEND w_saida TO t_saida.

  READ TABLE t_icon INTO w_icon WITH KEY name = 'ICON_DEFECT'.

  CLEAR w_saida.
  w_saida-celltab   = lt_celltab.
  w_saida-icon   = w_icon-id.
  w_saida-texto3 = 'Estornos Finalizados com Erro'.
  APPEND w_saida TO t_saida.

  CLEAR w_saida.
  APPEND w_saida TO t_saida.
  CLEAR w_saida.
  APPEND w_saida TO t_saida.

** create container for alv-tree
*  DATA: l_tree_container_name(30) TYPE c,
*        l_custom_container        TYPE REF TO cl_gui_custom_container.
*  l_tree_container_name = 'CONTAINER'.

  IF v_grid IS INITIAL.
    CREATE OBJECT v_grid
      EXPORTING
        i_parent          = v_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

* create emty tree-control
    CALL METHOD v_grid->set_table_for_first_display
      EXPORTING
*       IS_VARIANT                    = W_VARIANT
        i_save                        = ' '
        is_layout                     = w_layout
      CHANGING
        it_fieldcatalog               = t_fcat
        it_outtab                     = t_saida
        it_sort                       = t_sort
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                    " init_tree


*******************************************************************************************
*******************************************************************************************
