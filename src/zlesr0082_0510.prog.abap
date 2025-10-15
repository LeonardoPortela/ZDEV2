*----------------------------------------------------------------------*
***INCLUDE ZLESR0082_0510 .
*----------------------------------------------------------------------*

TABLES: zlest0103.

CONSTANTS: ok_cg_margead TYPE sy-ucomm VALUE 'CG_MARGEAD',
           ok_ref_marad  TYPE sy-ucomm VALUE 'REF_MARAD',
           ok_new_margem TYPE sy-ucomm VALUE 'NEW_MARGEM',
           ok_del_margem TYPE sy-ucomm VALUE 'DEL_MARGEM',
           ok_log_margem TYPE sy-ucomm VALUE 'LOG_MARGEM'. "US - 76561 - CBRAND

DATA: it_exclude_0510    TYPE ui_functions,
      wa_exclude_0510    LIKE LINE OF it_exclude_0510,
      ctl_alv_0510       TYPE REF TO cl_gui_alv_grid,
      ctl_con_0510       TYPE REF TO cl_gui_custom_container,
      gs_lay_0510        TYPE lvc_s_layo,
      gs_var_0510        TYPE disvariant,
      gs_scroll_col_0510 TYPE lvc_s_col,
      gs_scroll_row_0510 TYPE lvc_s_roid,
      it_catalog_0510    TYPE lvc_t_fcat.

DATA: it_0510          TYPE TABLE OF zde_zlest0103_alv WITH HEADER LINE,
      it_0510_sel      TYPE TABLE OF zde_zlest0103_alv WITH HEADER LINE,
      vg_empresa       TYPE butxt,
      vg_local_negocio TYPE name1,
      vg_fornecedor    TYPE name1_gp.


*&--------------------------------------------------------------------&*
*& TYPES
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_dbtablog,
         logdate  TYPE dbtablog-logdate,
         logtime  TYPE dbtablog-logtime,
         logid    TYPE dbtablog-logid,
         tabname  TYPE dbtablog-tabname,
         logkey   TYPE dbtablog-logkey,
         hostname TYPE dbtablog-hostname,
         username TYPE dbtablog-username,
         tcode    TYPE dbtablog-tcode,
         progname TYPE dbtablog-progname,
         optype   TYPE dbtablog-optype,
         versno   TYPE dbtablog-versno,
         language TYPE dbtablog-language,
         dataln   TYPE dbtablog-dataln,
         logdata  TYPE dbtablog-logdata,
       END OF ty_dbtablog,

       BEGIN OF ty_dd02t,
         tabname    TYPE dd02t-tabname,
         ddtext     TYPE dd02t-ddtext,
         ddlanguage TYPE dd02t-ddlanguage,
       END OF ty_dd02t,

       BEGIN OF ty_saida_mod,
         data        TYPE zfit0034-data,
         hora        TYPE zfit0034-hora,
         usuario     TYPE zfit0034-usuario,
         transacao   TYPE zfit0034-transacao,
         tabela      TYPE zfit0034-tabela,
         desc_tabela TYPE zfit0034-desc_tabela,
         tipo_modif  TYPE c LENGTH 15,
         logkey      TYPE zfit0034-logkey,
       END OF ty_saida_mod.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0510  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0510 OUTPUT.

  "Somente em Debugg""""""""""""""""""""""""""""" Para Carga
  wa_exclude_05xx = ok_cg_margead.
  APPEND wa_exclude_05xx TO it_exclude_05xx.
  """"""""""""""""""""""""""""""""""""""""""""""" Para Carga

  SET PF-STATUS 'PF0510' EXCLUDING it_exclude_05xx.
  SET TITLEBAR 'TB0510'.

  IF ctl_con_0510 IS INITIAL.

    PERFORM consultar_margens.

    CREATE OBJECT ctl_con_0510
      EXPORTING
        container_name = 'TL0510'.

    CREATE OBJECT ctl_alv_0510
      EXPORTING
        i_parent = ctl_con_0510.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZDE_ZLEST0103_ALV'
      CHANGING
        ct_fieldcat      = it_catalog_0510.

    gs_lay_0510-sel_mode   = 'A'.
    gs_lay_0510-zebra      = abap_true.

    CALL METHOD ctl_alv_0510->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0510
        is_variant           = gs_var_0510
        i_default            = space
        it_toolbar_excluding = it_exclude_0510
      CHANGING
        it_fieldcatalog      = it_catalog_0510
        it_outtab            = it_0510[].

    CALL METHOD ctl_alv_0510->refresh_table_display.

    "CREATE OBJECT EVENT_HANDLER_0500.
    "SET HANDLER EVENT_HANDLER_0500->HANDLE_DOUBLE_CLICK FOR CTL_ALV_0500.

  ELSE.
    CALL METHOD ctl_alv_0510->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_0510->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0510
      es_row_no   = gs_scroll_row_0510.

  "  'TL0500'

ENDMODULE.                 " STATUS_0510  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0510_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0510_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0510_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_0510  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info_0510 INPUT.

  CALL METHOD ctl_alv_0510->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0510
      es_row_no   = gs_scroll_row_0510.

ENDMODULE.                 " GET_SCROLL_INFO_0510  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_0510  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows_0510 INPUT.

  DATA: it_selected_rows_0510 TYPE lvc_t_row,
        wa_selected_rows_0510 TYPE lvc_s_row.

  CLEAR it_selected_rows.

  CALL METHOD ctl_alv_0510->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows_0510.

  CLEAR it_0510_sel[].

  LOOP AT it_selected_rows_0510 INTO wa_selected_rows_0510.
    READ TABLE it_0510 INDEX wa_selected_rows_0510-index.
    MOVE-CORRESPONDING it_0510 TO it_0510_sel.
    APPEND it_0510_sel.
  ENDLOOP.

ENDMODULE.                 " GET_SELECTED_ROWS_0510  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0510  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0510 INPUT.

  CASE ok_code.
    WHEN ok_ref_marad.
      PERFORM consultar_margens.
    WHEN ok_cg_margead.
      PERFORM carga_margens_adiantamento.
    WHEN ok_new_margem.
      PERFORM nova_margem.
    WHEN ok_del_margem.
      PERFORM excluir_margem.
    WHEN ok_log_margem.
      PERFORM log_margem.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0510  INPUT

*&---------------------------------------------------------------------*
*&      Form  CONSULTAR_MARGENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM consultar_margens .

  FIELD-SYMBOLS: <fs_103_alv> TYPE zde_zlest0103_alv.

  DATA: it_lfa1       TYPE TABLE OF lfa1 WITH HEADER LINE,
        it_t001       TYPE TABLE OF t001 WITH HEADER LINE,
        it_j_1bbranch TYPE TABLE OF j_1bbranch WITH HEADER LINE.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_0510
    FROM zlest0103.

  IF it_0510[] IS NOT INITIAL.

    SELECT * INTO TABLE it_t001
      FROM t001
       FOR ALL ENTRIES IN it_0510
     WHERE bukrs EQ it_0510-bukrs.

    SORT it_t001 BY bukrs.

    SELECT * INTO TABLE it_j_1bbranch
      FROM j_1bbranch
       FOR ALL ENTRIES IN it_0510
     WHERE bukrs  EQ it_0510-bukrs
       AND branch EQ it_0510-branch.

    SORT it_j_1bbranch BY bukrs branch.

    SELECT * INTO TABLE it_lfa1
      FROM lfa1
       FOR ALL ENTRIES IN it_0510
     WHERE lifnr EQ it_0510-tdlnr.

    SORT it_lfa1 BY lifnr.

  ENDIF.

  LOOP AT it_0510 ASSIGNING <fs_103_alv>.
    READ TABLE it_t001 WITH KEY bukrs = <fs_103_alv>-bukrs BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_103_alv>-butxt = it_t001-butxt.
    ENDIF.

    READ TABLE it_j_1bbranch WITH KEY bukrs  = <fs_103_alv>-bukrs
                                      branch = <fs_103_alv>-branch
                                      BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_103_alv>-name = it_j_1bbranch-name.
    ENDIF.

    READ TABLE it_lfa1 WITH KEY lifnr = <fs_103_alv>-tdlnr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_103_alv>-name1 = it_lfa1-name1.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CONSULTAR_MARGENS

*&---------------------------------------------------------------------*
*&      Form  CARGA_MARGENS_ADIANTAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carga_margens_adiantamento .

  DATA: it_zlest0001 TYPE TABLE OF zlest0001 WITH HEADER LINE,
        it_zlest0103 TYPE TABLE OF zlest0103 WITH HEADER LINE,
        id_margem	   TYPE z_sequencia.

  PERFORM consultar_margens.
  IF it_0510[] IS INITIAL.

    SELECT * INTO TABLE it_zlest0001
      FROM zlest0001.

    id_margem = 1.

    LOOP AT it_zlest0001.
      CLEAR: it_zlest0103.
      it_zlest0103-id_margem = id_margem.
      it_zlest0103-bukrs     = it_zlest0001-bukrs.
      it_zlest0103-branch    = it_zlest0001-branch.
      it_zlest0103-margadto  = it_zlest0001-margadto.
      APPEND it_zlest0103.
      ADD 1 TO id_margem.
    ENDLOOP.

    IF it_zlest0103[] IS NOT INITIAL.
      MODIFY zlest0103 FROM TABLE it_zlest0103.
      COMMIT WORK.
    ENDIF.

  ENDIF.

ENDFORM.                    " CARGA_MARGENS_ADIANTAMENTO

*&---------------------------------------------------------------------*
*&      Form  NOVA_MARGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM nova_margem .

  CLEAR: zlest0103.

  CALL SCREEN 0511 STARTING AT 15 01.
  PERFORM consultar_margens.

ENDFORM.                    " NOVA_MARGEM

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_MARGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM excluir_margem .

  IF it_0510_sel[] IS INITIAL.
    MESSAGE s085.
    RETURN.
  ENDIF.

  READ TABLE it_0510_sel INDEX 1.

  DELETE FROM zlest0103 WHERE id_margem EQ it_0510_sel-id_margem.
  COMMIT WORK.

  PERFORM consultar_margens.

ENDFORM.                    " EXCLUIR_MARGEM

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0511_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0511_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0511_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0511  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0511 OUTPUT.

  SET PF-STATUS 'PF0501'.
  SET TITLEBAR 'TB0511'.

  IF zlest0103-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt INTO vg_empresa
      FROM t001
     WHERE bukrs EQ zlest0103-bukrs.
  ENDIF.

  IF zlest0103-branch IS NOT INITIAL AND zlest0103-bukrs IS NOT INITIAL.
    SELECT SINGLE name INTO vg_local_negocio
      FROM j_1bbranch
     WHERE bukrs  EQ zlest0103-bukrs
       AND branch EQ zlest0103-branch.
  ENDIF.

  IF zlest0103-tdlnr IS NOT INITIAL.
    SELECT SINGLE name1 INTO vg_fornecedor
      FROM lfa1
     WHERE lifnr EQ zlest0103-tdlnr.
  ENDIF.

ENDMODULE.                 " STATUS_0511  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0511  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0511 INPUT.

  CASE ok_code.
    WHEN 'SALVAR'.
      PERFORM salvar_margem.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0511  INPUT


*&---------------------------------------------------------------------*
*&      Form  SALVAR_MARGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_margem .

  DATA: pid_margem   TYPE z_sequencia,
        wa_zlest0103 TYPE zlest0103.

  CHECK: vg_empresa IS NOT INITIAL AND vg_local_negocio IS NOT INITIAL.

  SELECT SINGLE * INTO wa_zlest0103
    FROM zlest0103
   WHERE bukrs  EQ zlest0103-bukrs
     AND branch EQ zlest0103-branch
     AND tdlnr  EQ zlest0103-tdlnr.

  IF sy-subrc IS INITIAL.
    MESSAGE s095.
  ELSE.

    SELECT MAX( id_margem ) INTO pid_margem
      FROM zlest0103.

    IF pid_margem IS INITIAL.
      pid_margem = 1.
    ELSE.
      ADD 1 TO pid_margem.
    ENDIF.

    zlest0103-id_margem = pid_margem.
*** US - 76561 - Inicio - CBRAND
    zlest0103-usnam_cad = sy-uname.
    zlest0103-dt_cad = sy-datum.
    zlest0103-hr_cad = sy-uzeit.
*** US - 76561 - Fim - CBRAND

    MODIFY zlest0103.
    COMMIT WORK.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " SALVAR_MARGEM

*&---------------------------------------------------------------------*
*&      Module  SET_TROCA_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_troca_info INPUT.
  CLEAR: vg_empresa, vg_local_negocio, vg_fornecedor.
ENDMODULE.                 " SET_TROCA_INFO  INPUT
*&---------------------------------------------------------------------*
*&      Form  LOG_MARGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM log_margem .
  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZLEST0103'
      tabfirst = 'X'.
ENDFORM.
