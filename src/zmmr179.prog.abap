**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*


**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Antonio Rodrigues ( antonio.rodrigues@amaggi.com.br )                |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| DE/PARA Empresas SIS x SAP                                                |*
**/===========================================================================\*

REPORT zmmr179 MESSAGE-ID zcarga.

"Tabelas
TABLES: zmmt0165.

"Tabela Interna Global
DATA: git_zmmt0165 TYPE TABLE OF zmmt0165_out,
      git_save     TYPE TABLE OF zmmt0165,
      git_t001w    TYPE TABLE OF t001w,
      git_t001     TYPE TABLE OF t001.
DATA: git_filtro TYPE zif_screen_linha_filtro_t.

"Work Área
DATA: gwa_zmmt0165 TYPE zmmt0165,
      gwa_stable   TYPE lvc_s_stbl.

"Objetos
DATA: gob_custom_container        TYPE REF TO cl_gui_custom_container,
      gob_dd_document             TYPE REF TO cl_dd_document,
      gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
      gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,
      gob_gui_container_topo      TYPE REF TO cl_gui_container,
      gob_gui_container_filtro    TYPE REF TO cl_gui_container,
      gob_gui_container_logo      TYPE REF TO cl_gui_container,
      gob_gui_container_grid      TYPE REF TO cl_gui_container,
      gob_gui_picture             TYPE REF TO cl_gui_picture,
      git_fcat                    TYPE lvc_t_fcat,
      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid.


AT SELECTION-SCREEN.
  PERFORM fm_at_selection_screen.

START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.

**********************************************************************
* classes / implementacoes
**********************************************************************
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_toolbar.

    DELETE e_object->mt_toolbar WHERE function EQ '&LOCAL&APPEND'.
    DELETE e_object->mt_toolbar WHERE function EQ '&LOCAL&INSERT_ROW'.
    DELETE e_object->mt_toolbar WHERE function EQ '&LOCAL&DELETE_ROW'.
    DELETE e_object->mt_toolbar WHERE function EQ '&LOCAL&COPY_ROW'.

  ENDMETHOD.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD data_changed_finished.

    CHECK e_modified IS NOT INITIAL.

    LOOP AT et_good_cells INTO DATA(wa_good_cells).

      READ TABLE git_zmmt0165 ASSIGNING FIELD-SYMBOL(<f_zmmt0165>) INDEX wa_good_cells-row_id.
      CHECK sy-subrc IS INITIAL.

      <f_zmmt0165>-werks = |{ wa_good_cells-value ALPHA = IN  }|.

      IF <f_zmmt0165>-werks IS NOT INITIAL.

        SELECT SINGLE name1
          FROM t001w
          INTO <f_zmmt0165>-name_sap
        WHERE werks EQ <f_zmmt0165>-werks.

        IF sy-subrc IS NOT INITIAL.

          SELECT SINGLE butxt
            FROM t001
            INTO <f_zmmt0165>-name_sap
          WHERE bukrs EQ <f_zmmt0165>-werks.

        ENDIF.

      ELSE.
        CLEAR <f_zmmt0165>-name_sap.
      ENDIF.

    ENDLOOP.

    PERFORM fm_refresh.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_EMPRESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fm_seleciona_empresa .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .
  PERFORM fm_filtros.
  CALL SCREEN 0100.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_dados_seleciona.
  PERFORM fm_dados_processa.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_processa .

  LOOP AT git_zmmt0165 ASSIGNING FIELD-SYMBOL(<f_zmmt0165>).

    READ TABLE git_t001w INTO DATA(lwa_t001w) WITH KEY werks = <f_zmmt0165>-werks.
    IF sy-subrc IS INITIAL.
      <f_zmmt0165>-name_sap = lwa_t001w-name1.
    ELSE.
      READ TABLE git_t001 INTO DATA(lwa_t001) WITH KEY bukrs = <f_zmmt0165>-werks.
      IF sy-subrc IS INITIAL.
        <f_zmmt0165>-name_sap = lwa_t001-butxt.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona .

  SELECT *
    FROM zmmt0165
    INTO CORRESPONDING FIELDS OF TABLE git_zmmt0165.

  CHECK git_zmmt0165 IS NOT INITIAL.

  SELECT *
    FROM t001w
    INTO TABLE git_t001w
    FOR ALL ENTRIES IN git_zmmt0165
  WHERE werks EQ git_zmmt0165-werks.

  SELECT *
    FROM t001
    INTO TABLE git_t001
    FOR ALL ENTRIES IN git_zmmt0165
  WHERE bukrs EQ git_zmmt0165-werks.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_at_selection_screen .

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_AT_SELECTION_SCREEN_P_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_at_selection_screen_p_bukrs .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100' WITH 'Padrao'.

  PERFORM fm_criar_objetos.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM fm_salvar.
    WHEN 'REFRESH'.
      PERFORM fm_start_of_selection.
      PERFORM fm_refresh.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FM_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos.

  PERFORM fm_cria_fieldcat.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'De/Para Empresas SIS x SAP'
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.

    SET HANDLER lcl_event_handler=>on_data_changed  FOR gob_gui_alv_grid.
    SET HANDLER lcl_event_handler=>on_toolbar   FOR gob_gui_alv_grid.
    SET HANDLER lcl_event_handler=>data_changed_finished FOR gob_gui_alv_grid.

    CALL METHOD gob_gui_alv_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD gob_gui_alv_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    gwa_stable = VALUE #(
                        row = abap_true
                        col = abap_true
                        ).

    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      CHANGING
        it_outtab                     = git_zmmt0165
        it_fieldcatalog               = git_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    PERFORM fm_refresh.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat.

  DATA: lc_col_pos  TYPE lvc_colpos.
  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.
  CLEAR: git_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZMMT0165_OUT'
    CHANGING
      ct_fieldcat      = git_fcat.

  LOOP AT git_fcat ASSIGNING <fs_cat>.

    <fs_cat>-tabname = 'ZMMT0165'.
    <fs_cat>-col_opt = abap_true.

    CASE <fs_cat>-fieldname.
      WHEN 'ID'.
        <fs_cat>-col_pos = 1.
        <fs_cat>-scrtext_s = 'idSIS'.
        <fs_cat>-scrtext_m = 'Id SIS'.
        <fs_cat>-scrtext_l = 'ID SIS'.
        <fs_cat>-reptext = <fs_cat>-scrtext_m.
      WHEN 'NAME_SIS'.
        <fs_cat>-col_pos = 2.
        <fs_cat>-scrtext_s = 'EmpSIS'.
        <fs_cat>-scrtext_m = 'EmpresaSIS'.
        <fs_cat>-scrtext_l = 'Empresa SIS'.
        <fs_cat>-reptext = <fs_cat>-scrtext_m.
      WHEN 'WERKS'.
        <fs_cat>-col_pos = 3.
        <fs_cat>-edit = abap_true.
        <fs_cat>-scrtext_s = 'Cent.'.
        <fs_cat>-scrtext_m = 'Centro'.
        <fs_cat>-scrtext_l = 'Centro SAP'.
        <fs_cat>-reptext = <fs_cat>-scrtext_m.
      WHEN 'NAME_SAP'.
        <fs_cat>-col_pos = 4.
        <fs_cat>-scrtext_s = 'DesCent.'.
        <fs_cat>-scrtext_m = 'Desc.Centro'.
        <fs_cat>-scrtext_l = 'Descrição Centro SAP'.
        <fs_cat>-reptext = <fs_cat>-scrtext_m.
      WHEN 'US_REGISTRO'.
        <fs_cat>-col_pos = 5.
      WHEN 'DT_REGISTRO'.
        <fs_cat>-col_pos = 6.
      WHEN 'HR_REGISTRO'.
        <fs_cat>-col_pos = 7.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_filtros.

  DATA vl_text TYPE TABLE OF textpool.

  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname = sy-repid
      action     = 'SHOW'
      language   = sy-langu
    TABLES
      tpool      = vl_text.

  FREE: git_filtro.

*  LOOP AT SCREEN.
*    git_filtro = VALUE #(
*      ( parametro = '' valor = p_bukrs )
*      ( parametro = '' valor = p_werks )
*    ).
*  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SALVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_salvar .

  FREE git_save.

  LOOP AT git_zmmt0165 INTO DATA(lwa_zmmt0165).
    APPEND VALUE #(
                    id             = lwa_zmmt0165-id
                    name_sis       = lwa_zmmt0165-name_sis
                    werks          = lwa_zmmt0165-werks
                    us_registro    = lwa_zmmt0165-us_registro
                    dt_registro    = lwa_zmmt0165-dt_registro
                    hr_registro    = lwa_zmmt0165-hr_registro
                   ) TO git_save.
  ENDLOOP.


  MODIFY zmmt0165 FROM TABLE git_save.

  IF sy-subrc IS INITIAL.

    COMMIT WORK.
    MESSAGE 'Salvo com Sucesso!' TYPE 'S'.

  ELSE.

    ROLLBACK WORK.
    MESSAGE 'Não foi Possivel Salvar!' TYPE 'W'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_refresh .

  CHECK gob_gui_alv_grid IS NOT INITIAL.

  CALL METHOD gob_gui_alv_grid->refresh_table_display
    EXPORTING
      is_stable = gwa_stable.

ENDFORM.
