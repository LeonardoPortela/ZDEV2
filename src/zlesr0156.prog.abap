*&---------------------------------------------------------------------*
*& Report  ZLESR0156
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0156.

**********************************************************************
* tabelas
**********************************************************************
TABLES: zlest0222, sscrfields.

**********************************************************************
* types
**********************************************************************
TYPES: BEGIN OF ty_rbkp_sel,
         lifnr TYPE rbkp-lifnr,
         xblnr TYPE rbkp-xblnr,
         stblg TYPE rbkp-stblg.
TYPES: END   OF ty_rbkp_sel.

TYPES: BEGIN OF ty_bkpf_sel,
         bukrs TYPE bkpf-bukrs,
         gjahr TYPE bkpf-gjahr,
         awkey TYPE bkpf-awkey.
TYPES: END   OF ty_bkpf_sel.

TYPES: BEGIN OF ty_alv,
         bukrs           TYPE zlest0222-bukrs,
         cnpj_filial     TYPE zlest0222-cnpj_filial,
         cnpj_transbordo TYPE zlest0222-cnpj_transbordo,
         nfps            TYPE zlest0222-nfps,
         data            TYPE zlest0222-data,
         valor_servico   TYPE zlest0222-valor_servico,
         datatransb_de   TYPE zlest0222-datatransb_de,
         datatransb_ate  TYPE zlest0222-datatransb_ate,
         dias_transito   TYPE zlest0222-dias_transito,
         matnr           TYPE zlest0222-matnr,
         valor_dia       TYPE zlest0222-valor_dia,
         pesochegada     TYPE zlest0222-pesochegada,
         vlr_tot_est     TYPE zlest0222-vlr_tot_est,
         ebeln           TYPE ekbe-ebeln,
         belnr           TYPE rbkp-belnr,
         augbl           TYPE bsak-augbl,
         augdt           TYPE bsak-augdt.
TYPES: END   OF ty_alv.

**********************************************************************
* variaveis
**********************************************************************
DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: l_sel_button         TYPE smp_dyntxt,
      l_opcao              TYPE char1,
      lt_fm_name           TYPE sys_callst,
      ls_fm_name           TYPE sys_calls,
      t_zlest0222          TYPE TABLE OF zlest0222,
      w_zlest0222          TYPE zlest0222,
      t_rbkp_sel           TYPE TABLE OF ty_rbkp_sel,
      w_rbkp_sel           TYPE ty_rbkp_sel,
      t_bkpf_sel           TYPE TABLE OF ty_bkpf_sel,
      w_bkpf_sel           TYPE ty_bkpf_sel,
      t_alv                TYPE TABLE OF ty_alv,
      w_alv                TYPE ty_alv,
      l_leave              TYPE syst_ucomm,

*------------------------------------
*---- ALV
*------------------------------------
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      picture              TYPE REF TO cl_gui_picture,
      l_graphic_conv       TYPE i,
      l_graphic_offs       TYPE i,
      graphic_size         TYPE i,
      l_graphic_xstr       TYPE xstring,
      url(255)             TYPE c,
      graphic_url(255),
      t_function           TYPE ui_functions,
      w_function           TYPE ui_func,
*
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_colorcell          TYPE TABLE OF lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
      ok_code              TYPE sy-ucomm.
*
*------------------------------------
*---- figuras
*------------------------------------
DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

DATA: ty_toolbar           TYPE stb_button.
DATA: v_passou TYPE c.



**********************************************************************
* classes / implementacoes
**********************************************************************

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION


CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.



ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_hotspot_click.
  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
*BREAK-POINT.
*      ty_toolbar-icon      = icon_delete_row.
*      ty_toolbar-function  = 'DEL'.
*      ty_toolbar-text      = 'Deletar'.
*      ty_toolbar-butn_type = 0.
*      APPEND ty_toolbar TO e_object->mt_toolbar.
*      CLEAR ty_toolbar.

*    IF     p_popup IS NOT INITIAL.
*      ty_toolbar-icon      = icon_wizard.
*      ty_toolbar-function  = c_func_02.
*      ty_toolbar-text      = p_ti_01.
*      ty_toolbar-butn_type = 0.
*      APPEND ty_toolbar TO e_object->mt_toolbar.
*      CLEAR ty_toolbar.
*    ELSEIF p_ti_01 IS NOT INITIAL.
*      ty_toolbar-icon      = icon_wizard.
*      ty_toolbar-function  = c_func_01.
*      ty_toolbar-text      = p_ti_01.
*      ty_toolbar-butn_type = 0.
*      APPEND ty_toolbar TO e_object->mt_toolbar.
*      CLEAR ty_toolbar.
*    ENDIF.

*    IF p_act_01 is NOT INITIAL.
*      ty_toolbar-icon      = icon_wizard.
*      ty_toolbar-function  = c_action_01.
*      ty_toolbar-text      = p_act_01.
*      ty_toolbar-butn_type = 0.
*      APPEND ty_toolbar TO e_object->mt_toolbar.
*      CLEAR ty_toolbar.
*    ENDIF.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

*    DATA: var_answer TYPE c.
*
*    CASE e_ucomm.
*      WHEN 'DEL'.
*
*        CLEAR: it_sel_rows[], wa_sel_rows.
*
*        CALL METHOD g_grid->get_selected_rows
*          IMPORTING
*            et_index_rows = it_sel_rows.
*
*        IF it_sel_rows[] IS INITIAL.
*          MESSAGE 'Selecione uma linha!' TYPE 'S'.
*          EXIT.
*        ENDIF.
*
*        IF lines( it_sel_rows ) NE 1.
*          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
*          EXIT.
*        ENDIF.
*
*        CALL FUNCTION 'POPUP_TO_CONFIRM'
*          EXPORTING
*            titlebar              = 'Confirmação'
*            text_question         = 'Deseja realmente deletar o registro?'
*            text_button_1         = 'Sim'
*            text_button_2         = 'Não'
*            default_button        = '1'
*            display_cancel_button = ''
*          IMPORTING
*            answer                = var_answer
*          EXCEPTIONS
*            text_not_found        = 1
*            OTHERS                = 2.
*
*        CHECK var_answer EQ '1'.
*
*        READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
*
*        READ TABLE t_alv INTO w_alv INDEX wa_sel_rows-index.
*
*        CHECK sy-subrc = 0.
*
*        DATA(_break)   = abap_false.
*        "PERFORM f_exit_0014 USING <fs_wa_saida> CHANGING _break.
*
*        if _break = abap_true.
*          PERFORM: fm_consulta.
*                   "f_processa_dados.
*
*          "LEAVE TO SCREEN 0001.
*        ENDIF.
*
*        DATA(_error) = abap_false.
*        "PERFORM f_exit_0006 USING <fs_wa_saida> CHANGING _error.
*
*        CHECK _error = abap_false.
*
*        "Criar condição dinamica
*        "PERFORM f_get_cond_chave USING 'w_alv'
*         "                     CHANGING vg_cond.
*
*        "CHECK vg_cond-where_tab[] IS NOT INITIAL.
*
*        DELETE FROM zlest0222 where cnpj_filial = w_alv-cnpj_filial and cnpj_transbordo = w_alv-cnpj_transbordo and nfps = w_alv-nfps .
*
*        IF sy-subrc = 0.
*          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
*          PERFORM: fm_consulta.
*                   "f_processa_dados.
*
*          "LEAVE TO SCREEN 0001.
*        ELSE.
*          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'.
*        ENDIF.
*
*    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

DATA: obg_toolbar TYPE REF TO lcl_alv_toolbar.

**********************************************************************
* tela selecao
**********************************************************************
PARAMETERS: p_submit TYPE char20 NO-DISPLAY.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-003.
PARAMETERS:     p_lancto RADIOBUTTON GROUP g1 USER-COMMAND rad1 DEFAULT 'X'.
PARAMETERS:     p_consul RADIOBUTTON GROUP g1.
SELECTION-SCREEN END   OF BLOCK b0.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs    FOR zlest0222-bukrs           MODIF ID t1  NO-EXTENSION NO INTERVALS , "Empresa
                s_cnpj_f   FOR zlest0222-cnpj_filial     MODIF ID t1  NO-EXTENSION NO INTERVALS , "CNPJ Filial
                s_cnpj_p   FOR zlest0222-cnpj_transbordo MODIF ID t1  NO-EXTENSION NO INTERVALS , "CNPJ Prestador de Serviço
                s_prod     FOR zlest0222-matnr           MODIF ID t1  NO-EXTENSION NO INTERVALS , "Produto
                s_dt_com   FOR zlest0222-datatransb_de   MODIF ID t1  NO-EXTENSION, "Data Período Competência
                s_qt_dia   FOR zlest0222-dias_transito   MODIF ID so2 NO-EXTENSION NO INTERVALS, "Quantidade de Dias
                s_tar_un   FOR zlest0222-tarifa_unitaria MODIF ID so2 NO-EXTENSION NO INTERVALS, "Tarifa Unitária (quinzena)
                s_v_dia    FOR zlest0222-valor_dia       MODIF ID so2 NO-EXTENSION NO INTERVALS, ".     Valor por Dia (R$/Dia)
                s_peso_c   FOR zlest0222-pesochegada     MODIF ID t1  NO-EXTENSION NO INTERVALS , "Volume Armazenado no Período
                s_vt_est   FOR zlest0222-vlr_tot_est     MODIF ID so2 NO-EXTENSION NO INTERVALS, "Valor Total Armazenagem Estática
                s_nfsv     FOR zlest0222-nfps            MODIF ID t1  NO-EXTENSION NO INTERVALS , " NF Serviço
                s_d_nfsv   FOR zlest0222-data            MODIF ID t1  NO-EXTENSION NO INTERVALS , " Data Emissão NF Serviço
                s_val_se   FOR zlest0222-valor_servico   MODIF ID t1  NO-EXTENSION NO INTERVALS . " Valor Total do Serviço
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_bukrs2   FOR zlest0222-bukrs           MODIF ID t2a  NO INTERVALS , "Empresa
                s_cnpjf2   FOR zlest0222-cnpj_filial     MODIF ID t2   NO INTERVALS , "CNPJ Filial
                s_cnpjp2   FOR zlest0222-cnpj_transbordo MODIF ID t2   NO INTERVALS , "CNPJ Prestador de Serviço
                s_prod2    FOR zlest0222-matnr           MODIF ID t2   NO INTERVALS , "Produto
                s_dtcom2   FOR zlest0222-datatransb_de   MODIF ID t2a, "Data Período Competência
                s_dnfsv2   FOR zlest0222-data            MODIF ID t2   NO INTERVALS . " Data Emissão NF Serviço


SELECTION-SCREEN FUNCTION KEY 1.  "Will have a function code of 'FC01'
SELECTION-SCREEN FUNCTION KEY 2.  "Will have a function code of 'FC02'
SELECTION-SCREEN FUNCTION KEY 3.  "Will have a function code of 'FC02'
SELECTION-SCREEN FUNCTION KEY 4.  "Will have a function code of 'FC03'

SELECTION-SCREEN SKIP.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: PUSHBUTTON 25(45) marct USER-COMMAND marct
VISIBLE LENGTH 16.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: END OF BLOCK b2.


**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.
  marct = 'Salvar'.
  FREE MEMORY ID 'ZLESR0154'.
  l_leave = 'LEAVE'.
  EXPORT l_leave FROM l_leave TO MEMORY ID 'ZLESR0154'.

  l_opcao                = '2'.

  l_sel_button-icon_id   = icon_dangerous_goods.
  l_sel_button-icon_text = 'Transbordo'.
  sscrfields-functxt_01  = l_sel_button.

  l_sel_button-icon_id   = icon_warehouse.
  l_sel_button-icon_text = 'Armazenagem'.
  sscrfields-functxt_02  = l_sel_button.

  l_sel_button-icon_id   = icon_work_center.
  l_sel_button-icon_text = 'Prestação Serviços'.
  sscrfields-functxt_03  = l_sel_button.


**********************************************************************
*SELECTION-SCREEN
**********************************************************************
AT SELECTION-SCREEN.

  FREE MEMORY ID 'ZLESR0154'.
  FREE: l_leave.

  CASE sy-ucomm.
    WHEN 'FC01'.
      l_opcao = '2'.
      IF p_submit = 'ZLESR0154'.
        LEAVE PROGRAM.
      ELSE.
        SUBMIT zlesr0154 VIA SELECTION-SCREEN WITH p_submit = sy-cprog
                         AND RETURN.
        IMPORT l_leave TO l_leave FROM MEMORY ID 'ZLESR0154'.
        IF l_leave = 'LEAVE'.
          LEAVE PROGRAM.
        ENDIF.

      ENDIF.

    WHEN 'FC02'.
      l_opcao = '2'.

    WHEN 'FC03'.
      l_opcao = '2'.
      IF p_submit = 'ZLESR0155'.
        LEAVE PROGRAM.
      ELSE.
        SUBMIT zlesr0155 VIA SELECTION-SCREEN WITH p_submit = sy-cprog
                         AND RETURN.
        IMPORT l_leave TO l_leave FROM MEMORY ID 'ZLESR0154'.
        IF l_leave = 'LEAVE'.
          LEAVE PROGRAM.
        ENDIF.
      ENDIF.
    WHEN 'MARCT'.
      l_opcao = '3'.
      PERFORM fm_start_of_selection.
  ENDCASE.





**********************************************************************
*SELECTION-SCREEN
**********************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_lancto = abap_true.
      IF screen-group1(2) = 'T2'.
        screen-input     = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF p_consul = abap_true.
      IF v_passou IS INITIAL AND s_bukrs2 IS NOT INITIAL.
        v_passou = 'X'.
        SUBMIT zlesr0156_2                  WITH p_db_tab  = 'zlest0222'
                                            WITH p_stcnam = 'zlest0222_OUT'
                                            WITH p_scmant = '0051'
                                            WITH p_title = 'Cockpit Transbordo - Armazenagem'
                                            WITH p_empres = s_bukrs2-low
                                            WITH p_cnpj_f = s_cnpjf2-low
                                            WITH p_cnpj_p = s_cnpjp2-low
                                            WITH p_prod = s_prod2-low
                                            WITH p_d_de = s_dtcom2-low
                                            WITH p_d_ate = s_dtcom2-high
                                            WITH p_dt_em = s_dnfsv2-low
AND  RETURN.

      ENDIF.
      IF screen-group1(2) = 'T1'  OR
         screen-group1(3) = 'SO2'.
        screen-input     = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1(3) = 'T2A'.
        screen-required = 2.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name =  'MARCT'.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF p_lancto = abap_true.
      IF screen-group1 = 'SO2'.
        screen-input = 0.
        zlest0222-nfps = '2'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


AT SELECTION-SCREEN ON s_prod.

  PERFORM fm_at_selection_screen_s_prod. "Tarifa Unitária

AT SELECTION-SCREEN ON s_dt_com.

  PERFORM fm_at_selection_screen_dt_com. "quantidade de dias

AT SELECTION-SCREEN ON s_peso_c.

  PERFORM fm_at_selection_screen_peso_c. "Valor Total Armazenagem Estática

AT SELECTION-SCREEN ON s_val_se.

  PERFORM fm_at_selection_screen_val_se. "Diferença entre valor calculado e o digitado( Valor TO. Armazenagem Estática e Valor Serviço)




**********************************************************************
*start
**********************************************************************
START-OF-SELECTION.

  IF p_consul = abap_true.
    IF s_bukrs2 IS INITIAL OR
       s_dtcom2 IS INITIAL.
      MESSAGE s024(sd) WITH 'Informar campos obrigatórios.' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

  CASE abap_true.
    WHEN p_lancto.
      "PERFORM fm_start_of_selection.

    WHEN p_consul.

      SUBMIT zlesr0156_2                  WITH p_db_tab  = 'zlest0222'
                                            WITH p_stcnam = 'zlest0222_OUT'
                                            WITH p_scmant = '0051'
                                            WITH p_title = 'Cockpit Transbordo - Armazenagem'
                                            WITH p_empres = s_bukrs2-low
                                            WITH p_cnpj_f = s_cnpjf2-low
                                            WITH p_cnpj_p = s_cnpjp2-low
                                            WITH p_prod = s_prod2-low
                                            WITH p_d_de = s_dtcom2-low
                                            WITH p_d_ate = s_dtcom2-high
                                            WITH p_dt_em = s_dnfsv2-low
AND  RETURN.

      "PERFORM fm_consulta.

*      IF t_alv[] IS INITIAL.
*        MESSAGE s024(sd) WITH text-100 DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.
*
*      PERFORM fm_exibe_alv.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .
  DATA: lva_ans  TYPE c.


  SELECT SINGLE *
    FROM lfa1 INTO @DATA(lwa_lfa1)
    WHERE stcd1 EQ @s_cnpj_p-low.

  IF s_tar_un-low IS INITIAL.
    "MESSAGE 'Tarifa de frete não cadastrada, favor verificar' TYPE 'E'.
    MESSAGE i000(z01) WITH 'Tarifa de frete não cadastrada, favor verificar!' .
    RETURN.
  ENDIF.

  IF s_nfsv-low IS INITIAL.
    MESSAGE i000(z01) WITH 'NF Serviço não informada, favor verificar'.
    RETURN.
  ENDIF.

  IF s_vt_est-low IS INITIAL.
    MESSAGE i000(z01) WITH 'Valor Total Armazenagem Estática não informada, favor verificar'.
    RETURN.
  ENDIF.

  IF s_d_nfsv-low IS INITIAL.
    MESSAGE i000(z01) WITH 'Data Emissão NF Serviço não informada, favor verificar'.
    RETURN.
  ENDIF.

  IF lwa_lfa1-lifnr IS NOT INITIAL.

    SELECT SINGLE *
      FROM zlest0222
      INTO @DATA(w_zlest0222)
      WHERE  bukrs = @s_bukrs-low
      AND    cnpj_filial =  @s_cnpj_f-low
      AND    nfps = @s_nfsv-low
      AND    cnpj_transbordo = @s_cnpj_p-low.
    IF sy-subrc IS NOT INITIAL.

      DATA: lwa_zlest0222 TYPE zlest0222.

      lwa_zlest0222-bukrs = s_bukrs-low.
      lwa_zlest0222-cnpj_filial =  s_cnpj_f-low.
      lwa_zlest0222-matnr = s_prod-low.
      lwa_zlest0222-cnpj_transbordo = s_cnpj_p-low.
      lwa_zlest0222-datatransb_de = s_dt_com-low.
      lwa_zlest0222-datatransb_ate = s_dt_com-high.
      lwa_zlest0222-dias_transito = s_qt_dia-low.
      lwa_zlest0222-tarifa_unitaria = s_tar_un-low.
      lwa_zlest0222-valor_dia = s_v_dia-low.
      lwa_zlest0222-pesochegada = s_peso_c-low.
      lwa_zlest0222-vlr_tot_est = s_vt_est-low.
      lwa_zlest0222-nfps = s_nfsv-low.
      lwa_zlest0222-data = s_d_nfsv-low.
      lwa_zlest0222-valor_servico = s_val_se-low.

      MODIFY zlest0222 FROM lwa_zlest0222.

      COMMIT WORK.
      CLEAR: lwa_zlest0222.

      MESSAGE i000(z01) WITH 'NF Lançada com Sucesso!'.
      RETURN.
    ELSE.
      MESSAGE i000(z01) WITH 'Dados ja cadastrados!'.

    ENDIF.
  ELSE.
    MESSAGE i000(z01) WITH 'CNPJ Prestador de Serviço não encontrado'.
    RETURN.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fm_consulta.

  DATA: l_awkey    TYPE bkpf-awkey.

  FREE: t_rbkp_sel, t_bkpf_sel, t_alv.

*------------------------------------------------
* selecao
*------------------------------------------------
  SELECT *
    FROM zlest0222
    INTO TABLE t_zlest0222
   WHERE bukrs           IN s_bukrs2
     AND cnpj_filial     IN s_cnpjf2
     AND cnpj_transbordo IN s_cnpjp2
     AND matnr           IN s_prod2
     AND datatransb_de   IN s_dtcom2
     AND data            IN s_dnfsv2.

  CHECK t_zlest0222[] IS NOT INITIAL.

  SELECT lifnr, stcd1
    FROM lfa1
    INTO TABLE @DATA(t_lfa1)
     FOR ALL ENTRIES IN @t_zlest0222
   WHERE stcd1 = @t_zlest0222-cnpj_transbordo.

  SORT t_lfa1 BY stcd1.
  DELETE ADJACENT DUPLICATES FROM t_lfa1
                        COMPARING stcd1.

  LOOP AT t_zlest0222 INTO w_zlest0222.
    READ TABLE t_lfa1 INTO DATA(w_lfa1) WITH KEY stcd1 = w_zlest0222-cnpj_transbordo
                      BINARY SEARCH.
    CHECK sy-subrc = 0.
    w_rbkp_sel-lifnr     = w_lfa1-lifnr.
    w_rbkp_sel-xblnr     = w_zlest0222-nfps.
    w_rbkp_sel-stblg     = abap_off.
    APPEND w_rbkp_sel   TO t_rbkp_sel.
  ENDLOOP.

  IF  t_rbkp_sel[] IS NOT INITIAL.
    SELECT bukrs, belnr, gjahr, lifnr, xblnr, stblg
      FROM rbkp
      INTO TABLE @DATA(t_rbkp)
       FOR ALL ENTRIES IN @t_rbkp_sel
     WHERE lifnr = @t_rbkp_sel-lifnr
       AND xblnr = @t_rbkp_sel-xblnr
       AND stblg = @t_rbkp_sel-stblg.
  ENDIF.

  IF t_rbkp[] IS NOT INITIAL.
    SELECT belnr, gjahr, ebeln
      FROM ekbe
      INTO TABLE @DATA(t_ekbe)
       FOR ALL ENTRIES IN @t_rbkp
     WHERE belnr = @t_rbkp-belnr
       AND gjahr = @t_rbkp-gjahr.
  ENDIF.

  LOOP AT t_rbkp     INTO DATA(w_rbkp).
    w_bkpf_sel-bukrs    = w_rbkp-bukrs.
    w_bkpf_sel-gjahr    = w_rbkp-gjahr.
    w_bkpf_sel-awkey    = w_rbkp-belnr && w_rbkp-gjahr.
    APPEND w_bkpf_sel  TO t_bkpf_sel.
  ENDLOOP.

  IF t_bkpf_sel[] IS NOT INITIAL.
    SELECT bukrs, belnr, gjahr, awkey
      FROM bkpf
      INTO TABLE @DATA(t_bkpf)
       FOR ALL ENTRIES IN @t_bkpf_sel
     WHERE bukrs = @t_bkpf_sel-bukrs
       AND gjahr = @t_bkpf_sel-gjahr
       AND awkey = @t_bkpf_sel-awkey.
  ENDIF.

  IF t_bkpf[] IS NOT INITIAL.
    SELECT bukrs, belnr, gjahr
      FROM bsik
      INTO TABLE @DATA(t_bsik)
       FOR ALL ENTRIES IN @t_bkpf
     WHERE bukrs = @t_bkpf-bukrs
       AND belnr = @t_bkpf-belnr
       AND gjahr = @t_bkpf-gjahr.

    SELECT bukrs, belnr, gjahr, augbl, augdt
      FROM bsak
      INTO TABLE @DATA(t_bsak)
       FOR ALL ENTRIES IN @t_bkpf
     WHERE bukrs = @t_bkpf-bukrs
       AND belnr = @t_bkpf-belnr
       AND gjahr = @t_bkpf-gjahr.
  ENDIF.

*--------------------------------------------------------
* montar alv
*--------------------------------------------------------
  LOOP AT t_zlest0222 INTO w_zlest0222.
    FREE: w_alv,
          w_lfa1,
          w_rbkp.

    READ TABLE t_lfa1 INTO w_lfa1 WITH KEY stcd1 = w_zlest0222-cnpj_transbordo
                                  BINARY SEARCH.

    READ TABLE t_rbkp INTO w_rbkp WITH KEY lifnr = w_lfa1-lifnr
                                           xblnr = w_zlest0222-nfps
                                           stblg = abap_off.

    READ TABLE t_ekbe INTO DATA(w_ekbe) WITH KEY belnr = w_rbkp-belnr
                                                 gjahr = w_rbkp-gjahr.

    l_awkey = w_rbkp-belnr && w_rbkp-gjahr.

    READ TABLE t_bkpf INTO DATA(w_bkpf) WITH KEY bukrs = w_rbkp-bukrs
                                                 gjahr = w_rbkp-gjahr
                                                 awkey = l_awkey.

    LOOP AT t_bsik INTO DATA(w_bsik) WHERE bukrs = w_bkpf-bukrs
                                       AND belnr = w_bkpf-belnr
                                       AND gjahr = w_bkpf-gjahr.
    ENDLOOP.
    IF sy-subrc <> 0.
      LOOP AT t_bsak INTO DATA(w_bsak) WHERE bukrs = w_bkpf-bukrs
                                         AND belnr = w_bkpf-belnr
                                         AND gjahr = w_bkpf-gjahr.
        w_alv-augbl        = w_bsak-augbl.
        w_alv-augdt        = w_bsak-augdt.
        EXIT.
      ENDLOOP.
    ENDIF.

    w_alv-bukrs            = w_zlest0222-bukrs.
    w_alv-cnpj_filial      = w_zlest0222-cnpj_filial.
    w_alv-cnpj_transbordo  = w_zlest0222-cnpj_transbordo.
    w_alv-nfps             = w_zlest0222-nfps.
    w_alv-data             = w_zlest0222-data.
    w_alv-valor_servico    = w_zlest0222-valor_servico.
    w_alv-datatransb_de    = w_zlest0222-datatransb_de.
    w_alv-datatransb_ate   = w_zlest0222-datatransb_ate.
    w_alv-dias_transito    = w_zlest0222-dias_transito.
    w_alv-matnr            = w_zlest0222-matnr.
    w_alv-valor_dia        = w_zlest0222-valor_dia.
    w_alv-pesochegada      = w_zlest0222-pesochegada.
    w_alv-vlr_tot_est      = w_zlest0222-vlr_tot_est.
    w_alv-ebeln            = w_ekbe-ebeln.
    w_alv-belnr            = w_rbkp-belnr.

    APPEND w_alv          TO t_alv.

    FREE: w_ekbe, w_bkpf.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& exibe alv
*&---------------------------------------------------------------------*
FORM fm_exibe_alv.

  CALL SCREEN 200.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN  'ENTER'.
      "PERFORM fm_salva_dados.

*      when 'DELETAR'.
*         CALL METHOD ctl_alv->get_selected_rows
*        IMPORTING
*          et_index_rows = it_sel_rows_lote_5120.
*
*      DESCRIBE TABLE it_sel_rows_lote_5120 LINES vl_lines.
*
*      IF vl_lines NE 1.
*        MESSAGE text-022 TYPE 'S' DISPLAY LIKE 'E'.
*        MOVE abap_true TO vl_check.
*      ENDIF.
*
*      "Consistência se há dados faltantes
*      IF vl_check IS INITIAL.
  ENDCASE.
ENDMODULE.

*FORM fm_salva_dados .
*
*ENDFORM.

FORM fm_at_selection_screen_s_prod ."Tarifa Unitária

  CHECK p_lancto = abap_true.

  IF s_bukrs-low IS NOT INITIAL AND s_cnpj_p-low IS NOT INITIAL AND s_dt_com-low IS NOT INITIAL AND s_dt_com-high IS NOT INITIAL.

    SELECT SINGLE *
    FROM lfa1 INTO @DATA(lwa_lfa1)
    WHERE stcd1 EQ @s_cnpj_p-low.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE *
      FROM a917
      INTO @DATA(lwa_a917)
      WHERE kappl EQ 'F'
        AND kschl EQ 'ZTRA'
        AND matnr EQ @s_prod-low
        AND tdlnr EQ @lwa_lfa1-lifnr
        AND kfrst EQ ''
    AND  datab LE @s_dt_com-low
    AND  datbi GE @s_dt_com-high
    AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a917~knumh
                  AND   loevm_ko EQ '' ).

      IF sy-subrc EQ 0.
        SELECT SINGLE *
          FROM konp
          INTO @DATA(lwa_konp)
          WHERE knumh EQ @lwa_a917-knumh
           AND loevm_ko EQ @space.
      ENDIF.
      s_tar_un-low = lwa_konp-kbetr.

    ENDIF.
  ELSE.
    "clear:  s_tar_un.

  ENDIF.

ENDFORM.

FORM fm_at_selection_screen_dt_com . "quantidade de dias e valor por dia
  DATA v_qtd_dt TYPE zlest0222-dias_transito.
  CHECK p_lancto = abap_true.

  IF s_bukrs-low IS NOT INITIAL AND s_cnpj_p-low IS NOT INITIAL AND s_dt_com-low IS NOT INITIAL AND s_dt_com-high IS NOT INITIAL.

    SELECT SINGLE *
    FROM lfa1 INTO @DATA(lwa_lfa1)
    WHERE stcd1 EQ @s_cnpj_p-low.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE *
      FROM a917
      INTO @DATA(lwa_a917)
      WHERE kappl EQ 'F'
        AND kschl EQ 'ZTRA'
        AND matnr EQ @s_prod-low
        AND tdlnr EQ @lwa_lfa1-lifnr
        AND kfrst EQ ''
    AND  datab LE @s_dt_com-low
    AND  datbi GE @s_dt_com-high
    AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a917~knumh
                  AND   loevm_ko EQ '' ).

      IF sy-subrc EQ 0.
        SELECT SINGLE *
          FROM konp
          INTO @DATA(lwa_konp)
          WHERE knumh EQ @lwa_a917-knumh
           AND loevm_ko EQ @space.
      ENDIF.
      s_tar_un-low = lwa_konp-kbetr.

    ENDIF.
  ELSE.
    "clear:  s_tar_un.

  ENDIF.

  IF s_dt_com-low IS NOT INITIAL AND s_dt_com-high IS NOT INITIAL.

    CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
      EXPORTING
        i_datum_bis = s_dt_com-low
        i_datum_von = s_dt_com-high
        i_szbmeth   = 2
      IMPORTING
        e_tage      = v_qtd_dt.
    IF sy-subrc <> 0.

    ELSE.
      s_qt_dia-low = v_qtd_dt + 1.
    ENDIF.
  ELSE.
    "s_qt_dia-low = 0.
    "clear: s_qt_dia.
  ENDIF.

  IF s_qt_dia-low IS NOT INITIAL AND  s_tar_un-low IS NOT INITIAL.

    s_v_dia-low = s_tar_un-low / 15.
  ELSE.
    "s_v_dia-low = 0.
    "clear: s_v_dia.
  ENDIF.

  PERFORM fm_at_selection_screen_peso_c.

ENDFORM.

FORM fm_at_selection_screen_peso_c . "Valor Total Armazenagem Estática

  CHECK p_lancto = abap_true.

  IF s_peso_c-low IS NOT INITIAL AND s_qt_dia-low IS NOT INITIAL AND s_v_dia-low IS NOT INITIAL.
    "s_vt_est-low = s_peso_c-low * s_tar_un-low.
    s_vt_est-low = s_peso_c-low * s_v_dia-low * s_qt_dia-low.
  ELSE.
    "s_vt_est-low = 0.
    "clear: s_vt_est.
  ENDIF.
ENDFORM.

FORM fm_at_selection_screen_val_se . "Diferença entre valor calculado e o digitado( Valor TO. Armazenagem Estática e Valor Serviço)
  DATA v_diferenca TYPE i.

  CHECK p_lancto = abap_true.

  PERFORM fm_at_selection_screen_s_prod. "Tarifa Unitária
  PERFORM fm_at_selection_screen_dt_com. "quantidade de dias
  PERFORM fm_at_selection_screen_peso_c. "Valor Total Armazenagem Estática

  IF s_val_se-low IS NOT INITIAL.

    v_diferenca = abs( s_val_se-low - s_vt_est-low ).

    IF v_diferenca > 2.
      MESSAGE 'A diferança entre Valor toral do Serviço e Volume armazenado período não pode ser maior que R$2,00' TYPE 'E'.
    ENDIF.

  ENDIF.

ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

  PERFORM f_fieldcatalog.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
  w_layout-info_fname   = 'LINE_COLOR'.
  w_layout-zebra        = abap_false.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_false.
  w_layout-stylefname   = 'CELLSTYLES'.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

  IF g_grid IS INITIAL.
    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

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

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = g_grid.

    SET HANDLER lcl_event_handler=>on_hotspot_click  FOR g_grid.
    SET HANDLER lcl_event_handler=>on_data_changed  FOR g_grid.
    SET HANDLER obg_toolbar->on_toolbar FOR g_grid.

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
  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF lines( t_rows ) > 0.
    CALL METHOD g_grid->set_selected_rows
      EXPORTING
        it_index_rows = t_rows.
  ENDIF.

ENDFORM.

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
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
     01  ''      ''       'T_ALV'   'BUKRS'               'Empresa'                     '08'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     02  'LFA1'  'STCD1'  'T_ALV'   'CNPJ_FILIAL'         'CNPJ Filial'                 '16'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     03  'LFA1'  'STCD1'  'T_ALV'   'CNPJ_TRANSBORDO'     'CNPJ Prest.Serv.'            '16'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     04  ''      ''       'T_ALV'   'NFPS'                'NFPS'                        '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     05  ''      ''       'T_ALV'   'DATA'                'Data NFPS'                   '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     06  ''      ''       'T_ALV'   'VALOR_SERVICO'       'Valor NFPS'                  '16'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     07  ''      ''       'T_ALV'   'DATATRANSB_DE'       'Data Inicio'                 '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     08  ''      ''       'T_ALV'   'DATATRANSB_ATE'      'Data Fim'                    '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     09  ''      ''       'T_ALV'   'DIAS_TRANSITO'       'Dias'                        '05'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     10  'MARA'  'MATNR'  'T_ALV'   'MATNR'               'Material'                    '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     11  ''      ''       'T_ALV'   'VALOR_DIA'           'Valor Dia'                   '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     12  ''      ''       'T_ALV'   'PESOCHEGADA'         'Peso Chegada'                '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     13  ''      ''       'T_ALV'   'VLR_TOT_EST'         'Valor Estático'              '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     14  ''      ''       'T_ALV'   'EBELN'               'Pedido'                      '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     15  ''      ''       'T_ALV'   'BELNR'               'Miro'                        '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     16  ''      ''       'T_ALV'   'AUGBL'               'Doc.Compensação'             '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
     17  ''      ''       'T_ALV'   'AUGDT'               'Data Compensação'            '17'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

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

  wl_linha = text-123.

  wl_text  = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.

*  CALL METHOD obj_dyndoc_id->new_line.
*
*  IF s_bukrs[] IS NOT INITIAL.
*    CONCATENATE  'Empresa....:' s_bukrs2-low
*           INTO wl_linha SEPARATED BY space.
*
*    wl_text = wl_linha.
*    CALL METHOD obj_dyndoc_id->new_line.
*
*    CALL METHOD obj_dyndoc_id->add_text
*      EXPORTING
*        text         = wl_text
*        sap_fontsize = cl_dd_area=>list_normal.
*  ENDIF.
*
*  IF s_dnfsv2[] IS NOT INITIAL.
*    READ TABLE s_dnfsv2 INDEX 1.
*
*    wl_data1 = s_dnfsv2-low+6(2)  && '.' && s_dnfsv2-low+4(2)  && '.' && s_dnfsv2-low(4).
*    wl_data2 = s_dnfsv2-high+6(2) && '.' && s_dnfsv2-high+4(2) && '.' && s_dnfsv2-high(4).
*
*    IF s_dnfsv2-high IS NOT INITIAL.
*      CONCATENATE  'Dt.Emiss.NF Serviço..:' wl_data1 'a' wl_data2
*             INTO wl_linha SEPARATED BY space.
*    ELSE.
*      CONCATENATE  'Dt.Emiss.NF Serviço,,:' wl_data1
*             INTO wl_linha SEPARATED BY space.
*    ENDIF.
*
*    wl_text = wl_linha.
*    CALL METHOD obj_dyndoc_id->new_line.
*
*    CALL METHOD obj_dyndoc_id->add_text
*      EXPORTING
*        text         = wl_text
*        sap_fontsize = cl_dd_area=>list_normal.
*  ENDIF.

ENDFORM.                    " ZF_ALV_HEADER

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'ZLESR0154'.
  SET TITLEBAR  'ZLESR0154'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  FREE: t_rows[].

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
