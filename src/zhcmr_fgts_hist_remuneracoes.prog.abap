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
**|    + Rodrigo Carvalho ( rodrigo.sa@amaggi.com.br)                         |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Carolini santos ( carolini.santos@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| FGTS Digital Importação Histórico de Remunerações Pretéritas              |*
**/===========================================================================\*
REPORT zhcmr_fgts_hist_remuneracoes.


**********************************************************************
* Tables
**********************************************************************
TABLES: t54c6, pernr, /tmf/d_lanc_cont, pa0001, pa0000.

TYPE-POOLS : slis.



**********************************************************************
* Types
**********************************************************************

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF t_s_pn,
         paper TYPE paper,        "payroll period
         begda TYPE d,            "evaluation period
         endda TYPE d,            "  (= data selection interval)
         begps TYPE d,            "selection period
         endps TYPE d,            "  (= person selection interval)
         pabrp LIKE qppnp-pabrp,  "payroll period
         pabrj LIKE qppnp-pabrj,  "payroll year
         permo LIKE t549a-permo,  "payroll period parameter
       END OF t_s_pn,

       BEGIN OF ty_pa0001,
         pernr TYPE pa0001-pernr,
         bukrs TYPE pa0001-bukrs,
         endda TYPE pa0001-endda,
         begda TYPE pa0001-begda,
         werks TYPE pa0001-werks,
         persg TYPE pa0001-persg,
       END OF ty_pa0001,

       BEGIN OF ty_pa0000,
         pernr TYPE pa0000-pernr,
         stat2 TYPE pa0000-stat2,
       END OF ty_pa0000,

       BEGIN OF ty_pa0465,
         pernr  TYPE pa0465-pernr,
         cpf_nr TYPE pa0465-cpf_nr,
       END OF ty_pa0465,

       BEGIN OF ty_zfit0209,
         tipo_valor TYPE zfit0209-tipo_valor,
         lgart      TYPE zfit0209-lgart,
       END OF ty_zfit0209,

       BEGIN OF ty_alv_saida,
         tipo_1(40)        TYPE c,                     "Tipo
         cnpj(18)          TYPE c, "CNPJ Empregador
         cpf_nr            TYPE pa0465-cpf_nr,         "CPF Trabalhador
         dt_admissao       TYPE p0000-begda,           "Data Admissão
         pernr             TYPE pa0001-pernr,          "Matricula
         tipo_2(40)        TYPE c,                     "Tipo
         competencia(07)   TYPE c,                     "Competencia
         categoria(03)     TYPE c,                     "Categoria
         valor_principal   TYPE pc207-betrg,           "Valor Principal
         fgts_13           TYPE pc207-betrg,           "Valor 13º
         ausencia_fgts(01) TYPE c,                     "Aus. FGTS
         cellcolor         TYPE lvc_t_scol,
       END   OF ty_alv_saida,

       BEGIN OF ty_change_bukrs,
         pernr           TYPE pa0001-pernr,
         competencia(07) TYPE c,
       END   OF ty_change_bukrs,


       BEGIN OF ty_saida_arquivo,
         linha TYPE c LENGTH 3000,
       END   OF ty_saida_arquivo.


*Class definition for ALV toolbar
CLASS:lcl_alv_toolbar     DEFINITION DEFERRED.


*Declaration for toolbar buttons
DATA: ty_toolbar TYPE stb_button.


**********************************************************************
* Tabela interna
**********************************************************************
DATA: t_alv_saida      TYPE TABLE OF ty_alv_saida,
      wa_alv_saida     TYPE ty_alv_saida,
      t_change_bukrs   TYPE TABLE OF ty_change_bukrs,
      wa_change_bukrs  TYPE ty_change_bukrs,
      t_saida_arquivo  TYPE TABLE OF ty_saida_arquivo,
      wa_saida_arquivo TYPE ty_saida_arquivo,
      wa_color         TYPE          lvc_s_scol,  " Cor para célula
      it_color         TYPE TABLE OF lvc_s_scol,  " Cor para célula
      t_pa0001         TYPE TABLE OF ty_pa0001,
      wa_pa0001        TYPE ty_pa0001,
      t_pa0000         TYPE TABLE OF ty_pa0000,
      wa_pa0000        TYPE ty_pa0000,
      t_pa0465         TYPE TABLE OF ty_pa0465,
      wa_pa0465        TYPE ty_pa0465,
      t_zfit0209       TYPE TABLE OF ty_zfit0209,
      wa_zfit0209      TYPE ty_zfit0209,
      wa_pay_result    TYPE paybr_result.

*------------------------------------
*---- ALV
*------------------------------------
DATA: dg_splitter_1        TYPE REF TO cl_gui_splitter_container,
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      tl_function          TYPE ui_functions,
      wl_function          TYPE ui_func,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
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

DATA: lo_payroll TYPE REF TO cl_hr_br_read_payroll,
      zcl_util   TYPE REF TO zcl_util.

DATA: variante         LIKE disvariant.
DATA: gs_variant_c TYPE disvariant.

DATA: gv_begda           TYPE sy-datum,
      gv_begda_high      TYPE sy-datum,
      gv_endda           TYPE sy-datum,
      gv_endda_high      TYPE sy-datum,
      gv_cgc             LIKE bapibranch-cgc_number,
      gv_competencia(07) TYPE c.


CONSTANTS c_rub_fgts_13 TYPE t512w-lgart VALUE '/142'.


* set by LDB: period and payroll settings entered on selection-screen
DATA: pn TYPE t_s_pn.

DATA: l_leave        TYPE syst_ucomm,
      v_p_db_tab(8)  TYPE c,
      v_p_stcnam(12) TYPE c,
      v_p_scmant(4)  TYPE c,
      v_p_title(40)  TYPE c,
      v_filename     TYPE string,
      g_sel_var      TYPE rsvar-variant,
      g_sel_vartxt   TYPE rsvar-vtext,
      l_opcao        TYPE char1.


**********************************************************************
* ranges
**********************************************************************
DATA: rg_begda TYPE RANGE OF pa0001-begda,
      wa_begda LIKE LINE  OF rg_begda,
      rg_endda TYPE RANGE OF pa0001-endda,
      wa_endda LIKE LINE  OF rg_endda,
      rg_lgart TYPE RANGE OF t512w-lgart,
      wa_lgart LIKE LINE  OF rg_lgart.




SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_mes FOR t54c6-smont                NO-EXTENSION,
                  s_ano FOR /tmf/d_lanc_cont-exercicio NO-EXTENSION.
SELECTION-SCREEN END   OF BLOCK b1.

*SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-005.
*  PARAMETERS  p_file    TYPE string LOWER CASE OBLIGATORY.
*SELECTION-SCREEN END   OF BLOCK b02.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_pernr FOR pa0001-pernr NO INTERVALS, "Matrícula
                  s_bukrs FOR pa0001-bukrs NO INTERVALS, "Empresa
                  s_gsber FOR pa0001-gsber NO INTERVALS, "Divisão
                  s_werks FOR pa0001-werks NO INTERVALS, "Área de recursos humanos
                  s_btrtl FOR pa0001-btrtl NO INTERVALS, "Subárea de recursos humanos
                  s_persg FOR pa0001-persg NO INTERVALS, "Grupo de Empregados
                  s_persk FOR pa0001-persk NO INTERVALS, "Subgrupo de empregados
                  s_kostl FOR pa0001-kostl NO INTERVALS, "Centro de Custo
                  s_orgeh FOR pa0001-orgeh NO INTERVALS, "Unidade Organizacional
                  s_plans FOR pa0001-plans NO INTERVALS, "Posição
                  s_stell FOR pa0001-stell NO INTERVALS, "Cargo
                  s_stat2 FOR pa0000-stat2 NO INTERVALS. "Status da ocupação
SELECTION-SCREEN END   OF BLOCK b2.



CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor
        IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    ty_toolbar-icon      = icon_copy_object.
    ty_toolbar-function  = 'GERAR_ARQ'.
    ty_toolbar-disabled  = ''.
    ty_toolbar-text      = TEXT-003.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.


*    CALL REORGANIZE METHOD OF TOOLBAR MANAGER TO
*    DISPLAY THE TOOLBAR
*    CALL METHOD c_alv_toolbarmanager->reorganize
*      EXPORTING
*        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CONSTANTS lv_semicolon TYPE c VALUE ';'.

    DATA: vl_tipo_1              TYPE c,                     "Tipo
          vl_cnpj(08)            TYPE c,                     "CNPJ Empregador
          vl_cpf_nr              TYPE pa0465-cpf_nr,         "CPF Trabalhador
          vl_dt_admissao(10)     TYPE c,                     "Data Admissão
          vl_pernr(08)           TYPE c,                      "Matricula
          vl_tipo_2              TYPE c,                     "Tipo
          vl_competencia(07)     TYPE c,                     "Competencia
          vl_categoria(03)       TYPE c,                     "Categoria
          vl_valor_principal(20) TYPE c,                     "Valor Principal
          vl_fgts_13(20)         TYPE c,                     "Valor 13º
          vl_ausencia_fgts(01)   TYPE c.                     "Aus. FGTS



    DATA: wa_sel_rows     TYPE lvc_s_row,
          selected_folder	TYPE string.

*    FREE: t_rows[].
*
*    CALL METHOD g_grid->get_selected_rows
*      IMPORTING
*        et_index_rows = t_rows.

    CASE e_ucomm.

      WHEN 'GERAR_ARQ'.

        cl_gui_frontend_services=>directory_browse(
          EXPORTING
            window_title         = 'Pasta para salvar arquivos'
          CHANGING
            selected_folder      = selected_folder
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            OTHERS               = 4 ).


        DATA(filename) = selected_folder && '\' && 'FGTS Digital' && sy-datum && sy-uzeit && '.csv'.

        LOOP AT t_alv_saida INTO wa_alv_saida.


          CLEAR : vl_cnpj,      vl_cpf_nr,          vl_dt_admissao, vl_pernr, vl_competencia,
                  vl_categoria, vl_valor_principal, vl_fgts_13,     vl_ausencia_fgts.

          vl_tipo_1          = '1'.
          IF NOT wa_alv_saida-cnpj IS INITIAL.
            TRANSLATE wa_alv_saida-cnpj USING '. '. TRANSLATE wa_alv_saida-cnpj USING '/ '. TRANSLATE vl_cpf_nr USING '- '.
            CONDENSE wa_alv_saida-cnpj NO-GAPS.
            vl_cnpj            = wa_alv_saida-cnpj(08).
          ENDIF.
          vl_cpf_nr          = wa_alv_saida-cpf_nr. TRANSLATE vl_cpf_nr USING '. '. TRANSLATE vl_cpf_nr USING '- '.
          CONDENSE vl_cpf_nr NO-GAPS.
          IF NOT wa_alv_saida-dt_admissao IS INITIAL.
            CONCATENATE wa_alv_saida-dt_admissao+06(02) '-' wa_alv_saida-dt_admissao+04(02) '-' wa_alv_saida-dt_admissao(04)
            INTO vl_dt_admissao.
          ENDIF.
          vl_pernr           = wa_alv_saida-pernr.

          AT NEW pernr.

            CONCATENATE vl_tipo_1 lv_semicolon vl_cnpj lv_semicolon vl_cpf_nr lv_semicolon vl_dt_admissao lv_semicolon vl_pernr lv_semicolon
            INTO wa_saida_arquivo-linha. CONDENSE wa_saida_arquivo-linha NO-GAPS.
            APPEND wa_saida_arquivo TO t_saida_arquivo.

          ENDAT.


          vl_tipo_2          = '2'.
          IF NOT wa_alv_saida-competencia IS INITIAL.
            CONCATENATE wa_alv_saida-competencia(02) '-' wa_alv_saida-competencia+03(04) INTO vl_competencia.
          ENDIF.
          vl_categoria       = wa_alv_saida-categoria.
          IF NOT wa_alv_saida-valor_principal IS INITIAL.
            vl_valor_principal = wa_alv_saida-valor_principal. TRANSLATE vl_valor_principal USING '.,'.
          ENDIF.
          IF NOT wa_alv_saida-fgts_13 IS INITIAL.
            vl_fgts_13         = wa_alv_saida-fgts_13.         TRANSLATE vl_fgts_13         USING '.,'.
          ENDIF.
          vl_ausencia_fgts   = wa_alv_saida-ausencia_fgts.

          CONCATENATE vl_tipo_2 lv_semicolon vl_competencia lv_semicolon vl_categoria lv_semicolon vl_valor_principal lv_semicolon vl_fgts_13 lv_semicolon vl_ausencia_fgts
          INTO wa_saida_arquivo-linha. CONDENSE wa_saida_arquivo-linha NO-GAPS.
          APPEND wa_saida_arquivo TO t_saida_arquivo.

        ENDLOOP.


        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            filename         = filename
            filetype         = 'ASC'
*           write_field_separator = 'X'
          CHANGING
            data_tab         = t_saida_arquivo
          EXCEPTIONS
            file_write_error = 1
            no_batch         = 2
            OTHERS           = 24.

        IF sy-subrc = 0.
          MESSAGE TEXT-s01 TYPE 'S'.
        ENDIF.

*        g_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).
*        g_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).


    ENDCASE.

*** Método de atualização de dados na Tela
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

    CALL METHOD cl_gui_cfw=>flush.


*    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*      EXPORTING
*        functioncode           = '=ENT'
*      EXCEPTIONS
*        function_not_supported = 1
*        OTHERS                 = 2.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION



**********************************************************************
*SELECTION-SCREEN
**********************************************************************
AT SELECTION-SCREEN.

  FREE: l_leave.

  CASE sy-ucomm.
    WHEN 'BT001'.
      v_p_db_tab = 'ZFIT0209'.
      v_p_stcnam = 'ZFIT0209_OUT'.
      v_p_scmant = '0243'.
      v_p_title = 'Parametrização de rubricas'.
    WHEN 'ONLI'.
      "PERFORM f_start_selection.
    WHEN 'VARIANT'.
      PERFORM carrega_variantes.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

  IF v_p_db_tab IS NOT INITIAL.
    l_opcao = '1'.
    SUBMIT zregister_data       WITH p_db_tab = v_p_db_tab
                                WITH p_stcnam = v_p_stcnam
                                WITH p_scmant = v_p_scmant
                                WITH p_title  = v_p_title
    AND RETURN.
    CLEAR: v_p_db_tab, v_p_stcnam, v_p_scmant, v_p_title.
  ENDIF.



**********************************************************************
*SELECTION-SCREEN output
**********************************************************************
AT SELECTION-SCREEN OUTPUT.


  IF l_opcao = 1.
    LOOP AT SCREEN.
    ENDLOOP.

    FREE MEMORY ID 'ZHCMR_FGTS_HIST_REMUNERACOES'.
    l_leave = 'LEAVE'.
    EXPORT l_leave FROM l_leave TO MEMORY ID 'ZHCMR_FGTS_HIST_REMUNERACOES'.

    l_opcao                = '1'.

  ENDIF.


**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.


*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*
*  cl_gui_frontend_services=>directory_browse(
*    CHANGING
*      selected_folder      = p_file
*    EXCEPTIONS
*      cntl_error           = 1
*      error_no_gui         = 2
*      not_supported_by_gui = 3
*      OTHERS               = 4 ).


  SET PF-STATUS '1000'.

  FREE MEMORY ID 'ZHCMR_FGTS_HIST_REMUNERACOES'.
  l_leave = 'LEAVE'.
  EXPORT l_leave FROM l_leave TO MEMORY ID 'ZHCMR_FGTS_HIST_REMUNERACOES'.

  l_opcao                = '1'.


**********************************************************************
* START
**********************************************************************
START-OF-SELECTION.


  PERFORM f_selecao_dados.

  PERFORM f_processa_dados.

  PERFORM f_exibir_dados.


*&---------------------------------------------------------------------*
*& Form f_selecao_dados
*&---------------------------------------------------------------------*
FORM f_selecao_dados .

  IF s_mes[] IS INITIAL.
    MESSAGE s131(zfi) WITH 'Preencher campo Obrigatório Mês' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF s_ano[] IS INITIAL.
    MESSAGE s131(zfi) WITH 'Preencher campo Obrigatório Ano' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.


  IF NOT s_mes-low IS INITIAL AND NOT s_ano-low IS INITIAL AND s_mes-high IS INITIAL AND s_ano-high IS INITIAL.

    CONCATENATE s_ano-low s_mes-low '01' INTO gv_begda.

    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = gv_begda
      IMPORTING
        last_day_of_month = gv_endda.

    CLEAR: pn-begda, pn-endda.
    pn-begda = gv_begda.
    pn-endda = gv_endda.

  ELSEIF NOT s_mes-low IS INITIAL AND NOT s_ano-low IS INITIAL AND NOT s_mes-high IS INITIAL AND NOT s_ano-high IS INITIAL.

    CONCATENATE s_ano-low s_mes-low   '01' INTO gv_begda.
    CONCATENATE s_ano-high s_mes-high '01' INTO gv_begda_high.

    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = gv_begda_high
      IMPORTING
        last_day_of_month = gv_endda_high.

    CLEAR: pn-begda, pn-endda.
    pn-begda = gv_begda.
    pn-endda = gv_endda_high.


  ENDIF.


  " Seleciona Registro mestre HR: infotipo 0001 (atrib.org.)
  SELECT pernr bukrs endda begda werks persg
         FROM pa0001
         INTO TABLE t_pa0001
         WHERE pernr IN s_pernr
         AND   bukrs IN s_bukrs
         AND   werks IN s_werks
         AND   btrtl IN s_btrtl
         AND   persg IN s_persg
         AND   persk IN s_persk
         AND   gsber IN s_gsber
         AND   kostl IN s_kostl
         AND   orgeh IN s_orgeh
         AND   plans IN s_plans
         AND   stell IN s_stell
         AND   endda >= pn-begda
         AND   begda <= pn-endda.


  IF t_pa0001[] IS INITIAL.
    MESSAGE i005(zfi).
    STOP.
  ENDIF.

  SORT t_pa0001 BY pernr bukrs.

  " Seleciona Registro mestre HR infotipo 0000 (Medidas)
  SELECT pernr stat2
         FROM pa0000
         INTO TABLE t_pa0000
         FOR ALL ENTRIES IN t_pa0001
         WHERE pernr EQ t_pa0001-pernr
         AND endda >= sy-datum
         AND stat2 IN s_stat2.

  SORT t_pa0000 BY pernr.

  " Seleciona Registro mestre HR infotipo 0465.
  SELECT pernr cpf_nr
         FROM pa0465
         INTO TABLE t_pa0465
         FOR ALL ENTRIES IN t_pa0001
         WHERE pernr EQ t_pa0001-pernr
         AND   subty EQ '0001'.

  SORT t_pa0465 BY pernr.


  " Seleciona Cadastro de rubricas
  SELECT tipo_valor lgart
         FROM zfit0209
         INTO TABLE t_zfit0209.

  LOOP AT t_zfit0209 INTO DATA(wa_zfit0209).
    wa_lgart-sign   = 'I'.
    wa_lgart-option = 'EQ'.
    wa_lgart-low    = wa_zfit0209-lgart.
    APPEND wa_lgart TO rg_lgart.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form carrega_variantes
*&---------------------------------------------------------------------*
FORM carrega_variantes .

  PERFORM escolher_variante CHANGING g_sel_var.

  IF g_sel_var NE space.
    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING
        report               = sy-repid
        variant              = g_sel_var
      EXCEPTIONS
        variant_not_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form escolher_variante
*&---------------------------------------------------------------------*
FORM escolher_variante  CHANGING p_g_sel_var.

  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report               = sy-repid
      masked               = 'X'
    IMPORTING
      sel_variant          = p_g_sel_var
      sel_variant_text     = g_sel_vartxt
    EXCEPTIONS
      no_report            = 1
      report_not_existent  = 2
      report_not_supplied  = 3
      no_variants          = 4
      no_variant_selected  = 5
      variant_not_existent = 6
      OTHERS               = 7.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_dados
*&---------------------------------------------------------------------*
FORM f_processa_dados .


  DATA: vl_cnpj  TYPE bapibranch-cgc_number.

  CLEAR wa_color.
  MOVE 'tipo_1'   TO wa_color-fname.
  MOVE '1'        TO wa_color-color-col.
  MOVE '0'        TO wa_color-color-int.
  MOVE '0'        TO wa_color-color-inv.
  APPEND wa_color TO it_color.

  CLEAR wa_color.
  MOVE 'cnpj'    TO wa_color-fname.
  MOVE '1'        TO wa_color-color-col.
  MOVE '0'        TO wa_color-color-int.
  MOVE '0'        TO wa_color-color-inv.
  APPEND wa_color TO it_color.

  CLEAR wa_color.
  MOVE 'cpf_nr'   TO wa_color-fname.
  MOVE '1'        TO wa_color-color-col.
  MOVE '0'        TO wa_color-color-int.
  MOVE '0'        TO wa_color-color-inv.
  APPEND wa_color TO it_color.

  CLEAR wa_color.
  MOVE 'dt_admissao' TO wa_color-fname.
  MOVE '1'        TO wa_color-color-col.
  MOVE '0'        TO wa_color-color-int.
  MOVE '0'        TO wa_color-color-inv.
  APPEND wa_color TO it_color.

  CLEAR wa_color.
  MOVE 'pernr'    TO wa_color-fname.
  MOVE '1'        TO wa_color-color-col.
  MOVE '0'        TO wa_color-color-int.
  MOVE '0'        TO wa_color-color-inv.
  APPEND wa_color TO it_color.

  CLEAR wa_color.
  MOVE 'tipo_2'    TO wa_color-fname.
  MOVE '6'        TO wa_color-color-col.
  MOVE '0'        TO wa_color-color-int.
  MOVE '0'        TO wa_color-color-inv.
  APPEND wa_color TO it_color.

  CLEAR wa_color.
  MOVE 'competencia'    TO wa_color-fname.
  MOVE '6'        TO wa_color-color-col.
  MOVE '0'        TO wa_color-color-int.
  MOVE '0'        TO wa_color-color-inv.
  APPEND wa_color TO it_color.

  CLEAR wa_color.
  MOVE 'categoria'    TO wa_color-fname.
  MOVE '6'        TO wa_color-color-col.
  MOVE '0'        TO wa_color-color-int.
  MOVE '0'        TO wa_color-color-inv.
  APPEND wa_color TO it_color.

  CLEAR wa_color.
  MOVE 'valor_principal'    TO wa_color-fname.
  MOVE '6'        TO wa_color-color-col.
  MOVE '0'        TO wa_color-color-int.
  MOVE '0'        TO wa_color-color-inv.
  APPEND wa_color TO it_color.

  CLEAR wa_color.
  MOVE 'fgts_13'    TO wa_color-fname.
  MOVE '6'        TO wa_color-color-col.
  MOVE '0'        TO wa_color-color-int.
  MOVE '0'        TO wa_color-color-inv.
  APPEND wa_color TO it_color.

  CLEAR wa_color.
  MOVE 'ausencia_fgts'    TO wa_color-fname.
  MOVE '6'        TO wa_color-color-col.
  MOVE '0'        TO wa_color-color-int.
  MOVE '0'        TO wa_color-color-inv.
  APPEND wa_color TO it_color.

  CLEAR: t_change_bukrs[].

  LOOP AT t_pa0001 INTO wa_pa0001.

    CLEAR wa_alv_saida.


    "Tipo
    wa_alv_saida-tipo_1 = '01 - Identificação do trabalhador'.

    "Tipo
    wa_alv_saida-tipo_2 = '02 - Remuneração do trabalhador'.

    "Matricula
    wa_alv_saida-pernr = wa_pa0001-pernr.

    "CPF Trabalhador
    READ TABLE t_pa0465 INTO wa_pa0465 WITH KEY pernr = wa_pa0001-pernr BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-cpf_nr = wa_pa0465-cpf_nr.
    ENDIF.


    "Data Admissão
    CALL FUNCTION 'HR_ENTRY_DATE'
      EXPORTING
        persnr               = wa_pa0001-pernr
        endda                = '99991231'
      IMPORTING
        entrydate            = wa_alv_saida-dt_admissao
      EXCEPTIONS
        entry_date_not_found = 1
        pernr_not_assigned   = 2
        OTHERS               = 3.

    "Categoria
    IF     wa_pa0001-persg = '1'.
      wa_alv_saida-categoria = '101'.
    ELSEIF wa_pa0001-persg = '4'.
      wa_alv_saida-categoria = '103'.
    ELSEIF wa_pa0001-persg = '6'.
      wa_alv_saida-categoria = 'XXX'.
    ELSEIF wa_pa0001-persg = '9'.
      wa_alv_saida-categoria = '901'.
    ELSEIF wa_pa0001-persg = 'C'.
      wa_alv_saida-categoria = 'XXX'.
    ELSEIF wa_pa0001-persg = 'D'.
      wa_alv_saida-categoria = 'XXX'.
    ENDIF.


    CREATE OBJECT lo_payroll
      EXPORTING
        iv_pernr = wa_pa0001-pernr.

    CALL METHOD lo_payroll->get_rgdir
      EXPORTING
        iv_begda = pn-begda
        iv_endda = pn-endda
        iv_srtza = 'A'
      IMPORTING
        et_rgdir = DATA(lit_rgdir).

    IF lit_rgdir[] IS NOT INITIAL.

      CALL METHOD lo_payroll->get_pay_result_table
        EXPORTING
          it_rgdir        = lit_rgdir
        IMPORTING
          et_paybr_result = DATA(lit_paybr).


      SORT t_change_bukrs BY pernr competencia.

      LOOP AT lit_paybr  ASSIGNING FIELD-SYMBOL(<fs_paybr>).

        CLEAR gv_competencia.
        DATA(vl_competencia) = <fs_paybr>-inter-versc-inper.
        CONCATENATE vl_competencia+04(02) '/' vl_competencia(04) INTO gv_competencia.

        "Verifica matricula pode ter mais de uma empresa dentro do mesmo ano
        READ TABLE t_change_bukrs INTO wa_change_bukrs WITH KEY pernr = wa_pa0001-pernr competencia = gv_competencia BINARY SEARCH.
        IF sy-subrc EQ 0.
          CONTINUE.
        ENDIF.

        READ TABLE <fs_paybr>-inter-wpbp INTO DATA(wa_wpbp) INDEX 1.
        IF NOT s_bukrs[] IS INITIAL AND wa_wpbp-bukrs NOT IN s_bukrs.
          CONTINUE.
        ENDIF.

        "CNPJ Empregador
        CLEAR vl_cnpj.
        CALL FUNCTION 'HR_BR_LER_FILIAL_GERAL'
          EXPORTING
            company_code      = wa_wpbp-bukrs
            branch            = wa_wpbp-werks
          IMPORTING
            cgc               = vl_cnpj
          EXCEPTIONS
            branch_not_found  = 1
            address_not_found = 2
            company_not_found = 3
            OTHERS            = 4.

        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = vl_cnpj
          IMPORTING
            output = wa_alv_saida-cnpj.



        "Deleta rubricas fora do cadastro
        DELETE <fs_paybr>-inter-rt WHERE lgart NOT IN rg_lgart.


        LOOP AT <fs_paybr>-inter-rt ASSIGNING FIELD-SYMBOL(<fs_rt>).

          READ TABLE t_zfit0209 INTO wa_zfit0209 WITH KEY lgart = <fs_rt>-lgart.

          IF wa_zfit0209-tipo_valor EQ '02'.
            "Valor 13º
            wa_alv_saida-fgts_13 = wa_alv_saida-fgts_13 + <fs_rt>-betrg. "FGTS 13
          ELSEIF wa_zfit0209-tipo_valor EQ '01'.
            "Valor Principal
            wa_alv_saida-valor_principal = wa_alv_saida-valor_principal + <fs_rt>-betrg. "FGTS NORMAL
          ENDIF.

          "Competencia
          DATA(vl_inper) = <fs_paybr>-inter-versc-inper.
          CONCATENATE vl_inper+04(02) '/' vl_inper(04) INTO wa_alv_saida-competencia.

        ENDLOOP.

        IF vl_inper EQ '000000'.
          CONTINUE.
        ENDIF.

        IF <fs_paybr>-inter-rt[] IS INITIAL AND vl_inper IS INITIAL.
          IF <fs_paybr>-evp-inper EQ '000000'.
            CONTINUE.
          ELSE.
            vl_inper = <fs_paybr>-evp-inper.
            CONCATENATE vl_inper+04(02) '/' vl_inper(04) INTO wa_alv_saida-competencia.
          ENDIF.
        ENDIF.

        IF wa_alv_saida-valor_principal IS INITIAL AND wa_alv_saida-fgts_13 IS INITIAL.
          "Aus. FGTS
          wa_alv_saida-ausencia_fgts = 'S'.
        ENDIF.

        wa_change_bukrs-pernr       = wa_pa0001-pernr.
        wa_change_bukrs-competencia = wa_alv_saida-competencia.
        APPEND wa_change_bukrs TO t_change_bukrs.


        wa_alv_saida-cellcolor[] = it_color[].
        APPEND wa_alv_saida TO t_alv_saida.

        CLEAR: wa_alv_saida-valor_principal, wa_alv_saida-fgts_13,
               wa_alv_saida-ausencia_fgts,   wa_alv_saida-competencia, vl_inper.

      ENDLOOP.

    ENDIF.



  ENDLOOP.


  IF t_alv_saida[] IS INITIAL.
    MESSAGE i005(zfi).
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibir_dados
*&---------------------------------------------------------------------*
FORM f_exibir_dados .

  CALL SCREEN 100.

ENDFORM.

INCLUDE zhcmr_fgts_hist_remuneracoeo01.
*&---------------------------------------------------------------------*
*& Form f_init_alv
*&---------------------------------------------------------------------*
FORM f_init_alv .

  DATA: wl_layout TYPE slis_layout_alv.
  DATA:
    p_text      TYPE sdydo_text_element,
    filtros	    TYPE zif_screen_linha_filtro,
    i_filtros	  TYPE zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) TYPE c,
    v_uzeit(10) TYPE c.


  PERFORM f_fieldcatalog.

  variante = VALUE #( report = sy-repid ).

  "PERFORM zf_preenche_cell_color.

  IF g_grid IS INITIAL.

    CLEAR: i_filtros.
    CONCATENATE sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) INTO v_datum.
    CONCATENATE sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) INTO v_uzeit.
    DESCRIBE TABLE t_alv_saida LINES DATA(v_lines).
    APPEND VALUE #( parametro = 'Data:' valor = v_datum ) TO i_filtros.
    APPEND VALUE #( parametro = 'Hora:' valor = v_uzeit ) TO i_filtros.
    APPEND VALUE #( parametro = 'Registros:' valor = v_lines ) TO i_filtros.

  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      EXPORTING
        i_titulo  = CONV #( p_text )
        i_filtros = i_filtros
      CHANGING
        split     = dg_splitter_1
        alv       = g_grid ) = abap_true.


    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = g_grid.


    w_layout-sel_mode   = 'A'.
    w_layout-ctab_fname = 'CELLCOLOR'.
    "w_layout-edit     = 'X'.

    w_stable-row          = abap_true.
    w_stable-col          = abap_true.
*
*    w_layout-info_fname   = 'LINE_COLOR'.
*    w_layout-ctab_fname   = 'COLOR_CELL'.
***    w_layout-zebra        = abap_false.

    "w_layout-stylefname   = 'CELLSTYLES'.

    SET HANDLER obg_toolbar->on_toolbar FOR g_grid.
    SET HANDLER obg_toolbar->handle_user_command FOR g_grid.

*    SET HANDLER lcl_event_handler=>on_hotspot_click  FOR g_grid.
*    SET HANDLER lcl_event_handler=>on_data_changed  FOR g_grid.
*    SET HANDLER lcl_event_handler=>data_changed_finished FOR g_grid.

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

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      CHANGING
        it_outtab                     = t_alv_saida[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


*    CALL METHOD g_grid->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*    CALL METHOD g_grid->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    IF lines( t_rows ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows.
    ENDIF.

  ELSE.
    CALL METHOD g_grid->refresh_table_display( is_stable = w_stable ).
  ENDIF.

  "wl_layout-colwidth_optimize = 'X'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fieldcatalog
*&---------------------------------------------------------------------*
FORM f_fieldcatalog .


  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
  01  ''   ''   'T_ALV_SAIDA'   'tipo_1'                  'Tipo'                          '25'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  02  ''   ''   'T_ALV_SAIDA'   'cnpj'                    'CNPJ Empregador'               '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  03  ''   ''   'T_ALV_SAIDA'   'cpf_nr'                  'CPF Trabalhador'               '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  04  ''   ''   'T_ALV_SAIDA'   'dt_admissao'             'Data Admissão'                 '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  05  ''   ''   'T_ALV_SAIDA'   'pernr'                   'Matricula'                     '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  06  ''   ''   'T_ALV_SAIDA'   'tipo_2'                  'Tipo'                          '25'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  07  ''   ''   'T_ALV_SAIDA'   'competencia'             'Competencia'                   '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  08  ''   ''   'T_ALV_SAIDA'   'categoria'               'Categoria'                     '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  09  ''   ''   'T_ALV_SAIDA'   'valor_principal'         'Valor Principal'               '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  10  ''   ''   'T_ALV_SAIDA'   'fgts_13'                 'Valor 13º'                     '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  11  ''   ''   'T_ALV_SAIDA'   'ausencia_fgts'           'Aus. FGTS'                     '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_estrutura_alv
*&---------------------------------------------------------------------*
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
                           VALUE(p_fix).                                    "17

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


ENDFORM.

INCLUDE zhcmr_fgts_hist_remuneracoei01.
