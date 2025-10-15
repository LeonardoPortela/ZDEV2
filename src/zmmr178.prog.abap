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
**| Integração ESG - SIS                                                      |*
**/===========================================================================\*

REPORT zmmr178.

TABLES: sscrfields, t001w,cskb,csks,cobk.

SELECTION-SCREEN BEGIN OF BLOCK blo_1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: o_emp FOR t001w-werks NO-EXTENSION NO INTERVALS,
                o_kostl FOR csks-kostl,
                o_kstar FOR cskb-kstar,
                o_budat FOR cobk-budat NO-EXTENSION.

SELECTION-SCREEN END OF BLOCK blo_1.


*// Tipos de Dados
DATA: BEGIN OF it_saida OCCURS 0.
        INCLUDE STRUCTURE kaep_coac.
      DATA: END OF it_saida.

DATA: gob_gui_alv_grid TYPE REF TO cl_gui_alv_grid,
      git_fieldcat     TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY,
      git_sel_rows     TYPE lvc_t_row,
      gwa_variant      TYPE disvariant,
      gwa_layout       TYPE lvc_s_layo.


DATA: p_tcode  TYPE sy-tcode,
      p_belnr  TYPE co_belnr,
      p_area   TYPE tka01-kokrs VALUE 'MAGI',  "// Area de Custo

      r_kostl  TYPE RANGE OF csks-kostl,    "// Centro de Custo
      p_kstgr  TYPE rkpln-ksgru,  "// Grupo e centros de custo

      r_kstar  TYPE RANGE OF cskb-kstar,   "// Classe de Custo
      p_koagr  TYPE rkpln-kagru,           "// Grupo de Classe de custo

      r_budat  TYPE RANGE OF cobk-budat,   "// Data de Lançamento
      p_maxsel TYPE kaep_sett-maxsel,

      r_ordem  TYPE RANGE OF kaep_coac-pobid.


FIELD-SYMBOLS: <t_data>      TYPE ANY TABLE,
               <t_data_line> TYPE ANY TABLE,
               <w_data>      TYPE any,
               <w_data_line> TYPE any.

DATA: l_data            TYPE REF TO data,
      l_data_line       TYPE REF TO data,
      l_data_descr      TYPE REF TO cl_abap_datadescr,
      l_data_line_descr TYPE REF TO cl_abap_datadescr.

DATA: go_int TYPE REF TO zcl_integracao_sis.

SELECTION-SCREEN: FUNCTION KEY 1,  "// FC01 - Get Empresas SIS
                  FUNCTION KEY 2.  "// FC02 - De/para Empresa SAP x SIS

INITIALIZATION.

  sscrfields =
    VALUE #(
              functxt_01 = |{ icon_next_hierarchy_level }     { text-021 }|
              functxt_02 = |{ icon_previous_hierarchy_level } { text-022 }|
            ).



AT SELECTION-SCREEN.
  DATA(direcao) = COND #(
                          WHEN sy-ucomm EQ 'FC01' THEN 1
                          WHEN sy-ucomm EQ 'FC02' THEN 2
                        ).

  PERFORM fm_executar_api USING direcao.

START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.

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
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .
  CALL SCREEN 0100.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona.
  PERFORM fm_dados_seleciona_ksb1.
  PERFORM fm_dados_seleciona_kob1.
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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100'.
  PERFORM fm_criar_objetos.
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
    WHEN 'SEND_SIS'.
      PERFORM fm_send_sis.
  ENDCASE.
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

  DATA: i_filtros TYPE zif_screen_linha_filtro_t,
        vl_text   TYPE sdydo_text_element.

  PERFORM fm_cria_fieldcat.

  vl_text = text-003.

  zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      EXPORTING
         i_titulo  = CONV #( vl_text )
       CHANGING
         alv = gob_gui_alv_grid
       ).

  gwa_layout =
  VALUE #(
           sel_mode   = 'A'
           box_fname  = abap_true
           ctab_fname = 'CELLCOLOR'
         ).

  gwa_variant-report = sy-repid.

  CALL METHOD gob_gui_alv_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = gwa_layout
      is_variant                    = gwa_variant
      i_save                        = abap_true
    CHANGING
      it_outtab                     = it_saida[]
      it_fieldcatalog               = git_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

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

  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY,
         lit_fieldcat_log TYPE TABLE OF lvc_t_fcat WITH DEFAULT KEY.

  FREE git_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'kaep_coac'
    CHANGING
      ct_fieldcat      = git_fieldcat.

  LOOP AT git_fieldcat ASSIGNING FIELD-SYMBOL(<f_fieldcat>).
    <f_fieldcat>-col_opt = abap_true.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_RUN_TIME_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_prepare_run_time_info .


  IF <t_data> IS ASSIGNED.
    CLEAR: <t_data>[].
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR: <t_data_line>[].
  ENDIF.

  IF <t_data> IS ASSIGNED.
    CLEAR: <t_data>.
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR: <t_data_line>.
  ENDIF.

  FREE: l_data,  l_data_line,  l_data_descr,  l_data_line_descr.

  cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                          metadata = abap_false
                                          data     = abap_true ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_RUNTIME_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_runtime_info .

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
      IMPORTING r_data_descr  = l_data_descr
                r_data_line_descr = l_data_line_descr ).

      CHECK ( l_data_descr IS NOT INITIAL ) OR ( l_data_line_descr IS  NOT INITIAL ).

      CREATE DATA l_data      TYPE HANDLE  l_data_descr.
      CREATE DATA l_data_line TYPE HANDLE  l_data_line_descr.

      ASSIGN l_data->* TO <t_data>.
      ASSIGN l_data_line->* TO <t_data_line>.

      cl_salv_bs_runtime_info=>get_data( IMPORTING t_data  = <t_data>
                                                   t_data_line = <t_data_line> ).
    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  ASSIGN l_data->*        TO <w_data>.
  ASSIGN l_data_line->*   TO <w_data_line>.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SEND_SIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_send_sis .

  CALL METHOD gob_gui_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = git_sel_rows.


  CREATE OBJECT go_int
    EXPORTING
      i_servico = 'SIS_INDICADOR'.

  READ TABLE git_sel_rows INTO DATA(lwa_sel_rows) INDEX 1.
*  LOOP AT git_sel_rows INTO DATA(lwa_sel_rows).

  READ TABLE it_saida INTO DATA(lwa_saida) INDEX lwa_sel_rows-index.

  lwa_saida-matnr = '184924'.
  lwa_saida-gjahr = '2022'.
  lwa_saida-matnr = |{ lwa_saida-matnr ALPHA = IN }|.
  lwa_saida-werks = '1507'.
  lwa_saida-mbgbtr = '15'.

  go_int->zif_integracao_sis~set_indicador_sis( lwa_saida ).

*  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_EXECUTAR_API
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DIRECAO  text
*----------------------------------------------------------------------*
FORM fm_executar_api  USING    p_direcao.

  CASE p_direcao.
    WHEN 1.

      CREATE OBJECT go_int
        EXPORTING
          i_servico = 'SIS_EMPRESA'.

      go_int->zif_integracao_sis~get_empresas_sis( ).

    WHEN 2.

      SUBMIT zmmr179 AND RETURN.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA_KSB1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona_ksb1 .

  DATA: lit_texto TYPE TABLE OF string,
        lwa_texto TYPE string.

  p_tcode = 'KSB1'.
  p_maxsel = '999999999'.

  PERFORM f_prepare_run_time_info.

  SUBMIT rkaep000
           WITH p_tcode  EQ p_tcode
           WITH p_kokrs  EQ p_area
           WITH kostl    IN o_kostl
           WITH kstar    IN o_kstar
           WITH r_budat  IN o_budat
           WITH p_maxsel EQ p_maxsel
           EXPORTING LIST TO MEMORY AND RETURN.

  PERFORM f_get_runtime_info.

  IF <t_data> IS ASSIGNED.
    LOOP AT <t_data> ASSIGNING <w_data>.
      CLEAR: it_saida.
      MOVE-CORRESPONDING <w_data> TO it_saida.
      APPEND it_saida.
    ENDLOOP.
  ENDIF.

  FREE r_ordem.

  DELETE it_saida WHERE vrgng NE 'COIN'.

  LOOP AT it_saida INTO DATA(wa_saida) WHERE objnr_n1 IS NOT INITIAL.
    DATA(tabix) = sy-tabix.
    FREE lit_texto.

    SPLIT wa_saida-objnr_n1 AT space INTO TABLE lit_texto.

    READ TABLE lit_texto INTO lwa_texto INDEX 1.
    CHECK sy-subrc IS INITIAL.
    IF lwa_texto NE 'ORD' AND lwa_texto NE 'OPE'.
      CONTINUE.
    ENDIF.

    READ TABLE lit_texto INTO lwa_texto INDEX 2.

    CHECK sy-subrc IS INITIAL.
    CHECK lwa_texto IS NOT INITIAL.

    APPEND VALUE #( sign   = 'I' option = 'EQ' low = lwa_texto ) TO r_ordem.
    wa_saida-objnr_n2 = lwa_texto.
    MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING objnr_n2.
  ENDLOOP.

  SORT r_ordem BY low.
  DELETE ADJACENT DUPLICATES FROM r_ordem COMPARING low.

  DELETE it_saida WHERE mbgbtr IS INITIAL.
  DELETE it_saida WHERE objnr_n2 IN r_ordem.

  FREE: r_kostl.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA_KOB1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona_kob1 .

  p_tcode = 'KOB1'.

  CHECK r_ordem[] IS NOT INITIAL.

  PERFORM f_prepare_run_time_info.

  SUBMIT rkaep000
           WITH p_tcode EQ p_tcode
           WITH p_kokrs EQ p_area
           WITH aufnr IN r_ordem
           WITH kstgr EQ p_kstgr
           WITH kstar IN o_kstar
           WITH r_budat IN o_budat
           WITH p_maxsel EQ p_maxsel
           EXPORTING LIST TO MEMORY AND RETURN.

  PERFORM f_get_runtime_info.

  IF <t_data> IS ASSIGNED.
    LOOP AT <t_data> ASSIGNING <w_data>.
      CLEAR: it_saida.
      MOVE-CORRESPONDING <w_data> TO it_saida.
      APPEND it_saida.
    ENDLOOP.
  ENDIF.

  DELETE it_saida WHERE vrgng NE 'COIN'.
  DELETE it_saida WHERE mbgbtr IS INITIAL.


ENDFORM.
