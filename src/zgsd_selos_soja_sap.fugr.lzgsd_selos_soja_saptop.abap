FUNCTION-POOL zgsd_selos_soja_sap.          "MESSAGE-ID ..

******************************************************************
* tabelas
******************************************************************
TABLES: zpmt0056.

******************************************************************
* type pools
******************************************************************
TYPE-POOLS: pbr99, ibco2.

******************************************************************
* types
******************************************************************
TYPES: BEGIN OF ty_semana,
         week   TYPE scal-week,
         dt_ini TYPE sy-datum,
         dt_fim TYPE sy-datum.
TYPES: END OF ty_semana.

TYPES: BEGIN OF ty_zpmx0055.
         INCLUDE STRUCTURE zpmt0055.
         TYPES:   acao       TYPE char10,
         selo       TYPE char40,
         ciclo_desc TYPE char40.
TYPES: END OF ty_zpmx0055.

******************************************************************
* variaveis globais
******************************************************************
DATA: g_control_parameters      TYPE ssfctrlop,
      g_output_options          TYPE ssfcompop,
      l_docnum                  TYPE j_1bnfdoc-docnum,
      l_safra                   TYPE zdepm_safra,
      l_matnr                   TYPE mara-matnr,
      l_xml_doc                 TYPE string,
      w_j1bnfdoc                TYPE j_1bnfdoc,
      w_j1bnflin                TYPE j_1bnflin,
      wg_xml_sefaz              TYPE znfe_xml_sefaz_auth,
      t_element_array           TYPE zde_element_array_t,
*
      t_semana                  TYPE TABLE OF ty_semana,
      w_semana                  TYPE ty_semana,
      t_zpmt0054                TYPE TABLE OF zpmt0054,
      t_zpmt0055                TYPE TABLE OF zpmt0055,
      w_zpmt0054                TYPE zpmt0054,
      w_zpmt0055                TYPE zpmt0055,
      w_zpmt0056                TYPE zpmt0056,
      t_zpmx0055                TYPE TABLE OF ty_zpmx0055,
      w_zpmx0055                TYPE ty_zpmx0055,
      t_idd07v                  TYPE TABLE OF dd07v,
      w_idd07v                  TYPE dd07v,
      t_idd07v2                 TYPE TABLE OF dd07v,
      w_idd07v2                 TYPE dd07v,
      l_data_ref                TYPE sy-datum,
      l_dt_ini                  TYPE sy-datum,
      l_dt_fim                  TYPE sy-datum,
      l_dt_aux                  TYPE sy-datum,
      l_dt_monday               TYPE sy-datum,
      l_dt_sunday               TYPE sy-datum,
      l_week                    TYPE scal-week,
      l_week_ini                TYPE scal-week,
      l_week_fim                TYPE scal-week,
      l_semana_atu              TYPE numc2,
      l_semana_ini              TYPE numc2,
      l_semana_fim              TYPE numc2,
      l_ano_fim                 TYPE numc2,
      l_ano_atu                 TYPE numc2,
      l_ciclo                   TYPE numc2,
      l_lote01                  TYPE numc2,
      l_lote02                  TYPE numc2,
      l_lote01_ant              TYPE numc2,
      l_lote02_ant              TYPE numc2,
      l_lote_ano                TYPE numc2,
      l_chave01                 TYPE numc4,
      l_chave02                 TYPE numc4,
      l_set                     TYPE char1,
      l_cont                    TYPE numc2,
*
      l_selo                    TYPE zpmt0054-imagem,
      l_data_fabricacao         TYPE zpmt0054-data,
      l_data_validade           TYPE zpmt0054-data,
      l_numero_lote             TYPE zpmt0054-lote,
*
*** DANFE - Dados de cabeçalho
      t_danfe_cabecalho         TYPE STANDARD TABLE OF zbrnfe_danfe_cabecalho  WITH HEADER LINE,
*** DANFE - Dados de Item
      t_zbrnfe_danfe_item       TYPE STANDARD TABLE OF zbrnfe_danfe_item       WITH HEADER LINE,
*** DANFE - Descrição de Item Múltiplas Linhas
      t_zbrnfe_danfe_item_desc  TYPE STANDARD TABLE OF zbrnfe_danfe_item_desc  WITH HEADER LINE,
*** DANFE - Dados de fatura
      t_zbrnfe_danfe_fatura     TYPE STANDARD TABLE OF zbrnfe_danfe_fatura     WITH HEADER LINE,
*** DANFE - Dados CFOP
      t_zbrnfe_danfe_cfop       TYPE STANDARD TABLE OF zbrnfe_danfe_cfop       WITH HEADER LINE,
*** DANFE - Dados adicionais
      t_zbrnfe_danfe_dados_adic TYPE STANDARD TABLE OF zbrnfe_danfe_dados_adic WITH HEADER LINE,
*
*** Dados de endereco da filial
      g_sadr_branch             TYPE sadr,
      g_addr_branch             TYPE addr1_val,

*** Estruturas para busca de CNPJ
      lc_address                TYPE sadr,
      lc_branch_data            TYPE j_1bbranch,
      lc_cgc_number             TYPE j_1bwfield-cgc_number,
      lc_address1               TYPE addr1_val,
      lc_cgc_aux                TYPE pbr99_cgc,
*
*** Nome do módulo de função
      ls_funcname               TYPE rs38l_fnam,

*** Descrição do CFOP
      is_cfotxt                 TYPE j_1bagt-cfotxt,
*
      l_document_output_info    TYPE  ssfcrespd,
      l_job_output_info         TYPE  ssfcrescl,
      l_job_output_options      TYPE  ssfcresop,

      otfdata                   TYPE tsfotf,
      l_xstring_document        TYPE xstring,
      t_lines                   TYPE STANDARD TABLE OF tline,
      l_bin_fsize               TYPE i,
      st_job_output_info        TYPE ssfcrescl,
*
      tree1                     TYPE REF TO cl_hrpayna_gui_alv_tree, "cl_gui_alv_tree.
      mr_toolbar                TYPE REF TO cl_gui_toolbar,
      g_container               TYPE scrfname VALUE 'CONTAINER',
      g_container2              TYPE scrfname VALUE 'CONTAINER2',
      g_custom_container        TYPE REF TO cl_gui_custom_container,
      g_custom_container2       TYPE REF TO cl_gui_custom_container,
      g_grid                    TYPE REF TO cl_gui_alv_grid,
      g_grid2                   TYPE REF TO cl_gui_alv_grid,
      w_tool                    TYPE stb_button,
      ok_code                   TYPE sy-ucomm,
      l_ok                      TYPE c,
      l_ped_imp                 TYPE c,
*
      t_fieldcatalog            TYPE lvc_t_fcat, "Fieldcatalog
      t_exctab                  TYPE slis_t_extab,
      w_exctab                  TYPE slis_extab,
      w_item_layout             TYPE lvc_s_laci,
      w_layout                  TYPE lvc_s_layo,
      ls_fieldcatalog           TYPE lvc_s_fcat,
      ls_exclude                TYPE ui_func,
      pt_exclude                TYPE ui_functions,
      pt_exclude2               TYPE ui_functions,
      t_del_rows                TYPE lvc_t_row,
      w_del_rows                TYPE lvc_s_row,
      t_sel_cols                TYPE lvc_t_col,
      w_sel_cols                TYPE lvc_s_col,
      l_row_id                  TYPE lvc_s_row,
      l_column_id               TYPE lvc_s_col,
      l_stable                  TYPE lvc_s_stbl,
      t_fcat_lvc                TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE,
      t_fcat_kkb                TYPE kkblo_t_fieldcat.

*******************************************************************************************
* classes / implementacao
*******************************************************************************************
CLASS lcl_event DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event2 DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

*******************************************************************************************
* includes
*******************************************************************************************
INCLUDE <icon>.

DATA: m_event_handler  TYPE REF TO lcl_event,
      m_event_handler2 TYPE REF TO lcl_event2.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event IMPLEMENTATION.

  METHOD toolbar.
*    FREE e_object->mt_toolbar.
*
*    CLEAR w_tool.
*    w_tool-function = 'INSERT'. "cl_gui_alv_grid=>mc_fc_loc_insert_row.
*    w_tool-text     = ''.
*    w_tool-icon     = '@17@'.
*    APPEND w_tool TO e_object->mt_toolbar.
*
*    CLEAR w_tool.
*    w_tool-function = 'DELETE'. "cl_gui_alv_grid=>mc_fc_loc_delete_row.
*    w_tool-text     = ''.
*    w_tool-icon     = '@18@'.
*    APPEND w_tool TO e_object->mt_toolbar.

  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.
    CASE e_ucomm.

      WHEN 'INSERT'.

      WHEN 'DELETE'.

    ENDCASE.

    CALL METHOD g_grid->refresh_table_display.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event2 IMPLEMENTATION.

  METHOD toolbar.

  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.

    l_stable-row = 'X'.
    l_stable-col = 'X'.

    CALL METHOD g_grid2->refresh_table_display
      EXPORTING
        is_stable = l_stable.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.
******************************************************************
******************************************************************
