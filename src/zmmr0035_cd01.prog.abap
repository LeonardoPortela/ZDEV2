CLASS lcl_utilities DEFINITION. "Utilidades para todos as clases.

  PUBLIC SECTION.

    CLASS-METHODS: calculate_days_by_datum
      IMPORTING
        iv_number_of_days TYPE i
      RETURNING
        VALUE(rv_date)    TYPE d.

    CLASS-METHODS: remover_zeros_esquerda
      IMPORTING
        iv_input TYPE clike
      EXPORTING
        ev_ouput TYPE clike.

    CLASS-METHODS: adicionar_zeros_esquerda
      IMPORTING
        iv_input TYPE clike
      EXPORTING
        ev_ouput TYPE clike.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

INTERFACE if_process_lookup_values. "Interface modelo de execução do processo.

  "Métodos
  METHODS select_data.

  METHODS build_out_data.

  METHODS execute_process
    EXPORTING
      et_import_data TYPE zcoupa_import_data_tab.

  "Atributos
  DATA: go_coupa_integration_log TYPE REF TO lcl_coupa_integration_log,
        gt_import_data           TYPE zcoupa_import_data_tab.

ENDINTERFACE.

CLASS lcl_coupa_integration_log DEFINITION. "Classe responsável por administrar log de integração - Tabela ZINTEGRCOUPA01.

  PUBLIC SECTION.

    METHODS insert_new_log_key
      IMPORTING
        iv_zcoupa_integration_key TYPE zcoupa_integration_key.

    METHODS set_executed_status
      IMPORTING
        iv_zcoupa_integration_key TYPE zcoupa_integration_key.

    METHODS check_existence_of_key
      IMPORTING
        iv_zcoupa_integration_key TYPE zcoupa_integration_key
      RETURNING
        VALUE(rv_existence)       TYPE flag.

    METHODS save_log.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: gt_zintegrcoupa01 TYPE STANDARD TABLE OF zintegrcoupa01.

ENDCLASS.

CLASS lcl_process_lookup_values_e DEFINITION. "Importar dados Empresa

  PUBLIC SECTION.

    INTERFACES if_process_lookup_values.

    METHODS constructor
      IMPORTING
        iv_days TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: gv_amount_days_ago TYPE i,
          gv_base_date       TYPE d.

ENDCLASS.

CLASS lcl_process_lookup_values_cl DEFINITION. "Importar dados Centro Logístico

  PUBLIC SECTION.

    INTERFACES if_process_lookup_values.

    METHODS constructor
      IMPORTING
        iv_days TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    "Estrutura ABAP de acordo com estrutura do XML que será utilizado na API.
    DATA: gt_coupa_cl TYPE zcoupa_cl_tab,
          gs_coupa_cl TYPE zcoupa_cl.

    " => ZT_COUPA_CL -> Transformação utilizada nesse processo - STRANS - TCODE.

    DATA: gv_amount_days_ago TYPE i,
          gv_base_date       TYPE d.

ENDCLASS.

CLASS lcl_process_lookup_values_ccc DEFINITION. "Importar dados Categoria de Classificação Contábil

  PUBLIC SECTION.

    INTERFACES if_process_lookup_values.

    METHODS constructor
      IMPORTING
        iv_days TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: gv_amount_days_ago TYPE i,
          gv_base_date       TYPE d.

ENDCLASS.

CLASS lcl_process_lookup_values_cc DEFINITION. "Importar dados Centro de Custo / Aprovadores

  PUBLIC SECTION.

    INTERFACES if_process_lookup_values.

    METHODS constructor
      IMPORTING
        iv_days TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    "Estrutura ABAP de acordo com estrutura do XML que será utilizado na API.
    DATA: gt_coupa_cc TYPE zcoupa_cc_tab,
          gs_coupa_cc TYPE zcoupa_cc.

    " => ZT_COUPA_CC -> Transformação utilizada nesse processo - STRANS - TCODE.

    DATA: gv_amount_days_ago TYPE i,
          gv_base_date       TYPE d.

ENDCLASS.

CLASS lcl_process_lookup_values_oc DEFINITION. "Importar dados Objeto Contábil

  PUBLIC SECTION.

    INTERFACES if_process_lookup_values.

    METHODS constructor
      IMPORTING
        iv_days TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: tt_csks TYPE STANDARD TABLE OF ty_csks.

    "Estrutura ABAP de acordo com estrutura do XML que será utilizado na API.
    DATA: gt_coupa_oc TYPE zcoupa_oc_tab,
          gs_coupa_oc TYPE zcoupa_oc.

    " => ZT_COUPA_OC -> Transformação utilizada nesse processo - STRANS - TCODE.

    DATA: gt_csks     TYPE tt_csks.

    DATA: gv_amount_days_ago TYPE i,
          gv_base_date       TYPE d.

    DATA: rg_imobilizado  TYPE RANGE OF anla-anln1,
          rg_ordem_manu   TYPE RANGE OF coas-aufnr,
          rg_ordem_inve   TYPE RANGE OF coas-aufnr,
          rg_centro_custo TYPE RANGE OF csks-kostl.

ENDCLASS.

CLASS lcl_process_lookup_values_to DEFINITION. "Importar dados Tipo de Operação

  PUBLIC SECTION.

    INTERFACES if_process_lookup_values.

    METHODS constructor
      IMPORTING
        iv_days TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    "Estrutura ABAP de acordo com estrutura do XML que será utilizado na API.
    DATA: gt_coupa_to TYPE zcoupa_to_tab,
          gs_coupa_to TYPE zcoupa_to.

    " => ZT_COUPA_TO -> Transformação utilizada nesse processo - STRANS - TCODE.

    DATA: gv_amount_days_ago TYPE i,
          gv_base_date       TYPE d.

ENDCLASS.

CLASS lcl_process_lookup_values_cr DEFINITION. "Importar dados Conta Razão

  PUBLIC SECTION.

    INTERFACES if_process_lookup_values.

    METHODS constructor
      IMPORTING
        iv_days TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: tt_ska1 TYPE STANDARD TABLE OF ty_ska1.

    "Estrutura ABAP de acordo com estrutura do XML que será utilizado na API.
    DATA: gt_coupa_cr TYPE zcoupa_cr_tab,
          gs_coupa_cr TYPE zcoupa_cr.

    " => ZT_COUPA_CR -> Transformação utilizada nesse processo - STRANS - TCODE.

    DATA: gv_amount_days_ago TYPE i,
          gv_base_date       TYPE d.

    DATA: gt_ska1 TYPE tt_ska1.

ENDCLASS.

CLASS lcl_alv_return DEFINITION. "ALV de retorno do processamento.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS append_new_line
      IMPORTING
        iv_import_data     TYPE zcoupa_import_data
        iv_integration_log TYPE zintegracao_log.

    METHODS display.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS config_alv_columns.

    METHODS config_alv_functions.

    METHODS config_alv_events.

    METHODS on_link_click
        FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        row
        column.

    DATA: gt_alv_data TYPE zcoupa_import_data_alv_tab.

    DATA: go_alv     TYPE REF TO cl_salv_table,
          go_events  TYPE REF TO cl_salv_events_table,
          go_funct   TYPE REF TO cl_salv_functions,
          go_columns TYPE REF TO cl_salv_columns_table,
          go_column  TYPE REF TO cl_salv_column_table.

ENDCLASS.
