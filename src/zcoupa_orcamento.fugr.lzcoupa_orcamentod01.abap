INTERFACE if_check_budget. "Verificar orçamento

  METHODS check_budget_value
    EXPORTING
      ev_status   TYPE string
      ev_saldo    TYPE string
      ev_mensagem TYPE string.

  METHODS set_return_for_budget_value
    IMPORTING
      iv_result   TYPE bapicurr_d
      iv_no_valid TYPE xflag OPTIONAL
    EXPORTING
      ev_status   TYPE string
      ev_saldo    TYPE string
      ev_mensagem TYPE string.

ENDINTERFACE.

CLASS lcl_check_budget_k DEFINITION. "Verificar orçamento: Centro de Custo

  PUBLIC SECTION.

    INTERFACES if_check_budget.

    METHODS constructor
      IMPORTING
        is_parametros_orcamento TYPE zparametros_verifca_orcamento.

    METHODS get_planned_costs
      RETURNING
        VALUE(rv_planned_cost) TYPE bapicurr_d.

    METHODS get_real_costs
      RETURNING
        VALUE(rv_real_cost) TYPE bapicurr_d.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: gs_parametros_orcamento TYPE zparametros_verifca_orcamento.

ENDCLASS.

CLASS lcl_check_budget_a DEFINITION. "Verificar orçamento: Imobilizado

  PUBLIC SECTION.

    INTERFACES if_check_budget.

    METHODS constructor
      IMPORTING
        is_parametros_orcamento TYPE zparametros_verifca_orcamento.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: gs_parametros_orcamento TYPE zparametros_verifca_orcamento.

ENDCLASS.

CLASS lcl_check_budget_fi DEFINITION. "Verificar orçamento: Ordem de investimento

  PUBLIC SECTION.

    INTERFACES if_check_budget.

    METHODS constructor
      IMPORTING
        is_parametros_orcamento TYPE zparametros_verifca_orcamento.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: gs_parametros_orcamento TYPE zparametros_verifca_orcamento.

ENDCLASS.

CLASS lcl_check_budget_fm DEFINITION. "Verificar orçamento: Ordem de Manutenção

  PUBLIC SECTION.

    INTERFACES if_check_budget.

    METHODS constructor
      IMPORTING
        is_parametros_orcamento TYPE zparametros_verifca_orcamento.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: gs_parametros_orcamento TYPE zparametros_verifca_orcamento.

ENDCLASS.

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

CLASS lcl_co_utilities DEFINITION. "Utilidades módulo CO.

  PUBLIC SECTION.

    CLASS-METHODS: get_controlling_area
      IMPORTING
        iv_bukrs        TYPE bukrs
        iv_gsber        TYPE gsber
      RETURNING
        VALUE(rv_kokrs) TYPE kokrs.

    CLASS-METHODS: get_currency_value_brl
      IMPORTING
        iv_currency    TYPE tcurr-fcurr
      RETURNING
        VALUE(rv_taxa) TYPE tcurr-ukurs.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.
