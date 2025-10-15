*INTERFACE if_check_budget.
*
*  METHODS get_budget_value.
*
*  DATA: gv_empresa      TYPE bukrs,
*        gv_centro_custo TYPE kostl,
*        gv_conta_razao  TYPE saknr.
*
*ENDINTERFACE.
*
*CLASS lcl_check_budget_k DEFINITION. "Verificar orçamento: Centro de Custo
*
*  PUBLIC SECTION.
*
*    INTERFACES if_check_budget.
*
*    METHODS constructor
*      IMPORTING
*        iv_empresa      TYPE bukrs OPTIONAL
*        iv_centro_custo TYPE kostl OPTIONAL
*        iv_conta_razao  TYPE saknr OPTIONAL.
*
*  PROTECTED SECTION.
*
*  PRIVATE SECTION.
*
*ENDCLASS.
*
*CLASS lcl_check_budget_a DEFINITION. "Verificar orçamento: Imobilizado
*
*  PUBLIC SECTION.
*
*    INTERFACES if_check_budget.
*
*    METHODS constructor.
*
*  PROTECTED SECTION.
*
*  PRIVATE SECTION.
*
*ENDCLASS.
*
*CLASS lcl_check_budget_fi DEFINITION. "Verificar orçamento: Ordem de investimento
*
*  PUBLIC SECTION.
*
*    INTERFACES if_check_budget.
*
*    METHODS constructor.
*
*  PROTECTED SECTION.
*
*  PRIVATE SECTION.
*
*ENDCLASS.
*
*CLASS lcl_check_budget_fm DEFINITION. "Verificar orçamento: Ordem de Manutenção
*
*  PUBLIC SECTION.
*
*    INTERFACES if_check_budget.
*
*    METHODS constructor.
*
*  PROTECTED SECTION.
*
*  PRIVATE SECTION.
*
*ENDCLASS.
*
*CLASS lcl_check_budget_k IMPLEMENTATION. "Verificar orçamento: Centro de Custo
*
*  METHOD constructor.
*    me->if_check_budget~gv_empresa      = iv_empresa.
*    me->if_check_budget~gv_centro_custo = iv_centro_custo.
*    me->if_check_budget~gv_conta_razao  = iv_conta_razao.
*
*  ENDMETHOD.
*
*  METHOD if_check_budget~get_budget_value.
*
*    DATA: lo_data TYPE REF TO data.
*
*    FIELD-SYMBOLS: <out_data> TYPE ANY TABLE.
*
*    cl_salv_bs_runtime_info=>set(
*       EXPORTING
*         display  = abap_false
*         metadata = abap_false
*         data     = abap_true
*      ).
*
*    SUBMIT gp4qlqkh46vzj3p1a40a23s3hjg050
*      WITH $1kokre     EQ space
*      WITH $1gjahr     EQ sy-datum(4)
*      WITH $1periv     EQ '1'
*      WITH $1perib     EQ '12'
*      WITH $1verp      EQ '0'
*      WITH _1koset-low EQ me->if_check_budget~gv_centro_custo
*      WITH _1kstar-low EQ me->if_check_budget~gv_conta_razao
*      AND RETURN.
*
*    TRY.
*
*        cl_salv_bs_runtime_info=>get_data_ref(
*              IMPORTING
*                r_data = lo_data
*        ).
*
*        ASSIGN lo_data->* TO <out_data>.
*
*      CATCH cx_salv_bs_sc_runtime_info.
*
*    ENDTRY.
*
*  ENDMETHOD.
*
*ENDCLASS.
*
*CLASS lcl_check_budget_a IMPLEMENTATION. "Verificar orçamento: Imobilizado
*
*  METHOD constructor.
*
*  ENDMETHOD.
*
*  METHOD if_check_budget~get_budget_value.
*
*  ENDMETHOD.
*
*ENDCLASS.
*
*CLASS lcl_check_budget_fi IMPLEMENTATION. "Verificar orçamento: Ordem de investimento
*
*  METHOD constructor.
*
*  ENDMETHOD.
*
*  METHOD if_check_budget~get_budget_value.
*
*  ENDMETHOD.
*
*ENDCLASS.
*
*CLASS lcl_check_budget_fm IMPLEMENTATION. "Verificar orçamento: Ordem de Manutenção
*
*  METHOD constructor.
*
*  ENDMETHOD.
*
*  METHOD if_check_budget~get_budget_value.
*
*  ENDMETHOD.
*
*ENDCLASS.
