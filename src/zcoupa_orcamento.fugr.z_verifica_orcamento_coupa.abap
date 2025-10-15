FUNCTION z_verifica_orcamento_coupa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_CLASS_CONTABIL_COUPA) TYPE
*"        ZCLASSIFICACAO_CONTABIL_COUPA
*"     REFERENCE(IS_PARAMETROS_ORCAMENTO) TYPE
*"        ZPARAMETROS_VERIFCA_ORCAMENTO
*"  EXPORTING
*"     REFERENCE(EV_STATUS) TYPE  STRING
*"     REFERENCE(EV_SALDO) TYPE  STRING
*"     REFERENCE(EV_MENSAGEM) TYPE  STRING
*"  EXCEPTIONS
*"      CLASS_CONTABIL_COUPA_INVALIDA
*"----------------------------------------------------------------------

  CONCATENATE 'LCL_CHECK_BUDGET_' iv_class_contabil_coupa INTO DATA(l_object_reference).

  TRY.
      CREATE OBJECT go_check_budget TYPE (l_object_reference)
        EXPORTING
          is_parametros_orcamento = is_parametros_orcamento.
    CATCH cx_sy_create_object_error.
      RAISE class_contabil_coupa_invalida.
  ENDTRY.

  go_check_budget->check_budget_value( IMPORTING
                                        ev_status   = ev_status
                                        ev_saldo    = ev_saldo
                                        ev_mensagem = ev_mensagem ).

ENDFUNCTION.
