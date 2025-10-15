FUNCTION zpm_get_position_cont_epto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_EQUNR) TYPE  EQUNR
*"  EXPORTING
*"     VALUE(E_VALUE) TYPE  IMRC_CNTRC
*"----------------------------------------------------------------------

  DATA: integ_comb TYPE REF TO zcl_integ_comb.
  CREATE OBJECT integ_comb.


  "Selecionar pontos de medição equipamento.
  integ_comb->zif_integracao_comb~set_ponto_medicao( ).

  integ_comb->zif_integracao_comb~at_epto = |{ i_equnr ALPHA = IN }|.

  LOOP AT integ_comb->zif_integracao_comb~at_ponto_medicao  ASSIGNING FIELD-SYMBOL(<w_dimpt>) WHERE indtr NE abap_true AND ( atnam EQ 'HORIMETRO' ) OR ( atnam EQ 'ODOMETRO' ).
    IF sy-subrc EQ 0.
      zcl_int_sappm_autotrac=>m_check_pont_med(
        EXPORTING
          i_date  = sy-datum
          i_time  = sy-uzeit
          i_point = <w_dimpt>-point  " Ponto de medição
        IMPORTING
          e_value = e_value          " Unidade de medida ao entrar documento
      ).

      CONDENSE e_value NO-GAPS.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
