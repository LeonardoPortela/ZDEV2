FUNCTION zpm_cal_date_plan.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DATE_INICIO) TYPE  SY-DATUM
*"     REFERENCE(I_DATE_FIM) TYPE  SY-DATUM
*"  EXPORTING
*"     REFERENCE(E_STATUS) TYPE  ICON_D
*"----------------------------------------------------------------------
  DATA: qtde_dias_plan(8) TYPE c,
        qtde_dias(8)      TYPE c,
        per_desv          TYPE i,
        desvio(8)         TYPE c.

*  uso(8)    TYPE c,
*         desvio(8) TYPE c,
*         per_desv  TYPE i,


  "Calcular o percetual com base nas datas de planejamento.

  TRY.
      "Verificar se o prozo esta vencido.
      IF i_date_inicio IS NOT INITIAL AND i_date_fim IS NOT INITIAL.
        CLEAR: qtde_dias_plan, qtde_dias, per_desv, desvio, e_status.

        IF sy-datum > i_date_fim.
          "Prazo vencido.
          e_status = '@0A@'. "Red
        ELSE.
          "Prazo perto de vencer.
          IF sy-datum EQ i_date_fim.
            e_status = '@09@'. "Yellow
          ELSE.
            IF i_date_inicio > sy-datum.
              e_status = '@08@'. "Green
            ELSE.
              qtde_dias_plan =  i_date_fim - i_date_inicio. "Quantidade de dias planejado.
              CONDENSE qtde_dias_plan NO-GAPS.

              qtde_dias      =  i_date_fim - sy-datum. "Quantidade dias que falta vencer.
              CONDENSE qtde_dias  NO-GAPS.

              desvio = qtde_dias_plan - qtde_dias. "Quantidade de dias que ja realizado.
              CONDENSE desvio  NO-GAPS.

              per_desv = ( desvio / qtde_dias_plan ) * 100. "Percentual.

              IF per_desv > 80.
                e_status = '@09@'. "Yellow
              ELSE.
                e_status = '@08@'. "Green
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    CATCH cx_sy_zerodivide.

  ENDTRY.





ENDFUNCTION.
