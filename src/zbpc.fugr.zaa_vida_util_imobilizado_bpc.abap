FUNCTION zaa_vida_util_imobilizado_bpc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      TVIDA_UTIL_IMOBILIZADO_BPC STRUCTURE
*"        ZVIDA_UTIL_IMOBILIZADO_BPC
*"      TRETVIDA_UTIL_IMOBILIZADO_BPC STRUCTURE
*"        ZRETVIDA_UTIL_IMOBILIZADO_BPC
*"----------------------------------------------------------------------
  it_vida_util_imobilizado[]    = tvida_util_imobilizado_bpc[].

  DATA: v_data_i          TYPE sy-datum,
        v_data_f          TYPE sy-datum,
        wa_tretvida_util  TYPE zretvida_util_imobilizado_bpc,
        sw                TYPE i.


  LOOP AT it_vida_util_imobilizado INTO wa_vida_util_imobilizado.
    IF wa_vida_util_imobilizado-campo EQ 'DATA_EXEC'.
      p_dt_exec-low    = wa_vida_util_imobilizado-valor_de.
      p_dt_exec-high   = wa_vida_util_imobilizado-valor_ate.
      p_dt_exec-sign   = wa_vida_util_imobilizado-sign.
      p_dt_exec-option = wa_vida_util_imobilizado-option.
      APPEND p_dt_exec.
    ENDIF.
  ENDLOOP.


  IF p_dt_exec[] IS NOT INITIAL.
    " Pego o inicio do mes
    CONCATENATE p_dt_exec-low(4) p_dt_exec-low+4(2) '01' INTO v_data_i .
  ELSE.
    CONCATENATE sy-datum(4) sy-datum+4(2) '01' INTO v_data_i .
  ENDIF.


  " Pego o Final do mes
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = v_data_i
    IMPORTING
      last_day_of_month = v_data_f.


  " Verifica se o mes já foi processado e esta na tabela zfit0044.

  SELECT *
   INTO TABLE it_zfit0044
   FROM zfit0044
  WHERE data_aqui EQ  v_data_f .

  IF it_zfit0044 IS INITIAL.
    CLEAR: it_zfit0044.

    PERFORM : zseleciona_dados_vida_util,
              zprocessa_retorno_vida_util.

    SELECT *
     INTO TABLE it_zfit0044
     FROM zfit0044
    WHERE data_aqui EQ  v_data_f .

    LOOP AT it_zfit0044 INTO wa_zfit0044.

      wa_tretvida_util-empresa           = wa_zfit0044-empresa.
      wa_tretvida_util-nu_imobilizado    = wa_zfit0044-nu_imobilizado.
      wa_tretvida_util-su_imobilizado    = wa_zfit0044-su_imobilizado.
      wa_tretvida_util-data_aquis        = wa_zfit0044-data_aqui.
      wa_tretvida_util-classe_imob       = wa_zfit0044-classe_imob.
      wa_tretvida_util-det_conta         = wa_zfit0044-det_conta.
      wa_tretvida_util-area_aval         = wa_zfit0044-area_aval.
      wa_tretvida_util-vida_plan_anos    = wa_zfit0044-vida_plan_anos.
      wa_tretvida_util-vida_plan_per     = wa_zfit0044-vida_plan_per.
      wa_tretvida_util-valor_aqui_acu    = wa_zfit0044-valor_aqui_acu.
      wa_tretvida_util-conta_razao       = wa_zfit0044-conta_razao.
      wa_tretvida_util-exercicio         = wa_zfit0044-exercicio.
      wa_tretvida_util-moeda             = wa_zfit0044-moeda.
      wa_tretvida_util-valor_aqui_per    = wa_zfit0044-valor_aqui_per.
      wa_tretvida_util-vida_plan_anos_f  = wa_zfit0044-vida_plan_anos_f.

      APPEND wa_tretvida_util TO tretvida_util_imobilizado_bpc.


    ENDLOOP.

  ELSE.

    LOOP AT it_zfit0044 INTO wa_zfit0044.

      wa_tretvida_util-empresa           = wa_zfit0044-empresa.
      wa_tretvida_util-nu_imobilizado    = wa_zfit0044-nu_imobilizado.
      wa_tretvida_util-su_imobilizado    = wa_zfit0044-su_imobilizado.
      wa_tretvida_util-data_aquis        = wa_zfit0044-data_aqui.
      wa_tretvida_util-classe_imob       = wa_zfit0044-classe_imob.
      wa_tretvida_util-det_conta         = wa_zfit0044-det_conta.
      wa_tretvida_util-area_aval         = wa_zfit0044-area_aval.
      wa_tretvida_util-vida_plan_anos    = wa_zfit0044-vida_plan_anos.
      wa_tretvida_util-vida_plan_per     = wa_zfit0044-vida_plan_per.
      wa_tretvida_util-valor_aqui_acu    = wa_zfit0044-valor_aqui_acu.
      wa_tretvida_util-conta_razao       = wa_zfit0044-conta_razao.
      wa_tretvida_util-exercicio         = wa_zfit0044-exercicio.
      wa_tretvida_util-moeda             = wa_zfit0044-moeda.
      wa_tretvida_util-valor_aqui_per    = wa_zfit0044-valor_aqui_per.
      wa_tretvida_util-vida_plan_anos_f  = wa_zfit0044-vida_plan_anos_f.

      APPEND wa_tretvida_util TO tretvida_util_imobilizado_bpc.


    ENDLOOP.
  ENDIF.

  CLEAR: wa_tretvida_util, wa_zfit0044,it_zfit0044.


  "tretvida_util_imobilizado_bpc[] = it_retvida_util_imobilizado[].




ENDFUNCTION.
