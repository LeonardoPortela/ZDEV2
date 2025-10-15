*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 21.12.2023                                              &*
*& Descrição: Reenvio Ordem Vendas para o Trace Cotton                &*
*&--------------------------------------------------------------------&*
*& Projeto  : Algodão                                                 &*
*&--------------------------------------------------------------------&*
REPORT zsdr0170_job MESSAGE-ID zjob.

************************************************************************
* tabelas
************************************************************************
TABLES: zsdt0213_integra.

************************************************************************
*  parametro ID_CARGA
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS : s_nrosol FOR zsdt0213_integra-nro_sol_ov.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
*& variaveis globais
************************************************************************
DATA: t_zsdt0213_int TYPE TABLE OF zsdt0213_integra,
      t_status       TYPE zde_btcstatus_t,
      w_zsdt0213_int TYPE zsdt0213_integra,
      e_quantidade   TYPE i,
      lv_vzeit       TYPE i,              "*-IR 189210-14.01.2025-#163685-JT-inicio
      lv_diftime     TYPE i,              "*-IR 189210-14.01.2025-#163685-JT-inicio
      lv_time_ret    TYPE numc3,          "*-IR 189210-14.01.2025-#163685-JT-inicio
      lv_tentativas  TYPE numc3,          "*-IR 189210-14.01.2025-#163685-JT-inicio
      t_value        TYPE TABLE OF rgsb4. "*-IR 189210-14.01.2025-#163685-JT-inicio

************************************************************************
*&      Start-Of-Selection
************************************************************************
START-OF-SELECTION.

  FREE: t_status.

  APPEND 'R' TO t_status.

*---------------------------------------------
* se tem Job ativo, abandona
*---------------------------------------------
  IF sy-batch = abap_true.
    TRY .
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = sy-cprog    " Nome de um programa em uma etapa (p.ex. report)
            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
            i_status     = t_status    " Status de Jobs
          IMPORTING
            e_quantidade = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd > 1.
      EXIT.
    ENDIF.
  ENDIF.

*---------------------------------------------
*-Processamento
*---------------------------------------------
  PERFORM f_reenvia_ordem_venda.

************************************************************************
*&-processamento dos fardos
************************************************************************
FORM f_reenvia_ordem_venda.

*-IR 189210-14.01.2025-#163685-JT-inicio
*-------------------------------
*-- parametrizacao para reenvio
*-------------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'PARAM_TRACE_COTTON_OV'
      class           = '0000'
      no_descriptions = abap_off
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  LOOP AT t_value INTO DATA(w_value).
    IF     w_value-title = 'TEMPO_RETORNO_MINUTOS'.
      lv_time_ret        = w_value-from.
    ELSEIF w_value-title = 'TENTATIVAS'.
      lv_tentativas      = w_value-from.
    ENDIF.
  ENDLOOP.
*-IR 189210-14.01.2025-#163685-JT-fim

*----------------------------------------
* selecao ordens nao integradas
*----------------------------------------
  SELECT *
    FROM zsdt0213_integra
    INTO TABLE t_zsdt0213_int
   WHERE nro_sol_ov  IN s_nrosol
     AND integrado    = abap_false.

  CHECK t_zsdt0213_int[] IS NOT INITIAL.

*-IR 189210-14.01.2025-#163685-JT-inicio
*-------------------------------
* verifica nro de tentativas
*-------------------------------
  LOOP AT t_zsdt0213_int INTO w_zsdt0213_int.
    DATA(lv_tabix) = sy-tabix.

    w_zsdt0213_int-tenta_envio = w_zsdt0213_int-tenta_envio + 1.
    IF w_zsdt0213_int-tenta_envio > lv_tentativas.
      UPDATE zsdt0213_integra SET integrado  = abap_true
                            WHERE nro_sol_ov = w_zsdt0213_int-nro_sol_ov
                              AND posnr      = w_zsdt0213_int-posnr
                              AND vbeln      = w_zsdt0213_int-vbeln
                              AND metodo     = w_zsdt0213_int-metodo.
      DELETE t_zsdt0213_int INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

*-------------------------------
* verifica tempo de reenvio
*-------------------------------
  LOOP AT t_zsdt0213_int INTO w_zsdt0213_int.
    lv_tabix     = sy-tabix.

    IF w_zsdt0213_int-data_envio IS INITIAL OR
       w_zsdt0213_int-hora_envio IS INITIAL.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'SALP_SM_CALC_TIME_DIFFERENCE'
      EXPORTING
        date_1  = w_zsdt0213_int-data_envio
        time_1  = w_zsdt0213_int-hora_envio
        date_2  = sy-datum
        time_2  = sy-uzeit
      IMPORTING
        seconds = lv_vzeit.

    lv_diftime   = lv_vzeit / 60.

    IF lv_diftime < lv_time_ret.
      DELETE t_zsdt0213_int INDEX lv_tabix.
    ENDIF.
  ENDLOOP.
*-IR 189210-14.01.2025-#163685-JT-fim

  SORT t_zsdt0213_int BY nro_sol_ov vbeln data_reg hora_reg.

*----------------------------------------
* envio para o Trace na ordem das ocorrencias
*----------------------------------------
  LOOP AT t_zsdt0213_int  INTO w_zsdt0213_int.

    CASE w_zsdt0213_int-metodo.
      WHEN 'PUT' OR 'POST'.
        CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE'
          EXPORTING
            i_nro_sol_ov = w_zsdt0213_int-nro_sol_ov
            i_posnr      = w_zsdt0213_int-posnr
            i_acao       = 'C'
          EXCEPTIONS
            OTHERS       = 1.

      WHEN 'DELETE'.
        CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE'
          EXPORTING
            i_nro_sol_ov = w_zsdt0213_int-nro_sol_ov
            i_posnr      = w_zsdt0213_int-posnr
            i_vbeln      = w_zsdt0213_int-vbeln
            i_acao       = 'E'
          EXCEPTIONS
            OTHERS       = 1.
    ENDCASE.

*-IR 189210-14.01.2025-#163685-JT-inicio
    w_zsdt0213_int-tenta_envio  = w_zsdt0213_int-tenta_envio + 1.
    w_zsdt0213_int-data_envio   = sy-datum.
    w_zsdt0213_int-hora_envio   = sy-uzeit.
    w_zsdt0213_int-user_envio   = sy-uname.
    MODIFY zsdt0213_integra  FROM w_zsdt0213_int.
*-IR 189210-14.01.2025-#163685-JT-fim

  ENDLOOP.

ENDFORM.
