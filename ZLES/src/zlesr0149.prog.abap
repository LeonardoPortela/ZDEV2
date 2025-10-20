*----------------------------------------------------------------------*
* ID........:                                                          *
* Programa..: ZLESR0149                                                *
* Tipo......: R - Report                                               *
* Transação.:                                                          *
* Descrição.: JOB para atualizar descargas de integrações              *
* Autor.....: JBARBOSA                                                 *
* Data......: 18.09.2020                                               *
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data       | Change     | Autor        | Alteração                   *
*----------------------------------------------------------------------*
* 09.09.20   |            |JBARBOSA      | Codificação Inicial         *
*----------------------------------------------------------------------*
REPORT zlesr0149.

START-OF-SELECTION.

  DATA: vg_job_day    TYPE i,
        vg_job_minute TYPE i.
  DATA: lva_data_cons  TYPE sy-datum.
  DATA: lt_values  LIKE rgsb4 OCCURS 0 WITH HEADER LINE.


  IF sy-batch EQ abap_true. "Se JOB ATUALIZA_COMP_J1 ou ATUALIZA_COMP_J2 estiver em execução, aguardar finalização dos mesmos....

    DATA(_lva_continua) = abap_true.

    WHILE _lva_continua EQ abap_true.

      TRY.
          zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = 'ZLESJ0001' IMPORTING e_qtd  = DATA(e_qtd) ).
        CATCH zcx_job.
      ENDTRY.

      IF e_qtd GT 1.
        CONTINUE.
      ENDIF.

      TRY.
          zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = 'ZLESJ0002' IMPORTING e_qtd  = e_qtd ).
        CATCH zcx_job.
      ENDTRY.

      IF e_qtd GT 1.
        CONTINUE.
      ENDIF.

      TRY.
          zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = 'ZLESJ0003' IMPORTING e_qtd  = e_qtd ).
        CATCH zcx_job.
      ENDTRY.

      IF e_qtd GT 1.
        CONTINUE.
      ENDIF.

      _lva_continua  = abap_false.

    ENDWHILE.

  ENDIF.


*  SELECT SINGLE COUNT( * ) INTO vg_job
*    FROM tbtco
*   WHERE jobname EQ 'MAGGI_ZLES0170'
*     AND status EQ 'R'.
*
*  IF vg_job   EQ 1.
*
*    lva_data_cons = sy-datum - 1.
*
**---------------------------------------------------------------------
**L1 – Descarga Rodoviária
**---------------------------------------------------------------------
** Consulta
*    SUBMIT zlesr0128 WITH p_dtche = lva_data_cons   WITH p_tpproc = '1' AND RETURN.
*
** Processa
*    SUBMIT zlesr0128 WITH p_tpproc = '2' AND RETURN.
*
**---------------------------------------------------------------------
**L2- Carregamento Ferroviário
**---------------------------------------------------------------------
** Consulta
*    SUBMIT zlesr0130 WITH p_dtsai   = lva_data_cons   WITH p_tpproc = '1'   AND RETURN.
*
** Processa
*    SUBMIT zlesr0130 WITH p_tpproc = '2'   AND RETURN.
*
**---------------------------------------------------------------------
**L3- Descarga Ferroviária
**---------------------------------------------------------------------
**Consulta
*    SUBMIT zlesr0131 WITH p_dtche   = lva_data_cons   WITH p_tpproc = '1' AND RETURN.
*
**Processa
*    SUBMIT zlesr0131 WITH p_tpproc = '2' AND RETURN.
*
*  ENDIF.

*EXECUTA O JOB MAGGI_ZLES0170_DAY
  SELECT SINGLE COUNT( * ) INTO vg_job_day
    FROM tbtco
   WHERE jobname EQ 'MAGGI_ZLES0170_DAY'
     AND status EQ 'R'.

*EXECUTA O JOB MAGGI_ZLES0170_MINUTE
  SELECT SINGLE COUNT( * ) INTO vg_job_minute
     FROM tbtco
    WHERE jobname EQ 'MAGGI_ZLES0170_MINUTE'
      AND status EQ 'R'.

  IF vg_job_day  EQ 1.

    lva_data_cons = sy-datum - 1.

*---------------------------------------------------------------------
*L1 – Descarga Rodoviária
*---------------------------------------------------------------------
* Consulta
    SUBMIT zlesr0128 WITH p_dtche = lva_data_cons   WITH p_tpproc = '1' AND RETURN.

* Processa
    SUBMIT zlesr0128 WITH p_tpproc = '2' AND RETURN.

  ELSEIF vg_job_minute   EQ 1.

    lva_data_cons = sy-datum.

*---------------------------------------------------------------------
*L1 – Descarga Rodoviária
*---------------------------------------------------------------------
* Consulta
    SUBMIT zlesr0128 WITH p_dtche = lva_data_cons   WITH p_tpproc = '1' AND RETURN.

* Processa
    SUBMIT zlesr0128 WITH p_tpproc = '2' AND RETURN.

  ENDIF.

  IF vg_job_day EQ 1.

*---------------------------------------------------------------------
*L2- Carregamento Ferroviário
*---------------------------------------------------------------------
* Consulta
    SUBMIT zlesr0130 WITH p_dtsai   = lva_data_cons   WITH p_tpproc = '1'   AND RETURN.

* Processa
    SUBMIT zlesr0130 WITH p_tpproc = '2'   AND RETURN.

*---------------------------------------------------------------------
*L3- Descarga Ferroviária
*---------------------------------------------------------------------
*Consulta
    SUBMIT zlesr0131 WITH p_dtche   = lva_data_cons   WITH p_tpproc = '1' AND RETURN.

*Processa
    SUBMIT zlesr0131 WITH p_tpproc = '2' AND RETURN.

  ENDIF.

**EXECUTA O JOB MAGGI_ZLES0170_MINUTE
* SELECT SINGLE COUNT( * ) INTO vg_job
*    FROM tbtco
*   WHERE jobname EQ 'MAGGI_ZLES0170_MINUTE'
*     AND status EQ 'R'.
*
*  IF vg_job   EQ 1.
*
*    lva_data_cons = sy-datum - 1.
*
**---------------------------------------------------------------------
**L1 – Descarga Rodoviária
**---------------------------------------------------------------------
** Consulta
*    SUBMIT zlesr0128 WITH p_dtche = lva_data_cons   WITH p_tpproc = '1' AND RETURN.
*
** Processa
*    SUBMIT zlesr0128 WITH p_tpproc = '2' AND RETURN.
*
**---------------------------------------------------------------------
**L2- Carregamento Ferroviário
**---------------------------------------------------------------------
** Consulta
*    SUBMIT zlesr0130 WITH p_dtsai   = lva_data_cons   WITH p_tpproc = '1'   AND RETURN.
*
** Processa
*    SUBMIT zlesr0130 WITH p_tpproc = '2'   AND RETURN.
*
**---------------------------------------------------------------------
**L3- Descarga Ferroviária
**---------------------------------------------------------------------
**Consulta
*    SUBMIT zlesr0131 WITH p_dtche   = lva_data_cons   WITH p_tpproc = '1' AND RETURN.
*
**Processa
*    SUBMIT zlesr0131 WITH p_tpproc = '2' AND RETURN.
*
*  ENDIF.
