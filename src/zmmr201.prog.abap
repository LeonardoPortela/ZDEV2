*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: RICARDO PEREIRA                                         &*
*& Data.....: 09/08/2024                                              &*
*& Descrição: RTE - Controle de execução de JOB cadastro de materiais &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zmmr201.

*--------------------------------------------------------------------*
*--------------------------------------------------------------------*

DATA: wl_setleaf TYPE setleaf,
      i_head     TYPE tbtcjob.

DATA:   wl_job_id   LIKE tbtcjob-jobcount.
DATA:   wl_jobn(54).
DATA:   vg_job      TYPE i.
DATA:   v_matnr TYPE mara-matnr.
DATA:   v_werks TYPE marc-werks.

DATA: BEGIN OF i_steplist OCCURS 10.
        INCLUDE STRUCTURE tbtcstep.
DATA: END OF i_steplist.
DATA : c_no(1) TYPE c . "value 'N', " Criação do job

DATA: wl_tbtcjob  TYPE  tbtcjob,
      wl_tbtcstrt TYPE  tbtcstrt.

DATA lv_id_integr TYPE zcoupa_id_integr.
DATA wa_zintegrcoupa01 TYPE zintegrcoupa01.
DATA: lv_repname LIKE  rsvar-report.           " for variant handling
DATA: iv_varname LIKE  raldb-variant VALUE 'SAP_UPGRADE'.
*DATA: iv_varname LIKE  raldb-variant VALUE 'INTEGRA'.
DATA: iv_varianttext  LIKE  varit-vtext VALUE 'Upgrade variant'.
DATA: wl_subrc TYPE sy-subrc.
DATA: tt_reportparam TYPE TABLE OF  rsparams WITH HEADER LINE.
*
DATA: zmmt0184 TYPE zmmt0184.
DATA: tzmmt0184 TYPE TABLE OF zmmt0184.
DATA: l_zmmt0184 TYPE zmmt0184.
DATA: t_zmmt0184 TYPE TABLE OF zmmt0184.
DATA: l_tvarvc TYPE tvarvc.
DATA: lv_count TYPE i.
*** Stefanini - IR230283 - 08/01/2025 - LAZAROSR - Início de Alteração
DATA: lv_sucesso         TYPE zintegrcoupa01-status VALUE 'S'.
DATA: lv_filtro_pesquisa TYPE c VALUE '%'.
DATA: lv_matnr_pesquisa  TYPE string.
DATA: lv_inicio          TYPE i.
*** Stefanini - IR230283 - 08/01/2025 - LAZAROSR - Fim de Alteração

" Ler a STVARV
SELECT SINGLE * FROM tvarvc INTO l_tvarvc
  WHERE name = 'ZMM_JOBMAT'.

**********************************************************************
* INÍCIO CÓDIGO ORIGINAL
**********************************************************************
SELECT * FROM zmmt0184 INTO TABLE tzmmt0184
  WHERE fg_processado = 'N'.

CHECK tzmmt0184[] IS NOT INITIAL.


SELECT SINGLE *
 FROM setleaf
 INTO wl_setleaf
  WHERE setname EQ 'MAGGI_JOB_USER'.

IF sy-subrc NE 0.
  MESSAGE TEXT-e01 TYPE 'E'.
*    EXIT.
ENDIF.

CLEAR lv_count.
LOOP AT tzmmt0184 INTO l_zmmt0184.
  IF lv_count GE l_tvarvc-low.
    EXIT.
  ENDIF.

  lv_count = lv_count + 1.
  "
  v_werks = l_zmmt0184-werks.
  IF l_zmmt0184-werks IS  INITIAL.

*** Stefanini - IR230283 - 08/01/2025 - LAZAROSR - Início de Alteração
*    select single werks
*      into v_werks
*      from marc
*      where matnr = l_zmmt0184-matnr.

    CONCATENATE lv_filtro_pesquisa l_zmmt0184-matnr lv_filtro_pesquisa
                                                INTO lv_matnr_pesquisa.

    TRY.

        SELECT SINGLE id_integr
          FROM zintegrcoupa01
          INTO @DATA(lv_id_integracao)
          WHERE id_integr LIKE @lv_matnr_pesquisa
            AND status    EQ @lv_sucesso.

        IF sy-subrc IS INITIAL.

          " Pegar os últimos 4 caracteres do ID, pois ele é o centro
          lv_inicio = strlen( lv_id_integracao ) - 4.
          v_werks = lv_id_integracao+lv_inicio(4).

        ENDIF.

      CATCH cx_root.
        " Erro ao realizar a busca do centro...
    ENDTRY.

    IF v_werks IS INITIAL.

      SELECT SINGLE werks
       INTO v_werks
       FROM marc
      WHERE matnr = l_zmmt0184-matnr.

    ENDIF.
*** Stefanini - IR230283 - 08/01/2025 - LAZAROSR - Fim de Alteração
  ENDIF.
  l_zmmt0184-data_processo = sy-datum.
  l_zmmt0184-hora_processo = sy-uzeit.
  l_zmmt0184-fg_processado = 'S'.

  APPEND l_zmmt0184 TO t_zmmt0184.

  REFRESH: tt_reportparam,i_steplist.
  CLEAR: tt_reportparam, i_head.
  CONCATENATE 'COUPAMAT'  l_zmmt0184-matnr  v_werks INTO wl_jobn SEPARATED BY '|'.
  i_head-jobname = wl_jobn. " Nome do JOBi_head-sdlstrtdt = sy-datum. " Dia
  i_head-sdlstrttm = sy-uzeit + 20. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
  i_head-sdlstrtdt = sy-datum.
  i_head-stepcount = 1.


  lv_repname = 'ZMMR174'.
*    Write the variant first (Insert or Update)
  CALL FUNCTION 'SUBST_WRITE_UPGRADE_VARIANT'
    EXPORTING
      iv_reportname         = lv_repname
      iv_variantname        = iv_varname
      iv_varianttext        = iv_varianttext
    IMPORTING
      ev_funcrc             = wl_subrc
    TABLES
      tt_reportparam        = tt_reportparam
    EXCEPTIONS
      exist_check_failed    = 1
      update_failed         = 2
      update_not_authorized = 3
      update_no_report      = 4
      update_no_variant     = 5
      update_variant_locked = 6
      insert_failed         = 7
      insert_not_authorized = 8
      insert_no_report      = 9
      insert_variant_exists = 10
      insert_variant_locked = 11
      OTHERS                = 12.

  i_steplist-parameter = iv_varname. " Nome da variante
  i_steplist-program = 'ZMMR174'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
  i_steplist-typ = 'A'. " Tipo de Job
  i_steplist-authcknam = wl_setleaf-valfrom.
  i_steplist-language = sy-langu.
  i_steplist-arcuser = wl_setleaf-valfrom.

  APPEND i_steplist.


  c_no = 'N'.
  CALL FUNCTION 'BP_JOB_CREATE'
    EXPORTING
      job_cr_dialog       = c_no " Coloque 'Y' se quiser ver
      job_cr_head_inp     = i_head " os valores atribuidos
    IMPORTING
      job_cr_head_out     = wl_tbtcjob
      job_cr_stdt_out     = wl_tbtcstrt
    TABLES
      job_cr_steplist     = i_steplist
    EXCEPTIONS
      cant_create_job     = 1
      invalid_dialog_type = 2
      invalid_job_data    = 3
      job_create_canceled = 4
      OTHERS              = 5.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobname   = wl_jobn(32)
      jobcount  = wl_tbtcjob-jobcount
      strtimmed = 'X'.
ENDLOOP.

**********************************************************************
* FIM CÓDIGO ORIGINAL
**********************************************************************

MODIFY zmmt0184 FROM TABLE t_zmmt0184.
COMMIT WORK.
