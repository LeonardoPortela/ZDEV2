FUNCTION zlog_sm20n_001.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ERDAT) TYPE  ERDAT OPTIONAL
*"     VALUE(I_USNAM) TYPE  USNAM OPTIONAL
*"----------------------------------------------------------------------

  DATA: lt_seltab TYPE TABLE OF rsparams.

  CONSTANTS: lva_destination_qas TYPE char40 VALUE 'QAS'.

  DATA: lv_tempo_inatividade TYPE int4,
        lv_total_inatividade TYPE int4.

  DATA: lva_user TYPE usnam.

  DATA: lit_dados_sm20n_dev_qas TYPE rsau_t_result,
        lit_dados_sm20n         TYPE rsau_t_result.

  DATA: lva_dt_inicio_inatividade TYPE erdat,
        lva_hr_inicio_inatividade TYPE erzet,
        lva_dt_fim_initatividade  TYPE erdat,
        lva_hr_fim_initatividade  TYPE erzet.

  RANGES: lra_param1_del FOR zsdt0001-ds_obs,
          lra_param3_del FOR zsdt0001-ds_obs.


  "Horario de Monitoramento - ok
  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_tvarvc_par_001)
   WHERE name = 'ZLOG_SM20N_PAR_001'.

  CHECK sy-subrc EQ 0 AND lwa_tvarvc_par_001-low IS NOT INITIAL AND lwa_tvarvc_par_001-high IS NOT INITIAL.

  "Tolerancia de Tempo de Inatividade Cumulativo - OK
  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_tvarvc_par_002)
   WHERE name = 'ZLOG_SM20N_PAR_002'.

  CHECK sy-subrc EQ 0 AND lwa_tvarvc_par_002-low IS NOT INITIAL.

  "Tolerancia de Tempo de Inatividade para Alerta - OK
  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_tvarvc_par_003)
   WHERE name = 'ZLOG_SM20N_PAR_003'.

  CHECK sy-subrc EQ 0 AND lwa_tvarvc_par_003-low IS NOT INITIAL.

  "Tolerancia de Tempo Total de Inatividade - OK
  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_tvarvc_par_004)
   WHERE name = 'ZLOG_SM20N_PAR_004'.

  CHECK sy-subrc EQ 0 AND lwa_tvarvc_par_004-low IS NOT INITIAL.

  "Usuarios Monitoramento - ok
  SELECT *
      FROM tvarvc INTO TABLE @DATA(lit_tvarvc_par_006)
     WHERE name = 'ZLOG_SM20N_PAR_006'.

  DELETE lit_tvarvc_par_006 WHERE low = 'SAP'.

  IF i_usnam IS NOT INITIAL.
    CLEAR: lit_tvarvc_par_006[].

    APPEND INITIAL LINE TO lit_tvarvc_par_006 ASSIGNING FIELD-SYMBOL(<fs_tvarvc_par_006>).

    <fs_tvarvc_par_006>-low = i_usnam.
  ENDIF.

  CHECK lit_tvarvc_par_006[] IS NOT INITIAL.

  SELECT *
    FROM tvarvc INTO TABLE @DATA(lit_tvarvc_param1_exc)
   WHERE name = 'ZLOG_SM20N_PARAM1_DEL'.

  SELECT *
    FROM tvarvc INTO TABLE @DATA(lit_tvarvc_param3_exc)
   WHERE name = 'ZLOG_SM20N_PARAM3_DEL'.

  LOOP AT lit_tvarvc_param1_exc ASSIGNING FIELD-SYMBOL(<fs_tvarvc_param1_del>).
    APPEND INITIAL LINE TO lra_param1_del ASSIGNING FIELD-SYMBOL(<fs_param1_del>).
    <fs_param1_del>-sign   = 'I'.
    <fs_param1_del>-option = 'EQ'.
    <fs_param1_del>-low    = <fs_tvarvc_param1_del>-low.
  ENDLOOP.

  LOOP AT lit_tvarvc_param3_exc ASSIGNING FIELD-SYMBOL(<fs_tvarvc_param3_del>).
    APPEND INITIAL LINE TO lra_param3_del ASSIGNING FIELD-SYMBOL(<fs_param3_del>).
    <fs_param3_del>-sign   = 'I'.
    <fs_param3_del>-option = 'EQ'.
    <fs_param3_del>-low    = <fs_tvarvc_param3_del>-low.
  ENDLOOP.

  DATA(_envio_email) = abap_false.

  LOOP AT lit_tvarvc_par_006 INTO DATA(lwa_lit_tvarvc_par_006).

    CLEAR: git_periodo_inatividade[], lit_dados_sm20n_dev_qas[], lv_total_inatividade.
    CLEAR: git_periodo_atividade[].

    lva_user = lwa_lit_tvarvc_par_006-low.

    DATA(_emite_alerta) = abap_false.

*-----------------------------------------------------------------------------------*
*   Recuperar Logs Local
*-----------------------------------------------------------------------------------*
    CALL FUNCTION 'ZLOG_SM20N_002'
      EXPORTING
        i_usnam = lva_user
        i_erdat = i_erdat
      IMPORTING
        r_logs  = lit_dados_sm20n.

    APPEND LINES OF lit_dados_sm20n TO lit_dados_sm20n_dev_qas[].

*-----------------------------------------------------------------------------------*
*   Recuperar Logs QAS
*-----------------------------------------------------------------------------------*

    CALL FUNCTION 'ZLOG_SM20N_002' DESTINATION lva_destination_qas
      EXPORTING
        i_usnam = lva_user
        i_erdat = i_erdat
      IMPORTING
        r_logs  = lit_dados_sm20n.

    APPEND LINES OF lit_dados_sm20n TO lit_dados_sm20n_dev_qas[].

*-----------------------------------------------------------------------------------*
*   Tratamento Dados
*-----------------------------------------------------------------------------------*
    DATA(_use_eclipse_adt) = abap_false.
    READ TABLE lit_dados_sm20n_dev_qas WITH KEY param1 = 'SADT_REST' TRANSPORTING NO FIELDS.
    IF sy-subrc eq 0.
      _use_eclipse_adt = abap_true.
    ENDIF.

    DELETE lit_dados_sm20n_dev_qas WHERE txsubclsid EQ 'Logon diálogo'.
    DELETE lit_dados_sm20n_dev_qas WHERE param1 IN lra_param1_del.
    DELETE lit_dados_sm20n_dev_qas WHERE param3 IN lra_param3_del.
    DELETE lit_dados_sm20n_dev_qas WHERE sal_time IS INITIAL.
    DELETE lit_dados_sm20n_dev_qas WHERE sal_date NE i_erdat.

    PERFORM f_registrar_tempo_atividade TABLES lit_dados_sm20n_dev_qas USING lwa_tvarvc_par_002 _use_eclipse_adt. "Registrar Tempo Atividade

    DELETE lit_dados_sm20n_dev_qas WHERE sal_time <  lwa_tvarvc_par_001-low OR sal_time > lwa_tvarvc_par_001-high.

    IF lit_dados_sm20n_dev_qas[] IS INITIAL.

      _emite_alerta = abap_true.

      APPEND INITIAL LINE TO git_periodo_inatividade ASSIGNING FIELD-SYMBOL(<fs_periodo_inatividade>).

      <fs_periodo_inatividade>-usnam                = lva_user.
      <fs_periodo_inatividade>-erdat                = i_erdat.
      <fs_periodo_inatividade>-erzet_ini            = lwa_tvarvc_par_001-low.
      <fs_periodo_inatividade>-erzet_fim            = lwa_tvarvc_par_001-high.
      <fs_periodo_inatividade>-inatividade_segundos = 28800.
      <fs_periodo_inatividade>-inatividade_minutos  = <fs_periodo_inatividade>-inatividade_segundos / 60.
      <fs_periodo_inatividade>-inatividade_horas    = <fs_periodo_inatividade>-inatividade_minutos  / 60.

    ELSE.

      "Criar Registros de Inicio e Fim do Monitoramento - Inicio
      APPEND INITIAL LINE TO lit_dados_sm20n_dev_qas ASSIGNING FIELD-SYMBOL(<fs_dados_sm20n_dev_qas_ini>).
      APPEND INITIAL LINE TO lit_dados_sm20n_dev_qas ASSIGNING FIELD-SYMBOL(<fs_dados_sm20n_dev_qas_fim>).

      <fs_dados_sm20n_dev_qas_ini>-slguser  = lva_user.
      <fs_dados_sm20n_dev_qas_ini>-sal_date = i_erdat.
      <fs_dados_sm20n_dev_qas_ini>-sal_time = lwa_tvarvc_par_001-low. "Inicio Monitoramento

      <fs_dados_sm20n_dev_qas_fim>-slguser  = lva_user.
      <fs_dados_sm20n_dev_qas_fim>-sal_date = i_erdat.
      <fs_dados_sm20n_dev_qas_fim>-sal_time = lwa_tvarvc_par_001-high. "Fim Monitoramento
      "Criar Registros de Inicio e Fim do Monitoramento - Fim

      SORT lit_dados_sm20n_dev_qas BY sal_date sal_time.

      LOOP AT lit_dados_sm20n_dev_qas INTO DATA(lwa_dados_sm20n).

        DATA(_tabix) = sy-tabix.

        IF _tabix EQ 1.
          lva_dt_inicio_inatividade = lwa_dados_sm20n-sal_date.
          lva_hr_inicio_inatividade = lwa_dados_sm20n-sal_time.
          CONTINUE.
        ENDIF.

        lva_dt_fim_initatividade = lwa_dados_sm20n-sal_date.
        lva_hr_fim_initatividade = lwa_dados_sm20n-sal_time.

        lv_tempo_inatividade = lva_hr_fim_initatividade - lva_hr_inicio_inatividade.

        IF ( lv_tempo_inatividade > lwa_tvarvc_par_003-low )  OR  "Tolerancia de Tempo de Inatividade para Alerta
           ( lv_tempo_inatividade > lwa_tvarvc_par_002-low ).     "Tolerancia de Tempo de Inatividade Cumulativo

          IF lv_tempo_inatividade > lwa_tvarvc_par_003-low. "Tolerancia de Tempo de Inatividade para Alerta
            _emite_alerta = abap_true.
          ENDIF.

          ADD lv_tempo_inatividade TO lv_total_inatividade.

          APPEND INITIAL LINE TO git_periodo_inatividade ASSIGNING <fs_periodo_inatividade>.

          <fs_periodo_inatividade>-usnam                = lwa_dados_sm20n-slguser.
          <fs_periodo_inatividade>-erdat                = lva_dt_inicio_inatividade.
          <fs_periodo_inatividade>-erzet_ini            = lva_hr_inicio_inatividade.
          <fs_periodo_inatividade>-erzet_fim            = lva_hr_fim_initatividade.
          <fs_periodo_inatividade>-inatividade_segundos = lv_tempo_inatividade.
          <fs_periodo_inatividade>-inatividade_minutos  = <fs_periodo_inatividade>-inatividade_segundos / 60.
          <fs_periodo_inatividade>-inatividade_horas    = <fs_periodo_inatividade>-inatividade_minutos  / 60.

          lva_dt_inicio_inatividade = lwa_dados_sm20n-sal_date.
          lva_hr_inicio_inatividade = lwa_dados_sm20n-sal_time.

          CONTINUE.
        ENDIF.

        lva_dt_inicio_inatividade = lwa_dados_sm20n-sal_date.
        lva_hr_inicio_inatividade = lwa_dados_sm20n-sal_time.

      ENDLOOP.

      IF lv_total_inatividade > lwa_tvarvc_par_004-low . "Tolerancia de Tempo Total de Inatividade
        _emite_alerta = abap_true.
      ENDIF.

    ENDIF.

    CHECK git_periodo_inatividade[] IS NOT INITIAL AND _emite_alerta EQ abap_true.

    LOOP AT git_periodo_inatividade ASSIGNING <fs_periodo_inatividade>.
      <fs_periodo_inatividade>-eclipse = _use_eclipse_adt.
    ENDLOOP.

    MODIFY zlog1001 FROM TABLE git_periodo_inatividade.

    COMMIT WORK.

    _envio_email  = abap_true.

  ENDLOOP.

  CHECK _envio_email EQ abap_true.

*  CALL FUNCTION 'ZLOG_SM20N_003' DESTINATION lva_destination_qas
*    EXPORTING
*      i_erdat = i_erdat.

ENDFUNCTION.
