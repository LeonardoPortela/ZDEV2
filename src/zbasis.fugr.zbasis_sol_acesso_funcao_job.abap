FUNCTION zbasis_sol_acesso_funcao_job.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SE_RECORDID) TYPE  ZDE_SE_RECORDID_WORKFLOW OPTIONAL
*"     REFERENCE(I_CPF) TYPE  STRING OPTIONAL
*"     REFERENCE(I_UNAME) TYPE  UNAME OPTIONAL
*"     REFERENCE(I_FROM_DAT) TYPE  MENU_DATE
*"     REFERENCE(I_TO_DAT) TYPE  MENU_DATE
*"     REFERENCE(I_FROM_TIME) TYPE  ZDE_TIME_INICIO OPTIONAL
*"     REFERENCE(I_TO_TIME) TYPE  ZDE_TIME_FINAL OPTIONAL
*"     REFERENCE(I_FUNC_LIBERAR) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_SUCESSO) TYPE  CHAR01
*"  RAISING
*"      ZCX_USER_PERFIL
*"      ZCX_JOB
*"      ZCX_SOL_MOBILE_RH
*"----------------------------------------------------------------------

  DATA: i_funcao   TYPE bapiagr,
        cpf_limpo  TYPE string,
        lc_cpf_nr  TYPE char14,
        i_agr_name TYPE agr_name.

  DATA: number_entrada   TYPE tbtcjob-jobcount,
        number_saida     TYPE tbtcjob-jobcount,
        name_entrada     TYPE tbtcjob-jobname,
        name_saida       TYPE tbtcjob-jobname,
        print_parameters TYPE pri_params,
        it_pernr         TYPE TABLE OF zhcms_ret_pernr.

  DATA: wa_funcao TYPE zbasis_user_addf.

  e_sucesso = abap_false.

  IF i_func_liberar IS INITIAL.
    RAISE EXCEPTION TYPE zcx_user_perfil
      EXPORTING
        textid = VALUE #( msgid  = zcx_user_perfil=>zcx_sem_perfil_atribuir-msgid
                          msgno  = zcx_user_perfil=>zcx_sem_perfil_atribuir-msgno )
        msgid  = zcx_user_perfil=>zcx_sem_perfil_atribuir-msgid
        msgno  = zcx_user_perfil=>zcx_sem_perfil_atribuir-msgno
        msgty  = 'E'.
  ENDIF.

*YS:BS-FIREFIGHTER_ABAP
*YS:BS-FIREFIGHTER_BASIS
*YS:BS-FIREFIGHTER_FUNCIONAL
*YS:BS-FIREFIGHTER_GERAL

*  IF I_FUNC_LIBERAR IS INITIAL.
*    I_AGR_NAME = 'YS:BS-FIREFIGHTER_FUNCIONAL'.
*  ELSE.
  i_agr_name = i_func_liberar.
*  ENDIF.

*  CASE SY-SYSID.
*    WHEN 'PRD'.
*      I_AGR_NAME = 'Z_TI_FUNCIONAL_FIREFIGHTER_PRD'.
*    WHEN 'QAS'.
*      I_AGR_NAME = 'Z_TI_FUNCIONAL_FIREFIGHTER'.
*    WHEN 'DEV'.
*      I_AGR_NAME = 'Z_TI_FUNCIONAL_FIREFIGHTER'.
*  ENDCASE.

  IF i_uname IS INITIAL AND i_cpf IS NOT INITIAL.

    cpf_limpo = i_cpf.
    REPLACE ALL OCCURRENCES OF '.' IN cpf_limpo WITH '' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '-' IN cpf_limpo WITH '' IGNORING CASE.

    DATA(qtd_cpf) = strlen( cpf_limpo ).

    IF qtd_cpf NE 11.
      RAISE EXCEPTION TYPE zcx_user_perfil
        EXPORTING
          textid = VALUE #( msgid  = zcx_user_perfil=>zcx_sem_usuario-msgid
                            msgno  = zcx_user_perfil=>zcx_sem_usuario-msgno
                            attr1  = i_cpf )
          msgid  = zcx_user_perfil=>zcx_sem_usuario-msgid
          msgno  = zcx_user_perfil=>zcx_sem_usuario-msgno
          msgv1  = CONV #( i_cpf )
          msgty  = 'E'.
    ENDIF.

    lc_cpf_nr = cpf_limpo.

    SELECT * INTO TABLE @DATA(it_adcp)
      FROM adcp
     WHERE fax_number EQ @lc_cpf_nr.

    IF it_adcp[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_user_perfil
        EXPORTING
          textid = VALUE #( msgid  = zcx_user_perfil=>zcx_sem_usuario-msgid
                            msgno  = zcx_user_perfil=>zcx_sem_usuario-msgno
                            attr1  = CONV #( lc_cpf_nr )  )
          msgid  = zcx_user_perfil=>zcx_sem_usuario-msgid
          msgno  = zcx_user_perfil=>zcx_sem_usuario-msgno
          msgv1  = CONV #( lc_cpf_nr )
          msgty  = 'E'.
    ENDIF.

    SELECT * INTO TABLE @DATA(it_usr21)
      FROM usr21
       FOR ALL ENTRIES IN @it_adcp
     WHERE persnumber EQ @it_adcp-persnumber.

    IF it_usr21[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_user_perfil
        EXPORTING
          textid = VALUE #( msgid  = zcx_user_perfil=>zcx_sem_usuario-msgid
                            msgno  = zcx_user_perfil=>zcx_sem_usuario-msgno
                            attr1  = CONV #( lc_cpf_nr )  )
          msgid  = zcx_user_perfil=>zcx_sem_usuario-msgid
          msgno  = zcx_user_perfil=>zcx_sem_usuario-msgno
          msgv1  = CONV #( lc_cpf_nr )
          msgty  = 'E'.
    ENDIF.

    DESCRIBE TABLE it_usr21 LINES DATA(qt_linhas).

    IF qt_linhas GT 1.
      RAISE EXCEPTION TYPE zcx_user_perfil
        EXPORTING
          textid = VALUE #( msgid  = zcx_user_perfil=>zcx_many_usuario-msgid
                            msgno  = zcx_user_perfil=>zcx_many_usuario-msgno
                            attr1  = CONV #( lc_cpf_nr )  )
          msgid  = zcx_user_perfil=>zcx_many_usuario-msgid
          msgno  = zcx_user_perfil=>zcx_many_usuario-msgno
          msgv1  = CONV #( lc_cpf_nr )
          msgty  = 'E'.
    ENDIF.

    READ TABLE it_usr21 INDEX 1 INTO DATA(wa_usr21).
    DATA(lc_user) = wa_usr21-bname.

  ELSEIF i_uname IS NOT INITIAL .

    lc_user = i_uname.

    SELECT SINGLE * INTO @wa_usr21
      FROM usr21
     WHERE bname EQ @lc_user.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_user_perfil
        EXPORTING
          textid = VALUE #( msgid  = zcx_user_perfil=>zcx_usuario_nao_encontrado-msgid
                            msgno  = zcx_user_perfil=>zcx_usuario_nao_encontrado-msgno
                            attr1  = CONV #( lc_user ) )
          msgid  = zcx_user_perfil=>zcx_usuario_nao_encontrado-msgid
          msgno  = zcx_user_perfil=>zcx_usuario_nao_encontrado-msgno
          msgv1  = CONV #( lc_user )
          msgty  = 'E'.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_adcp)
      FROM adcp
     WHERE persnumber EQ @wa_usr21-persnumber.

    IF sy-subrc IS NOT INITIAL OR wa_adcp-fax_number IS INITIAL.
      RAISE EXCEPTION TYPE zcx_user_perfil
        EXPORTING
          textid = VALUE #( msgid  = zcx_user_perfil=>zcx_usuario_nao_encontrado-msgid
                            msgno  = zcx_user_perfil=>zcx_usuario_nao_encontrado-msgno
                            attr1  = CONV #( lc_user ) )
          msgid  = zcx_user_perfil=>zcx_usuario_nao_encontrado-msgid
          msgno  = zcx_user_perfil=>zcx_usuario_nao_encontrado-msgno
          msgv1  = CONV #( lc_user )
          msgty  = 'E'.
    ENDIF.

    lc_cpf_nr = wa_adcp-fax_number.

  ELSE.
    RAISE EXCEPTION TYPE zcx_user_perfil
      EXPORTING
        textid = VALUE #( msgid  = zcx_user_perfil=>zcx_cpf_ou_uname-msgid
                          msgno  = zcx_user_perfil=>zcx_cpf_ou_uname-msgno )
        msgid  = zcx_user_perfil=>zcx_cpf_ou_uname-msgid
        msgno  = zcx_user_perfil=>zcx_cpf_ou_uname-msgno
        msgty  = 'E'.
  ENDIF.

  CALL FUNCTION 'ZHCMF_RET_PERNR'
    EXPORTING
      cpf_nr  = lc_cpf_nr
    TABLES
      t_saida = it_pernr.

  IF it_pernr[] IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sol_mobile_rh
      EXPORTING
        textid = VALUE #( msgid  = zcx_sol_mobile_rh=>zcx_cpf_nao_encontrado-msgid
                          msgno  = zcx_sol_mobile_rh=>zcx_cpf_nao_encontrado-msgno  )
        msgid  = zcx_sol_mobile_rh=>zcx_cpf_nao_encontrado-msgid
        msgno  = zcx_sol_mobile_rh=>zcx_cpf_nao_encontrado-msgno
        msgty  = 'E'.
  ENDIF.

  READ TABLE it_pernr INTO DATA(wa_pernr) INDEX 1.

  SELECT SINGLE * INTO @DATA(wa_zhcmt0007)
    FROM zhcmt0007
   WHERE pernr EQ @wa_pernr-pernr.

  SELECT COUNT(*)
    FROM tvarvc
    WHERE name EQ 'Z_CENTRO_CUSTO_SAP'
      AND low EQ @wa_zhcmt0007-kostl.

*  IF wa_zhcmt0007-kostl NE '0010010297' AND wa_zhcmt0007-kostl NE '0010010032'.
  IF sy-subrc IS NOT INITIAL.

    RAISE EXCEPTION TYPE zcx_user_perfil
      EXPORTING
        textid = VALUE #( msgid  = zcx_user_perfil=>zcx_ccusto_nao_autorizado-msgid
                          msgno  = zcx_user_perfil=>zcx_ccusto_nao_autorizado-msgno
                          attr1  = CONV #( wa_zhcmt0007-kostl ) )
        msgid  = zcx_user_perfil=>zcx_ccusto_nao_autorizado-msgid
        msgno  = zcx_user_perfil=>zcx_ccusto_nao_autorizado-msgno
        msgv1  = CONV #( wa_zhcmt0007-kostl )
        msgty  = 'E'.
  ENDIF.

  SELECT SINGLE * INTO @DATA(wa_zhcmt0007_resp)
    FROM zhcmt0007
   WHERE pernr EQ @wa_zhcmt0007-sup_pernr.

  IF sy-subrc IS NOT INITIAL.
    RAISE EXCEPTION TYPE zcx_user_perfil
      EXPORTING
        textid = VALUE #( msgid  = zcx_user_perfil=>zcx_sem_mediado-msgid
                          msgno  = zcx_user_perfil=>zcx_sem_mediado-msgno
                          attr1  = CONV #( wa_zhcmt0007-cpf_nr ) )
        msgid  = zcx_user_perfil=>zcx_sem_mediado-msgid
        msgno  = zcx_user_perfil=>zcx_sem_mediado-msgno
        msgv1  = CONV #( wa_zhcmt0007-cpf_nr )
        msgty  = 'E'.
  ENDIF.

  " Buscar Usuário Mediato """""""""""""""""""""""""""""""""""""""""""""""""""""
  IF wa_zhcmt0007_resp-bname IS INITIAL.
    cpf_limpo = wa_zhcmt0007_resp-cpf_nr.
    REPLACE ALL OCCURRENCES OF '.' IN cpf_limpo WITH '' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '-' IN cpf_limpo WITH '' IGNORING CASE.

    qtd_cpf = strlen( cpf_limpo ).

    IF qtd_cpf NE 11.
      RAISE EXCEPTION TYPE zcx_user_perfil
        EXPORTING
          textid = VALUE #( msgid  = zcx_user_perfil=>zcx_sem_usuario-msgid
                            msgno  = zcx_user_perfil=>zcx_sem_usuario-msgno
                            attr1  = i_cpf )
          msgid  = zcx_user_perfil=>zcx_sem_usuario-msgid
          msgno  = zcx_user_perfil=>zcx_sem_usuario-msgno
          msgv1  = CONV #( i_cpf )
          msgty  = 'E'.
    ENDIF.

    lc_cpf_nr = cpf_limpo.
    CLEAR: it_adcp[],  it_adcp,
           it_usr21[], it_usr21.

    SELECT * INTO TABLE @it_adcp
      FROM adcp
     WHERE fax_number EQ @lc_cpf_nr.

    IF it_adcp[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_user_perfil
        EXPORTING
          textid = VALUE #( msgid  = zcx_user_perfil=>zcx_sem_usuario-msgid
                            msgno  = zcx_user_perfil=>zcx_sem_usuario-msgno
                            attr1  = CONV #( lc_cpf_nr )  )
          msgid  = zcx_user_perfil=>zcx_sem_usuario-msgid
          msgno  = zcx_user_perfil=>zcx_sem_usuario-msgno
          msgv1  = CONV #( lc_cpf_nr )
          msgty  = 'E'.
    ENDIF.

    SELECT * INTO TABLE @it_usr21
      FROM usr21
       FOR ALL ENTRIES IN @it_adcp
     WHERE persnumber EQ @it_adcp-persnumber.

    IF it_usr21[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_user_perfil
        EXPORTING
          textid = VALUE #( msgid  = zcx_user_perfil=>zcx_sem_usuario-msgid
                            msgno  = zcx_user_perfil=>zcx_sem_usuario-msgno
                            attr1  = CONV #( lc_cpf_nr )  )
          msgid  = zcx_user_perfil=>zcx_sem_usuario-msgid
          msgno  = zcx_user_perfil=>zcx_sem_usuario-msgno
          msgv1  = CONV #( lc_cpf_nr )
          msgty  = 'E'.
    ENDIF.

    DESCRIBE TABLE it_usr21 LINES qt_linhas.

    IF qt_linhas GT 1.
      RAISE EXCEPTION TYPE zcx_user_perfil
        EXPORTING
          textid = VALUE #( msgid  = zcx_user_perfil=>zcx_many_usuario-msgid
                            msgno  = zcx_user_perfil=>zcx_many_usuario-msgno
                            attr1  = CONV #( lc_cpf_nr )  )
          msgid  = zcx_user_perfil=>zcx_many_usuario-msgid
          msgno  = zcx_user_perfil=>zcx_many_usuario-msgno
          msgv1  = CONV #( lc_cpf_nr )
          msgty  = 'E'.
    ENDIF.

    READ TABLE it_usr21 INDEX 1 INTO wa_usr21.
    wa_zhcmt0007_resp-bname = wa_usr21-bname.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  i_funcao-agr_name = i_agr_name.

  "Data Inicial
  IF i_from_dat LT sy-datum.
    i_funcao-from_dat = sy-datum.
  ELSEIF i_from_dat IS INITIAL.
    i_funcao-from_dat = sy-datum.
  ELSE.
    i_funcao-from_dat = i_from_dat.
  ENDIF.

  "Data Final
  IF i_to_dat LT sy-datum.
    i_funcao-to_dat = sy-datum.
  ELSEIF i_to_dat IS INITIAL.
    i_funcao-to_dat = '99991231'.
  ELSE.
    i_funcao-to_dat = i_to_dat.
  ENDIF.

  "Hora Inicial
  IF i_from_time IS INITIAL.
    wa_funcao-from_time = sy-uzeit.
  ELSE.
    wa_funcao-from_time = i_from_time.
  ENDIF.

  "Hora Final
  IF i_to_time IS INITIAL.
    wa_funcao-to_time  = '235959'.
  ELSE.
    wa_funcao-to_time  = i_to_time.
  ENDIF.

  DATA: e_tstamp_ini LIKE  tzonref-tstamps,
        e_tstamp_fim LIKE  tzonref-tstamps.

  CALL FUNCTION 'OIL_DATE_TO_TIMESTAMP'
    EXPORTING
      i_date               = i_funcao-from_dat
      i_time               = wa_funcao-from_time
    IMPORTING
      e_tstamp             = e_tstamp_ini
    EXCEPTIONS
      invalid_date_or_time = 1
      invalid_time_zone    = 2
      OTHERS               = 3.

  CALL FUNCTION 'OIL_DATE_TO_TIMESTAMP'
    EXPORTING
      i_date               = i_funcao-to_dat
      i_time               = wa_funcao-to_time
    IMPORTING
      e_tstamp             = e_tstamp_fim
    EXCEPTIONS
      invalid_date_or_time = 1
      invalid_time_zone    = 2
      OTHERS               = 3.

  IF e_tstamp_ini GE e_tstamp_fim.
    RAISE EXCEPTION TYPE zcx_user_perfil
      EXPORTING
        textid = VALUE #( msgid  = zcx_user_perfil=>zcx_dh_inicio_fim-msgid
                          msgno  = zcx_user_perfil=>zcx_dh_inicio_fim-msgno )
        msgid  = zcx_user_perfil=>zcx_dh_inicio_fim-msgid
        msgno  = zcx_user_perfil=>zcx_dh_inicio_fim-msgno
        msgty  = 'E'.
  ENDIF.

*  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
*    EXPORTING
*      OBJECT           = 'ZSOLIFUNCA'
*    EXCEPTIONS
*      FOREIGN_LOCK     = 1
*      OBJECT_NOT_FOUND = 2
*      SYSTEM_FAILURE   = 3
*      OTHERS           = 4.
*
*  IF SY-SUBRC IS NOT INITIAL.
*    RAISE EXCEPTION TYPE ZCX_USER_PERFIL
*      EXPORTING
*        TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
*        MSGID  = SY-MSGID
*        MSGNO  = SY-MSGNO
*        MSGTY  = 'E'
*        MSGV1  = SY-MSGV1
*        MSGV2  = SY-MSGV2
*        MSGV3  = SY-MSGV3
*        MSGV4  = SY-MSGV4.
*  ENDIF.
*
*  CALL FUNCTION 'NUMBER_GET_NEXT'
*    EXPORTING
*      NR_RANGE_NR             = '01'
*      OBJECT                  = 'ZSOLIFUNCA'
*      QUANTITY                = '0000000001'
*      IGNORE_BUFFER           = 'X'
*    IMPORTING
*      NUMBER                  = WA_FUNCAO-ID_SOLICITACAO
*    EXCEPTIONS
*      INTERVAL_NOT_FOUND      = 1
*      NUMBER_RANGE_NOT_INTERN = 2
*      OBJECT_NOT_FOUND        = 3
*      QUANTITY_IS_0           = 4
*      QUANTITY_IS_NOT_1       = 5
*      INTERVAL_OVERFLOW       = 6
*      BUFFER_OVERFLOW         = 7
*      OTHERS                  = 8.
*
*  IF SY-SUBRC IS NOT INITIAL.
*    RAISE EXCEPTION TYPE ZCX_USER_PERFIL
*      EXPORTING
*        TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
*        MSGID  = SY-MSGID
*        MSGNO  = SY-MSGNO
*        MSGTY  = 'E'
*        MSGV1  = SY-MSGV1
*        MSGV2  = SY-MSGV2
*        MSGV3  = SY-MSGV3
*        MSGV4  = SY-MSGV4.
*  ENDIF.
*
** Desbloqueia o objeto de numeração
*  CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
*    EXPORTING
*      OBJECT           = 'ZSOLIFUNCA'
*    EXCEPTIONS
*      OBJECT_NOT_FOUND = 1
*      OTHERS           = 2.
*
*  IF SY-SUBRC IS NOT INITIAL.
*    RAISE EXCEPTION TYPE ZCX_USER_PERFIL
*      EXPORTING
*        TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
*        MSGID  = SY-MSGID
*        MSGNO  = SY-MSGNO
*        MSGTY  = 'E'
*        MSGV1  = SY-MSGV1
*        MSGV2  = SY-MSGV2
*        MSGV3  = SY-MSGV3
*        MSGV4  = SY-MSGV4.
*  ENDIF.

  wa_funcao-id_solicitacao = i_se_recordid.
  wa_funcao-uname          = lc_user.
  wa_funcao-agr_name       = i_funcao-agr_name.
  wa_funcao-from_dat       = i_funcao-from_dat.
  wa_funcao-to_dat         = i_funcao-to_dat.
  wa_funcao-from_time      = wa_funcao-from_time.
  wa_funcao-to_time        = wa_funcao-to_time.
  wa_funcao-se_recordid    = i_se_recordid.
  MODIFY zbasis_user_addf FROM wa_funcao.

  DATA: lc_ztopensis_004 TYPE ztopensis_004.
  lc_ztopensis_004-zcod_firefighter  = wa_funcao-id_solicitacao. "ZE_CDGO_FIREFIGHTER CHAR  10  0 Código Firefighter
  lc_ztopensis_004-zcod_user         = wa_funcao-uname.          "SYST_UNAME  CHAR  12  0 Campo do sistema ABAP: nome do usuário atual
  lc_ztopensis_004-zcod_role_user    = i_funcao-agr_name.        "AGR_NAME  CHAR  30  0 Nome da função
  lc_ztopensis_004-zcod_user_respon  = wa_zhcmt0007_resp-bname.  "SYST_UNAME  CHAR  12  0 Campo do sistema ABAP: nome do usuário atual
  lc_ztopensis_004-zdat_vali_inicio  = wa_funcao-from_dat.       "SUID_CHANGE_FROM_DAT  DATS  8 0 Início da data de modificação da validade
  lc_ztopensis_004-zdat_vali_final   = wa_funcao-to_dat.         "SUID_CHANGE_TO_DAT  DATS  8 0 FINAL DA DATA DE MODIFICAÇÃO DA VALIDADE
  lc_ztopensis_004-zcod_user_criacao = sy-uname.                 "SYST_UNAME  CHAR  12  0 CAMPO DO SISTEMA ABAP: NOME DO USUÁRIO ATUAL
  lc_ztopensis_004-zind_atribuir     = abap_false.               "FLAG  CHAR  1 0 FLAG GERAL
  MODIFY ztopensis_004 FROM lc_ztopensis_004.

  DATA(lc_user_job) = zcl_job=>get_user_job( ).

  CONCATENATE 'JOB_LIBERA_FUNCAO' lc_user INTO name_entrada SEPARATED BY '_'.
  CONCATENATE 'JOB_DELETA_FUNCAO' lc_user INTO name_saida SEPARATED BY '_'.

  "Job de Entrada
  IF i_from_dat IS NOT INITIAL.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = name_entrada
        sdlstrtdt        = wa_funcao-from_dat
        sdlstrttm        = i_from_time
      IMPORTING
        jobcount         = number_entrada
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc IS INITIAL.
      DATA(lc_open_entrada) = abap_true.
    ELSE.
      lc_open_entrada = abap_false.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO DATA(mtext_entrada) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      zcl_job=>gera_erro_geral( i_texto = mtext_entrada ).
    ENDIF.

  ENDIF.

  "Job de Saída
  IF i_to_dat IS NOT INITIAL.
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = name_saida
        sdlstrtdt        = wa_funcao-to_dat
        sdlstrttm        = wa_funcao-to_time
      IMPORTING
        jobcount         = number_saida
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc IS INITIAL.
      DATA(lc_open_saida) = abap_true.
    ELSE.
      lc_open_saida = abap_false.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext_entrada WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      PERFORM delete_job USING number_entrada name_entrada.
      zcl_job=>gera_erro_geral( i_texto = mtext_entrada ).
    ENDIF.
  ENDIF.

  IF lc_open_entrada EQ abap_true.

    SUBMIT zbasis_libera_funcao TO SAP-SPOOL SPOOL PARAMETERS print_parameters
    WITHOUT SPOOL DYNPRO VIA JOB name_entrada NUMBER number_entrada
      WITH puname  EQ lc_user
      WITH pfuncao EQ i_funcao-agr_name
      WITH pfromd  EQ i_funcao-from_dat
      WITH ptod    EQ i_funcao-to_dat
      WITH premove EQ abap_false
      WITH pidsol  EQ wa_funcao-id_solicitacao
      USER lc_user_job
       AND RETURN.

    IF sy-subrc IS INITIAL.
      DATA(lc_sumit_entrada) = abap_true.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext_entrada WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      PERFORM delete_job USING number_entrada name_entrada.
      PERFORM delete_job USING number_saida name_saida.
      zcl_job=>gera_erro_geral( i_texto = mtext_entrada ).
    ENDIF.

  ENDIF.

  IF lc_open_saida EQ abap_true.

    SUBMIT zbasis_libera_funcao TO SAP-SPOOL SPOOL PARAMETERS print_parameters
    WITHOUT SPOOL DYNPRO VIA JOB name_saida NUMBER number_saida
      WITH puname  EQ lc_user
      WITH pfuncao EQ i_funcao-agr_name
      WITH pfromd  EQ i_funcao-from_dat
      WITH ptod    EQ i_funcao-to_dat
      WITH premove EQ abap_true
      WITH pidsol  EQ wa_funcao-id_solicitacao
      USER lc_user_job
       AND RETURN.

    IF sy-subrc IS INITIAL.
      DATA(lc_sumit_saida) = abap_true.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext_entrada WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      PERFORM delete_job USING number_entrada name_entrada.
      PERFORM delete_job USING number_saida name_saida.
      zcl_job=>gera_erro_geral( i_texto = mtext_entrada ).
    ENDIF.

  ENDIF.

  IF lc_sumit_entrada EQ abap_true.

    IF i_from_time IS INITIAL.
      DATA(lc_from_time) = sy-uzeit.
    ELSE.
      lc_from_time = i_from_time.
    ENDIF.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = number_entrada
        jobname              = name_entrada
        sdlstrtdt            = wa_funcao-from_dat
        sdlstrttm            = lc_from_time
        strtimmed            = 'X'
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext_entrada WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      PERFORM delete_job USING number_entrada name_entrada.
      PERFORM delete_job USING number_saida name_saida.
      zcl_job=>gera_erro_geral( i_texto = mtext_entrada ).
    ENDIF.
  ENDIF.

  IF lc_sumit_saida EQ abap_true.

    IF i_to_time IS INITIAL.
      DATA(lc_to_time) = sy-uzeit.
      lc_to_time       = '235959'.
    ELSE.
      lc_to_time = i_to_time.
    ENDIF.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = number_saida
        jobname              = name_saida
        sdlstrtdt            = wa_funcao-to_dat
        sdlstrttm            = lc_to_time
        strtimmed            = 'X'
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext_entrada WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      PERFORM delete_job USING number_entrada name_entrada.
      PERFORM delete_job USING number_saida name_saida.
      zcl_job=>gera_erro_geral( i_texto = mtext_entrada ).
    ENDIF.
  ENDIF.

  COMMIT WORK AND WAIT.

  e_sucesso = abap_true.

ENDFUNCTION.
