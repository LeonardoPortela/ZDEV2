*&---------------------------------------------------------------------*
*& Report  ZHCMR_PA0012
*&
*&---------------------------------------------------------------------*
*&  TABELA DE DADOS DE FUNCIONÁRIO
*&  P/ SOFTEXPERT
*&  P/ Desativar Usuários do SAP
*&  P/ Desativar Usuários do Active Directory
*&  P/ Desativar Usuários do SIGAM
*&  P/ Desativar Usuários do COMEX
*&  P/ Desativar Usuários do OPUS
*&  P/ Desativar Usuários da Catraca TRIELO
*&---------------------------------------------------------------------*

REPORT zhcmr_pa0012.

PARAMETERS: ppernr TYPE persno.

TYPES: BEGIN OF ty_pernr,
         pernr TYPE zhcmt0007-pernr.
TYPES END OF ty_pernr.


TYPES BEGIN OF ty_zhcmt0007.
INCLUDE TYPE zhcmt0007.
TYPES:   cpf_fax TYPE ad_fxnmbr1.
TYPES END OF ty_zhcmt0007.

TYPES: ty_rg_pernr TYPE RANGE OF pa0001-pernr.
TYPES: ty_rg_bukrs TYPE RANGE OF pa0001-bukrs.
TYPES: ty_rg_werks TYPE RANGE OF pa0001-werks.

DATA: lc_endda             LIKE p0001-endda,
      it_zhcms_func_list   TYPE TABLE OF zhcms_func_list_pa WITH HEADER LINE,
      it_zhcms_func_list_s TYPE SORTED TABLE OF zhcms_func_list_pa WITH NON-UNIQUE KEY pernr WITH HEADER LINE,
      it_func_list         TYPE TABLE OF zhcms_func_list_pa WITH HEADER LINE,

      it_zhcmt0007         TYPE TABLE OF zhcmt0007 WITH HEADER LINE,
      lit_zhcmt0007_upd    TYPE TABLE OF zhcmt0007,
      it_adcp              TYPE TABLE OF adcp WITH HEADER LINE,
      it_usr21             TYPE TABLE OF usr21 WITH HEADER LINE,
      it_return            TYPE TABLE OF bapiret2 WITH HEADER LINE,
      it_j_1bbranch        TYPE TABLE OF j_1bbranch WITH HEADER LINE,
      it_t001              TYPE TABLE OF t001 WITH HEADER LINE,
      it_ret_diretor       TYPE TABLE OF zhcms_ret_diretor,
      lit_pernr_aux        TYPE TABLE OF ty_pernr,
      v_kostl01            TYPE pa0001-kostl,
      v_kostl02            TYPE pa0001-kostl.

DATA:wa_acao_ad        TYPE zhcmt0007_ad,
     it_activitygroups TYPE TABLE OF  bapiagr,
     wa_activitygroups TYPE bapiagr,
     it_return_g       TYPE TABLE OF bapiret2,
     wa_return_g       TYPE bapiret2.

DATA: erdata TYPE agr_users-from_dat.



"Para Execução em backgound (jobs) """"""""""""""""""""""""""""
IF sy-batch EQ abap_true.
  TRY .
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
    CATCH zcx_job.
      e_qtd = 1.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.
ENDIF.

lc_endda = sy-datum.

CLEAR: it_zhcms_func_list[], it_zhcmt0007[],lit_zhcmt0007_upd[].

IF ppernr IS INITIAL.

  CALL FUNCTION 'ZHCMF_DADOS_FUNCIONAIS_PA'
    EXPORTING
      endda   = lc_endda
    TABLES
      t_saida = it_zhcms_func_list.
  SORT it_zhcms_func_list[] BY pernr ASCENDING.

  it_zhcms_func_list_s[]  = it_zhcms_func_list[].

  lc_endda = lc_endda + 7.

  CALL FUNCTION 'ZHCMF_DADOS_FUNCIONAIS_PA'
    EXPORTING
      endda   = lc_endda
    TABLES
      t_saida = it_func_list.
  SORT it_func_list[] BY pernr ASCENDING.

  LOOP AT it_func_list INTO DATA(wa_func_list).

    READ TABLE it_zhcms_func_list_s INTO DATA(wa_zhcms_func)
        WITH  KEY pernr = wa_func_list-pernr BINARY SEARCH.
    IF sy-subrc <> 0.
      APPEND wa_func_list TO it_zhcms_func_list.
    ENDIF.

    CLEAR: wa_func_list, wa_zhcms_func.
  ENDLOOP.

ELSE.

  CALL FUNCTION 'ZHCMF_DADOS_FUNCIONAIS_PA'
    EXPORTING
      pernr   = ppernr
      endda   = lc_endda
    TABLES
      t_saida = it_zhcms_func_list.
  SORT it_zhcms_func_list[] BY pernr ASCENDING.

  lc_endda = lc_endda + 7.

  CALL FUNCTION 'ZHCMF_DADOS_FUNCIONAIS_PA'
    EXPORTING
      pernr   = ppernr
      endda   = lc_endda
    TABLES
      t_saida = it_func_list.
  SORT it_func_list[] BY pernr ASCENDING.


  LOOP AT it_func_list INTO wa_func_list.

    READ TABLE it_zhcms_func_list INTO wa_zhcms_func
        WITH  KEY pernr = wa_func_list-pernr BINARY SEARCH.
    IF sy-subrc <> 0.
      APPEND wa_func_list TO it_zhcms_func_list.
    ENDIF.

    CLEAR: wa_func_list, wa_zhcms_func.
  ENDLOOP.

ENDIF.

IF it_zhcms_func_list[] IS NOT INITIAL.

  DATA(lrg_bukrs) = VALUE ty_rg_bukrs( FOR lwa_func IN it_zhcms_func_list[] (
      sign   = 'I'
      option = 'EQ'
      low    = lwa_func-bukrs ) ).
  SORT lrg_bukrs[] BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lrg_bukrs[] COMPARING low.

  DATA(lrg_werks) = VALUE ty_rg_werks( FOR lwa_func IN it_zhcms_func_list[] (
      sign   = 'I'
      option = 'EQ'
      low    = lwa_func-werks ) ).
  SORT lrg_werks[] BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lrg_werks[] COMPARING low.

  SELECT bukrs, branch, name
    INTO TABLE @DATA(lit_j_1bbranch) "it_j_1bbranch
    FROM j_1bbranch
     "FOR ALL ENTRIES IN it_zhcms_func_list
   WHERE bukrs  IN @lrg_bukrs[]  "EQ it_zhcms_func_list-bukrs
     AND branch IN @lrg_werks[]. " EQ it_zhcms_func_list-werks.
  SORT lit_j_1bbranch BY bukrs branch ASCENDING.

  SELECT * INTO TABLE it_t001
    FROM t001
     "FOR ALL ENTRIES IN it_zhcms_func_list
   WHERE bukrs  IN lrg_bukrs[]. "EQ it_zhcms_func_list-bukrs.

  SORT it_t001 BY bukrs.

  " Selecionar Eventos Bloqueantes do RH """"""""""""""""""""
  SELECT * INTO TABLE @DATA(it_t001p)
    FROM t001p
     "FOR ALL ENTRIES IN @it_zhcms_func_list
   WHERE werks IN @lrg_werks. "EQ @it_zhcms_func_list-werks.

  SORT it_t001p BY werks btrtl.

  SELECT * INTO TABLE @DATA(it_pa_0015)
    FROM zhcmt_pa_0015.

  SORT it_pa_0015 BY moabw awart.

  SELECT * INTO TABLE @DATA(it_zhcmt0007_base)
    FROM zhcmt0007.

  SELECT * FROM pa0001 INTO TABLE @DATA(it_pa0001)
    WHERE pernr EQ @ppernr.

  SORT it_pa0001 BY endda DESCENDING.

  DATA(rg_pernr) = VALUE ty_rg_pernr( FOR lw_func IN it_zhcms_func_list[] (
      sign   = 'I'
      option = 'EQ'
      low    = lw_func-pernr
      high   = lw_func-pernr
  ) ).
  SORT rg_pernr[] BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM rg_pernr[] COMPARING low.


  CLEAR: lit_pernr_aux[].
  LOOP AT it_zhcms_func_list ASSIGNING FIELD-SYMBOL(<fs_func_list>).
    APPEND INITIAL LINE TO lit_pernr_aux ASSIGNING FIELD-SYMBOL(<fs_penr_aux>).
    <fs_penr_aux>-pernr = <fs_func_list>-pernr.
  ENDLOOP.

  "Buscar nome da mãe
  IF ( lit_pernr_aux[] IS NOT INITIAL ).
    SELECT pernr, famsa, fcnam
      FROM pa0021
      INTO TABLE @DATA(it_0021)
      FOR ALL ENTRIES IN @lit_pernr_aux
      WHERE pernr = @lit_pernr_aux-pernr
        AND subty = '12'
        AND objps = ''
        AND sprps = ''
        AND endda >= @sy-datum
        AND begda <= @sy-datum
        AND seqnr <> '999'.
    "AND famsa = '12'.
    SORT it_0021[] BY pernr ASCENDING.
    DELETE it_0021[] WHERE famsa <> '12'.
  ENDIF.


*  DO 2 TIMES.
*    CASE SY-INDEX.
*      WHEN '1'.
*        READ TABLE IT_PA0001 INTO DATA(WA_PA0001) INDEX SY-INDEX.
*        V_KOSTL01 = WA_PA0001-KOSTL.
*
*      WHEN '2'.
*        READ TABLE IT_PA0001 INTO WA_PA0001 INDEX SY-INDEX.
*        V_KOSTL02 = WA_PA0001-KOSTL.
*    ENDCASE.
*  ENDDO.


  SORT it_zhcmt0007_base BY pernr.
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  DELETE it_zhcms_func_list WHERE cpf_nr EQ space.

  LOOP AT it_zhcms_func_list.

    CLEAR: it_zhcmt0007.
    it_zhcmt0007-pernr        = it_zhcms_func_list-pernr.
    it_zhcmt0007-cname        = it_zhcms_func_list-cname.
    it_zhcmt0007-cpf_nr       = it_zhcms_func_list-cpf_nr.
    it_zhcmt0007-pis_nr       = it_zhcms_func_list-pis_nr.
    it_zhcmt0007-departamento = it_zhcms_func_list-uniorg.
    it_zhcmt0007-funcao       = it_zhcms_func_list-posicao.
    it_zhcmt0007-bukrs        = it_zhcms_func_list-bukrs.
    it_zhcmt0007-werks        = it_zhcms_func_list-werks.

    READ TABLE it_t001 WITH KEY bukrs = it_zhcms_func_list-bukrs BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      it_zhcmt0007-butxt = it_t001-butxt.
    ENDIF.

*    READ TABLE it_j_1bbranch WITH KEY bukrs  = it_zhcms_func_list-bukrs branch = it_zhcms_func_list-werks BINARY SEARCH.
*    IF sy-subrc IS INITIAL.
*      it_zhcmt0007-werksn = it_j_1bbranch-name.
*      it_zhcmt0007-stcd1  = it_j_1bbranch-stcd1.
*    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_CGCBR_INPUT'
      EXPORTING
        input     = it_zhcms_func_list-cgc_number    " CGC in screen format (99.999.999/9999-99)
      IMPORTING
        output    = it_zhcmt0007-stcd1    " CGC in internal format (NUMC 14)
      EXCEPTIONS
        not_valid = 1
        OTHERS    = 2.

    TRY.
        it_zhcmt0007-werksn = lit_j_1bbranch[
            bukrs = it_zhcms_func_list-bukrs
            branch = it_zhcms_func_list-werks ]-name.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    it_zhcmt0007-kokrs        = it_zhcms_func_list-kokrs.
    it_zhcmt0007-kostl        = it_zhcms_func_list-kostl.
    it_zhcmt0007-ccusto       = it_zhcms_func_list-ccusto.
    it_zhcmt0007-gbdat        = it_zhcms_func_list-gbdat.
    it_zhcmt0007-email        = it_zhcms_func_list-email.
    it_zhcmt0007-sup_pernr    = it_zhcms_func_list-sup_pernr.
    it_zhcmt0007-sup_cname    = it_zhcms_func_list-sup_cname.
    it_zhcmt0007-sup_cpf_nr   = it_zhcms_func_list-sup_cpf_nr.
    it_zhcmt0007-sup_email    = it_zhcms_func_list-sup_email.
    it_zhcmt0007-sup_persk    = it_zhcms_func_list-sup_persk.
    it_zhcmt0007-endda        = it_zhcms_func_list-endda.
    it_zhcmt0007-begda        = it_zhcms_func_list-begda.
    it_zhcmt0007-dat01        = it_zhcms_func_list-dat01.
    it_zhcmt0007-fdate        = it_zhcms_func_list-fdate.

    TRANSLATE it_zhcms_func_list-situacao TO UPPER CASE.
    it_zhcmt0007-situacao     = it_zhcms_func_list-situacao.

    "Verificar Bloqueio RH """""""""""""""""""""""""""""""""""""""""""""""""
    READ TABLE it_t001p INTO DATA(wa_t001p) WITH KEY werks = it_zhcms_func_list-werks BINARY SEARCH.
    IF sy-subrc IS INITIAL AND it_zhcms_func_list-awart IS NOT INITIAL.
      READ TABLE it_pa_0015 INTO DATA(wa_pa_0015) WITH KEY moabw = wa_t001p-molga awart = it_zhcms_func_list-awart BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        it_zhcmt0007-bloqueio_rh = abap_true.
        it_zhcmt0007-moabw       = wa_pa_0015-moabw.
        it_zhcmt0007-awart       = wa_pa_0015-awart.
      ELSE.
        CLEAR: it_zhcmt0007-bloqueio_rh.
      ENDIF.
    ELSE.
      CLEAR: it_zhcmt0007-bloqueio_rh.
    ENDIF.

    "Nome da mãe
    READ TABLE it_0021[] ASSIGNING FIELD-SYMBOL(<lfs_0021>)
        WITH KEY pernr = it_zhcms_func_list-pernr BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      it_zhcmt0007-nome_mae = <lfs_0021>-fcnam.
    ENDIF.
***    TRY.
***        it_zhcmt0007-nome_mae = it_0021[ pernr = it_zhcms_func_list-pernr ]-fcnam.
***      CATCH cx_sy_itab_line_not_found.
***    ENDTRY.

    "Caso exista um bloqueio efetuado pelo RH "Desbloqueia """"""""""""""""""""""""""""""""
    READ TABLE it_zhcmt0007_base INTO DATA(wa_zhcmt0007_base) WITH KEY pernr = it_zhcms_func_list-pernr BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      "Transferido """""""""""""""""""""""""""""""""""
      IF ( wa_zhcmt0007_base-werks NE it_zhcmt0007-werks ) OR ( wa_zhcmt0007_base-kostl NE it_zhcms_func_list-kostl )  AND it_zhcmt0007-situacao EQ 'ATIVO' .
        "Comentado ref CS2020001244 - inicio
*        DATA: wa_zhcmt0007_tf TYPE zhcmt0007_tf.
*        CLEAR: wa_zhcmt0007_tf.
*        wa_zhcmt0007_tf-pernr        = wa_zhcmt0007_base-pernr.
*        wa_zhcmt0007_tf-dt_registro  = sy-datum.
*        wa_zhcmt0007_tf-hr_registro  = sy-uzeit.
*        wa_zhcmt0007_tf-werks_de     = wa_zhcmt0007_base-werks.
*        wa_zhcmt0007_tf-werks_para   = it_zhcmt0007-werks.
*        wa_zhcmt0007_tf-ck_executado = abap_false.

*        CALL FUNCTION 'CALCULATE_DATE'
*          EXPORTING
*            days        = '30'
*            months      = '0'
*            start_date  = sy-datum
*          IMPORTING
*            result_date = wa_zhcmt0007_tf-dt_executar.
*
*        "wa_zhcmt0007_tf-dt_executar  = sy-datum + 30.
*        MODIFY zhcmt0007_tf FROM wa_zhcmt0007_tf.
*
*
*        REPLACE ALL OCCURRENCES OF '.' IN wa_zhcmt0007_base-cpf_nr WITH ''.
*        REPLACE ALL OCCURRENCES OF '-' IN wa_zhcmt0007_base-cpf_nr WITH ''.
*
*        DATA: wa_fax_number TYPE ad_fxnmbr1.
*        wa_fax_number = wa_zhcmt0007_base-cpf_nr.
*
*        SELECT SINGLE * INTO @DATA(wa_adcp_)
*          FROM adcp
*         WHERE fax_number EQ @wa_fax_number.
*
*        SELECT SINGLE * INTO @DATA(wa_usr21_)
*          FROM usr21
*         WHERE persnumber EQ @wa_adcp_-persnumber.
*
*        SELECT * FROM agr_users INTO TABLE @DATA(it_agr_users)
*         WHERE uname EQ @wa_usr21_-bname.
*
*        erdata = wa_zhcmt0007_tf-dt_registro.
*
*
*        LOOP AT it_agr_users INTO DATA(wa_agr_users).
*          wa_activitygroups-agr_name   = wa_agr_users-agr_name.
*
*          IF wa_agr_users-from_dat < erdata.
*            wa_activitygroups-from_dat   = wa_agr_users-from_dat.
*            wa_activitygroups-to_dat     = wa_zhcmt0007_tf-dt_executar.
*          ELSE.
*            wa_activitygroups-from_dat   = wa_agr_users-from_dat.
*            wa_activitygroups-to_dat     = wa_agr_users-to_dat.
*          ENDIF.
*
*          APPEND wa_activitygroups TO it_activitygroups.
*          CLEAR wa_activitygroups.
*        ENDLOOP.
*
*        "Alterar data função na SU01
*        CALL FUNCTION 'BAPI_USER_ACTGROUPS_ASSIGN'
*          EXPORTING
*            username       = wa_usr21_-bname
*          TABLES
*            activitygroups = it_activitygroups
*            return         = it_return_g.
*
*        READ TABLE it_return_g INTO wa_return_g WITH KEY number = 048.
*
*        IF sy-subrc IS INITIAL.
*          "Incluindo Ação de Transferência
*          CLEAR: wa_acao_ad.
*          wa_acao_ad-pernr              = wa_zhcmt0007_tf-pernr.
*          wa_acao_ad-cpf                = wa_zhcmt0007_base-cpf_nr.
*          wa_acao_ad-dt_registro        = sy-datum.
*          wa_acao_ad-hr_registro        = sy-uzeit.
*          wa_acao_ad-ck_transferido     = abap_true.
*          wa_acao_ad-ds_motivo_bloqueio = 'Transferência de Filial'.
*          wa_acao_ad-ck_executado       = abap_false.
*          wa_acao_ad-dt_agendada        =  wa_zhcmt0007_tf-dt_executar.
*
*          MODIFY zhcmt0007_ad FROM wa_acao_ad.
*        ENDIF.
        "Comentado ref CS2020001244 - fim

      ENDIF.


      IF wa_zhcmt0007_base-bloqueio_rh EQ abap_true AND it_zhcmt0007-bloqueio_rh IS INITIAL.
        it_zhcmt0007-bloqueio_rh_efe = wa_zhcmt0007_base-bloqueio_rh_efe.
        CLEAR: it_zhcmt0007-moabw,
               it_zhcmt0007-awart.
      ENDIF.

      "Se não mudou o status, continua com o mesmo status
      IF wa_zhcmt0007_base-ck_user_desligado EQ abap_true AND it_zhcmt0007-situacao EQ 'ATIVO' AND wa_zhcmt0007_base-situacao NE 'ATIVO'.
        CLEAR: it_zhcmt0007-ck_user_desligado.
        it_zhcmt0007-bloqueio_rh     = abap_false.
        it_zhcmt0007-bloqueio_rh_efe = abap_true.
      ELSEIF wa_zhcmt0007_base-ck_user_desligado EQ abap_true AND it_zhcmt0007-situacao EQ 'ATIVO'.
        CLEAR: it_zhcmt0007-ck_user_desligado.
      ELSE.
        it_zhcmt0007-ck_user_desligado = wa_zhcmt0007_base-ck_user_desligado.
      ENDIF.

      it_zhcmt0007-endereco_ad    = wa_zhcmt0007_base-endereco_ad.
      it_zhcmt0007-senha_inicial  = wa_zhcmt0007_base-senha_inicial.
      it_zhcmt0007-samaccountname = wa_zhcmt0007_base-samaccountname.
      it_zhcmt0007-email_ad       = wa_zhcmt0007_base-email_ad.

      "Se o bloqueio foi efetuado e ainda está com motivo de ausencia, permanece bloqueio efetivado
      IF wa_zhcmt0007_base-bloqueio_rh_efe EQ abap_true AND it_zhcmt0007-bloqueio_rh_efe IS INITIAL AND
         wa_zhcmt0007_base-awart IS NOT INITIAL AND it_zhcmt0007-awart IS NOT INITIAL.
        it_zhcmt0007-bloqueio_rh_efe = wa_zhcmt0007_base-bloqueio_rh_efe.
      ENDIF.

      "Se o bloqueio foi efetuado e ainda está com motivo de ausencia, permanece bloqueio efetivado
      IF wa_zhcmt0007_base-bloqueio_rh_efe   EQ abap_true AND it_zhcmt0007-bloqueio_rh_efe   EQ abap_false AND
         wa_zhcmt0007_base-ck_user_desligado EQ abap_true AND it_zhcmt0007-situacao NE 'ATIVO'.
        it_zhcmt0007-bloqueio_rh_efe   = wa_zhcmt0007_base-bloqueio_rh_efe.
        it_zhcmt0007-ck_user_desligado = wa_zhcmt0007_base-ck_user_desligado.
      ENDIF.

      "Se o bloqueio foi efetuado e ainda está com motivo de ausencia, permanece bloqueio efetivado
      IF wa_zhcmt0007_base-bloqueio_rh_efe EQ abap_true  AND it_zhcmt0007-bloqueio_rh_efe EQ abap_false AND
         wa_zhcmt0007_base-bloqueio_rh     EQ abap_false AND it_zhcmt0007-situacao EQ 'ATIVO'.
        it_zhcmt0007-bloqueio_rh_efe = wa_zhcmt0007_base-bloqueio_rh_efe.
      ENDIF.

    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*** BUG - 102986 - CBRAND - Inicio
    CALL FUNCTION 'ZHCMF_RET_DIRETOR'
      EXPORTING
        pernr   = it_zhcms_func_list-pernr
      TABLES
        t_saida = it_ret_diretor.

    READ TABLE it_ret_diretor INTO DATA(lwa_ret_diretor) INDEX 1.

    it_zhcmt0007-pernr_diretor  = lwa_ret_diretor-pernr_diretor.
    it_zhcmt0007-nome_diretor   = lwa_ret_diretor-nome_diretor.
    it_zhcmt0007-cpf_diretor    = lwa_ret_diretor-cpf_diretor.

    CLEAR: it_ret_diretor[], lwa_ret_diretor.

*** BUG - 102986 - CBRAND - Fim

    APPEND it_zhcmt0007.
  ENDLOOP.

  DELETE it_zhcmt0007 WHERE cpf_nr EQ space.

  IF it_zhcmt0007[] IS NOT INITIAL.
    "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
    LOOP AT it_zhcmt0007 ASSIGNING FIELD-SYMBOL(<fs_zhcmt0007>).
      READ TABLE lit_zhcmt0007_upd ASSIGNING FIELD-SYMBOL(<fs_zhcmt0007_upd>) WITH KEY pernr = <fs_zhcmt0007>-pernr.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <fs_zhcmt0007> TO <fs_zhcmt0007_upd>.
      ELSE.
        APPEND <fs_zhcmt0007> TO lit_zhcmt0007_upd.
      ENDIF.
    ENDLOOP.

    "DELETE FROM ZHCMT0007.
    "MODIFY zhcmt0007 FROM TABLE it_zhcmt0007.
    "COMMIT WORK.
    "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
  ENDIF.

  DATA: it_zhcmt0007_aux TYPE TABLE OF ty_zhcmt0007 WITH HEADER LINE.

  SELECT * INTO TABLE it_zhcmt0007
    FROM zhcmt0007 AS a
   WHERE situacao EQ 'ATIVO'.

  LOOP AT it_zhcmt0007.
    CLEAR: it_zhcmt0007_aux.
    it_adcp-fax_number = it_zhcmt0007-cpf_nr.
    REPLACE ALL OCCURRENCES OF '.' IN it_adcp-fax_number WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN it_adcp-fax_number WITH ''.
    IF it_adcp-fax_number IS NOT INITIAL.
      MOVE-CORRESPONDING it_zhcmt0007 TO it_zhcmt0007_aux.
      it_zhcmt0007_aux-cpf_fax = it_adcp-fax_number.
      APPEND it_zhcmt0007_aux.
    ENDIF.
  ENDLOOP.

  IF it_zhcmt0007_aux[] IS NOT INITIAL.
    SELECT * INTO TABLE it_adcp
      FROM adcp
       FOR ALL ENTRIES IN it_zhcmt0007_aux
     WHERE fax_number EQ it_zhcmt0007_aux-cpf_fax.
  ENDIF.

  "Localizar os Usuários das Pessoas
  IF it_adcp[] IS NOT INITIAL.
    SELECT * INTO TABLE it_usr21
      FROM usr21
      FOR ALL ENTRIES IN it_adcp
    WHERE persnumber EQ it_adcp-persnumber.
    SORT it_usr21 BY bname.
    DELETE ADJACENT DUPLICATES FROM it_usr21 COMPARING bname.
  ENDIF.

  SORT it_adcp[] BY persnumber ASCENDING.
  SORT it_zhcmt0007_aux[] BY cpf_fax ASCENDING.
  SORT it_zhcmt0007[] BY pernr ASCENDING.

  "Excluir Usuário SAP
  IF it_usr21[] IS NOT INITIAL.
    LOOP AT it_usr21 INTO DATA(wa_usr21).

      READ TABLE it_adcp INTO DATA(wa_adcp) WITH KEY persnumber = wa_usr21-persnumber BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE it_zhcmt0007_aux INTO DATA(wa_zhcmt0007_aux) WITH KEY cpf_fax = wa_adcp-fax_number BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE it_zhcmt0007 ASSIGNING FIELD-SYMBOL(<fs_0007>) WITH KEY pernr = wa_zhcmt0007_aux-pernr BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      <fs_0007>-bname = wa_usr21-bname.

      "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
      READ TABLE lit_zhcmt0007_upd ASSIGNING <fs_zhcmt0007_upd> WITH KEY pernr = <fs_0007>-pernr.
      IF sy-subrc EQ 0.
        <fs_zhcmt0007_upd>-bname = <fs_0007>-bname.
      ELSE.
        APPEND <fs_0007> TO lit_zhcmt0007_upd.
      ENDIF.
      "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
    ENDLOOP.

    "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
    "MODIFY zhcmt0007 FROM TABLE it_zhcmt0007.
    "COMMIT WORK.
    "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP

  ENDIF.

*"//  Incluir a Ausencia data de inicio e Fim
  IF it_zhcmt0007[] IS NOT INITIAL.
    SELECT pernr, awart, begda, endda
      FROM pa2001
      INTO TABLE @DATA(it_2001)
      FOR ALL ENTRIES IN @it_zhcmt0007
      WHERE pernr EQ @it_zhcmt0007-pernr
        AND begda <= @sy-datum
        AND endda >= @sy-datum.
  ENDIF.

  IF it_2001 IS NOT INITIAL.

    SELECT awart, atext
      FROM t554t
      INTO TABLE @DATA(it_t554t)
      FOR ALL ENTRIES IN @it_2001
      WHERE awart EQ @it_2001-awart
        AND sprsl EQ @sy-langu
        AND moabw EQ '37'.

  ENDIF.

  LOOP AT it_zhcmt0007 ASSIGNING FIELD-SYMBOL(<f_zhcmt0007>).

    CLEAR: <f_zhcmt0007>-cod_ausencia, <f_zhcmt0007>-inicio_ausencia, <f_zhcmt0007>-fim_ausencia, <f_zhcmt0007>-desc_ausencia.

    READ TABLE it_2001 INTO DATA(wa_2001) WITH KEY pernr = <f_zhcmt0007>-pernr.
    IF sy-subrc IS INITIAL.
      <f_zhcmt0007>-cod_ausencia    = wa_2001-awart.
      <f_zhcmt0007>-inicio_ausencia = wa_2001-begda.
      <f_zhcmt0007>-fim_ausencia    = wa_2001-endda.
    ENDIF.

    READ TABLE it_t554t INTO DATA(wa_t554t) WITH KEY awart = wa_2001-awart.
    IF sy-subrc IS INITIAL.
      <f_zhcmt0007>-desc_ausencia = wa_t554t-atext.
    ENDIF.
********************************************************************** Start "171480 CS2025000337 Inc. cód. setor na view do SE PSA
    SELECT SINGLE * FROM pa0001 WHERE pernr = @<f_zhcmt0007>-pernr AND endda >= @sy-datum INTO @DATA(wa_pa0001). "171480 CS2025000337 Inc. cód. setor na view do SE PSA
    IF sy-subrc = 0.
      SELECT SINGLE * FROM hrp9665 WHERE objid = @wa_pa0001-plans AND endda >= @sy-datum INTO @DATA(wa_HRP9665).
      IF sy-subrc = 0.
        DATA(_setrisc) = |{ wa_HRP9665-setrisc } - { wa_HRP9665-stext }|.
        <f_zhcmt0007>-setrisc = _setrisc.
      ENDIF.
    ENDIF.
********************************************************************** end "171480 CS2025000337 Inc. cód. setor na view do SE PSA
    "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
    READ TABLE lit_zhcmt0007_upd ASSIGNING <fs_zhcmt0007_upd> WITH KEY pernr = <f_zhcmt0007>-pernr.
    IF sy-subrc EQ 0.
      <fs_zhcmt0007_upd>-cod_ausencia     = <f_zhcmt0007>-cod_ausencia.
      <fs_zhcmt0007_upd>-desc_ausencia    = <f_zhcmt0007>-desc_ausencia.
      <fs_zhcmt0007_upd>-inicio_ausencia  = <f_zhcmt0007>-inicio_ausencia.
      <fs_zhcmt0007_upd>-fim_ausencia     = <f_zhcmt0007>-fim_ausencia.
      <fs_zhcmt0007_upd>-setrisc     = <f_zhcmt0007>-setrisc."171480 CS2025000337 Inc. cód. setor na view do SE PSA
    ELSE.
      APPEND <f_zhcmt0007> TO lit_zhcmt0007_upd.
    ENDIF.
    "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
*** BUG - 147330 - Inicio - CBRAND
    CLEAR: wa_2001,
           wa_t554t.
*** BUG - 147330 - Fim - CBRAND
  ENDLOOP.

  "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
  CALL FUNCTION 'ZHCM_REGISTER_ZHCMT0007'
    EXPORTING
      i_zhcmt0007_t = lit_zhcmt0007_upd.
  "MODIFY zhcmt0007 FROM TABLE it_zhcmt0007.
  "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP

  COMMIT WORK.

ENDIF.

*** US - 178705 - CBRAND - Incio
DATA: lva_day TYPE pa0001-begda.
lva_day = ( sy-datum - 1 ).

SELECT * INTO TABLE @DATA(it_zhcmt_pa0001)
  FROM zhcmt_pa0001
 WHERE data_desligamento = @lva_day.

IF it_zhcmt_pa0001 IS NOT INITIAL.

  DATA   lra_pernr TYPE RANGE OF pa0001-pernr.

  lra_pernr = VALUE ty_rg_pernr( FOR w_pa0001 IN it_zhcmt_pa0001[] (
                             sign = 'I'
                             option = 'EQ'
                             low = w_pa0001-matricula )  ).


  SELECT * INTO TABLE @DATA(it_zhcmt0007_ad)
    FROM zhcmt0007_ad
   WHERE pernr IN @lra_pernr
    AND dt_registro = @sy-datum.

  SORT it_zhcmt0007_ad BY pernr.

  LOOP AT it_zhcmt_pa0001 INTO DATA(lwa_zhcmt_pa0001).

    SELECT SINGLE cpf_nr FROM pa0465 INTO @DATA(lva_cpf)
      WHERE pernr EQ @lwa_zhcmt_pa0001-matricula
        AND subty = '0001'
        AND endda >= @sy-datum.

    REPLACE ALL OCCURRENCES OF '.' IN lva_cpf WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN lva_cpf WITH ''.

    CONDENSE lva_cpf NO-GAPS.

    READ TABLE it_zhcmt0007_ad INTO DATA(lwa_hcmt0007_ad) WITH KEY pernr = lwa_zhcmt_pa0001-matricula
                                                                   cpf = lva_cpf
                                                                   ck_bloquear = 'X'
                                                                   ck_desligado = 'X'.

    IF sy-subrc <> 0.

      wa_acao_ad-pernr              = lwa_zhcmt_pa0001-matricula.
      wa_acao_ad-cpf                = lva_cpf.
      wa_acao_ad-dt_registro        = lwa_zhcmt_pa0001-data_desligamento.
      wa_acao_ad-hr_registro        = sy-uzeit.
      wa_acao_ad-ck_bloquear        = 'X'.
      wa_acao_ad-ck_desligado       = 'X'.
      wa_acao_ad-ds_motivo_bloqueio = 'Colaborador Desligado'.
      wa_acao_ad-cod_rescisao       =  lwa_zhcmt_pa0001-cod_rescisao.

      MODIFY zhcmt0007_ad FROM wa_acao_ad.
      CLEAR: wa_acao_ad.
    ENDIF.
    CLEAR: lwa_hcmt0007_ad, lwa_zhcmt_pa0001, lva_cpf, lra_pernr.
  ENDLOOP.
ENDIF.
*** US - 178705 - CBRAND - Fim
