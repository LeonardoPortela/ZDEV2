*&---------------------------------------------------------------------*
*& Report  ZHCMR_PA0013_AD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zhcmr_pa0013_ad.


TYPES: BEGIN OF ty_get_usr,
         cpf TYPE   string,
       END OF ty_get_usr.

TYPES: BEGIN OF ty_set_bloq,
         cpf            TYPE   string,
         motivobloqueio TYPE   string,
         desligado      TYPE   string,
         transferido    TYPE   string,
       END OF ty_set_bloq.

TYPES: BEGIN OF ty_add_usr,
         cpf               TYPE   string,
         nomecompleto      TYPE   string,
         pais              TYPE   string,
         filial            TYPE   string,
         empresa           TYPE   string,
         funcao            TYPE   string,
         departamento      TYPE   string,
         cidade            TYPE   string,
         estado            TYPE   string,
         cpfgestorimediato TYPE   string,
         dominio           TYPE   string,
         idunidade         TYPE   string,
       END OF ty_add_usr.


TYPES: BEGIN OF ty_upd_usr,
         cpf               TYPE   string,
         nomecompleto      TYPE   string,
         filial            TYPE   string,
         empresa           TYPE   string,
         funcao            TYPE   string,
         departamento      TYPE   string,
         cidade            TYPE   string,
         estado            TYPE   string,
         cpfgestorimediato TYPE   string,
         idunidade         TYPE   string,
       END OF ty_upd_usr.

TYPES: BEGIN OF ty_domi,
         bukrs   TYPE bukrs,
         dominio TYPE char50.
TYPES: END   OF ty_domi.

DATA: i_cpf	TYPE string.
DATA: lwa_active_directory TYPE zde_active_directory.
DATA: obj_directory TYPE REF TO zcl_int_ob_active_directory.
DATA:
*** BUG - 150010 - CBRAND - Inicio
  "lc_retorno_usr      TYPE zde_ret_usr,
  lc_retorno_usr      TYPE zde_ret_usr_ad,
*** BUG - 150010 - CBRAND - Fim
  lc_retorno_usr_add  TYPE zde_ret_usr_add,
  lc_retorno_usr_upd  TYPE zde_ret_usr_add,
  lc_retorno_usr_bloq TYPE zde_ret_usr_bloq.


DATA: git_get_usr  TYPE TABLE OF ty_get_usr,
      gwa_get_usr  TYPE ty_get_usr,
      git_set_usr  TYPE TABLE OF ty_get_usr,
      gwa_set_usr  TYPE ty_get_usr,
      gwa_set_bloq TYPE ty_set_bloq,
      gwa_add_usr  TYPE ty_add_usr,
      gwa_upd_usr  TYPE ty_upd_usr.

DATA: t_set  TYPE TABLE OF rgsb4,
      t_domi TYPE TABLE OF ty_domi,
      w_set  TYPE rgsb4,
      w_domi TYPE ty_domi.

*** CHECK sy-sysid EQ 'PRD'.

CREATE OBJECT obj_directory.

*-recuperar set dados - empresa x dominio
CALL FUNCTION 'G_SET_GET_ALL_VALUES'
  EXPORTING
    class           = '0000'
    setnr           = 'ZHCMR_PA0064'
    no_descriptions = ''
  TABLES
    set_values      = t_set
  EXCEPTIONS
    set_not_found   = 1
    OTHERS          = 2.

LOOP AT t_set INTO w_set.
  w_domi-bukrs   = w_set-from.
  w_domi-dominio = w_set-title.
  APPEND w_domi TO t_domi.
ENDLOOP.

SORT t_domi BY bukrs.

"Verificar Usuarios sem distinguishedName
SELECT * INTO TABLE @DATA(it_zhcmt0007)
  FROM zhcmt0007.

SORT it_zhcmt0007 BY pernr.

"Selecionar Ações Pendentes
SELECT * INTO TABLE @DATA(it_zhcmt0007_ad)
  FROM zhcmt0007_ad
 WHERE ck_executado EQ @abap_false
   AND ck_cancelado EQ @space.

SORT it_zhcmt0007_ad BY dt_registro hr_registro ASCENDING.

LOOP AT it_zhcmt0007_ad INTO DATA(wa_zhcmt0007_ad).

  READ TABLE it_zhcmt0007 INTO DATA(wa_zhcmt0007) WITH KEY pernr = wa_zhcmt0007_ad-pernr BINARY SEARCH.
  IF sy-subrc IS NOT INITIAL.
    CONTINUE.
  ENDIF.

  IF wa_zhcmt0007-endereco_ad IS INITIAL.
    "Busca Endereço do Ad do Usuario
    i_cpf = wa_zhcmt0007-cpf_nr.
    REPLACE ALL OCCURRENCES OF '.' IN i_cpf WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN i_cpf WITH ''.

    CLEAR: lwa_active_directory, lc_retorno_usr, gwa_get_usr.
    lwa_active_directory-tipo = 'GET_USUARIO'.
    lwa_active_directory-pernr = wa_zhcmt0007-pernr. "BUG - 15245 - CBRAND

    gwa_get_usr-cpf = i_cpf.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        = gwa_get_usr
        pretty_name = 'L'
      RECEIVING
        r_json      = DATA(lwa_json).

    lwa_active_directory-json = lwa_json.

* Primeiro verifica o status do usuário
    TRY .
        obj_directory->zif_integracao_outbound~get_instance(
        )->execute_request( EXPORTING i_info_request = lwa_active_directory IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
        IF _lwa_ret_call IS NOT INITIAL.
          /ui2/cl_json=>deserialize( EXPORTING json = _lwa_ret_call-ds_data_retorno CHANGING data = lc_retorno_usr ).
          IF lc_retorno_usr-distinguishedname IS NOT INITIAL.
            wa_zhcmt0007-endereco_ad = lc_retorno_usr-distinguishedname.

            CALL FUNCTION 'ZHCM_REGISTER_ZHCMT0007'
              EXPORTING
                i_zhcmt0007 = wa_zhcmt0007.
          ENDIF.
        ENDIF.
      CATCH zcx_integracao.
      CATCH zcx_error.
    ENDTRY.
    CLEAR: _lwa_ret_call.
* US - 79222 - Fim - CBRAND
    "Se Usuário Não Existe não faz nada!
    IF wa_zhcmt0007-endereco_ad IS INITIAL.
      IF wa_zhcmt0007_ad-ck_desligado EQ abap_true.
        wa_zhcmt0007_ad-ck_executado = abap_true.
        wa_zhcmt0007_ad-dt_executado = sy-datum.
        wa_zhcmt0007_ad-hr_executado = sy-uzeit.
        MODIFY zhcmt0007_ad FROM wa_zhcmt0007_ad.
        COMMIT WORK.
      ENDIF.
      CONTINUE.
    ELSE.
      "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
      CALL FUNCTION 'ZHCM_REGISTER_ZHCMT0007'
        EXPORTING
          i_zhcmt0007 = wa_zhcmt0007.
      "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
    ENDIF.
  ENDIF.

  CASE abap_true.
    WHEN wa_zhcmt0007_ad-ck_ativar.

      CLEAR: lwa_active_directory, _lwa_ret_call, lc_retorno_usr, lwa_json, gwa_set_usr.
      lwa_active_directory-tipo = 'SET_ATIVAR'.
      lwa_active_directory-pernr = wa_zhcmt0007_ad-pernr. "BUG - 15245 - CBRAND

      gwa_set_usr-cpf = CONV #( wa_zhcmt0007_ad-cpf ).
      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data        = gwa_set_usr
          pretty_name = 'L'
        RECEIVING
          r_json      = lwa_json.

      lwa_active_directory-json = lwa_json.

* Primeiro verifica o status do usuário
      TRY .
          obj_directory->zif_integracao_outbound~get_instance(
          )->execute_request( EXPORTING i_info_request = lwa_active_directory IMPORTING e_integracao = _lwa_ret_call  ).
          IF _lwa_ret_call IS NOT INITIAL.
            /ui2/cl_json=>deserialize( EXPORTING json = _lwa_ret_call-ds_data_retorno CHANGING data = lc_retorno_usr ).
*** BUG - 150010 - CBRAND - Inicio
*            IF _lwa_ret_call-nm_code = '0200'.
            IF _lwa_ret_call-nm_code = '0200'  AND lc_retorno_usr-sucesso = 'S'.
*** BUG - 150010 - CBRAND - Fim
              wa_zhcmt0007_ad-ck_executado = abap_true.
              wa_zhcmt0007_ad-dt_executado = sy-datum.
              wa_zhcmt0007_ad-hr_executado = sy-uzeit.
              MODIFY zhcmt0007_ad FROM wa_zhcmt0007_ad.
              COMMIT WORK.
            ENDIF.
          ENDIF.
        CATCH zcx_integracao.
        CATCH zcx_error.
      ENDTRY.
* US - 79222 - FIM - CBRAND

    WHEN wa_zhcmt0007_ad-ck_bloquear.

*     "// Verifica se esta Desativado o usuario no AD
      i_cpf = wa_zhcmt0007-cpf_nr.
      REPLACE ALL OCCURRENCES OF '.' IN i_cpf WITH ''.
      REPLACE ALL OCCURRENCES OF '-' IN i_cpf WITH ''.
      CLEAR: _lwa_ret_call, lc_retorno_usr_bloq, lwa_active_directory, gwa_set_bloq, lwa_json.

      lwa_active_directory-tipo = 'SET_BLOQUEAR'.
      lwa_active_directory-pernr = wa_zhcmt0007_ad-pernr. "BUG - 15245 - CBRAND
      gwa_set_bloq-cpf              = i_cpf.
      gwa_set_bloq-motivobloqueio   = wa_zhcmt0007_ad-ds_motivo_bloqueio.
      gwa_set_bloq-desligado        = CONV #( wa_zhcmt0007_ad-ck_desligado ).

      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data        = gwa_set_bloq
          pretty_name = 'L'
        RECEIVING
          r_json      = lwa_json.

      lwa_active_directory-json = lwa_json.

      TRY .
          obj_directory->zif_integracao_outbound~get_instance(
          )->execute_request( EXPORTING i_info_request = lwa_active_directory IMPORTING e_integracao = _lwa_ret_call  ).
          IF _lwa_ret_call IS NOT INITIAL.
            /ui2/cl_json=>deserialize( EXPORTING json = _lwa_ret_call-ds_data_retorno CHANGING data = lc_retorno_usr_bloq ).
*** BUG - 150010 - CBRAND - Inicio
*            IF _lwa_ret_call-nm_code = '0200'.
            IF _lwa_ret_call-nm_code = '0200' AND lc_retorno_usr_bloq-sucesso = 'S'.
*** BUG - 150010 - CBRAND - Fim
              wa_zhcmt0007_ad-ck_executado = abap_true.
              wa_zhcmt0007_ad-dt_executado = sy-datum.
              wa_zhcmt0007_ad-hr_executado = sy-uzeit.
              MODIFY zhcmt0007_ad FROM wa_zhcmt0007_ad.
              COMMIT WORK.
            ENDIF.
          ENDIF.
        CATCH zcx_integracao.
        CATCH zcx_error.
      ENDTRY.
* US - 79222 - FIM - CBRAND
    WHEN wa_zhcmt0007_ad-ck_enviar_e_mail.

      IF wa_zhcmt0007_ad-tx_email IS INITIAL.
        PERFORM corpo_email USING wa_zhcmt0007_ad wa_zhcmt0007 CHANGING wa_zhcmt0007_ad-tx_email .
      ENDIF.

      PERFORM enviar_email USING wa_zhcmt0007_ad CHANGING wa_zhcmt0007_ad-ck_executado wa_zhcmt0007_ad-ds_email.
      IF wa_zhcmt0007_ad-ck_executado EQ abap_true.
        wa_zhcmt0007_ad-dt_executado = sy-datum.
        wa_zhcmt0007_ad-hr_executado = sy-uzeit.
      ENDIF.
      MODIFY zhcmt0007_ad FROM wa_zhcmt0007_ad.
      COMMIT WORK.

    WHEN wa_zhcmt0007_ad-ck_transferido.

      IF sy-datum LT wa_zhcmt0007_ad-dt_agendada.
        CONTINUE.
      ENDIF.

      CLEAR: _lwa_ret_call, lc_retorno_usr_bloq, lwa_active_directory, gwa_set_bloq, lwa_json.

      lwa_active_directory-tipo = 'SET_BLOQUEAR'.
      lwa_active_directory-pernr = wa_zhcmt0007_ad-pernr. "BUG - 15245 - CBRAND
      gwa_set_bloq-cpf              = CONV #( wa_zhcmt0007_ad-cpf ).
      gwa_set_bloq-motivobloqueio   = wa_zhcmt0007_ad-ds_motivo_bloqueio.
      gwa_set_bloq-transferido       = CONV #( wa_zhcmt0007_ad-ck_transferido ).

      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data        = gwa_set_bloq
          pretty_name = 'L'
        RECEIVING
          r_json      = lwa_json.

      lwa_active_directory-json = lwa_json.

      TRY .
          obj_directory->zif_integracao_outbound~get_instance(
          )->execute_request( EXPORTING i_info_request = lwa_active_directory IMPORTING e_integracao = _lwa_ret_call  ).
          IF _lwa_ret_call IS NOT INITIAL.
            /ui2/cl_json=>deserialize( EXPORTING json = _lwa_ret_call-ds_data_retorno CHANGING data = lc_retorno_usr_bloq ).
*** BUG - 150010 - CBRAND - Inicio
*            IF _lwa_ret_call-nm_code = '0200'.
            IF _lwa_ret_call-nm_code = '0200' AND  lc_retorno_usr_bloq-sucesso = 'S'.
*** BUG - 150010 - CBRAND - Fim
              wa_zhcmt0007_ad-ck_executado = abap_true.
              wa_zhcmt0007_ad-dt_executado = sy-datum.
              wa_zhcmt0007_ad-hr_executado = sy-uzeit.
              MODIFY zhcmt0007_ad FROM wa_zhcmt0007_ad.
              COMMIT WORK.
            ENDIF.
          ENDIF.
        CATCH zcx_integracao.
        CATCH zcx_error.
      ENDTRY.
  ENDCASE.
ENDLOOP.

"Verificar Usuarios sem distinguishedName (Somente Usuários Ativos)
* BUG - 147330 - Inicio - CBRAND
* LOOP AT it_zhcmt0007 INTO wa_zhcmt0007 WHERE situacao EQ 'ATIVO'
LOOP AT it_zhcmt0007 INTO wa_zhcmt0007 WHERE situacao EQ 'ATIVO' AND mark_elimina <> 'X'.
* BUG - 147330 - fim - CBRAND
  IF wa_zhcmt0007-endereco_ad IS NOT INITIAL AND
     wa_zhcmt0007-samaccountname IS NOT INITIAL .
    "AND wa_zhcmt0007-email_ad IS NOT INITIAL. * BUG - 147330
    CONTINUE.
  ENDIF.

  CLEAR: i_cpf.
  i_cpf = wa_zhcmt0007-cpf_nr.
  REPLACE ALL OCCURRENCES OF '.' IN i_cpf WITH ''.
  REPLACE ALL OCCURRENCES OF '-' IN i_cpf WITH ''.

  CLEAR: lwa_active_directory, _lwa_ret_call, lc_retorno_usr, gwa_get_usr, lwa_json,
   gwa_add_usr,  gwa_upd_usr.

  lwa_active_directory-tipo = 'GET_USUARIO'.
  lwa_active_directory-pernr = wa_zhcmt0007-pernr. "BUG - 15245 - CBRAND
  gwa_get_usr-cpf = i_cpf.

  CALL METHOD /ui2/cl_json=>serialize
    EXPORTING
      data        = gwa_get_usr
      pretty_name = 'L'
    RECEIVING
      r_json      = lwa_json.

  lwa_active_directory-json = lwa_json.

* Primeiro verifica o status do usuário
  TRY .
      obj_directory->zif_integracao_outbound~get_instance(
      )->execute_request( EXPORTING i_info_request = lwa_active_directory IMPORTING e_integracao = _lwa_ret_call  ).
      IF _lwa_ret_call IS NOT INITIAL.
        /ui2/cl_json=>deserialize( EXPORTING json = _lwa_ret_call-ds_data_retorno CHANGING data = lc_retorno_usr ).

        IF lc_retorno_usr-distinguishedname IS NOT INITIAL.
          wa_zhcmt0007-endereco_ad    = lc_retorno_usr-distinguishedname.
          wa_zhcmt0007-samaccountname = lc_retorno_usr-samaccountname.
*** BUG - 150010 - CBRAND - Inicio
          "wa_zhcmt0007-email_ad       = lc_retorno_usr-mail.
          wa_zhcmt0007-email_ad       = lc_retorno_usr-email.
*** BUG - 150010 - CBRAND - Fim
          CALL FUNCTION 'ZHCM_REGISTER_ZHCMT0007'
            EXPORTING
              i_zhcmt0007 = wa_zhcmt0007.
        ENDIF.

      ENDIF.

*** Aqui entende-se que não existe usuário no ad.
      IF lc_retorno_usr-distinguishedname IS  INITIAL.
        TRY .
            CLEAR: lwa_active_directory, _lwa_ret_call, lc_retorno_usr, lc_retorno_usr_add, gwa_add_usr,lwa_json, lc_retorno_usr. "BUG - 152454 - CBRAND
            lwa_active_directory-tipo = 'SET_ADDUSUARIO'.
            lwa_active_directory-pernr = wa_zhcmt0007-pernr. "BUG - 15245 - CBRAND

            gwa_add_usr-cpf               = i_cpf.
            gwa_add_usr-nomecompleto      = CONV #( wa_zhcmt0007-cname ).
            gwa_add_usr-pais              = 'BR'.
            gwa_add_usr-filial            = wa_zhcmt0007-werksn. "wa_zhcmt0007-werks.
            gwa_add_usr-empresa           = wa_zhcmt0007-butxt.  "wa_zhcmt0007-bukrs.
            gwa_add_usr-funcao            = wa_zhcmt0007-funcao.
            CONCATENATE wa_zhcmt0007-departamento  '-' wa_zhcmt0007-kostl INTO gwa_add_usr-departamento SEPARATED BY space.

            SELECT SINGLE *
              FROM pa0006 INTO @DATA(lwa_pa0006)
             WHERE pernr EQ @wa_zhcmt0007-pernr
               AND subty = '1' "ENDERECO RESIDENCIAL
               AND endda >= @sy-datum.

            gwa_add_usr-cidade = lwa_pa0006-ort01.
            gwa_add_usr-estado = lwa_pa0006-state.

            gwa_add_usr-cpfgestorimediato = wa_zhcmt0007-sup_cpf_nr.

            REPLACE ALL OCCURRENCES OF '.' IN gwa_add_usr-cpfgestorimediato WITH ''.
            REPLACE ALL OCCURRENCES OF '-' IN gwa_add_usr-cpfgestorimediato WITH ''.

            CONDENSE gwa_add_usr-cpfgestorimediato NO-GAPS.

            "gwa_add_usr-dominio           = 'amaggi.com.br'.

            READ TABLE t_domi INTO w_domi WITH KEY bukrs = wa_zhcmt0007-bukrs
                             BINARY SEARCH.
            IF sy-subrc = 0.
              gwa_add_usr-dominio = w_domi-dominio.
            ENDIF.

            SELECT SINGLE *
               FROM pa0001 INTO @DATA(lwa_pa0001_aux)
              WHERE pernr EQ @wa_zhcmt0007-pernr
                AND endda >= @sy-datum.

            gwa_add_usr-idunidade = lwa_pa0001_aux-orgeh.

            CALL METHOD /ui2/cl_json=>serialize
              EXPORTING
                data        = gwa_add_usr
                pretty_name = 'L'
              RECEIVING
                r_json      = lwa_json.

            lwa_active_directory-json = lwa_json.

            TRY .
                obj_directory->zif_integracao_outbound~get_instance(
                )->execute_request( EXPORTING i_info_request = lwa_active_directory IMPORTING e_integracao = _lwa_ret_call  ).

                IF _lwa_ret_call IS NOT INITIAL.
*** BUG - 1524547 - Inicio - CBRAND
                  "/ui2/cl_json=>deserialize( EXPORTING json = _lwa_ret_call-ds_data_retorno CHANGING data = lc_retorno_usr_add ).
*               IF _lwa_ret_call-nm_code = '0200' AND lc_retorno_usr_add-distinguishedname IS NOT INITIAL.
*                    wa_zhcmt0007-endereco_ad    = lc_retorno_usr_add-distinguishedname.
*                    wa_zhcmt0007-senha_inicial  = lc_retorno_usr_add-senhadefinida.  "VERIFICAR
*                    wa_zhcmt0007-samaccountname = lc_retorno_usr_add-samaccountname. "VERIFICAR
*                    wa_zhcmt0007-email          = lc_retorno_usr_add-email. "Adicionado 23.07.2024

                  /ui2/cl_json=>deserialize( EXPORTING json = _lwa_ret_call-ds_data_retorno CHANGING data = lc_retorno_usr ).

                  "IF _lwa_ret_call-nm_code = '0200' AND lc_retorno_usr_add-distinguishedname IS NOT INITIAL.  "US - 173786 - CBRAND

                  IF _lwa_ret_call-nm_code = '0200' AND lc_retorno_usr-distinguishedname IS NOT INITIAL. "US - 173786 - CBRAND

                    wa_zhcmt0007-endereco_ad    = lc_retorno_usr-distinguishedname.
                    wa_zhcmt0007-senha_inicial  = lc_retorno_usr-pwdlastset.
                    wa_zhcmt0007-samaccountname = lc_retorno_usr-samaccountname.
                    wa_zhcmt0007-email          = lc_retorno_usr-email.
*** BUG - 1524547 - Fim - CBRAND

                    CALL FUNCTION 'ZHCM_REGISTER_ZHCMT0007'
                      EXPORTING
                        i_zhcmt0007 = wa_zhcmt0007.

                    CLEAR wa_zhcmt0007_ad.
                    wa_zhcmt0007_ad-pernr            = wa_zhcmt0007-pernr.
                    wa_zhcmt0007_ad-cpf              = i_cpf.
                    wa_zhcmt0007_ad-dt_registro      = sy-datum.
                    wa_zhcmt0007_ad-hr_registro      = sy-uzeit.
                    wa_zhcmt0007_ad-ck_enviar_e_mail = abap_true.
                    wa_zhcmt0007_ad-ds_email         = wa_zhcmt0007-sup_email.
                    TRANSLATE wa_zhcmt0007_ad-ds_email TO LOWER CASE.
                    PERFORM corpo_email USING wa_zhcmt0007_ad wa_zhcmt0007 CHANGING wa_zhcmt0007_ad-tx_email .
                    MODIFY zhcmt0007_ad FROM wa_zhcmt0007_ad.
                    COMMIT WORK.
                  ENDIF.
                ENDIF.
              CATCH zcx_integracao.
              CATCH zcx_error.
            ENDTRY.
            CLEAR:  lwa_pa0001_aux, lwa_pa0006, w_domi. "it_pa0002, lwa_pa0002,
        ENDTRY.
**      ELSE.
      ENDIF.
    CATCH zcx_integracao.
    CATCH zcx_error INTO DATA(ex_erro).
      DATA(lv_text) = ex_erro->get_longtext( ).
  ENDTRY.
  CLEAR: wa_zhcmt0007. "US - 173786 - CBRAND
ENDLOOP.

************ Atualização - Buscar alteração.
DATA: lva_data_atu TYPE sy-datum,
      gva_firedate TYPE dats.

TYPES:  ty_rg_pernr  TYPE RANGE OF pa0001-pernr.

lva_data_atu = ( sy-datum - 3 ).

DATA: it_tabelas TYPE TABLE OF tabname,
      it_pernr   TYPE TABLE OF pernr_d.


it_tabelas = VALUE #( ( 'PA9002' )
                      ( 'PA0001' )
                      ( 'PA0000' ) ).


LOOP AT it_tabelas[] INTO DATA(w_tabela).
  SELECT pernr
    FROM (w_tabela)
    APPENDING TABLE it_pernr
    WHERE aedtm >= lva_data_atu.
ENDLOOP.

SORT it_pernr[] ASCENDING.

IF ( it_pernr[] IS NOT INITIAL ).

  DELETE ADJACENT DUPLICATES FROM it_pernr[].

** Remover os demitidos
  DATA(r_pernr) = VALUE ty_rg_pernr( FOR lw_pernr IN it_pernr[] (
    sign = 'I' option = 'EQ' low =  lw_pernr  ) ).

  SELECT pernr INTO TABLE @DATA(it_pernr_dem)
     FROM pa0000
  WHERE pernr IN @r_pernr
     AND stat2 = '0'.

  IF it_pernr_dem IS NOT INITIAL.

    DATA(r_pernr_dem) = VALUE ty_rg_pernr( FOR lw_pernr_dem IN it_pernr_dem[] (
         sign = 'I' option = 'EQ' low =  lw_pernr_dem-pernr  ) ).

    DELETE it_pernr WHERE table_line IN r_pernr_dem.

  ENDIF.
*** Fim demitidos.

  LOOP AT it_pernr INTO DATA(lwa_pernr).

    CLEAR: gwa_get_usr, lwa_json, lwa_active_directory, _lwa_ret_call, lc_retorno_usr.

    SELECT SINGLE cpf_nr FROM pa0465 INTO @DATA(lva_cpf)
      WHERE pernr EQ @lwa_pernr
        AND subty = '0001'
        AND endda >= @sy-datum.

    i_cpf = lva_cpf.
    REPLACE ALL OCCURRENCES OF '.' IN i_cpf WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN i_cpf WITH ''.

    lwa_active_directory-tipo = 'GET_USUARIO'.
    lwa_active_directory-pernr = lwa_pernr. "BUG - 15245 - CBRAND

    gwa_get_usr-cpf = i_cpf.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        = gwa_get_usr
        pretty_name = 'L'
      RECEIVING
        r_json      = lwa_json.

    lwa_active_directory-json = lwa_json.

    TRY .
        obj_directory->zif_integracao_outbound~get_instance(
        )->execute_request( EXPORTING i_info_request = lwa_active_directory IMPORTING e_integracao = _lwa_ret_call  ).
        IF _lwa_ret_call IS NOT INITIAL.
          /ui2/cl_json=>deserialize( EXPORTING json = _lwa_ret_call-ds_data_retorno CHANGING data = lc_retorno_usr ).

          IF lc_retorno_usr-distinguishedname IS NOT INITIAL.
            CLEAR: lwa_json, lwa_active_directory,  _lwa_ret_call , lc_retorno_usr.

            SELECT * INTO TABLE @DATA(it_pa0001)
               FROM pa0001
             WHERE pernr EQ @lwa_pernr
               AND endda >= @lva_data_atu.

            SELECT * INTO TABLE @DATA(it_pa0002)
              FROM pa0002
            WHERE pernr EQ @lwa_pernr
              AND endda >= @lva_data_atu.

            SELECT * INTO TABLE @DATA(it_pa9002)
               FROM pa9002
            WHERE pernr EQ @lwa_pernr
               AND endda >= @lva_data_atu.

            SORT: it_pa0001,
                  it_pa0002,
                  it_pa9002 BY endda DESCENDING.

            READ TABLE it_pa0001 INTO DATA(lwa_pa0001) INDEX 1.
            READ TABLE it_pa0002 INTO DATA(lwa_pa0002) INDEX 1.
            READ TABLE it_pa9002 INTO DATA(lwa_pa9002) INDEX 1.

            SELECT SINGLE butxt
              FROM t001
            INTO gwa_upd_usr-empresa
              WHERE bukrs EQ lwa_pa0001-bukrs
                AND spras EQ sy-langu.

            SELECT SINGLE name1
              FROM t001w
            INTO gwa_upd_usr-filial
              WHERE werks = lwa_pa0001-werks
                AND spras = sy-langu.

            SELECT SINGLE stext
              FROM hrp1000
            INTO @DATA(lva_funcao)
             WHERE langu  = @sy-langu
               AND plvar  = '01'
               AND otype  = 'C'
               AND objid  = @lwa_pa0001-stell
               AND endda >= @sy-datum.

            gwa_upd_usr-funcao    = lva_funcao.
            gwa_upd_usr-idunidade = lwa_pa0001-orgeh.

            SELECT SINGLE stext FROM hrp1000 INTO @DATA(lva_dep)
              WHERE plvar = '01'
                AND otype = 'O'
                AND objid = @lwa_pa0001-orgeh
                AND endda >= @sy-datum
                AND langu = @sy-langu
                AND infty = '1000'.

            CONCATENATE lva_dep  '-' lwa_pa0001-kostl INTO gwa_upd_usr-departamento SEPARATED BY space.

            CLEAR: lwa_pa0006.
            SELECT SINGLE *
              FROM pa0006 INTO lwa_pa0006
             WHERE pernr EQ lwa_pa0001-pernr
               AND subty = '1' "ENDERECO RESIDENCIAL
               AND endda >= sy-datum.

            gwa_upd_usr-cidade = lwa_pa0006-ort01.
            gwa_upd_usr-estado = lwa_pa0006-state.

            TRY .
                CLEAR: lwa_active_directory, _lwa_ret_call, lc_retorno_usr, lc_retorno_usr_add, lwa_json.
                lwa_active_directory-tipo = 'SET_UPUSUARIO'.
                lwa_active_directory-pernr = lwa_pa0001-pernr. "BUG - 15245 - CBRAND
                gwa_upd_usr-cpf  = i_cpf.
                gwa_upd_usr-nomecompleto = CONV #( lwa_pa0002-cname ).
                gwa_upd_usr-cpfgestorimediato = lwa_pa9002-gestimed.

                REPLACE ALL OCCURRENCES OF '.' IN gwa_upd_usr-cpfgestorimediato WITH ''.
                REPLACE ALL OCCURRENCES OF '-' IN gwa_upd_usr-cpfgestorimediato WITH ''.

                CONDENSE gwa_upd_usr-cpfgestorimediato NO-GAPS.

                CALL METHOD /ui2/cl_json=>serialize
                  EXPORTING
                    data        = gwa_upd_usr
                    pretty_name = 'L'
                  RECEIVING
                    r_json      = lwa_json.

                lwa_active_directory-json = lwa_json.

                TRY .
                    obj_directory->zif_integracao_outbound~get_instance(
                    )->execute_request( EXPORTING i_info_request = lwa_active_directory IMPORTING e_integracao = _lwa_ret_call  ).

                    IF _lwa_ret_call IS NOT INITIAL.
                      /ui2/cl_json=>deserialize( EXPORTING json = _lwa_ret_call-ds_data_retorno CHANGING data = lc_retorno_usr_upd ).
                    ENDIF.
                ENDTRY.
            ENDTRY.
          ENDIF.
        ENDIF.
      CATCH zcx_integracao.
      CATCH zcx_error INTO ex_erro.
        lv_text = ex_erro->get_longtext( ).
    ENDTRY.

    CLEAR: lwa_pernr,
           it_pa0001,
           lwa_pa0001_aux,
           i_cpf,
           lva_cpf,
           "it_pa0000,
           it_pa9002,
           lwa_pa0001,
           "lwa_pa0000,
           lwa_pa9002,
           lva_dep,
           lva_funcao,
           gwa_upd_usr,
           lc_retorno_usr_upd.

  ENDLOOP.
ENDIF.
*********** Fim atualização

COMMIT WORK.

INCLUDE zhcmr_pa0013_ad_corpo_emailf01.
INCLUDE zhcmr_pa0013_ad_enviar_emaif01.
