*&---------------------------------------------------------------------*
*& Report  ZHCMR_PA0013
*&
*&---------------------------------------------------------------------*
*& EXCLUIR USUÁRIO DE SISTEMA DE FUNCIONÁRIO DESLIGADO
*&  P/ Desativar Usuários do SAP
*&---------------------------------------------------------------------*
REPORT zhcmr_pa0013.

TYPES BEGIN OF ty_zhcmt0007.
INCLUDE TYPE zhcmt0007.
TYPES:   cpf_fax TYPE ad_fxnmbr1.
TYPES END OF ty_zhcmt0007.

DATA: it_zhcmt0007      TYPE TABLE OF zhcmt0007 WITH HEADER LINE,
      it_adcp           TYPE TABLE OF adcp WITH HEADER LINE,
      it_usr21          TYPE TABLE OF usr21 WITH HEADER LINE,
      it_return         TYPE TABLE OF bapiret2 WITH HEADER LINE,
      it_zhcmt0007_aux  TYPE TABLE OF ty_zhcmt0007 WITH HEADER LINE,
      wa_acao_ad        TYPE zhcmt0007_ad,
      it_activitygroups TYPE TABLE OF  bapiagr,
      wa_activitygroups TYPE bapiagr,
      it_return_g       TYPE TABLE OF bapiret2,
      wa_return_g       TYPE bapiret2.

" US - 159134 - CBRAND - Inicio
DATA:

  lit_usdocu_insert TYPE suid_tt_usdocu,
  ls_timestamp      TYPE  cl_identity=>ty_timestamp.
FIELD-SYMBOLS: <lf_docu> LIKE LINE OF lit_usdocu_insert.
" US - 159134 - CBRAND - Fim

DATA: erdata TYPE agr_users-from_dat.

" Eliminar Cadastro """""""""""""""""""""""""""""""""""""""""
" Eliminar Cadastro """""""""""""""""""""""""""""""""""""""""
" Eliminar Cadastro """""""""""""""""""""""""""""""""""""""""
" Eliminar Cadastro """""""""""""""""""""""""""""""""""""""""
" Eliminar Cadastro """""""""""""""""""""""""""""""""""""""""
" Eliminar Cadastro """""""""""""""""""""""""""""""""""""""""
" Eliminar Cadastro """""""""""""""""""""""""""""""""""""""""
" Eliminar Cadastro """""""""""""""""""""""""""""""""""""""""
" Eliminar Cadastro """""""""""""""""""""""""""""""""""""""""

SELECT * INTO TABLE it_zhcmt0007
  FROM zhcmt0007 AS a
 WHERE situacao NE 'ATIVO'
   AND NOT EXISTS ( SELECT * FROM zhcmt0007 AS b WHERE b~cpf_nr = a~cpf_nr AND b~situacao = 'ATIVO' AND mark_elimina = abap_false )
   AND ck_user_desligado EQ abap_false.

CLEAR: it_zhcmt0007_aux[].

LOOP AT it_zhcmt0007.
  CLEAR: it_zhcmt0007_aux.
  it_adcp-fax_number = it_zhcmt0007-cpf_nr.
  REPLACE ALL OCCURRENCES OF '.' IN it_adcp-fax_number WITH ''.
  REPLACE ALL OCCURRENCES OF '-' IN it_adcp-fax_number WITH ''.
  IF it_adcp-fax_number IS NOT INITIAL.
    MOVE-CORRESPONDING it_zhcmt0007 TO it_zhcmt0007_aux.
    it_zhcmt0007_aux-cpf_fax = it_adcp-fax_number.
    APPEND it_zhcmt0007_aux.
    APPEND it_adcp.
  ENDIF.
ENDLOOP.

SORT it_zhcmt0007_aux BY cpf_fax.

"Localizar as pessoas por CPF
IF it_adcp[] IS NOT INITIAL.
  SELECT * INTO TABLE it_adcp
    FROM adcp
     FOR ALL ENTRIES IN it_adcp
   WHERE fax_number EQ it_adcp-fax_number.
ENDIF.

"Localizar os Usuários das Pessoas
IF it_adcp[] IS NOT INITIAL.

  SORT it_adcp BY persnumber.

  SELECT * INTO TABLE it_usr21
    FROM usr21
    FOR ALL ENTRIES IN it_adcp
  WHERE persnumber EQ it_adcp-persnumber.

  SORT it_usr21 BY bname.
  DELETE ADJACENT DUPLICATES FROM it_usr21 COMPARING bname.
ENDIF.

"Excluir Usuário SAP

IF it_usr21[] IS NOT INITIAL.
  " US - 159134 - CBRAND - Inicio
  DATA: lva_data(10) TYPE c.
  DATA: lva_user_det TYPE zde_usnam_basis.
  DATA: lit_bapilogond LIKE bapilogond.
  DATA: lit_logondata TYPE  bapilogond.
  DATA: lit_logondatax LIKE bapilogonx.
  DATA: lit_retorno TYPE bapiret2_t.
  DATA: lit_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  LOOP AT it_usr21.

    CALL FUNCTION 'BAPI_USER_LOCK'
      EXPORTING
        username = it_usr21-bname
      TABLES
        return   = it_return.


    READ TABLE it_adcp WITH KEY persnumber = it_usr21-persnumber INTO DATA(wa_adcp) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE it_zhcmt0007_aux WITH KEY cpf_fax = wa_adcp-fax_number INTO DATA(wa_zhcmt0007_aux) BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        CLEAR: wa_acao_ad.
        wa_acao_ad-pernr              = wa_zhcmt0007_aux-pernr.
        wa_acao_ad-cpf                = wa_zhcmt0007_aux-cpf_fax.
        wa_acao_ad-dt_registro        = sy-datum.
        wa_acao_ad-hr_registro        = sy-uzeit.
        wa_acao_ad-ck_bloquear        = abap_true.
        wa_acao_ad-ds_motivo_bloqueio = 'Colaborador Desligado'.
        wa_acao_ad-ck_executado       = abap_false.
        wa_acao_ad-ck_desligado       = abap_true.
        MODIFY zhcmt0007_ad FROM wa_acao_ad.
        CLEAR: wa_adcp.

        READ TABLE it_zhcmt0007 WITH KEY pernr = wa_zhcmt0007_aux-pernr INTO DATA(wa_zhcmt0007).
        IF sy-subrc IS INITIAL.
          wa_zhcmt0007-ck_user_desligado = abap_true.

          CALL FUNCTION 'ZHCM_REGISTER_ZHCMT0007'
            EXPORTING
              i_zhcmt0007 = wa_zhcmt0007.

          DATA(lva_new_username) = it_usr21-bname.

          INSERT INITIAL LINE INTO TABLE lit_usdocu_insert ASSIGNING <lf_docu>.
          <lf_docu>-bname = lva_new_username.
          <lf_docu>-modda = sy-datum.
          <lf_docu>-modti = sy-uzeit.
          <lf_docu>-modbe = sy-uname.

          CONCATENATE wa_zhcmt0007_aux-fdate+6(2) '.' wa_zhcmt0007_aux-fdate+4(2)'.'  wa_zhcmt0007_aux-fdate+0(4) INTO lva_data.

          CONCATENATE 'BAIXA DE USUÁRIO POR MOTIVO DE DESLIGAMENTO NA DATA DE' lva_data
                INTO <lf_docu>-docu SEPARATED BY space.

          CALL FUNCTION 'SUID_IDENTITY_SAVE_TO_DB'
            EXPORTING
              it_usdocu_insert = lit_usdocu_insert
              is_timestamp     = ls_timestamp.


          lva_user_det = lva_new_username.

          CALL FUNCTION 'BAPI_USER_GET_DETAIL'
            EXPORTING
              username  = lva_user_det
            IMPORTING
              logondata = lit_logondata
            TABLES
              return    = lit_retorno.

          READ TABLE lit_retorno WITH KEY type = 'E' INTO DATA(lwa_retorno).
          IF sy-subrc IS NOT INITIAL.

            CLEAR lit_bapilogond.
            lit_logondata-gltgb = wa_zhcmt0007_aux-fdate.
            MOVE-CORRESPONDING lit_logondata TO lit_bapilogond.
            lit_logondatax = VALUE #( gltgb = abap_true ).

            TRY.
                CALL FUNCTION 'SUSR_BAPI_USER_CHANGE'
                  EXPORTING
                    username   = lva_user_det
                    logondata  = lit_bapilogond
                    logondatax = lit_logondatax
                  TABLES
                    return     = lit_return.
              CATCH cx_salv_not_found.
            ENDTRY.

          ENDIF.

        ENDIF.
        COMMIT WORK.
      ENDIF.
    ENDIF.
    CLEAR: lit_usdocu_insert, lva_new_username, lva_data, lit_logondata, lit_return, lit_bapilogond,
    lit_logondatax, lit_retorno, lva_user_det.
    IF <lf_docu> IS ASSIGNED.
      UNASSIGN <lf_docu>.
    ENDIF.
  ENDLOOP.
*  LOOP AT it_usr21.
*    CALL FUNCTION 'BAPI_USER_DELETE'
*      EXPORTING
*        username = it_usr21-bname
*      TABLES
*        return   = it_return.
*    READ TABLE it_adcp WITH KEY persnumber = it_usr21-persnumber INTO DATA(wa_adcp) BINARY SEARCH.
*    IF sy-subrc IS INITIAL.
*      READ TABLE it_zhcmt0007_aux WITH KEY cpf_fax = wa_adcp-fax_number INTO DATA(wa_zhcmt0007_aux) BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        CLEAR: wa_acao_ad.
*        wa_acao_ad-pernr       = wa_zhcmt0007_aux-pernr.
*        wa_acao_ad-cpf         = wa_zhcmt0007_aux-cpf_fax.
*        wa_acao_ad-dt_registro = sy-datum.
*        wa_acao_ad-hr_registro = sy-uzeit.
*        wa_acao_ad-ck_bloquear = abap_true.
*        wa_acao_ad-ds_motivo_bloqueio = 'Colaborador Desligado'.
*        wa_acao_ad-ck_executado       = abap_false.
*        wa_acao_ad-ck_desligado       = abap_true.
*        MODIFY zhcmt0007_ad FROM wa_acao_ad.
*        CLEAR: wa_adcp.
*
*        READ TABLE it_zhcmt0007 WITH KEY pernr = wa_zhcmt0007_aux-pernr INTO DATA(wa_zhcmt0007).
*        IF sy-subrc IS INITIAL.
*          wa_zhcmt0007-ck_user_desligado = abap_true.
*
*          "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
*          CALL FUNCTION 'ZHCM_REGISTER_ZHCMT0007'
*            EXPORTING
*              i_zhcmt0007 = wa_zhcmt0007.
*          "MODIFY zhcmt0007 FROM wa_zhcmt0007.
*          "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
*
*        ENDIF.
*        COMMIT WORK.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
ENDIF.
" US - 159134 - CBRAND - Fim

"Registrar Bloqueio de Ad de Usuário Desligado que não tem usuário SAP
LOOP AT it_zhcmt0007_aux INTO wa_zhcmt0007_aux.
  READ TABLE it_adcp WITH KEY fax_number = wa_zhcmt0007_aux-cpf_fax INTO wa_adcp.

  IF sy-subrc IS INITIAL.
    READ TABLE it_usr21 WITH KEY persnumber = wa_adcp-persnumber TRANSPORTING NO FIELDS.
  ENDIF.

  IF sy-subrc IS NOT INITIAL.
    CLEAR: wa_acao_ad.
    wa_acao_ad-pernr       = wa_zhcmt0007_aux-pernr.
    wa_acao_ad-cpf         = wa_zhcmt0007_aux-cpf_fax.
    wa_acao_ad-dt_registro = sy-datum.
    wa_acao_ad-hr_registro = sy-uzeit.
    wa_acao_ad-ck_bloquear = abap_true.
    wa_acao_ad-ds_motivo_bloqueio = 'Colaborador Desligado'.
    wa_acao_ad-ck_executado       = abap_false.
    wa_acao_ad-ck_desligado       = abap_true.
    MODIFY zhcmt0007_ad FROM wa_acao_ad.
    CLEAR: wa_adcp.

    READ TABLE it_zhcmt0007 WITH KEY pernr = wa_zhcmt0007_aux-pernr INTO wa_zhcmt0007.
    IF sy-subrc IS INITIAL.
      wa_zhcmt0007-ck_user_desligado = abap_true.
      "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
      CALL FUNCTION 'ZHCM_REGISTER_ZHCMT0007'
        EXPORTING
          i_zhcmt0007 = wa_zhcmt0007.
      "MODIFY zhcmt0007 FROM wa_zhcmt0007.
      "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
    ENDIF.
    COMMIT WORK.

  ENDIF.
ENDLOOP.

"Desativa do Mobile
DATA: i_ds_motivo	TYPE zde_motivo_status.

LOOP AT it_zhcmt0007 INTO wa_zhcmt0007.
  i_ds_motivo = wa_zhcmt0007-situacao.
  TRY .
      zcl_sol_mobile_rh=>zif_sol_mobile_rh~set_desativa_acesso(
        EXPORTING
          i_pernr     = wa_zhcmt0007-pernr
          i_ds_motivo = i_ds_motivo
      ).
    CATCH zcx_sol_mobile_rh.
  ENDTRY.
ENDLOOP.

" Bloquear Cadastro """""""""""""""""""""""""""""""""""""""""
" Bloquear Cadastro """""""""""""""""""""""""""""""""""""""""
" Bloquear Cadastro """""""""""""""""""""""""""""""""""""""""
" Bloquear Cadastro """""""""""""""""""""""""""""""""""""""""
" Bloquear Cadastro """""""""""""""""""""""""""""""""""""""""
" Bloquear Cadastro """""""""""""""""""""""""""""""""""""""""
" Bloquear Cadastro """""""""""""""""""""""""""""""""""""""""
" Bloquear Cadastro """""""""""""""""""""""""""""""""""""""""
" Bloquear Cadastro """""""""""""""""""""""""""""""""""""""""
" Bloquear Cadastro """""""""""""""""""""""""""""""""""""""""

CLEAR:
it_zhcmt0007, it_zhcmt0007[],
it_adcp     , it_adcp[],
it_usr21    , it_usr21[],
it_return   , it_return[],
it_zhcmt0007_aux, it_zhcmt0007_aux[].

SELECT * INTO TABLE it_zhcmt0007
  FROM zhcmt0007 AS a
 WHERE situacao    EQ 'ATIVO'
   AND bloqueio_rh EQ abap_true.

SELECT * APPENDING TABLE it_zhcmt0007
  FROM zhcmt0007 AS a
 WHERE situacao        EQ 'ATIVO'
   AND bloqueio_rh     EQ abap_false
   AND bloqueio_rh_efe EQ abap_true.

LOOP AT it_zhcmt0007.
  CLEAR: it_zhcmt0007_aux.
  it_adcp-fax_number = it_zhcmt0007-cpf_nr.
  REPLACE ALL OCCURRENCES OF '.' IN it_adcp-fax_number WITH ''.
  REPLACE ALL OCCURRENCES OF '-' IN it_adcp-fax_number WITH ''.
  IF it_adcp-fax_number IS NOT INITIAL.
    MOVE-CORRESPONDING it_zhcmt0007 TO it_zhcmt0007_aux.
    it_zhcmt0007_aux-cpf_fax = it_adcp-fax_number.
    APPEND it_zhcmt0007_aux.
    APPEND it_adcp.
  ENDIF.
ENDLOOP.

"Localizar as pessoas por CPF
IF it_adcp[] IS NOT INITIAL.
  SELECT * INTO TABLE it_adcp
    FROM adcp
     FOR ALL ENTRIES IN it_adcp
   WHERE fax_number EQ it_adcp-fax_number.
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

"Excluir Usuário SAP
IF it_usr21[] IS NOT INITIAL.

  LOOP AT it_usr21.

    READ TABLE it_adcp INTO wa_adcp WITH KEY persnumber = it_usr21-persnumber.
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    READ TABLE it_zhcmt0007_aux INTO wa_zhcmt0007_aux WITH KEY cpf_fax = wa_adcp-fax_number.
    IF sy-subrc IS NOT INITIAL.
      CLEAR: wa_zhcmt0007_aux.
      CONTINUE.
    ENDIF.

    CASE wa_zhcmt0007_aux-bloqueio_rh.
      WHEN abap_true.
        IF wa_zhcmt0007_aux-bloqueio_rh_efe EQ abap_false.
          CALL FUNCTION 'BAPI_USER_LOCK'
            EXPORTING
              username = it_usr21-bname
            TABLES
              return   = it_return.

          READ TABLE it_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            CLEAR: wa_acao_ad.
            wa_acao_ad-pernr              = wa_zhcmt0007_aux-pernr.
            wa_acao_ad-cpf                = wa_zhcmt0007_aux-cpf_fax.
            wa_acao_ad-dt_registro        = sy-datum.
            wa_acao_ad-hr_registro        = sy-uzeit.
            wa_acao_ad-ck_bloquear        = abap_true.
            wa_acao_ad-ck_executado       = abap_false.

            SELECT SINGLE * INTO @DATA(wa_t554t)
              FROM t554t
             WHERE sprsl EQ @sy-langu
               AND moabw EQ '37'
               AND awart EQ @wa_zhcmt0007_aux-awart.

            IF sy-subrc IS INITIAL.
              CONCATENATE 'Bloqueado pelo RH' wa_t554t-atext INTO wa_acao_ad-ds_motivo_bloqueio SEPARATED BY space.
            ELSE.
              CONCATENATE 'Bloqueado pelo RH ' wa_zhcmt0007_aux-awart INTO wa_acao_ad-ds_motivo_bloqueio SEPARATED BY space.
            ENDIF.

            MODIFY zhcmt0007_ad FROM wa_acao_ad.

            "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
*            UPDATE zhcmt0007
*               SET bloqueio_rh_efe = abap_true
*             WHERE pernr EQ wa_zhcmt0007_aux-pernr.

            SELECT SINGLE *
              FROM zhcmt0007 INTO @DATA(lwa_hcmt0007_upd)
              WHERE pernr EQ @wa_zhcmt0007_aux-pernr.

            IF sy-subrc EQ 0.
              lwa_hcmt0007_upd-bloqueio_rh_efe = abap_true.

              CALL FUNCTION 'ZHCM_REGISTER_ZHCMT0007'
                EXPORTING
                  i_zhcmt0007 = lwa_hcmt0007_upd.
            ENDIF.
            "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP

            COMMIT WORK.
          ENDIF.
        ENDIF.

      WHEN abap_false.
        IF wa_zhcmt0007_aux-bloqueio_rh_efe EQ abap_true.
          CALL FUNCTION 'BAPI_USER_UNLOCK'
            EXPORTING
              username = it_usr21-bname
            TABLES
              return   = it_return.

          READ TABLE it_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.

            CLEAR: wa_acao_ad.
            wa_acao_ad-pernr              = wa_zhcmt0007_aux-pernr.
            wa_acao_ad-cpf                = wa_zhcmt0007_aux-cpf_fax.
            wa_acao_ad-dt_registro        = sy-datum.
            wa_acao_ad-hr_registro        = sy-uzeit.
            wa_acao_ad-ck_ativar          = abap_true.
            wa_acao_ad-ds_motivo_bloqueio = ''.
            wa_acao_ad-ck_executado       = abap_false.
            MODIFY zhcmt0007_ad FROM wa_acao_ad.

            "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
*            UPDATE zhcmt0007
*               SET bloqueio_rh_efe = abap_false
*             WHERE pernr EQ wa_zhcmt0007_aux-pernr.

            SELECT SINGLE *
              FROM zhcmt0007 INTO lwa_hcmt0007_upd
             WHERE pernr EQ wa_zhcmt0007_aux-pernr.

            IF sy-subrc EQ 0.
              lwa_hcmt0007_upd-bloqueio_rh_efe = abap_false.

              CALL FUNCTION 'ZHCM_REGISTER_ZHCMT0007'
                EXPORTING
                  i_zhcmt0007 = lwa_hcmt0007_upd.
            ENDIF.
            "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP

            COMMIT WORK.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDLOOP.

ENDIF.

"Bloquear e Desbloquear Usuário que não tem Usuário SAP
LOOP AT it_zhcmt0007_aux INTO wa_zhcmt0007_aux.

  READ TABLE it_adcp WITH KEY fax_number = wa_zhcmt0007_aux-cpf_fax INTO wa_adcp.

  IF sy-subrc IS INITIAL.
    READ TABLE it_usr21 WITH KEY persnumber = wa_adcp-persnumber TRANSPORTING NO FIELDS.
  ENDIF.

  IF sy-subrc IS NOT INITIAL.
    CASE wa_zhcmt0007_aux-bloqueio_rh.
      WHEN abap_true.

        IF wa_zhcmt0007_aux-bloqueio_rh_efe EQ abap_false.
          CLEAR: wa_acao_ad.
          wa_acao_ad-pernr              = wa_zhcmt0007_aux-pernr.
          wa_acao_ad-cpf                = wa_zhcmt0007_aux-cpf_fax.
          wa_acao_ad-dt_registro        = sy-datum.
          wa_acao_ad-hr_registro        = sy-uzeit.
          wa_acao_ad-ck_bloquear        = abap_true.
          wa_acao_ad-ck_executado       = abap_false.

          SELECT SINGLE * INTO @wa_t554t
            FROM t554t
           WHERE sprsl EQ @sy-langu
             AND moabw EQ '37'
             AND awart EQ @wa_zhcmt0007_aux-awart.

          IF sy-subrc IS INITIAL.
            CONCATENATE 'Bloqueado pelo RH' wa_t554t-atext INTO wa_acao_ad-ds_motivo_bloqueio SEPARATED BY space.
          ELSE.
            CONCATENATE 'Bloqueado pelo RH ' wa_zhcmt0007_aux-awart INTO wa_acao_ad-ds_motivo_bloqueio SEPARATED BY space.
          ENDIF.

          MODIFY zhcmt0007_ad FROM wa_acao_ad.

          "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
*          UPDATE zhcmt0007
*             SET bloqueio_rh_efe = abap_true
*           WHERE pernr EQ wa_zhcmt0007_aux-pernr.

          SELECT SINGLE *
            FROM zhcmt0007 INTO lwa_hcmt0007_upd
           WHERE pernr EQ wa_zhcmt0007_aux-pernr.

          IF sy-subrc EQ 0.
            lwa_hcmt0007_upd-bloqueio_rh_efe = abap_true.

            CALL FUNCTION 'ZHCM_REGISTER_ZHCMT0007'
              EXPORTING
                i_zhcmt0007 = lwa_hcmt0007_upd.
          ENDIF.
          "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP


          COMMIT WORK.
        ENDIF.

      WHEN abap_false.
        IF wa_zhcmt0007_aux-bloqueio_rh_efe EQ abap_true.
          CLEAR: wa_acao_ad.
          wa_acao_ad-pernr              = wa_zhcmt0007_aux-pernr.
          wa_acao_ad-cpf                = wa_zhcmt0007_aux-cpf_fax.
          wa_acao_ad-dt_registro        = sy-datum.
          wa_acao_ad-hr_registro        = sy-uzeit.
          wa_acao_ad-ck_ativar          = abap_true.
          wa_acao_ad-ds_motivo_bloqueio = ''.
          wa_acao_ad-ck_executado       = abap_false.
          MODIFY zhcmt0007_ad FROM wa_acao_ad.

          "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP
*          UPDATE zhcmt0007
*             SET bloqueio_rh_efe = abap_false
*           WHERE pernr EQ wa_zhcmt0007_aux-pernr.

          SELECT SINGLE *
            FROM zhcmt0007 INTO lwa_hcmt0007_upd
            WHERE pernr EQ wa_zhcmt0007_aux-pernr.

          IF sy-subrc EQ 0.
            lwa_hcmt0007_upd-bloqueio_rh_efe = abap_false.

            CALL FUNCTION 'ZHCM_REGISTER_ZHCMT0007'
              EXPORTING
                i_zhcmt0007 = lwa_hcmt0007_upd.
          ENDIF.
          "HCM - Disp. Dados Funcionario Legados  US #102068 - WPP


          COMMIT WORK.
        ENDIF.
    ENDCASE.
  ENDIF.
ENDLOOP.

"Transferência de Colaborador

"Comentado ref CS2020001244 - inicio
*SELECT *
*  INTO TABLE @DATA(it_zhcmt0007_tf)
*  FROM zhcmt0007_tf AS a
* WHERE ck_executado EQ @abap_false
*  AND  dt_executar  EQ @sy-datum.
*
*LOOP AT it_zhcmt0007_tf INTO DATA(wa_zhcmt0007_tf).
*
*  wa_zhcmt0007_tf-ck_executado = abap_true.
*  wa_zhcmt0007_tf-dt_executado = sy-datum.
*  wa_zhcmt0007_tf-hr_executado = sy-uzeit.
*
*  SELECT SINGLE * INTO @wa_zhcmt0007
*    FROM zhcmt0007
*   WHERE pernr EQ @wa_zhcmt0007_tf-pernr.
*
*  IF sy-subrc IS NOT INITIAL.
*    MODIFY zhcmt0007_tf FROM wa_zhcmt0007_tf.
*    CONTINUE.
*  ENDIF.
*
*  REPLACE ALL OCCURRENCES OF '.' IN wa_zhcmt0007-cpf_nr WITH ''.
*  REPLACE ALL OCCURRENCES OF '-' IN wa_zhcmt0007-cpf_nr WITH ''.
*
*  DATA: wa_fax_number TYPE ad_fxnmbr1.
*  wa_fax_number = wa_zhcmt0007-cpf_nr.
*
*  SELECT SINGLE * INTO @wa_adcp
*    FROM adcp
*   WHERE fax_number EQ @wa_fax_number.
*
*  IF sy-subrc IS NOT INITIAL.
*    MODIFY zhcmt0007_tf FROM wa_zhcmt0007_tf.
*    CONTINUE.
*  ENDIF.
*
*  SELECT SINGLE * INTO @DATA(wa_usr21)
*    FROM usr21
*  WHERE persnumber EQ @wa_adcp-persnumber.
*
*  IF sy-subrc IS NOT INITIAL.
*    MODIFY zhcmt0007_tf FROM wa_zhcmt0007_tf.
*    CONTINUE.
*  ENDIF.
"Comentado ref CS2020001244 - fim



*  "Excluir Usuário SAP
**  CALL FUNCTION 'BAPI_USER_DELETE'
**    EXPORTING
**      USERNAME = WA_USR21-BNAME
**    TABLES
**      RETURN   = IT_RETURN.
*
**  CLEAR erdata.
**
**  erdata = wa_zhcmt0007_tf-dt_registro - 30.
**
**  SELECT * FROM agr_users INTO TABLE @DATA(it_agr_users)
**   WHERE uname EQ @wa_usr21-bname.
**
**
**  LOOP AT it_agr_users INTO DATA(wa_agr_users).
**    wa_activitygroups-agr_name   = wa_agr_users-agr_name.
**
**    IF wa_agr_users-from_dat < erdata.
**      wa_activitygroups-from_dat   = wa_agr_users-from_dat.
**      wa_activitygroups-to_dat     = wa_zhcmt0007_tf-dt_executar.
**    ELSE.
**      wa_activitygroups-from_dat   = wa_agr_users-from_dat.
**      wa_activitygroups-to_dat     = wa_agr_users-to_dat.
**    ENDIF.
**
**    APPEND wa_activitygroups TO it_activitygroups.
**    CLEAR wa_activitygroups.
**  ENDLOOP.
**
**  "Alterar data função na SU01
**  CALL FUNCTION 'BAPI_USER_ACTGROUPS_ASSIGN'
**    EXPORTING
**      username       = wa_usr21-bname
**    TABLES
**      activitygroups = it_activitygroups
**      return         = it_return_g.
**
**  READ TABLE it_return_g INTO wa_return_g WITH KEY number = 048.
**
**  IF sy-subrc IS INITIAL.
**
**    MODIFY zhcmt0007_tf FROM wa_zhcmt0007_tf.
**
**    "Incluindo Ação de Transferência
**    CLEAR: wa_acao_ad.
**    wa_acao_ad-pernr              = wa_zhcmt0007_tf-pernr.
**    wa_acao_ad-cpf                = wa_zhcmt0007-cpf_nr.
**    wa_acao_ad-dt_registro        = sy-datum.
**    wa_acao_ad-hr_registro        = sy-uzeit.
**    wa_acao_ad-ck_transferido     = abap_true.
**    wa_acao_ad-ds_motivo_bloqueio = 'Transferência de Filial'.
**    wa_acao_ad-ck_executado       = abap_false.
**
**    CALL FUNCTION 'CALCULATE_DATE'
**      EXPORTING
**        days        = '30'
**        months      = '0'
**        start_date  = sy-datum
**      IMPORTING
**        result_date = wa_acao_ad-dt_agendada.
**
**    MODIFY zhcmt0007_ad FROM wa_acao_ad.
**
**  ENDIF.
**
**  CLEAR: it_activitygroups[], it_agr_users[].

"ENDLOOP


"Comentado ref CS2020001244 - inicio
"PERFORM zf_envia_email_colab_transf.
"Comentado ref CS2020001244 - fim

SUBMIT zhcmr_pa0013_ad.

*&---------------------------------------------------------------------*
*&      Form  ZF_ENVIA_EMAIL_COLAB_TRANSF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_envia_email_colab_transf.

  DATA: t_gestores TYPE TABLE OF zhcms_ret_sup9002.

  DATA: l_dt_prim_envio TYPE zhcmt0007_ad-dt_agendada,
        l_dt_seg_envio  TYPE zhcmt0007_ad-dt_agendada,
        l_nro_envio     TYPE c.

*---------------------------------------------
* Atividades p/ Executar Active Directory
*---------------------------------------------

  SELECT * FROM zhcmt0007_ad
    INTO TABLE @DATA(t_0007_ad)
    WHERE ck_transferido = @abap_true
      AND ck_executado   = @abap_false.


  CHECK sy-subrc IS INITIAL.

  LOOP AT t_0007_ad INTO DATA(w_0007_ad).

    CHECK w_0007_ad-dt_agendada IS NOT INITIAL.
*---------------------------------------------
* Primeiro envio
*---------------------------------------------
    l_dt_prim_envio = w_0007_ad-dt_agendada - 30.
    IF l_dt_prim_envio = sy-datum.
      l_nro_envio = 1.
    ENDIF.

*---------------------------------------------
*Segundo envio
*---------------------------------------------
    l_dt_seg_envio  = w_0007_ad-dt_agendada - 15.
    IF  l_dt_seg_envio  = sy-datum.
      l_nro_envio = 2.
    ENDIF.

    CHECK l_nro_envio IS NOT INITIAL.

*---------------------------------------------
* Email do Colaborador
*---------------------------------------------
    SELECT * "pernr, usrid_long
      FROM pa0105
       INTO TABLE @DATA(it_email)
          WHERE  pernr  = @w_0007_ad-pernr
            AND ( usrty = 'MAIL' OR usrty = '0010' ).

*Usar a função abaixo para buscar o Gestor Mediato e Imediato
    REFRESH: t_gestores.
    CALL FUNCTION 'ZHCMF_RET_SUPERIOR_9002'
      EXPORTING
        pernr          = w_0007_ad-pernr
      TABLES
        t_saida        = t_gestores
      CHANGING
        c_data_posicao = sy-datum.

*selecionar apenas o Gestor Imediato.
    DELETE t_gestores WHERE tp_gest = 'M'.

    " Email
    IF t_gestores[] IS NOT INITIAL.

      SELECT * " pernr, usrid_long
        FROM pa0105
       INTO TABLE @DATA(it_gestor_email)
        FOR ALL ENTRIES IN @t_gestores
          WHERE  pernr = @t_gestores-pernr
            AND ( usrty = 'MAIL' OR usrty = '0010' ).

    ENDIF.

    IF  it_gestor_email[] IS NOT INITIAL.
      APPEND LINES OF it_gestor_email TO it_email.
    ENDIF.

    CHECK it_email[] IS NOT INITIAL.

    SORT it_email[] BY pernr ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_email COMPARING usrid_long.

    CALL FUNCTION 'ZHCMF_NOTIF_VENCIMENTO'
      EXPORTING
        i_colaborador  = w_0007_ad-pernr
        i_nro_aviso    = l_nro_envio
        i_zhcmt0007_ad = w_0007_ad
      TABLES
        tl_email       = it_email.

    REFRESH: it_email, it_gestor_email.

  ENDLOOP.

ENDFORM.
