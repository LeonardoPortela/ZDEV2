*&---------------------------------------------------------------------*
*& Report  zjob0004
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zjob0004.

TYPES: BEGIN OF ty_body_retorn,
         message     TYPE string,
         status_code TYPE string,
         detail      TYPE string,
         response    TYPE string,
       END OF ty_body_retorn.

TYPES: BEGIN OF ty_anexo,
         anexo TYPE string,
       END OF ty_anexo.

TYPES: BEGIN OF ty_body,
         userafetado  TYPE string,
         contato      TYPE string,
         categoria    TYPE string,
         subcategoria TYPE string,
         tip          TYPE string,
         descricao    TYPE string,
         prioridad    TYPE string,
         anexos       TYPE ty_anexo,
       END OF ty_body,

       BEGIN OF ty_dados_user,
         logsap TYPE string, "bname,
         nome   TYPE string, "cname,
         nmsnc  TYPE string, "SNC Name,
         logad  TYPE string,
         domain TYPE string,
       END OF ty_dados_user,

       BEGIN OF ty_user_sap,
         bname TYPE xubname,
       END OF ty_user_sap.

DATA: vg_job        TYPE i,
      lc_inicio     TYPE datum,
      it_zjob0002   TYPE TABLE OF zjob0002 WITH HEADER LINE,
      it_zjob0001   TYPE TABLE OF zjob0001 WITH HEADER LINE,
      lt_zjob0002   TYPE TABLE OF zjob0002,
      v_anal_resp   TYPE zde_usnam_basis,
      ztexto_ir     TYPE char255,
      it_dados_user TYPE TABLE OF ty_dados_user,
      it_user_sap   TYPE TABLE OF ty_user_sap.

DATA: zdate_hr_ult_ir         TYPE char14,
      zdate_hr_exec           TYPE char14,
      lva_data_limite         TYPE char14,
      lva_ret_date            TYPE tvpod-rudat,
      lva_ret_time            TYPE tvpod-rutim,
      lva_begin_datelocal_req TYPE d,
      lva_begin_timelocal_req TYPE t,

      zintervalo_hr_exec      TYPE p.

DATA: lc_retorno           TYPE ty_body_retorn,
      zdata                TYPE string,
      l_data               TYPE TABLE OF w3mime,
      zva_modulo           TYPE char05,
      lva_dt_start         TYPE erdat,
      lva_duration_integer TYPE i,
      rg_user              TYPE RANGE OF xubname.

DATA empty TYPE char1.

SELECT SINGLE COUNT( * ) INTO vg_job
  FROM tbtco
 WHERE jobname EQ 'CRIAR_IR_JOB_CANCELADO'
   AND status  EQ 'R'.

IF ( vg_job NE 1 ).
  EXIT.
ENDIF.

SELECT SINGLE low
FROM tvarvc
  INTO @DATA(lva_duration)
WHERE name EQ 'Z_ZJOB0004_SEC_ABRIR_IR'.

IF sy-subrc NE 0.
  lva_duration_integer = 1800.
ELSE.
  lva_duration_integer = lva_duration.
ENDIF.

lva_dt_start = sy-datum - 2.

SELECT * INTO TABLE lt_zjob0002
  FROM zjob0002 AS j
 WHERE j~dt_cancel GE lva_dt_start.

DELETE lt_zjob0002 WHERE nr_incidente IS NOT INITIAL.
DELETE lt_zjob0002 WHERE dt_cancel IS INITIAL OR hr_cancel IS INITIAL.

CHECK lt_zjob0002 IS NOT INITIAL.

"Issue #152778  - Ajuste Regra Job abertura IR - WPP - Ini
SELECT b~jobname, b~dt_cancel, b~hr_cancel
FROM zjob0002 AS b INTO TABLE @DATA(tg_zjob0002_last_ir)
 WHERE b~dt_cancel    GE @lva_dt_start
 AND   b~nr_incidente NE @space.

SORT tg_zjob0002_last_ir BY  jobname dt_cancel DESCENDING hr_cancel DESCENDING.
DELETE ADJACENT DUPLICATES FROM tg_zjob0002_last_ir COMPARING jobname.
"Issue #152778  - Ajuste Regra Job abertura IR - WPP - Ini

FREE: it_zjob0001.
SELECT * FROM zjob0001 INTO TABLE it_zjob0001
  FOR ALL ENTRIES IN lt_zjob0002
  WHERE jobname EQ lt_zjob0002-jobname.

"Usuario do AD.
IF it_zjob0001[] IS NOT INITIAL.

  rg_user = VALUE #( FOR i IN it_zjob0001 ( sign = 'I' option = 'EQ' low = i-anal_resp ) ).

  SELECT *
  FROM zhcmt0007
  INTO TABLE @DATA(it_login_sap)
    WHERE bname IN @rg_user.
ENDIF.

SORT lt_zjob0002 BY jobname dt_cancel hr_cancel.

LOOP AT lt_zjob0002 ASSIGNING FIELD-SYMBOL(<fs_zjob0002>).

  READ TABLE it_zjob0001 INTO DATA(ws_zjob0001) WITH KEY jobname = <fs_zjob0002>-jobname.
  CHECK sy-subrc EQ 0.

  READ TABLE tg_zjob0002_last_ir ASSIGNING FIELD-SYMBOL(<ws_zjob0002_last_ir>) WITH KEY jobname = <fs_zjob0002>-jobname. "Verifica ultimo registro job cancelado.
  IF sy-subrc EQ 0.
    "Verifica se tem mais de 30min o ultimo IR registrado no SE.
    zdate_hr_ult_ir = |{ <ws_zjob0002_last_ir>-dt_cancel }{ <ws_zjob0002_last_ir>-hr_cancel }|. "Ultima execução que foi criado o IR

    lva_begin_datelocal_req = <fs_zjob0002>-dt_cancel.
    lva_begin_timelocal_req = <fs_zjob0002>-hr_cancel.

    CALL FUNCTION 'TSTR_CALC_TIME'
      EXPORTING
        iv_begin_datelocal_req   = lva_begin_datelocal_req
        iv_begin_timelocal_req   = lva_begin_timelocal_req
        iv_duration_integer      = lva_duration_integer
        iv_direction             = '-'
      IMPORTING
        ev_end_datelocal         = lva_ret_date
        ev_end_timelocal         = lva_ret_time
      EXCEPTIONS
        fatal_error              = 1
        time_invalid             = 2
        time_missing             = 3
        tstream_not_loadable     = 4
        tstream_generation_error = 5
        parameter_error          = 6
        unspecified_error        = 7
        OTHERS                   = 8.

    lva_data_limite = |{ lva_ret_date }{ lva_ret_time }|.

    IF  zdate_hr_ult_ir < lva_data_limite.
      <fs_zjob0002>-ck_registrar_ir         = abap_true.
      <fs_zjob0002>-usuario_proc            = sy-uname.
      <fs_zjob0002>-mod_sap                 = ws_zjob0001-mod_sap.
      <ws_zjob0002_last_ir>-dt_cancel       = <fs_zjob0002>-dt_cancel.
      <ws_zjob0002_last_ir>-hr_cancel       = <fs_zjob0002>-hr_cancel.
    ENDIF.
  ELSE.
    <fs_zjob0002>-ck_registrar_ir = abap_true.
    <fs_zjob0002>-usuario_proc    = sy-uname.
    <fs_zjob0002>-mod_sap         = ws_zjob0001-mod_sap.

    "Issue #152778  - Ajuste Regra Job abertura IR - WPP - Ini
    APPEND INITIAL LINE TO tg_zjob0002_last_ir ASSIGNING <ws_zjob0002_last_ir>.

    <ws_zjob0002_last_ir>-jobname   = <fs_zjob0002>-jobname.
    <ws_zjob0002_last_ir>-dt_cancel = <fs_zjob0002>-dt_cancel.
    <ws_zjob0002_last_ir>-hr_cancel = <fs_zjob0002>-hr_cancel.
    "Issue #152778  - Ajuste Regra Job abertura IR - WPP - Fim


  ENDIF.
ENDLOOP.

DELETE lt_zjob0002 WHERE ck_registrar_ir EQ abap_false.

LOOP AT lt_zjob0002 ASSIGNING <fs_zjob0002> WHERE ck_registrar_ir EQ abap_true.

  ztexto_ir = |JOB SAP Cancelado { <fs_zjob0002>-jobname } Data: { <fs_zjob0002>-dt_cancel+6(2) }/{ <fs_zjob0002>-dt_cancel+4(2) }/{ <fs_zjob0002>-dt_cancel+0(4)
                     } Hora: { <fs_zjob0002>-hr_cancel+0(2) }:{ <fs_zjob0002>-hr_cancel+2(2) }:{ <fs_zjob0002>-hr_cancel+4(2) }|.

  CASE <fs_zjob0002>-mod_sap.
    WHEN 'CO'.
      zva_modulo = <fs_zjob0002>-mod_sap.
    WHEN 'CPI'.
      zva_modulo = <fs_zjob0002>-mod_sap.
    WHEN 'DRC'.
      zva_modulo = <fs_zjob0002>-mod_sap.
    WHEN 'FI'.
      zva_modulo = <fs_zjob0002>-mod_sap.
    WHEN 'HCM'.
      zva_modulo = <fs_zjob0002>-mod_sap.
    WHEN 'LES'.
      zva_modulo = <fs_zjob0002>-mod_sap.
    WHEN 'MM'.
      zva_modulo = <fs_zjob0002>-mod_sap.
    WHEN 'PM'.
      zva_modulo = <fs_zjob0002>-mod_sap.
    WHEN 'PP'.
      zva_modulo = <fs_zjob0002>-mod_sap.
    WHEN 'SD'.
      zva_modulo = <fs_zjob0002>-mod_sap.
    WHEN OTHERS.
      zva_modulo = 'Basis'.
  ENDCASE.

  READ TABLE it_zjob0001 INTO ws_zjob0001 WITH KEY jobname = <fs_zjob0002>-jobname.
  IF sy-subrc EQ 0.
    READ TABLE it_login_sap INTO DATA(ws_login_sap) WITH KEY bname = ws_zjob0001-anal_resp.
    IF sy-subrc NE 0.
      SELECT SINGLE *
        FROM tvarvc INTO @DATA(lwa_tvarvc)
        WHERE name = 'ZJOB0004_USER_AFETADO_DEFAULT'.

      IF sy-subrc EQ 0.
        TRANSLATE lwa_tvarvc-low TO LOWER CASE.
        ws_login_sap-samaccountname = lwa_tvarvc-low.
      ENDIF.
    ENDIF.
  ENDIF.

  DATA(zbody) = '{"userafetado": " ' && ws_login_sap-samaccountname && '",'
             && '"contato": "",  "categoria": "SAP",'
             && '"subcategoria": "'&& zva_modulo && '",'
             && '"tipo": "Falha JOB",'
             && '"descricao":" ' && ztexto_ir
             && '","prioridade": 2,'
             && '"anexos": [ ] }'.

  TRY .
      "Chama API para criar incidente.
      zcl_int_ob_create_incidente=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = zbody IMPORTING e_integracao = DATA(r_response) ).
      IF r_response IS NOT INITIAL.
        CLEAR: zdata, lc_retorno.
        zdata = r_response-ds_data_retorno.

        /ui2/cl_json=>deserialize( EXPORTING json = zdata CHANGING data = lc_retorno ).
        IF lc_retorno IS NOT INITIAL.
          <fs_zjob0002>-nr_incidente = lc_retorno-response.
          <fs_zjob0002>-dt_criacao   = sy-datum.
          <fs_zjob0002>-hr_criacao   = sy-uzeit.

          MODIFY zjob0002 FROM <fs_zjob0002>.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    CATCH zcx_integracao.
    CATCH zcx_error.
  ENDTRY.
ENDLOOP.
*
*"Registrar execução.
*if lt_zjob0002 is not initial.
*  modify zjob0002 from table lt_zjob0002.
*  commit work.
*endif.
