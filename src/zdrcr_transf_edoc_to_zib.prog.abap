************************************************************************
*                         ROLLOUT - hana                               *
************************************************************************
*  Projeto..: DRC                                                      *
*  Autor....: Jaime Tassoni                                            *
*  Data.....: 14.09.2023                                               *
*  Descrição: Transferencia XML --> Inbound                            *
************************************************************************
REPORT zdrcr_transf_edoc_to_zib.

************************************************************************
* tabelas
************************************************************************
TABLES edobrincoming.

************************************************************************
* parametros selecao
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK nfedata WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: pchave    FOR edobrincoming-accesskey,
                  pdata     FOR edobrincoming-issue_date.
  PARAMETERS    : pstcod   TYPE numc3.
  PARAMETERS    : pfzib  AS CHECKBOX,
                  pfdist AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK nfedata.

************************************************************************
* types
************************************************************************
TYPES: BEGIN OF ty_edocumentfile,
         edoc_guid TYPE edocumentfile-edoc_guid,
         file_raw  TYPE edocumentfile-file_raw,
         file_type TYPE edocumentfile-file_type,
       END   OF ty_edocumentfile,

       BEGIN OF ty_edobrincoming,
         edoc_guid TYPE edobrincoming-edoc_guid,
         accesskey TYPE edobrincoming-accesskey,
       END   OF ty_edobrincoming.


************************************************************************
* locais
************************************************************************
DATA: lv_rfcdest       TYPE bdbapidst,
      lt_edocument     TYPE edocument_tab,
      ws_edocument     TYPE edocument,
      git_access_key   TYPE TABLE OF ty_edobrincoming,
      lt_edocumentfile TYPE TABLE OF ty_edocumentfile,
      lv_xml_xstring   TYPE edocumentfile-file_raw,
      lt_status        TYPE zde_btcstatus_t,
      lv_inxstring     TYPE xstring,
      lv_outstring     TYPE string,
      lv_doctype       TYPE j_1b_nfe_doctype,
      lv_statcod       TYPE j_1bstatuscode,
      lv_dt_limite     TYPE edoc_change_date,
      lv_xml_com_erro  TYPE char01,
      lv_ds_erro       TYPE char255,
      lv_data_base     TYPE edocument-create_date,
      r_proc_status    TYPE RANGE OF edoc_status.

DATA: converter TYPE REF TO cl_abap_conv_out_ce.

DATA: lva_force_zib  TYPE c,
      lva_force_dist TYPE c.

************************************************************************
* start
************************************************************************
START-OF-SELECTION.

  FREE: lt_edocument, git_access_key, lt_edocumentfile.

*--------------------------------------
* status JOB Ativo
*--------------------------------------
  APPEND 'R' TO lt_status.

*** US #173081 - MMSILVA - 03.07.2025 - Ini ***
  PERFORM: distribuir_xml,
           distribuir_eventos.
*** US #173081 - MMSILVA - 03.07.2025 - Fim ***

FORM distribuir_xml. "US #173081 - MMSILVA - 03.07.2025 - Inserido todo o código dentro do FORM
  IF sy-batch = abap_true.

    TRY .
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = sy-cprog    " Nome de um programa em uma etapa (p.ex. report)
            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
            i_status     = lt_status    " Status de Jobs
          IMPORTING
            e_quantidade = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    CHECK e_qtd <= 1.

    lv_data_base = sy-datum - 2.

    SELECT *
      FROM edocument
      INTO TABLE lt_edocument
     WHERE create_date GE lv_data_base.

    SELECT *
      FROM edocument
      APPENDING TABLE lt_edocument
     WHERE change_date GE lv_data_base.

    SORT lt_edocument BY edoc_guid.
    DELETE ADJACENT DUPLICATES FROM lt_edocument COMPARING edoc_guid.

    IF lt_edocument[] IS NOT INITIAL.
      "Processo NF-e
      SELECT edoc_guid accesskey
        FROM edobrincoming
        INTO TABLE git_access_key
         FOR ALL ENTRIES IN lt_edocument
       WHERE edoc_guid = lt_edocument-edoc_guid.

      "Processo CT-e
      SELECT edoc_guid accesskey
        FROM edobrcteincoming
        APPENDING TABLE git_access_key
         FOR ALL ENTRIES IN lt_edocument
       WHERE edoc_guid = lt_edocument-edoc_guid.
    ENDIF.

  ELSEIF ( pchave[] IS NOT INITIAL ) OR ( pdata[] IS NOT INITIAL ).

    "Processo NF-e
    SELECT edoc_guid accesskey
      FROM edobrincoming
      INTO TABLE git_access_key
     WHERE accesskey  IN pchave
       AND issue_date IN pdata.

    "Processo CT-e
    SELECT edoc_guid accesskey
      FROM edobrcteincoming
      APPENDING TABLE git_access_key
     WHERE accesskey  IN pchave
       AND issue_date IN pdata.

    IF git_access_key[] IS NOT INITIAL.
      SELECT *
        FROM edocument INTO TABLE lt_edocument
         FOR ALL ENTRIES IN git_access_key
       WHERE edoc_guid = git_access_key-edoc_guid.
    ENDIF.

  ENDIF.

  zcl_drc_utils=>exclude_edoc_not_validated( CHANGING c_edocuments = lt_edocument ). "Tratar aqui para descartar EDOCUMENT não validados...

  IF ( pfzib  IS INITIAL AND pfdist IS INITIAL ).
    zcl_drc_utils=>exclude_edoc_transfered_to_zib( CHANGING c_edocuments = lt_edocument  ). "Tratar aqui para descartar EDOCUMENT já transferidos...
  ENDIF.

  CHECK lt_edocument[] IS NOT INITIAL.

*--------------------------------------
* selecao XML
*--------------------------------------
  SELECT edoc_guid file_raw file_type
    FROM edocumentfile
    INTO TABLE lt_edocumentfile
     FOR ALL ENTRIES IN lt_edocument
   WHERE edoc_guid EQ lt_edocument-edoc_guid.

*--------------------------------------
* Xml recuperados
*--------------------------------------

  DELETE lt_edocumentfile WHERE file_type NE 'NFE_XML' AND
                                file_type NE 'CTE_XML'.

  lva_force_zib  = pfzib.
  lva_force_dist = pfdist.

  IF sy-batch EQ abap_true.
    lva_force_zib  = abap_true.
    lva_force_dist = abap_true.
  ENDIF.


  LOOP AT lt_edocument INTO DATA(lw_edocument).

    CLEAR: lv_xml_com_erro, lv_ds_erro, lv_doctype.

    READ TABLE git_access_key  INTO DATA(lw_access_key)  WITH KEY edoc_guid = lw_edocument-edoc_guid.
    CHECK sy-subrc EQ 0.

    CASE lw_access_key-accesskey+20(2).
      WHEN '55'.
        lv_doctype = 'NFE'.
      WHEN '57'.
        lv_doctype = 'CTE'.
      WHEN '67'.
        lv_doctype = 'CTE'.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    READ TABLE lt_edocumentfile INTO DATA(lw_edocumentfile) WITH KEY edoc_guid = lw_edocument-edoc_guid.
    CHECK sy-subrc EQ 0.

    "Determinação Status Code SEFAZ
    IF ( sy-batch EQ abap_false ) AND ( pstcod IS NOT INITIAL ).
      lv_statcod = pstcod.
    ELSE.
      IF ( lw_edocument-edoc_type = 'BR_IN_CP'   ) OR  "Processo Cancelamento NF-e
         ( lw_edocument-edoc_type = 'BR_INCTECP' ) .   "Processo Cancelamento CT-e
        lv_statcod = '101'. "Status Cancelado SEFAZ
      ELSE.
        lv_statcod = '100'. "Status Autorizado SEFAZ
      ENDIF.
    ENDIF.

    PERFORM f_check_xml_erro USING lw_edocument
                          CHANGING lv_xml_com_erro
                                   lv_ds_erro.

    lv_outstring = zcl_string=>xstring_to_string( i_xstring = CONV #( lw_edocumentfile-file_raw ) ).

    CONCATENATE '<?xml version="1.0" encoding="UTF-8"?>' lv_outstring INTO lv_outstring.

    "Atualização Tabelas ZIB
    CALL FUNCTION 'Z_GRC_UPDATE_INBOUND'
      EXPORTING
        i_xml            = lv_outstring
        i_doctype        = lv_doctype
        i_cd_st_sefaz    = lv_statcod
        i_force_upd_dist = lva_force_dist
        i_force_upd_zib  = lva_force_zib
        i_xml_com_erro   = lv_xml_com_erro
        i_msg_erro       = lv_ds_erro.

  ENDLOOP.
ENDFORM. "US #173081 - MMSILVA - 03.07.2025 - Inserido todo o código dentro do FORM

FORM f_check_xml_erro  USING p_edocument TYPE edocument
                  CHANGING p_xml_com_erro TYPE char01
                           p_ds_erro      TYPE char255.

  "Documento Recebido com erro
*    lv_statcod = lw_edocument-metastatus.

*    CASE ws_edocument-proc_status.
*      WHEN '02' OR '21'.
*        lv_xml_com_erro = abap_true.
*
**       SELECT SINGLE *
**         FROM zgrct0004 INTO @DATA(wl_zgrct0004)
**        WHERE msgid EQ @wl_symsg-msgid
**          AND msgno EQ @wl_symsg-msgno.
***
**       IF ( sy-subrc EQ 0 ) AND ( wl_zgrct0004-msg IS NOT INITIAL ).
**         lv_ds_erro = wl_zgrct0004-msg.
**       ELSE.
**         MESSAGE ID wl_symsg-msgid TYPE 'S' NUMBER wl_symsg-msgno WITH wl_symsg-msgv1 wl_symsg-msgv2 wl_symsg-msgv3 wl_symsg-msgv4 INTO lv_ds_erro.
**       ENDIF.
*
*
*      WHEN OTHERS.
*        CONTINUE.
*    ENDCASE.


ENDFORM.

*** US #173081 - MMSILVA - 03.07.2025 - Ini ***
FORM distribuir_eventos.
  FREE: lt_edocument, git_access_key, lt_edocumentfile.
  CLEAR: lv_outstring, lv_data_base, lv_statcod.

  IF sy-batch = abap_true.

    TRY .
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = sy-cprog    " Nome de um programa em uma etapa (p.ex. report)
            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
            i_status     = lt_status    " Status de Jobs
          IMPORTING
            e_quantidade = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    CHECK e_qtd <= 1.

    lv_data_base = sy-datum - 2.

    SELECT *
      FROM edocument
      INTO TABLE lt_edocument
     WHERE create_date GE lv_data_base.

    SELECT *
      FROM edocument
      APPENDING TABLE lt_edocument
     WHERE change_date GE lv_data_base.

    SORT lt_edocument BY edoc_guid.
    DELETE ADJACENT DUPLICATES FROM lt_edocument COMPARING edoc_guid.

    IF lt_edocument[] IS NOT INITIAL.
      "Processo NF-e
      SELECT edoc_guid accesskey
        FROM edobrevent
        INTO TABLE git_access_key
        FOR ALL ENTRIES IN lt_edocument
        WHERE edoc_guid = lt_edocument-edoc_guid.
    ENDIF.

  ELSEIF ( pchave[] IS NOT INITIAL ) OR ( pdata[] IS NOT INITIAL ).

    "Processo NF-e
    SELECT edoc_guid accesskey
      FROM edobrevent
      INTO TABLE git_access_key
      WHERE accesskey   IN pchave
      AND issuing_date  IN pdata.

    IF git_access_key[] IS NOT INITIAL.
      SELECT *
        FROM edocument INTO TABLE lt_edocument
         FOR ALL ENTRIES IN git_access_key
       WHERE edoc_guid = git_access_key-edoc_guid.
    ENDIF.

  ENDIF.

  zcl_drc_utils=>exclude_edoc_not_validated( CHANGING c_edocuments = lt_edocument ). "Tratar aqui para descartar EDOCUMENT não validados... VERIFICAR DEPOIS SE É MESMO PARA COMENTAR

  IF ( pfzib  IS INITIAL AND pfdist IS INITIAL ).
    zcl_drc_utils=>exclude_edoc_transfered_to_zib( CHANGING c_edocuments = lt_edocument  ). "Tratar aqui para descartar EDOCUMENT já transferidos...
  ENDIF.

  CHECK lt_edocument[] IS NOT INITIAL.

*--------------------------------------
* selecao XML
*--------------------------------------
  SELECT edoc_guid file_raw file_type
    FROM edocumentfile
    INTO TABLE lt_edocumentfile
     FOR ALL ENTRIES IN lt_edocument
   WHERE edoc_guid EQ lt_edocument-edoc_guid.

*--------------------------------------
* Xml recuperados
*--------------------------------------

  DELETE lt_edocumentfile WHERE file_type NE 'EVT_XML'.

  lva_force_zib  = pfzib.
  lva_force_dist = pfdist.

  IF sy-batch EQ abap_true.
    lva_force_zib  = abap_true.
    lva_force_dist = abap_true.
  ENDIF.


  LOOP AT lt_edocument INTO DATA(lw_edocument).

    CLEAR: lv_xml_com_erro, lv_ds_erro, lv_doctype.

    READ TABLE git_access_key  INTO DATA(lw_access_key)  WITH KEY edoc_guid = lw_edocument-edoc_guid.
    CHECK sy-subrc EQ 0.

    CASE lw_access_key-accesskey+20(2).
      WHEN '55'.
        lv_doctype = 'NFE'.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    READ TABLE lt_edocumentfile INTO DATA(lw_edocumentfile) WITH KEY edoc_guid = lw_edocument-edoc_guid.
    CHECK sy-subrc EQ 0.

    "Determinação Status Code SEFAZ
    IF ( sy-batch EQ abap_false ) AND ( pstcod IS NOT INITIAL ).
      lv_statcod = pstcod.
    ELSE.
      IF ( lw_edocument-edoc_type = 'BR_IN_CP'   ) .  "Processo Cancelamento NF-e
        lv_statcod = '101'. "Status Cancelado SEFAZ
      ELSE.
        lv_statcod = '100'. "Status Autorizado SEFAZ
      ENDIF.
    ENDIF.

    lv_outstring = zcl_string=>xstring_to_string( i_xstring = CONV #( lw_edocumentfile-file_raw ) ).

    CONCATENATE '<?xml version="1.0" encoding="UTF-8"?>' lv_outstring INTO lv_outstring.

    "Atualização Tabelas ZIB
    CALL FUNCTION 'Z_GRC_UPDATE_INBOUND_EVENTOS'
      EXPORTING
        i_xml            = lv_outstring
        i_doctype        = lv_doctype
        i_cd_st_sefaz    = lv_statcod
        i_force_upd_dist = lva_force_dist
        i_force_upd_zib  = lva_force_zib
        i_xml_com_erro   = lv_xml_com_erro
        i_msg_erro       = lv_ds_erro.

  ENDLOOP.
ENDFORM.
*** US #173081 - MMSILVA - 03.07.2025 - Fim ***
