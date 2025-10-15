*&---------------------------------------------------------------------*
*& Report  ZMMR0030
*&--------------------------------------------------------------------&*
*& Projeto..: Amaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 14/04/2015                                              &*
*& Descrição: Interface TRACECOTTON x SAP PP - IN                     &*
*& Transação: PP                                                      &*
*& Request..: DEVK945561                                              &*
*&--------------------------------------------------------------------&*
REPORT  zppr0030  MESSAGE-ID ztracecotton.
TYPE-POOLS vrm.

TABLES: zpps_ximfbf_log.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS : s_objkey FOR zpps_ximfbf_log-obj_key.
SELECTION-SCREEN END OF BLOCK b1.

**----------------------------------------------------------------------
** VARIEVEIS
**----------------------------------------------------------------------
TYPES: BEGIN OF ty_execretrn,
         type       TYPE bapi_mtype,
         id         TYPE symsgid,
         number     TYPE symsgno,
         message    TYPE bapi_msg,
         message_v1 TYPE symsgv,
         message_v2 TYPE symsgv,
         message_v3 TYPE symsgv,
         message_v4 TYPE symsgv,
       END    OF ty_execretrn.

TYPES: BEGIN OF ty_doc_material,
         obj_key    TYPE zpps_ximfbf_log-obj_key,
         mblnr      TYPE zpps_ximfbf_log-mblnr,
         cd_safra   TYPE zpps_ximfbf_log-cd_safra,
         nrobol     TYPE zpps_ximfbf_log-nrobol,
         fgorigem   TYPE zpps_ximfbf_log-fgorigem,
         id_matgeo  TYPE zpps_ximfbf_log-id_matgeo,
         matnr      TYPE zpps_ximfbf_log-matnr,
         dtmvto     TYPE zpps_ximfbf_log-dtmvto,
         processado TYPE zpps_ximfbf_log-processado,
         fg_tpmovto TYPE zpps_ximfbf_log-fg_tpmovto,
         cd_ccusto  TYPE bseg-kostl,
         zst_atlz   TYPE zpps_ximfbf_log-zst_atlz,
         id_cotton  TYPE zpps_ximfbf_log-id_cotton,
         charg      TYPE zpps_ximfbf_log-charg,
         zst_atlz_2 TYPE zpps_ximfbf_log-zst_atlz,
         hora       TYPE zpps_ximfbf_log-hora,
       END OF  ty_doc_material,

       BEGIN OF ty_bkpf,
         belnr TYPE bkpf-belnr,
         awkey TYPE bkpf-awkey,
         bktxt TYPE bkpf-bktxt,
         budat TYPE bkpf-budat,
       END OF ty_bkpf,

       BEGIN OF ty_bseg,
         belnr TYPE bseg-belnr,
         dmbtr TYPE bseg-dmbtr,
         dmbe2 TYPE bseg-dmbe2,
         dmbe3 TYPE bseg-dmbe3,
         matnr TYPE bseg-matnr,
         werks TYPE bseg-werks,
         menge TYPE bseg-menge,
         bewar TYPE bseg-bewar,
       END OF ty_bseg.

DATA: t_ximfbf          TYPE TABLE OF zpps_ximfbf_log,
      w_ximfbf          TYPE zpps_ximfbf_log,
      t_ximfbf_ger      TYPE TABLE OF zpps_ximfbf_log,
      w_ximfbf_ger      TYPE zpps_ximfbf_log,
      t_ximfbf_geo      TYPE TABLE OF zpps_ximfbf_log,
      w_ximfbf_geo      TYPE zpps_ximfbf_log,
      t_ximfbf_obm      TYPE TABLE OF zpps_ximfbf_log,
      w_ximfbf_obm      TYPE zpps_ximfbf_log,
      yt_log_mfpf       TYPE TABLE OF zfie_ret_document WITH HEADER LINE INITIAL SIZE 0,
      yt_execretrn      TYPE TABLE OF ty_execretrn WITH HEADER LINE INITIAL SIZE 0,
*
      wa_doc_material   TYPE ty_doc_material,
      wa_bkpf           TYPE ty_bkpf,
      wa_bseg           TYPE ty_bseg,
      ti_doc_material   TYPE TABLE OF ty_doc_material,
      ti_bkpf           TYPE TABLE OF ty_bkpf,
      ti_bseg           TYPE TABLE OF ty_bseg,
      yt_geo_oubound_ok TYPE TABLE OF zmme_return_sucess WITH HEADER LINE INITIAL SIZE 0,
      lp_matnr          TYPE matnr,
      l_text_m02        TYPE string,
      l_awkey           TYPE bkpf-awkey,
      l_grid_title      TYPE lvc_title,
      l_program         TYPE sy-repid,
      w_layout          TYPE slis_layout_alv,
      t_fieldcat        TYPE slis_t_fieldcat_alv.

*&---------------------------------------------------------------------*
*&      Start-Of-Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM f_seleciona_dados.

  IF t_ximfbf[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não foram selecionados dados!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM f_processa_dados.
  PERFORM f_exibe_envio.

*&---------------------------------------------------------------------*
*& selecao dados
*&---------------------------------------------------------------------*
FORM f_seleciona_dados.

  SELECT *
    INTO TABLE t_ximfbf
    FROM zpps_ximfbf_log
   WHERE obj_key IN s_objkey.

ENDFORM.

*&---------------------------------------------------------------------*
*& processar dados
*&---------------------------------------------------------------------*
FORM f_processa_dados.

  CHECK t_ximfbf[] IS NOT INITIAL.

  t_ximfbf_ger[] = t_ximfbf[].

*----------------------------------
* GEO
*----------------------------------
  t_ximfbf_geo[] = t_ximfbf_ger[].
  DELETE t_ximfbf_geo WHERE nrobol IS INITIAL.

  PERFORM f_processa_geo.

*----------------------------------
* OB MENSAGEM
*----------------------------------
  t_ximfbf_obm[] = t_ximfbf_ger[].
  DELETE t_ximfbf_obm WHERE nrobol IS NOT INITIAL.

  PERFORM f_processa_obm.

ENDFORM.

FORM f_processa_geo.

  t_ximfbf[] = t_ximfbf_geo[].

  FREE: ti_doc_material.

  LOOP AT t_ximfbf INTO w_ximfbf.

    CLEAR wa_doc_material.

    CHECK w_ximfbf-mblnr IS NOT INITIAL.

    SELECT SINGLE smbln
      FROM mseg
      INTO @DATA(_smbln)
     WHERE smbln = @w_ximfbf-mblnr.

    CHECK sy-subrc <> 0.

    wa_doc_material-mblnr       = w_ximfbf-mblnr.
    wa_doc_material-processado  = w_ximfbf-processado.
    wa_doc_material-charg       = w_ximfbf-charg.
    wa_doc_material-matnr       = w_ximfbf-matnr.
*
    wa_doc_material-obj_key     = w_ximfbf-obj_key  .
    wa_doc_material-cd_safra    = w_ximfbf-dtmvto(4) .
    wa_doc_material-nrobol      = w_ximfbf-nrobol   .
    wa_doc_material-fgorigem    = w_ximfbf-fgorigem .
    " wa_doc_material-id_matgeo  = w_xi_mfbf-id_matgeo.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_ximfbf-id_matgeo
      IMPORTING
        output = wa_doc_material-id_matgeo.

    wa_doc_material-dtmvto      = w_ximfbf-dtmvto.
    wa_doc_material-hora        = w_ximfbf-hora.
    wa_doc_material-fg_tpmovto  = w_ximfbf-fg_tpmovto.
    wa_doc_material-zst_atlz_2  = w_ximfbf-zst_atlz.
    wa_doc_material-zst_atlz    = wa_doc_material-zst_atlz_2.
    wa_doc_material-id_cotton   = w_ximfbf-id_cotton.

    APPEND wa_doc_material     TO ti_doc_material.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM ti_doc_material.

  FREE: yt_geo_oubound_ok.
  LOOP AT ti_doc_material INTO wa_doc_material.

    REFRESH : ti_bseg, ti_bkpf.

    CLEAR: yt_geo_oubound_ok.

    CONCATENATE wa_doc_material-mblnr  wa_doc_material-cd_safra  INTO l_awkey.

    yt_geo_oubound_ok-idbol       = wa_doc_material-obj_key .
    yt_geo_oubound_ok-nrobol      = wa_doc_material-nrobol  .

    IF yt_geo_oubound_ok-nrobol IS INITIAL.
      yt_geo_oubound_ok-nrobol    = wa_doc_material-id_cotton.
    ENDIF.

    yt_geo_oubound_ok-fgorigem    = wa_doc_material-fgorigem.
    yt_geo_oubound_ok-bwart       = wa_doc_material-fg_tpmovto.
    yt_geo_oubound_ok-erzet       = wa_doc_material-hora. "sy-uzeit.
    yt_geo_oubound_ok-id_matgeo   = wa_doc_material-id_matgeo.

    lp_matnr = wa_doc_material-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lp_matnr
      IMPORTING
        output = lp_matnr.

    yt_geo_oubound_ok-matnr       = lp_matnr .
    yt_geo_oubound_ok-docmat      = l_awkey .
    yt_geo_oubound_ok-dmbtr       = 0.
    yt_geo_oubound_ok-dmbe2       = 0.
    yt_geo_oubound_ok-dmbe3       = 0.
    yt_geo_oubound_ok-dtproc      = wa_doc_material-dtmvto. "sy-datum.
    yt_geo_oubound_ok-hrproc      = wa_doc_material-hora.   "sy-uzeit.
    yt_geo_oubound_ok-erdat       = wa_doc_material-dtmvto.

    SELECT belnr awkey bktxt budat
      INTO TABLE ti_bkpf
      FROM bkpf
     WHERE gjahr EQ wa_doc_material-cd_safra
       AND tcode IN ('MFBF','MB1A', 'MF41')
       AND awtyp = 'MKPF'
       AND blart = 'WA'
       AND awkey = l_awkey  .

    IF sy-subrc IS INITIAL.
      CLEAR:wa_bkpf.
      "READ TABLE TI_BKPF INTO WA_BKPF INDEX 1.

      IF wa_doc_material-zst_atlz = 'M' .
        DATA lt_fields TYPE fagl_t_field.
        DATA lt_bseg TYPE TABLE OF bseg.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = wa_doc_material-matnr
          IMPORTING
            output = wa_doc_material-matnr.

        SELECT belnr dmbtr dmbe2 dmbe3 matnr werks menge bewar
          FROM bseg
          INTO TABLE ti_bseg
           FOR ALL ENTRIES IN ti_bkpf
         WHERE belnr EQ ti_bkpf-belnr
           AND gjahr = wa_doc_material-cd_safra  "(exercício)
           AND matnr = wa_doc_material-matnr
           AND kostl = wa_doc_material-cd_ccusto
           AND buzid = 'S'
           AND shkzg = 'S'.

*        lt_fields = VALUE #( ( line = 'BELNR' )
*                             ( line = 'DMBTR' )
*                             ( line = 'DMBE2' )
*                             ( line = 'DMBE3' )
*                             ( line = 'MATNR' )
*                             ( line = 'WERKS' )
*                             ( line = 'MENGE' )
*                             ( line = 'BEWAR' )
*                              ).
*
*        CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
*          EXPORTING
*            it_for_all_entries = ti_bkpf
*            i_where_clause     = | BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR|
*            it_fieldlist       = lt_fields
*          IMPORTING
*            et_bseg            = lt_bseg
*          EXCEPTIONS
*            not_found          = 1.

        DELETE lt_bseg WHERE   matnr NE wa_doc_material-matnr
                           AND kostl NE wa_doc_material-cd_ccusto
                           AND buzid NE 'S'
                           AND shkzg NE 'S'.

        IF sy-subrc = 0 AND lines( lt_bseg ) > 0.
          sy-dbcnt = lines( lt_bseg ).
          MOVE-CORRESPONDING lt_bseg TO ti_bseg.
        ELSE.
          sy-subrc = 4.
          sy-dbcnt = 0.
        ENDIF.
* <--- S4 Migration - 16/06/2023 - JV

      ELSE.
        "YT_GEO_OUBOUND_OK-ERDAT    = WA_BKPF-BUDAT.
        "Recuperando os valores reais, dolar, ufir e material
* ---> S4 Migration - 19/06/2023 - JS

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = wa_doc_material-matnr
          IMPORTING
            output = wa_doc_material-matnr.

        SELECT belnr dmbtr dmbe2 dmbe3 matnr werks menge bewar
          FROM bseg
          INTO TABLE ti_bseg
           FOR ALL ENTRIES IN ti_bkpf
         WHERE belnr EQ ti_bkpf-belnr
           AND gjahr   = wa_doc_material-cd_safra  "(exercício)
           AND matnr   = wa_doc_material-matnr
           AND ( buzid = 'M' OR buzid = 'S' )
           AND shkzg   = 'H'."(‘H’).

        DELETE lt_bseg WHERE matnr  NE wa_doc_material-matnr AND
                           ( buzid  NE 'M' OR buzid NE 'S' ) AND
                             shkzg  NE 'H'."(‘H’).

*       IF sy-subrc = 0 AND lines( lt_bseg ) > 0.
*         MOVE-CORRESPONDING lt_bseg TO ti_bseg.
*         sy-dbcnt = lines( lt_bseg ).
*       ELSE.
*         sy-subrc = 4.
*         sy-dbcnt = 0.
*       ENDIF.
** <--- S4 Migration - 19/06/2023 - JS

      ENDIF.

      LOOP AT ti_bseg INTO wa_bseg.
        yt_geo_oubound_ok-dmbtr = wa_bseg-dmbtr .
        yt_geo_oubound_ok-dmbe2 = wa_bseg-dmbe2 .
        yt_geo_oubound_ok-dmbe3 = wa_bseg-dmbe3 .
      ENDLOOP.

    ENDIF.

    APPEND yt_geo_oubound_ok.
  ENDLOOP.

  IF NOT yt_geo_oubound_ok[] IS INITIAL.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_MM_OUTBOUND_BOL_SUCESS'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        TABLES
          return_sucess = yt_geo_oubound_ok.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          return_sucess = yt_geo_oubound_ok.
    ENDIF.

  ENDIF.

  COMMIT WORK.

ENDFORM.

*&---------------------------------------------------------------------*
*& processar dados
*&---------------------------------------------------------------------*
FORM f_exibe_envio.

  l_program                  = sy-repid.
  l_grid_title               = 'Boletins enviados para GEO'.
  w_layout-expand_all        = abap_true.
  w_layout-colwidth_optimize = abap_true.

  IF yt_geo_oubound_ok[] IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = l_program
        i_structure_name       = 'ZMME_RETURN_SUCESS'
      CHANGING
        ct_fieldcat            = t_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = l_program
        is_layout          = w_layout
        it_fieldcat        = t_fieldcat
        i_grid_title       = l_grid_title
*       i_screen_start_column = 10
*       i_screen_start_line   = 02
*       i_screen_end_column   = 182
*       i_screen_end_line  = 20
      TABLES
        t_outtab           = yt_geo_oubound_ok
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
  ENDIF.

  IF yt_log_mfpf[] IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = l_program
        i_structure_name       = 'ZFIE_RET_DOCUMENT'
      CHANGING
        ct_fieldcat            = t_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = l_program
        is_layout          = w_layout
        it_fieldcat        = t_fieldcat
        i_grid_title       = l_grid_title
*       i_screen_start_column = 10
*       i_screen_start_line   = 02
*       i_screen_end_column   = 182
*       i_screen_end_line  = 20
      TABLES
        t_outtab           = yt_log_mfpf
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
  ENDIF.

ENDFORM.

FORM f_processa_obm.

  l_text_m02 = 'Confirmação &1 - Documento de material &2 / &3 gerado com sucesso.'.

  t_ximfbf[] = t_ximfbf_obm[].

  FREE: yt_execretrn, yt_log_mfpf.

  LOOP AT t_ximfbf INTO w_ximfbf.

    CLEAR: yt_execretrn, yt_log_mfpf.

    CHECK w_ximfbf-mblnr IS NOT INITIAL.

    SELECT SINGLE smbln
      FROM mseg
      INTO @DATA(_smbln)
     WHERE smbln = @w_ximfbf-mblnr.

    CHECK sy-subrc <> 0.

    yt_execretrn-type    = 'S'.
    yt_execretrn-id      = 'Z01'.
    yt_execretrn-number  = '000'.
    yt_execretrn-message = l_text_m02.
    yt_execretrn-message_v1 = w_ximfbf-confirmation.
    yt_execretrn-message_v2 = w_ximfbf-mblnr.
    yt_execretrn-message_v3 = w_ximfbf-dtmvto(4).

    REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
            WITH w_ximfbf-confirmation.
    REPLACE FIRST OCCURRENCE OF '&2' IN yt_execretrn-message
            WITH yt_execretrn-message_v2.
    REPLACE FIRST OCCURRENCE OF '&3' IN yt_execretrn-message
            WITH yt_execretrn-message_v3.

    MOVE: w_ximfbf-obj_key            TO yt_log_mfpf-obj_key,
          sy-datum                    TO yt_log_mfpf-dt_atualizacao,
          sy-uzeit                    TO yt_log_mfpf-hr_atualizacao.

    IF w_ximfbf-zst_atlz = 'I'.
      MOVE  '09'                      TO yt_log_mfpf-interface.
    ELSE.
      MOVE  '27'                      TO yt_log_mfpf-interface.
    ENDIF.

    MOVE-CORRESPONDING yt_execretrn TO yt_log_mfpf.
    MOVE: yt_execretrn-number       TO yt_log_mfpf-num.
    MOVE 'ZPPR0030'                 TO yt_log_mfpf-info_adicional_1.

    APPEND yt_log_mfpf.
  ENDLOOP.

  IF NOT yt_log_mfpf[] IS INITIAL.

    CONSTANTS: cc_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.
    DATA: lv_rfc TYPE rfcdest.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = cc_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION cc_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = yt_log_mfpf.
    ELSE.
      CALL FUNCTION cc_fm IN BACKGROUND TASK
        TABLES
          outreturn = yt_log_mfpf.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
  ENDIF.

  COMMIT WORK.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
