FUNCTION z_pp_inbound_mfbf.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_XI_MFBF TYPE  ZPPT_XIMFBF
*"----------------------------------------------------------------------
*" Histórico de modificações                                          "*
*" Data         :                                                     "*
*" Autor        :                                                     "*
*" Descrição    :                                                     "*
*" Versão       :                                                     "*
************************************************************************
*

* STEFANINI - IR225952 - RBRIBEIRO - INICIO

  INCLUDE zafl_macros.
**  ***initialize logger. It should be always on the top of the FUNCTION.
  /afl/log_init.

* STEFANINI - IR225952 - RBRIBEIRO - FIM

* INICIALIZAÇÕES
  "Tipos
  TYPES: BEGIN OF ty_doc_material,
           obj_key      TYPE zpps_ximfbf_log-obj_key,
           mblnr        TYPE zpps_ximfbf_log-mblnr,
           cd_safra     TYPE zpps_ximfbf_log-cd_safra,
           nrobol       TYPE zpps_ximfbf_log-nrobol,
           fgorigem     TYPE zpps_ximfbf_log-fgorigem,
           id_matgeo    TYPE zpps_ximfbf_log-id_matgeo,
           matnr        TYPE zpps_ximfbf_log-matnr,
           dtmvto       TYPE zpps_ximfbf_log-dtmvto,
           processado   TYPE zpps_ximfbf_log-processado,
           fg_tpmovto   TYPE zpps_ximfbf_log-fg_tpmovto,
           cd_ccusto    TYPE bseg-kostl,
           zst_atlz     TYPE zpps_ximfbf_log-zst_atlz,
           id_cotton    TYPE zpps_ximfbf_log-id_cotton,
           charg        TYPE zpps_ximfbf_log-charg,
           zst_atlz_2   TYPE zpps_ximfbf_log-zst_atlz,
           confirmation TYPE zpps_ximfbf_log-confirmation,  "US 126585-06.11.20023-JT-inicio
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
         END OF ty_bseg .


  "Work Areas
  DATA: wa_zpps_ximfbf_log TYPE zpps_ximfbf_log,
        wa_doc_material    TYPE ty_doc_material,
        wa_bkpf            TYPE ty_bkpf,
        wa_bseg            TYPE ty_bseg.
  DATA : lp_matnr   TYPE matnr,
         l_text_m02 TYPE string.

  "Tabelas Internas
  DATA: ti_doc_material   TYPE TABLE OF ty_doc_material,
        ti_bkpf           TYPE TABLE OF ty_bkpf,
        ti_bseg           TYPE TABLE OF ty_bseg,
        yt_log_mfpf       TYPE TABLE OF zfie_ret_document WITH HEADER LINE INITIAL SIZE 0,
        yt_geo_oubound_ok TYPE TABLE OF zmme_return_sucess WITH HEADER LINE INITIAL SIZE 0,
        yt_execretrn      TYPE TABLE OF ty_execretrn WITH HEADER LINE INITIAL SIZE 0.

  "Variaveis
  DATA: p_awkey TYPE bkpf-awkey,
        p_smbln TYPE mseg-smbln.

  "Projeto Restruturação Algodao - WPP - Ini
  LOOP AT t_xi_mfbf ASSIGNING FIELD-SYMBOL(<fs_xi_mfbf>).
    CASE <fs_xi_mfbf>-zst_atlz.
      WHEN 'I' OR "Entrada Produção Sigam
           'E' OR "Estorno Entrada Produção Sigam
           'X' OR "Entrada Produção Sigam - Origem Transf.
           'Y'.   "Estorno Entrada Produção Sigam - Origem Transf.
        <fs_xi_mfbf>-id_interface = 'S'.
      WHEN 'M' OR "Entrada combustivel - GEO
           'C'.   "Entrada Consumo     - GEO
        <fs_xi_mfbf>-id_interface = 'G'.
      WHEN OTHERS.
        <fs_xi_mfbf>-id_interface = 'G'. "GEO
    ENDCASE.
  ENDLOOP.

  DATA(t_xi_mfbf_ent_producao_sigam) = t_xi_mfbf[].

  DELETE t_xi_mfbf_ent_producao_sigam WHERE id_interface NE 'S'.
  DELETE t_xi_mfbf WHERE id_interface NE 'G'.

  "Chamada Função para recebimento de Registros de Entrada Produção Sigam
  CALL FUNCTION 'ZPP_INBOUND_ENTRADA_PRODUCAO'
    TABLES
      t_xi_mfbf = t_xi_mfbf_ent_producao_sigam.
  "Projeto Restruturação Algodao - WPP - Fim

  "Inicio
  SORT: t_xi_mfbf BY obj_key aufnr mblnr zst_atlz matnr.

  CLEAR: w_xi_mfbf,
         yt_log_mfpf[],
         wa_zpps_ximfbf_log.

  LOOP AT t_xi_mfbf INTO w_xi_mfbf.


    CLEAR: wa_doc_material.

    SELECT SINGLE mblnr processado charg matnr nrobol confirmation dtmvto
      INTO (wa_doc_material-mblnr,wa_doc_material-processado,wa_doc_material-charg,wa_doc_material-matnr,
            wa_doc_material-nrobol,wa_doc_material-confirmation,wa_doc_material-dtmvto) "US 126585-06.11.20023-JT-inicio
      FROM zpps_ximfbf_log
     WHERE obj_key = w_xi_mfbf-obj_key.

    IF sy-subrc IS INITIAL AND wa_doc_material-mblnr NE '' AND w_xi_mfbf-zst_atlz NE 'E' .

      SELECT SINGLE smbln
        FROM mseg
        INTO p_smbln
       WHERE smbln = wa_doc_material-mblnr.

      IF sy-subrc IS NOT INITIAL .
        wa_doc_material-obj_key    = w_xi_mfbf-obj_key  .
        wa_doc_material-cd_safra   = w_xi_mfbf-dtmvto(4) .
        wa_doc_material-nrobol     = w_xi_mfbf-nrobol   .
        wa_doc_material-fgorigem   = w_xi_mfbf-fgorigem .
        " wa_doc_material-id_matgeo  = w_xi_mfbf-id_matgeo.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = w_xi_mfbf-id_matgeo
          IMPORTING
            output = wa_doc_material-id_matgeo.


        wa_doc_material-dtmvto     = w_xi_mfbf-dtmvto.
        wa_doc_material-fg_tpmovto = w_xi_mfbf-fg_tpmovto.
        wa_doc_material-zst_atlz_2 = w_xi_mfbf-zst_atlz.
        wa_doc_material-zst_atlz   = wa_doc_material-zst_atlz.
        wa_doc_material-id_cotton  = w_xi_mfbf-id_cotton.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = w_xi_mfbf-cd_ccusto
          IMPORTING
            output = wa_doc_material-cd_ccusto.


*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = w_xi_mfbf-matnr
*          IMPORTING
*            output = wa_doc_material-matnr.
        wa_doc_material-matnr = w_xi_mfbf-matnr.

        APPEND wa_doc_material TO ti_doc_material.

      ELSE.
        IF wa_doc_material-processado NE 'P' ."--P = Em processamento
          MOVE-CORRESPONDING w_xi_mfbf TO wa_zpps_ximfbf_log.
          wa_zpps_ximfbf_log-mandt         = sy-mandt.
          wa_zpps_ximfbf_log-data          = sy-datum.
          wa_zpps_ximfbf_log-hora          = sy-uzeit.
          wa_zpps_ximfbf_log-zrg_atulizado = 'N'.
          wa_zpps_ximfbf_log-processado    = 'N'.
          IF w_xi_mfbf-zst_atlz = 'I'.
            wa_zpps_ximfbf_log-charg       = wa_doc_material-charg. "*-CS2022000332-#84404-02.08.2022-JT-inicio
            wa_zpps_ximfbf_log-matnr       = wa_doc_material-matnr. "*-CS2022000332-#84404-02.08.2022-JT-inicio

          ENDIF.
          MODIFY zpps_ximfbf_log FROM wa_zpps_ximfbf_log.

        ENDIF.
      ENDIF.
    ELSE.
      IF wa_doc_material-processado NE 'P' ."--P = Em processamento

*-CS2022000332-#84404-02.08.2022-JT-inicio
        IF w_xi_mfbf-zst_atlz = 'I' AND w_xi_mfbf-id_cotton IS NOT INITIAL. " AND
*          wa_doc_material-mblnr IS NOT INITIAL.
          SELECT SINGLE id_cotton
            INTO @DATA(_id_cotton)
            FROM zpps_ximfbf_log
           WHERE obj_key   = @w_xi_mfbf-obj_key
             AND id_cotton = @w_xi_mfbf-id_cotton
             AND zst_atlz  = 'I'.
        ELSE.
          sy-subrc = 4.
        ENDIF.

        IF sy-subrc <> 0.
          MOVE-CORRESPONDING w_xi_mfbf TO wa_zpps_ximfbf_log.
          wa_zpps_ximfbf_log-mandt         = sy-mandt.
          wa_zpps_ximfbf_log-data          = sy-datum.
          wa_zpps_ximfbf_log-hora          = sy-uzeit.
          wa_zpps_ximfbf_log-zrg_atulizado = 'N'.
          wa_zpps_ximfbf_log-processado    = 'N'.
          MODIFY zpps_ximfbf_log FROM wa_zpps_ximfbf_log.
*-CS2022000332-#84404-02.08.2022-JT-fim
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.


  "Retorna o log para o sigam caso o documento ja tenha sido gerado

  DELETE ADJACENT DUPLICATES FROM ti_doc_material.

  REFRESH :yt_geo_oubound_ok.
  LOOP AT ti_doc_material INTO wa_doc_material.

    REFRESH : ti_bseg, ti_bkpf.

    CLEAR:yt_geo_oubound_ok.

    CONCATENATE wa_doc_material-mblnr  wa_doc_material-cd_safra  INTO p_awkey.

    yt_geo_oubound_ok-idbol       = wa_doc_material-obj_key .
    yt_geo_oubound_ok-nrobol      = wa_doc_material-nrobol  .

    IF yt_geo_oubound_ok-nrobol IS INITIAL.
      yt_geo_oubound_ok-nrobol    = wa_doc_material-id_cotton.
    ENDIF.

    yt_geo_oubound_ok-fgorigem    = wa_doc_material-fgorigem.
    yt_geo_oubound_ok-bwart       = wa_doc_material-fg_tpmovto.
    yt_geo_oubound_ok-erzet       = sy-uzeit.
    "yt_geo_oubound_ok-id_matgeo   = wa_doc_material-id_matgeo  .

*        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*      EXPORTING
*        input  =  wa_doc_material-id_matgeo
*      IMPORTING
*        output = yt_geo_oubound_ok-id_matgeo
    yt_geo_oubound_ok-id_matgeo  = wa_doc_material-id_matgeo.
    CLEAR : lp_matnr.

    lp_matnr = wa_doc_material-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lp_matnr
      IMPORTING
        output = lp_matnr.

    yt_geo_oubound_ok-matnr       = lp_matnr .
    yt_geo_oubound_ok-docmat      = p_awkey .
    yt_geo_oubound_ok-dmbtr       = 0.
    yt_geo_oubound_ok-dmbe2       = 0.
    yt_geo_oubound_ok-dmbe3       = 0.
    yt_geo_oubound_ok-dtproc      = sy-datum.
    yt_geo_oubound_ok-hrproc      = sy-uzeit.
    yt_geo_oubound_ok-erdat       = wa_doc_material-dtmvto.

    SELECT belnr awkey bktxt budat
      INTO TABLE ti_bkpf
      FROM bkpf
     WHERE gjahr EQ wa_doc_material-cd_safra
       AND tcode IN ('MFBF','MB1A', 'MF41')
       AND awtyp = 'MKPF'
       AND blart = 'WA'
       AND awkey = p_awkey  .


    IF sy-subrc IS INITIAL.
      CLEAR:wa_bkpf.
      "READ TABLE TI_BKPF INTO WA_BKPF INDEX 1.

      IF wa_doc_material-zst_atlz = 'M' .
* ---> S4 Migration - 16/06/2023 - JS

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

        DATA lt_fields TYPE fagl_t_field.
        DATA lt_bseg TYPE TABLE OF bseg.
*
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

*        LT_FIELDS = value #( ( LINE = 'BELNR' )
*                             ( LINE = 'DMBTR' )
*                             ( LINE = 'DMBE2' )
*                             ( LINE = 'DMBE3' )
*                             ( LINE = 'MATNR' )
*                             ( LINE = 'WERKS' )
*                             ( LINE = 'MENGE' )
*                             ( LINE = 'BEWAR' )
*                             ).
*
*        call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
*          exporting
*            IT_FOR_ALL_ENTRIES = TI_BKPF
*            I_WHERE_CLAUSE     = |BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-CD_SAFRA|
*            IT_FIELDLIST       = LT_FIELDS
*          importing
*            ET_BSEG            = LT_BSEG
*          exceptions
*            NOT_FOUND          = 1.

        DELETE lt_bseg WHERE matnr  NE wa_doc_material-matnr AND
                           ( buzid  NE 'M' OR buzid NE 'S' ) AND
                             shkzg  NE 'H'."(‘H’).

        IF sy-subrc = 0 AND lines( lt_bseg ) > 0.
          MOVE-CORRESPONDING lt_bseg TO ti_bseg.
          sy-dbcnt = lines( lt_bseg ).
        ELSE.
          sy-subrc = 4.
          sy-dbcnt = 0.
        ENDIF.
** <--- S4 Migration - 19/06/2023 - JS

      ENDIF.

      LOOP AT ti_bseg INTO wa_bseg.
        yt_geo_oubound_ok-dmbtr = wa_bseg-dmbtr .
        yt_geo_oubound_ok-dmbe2 = wa_bseg-dmbe2 .
        yt_geo_oubound_ok-dmbe3 = wa_bseg-dmbe3 .
      ENDLOOP.

    ENDIF.

    APPEND yt_geo_oubound_ok.

    "Monta Retorno Sigam
    IF wa_doc_material-zst_atlz_2 = 'I'.

*-US 126585-06.11.20023-JT-inicio
      IF wa_doc_material-nrobol IS INITIAL.
        l_text_m02              = 'Confirmação &1 - Documento de material &2 / &3 gerado com sucesso.'.
        yt_execretrn-type       = 'S'.
        yt_execretrn-id         = 'Z01'.
        yt_execretrn-number     = '000'.
        yt_execretrn-message    = l_text_m02.
        yt_execretrn-message_v1 = wa_doc_material-confirmation.
        yt_execretrn-message_v2 = wa_doc_material-mblnr.
        yt_execretrn-message_v3 = wa_doc_material-dtmvto(4).

        REPLACE FIRST OCCURRENCE OF '&1' IN yt_execretrn-message
                WITH wa_doc_material-confirmation.
        REPLACE FIRST OCCURRENCE OF '&2' IN yt_execretrn-message
                WITH wa_doc_material-mblnr.
        REPLACE FIRST OCCURRENCE OF '&3' IN yt_execretrn-message
                WITH wa_doc_material-dtmvto(4).

        MOVE-CORRESPONDING yt_execretrn TO yt_log_mfpf.
        MOVE: yt_execretrn-number       TO yt_log_mfpf-num.

        yt_log_mfpf-obj_key          = wa_doc_material-obj_key.
        yt_log_mfpf-dt_atualizacao   = sy-datum.
        yt_log_mfpf-hr_atualizacao   = sy-uzeit.
        yt_log_mfpf-interface        = '09'.
        " yt_log_mfpf-type             = 'E'.
        yt_log_mfpf-type             = 'S'.
        yt_log_mfpf-id               = 'Z01'.
        yt_log_mfpf-num              = '000'.
        yt_log_mfpf-info_adicional_1 = 'Z_PP_INBOUND_MFBF'.
        APPEND yt_log_mfpf.
      ELSE.
*-US 126585-06.11.20023-JT-fim

        yt_log_mfpf-obj_key          = wa_doc_material-obj_key.
        yt_log_mfpf-dt_atualizacao   = sy-datum.
        yt_log_mfpf-hr_atualizacao   = sy-uzeit.
        yt_log_mfpf-interface        = '09'.
        yt_log_mfpf-type             = 'E'.
        yt_log_mfpf-id               = 'Z01'.
        yt_log_mfpf-num              = '000'.

        CONCATENATE 'Documento' wa_doc_material-mblnr 'já gerado anteriormente!' INTO yt_log_mfpf-message SEPARATED BY space.
        yt_log_mfpf-info_adicional_1 = 'Z_PP_INBOUND_MFBF-ELSE'.


        APPEND yt_log_mfpf.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF NOT yt_geo_oubound_ok[] IS INITIAL.

*    CALL FUNCTION 'Z_MM_GRAVA_BOL_SUCESS'
*      TABLES
*        RETURN_SUCESS = YT_GEO_OUBOUND_OK.

*--> 25.08.2023 15:54:16 - Migração S4 – ML - Início
*    call function 'Z_MM_OUTBOUND_BOL_SUCESS' in background task
*      destination 'XI_GEO_RETURN_SUCESS'
*      tables
*        RETURN_SUCESS = YT_GEO_OUBOUND_OK.

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
*<-- 25.08.2023 15:54:16 - Migração S4 – ML – Fim

  ENDIF.

  "US 107592 - Bloqueio Duplicidade - Ini
  IF yt_log_mfpf[] IS NOT INITIAL.
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    call function 'Z_FI_OUTBOUND_RETURN' in background task
*      destination 'XI_SIGAM_RETURN'
*      tables
*        OUTRETURN = YT_LOG_MFPF.

    CONSTANTS: cc_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

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
  "US 107592 - Bloqueio Duplicidade - Fim


  COMMIT WORK.
* STEFANINI - IR225952 - RBRIBEIRO - INICIO

**save logs. It should be always on the bottom of the FUNCTION.
  /afl/save.

* STEFANINI - IR225952 - RBRIBEIRO - FIM

ENDFUNCTION.
