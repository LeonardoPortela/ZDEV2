FUNCTION z_fi_manydocument_processa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_DOCUMENT STRUCTURE  ZFIE_DOCUMENT
*"      IT_HEADER STRUCTURE  ZFIE_DOCUMENTHEADER OPTIONAL
*"      IT_ITEM STRUCTURE  ZFIE_DOCUMENTITEM OPTIONAL
*"      IT_RETURN STRUCTURE  ZFIE_RETURN OPTIONAL
*"      IT_IMPOSTOS_RETIDOS STRUCTURE  ZIB_CONTABIL_IRT OPTIONAL
*"----------------------------------------------------------------------

  TABLES: zib_contabil_err.

  TYPES: BEGIN OF ty_bkpf_gen,
           belnr TYPE bkpf-belnr,
           bukrs TYPE bkpf-bukrs,
           gjahr TYPE bkpf-gjahr,
           budat TYPE bkpf-budat,
           kurs2 TYPE bkpf-kurs2,
           cpudt TYPE bkpf-cpudt,
         END OF ty_bkpf_gen.


  DATA: wa_header       LIKE zfie_documentheader,
        wa_item         LIKE zfie_documentitem,
        wa_aux          LIKE zfie_document,
        wa_return       LIKE zfie_return,
        wa_ret_document TYPE zfie_ret_document,
        nr_item         LIKE zib_contabil_err-nr_item.

  DATA: vg_obj_key     LIKE zib_contabil-obj_key,
        wa_document    LIKE zfie_document,
        vg_obj_xrt(30),
        ax_obj_xrt(30),
        ax_obj_key     LIKE zib_contabil-obj_key,
        xr_obj_key     LIKE zib_contabil-obj_key,
        vg_xrt         TYPE i,
        tg_xrt(3).

  DATA: it_erro_contabil LIKE STANDARD TABLE OF wa_ret_document,
        it_ret_document  LIKE STANDARD TABLE OF wa_ret_document.

  REFRESH: it_header, it_item, it_return.
  CLEAR  : wa_header, wa_item.

  CLEAR vg_seqlan.

  SORT it_document BY obj_key seqitem.

  CLEAR vg_obj_key.

  LOOP AT it_document INTO wa_document.
    IF wa_document-obj_key NE vg_obj_key.
      vg_obj_key = wa_document-obj_key.
      UPDATE zib_contabil
         SET rg_atualizado = 'S'
       WHERE rg_atualizado = 'N'
         AND obj_key EQ vg_obj_key.
    ENDIF.
  ENDLOOP.

  COMMIT WORK.

  LOOP AT it_document INTO wa_document.

    wa_aux = wa_document.

    IF wa_document-bktxt EQ 'XRT'.
      CONCATENATE wa_document-obj_key wa_document-xref1 INTO ax_obj_xrt.
      ax_obj_key = wa_document-obj_key.

      IF vg_obj_xrt NE ax_obj_xrt.
        IF ax_obj_key NE xr_obj_key.
          vg_xrt = 0.
        ENDIF.
        vg_xrt = vg_xrt + 1.
        CONCATENATE wa_document-obj_key wa_document-xref1 INTO vg_obj_xrt.
        xr_obj_key = wa_document-obj_key.
        CLEAR wa_header.
        vg_seqlan = vg_seqlan + 1.
        wa_header-seqlan = vg_seqlan.
        MOVE-CORRESPONDING wa_aux TO wa_header.
        tg_xrt = vg_xrt.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = tg_xrt
          IMPORTING
            output = tg_xrt.

        SHIFT wa_header-obj_key LEFT DELETING LEADING ' '.
        SHIFT tg_xrt LEFT DELETING LEADING ' '.

        CONCATENATE wa_header-obj_key tg_xrt INTO wa_header-obj_key.

        IF ( NOT wa_aux-bldat IS INITIAL ).
          DATA(_error) = abap_false.
          PERFORM f_check_data TABLES it_return
                                USING wa_header-seqlan
                                      wa_aux-bldat
                                      abap_true
                             CHANGING _error.

          IF _error = abap_false.
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
              EXPORTING
                date_external = wa_aux-bldat
              IMPORTING
                date_internal = wa_header-bldat.
          ENDIF.
        ENDIF.

        IF ( NOT wa_aux-budat IS INITIAL ).

          PERFORM f_check_data TABLES it_return
                                USING wa_header-seqlan
                                      wa_aux-budat
                                      abap_true
                             CHANGING _error.

          IF _error = abap_false.
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
              EXPORTING
                date_external = wa_aux-budat
              IMPORTING
                date_internal = wa_header-budat.
          ENDIF.

        ENDIF.
        wa_header-xblnr     = wa_document-kidno.
        APPEND wa_header TO it_header.
      ENDIF.

    ELSE.

      AT NEW obj_key.
        CLEAR wa_header.
        vg_seqlan = vg_seqlan + 1.
        wa_header-seqlan = vg_seqlan.
        MOVE-CORRESPONDING wa_aux TO wa_header.

        IF ( NOT wa_aux-bldat IS INITIAL ).

          PERFORM f_check_data TABLES it_return
                                USING wa_header-seqlan
                                      wa_aux-bldat
                                      abap_true
                            CHANGING _error.

          IF _error = abap_false.
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
              EXPORTING
                date_external = wa_aux-bldat
              IMPORTING
                date_internal = wa_header-bldat.
          ENDIF.
        ENDIF.

        IF ( NOT wa_aux-budat IS INITIAL ).

          PERFORM f_check_data TABLES it_return
                                USING wa_header-seqlan
                                      wa_aux-budat
                                      abap_true
                            CHANGING _error.

          IF _error = abap_false.
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
              EXPORTING
                date_external = wa_aux-budat
              IMPORTING
                date_internal = wa_header-budat.
          ENDIF.
        ENDIF.
        APPEND wa_header TO it_header.
      ENDAT.
    ENDIF.

    CLEAR wa_item.
    wa_item-seqlan = vg_seqlan.
    MOVE-CORRESPONDING wa_aux TO wa_item.

    IF ( NOT wa_aux-zfbdt IS INITIAL ).

      PERFORM f_check_data TABLES it_return
                            USING wa_header-seqlan
                                  wa_aux-zfbdt
                                  abap_true
                        CHANGING _error.

      IF _error = abap_false.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            date_external = wa_aux-zfbdt
          IMPORTING
            date_internal = wa_item-zfbdt.
      ENDIF.
    ENDIF.

    APPEND wa_item TO it_item.

  ENDLOOP.

  CALL FUNCTION 'Z_FI_DOCUMENT_POST'
    TABLES
      it_documentheader   = it_header
      it_documentitem     = it_item
      it_return           = it_return
      it_impostos_retidos = it_impostos_retidos
    EXCEPTIONS
      bschl_not_found     = 1
      OTHERS              = 2.

  IF sy-subrc <> 0.
    CLEAR wa_return.
    it_return-type       = sy-msgty.
    it_return-id         = sy-msgid.
    it_return-number     = sy-msgno.
    it_return-message_v1 = sy-msgv1.
    it_return-message_v2 = sy-msgv2.
    it_return-message_v3 = sy-msgv3.
    it_return-message_v4 = sy-msgv4.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO it_return-message.
    APPEND it_return.
  ENDIF.

  REFRESH: it_ret_document,
           it_erro_contabil.

  SORT it_return BY seqlan type.
  SORT it_header BY seqlan.

  vg_datum = sy-datum.
  vg_uzeit = sy-uzeit.

  LOOP AT it_header INTO wa_header.

    DELETE FROM zib_contabil_err
          WHERE obj_key EQ wa_header-obj_key.

    LOOP AT it_return INTO wa_return
                     WHERE ( seqlan EQ wa_header-seqlan ).
      grava_return.
*** 19.10.2023 - CSB - Inicio
* A = termination message (abort)
      IF ( wa_return-type EQ 'E' ) OR ( wa_return-type EQ 'A' ).
        APPEND wa_ret_document  TO it_erro_contabil.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

*   Grava erro de processamento, Elimina duplicidades de mensagens
*   Caso crédito e débito gerem a mesma informação.
  IF NOT it_erro_contabil[] IS INITIAL.

    SORT it_erro_contabil BY message.
    DELETE ADJACENT DUPLICATES FROM it_erro_contabil COMPARING message.

    CLEAR: nr_item.
    LOOP AT it_erro_contabil INTO wa_ret_document.
      ADD 1 TO nr_item.
      MOVE-CORRESPONDING wa_ret_document TO zib_contabil_err.
      zib_contabil_err-nr_item = nr_item.
      MODIFY zib_contabil_err.
    ENDLOOP.

  ENDIF.

*> Chama função assincrona de retorno, confirmando a criação ou não do
*> documento.
  DELETE it_ret_document WHERE type = 'S' AND id = 'Z01' AND num = '003' AND message_v1 = ' '.
  IF NOT it_ret_document IS INITIAL.


    "Obtem Informações Adicionais - Ini
    DATA: lwa_dados_add             TYPE zsfi0001,
          lva_json_dados_add(30000) TYPE c,  "TYPE char30000  "*-Equalização RISE x PRD - 19.07.2023 - JT
          lit_bkpf                  TYPE TABLE OF ty_bkpf_gen.

    DATA(_ret_document_aux) = it_ret_document[].
    DELETE _ret_document_aux WHERE type NE 'S'.
    DELETE _ret_document_aux WHERE message_v1 IS INITIAL.

    IF _ret_document_aux[] IS NOT INITIAL.

      SELECT belnr bukrs gjahr budat kurs2 cpudt
         FROM bkpf INTO TABLE lit_bkpf
         FOR ALL ENTRIES IN _ret_document_aux
        WHERE awkey EQ _ret_document_aux-obj_key.

      LOOP AT it_ret_document ASSIGNING FIELD-SYMBOL(<fs_ret_document>) WHERE type EQ 'S' AND message_v1 IS NOT INITIAL.

        CLEAR: lwa_dados_add.

        READ TABLE lit_bkpf INTO DATA(lwa_bkpf) WITH KEY bukrs = <fs_ret_document>-message_v2(04)
                                                         belnr = <fs_ret_document>-message_v1(10)
                                                         gjahr = <fs_ret_document>-message_v3(04).
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING lwa_bkpf TO lwa_dados_add.

          lva_json_dados_add  = /ui2/cl_json=>serialize( EXPORTING data = lwa_dados_add ).

          <fs_ret_document>-info_adicional_1 = lva_json_dados_add+0000(4000).
          <fs_ret_document>-info_adicional_2 = lva_json_dados_add+4000(4000).
          <fs_ret_document>-info_adicional_3 = lva_json_dados_add+8000(4000).
        ENDIF.
      ENDLOOP.
    ENDIF.
    "Obtem Informações Adicionais - Fim


* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      AS SEPARATE UNIT
*      TABLES
*        outreturn = it_ret_document.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

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
        AS SEPARATE UNIT
        TABLES
          outreturn = it_ret_document.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = it_ret_document.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim

  ENDIF.

  COMMIT WORK.


ENDFUNCTION.
