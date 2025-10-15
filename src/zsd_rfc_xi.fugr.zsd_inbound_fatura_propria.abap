FUNCTION zsd_inbound_fatura_propria.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IB_FATURA_PROPRIA STRUCTURE  ZSDYT0049
*"----------------------------------------------------------------------

  DATA: it_zsdyt0049 LIKE STANDARD TABLE OF zsdyt0049,
        wa_zsdyt0049 LIKE zsdyt0049,
        index        TYPE sy-tabix,
        wa_outreturn TYPE zfie_ret_document,
        it_outreturn TYPE TABLE OF zfie_ret_document.

  DATA: wa_ib_fatura_propria TYPE zsdyt0049,
        vl_docfat            TYPE zsdyt0049-doc_fat.

  CLEAR: it_zsdyt0049[], wa_zsdyt0049.

  IF NOT ( ib_fatura_propria[] IS INITIAL ).

    DATA(lt_fatura) = ib_fatura_propria[].
    SORT lt_fatura BY obj_key.
    DELETE ADJACENT DUPLICATES FROM lt_fatura COMPARING obj_key.

    SELECT *
      FROM zob_mensagem
      INTO TABLE @DATA(lt_ob_msg)
      FOR ALL ENTRIES IN @lt_fatura
      WHERE obj_key = @lt_fatura-obj_key.
    IF sy-subrc IS INITIAL.
      SORT lt_ob_msg BY obj_key.
    ENDIF.

    LOOP AT ib_fatura_propria INTO wa_ib_fatura_propria.

      READ TABLE lt_ob_msg ASSIGNING FIELD-SYMBOL(<fs_ob_msg>)
      WITH KEY obj_key = wa_ib_fatura_propria-obj_key
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        wa_outreturn-obj_key        = <fs_ob_msg>-obj_key.
        wa_outreturn-interface      = <fs_ob_msg>-interface.
        wa_outreturn-dt_atualizacao = sy-datum.
        wa_outreturn-hr_atualizacao = sy-uzeit.
        wa_outreturn-type           = <fs_ob_msg>-type.
        wa_outreturn-id             = <fs_ob_msg>-id.
        wa_outreturn-num            = '899'.
        wa_outreturn-message        = <fs_ob_msg>-message.

        IF <fs_ob_msg>-message_v1 IS NOT INITIAL."Ordem
          wa_outreturn-message_v1 = <fs_ob_msg>-message_v1.
        ENDIF.

        IF <fs_ob_msg>-message_v2 IS NOT INITIAL."Fatura
          wa_outreturn-message_v2 = <fs_ob_msg>-message_v2.
        ENDIF.

        IF <fs_ob_msg>-message_v3 IS NOT INITIAL."Fatura Eletronica
          wa_outreturn-message_v3 = <fs_ob_msg>-message_v3.
        ENDIF.

        IF  <fs_ob_msg>-message_v4 IS NOT INITIAL."Valor Fatura
          wa_outreturn-message_v4 = <fs_ob_msg>-message_v4.
        ENDIF.

        APPEND wa_outreturn TO it_outreturn.
        CLEAR wa_outreturn.

      ELSE.

        CLEAR : vl_docfat.

        SELECT SINGLE doc_fat
          FROM zsdyt0049
          INTO vl_docfat
         WHERE obj_key = wa_ib_fatura_propria-obj_key.

        IF vl_docfat IS INITIAL.

          IF wa_ib_fatura_propria-vr_base_iva < 0.
            wa_ib_fatura_propria-vr_base_iva  = ( wa_ib_fatura_propria-vr_base_iva * -1 ).
          ENDIF.

        ENDIF.

        wa_ib_fatura_propria-rg_pdf_fatura = 'N'.
        wa_ib_fatura_propria-rg_ctb_fatura = 'N'.

        APPEND wa_ib_fatura_propria TO it_zsdyt0049.
        CLEAR  wa_ib_fatura_propria.


      ENDIF.

    ENDLOOP.

    IF it_outreturn[] IS NOT INITIAL.

      CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
        TABLES
          outreturn = it_outreturn[].

      COMMIT WORK.

    ENDIF.

    CHECK NOT it_zsdyt0049[] IS INITIAL.
    MODIFY zsdyt0049 FROM TABLE it_zsdyt0049.

    COMMIT WORK.

  ENDIF.
ENDFUNCTION.
