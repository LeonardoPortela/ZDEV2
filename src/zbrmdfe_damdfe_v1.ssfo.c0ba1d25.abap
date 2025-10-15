    "BREAK-POINT 'LPORTELA'.
* Dados MDF-e
    DATA: wa_zsdt0102 TYPE zsdt0102.
    DATA: vl_ds_url TYPE agr_url.

    CLEAR: g_valida_selo.
    SELECT SINGLE *
      INTO wa_zsdt0102
      FROM zsdt0102
     WHERE docnum = i_dados_mdfe-docnum.

    IF sy-subrc IS INITIAL.
      IF wa_zsdt0102-encerrado EQ abap_true.
        g_valida_selo = abap_true.
        g_doc_encerra = 'DOC_ENCER22'.
*       G_DOC_ENCERRA = 'DOC_ENCER8'.
      ENDIF.
      IF wa_zsdt0102-contingencia EQ abap_true.
        g_valida_selo = abap_true.
        g_doc_encerra = 'CONTINGENCIA'.
      ENDIF.

    ENDIF.







