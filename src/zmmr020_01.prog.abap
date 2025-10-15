*&---------------------------------------------------------------------*
*& Report  ZMMR020_01
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr020_01.

*FORM retorno_bkpf_original
*  USING p_inv_doc_no TYPE re_belnr
*        p_fisc_year  TYPE gjahr
*  CHANGING wa_bseg_ori TYPE bseg.
*
*  "ALRS
*  " busca texto do item do documento contabil
*
*  DATA(vawkey) = |{ p_inv_doc_no } { p_fisc_year }|.
*  CONDENSE vawkey NO-GAPS.
*  SELECT SINGLE * FROM bkpf INTO @DATA(wa_bkpf_ori)
*         WHERE awkey = @vawkey
*         AND   blart = 'ZG'.
*  IF sy-subrc = 0.
*
*    SELECT SINGLE * FROM bseg INTO @wa_bseg_ori
*        WHERE bukrs = @wa_bkpf_ori-bukrs AND
*              belnr = @wa_bkpf_ori-belnr AND
*              gjahr = @wa_bkpf_ori-gjahr AND
*              bschl = '31'.
*  ENDIF.
*  "ALRS
*
*ENDFORM.

FORM limpa_ref_miro_fiscal
    USING p_inv_doc_no TYPE re_belnr
        p_fisc_year  TYPE gjahr.

  CALL FUNCTION 'ZGRC_LIMPA_REF_MIRO_FISCAL'
    EXPORTING
      invoicedocnumber = p_inv_doc_no
      fiscalyear       = p_fisc_year.

ENDFORM.

*FORM troca_texto_doc_contabil
*  USING p_inv_doc_no TYPE re_belnr
*        p_fisc_year  TYPE gjahr
*        wa_bseg_ori TYPE bseg.
*
*  DATA: i_bseg TYPE bseg OCCURS 1,
*        i_bkpf TYPE bkpf OCCURS 1,
*        i_bsec TYPE bsec OCCURS 1,       " int. table for bsec
*        i_bsed TYPE bsed OCCURS 1,       " int. table for bsed
*        i_bset TYPE bset OCCURS 1,       " int. table for bset
*        i_bkdf TYPE bkdf OCCURS 1.       " int. table for bkdf
*
*  DATA(vawkey2) = |{ p_inv_doc_no } { p_fisc_year }|.
*  CONDENSE vawkey2 NO-GAPS.
*  SELECT SINGLE * FROM bkpf INTO @DATA(wa_bkpf_est)
*       WHERE awkey = @vawkey2
*       AND   blart = 'ZG'.
*  IF sy-subrc = 0.
*    REFRESH: i_bkpf, i_bseg.
*    APPEND wa_bkpf_est TO i_bkpf.
*    SELECT * FROM bseg INTO TABLE  i_bseg
*        WHERE bukrs = wa_bkpf_est-bukrs AND
*              belnr = wa_bkpf_est-belnr AND
*              gjahr = wa_bkpf_est-gjahr AND
*              bschl = '21'.
*
*    IF wa_bseg_ori-sgtxt IS NOT INITIAL.
*      LOOP AT i_bseg INTO DATA(wa_bseg).
*        wa_bseg-sgtxt = wa_bseg_ori-sgtxt.
*        MODIFY i_bseg FROM wa_bseg.
*      ENDLOOP.
*    ENDIF.
*
*    CALL FUNCTION 'CHANGE_DOCUMENT'
*      TABLES
*        t_bkdf = i_bkdf
*        t_bkpf = i_bkpf
*        t_bsec = i_bsec
*        t_bsed = i_bsed
*        t_bseg = i_bseg
*        t_bset = i_bset.
*    IF sy-subrc = 0.
*      WAIT UP TO 1 SECONDS.
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
*      WAIT UP TO 1 SECONDS.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
