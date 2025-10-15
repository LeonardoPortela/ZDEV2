*&---------------------------------------------------------------------*
*& Report  ZMMR019_05
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr019_06.

FORM troca_texto_graos_est  USING   p_invoicedocnumber    TYPE bapi_incinv_fld
                                    p_number_miro_est  TYPE bapi_incinv_fld-inv_doc_no.

  DATA: i_bseg TYPE bseg OCCURS 1,
        i_bkpf TYPE bkpf OCCURS 1,
        i_bsec TYPE bsec OCCURS 1,       " int. table for bsec
        i_bsed TYPE bsed OCCURS 1,       " int. table for bsed
        i_bset TYPE bset OCCURS 1,       " int. table for bset
        i_bkdf TYPE bkdf OCCURS 1.       " int. table for bkdf

  DATA(vawkey) = |{ p_invoicedocnumber-inv_doc_no } { p_invoicedocnumber-fisc_year }|.
  CONDENSE vawkey NO-GAPS.
  SELECT SINGLE * FROM bkpf INTO @DATA(wa_bkpf_ori)
         WHERE awkey = @vawkey
         AND   blart = 'ZG'.
  IF sy-subrc = 0.
* ---> S4 Migration - 19/06/2023 - MA
*    SELECT SINGLE * FROM bseg INTO @DATA(wa_bseg_ori)
*        WHERE bukrs = @wa_bkpf_ori-bukrs AND
*              belnr = @wa_bkpf_ori-belnr AND
*              gjahr = @wa_bkpf_ori-gjahr AND
*              bschl = '31'.

    DATA: lt_bseg     TYPE fagl_t_bseg,
          wa_bseg_ori TYPE bseg.

    CALL FUNCTION 'FAGL_GET_BSEG'
      EXPORTING
        i_bukrs   = wa_bkpf_ori-bukrs
        i_belnr   = wa_bkpf_ori-belnr
        i_gjahr   = wa_bkpf_ori-gjahr
      IMPORTING
        et_bseg   = lt_bseg
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    DELETE lt_bseg WHERE bschl NE '31'..

    READ TABLE lt_bseg INTO DATA(ls_bseg) INDEX 1.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_bseg TO wa_bseg_ori.
    ENDIF.
*<--- S4 Migration - 19/06/2023 - MA

    DATA(vawkey2) = |{ p_number_miro_est } { p_invoicedocnumber-fisc_year }|.
    CONDENSE vawkey2 NO-GAPS.
    SELECT SINGLE * FROM bkpf INTO @DATA(wa_bkpf_est)
         WHERE awkey = @vawkey2
         AND   blart = 'ZG'.
    IF sy-subrc = 0.
      REFRESH: i_bkpf, i_bseg.
      APPEND wa_bkpf_est TO i_bkpf.
      DATA rldnr_l41c6r6374 TYPE rldnr.
      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          e_rldnr       = rldnr_l41c6r6374
        EXCEPTIONS
          not_found     = 1
          more_than_one = 2.
      IF sy-subrc = 0.
        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
          EXPORTING
            i_rldnr         = rldnr_l41c6r6374
            i_bukrs         = wa_bkpf_est-bukrs
            i_belnr         = wa_bkpf_est-belnr
            i_gjahr         = wa_bkpf_est-gjahr
            it_where_clause = VALUE tt_rsdswhere( ( |BSCHL = '21'| ) )
          IMPORTING
            et_bseg         = i_bseg
          EXCEPTIONS
            not_found       = 1.
      ENDIF.
      IF sy-subrc <> 0 OR lines( i_bseg ) = 0.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ELSE.
        sy-dbcnt = lines( i_bseg ).
      ENDIF.


      IF wa_bseg_ori-sgtxt IS NOT INITIAL.
        LOOP AT i_bseg INTO DATA(wa_bseg).
          wa_bseg-sgtxt = wa_bseg_ori-sgtxt.
          MODIFY i_bseg FROM wa_bseg.
        ENDLOOP.
      ENDIF.

      CALL FUNCTION 'CHANGE_DOCUMENT'
        TABLES
          t_bkdf = i_bkdf
          t_bkpf = i_bkpf
          t_bsec = i_bsec
          t_bsed = i_bsed
          t_bseg = i_bseg
          t_bset = i_bset.
      IF sy-subrc = 0.
        WAIT UP TO 1 SECONDS.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.
