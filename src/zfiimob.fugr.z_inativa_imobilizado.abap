FUNCTION z_inativa_imobilizado.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  ANLA-BUKRS
*"     REFERENCE(I_ANLN1) TYPE  ANLA-ANLN1
*"     REFERENCE(I_ANLN2) TYPE  ANLA-ANLN2 DEFAULT '0000'
*"     REFERENCE(I_DATE) TYPE  SY-DATUM
*"  EXPORTING
*"     REFERENCE(E_RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  IF i_bukrs IS NOT INITIAL AND i_anln1 IS NOT INITIAL AND i_date IS NOT INITIAL.

    DATA: empresa     TYPE anla-bukrs,
          imobilizado TYPE anla-anln1,
          subnr       TYPE anla-anln2,
          dt          TYPE sy-datum,
          wa_anla     TYPE anla.

    empresa = i_bukrs.
    imobilizado = i_anln1.
    subnr = i_anln2.
    dt = i_date.

    IF subnr IS INITIAL.
      subnr = '0000'.
    ENDIF.

    SELECT SINGLE * FROM anla WHERE bukrs = @empresa AND anln1 = @imobilizado AND anln2 = @subnr INTO @wa_anla.

    IF sy-subrc = 0.
      DATA:
        lt_return_all	        TYPE TABLE OF bapiret2, "
        lv_documentreference  TYPE bapifapo_doc_ref, "
        lv_origindocreference	TYPE bapifapo_doc_ref, "
        lv_return	            TYPE bapiret2, "
        lv_generalpostingdata	TYPE bapifapo_gen_info, "
        lv_retirementdata	    TYPE bapifapo_ret, "
        lv_accountassignments	TYPE bapifapo_acc_ass, "
        lv_furtherpostingdata	TYPE bapifapo_add_info. "

      lv_generalpostingdata-username = sy-uname.
      lv_generalpostingdata-doc_type = 'AA'.
      lv_generalpostingdata-doc_date = dt.
      lv_generalpostingdata-pstng_date = dt.
      lv_generalpostingdata-fis_period = dt+4(2).
      lv_generalpostingdata-trans_date = dt.
      lv_generalpostingdata-comp_code = wa_anla-bukrs.
      lv_generalpostingdata-assetmaino = wa_anla-anln1.
      lv_generalpostingdata-assetsubno = wa_anla-anln2.
      lv_generalpostingdata-assettrtyp = abap_false.
      lv_generalpostingdata-depr_area = abap_false.
      lv_generalpostingdata-ledger_group = abap_false.
      lv_generalpostingdata-acc_principle = abap_false.


      lv_retirementdata-valuedate = dt.

      CALL FUNCTION 'BAPI_ASSET_RETIREMENT_POST'  "Post asset retirement
        EXPORTING
          origindocreference = lv_origindocreference
          generalpostingdata = lv_generalpostingdata
          retirementdata     = lv_retirementdata
          accountassignments = lv_accountassignments
          furtherpostingdata = lv_furtherpostingdata
        IMPORTING
          documentreference  = lv_documentreference
          return             = lv_return
        TABLES
          return_all         = lt_return_all. " BAPI_ASSET_RETIREMENT_POST

      MOVE-CORRESPONDING lv_return TO e_return.

      IF sy-subrc = 0.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = space.

      ELSE.

      ENDIF.
    ELSE.
    ENDIF.
  ELSE.
  ENDIF.

ENDFUNCTION.
