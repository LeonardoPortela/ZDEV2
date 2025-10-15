FUNCTION ZMAP2I_B2017_GM_HEAD_01_TO_IMK.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(BAPI2017_GM_HEAD_01) LIKE  BAPI2017_GM_HEAD_01
*"  STRUCTURE  BAPI2017_GM_HEAD_01
*"  CHANGING
*"     REFERENCE(IMKPF) LIKE  IMKPF STRUCTURE  IMKPF
*"--------------------------------------------------------------------

* This function module was generated. Don't change it manually!
* <VERSION>|4
* <BAPI_STRUCTURE>|BAPI2017_GM_HEAD_01
* <SAP_STRUCTURE>|IMKPF
* <INTERN_TO_EXTERN>|
* <APPEND FORM>|

* <BAPI_FIELD>|PR_UNAME
* <SAP_FIELD>|PR_UNAME
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_head_01-pr_uname
    TO imkpf-pr_uname                                               .

* <BAPI_FIELD>|GR_GI_SLIP_NO
* <SAP_FIELD>|XABLN
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_head_01-gr_gi_slip_no
    TO imkpf-xabln                                                  .

* <BAPI_FIELD>|BILL_OF_LADING
* <SAP_FIELD>|FRBNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_head_01-bill_of_lading
    TO imkpf-frbnr                                                  .

* <BAPI_FIELD>|REF_DOC_NO
* <SAP_FIELD>|XBLNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_head_01-ref_doc_no
    TO imkpf-xblnr                                                  .

* <BAPI_FIELD>|DOC_DATE
* <SAP_FIELD>|BLDAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_head_01-doc_date
    TO imkpf-bldat                                                  .

* <BAPI_FIELD>|PSTNG_DATE
* <SAP_FIELD>|BUDAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_head_01-pstng_date
    TO imkpf-budat                                                  .

* <BAPI_FIELD>|HEADER_TXT
* <SAP_FIELD>|BKTXT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_head_01-header_txt
    TO imkpf-bktxt.

* <BAPI_FIELD>|BAR_CODE
* <SAP_FIELD>|BAR_CODE
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE bapi2017_gm_head_01-bar_code
    TO imkpf-bar_code.

*                           begin of note 356665
* Version for printing of GR/GI slip
  MOVE bapi2017_gm_head_01-ver_gr_gi_slip
    TO imkpf-wever.

  MOVE bapi2017_gm_head_01-ver_gr_gi_slipx
    TO imkpf-weverx.
*Control posting for external WMS
  MOVE bapi2017_gm_head_01-ext_wms
    TO imkpf-bfwms.
*                           end of note 356665

* <BAPI_FIELD>|REF_DOC_NO / REF_DOC_NO_LONG
* <SAP_FIELD>|XBLNR
* <CODE_PART>|
* <ADD_FIELD>|

* IS-PS: special logic because the length of domain XBLNR1 could be 35
  CALL FUNCTION 'REF_DOC_NO_CONVERSION_INBOUND'
    EXPORTING
      i_ref_doc_no      = bapi2017_gm_head_01-ref_doc_no
      i_ref_doc_no_long = bapi2017_gm_head_01-ref_doc_no_long
    IMPORTING
      e_ref_doc_no_long = imkpf-xblnr.

* <BAPI_FIELD>|BILL_OF_LADING / BILL_OF_LADING_LONG
* <SAP_FIELD>|FRBNR
* <CODE_PART>|
* <ADD_FIELD>|

* IS-PS: special logic because the length of domain XBLNR1 could be 35
  CALL FUNCTION 'REF_DOC_NO_CONVERSION_INBOUND'
    EXPORTING
      i_ref_doc_no      = bapi2017_gm_head_01-bill_of_lading
      i_ref_doc_no_long = bapi2017_gm_head_01-bill_of_lading_long
    IMPORTING
      e_ref_doc_no_long = imkpf-frbnr.


ENDFUNCTION.
