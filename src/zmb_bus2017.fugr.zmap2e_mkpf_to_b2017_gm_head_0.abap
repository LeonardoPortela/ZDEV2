FUNCTION ZMAP2E_MKPF_TO_B2017_GM_HEAD_0.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MKPF) LIKE  MKPF STRUCTURE  MKPF
*"  CHANGING
*"     REFERENCE(BAPI2017_GM_HEAD_02) LIKE  BAPI2017_GM_HEAD_02
*"  STRUCTURE  BAPI2017_GM_HEAD_02
*"--------------------------------------------------------------------

* This function module was generated. Don't change it manually!
* <VERSION>|5
* <BAPI_STRUCTURE>|BAPI2017_GM_HEAD_02
* <SAP_STRUCTURE>|MKPF
* <INTERN_TO_EXTERN>|X
* <APPEND FORM>|

* <BAPI_FIELD>|ENTRY_DATE
* <SAP_FIELD>|CPUDT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mkpf-cpudt
    TO bapi2017_gm_head_02-entry_date                               .

* <BAPI_FIELD>|ENTRY_TIME
* <SAP_FIELD>|CPUTM
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mkpf-cputm
    TO bapi2017_gm_head_02-entry_time                               .

* <BAPI_FIELD>|USERNAME
* <SAP_FIELD>|USNAM
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mkpf-usnam
    TO bapi2017_gm_head_02-username                                 .

* <BAPI_FIELD>|VER_GR_GI_SLIP
* <SAP_FIELD>|WEVER
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mkpf-wever
    TO bapi2017_gm_head_02-ver_gr_gi_slip                           .

* <BAPI_FIELD>|EXPIMP_NO
* <SAP_FIELD>|EXNUM
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mkpf-exnum
    TO bapi2017_gm_head_02-expimp_no                                .

* <BAPI_FIELD>|MAT_DOC
* <SAP_FIELD>|MBLNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mkpf-mblnr
    TO bapi2017_gm_head_02-mat_doc                                  .

* <BAPI_FIELD>|DOC_YEAR
* <SAP_FIELD>|MJAHR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mkpf-mjahr
    TO bapi2017_gm_head_02-doc_year                                 .

* <BAPI_FIELD>|TR_EV_TYPE
* <SAP_FIELD>|VGART
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mkpf-vgart
    TO bapi2017_gm_head_02-tr_ev_type                               .

* <BAPI_FIELD>|DOC_DATE
* <SAP_FIELD>|BLDAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mkpf-bldat
    TO bapi2017_gm_head_02-doc_date                                 .

* <BAPI_FIELD>|PSTNG_DATE
* <SAP_FIELD>|BUDAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mkpf-budat
    TO bapi2017_gm_head_02-pstng_date                               .

* <BAPI_FIELD>|REF_DOC_NO
* <SAP_FIELD>|XBLNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|

*  MOVE mkpf-xblnr
*    TO bapi2017_gm_head_02-ref_doc_no                               .

* IS-PS: special logic because the length of domain XBLNR1 could be 35
  CALL FUNCTION 'REF_DOC_NO_CONVERSION_OUTBOUND'
    EXPORTING
      i_ref_doc_no_long = mkpf-xblnr
    IMPORTING
      e_ref_doc_no      = bapi2017_gm_head_02-ref_doc_no
      e_ref_doc_no_long = bapi2017_gm_head_02-ref_doc_no_long.


* <BAPI_FIELD>|HEADER_TXT
* <SAP_FIELD>|BKTXT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
  MOVE mkpf-bktxt
    TO bapi2017_gm_head_02-header_txt                               .






ENDFUNCTION.
