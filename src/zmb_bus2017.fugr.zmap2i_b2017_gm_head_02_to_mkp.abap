FUNCTION ZMAP2I_B2017_GM_HEAD_02_TO_MKP.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(BAPI2017_GM_HEAD_02) LIKE  BAPI2017_GM_HEAD_02
*"  STRUCTURE  BAPI2017_GM_HEAD_02
*"  CHANGING
*"     REFERENCE(MKPF) LIKE  MKPF STRUCTURE  MKPF
*"--------------------------------------------------------------------

* This function module was generated. Don't change it manually!
* <VERSION>|4
* <BAPI_STRUCTURE>|BAPI2017_GM_HEAD_02
* <SAP_STRUCTURE>|MKPF
* <INTERN_TO_EXTERN>|
* <APPEND FORM>|

* <BAPI_FIELD>|ENTRY_DATE
* <SAP_FIELD>|CPUDT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_HEAD_02-ENTRY_DATE
  TO MKPF-CPUDT                                                   .

* <BAPI_FIELD>|ENTRY_TIME
* <SAP_FIELD>|CPUTM
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_HEAD_02-ENTRY_TIME
  TO MKPF-CPUTM                                                   .

* <BAPI_FIELD>|USERNAME
* <SAP_FIELD>|USNAM
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_HEAD_02-USERNAME
  TO MKPF-USNAM                                                   .

* <BAPI_FIELD>|VER_GR_GI_SLIP
* <SAP_FIELD>|WEVER
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_HEAD_02-VER_GR_GI_SLIP
  TO MKPF-WEVER                                                   .

* <BAPI_FIELD>|EXPIMP_NO
* <SAP_FIELD>|EXNUM
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_HEAD_02-EXPIMP_NO
  TO MKPF-EXNUM                                                   .

* <BAPI_FIELD>|MAT_DOC
* <SAP_FIELD>|MBLNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_HEAD_02-MAT_DOC
  TO MKPF-MBLNR                                                   .

* <BAPI_FIELD>|DOC_YEAR
* <SAP_FIELD>|MJAHR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_HEAD_02-DOC_YEAR
  TO MKPF-MJAHR                                                   .

* <BAPI_FIELD>|TR_EV_TYPE
* <SAP_FIELD>|VGART
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_HEAD_02-TR_EV_TYPE
  TO MKPF-VGART                                                   .

* <BAPI_FIELD>|DOC_DATE
* <SAP_FIELD>|BLDAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_HEAD_02-DOC_DATE
  TO MKPF-BLDAT                                                   .

* <BAPI_FIELD>|PSTNG_DATE
* <SAP_FIELD>|BUDAT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_HEAD_02-PSTNG_DATE
  TO MKPF-BUDAT                                                   .

* <BAPI_FIELD>|REF_DOC_NO
* <SAP_FIELD>|XBLNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_HEAD_02-REF_DOC_NO
  TO MKPF-XBLNR                                                   .

* <BAPI_FIELD>|HEADER_TXT
* <SAP_FIELD>|BKTXT
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_HEAD_02-HEADER_TXT
  TO MKPF-BKTXT                                                   .

* <BAPI_FIELD>|REF_DOC_NO / REF_DOC_NO_LONG
* <SAP_FIELD>|XBLNR
* <CODE_PART>|
* <ADD_FIELD>|

* IS-PS: special logic because the length of domain XBLNR1 could be 35
  CALL FUNCTION 'REF_DOC_NO_CONVERSION_INBOUND'
    EXPORTING
      i_ref_doc_no      = BAPI2017_GM_HEAD_02-ref_doc_no
      i_ref_doc_no_long = BAPI2017_GM_HEAD_02-ref_doc_no_long
    IMPORTING
      e_ref_doc_no_long = mkpf-xblnr.


ENDFUNCTION.
