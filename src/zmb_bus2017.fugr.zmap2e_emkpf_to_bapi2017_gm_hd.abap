FUNCTION ZMAP2E_EMKPF_TO_BAPI2017_GM_HD.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(EMKPF) LIKE  EMKPF STRUCTURE  EMKPF
*"  CHANGING
*"     REFERENCE(BAPI2017_GM_HEAD_RET) LIKE  BAPI2017_GM_HEAD_RET
*"  STRUCTURE  BAPI2017_GM_HEAD_RET
*"--------------------------------------------------------------------

* This function module was generated. Don't change it manually!
* <VERSION>|5
* <BAPI_STRUCTURE>|BAPI2017_GM_HEAD_RET
* <SAP_STRUCTURE>|EMKPF
* <INTERN_TO_EXTERN>|X
* <APPEND FORM>|

* <BAPI_FIELD>|MAT_DOC
* <SAP_FIELD>|MBLNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE EMKPF-MBLNR
  TO BAPI2017_GM_HEAD_RET-MAT_DOC                                 .

* <BAPI_FIELD>|DOC_YEAR
* <SAP_FIELD>|MJAHR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE EMKPF-MJAHR
  TO BAPI2017_GM_HEAD_RET-DOC_YEAR                                .





ENDFUNCTION.
