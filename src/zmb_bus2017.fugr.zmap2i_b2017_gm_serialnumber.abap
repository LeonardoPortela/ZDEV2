FUNCTION ZMAP2I_B2017_GM_SERIALNUMBER.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(BAPI2017_GM_SERIALNUMBER)
*"  LIKE  BAPI2017_GM_SERIALNUMBER STRUCTURE  BAPI2017_GM_SERIALNUMBER
*"  CHANGING
*"     REFERENCE(ISERI) LIKE  ISERI STRUCTURE  ISERI
*"--------------------------------------------------------------------

* This function module was generated. Don't change it manually!
* <VERSION>|4
* <BAPI_STRUCTURE>|BAPI2017_GM_SERIALNUMBER
* <SAP_STRUCTURE>|ISERI
* <INTERN_TO_EXTERN>|
* <APPEND FORM>|

* <BAPI_FIELD>|MATDOC_ITM
* <SAP_FIELD>|MBLPO
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_SERIALNUMBER-MATDOC_ITM
  TO ISERI-MBLPO                                                  .

* <BAPI_FIELD>|SERIALNO
* <SAP_FIELD>|SERIALNR
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_SERIALNUMBER-SERIALNO
  TO ISERI-SERIALNR                                               .

* <BAPI_FIELD>|UII
* <SAP_FIELD>|UII
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_SERIALNUMBER-UII
  TO ISERI-UII                                                    .





ENDFUNCTION.
