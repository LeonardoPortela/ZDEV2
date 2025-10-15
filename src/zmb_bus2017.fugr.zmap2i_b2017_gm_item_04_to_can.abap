FUNCTION ZMAP2I_B2017_GM_ITEM_04_TO_CAN.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(BAPI2017_GM_ITEM_04) LIKE  BAPI2017_GM_ITEM_04
*"  STRUCTURE  BAPI2017_GM_ITEM_04
*"  CHANGING
*"     REFERENCE(IMSEG_CANCEL) LIKE  IMSEG_CANCEL
*"  STRUCTURE  IMSEG_CANCEL
*"--------------------------------------------------------------------

* This function module was generated. Don't change it manually!
* <VERSION>|4
* <BAPI_STRUCTURE>|BAPI2017_GM_ITEM_04
* <SAP_STRUCTURE>|IMSEG_CANCEL
* <INTERN_TO_EXTERN>|
* <APPEND FORM>|

* <BAPI_FIELD>|MATDOC_ITEM
* <SAP_FIELD>|MBLPO
* <CODE_PART>|A_MOVE
* <ADD_FIELD>|
MOVE BAPI2017_GM_ITEM_04-MATDOC_ITEM
  TO IMSEG_CANCEL-MBLPO                                           .





ENDFUNCTION.
