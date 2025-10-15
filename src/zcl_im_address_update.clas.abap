class ZCL_IM_ADDRESS_UPDATE definition
  public
  final
  create public .

*"* public components of class ZCL_IM_ADDRESS_UPDATE
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_ADDRESS_UPDATE .
protected section.
*"* protected components of class ZCL_IM_ADDRESS_UPDATE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_ADDRESS_UPDATE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_ADDRESS_UPDATE IMPLEMENTATION.


method IF_EX_ADDRESS_UPDATE~ADDRESS1_SAVED.

endmethod.


METHOD IF_EX_ADDRESS_UPDATE~ADDRESS2_SAVED.

ENDMETHOD.


method IF_EX_ADDRESS_UPDATE~ADDRESS3_SAVED.


endmethod.


METHOD IF_EX_ADDRESS_UPDATE~FINISHED.

*  DATA: WA_ADCP  TYPE ADCP,
*        WA_ADCP2 TYPE ADCP,
*        IT_ADCP  TYPE  TABLE OF ADCP.
*
*  SELECT SINGLE *
*    FROM ADCP
*    INTO WA_ADCP
*    WHERE ADDRNUMBER EQ IM_ADDRESS_NUMBER
*    AND   PERSNUMBER EQ IM_PERSON_NUMBER.
*
*  IF SY-SUBRC = 0.
*    SELECT *
*        FROM ADCP
*        INTO TABLE IT_ADCP
*        WHERE FAX_NUMBER = WA_ADCP-FAX_NUMBER.
*
*    LOOP AT IT_ADCP INTO WA_ADCP2.
*      IF WA_ADCP2-ADDRNUMBER NE WA_ADCP-ADDRNUMBER AND
*         WA_ADCP2-PERSNUMBER NE WA_ADCP-PERSNUMBER.
*        BREAK-POINT.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

ENDMETHOD.
ENDCLASS.
