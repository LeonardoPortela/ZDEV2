*****           Implementation of object type ZGL026               *****
INCLUDE <OBJECT>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF KEY,
      MONAT LIKE ZGLT043-MONAT,
      GJAHR LIKE ZGLT043-GJAHR,
      BUKRS LIKE ZGLT043-BUKRS,
      SAKNR LIKE ZGLT043-SAKNR,
  END OF KEY.
END_DATA OBJECT. " Do not change.. DATA is generated


BEGIN_METHOD GOSADDOBJECTS CHANGING CONTAINER.
DATA:
      SERVICE(255),
      BUSIDENTIFS LIKE BORIDENT OCCURS 0,
      LS_BORIDENT type BORIDENT.

  CLEAR LS_BORIDENT.
  LS_BORIDENT-LOGSYS  = SPACE.
  LS_BORIDENT-OBJTYPE = 'ZGL026'.
  LS_BORIDENT-OBJKEY  = OBJECT-KEY.
  APPEND LS_BORIDENT TO BUSIDENTIFS.

  SWC_GET_ELEMENT CONTAINER 'Service' SERVICE.
  SWC_SET_TABLE CONTAINER 'BusIdentifs' BUSIDENTIFS.

END_METHOD.

BEGIN_METHOD SELECTSERVICES CHANGING CONTAINER.
DATA:
      OPTIONS LIKE SGOS_SELS OCCURS 0.
  SWC_SET_TABLE CONTAINER 'Options' OPTIONS.
END_METHOD.
