
*----------------------------------------------------------------------*
*   INCLUDE HBRCVT02   " Data declaration for list and tree outputs
*----------------------------------------------------------------------*

INCLUDE <COLOR>.

TABLES: ICON.

DATA: W_ICON      LIKE ICON-ID,
      REJECTED    TYPE I.

INCLUDE ZHBRCVTR0TOP.

TYPE-POOLS SLIS.                       "for 'REUSE_ALV...'

*data: vt_program like sy-repid.        "for 'REUSE_ALV_LIST_DISPLAY

* tables for tree
DATA: TREE_TAB LIKE SNODETEXT OCCURS 0.

* tables for list
DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.  "for 'REUSE..

DATA: L_HIRE_DATE       TYPE D,
      L_FIRE_DATE       TYPE D.

DATA G_BUKRS TYPE BUKRS.
