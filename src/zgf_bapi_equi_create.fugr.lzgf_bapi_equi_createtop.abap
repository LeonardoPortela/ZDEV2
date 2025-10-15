FUNCTION-POOL zgf_bapi_equi_create.         "MESSAGE-ID ..

* INCLUDE LZGF_BAPI_EQUI_CREATED...          " Local class definition

CONSTANTS:
* access levels for calling applications
  gc_level_undefined TYPE char1 VALUE 'A', "undefined (not set)

  BEGIN OF gc_level_equi,
    ito1   TYPE char1 VALUE 'B', "internal (ITOB_BUF_CHANGE)
    ieqcm1 TYPE char1 VALUE 'C', "internal (change of history IEQCM1)
    ie01   TYPE char1 VALUE 'D', "internal (EQUIPMENT_UPDATE)
    ili1   TYPE char1 VALUE 'F', "data transfer (func group ILI1)
    ito3   TYPE char1 VALUE 'H', "new APIs (func group ITO3)
    ibeq   TYPE char1 VALUE 'K', "old APIs (func group IBEQ)
    bapi   TYPE char1 VALUE 'M', "new BAPIs (func group ITOB_BAPI_EQ)
    iwww   TYPE char1 VALUE 'O', "old BAPIs (func group IWWW)
    ipw1   TYPE char1 VALUE 'P', "serial movement (func group IPW1)
    ipmb   TYPE char1 VALUE 'Q', "serial BAPI (e.g. GOODSMVT_CREATE)
    iel2   TYPE char1 VALUE 'R', "sub equipment (func group IEL2)
    aapm   TYPE char1 VALUE 'T', "aapm interface (func group AAPM)
    ieq0   TYPE char1 VALUE 'V', "transaction (module pool SAPMIEQ0)
    cc04   TYPE char1 VALUE 'X', "browser (func group ITOBBROWSER)
    ibip   TYPE char1 VALUE 'Z', "batch input (func group IBIP)
  END   OF gc_level_equi.


* global type pools
TYPE-POOLS:
  itob,
  slis.

* global variables
*ATA:
* BEGIN OF G_PARMS,
*   CALLER(2)            TYPE C,
* END   OF G_PARMS.

* global constants
CONSTANTS:
  gc_x TYPE char1 VALUE 'X',                                "n1600408
* BEGIN OF GC_CALLER,
*   CREATE               LIKE G_PARMS-CALLER VALUE '01',
*   CHANGE               LIKE G_PARMS-CALLER VALUE '02',
*   GETDETAIL            LIKE G_PARMS-CALLER VALUE '03',
*   CREATE_FROM_INSTANCE LIKE G_PARMS-CALLER VALUE '04',
*   INSTALL              LIKE G_PARMS-CALLER VALUE '05',
* END   OF GC_CALLER.

  BEGIN OF gc_transaction,
    equi_create LIKE sy-tcode       VALUE 'IE01',
    equi_change LIKE sy-tcode       VALUE 'IE02',
    equi_read   LIKE sy-tcode       VALUE 'IE03',           "P9CK232793
  END   OF gc_transaction.

*--- table for text search in other languages
TYPES: BEGIN OF gtype_s_equi_text,
         equnr TYPE equnr,
         spras TYPE spras,
         txasp TYPE kzmla,
         eqktx TYPE ktx01,
       END OF gtype_s_equi_text.

DATA: gt_equi_text TYPE SORTED TABLE OF gtype_s_equi_text WITH UNIQUE KEY equnr spras.

* DATA:  gv_init_request TYPE char1.                            "not needed any more

DATA: go_ITOB_ITO3            TYPE REF TO if_itob_ito3.            "note 2158328
DATA: go_ITOB_USTAT_FUNCTIONS TYPE REF TO if_itob_ustat_functions. "note 2158328
