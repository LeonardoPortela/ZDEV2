*TYPES: BEGIN OF TY_WITH_ITEM,
*        WITHT    TYPE WITH_ITEM-WITHT,
*        WT_QBSHH TYPE WITH_ITEM-WT_QBSHH,
*        BELNR    TYPE WITH_ITEM-BELNR,
*       END OF TY_WITH_ITEM.

*       begin of ty_ekpo,
*         ebeln type ekpo-ebeln,
*         ebelp type ekpo-ebelp,
*       end of ty_ekpo,

*       BEGIN OF TY_RBKP,
*         XBLNR TYPE RBKP-XBLNR,
*         LIFNR TYPE RBKP-LIFNR,
*         RMWWR TYPE RBKP-RMWWR,
*         BELNR TYPE RBKP-BELNR,
*       END OF TY_RBKP.

TYPES: Ty_Lfa1     TYPE TABLE OF LFA1,
       Ty_EKKO     TYPE TABLE OF EKKO,
*       Ty_EKPO      TYPE TABLE OF EKPO,
       Ty_RBKP     TYPE TABLE OF RBKP,
       ty_with_item type TABLE OF with_item,
       ty_ekbe     type TABLE OF ekbe,
       TY_BKPF     TYPE TABLE OF bkpf.



