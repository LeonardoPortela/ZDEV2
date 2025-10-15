FUNCTION-POOL ZXIPOZGR                     MESSAGE-ID Z01.

***********************************************************************
* CONSTANTES
***********************************************************************
CONSTANTS:
  CC_A                                  VALUE 'A',
  CC_E                                  VALUE 'E',
  CC_I                                  VALUE 'I',
  CC_S                                  VALUE 'S',
  CC_X                                  VALUE 'X',
  CC_CLASSE_MENSAGEM       TYPE ARBGB   VALUE 'Z01'.

***********************************************************************
* TABELAS Internas
***********************************************************************
DATA: YT_LOG_POZGR         TYPE TABLE OF ZFIE_RET_DOCUMENT
                           WITH HEADER LINE INITIAL SIZE 0,
      YT_POITEM            TYPE TABLE OF BAPIMEPOITEM
                           WITH HEADER LINE INITIAL SIZE 0,
      YT_PARTNER           TYPE TABLE OF  BAPIEKKOP
                           WITH HEADER LINE INITIAL SIZE 0,
      YT_POSHIP            TYPE TABLE OF  BAPIITEMSHIP
                           WITH HEADER LINE INITIAL SIZE 0,

      YT_POSHIPX            TYPE TABLE OF  BAPIITEMSHIPX
                           WITH HEADER LINE INITIAL SIZE 0,

      YT_POITEMX           TYPE TABLE OF BAPIMEPOITEMX
                           WITH HEADER LINE INITIAL SIZE 0,
      YT_POSCHEDULE        TYPE TABLE OF BAPIMEPOSCHEDULE
                           WITH HEADER LINE INITIAL SIZE 0,
      YT_POSCHEDULEX       TYPE TABLE OF BAPIMEPOSCHEDULX
                           WITH HEADER LINE INITIAL SIZE 0,
      YT_POACCOUNT         TYPE TABLE OF BAPIMEPOACCOUNT
                           WITH HEADER LINE INITIAL SIZE 0,
      YT_POACCOUNTX        TYPE TABLE OF BAPIMEPOACCOUNTX
                           WITH HEADER LINE INITIAL SIZE 0,
      YT_T001L             TYPE TABLE OF T001L
                           WITH HEADER LINE INITIAL SIZE 0,
      YT_RETURN            TYPE TABLE OF BAPIRET2
                           WITH HEADER LINE INITIAL SIZE 0.

***********************************************************************
* VARIÁVEIS
***********************************************************************
DATA: W_ZMMT_PO_ZGR        TYPE ZMMT_PO_ZGR,
      "W_ZMMPO_ITEM_ZGR     type ZMMT_PO_ITEM_ZGR,
      W_POHEADER           TYPE BAPIMEPOHEADER,
      W_PARTNER            TYPE BAPIEKKOP,
      W_POHEADERX          TYPE BAPIMEPOHEADERX,
      W_EXPHEADER          TYPE BAPIMEPOHEADER,
      W_T001L              TYPE T001L.

"data it_ZMMPO_ITEM_ZGR     type TABLE OF ZMMT_PO_ITEM_ZGR.

DATA: VC_PURCHASEORDER     LIKE BAPIMEPOHEADER-PO_NUMBER,
      VC_OBJ_KEY           TYPE AWKEY,
      VN_EBELP             TYPE EBELP,
      VG_ERRO              TYPE C LENGTH 1,
      VG_NO_PRICE_FROM_PO  TYPE BAPIFLAG-BAPIFLAG.
