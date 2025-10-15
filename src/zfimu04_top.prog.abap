*&---------------------------------------------------------------------*
*&  Include           ZFIMU04_TOP
*&---------------------------------------------------------------------*
PROGRAM  ZFIMU04.

TYPES: BEGIN OF TY_CADLAN,
         ZID_CONTR       TYPE ZFITAXCTR-ZID_CONTR,
         NRO_SOL         TYPE ZFISOLCTR-NRO_SOL,
         BUKRS_F         TYPE ZFITAXCTR-BUKRS_F,
         BUTXT_F         TYPE T001-BUTXT,
         LIFNR           TYPE ZFITAXCTR-LIFNR,
         NAME_F          TYPE LFA1-NAME1,
         HKONT_F         TYPE ZFITAXCTR-HKONT_F,
         BUKRS_C         TYPE ZFITAXCTR-BUKRS_C,
         BUTXT_C         TYPE T001-BUTXT,
         KUNNR           TYPE ZFITAXCTR-KUNNR,
         NAME_C          TYPE KNA1-NAME1,
         HKONT_C         TYPE ZFITAXCTR-HKONT_C,
         CD_MOD          TYPE ZFISOLCTR-CD_MOD,
         DT_LCTO         TYPE ZFISOLCTR-DT_LCTO,
         DT_VCT          TYPE ZFISOLCTR-DT_VCT,
         WAERS           TYPE ZFISOLCTR-WAERS,
         VLR_MOEDA_DOC   TYPE ZFISOLCTR-VLR_MOEDA_DOC,
         VLR_MOEDA_INT   TYPE ZFISOLCTR-VLR_MOEDA_INT,
         VLR_MOEDA_FORTE TYPE ZFISOLCTR-VLR_MOEDA_FORTE,
         HBKID           TYPE ZFISOLCTR-HBKID,
         BVTYP           TYPE ZFISOLCTR-BVTYP,
         ZLSCH           TYPE ZFISOLCTR-ZLSCH,
         GSBER_F         TYPE ZFISOLCTR-GSBER_F,
         GSBER_C         TYPE ZFISOLCTR-GSBER_C,
         DOC_LCTOF       TYPE ZFISOLCTR-DOC_LCTOF,
         BELNRF          TYPE ZFISOLCTR-BELNRF,
         GJAHRF          TYPE ZFISOLCTR-GJAHRF,
         DOC_LCTOC       TYPE ZFISOLCTR-DOC_LCTOC,
         BELNRC          TYPE ZFISOLCTR-BELNRC,
         GJAHRC          TYPE ZFISOLCTR-GJAHRC,
       END OF TY_CADLAN,

       BEGIN OF TY_FIELDS,
         CAMPO(30) TYPE C,
         GROUP1(5) TYPE C,
         VALUE     TYPE SY-TABIX,
         INVISIBLE TYPE SY-TABIX,
       END OF TY_FIELDS.


*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: OK-CODE         TYPE SY-UCOMM,
      X_FIELD(30),
      WG_MENSAGEM(30),
      WG_ACAO(30),
      VMUDAR(1).

* Work AREAS
DATA: WG_CADLAN TYPE TY_CADLAN.

* Tabelas
DATA: TG_FIELDS  TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
      TG_MSG_RET TYPE TABLE OF ZFIWRS0002  WITH HEADER LINE,
      T_USERMD   TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS:
  C_0              TYPE C VALUE '0',
  C_1              TYPE C VALUE '1',
  C_X              TYPE C VALUE 'X',
  C_I              TYPE C VALUE 'I',
  C_N              TYPE C VALUE 'N',
  C_NE(2)          TYPE C VALUE 'NE',
  C_ADD(3)         TYPE C VALUE 'ADD',
  C_DEL(3)         TYPE C VALUE 'DEL',
  C_DG1(3)         TYPE C VALUE 'DG1',
  C_DG2(3)         TYPE C VALUE 'DG2',
  C_EXIT(4)        TYPE C VALUE 'EXIT',
  C_BACK(4)        TYPE C VALUE 'BACK',
  C_SAVE(4)        TYPE C VALUE 'SAVE',
  C_MODIF(5)       TYPE C VALUE 'MODIF',
  C_CANCEL(6)      TYPE C VALUE 'CANCEL',
  C_DELDOC(6)      TYPE C VALUE 'DELDOC',
  C_SEARCH(6)      TYPE C VALUE 'SEARCH',
  C_DISPLA(6)      TYPE C VALUE 'DISPLA',
  C_ATUALI(6)      TYPE C VALUE 'ATUALI',
  C_CLOS_MSG(8)    TYPE C VALUE 'CLOS_MSG',
  C_SHOW_MSGRE(10) TYPE C VALUE 'SHOW_MSGRE',
  C_COL_EXP(7)     TYPE C VALUE 'COL_EXP'.
