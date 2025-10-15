*&---------------------------------------------------------------------*
*& Include ZGL014_TOP                                        PoolMóds.        ZGL014
*&
*&---------------------------------------------------------------------*

PROGRAM  ZGL014.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
RANGES: P_BUKRS      FOR ZGLT034-BUKRS.

INCLUDE: <ICON>.

TYPES:  BEGIN OF TY_GLT034,
          LOTE        TYPE ZGLT034-LOTE,
          DESCR_LOTE  TYPE ZGLT034-DESCR_LOTE,
          BUKRS       TYPE ZGLT034-BUKRS,
          USNAM       TYPE ZGLT034-USNAM,
          DEP_RESP    TYPE ZGLT034-DEP_RESP,
          STATUS_LOTE TYPE ZGLT034-STATUS_LOTE,
          DATA_ATUAL  TYPE ZGLT034-DATA_ATUAL,
          HORA_ATUAL  TYPE ZGLT034-HORA_ATUAL,
          USUARIO     TYPE ZGLT034-USUARIO,
        END OF TY_GLT034,

        BEGIN OF TY_FIELDS,
          CAMPO(30)   TYPE C,
          GROUP1(5)   TYPE C,
          VALUE       TYPE SY-TABIX,
          INVISIBLE   TYPE SY-TABIX,
        END OF TY_FIELDS.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: OK-CODE         TYPE SY-UCOMM,
      X_FIELD(30),
      WG_BTN(4)       VALUE '@1F@',

      TG_ZGLT034      TYPE TABLE OF TY_GLT034,
      WG_ZGLT034      TYPE TY_GLT034,

      TG_FIELDS       TYPE TABLE OF TY_FIELDS   WITH HEADER LINE.

DATA: OK_CODE         LIKE SY-UCOMM,
      WG_MENSAGEM(30),
      WG_MENSLAN(30),
      WG_ACAO(30),
      WL_ERRO(1),
      VG_LOTE               TYPE ZGLT034-LOTE,
      VG_BNAME              TYPE USER_ADDR-BNAME.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS:
    C_0               TYPE C VALUE '0',
    C_1               TYPE C VALUE '1',
    C_X               TYPE C VALUE 'X',
    C_ADD(3)          TYPE C VALUE 'ADD',
    C_DEL(3)          TYPE C VALUE 'DEL',
    C_EXIT(4)         TYPE C VALUE 'EXIT',
    C_BACK(4)         TYPE C VALUE 'BACK',
    C_SAVE(4)         TYPE C VALUE 'SAVE',
    C_MODIF(5)        TYPE C VALUE 'MODIF',
    C_CANCEL(6)       TYPE C VALUE 'CANCEL',
    C_DELDOC(6)       TYPE C VALUE 'DELDOC',
    C_SEARCH(6)       TYPE C VALUE 'SEARCH',
    C_ATUALI(6)       TYPE C VALUE 'ATUALI',
    C_ENTER(6)        TYPE C VALUE 'ENTER',
    C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE'.
*&--------------------------------------------------------------------&*
*& Declaração Tela                                                    &*
*&--------------------------------------------------------------------&*
DATA  WGLT034-LOTE(20).
DATA  WGLT034-DESCR_LOTE(20).
DATA  WGLT034-BUKRS(10).
DATA  WGZGLT034-USNAM(20).
DATA  WGLT034-DEP_RESP(28).
