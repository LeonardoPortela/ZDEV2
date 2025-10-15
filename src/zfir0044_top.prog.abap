*&---------------------------------------------------------------------*
*& Include ZFIR0044_TOP                                      PoolMóds.        ZFIR0044
*&
*&---------------------------------------------------------------------*

PROGRAM  ZFIR0044.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*

TYPES:
    BEGIN OF TY_ZFIT0065,
        BUKRS            TYPE ZFIT0065-BUKRS,
        BUTXT            TYPE T001-BUTXT,
        NRO_CTO          TYPE ZFIT0065-NRO_CTO,
        NRO_PAR          TYPE ZFIT0065-NRO_PAR,
        COD_OPER         TYPE ZFIT0065-COD_OPER,
        DESCR_OPER       TYPE ZFIT0063-DESCR,
        BANCO            TYPE ZFIT0065-BANCO,
        POSICAO          TYPE ZFIT0065-POSICAO,
        DESCR_POS        TYPE ZFIT0063-DESCR,
        NATUREZA_CTO     TYPE ZFIT0065-NATUREZA_CTO,
        TP_OPERACAO      TYPE ZFIT0065-TP_OPERACAO,
        DESCR_TP         TYPE ZFIT0063-DESCR,
        DT_INICIO_CTO    TYPE ZFIT0065-DT_INICIO_CTO,
        DT_FIM_CTO       TYPE ZFIT0065-DT_FIM_CTO,
        VLR_CTO_R        TYPE ZFIT0065-VLR_CTO_R,
        VLR_CTO_US       TYPE ZFIT0065-VLR_CTO_US,
        VLR_PARC         TYPE ZFIT0065-VLR_PARC,
        MOEDA            TYPE ZFIT0065-MOEDA,
        TX_CAMBIO_FUT    TYPE ZFIT0065-TX_CAMBIO_FUT,
        TP_TX_ATIVA      TYPE ZFIT0065-TP_TX_ATIVA,
        DESCR_ATIVA      TYPE ZFIT0063-DESCR,
        TX_CAMBIO_IN_OP  TYPE ZFIT0065-TX_CAMBIO_IN_OP,
        TX_JROS_ATIVA    TYPE ZFIT0065-TX_JROS_ATIVA,
        TP_TX_PASSIVA    TYPE ZFIT0065-TP_TX_PASSIVA,
        DESCR_PASSIVA    TYPE ZFIT0063-DESCR,
        TX_JROS_PASSIVA  TYPE ZFIT0065-TX_JROS_PASSIVA,
        TX_ACIMA_IND     TYPE ZFIT0065-TX_ACIMA_IND,
        INDEX_PASSIVO    TYPE ZFIT0065-INDEX_PASSIVO,
        TX_INDEX_PASSIVO TYPE ZFIT0065-TX_INDEX_PASSIVO,
        TX_AC_INDEX_PASS TYPE ZFIT0065-TX_AC_INDEX_PASS,
        INDEX_ATIVO      TYPE ZFIT0065-INDEX_ATIVO,
        TX_INDEX_ATIVO   TYPE ZFIT0065-TX_INDEX_ATIVO,
    END OF TY_ZFIT0065,

     BEGIN OF TY_FIELDS,
        CAMPO(30) TYPE C,
        GROUP1(5) TYPE C,
        VALUE     TYPE SY-TABIX,
        INVISIBLE TYPE SY-TABIX,
    END   OF TY_FIELDS.


*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: OK-CODE       TYPE SY-UCOMM,
      WG_CADCTO     TYPE TY_ZFIT0065,

      TG_FIELDS             TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
      TG_MSG_RET            TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE,

      X_FIELD(30),
      WG_MENSAGEM(30),
      WG_ACAO(30),
      XEXISTE(1),
      XMODIF(1).

DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
      TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_0               TYPE C VALUE '0',
         C_1               TYPE C VALUE '1',
         C_2               TYPE C VALUE '2',
         C_B               TYPE C VALUE 'B',
         C_S               TYPE C VALUE 'S',
         C_L               TYPE C VALUE 'L',
         C_X               TYPE C VALUE 'X',
         C_D               TYPE C VALUE 'D',
         C_K               TYPE C VALUE 'K',
         C_W               TYPE C VALUE 'W',
         C_F               TYPE C VALUE 'F',
         C_T               TYPE C VALUE 'T',
         C_I               TYPE C VALUE 'I',
         C_N               TYPE C VALUE 'N',
         C_H               TYPE C VALUE 'H',
         C_AG(2)           TYPE C VALUE 'AG',
         C_NE(2)           TYPE C VALUE 'NE',
         C_01(2)           TYPE C VALUE '01',
         C_30(2)           TYPE C VALUE '30',
         C_40(2)           TYPE C VALUE '40',
         C_50(4)           TYPE C VALUE '0050',
         C_76(2)           TYPE C VALUE '76',
         C_71(2)           TYPE C VALUE '71',
         C_72(2)           TYPE C VALUE '72',
         C_BR(2)           TYPE C VALUE 'BR',
         C_LF(2)           TYPE C VALUE 'LF',
         C_LR(2)           TYPE C VALUE 'LR',
         C_Z1(2)           TYPE C VALUE 'Z1',
         C_ADD(3)          TYPE C VALUE 'ADD',
         C_DEL(3)          TYPE C VALUE 'DEL',
         C_DG1(3)          TYPE C VALUE 'DG1',
         C_DG2(3)          TYPE C VALUE 'DG2',
         C_DUMMY_HEADER(3) TYPE C VALUE '099',
         C_DUMMY_ITENS(3)  TYPE C VALUE '098',
         C_EXIT(4)         TYPE C VALUE 'EXIT',
         C_ROOT(4)         TYPE C VALUE 'ROOT',
         C_MINIMIZAR(4)    TYPE C VALUE '@K2@',
         C_MAXIMIZAR(4)    TYPE C VALUE '@K1@',
         C_BACK(4)         TYPE C VALUE 'BACK',
         C_SAVE(4)         TYPE C VALUE 'SAVE',
         C_DESAT(5)        TYPE C VALUE 'DESAT',
         C_DMBTR(5)        TYPE C VALUE 'DMBTR',
         C_MODIF(5)        TYPE C VALUE 'MODIF',
         C_CANCEL(6)       TYPE C VALUE 'CANCEL',
         C_DELDOC(6)       TYPE C VALUE 'DELDOC',
         C_DCLICK(6)       TYPE C VALUE 'DCLICK',
         C_SEARCH(6)       TYPE C VALUE 'SEARCH',
         C_ATUALI(6)       TYPE C VALUE 'ATUALI',
         C_ADD_MSG(7)      TYPE C VALUE 'ADD_MSG',
         C_DEL_MSG(7)      TYPE C VALUE 'DEL_MSG',
         C_CLOS_MSG(8)     TYPE C VALUE 'CLOS_MSG',
         C_SAVE_MSG(8)     TYPE C VALUE 'SAVE_MSG',
         C_COL_EXP(7)      TYPE C VALUE 'COL_EXP',
         C_DISPLA(6)       TYPE C VALUE 'DISPLA',
         C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE'.
