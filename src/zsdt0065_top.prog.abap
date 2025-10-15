*&---------------------------------------------------------------------*
*& Include ZSDT0065_TOP                                      PoolMóds.        ZSDT0065
*&
*&---------------------------------------------------------------------*

PROGRAM  ZSDT0065.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES:  BEGIN OF TY_CADLAN,
          INSTRUCAO         TYPE ZFIT0048-INSTRUCAO,
          DES_INSTRUCAO(15)                        ,
          BUKRS             TYPE T001-BUKRS        ,
          BUTXT             TYPE T001-BUTXT        ,
          WERKS             TYPE J_1BBRANCH-BRANCH ,
          NAMEW             TYPE J_1BBRANCH-NAME   ,
          KUNNR             TYPE KNA1-KUNNR        ,
          NAME1             TYPE KNA1-NAME1        ,
          TXT_INSTRUCAO     TYPE ZFIT0048-TXT_INSTRUCAO,
        END OF TY_CADLAN,

        BEGIN OF TY_FIELDS,
            CAMPO(30) TYPE C,
            GROUP1(5) TYPE C,
            VALUE     TYPE SY-TABIX,
            INVISIBLE TYPE SY-TABIX,
        END   OF TY_FIELDS,

       BEGIN OF TY_EDITOR,
          LINE(80),
       END   OF TY_EDITOR.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas Seleção                                      &*
*&--------------------------------------------------------------------&*
DATA :  IT_ZFIT0048           TYPE TABLE OF ZFIT0048,
        TG_FIELDS             TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
        TG_EDITOR             TYPE TABLE OF TY_EDITOR.

DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
      TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.
*&--------------------------------------------------------------------&*
*& Declaração de Work Areas Seleção                                   &*
*&--------------------------------------------------------------------&*
DATA :  WA_ZFIT0048             TYPE ZFIT0048.

DATA: OK-CODE               LIKE SY-UCOMM ,
      WG_ACAO(30)                         ,
      WG_MENSAGEM(30)                     ,
      X_FIELD(30)                         ,
      WG_CADLAN             TYPE TY_CADLAN,
      WG_EDITOR             TYPE TY_EDITOR,
      TG_MSG_RET            TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: G_DESCBOX            TYPE SCRFNAME VALUE 'CC_INSTR',
      G_CUSTOM_INSTR       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBG_DESCBOX          TYPE REF TO CL_GUI_TEXTEDIT.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS:
    C_0               TYPE C VALUE '0',
    C_1               TYPE C VALUE '1',
    C_X               TYPE C VALUE 'X',
    C_I               TYPE C VALUE 'I',
    C_N               TYPE C VALUE 'N',
    C_NE(2)           TYPE C VALUE 'NE',
    C_ADD(3)          TYPE C VALUE 'ADD',
    C_DEL(3)          TYPE C VALUE 'DEL',
    C_DG1(3)          TYPE C VALUE 'DG1',
    C_DG2(3)          TYPE C VALUE 'DG2',
    C_EXIT(4)         TYPE C VALUE 'EXIT',
    C_BACK(4)         TYPE C VALUE 'BACK',
    C_SAVE(4)         TYPE C VALUE 'SAVE',
    C_MODIF(5)        TYPE C VALUE 'MODIF',
    C_CANCEL(6)       TYPE C VALUE 'CANCEL',
    C_DELDOC(6)       TYPE C VALUE 'DELDOC',
    C_SEARCH(6)       TYPE C VALUE 'SEARCH',
    C_DISPLA(6)       TYPE C VALUE 'DISPLA',
    C_ATUALI(6)       TYPE C VALUE 'ATUALI',
    C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE'.
