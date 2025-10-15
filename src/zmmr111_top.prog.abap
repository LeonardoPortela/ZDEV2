*&---------------------------------------------------------------------*
*& Include ZMMR111_TOP                                       PoolMóds.        ZMMR111
*&
*&---------------------------------------------------------------------*
PROGRAM ZMMR111.

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
  C_SHOW_MSGRE(10) TYPE C VALUE 'SHOW_MSGRE'.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*

TYPES:  BEGIN OF TY_CADMAT,
          NUREQ       TYPE  ZMMT0069-NUREQ,
          WERKS       TYPE  ZMMT0069-WERKS,
          VKORG       TYPE  ZMMT0069-VKORG,
          VTWEG       TYPE  ZMMT0069-VTWEG,
          HOM_SUP     TYPE  ZMMT0069-HOMOLOG,
          HOM_SSO     TYPE  ZMMT0069-HOMOLOG,
          HOM_EPI     TYPE  ZMMT0069-HOMOLOG,
          HOM_TI      TYPE  ZMMT0069-HOMOLOG,
          EXPANSAO    TYPE  ZMMT0069-EXPANSAO,
          MATNR       TYPE  ZMMT0069-MATNR, "material equivalente no SAP
          MAKTX       TYPE  ZMMT0069-MAKTX, "Descrição breve sugerida
          WGBEZ       TYPE  T023T-WGBEZ,
          MATKL       TYPE  ZMMT0069-MATKL, "grupo SAP sugerido
          MEINS       TYPE  MARA-MEINS, "Unid medida Sugerida
          MATNRG      TYPE  MARA-MATNR, "codigo SAP gerado
          MBRSH       TYPE  MARA-MBRSH,
          MBBEZ       TYPE  T137T-MBBEZ,
          MTART       TYPE  MARA-MTART,
          MTBEZ       TYPE  T134T-MTBEZ,
          CODAGREPI   TYPE  ZMMT0085-CODAGREPI,
          DENAGREPI   TYPE  ZHRST_MED_AG_EPI-DENAGREPI,
          SOLIC       TYPE  ZMMT0069-SOLIC,
          USNAM       TYPE  ZMMT0069-USNAM,
          DT_ENTRADA  TYPE  ZMMT0069-DT_ENTRADA,
          HR_ENTRADA  TYPE  ZMMT0069-HR_ENTRADA,
          CRIADO      TYPE  ZMMT0069-CRIADO,
          DT_CRIACAO  TYPE  ZMMT0069-DT_CRIACAO,
          HR_CRIACAO  TYPE  ZMMT0069-HR_CRIACAO,
          ELIMINADO   TYPE  ZMMT0069-ELIMINADO,
          RECUSA_FLAG TYPE  ZMMT0069-RECUSA_FLAG,
        END OF TY_CADMAT,

        BEGIN OF TY_FIELDS,
          CAMPO(30) TYPE C,
          GROUP1(5) TYPE C,
          VALUE     TYPE SY-TABIX,
          INVISIBLE TYPE SY-TABIX,
        END OF TY_FIELDS,

        BEGIN OF TY_EDITOR,
          LINE(72),
        END OF TY_EDITOR.

*&--------------------------------------------------------------------&*
*& Variáveis                                                          &*
*&--------------------------------------------------------------------&*
DATA: OK-CODE         TYPE SY-UCOMM,
      X_FIELD(30),
      WG_MENSAGEM(30),
      WG_ACAO(30).

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: OBG_DESCBOX         TYPE REF TO CL_GUI_TEXTEDIT,
      G_DESCBOX           TYPE SCRFNAME VALUE 'CC_OBS',
      G_CUSTOM_CONT_DESC  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

      OBG_DESCBOXR        TYPE REF TO CL_GUI_TEXTEDIT,
      G_DESCBOXR          TYPE SCRFNAME VALUE 'CC_REC',
      G_CUSTOM_CONT_DESCR TYPE REF TO CL_GUI_CUSTOM_CONTAINER.


*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*

DATA: WG_CADMAT  TYPE TY_CADMAT,
      WL_LOG     TYPE ZMMT0069_LOG,
      TG_FIELDS  TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
      TG_EDITOR  TYPE TABLE OF TY_EDITOR,
      WG_EDITOR  TYPE TY_EDITOR,
      TG_MSG_RET TYPE TABLE OF ZFIWRS0002  WITH HEADER LINE.
