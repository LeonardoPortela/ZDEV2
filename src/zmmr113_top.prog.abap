*&---------------------------------------------------------------------*
*&  Include           ZMMR113_TOP
*&---------------------------------------------------------------------*

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
  C_023            TYPE C  LENGTH 3 VALUE '023',
  C_MCH1           TYPE C  LENGTH 4 VALUE 'MCH1'.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.


TYPES:
  BEGIN OF TY_CDHDR,
    OBJECTID TYPE CDHDR-OBJECTID,
    CHANGENR TYPE CDHDR-CHANGENR,
    USERNAME TYPE CDHDR-USERNAME,
    UDATE    TYPE CDHDR-UDATE,
    UTIME    TYPE CDHDR-UTIME,
  END OF TY_CDHDR,

  BEGIN OF TY_CDPOS,
    OBJECTID  TYPE CDPOS-OBJECTID,
    CHANGENR  TYPE CDPOS-CHANGENR,
    TABKEY    TYPE CDPOS-TABKEY,
    VALUE_NEW TYPE CDPOS-VALUE_NEW,
    VALUE_OLD TYPE CDPOS-VALUE_OLD,
    ATINN     TYPE CABN-ATINN,
  END OF TY_CDPOS,

  BEGIN OF TY_MCHB,
    MATNR TYPE MCHB-MATNR,
    CHARG TYPE MCHB-CHARG,
    LGORT TYPE MCHB-LGORT,
  END OF TY_MCHB,

  BEGIN OF TY_INOB,
    CUOBJ    TYPE INOB-CUOBJ,
    OBJEK    TYPE KSSK-OBJEK,
    OBJECTID TYPE CDHDR-OBJECTID,
  END OF TY_INOB,

  BEGIN OF TY_INOB2,
    OBJEK TYPE KSSK-OBJEK,
  END OF TY_INOB2,

  BEGIN OF TY_SAIDA,
    MATNR     TYPE MCHB-MATNR,
    CHARG     TYPE MCHB-CHARG,
    LGORT     TYPE MCHB-LGORT,
    OBJECTID  TYPE CDHDR-OBJECTID,
    CHANGENR  TYPE CDHDR-CHANGENR,
    USERNAME  TYPE CDHDR-USERNAME,
    UDATE     TYPE CDHDR-UDATE,
    UTIME     TYPE CDHDR-UTIME,
    ATINN     TYPE CABN-ATINN,
    ATNAM     TYPE CABN-ATNAM,
    VALUE_NEW TYPE CDPOS-VALUE_NEW,
    VALUE_OLD TYPE CDPOS-VALUE_OLD,
  END OF TY_SAIDA,

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
      V_WERKS         TYPE MCHB-WERKS.

DATA:
  L_ID       LIKE THEAD-TDID,
  L_NAME(70),
  W_OBJECT   LIKE THEAD-TDOBJECT VALUE 'MATERIAL'.




*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*

DATA:
  TG_FIELDS   TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
  TG_EDITOR   TYPE TABLE OF TY_EDITOR,
  WG_EDITOR   TYPE TY_EDITOR,
  TG_MSG_RET  TYPE TABLE OF ZFIWRS0002  WITH HEADER LINE,

  T_LINE      LIKE TLINE OCCURS 0 WITH HEADER LINE, "gravar os textos básicos

  IT_SAIDA    TYPE TABLE OF TY_SAIDA,
  WA_SAIDA    TYPE TY_SAIDA.

** INTERNAL TABLES
**----------------------------------------------------------------------
DATA: IT_INOB     TYPE TABLE OF TY_INOB,
      IT_INOB_AUX TYPE TABLE OF TY_INOB,
      IT_INOB2    TYPE TABLE OF TY_INOB2,
      IT_MCHB     TYPE TABLE OF TY_MCHB,
      IT_CDHDR    TYPE TABLE OF TY_CDHDR,
      IT_CDPOS    TYPE TABLE OF TY_CDPOS,
      IT_CABN     TYPE TABLE OF CABN.

**----------------------------------------------------------------------
** WORKAREA
DATA: WA_INOB     TYPE TY_INOB,
      WA_INOB_AUX TYPE TY_INOB,
      WA_INOB2    TYPE TY_INOB2,
      WA_MCHB     TYPE TY_MCHB,
      WA_CDHDR    TYPE TY_CDHDR,
      WA_CDPOS    TYPE TY_CDPOS,
      WA_CABN     TYPE CABN.

** VARIABLES
**----------------------------------------------------------------------
DATA: V_OBJEK       TYPE INOB-OBJEK.

***********************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
DATA: EDITCONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

      CONTAINER_1     TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1
      CONTAINER_2     TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 2
      SPLITTER        TYPE REF TO CL_GUI_SPLITTER_CONTAINER,

      EDITOR          TYPE REF TO CL_GUI_TEXTEDIT,
      CL_CONTAINER_95 TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      CL_CONTAINER_05 TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID   TYPE REF TO CL_DD_DOCUMENT,
      CL_GRID         TYPE REF TO CL_GUI_ALV_GRID,
      WA_STABLE       TYPE LVC_S_STBL,
      WA_AFIELD       TYPE LVC_S_FCAT,
      IT_FIELDCAT     TYPE LVC_T_FCAT,
      W_FIELDCAT      TYPE LVC_S_FCAT,
      I_SORT          TYPE LVC_T_SORT,
      WA_LAYOUT       TYPE LVC_S_LAYO,
      IS_STABLE       TYPE LVC_S_STBL VALUE 'XX'.

** Criação de tabela dinamica
DATA: T_FIELDCATALOG TYPE LVC_T_FCAT,
      W_FIELDCATALOG TYPE LVC_S_FCAT.

DATA: TI_BDCDATA TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      WA_BDCDATA LIKE LINE OF TI_BDCDATA,
      T_MESSTAB  TYPE TABLE OF BDCMSGCOLL.
