*&---------------------------------------------------------------------*
*& Include ZMMR112_TOP                                       PoolMóds.        ZMMR112
*&
*&---------------------------------------------------------------------*
PROGRAM ZMMR112.
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
DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.


TYPES:  BEGIN OF TY_CADMAT2,
          WERKS       TYPE  ZMMT0069-WERKS,
          DT_ENTRADA  TYPE  ZMMT0069-DT_ENTRADA,
          DT_FINAL    TYPE  ZMMT0069-DT_ENTRADA,
          NOVOS(1),
          FINAL(1),
          RECUSA_S(1),
          HOM_SUP     TYPE  ZMMT0069-HOMOLOG,
          HOM_SSO     TYPE  ZMMT0069-HOMOLOG,
          HOM_EPI     TYPE  ZMMT0069-HOMOLOG,
          HOM_TI      TYPE  ZMMT0069-HOMOLOG,
        END OF TY_CADMAT2,

        BEGIN OF TY_CADMAT,
          NUREQ       TYPE  ZMMT0069-NUREQ,
          WERKS       TYPE  ZMMT0069-WERKS,
          VKORG       TYPE  ZMMT0069-VKORG,
          VTWEG       TYPE  ZMMT0069-VTWEG,
          EXPANSAO    TYPE  ZMMT0069-EXPANSAO,
          MATNR       TYPE  ZMMT0069-MATNR, "material equivalente no SAP
          MAKTX       TYPE  ZMMT0069-MAKTX, "Descrição breve sugerida
          MBRSH       TYPE  MARA-MBRSH,
          MBBEZ       TYPE  T137T-MBBEZ,
          MTART       TYPE  MARA-MTART,
          MTBEZ       TYPE  T134T-MTBEZ,
          MATKL       TYPE  ZMMT0069-MATKL, "grupo SAP sugerido
          WGBEZ       TYPE  T023T-WGBEZ,
          MEINS       TYPE  ZMMT0069-MEINS, "Unid medida Sugerida
          MATNRG      TYPE  MARA-MATNR, "codigo SAP gerado
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
          DT_HOMOL    TYPE  ZMMT0069-DT_CRIACAO,
          HR_HOMOL    TYPE  ZMMT0069-HR_CRIACAO,
          HOMOLOGA    TYPE  ZMMT0069-HOMOLOGA,
          DT_HOMOLF   TYPE  ZMMT0069-DT_HOMOLF,
          HR_HOMOLF   TYPE  ZMMT0069-HR_HOMOLF,
          HOMOLOGAF   TYPE  ZMMT0069-HOMOLOGAF,
          EPI_CA      TYPE  ZMMT0069-EPI_CA,
          EPI_PERI    TYPE  ZMMT0069-EPI_PERI,
          EPI_VAL     TYPE  ZMMT0069-EPI_VAL,
          RECUSA_FLAG TYPE  ZMMT0069-RECUSA_FLAG,
        END OF TY_CADMAT,

        BEGIN OF TY_SAIDA,
          ICON(4),
          CHECKBOX(1),
          NUREQ       TYPE  ZMMT0069-NUREQ,
          WERKS       TYPE  ZMMT0069-WERKS,
          VKORG       TYPE  ZMMT0069-VKORG,
          VTWEG       TYPE  ZMMT0069-VTWEG,
          EXPANSAO    TYPE  ZMMT0069-EXPANSAO,
          MATNR       TYPE  ZMMT0069-MATNR,
          MAKTX       TYPE  ZMMT0069-MAKTX,
          MATKL       TYPE  ZMMT0069-MATKL,
          MEINS       TYPE  ZMMT0069-MEINS,
          MATNRG      TYPE  ZMMT0069-MATNRG,
          CRIADO      TYPE  ZMMT0069-CRIADO,
          DT_CRIACAO  TYPE  ZMMT0069-DT_CRIACAO,
          HR_CRIACAO  TYPE  ZMMT0069-HR_CRIACAO,
          SOLIC       TYPE  ZMMT0069-SOLIC,
          USNAM       TYPE  ZMMT0069-USNAM,
          DT_ENTRADA  TYPE  ZMMT0069-DT_ENTRADA,
          HR_ENTRADA  TYPE  ZMMT0069-HR_ENTRADA,
          RECUSA_FLAG TYPE  ZMMT0069-RECUSA_FLAG,
        END OF TY_SAIDA,

        BEGIN OF TYPE_MCHB,
          MATNR TYPE MCHB-MATNR,
          WERKS TYPE MCHB-WERKS,
          LGORT TYPE MCHB-LGORT,
          CHARG TYPE MCHB-CHARG,
          CLABS TYPE MCHB-CLABS,
          OBJEK TYPE INOB-OBJEK,
        END   OF TYPE_MCHB,

        BEGIN OF TYPE_INOB,
          CUOBJ  TYPE INOB-CUOBJ,
          KLART  TYPE INOB-KLART,
          OBTAB  TYPE INOB-OBTAB,
          OBJEK  TYPE INOB-OBJEK,
          OBJEKK TYPE KSSK-OBJEK,
        END   OF TYPE_INOB,

        BEGIN OF TYPE_KSSK,
          OBJEK TYPE KSSK-OBJEK,
          MAFID TYPE KSSK-MAFID,
          KLART TYPE KSSK-KLART,
          CLINT TYPE KSSK-CLINT,
          ADZHL TYPE KSSK-ADZHL,
        END   OF TYPE_KSSK,

        BEGIN OF TYPE_KLAH,
          CLINT TYPE KLAH-CLINT,
          KLART TYPE KLAH-KLART,
          CLASS TYPE KLAH-CLASS,
          VONDT TYPE KLAH-VONDT,
          BISDT TYPE KLAH-BISDT,
        END   OF TYPE_KLAH,

        BEGIN OF TYPE_KSML,
          CLINT TYPE KSML-CLINT,
          POSNR TYPE KSML-POSNR,
          ADZHL TYPE KSML-ADZHL,
          IMERK TYPE KSML-IMERK,
          KLART TYPE KSML-KLART,
        END   OF TYPE_KSML,

        BEGIN OF TYPE_CABNT,
          ATINN TYPE CABNT-ATINN,
          SPRAS TYPE CABNT-SPRAS,
          ADZHL TYPE CABNT-ADZHL,
          ATBEZ TYPE CABNT-ATBEZ,
          ATNAM TYPE CABN-ATNAM,
          ATFOR TYPE CABN-ATFOR,
        END   OF TYPE_CABNT,


        BEGIN OF TYPE_RMCLM,
          KLART TYPE TCLA-KLART,
          CUOBJ TYPE INOB-CUOBJ,
          CLINT TYPE KSSK-CLINT,
          CLASS TYPE KLAH-CLASS,
          VONDT TYPE KLAH-VONDT,
          BISDT TYPE KLAH-BISDT,
        END   OF TYPE_RMCLM,


        BEGIN OF TYPE_MSG,
          MATERIAL TYPE MCHB-MATNR,
          LOTE     TYPE MCHB-CHARG,
          CENTRO   TYPE MCHB-WERKS,
          TIPO     TYPE CHAR1,
          NUMERO   TYPE ZTFARDOSMSG-NUMERO,
          MENSAGEM TYPE CHAR40,
        END   OF TYPE_MSG,

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
      WG_ACAO(30),
      WG_GER_WF       TYPE ZMMT0069-GER_WF.

DATA:
  L_ID       LIKE THEAD-TDID,
  L_NAME(70),
  W_OBJECT   LIKE THEAD-TDOBJECT VALUE 'MATERIAL'.

DATA:         WA_T134T TYPE T134T,
              WA_T137T TYPE T137T,
              WA_T023T TYPE T023T,
              WL_LOG   TYPE ZMMT0069_LOG.

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

DATA: WG_CADMAT   TYPE TY_CADMAT,
      WG_CADMAT2  TYPE TY_CADMAT2,
      TG_FIELDS   TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
      TG_EDITOR   TYPE TABLE OF TY_EDITOR,
      WG_EDITOR   TYPE TY_EDITOR,
      TG_MSG_RET  TYPE TABLE OF ZFIWRS0002  WITH HEADER LINE,

      IT_ZMMT0069 TYPE TABLE OF ZMMT0069 WITH HEADER LINE,

      IT_ZMMT0078 TYPE TABLE OF ZMMT0078,
      WA_ZMMT0078 TYPE ZMMT0078,
      T_LINE      LIKE TLINE OCCURS 0 WITH HEADER LINE, "gravar os textos básicos

      IT_SAIDA    TYPE TABLE OF TY_SAIDA,
      WA_SAIDA    TYPE TY_SAIDA.

DATA: T_SET TYPE STANDARD TABLE OF SETLEAF  WITH HEADER LINE.
DATA: T_LAY TYPE STANDARD TABLE OF SETLINET WITH HEADER LINE.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas CLASSIFICACAO MATERIAIS         &*
*&--------------------------------------------------------------------&*

DATA: T_MCHB   TYPE TABLE OF TYPE_MCHB,
      W_MCHB   TYPE TYPE_MCHB,
      T_INOB   TYPE TABLE OF TYPE_INOB,
      T_KSSK   TYPE TABLE OF TYPE_KSSK,
      T_KLAH   TYPE TABLE OF TYPE_KLAH,
      T_MSG    TYPE TABLE OF TYPE_MSG,
      T_KSML   TYPE TABLE OF TYPE_KSML,
      T_CABNT  TYPE TABLE OF TYPE_CABNT,
      ST_RMCLM TYPE TYPE_RMCLM,
      SL_MSG   TYPE TYPE_MSG.

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
