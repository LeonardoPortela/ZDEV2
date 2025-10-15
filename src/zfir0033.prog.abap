*&---------------------------------------------------------------------*
*& Report   ZFIR0033
*&
*&---------------------------------------------------------------------*
*&TITULO: Relatório Solicitação Adiantamento
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 01.08..2013
*&TRANSAÇÃO: ZFI0032
*&---------------------------------------------------------------------*


REPORT  ZFIR0033.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ZFIT0045.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.
DATA: WL_MODE(1).

TYPES:
   BEGIN OF TY_ZFIT0045,
      BUKRS       TYPE ZFIT0045-BUKRS,
      NRO_SOL     TYPE ZFIT0045-NRO_SOL,
      EBELN       TYPE ZFIT0045-EBELN,
      LIFNR       TYPE ZFIT0045-LIFNR,
      BELNR       TYPE ZFIT0045-BELNR,
      DT_PGTO     TYPE ZFIT0045-DT_PGTO,
      DT_PREV_LIQ TYPE ZFIT0045-DT_PREV_LIQ,
      MOEDA_PGTO  TYPE ZFIT0045-MOEDA_PGTO,
      MOTIVO      TYPE ZFIT0045-MOTIVO,
      DEP_RESP    TYPE ZFIT0045-DEP_RESP,
      RESP_NEG    TYPE ZFIT0045-RESP_NEG,
      USNAM       TYPE ZFIT0045-USNAM,
      GJAHR       TYPE BSAK-GJAHR,
      DELE(1),
   END OF TY_ZFIT0045,

   BEGIN OF TY_ZFIT0046,
      NRO_SOL   TYPE ZFIT0046-NRO_SOL,
      EBELN     TYPE ZFIT0046-EBELN,
      EBELP     TYPE ZFIT0046-EBELP,
      ANLN1     TYPE ZFIT0046-ANLN1,
      ANLN2     TYPE ZFIT0046-ANLN2,
      VLR_ADIANTAMENTO TYPE ZFIT0046-VLR_ADIANTAMENTO,
   END OF TY_ZFIT0046,

   BEGIN OF TY_BSAK,
      BUKRS     TYPE BSAK-BUKRS,
      BELNR     TYPE BSAK-BELNR,
      GJAHR     TYPE BSAK-GJAHR,
      AUGBL     TYPE BSAK-AUGBL,
      AUGDT     TYPE BSAK-AUGDT,
      HKONT     TYPE BSAK-HKONT,
      DMBTR     TYPE BSAK-DMBTR,
      DMBE2     TYPE BSAK-DMBE2,
   END OF TY_BSAK,

   BEGIN OF TY_SKA1,
      SAKNR     TYPE SKA1-SAKNR,
      TXT50     TYPE SKAT-TXT50,
   END OF TY_SKA1,

   BEGIN OF TY_LFA1,
     LIFNR          TYPE LFA1-LIFNR,
     NAME1          TYPE LFA1-NAME1,
   END OF TY_LFA1,

   BEGIN OF TY_T001,
     BUKRS          TYPE T001-BUKRS,
     BUTXT          TYPE T001-BUTXT,
   END OF TY_T001,

   BEGIN OF TY_SAIDA,
      CHECKBOX(1),
      ICON1(4),
      BUTXT             TYPE T001-BUTXT,
      NRO_SOL           TYPE ZFIT0045-NRO_SOL,
      EBELN             TYPE ZFIT0046-EBELN,
      NAME1_F           TYPE LFA1-NAME1,
      BELNR             TYPE ZFIT0045-BELNR,
      DT_PGTO           TYPE ZFIT0045-DT_PGTO,
      MOEDA_PGTO        TYPE ZFIT0045-MOEDA_PGTO,
      VLR_ADTO_R        TYPE BSAK-DMBTR,
      VLR_ADTO_U        TYPE BSAK-DMBE2,
      VLR_COMP_R        TYPE BSAK-DMBTR,
      VLR_COMP_U        TYPE BSAK-DMBE2,
      VLR_SALD_R        TYPE BSAK-DMBTR,
      VLR_SALD_U        TYPE BSAK-DMBE2,
      AUGBL             TYPE BSAK-AUGBL,
      DT_PREV_LIQ       TYPE ZFIT0045-DT_PREV_LIQ,
      DEP_RESP          TYPE ZFIT0045-DEP_RESP,
      RESP_NEG          TYPE ZFIT0045-RESP_NEG,
      USNAM             TYPE ZFIT0045-USNAM,
      MOTIVO            TYPE ZFIT0045-MOTIVO,
      ANLN1(16),
   END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: TI_BDCDATA          TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      T_MESSTAB           TYPE TABLE OF BDCMSGCOLL,

      IT_ZFIT0045         TYPE TABLE OF TY_ZFIT0045,
      IT_ZFIT0046         TYPE TABLE OF TY_ZFIT0046,
      IT_BSAK             TYPE TABLE OF TY_BSAK,
      IT_SKA1             TYPE TABLE OF TY_SKA1,
      IT_LFA1             TYPE TABLE OF TY_LFA1    ,
      IT_T001             TYPE TABLE OF TY_T001    ,

      IT_SAIDA            TYPE TABLE OF TY_SAIDA          ,
      IT_SAIDA_MAIL       TYPE TABLE OF TY_SAIDA          ,
      IT_COLOR            TYPE TABLE OF LVC_S_SCOL        ,
      TG_MSG_RET          TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
      WA_CONT   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV    TYPE REF TO CL_GUI_ALV_GRID ,
      WA_BDCDATA    LIKE LINE OF TI_BDCDATA ,

      WA_ZFIT0045         TYPE TY_ZFIT0045,
      WA_ZFIT0046         TYPE TY_ZFIT0046,
      WA_BSAK             TYPE TY_BSAK,
      WA_BSAK_CP          TYPE TY_BSAK,
      WA_BSIK             TYPE BSIK,
      WA_SKA1             TYPE TY_SKA1,
      WA_LFA1             TYPE TY_LFA1,
      WA_T001             TYPE TY_T001,
      WA_SAIDA            TYPE TY_SAIDA,
      WA_SAIDA_MAIL       TYPE TY_SAIDA.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
     IT_FCAT    TYPE TABLE OF TY_ESTRUTURA,
     S_VARIANT  TYPE DISVARIANT           , " Tabela Estrutura co
     T_TOP      TYPE SLIS_T_LISTHEADER    ,
     XS_EVENTS  TYPE SLIS_ALV_EVENT       ,
     EVENTS     TYPE SLIS_T_EVENT         ,
     GD_LAYOUT  TYPE SLIS_LAYOUT_ALV      ,
     T_PRINT    TYPE SLIS_PRINT_ALV       ,
     V_REPORT   LIKE SY-REPID             ,
     T_SORT     TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
     IT_SETLEAF LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
     ESTRUTURA  TYPE TABLE OF TY_ESTRUTURA,
     VG_I       TYPE I,
     WG_MENSAGEM(30).

DATA: OK-CODE       TYPE SY-UCOMM,
      TABIX         TYPE SY-TABIX,
      VNUM(10)    TYPE C,
      VSEQ(10)    TYPE P,
      WL_ERRO(1),
      WG_DOCUMENTO(10),
      VSTATUS(1),
      INDROW TYPE LVC_T_ROW,
      W_IND TYPE LVC_T_ROW WITH HEADER LINE,
      W_CONT TYPE I,
      W_CONTC(5),
      W_MENSAGEM(50),
      W_FLAG(1) VALUE '',
      VL_FORM      TYPE TDSFNAME    ,
      VL_NAME      TYPE RS38L_FNAM  .

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
DATA: EDITCONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      EDITOR           TYPE REF TO CL_GUI_TEXTEDIT,
      CL_CONTAINER_95  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      CL_CONTAINER_05  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID    TYPE REF TO CL_DD_DOCUMENT,
      CL_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      WA_STABLE         TYPE LVC_S_STBL,
      WA_AFIELD        TYPE LVC_S_FCAT,
      IT_FIELDCAT      TYPE LVC_T_FCAT,
      W_FIELDCAT       TYPE LVC_S_FCAT,
      I_SORT           TYPE LVC_T_SORT,
      WA_LAYOUT        TYPE LVC_S_LAYO,
      IS_STABLE        TYPE LVC_S_STBL VALUE 'XX',
      WG_REPNAME LIKE SY-REPID,
      WG_X_VARIANT LIKE DISVARIANT,
      WG_EXIT(1) TYPE C,
      WG_SAVE(1) TYPE C,
      WG_VARIANT LIKE DISVARIANT.


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
         C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE'.


************************************************************************
* D E F I N I T I O N
************************************************************************
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
             CATCH_HOTSPOT
                  FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
                     IMPORTING E_ROW_ID
                               E_COLUMN_ID
                               ES_ROW_NO.

    METHODS:
   ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
                     IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .


    METHODS:
      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
                     IMPORTING E_MODIFIED ET_GOOD_CELLS.

  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION

************************************************************************
* I M P L E M E N T A T I O N
************************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD ON_DATA_CHANGED.

  ENDMETHOD.                    "ON_DATA_CHANGED
  METHOD ON_DATA_CHANGED_FINISHED.

  ENDMETHOD.                    "on_data_changed_finisheD
  METHOD CATCH_HOTSPOT.
    IF E_ROW_ID IS NOT INITIAL.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX E_ROW_ID.
      IF E_COLUMN_ID = ''.

      ENDIF.
    ENDIF.
  ENDMETHOD.                    "CATCH_HOTSPOT



ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

DATA:       EVENT_RECEIVER   TYPE REF TO LCL_EVENT_RECEIVER.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:  P_BUKRS  FOR ZFIT0045-BUKRS NO INTERVALS NO-EXTENSION OBLIGATORY,
                 P_BUDAT  FOR  ZFIT0045-DT_PGTO OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
            F_SELECIONA_DADOS, " Form seleciona dados
            F_SAIDA, " Form de saida
            F_IMPRIME_DADOS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

  SELECT  BUKRS NRO_SOL EBELN LIFNR BELNR DT_PGTO DT_PREV_LIQ MOEDA_PGTO MOTIVO DEP_RESP RESP_NEG USNAM
    FROM ZFIT0045
    INTO TABLE IT_ZFIT0045
    WHERE BELNR NE ''
    AND STATUS  EQ 'A'
    AND BUKRS IN P_BUKRS
    AND DT_PGTO IN P_BUDAT
    AND LOEKZ   = ''.

  LOOP AT IT_ZFIT0045 INTO WA_ZFIT0045.
    TABIX = SY-TABIX.
    WA_ZFIT0045-GJAHR = WA_ZFIT0045-DT_PGTO+0(4).
    MODIFY IT_ZFIT0045 FROM WA_ZFIT0045 INDEX TABIX TRANSPORTING GJAHR.
    SELECT SINGLE *
      FROM BSIK
      INTO WA_BSIK
      WHERE BUKRS	=	WA_ZFIT0045-BUKRS
      AND   BELNR = WA_ZFIT0045-BELNR
      AND   GJAHR = WA_ZFIT0045-GJAHR.
    IF SY-SUBRC = 0.
      WA_ZFIT0045-DELE = 'X'.
      MODIFY IT_ZFIT0045 FROM WA_ZFIT0045 INDEX TABIX TRANSPORTING DELE.
    ENDIF.
  ENDLOOP.
  DELETE IT_ZFIT0045 WHERE DELE = 'X'.

  IF IT_ZFIT0045[] IS INITIAL.
    EXIT.
  ENDIF.


  SELECT  NRO_SOL EBELN  EBELP ANLN1 ANLN2 VLR_ADIANTAMENTO
    FROM ZFIT0046
    INTO TABLE IT_ZFIT0046
    FOR ALL ENTRIES IN IT_ZFIT0045
    WHERE NRO_SOL EQ IT_ZFIT0045-NRO_SOL.

  SELECT BUKRS BELNR GJAHR AUGBL AUGDT HKONT DMBTR DMBE2
    FROM BSAK
    INTO TABLE IT_BSAK
    FOR ALL ENTRIES IN IT_ZFIT0045
    WHERE BUKRS	=	IT_ZFIT0045-BUKRS
    AND   BELNR	=	IT_ZFIT0045-BELNR
    AND   GJAHR	=	IT_ZFIT0045-GJAHR.

  SELECT LIFNR NAME1
    FROM LFA1
    INTO TABLE IT_LFA1
    FOR ALL ENTRIES IN IT_ZFIT0045
    WHERE LIFNR EQ IT_ZFIT0045-LIFNR.

  SELECT BUKRS BUTXT
    FROM T001
    INTO TABLE IT_T001
    FOR ALL ENTRIES IN IT_ZFIT0045
    WHERE BUKRS EQ IT_ZFIT0045-BUKRS.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .
  SORT : IT_ZFIT0045 BY BUKRS NRO_SOL EBELN,
         IT_ZFIT0046 BY NRO_SOL EBELN EBELP,
         IT_BSAK     BY BUKRS BELNR GJAHR,
         IT_LFA1     BY LIFNR,
         IT_SKA1     BY SAKNR,
         IT_T001     BY BUKRS.

  DATA:  XCP_ADTO_R  TYPE BSAK-DMBTR,
         XCP_ADTO_U  TYPE BSAK-DMBE2,
         XVL_ADTO_R  TYPE BSAK-DMBTR,
         XVL_ADTO_U  TYPE BSAK-DMBE2,
         V_AUGBL     TYPE BSAK-AUGBL,
         V_AUGDT     TYPE BSAK-AUGDT.


  LOOP AT IT_ZFIT0045 INTO WA_ZFIT0045.
    WA_SAIDA-CHECKBOX           = ''.
    IF SY-DATUM GT WA_ZFIT0045-DT_PREV_LIQ .
      WA_SAIDA-ICON1   = ICON_ALERT.
    ENDIF.
    READ TABLE IT_T001 INTO WA_T001 WITH KEY BUKRS = WA_ZFIT0045-BUKRS BINARY SEARCH.
    CONCATENATE WA_T001-BUKRS '-' WA_T001-BUTXT INTO  WA_SAIDA-BUTXT.

    WA_SAIDA-NRO_SOL            = WA_ZFIT0045-NRO_SOL.


    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_ZFIT0045-LIFNR BINARY SEARCH.
    CONCATENATE WA_LFA1-LIFNR '-' WA_LFA1-NAME1 INTO WA_SAIDA-NAME1_F.

    WA_SAIDA-BELNR              = WA_ZFIT0045-BELNR.
    WA_SAIDA-DT_PGTO            = WA_ZFIT0045-DT_PGTO.
    WA_SAIDA-MOEDA_PGTO         = WA_ZFIT0045-MOEDA_PGTO.
    WA_SAIDA-DEP_RESP           = WA_ZFIT0045-DEP_RESP.
    WA_SAIDA-RESP_NEG           = WA_ZFIT0045-RESP_NEG.
    WA_SAIDA-USNAM              = WA_ZFIT0045-USNAM.
    WA_SAIDA-MOTIVO             = WA_ZFIT0045-MOTIVO.
    WA_SAIDA-DT_PREV_LIQ        = WA_ZFIT0045-DT_PREV_LIQ.

    WA_SAIDA-EBELN              = WA_ZFIT0045-EBELN.
    XCP_ADTO_R = 0.
    XCP_ADTO_U = 0.
    XVL_ADTO_R = 0.
    XVL_ADTO_U = 0.
    LOOP AT IT_BSAK INTO WA_BSAK WHERE BUKRS = WA_ZFIT0045-BUKRS
                                 AND   BELNR = WA_ZFIT0045-BELNR
                                 AND   GJAHR = WA_ZFIT0045-GJAHR.
      XVL_ADTO_R = WA_BSAK-DMBTR.
      XVL_ADTO_U = WA_BSAK-DMBE2.
      IF WA_BSAK-AUGBL IS NOT INITIAL.
        V_AUGBL = WA_BSAK-AUGBL. " Primeira compensação
        V_AUGDT = WA_BSAK-AUGDT. " Primeira compensação
        XCP_ADTO_R = 0.
        XCP_ADTO_U = 0.
        WHILE V_AUGBL IS NOT INITIAL.
          SELECT SINGLE BUKRS BELNR GJAHR AUGBL AUGDT HKONT DMBTR DMBE2
           FROM BSAK
           INTO WA_BSAK_CP
           WHERE BUKRS  = WA_ZFIT0045-BUKRS
           AND   BELNR  = V_AUGBL
           AND   GJAHR  = V_AUGDT+0(4).
          IF SY-SUBRC = 0.
            ADD WA_BSAK_CP-DMBTR TO XCP_ADTO_R.
            ADD WA_BSAK_CP-DMBE2 TO XCP_ADTO_U.
          ENDIF.
          V_AUGBL = WA_BSAK_CP-AUGBL. " compensação seguinte
          V_AUGDT = WA_BSAK_CP-AUGDT. " compensação seguinte
        ENDWHILE.
      ENDIF.
      WA_SAIDA-VLR_ADTO_R = XVL_ADTO_R.
      WA_SAIDA-VLR_ADTO_U = XVL_ADTO_U.
      WA_SAIDA-VLR_COMP_R = XCP_ADTO_R.
      WA_SAIDA-VLR_COMP_U = XCP_ADTO_U.
      WA_SAIDA-VLR_SALD_R = XVL_ADTO_R - XCP_ADTO_R.
      WA_SAIDA-VLR_SALD_U = XVL_ADTO_U - XCP_ADTO_U.
      WA_SAIDA-AUGBL      = WA_BSAK-AUGBL.
      APPEND WA_SAIDA TO IT_SAIDA.
    ENDLOOP.
    CLEAR WA_SAIDA.
  ENDLOOP.


ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .

  PERFORM F_ALV_FIELDCAT.

  WA_LAYOUT-ZEBRA      = 'X'.
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.
  WA_LAYOUT-GRID_TITLE = 'Relatório Solicitação Adiantamento'.
  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT   = 'X'.

  CALL SCREEN 0100.
ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .

  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_SAIDA'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CHECKBOX'.
  WA_AFIELD-CHECKBOX      = 'X'.
  WA_AFIELD-SCRTEXT_S = 'Chk'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = 'X'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ICON1'.
  WA_AFIELD-ICON          = 'X'.
  WA_AFIELD-SCRTEXT_S = 'St.Ped.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BUTXT'.
  WA_AFIELD-SCRTEXT_S = 'Empresa'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NRO_SOL'.
  WA_AFIELD-SCRTEXT_S = 'Nro.Sol.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'EBELN'.
  WA_AFIELD-SCRTEXT_S = 'Ped.Compra'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NAME1_F'.
  WA_AFIELD-SCRTEXT_S = 'Fornecedor'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BELNR'.
  WA_AFIELD-SCRTEXT_S = 'Doc.Adto'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DT_PGTO'.
  WA_AFIELD-SCRTEXT_S = 'Dt.Pgto'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MOEDA_PGTO'.
  WA_AFIELD-SCRTEXT_S = 'Moeda Pgto'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_ADTO_R'.
  WA_AFIELD-SCRTEXT_S = 'Vlr.Adto R$'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_ADTO_U'.
  WA_AFIELD-SCRTEXT_S = 'Vlr.Adto US$'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_COMP_R'.
  WA_AFIELD-SCRTEXT_S = 'Vlr.Comp.R$'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_COMP_U'.
  WA_AFIELD-SCRTEXT_S = 'Vlr.Comp.US$'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_SALD_R'.
  WA_AFIELD-SCRTEXT_S = 'Saldo R$'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_SALD_U'.
  WA_AFIELD-SCRTEXT_S = 'Saldo US$'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DT_PREV_LIQ'.
  WA_AFIELD-SCRTEXT_S = 'Dt.Prev.Liquid.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DEP_RESP'.
  WA_AFIELD-SCRTEXT_S = 'Departamento'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'RESP_NEG'.
  WA_AFIELD-SCRTEXT_S = 'Resp.Negociação'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'USNAM'.
  WA_AFIELD-SCRTEXT_S = 'Solicitante'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MOTIVO'.
  WA_AFIELD-SCRTEXT_S = 'Motivo'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*  I = I + 1.
*  CLEAR WA_AFIELD.
*  WA_AFIELD-COL_POS       = I.
*  WA_AFIELD-FIELDNAME     = 'ANLN1'.
*  WA_AFIELD-SCRTEXT_S = 'Imobilizado'.
*  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
*  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
*  WA_AFIELD-EDIT          = ''.
*  WA_AFIELD-KEY           = ''.
*  APPEND WA_AFIELD TO IT_FIELDCAT.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.

  SET PF-STATUS 'F_SET_PF' EXCLUDING FCODE.
  SET TITLEBAR  'ZFTITLE'.


  IF CL_CONTAINER_95 IS INITIAL.
    CREATE OBJECT CL_CONTAINER_95
      EXPORTING
        SIDE  = '4'
        RATIO = '80'.
  ENDIF.

  IF NOT CL_GRID IS INITIAL.

    PERFORM ZF_ALV_HEADER.
    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT OBJ_DYNDOC_ID
      EXPORTING
*    STYLE  =
*    BACKGROUND_COLOR =
*    BDS_STYLESHEET =
        NO_MARGINS = 'X'.

    PERFORM ZF_ALV_HEADER .


    IF EDITCONTAINER IS INITIAL .
      CREATE OBJECT EDITCONTAINER
        EXPORTING
          CONTAINER_NAME = 'HEADER'.
    ENDIF .

    CALL METHOD OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    CALL METHOD OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.


    CREATE OBJECT CL_GRID
      EXPORTING
        I_PARENT = CL_CONTAINER_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    WG_SAVE = 'X'.
    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    WA_STABLE-ROW        = C_X.
    WG_X_VARIANT-REPORT  = SY-REPID.
    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT      = WG_X_VARIANT
        IS_LAYOUT       = WA_LAYOUT
        I_SAVE          = WG_SAVE
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCAT[]
        IT_SORT         = I_SORT[]
        IT_OUTTAB       = IT_SAIDA[].


    CREATE OBJECT EVENT_RECEIVER.
    SET HANDLER EVENT_RECEIVER->CATCH_HOTSPOT           FOR CL_GRID.
*    SET HANDLER EVENT_RECEIVER->ON_DATA_CHANGED_FINISHED   FOR CL_GRID.



  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ALV_HEADER .
  DATA:   WL_DATA(10),
              WL_HORA(8),
              WL_LINHA(60),
              WL_TEXT TYPE SDYDO_TEXT_ELEMENT.

*  WL_TEXT = ''.
*
*  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
*    EXPORTING
*      TEXT         = WL_TEXT
*      SAP_STYLE    = CL_DD_AREA=>HEADING
*      SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.



  IF P_BUKRS   IS NOT INITIAL.
    CONCATENATE 'Empresa  :' P_BUKRS-LOW
      INTO WL_LINHA SEPARATED BY SPACE.

    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*        SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF P_BUDAT IS NOT INITIAL.
    CONCATENATE P_BUDAT-LOW+6(2) P_BUDAT-LOW+4(2) P_BUDAT-LOW+0(4) INTO  WL_DATA SEPARATED BY '.'.
    IF P_BUDAT-HIGH IS INITIAL.
      CONCATENATE 'Dt. Requisição  :' WL_DATA
      INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Dt. Requisição :' WL_DATA  INTO WL_LINHA SEPARATED BY SPACE.
      CONCATENATE P_BUDAT-HIGH+6(2) P_BUDAT-HIGH+4(2) P_BUDAT-HIGH+0(4) INTO  WL_DATA SEPARATED BY '.'.
      CONCATENATE WL_LINHA 'à' WL_DATA  INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*         SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.


ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP'.
      REFRESH IT_SAIDA.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'E-MAIL'.
      IT_SAIDA_MAIL[] = IT_SAIDA[].
      DELETE IT_SAIDA_MAIL WHERE CHECKBOX NE 'X'.
      SORT IT_SAIDA_MAIL  BY RESP_NEG.
      DELETE ADJACENT DUPLICATES FROM IT_SAIDA_MAIL COMPARING RESP_NEG.
      LOOP AT IT_SAIDA_MAIL INTO WA_SAIDA.
        PERFORM GERA_PDF USING WA_SAIDA.
      ENDLOOP.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GERA_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GERA_PDF USING WA_LINHA LIKE WA_SAIDA.
  DATA: LS_CONTROL            TYPE SSFCTRLOP,
        LS_OPTIONS            TYPE SSFCOMPOP,
        JOB_OUTPUT_INFO       TYPE SSFCRESCL,
        LS_XSFPARAM_LINE      TYPE SSFXSFP,
        V_BIN_FILESIZE TYPE I,
        IT_DOCS  TYPE STANDARD TABLE OF DOCS,
        IT_LINES TYPE STANDARD TABLE OF TLINE,
         LV_FNAME TYPE RS38L_FNAM ,
      LV_MAIL_RECIPIENT TYPE SWOTOBJID ,
      LV_MAIL_SENDER    TYPE SWOTOBJID ,
      LV_CONTROL        TYPE SSFCTRLOP ,
      LV_NAME           TYPE SO_NAME   ,
      LV_OUTPUT         TYPE SSFCOMPOP ,
      WL_ZMENG(20),
      WL_DMBTR(20),
      WL_VLRTOT(20).

  DATA: I_OTF TYPE ITCOO OCCURS 0 WITH HEADER LINE,
  I_TLINE TYPE TABLE OF TLINE WITH HEADER LINE,
  I_RECEIVERS TYPE TABLE OF SOMLRECI1 WITH HEADER LINE,
  I_RECORD LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
* Objects to send mail.
  I_OBJPACK LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
  I_OBJTXT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
  I_OBJBIN LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
  I_RECLIST LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
* Work Area declarations
  WA_OBJHEAD TYPE SOLI_TAB,
  W_CTRLOP TYPE SSFCTRLOP,
  W_COMPOP TYPE SSFCOMPOP,
  W_RETURN TYPE SSFCRESCL,
  WA_DOC_CHNG TYPE SODOCCHGI1,
  W_DATA TYPE SODOCCHGI1,
  WA_BUFFER TYPE STRING, "To convert from 132 to 255
* Variables declarations
  V_FORM_NAME TYPE RS38L_FNAM,
  V_LEN_IN LIKE SOOD-OBJLEN,
  V_LEN_OUT LIKE SOOD-OBJLEN,
  V_LEN_OUTN TYPE I,
  V_LINES_TXT TYPE I,
  V_LINES_BIN TYPE I.

  VL_FORM = 'ZFIR0001'.
*
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = VL_FORM
    IMPORTING
      FM_NAME            = VL_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  Impresora
  LS_CONTROL-NO_DIALOG = 'X'. "Evita la pantalla de opciones de salida del formulario
  LS_OPTIONS-TDDEST   = 'LOCL'.
  LS_OPTIONS-TDIMMED  = C_X.
  LS_OPTIONS-TDNEWID  = C_X.
  LS_OPTIONS-TDNOARCH = C_X.

  LS_CONTROL-PREVIEW = SPACE.
  LS_CONTROL-DEVICE  = 'PRINTER'.
  LS_CONTROL-GETOTF  = 'X'.

  CLEAR:JOB_OUTPUT_INFO.
  CALL FUNCTION VL_NAME
    EXPORTING
      USER_SETTINGS      = ' '
      CONTROL_PARAMETERS = LS_CONTROL
      OUTPUT_OPTIONS     = LS_OPTIONS
      I_BUKRS            = P_BUKRS-LOW
      I_DATA_I           = P_BUDAT-LOW
      I_DATA_F           = P_BUDAT-HIGH
      I_RESP             = WA_LINHA-RESP_NEG
    IMPORTING
      JOB_OUTPUT_INFO    = JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ELSE.
*--- Convert OTF to PDF
*      CALL FUNCTION 'CONVERT_OTF_2_PDF'
*        IMPORTING
*          BIN_FILESIZE           = V_BIN_FILESIZE
*        TABLES
*          OTF                    = JOB_OUTPUT_INFO-OTFDATA
*          DOCTAB_ARCHIVE         = IT_DOCS
*          LINES                  = IT_LINES
*        EXCEPTIONS
*          ERR_CONV_NOT_POSSIBLE  = 1
*          ERR_OTF_MC_NOENDMARKER = 2
*          OTHERS                 = 3.
*      IF SY-SUBRC <> 0.
**      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
    I_OTF[] = JOB_OUTPUT_INFO-OTFDATA[].
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        FORMAT                = 'PDF'
        MAX_LINEWIDTH         = 132
      IMPORTING
        BIN_FILESIZE          = V_BIN_FILESIZE
      TABLES
        OTF                   = I_OTF
        LINES                 = I_TLINE
      EXCEPTIONS
        ERR_MAX_LINEWIDTH     = 1
        ERR_FORMAT            = 2
        ERR_CONV_NOT_POSSIBLE = 3
        OTHERS                = 4.
    IF SY-SUBRC EQ 0.
    ENDIF.
    LOOP AT I_TLINE.
      TRANSLATE I_TLINE USING '~'.
      CONCATENATE WA_BUFFER I_TLINE INTO WA_BUFFER.
    ENDLOOP.
    TRANSLATE WA_BUFFER USING '~'.
    DO.
      I_RECORD = WA_BUFFER.
      APPEND I_RECORD.
      SHIFT WA_BUFFER LEFT BY 255 PLACES.
      IF WA_BUFFER IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
* Attachment
    REFRESH: I_RECLIST,
    I_OBJTXT,
    I_OBJBIN,
    I_OBJPACK.
    CLEAR WA_OBJHEAD.
    I_OBJBIN[] = I_RECORD[].
* Create Message Body Title and Description
    I_OBJTXT = 'Segue em anexo relatório dos adiantamentos de sua responsabilidade pendentes de compensação'.
    APPEND I_OBJTXT.

    DESCRIBE TABLE I_OBJTXT LINES V_LINES_TXT.
    READ TABLE I_OBJTXT INDEX V_LINES_TXT.
    WA_DOC_CHNG-OBJ_NAME = 'smartform'.
    WA_DOC_CHNG-EXPIRY_DAT = SY-DATUM + 10.
    WA_DOC_CHNG-OBJ_DESCR = 'Relatório de Solicitação de Adiantamento'.
*     = 'smartform'.
    WA_DOC_CHNG-SENSITIVTY = 'F'.
    WA_DOC_CHNG-DOC_SIZE = V_LINES_TXT * 255.
* Main Text
    CLEAR I_OBJPACK-TRANSF_BIN.
    I_OBJPACK-HEAD_START = 1.
    I_OBJPACK-HEAD_NUM = 0.
    I_OBJPACK-BODY_START = 1.
    I_OBJPACK-BODY_NUM = V_LINES_TXT.
    I_OBJPACK-DOC_TYPE = 'RAW'.
    APPEND I_OBJPACK.
* Attachment (pdf-Attachment)
    I_OBJPACK-TRANSF_BIN = 'X'.
    I_OBJPACK-HEAD_START = 1.
    I_OBJPACK-HEAD_NUM = 0.
    I_OBJPACK-BODY_START = 1.
    DESCRIBE TABLE I_OBJBIN LINES V_LINES_BIN.
    READ TABLE I_OBJBIN INDEX V_LINES_BIN.
    I_OBJPACK-DOC_SIZE = V_LINES_BIN * 255 .
    I_OBJPACK-BODY_NUM = V_LINES_BIN.
    I_OBJPACK-DOC_TYPE = 'PDF'.
    I_OBJPACK-OBJ_NAME = 'smart'.
    I_OBJPACK-OBJ_DESCR = 'ADIANTAMENTO.PDF'.
    APPEND I_OBJPACK.
    CLEAR I_RECLIST.

    DATA: BSMTP_ADDR TYPE ADR6-SMTP_ADDR.

    SELECT SINGLE ADR6~SMTP_ADDR INTO BSMTP_ADDR
    FROM USR21
      INNER JOIN ADR6
         ON  USR21~ADDRNUMBER = ADR6~ADDRNUMBER
        AND USR21~PERSNUMBER = ADR6~PERSNUMBER
            WHERE USR21~BNAME = WA_LINHA-RESP_NEG.

    IF SY-SUBRC = 0.
      I_RECLIST-RECEIVER = BSMTP_ADDR.
      I_RECLIST-REC_TYPE = 'U'.
      APPEND I_RECLIST.
    ENDIF.

    SELECT SINGLE ADR6~SMTP_ADDR INTO BSMTP_ADDR
    FROM USR21
      INNER JOIN ADR6
         ON  USR21~ADDRNUMBER = ADR6~ADDRNUMBER
        AND USR21~PERSNUMBER = ADR6~PERSNUMBER
            WHERE USR21~BNAME = WA_LINHA-USNAM.
    IF SY-SUBRC = 0.
      I_RECLIST-RECEIVER = BSMTP_ADDR.
      I_RECLIST-REC_TYPE = 'U'.                    "Define email externo
      APPEND I_RECLIST.
    ENDIF.

    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        DOCUMENT_DATA              = WA_DOC_CHNG
        PUT_IN_OUTBOX              = 'X'
        COMMIT_WORK                = 'X'
      TABLES
        PACKING_LIST               = I_OBJPACK
        OBJECT_HEADER              = WA_OBJHEAD
        CONTENTS_BIN               = I_OBJBIN
        CONTENTS_TXT               = I_OBJTXT
        RECEIVERS                  = I_RECLIST
      EXCEPTIONS
        TOO_MANY_RECEIVERS         = 1
        DOCUMENT_NOT_SENT          = 2
        DOCUMENT_TYPE_NOT_EXIST    = 3
        OPERATION_NO_AUTHORIZATION = 4
        PARAMETER_ERROR            = 5
        X_ERROR                    = 6
        ENQUEUE_ERROR              = 7
        OTHERS                     = 8.
    IF SY-SUBRC NE 0.
*WRITE:/ ‘Error When Sending the File’, SY-SUBRC.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Ocorreu um erro ao enviar o e-mail'.
    ELSE.
      MESSAGE S836(SD) WITH 'E-mail enviado com sucesso'.
*WRITE:/ ‘Mail sent’.
    ENDIF.

  ENDIF.
ENDFORM.                    " GERA_PDF
