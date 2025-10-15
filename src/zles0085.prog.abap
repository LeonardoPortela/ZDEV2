*&---------------------------------------------------------------------*
*& Report  ZLES0085
*&
*&---------------------------------------------------------------------*
*&TITULO: Relatório Fisico x Fiscal - Diário  ( Aquaviário )
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 05.11.2013
*TRANSACAO: ZLES0083
*&---------------------------------------------------------------------*

REPORT  ZLES0085.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS,
            VRM.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ZSDT0001, LFA1, J_1BBRANCH, ZLEST0114.



*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF TY_J_1BNFDOC,
    DOCNUM TYPE J_1BNFDOC-DOCNUM,
    PSTDAT TYPE J_1BNFDOC-PSTDAT,
  END OF TY_J_1BNFDOC,

  BEGIN OF TY_SAIDA,
    DT_MOVIMENTO TYPE J_1BNFDOC-PSTDAT, " DATA MOVIMENTO
*     ENT_FISICO    TYPE ZSDT0001-PESO_LIQ, "FISICO – ENTRADA
*     SAI_FISICO    TYPE ZSDT0001-PESO_LIQ, "FISICO – SAIDA
*     SAL_FISICO    TYPE ZSDT0001-PESO_LIQ, "FISICO – SALDO
*     ENT_FISCAL    TYPE ZSDT0001-PESO_FISCAL, "FISCAL -  ENTRADA
*     SAI_FISCAL    TYPE ZSDT0001-PESO_FISCAL, "FISCAL – SAIDA
*     SAL_FISCAL    TYPE ZSDT0001-PESO_FISCAL, "FISCAL – SALDO

    ENT_FISICO   TYPE BRGEW, "FISICO – ENTRADA
    SAI_FISICO   TYPE BRGEW, "FISICO – SAIDA
    SAL_FISICO   TYPE BRGEW, "FISICO – SALDO
    ENT_FISCAL   TYPE BRGEW, "FISCAL -  ENTRADA
    SAI_FISCAL   TYPE BRGEW, "FISCAL – SAIDA
    SAL_FISCAL   TYPE BRGEW, "FISCAL – SALDO

    ENT_LIQRET   TYPE BRGEW, "Peso Liq.Ret - Entrada
    SAI_LIQRET   TYPE BRGEW, "Peso Liq.Ret - Saida
    SAL_LIQRET   TYPE BRGEW, "Peso Liq.Ret - Saldo

    SEPARA(02),
    NR_ROMANEIO  TYPE ZSDT0001-NR_ROMANEIO,
    BUKRS        TYPE ZSDT0001-BUKRS,
    BRANCH       TYPE ZSDT0001-BRANCH,
    NR_SAFRA     TYPE ZSDT0001-NR_SAFRA,
    DOCNUM       TYPE J_1BNFDOC-DOCNUM,
    MATNR        TYPE ZSDT0001-MATNR,

    DOCDAT       TYPE ZSDT0001-DOCDAT,
    NFNUM        TYPE ZSDT0001-NFNUM,
    NETWR        TYPE ZSDT0001-NETWR,
    PARID        TYPE ZSDT0001-PARID,
  END OF TY_SAIDA,

  BEGIN OF TY_SAIDA_ENTR_FISCAL,
    DT_MOVIMENTO  TYPE ZSDT0001-DT_MOVIMENTO,
    NR_ROMANEIO   TYPE ZSDT0001-NR_ROMANEIO,
    DOCDAT        TYPE ZSDT0001-DOCDAT,
    NFNUM         TYPE ZSDT0001-NFNUM,
    PESO_SUBTOTAL TYPE BRGEW,
    NETWR         TYPE P DECIMALS 2, "ZSDT0001-NETWR,
    PESO_FISCAL   TYPE BRGEW,
    PESO_LIQRET   TYPE BRGEW,
    PARID         TYPE ZSDT0001-PARID,
  END OF TY_SAIDA_ENTR_FISCAL,

  BEGIN OF TY_SAIDA_FISCAL,
    DT_MOVIMENTO  TYPE ZSDT0001-DT_MOVIMENTO,
    NR_ROMANEIO   TYPE ZSDT0001-NR_ROMANEIO,
    DOCDAT        TYPE ZSDT0001-DOCDAT,
    NFNUM         TYPE ZSDT0001-NFNUM,
    PESO_SUBTOTAL TYPE BRGEW,
    NETWR         TYPE P DECIMALS 2, "ZSDT0001-NETWR,
    PESO_FISCAL   TYPE BRGEW,
    PESO_LIQRET   TYPE BRGEW,
    PARID         TYPE ZSDT0001-PARID,
    DOCNUM        TYPE J_1BNFDOC-DOCNUM,
  END OF TY_SAIDA_FISCAL.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.


TYPES: BEGIN OF TY_LOC_REC,
         LOCAL_DESCARGA  TYPE ZLEST0114-LOCAL_DESCARGA,
         DESC_LOCAL_DESC TYPE ZLEST0114-DESC_LOCAL_DESC,
       END OF TY_LOC_REC.

TYPES: BEGIN OF TY_SAFRA,
         NR_SAFRA TYPE ZSDT0001-NR_SAFRA.
TYPES: END OF TY_SAFRA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: IT_ZSDT0001           TYPE TABLE OF ZSDT0001,
      IT_ZSDT0001_CTE       TYPE TABLE OF ZSDT0001,
      IT_J_1BNFDOC          TYPE TABLE OF TY_J_1BNFDOC,
      IT_J_1BNFDOC_CTE      TYPE TABLE OF TY_J_1BNFDOC,
      IT_ZLEST0060          TYPE TABLE OF ZLEST0060,
      IT_ZLEST0060_CTE      TYPE TABLE OF ZLEST0060,
      IT_SAIDA_AUX          TYPE TABLE OF TY_SAIDA,
      IT_SAIDA              TYPE TABLE OF TY_SAIDA,
      IT_SAIDA_MOV          TYPE TABLE OF TY_SAIDA,
      IT_SAIDA_ENTR_FISCAL  TYPE TABLE OF TY_SAIDA_ENTR_FISCAL,
      IT_SAIDA_SALDO_FISCAL TYPE TABLE OF TY_SAIDA_ENTR_FISCAL,
      IT_SAIDA_FISCAL_ALV   TYPE TABLE OF TY_SAIDA_FISCAL,
      IT_SAFRA              TYPE TABLE OF TY_SAFRA.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*

DATA: WA_ZSDT0001           TYPE ZSDT0001,
      WA_ZSDT0001_CTE       TYPE ZSDT0001,
      WA_J_1BNFDOC          TYPE TY_J_1BNFDOC,
      WA_J_1BNFDOC_CTE      TYPE TY_J_1BNFDOC,
      WA_ZLEST0060          TYPE ZLEST0060,
      WA_ZLEST0060_CTE      TYPE ZLEST0060,
      WA_SAIDA              TYPE TY_SAIDA,
      WA_SAIDA_AUX          TYPE TY_SAIDA,
      WA_SAIDA_MOV          TYPE TY_SAIDA,
      WA_SAIDA_ENTR_FISCAL  TYPE TY_SAIDA_ENTR_FISCAL,
      WA_SAIDA_SALDO_FISCAL TYPE TY_SAIDA_ENTR_FISCAL,
      WA_SAIDA_FISCAL_ALV   TYPE TY_SAIDA_FISCAL,
      WA_SAFRA              TYPE TY_SAFRA.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  IT_FCAT         TYPE TABLE OF TY_ESTRUTURA,
  S_VARIANT       TYPE DISVARIANT           , " Tabela Estrutura co
  T_TOP           TYPE SLIS_T_LISTHEADER,
  XS_EVENTS       TYPE SLIS_ALV_EVENT,
  EVENTS          TYPE SLIS_T_EVENT,
  GD_LAYOUT       TYPE SLIS_LAYOUT_ALV,
  T_PRINT         TYPE SLIS_PRINT_ALV,
  V_REPORT        LIKE SY-REPID,
  T_SORT          TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
  IT_SETLEAF      LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
  ESTRUTURA       TYPE TABLE OF TY_ESTRUTURA,
  VG_I            TYPE I,
  WG_MENSAGEM(30).

DATA: OK-CODE TYPE SY-UCOMM,
      VL_FORM TYPE TDSFNAME,
      VL_NAME TYPE RS38L_FNAM.

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
DATA: EDITCONTAINER        TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      EDITOR               TYPE REF TO CL_GUI_TEXTEDIT,
      CL_CONTAINER_95      TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      CL_CONTAINER_05      TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID        TYPE REF TO CL_DD_DOCUMENT,
      CL_GRID              TYPE REF TO CL_GUI_ALV_GRID,
      CL_CONTAINER_ENTRADA TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_GRID_ENTRADA      TYPE REF TO CL_GUI_ALV_GRID,
      CL_CONTAINER_SAIDA   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_GRID_SAIDA        TYPE REF TO CL_GUI_ALV_GRID,
      CL_CONTAINER_SALDO   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_GRID_SALDO        TYPE REF TO CL_GUI_ALV_GRID,

      WA_STABLE            TYPE LVC_S_STBL,
      WA_AFIELD            TYPE LVC_S_FCAT,
      IT_FIELDCAT          TYPE LVC_T_FCAT,
      W_FIELDCAT           TYPE LVC_S_FCAT,
      IT_FIELDCAT_ENTRADA  TYPE LVC_T_FCAT,
      WA_FIELDCAT_ENTRADA  TYPE LVC_S_FCAT,
      IT_FIELDCAT_SAIDA    TYPE LVC_T_FCAT,
      WA_FIELDCAT_SAIDA    TYPE LVC_S_FCAT,


      I_SORT               TYPE LVC_T_SORT,
      WA_SORT              TYPE LVC_S_SORT,
      WA_LAYOUT            TYPE LVC_S_LAYO,
      IS_STABLE            TYPE LVC_S_STBL VALUE 'XX',
      WG_REPNAME           LIKE SY-REPID,
      WG_X_VARIANT         LIKE DISVARIANT,
      WG_EXIT(1)           TYPE C,
      WG_SAVE(1)           TYPE C,
      WG_VARIANT           LIKE DISVARIANT,
      GT_F4                TYPE LVC_T_F4 WITH HEADER LINE.

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

DATA: NAME  TYPE VRM_ID,
      LIST  TYPE VRM_VALUES,
      VALUE LIKE LINE OF LIST.

DATA: VG_REPID   LIKE SY-REPID,
      VG_VARIANT TYPE DISVARIANT.

DATA: VARIANTE     LIKE DISVARIANT.
DATA: GS_VARIANT_C TYPE DISVARIANT.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*


SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECTION-SCREEN: BEGIN OF LINE,
COMMENT 1(28)    X_LCREC.
SELECT-OPTIONS:  P_LCREC  FOR ZLEST0114-LOCAL_DESCARGA NO INTERVALS NO-EXTENSION .
PARAMETERS:      P_LCDES  TYPE J_1BBRANCH-NAME MODIF ID GP1.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE,
COMMENT 1(28)    X_BUKRS.
SELECT-OPTIONS:  P_BUKRS  FOR  ZSDT0001-BUKRS        NO INTERVALS NO-EXTENSION OBLIGATORY.
PARAMETERS:      P_BUTXT  TYPE T001-BUTXT MODIF ID GP1.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE,
COMMENT 1(28)    X_WERKS.
SELECT-OPTIONS:  P_WERKS  FOR J_1BBRANCH-BRANCH  NO INTERVALS NO-EXTENSION OBLIGATORY.
PARAMETERS:      P_NAME   TYPE J_1BBRANCH-NAME MODIF ID GP1.
SELECTION-SCREEN: END OF LINE.


SELECT-OPTIONS:  P_MATNR  FOR ZSDT0001-MATNR        NO INTERVALS NO-EXTENSION OBLIGATORY,
                 P_SAFRA  FOR ZSDT0001-NR_SAFRA     NO INTERVALS NO-EXTENSION,
                 P_FORN   FOR LFA1-LIFNR,
                 P_DATA   FOR ZSDT0001-DT_MOVIMENTO NO-EXTENSION.
PARAMETERS       P_CLAS(15) AS LISTBOX VISIBLE LENGTH 15.

SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME.
PARAMETER: P_VARIA TYPE DISVARIANT-VARIANT.
SELECTION-SCREEN: END OF BLOCK B2.



AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARIA.

  VG_REPID        = SY-REPID.
  VARIANTE-REPORT = VG_REPID.

  IF ( NOT P_VARIA IS INITIAL ).
    VG_VARIANT-VARIANT = P_VARIA.

  ENDIF.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = VARIANTE
      I_SAVE        = 'A'
    IMPORTING
      ES_VARIANT    = VARIANTE
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.

  IF ( SY-SUBRC NE 0 ).
    MESSAGE S000(Z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE VARIANTE-VARIANT TO P_VARIA.
    MOVE VARIANTE-VARIANT TO WG_X_VARIANT-VARIANT.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LCREC-LOW.


  DATA: LT_MAP     TYPE TABLE OF DSELC,
        LS_MAP     TYPE DSELC,
        LT_RETURN  TYPE TABLE OF DDSHRETVAL,
        LS_RETURN  TYPE DDSHRETVAL,
        LS_STABLE  TYPE LVC_S_STBL,
        LT_LOC_REC TYPE TABLE OF TY_LOC_REC,
        LS_LOC_REC TYPE TY_LOC_REC.

  "LOAD F4 DATA
  SELECT LOCAL_DESCARGA DESC_LOCAL_DESC
    INTO TABLE LT_LOC_REC
    FROM ZLEST0114.

  SORT LT_LOC_REC BY LOCAL_DESCARGA.

  "SET RETURN FIELD
  CLEAR LS_MAP.
  LS_MAP-FLDNAME = 'F0001'.
  LS_MAP-DYFLDNAME = 'LOCAL_DESCARGA'.
  APPEND LS_MAP TO LT_MAP.

  " CALL SEARCH HELP POPUP FUNCTION
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'LOCAL_DESCARGA'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = LT_LOC_REC
      DYNPFLD_MAPPING = LT_MAP
      RETURN_TAB      = LT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

  " READ SELECTED F4 VALUE
  READ TABLE LT_RETURN INTO LS_RETURN WITH KEY FIELDNAME = 'F0001'.
  IF LS_RETURN IS NOT INITIAL.
    CLEAR: P_LCREC[].
    P_LCREC-SIGN   = 'I'.
    P_LCREC-OPTION = 'EQ'.
    P_LCREC-LOW    = LS_RETURN-FIELDVAL.
    READ TABLE LT_LOC_REC INTO LS_LOC_REC WITH KEY LOCAL_DESCARGA = LS_RETURN-FIELDVAL.
    IF SY-SUBRC = 0.
      P_LCDES = LS_LOC_REC-DESC_LOCAL_DESC.
      APPEND P_LCREC.

      SELECT SINGLE *
        FROM ZLEST0104 INTO @DATA(_WL_0104)
       WHERE LOCAL_DESCARGA IN @P_LCREC.

      IF ( SY-SUBRC EQ 0 ) AND ( P_LCREC-LOW IS NOT INITIAL ).
        IF ( _WL_0104-BUKRS  IS NOT INITIAL ) AND
           ( _WL_0104-BRANCH IS NOT INITIAL ).

          CLEAR: P_BUKRS[], P_WERKS[].

          P_BUKRS-SIGN   = 'I'.
          P_BUKRS-OPTION = 'EQ'.
          P_BUKRS-LOW    = _WL_0104-BUKRS.
          APPEND P_BUKRS.

          SELECT SINGLE BUTXT INTO (P_BUTXT)
            FROM T001
           WHERE BUKRS IN P_BUKRS.

          P_WERKS-SIGN   = 'I'.
          P_WERKS-OPTION = 'EQ'.
          P_WERKS-LOW    = _WL_0104-BRANCH.
          APPEND P_WERKS.

          SELECT SINGLE NAME INTO (P_NAME)
            FROM J_1BBRANCH
           WHERE BRANCH IN P_WERKS.
        ENDIF.
      ENDIF.

      LEAVE TO SCREEN 1000.
    ENDIF.
  ENDIF.



AT SELECTION-SCREEN ON P_BUKRS.

  SELECT SINGLE BUTXT INTO (P_BUTXT)
    FROM T001
   WHERE BUKRS IN P_BUKRS.

AT SELECTION-SCREEN ON P_WERKS.

  SELECT SINGLE NAME INTO (P_NAME)
    FROM J_1BBRANCH
   WHERE BRANCH IN P_WERKS.

AT SELECTION-SCREEN ON P_LCREC.

  SELECT SINGLE DESC_LOCAL_DESC INTO (P_LCDES)
    FROM ZLEST0114
   WHERE LOCAL_DESCARGA IN P_LCREC.

AT SELECTION-SCREEN OUTPUT.

  X_BUKRS = 'Empresa'.
  X_WERKS = 'Centro Recebedor'.
  X_LCREC = 'Local Recebimento'.

  VALUE-KEY =  'CO'.
  VALUE-TEXT = 'Convencional'.
  APPEND VALUE TO LIST.

  VALUE-KEY =  'R1'.
  VALUE-TEXT = 'RR'.
  APPEND VALUE TO LIST.

  VALUE-KEY =  'R2'.
  VALUE-TEXT = 'RR2'.
  APPEND VALUE TO LIST.

  NAME = 'P_CLAS'.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = NAME
      VALUES = LIST.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'GP1'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM: VALIDA_PARAMETROS.

  PERFORM:
            F_SELECIONA_DADOS, " Form seleciona dados
            F_SAIDA, " Form de saida
            F_IMPRIME_DADOS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*

FORM F_SELECIONA_DADOS .

  RANGES: R_DT_MOV FOR ZSDT0001-DT_MOVIMENTO.

  CLEAR: IT_SAIDA_MOV[].

  DATA: V_DT_MOV_INI TYPE ZSDT0001-DT_MOVIMENTO.

  SELECT *
    FROM ZSDT0001
    INTO TABLE IT_ZSDT0001
    WHERE TP_MOVIMENTO  = 'E'
    AND BUKRS          = P_BUKRS-LOW
    AND BRANCH         = P_WERKS-LOW
    AND MATNR          = P_MATNR-LOW
    AND NR_SAFRA       IN P_SAFRA
    AND LOCAL_DESCARGA IN P_LCREC
    AND PARID          IN P_FORN
    AND DT_MOVIMENTO   IN P_DATA.

  IF P_CLAS IS NOT INITIAL.
    IF P_CLAS  = 'CO'.
      DELETE IT_ZSDT0001 WHERE TP_TRANSGENIA NE 'CO'.
    ELSEIF P_CLAS  = 'R1'.
      DELETE IT_ZSDT0001 WHERE TP_TRANSGENIA NE 'D1'
                         AND   TP_TRANSGENIA NE 'T1'
                         AND   TP_TRANSGENIA NE 'P1'.
    ELSEIF P_CLAS  = 'R2'.
      DELETE IT_ZSDT0001 WHERE TP_TRANSGENIA NE 'D2'
                         AND   TP_TRANSGENIA NE 'T2'
                         AND   TP_TRANSGENIA NE 'P2'.
    ENDIF.
  ENDIF.

  CHECK NOT IT_ZSDT0001[] IS INITIAL.

  "Busca Notas Vinculadas
  SELECT *
    FROM ZLEST0060 INTO TABLE IT_ZLEST0060
    FOR ALL ENTRIES IN IT_ZSDT0001
  WHERE NR_ROMANEIO EQ IT_ZSDT0001-NR_ROMANEIO
    AND SAFRA       EQ IT_ZSDT0001-NR_SAFRA
    AND RM_CODIGO   EQ IT_ZSDT0001-PARID
    AND NFNUM       EQ IT_ZSDT0001-NFNUM.

  IF IT_ZLEST0060[] IS NOT INITIAL.

    SELECT DOCNUM PSTDAT
      FROM J_1BNFDOC
      INTO TABLE IT_J_1BNFDOC
      FOR ALL ENTRIES IN IT_ZLEST0060
      WHERE DOCNUM  = IT_ZLEST0060-DOCNUM
      AND ( PSTDAT IN P_DATA OR PSTDAT LT P_DATA-LOW ).

  ENDIF.

  IF ( P_DATA IS NOT INITIAL ) AND ( IT_J_1BNFDOC[] IS NOT INITIAL ).

    CLEAR: R_DT_MOV[].

    IF P_SAFRA[] IS INITIAL.
      R_DT_MOV-SIGN   = 'I'.
      R_DT_MOV-OPTION = 'BT'.
      R_DT_MOV-LOW    = P_DATA-LOW - 361.
      R_DT_MOV-HIGH   = P_DATA-LOW - 1.
      APPEND R_DT_MOV.
    ELSE.
      R_DT_MOV-SIGN   = 'I'.
      R_DT_MOV-OPTION = 'LT'.
      R_DT_MOV-LOW    = P_DATA-LOW.
      APPEND R_DT_MOV.
    ENDIF.

    SELECT *
      FROM ZSDT0001
      INTO TABLE IT_ZSDT0001_CTE
      WHERE TP_MOVIMENTO  EQ 'E'
      AND BUKRS           IN P_BUKRS
      AND BRANCH          IN P_WERKS
      AND MATNR           IN P_MATNR
      AND NR_SAFRA        IN P_SAFRA
      AND LOCAL_DESCARGA  IN P_LCREC
      AND PARID           IN P_FORN
      AND CT_AQUAV        EQ 'X'
      AND DT_MOVIMENTO    IN R_DT_MOV.

    CHECK NOT IT_ZSDT0001_CTE[] IS INITIAL.

    "Busca Notas Vinculadas
    SELECT *
      FROM ZLEST0060 INTO TABLE IT_ZLEST0060_CTE
      FOR ALL ENTRIES IN IT_ZSDT0001_CTE
    WHERE NR_ROMANEIO EQ IT_ZSDT0001_CTE-NR_ROMANEIO
      AND SAFRA       EQ IT_ZSDT0001_CTE-NR_SAFRA
      AND RM_CODIGO   EQ IT_ZSDT0001_CTE-PARID
      AND NFNUM       EQ IT_ZSDT0001_CTE-NFNUM.

    IF IT_ZLEST0060_CTE[] IS NOT INITIAL.

      SELECT DOCNUM PSTDAT
        FROM J_1BNFDOC
        INTO TABLE IT_J_1BNFDOC_CTE
        FOR ALL ENTRIES IN IT_ZLEST0060_CTE
        WHERE DOCNUM EQ IT_ZLEST0060_CTE-DOCNUM
        AND PSTDAT IN P_DATA.

    ENDIF.

  ENDIF.

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

  DATA: IT_SAIDA_FISCAL TYPE TABLE OF ZSDT0001,
        WA_SAIDA_FISCAL TYPE ZSDT0001.

  IF NOT ( P_DATA  IS INITIAL ) AND ( IT_J_1BNFDOC[] IS INITIAL ).

    SELECT * FROM ZSDT0001
      INTO TABLE IT_SAIDA_FISCAL
     WHERE TP_MOVIMENTO  EQ 'E'
      AND BUKRS          IN P_BUKRS
      AND BRANCH         IN P_WERKS
      AND MATNR          IN P_MATNR
      AND NR_SAFRA       IN P_SAFRA
      AND LOCAL_DESCARGA IN P_LCREC
      AND PARID          IN P_FORN
      AND CT_AQUAV       EQ 'X'.

    CHECK NOT IT_SAIDA_FISCAL[] IS INITIAL.

    "Busca Notas Vinculadas
    SELECT *
      FROM ZLEST0060 INTO TABLE IT_ZLEST0060
      FOR ALL ENTRIES IN IT_SAIDA_FISCAL
    WHERE NR_ROMANEIO EQ IT_SAIDA_FISCAL-NR_ROMANEIO
      AND SAFRA       EQ IT_SAIDA_FISCAL-NR_SAFRA
      AND RM_CODIGO   EQ IT_SAIDA_FISCAL-PARID
      AND NFNUM       EQ IT_SAIDA_FISCAL-NFNUM.

    IF IT_ZLEST0060[] IS NOT INITIAL.

      SELECT DOCNUM PSTDAT
        FROM J_1BNFDOC
        INTO TABLE IT_J_1BNFDOC
        FOR ALL ENTRIES IN IT_ZLEST0060
      WHERE DOCNUM EQ IT_ZLEST0060-DOCNUM
        AND PSTDAT IN P_DATA.

    ENDIF.

  ENDIF.

  SORT IT_J_1BNFDOC BY DOCNUM.

  LOOP AT IT_ZSDT0001 INTO WA_ZSDT0001.
    "Entrada.
    WA_SAIDA-DT_MOVIMENTO  = WA_ZSDT0001-DT_MOVIMENTO. "WA_J_1BNFDOC-PSTDAT. " DATA MOVIMENTO


    "----------------------------------------------------------------"
    " FISICO
    "----------------------------------------------------------------"

    WA_SAIDA-ENT_FISICO    = WA_ZSDT0001-PESO_SUBTOTAL. "FISICO – ENTRADA
    WA_SAIDA-SAI_FISICO    = 0. "FISICO – SAIDA
    WA_SAIDA-SAL_FISICO    = 0.


    "----------------------------------------------------------------"
    " FISCAL
    "----------------------------------------------------------------"

    WA_SAIDA-ENT_FISCAL    = WA_ZSDT0001-PESO_FISCAL. "FISCAL -  ENTRADA
    WA_SAIDA-SAI_FISCAL    = 0. "FISCAL – SAIDA
    WA_SAIDA-SAL_FISCAL    = 0.


    "----------------------------------------------------------------"
    " LIQ.RET
    "----------------------------------------------------------------"

    CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
      EXPORTING
        I_BUKRS         = WA_ZSDT0001-BUKRS
        I_BRANCH        = WA_ZSDT0001-BRANCH
        I_LIFNR         = WA_ZSDT0001-PARID
        I_PESO          = WA_ZSDT0001-PESO_SUBTOTAL
        I_PESO_FISCAL   = WA_ZSDT0001-PESO_FISCAL
      IMPORTING
        E_PESO_LIQUIDO  = WA_SAIDA-ENT_LIQRET.

    WA_SAIDA-SAI_LIQRET    = 0.
    WA_SAIDA-SAL_LIQRET    = 0.

    WA_SAIDA-NR_ROMANEIO   = WA_ZSDT0001-NR_ROMANEIO.
    WA_SAIDA-BUKRS         = WA_ZSDT0001-BUKRS.
    WA_SAIDA-BRANCH        = WA_ZSDT0001-BRANCH.
    WA_SAIDA-NR_SAFRA      = WA_ZSDT0001-NR_SAFRA.
    WA_SAIDA-MATNR         = WA_ZSDT0001-MATNR.
    WA_SAIDA-DOCDAT        = WA_ZSDT0001-DOCDAT.
    WA_SAIDA-NFNUM         = WA_ZSDT0001-NFNUM.
    WA_SAIDA-NETWR         = WA_ZSDT0001-NETWR.
    WA_SAIDA-PARID         = WA_ZSDT0001-PARID.

    APPEND WA_SAIDA TO IT_SAIDA.
    CLEAR WA_SAIDA.
    "Saida.
    IF ( WA_ZSDT0001-CT_AQUAV EQ 'X').
      CLEAR: WA_ZLEST0060.
      LOOP AT IT_ZLEST0060 INTO WA_ZLEST0060 WHERE NR_ROMANEIO = WA_ZSDT0001-NR_ROMANEIO
                                               AND SAFRA       = WA_ZSDT0001-NR_SAFRA
                                               AND RM_CODIGO   = WA_ZSDT0001-PARID
                                               AND NFNUM       = WA_ZSDT0001-NFNUM.

        READ TABLE IT_J_1BNFDOC INTO WA_J_1BNFDOC WITH KEY DOCNUM = WA_ZLEST0060-DOCNUM BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA-DT_MOVIMENTO  = WA_J_1BNFDOC-PSTDAT. " DATA MOVIMENTO

          "----------------------------------------------------------------"
          " FISICO
          "----------------------------------------------------------------"

          WA_SAIDA-ENT_FISICO    = 0. "FISICO – ENTRADA

          IF WA_ZLEST0060-PESO_SUBTOTAL IS NOT INITIAL.
            WA_SAIDA-SAI_FISICO = WA_ZLEST0060-PESO_SUBTOTAL. "FISICO – SAIDA
          ELSE.
            WA_SAIDA-SAI_FISICO = WA_ZSDT0001-PESO_SUBTOTAL. "FISICO – SAIDA
          ENDIF.

          WA_SAIDA-SAL_FISICO    = 0.

          "----------------------------------------------------------------"
          " FISCAL
          "----------------------------------------------------------------"

          WA_SAIDA-ENT_FISCAL    = 0. "FISCAL -  ENTRADA

          IF WA_ZLEST0060-PESO_FISCAL IS NOT INITIAL.
            WA_SAIDA-SAI_FISCAL  = WA_ZLEST0060-PESO_FISCAL.
          ELSE.
            WA_SAIDA-SAI_FISCAL  = WA_ZSDT0001-PESO_FISCAL. "FISCAL – SAIDA
          ENDIF.

          WA_SAIDA-SAL_FISCAL    = 0.


          "----------------------------------------------------------------"
          " LIQ.RET.
          "----------------------------------------------------------------"

          WA_SAIDA-ENT_LIQRET    = 0. "LIQ.RET -  ENTRADA
          WA_SAIDA-SAI_LIQRET    = WA_ZLEST0060-PESO_LIQ_RET. "LIQ.RET – SAIDA
          WA_SAIDA-SAL_LIQRET    = 0.


          WA_SAIDA-DOCNUM        = WA_J_1BNFDOC-DOCNUM.
          WA_SAIDA-BUKRS         = WA_ZSDT0001-BUKRS.
          WA_SAIDA-BRANCH        = WA_ZSDT0001-BRANCH.
          WA_SAIDA-NR_SAFRA      = WA_ZSDT0001-NR_SAFRA.
          WA_SAIDA-MATNR         = WA_ZSDT0001-MATNR.
          WA_SAIDA-DOCDAT        = WA_ZSDT0001-DOCDAT.
          WA_SAIDA-NFNUM         = WA_ZSDT0001-NFNUM.
          WA_SAIDA-NETWR         = WA_ZSDT0001-NETWR.
          WA_SAIDA-PARID         = WA_ZSDT0001-PARID.
          WA_SAIDA-NR_ROMANEIO   = WA_ZSDT0001-NR_ROMANEIO.

          APPEND WA_SAIDA TO IT_SAIDA.
          CLEAR WA_SAIDA.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDLOOP.

  IF ( IT_SAIDA_FISCAL[] IS NOT INITIAL ) AND ( IT_J_1BNFDOC[] IS NOT INITIAL ).
    LOOP AT IT_SAIDA_FISCAL INTO WA_SAIDA_FISCAL.
      IF ( WA_SAIDA_FISCAL-CT_AQUAV EQ 'X').

        CLEAR: WA_ZLEST0060.
        LOOP AT IT_ZLEST0060 INTO WA_ZLEST0060 WHERE NR_ROMANEIO = WA_SAIDA_FISCAL-NR_ROMANEIO
                                                 AND SAFRA       = WA_SAIDA_FISCAL-NR_SAFRA
                                                 AND RM_CODIGO   = WA_SAIDA_FISCAL-PARID
                                                 AND NFNUM       = WA_SAIDA_FISCAL-NFNUM.

          READ TABLE IT_J_1BNFDOC INTO WA_J_1BNFDOC WITH KEY DOCNUM = WA_ZLEST0060-DOCNUM BINARY SEARCH.
          IF ( SY-SUBRC EQ 0 ).
            WA_SAIDA-DT_MOVIMENTO  = WA_J_1BNFDOC-PSTDAT. " DATA MOVIMENTO

            "----------------------------------------------------------------"
            " FISICO
            "----------------------------------------------------------------"
            WA_SAIDA-ENT_FISICO    = 0. "FISICO – ENTRADA

            IF WA_ZLEST0060-PESO_SUBTOTAL IS NOT INITIAL.
              WA_SAIDA-SAI_FISICO    = WA_ZLEST0060-PESO_SUBTOTAL. "FISICO – SAIDA
            ELSE.
              WA_SAIDA-SAI_FISICO    = WA_SAIDA_FISCAL-PESO_SUBTOTAL. "FISICO – SAIDA
            ENDIF.

            WA_SAIDA-SAL_FISICO    = 0.

            "----------------------------------------------------------------"
            " FISCAL
            "----------------------------------------------------------------"
            WA_SAIDA-ENT_FISCAL    = 0. "FISCAL -  ENTRADA

            IF WA_ZLEST0060-PESO_FISCAL IS NOT INITIAL.
              WA_SAIDA-SAI_FISCAL    = WA_ZLEST0060-PESO_FISCAL. "FISCAL – SAIDA
            ELSE.
              WA_SAIDA-SAI_FISCAL    = WA_SAIDA_FISCAL-PESO_FISCAL. "FISCAL – SAIDA
            ENDIF.

            WA_SAIDA-SAL_FISCAL    = 0.

            "----------------------------------------------------------------"
            " LIQ.RET.
            "----------------------------------------------------------------"
            WA_SAIDA-ENT_LIQRET    = 0. "LIQ.RET -  ENTRADA
            WA_SAIDA-SAI_LIQRET    = WA_ZLEST0060-PESO_LIQ_RET. "LIQ.RET. – SAIDA
            WA_SAIDA-SAL_LIQRET    = 0.

            WA_SAIDA-DOCNUM        = WA_J_1BNFDOC-DOCNUM.
            WA_SAIDA-BUKRS         = WA_SAIDA_FISCAL-BUKRS.
            WA_SAIDA-BRANCH        = WA_SAIDA_FISCAL-BRANCH.
            WA_SAIDA-NR_SAFRA      = WA_SAIDA_FISCAL-NR_SAFRA.
            WA_SAIDA-MATNR         = WA_SAIDA_FISCAL-MATNR.
            WA_SAIDA-DOCDAT        = WA_ZSDT0001-DOCDAT.
            WA_SAIDA-NFNUM         = WA_ZSDT0001-NFNUM.
            WA_SAIDA-NETWR         = WA_ZSDT0001-NETWR.
            WA_SAIDA-PARID         = WA_ZSDT0001-PARID.
            WA_SAIDA-NR_ROMANEIO   = WA_ZSDT0001-NR_ROMANEIO.
            APPEND WA_SAIDA TO IT_SAIDA.
            CLEAR WA_SAIDA.
          ENDIF.

        ENDLOOP.

      ENDIF.
    ENDLOOP.
  ENDIF.

  " Loop para adicionar na ALV os registros dos romaneiros com CTEs na
  " data pesquisada para as saídas. A pesquisa acima considera apenas
  " os registros dos romaneios na data pesquisada para as saídas.
  LOOP AT IT_ZSDT0001_CTE INTO WA_ZSDT0001_CTE.

    CLEAR: WA_ZLEST0060_CTE.
    LOOP AT IT_ZLEST0060_CTE INTO WA_ZLEST0060_CTE WHERE NR_ROMANEIO = WA_ZSDT0001_CTE-NR_ROMANEIO
                                                     AND SAFRA       = WA_ZSDT0001_CTE-NR_SAFRA
                                                     AND RM_CODIGO   = WA_ZSDT0001_CTE-PARID
                                                     AND NFNUM       = WA_ZSDT0001_CTE-NFNUM.

      READ TABLE IT_J_1BNFDOC_CTE INTO WA_J_1BNFDOC_CTE WITH KEY DOCNUM = WA_ZLEST0060_CTE-DOCNUM.
      IF SY-SUBRC EQ 0.
        WA_SAIDA-DT_MOVIMENTO  = WA_J_1BNFDOC_CTE-PSTDAT. " DATA MOVIMENTO

        "----------------------------------------------------------------"
        " FISICO
        "----------------------------------------------------------------"
        WA_SAIDA-ENT_FISICO    = 0. "FISICO – ENTRADA

        IF WA_ZLEST0060_CTE-PESO_SUBTOTAL IS NOT INITIAL.
          WA_SAIDA-SAI_FISICO    = WA_ZLEST0060_CTE-PESO_SUBTOTAL. "FISICO – SAIDA
        ELSE.
          WA_SAIDA-SAI_FISICO    = WA_ZSDT0001_CTE-PESO_SUBTOTAL. "FISICO – SAIDA
        ENDIF.

        WA_SAIDA-SAL_FISICO    = 0.

        "----------------------------------------------------------------"
        " FISCAL
        "----------------------------------------------------------------"
        WA_SAIDA-ENT_FISCAL    = 0. "FISCAL -  ENTRADA

        IF WA_ZLEST0060_CTE-PESO_FISCAL IS NOT INITIAL.
          WA_SAIDA-SAI_FISCAL    = WA_ZLEST0060_CTE-PESO_FISCAL. "FISCAL – SAIDA
        ELSE.
          WA_SAIDA-SAI_FISCAL    = WA_ZSDT0001_CTE-PESO_FISCAL. "FISCAL – SAIDA
        ENDIF.

        WA_SAIDA-SAL_FISCAL    = 0.

        "----------------------------------------------------------------"
        " LIQ.RET.
        "----------------------------------------------------------------"
        WA_SAIDA-ENT_LIQRET    = 0. "LIQ.RET. -  ENTRADA
        WA_SAIDA-SAI_LIQRET    = WA_ZLEST0060_CTE-PESO_LIQ_RET. "LIQ.RET. – SAIDA
        WA_SAIDA-SAL_LIQRET    = 0.

        WA_SAIDA-DOCNUM        = WA_J_1BNFDOC_CTE-DOCNUM.
        WA_SAIDA-BUKRS         = WA_ZSDT0001_CTE-BUKRS.
        WA_SAIDA-BRANCH        = WA_ZSDT0001_CTE-BRANCH.
        WA_SAIDA-NR_SAFRA      = WA_ZSDT0001_CTE-NR_SAFRA.
        WA_SAIDA-MATNR         = WA_ZSDT0001_CTE-MATNR.
        WA_SAIDA-DOCDAT        = WA_ZSDT0001-DOCDAT.
        WA_SAIDA-NFNUM         = WA_ZSDT0001-NFNUM.
        WA_SAIDA-NETWR         = WA_ZSDT0001-NETWR.
        WA_SAIDA-PARID         = WA_ZSDT0001-PARID.
        WA_SAIDA-NR_ROMANEIO   = WA_ZSDT0001-NR_ROMANEIO.
        APPEND WA_SAIDA TO IT_SAIDA.
        CLEAR WA_SAIDA.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

  LOOP AT IT_SAIDA ASSIGNING FIELD-SYMBOL(<FS_SAIDA>) WHERE NR_SAFRA < '2018'
                                                        AND ( ( ENT_LIQRET NE 0 ) OR
                                                              ( SAI_LIQRET NE 0 ) OR
                                                              ( SAL_LIQRET NE 0 ) ).
    CLEAR: <FS_SAIDA>-ENT_LIQRET,
           <FS_SAIDA>-SAI_LIQRET,
           <FS_SAIDA>-SAL_LIQRET.
  ENDLOOP.

  IT_SAIDA_AUX[] = IT_SAIDA[].

  SORT: IT_SAIDA_AUX BY NR_SAFRA
                        DT_MOVIMENTO,
        IT_SAIDA     BY NR_SAFRA
                        DT_MOVIMENTO.

  DELETE ADJACENT DUPLICATES FROM IT_SAIDA_AUX COMPARING NR_SAFRA DT_MOVIMENTO.

  LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX.
    READ TABLE IT_SAFRA WITH KEY NR_SAFRA = WA_SAIDA_AUX-NR_SAFRA TRANSPORTING NO FIELDS.
    IF SY-SUBRC NE 0.
      WA_SAFRA-NR_SAFRA = WA_SAIDA_AUX-NR_SAFRA.
      APPEND WA_SAFRA TO IT_SAFRA.
    ENDIF.
  ENDLOOP.

  CLEAR: WA_SAIDA_AUX, WA_SAFRA.

  DATA: V_TOTAL_FISICO TYPE ZSDT0001-PESO_LIQ,
        V_TOTAL_FISCAL TYPE ZSDT0001-PESO_FISCAL,
        V_TOTAL_LIQRET TYPE ZLEST0060-PESO_LIQ_RET,
        TABIX          TYPE SY-TABIX.

  LOOP AT IT_SAFRA INTO WA_SAFRA.

    V_TOTAL_FISICO = 0.
    V_TOTAL_FISCAL = 0.
    V_TOTAL_LIQRET = 0.
    LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX WHERE NR_SAFRA EQ WA_SAFRA-NR_SAFRA .

      TABIX = SY-TABIX.
      WA_SAIDA_AUX-ENT_FISICO    = 0.
      WA_SAIDA_AUX-SAI_FISICO    = 0.
      WA_SAIDA_AUX-ENT_FISCAL    = 0.
      WA_SAIDA_AUX-SAI_FISCAL    = 0.
      WA_SAIDA_AUX-ENT_LIQRET    = 0.
      WA_SAIDA_AUX-SAI_LIQRET    = 0.

      LOOP AT IT_SAIDA INTO WA_SAIDA WHERE DT_MOVIMENTO = WA_SAIDA_AUX-DT_MOVIMENTO
                                       AND NR_SAFRA     = WA_SAIDA_AUX-NR_SAFRA.

        ADD WA_SAIDA-ENT_FISICO TO WA_SAIDA_AUX-ENT_FISICO.
        ADD WA_SAIDA-SAI_FISICO TO WA_SAIDA_AUX-SAI_FISICO.
        ADD WA_SAIDA-ENT_FISCAL TO WA_SAIDA_AUX-ENT_FISCAL.
        ADD WA_SAIDA-SAI_FISCAL TO WA_SAIDA_AUX-SAI_FISCAL.
        ADD WA_SAIDA-ENT_LIQRET TO WA_SAIDA_AUX-ENT_LIQRET.
        ADD WA_SAIDA-SAI_LIQRET TO WA_SAIDA_AUX-SAI_LIQRET.

      ENDLOOP.

      V_TOTAL_FISICO = V_TOTAL_FISICO + ( WA_SAIDA_AUX-ENT_FISICO - WA_SAIDA_AUX-SAI_FISICO ).
      V_TOTAL_FISCAL = V_TOTAL_FISCAL + ( WA_SAIDA_AUX-ENT_FISCAL - WA_SAIDA_AUX-SAI_FISCAL ).
      V_TOTAL_LIQRET = V_TOTAL_LIQRET + ( WA_SAIDA_AUX-ENT_LIQRET - WA_SAIDA_AUX-SAI_LIQRET ).

      WA_SAIDA_AUX-SAL_FISICO    = V_TOTAL_FISICO.
      WA_SAIDA_AUX-SAL_FISCAL    = V_TOTAL_FISCAL.
      WA_SAIDA_AUX-SAL_LIQRET    = V_TOTAL_LIQRET.

      MODIFY IT_SAIDA_AUX FROM WA_SAIDA_AUX
       INDEX TABIX
       TRANSPORTING ENT_FISICO SAI_FISICO SAL_FISICO
                    ENT_FISCAL SAI_FISCAL SAL_FISCAL
                    ENT_LIQRET SAI_LIQRET SAL_LIQRET.
    ENDLOOP.

  ENDLOOP.


  IT_SAIDA_MOV[] = IT_SAIDA[].
  IT_SAIDA[]     = IT_SAIDA_AUX[].


ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .
  PERFORM F_ALV_FIELDCAT.


  WA_LAYOUT-ZEBRA      = 'X'.
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.
  WA_LAYOUT-GRID_TITLE = 'Totais por data'.
  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT = 'X'.

  CALL SCREEN 0100.
ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .

  REFRESH IT_FIELDCAT.
  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_SAIDA'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NR_SAFRA'.
  WA_AFIELD-SCRTEXT_S = 'SAFRA'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DT_MOVIMENTO'.
  WA_AFIELD-SCRTEXT_S = 'DT. MOVTO'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 12.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ENT_FISICO'.
  WA_AFIELD-SCRTEXT_M = 'FISICO – ENTRADA'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 20.
  WA_AFIELD-DO_SUM = ABAP_TRUE.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SAI_FISICO'.
  WA_AFIELD-SCRTEXT_M = 'FISICO – SAIDA'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 20.
  WA_AFIELD-DO_SUM = ABAP_TRUE.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SAL_FISICO'.
  WA_AFIELD-SCRTEXT_M = 'FISICO – SALDO'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 20.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SEPARA'.
  WA_AFIELD-SCRTEXT_M = ' '.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 5.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ENT_FISCAL'.
  WA_AFIELD-SCRTEXT_M = 'FISCAL – ENTRADA'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 20.
  WA_AFIELD-HOTSPOT       = 'X'.
  WA_AFIELD-DO_SUM = ABAP_TRUE.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SAI_FISCAL'.
  WA_AFIELD-SCRTEXT_M = 'FISCAL – SAIDA'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 20.
  WA_AFIELD-HOTSPOT       = 'X'.
  WA_AFIELD-DO_SUM = ABAP_TRUE.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SAL_FISCAL'.
  WA_AFIELD-SCRTEXT_M = 'FISCAL – SALDO'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 20.
  WA_AFIELD-HOTSPOT       = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SEPARA'.
  WA_AFIELD-SCRTEXT_M = ' '.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 5.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ENT_LIQRET'.
  WA_AFIELD-SCRTEXT_M = 'LIQ.RET – ENTRADA'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 20.
  WA_AFIELD-DO_SUM = ABAP_TRUE.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SAI_LIQRET'.
  WA_AFIELD-SCRTEXT_M = 'LIQ.RET – SAIDA'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 20.
  WA_AFIELD-DO_SUM = ABAP_TRUE.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SAL_LIQRET'.
  WA_AFIELD-SCRTEXT_M = 'LIQ.RET – SALDO'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 20.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  "Subtotal por Safra
  WA_SORT-SPOS = 1.
  WA_SORT-FIELDNAME = 'NR_SAFRA'.
  WA_SORT-DOWN = 'X'.
  WA_SORT-GROUP = '*'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO I_SORT.

ENDFORM.                    " F_ALV_FIELDCAT

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS: ZM_HANDLE_HOTSPOT FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID
                  E_COLUMN_ID
                  ES_ROW_NO.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD ZM_HANDLE_HOTSPOT.

    CHECK E_ROW_ID-ROWTYPE(1) NE 'S'.
    CHECK E_ROW_ID-ROWTYPE(1) NE 'T'.

    PERFORM Z_HANDLE_HOTSPOT USING E_ROW_ID
                                   E_COLUMN_ID
                                   ES_ROW_NO.

  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.
  DATA: WA_EVENT TYPE REF TO LCL_EVENT_RECEIVER.

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
    PERFORM F_ALV_FIELDCAT.
    CALL METHOD CL_GRID->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = IT_FIELDCAT[].

    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT OBJ_DYNDOC_ID
      EXPORTING
*       STYLE      =
*       BACKGROUND_COLOR =
*       BDS_STYLESHEET =
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

    CREATE OBJECT WA_EVENT.
    SET HANDLER: WA_EVENT->ZM_HANDLE_HOTSPOT FOR CL_GRID.

    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT      = WG_X_VARIANT
        IS_LAYOUT       = WA_LAYOUT
        I_SAVE          = WG_SAVE
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCAT[]
        IT_SORT         = I_SORT[]
        IT_OUTTAB       = IT_SAIDA[].

  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
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
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
FORM ZF_ALV_HEADER .

  DATA:   WL_DATA(10),
          WL_LINHA(60),
          WL_TEXT TYPE SDYDO_TEXT_ELEMENT.

  DATA: WL_T001       TYPE T001,
        WL_MAKT       TYPE MAKT,
        WL_J_1BBRANCH TYPE J_1BBRANCH,
        WL_LFA1       TYPE LFA1.

  IF P_BUKRS   IS NOT INITIAL.
    SELECT SINGLE *
      FROM T001
      INTO WL_T001
      WHERE BUKRS = P_BUKRS-LOW.

    CONCATENATE 'Empresa  :' P_BUKRS-LOW '-' WL_T001-BUTXT
      INTO WL_LINHA SEPARATED BY SPACE.

    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF P_WERKS   IS NOT INITIAL.
    SELECT SINGLE *
     FROM J_1BBRANCH
     INTO WL_J_1BBRANCH
     WHERE BUKRS = P_BUKRS-LOW
     AND   BRANCH = P_WERKS-LOW.

    CONCATENATE 'Centro :' P_WERKS-LOW '-' WL_J_1BBRANCH-NAME
      INTO WL_LINHA SEPARATED BY SPACE.

    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  SELECT SINGLE *
    FROM MAKT
    INTO WL_MAKT
    WHERE MATNR = P_MATNR-LOW
    AND SPRAS EQ SY-LANGU.

  CONCATENATE 'Produto :' P_MATNR-LOW '-' WL_MAKT-MAKTX
     INTO WL_LINHA SEPARATED BY SPACE.

  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  CONCATENATE 'Safra :' P_SAFRA-LOW
      INTO WL_LINHA SEPARATED BY SPACE.

  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  IF ( P_CLAS IS NOT INITIAL ).
    CONCATENATE 'Classificação :' P_CLAS
        INTO WL_LINHA SEPARATED BY SPACE.

    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF ( P_FORN IS NOT INITIAL ).

    SELECT SINGLE * FROM LFA1 INTO WL_LFA1 WHERE LIFNR EQ P_FORN-LOW.

    CONCATENATE 'Fornecedor  :' P_FORN-LOW '-' WL_LFA1-NAME1
      INTO WL_LINHA SEPARATED BY SPACE.

    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


  ENDIF.

ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
FORM Z_HANDLE_HOTSPOT  USING    P_ROW_ID
                                P_COLUMN_ID
                                P_ROW_NO.
  CLEAR: WA_SAIDA.

  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_ROW_ID.

  IF ( SY-SUBRC EQ 0 ).


    CASE P_COLUMN_ID.

      WHEN: 'ENT_FISCAL'.

        PERFORM: ENTR_FISCAL USING WA_SAIDA-BUKRS
                                   WA_SAIDA-BRANCH
                                   WA_SAIDA-NR_SAFRA
                                   WA_SAIDA-MATNR
                                   WA_SAIDA-DT_MOVIMENTO.

      WHEN: 'SAI_FISCAL'.

        PERFORM: SAIDA_FISCAL USING WA_SAIDA-BUKRS
                                    WA_SAIDA-BRANCH
                                    WA_SAIDA-NR_SAFRA
                                    WA_SAIDA-MATNR
                                    WA_SAIDA-DT_MOVIMENTO.
      WHEN: 'SAL_FISCAL'.

        PERFORM: SALDO_FISCAL USING WA_SAIDA-BUKRS
                                    WA_SAIDA-BRANCH
                                    WA_SAIDA-NR_SAFRA
                                    WA_SAIDA-MATNR
                                    WA_SAIDA-DT_MOVIMENTO
                                    P_DATA[].
    ENDCASE.
  ENDIF.

ENDFORM.                    " Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*&      Form  ENTR_FISCAL
*&---------------------------------------------------------------------*
FORM ENTR_FISCAL  USING    P_BUKRS          TYPE ZSDT0001-BUKRS
                           P_BRANCH         TYPE ZSDT0001-BRANCH
                           P_NR_SAFRA       TYPE ZSDT0001-NR_SAFRA
                           P_MATNR          TYPE ZSDT0001-MATNR
                           P_DATA_MOVIMENTO TYPE ZSDT0001-DT_MOVIMENTO.

  DATA: IT_ENTR_FISCAL TYPE TABLE OF ZSDT0001,
        WA_ENTR_FISCAL TYPE ZSDT0001.

  REFRESH: IT_ENTR_FISCAL[].

  LOOP AT IT_SAIDA_MOV INTO WA_SAIDA_MOV WHERE DT_MOVIMENTO EQ P_DATA_MOVIMENTO
                                           AND NR_SAFRA     EQ P_NR_SAFRA
                                           AND ( ENT_FISICO   NE 0 OR
                                                 ENT_FISCAL   NE 0 ).

    CLEAR: WA_SAIDA_ENTR_FISCAL.

    WA_SAIDA_ENTR_FISCAL-DT_MOVIMENTO  = WA_SAIDA_MOV-DT_MOVIMENTO.
    WA_SAIDA_ENTR_FISCAL-NR_ROMANEIO   = WA_SAIDA_MOV-NR_ROMANEIO.
    WA_SAIDA_ENTR_FISCAL-DOCDAT        = WA_SAIDA_MOV-DOCDAT.
    WA_SAIDA_ENTR_FISCAL-PESO_SUBTOTAL = WA_SAIDA_MOV-ENT_FISICO.
    WA_SAIDA_ENTR_FISCAL-NFNUM         = WA_SAIDA_MOV-NFNUM.
    WA_SAIDA_ENTR_FISCAL-NETWR         = WA_SAIDA_MOV-NETWR.
    WA_SAIDA_ENTR_FISCAL-PESO_FISCAL   = WA_SAIDA_MOV-ENT_FISCAL.
    WA_SAIDA_ENTR_FISCAL-PARID         = WA_SAIDA_MOV-PARID.
    WA_SAIDA_ENTR_FISCAL-PESO_LIQRET   = WA_SAIDA_MOV-ENT_LIQRET.

    APPEND WA_SAIDA_ENTR_FISCAL TO IT_SAIDA_ENTR_FISCAL.

  ENDLOOP.


*  SELECT * FROM ZSDT0001
*    INTO TABLE IT_ENTR_FISCAL
*  WHERE TP_MOVIMENTO   EQ 'E'
*    AND BUKRS          EQ P_BUKRS
*    AND BRANCH         EQ P_BRANCH
*    AND NR_SAFRA       EQ P_NR_SAFRA
*    AND LOCAL_DESCARGA IN P_LCREC
*    AND MATNR          EQ P_MATNR
*    AND PARID          IN P_FORN
*    AND DT_MOVIMENTO   EQ P_DATA_MOVIMENTO.
*
*  LOOP AT IT_ENTR_FISCAL INTO WA_ENTR_FISCAL.
*
*    CLEAR: WA_SAIDA_ENTR_FISCAL.
*
*    WA_SAIDA_ENTR_FISCAL-DT_MOVIMENTO  = WA_ENTR_FISCAL-DT_MOVIMENTO.
*    WA_SAIDA_ENTR_FISCAL-NR_ROMANEIO   = WA_ENTR_FISCAL-NR_ROMANEIO.
*    WA_SAIDA_ENTR_FISCAL-DOCDAT        = WA_ENTR_FISCAL-DOCDAT.
*    WA_SAIDA_ENTR_FISCAL-PESO_SUBTOTAL = WA_ENTR_FISCAL-PESO_SUBTOTAL.
*    WA_SAIDA_ENTR_FISCAL-NFNUM         = WA_ENTR_FISCAL-NFNUM.
*    WA_SAIDA_ENTR_FISCAL-NETWR         = WA_ENTR_FISCAL-NETWR.
*    WA_SAIDA_ENTR_FISCAL-PESO_FISCAL   = WA_ENTR_FISCAL-PESO_FISCAL.
*    WA_SAIDA_ENTR_FISCAL-PARID         = WA_ENTR_FISCAL-PARID.
*
*    CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
*      EXPORTING
*        I_BUKRS         = WA_ENTR_FISCAL-BUKRS
*        I_BRANCH        = WA_ENTR_FISCAL-BRANCH
*        I_LIFNR         = WA_ENTR_FISCAL-PARID
*        I_PESO          = WA_ENTR_FISCAL-PESO_SUBTOTAL
*        I_PESO_FISCAL   = WA_ENTR_FISCAL-PESO_FISCAL
*      IMPORTING
*        E_PESO_LIQUIDO  = WA_SAIDA_ENTR_FISCAL-PESO_LIQRET.
*
*
*    APPEND WA_SAIDA_ENTR_FISCAL TO IT_SAIDA_ENTR_FISCAL.
*
*
*  ENDLOOP.




  CHECK NOT IT_SAIDA_ENTR_FISCAL[] IS INITIAL.

  PERFORM: CRIAR_ALV_ENTR_FISCAL.

  CALL SCREEN 0200.
ENDFORM.                    " ENTR_FISCAL
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_ENTR_FISCAL
*&---------------------------------------------------------------------*
FORM CRIAR_ALV_ENTR_FISCAL .

  DATA: WL_VARIANT  TYPE DISVARIANT.

  IF ( CL_CONTAINER_ENTRADA IS INITIAL ).
    PERFORM: CRIAR_CATALOG_ENTRADA.

    CREATE OBJECT CL_CONTAINER_ENTRADA
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_ENTRADA'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CREATE OBJECT CL_GRID_ENTRADA
      EXPORTING
        I_PARENT          = CL_CONTAINER_ENTRADA
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    WL_VARIANT-REPORT   = SY-REPID.
    WL_VARIANT-USERNAME = SY-UNAME.

    CALL METHOD CL_GRID_ENTRADA->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*       IS_LAYOUT                     = WL_LAYOUT
*       I_SAVE                        = 'A'
*       IT_TOOLBAR_EXCLUDING          = TL_FUNCTION
        IS_VARIANT                    = WL_VARIANT
      CHANGING
        IT_OUTTAB                     = IT_SAIDA_ENTR_FISCAL[]
        IT_FIELDCATALOG               = IT_FIELDCAT_ENTRADA[]
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

  ELSE.
    CALL METHOD CL_GRID_ENTRADA->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

ENDFORM.                    " CRIAR_ALV_ENTR_FISCAL
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_ENTRADA
*&---------------------------------------------------------------------*
FORM CRIAR_CATALOG_ENTRADA .
  REFRESH: IT_FIELDCAT_ENTRADA[].
  PERFORM MONTAR_CATALOG_ENTRADA USING:

        'DT_MOVIMENTO'    'Data Movimento.'   '10'    ''  '' '' '' ''  ''  '' '' '' '',
        'NR_ROMANEIO'     'Nr.Romaneio'       '8'     ''  '' '' '' ''  ''  '' '' '' '',
        'DOCDAT'          'Data Nota'         '10'    ''  '' '' '' ''  ''  '' '' '' '',
        'NFNUM'           'Nota'              '10'    ''  '' '' '' ''  ''  '' '' '' '',
        'PESO_SUBTOTAL'   'Peso Físico'       '10'    ''  '' '' '' 'X' ''  '' '' '' '0',
        'PESO_FISCAL'     'Peso Fiscal'       '10'    ''  '' '' '' 'X' ''  '' '' '' '',
        'PESO_LIQRET'     'Peso Liq.Ret'      '10'    ''  '' '' '' 'X' ''  '' '' '' '',
        'NETWR'           'Vlr. Nota'         '12'    ''  '' '' '' 'X' ''  '' '' '' '',
        'PARID'           'Cod. Fornecedor'   '15'    'X' '' '' '' ''  ''  '' '' '' ''.

ENDFORM.                    " CRIAR_CATALOG_ENTRADA
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_ENTRADA
*&---------------------------------------------------------------------*
FORM MONTAR_CATALOG_ENTRADA  USING   VALUE(P_FIELDNAME)
                                     VALUE(P_DESC)
                                     VALUE(P_TAM)
                                     VALUE(P_NO_ZERO)
                                     VALUE(P_HOTSPOT)
                                     VALUE(P_COR)
                                     VALUE(P_JUST)
                                     VALUE(P_SUM)
                                     VALUE(P_EDIT)
                                     VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                                     VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                                     VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                                     VALUE(P_DECIMALS).


  CLEAR: WA_FIELDCAT_ENTRADA.

  WA_FIELDCAT_ENTRADA-FIELDNAME  = P_FIELDNAME.
  WA_FIELDCAT_ENTRADA-REF_TABLE  = P_REF_TABNAME..
  WA_FIELDCAT_ENTRADA-REF_FIELD  = P_REF_FIELDNAME.
  WA_FIELDCAT_ENTRADA-TABNAME    = P_TABNAME.
  WA_FIELDCAT_ENTRADA-SCRTEXT_L  = P_DESC.
  WA_FIELDCAT_ENTRADA-SCRTEXT_M  = P_DESC.
  WA_FIELDCAT_ENTRADA-SCRTEXT_S  = P_DESC.
  WA_FIELDCAT_ENTRADA-OUTPUTLEN  = P_TAM.
  WA_FIELDCAT_ENTRADA-NO_ZERO    = P_NO_ZERO.
  WA_FIELDCAT_ENTRADA-HOTSPOT    = P_HOTSPOT.
  WA_FIELDCAT_ENTRADA-EMPHASIZE  = P_COR.
  WA_FIELDCAT_ENTRADA-JUST       = P_JUST.
  WA_FIELDCAT_ENTRADA-DO_SUM     = P_SUM.
  WA_FIELDCAT_ENTRADA-EDIT       = P_EDIT.
  WA_FIELDCAT_ENTRADA-DECIMALS_O = P_DECIMALS.

  APPEND WA_FIELDCAT_ENTRADA TO IT_FIELDCAT_ENTRADA.


ENDFORM.                    " MONTAR_CATALOG_ENTRADA
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR  'TB0200'.
ENDMODULE.                 " PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0200 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'CANC'.
      REFRESH: IT_SAIDA_ENTR_FISCAL[].
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " PAI_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAIDA_FISCAL
*&---------------------------------------------------------------------*

FORM SAIDA_FISCAL  USING   P_BUKRS          TYPE ZSDT0001-BUKRS
                           P_BRANCH         TYPE ZSDT0001-BRANCH
                           P_NR_SAFRA       TYPE ZSDT0001-NR_SAFRA
                           P_MATNR          TYPE ZSDT0001-MATNR
                           P_DATA_MOVIMENTO TYPE ZSDT0001-DT_MOVIMENTO.


  DATA: IT_SAIDA_FISCAL TYPE TABLE OF ZSDT0001,
        WL_SAIDA_FISCAL TYPE ZSDT0001,
        IT_J_1BNFDOC    TYPE TABLE OF J_1BNFDOC,
        WL_J_1BNFDOC    TYPE J_1BNFDOC,
        IT_0060         TYPE TABLE OF ZLEST0060,
        WA_0060         TYPE ZLEST0060.

  REFRESH: IT_SAIDA_FISCAL[], IT_0060[], IT_SAIDA_FISCAL_ALV[].

  LOOP AT IT_SAIDA_MOV INTO WA_SAIDA_MOV WHERE DT_MOVIMENTO   EQ P_DATA_MOVIMENTO
                                           AND NR_SAFRA       EQ P_NR_SAFRA
                                           AND ( SAI_FISICO   NE 0 OR
                                                 SAI_FISCAL   NE 0 ).

    CLEAR: WA_SAIDA_FISCAL_ALV.

    WA_SAIDA_FISCAL_ALV-DT_MOVIMENTO  = WA_SAIDA_MOV-DT_MOVIMENTO.
    WA_SAIDA_FISCAL_ALV-NR_ROMANEIO   = WA_SAIDA_MOV-NR_ROMANEIO.
    WA_SAIDA_FISCAL_ALV-DOCDAT        = WA_SAIDA_MOV-DOCDAT.
    WA_SAIDA_FISCAL_ALV-PESO_SUBTOTAL = WA_SAIDA_MOV-SAI_FISICO.
    WA_SAIDA_FISCAL_ALV-NFNUM         = WA_SAIDA_MOV-NFNUM.
    WA_SAIDA_FISCAL_ALV-NETWR         = WA_SAIDA_MOV-NETWR.
    WA_SAIDA_FISCAL_ALV-PESO_FISCAL   = WA_SAIDA_MOV-SAI_FISCAL.
    WA_SAIDA_FISCAL_ALV-PARID         = WA_SAIDA_MOV-PARID.
    WA_SAIDA_FISCAL_ALV-PESO_LIQRET   = WA_SAIDA_MOV-SAI_LIQRET.
    WA_SAIDA_FISCAL_ALV-DOCNUM        = WA_SAIDA_MOV-DOCNUM.

    APPEND WA_SAIDA_FISCAL_ALV TO IT_SAIDA_FISCAL_ALV.

  ENDLOOP.


*  SELECT * FROM ZSDT0001
*    INTO TABLE IT_SAIDA_FISCAL
*   WHERE TP_MOVIMENTO  EQ 'E'
*    AND BUKRS          EQ P_BUKRS
*    AND BRANCH         EQ P_BRANCH
*    AND LOCAL_DESCARGA IN P_LCREC
*    AND NR_SAFRA       EQ P_NR_SAFRA
*    AND MATNR          EQ P_MATNR
*    AND PARID          IN P_FORN
*    AND CT_AQUAV       EQ 'X'.
*
*  CHECK NOT IT_SAIDA_FISCAL[] IS INITIAL.
*
*  "Busca Notas Vinculadas
*  SELECT *
*    FROM ZLEST0060 INTO TABLE IT_0060
*    FOR ALL ENTRIES IN IT_SAIDA_FISCAL
*  WHERE NR_ROMANEIO EQ IT_SAIDA_FISCAL-NR_ROMANEIO
*    AND SAFRA       EQ IT_SAIDA_FISCAL-NR_SAFRA
*    AND RM_CODIGO   EQ IT_SAIDA_FISCAL-PARID
*    AND NFNUM       EQ IT_SAIDA_FISCAL-NFNUM.
*
*  IF IT_0060[] IS NOT INITIAL.
*
*    SELECT * FROM J_1BNFDOC
*      INTO TABLE IT_J_1BNFDOC
*      FOR ALL ENTRIES IN IT_0060
*    WHERE DOCNUM EQ  IT_0060-DOCNUM
*      AND PSTDAT EQ P_DATA_MOVIMENTO.
*
*  ENDIF.
*
*  LOOP AT IT_SAIDA_FISCAL INTO WL_SAIDA_FISCAL.
*
*    CLEAR: WA_0060.
*    LOOP AT IT_0060 INTO WA_0060 WHERE NR_ROMANEIO = WL_SAIDA_FISCAL-NR_ROMANEIO
*                                   AND SAFRA       = WL_SAIDA_FISCAL-NR_SAFRA
*                                   AND RM_CODIGO   = WL_SAIDA_FISCAL-PARID
*                                   AND NFNUM       = WL_SAIDA_FISCAL-NFNUM.
*
*      READ TABLE IT_J_1BNFDOC INTO WL_J_1BNFDOC WITH KEY DOCNUM = WA_0060-DOCNUM.
*
*      IF ( SY-SUBRC EQ 0 ).
*        WA_SAIDA_FISCAL_ALV-DT_MOVIMENTO  = WL_J_1BNFDOC-PSTDAT.
*        WA_SAIDA_FISCAL_ALV-NR_ROMANEIO   = WL_SAIDA_FISCAL-NR_ROMANEIO.
*        WA_SAIDA_FISCAL_ALV-DOCDAT        = WL_SAIDA_FISCAL-DOCDAT.
*        WA_SAIDA_FISCAL_ALV-NFNUM         = WL_SAIDA_FISCAL-NFNUM.
*
*        IF WA_0060-PESO_SUBTOTAL IS NOT INITIAL.
*          WA_SAIDA_FISCAL_ALV-PESO_SUBTOTAL = WA_0060-PESO_SUBTOTAL.
*        ELSE.
*          WA_SAIDA_FISCAL_ALV-PESO_SUBTOTAL = WL_SAIDA_FISCAL-PESO_SUBTOTAL.
*        ENDIF.
*
*        IF WA_0060-PESO_FISCAL IS NOT INITIAL.
*          WA_SAIDA_FISCAL_ALV-NETWR         = WA_0060-NETWR.
*          WA_SAIDA_FISCAL_ALV-PESO_FISCAL   = WA_0060-PESO_FISCAL.
*        ELSE.
*          WA_SAIDA_FISCAL_ALV-NETWR         = WL_SAIDA_FISCAL-NETWR.
*          WA_SAIDA_FISCAL_ALV-PESO_FISCAL   = WL_SAIDA_FISCAL-PESO_FISCAL.
*        ENDIF.
*
*        WA_SAIDA_FISCAL_ALV-PESO_LIQRET     = WA_0060-PESO_LIQ_RET.
*
*        WA_SAIDA_FISCAL_ALV-PARID         = WL_SAIDA_FISCAL-PARID.
*        WA_SAIDA_FISCAL_ALV-DOCNUM        = WL_J_1BNFDOC-DOCNUM.
*
*        APPEND WA_SAIDA_FISCAL_ALV TO IT_SAIDA_FISCAL_ALV.
*
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDLOOP.

  CHECK NOT IT_SAIDA_FISCAL_ALV IS INITIAL.

  PERFORM: CRIAR_ALV_SAIDA_FISCAL.

  CALL SCREEN 0300.

ENDFORM.                    " SAIDA_FISCAL
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_SAIDA_FISCAL
*&---------------------------------------------------------------------*
FORM CRIAR_ALV_SAIDA_FISCAL .

  DATA: WL_VARIANT  TYPE DISVARIANT.

  IF ( CL_CONTAINER_SAIDA IS INITIAL ).
    PERFORM: CRIAR_CATALOG_SAIDA.

    CREATE OBJECT CL_CONTAINER_SAIDA
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_SAIDA'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CREATE OBJECT CL_GRID_SAIDA
      EXPORTING
        I_PARENT          = CL_CONTAINER_SAIDA
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    WL_VARIANT-REPORT   = SY-REPID.
    WL_VARIANT-USERNAME = SY-UNAME.

    CALL METHOD CL_GRID_SAIDA->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*       IS_LAYOUT                     = WL_LAYOUT
*       I_SAVE                        = 'A'
*       IT_TOOLBAR_EXCLUDING          = TL_FUNCTION
        IS_VARIANT                    = WL_VARIANT
      CHANGING
        IT_OUTTAB                     = IT_SAIDA_FISCAL_ALV[]
        IT_FIELDCATALOG               = IT_FIELDCAT_SAIDA[]
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

  ELSE.
    CALL METHOD CL_GRID_SAIDA->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

ENDFORM.                    " CRIAR_ALV_SAIDA_FISCAL
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_SAIDA
*&---------------------------------------------------------------------*
FORM CRIAR_CATALOG_SAIDA .

  REFRESH: IT_FIELDCAT_SAIDA[].

  PERFORM MONTAR_CATALOG_SAIDA USING:

        'DT_MOVIMENTO'    'Data Movimento.'   '10'   ''  '' '' '' ''  '' '' '' '' '',
        'NR_ROMANEIO'     'Nr.Romaneio'       '8'    ''  '' '' '' ''  '' '' '' '' '',
        'DOCDAT'          'Data Nota'         '10'   ''  '' '' '' ''  '' '' '' '' '',
        'NFNUM'           'Nota'              '10'   ''  '' '' '' ''  '' '' '' '' '',
        'PESO_SUBTOTAL'   'Peso Físico'       '10'   ''  '' '' '' 'X' '' '' '' '' '0',
        'PESO_FISCAL'     'Peso Fiscal'       '10'   ''  '' '' '' 'X' '' '' '' '' '',
        'NETWR'           'Vlr. Nota'         '12'   ''  '' '' '' 'X' '' '' '' '' '',
        'PESO_LIQRET'     'Peso Liq.Ret'      '10'   ''  '' '' '' 'X' '' '' '' '' '',
        'DOCNUM'          'Docnum'            '12'   ''  '' '' '' ''  '' '' '' '' '',
        'PARID'           'Cod. Fornecedor'   '15'   'X' '' '' '' ''  '' '' '' '' ''.

ENDFORM.                    " CRIAR_CATALOG_SAIDA
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_SAIDA
*&---------------------------------------------------------------------*
FORM MONTAR_CATALOG_SAIDA  USING     VALUE(P_FIELDNAME)
                                     VALUE(P_DESC)
                                     VALUE(P_TAM)
                                     VALUE(P_NO_ZERO)
                                     VALUE(P_HOTSPOT)
                                     VALUE(P_COR)
                                     VALUE(P_JUST)
                                     VALUE(P_SUM)
                                     VALUE(P_EDIT)
                                     VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                                     VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                                     VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                                     VALUE(P_DECIMALS).


  CLEAR: WA_FIELDCAT_SAIDA.

  WA_FIELDCAT_SAIDA-FIELDNAME  = P_FIELDNAME.
  WA_FIELDCAT_SAIDA-REF_TABLE  = P_REF_TABNAME..
  WA_FIELDCAT_SAIDA-REF_FIELD  = P_REF_FIELDNAME.
  WA_FIELDCAT_SAIDA-TABNAME    = P_TABNAME.
  WA_FIELDCAT_SAIDA-SCRTEXT_L  = P_DESC.
  WA_FIELDCAT_SAIDA-SCRTEXT_M  = P_DESC.
  WA_FIELDCAT_SAIDA-SCRTEXT_S  = P_DESC.
  WA_FIELDCAT_SAIDA-OUTPUTLEN  = P_TAM.
  WA_FIELDCAT_SAIDA-NO_ZERO    = P_NO_ZERO.
  WA_FIELDCAT_SAIDA-HOTSPOT    = P_HOTSPOT.
  WA_FIELDCAT_SAIDA-EMPHASIZE  = P_COR.
  WA_FIELDCAT_SAIDA-JUST       = P_JUST.
  WA_FIELDCAT_SAIDA-DO_SUM     = P_SUM.
  WA_FIELDCAT_SAIDA-EDIT       = P_EDIT.
  WA_FIELDCAT_SAIDA-DECIMALS_O = P_DECIMALS.

  APPEND WA_FIELDCAT_SAIDA TO IT_FIELDCAT_SAIDA.


ENDFORM.                    " MONTAR_CATALOG_SAIDA
*&--------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0300 OUTPUT.
  SET PF-STATUS 'PF0300'.
  SET TITLEBAR  'TB0300'.
ENDMODULE.                 " PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0300  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0300 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'CANC'.
      REFRESH: IT_SAIDA_FISCAL_ALV[].
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " PAI_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  VALIDA_PARAMETROS
*&---------------------------------------------------------------------*
FORM VALIDA_PARAMETROS .

  IF P_SAFRA IS INITIAL AND P_DATA IS INITIAL.
    MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALDO_FISCAL
*&---------------------------------------------------------------------*
FORM SALDO_FISCAL  USING    P_BUKRS             TYPE ZSDT0001-BUKRS
                            P_BRANCH            TYPE ZSDT0001-BRANCH
                            P_NR_SAFRA          TYPE ZSDT0001-NR_SAFRA
                            P_MATNR             TYPE ZSDT0001-MATNR
                            P_DATA_MOVIMENTO    TYPE ZSDT0001-DT_MOVIMENTO
                            P_DATA              TYPE ANY TABLE.

  DATA: IT_ZSDT0001_SALDO  TYPE TABLE OF ZSDT0001,
        WA_ZSDT0001_SALDO  TYPE ZSDT0001,
        IT_J_1BNFDOC_SALDO TYPE TABLE OF TY_J_1BNFDOC,
        IT_0060_SLD        TYPE TABLE OF ZLEST0060,
        WA_0060_SLD        TYPE ZLEST0060,
        P_DATA_RANGE       TYPE RANGE OF ZSDT0001-DT_MOVIMENTO.

  IF P_DATA IS NOT INITIAL.
    MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0100.
  ENDIF.

  P_DATA_RANGE = P_DATA.
  REFRESH:IT_ZSDT0001_SALDO[].

  SELECT *
    FROM ZSDT0001
    INTO TABLE IT_ZSDT0001_SALDO
    WHERE TP_MOVIMENTO   EQ 'E'
    AND BUKRS            EQ P_BUKRS
    AND BRANCH           EQ P_BRANCH
    AND NR_SAFRA         EQ P_NR_SAFRA
    AND LOCAL_DESCARGA   IN P_LCREC
    AND MATNR            EQ P_MATNR
    AND PARID            IN P_FORN
    AND DT_MOVIMENTO     LE P_DATA_MOVIMENTO.

  CHECK NOT IT_ZSDT0001_SALDO[] IS INITIAL.

  "Busca Notas Vinculadas
  SELECT *
    FROM ZLEST0060 INTO TABLE IT_0060_SLD
    FOR ALL ENTRIES IN IT_ZSDT0001_SALDO
  WHERE NR_ROMANEIO EQ IT_ZSDT0001_SALDO-NR_ROMANEIO
    AND SAFRA       EQ IT_ZSDT0001_SALDO-NR_SAFRA
    AND RM_CODIGO   EQ IT_ZSDT0001_SALDO-PARID
    AND NFNUM       EQ IT_ZSDT0001_SALDO-NFNUM.

  IF IT_0060_SLD[] IS NOT INITIAL.

    SELECT DOCNUM PSTDAT
      FROM J_1BNFDOC
      INTO TABLE IT_J_1BNFDOC_SALDO
      FOR ALL ENTRIES IN IT_0060_SLD
      WHERE DOCNUM    EQ IT_0060_SLD-DOCNUM
      AND PSTDAT      LE P_DATA_MOVIMENTO.

  ENDIF.

  LOOP AT IT_ZSDT0001_SALDO INTO WA_ZSDT0001_SALDO.
    CLEAR: WA_0060_SLD.
    LOOP AT IT_0060_SLD INTO WA_0060_SLD WHERE NR_ROMANEIO = WA_ZSDT0001_SALDO-NR_ROMANEIO
                                           AND SAFRA       = WA_ZSDT0001_SALDO-NR_SAFRA
                                           AND RM_CODIGO   = WA_ZSDT0001_SALDO-PARID
                                           AND NFNUM       = WA_ZSDT0001_SALDO-NFNUM.

      READ TABLE IT_J_1BNFDOC_SALDO WITH KEY DOCNUM = WA_0060_SLD-DOCNUM TRANSPORTING NO FIELDS.
      IF SY-SUBRC NE 0.
        WA_SAIDA_SALDO_FISCAL-DT_MOVIMENTO  = WA_ZSDT0001_SALDO-DT_MOVIMENTO.
        WA_SAIDA_SALDO_FISCAL-NR_ROMANEIO   = WA_ZSDT0001_SALDO-NR_ROMANEIO.
        WA_SAIDA_SALDO_FISCAL-DOCDAT        = WA_ZSDT0001_SALDO-DOCDAT.
        WA_SAIDA_SALDO_FISCAL-NFNUM         = WA_ZSDT0001_SALDO-NFNUM.

        IF WA_0060_SLD-PESO_SUBTOTAL IS NOT INITIAL.
          WA_SAIDA_SALDO_FISCAL-PESO_SUBTOTAL = WA_0060_SLD-PESO_SUBTOTAL.
        ELSE.
          WA_SAIDA_SALDO_FISCAL-PESO_SUBTOTAL = WA_ZSDT0001_SALDO-PESO_SUBTOTAL.
        ENDIF.

        IF WA_0060_SLD-PESO_FISCAL IS NOT INITIAL.
          WA_SAIDA_SALDO_FISCAL-NETWR         = WA_0060_SLD-NETWR.
          WA_SAIDA_SALDO_FISCAL-PESO_FISCAL   = WA_0060_SLD-PESO_FISCAL.
        ELSE.
          WA_SAIDA_SALDO_FISCAL-NETWR         = WA_ZSDT0001_SALDO-NETWR.
          WA_SAIDA_SALDO_FISCAL-PESO_FISCAL   = WA_ZSDT0001_SALDO-PESO_FISCAL.
        ENDIF.

        WA_SAIDA_SALDO_FISCAL-PESO_LIQRET     = WA_0060_SLD-PESO_LIQ_RET.

        WA_SAIDA_SALDO_FISCAL-PARID         = WA_ZSDT0001_SALDO-PARID.
        APPEND WA_SAIDA_SALDO_FISCAL TO IT_SAIDA_SALDO_FISCAL.
      ENDIF.

    ENDLOOP.
  ENDLOOP.

  CHECK NOT IT_SAIDA_SALDO_FISCAL[] IS INITIAL.
  PERFORM: CRIAR_ALV_SALDO_FISCAL.

  CALL SCREEN 0400.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_SALDO_FISCAL
*&---------------------------------------------------------------------*
FORM CRIAR_ALV_SALDO_FISCAL .

  DATA: WL_VARIANT  TYPE DISVARIANT.

  IF ( CL_CONTAINER_SALDO IS INITIAL ).
    PERFORM: CRIAR_CATALOG_ENTRADA.

    CREATE OBJECT CL_CONTAINER_SALDO
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_SALDO'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CREATE OBJECT CL_GRID_SALDO
      EXPORTING
        I_PARENT          = CL_CONTAINER_SALDO
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    WL_VARIANT-REPORT   = SY-REPID.
    WL_VARIANT-USERNAME = SY-UNAME.

    CALL METHOD CL_GRID_SALDO->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*       IS_LAYOUT                     = WL_LAYOUT
*       I_SAVE                        = 'A'
*       IT_TOOLBAR_EXCLUDING          = TL_FUNCTION
        IS_VARIANT                    = WL_VARIANT
      CHANGING
        IT_OUTTAB                     = IT_SAIDA_SALDO_FISCAL[]
        IT_FIELDCATALOG               = IT_FIELDCAT_ENTRADA[]
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

  ELSE.
    CALL METHOD CL_GRID_SALDO->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

ENDFORM.                    " CRIAR_ALV_SALDO_FISCAL
*&---------------------------------------------------------------------*
*&      Module  PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0400 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR  'TB0400'.
ENDMODULE.                 " PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0400  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0400 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'CANC'.
      REFRESH: IT_SAIDA_SALDO_FISCAL[].
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " PAI_0400  INPUT
