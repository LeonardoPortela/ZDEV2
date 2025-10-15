*&---------------------------------------------------------------------*
*& Report  ZMMR033
*&
*&---------------------------------------------------------------------*
*&TITULO: Relatório de Despesas com Vendas
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 20.05.2013
*&---------------------------------------------------------------------*


REPORT  ZMMR033.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ZMMT0033, MAKT, EKBE, ZNOM_TRANSPORTE.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
      DATA: END OF IT_MSG.
DATA: WL_MODE(1).

TYPES:
  BEGIN OF TY_ZMMT0033,
    BUKRS            TYPE ZMMT0033-BUKRS,
    NAVIO            TYPE ZMMT0033-ID_TRANSPORTE,
    DS_NOME_TRANSPOR TYPE ZMMT0033-DS_NOME_TRANSPOR,
    BANFN            TYPE ZMMT0033-BANFN,
    BNFPO            TYPE ZMMT0033-BNFPO,
    ID_NOMEACAO_TRAN TYPE ZMMT0033-ID_NOMEACAO_TRAN,
    INSTRUCAO        TYPE ZMMT0033-INSTRUCAO,
  END OF TY_ZMMT0033,

  BEGIN OF TY_ZNOM_TRANSPORTE,
    ID_NOMEACAO_TRAN TYPE ZNOM_TRANSPORTE-ID_NOMEACAO_TRAN,
    DS_PORTO         TYPE ZNOM_TRANSPORTE-DS_PORTO,
  END OF TY_ZNOM_TRANSPORTE,

  BEGIN OF TY_PEDI,
    EBELN  TYPE EKPO-EBELN,
    EBELP  TYPE EKPO-EBELP,
    TXZ01  TYPE EKPO-TXZ01,
    PACKNO TYPE EKPO-PACKNO,
    LIFNR  TYPE EKKO-LIFNR,
    NETPR  TYPE EKPO-NETPR,
  END OF TY_PEDI,

  BEGIN OF TY_EKKN,
    EBELN TYPE EKKN-EBELN,
    EBELP TYPE EKKN-EBELP,
    SAKTO TYPE EKKN-SAKTO,
    KOSTL TYPE EKKN-KOSTL,
  END OF TY_EKKN,


  BEGIN OF TY_ZMMT0034,
    KOSTL    TYPE ZMMT0034-KOSTL,
    MATNR    TYPE ZMMT0034-MATNR,
    DS_PORTO TYPE ZMMT0034-DS_PORTO,
  END OF TY_ZMMT0034,

  BEGIN OF TY_MAKT,
    MATNR TYPE MAKT-MATNR	,
    MAKTX TYPE MAKT-MAKTX,
  END OF TY_MAKT,

  BEGIN OF TY_ESLL,
    PACKNO     TYPE ESLL-PACKNO,
    SUB_PACKNO TYPE ESLL-SUB_PACKNO,
    KTEXT1     TYPE ESLL-KTEXT1,
  END OF TY_ESLL,

  BEGIN OF TY_EKBE,
    EBELN TYPE EKBE-EBELN,
    VGABE TYPE EKBE-VGABE,
    BUDAT TYPE EKBE-BUDAT,
    BELNR TYPE EKBE-BELNR,
    GJAHR TYPE EKBE-GJAHR,
    AWKEY TYPE BKPF-AWKEY,
  END OF TY_EKBE,

  BEGIN OF TY_BKPF,
    BUKRS TYPE BKPF-BUKRS,
    GJAHR TYPE BKPF-GJAHR,
    AWKEY TYPE BKPF-AWKEY,
    BELNR TYPE BKPF-BELNR,
  END OF TY_BKPF,

  BEGIN OF TY_BSAK,
    BUKRS TYPE BSAK-BUKRS,
    BELNR TYPE BSAK-BELNR,
    GJAHR TYPE BSAK-GJAHR,
    XBLNR TYPE BSAK-XBLNR,
    SHKZG TYPE BSAK-SHKZG,
    DMBTR TYPE BSAK-DMBTR,
    DMBE2 TYPE BSAK-DMBE2,
    LIFNR TYPE BSAK-LIFNR,
  END OF TY_BSAK,

  BEGIN OF TY_BSIK,
    BUKRS TYPE BSIK-BUKRS,
    BELNR TYPE BSIK-BELNR,
    GJAHR TYPE BSIK-GJAHR,
    XBLNR TYPE BSIK-XBLNR,
    SHKZG TYPE BSIK-SHKZG,
    DMBTR TYPE BSIK-DMBTR,
    DMBE2 TYPE BSIK-DMBE2,
    LIFNR TYPE BSIK-LIFNR,
  END OF TY_BSIK,

  BEGIN OF TY_LFA1,
    LIFNR TYPE LFA1-LIFNR,
    NAME1 TYPE LFA1-NAME1,
  END OF TY_LFA1,

  BEGIN OF TY_SAIDA,
    DS_NOME_TRANSPOR TYPE ZMMT0033-DS_NOME_TRANSPOR,
    LIFNR            TYPE BSAK-LIFNR,
    NAME1            TYPE LFA1-NAME1,
    TXZ01            TYPE EKPO-TXZ01,
    KTEXT1           TYPE ESLL-KTEXT1,
    MAKTX            TYPE MAKT-MAKTX,
    EBELN            TYPE EKPO-EBELN,
    EBELP            TYPE EKPO-EBELP,
    BELNR            TYPE BKPF-BELNR,
    DMBTR            TYPE BSAK-DMBTR,
    DMBE2            TYPE BSAK-DMBE2,
    GJAHR            TYPE EKBE-GJAHR,
    BUKRS            TYPE BKPF-BUKRS,
    PORTO            TYPE ZNOM_TRANSPORTE-DS_PORTO,
    NETPR            TYPE EKPO-NETPR,
    INSTRUCAO        TYPE ZMMT0033-INSTRUCAO,
  END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
         INCLUDE TYPE SLIS_FIELDCAT_MAIN.
         INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
       TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: TI_BDCDATA         TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      T_MESSTAB          TYPE TABLE OF BDCMSGCOLL,
      IT_ZNOM_TRANSPORTE TYPE TABLE OF TY_ZNOM_TRANSPORTE,
      IT_PEDI            TYPE TABLE OF TY_PEDI,
      IT_EKKN            TYPE TABLE OF TY_EKKN,
      IT_ZMMT0033        TYPE TABLE OF TY_ZMMT0033,
      IT_ZMMT0034        TYPE TABLE OF TY_ZMMT0034,
      IT_MAKT            TYPE TABLE OF TY_MAKT,
      IT_ESLL            TYPE TABLE OF TY_ESLL,
      IT_ESLL_2          TYPE TABLE OF TY_ESLL,
      IT_EKBE            TYPE TABLE OF TY_EKBE,
      IT_RBKP            TYPE TABLE OF RBKP,
      TI_RSEG_NEW        TYPE TABLE OF RSEG,
      IT_BKPF            TYPE TABLE OF TY_BKPF,
      IT_BSAK            TYPE TABLE OF TY_BSAK,
      IT_BSIK            TYPE TABLE OF TY_BSIK,
      IT_LFA1            TYPE TABLE OF TY_LFA1,
      IT_LFA1_AUX        TYPE TABLE OF TY_LFA1,
      IT_SAIDA           TYPE TABLE OF TY_SAIDA,
      IT_COLOR           TYPE TABLE OF LVC_S_SCOL,
      TG_MSG_RET         TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  WA_CONT            TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_ALV             TYPE REF TO CL_GUI_ALV_GRID,
  WA_BDCDATA         LIKE LINE OF TI_BDCDATA,
  WA_ZNOM_TRANSPORTE TYPE TY_ZNOM_TRANSPORTE,
  WA_PEDI            TYPE TY_PEDI,
  WA_EKKN            TYPE TY_EKKN,
  WA_ZMMT0033        TYPE TY_ZMMT0033,
  WA_ZMMT0034        TYPE TY_ZMMT0034,
  WA_MAKT            TYPE TY_MAKT,
  WA_ESLL            TYPE TY_ESLL,
  WA_ESLL_2          TYPE TY_ESLL,
  WA_EKBE            TYPE TY_EKBE,
  WA_RBKP            TYPE RBKP,
  WA_RSEG_NEW        TYPE RSEG,
  WA_BKPF            TYPE TY_BKPF,
  WA_BSAK            TYPE TY_BSAK,
  WA_BSIK            TYPE TY_BSIK,
  WA_LFA1            TYPE TY_LFA1,
  WA_SAIDA           TYPE TY_SAIDA.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA: IT_FCAT         TYPE TABLE OF TY_ESTRUTURA,
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

DATA: OK-CODE          TYPE SY-UCOMM,
      VNUM(10)         TYPE C,
      VSEQ(10)         TYPE P,
      WL_ERRO(1),
      WG_DOCUMENTO(10),
      VSTATUS(1),
      INDROW           TYPE LVC_T_ROW,
      W_IND            TYPE LVC_T_ROW WITH HEADER LINE,
      W_CONT           TYPE I,
      W_CONTC(5),
      W_MENSAGEM(50),
      CONT             TYPE SY-TABIX,
      W_FLAG(1)        VALUE '',
      IT_RETURN_TAB2   TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
      IT_DSELC2        TYPE TABLE OF DSELC      WITH HEADER LINE..

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
DATA: EDITCONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
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
      IS_STABLE       TYPE LVC_S_STBL VALUE 'XX',
      WG_REPNAME      LIKE SY-REPID,
      WG_X_VARIANT    LIKE DISVARIANT,
      WG_EXIT(1)      TYPE C,
      WG_SAVE(1)      TYPE C,
      WG_VARIANT      LIKE DISVARIANT.


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

    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          VL_VALUE TYPE LVC_VALUE.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'ID_TRANSPORTE'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

*      SELECT SINGLE ID_TRANSPORTE DS_NOME_TRANSPOR
*        FROM ZNOM_TRANSPORTE
*        INTO WA_ZNOM_TRANSPORTE
*          WHERE ID_TRANSPORTE EQ LV_VALUE.
*
*      IF SY-SUBRC IS INITIAL.
*        MOVE WA_ZNOM_TRANSPORTE-DS_NOME_TRANSPOR TO LV_VALUE.
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'DS_NOME_TRANSPOR'
*            I_VALUE     = LV_VALUE.
*      ELSE.
*
*      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED
  METHOD ON_DATA_CHANGED_FINISHED.

  ENDMETHOD.                    "on_data_changed_finisheD
  METHOD CATCH_HOTSPOT.
    IF E_ROW_ID IS NOT INITIAL.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX E_ROW_ID.
      IF E_COLUMN_ID = 'EBELN'.
        SET PARAMETER ID 'BES' FIELD WA_SAIDA-EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ELSEIF E_COLUMN_ID = 'BELNR'.
        SET PARAMETER ID 'BLN' FIELD WA_SAIDA-BELNR.
        SET PARAMETER ID 'BUK' FIELD WA_SAIDA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD WA_SAIDA-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "CATCH_HOTSPOT



ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

DATA:       EVENT_RECEIVER   TYPE REF TO LCL_EVENT_RECEIVER.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUKRS  FOR ZMMT0033-BUKRS OBLIGATORY,
                P_MATNR  FOR MAKT-MATNR               ,
                P_NAVIO  FOR ZMMT0033-DS_NOME_TRANSPOR," NO INTERVALS NO-EXTENSION,
                P_BUDAT  FOR EKBE-BUDAT OBLIGATORY.
PARAMETER:      P_PORTO  TYPE ZNOM_TRANSPORTE-DS_PORTO.
SELECTION-SCREEN: END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PORTO.
  PERFORM SEARCH_PORTO.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_NAVIO-LOW.
  PERFORM SEARCH_NAVIO.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_NAVIO-HIGH.
  PERFORM SEARCH_NAVIO.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: F_SELECIONA_DADOS, " Form seleciona dados
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

  SELECT BUKRS ID_TRANSPORTE DS_NOME_TRANSPOR BANFN BNFPO ID_NOMEACAO_TRAN INSTRUCAO
    FROM ZMMT0033
    INTO TABLE IT_ZMMT0033
    WHERE BUKRS            IN P_BUKRS
    AND   DS_NOME_TRANSPOR IN P_NAVIO.

  CHECK IT_ZMMT0033[] IS NOT INITIAL.

  SELECT ID_NOMEACAO_TRAN DS_PORTO
    FROM ZNOM_TRANSPORTE
    INTO CORRESPONDING FIELDS OF TABLE IT_ZNOM_TRANSPORTE
    FOR ALL ENTRIES IN IT_ZMMT0033
    WHERE ID_NOMEACAO_TRAN = IT_ZMMT0033-ID_NOMEACAO_TRAN.

  SELECT EKPO~EBELN EKPO~EBELP EKPO~TXZ01 EKPO~PACKNO EKKO~LIFNR EKPO~NETPR
    FROM EKPO
    INNER JOIN EKKO ON EKKO~EBELN = EKPO~EBELN
    INTO TABLE IT_PEDI
    FOR ALL ENTRIES IN IT_ZMMT0033
    WHERE   EKPO~EBELN      EQ  IT_ZMMT0033-BANFN
    AND     EKPO~EBELP      EQ  IT_ZMMT0033-BNFPO.

  IF IT_PEDI[] IS NOT INITIAL.
    SELECT  EBELN EBELP SAKTO KOSTL
      FROM EKKN
      INTO TABLE IT_EKKN
      FOR ALL ENTRIES IN IT_PEDI
      WHERE EBELN     EQ  IT_PEDI-EBELN
      AND   EBELP     EQ  IT_PEDI-EBELP.

    SELECT  KOSTL MATNR DS_PORTO
      FROM ZMMT0034
      INTO TABLE IT_ZMMT0034
      FOR ALL ENTRIES IN IT_EKKN
      WHERE KOSTL	EQ IT_EKKN-KOSTL
      AND MATNR IN P_MATNR.

    IF IT_ZMMT0034[] IS NOT INITIAL.
      SELECT MATNR MAKTX
        FROM MAKT
        INTO TABLE IT_MAKT
        FOR ALL ENTRIES IN IT_ZMMT0034
        WHERE MATNR EQ IT_ZMMT0034-MATNR
        AND   SPRAS	EQ SY-LANGU.
    ENDIF.

    SELECT PACKNO SUB_PACKNO KTEXT1
      FROM ESLL
      INTO TABLE IT_ESLL
      FOR ALL ENTRIES IN IT_PEDI
      WHERE PACKNO  EQ  IT_PEDI-PACKNO.

    IF IT_ESLL[] IS NOT INITIAL.
      SELECT PACKNO SUB_PACKNO KTEXT1
      FROM ESLL
      INTO TABLE IT_ESLL_2
      FOR ALL ENTRIES IN IT_ESLL
      WHERE PACKNO  EQ  IT_ESLL-SUB_PACKNO.
    ENDIF.

*    SELECT EBELN VGABE BUDAT BELNR GJAHR
*      FROM EKBE
*      INTO TABLE IT_EKBE
*      FOR ALL ENTRIES IN IT_EBAN
*      WHERE EBELN EQ IT_EBAN-EBELN
*        AND VGABE IN ('2','3')
*        AND BUDAT IN P_BUDAT.

    SELECT EBELN VGABE BUDAT BELNR GJAHR
      FROM EKBE
      INTO TABLE IT_EKBE
      FOR ALL ENTRIES IN IT_PEDI
      WHERE EBELN EQ IT_PEDI-EBELN
        AND VGABE IN ('2','3')
        AND BUDAT IN P_BUDAT
        AND BEWTP EQ 'Q'.

    IF IT_EKBE IS NOT INITIAL.
      SELECT *
        FROM RBKP
        INTO TABLE IT_RBKP
        FOR ALL ENTRIES IN IT_EKBE
        WHERE BELNR EQ IT_EKBE-BELNR
          AND GJAHR EQ IT_EKBE-GJAHR
          AND TCODE EQ 'MIRO'
          AND STBLG EQ ''.

      REFRESH IT_EKBE.

      SELECT EBELN VGABE BUDAT BELNR GJAHR
        FROM EKBE
        INTO TABLE IT_EKBE
        FOR ALL ENTRIES IN IT_RBKP
        WHERE BELNR EQ IT_RBKP-BELNR
          AND GJAHR EQ IT_RBKP-GJAHR.

    ENDIF.


    LOOP AT IT_EKBE INTO WA_EKBE.
      CONCATENATE WA_EKBE-BELNR WA_EKBE-GJAHR INTO WA_EKBE-AWKEY .
      MODIFY IT_EKBE FROM WA_EKBE INDEX SY-TABIX TRANSPORTING AWKEY.
    ENDLOOP.

    IF  IT_EKBE[] IS NOT INITIAL.
      SELECT BUKRS GJAHR AWKEY BELNR
        FROM BKPF
        INTO TABLE IT_BKPF
        FOR ALL ENTRIES IN IT_EKBE
        WHERE AWKEY EQ IT_EKBE-AWKEY
        AND   GJAHR EQ IT_EKBE-GJAHR.

      IF IT_BKPF[] IS NOT INITIAL.
        SELECT BUKRS BELNR GJAHR XBLNR SHKZG DMBTR DMBE2 LIFNR
          FROM BSAK
          INTO TABLE IT_BSAK
          FOR ALL ENTRIES IN IT_BKPF
          WHERE BUKRS EQ  IT_BKPF-BUKRS
          AND  BELNR  EQ  IT_BKPF-BELNR
          AND  GJAHR  EQ  IT_BKPF-GJAHR.

        IF IT_BSAK[] IS NOT INITIAL.
          SELECT LIFNR NAME1
          FROM LFA1
          INTO TABLE IT_LFA1
          FOR ALL ENTRIES IN IT_BSAK
          WHERE LIFNR EQ IT_BSAK-LIFNR.
        ENDIF.

        SELECT BUKRS BELNR GJAHR XBLNR SHKZG DMBTR DMBE2 LIFNR
          FROM BSIK
          INTO TABLE IT_BSIK
          FOR ALL ENTRIES IN IT_BKPF
          WHERE BUKRS EQ  IT_BKPF-BUKRS
          AND  BELNR  EQ  IT_BKPF-BELNR
          AND  GJAHR  EQ  IT_BKPF-GJAHR.

        IF IT_BSIK[] IS NOT INITIAL.
          SELECT LIFNR NAME1
             FROM LFA1
             INTO TABLE IT_LFA1_AUX
             FOR ALL ENTRIES IN IT_BSIK
             WHERE LIFNR EQ IT_BSIK-LIFNR.

          LOOP AT IT_LFA1_AUX INTO WA_LFA1.
            APPEND WA_LFA1 TO IT_LFA1.
          ENDLOOP.
        ENDIF.
      ENDIF.
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

  SORT: IT_EKKN           BY EBELN EBELP,
        IT_LFA1           BY LIFNR,
        IT_PEDI           BY EBELN EBELP,
        IT_ZMMT0034       BY KOSTL,
        IT_ZMMT0033       BY BANFN BNFPO,
        IT_MAKT           BY MATNR,
        IT_ESLL           BY PACKNO,
        IT_ESLL_2         BY PACKNO,
        IT_EKBE           BY EBELN,
        IT_BKPF           BY BUKRS GJAHR AWKEY,
        IT_BSAK           BY BUKRS BELNR GJAHR,
        IT_BSIK           BY BUKRS BELNR GJAHR,
        IT_LFA1           BY LIFNR.



  LOOP AT IT_ZMMT0033 INTO WA_ZMMT0033.
    WA_SAIDA-DS_NOME_TRANSPOR  = WA_ZMMT0033-DS_NOME_TRANSPOR.
    WA_SAIDA-INSTRUCAO         = WA_ZMMT0033-INSTRUCAO.

    READ TABLE IT_PEDI INTO WA_PEDI WITH KEY EBELN = WA_ZMMT0033-BANFN
                                             EBELP = WA_ZMMT0033-BNFPO BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SAIDA-TXZ01             = WA_PEDI-TXZ01.
      WA_SAIDA-EBELN             = WA_PEDI-EBELN.
      WA_SAIDA-EBELP             = WA_PEDI-EBELP.
      WA_SAIDA-NETPR             = WA_PEDI-NETPR.
    ENDIF.

    READ TABLE IT_EKKN INTO WA_EKKN WITH KEY EBELN = WA_ZMMT0033-BANFN
                                             EBELP = WA_ZMMT0033-BNFPO BINARY SEARCH.

    READ TABLE IT_ZNOM_TRANSPORTE INTO WA_ZNOM_TRANSPORTE WITH KEY ID_NOMEACAO_TRAN = WA_ZMMT0033-ID_NOMEACAO_TRAN.
    WA_SAIDA-PORTO = WA_ZNOM_TRANSPORTE-DS_PORTO.

    IF P_PORTO IS NOT INITIAL.
      CONDENSE P_PORTO.
      IF NOT P_PORTO CS WA_ZNOM_TRANSPORTE-DS_PORTO OR
         WA_ZNOM_TRANSPORTE-DS_PORTO IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE IT_ZMMT0034 INTO WA_ZMMT0034 WITH KEY KOSTL = WA_EKKN-KOSTL BINARY SEARCH.
    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_ZMMT0034-MATNR BINARY SEARCH.
    WA_SAIDA-MAKTX = WA_MAKT-MAKTX.
    IF P_MATNR IS NOT INITIAL.
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE IT_ESLL INTO WA_ESLL WITH KEY PACKNO = WA_PEDI-PACKNO BINARY SEARCH.
    IF SY-SUBRC = 0.
      READ TABLE IT_ESLL_2 INTO WA_ESLL_2 WITH KEY PACKNO = WA_ESLL-SUB_PACKNO BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_SAIDA-KTEXT1 = WA_ESLL_2-KTEXT1.
      ENDIF.
    ENDIF.

    LOOP AT IT_EKBE INTO WA_EKBE WHERE EBELN = WA_PEDI-EBELN.

      WA_SAIDA-GJAHR = WA_EKBE-GJAHR.
      READ TABLE IT_BKPF INTO WA_BKPF WITH KEY  BUKRS = WA_ZMMT0033-BUKRS
                                                GJAHR = WA_EKBE-GJAHR
                                                AWKEY = WA_EKBE-AWKEY BINARY SEARCH.

      WA_SAIDA-BELNR             = WA_BKPF-BELNR.
      WA_SAIDA-BUKRS             = WA_BKPF-BUKRS.

      READ TABLE IT_BSAK INTO WA_BSAK WITH KEY BUKRS  = WA_BKPF-BUKRS
                                               BELNR  = WA_BKPF-BELNR
                                               GJAHR  = WA_BKPF-GJAHR BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_SAIDA-LIFNR             = WA_BSAK-LIFNR.
        WA_SAIDA-DMBTR             = WA_BSAK-DMBTR.
        WA_SAIDA-DMBE2             = WA_BSAK-DMBE2.
      ELSE.
        READ TABLE IT_BSIK INTO WA_BSIK WITH KEY BUKRS  = WA_BKPF-BUKRS
                                                 BELNR  = WA_BKPF-BELNR
                                                 GJAHR  = WA_BKPF-GJAHR BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA-LIFNR             = WA_BSIK-LIFNR.
          WA_SAIDA-DMBTR             = WA_BSIK-DMBTR.
          WA_SAIDA-DMBE2             = WA_BSIK-DMBE2.
        ENDIF.

      ENDIF.
      READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_SAIDA-LIFNR BINARY SEARCH.
      WA_SAIDA-NAME1             = WA_LFA1-NAME1.

      APPEND WA_SAIDA TO IT_SAIDA.

      CLEAR : WA_SAIDA-GJAHR,
              WA_SAIDA-BELNR,
              WA_SAIDA-BUKRS,
              WA_SAIDA-LIFNR,
              WA_SAIDA-DMBTR,
              WA_SAIDA-DMBE2,
              WA_SAIDA-NAME1.
    ENDLOOP.

    CLEAR: WA_SAIDA, WA_ZMMT0034, WA_MAKT, WA_EKKN, WA_PEDI, WA_ZNOM_TRANSPORTE.
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
  WA_LAYOUT-GRID_TITLE = 'Relatório de Despesas com Vendas'.
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
  WA_AFIELD-FIELDNAME     = 'PORTO'.
  WA_AFIELD-SCRTEXT_S = 'Porto'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DS_NOME_TRANSPOR'.
  WA_AFIELD-SCRTEXT_S = 'Navio'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'INSTRUCAO'.
  WA_AFIELD-SCRTEXT_S = 'Instrução de Embarque'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ' '.
  WA_AFIELD-DD_OUTLEN    = '10'.
  WA_AFIELD-KEY           = ''.
*  WA_AFIELD-F4AVAILABL    = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'LIFNR'.
  WA_AFIELD-SCRTEXT_S = 'Fornecedor'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NAME1'.
  WA_AFIELD-SCRTEXT_S = 'Nome do Fornecedor'.
  WA_AFIELD-SCRTEXT_L = 'Nome do Fornecedor'.
  WA_AFIELD-SCRTEXT_M = 'Nome do Fornecedor'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TXZ01'.
  WA_AFIELD-SCRTEXT_S = 'Descrição do Item'.
  WA_AFIELD-SCRTEXT_L = 'Descrição do Item'.
  WA_AFIELD-SCRTEXT_M = 'Descrição do Item'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'KTEXT1'.
  WA_AFIELD-SCRTEXT_S = 'Item Serviço'.
  WA_AFIELD-SCRTEXT_L = 'Item Serviço'.
  WA_AFIELD-SCRTEXT_M = 'Item Serviço'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MAKTX'.
  WA_AFIELD-SCRTEXT_S = 'Produto'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*  I = I + 1.
*  CLEAR WA_AFIELD.
*  WA_AFIELD-COL_POS       = I.
*  WA_AFIELD-FIELDNAME     = 'BANFN'.
*  WA_AFIELD-SCRTEXT_S = 'Nro. Pedido'.
*  WA_AFIELD-SCRTEXT_L = 'Nro. Pedido'.
*  WA_AFIELD-SCRTEXT_M = 'Nro. Pedido'.
*  WA_AFIELD-EDIT          = ''.
*  WA_AFIELD-KEY           = ''.
*  WA_AFIELD-HOTSPOT       = 'X'.
*  APPEND WA_AFIELD TO IT_FIELDCAT.
*
*  I = I + 1.
*  CLEAR WA_AFIELD.
*  WA_AFIELD-COL_POS       = I.
*  WA_AFIELD-FIELDNAME     = 'BNFPO'.
*  WA_AFIELD-SCRTEXT_S = 'Item'.
*  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
*  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
*  WA_AFIELD-EDIT          = ''.
*  WA_AFIELD-KEY           = ''.
*  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'EBELN'.
  WA_AFIELD-SCRTEXT_S = 'Nro.Pedido'.
  WA_AFIELD-SCRTEXT_L = 'Nro.Pedido'.
  WA_AFIELD-SCRTEXT_M = 'Nro.Pedido'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = 'X'.
  WA_AFIELD-F4AVAILABL    = C_X.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'EBELP'.
  WA_AFIELD-SCRTEXT_S = 'Item'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BELNR'.
  WA_AFIELD-SCRTEXT_S = 'Nro.Doc.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NETPR'.
  WA_AFIELD-SCRTEXT_S = 'Valor Pedido'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DMBTR'.
  WA_AFIELD-SCRTEXT_S = 'Valor R$'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DMBE2'.
  WA_AFIELD-SCRTEXT_S = 'Valor US$'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

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
    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING        "IS_VARIANT = WG_X_VARIANT
        "I_SAVE = WG_SAVE
        IS_LAYOUT       = WA_LAYOUT
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
* ---> S4 Migration - 19/06/2023 - MA
*              WL_LINHA(60),
              WL_LINHA(110),
* <--- S4 Migration - 19/06/2023 - MA
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
    IF P_BUKRS-HIGH IS INITIAL.
      CONCATENATE 'Empresa  :' P_BUKRS-LOW
       INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Empresa  :' P_BUKRS-LOW 'à' P_BUKRS-HIGH
      INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF P_MATNR   IS NOT INITIAL.
    IF P_MATNR-HIGH IS INITIAL.
      CONCATENATE 'Material  :' P_MATNR-LOW
       INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Material  :' P_MATNR-LOW 'à' P_MATNR-HIGH
      INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*          SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF P_NAVIO   IS NOT INITIAL.
    IF P_NAVIO-HIGH IS INITIAL.
      CONCATENATE 'Navio  :' P_NAVIO-LOW
       INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Navio  :' P_NAVIO-LOW 'à' P_NAVIO-HIGH
      INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*          SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF P_BUDAT IS NOT INITIAL.
    CONCATENATE P_BUDAT-LOW+6(2) P_BUDAT-LOW+4(2) P_BUDAT-LOW+0(4) INTO  WL_DATA SEPARATED BY '.'.
    IF P_BUDAT-HIGH IS INITIAL.
      CONCATENATE 'Dt. Pedido  :' WL_DATA
      INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Dt. Pedido :' WL_DATA  INTO WL_LINHA SEPARATED BY SPACE.
      CONCATENATE P_BUDAT-HIGH+6(2) P_BUDAT-HIGH+4(2) P_BUDAT-HIGH+0(4) INTO  WL_DATA SEPARATED BY '.'.
      CONCATENATE WL_LINHA 'à' WL_DATA  INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
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
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_PORTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SEARCH_PORTO.
  DATA: BEGIN OF TL_PORTO OCCURS 0,
          DS_PORTO TYPE ZNOM_TRANSPORTE-DS_PORTO,
        END OF TL_PORTO.

  REFRESH: IT_RETURN_TAB2, IT_DSELC2.

  SELECT DISTINCT DS_PORTO
     FROM  ZNOM_TRANSPORTE INTO TABLE TL_PORTO.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DS_PORTO'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZMMR033'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_PORTO
      RETURN_TAB      = IT_RETURN_TAB2
      DYNPFLD_MAPPING = IT_DSELC2.
ENDFORM.                 " SEARCH_PORTO  INPUT
*&---------------------------------------------------------------------*
*&      Form  SEARCH_NAVIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_NAVIO .
  DATA: BEGIN OF TL_NAVIO OCCURS 0,
          DS_PORTO TYPE ZNOM_TRANSPORTE-DS_PORTO,
        END OF TL_NAVIO.

  REFRESH: IT_RETURN_TAB2, IT_DSELC2.

  SELECT DISTINCT DS_NOME_TRANSPOR
     FROM  ZNOM_TRANSPORTE INTO TABLE TL_NAVIO.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DS_NOME_TRANSPOR'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZMMR033'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_NAVIO
      RETURN_TAB      = IT_RETURN_TAB2
      DYNPFLD_MAPPING = IT_DSELC2.
ENDFORM.                    " SEARCH_NAVIO
