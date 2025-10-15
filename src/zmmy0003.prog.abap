
*&---------------------------------------------------------------------*
*& Report  ZMMY0003
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMMY0003.
TABLES BSIS.
TYPE-POOLS: PMST.
TYPE-POOLS: VRM. " Use type group VRM for list

DATA: IT_LIST     TYPE VRM_VALUES.
DATA: WA_LIST    TYPE VRM_VALUE.
DATA: IT_VALUES   TYPE TABLE OF DYNPREAD,
      WA_VALUES  TYPE DYNPREAD.

DATA: LV_SELECTED_VALUE(10) TYPE C,
      LV_SELECTED_VALUE1(10) TYPE C,
      LV_SELECTED_INDEX TYPE I.

TYPES:
        BEGIN OF TY_EKBE,
         EBELN TYPE EKBE-EBELN,
         EBELP TYPE EKBE-EBELP,
         ZEKKN TYPE EKBE-ZEKKN,
         VGABE TYPE EKBE-VGABE,
         GJAHR TYPE EKBE-GJAHR,
         BELNR TYPE EKBE-BELNR,
         BUZEI TYPE EKBE-BUZEI,
         MATNR TYPE EKBE-MATNR,
         MENGE1 TYPE EKBE-MENGE,
         MENGE2 TYPE EKBE-MENGE,
         BEWTP TYPE EKBE-BEWTP,
         SHKZG TYPE EKBE-SHKZG,
        END OF TY_EKBE,

       BEGIN OF TY_EKBET,
         EBELN TYPE EKBE-EBELN,
         MATNR TYPE EKBE-MATNR,
         MENGE1 TYPE EKBE-MENGE,
         MENGE2 TYPE EKBE-MENGE,
         BEWTP TYPE EKBE-BEWTP,
         SHKZG TYPE EKBE-SHKZG,
        END OF TY_EKBET,

        BEGIN OF TY_EKET,
         EBELN TYPE EKET-EBELN,
         CHARG TYPE EKET-CHARG,
        END OF TY_EKET,

        BEGIN OF TY_MSEG,
         EBELN TYPE MSEG-EBELN,
         MATNR TYPE MSEG-MATNR,
         MENGE TYPE MSEG-MENGE,
         SHKZG TYPE MSEG-SHKZG,
        END OF TY_MSEG,

        BEGIN OF TY_RSEG,
         EBELN TYPE RSEG-EBELN,
         MATNR TYPE RSEG-MATNR,
         MENGE TYPE RSEG-MENGE,
         SHKZG TYPE RSEG-SHKZG,
        END OF TY_RSEG,

         BEGIN OF TY_BSIT,
           ZUONR     TYPE BSIS-ZUONR,
           MATNR     TYPE EKPO-MATNR,
           DMBTR     TYPE BSIS-DMBTR,
           DMBE2     TYPE BSIS-DMBE2,
           DMBE3     TYPE BSIS-DMBE3,
         END OF TY_BSIT,

        BEGIN OF TY_BSIS,
          BUKRS     TYPE BSIS-BUKRS,
          GSBER     TYPE BSIS-GSBER,
          BELNR     TYPE BSIS-BELNR,
          BUDAT     TYPE BSIS-BUDAT,
          MONAT     TYPE BSIS-MONAT,
          GJAHR     TYPE BSIS-GJAHR,
          DMBTR     TYPE BSIS-DMBTR,
          DMBE2     TYPE BSIS-DMBE2,
          DMBE3     TYPE BSIS-DMBE3,
          WAERS     TYPE BSIS-WAERS,
          BLART     TYPE BSIS-BLART,
          SHKZG     TYPE BSIS-SHKZG,
          BUZEI     TYPE BSIS-BUZEI,
          ZUONR     TYPE BSIS-ZUONR,
          SGTXT     TYPE BSIS-SGTXT,
          XBLNR     TYPE BKPF-XBLNR,
          USNAM     TYPE BKPF-USNAM,
          LIFNR     TYPE EKKO-LIFNR,
          EBELN     TYPE EKKO-EBELN,
          BSART     TYPE EKKO-BSART,
          MATNR     TYPE EKPO-MATNR,
          MAKTX     TYPE MAKT-MAKTX,
          EBELP     TYPE EKPO-EBELP,
          CHARG     TYPE EKET-CHARG,
          BUTXT     TYPE T001-BUTXT,
          SAKNR     TYPE SKA1-SAKNR,
          NAME1     TYPE LFA1-NAME1,
          SALDO     TYPE MSEG-MENGE,
          SALMG     TYPE MSEG-MENGE,
          SALMI     TYPE RSEG-MENGE,
          BELNR_G   TYPE BSIS-BELNR,
          GJAHR_G   TYPE BSIS-GJAHR,
          BELNR_E   TYPE BSIS-BELNR,
          GJAHR_E   TYPE BSIS-GJAHR,
          ICON(4)   TYPE C,
        END OF TY_BSIS,


    BEGIN OF TY_ZIB_CONTABIL_ERR,
      OBJ_KEY         TYPE ZIB_CONTABIL_ERR-OBJ_KEY,
      NR_ITEM         TYPE ZIB_CONTABIL_ERR-NR_ITEM,
      INTERFACE       TYPE ZIB_CONTABIL_ERR-INTERFACE,
      DT_ATUALIZACAO  TYPE ZIB_CONTABIL_ERR-DT_ATUALIZACAO,
      HR_ATUALIZACAO  TYPE ZIB_CONTABIL_ERR-HR_ATUALIZACAO,
      TYPE            TYPE ZIB_CONTABIL_ERR-TYPE,
      ID              TYPE ZIB_CONTABIL_ERR-ID,
      NUM             TYPE ZIB_CONTABIL_ERR-NUM,
      MESSAGE         TYPE ZIB_CONTABIL_ERR-MESSAGE,
      MESSAGE_V1      TYPE ZIB_CONTABIL_ERR-MESSAGE_V1,
      MESSAGE_V2      TYPE ZIB_CONTABIL_ERR-MESSAGE_V2,
      MESSAGE_V3      TYPE ZIB_CONTABIL_ERR-MESSAGE_V3,
      MESSAGE_V4      TYPE ZIB_CONTABIL_ERR-MESSAGE_V4,
    END OF TY_ZIB_CONTABIL_ERR.

DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
*Class definition for ALV toolbar
"CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

DATA: CL_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_DYNDOC_ID    TYPE REF TO CL_DD_DOCUMENT,
      EDITCONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER_95  TYPE REF TO CL_GUI_DOCKING_CONTAINER,


      WA_AFIELD        TYPE LVC_S_FCAT,
      IT_FIELDCAT      TYPE LVC_T_FCAT,
      W_FIELDCAT       TYPE LVC_S_FCAT,
      I_SORT           TYPE LVC_T_SORT,

      T_FIELDCATALOG        TYPE LVC_T_FCAT,
      W_FIELDCATALOG        TYPE LVC_S_FCAT,

      GS_VARIANT_C     TYPE DISVARIANT,
      WA_LAYOUT        TYPE LVC_S_LAYO,
      WG_SAVE(1)       TYPE C,
      P_ERRO(1),
      WL_MODE(1),
      V_MES TYPE BSIS-MONAT,
      V_ANO TYPE BSIS-GJAHR,
      WA_STABLE        TYPE LVC_S_STBL.

DATA: TI_BDCDATA          TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      WA_BDCDATA          LIKE LINE OF TI_BDCDATA ,
      T_MESSTAB           TYPE TABLE OF BDCMSGCOLL,
      WL_MESSAGE          TYPE PMST_RAW_MESSAGE,
      WL_SHDBNR           TYPE ZSHDBT0001-SHDBNR,
      VOBJ_KEY            TYPE ZIB_CONTABIL_ERR-OBJ_KEY,
      VSTBLG              TYPE BKPF-STBLG,
      TABIX               TYPE SY-TABIX,
      WG_DOCUMENTO(10).

DATA:  VG_LAST_DAY  TYPE SY-DATUM,
       VG_FIRST_DAY TYPE SY-DATUM,
       VG_LAST_DAY_AUX(8),
       VG_LAST     TYPE ZIB_CONTABIL-BUDAT,
       VG_FIRST    TYPE ZIB_CONTABIL-BUDAT.

DATA:
      GRID1                TYPE REF TO CL_GUI_ALV_GRID,
      OBG_CONTEINER_ERR  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CC_ERR           TYPE SCRFNAME VALUE 'CC_ERR'.
*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: IT_BSIS      TYPE TABLE OF TY_BSIS,
      IT_BSIS_TOT  TYPE TABLE OF TY_BSIS,
      IT_BSIS_SOM  TYPE TABLE OF TY_BSIT,
      IT_BSIS_TOT_AUX  TYPE TABLE OF TY_BSIS,

      IT_EKBE      TYPE TABLE OF TY_EKBE,
      IT_EKBE_TOT  TYPE TABLE OF TY_EKBET,
      WA_EKBE      TYPE TY_EKBE,
      WA_EKBE_TOT  TYPE TY_EKBET,
      IT_EKET      TYPE TABLE OF TY_EKET,
      WA_EKET      TYPE TY_EKET,

      IT_ZMMT0062  TYPE TABLE OF ZMMT0062,
      IT_ZIB_CONTABIL    TYPE TABLE OF ZIB_CONTABIL,
      IT_ZIB_CONTABIL_ERR TYPE TABLE OF ZIB_CONTABIL_ERR WITH HEADER LINE,
      IT_ZIB_CONTABIL_ALV TYPE TABLE OF TY_ZIB_CONTABIL_ERR WITH HEADER LINE,
      IT_SELECTED_ROWS   TYPE LVC_T_ROW.

*----------------------------------------------------------------------*
* WORKAREAS
*----------------------------------------------------------------------*
DATA: WA_BSIS      TYPE TY_BSIS,
      WA_BSIS_TOT  TYPE TY_BSIS,
      WA_BSIS_SOM  TYPE TY_BSIT,
      WA_ZMMT0062  TYPE ZMMT0062,
      WA_ZIB_CONTABIL      TYPE ZIB_CONTABIL,
      WA_ZIB_CONTABIL_ERR  TYPE ZIB_CONTABIL_ERR,
      WA_ZIB_CONTABIL_CHV  TYPE ZIB_CONTABIL_CHV,
      WA_SELECTED_ROWS   TYPE LVC_S_ROW.

CONSTANTS: C_X               TYPE C VALUE 'X'.

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

ENDCLASS.                    "lcl_event_receiver DEFINITION

************************************************************************
* I M P L E M E N T A T I O N
************************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD CATCH_HOTSPOT.

    READ TABLE IT_BSIS_TOT INTO WA_BSIS_TOT INDEX E_ROW_ID-INDEX.
    IF SY-SUBRC = 0.
      IF E_COLUMN_ID = 'BELNR' AND WA_BSIS_TOT-BELNR IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD WA_BSIS_TOT-BELNR.
        SET PARAMETER ID 'BUK' FIELD WA_BSIS_TOT-BUKRS.
        SET PARAMETER ID 'GJR' FIELD WA_BSIS_TOT-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSEIF E_COLUMN_ID = 'BELNR_G' AND WA_BSIS_TOT-BELNR_G IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD WA_BSIS_TOT-BELNR_G.
        SET PARAMETER ID 'BUK' FIELD WA_BSIS_TOT-BUKRS.
        SET PARAMETER ID 'GJR' FIELD WA_BSIS_TOT-GJAHR_G.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSEIF E_COLUMN_ID = 'BELNR_E' AND WA_BSIS_TOT-BELNR_E IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD WA_BSIS_TOT-BELNR_E.
        SET PARAMETER ID 'BUK' FIELD WA_BSIS_TOT-BUKRS.
        SET PARAMETER ID 'GJR' FIELD WA_BSIS_TOT-GJAHR_G.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSEIF E_COLUMN_ID = 'ICON' AND ( WA_BSIS_TOT-ICON = ICON_INCOMPLETE OR
                                        WA_BSIS_TOT-ICON = ICON_FAILURE ).
        CONCATENATE 'AR' WA_BSIS_TOT-EBELN V_ANO V_MES '%' INTO VOBJ_KEY.
        SELECT  OBJ_KEY NR_ITEM INTERFACE DT_ATUALIZACAO HR_ATUALIZACAO TYPE ID NUM MESSAGE MESSAGE_V1 MESSAGE_V2 MESSAGE_V3  MESSAGE_V4
         FROM ZIB_CONTABIL_ERR
         INTO TABLE IT_ZIB_CONTABIL_ALV
         WHERE OBJ_KEY LIKE VOBJ_KEY.

        CALL SCREEN 0200 STARTING AT 20  1
                   ENDING   AT 150 20.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "CATCH_HOTSPOT
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

DATA: EVENT_RECEIVER   TYPE REF TO LCL_EVENT_RECEIVER.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS    : P_BUKRS  TYPE BSIS-BUKRS OBLIGATORY,
                P_MES    TYPE BSIS-MONAT OBLIGATORY,
                P_ANO    TYPE BSIS-GJAHR OBLIGATORY,
                P_HKONT  TYPE C AS LISTBOX VISIBLE LENGTH 20. "Parameter.

SELECT-OPTIONS: S_BLART FOR BSIS-BLART.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_ACT RADIOBUTTON GROUP G1,
            P_ANA RADIOBUTTON GROUP G1,
            P_CON RADIOBUTTON GROUP G1.

SELECTION-SCREEN: END OF BLOCK B2.
SELECTION-SCREEN: END OF BLOCK B1.

INITIALIZATION.
  GS_VARIANT_C-REPORT      = SY-REPID.
  "
  WRITE '1' TO P_HKONT.
  WA_LIST-KEY = '1'.
  WA_LIST-TEXT = 'Todas'.
  APPEND WA_LIST TO IT_LIST.
  WA_LIST-KEY = '2'.
  WA_LIST-TEXT = '0000212100'.
  APPEND WA_LIST TO IT_LIST.
  WA_LIST-KEY = '3'.
  WA_LIST-TEXT = '0000112050'.
  APPEND WA_LIST TO IT_LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = 'P_HKONT'
      VALUES          = IT_LIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.

*--------------------------------------------------------------*
*At Selection Screen
*--------------------------------------------------------------*
AT SELECTION-SCREEN ON P_HKONT.
  CLEAR: WA_VALUES, IT_VALUES.
  REFRESH IT_VALUES.
  WA_VALUES-FIELDNAME = 'P_HKONT'.
  APPEND WA_VALUES TO IT_VALUES.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME             = SY-CPROG
      DYNUMB             = SY-DYNNR
      TRANSLATE_TO_UPPER = 'X'
    TABLES
      DYNPFIELDS         = IT_VALUES.

  READ TABLE IT_VALUES INDEX 1 INTO WA_VALUES.
  IF SY-SUBRC = 0 AND WA_VALUES-FIELDVALUE IS NOT INITIAL.
    READ TABLE IT_LIST INTO WA_LIST
                      WITH KEY KEY = WA_VALUES-FIELDVALUE.
    IF SY-SUBRC = 0.
      LV_SELECTED_VALUE = WA_LIST-TEXT.
      LV_SELECTED_VALUE1 = WA_LIST-TEXT.
      LV_SELECTED_INDEX = WA_LIST-KEY.
      IF LV_SELECTED_INDEX = 1.
        LV_SELECTED_VALUE = '0000212100'.
        LV_SELECTED_VALUE1 = '0000112050'.
      ENDIF.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  PERFORM SELECIONAR_DADOS.
  PERFORM ORGANIZACAO_DADOS.
  PERFORM IMPRIMIR_DADOS.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .
  V_ANO = P_ANO.
  V_MES = P_MES.
  CONCATENATE P_ANO P_MES '01' INTO VG_LAST_DAY_AUX.
  VG_LAST_DAY = VG_LAST_DAY_AUX.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      I_DATE = VG_LAST_DAY
    IMPORTING
      E_DATE = VG_LAST_DAY.

  CONCATENATE VG_LAST_DAY+6(2) '.' VG_LAST_DAY+4(2) '.' VG_LAST_DAY+0(4) INTO VG_LAST.

  VG_FIRST_DAY  = VG_LAST_DAY + 1.
  CONCATENATE VG_FIRST_DAY+6(2) '.' VG_FIRST_DAY+4(2) '.' VG_FIRST_DAY+0(4) INTO VG_FIRST.

  SELECT  BSIS~BUKRS BSIS~GSBER BSIS~BELNR BSIS~BUDAT BSIS~MONAT BSIS~GJAHR BSIS~DMBTR BSIS~DMBE2 BSIS~DMBE3 BSIS~WAERS BSIS~BLART BSIS~SHKZG BSIS~BUZEI BSIS~ZUONR BSIS~SGTXT
          BKPF~XBLNR
          BKPF~USNAM
          EKKO~LIFNR
          EKKO~EBELN
          EKKO~BSART
          EKPO~MATNR
          MAKT~MAKTX
          EKPO~EBELP
          EKET~CHARG
          T001~BUTXT
          SKA1~SAKNR
          LFA1~NAME1
    FROM BSIS
    INNER JOIN BKPF
    ON  BKPF~BUKRS  = BSIS~BUKRS
    AND BKPF~BELNR  = BSIS~BELNR
    AND BKPF~GJAHR  = BSIS~GJAHR
    INNER JOIN EKKO
    ON EKKO~EBELN = BSIS~ZUONR
    INNER JOIN EKPO
    ON EKKO~EBELN = EKPO~EBELN
    AND EKPO~EBELP = '00010'
    INNER JOIN  EKET ON EKET~EBELN = EKPO~EBELN
    AND EKET~EBELP = EKPO~EBELP
    AND EKET~ETENR = 1
    INNER JOIN MAKT
    ON MAKT~MATNR  = EKPO~MATNR
    AND MAKT~SPRAS = SY-LANGU
    INNER JOIN LFA1
    ON LFA1~LIFNR = EKKO~LIFNR
    INNER JOIN T001
    ON T001~BUKRS = BSIS~BUKRS
    INNER JOIN SKA1 "#EC CI_DB_OPERATION_OK[2431747]
    ON  SKA1~SAKNR =  BSIS~HKONT "#EC CI_DB_OPERATION_OK[2389136]
    AND SKA1~KTOPL  = '0050'
    INTO TABLE IT_BSIS
    WHERE BSIS~BUKRS  = P_BUKRS
    AND   BSIS~HKONT  IN (LV_SELECTED_VALUE , LV_SELECTED_VALUE1)
    AND   BSIS~GJAHR  LE P_ANO
    AND   BSIS~BLART  IN S_BLART
    AND   BSIS~GJAHR  GE 2012
    AND   BSIS~BUDAT  LE VG_LAST_DAY.

  " compensados após a data
  SELECT  BSAS~BUKRS BSAS~GSBER BSAS~BELNR BSAS~BUDAT BSAS~MONAT BSAS~GJAHR BSAS~DMBTR BSAS~DMBE2 BSAS~DMBE3 BSAS~WAERS BSAS~BLART BSAS~SHKZG BSAS~BUZEI BSAS~ZUONR BSAS~SGTXT
          BKPF~XBLNR
          BKPF~USNAM
          EKKO~LIFNR
          EKKO~EBELN
          EKKO~BSART
          EKPO~MATNR
          MAKT~MAKTX
          EKPO~EBELP
          EKET~CHARG
          T001~BUTXT
          SKA1~SAKNR
          LFA1~NAME1
    FROM BSAS
    INNER JOIN BKPF
    ON  BKPF~BUKRS  = BSAS~BUKRS
    AND BKPF~BELNR  = BSAS~BELNR
    AND BKPF~GJAHR  = BSAS~GJAHR
    INNER JOIN EKKO
    ON EKKO~EBELN = BSAS~ZUONR
     INNER JOIN EKPO
    ON  EKKO~EBELN = EKPO~EBELN
    AND EKPO~EBELP = '00010'
    INNER JOIN  EKET ON EKET~EBELN = EKPO~EBELN
    AND EKET~EBELP = EKPO~EBELP
    AND EKET~ETENR = 1
    INNER JOIN MAKT
    ON MAKT~MATNR  = EKPO~MATNR
    AND MAKT~SPRAS = SY-LANGU
    INNER JOIN LFA1
    ON LFA1~LIFNR = EKKO~LIFNR
    INNER JOIN T001
    ON T001~BUKRS = BSAS~BUKRS
    INNER JOIN SKA1 "#EC CI_DB_OPERATION_OK[2431747]
    ON  SKA1~SAKNR =  BSAS~HKONT "#EC CI_DB_OPERATION_OK[2389136]
    AND SKA1~KTOPL  = '0050'
    APPENDING TABLE IT_BSIS
    WHERE BSAS~BUKRS  = P_BUKRS
    AND   BSAS~HKONT  IN (LV_SELECTED_VALUE , LV_SELECTED_VALUE1)
    AND   BSAS~BLART  IN S_BLART
    AND   BSAS~BUDAT  LE VG_LAST_DAY.

  DELETE IT_BSIS WHERE BUDAT GT VG_LAST_DAY.
  DELETE IT_BSIS WHERE DMBTR = 0 AND DMBE2 = 0 AND DMBE3 = 0.

  CHECK IT_BSIS[] IS NOT INITIAL.

  SELECT *
    FROM ZMMT0062
    INTO TABLE IT_ZMMT0062
    FOR ALL ENTRIES IN IT_BSIS
    WHERE BUKRS = IT_BSIS-BUKRS
    AND   MONAT = P_MES
    AND   GJAHR = P_ANO
    AND   EBELN = IT_BSIS-EBELN.

  SELECT  EBELN EBELP ZEKKN VGABE GJAHR BELNR BUZEI
          MATNR MENGE MENGE BEWTP SHKZG
    FROM EKBE
    INTO TABLE IT_EKBE
    FOR ALL ENTRIES IN IT_BSIS
    WHERE EBELN = IT_BSIS-EBELN
    AND BEWTP IN ('E','Q')
    AND BUDAT LE VG_LAST_DAY.

*  SELECT EBELN CHARG
*    FROM EKET
*    INTO TABLE IT_EKET
*    FOR ALL ENTRIES IN IT_BSIS
*    WHERE EBELN EQ IT_BSIS-EBELN
*    AND   EBELP EQ IT_BSIS-EBELP.


ENDFORM.                    "SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZACAO_DADOS .
  DATA: TABIX     TYPE SY-TABIX,
        VOBJ_KEY  TYPE ZIB_CONTABIL-OBJ_KEY,
        VCONT     TYPE I.

  SORT: IT_BSIS     BY ZUONR MATNR,
        IT_ZMMT0062 BY EBELN EBELP.


  SORT: IT_EKBE BY EBELN MATNR.
  REFRESH: IT_EKBE_TOT.

  LOOP AT IT_EKBE  INTO WA_EKBE.
    IF WA_EKBE-BEWTP = 'E'. " migo
      CLEAR WA_EKBE-MENGE2. "Zera miro
    ELSE.
      CLEAR WA_EKBE-MENGE1.
    ENDIF.
    IF WA_EKBE-SHKZG = 'H'.
      MULTIPLY WA_EKBE-MENGE1 BY -1.
      MULTIPLY WA_EKBE-MENGE2 BY -1.
    ENDIF.

    CLEAR: WA_EKBE-BEWTP, WA_EKBE-SHKZG.
    MOVE-CORRESPONDING WA_EKBE TO WA_EKBE_TOT.
    COLLECT WA_EKBE_TOT INTO IT_EKBE_TOT.
  ENDLOOP.

  SORT: IT_EKBE_TOT BY EBELN MATNR.

  REFRESH: IT_BSIS_TOT_AUX,IT_BSIS_SOM.
  LOOP AT IT_BSIS INTO WA_BSIS.
    IF WA_BSIS-SHKZG = 'H'.
      MULTIPLY WA_BSIS-DMBTR BY -1.
      MULTIPLY WA_BSIS-DMBE2 BY -1.
      MULTIPLY WA_BSIS-DMBE3 BY -1.
    ENDIF.
    WA_BSIS_SOM-ZUONR = WA_BSIS-ZUONR.
    WA_BSIS_SOM-MATNR = WA_BSIS-MATNR.
    WA_BSIS_SOM-DMBTR = WA_BSIS-DMBTR.
    WA_BSIS_SOM-DMBE2 = WA_BSIS-DMBE2.
    WA_BSIS_SOM-DMBE3 = WA_BSIS-DMBE3.
    COLLECT WA_BSIS_SOM INTO IT_BSIS_SOM.
  ENDLOOP.

  SORT IT_BSIS_SOM BY ZUONR MATNR.

  IT_BSIS_TOT[] = IT_BSIS[].
  DELETE ADJACENT DUPLICATES FROM IT_BSIS_TOT COMPARING ZUONR MATNR.

  LOOP AT IT_BSIS_TOT INTO WA_BSIS_TOT.
    TABIX = SY-TABIX.
    READ TABLE IT_BSIS_SOM INTO WA_BSIS_SOM WITH KEY ZUONR = WA_BSIS_TOT-ZUONR
                                                     MATNR = WA_BSIS_TOT-MATNR BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.
    WA_BSIS_TOT-DMBTR = WA_BSIS_SOM-DMBTR.
    WA_BSIS_TOT-DMBE2 = WA_BSIS_SOM-DMBE2.
    WA_BSIS_TOT-DMBE3 = WA_BSIS_SOM-DMBE3.

    WA_BSIS_TOT-ICON = ''.
    IF P_CON = 'X'.
      VCONT = 0.
      LOOP AT IT_ZMMT0062 INTO WA_ZMMT0062 WHERE EBELN = WA_BSIS_TOT-EBELN.
*                                           and   ebelp = wa_bsis_tot-ebelp.
        ADD 1 TO VCONT.
        WA_BSIS_TOT-BELNR_G = WA_ZMMT0062-BELNR_G.
        WA_BSIS_TOT-GJAHR_G = WA_ZMMT0062-GJAHR_G.
        WA_BSIS_TOT-BELNR_E = WA_ZMMT0062-BELNR_E.
        WA_BSIS_TOT-DMBTR = WA_ZMMT0062-VLR_ARS.
        WA_BSIS_TOT-DMBE2 = WA_ZMMT0062-VLR_USD.
        WA_BSIS_TOT-DMBE3 = WA_ZMMT0062-VLR_BRL.
        "WA_BSIS_TOT-ICON  = ICON_SYSTEM_OKAY.
        IF VCONT = 1.
          MODIFY IT_BSIS_TOT FROM WA_BSIS_TOT INDEX TABIX TRANSPORTING DMBTR DMBE2 DMBE3 ICON BELNR_G BELNR_E.
        ELSE.
          APPEND WA_BSIS_TOT TO IT_BSIS_TOT_AUX.
        ENDIF.
      ENDLOOP.
      IF VCONT = 0.
        MODIFY IT_BSIS_TOT FROM WA_BSIS_TOT INDEX TABIX TRANSPORTING DMBTR DMBE2 DMBE3 ICON BELNR_G BELNR_E.
      ENDIF.
    ELSE.
      CLEAR  VOBJ_KEY.
      "pega o ultimo
      LOOP AT IT_ZMMT0062 INTO WA_ZMMT0062 WHERE EBELN = WA_BSIS_TOT-EBELN.
        WA_BSIS_TOT-BELNR_G = WA_ZMMT0062-BELNR_G.
        WA_BSIS_TOT-GJAHR_G = WA_ZMMT0062-GJAHR_G.
        WA_BSIS_TOT-BELNR_E = WA_ZMMT0062-BELNR_E.
        VOBJ_KEY = WA_ZMMT0062-OBJ_KEY.
      ENDLOOP.
      IF WA_BSIS_TOT-BELNR_G IS INITIAL AND VOBJ_KEY IS NOT INITIAL.
        SELECT SINGLE *
         FROM ZIB_CONTABIL_CHV
         INTO WA_ZIB_CONTABIL_CHV
        WHERE OBJ_KEY = VOBJ_KEY.

        IF SY-SUBRC = 0.
          UPDATE ZMMT0062 SET BELNR_G = WA_ZIB_CONTABIL_CHV-BELNR
                              GJAHR_G = WA_ZIB_CONTABIL_CHV-GJAHR
          WHERE BUKRS     = WA_BSIS_TOT-BUKRS
          AND   MONAT     = P_MES
          AND   GJAHR     = P_ANO
          AND   EBELN     = WA_BSIS_TOT-EBELN
*          and   ebelp     = wa_bsis_tot-ebelp
          AND   OBJ_KEY   = VOBJ_KEY.
          COMMIT WORK.

          WA_BSIS_TOT-ICON    = ICON_SYSTEM_OKAY.
          WA_BSIS_TOT-BELNR_G = WA_ZIB_CONTABIL_CHV-BELNR.
          WA_BSIS_TOT-GJAHR_G = WA_ZIB_CONTABIL_CHV-GJAHR.

          " Estorno mes seguinte
          REFRESH TI_BDCDATA.
          PERFORM F_BDC_DATA USING:
                'SAPMF05A'  '0105'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'	      '/00',
                ''          ''      ''   'RF05A-BELNS'      WA_ZIB_CONTABIL_CHV-BELNR,
                ''          ''      ''   'BKPF-BUKRS'       WA_ZIB_CONTABIL_CHV-BUKRS,
                ''          ''      ''   'RF05A-GJAHS'      WA_ZIB_CONTABIL_CHV-GJAHR,
                ''          ''      ''   'UF05A-STGRD'      '02',
                ''          ''      ''   'BSIS-BUDAT'       VG_FIRST,
                'SAPMF05A'  '0105'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'	      '=BU'.

          CLEAR P_ERRO.
          PERFORM ZF_CALL_TRANSACTION USING 'FB08' CHANGING P_ERRO.
          IF P_ERRO NE 'X'.
            WA_ZMMT0062-BELNR_E = WG_DOCUMENTO.
            WA_BSIS_TOT-BELNR_E = WG_DOCUMENTO.
            UPDATE ZMMT0062 SET BELNR_E = WG_DOCUMENTO
             WHERE BUKRS     = WA_BSIS_TOT-BUKRS
             AND   MONAT     = P_MES
             AND   GJAHR     = P_ANO
             AND   EBELN     = WA_BSIS_TOT-EBELN
*             and   ebelp     = wa_bsis_tot-ebelp
             AND   OBJ_KEY   = VOBJ_KEY.
            COMMIT WORK.
          ELSE.
            WA_BSIS_TOT-ICON = ICON_FAILURE.
          ENDIF.
          WAIT UP TO 5 SECONDS. " espera
          "
        ELSE.
          SELECT SINGLE *
            FROM ZIB_CONTABIL_ERR
            INTO WA_ZIB_CONTABIL_ERR
           WHERE OBJ_KEY = VOBJ_KEY.
          IF SY-SUBRC = 0.
            WA_BSIS_TOT-ICON = ICON_INCOMPLETE.
          ELSE.
            SELECT SINGLE *
           FROM ZIB_CONTABIL
           INTO WA_ZIB_CONTABIL
          WHERE OBJ_KEY = VOBJ_KEY.
            IF SY-SUBRC = 0.
              WA_BSIS_TOT-ICON = ICON_ACTIVITY.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSEIF WA_ZMMT0062-BELNR_E IS INITIAL AND VOBJ_KEY IS NOT INITIAL. " Erro no estorno (reprocessa)
        " Estorno mes seguinte
        WA_BSIS_TOT-ICON = ICON_FAILURE.
        SELECT SINGLE STBLG
          INTO VSTBLG
          FROM BKPF
          WHERE BUKRS = WA_BSIS_TOT-BUKRS
          AND   BELNR = WA_BSIS_TOT-BELNR_G
          AND   GJAHR = WA_BSIS_TOT-GJAHR_G.

        IF SY-SUBRC = 0.
          UPDATE ZMMT0062 SET BELNR_E = VSTBLG
               WHERE BUKRS     = WA_BSIS_TOT-BUKRS
               AND   MONAT     = P_MES
               AND   GJAHR     = P_ANO
               AND   EBELN     = WA_BSIS_TOT-EBELN
*               and   ebelp     = wa_bsis_tot-ebelp
               AND   OBJ_KEY   = VOBJ_KEY.
          COMMIT WORK.
          WA_BSIS_TOT-BELNR_E = VSTBLG.
          WA_BSIS_TOT-ICON    = ICON_SYSTEM_OKAY.
        ELSE.
          REFRESH TI_BDCDATA.
          PERFORM F_BDC_DATA USING:
                'SAPMF05A'  '0105'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'	      '/00',
                ''          ''      ''   'RF05A-BELNS'      WA_ZMMT0062-BELNR_G,
                ''          ''      ''   'BKPF-BUKRS'       WA_ZMMT0062-BUKRS,
                ''          ''      ''   'RF05A-GJAHS'      WA_ZMMT0062-GJAHR_G,
                ''          ''      ''   'UF05A-STGRD'      '02',
                ''          ''      ''   'BSIS-BUDAT'       VG_FIRST,
                'SAPMF05A'  '0105'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'	      '=BU'.

          CLEAR P_ERRO.
          PERFORM ZF_CALL_TRANSACTION USING 'FB08' CHANGING P_ERRO.
          IF P_ERRO NE 'X'.
            UPDATE ZMMT0062 SET BELNR_E = WG_DOCUMENTO
               WHERE BUKRS     = WA_BSIS_TOT-BUKRS
               AND   MONAT     = P_MES
               AND   GJAHR     = P_ANO
               AND   EBELN     = WA_BSIS_TOT-EBELN
*               and   ebelp     = wa_bsis_tot-ebelp
               AND   OBJ_KEY   = VOBJ_KEY.
            COMMIT WORK.
            WA_BSIS_TOT-BELNR_E = WG_DOCUMENTO.
            WA_BSIS_TOT-ICON    = ICON_SYSTEM_OKAY.
          ENDIF.
          WAIT UP TO 2 SECONDS. " espera
        ENDIF.
        "
      ENDIF.
      "Quantidade saldo
      READ TABLE IT_EKBE_TOT INTO WA_EKBE_TOT WITH KEY EBELN = WA_BSIS_TOT-EBELN
                                                   MATNR = WA_BSIS_TOT-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_BSIS_TOT-SALDO = WA_EKBE_TOT-MENGE2 - WA_EKBE_TOT-MENGE1.
      ELSE.
        CLEAR WA_BSIS_TOT-SALDO.
      ENDIF.

*      "migo
*      read table it_mseg_tot into wa_mseg with key ebeln = wa_bsis_tot-ebeln
*                                                   matnr = wa_bsis_tot-matnr binary search.
*      if sy-subrc = 0.
*        wa_bsis_tot-saldo = wa_mseg-menge.
*      endif.
*
*      "miro
*      read table it_rseg_tot into wa_rseg with key ebeln = wa_bsis_tot-ebeln
*                                                   matnr = wa_bsis_tot-matnr binary search.
*      if sy-subrc = 0.
*        wa_bsis_tot-saldo = wa_bsis_tot-saldo - wa_rseg-menge.
*      endif.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_BSIS_TOT-MATNR
        IMPORTING
          OUTPUT = WA_BSIS_TOT-MATNR.


      MODIFY IT_BSIS_TOT FROM WA_BSIS_TOT INDEX TABIX TRANSPORTING DMBTR DMBE2 DMBE3 ICON BELNR_G BELNR_E SALDO MATNR.
    ENDIF.
  ENDLOOP.
  LOOP AT IT_BSIS_TOT_AUX INTO WA_BSIS_TOT.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_BSIS_TOT-MATNR
      IMPORTING
        OUTPUT = WA_BSIS_TOT-MATNR.
    APPEND WA_BSIS_TOT TO IT_BSIS_TOT.
  ENDLOOP.
  DELETE IT_BSIS_TOT WHERE DMBTR = 0 AND DMBE2 = 0 AND DMBE3 = 0. " AND SALDO = 0.
*  IF P_ACT = 'X'.
*    DELETE IT_BSIS_TOT WHERE BELNR_G IS NOT INITIAL AND BELNR_E IS NOT INITIAL.
*  ENDIF.
  SORT IT_BSIS_TOT BY ZUONR EBELP.
ENDFORM.                    " ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  IF P_ACT  = 'X' OR P_CON = 'X'.
    PERFORM F_ALV_FIELDCAT.
  ELSE.
    PERFORM F_ALV_FIELDCAT_ANA.
  ENDIF.

  WA_LAYOUT-ZEBRA      = 'X'.
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.

  WA_LAYOUT-GRID_TITLE = ''.
  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT = 'X'.
  WA_LAYOUT-BOX_FNAME  = 'MARK'.

  CALL SCREEN 0100.
ENDFORM.                    " IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  IF P_ACT  = 'X'.
    SET PF-STATUS 'F_SET_PF' .
  ELSE.
    SET PF-STATUS 'F_SET_AN' .
  ENDIF.
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

    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    WA_STABLE-ROW        = C_X.

    WG_SAVE = 'X'.
    WA_LAYOUT-INFO_FNAME    = 'LINE_COLOR'.
    GS_VARIANT_C-REPORT      = SY-REPID.
    IF P_ACT  = 'X' OR P_CON = 'X'.
      CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_VARIANT      = GS_VARIANT_C
          IS_LAYOUT       = WA_LAYOUT
          I_SAVE          = WG_SAVE
          I_DEFAULT       = 'X'
        CHANGING
          IT_FIELDCATALOG = IT_FIELDCAT[]
          IT_SORT         = I_SORT[]
          IT_OUTTAB       = IT_BSIS_TOT[].

      CREATE OBJECT EVENT_RECEIVER.
      SET HANDLER EVENT_RECEIVER->CATCH_HOTSPOT              FOR CL_GRID.
    ELSE.
      CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_VARIANT      = GS_VARIANT_C
          IS_LAYOUT       = WA_LAYOUT
          I_SAVE          = WG_SAVE
          I_DEFAULT       = 'X'
        CHANGING
          IT_FIELDCATALOG = IT_FIELDCAT[]
          IT_SORT         = I_SORT[]
          IT_OUTTAB       = IT_BSIS[].

    ENDIF.




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
            VBUTXT TYPE T001-BUTXT,
            WL_TEXT TYPE SDYDO_TEXT_ELEMENT.

  IF P_ACT = 'X'.
    WL_TEXT = 'Actualización'.
  ELSE.
    WL_TEXT = 'Analítico'.
  ENDIF.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT
      SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  SELECT SINGLE BUTXT
    FROM T001
    INTO VBUTXT
    WHERE BUKRS = P_BUKRS.
  CONCATENATE  'Sociedad:' P_BUKRS '-' VBUTXT
          INTO WL_LINHA SEPARATED BY SPACE.
  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
*      SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  CONCATENATE  'Mês/Ano:' P_MES '/' P_ANO
           INTO WL_LINHA .
  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
*      SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .
  REFRESH IT_FIELDCAT.
  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_BSIS_TOT'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ICON'.
  WA_AFIELD-ICON          = 'X'.
  WA_AFIELD-SCRTEXT_S = 'status'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'ZUONR'.
  WA_AFIELD-SCRTEXT_S = 'Pedido'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'BSART'.
  WA_AFIELD-SCRTEXT_S = 'Tipo'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'CHARG'.
  WA_AFIELD-SCRTEXT_S = 'Safra'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'LIFNR'.
  WA_AFIELD-SCRTEXT_S = 'Cod.Proveedor'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 10.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'NAME1'.
  WA_AFIELD-SCRTEXT_S = 'Nombre Proveedor'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 40.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'SALDO'.
  WA_AFIELD-SCRTEXT_S = 'Saldo Qtde'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'MATNR'.
  WA_AFIELD-SCRTEXT_S = 'Material'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'MAKTX'.
  WA_AFIELD-SCRTEXT_S = 'Descrição'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-OUTPUTLEN     = 40.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'DMBTR'.
  WA_AFIELD-SCRTEXT_S = 'Valor ARS'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-DO_SUM        = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'DMBE2'.
  WA_AFIELD-SCRTEXT_S = 'Valor USD'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-DO_SUM        = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'DMBE3'.
  WA_AFIELD-SCRTEXT_S = 'Valor BRL'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-DO_SUM        = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'BELNR_G'.
  WA_AFIELD-SCRTEXT_S = 'Documento Gerado'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'BELNR_E'.
  WA_AFIELD-SCRTEXT_S = 'Estorno Gerado'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT_ANA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT_ANA .
  REFRESH IT_FIELDCAT.
  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_BSIS'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'BELNR'.
  WA_AFIELD-SCRTEXT_S = 'Nro.documento'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  WA_AFIELD-HOTSPOT       = 'X'.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'BUDAT'.
  WA_AFIELD-SCRTEXT_S = 'Fecha contab.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'ZUONR'.
  WA_AFIELD-SCRTEXT_S = 'Pedido'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'BSART'.
  WA_AFIELD-SCRTEXT_S = 'Tipo'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'CHARG'.
  WA_AFIELD-SCRTEXT_S = 'Safra'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'LIFNR'.
  WA_AFIELD-SCRTEXT_S = 'Cod.Proveedor'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'NAME1'.
  WA_AFIELD-SCRTEXT_S = 'Nombre Proveedor'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*  i = i + 1.
*  clear wa_afield.
*  wa_afield-col_pos   = i.
*  wa_afield-fieldname = 'SALDO'.
*  wa_afield-scrtext_s = 'Saldo Qtde'.
*  wa_afield-scrtext_l = wa_afield-scrtext_s.
*  wa_afield-scrtext_m = wa_afield-scrtext_s.
*  wa_afield-edit          = ''.
*  wa_afield-key           = ''.
*  append wa_afield to it_fieldcat.
*
*  i = i + 1.
*  clear wa_afield.
*  wa_afield-col_pos   = i.
*  wa_afield-fieldname = 'MATNR'.
*  wa_afield-scrtext_s = 'Material'.
*  wa_afield-scrtext_l = wa_afield-scrtext_s.
*  wa_afield-scrtext_m = wa_afield-scrtext_s.
*  wa_afield-edit          = ''.
*  wa_afield-key           = ''.
*  append wa_afield to it_fieldcat.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'DMBTR'.
  WA_AFIELD-SCRTEXT_S = 'Valor ARS'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'DMBE2'.
  WA_AFIELD-SCRTEXT_S = 'Valor USD'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'DMBE3'.
  WA_AFIELD-SCRTEXT_S = 'Valor BRL'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'BLART'.
  WA_AFIELD-SCRTEXT_S = 'Clase'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'SGTXT'.
  WA_AFIELD-SCRTEXT_S = 'Texto Contabilidad'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'XBLNR'.
  WA_AFIELD-SCRTEXT_S = 'Nro.Referencia'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS   = I.
  WA_AFIELD-FIELDNAME = 'USNAM'.
  WA_AFIELD-SCRTEXT_S = 'Usuário'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


ENDFORM.                    " F_ALV_FIELDCAT_ANA
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP'.
      REFRESH: IT_BSIS, IT_BSIS_TOT.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'REFRESH'.
      PERFORM SELECIONAR_DADOS.
      PERFORM ORGANIZACAO_DADOS.
      IF NOT CL_GRID IS INITIAL.
        CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      ENDIF.
    WHEN 'GERAR'.
      CALL METHOD CL_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SELECTED_ROWS.

      LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS WHERE INDEX IS NOT INITIAL.
        READ TABLE IT_BSIS_TOT INTO WA_BSIS_TOT INDEX WA_SELECTED_ROWS-INDEX.
        IF ( WA_BSIS_TOT-ICON = ICON_INCOMPLETE OR WA_BSIS_TOT-ICON = '' ) AND
           ( WA_BSIS_TOT-DMBTR NE 0 OR WA_BSIS_TOT-DMBE2 NE 0 OR WA_BSIS_TOT-DMBE3 NE 0 ).
          PERFORM F_GRAVA_ZIB.
          WA_BSIS_TOT-ICON = ICON_ACTIVITY.
          MODIFY IT_BSIS_TOT FROM WA_BSIS_TOT INDEX WA_SELECTED_ROWS-INDEX TRANSPORTING ICON.
        ENDIF.
      ENDLOOP.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_ZIB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GRAVA_ZIB.
  DATA: VCONT TYPE I,
        SCONT(2).

  VCONT = 0.
  LOOP AT IT_ZMMT0062 INTO WA_ZMMT0062 WHERE EBELN = WA_BSIS-EBELN.
    IF WA_ZMMT0062-BELNR_G IS NOT INITIAL.
      ADD 1 TO VCONT.
    ENDIF.
  ENDLOOP.
  ADD 1 TO VCONT.
  SCONT = VCONT.
  "
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = SCONT
    IMPORTING
      OUTPUT = SCONT.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = SCONT
    IMPORTING
      OUTPUT = SCONT.

  CONCATENATE 'AR' WA_BSIS_TOT-EBELN P_ANO P_MES SCONT INTO VOBJ_KEY.

  IF WA_BSIS_TOT-ICON = ICON_INCOMPLETE.
    DELETE FROM ZIB_CONTABIL     WHERE OBJ_KEY = VOBJ_KEY.
    DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY = VOBJ_KEY.
  ENDIF.
  "
  REFRESH IT_ZIB_CONTABIL.
  DO 2 TIMES.
    WA_ZIB_CONTABIL-OBJ_KEY = VOBJ_KEY.
    WA_ZIB_CONTABIL-SEQITEM = SY-INDEX.
    "neg
    IF WA_BSIS_TOT-DMBTR LT 0.
      IF SY-INDEX = 1.
        WA_ZIB_CONTABIL-BSCHL = '40'.
        WA_ZIB_CONTABIL-HKONT       = '0000212100'.
      ELSE.
        WA_ZIB_CONTABIL-BSCHL = '31'.
        WA_ZIB_CONTABIL-HKONT       = WA_BSIS_TOT-LIFNR.
      ENDIF.
    ELSE.
      IF SY-INDEX = 1.
        WA_ZIB_CONTABIL-BSCHL = '21'.
        WA_ZIB_CONTABIL-HKONT       = WA_BSIS_TOT-LIFNR.
      ELSE.
        WA_ZIB_CONTABIL-BSCHL = '50'.
        WA_ZIB_CONTABIL-HKONT       = '0000212100'.
      ENDIF.
    ENDIF.
    WA_ZIB_CONTABIL-GSBER       = WA_BSIS_TOT-GSBER.
    WA_ZIB_CONTABIL-BUKRS       = WA_BSIS_TOT-BUKRS.
    WA_ZIB_CONTABIL-INTERFACE   = '35'.
    WA_ZIB_CONTABIL-BKTXT       = 'Actualización'.
    WA_ZIB_CONTABIL-BLDAT       = VG_LAST.
    WA_ZIB_CONTABIL-BUDAT       = VG_LAST.
    WA_ZIB_CONTABIL-GJAHR       = P_ANO.
    WA_ZIB_CONTABIL-MONAT       = P_MES.
    WA_ZIB_CONTABIL-BLART       = 'AB'.
    WA_ZIB_CONTABIL-XBLNR       = ''.
    WA_ZIB_CONTABIL-ZFBDT       = ''.
    WA_ZIB_CONTABIL-SGTXT       = 'Mercadería pendiente de pgto'.
    WA_ZIB_CONTABIL-BUPLA       = ''.
    WA_ZIB_CONTABIL-ZUONR       = WA_BSIS_TOT-ZUONR.
    WA_ZIB_CONTABIL-WAERS       = 'ARS'.
    WA_ZIB_CONTABIL-WAERS_I     = 'ARS'.
    WA_ZIB_CONTABIL-WAERS_F     = 'USD'.
    WA_ZIB_CONTABIL-WAERS_G     = 'BRL'.
    IF WA_BSIS_TOT-DMBTR EQ 0.
      CLEAR WA_ZIB_CONTABIL-WAERS_I.
    ENDIF.
    IF WA_BSIS_TOT-DMBE2 EQ 0.
      CLEAR WA_ZIB_CONTABIL-WAERS_F.
    ENDIF.
    IF WA_BSIS_TOT-DMBE3 EQ 0.
      CLEAR WA_ZIB_CONTABIL-WAERS_G.
    ENDIF.
    WA_ZIB_CONTABIL-WRBTR = WA_BSIS_TOT-DMBTR.
    WA_ZIB_CONTABIL-DMBTR = WA_BSIS_TOT-DMBTR.
    WA_ZIB_CONTABIL-DMBE2 = WA_BSIS_TOT-DMBE2.
    WA_ZIB_CONTABIL-DMBE3 = WA_BSIS_TOT-DMBE3.
    IF WA_BSIS_TOT-DMBTR LT 0 OR WA_ZIB_CONTABIL-DMBE2 LT 0 OR WA_ZIB_CONTABIL-DMBE3 LT 0.
      MULTIPLY WA_ZIB_CONTABIL-WRBTR BY -1.
      MULTIPLY WA_ZIB_CONTABIL-DMBTR BY -1.
      MULTIPLY WA_ZIB_CONTABIL-DMBE2 BY -1.
      MULTIPLY WA_ZIB_CONTABIL-DMBE3 BY -1.
    ENDIF.

    WA_ZIB_CONTABIL-RG_ATUALIZADO = 'N'.
    APPEND WA_ZIB_CONTABIL TO IT_ZIB_CONTABIL.
  ENDDO.
  MODIFY ZIB_CONTABIL FROM TABLE IT_ZIB_CONTABIL.
  COMMIT WORK.
  "Grava SIGAM (valores iniciais)
  CLEAR WA_ZMMT0062.
  DELETE FROM ZMMT0062
     WHERE BUKRS     = WA_BSIS_TOT-BUKRS
     AND   MONAT     = P_MES
     AND   GJAHR     = P_ANO
     AND   EBELN     = WA_BSIS_TOT-EBELN
     AND   BELNR_G   = ''.

  WA_ZMMT0062-BUKRS     = WA_BSIS_TOT-BUKRS.
  WA_ZMMT0062-MONAT     = P_MES.
  WA_ZMMT0062-GJAHR     = P_ANO.
  WA_ZMMT0062-EBELN     = WA_BSIS_TOT-EBELN.
  WA_ZMMT0062-EBELP     = VCONT.
  WA_ZMMT0062-VLR_ARS   = WA_BSIS_TOT-DMBTR.
  WA_ZMMT0062-VLR_USD   = WA_BSIS_TOT-DMBE2.
  WA_ZMMT0062-VLR_BRL   = WA_BSIS_TOT-DMBE3.
  WA_ZMMT0062-BELNR_G   = ''.
  WA_ZMMT0062-GJAHR_G   = ''.
  WA_ZMMT0062-BELNR_E   = ''.
  WA_ZMMT0062-GJAHR_E   = ''.
  WA_ZMMT0062-OBJ_KEY   = VOBJ_KEY.
  WA_ZMMT0062-USUARIO   = SY-UNAME.
  WA_ZMMT0062-DATA      = SY-DATUM.
  WA_ZMMT0062-HORA      = SY-UZEIT.
  MODIFY ZMMT0062  FROM WA_ZMMT0062.
  COMMIT WORK.

ENDFORM.                    " F_GRAVA_ZIB

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_program   programa
*      -->P_dynpro    tela
*      -->P_start     define a tela
*      -->P_fnam      nome do campo ou comando
*      -->P_fval      conteúdo do campo ou comando
*----------------------------------------------------------------------*
FORM F_BDC_DATA  USING P_PROGRAM P_DYNPRO P_START P_FNAM P_FVAL.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR WA_BDCDATA.
  WA_BDCDATA-PROGRAM   = P_PROGRAM.
  WA_BDCDATA-DYNPRO    = P_DYNPRO.
  WA_BDCDATA-DYNBEGIN  = P_START.
  WA_BDCDATA-FNAM      = P_FNAM.
  WA_BDCDATA-FVAL      = P_FVAL.
  APPEND WA_BDCDATA TO TI_BDCDATA.

ENDFORM.                    " F_BDC_DATA


*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*----------------------------------------------------------------------*
FORM ZF_CALL_TRANSACTION USING P_TRANS CHANGING P_ERRO.
  CONSTANTS: C_MSGID LIKE IT_MSG-MSGID VALUE 'F5',
             C_MSGNR LIKE IT_MSG-MSGNR VALUE '312',
             C_MSGNE LIKE IT_MSG-MSGNR VALUE '539'.

  DATA: WL_CONT     TYPE SY-TABIX.

  REFRESH: IT_MSG, IT_ZIB_CONTABIL_ERR.
  CLEAR IT_ZIB_CONTABIL_ERR.

  WL_MODE = 'E'.
  IF P_TRANS = 'F-53'.
    CALL TRANSACTION P_TRANS USING TI_BDCDATA
      MODE WL_MODE
      MESSAGES INTO IT_MSG
      UPDATE 'S'.
  ELSE.
    CALL TRANSACTION P_TRANS USING TI_BDCDATA
          MODE WL_MODE
          MESSAGES INTO IT_MSG.
  ENDIF.
  CLEAR: WL_CONT.

  LOOP AT IT_MSG WHERE MSGTYP EQ 'E'.
    ADD 1 TO WL_CONT.
  ENDLOOP.
  IF WL_CONT  GT 0.
    CLEAR WL_CONT.
    DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY  = VOBJ_KEY.
    LOOP AT IT_MSG WHERE MSGTYP EQ 'E'.
      ADD 1 TO WL_CONT.
      CLEAR: WL_MESSAGE.
      CALL FUNCTION 'CUTC_GET_MESSAGE'
        EXPORTING
          MSG_TYPE       = IT_MSG-MSGTYP
          MSG_ID         = IT_MSG-MSGID
          MSG_NO         = SY-MSGNO
          MSG_ARG1       = SY-MSGV1
          MSG_ARG2       = SY-MSGV2
          MSG_ARG3       = SY-MSGV3
          MSG_ARG4       = SY-MSGV4
        IMPORTING
          RAW_MESSAGE    = WL_MESSAGE
        EXCEPTIONS
          MSG_NOT_FOUND  = 1
          INTERNAL_ERROR = 2
          OTHERS         = 3.

      IF ( SY-SUBRC NE 0 ).
        WL_MESSAGE = 'Erro na mensagem do BATCH-INPUT'.
      ENDIF.

      IT_ZIB_CONTABIL_ERR-OBJ_KEY            = VOBJ_KEY.
      IT_ZIB_CONTABIL_ERR-NR_ITEM            = WL_CONT.
      IT_ZIB_CONTABIL_ERR-INTERFACE          = ''.
      IT_ZIB_CONTABIL_ERR-DT_ATUALIZACAO     = SY-DATUM.
      IT_ZIB_CONTABIL_ERR-HR_ATUALIZACAO     = SY-UZEIT.
      IT_ZIB_CONTABIL_ERR-TYPE               = IT_MSG-MSGTYP.
      IT_ZIB_CONTABIL_ERR-ID                 = IT_MSG-MSGID.
      IT_ZIB_CONTABIL_ERR-NUM                = SY-MSGNO.
      IT_ZIB_CONTABIL_ERR-MESSAGE            = WL_MESSAGE.
      IT_ZIB_CONTABIL_ERR-MESSAGE_V1         = IT_MSG-MSGV1.
      IT_ZIB_CONTABIL_ERR-MESSAGE_V2         = IT_MSG-MSGV2.
      IT_ZIB_CONTABIL_ERR-MESSAGE_V3         = IT_MSG-MSGV3.
      IT_ZIB_CONTABIL_ERR-MESSAGE_V4         = IT_MSG-MSGV4.

      APPEND IT_ZIB_CONTABIL_ERR.
      CLEAR IT_ZIB_CONTABIL_ERR.

    ENDLOOP.

    MODIFY ZIB_CONTABIL_ERR FROM TABLE IT_ZIB_CONTABIL_ERR.
  ENDIF.

  READ TABLE IT_MSG WITH KEY MSGTYP = 'A'.
  IF SY-SUBRC = 0.
    P_ERRO = 'X'.
  ELSE.
    READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      P_ERRO = 'X'.
    ENDIF.
  ENDIF.

  CLEAR WG_DOCUMENTO.
  IF P_TRANS = 'FBRA'.
    READ TABLE IT_MSG WITH KEY MSGID = C_MSGID
                           MSGNR = C_MSGNE
                           MSGTYP = 'S'.
  ELSE.
    READ TABLE IT_MSG WITH KEY MSGID = C_MSGID
                               MSGNR = C_MSGNR
                               MSGTYP = 'S'.
  ENDIF.
  IF SY-SUBRC = 0.
    MOVE IT_MSG-MSGV1 TO WG_DOCUMENTO.
  ENDIF.

  IF  WG_DOCUMENTO IS INITIAL.
    P_ERRO = 'X'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_DOCUMENTO
      IMPORTING
        OUTPUT = WG_DOCUMENTO.
  ENDIF.


ENDFORM.                    "ZF_CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ERR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT_ERR .
  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
        1 'ZIB_CONTABIL_ERR'         'OBJ_KEY'        'IT_ZIB_CONTABIL_ALV' 'OBJ_KEY'         ' '   '20' ' ' ' ' ' ',
        1 'ZIB_CONTABIL_ERR'         'NR_ITEM'        'IT_ZIB_CONTABIL_ALV' 'NR_ITEM'         ' '   '10' ' ' ' ' ' ',
        2 'ZIB_CONTABIL_ERR'         'INTERFACE'      'IT_ZIB_CONTABIL_ALV' 'INTERFACE'       ' '   '15' ' ' ' ' ' ',
        3 'ZIB_CONTABIL_ERR'         'DT_ATUALIZACAO' 'IT_ZIB_CONTABIL_ALV' 'DT_ATUALIZACAO'  ' '   '15' ' ' ' ' ' ',
        4 'ZIB_CONTABIL_ERR'         'HR_ATUALIZACAO' 'IT_ZIB_CONTABIL_ALV' 'HR_ATUALIZACAO'  ' '   '15' ' ' ' ' ' ',
        5 'ZIB_CONTABIL_ERR'         'TYPE'           'IT_ZIB_CONTABIL_ALV' 'TYPE'            ' '   '08' ' ' ' ' ' ',
        6 'ZIB_CONTABIL_ERR'         'ID'             'IT_ZIB_CONTABIL_ALV' 'ID'              ' '   '10' ' ' ' ' ' ',
        7 'ZIB_CONTABIL_ERR'         'NUM'            'IT_ZIB_CONTABIL_ALV' 'NUM'             ' '   '10' ' ' ' ' ' ',
        "8 'ZIB_CONTABIL_ERR'         'MESSAGE'        'IT_ZIB_CONTABIL_ALV' 'MESSAGE'         ' '   '20' ' ' ' ' ' ',
        8 ' '                        ' '              'IT_ZIB_CONTABIL_ALV' 'MESSAGE'         'Mensagem de Erro '   '100' ' ' ' ' ' ',
        9 'ZIB_CONTABIL_ERR'         'MESSAGE_V1'     'IT_ZIB_CONTABIL_ALV' 'MESSAGE_V1'      ' '   '50' ' ' ' ' ' ',
       10 'ZIB_CONTABIL_ERR'         'MESSAGE_V2'     'IT_ZIB_CONTABIL_ALV' 'MESSAGE_V2'      ' '   '30' ' ' ' ' ' ',
       11 'ZIB_CONTABIL_ERR'         'MESSAGE_V3'     'IT_ZIB_CONTABIL_ALV' 'MESSAGE_V3'      ' '   '30' ' ' ' ' ' ',
       12 'ZIB_CONTABIL_ERR'         'MESSAGE_V4'     'IT_ZIB_CONTABIL_ALV' 'MESSAGE_V4'      ' '   '30' ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_ERR

*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_EDIT)
                            VALUE(P_SUM)
                            VALUE(P_EMPHASIZE).

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.
  W_FIELDCATALOG-KEY           = ' '.
  W_FIELDCATALOG-EDIT          = P_EDIT.
  W_FIELDCATALOG-DO_SUM        = P_SUM.

  W_FIELDCATALOG-COL_POS         = P_COL_POS.
  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN      = P_OUTPUTLEN.
  ENDIF.
  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'F_SET_AN'.
  SET TITLEBAR '0200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS_200 OUTPUT.
  DATA: EVENT TYPE CNTL_SIMPLE_EVENT,
               "EVENTS TYPE CNTL_SIMPLE_EVENTS,
               TL_FILTER           TYPE LVC_T_FILT,
               WL_FILTER           TYPE LVC_S_FILT,
               TL_FUNCTION         TYPE UI_FUNCTIONS,
               WL_FUNCTION         LIKE TL_FUNCTION WITH HEADER LINE.

  IF OBG_CONTEINER_ERR IS INITIAL.
    CREATE OBJECT OBG_CONTEINER_ERR
      EXPORTING
        CONTAINER_NAME = G_CC_ERR.


    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = OBG_CONTEINER_ERR.


    PERFORM MONTAR_LAYOUT_ERR.

    REFRESH: TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.

*    wa_layout-cwidth_opt = c_x.
    WA_LAYOUT-NO_TOOLBAR = SPACE.
*     WA_LAYOUT-COL_OPT    = C_X.
    WA_LAYOUT-GRID_TITLE = 'Erros contabilização'.
    WA_LAYOUT-NO_TOOLBAR = C_X.

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = WA_LAYOUT
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
      CHANGING
        IT_FILTER            = TL_FILTER
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = IT_ZIB_CONTABIL_ALV[].

  ELSE.
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP'.
      REFRESH: IT_ZIB_CONTABIL_ERR.
      CALL METHOD GRID1->REFRESH_TABLE_DISPLAY.
      SET SCREEN 0.
    WHEN 'CANCEL'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
