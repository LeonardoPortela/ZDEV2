*&---------------------------------------------------------------------*
*&  Include           ZSDY0002_TOP
*&---------------------------------------------------------------------*


* ----------------------------------------------------------------------
* DATA
* ----------------------------------------------------------------------
  TYPE-POOLS: SLIS.

* Tabla bdcdata
  DATA: BEGIN OF T_BDCTAB OCCURS 0.
          INCLUDE STRUCTURE BDCDATA.
  DATA: END OF T_BDCTAB.

  DATA: LS_MSG TYPE BAL_S_MSG.

  DATA: BEGIN OF T_KNA1 OCCURS 0,
          STCD1 TYPE KNA1-STCD1 ,
          KUNNR TYPE KNA1-KUNNR ,
          BUKRS TYPE BUKRS      ,
        END OF T_KNA1.

  DATA: BEGIN OF T_KNVV OCCURS 0,
          KUNNR LIKE KNVV-KUNNR,
          VKORG LIKE KNVV-VKORG,
          VTWEG LIKE KNVV-VTWEG,
        END OF T_KNVV.

  DATA: BEGIN OF T_KNVI OCCURS 0,
          KUNNR LIKE KNVI-KUNNR,
          TAXKD LIKE KNVI-TAXKD,
        END OF T_KNVI.

* Tabla de mensajes
  DATA: T_MESS LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.


* Tabla donde se levanta el archivo de texto
  DATA: BEGIN OF GT_OUT_AUX OCCURS 0,
        LINE(255),
        END OF GT_OUT_AUX.


  DATA: BEGIN OF T_DATA OCCURS 0,
        ARQ(1),
        PROV(1),
        PUBLI(8),
        VIGEN(8),
        FINAL(8),
        STCD1 TYPE LFA1-STCD1,
        TIPO(1),
        ALTA_SUJETO(1),
        CAMBIO_ALIC(1),
        ALICU_PER(4),
        ALICU_RET(4),
        GR_PER(2),
        GR_RET(2),
        F_PUBLI(10),
        F_VIGEN(10),
        F_FINAL(10),
        LIFNR TYPE LFA1-LIFNR,
        KUNNR TYPE KNA1-KUNNR,
        TAXKD TYPE KNVI-TAXKD,
        BUKRS TYPE BUKRS     ,
        END OF T_DATA.

  DATA: T_BATCH LIKE T_DATA OCCURS 0 WITH HEADER LINE.

* ----------------------------------------------------------------------
* SELECTION-SCREEN.
* ----------------------------------------------------------------------
  SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-000.
  SELECTION-SCREEN SKIP 1.
  PARAMETER: P_PATH  LIKE  RLGRAP-FILENAME.
* PARAMETER: p_vKORG type  vKORG OBLIGATORY.
  SELECTION-SCREEN SKIP 1.
  PARAMETER: P_PROV TYPE C NO-DISPLAY DEFAULT 'X',  "RADIOBUTTON GROUP r1,
             P_CAP  TYPE C NO-DISPLAY.              "RADIOBUTTON GROUP r1.

  SELECTION-SCREEN SKIP 1.
  PARAMETER: P_MODE TYPE C AS CHECKBOX .  "DEFAULT 'N'.
  SELECTION-SCREEN END OF BLOCK BL1.

* ----------------------------------------------------------------------
* AT SELECTION-SCREEN.
* ----------------------------------------------------------------------
  AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.

    DATA: LC_PERIODO(7).

    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      CHANGING
        FILE_NAME = P_PATH.
