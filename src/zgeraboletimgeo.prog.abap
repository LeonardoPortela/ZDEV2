*&---------------------------------------------------------------------*
*& Report  ZGERABOLETIMGEO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZGERABOLETIMGEO.

TABLES:ZPPS_GOODSMV_LOG.

TYPES: BEGIN OF TY_ZPPS_GOODSMV_LOG ,
         OBJ_KEY TYPE ZPPS_GOODSMV_LOG-OBJ_KEY,
       END OF TY_ZPPS_GOODSMV_LOG .

DATA : T_ZPPS_GOODSMV_LOG      TYPE TABLE OF TY_ZPPS_GOODSMV_LOG,
       T_ZPPS_GOODSMV_LOG_AUX  TYPE TABLE OF TY_ZPPS_GOODSMV_LOG,
       IT_ZPPS_GOODSMV_LOG     TYPE TABLE OF ZPPS_GOODSMV_LOG,
       TI_BDC                  TYPE TABLE OF BDCDATA,
       T_BDCMSGCOLL TYPE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

DATA : WA_ZPPS_GOODSMV_LOG TYPE ZPPS_GOODSMV_LOG,
       ST_ZPPS_GOODSMV_LOG TYPE TY_ZPPS_GOODSMV_LOG ,
       WA_BDC              TYPE BDCDATA.


SELECT-OPTIONS: POBJ_KEY FOR  ZPPS_GOODSMV_LOG-OBJ_KEY OBLIGATORY.



*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*

PERFORM PROCESSA_SHDB.

*&---------------------------------------------------------------------*
*&      Form  seleciona_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS.

  DATA : VL_OBJ_KEY TYPE ZPPS_GOODSMV_LOG-OBJ_KEY.

  SELECT OBJ_KEY
    FROM ZPPS_GOODSMV_LOG
    INTO TABLE T_ZPPS_GOODSMV_LOG_AUX
   WHERE OBJ_KEY IN POBJ_KEY .

  SORT : T_ZPPS_GOODSMV_LOG_AUX BY OBJ_KEY.

  VL_OBJ_KEY = ''.

  LOOP AT T_ZPPS_GOODSMV_LOG_AUX INTO ST_ZPPS_GOODSMV_LOG.

    IF VL_OBJ_KEY <> ST_ZPPS_GOODSMV_LOG-OBJ_KEY .
      APPEND ST_ZPPS_GOODSMV_LOG TO T_ZPPS_GOODSMV_LOG.
      VL_OBJ_KEY = ST_ZPPS_GOODSMV_LOG-OBJ_KEY.
    ENDIF.

  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM T_ZPPS_GOODSMV_LOG COMPARING OBJ_KEY.




  SELECT *
    FROM ZPPS_GOODSMV_LOG
    INTO TABLE IT_ZPPS_GOODSMV_LOG
   WHERE OBJ_KEY IN POBJ_KEY .


  SORT : T_ZPPS_GOODSMV_LOG  BY OBJ_KEY,
         IT_ZPPS_GOODSMV_LOG BY OBJ_KEY.

ENDFORM.                    "seleciona_dados

*&---------------------------------------------------------------------*
*&      Form  processa_shdb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PROCESSA_SHDB.

  PERFORM SELECIONA_DADOS.

  LOOP AT T_ZPPS_GOODSMV_LOG INTO ST_ZPPS_GOODSMV_LOG.
    PERFORM ZSHDB USING ST_ZPPS_GOODSMV_LOG-OBJ_KEY.
  ENDLOOP.

ENDFORM.                    "processa_shdb


*&---------------------------------------------------------------------*
*&      Form  zshdb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VOBJ_KEY   text
*----------------------------------------------------------------------*
FORM ZSHDB USING  VOBJ_KEY.
  DATA: VL_TABIX     TYPE SY-TABIX,
       VL_CNT(2)           TYPE N,
       VL_QUEBRA(2)        TYPE N,
       VL_QTDE             TYPE C LENGTH 16,
       VL_BUDAT_CHAR       TYPE CHAR10,
       VL_BLDAT_CHAR       TYPE CHAR10,
       VL_FIELD_MAT        TYPE C LENGTH 30,
       VL_FIELD_QUANT      TYPE C LENGTH 30,
       VL_FIELD_CENTRO     TYPE C LENGTH 30,
       VL_FIELD_DEP        TYPE C LENGTH 30,
       VL_MESSAGE          TYPE STRING,
       VL_MODE             TYPE C LENGTH 1,
       VL_UPDATE           TYPE C LENGTH 1,
       VC_VERID_PAI        TYPE CKMLMV013-VERID,
       VC_WERKS_PAI        TYPE CKMLMV013-PRWRK,
       VC_MATNR_PAI        TYPE CKMLMV013-PMATN,
       VC_AUFNR_PAI        TYPE AUFNR,
       V_AUFNR_PAI         TYPE AUFNR,
       V_DTMVTO            TYPE ZPPS_XIMFBF_LOG-DTMVTO.

  REFRESH TI_BDC.

  DATA: WA_GOODSMOVEMENTS TYPE BAPI2017_GM_ITEM_CREATE.

  VL_TABIX = SY-TABIX.

  SELECT SINGLE AUFNR DTMVTO
    FROM ZPPS_XIMFBF_LOG
    INTO (V_AUFNR_PAI, V_DTMVTO)
   WHERE OBJ_KEY = VOBJ_KEY.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = V_AUFNR_PAI
    IMPORTING
      OUTPUT = VC_AUFNR_PAI.

  SELECT SINGLE VERID PRWRK PMATN
    INTO (VC_VERID_PAI, VC_WERKS_PAI, VC_MATNR_PAI)
    FROM CKMLMV013
   WHERE AUFNR = VC_AUFNR_PAI.

  READ TABLE IT_ZPPS_GOODSMV_LOG  INTO WA_ZPPS_GOODSMV_LOG WITH KEY OBJ_KEY = VOBJ_KEY.

  CONCATENATE V_DTMVTO+6(2) '.' V_DTMVTO+4(2) '.' V_DTMVTO(4)

       INTO VL_BUDAT_CHAR.

  "  CONCATENATE w_bflushdatagen-postdate+6(2) '. ' w_bflushdatagen-postdate+4(2) '.' w_bflushdatagen-postdate(4)
  "       INTO vl_budat_char.
  CONCATENATE SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM(4)
       INTO VL_BLDAT_CHAR.
*  break-point.

  PERFORM ZF_BDC USING:


  'X'   'SAPLBARM'            '0800',
  ' '   'BDC_CURSOR'          'RM61B-RB_KOMPO',
  ' '   'BDC_OKCODE'          '=RBTYP',
  ' '   'RM61B-RB_KOMPO'      'X',
  ' '   'RM61B-BUDAT'         VL_BUDAT_CHAR, "w_xi_mfbf-dtmvto
  ' '   'RM61B-BLDAT'         VL_BLDAT_CHAR, "w_xi_mfbf-dtmvto
  ' '   'RM61B-WERKS'         WA_ZPPS_GOODSMV_LOG-PLANT,

  'X'   'SAPLBARM'            '0800',
  ' '   'BDC_OKCODE'          '=ISTDA',
  ' '   'RM61B-RB_KOMPO'      'X',
  ' '   'RM61B-BUDAT'         VL_BUDAT_CHAR, "w_xi_mfbf-dtmvto
  ' '   'RM61B-BLDAT'         VL_BLDAT_CHAR, "w_xi_mfbf-dtmvto
  ' '   'RM61B-BKTXT'         WA_ZPPS_GOODSMV_LOG-DOCHEADERTXT,
  ' '   'BDC_CURSOR'          'RM61B-VERID',
  ' '   'RM61B-MATNR'         VC_MATNR_PAI,
  ' '   'RM61B-WERKS'         WA_ZPPS_GOODSMV_LOG-PLANT,
  ' '   'RM61B-VERID'         VC_VERID_PAI,
  ' '   'RM61B-BOM_OFF'       'X',

* loop dos materiais para baixa
  'X'   'SAPLCOWB'            '0130',
  ' '   'BDC_OKCODE'          '/00'.
*  ' '   'BDC_CURSOR'          'COWB_COMP-LGORT(16)'.

  CLEAR: VL_CNT.
  VL_CNT = 1.

  LOOP AT IT_ZPPS_GOODSMV_LOG INTO WA_ZPPS_GOODSMV_LOG WHERE OBJ_KEY = VOBJ_KEY.
    CONCATENATE 'COWB_COMP-MATNR('
                 VL_CNT
                           ')' INTO VL_FIELD_MAT.
    CONCATENATE 'COWB_COMP-ERFMG_R('
                 VL_CNT
                           ')' INTO VL_FIELD_QUANT.
    CONCATENATE 'COWB_COMP-WERKS('
                 VL_CNT
                           ')' INTO VL_FIELD_CENTRO.
    CONCATENATE 'COWB_COMP-LGORT('
                 VL_CNT
                           ')' INTO VL_FIELD_DEP.
    "vl_qtde = wa_goodsmovements-ENTRY_QNT.
    WRITE WA_ZPPS_GOODSMV_LOG-ENTRY_QNT TO VL_QTDE.
*     replace all occurrences of regex ',' in vl_qtde with '.'.
    CONDENSE VL_QTDE.
    PERFORM ZF_BDC USING:
    ' '   VL_FIELD_MAT      WA_ZPPS_GOODSMV_LOG-MATNR,
    ' '   VL_FIELD_QUANT    VL_QTDE,
    ' '   VL_FIELD_DEP      WA_ZPPS_GOODSMV_LOG-STGE_LOC,
    ' '   VL_FIELD_CENTRO   WA_ZPPS_GOODSMV_LOG-PLANT.
    VL_CNT = VL_CNT + 1.
  ENDLOOP.

  PERFORM ZF_BDC USING:
  'X'   'SAPLCOWB'            '0130',
  ' '   'BDC_OKCODE'          '=WEIT',
  ' '   'BDC_SUBSCR'          'SAPLCOWB',

  'X'   'SAPLBARM'            '0800',
  ' '   'BDC_OKCODE'          '/EEND'.

  VL_MODE = 'N'.
  VL_UPDATE = 'S'.

  CALL TRANSACTION 'MFBF'
     USING TI_BDC
     MODE   VL_MODE
     UPDATE VL_UPDATE
     MESSAGES INTO T_BDCMSGCOLL.


  COMMIT WORK.

ENDFORM.                    "zshdb

*&---------------------------------------------------------------------*
*&      Form  zf_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DYNBEGIN text
*      -->P_NAME     text
*      -->P_VALUE    text
*----------------------------------------------------------------------*
FORM ZF_BDC USING P_DYNBEGIN TYPE ANY
                  P_NAME     TYPE ANY
                  P_VALUE    TYPE ANY.

  IF P_DYNBEGIN EQ 'X'.
    WA_BDC-PROGRAM  = P_NAME.
    WA_BDC-DYNPRO   = P_VALUE.
    WA_BDC-DYNBEGIN = P_DYNBEGIN.

    APPEND WA_BDC
      TO TI_BDC.
  ELSE.
    WA_BDC-FNAM = P_NAME.
    WA_BDC-FVAL = P_VALUE.

    APPEND WA_BDC
      TO TI_BDC.
  ENDIF.

  CLEAR WA_BDC.
ENDFORM.                    " ZF_BDC
