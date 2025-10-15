*&---------------------------------------------------------------------*
*&  Include           ZSDY0002_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------
*&      Form  DYNPRO
*&---------------------------------------------------------------------
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR T_BDCTAB.
    MOVE : NAME  TO T_BDCTAB-PROGRAM,
           VALUE TO T_BDCTAB-DYNPRO,
           'X'   TO T_BDCTAB-DYNBEGIN.
    APPEND T_BDCTAB.
  ELSE.
    CLEAR T_BDCTAB.
    MOVE : NAME  TO T_BDCTAB-FNAM,
           VALUE TO T_BDCTAB-FVAL.
    APPEND T_BDCTAB.
  ENDIF.
ENDFORM.                    "dynpro

*&---------------------------------------------------------------------*
*&      Form  subida_txt
*&---------------------------------------------------------------------*
FORM F_SUBIDA_TXT  USING    P_WT_SUBIDA TYPE RLGRAP-FILENAME.
  DATA: LV_PATH TYPE STRING.

  LV_PATH = P_PATH.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = LV_PATH
      FILETYPE                = 'ASC'
    TABLES
      DATA_TAB                = GT_OUT_AUX
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1
      FILE_READ_ERROR         = 2
      NO_BATCH                = 3
      GUI_REFUSE_FILETRANSFER = 4
      INVALID_TYPE            = 5
      NO_AUTHORITY            = 6
      UNKNOWN_ERROR           = 7
      BAD_DATA_FORMAT         = 8
      HEADER_NOT_ALLOWED      = 9
      SEPARATOR_NOT_ALLOWED   = 10
      HEADER_TOO_LONG         = 11
      UNKNOWN_DP_ERROR        = 12
      ACCESS_DENIED           = 13
      DP_OUT_OF_MEMORY        = 14
      DISK_FULL               = 15
      DP_TIMEOUT              = 16
      OTHERS                  = 17.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  PERFORM F_FORMAT_FILE.
ENDFORM.                    " subida_txt


*&---------------------------------------------------------------------*
*&      Form  format_file
*&---------------------------------------------------------------------*
FORM F_FORMAT_FILE.

  DATA: W_SEP TYPE C VALUE ';'.
  DATA: W_INDICE TYPE SY-TABIX.

  LOOP AT GT_OUT_AUX.

    CLEAR:  T_DATA.

    CONDENSE GT_OUT_AUX NO-GAPS.

    SPLIT GT_OUT_AUX AT W_SEP INTO T_DATA-ARQ
                                   T_DATA-PUBLI
                                   T_DATA-VIGEN
                                   T_DATA-FINAL
                                   T_DATA-STCD1
                                   T_DATA-TIPO
                                   T_DATA-ALTA_SUJETO
                                   T_DATA-CAMBIO_ALIC
                                   T_DATA-ALICU_PER
                                   T_DATA-ALICU_RET
                                   T_DATA-GR_PER
                                   T_DATA-GR_RET.
    IF T_DATA-ARQ NE 'P'.
      CONTINUE.
    ENDIF.
    T_DATA-F_PUBLI(2)   = T_DATA-PUBLI(2).
    T_DATA-F_PUBLI+2(1) = '.'.
    T_DATA-F_PUBLI+3(2) = T_DATA-PUBLI+2(2).
    T_DATA-F_PUBLI+5(1) = '.'.
    T_DATA-F_PUBLI+6(4) = T_DATA-PUBLI+4(4).

    T_DATA-F_VIGEN(2)   = T_DATA-VIGEN(2).
    T_DATA-F_VIGEN+2(1) = '.'.
    T_DATA-F_VIGEN+3(2) = T_DATA-VIGEN+2(2).
    T_DATA-F_VIGEN+5(1) = '.'.
    T_DATA-F_VIGEN+6(4) = T_DATA-VIGEN+4(4).

    T_DATA-F_FINAL(2)   = T_DATA-FINAL(2).
    T_DATA-F_FINAL+2(1) = '.'.
    T_DATA-F_FINAL+3(2) = T_DATA-FINAL+2(2).
    T_DATA-F_FINAL+5(1) = '.'.
    T_DATA-F_FINAL+6(4) = T_DATA-FINAL+4(4).

    APPEND T_DATA.

  ENDLOOP.


ENDFORM.                    " format_file


*---------------------------------------------------------------------*
*       FORM f_batch                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM F_BATCH.

  DATA: W_CANT TYPE CHAR17,
        W_MODE TYPE CHAR1.
  IF P_MODE EQ 'X'.
    W_MODE = 'A'.
  ELSE.
    W_MODE = 'N'.
  ENDIF.
  PERFORM F_LOG_CREATE.

  LOOP AT T_BATCH." where kunnr > 999999.


* Para el caso de Provincia de Buenos Aires
    IF P_PROV = 'X'.

*      LOOP AT t_knvv WHERE kunnr = t_batch-kunnr
*                     AND   vkorg = p_vkorg.

      PERFORM DYNPRO USING:
      'X' 'SAPMV13A'  '0100'                 ,
      ' ' 'BDC_OKCODE'  '/00'                ,
      ' ' 'RV13A-KSCHL'	'J1A2'               .

      PERFORM DYNPRO USING:
      'X' 'SAPLV14A' '0100'                  ,
      ' ' 'BDC_CURSOR'  'RV130-SELKZ(02)'    ,
      ' ' 'BDC_OKCODE'  '=WEIT'              ,
      ' ' 'RV130-SELKZ(01)'  ' '             ,
      ' ' 'RV130-SELKZ(02)'  'X'             .

      PERFORM DYNPRO USING:
      'X' 'SAPMV13A'           '1925'           ,
      ' ' 'BDC_OKCODE'         '/00'            ,
      ' ' 'KOMG-ALAND'         'AR'             ,
      ' ' 'KOMG-REGIO'         '01'             ,
      ' ' 'KOMG-KUNNR(01)'     T_BATCH-KUNNR    ,
      ' ' 'KONP-KBETR(01)'     T_BATCH-ALICU_PER,
      ' ' 'RV13A-DATAB(01)'	   T_BATCH-F_VIGEN  ,
      ' ' 'RV13A-DATBI(01)'	   T_BATCH-F_FINAL  ,
      ' ' 'KONP-MWSK1(01)'     'SD'             .

      PERFORM DYNPRO USING:
      'X' 'SAPMV13A'    '1925'               ,
      ' ' 'BDC_OKCODE'  '=SICH'              .

      CALL TRANSACTION 'VK11'
      USING T_BDCTAB
            MODE W_MODE
            MESSAGES INTO T_MESS
            UPDATE 'S'.

      CLEAR T_BDCTAB.
      REFRESH T_BDCTAB.
      LS_MSG-DETLEVEL = 3.

      LOOP AT T_MESS.

        LS_MSG-MSGTY     = T_MESS-MSGTYP.
        LS_MSG-MSGID     = T_MESS-MSGID.
        LS_MSG-MSGNO     = T_MESS-MSGNR.
        LS_MSG-MSGV1     = T_MESS-MSGV1.
        LS_MSG-MSGV2     = T_MESS-MSGV2.
        LS_MSG-MSGV3     = T_MESS-MSGV3.
        LS_MSG-MSGV4     = T_MESS-MSGV4.

        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            I_S_MSG       = LS_MSG
          EXCEPTIONS
            LOG_NOT_FOUND = 0
            OTHERS        = 1.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

      ENDLOOP.

      CLEAR T_MESS.
      CLEAR LS_MSG.
      REFRESH T_MESS.

*      ENDLOOP.

    ENDIF. " Provincia de Buenos Aires

* Para el caso de Capital Federal
    IF P_CAP = 'X'.
      PERFORM DYNPRO USING:
      'X' 'SAPMV13A'  '0100'                 ,
      ' ' 'BDC_OKCODE'  '/00'                ,
      ' ' 'RV13A-KSCHL'	'J1A1'               .

      PERFORM DYNPRO USING:
        'X' 'SAPLV14A' '0100'                  ,
        ' ' 'BDC_CURSOR'  'RV130-SELKZ(02)'    ,
        ' ' 'BDC_OKCODE'  '=WEIT'              ,
        ' ' 'RV130-SELKZ(01)'  ' '             ,
        ' ' 'RV130-SELKZ(02)'  'X'             .

      PERFORM DYNPRO USING:
        'X' 'SAPMV13A'           '1925'           ,
        ' ' 'BDC_OKCODE'         '/00'            ,
        ' ' 'KOMG-ALAND'         'AR'             ,
        ' ' 'KOMG-REGIO'         '01'             ,
        ' ' 'KOMG-KUNNR(01)'     T_BATCH-KUNNR    ,
        ' ' 'KONP-KBETR(01)'     T_BATCH-ALICU_PER,
        ' ' 'RV13A-DATAB(01)'	   T_BATCH-F_VIGEN  ,
        ' ' 'RV13A-DATBI(01)'	   T_BATCH-F_FINAL  ,
        ' ' 'KONP-MWSK1(01)'     'SD'             .


      PERFORM DYNPRO USING:
        'X' 'SAPMV13A'    '1925'               ,
        ' ' 'BDC_OKCODE'  '=SICH'              .

      CALL TRANSACTION 'VK11'
      USING T_BDCTAB
            MODE W_MODE
            MESSAGES INTO T_MESS
            UPDATE 'S'.

      CLEAR T_BDCTAB.
      REFRESH T_BDCTAB.
      LS_MSG-DETLEVEL = 3.

      LOOP AT T_MESS.

        LS_MSG-MSGTY     = T_MESS-MSGTYP.
        LS_MSG-MSGID     = T_MESS-MSGID.
        LS_MSG-MSGNO     = T_MESS-MSGNR.
        LS_MSG-MSGV1     = T_MESS-MSGV1.
        LS_MSG-MSGV2     = T_MESS-MSGV2.
        LS_MSG-MSGV3     = T_MESS-MSGV3.
        LS_MSG-MSGV4     = T_MESS-MSGV4.

        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            I_S_MSG       = LS_MSG
          EXCEPTIONS
            LOG_NOT_FOUND = 0
            OTHERS        = 1.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

      ENDLOOP.

      CLEAR T_MESS.
      CLEAR LS_MSG.
      REFRESH T_MESS.

    ENDIF. " Capital Federal

  ENDLOOP.  "LOOP AT T_BATCH

  PERFORM F_MUESTRA_LOG.

ENDFORM.                    "f_batch

*&---------------------------------------------------------------------*
*&      Form  f_muestra_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MUESTRA_LOG.

  DATA: LS_FCAT TYPE BAL_S_FCAT.
  DATA:LS_DISPLAY_PROFILE TYPE BAL_S_PROF.
  DATA: LC_X TYPE C VALUE 'X',
        LC_2 TYPE C VALUE '2',
        LC_3 TYPE C VALUE '3',
        LC_4 TYPE C VALUE '4',
        LC_5 TYPE C VALUE '5',
        LC_MSGTY TYPE DD03P-FIELDNAME VALUE 'MSGTY',
        LC_MSGID TYPE DD03P-FIELDNAME VALUE 'MSGID',
        LC_MSGNO TYPE DD03P-FIELDNAME VALUE 'MSGNO',
        LC_T_MSG TYPE DD03P-FIELDNAME VALUE 'T_MSG',
        LV_TITLE LIKE SY-TITLE.

* get variant which creates hierarchy according to field DETLEVEL
  CALL FUNCTION 'BAL_DSP_PROFILE_DETLEVEL_GET'
    IMPORTING
      E_S_DISPLAY_PROFILE = LS_DISPLAY_PROFILE
    EXCEPTIONS
      OTHERS              = 1.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* use grid for display if wanted
  LS_DISPLAY_PROFILE-USE_GRID = LC_X.

*DISPLAYING THE ADDITIONAL FIELDS BY MODIFYING THE FIELDCAT
  LOOP AT LS_DISPLAY_PROFILE-MESS_FCAT INTO LS_FCAT.

    IF LS_FCAT-REF_FIELD = LC_MSGTY.
      LS_FCAT-COL_POS = LC_2.
      LS_FCAT-NO_OUT  = SPACE.
    ENDIF.

    IF LS_FCAT-REF_FIELD = LC_MSGID.
      LS_FCAT-COL_POS = LC_3.
      LS_FCAT-NO_OUT  = SPACE.
    ENDIF.

    IF LS_FCAT-REF_FIELD = LC_MSGNO.
      LS_FCAT-COL_POS = LC_4.
      LS_FCAT-NO_OUT  = SPACE.
    ENDIF.

    IF LS_FCAT-REF_FIELD = LC_T_MSG.
      LS_FCAT-COL_POS = LC_5.
      LS_FCAT-NO_OUT  = SPACE.
    ENDIF.

    MODIFY LS_DISPLAY_PROFILE-MESS_FCAT FROM LS_FCAT.

  ENDLOOP.

* set report to allow saving of variants
  LS_DISPLAY_PROFILE-DISVARIANT-REPORT = SY-REPID.

* use grid for display if wanted
  LS_DISPLAY_PROFILE-USE_GRID = LC_X.

  LV_TITLE = SY-TITLE.

* Display the title of the Application Log
  MOVE LV_TITLE TO LS_DISPLAY_PROFILE-TITLE.                "#EC NOTEXT

* Keep tree on top of the messages list
  LS_DISPLAY_PROFILE-TREE_ONTOP = LC_X.
  LS_DISPLAY_PROFILE-TREE_ADJST = LC_X.

  LS_DISPLAY_PROFILE-BYDETLEVEL = SPACE.

* call display function module
* We do not specify any filter (like I_S_LOG_FILTER, ...,
* I_T_MSG_HANDLE) since we want to display all logs available
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      I_S_DISPLAY_PROFILE = LS_DISPLAY_PROFILE
    EXCEPTIONS
      OTHERS              = 1.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " f_muestra_log


*&---------------------------------------------------------------------*
*&      Form  P_LOG_CREATE
*&---------------------------------------------------------------------*
FORM F_LOG_CREATE.
  DATA:
    L_S_LOG TYPE BAL_S_LOG.

* define some header data of this log
  L_S_LOG-EXTNUMBER = 'Application Log'.                    "#EC NOTEXT
  L_S_LOG-ALUSER    = SY-UNAME.
  L_S_LOG-ALPROG    = SY-REPID.
* ...

* create a log
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      I_S_LOG = L_S_LOG
    EXCEPTIONS
      OTHERS  = 1.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " P_LOG_CREATE

*&---------------------------------------------------------------------*
*&      Form  selecciona_datos
*&---------------------------------------------------------------------*
FORM F_SELECCIONA_DATOS.

  DATA: W_INDICE TYPE SY-TABIX,
        W_ALIC_CLI TYPE P DECIMALS 2,
        W_DCPFM TYPE USR01-DCPFM.

  PERFORM F_FILTRO_CLIENTE.

  IF T_DATA[] IS NOT INITIAL.
*    IF t_kna1[] IS NOT INITIAL.
*      SELECT kunnr vkorg vtweg FROM knvv
*             INTO TABLE t_knvv
*             FOR ALL ENTRIES IN t_kna1
*             WHERE kunnr = t_kna1-kunnr
*             AND   vkorg = p_vkorg.      "Org. de Ventas de Argetina
*
*    ENDIF.

*    LOOP AT t_kna1.
*      READ TABLE t_data WITH KEY stcd1 = t_kna1-stcd1.
*      t_data-kunnr = t_kna1-kunnr.
*      READ TABLE t_knvi WITH KEY kunnr = t_kna1-kunnr.
*      t_data-taxkd = t_knvi-taxkd.
*      APPEND t_data TO t_batch.
*    ENDLOOP.

    T_BATCH[] = T_DATA[].

    SELECT SINGLE DCPFM FROM USR01 INTO W_DCPFM WHERE BNAME = SY-UNAME.

    LOOP AT T_BATCH.

      IF W_DCPFM = 'X'.
        REPLACE ',' WITH '.' INTO T_BATCH-ALICU_PER.
        REPLACE ',' WITH '.' INTO T_BATCH-ALICU_RET.
        MODIFY T_BATCH INDEX SY-TABIX.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " selecciona_datos
*&---------------------------------------------------------------------*
*&      Form  F_FILTRO_CLIENTE
*&---------------------------------------------------------------------*
FORM F_FILTRO_CLIENTE.
  DATA: " tl_knb1 TYPE STANDARD TABLE OF knb1,
        " stl_knb1 TYPE knb1                 ,
        W_INDEX TYPE SY-TABIX              .
*- Buscamos los clientes que tenemos en sap por el cuit que nos
*  informa la DGP
  SELECT STCD1 KUNNR FROM KNA1 INTO TABLE T_KNA1
    FOR ALL ENTRIES IN T_DATA
      WHERE STCD1 = T_DATA-STCD1 AND
      LAND1 EQ 'AR' .

* Reducimos el archivo
  LOOP AT T_DATA.
    W_INDEX = SY-TABIX.
    READ TABLE T_KNA1
    WITH KEY STCD1 = T_DATA-STCD1.
    IF SY-SUBRC NE 0.
      DELETE T_DATA INDEX W_INDEX.
    ELSE.
      MOVE T_KNA1-KUNNR TO T_DATA-KUNNR.
      MODIFY T_DATA INDEX W_INDEX .
    ENDIF.
  ENDLOOP.
  REFRESH T_KNA1.
**---- BUscamos el cliente con la sociedad del parametro
*  SELECT stcd1 kunnr FROM kna1 INTO TABLE t_kna1
*  FOR ALL ENTRIES IN t_data
*    WHERE stcd1 = t_data-stcd1.
*
*  LOOP AT t_kna1.
*    READ TABLE t_data WITH KEY stcd1 = t_kna1-stcd1.
*    IF sy-subrc EQ 0.
*      MOVE:  t_kna1-kunnr TO t_data-kunnr.
*      APPEND t_data.
*    ENDIF.
*  ENDLOOP.
*
*  SELECT *
*  FROM knb1
*  INTO TABLE tl_knb1
*  FOR ALL ENTRIES IN t_data
*  WHERE kunnr EQ t_data-kunnr
*    AND bukrs EQ p_vkorg.
**--borro clientes diferente a la sociedad del parametro
*  LOOP AT t_data.
*    w_index = sy-tabix.
*    READ TABLE tl_knb1 INTO stl_knb1
*    WITH KEY  kunnr = t_data-kunnr.
*    IF sy-subrc EQ 0.
*      MOVE stl_knb1-bukrs TO t_data-bukrs.
*      MODIFY t_data INDEX w_index .
*    ELSE.
*      DELETE t_data INDEX w_index .
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " F_FILTRO_CLIENTE
