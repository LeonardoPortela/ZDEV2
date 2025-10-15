*&---------------------------------------------------------------------*
*&  Include           ZFIY0001_FORM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_SUBIDA_TXT
*&---------------------------------------------------------------------*
FORM F_SUBIDA_TXT USING P_WT_SUBIDA TYPE RLGRAP-FILENAME  CHANGING P_ERRO.
  DATA: LV_PATH TYPE STRING.
  CLEAR LV_PATH.
  IF P_CHECK2 EQ 'X'.
    IF P_PATH IS NOT INITIAL.
      LV_PATH = P_PATH.
    ENDIF.

    PERFORM F_SHOW_PROGRESS_IND USING 'Obteniendo archivo local' 10.

    IF LV_PATH  IS NOT INITIAL.
      CALL FUNCTION 'C13Z_UPLOAD'
        EXPORTING
          FILENAME                = LV_PATH
          FILETYPE                = 'ASC'
        TABLES
          DATA_TAB                = T_OUT_AUX
        EXCEPTIONS
          CONVERSION_ERROR        = 1
          FILE_OPEN_ERROR         = 2
          FILE_READ_ERROR         = 3
          INVALID_TYPE            = 4
          NO_BATCH                = 5
          UNKNOWN_ERROR           = 6
          INVALID_TABLE_WIDTH     = 7
          GUI_REFUSE_FILETRANSFER = 8
          CUSTOMER_ERROR          = 9
          NO_AUTHORITY            = 10
          BAD_DATA_FORMAT         = 11
          HEADER_NOT_ALLOWED      = 12
          SEPARATOR_NOT_ALLOWED   = 13
          HEADER_TOO_LONG         = 14
          UNKNOWN_DP_ERROR        = 15
          ACCESS_DENIED           = 16
          DP_OUT_OF_MEMORY        = 17
          DISK_FULL               = 18
          DP_TIMEOUT              = 19
          NOT_SUPPORTED_BY_GUI    = 20
          ERROR_NO_GUI            = 21
          OTHERS                  = 22.
      IF SY-SUBRC <> 0.
        P_ERRO = 'X'.
      ENDIF.
    ENDIF.
    CLEAR LV_PATH.
    IF P_PATHI IS NOT INITIAL.
      LV_PATH = P_PATHI.
    ENDIF.
    " Ruta2
    IF LV_PATH  IS NOT INITIAL.
      CALL FUNCTION 'C13Z_UPLOAD'
        EXPORTING
          FILENAME                = LV_PATH
          FILETYPE                = 'ASC'
        TABLES
          DATA_TAB                = T_OUT_AUX2
        EXCEPTIONS
          CONVERSION_ERROR        = 1
          FILE_OPEN_ERROR         = 2
          FILE_READ_ERROR         = 3
          INVALID_TYPE            = 4
          NO_BATCH                = 5
          UNKNOWN_ERROR           = 6
          INVALID_TABLE_WIDTH     = 7
          GUI_REFUSE_FILETRANSFER = 8
          CUSTOMER_ERROR          = 9
          NO_AUTHORITY            = 10
          BAD_DATA_FORMAT         = 11
          HEADER_NOT_ALLOWED      = 12
          SEPARATOR_NOT_ALLOWED   = 13
          HEADER_TOO_LONG         = 14
          UNKNOWN_DP_ERROR        = 15
          ACCESS_DENIED           = 16
          DP_OUT_OF_MEMORY        = 17
          DISK_FULL               = 18
          DP_TIMEOUT              = 19
          NOT_SUPPORTED_BY_GUI    = 20
          ERROR_NO_GUI            = 21
          OTHERS                  = 22.
      IF SY-SUBRC <> 0.
        P_ERRO = 'X'.
      ENDIF.
    ENDIF.

    LOOP AT T_OUT_AUX INTO ST_OUT_AUX.
      EXIT.
    ENDLOOP.

    LOOP AT T_OUT_AUX2 INTO ST_OUT_AUX2.
      IF ST_OUT_AUX+0(1) = ST_OUT_AUX2+0(1). " ERRO
        P_ERRO = 'Z'.
        EXIT.
      ENDIF.
      APPEND ST_OUT_AUX2 TO T_OUT_AUX.
    ENDLOOP.
    REFRESH T_OUT_AUX2.
  ELSE.

*-------Subida del Servidor
    PERFORM F_SHOW_PROGRESS_IND USING 'Obteniendo archivo del servidor' 10.
    PERFORM F_SUBIDA_SERVIDOR.
  ENDIF.


ENDFORM. " F_SUBIDA_TXT

*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_FILE
*&---------------------------------------------------------------------*
FORM F_FORMAT_FILE .
  DATA: W_SEP TYPE C VALUE ';'.
  DATA: W_INDICE TYPE SY-TABIX.

  CLEAR:  ST_DATA,
           T_DATA,
           V_ANS .

  PERFORM F_SHOW_PROGRESS_IND USING 'Procesando archivo' 25.

  LOOP AT T_OUT_AUX INTO ST_OUT_AUX.

    CONDENSE ST_OUT_AUX NO-GAPS.

    IF P_CAP EQ 'X'.

      CLEAR:  ST_DATA.
      SPLIT ST_OUT_AUX AT W_SEP INTO "ST_DATA-ARQ
                                     ST_DATA-PUBLI
                                     ST_DATA-VIGEN
                                     ST_DATA-FINAL
                                     ST_DATA-STCD1
                                     ST_DATA-TIPO
                                     ST_DATA-ALTA_SUJETO
                                     ST_DATA-CAMBIO_ALIC
                                     ST_DATA-ALICU
                                     ST_DATA-ALICU_PER
                                     ST_DATA-GRUPO.

*    Fecha de Publicacion se usa para el archivo a bajar
      V_F_PUBLI(2)   = ST_DATA-PUBLI(2).
      V_F_PUBLI+2(1) = '.'.
      V_F_PUBLI+3(2) = ST_DATA-PUBLI+2(2).
      V_F_PUBLI+5(1) = '.'.
      V_F_PUBLI+6(4) = ST_DATA-PUBLI+4(4).
      IF V_ANS IS INITIAL.
        PERFORM F_POPUP_TO_DECIDE  USING V_F_PUBLI     CHANGING V_ANS.
        IF V_ANS EQ '1'.
*          PERFORM FECHA_EJECUCION    USING ST_DATA-PUBLI CHANGING V_ANS.
        ENDIF.
      ENDIF.

*   Completo la tabla si acepta la fecha de publicacion
      IF V_ANS EQ 1.
        APPEND ST_DATA    TO T_DATA.
        APPEND ST_OUT_AUX TO T_PADRONES.

      ELSE.
        MESSAGE TEXT-E03 TYPE 'S' DISPLAY LIKE 'E' .
        EXIT.
      ENDIF.

    ELSEIF P_PROV = 'X'.

      CLEAR:  ST_DATA_ARBA.
      SPLIT ST_OUT_AUX AT W_SEP INTO ST_DATA_ARBA-ARQ
                                     ST_DATA_ARBA-PUBLI
                                     ST_DATA_ARBA-VIGEN
                                     ST_DATA_ARBA-FINAL
                                     ST_DATA_ARBA-STCD1
                                     ST_DATA_ARBA-TIPO
                                     ST_DATA_ARBA-ALTA_SUJETO
                                     ST_DATA_ARBA-CAMBIO_ALIC
                                     ST_DATA_ARBA-ALICU
                                     ST_DATA_ARBA-GRUPO.

*    Fecha de Publicacion se usa para el archivo a bajar
      V_F_PUBLI(2)   = ST_DATA_ARBA-PUBLI(2).
      V_F_PUBLI+2(1) = '.'.
      V_F_PUBLI+3(2) = ST_DATA_ARBA-PUBLI+2(2).
      V_F_PUBLI+5(1) = '.'.
      V_F_PUBLI+6(4) = ST_DATA_ARBA-PUBLI+4(4).
      IF V_ANS IS INITIAL.
        PERFORM F_POPUP_TO_DECIDE  USING V_F_PUBLI     CHANGING V_ANS.
        IF V_ANS EQ '1'.
*          PERFORM FECHA_EJECUCION    USING ST_DATA_ARBA-PUBLI CHANGING V_ANS.
        ENDIF.
      ENDIF.

*   Completo la tabla si acepta la fecha de publicacion
      IF V_ANS EQ 1.
        APPEND ST_DATA_ARBA    TO T_DATA_ARBA.
        APPEND ST_OUT_AUX TO T_PADRONES.

      ELSE.
        MESSAGE TEXT-E03 TYPE 'S' DISPLAY LIKE 'E' .
        EXIT.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM. " F_FORMAT_FILE

*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_DECIDE
*&---------------------------------------------------------------------*
FORM F_POPUP_TO_DECIDE USING PI_PUBLI CHANGING PO_ANS.

  IF SY-BATCH EQ ' '.
    CALL FUNCTION 'POPUP_TO_DECIDE'
      EXPORTING
        DEFAULTOPTION  = '1'
        TEXTLINE1      = 'La fecha de publicación es'
        TEXTLINE2      = PI_PUBLI
        TEXTLINE3      = '¿Desea cargarlo?'
        TEXT_OPTION1   = 'Si'
        TEXT_OPTION2   = 'No'
        TITEL          = ''
        START_COLUMN   = 25
        START_ROW      = 6
        CANCEL_DISPLAY = ' '
      IMPORTING
        ANSWER         = PO_ANS.
  ELSE.
    PO_ANS = 1.
  ENDIF.

  PERFORM F_SHOW_PROGRESS_IND USING 'Procesando archivo' 30.

ENDFORM. " F_POPUP_TO_DECIDE

*&---------------------------------------------------------------------*
*&      Form  KD_GET_FILENAME_ON_F4
*&---------------------------------------------------------------------*
FORM KD_GET_FILENAME_ON_F4 CHANGING PO_PATH.
  DATA: LC_PERIODO(7).

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      FILE_NAME = PO_PATH.
ENDFORM. " KD_GET_FILENAME_ON_F4

************************************************************************
* F4-help for filename
************************************************************************
FORM FILE_F4 CHANGING PO_PATH.

  DATA:
    LT_FILETABLE TYPE FILETABLE,
    LF_RC        TYPE I.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    CHANGING
      SELECTED_FOLDER      = PO_PATH
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
               DISPLAY LIKE 'E'
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

ENDFORM.                                                    "file_f4

*&---------------------------------------------------------------------*
*&      Form  F_SELE_LFA1
*&---------------------------------------------------------------------*
FORM F_SELE_LFA1 .
  DATA: VL_IDX TYPE SY-TABIX.

  PERFORM F_SHOW_PROGRESS_IND USING 'Obteniendo datos maestros' 60.

*  Busco los proveedores.
  IF T_DATA[] IS NOT INITIAL.
    SELECT STCD1 LIFNR "BUKRS
    FROM LFA1
    INTO TABLE T_LFA1
    FOR ALL ENTRIES IN T_DATA
    WHERE STCD1 = T_DATA-STCD1.

  ELSEIF T_DATA_ARBA[] IS NOT INITIAL.
    SELECT STCD1 LIFNR "BUKRS
    FROM LFA1
    INTO TABLE T_LFA1
    FOR ALL ENTRIES IN T_DATA_ARBA
    WHERE STCD1 = T_DATA_ARBA-STCD1.
  ENDIF.

  SORT T_LFA1 BY LIFNR STCD1.

* BUsco la sociedad que esta cada proveedor.
  SELECT LIFNR BUKRS
  FROM LFB1
  INTO TABLE T_LFB1
  FOR ALL ENTRIES IN T_LFA1
  WHERE LIFNR EQ T_LFA1-LIFNR
  AND   BUKRS IN SO_BUKRS   .

  SORT T_LFB1 BY LIFNR BUKRS.

* armo la tabla con todo los datos del proveedor.
  LOOP AT T_LFA1 INTO ST_LFA1.
    VL_IDX = SY-TABIX.
    READ TABLE T_LFB1 INTO ST_LFB1
    WITH KEY LIFNR = ST_LFA1-LIFNR BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE: ST_LFB1-BUKRS TO ST_LFA1-BUKRS.
      MODIFY T_LFA1 INDEX VL_IDX FROM ST_LFA1.
    ELSE.
      DELETE T_LFA1 INDEX VL_IDX.
    ENDIF.
  ENDLOOP.
ENDFORM. " F_SELE_LFA1

*&---------------------------------------------------------------------*
*&      Form  F_BAJADA_SERVIDOR
*&---------------------------------------------------------------------*
FORM F_BAJADA_SERVIDOR .
  DATA: BEGIN OF TABL OCCURS 0,
          LINE(560),
        END OF TABL.

  PERFORM F_SHOW_PROGRESS_IND USING 'Bajando archivo al servidor' 95.

  DATA: VL_RUTA      TYPE CHAR100  VALUE '/Finanzas/Padrones_Arba/'    ,
        VL_PADRON    TYPE CHAR100  VALUE 'Padron_'           ,
        VL_CORRECTO  TYPE CHAR100  VALUE 'Datos_Correctos_'  ,
        VL_INCORREC  TYPE CHAR100  VALUE 'Datos_Incorrectos_',
        VL_DSN       TYPE CHAR200                               ,
        VL_CHOW      TYPE CHAR17  VALUE 'chown pagnrl:spa '    ,
        PARCOM_LOC(100)                                        .

*   Retenciones Capital Federal
  IF P_CAP EQ 'X'.

    CONCATENATE P_PATH VL_RUTA
                'Padrones_CABA_IIBB/'
                VL_PADRON
                V_F_PUBLI
                '.txt'
    INTO VL_DSN.
  ENDIF.

*   Retenciones de la Provincia de Bs As.
  IF P_PROV EQ 'X'.
    CONCATENATE P_PATH VL_RUTA
                'Padrones_Arba_IIBB/'
                VL_PADRON
                V_F_PUBLI
                '.txt'
    INTO VL_DSN.
  ENDIF.

  OPEN DATASET VL_DSN FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  CHECK SY-SUBRC EQ 0.
  LOOP AT T_PADRONES INTO ST_PADRONES.
    TRANSFER ST_PADRONES TO  VL_DSN." LENGTH '255'.
    IF SY-SUBRC <> 0.
      MESSAGE I601(6P) WITH VL_DSN.
      STOP.
    ENDIF.
  ENDLOOP.

  PERFORM DATASET USING VL_CHOW
                        VL_DSN.

* Bajo los datos correctos
  DATA VL_TIME TYPE CHAR1.

* Bajo 2 archivos iguales.
  DO 2 TIMES.
    VL_TIME = VL_TIME + 1.
    CLEAR: VL_DSN,
           ST_DATA,
           ST_PADRONES.

*Bajo el archivo de la retencion de Capital Federal
    IF P_CAP EQ 'X' .
      IF VL_TIME EQ '1'.
        CONCATENATE P_PATH VL_RUTA
                    'Padrones_Reducidos_CABA_IIBB/'
                    VL_CORRECTO
                    V_F_PUBLI
                    '.txt'
        INTO VL_DSN.
      ELSE.
        CONCATENATE P_PATH VL_RUTA
                    'Padrones_Reducidos_CABA_IIBB/Datos_Correctos'
                    '.txt'
        INTO VL_DSN.
      ENDIF.
    ENDIF.

*Bajo el archivo de la retencion de Provincia Bs As
    IF P_PROV EQ 'X'.
      IF  VL_TIME EQ '1'.
        CONCATENATE P_PATH VL_RUTA
                    'Padrones_Reducidos_Arba_IIBB/'
                    VL_CORRECTO
                    V_F_PUBLI
                    '.txt'
        INTO VL_DSN.
      ELSE.
        CONCATENATE P_PATH VL_RUTA
                    'Padrones_Reducidos_Arba_IIBB/Datos_Correctos'
                    '.txt'
        INTO VL_DSN.
      ENDIF.
    ENDIF.

* Abro servidor
    OPEN DATASET VL_DSN FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    CHECK SY-SUBRC EQ 0.

    LOOP AT T_BAJ_COR INTO ST_PADRONES.
      TRANSFER ST_PADRONES TO  VL_DSN." LENGTH '255'.
      IF SY-SUBRC <> 0.
        MESSAGE I601(6P) WITH VL_DSN.
        STOP.
      ENDIF.
    ENDLOOP.

    PERFORM DATASET USING VL_CHOW
                          VL_DSN.
  ENDDO.

* Bajo los datos incorrectos
  IF P_CAP EQ 'X'.
    CLEAR: VL_DSN,
           ST_PADRONES.
    CONCATENATE P_PATH VL_RUTA
                'Padrones_Reducidos_CABA_IIBB/'
                VL_INCORREC
                V_F_PUBLI
                '.txt'
    INTO VL_DSN.
  ELSE.
    CLEAR: VL_DSN,
           ST_PADRONES.
    CONCATENATE P_PATH VL_RUTA
                'Padrones_Reducidos_Arba_IIBB/'
                VL_INCORREC
                V_F_PUBLI
                '.txt'
    INTO VL_DSN.
  ENDIF.

* abro servidor
  OPEN DATASET VL_DSN FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  CHECK SY-SUBRC EQ 0.
  LOOP AT T_BAJ_INC INTO  ST_PADRONES.
    TRANSFER  ST_PADRONES TO  VL_DSN." LENGTH '255'.
    IF SY-SUBRC <> 0.
      MESSAGE I601(6P) WITH VL_DSN.
      STOP.
    ENDIF.
  ENDLOOP.

  PERFORM DATASET USING VL_CHOW
                        VL_DSN.

  PERFORM F_ACTUALIZO_FECHA_GS02.

ENDFORM. " F_BAJADA_SERVIDOR

*&---------------------------------------------------------------------*
*&      Form  F_LLENO_TABLAS_INTERNAS
*&---------------------------------------------------------------------*
FORM F_LLENO_TABLAS_INTERNAS .

  FIELD-SYMBOLS: <FS_DATA> TYPE TY_DATA,
                 <FS_DATA_ARBA> TYPE TY_DATA_ARBA,
                 <FS_LFA1> TYPE TY_LFA1.

  PERFORM F_SHOW_PROGRESS_IND USING 'Filtrando datos de entrada' 70.

  SORT T_LFA1 BY STCD1.

* Borro todo los registro que no son Proveedores de iecsa y crea.
  IF P_CAP EQ 'X'.

    LOOP AT T_DATA ASSIGNING <FS_DATA>.
      CLEAR ST_LFA1.
      READ TABLE T_LFA1
        INTO ST_LFA1
        WITH KEY STCD1 = <FS_DATA>-STCD1 BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <FS_DATA>-BUKRS = ST_LFA1-BUKRS.
        <FS_DATA>-LIFNR = ST_LFA1-LIFNR.

        PERFORM F_DEFINE_GRUPO USING <FS_DATA>-ALICU_PER
                            CHANGING <FS_DATA>-GRUPO.

        APPEND <FS_DATA> TO T_MODIF.
      ELSE.
        "não gerar arquivo de datos incorrectos 29/06/2015
*        "Guardo los proveedores que no van a ser modificados.
*        ST_LFA1-STCD1 = <FS_DATA>-STCD1.
*        ST_LFA1-LIFNR = '0000000000'.
*        ST_LFA1-NAME1 = 'sin registro'.
*        APPEND ST_LFA1 TO T_NO_MOD.
      ENDIF.
    ENDLOOP.

  ELSEIF P_PROV EQ 'X'.

    LOOP AT T_DATA_ARBA ASSIGNING <FS_DATA_ARBA>.
      CLEAR ST_LFA1.
      READ TABLE T_LFA1
        INTO ST_LFA1
        WITH KEY STCD1 = <FS_DATA_ARBA>-STCD1 BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <FS_DATA_ARBA>-BUKRS = ST_LFA1-BUKRS.
        <FS_DATA_ARBA>-LIFNR = ST_LFA1-LIFNR.

        APPEND <FS_DATA_ARBA> TO T_MODIF_ARBA.
      ELSE.
        "não gerar arquivo de datos incorrectos 29/06/2015
*        "Guardo los proveedores que no van a ser modificados.
*        ST_LFA1-STCD1 = <FS_DATA_ARBA>-STCD1.
*        ST_LFA1-LIFNR = '0000000000'.
*        ST_LFA1-NAME1 = 'sin registro'.
*        APPEND ST_LFA1 TO T_NO_MOD.
      ENDIF.
    ENDLOOP.

  ENDIF.


  PERFORM F_SHOW_PROGRESS_IND USING 'Guardando prov. no modificados' 80.

*  Guardo los proveedores que no van a ser modificados.
*  LOOP AT T_LFA1 ASSIGNING <FS_LFA1>.
*    READ TABLE T_DATA
*      INTO ST_DATA
*      WITH KEY STCD1 = <FS_LFA1>-STCD1.
*    IF SY-SUBRC NE 0.
*      APPEND <FS_LFA1> TO T_NO_MOD.
*    ENDIF.
*  ENDLOOP.

ENDFORM. " F_LLENO_TABLAS_INTERNAS

*&---------------------------------------------------------------------*
*&      Form  F_BAJADA_FICHERO_LOCAL
*&---------------------------------------------------------------------*
FORM F_BAJADA_FICHERO_LOCAL .

  DATA: T_BAJADA  TYPE STANDARD TABLE OF TY_BAJADA,
        ST_BAJADA TYPE TY_BAJADA,
        VL_NO_MOD TYPE STRING,
        FLEN      TYPE I,
        VL_RUTA   TYPE CHAR100 VALUE '/Finanzas/Padrones_Arba/',
        VL_PADRON TYPE CHAR100 VALUE 'Padron_',
        VL_CHOW   TYPE CHAR17  VALUE 'chown pagnrl:spa ',
        VL_DSN    TYPE CHAR200,
        VL_MODIF  TYPE STRING.

  PERFORM F_SHOW_PROGRESS_IND USING 'Bajando archivo local' 95.

* Si el archivo no se importó de la pc, se debe grabar el archivo de
*  padron con la fecha en el servidor.
  IF P_CHECK2 IS INITIAL.
*   Retenciones Capital Federal
    IF P_CAP EQ 'X'.

      CONCATENATE P_PATH VL_RUTA
                  'Padrones_CABA_IIBB/'
                  VL_PADRON
                  V_F_PUBLI
*                  '.txt'
      INTO VL_DSN.
    ENDIF.

*   Retenciones de la Provincia de Bs As.
    IF P_PROV EQ 'X'.
      CONCATENATE P_PATH VL_RUTA
                  'Padrones_Arba_IIBB/'
                  VL_PADRON
                  V_F_PUBLI
*                  '.txt'
      INTO VL_DSN.
    ENDIF.

    OPEN DATASET VL_DSN FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    CHECK SY-SUBRC EQ 0.
    LOOP AT T_PADRONES INTO ST_PADRONES.
      TRANSFER ST_PADRONES TO  VL_DSN." LENGTH '255'.
      IF SY-SUBRC <> 0.
        MESSAGE I601(6P) WITH VL_DSN.
        STOP.
      ENDIF.
    ENDLOOP.

    PERFORM DATASET USING VL_CHOW
                          VL_DSN.

  ENDIF.

  IF P_CAP EQ 'X'.
    CONCATENATE P_PATH2
                '/CABA_Datos_Correctos_'
                V_F_PUBLI
*                '.txt'
           INTO VL_MODIF.
  ELSE.
    CONCATENATE P_PATH2
                '/Arba_Datos_Correctos_'
                V_F_PUBLI
*                '.txt'
           INTO VL_MODIF.
  ENDIF.

  CALL FUNCTION 'C13Z_DOWNLOAD'
    EXPORTING
      FILENAME                = VL_MODIF
      FILETYPE                = 'DAT' "VSS
    TABLES
      DATA_TAB                = T_BAJ_COR[]
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1
      FILE_WRITE_ERROR        = 2
      INVALID_FILESIZE        = 3
      INVALID_TYPE            = 4
      NO_BATCH                = 5
      UNKNOWN_ERROR           = 6
      INVALID_TABLE_WIDTH     = 7
      GUI_REFUSE_FILETRANSFER = 8
      CUSTOMER_ERROR          = 9
      NO_AUTHORITY            = 10
      HEADER_NOT_ALLOWED      = 11
      SEPARATOR_NOT_ALLOWED   = 12
      HEADER_TOO_LONG         = 13
      DP_ERROR_CREATE         = 14
      DP_ERROR_SEND           = 15
      DP_ERROR_WRITE          = 16
      UNKNOWN_DP_ERROR        = 17
      ACCESS_DENIED           = 18
      DP_OUT_OF_MEMORY        = 19
      DISK_FULL               = 20
      DP_TIMEOUT              = 21
      FILE_NOT_FOUND          = 22
      DATAPROVIDER_EXCEPTION  = 23
      CONTROL_FLUSH_ERROR     = 24
      NOT_SUPPORTED_BY_GUI    = 25
      ERROR_NO_GUI            = 26
      OTHERS                  = 27.
  IF SY-SUBRC <> 0.
*
  ENDIF.

* Bajo al fichero local el archivo con los proveedores que no se proceso.
  CONCATENATE P_PATH2
              '/Datos_Incorrectos'
              V_F_PUBLI
*              '.txt'
         INTO  VL_NO_MOD.

  IF P_CAP EQ 'X'.
    CONCATENATE P_PATH2
                '/CABA_Datos_Incorrectos_'
                V_F_PUBLI
*                '.txt'
           INTO VL_MODIF.
  ELSE.
    CONCATENATE P_PATH2
                '/Arba_Datos_Incorrectos_'
                V_F_PUBLI
*                '.txt'
           INTO VL_MODIF.
  ENDIF.

  CLEAR:  ST_BAJADA,
          T_BAJADA.
  REFRESH T_BAJADA.

  IF SY-SUBRC <> 0.
*
  ENDIF.

  "não gerar datos incorrectos 29/06/2015 ALRS
*  CALL FUNCTION 'C13Z_DOWNLOAD'
*    EXPORTING
*      FILENAME                = VL_NO_MOD
*      FILETYPE                = 'DAT'
*    TABLES
*      DATA_TAB                = T_BAJ_INC[]
*    EXCEPTIONS
*      FILE_OPEN_ERROR         = 1
*      FILE_WRITE_ERROR        = 2
*      INVALID_FILESIZE        = 3
*      INVALID_TYPE            = 4
*      NO_BATCH                = 5
*      UNKNOWN_ERROR           = 6
*      INVALID_TABLE_WIDTH     = 7
*      GUI_REFUSE_FILETRANSFER = 8
*      CUSTOMER_ERROR          = 9
*      NO_AUTHORITY            = 10
*      HEADER_NOT_ALLOWED      = 11
*      SEPARATOR_NOT_ALLOWED   = 12
*      HEADER_TOO_LONG         = 13
*      DP_ERROR_CREATE         = 14
*      DP_ERROR_SEND           = 15
*      DP_ERROR_WRITE          = 16
*      UNKNOWN_DP_ERROR        = 17
*      ACCESS_DENIED           = 18
*      DP_OUT_OF_MEMORY        = 19
*      DISK_FULL               = 20
*      DP_TIMEOUT              = 21
*      FILE_NOT_FOUND          = 22
*      DATAPROVIDER_EXCEPTION  = 23
*      CONTROL_FLUSH_ERROR     = 24
*      NOT_SUPPORTED_BY_GUI    = 25
*      ERROR_NO_GUI            = 26
*      OTHERS                  = 27.
*  IF SY-SUBRC <> 0.
**
*  ENDIF.

  PERFORM F_ACTUALIZO_FECHA_GS02.

ENDFORM. " F_BAJADA_FICHERO_LOCAL

*&---------------------------------------------------------------------*
*&      Form  DATASET
*&---------------------------------------------------------------------*
FORM DATASET USING VL_CHOW
                       VL_DSN.
  DATA PARCOM_LOC(100).
  DATA: BEGIN OF TABL OCCURS 0,
          LINE(560),
        END OF TABL.
  CONCATENATE VL_CHOW VL_DSN  INTO PARCOM_LOC
                  SEPARATED BY SPACE.

  CLOSE DATASET VL_DSN.

  REFRESH TABL.
  CALL 'SYSTEM'
    ID 'COMMAND' FIELD PARCOM_LOC
    ID 'TAB'     FIELD TABL-*SYS*.

ENDFORM. " DATASET

*&---------------------------------------------------------------------*
*&      Form  F_TABLAS_PARA_SER_BAJADAS
*&---------------------------------------------------------------------*
FORM F_TABLAS_PARA_SER_BAJADAS .

  FIELD-SYMBOLS: <FS_DATA> TYPE TY_DATA,
                 <FS_DATA_ARBA> TYPE TY_DATA_ARBA,
                 <FS_LFA1> TYPE TY_LFA1.

  PERFORM F_SHOW_PROGRESS_IND USING 'Preparando tablas para descarga' 90.


  IF P_CAP EQ 'X'.

    LOOP AT T_MODIF ASSIGNING <FS_DATA>.
      CONCATENATE   "<FS_DATA>-ARQ
                    <FS_DATA>-PUBLI
                    <FS_DATA>-VIGEN
                    <FS_DATA>-FINAL
                    <FS_DATA>-STCD1
                    <FS_DATA>-TIPO
                    <FS_DATA>-ALTA_SUJETO
                    <FS_DATA>-CAMBIO_ALIC
                    <FS_DATA>-ALICU_PER
                    <FS_DATA>-GRUPO
                    <FS_DATA>-LIFNR
                    <FS_DATA>-BUKRS
         INTO ST_BAJADA-LINEA SEPARATED BY ';'.
      APPEND ST_BAJADA TO T_BAJ_COR.
    ENDLOOP.

  ELSEIF P_PROV EQ 'X'.

    LOOP AT T_MODIF_ARBA ASSIGNING <FS_DATA_ARBA>.
      CONCATENATE   <FS_DATA_ARBA>-ARQ
                    <FS_DATA_ARBA>-PUBLI
                    <FS_DATA_ARBA>-VIGEN
                    <FS_DATA_ARBA>-FINAL
                    <FS_DATA_ARBA>-STCD1
                    <FS_DATA_ARBA>-TIPO
                    <FS_DATA_ARBA>-ALTA_SUJETO
                    <FS_DATA_ARBA>-CAMBIO_ALIC
                    <FS_DATA_ARBA>-ALICU
                    <FS_DATA_ARBA>-GRUPO
                    <FS_DATA_ARBA>-LIFNR
                    <FS_DATA_ARBA>-BUKRS
         INTO ST_BAJADA-LINEA SEPARATED BY ';'.
      APPEND ST_BAJADA TO T_BAJ_COR.
    ENDLOOP.

  ENDIF.

  LOOP AT T_NO_MOD ASSIGNING <FS_LFA1>.
    CONCATENATE   <FS_LFA1>-LIFNR
                  <FS_LFA1>-STCD1
                  <FS_LFA1>-NAME1
       INTO ST_BAJADA-LINEA SEPARATED BY ';'.
    APPEND ST_BAJADA TO T_BAJ_INC .
  ENDLOOP.

ENDFORM. " F_TABLAS_PARA_SER_BAJADAS

*&---------------------------------------------------------------------*
*&      Form  FECHA_EJECUCION
*&---------------------------------------------------------------------*
FORM FECHA_EJECUCION USING PI_PUBLI CHANGING PO_ANS.
  DATA: VL_SETNAME TYPE SETLEAF-SETNAME.
  CLEAR: ST_SETLEAF,
          V_VALFROM,
          V_FECHA  .
  IF P_PROV EQ 'X'.
    MOVE 'ZIIBB_BSAS' TO VL_SETNAME.
  ELSE.
    MOVE 'ZIIBB_CAP' TO  VL_SETNAME.
  ENDIF.

  SELECT SINGLE * FROM SETLEAF CLIENT SPECIFIED
           INTO  ST_SETLEAF
           WHERE MANDT    = SY-MANDT
             AND SETCLASS = '0000'"
             AND SETNAME  = VL_SETNAME."'ZIIBB_BSAS'.

  IF SY-SUBRC EQ 0.
    CONCATENATE PI_PUBLI+4(4)
                PI_PUBLI+2(2)
                PI_PUBLI(2)
    INTO  V_FECHA .
    CONDENSE ST_SETLEAF-VALFROM NO-GAPS.
    MOVE ST_SETLEAF-VALFROM TO V_VALFROM.

    IF V_FECHA  > V_VALFROM.
      PO_ANS = '1'.
    ELSE.
      PO_ANS = '2'.
    ENDIF.
  ELSE.
    PO_ANS = '2'.
  ENDIF.

*   Mensaje de error
  IF PO_ANS EQ '2'.
    MESSAGE TEXT-E04 TYPE 'I' DISPLAY LIKE 'E' .
    EXIT.
  ENDIF.

ENDFORM. " FECHA_EJECUCION

*&---------------------------------------------------------------------*
*&      Form  F_ACTUALIZO_FECHA_GS02
*&---------------------------------------------------------------------*
FORM F_ACTUALIZO_FECHA_GS02 .

  IF V_FECHA  > V_VALFROM.
    UPDATE SETLEAF
       SET VALFROM   = V_FECHA
           VALTO     = V_FECHA
     WHERE SETCLASS EQ ST_SETLEAF-SETCLASS
       AND SUBCLASS EQ ST_SETLEAF-SUBCLASS
       AND SETNAME  EQ ST_SETLEAF-SETNAME
       AND LINEID   EQ ST_SETLEAF-LINEID.

    IF SY-SUBRC EQ 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.


ENDFORM. " F_ACTUALIZO_FECHA_GS02

*&---------------------------------------------------------------------*
*&      Form  F_GRISADO_CAMPO_PANTALLA
*&---------------------------------------------------------------------*
FORM F_GRISADO_CAMPO_PANTALLA .

  P_PATH  = V_PATH.
  P_PATHI = V_PATH.

  LOOP AT SCREEN.
    IF P_CHECK EQ ' '.
      IF SCREEN-NAME = 'P_PATH2'.
        MOVE '0' TO SCREEN-INPUT.
      ENDIF.
    ENDIF.
    IF P_CHECK EQ 'X'.
      IF SCREEN-NAME = 'P_PATH2'.
        MOVE '1' TO SCREEN-INPUT.
      ENDIF.
    ENDIF.

    IF P_CHECK2 EQ ' '.
      IF SCREEN-NAME = 'P_PATH' OR SCREEN-NAME = 'P_PATHI'.
        MOVE '0' TO SCREEN-INPUT.
      ENDIF.
    ENDIF.
    IF P_CHECK2 EQ 'X'.
      IF SCREEN-NAME = 'P_PATH' OR SCREEN-NAME = 'P_PATHI'.
        MOVE '1' TO SCREEN-INPUT.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDFORM. " F_GRISADO_CAMPO_PANTALLA

*&---------------------------------------------------------------------*
*&      Form  F_SUBIDA_SERVIDOR
*&---------------------------------------------------------------------*
FORM F_SUBIDA_SERVIDOR .
  DATA: VL_LENG   TYPE I       ,
        VL_FNAME  TYPE CHAR200 ,
        VL_DSN    TYPE CHAR200 ,
        ST_DATA   TYPE  TY_DATA,
        VL_LINE   TYPE  TY_OUT_AUX.
  DATA: WA TYPE TY_OUT_AUX.

  FIELD-SYMBOLS <HEX_CONTAINER> TYPE TY_OUT_AUX ."ty_gcias.

  ASSIGN WA TO <HEX_CONTAINER> CASTING.

* Bajo los datos correctos
  CLEAR: VL_DSN,
         ST_DATA.

*  vl_fname = '/Padron.txt'.
  VL_FNAME = '/Padron'.

  IF P_CAP EQ 'X'.
    CLEAR: VL_DSN,
           ST_PADRONES.
    CONCATENATE P_PATH '/Finanzas/Padrones_Arba/Padrones_CABA_IIBB'
                VL_FNAME
           INTO VL_DSN.
  ELSE.
    CLEAR: VL_DSN,
           ST_PADRONES.
    CONCATENATE P_PATH '/Finanzas/Padrones_Arba/Padrones_Arba_IIBB'
                VL_FNAME
           INTO VL_DSN.
  ENDIF.

  OPEN DATASET VL_DSN FOR INPUT IN TEXT MODE ENCODING  NON-UNICODE .
  ASSIGN WA TO <HEX_CONTAINER> CASTING.
  DO.
    READ DATASET VL_DSN INTO <HEX_CONTAINER> LENGTH VL_LENG.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSE.
      WRITE  WA-LINE TO  ST_OUT_AUX-LINE.
      APPEND ST_OUT_AUX-LINE TO  T_OUT_AUX.
    ENDIF.
  ENDDO.
  CLOSE DATASET VL_DSN.

ENDFORM. " F_SUBIDA_SERVIDOR

*&---------------------------------------------------------------------*
*&      Form  F_CREAR_PROGRESS_IND
*&---------------------------------------------------------------------*
FORM F_CREAR_PROGRESS_IND .

  IF O_PROGRESS_IND IS INITIAL.
    CREATE OBJECT O_PROGRESS_IND.
  ENDIF.

ENDFORM. " F_CREAR_PROGRESS_IND

*&---------------------------------------------------------------------*
*&      Form  F_SHOW_PROGRESS_IND
*&---------------------------------------------------------------------*
FORM F_SHOW_PROGRESS_IND USING P_MESSAGE TYPE CLIKE
                               P_PROCESSED TYPE I.

  O_PROGRESS_IND->DISPLAY( TOTAL = 100 PROCESSED = P_PROCESSED MESSAGE = P_MESSAGE
                           FORCE_DISPLAY = ABAP_TRUE ).

ENDFORM. " F_SHOW_PROGRESS_IND

FORM F_DEFINE_GRUPO  USING P_ALICU
                  CHANGING P_GRUPO.

  CASE P_ALICU .
    WHEN '0,00'.  P_GRUPO = '01'.
    WHEN '0,10'.  P_GRUPO = '02'.
    WHEN '0,20'.  P_GRUPO = '03'.
    WHEN '0,50'.  P_GRUPO = '04'.
    WHEN '0,75'.  P_GRUPO = '05'.
    WHEN '0,90'.  P_GRUPO = '06'.
    WHEN '1,00'.  P_GRUPO = '07'.
    WHEN '1,25'.  P_GRUPO = '08'.
    WHEN '1,50'.  P_GRUPO = '09'.
    WHEN '1,75'.  P_GRUPO = '10'.
    WHEN '2,00'.  P_GRUPO = '11'.
    WHEN '2,50'.  P_GRUPO = '12'.
    WHEN '2,75'.  P_GRUPO = '13'.
    WHEN '3,00'.  P_GRUPO = '14'.
    WHEN '4,00'.  P_GRUPO = '15'.
    WHEN '4,50'.  P_GRUPO = '16'.
* Inicio - RRRIBEIRO - 2000046471 - 13/06/2025 - Stefanini
    WHEN '3,50'.  P_GRUPO = '17'.
* Fim - RRRIBEIRO - 2000046471 - 13/06/2025 - Stefanini
  ENDCASE.

ENDFORM.
