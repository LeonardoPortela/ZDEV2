*&---------------------------------------------------------------------*
*&  Include           ZFIY0007_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  KD_GET_FILENAME_ON_F4
*&---------------------------------------------------------------------*
FORM KD_GET_FILENAME_ON_F4  CHANGING    PO_PATH.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      FILE_NAME = PO_PATH.

ENDFORM.                    " KD_GET_FILENAME_ON_F4
*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_FILE
*&---------------------------------------------------------------------*
FORM F_FORMAT_FILE USING PI_ARCHIVO.

  DATA: STL_GCIA  TYPE TY_OUT_AUX,
        STL_IVA   TYPE TY_IVA,
        STL_SUSS  TYPE TY_SUSS,
        W_SEP     TYPE C VALUE ';',
        W_INDICE  TYPE SY-TABIX,
        VL_FECHA  TYPE CHAR10,
        VL_WITHT  TYPE WITHT,
        VL_STCD1  TYPE STCD1,
        VL_CERT   TYPE CHAR30,
        VL_EMIS   TYPE CHAR10,
        VL_PUBL   TYPE CHAR10,
        VL_VIGE   TYPE CHAR10.


  REFRESH: T_MONTH_NAMES.
  CLEAR:   ST_DATA,
           ST_MONTH,
           T_DATA.

  IF PI_ARCHIVO EQ 'SUSS'.

    PERFORM F_MONTH_NAMES_GET.
    LOOP AT T_OUT_AUX INTO ST_OUT_AUX.
      CONDENSE ST_OUT_AUX NO-GAPS.
      CLEAR:  ST_DATA.
      SPLIT ST_OUT_AUX-LINEA
       AT   W_SEP
       INTO ST_SUSS-CUIT
            ST_SUSS-CERTIFICADO
            ST_SUSS-ESPACIO
            ST_SUSS-PORCENTAJE
            ST_SUSS-RES_GRAL
            ST_SUSS-ESTADO
            ST_SUSS-F_EMISION
            ST_SUSS-F_VIGENCIA.

      READ TABLE T_MONTH_NAMES INTO ST_MONTH
      WITH KEY KTX = ST_SUSS-F_EMISION+3(3).
      IF SY-SUBRC EQ 0.
        MOVE ST_SUSS-F_EMISION TO VL_FECHA.
        CLEAR ST_SUSS-F_EMISION.
        CONCATENATE  VL_FECHA(2) ST_MONTH-MNR '20'VL_FECHA+7(2)
        INTO ST_SUSS-F_EMISION .
      ENDIF.

      READ TABLE T_MONTH_NAMES INTO ST_MONTH
      WITH KEY KTX = ST_SUSS-F_VIGENCIA+3(3).
      IF SY-SUBRC EQ 0.
        MOVE ST_SUSS-F_VIGENCIA TO VL_FECHA.
        CLEAR ST_SUSS-F_VIGENCIA.
        CONCATENATE  VL_FECHA(2) ST_MONTH-MNR '20'VL_FECHA+7(2)
        INTO ST_SUSS-F_VIGENCIA  .
      ENDIF.

      APPEND ST_SUSS TO T_SUSS.

    ENDLOOP.

  ELSEIF PI_ARCHIVO EQ 'IVA'.

    LOOP AT T_OUT_AUX INTO ST_OUT_AUX WHERE LINEA+109(6) EQ 'RG2226'.
      MOVE: ST_OUT_AUX-LINEA+1(11)  TO ST_IVA-CUIT,
            ST_OUT_AUX-LINEA+13(50) TO ST_IVA-RSOCIAL,
            ST_OUT_AUX-LINEA+74(10) TO ST_IVA-F_EMISION,
            ST_OUT_AUX-LINEA+85(10) TO ST_IVA-F_VIGENCIA,
            ST_OUT_AUX-LINEA+105(4) TO ST_IVA-PORCENTAJE,
            ST_OUT_AUX-LINEA+109(6) TO ST_IVA-RES_GRAL.

      APPEND ST_IVA TO T_IVA.

    ENDLOOP.

  ELSEIF PI_ARCHIVO EQ 'G2681'.

    LOOP AT T_G2681 INTO ST_G2681.

      MOVE-CORRESPONDING ST_G2681 TO ST_GCIAS.

      TRANSLATE ST_G2681-VIGENCIADESDE USING '/ . '.
      TRANSLATE ST_G2681-VIGENCIAHASTA USING '/ . '.
      CONDENSE ST_G2681-VIGENCIADESDE NO-GAPS.
      CONDENSE ST_G2681-VIGENCIAHASTA NO-GAPS.

      MOVE ST_G2681-VIGENCIADESDE(8) TO ST_GCIAS-F_EMISION.
      MOVE ST_G2681-VIGENCIAHASTA(8) TO ST_GCIAS-F_VIGENCIA.

      APPEND ST_GCIAS TO T_GCIAS.

    ENDLOOP.

  ELSEIF PI_ARCHIVO EQ 'GCIAS'. "AND p_check NE 'X'.  "Modificado por Diego 21.01.2011

    LOOP AT T_OUT_AUX INTO ST_OUT_AUX.
      CONDENSE ST_OUT_AUX NO-GAPS.
      SPLIT ST_OUT_AUX-LINEA AT W_SEP INTO ST_GCIAS-CERTIFICADO
                                           ST_GCIAS-CUIT
                                           ST_GCIAS-PERIODO_FISCAL
                                           ST_GCIAS-PORCENTAJE
                                           ST_GCIAS-RES_GRAL
                                           ST_GCIAS-ESTADO
                                           ST_GCIAS-NRO_LEGAJO
                                           VL_EMIS
                                           VL_PUBL
                                           VL_VIGE.
      DO 10 TIMES.
        REPLACE '.' WITH ' ' INTO VL_PUBL.
        REPLACE '.' WITH ' ' INTO VL_VIGE.
        REPLACE '.' WITH ' ' INTO VL_EMIS.

        REPLACE '/' WITH ' ' INTO VL_PUBL.
        REPLACE '/' WITH ' ' INTO VL_VIGE.
        REPLACE '/' WITH ' ' INTO VL_EMIS.

        REPLACE ',' WITH ' ' INTO VL_PUBL.
        REPLACE ',' WITH ' ' INTO VL_VIGE.
        REPLACE ',' WITH ' ' INTO VL_EMIS.
      ENDDO.

      CONDENSE VL_PUBL NO-GAPS.
      CONDENSE VL_VIGE NO-GAPS.
      CONDENSE VL_EMIS NO-GAPS.
      WRITE VL_PUBL TO ST_GCIAS-F_EMISION.
      WRITE VL_EMIS TO ST_GCIAS-F_PUBLICACION.
      WRITE VL_VIGE TO ST_GCIAS-F_VIGENCIA.

      APPEND ST_GCIAS TO T_GCIAS.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " F_FORMAT_FILE
*&---------------------------------------------------------------------*
*&      Form  F_SUBIDA_GCIAS
*&---------------------------------------------------------------------*
FORM F_SUBIDA_GCIAS USING P_WT_SUBIDA TYPE RLGRAP-FILENAME.

  DATA: LV_PATH TYPE STRING.

  IF P_WT_SUBIDA IS NOT INITIAL.
    LV_PATH = P_WT_SUBIDA.
  ENDIF.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = LV_PATH
      FILETYPE                = 'DAT'
    TABLES
      DATA_TAB                = T_OUT_AUX
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


ENDFORM.                    " F_SUBIDA_GCIAS
*&---------------------------------------------------------------------*
*&      Form  F_SUBIDA_G2681
*&---------------------------------------------------------------------*
FORM F_SUBIDA_G2681 USING P_WT_SUBIDA TYPE RLGRAP-FILENAME.

  DATA: LV_PATH TYPE STRING.

  IF P_WT_SUBIDA IS NOT INITIAL.
    LV_PATH = P_WT_SUBIDA.
  ENDIF.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = LV_PATH
      FILETYPE                = 'DAT'
    TABLES
      DATA_TAB                = T_G2681
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

ENDFORM.                    " F_SUBIDA_G2681
*&---------------------------------------------------------------------*
*&      Form  F_SUBIDA_IVA
*&---------------------------------------------------------------------*
FORM F_SUBIDA_IVA USING P_WT_SUBIDA TYPE RLGRAP-FILENAME.

  DATA: LV_PATH TYPE STRING.

  IF P_WT_SUBIDA IS NOT INITIAL.
    LV_PATH = P_WT_SUBIDA.
  ENDIF.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = LV_PATH
      FILETYPE                = 'ASC'
    TABLES
      DATA_TAB                = T_OUT_AUX
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
ENDFORM.                    " F_SUBIDA_TXT
*&---------------------------------------------------------------------*
*&      Form  F_SUBIDA_TXT
*&---------------------------------------------------------------------*
FORM F_SUBIDA_SUSS USING P_WT_SUBIDA TYPE RLGRAP-FILENAME.

  DATA: LV_PATH TYPE STRING.

  IF P_WT_SUBIDA IS NOT INITIAL.
    LV_PATH = P_WT_SUBIDA.
  ENDIF.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = LV_PATH
      FILETYPE                = 'DAT'
    TABLES
      DATA_TAB                = T_OUT_AUX
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


ENDFORM.                    " F_SUBIDA_TXT

*&---------------------------------------------------------------------*
*&      Form  F_SELE_LFA1
*&---------------------------------------------------------------------*
FORM F_SELE_LFA1.

  DATA: TL_LFA1   TYPE STANDARD TABLE OF TY_LFA1,
        VL_NOPROC TYPE CHAR1.

*  Busco los proveedores.
  SELECT STCD1 LIFNR NAME1 "BUKRS
  FROM LFA1
  INTO TABLE T_LFA1.

  CHECK SY-SUBRC EQ 0.
  SORT  T_LFA1 BY LIFNR.

* BUsco la sociedad que esta cada proveedor.
  SELECT LIFNR BUKRS
    FROM LFB1
    INTO TABLE T_LFB1
    FOR ALL ENTRIES IN T_LFA1
    WHERE LIFNR EQ T_LFA1-LIFNR
      AND BUKRS IN SO_BUKRS.

  SORT T_LFB1 BY  LIFNR BUKRS.
* Busco los impuesto del cliente
  PERFORM F_SEL_LFBW USING P_GCIAS P_G2681 P_SUSS P_IVA.

* armo la tabla con todo los datos del proveedor.
  LOOP AT T_LFA1 INTO ST_LFA1.
    LOOP AT T_LFB1 INTO ST_LFB1 WHERE LIFNR = ST_LFA1-LIFNR.
      ST_LFA1-BUKRS = ST_LFB1-BUKRS.
*      Indico si tiene indicador de ganancia el proveedor
      LOOP AT T_LFBW INTO ST_LFBW
      WHERE LIFNR = ST_LFA1-LIFNR
       AND  BUKRS = ST_LFA1-BUKRS.
        CASE ST_LFBW-WITHT.
          WHEN C_G1.
            ST_LFA1-G1 = C_X.
          WHEN C_GA.
            ST_LFA1-GA = C_X.
          WHEN C_G2.
            ST_LFA1-G2 = C_X.
          WHEN C_GB.
            ST_LFA1-GB = C_X.
          WHEN C_CO.
            ST_LFA1-CO = C_X.
          WHEN C_EE.
            ST_LFA1-EE = C_X.
          WHEN C_EG.
            ST_LFA1-EG = C_X.
          WHEN C_EL.
            ST_LFA1-EL = C_X.
          WHEN C_ES.
            ST_LFA1-ES = C_X.
          WHEN C_IV.
            ST_LFA1-IV = C_X.
        ENDCASE.
        APPEND ST_LFA1 TO TL_LFA1.
      ENDLOOP.

      IF SY-SUBRC <> 0.
        PERFORM F_MENSAJES_ERROR USING ST_LFA1 TEXT-E03.
      ENDIF.

    ENDLOOP.
  ENDLOOP.


  REFRESH T_LFA1.
  T_LFA1[] = TL_LFA1[].

ENDFORM.                    " F_SELE_LFA1

*&---------------------------------------------------------------------*
*&      Form  F_ARMO_TABLA_batch
*&---------------------------------------------------------------------*
FORM F_ARMO_TABLA_BATCH_GANANCIA.

  DATA: STL_GCIA TYPE TY_OUT_AUX,
        VL_WITHT TYPE WITHT,
        VL_STCD1 TYPE STCD1,
        VL_CERT  TYPE CHAR30,
        VL_EMIS  TYPE CHAR10,
        VL_PUBL  TYPE CHAR10,
        VL_VIGE  TYPE CHAR10.

  LOOP AT T_GCIAS INTO ST_GCIAS.
*   Saco los "-" del campo . Solo dejo de la forma XXXX-XXXXXXXXXXXXXX

    MOVE ST_GCIAS-CERTIFICADO	TO VL_CERT.
    CLEAR ST_GCIAS-CERTIFICADO.
    PERFORM F_CONVERSION_NRO_CERT USING VL_CERT
                                  CHANGING ST_GCIAS-CERTIFICADO.

*   Valido el cliente si existe en sap
    LOOP AT T_LFA1 INTO ST_LFA1 WHERE STCD1 = ST_GCIAS-CUIT.
*    lleno la tabla que vamos a usar en el batch input.
      ST_DATA-BUKRS = ST_LFA1-BUKRS.
      ST_DATA-LIFNR = ST_LFA1-LIFNR.
      ST_DATA-WT_EXNR = ST_GCIAS-CERTIFICADO.
      ST_DATA-WT_EXRT = ST_GCIAS-PORCENTAJE.
      ST_DATA-WT_WTEXRS = '2'.
      ST_DATA-WT_EXDF = ST_GCIAS-F_EMISION.
      ST_DATA-WT_EXDT = ST_GCIAS-F_VIGENCIA.

      IF ST_LFA1-G1 EQ C_X.
        ST_DATA-WITHT = C_G1.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-GA EQ C_X.
        ST_DATA-WITHT = C_GA.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-G2 EQ C_X.
        ST_DATA-WITHT = C_G2.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-GB EQ C_X.
        ST_DATA-WITHT = C_GB.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      CLEAR ST_DATA.

    ENDLOOP.
  ENDLOOP.

*  ENDIF.

ENDFORM.                    " F_ARMO_TABLA_batch

*&---------------------------------------------------------------------*
*&      Form  F_ARMO_TABLA_BATCH_G2681
*&---------------------------------------------------------------------*
FORM F_ARMO_TABLA_BATCH_G2681.

  DATA: STL_GCIA TYPE TY_OUT_AUX,
        VL_WITHT TYPE WITHT,
        VL_STCD1 TYPE STCD1,
        VL_CERT  TYPE CHAR30,
        VL_EMIS  TYPE CHAR10,
        VL_PUBL  TYPE CHAR10,
        VL_VIGE  TYPE CHAR10.

  LOOP AT T_GCIAS INTO ST_GCIAS.
*   Saco los "-" del campo . Solo dejo de la forma XXXX-XXXXXXXXXXXXXX

    MOVE ST_GCIAS-CERTIFICADO	TO VL_CERT.
    CLEAR ST_GCIAS-CERTIFICADO.
    PERFORM F_CONVERSION_NRO_CERT USING VL_CERT
                                  CHANGING ST_GCIAS-CERTIFICADO.

*   Valido el cliente si existe en sap
    LOOP AT T_LFA1 INTO ST_LFA1 WHERE STCD1 = ST_GCIAS-CUIT.
*    Lleno la tabla que vamos a usar en el batch input.
      ST_DATA-BUKRS = ST_LFA1-BUKRS.
      ST_DATA-LIFNR = ST_LFA1-LIFNR.
      ST_DATA-WT_EXNR = ST_GCIAS-CERTIFICADO.
      ST_DATA-WT_EXRT = ST_GCIAS-PORCENTAJE.
      ST_DATA-WT_WTEXRS = '2'.
      ST_DATA-WT_EXDF = ST_GCIAS-F_EMISION.
      ST_DATA-WT_EXDT = ST_GCIAS-F_VIGENCIA.

      IF ST_LFA1-G2 EQ C_X.
        ST_DATA-WITHT = C_G2.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-GB EQ C_X.
        ST_DATA-WITHT = C_GB.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-G1 EQ C_X.
        ST_DATA-WITHT = C_G1.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-GA EQ C_X.
        ST_DATA-WITHT = C_GA.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      CLEAR ST_DATA.

    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " F_ARMO_TABLA_BATCH
*&---------------------------------------------------------------------*
*&      Form  F_INICIALIZO_TABLAS
*&---------------------------------------------------------------------*
FORM F_INICIALIZO_TABLAS .

  DATA VL_FECHA TYPE SY-DATUM.
  CLEAR: T_GCIAS,
         T_SUSS,
         T_IVA,
         T_MENSAJE_E,
         T_LFA1,
         T_LFBW,
         T_LFB1,
         T_DATA,
         ST_DATA,
         ST_MENSAJE_E,
         ST_GCIAS,
         ST_SUSS,
         ST_LFA1,
         ST_LFB1,
         ST_LFBW,
         ST_IVA,
         V_SMS.

  REFRESH: T_GCIAS,
           T_SUSS,
           T_IVA,
           T_MENSAJE_E,
           T_LFA1,
           T_LFBW,
           T_LFB1,
           T_DATA.

*  VL_FECHA = SY-DATUM - 3.
*     Borro de la tabla log de informe zfiyt_exclusion
*  DELETE FROM ZFIYT_EXCLUSION WHERE FECHA LE VL_FECHA.
*  IF SY-SUBRC EQ 0.
*    COMMIT WORK AND WAIT.
*  ELSE.
*    ROLLBACK WORK.
*  ENDIF.

ENDFORM.                    " F_INICIALIZO_TABLAS
*&---------------------------------------------------------------------*
*&      Form  F_SUBO_ARCHIVO_A_TABLAS
*&---------------------------------------------------------------------*
FORM F_SUBO_ARCHIVO_A_TABLAS USING P_ARCHIVO CHANGING PO_SMS.

* Si el check esta en blanco, levantamos los archivos del servidor.
  IF P_CHECK IS INITIAL.
    PERFORM F_SUBIDA_ARCHIVO_SERVIDOR USING P_ARCHIVO.
  ELSE.
* Levanta de la maquina
    CASE P_ARCHIVO .
      WHEN 'SUSS'.
*   Subo los datos del archivo
        PERFORM F_SUBIDA_SUSS USING P_PATH.
        IF T_OUT_AUX IS INITIAL.
          PO_SMS = C_X.
        ENDIF.
      WHEN 'IVA'.
*   Subo los datos del archivo
        PERFORM F_SUBIDA_IVA  USING P_PATH .
        IF T_OUT_AUX IS INITIAL.
          PO_SMS = C_X.
        ENDIF.
      WHEN 'GCIAS'.
*   Subo los datos del archivo
        PERFORM F_SUBIDA_GCIAS USING P_PATH.
        IF T_OUT_AUX IS INITIAL.
          PO_SMS = C_X.
        ENDIF.
      WHEN 'G2681'.
*   Subo los datos del archivo
        PERFORM F_SUBIDA_G2681  USING P_PATH .
        IF T_G2681 IS INITIAL.
          PO_SMS = C_X.
* ----------------------------------------------------------
        ELSE.
          READ TABLE T_G2681 INTO ST_G2681 INDEX 1.
          IF SY-SUBRC IS INITIAL.

            CLEAR: ST_SETLEAF,
                    V_VALFROM,
                    V_FECHA,
                    V_PUBLI.

            IF ST_G2681-CUIT(19) EQ 'Fecha de Descarga: '.

              MOVE ST_G2681-CUIT+19(10) TO V_PUBLI.

              MOVE 'ZGCIAS_EXCL2' TO VL_SETNAME.

              SELECT SINGLE * FROM SETLEAF CLIENT SPECIFIED
                       INTO  ST_SETLEAF
                       WHERE MANDT    = SY-MANDT
                         AND SETCLASS = '0000'
                         AND SETNAME  = VL_SETNAME.

              IF SY-SUBRC EQ 0.

                CONCATENATE V_PUBLI+6(4)
                            V_PUBLI+3(2)
                            V_PUBLI(2)
                      INTO  V_FECHA.

                CONDENSE ST_SETLEAF-VALFROM NO-GAPS.

                MOVE ST_SETLEAF-VALFROM TO V_VALFROM.

                IF V_FECHA LE V_VALFROM.
                  MESSAGE TEXT-E05 TYPE 'E'.
                ENDIF.

              ENDIF.

            ENDIF.
* ---------------------------------------------------------
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDIF.

ENDFORM.                    " F_SUBO_ARCHIVO_A_TABLAS
*&---------------------------------------------------------------------*
*&      Form  F_CONVERSION_NRO_CERT
*&---------------------------------------------------------------------*
FORM F_CONVERSION_NRO_CERT  USING    PI_CERT
                            CHANGING PO_CERTIFICADO.

  DO 5 TIMES.
    REPLACE '-' WITH ' ' INTO PI_CERT.
  ENDDO.
  CONDENSE PI_CERT NO-GAPS.

  CONCATENATE PI_CERT(4)  PI_CERT+4(26)
  INTO PO_CERTIFICADO SEPARATED BY '-'.



ENDFORM.                    " F_CONVERSION_NRO_CERT
*&---------------------------------------------------------------------*
*&      Form  F_MENSAJES_ERROR
*&---------------------------------------------------------------------*
FORM F_MENSAJES_ERROR  USING    STL_LFA1 TYPE TY_LFA1
                                PI_TEXT.

  DATA: VL_AGREGAR_MSG TYPE CHAR1.

  CLEAR: ST_MENSAJE_E.

* Se valida que el proveedor esté en el archivo para agregar el mensaje
  IF P_SUSS EQ C_X.
    PERFORM F_VALIDAR_PROV_SUSS USING ST_LFA1-STCD1 CHANGING VL_AGREGAR_MSG.
    MOVE 'SUSS' TO ST_MENSAJE_E-INDICADOR.
  ELSEIF P_GCIAS EQ C_X.
    PERFORM F_VALIDAR_PROV_GCIAS USING ST_LFA1-STCD1 CHANGING VL_AGREGAR_MSG.
    MOVE 'Ganancia' TO ST_MENSAJE_E-INDICADOR.
  ELSEIF P_IVA EQ C_X.
    PERFORM F_VALIDAR_PROV_IVA USING ST_LFA1-STCD1 CHANGING VL_AGREGAR_MSG.
    MOVE 'IVA' TO ST_MENSAJE_E-INDICADOR.
  ELSEIF P_G2681 EQ C_X.
    PERFORM F_VALIDAR_PROV_GCIAS USING ST_LFA1-STCD1 CHANGING VL_AGREGAR_MSG.
    MOVE 'Gcias 2681' TO ST_MENSAJE_E-INDICADOR.
  ENDIF.

* Si el proveedor no se encuentra en el archivo, no se agrega el mensaje
  CHECK VL_AGREGAR_MSG IS INITIAL.

  ST_MENSAJE_E-MANDT = SY-MANDT.
  ST_MENSAJE_E-LIFNR = ST_LFA1-LIFNR.
  ST_MENSAJE_E-BUKRS = ST_LFA1-BUKRS.
  ST_MENSAJE_E-NAME1 = ST_LFA1-NAME1.
  ST_MENSAJE_E-STCD1 = ST_LFA1-STCD1.
  ST_MENSAJE_E-FECHA = SY-DATUM.
  ST_MENSAJE_E-HORA  = SY-UZEIT.
  ST_MENSAJE_E-TIPO  = 'E'.
  ST_MENSAJE_E-TEXTO = PI_TEXT.

  APPEND ST_MENSAJE_E  TO T_MENSAJE_E.


ENDFORM.                    " F_MENSAJES_ERROR
*&---------------------------------------------------------------------*
*&      Form  F_ARMO_TABLA_batch_SUSS
*&---------------------------------------------------------------------*
FORM F_ARMO_TABLA_BATCH_SUSS .
  LOOP AT T_SUSS INTO ST_SUSS.

*   Valido el cliente si existe en sap
    LOOP AT T_LFA1 INTO ST_LFA1 WHERE STCD1 = ST_SUSS-CUIT.
*    lleno la tabla que vamos a usar en el batch input.
      ST_DATA-BUKRS = ST_LFA1-BUKRS.
      ST_DATA-LIFNR = ST_LFA1-LIFNR.
      ST_DATA-WT_EXNR = ST_SUSS-CERTIFICADO.
      ST_DATA-WT_EXRT = ST_SUSS-PORCENTAJE.
      ST_DATA-WT_WTEXRS = '2'.
      ST_DATA-WT_EXDF = ST_SUSS-F_EMISION.
      ST_DATA-WT_EXDT = ST_SUSS-F_VIGENCIA.

      IF ST_LFA1-EG EQ C_X.
        ST_DATA-WITHT = C_EG.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-EL EQ C_X.
        ST_DATA-WITHT = C_EL.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-ES EQ C_X.
        ST_DATA-WITHT = C_ES.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-CO EQ C_X.
        ST_DATA-WITHT = C_CO.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-EE EQ C_X.
        ST_DATA-WITHT = C_EE.
        APPEND ST_DATA TO T_DATA.
      ENDIF.
      CLEAR ST_DATA.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " F_ARMO_TABLA_batch_SUSS
*&---------------------------------------------------------------------*
*&      Form  F_MONTH_NAMES_GET
*&---------------------------------------------------------------------*
FORM F_MONTH_NAMES_GET .

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      LANGUAGE              = 'E'
    TABLES
      MONTH_NAMES           = T_MONTH_NAMES
    EXCEPTIONS
      MONTH_NAMES_NOT_FOUND = 1
      OTHERS                = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      LANGUAGE              = 'S'
    TABLES
      MONTH_NAMES           = T_MONTH_NAMES
    EXCEPTIONS
      MONTH_NAMES_NOT_FOUND = 1
      OTHERS                = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_MONTH_NAMES_GET
*&---------------------------------------------------------------------*
*&      Form  F_SEL_LFBW
*&---------------------------------------------------------------------*
FORM F_SEL_LFBW  USING    PI_GCIAS
                          PI_G2681
                          PI_SUSS
                          PI_IVA.
  REFRESH T_LFBW.

  IF PI_SUSS EQ  C_X.
    SELECT *
    FROM LFBW
    INTO TABLE T_LFBW
    FOR ALL ENTRIES IN T_LFA1
    WHERE    LIFNR EQ T_LFA1-LIFNR " Proveedores
      AND    BUKRS IN SO_BUKRS     " Sociedad
      AND (  WITHT EQ C_CO        "indicador de SUSS
       OR    WITHT EQ C_EE        "indicador de SUSS
       OR    WITHT EQ C_EG        "indicador de SUSS
       OR    WITHT EQ C_EL        "indicador de SUSS
       OR    WITHT EQ C_ES ).     "indicador de SUSS
  ENDIF.

  IF PI_GCIAS EQ C_X.
    SELECT *
    FROM LFBW
    INTO TABLE T_LFBW
    FOR ALL ENTRIES IN T_LFA1
    WHERE   LIFNR EQ T_LFA1-LIFNR " Proveedores
      AND   BUKRS IN SO_BUKRS     " Sociedad
      AND ( WITHT EQ C_G1
      OR    WITHT EQ C_G2
      OR    WITHT EQ C_GB       "indicador de ganancias
      OR    WITHT EQ C_GA ).      "indicador de ganancias
  ENDIF.

  IF PI_IVA EQ C_X.
    SELECT *
    FROM LFBW
    INTO TABLE T_LFBW
    FOR ALL ENTRIES IN T_LFA1
    WHERE LIFNR EQ T_LFA1-LIFNR " Proveedores
      AND BUKRS IN SO_BUKRS     " Sociedad
      AND WITHT EQ C_IV.        "indicador de IVA
  ENDIF.

  IF PI_G2681 EQ C_X.
    SELECT *
    FROM LFBW
    INTO TABLE T_LFBW
    FOR ALL ENTRIES IN T_LFA1
    WHERE   LIFNR EQ T_LFA1-LIFNR " Proveedores
      AND   BUKRS IN SO_BUKRS     " Sociedad
      AND ( WITHT EQ C_G2
       OR   WITHT EQ C_G1
       OR   WITHT EQ C_GA      "indicador de ganancias
       OR   WITHT EQ C_GB ).    "indicador de ganancias
  ENDIF.

  SORT  T_LFA1 BY  LIFNR BUKRS.

ENDFORM.                    " F_SEL_LFBW
*&---------------------------------------------------------------------*
*&      Form  F_MARCO_LOS_INDICADORES
*&---------------------------------------------------------------------*
FORM F_MARCO_LOS_INDICADORES USING PI_INDICADOR PI_IDX CHANGING STL_LFA1 TYPE TY_LFA1 .
  CASE PI_INDICADOR.
    WHEN 'SUSS'.
      IF ( STL_LFA1-CO IS INITIAL AND
           STL_LFA1-EG IS INITIAL AND
           STL_LFA1-EE IS INITIAL AND
           STL_LFA1-EL IS INITIAL AND
           STL_LFA1-ES IS INITIAL ).
        PERFORM F_MENSAJES_ERROR USING STL_LFA1 TEXT-E03.
        DELETE T_LFA1 INDEX PI_IDX.
      ENDIF.
    WHEN 'IVA'.
      IF  STL_LFA1-IV IS INITIAL.
        PERFORM F_MENSAJES_ERROR USING STL_LFA1 TEXT-E03.
        DELETE T_LFA1 INDEX PI_IDX.
      ENDIF.
    WHEN 'GCIAS'.
      IF ( STL_LFA1-G1 IS INITIAL AND STL_LFA1-G2 IS INITIAL ).
        PERFORM F_MENSAJES_ERROR USING STL_LFA1 TEXT-E03.
        DELETE T_LFA1 INDEX PI_IDX.
      ENDIF.
  ENDCASE.



ENDFORM.                    " F_MARCO_LOS_INDICADORES
*&---------------------------------------------------------------------*
*&      Form  F_batch_INPUT
*&---------------------------------------------------------------------*
FORM F_BATCH_INPUT .

  DATA: VL_LINE  TYPE SY-TABIX,
        VL_PORC  TYPE CHAR6,
        VL_MODO  TYPE CHAR1 VALUE 'N',
        VL_TABIX TYPE SY-TABIX,
        VL_LIFNR TYPE LIFNR,
        VL_WT_EXDT TYPE WT_EXDT,
        VL_WT_EXDF TYPE WT_EXDF,
        VL_TEXTO TYPE CHAR200.

  REFRESH: T_IVA,
           T_SUSS,
           T_GCIAS,
           T_MES_SAL.

  LOOP AT T_DATA INTO ST_DATA.

* Se reinician las tablas para realizar el batch input
    REFRESH: T_BDCDATA,
             T_MESSTAB.
    CLEAR:   T_BDCDATA,
             T_MESSTAB.

    WRITE ST_DATA-WT_EXRT TO VL_PORC.

* Se quitan las comas y puntos de centenas
    DO 5 TIMES.
      REPLACE '.'  INTO VL_PORC WITH ''.
      REPLACE ','  INTO VL_PORC WITH ''.
    ENDDO.
    CONDENSE VL_PORC NO-GAPS.

    CONCATENATE ST_DATA-WT_EXDF+4(4)
                ST_DATA-WT_EXDF+2(2)
                ST_DATA-WT_EXDF(2)
      INTO VL_WT_EXDF.

    CONCATENATE ST_DATA-WT_EXDT+4(4)
                ST_DATA-WT_EXDT+2(2)
                ST_DATA-WT_EXDT(2)
      INTO VL_WT_EXDT.

* Se valida que el dato no haya sido procesado con anterioridad
    SELECT SINGLE LIFNR
      INTO VL_LIFNR
      FROM LFBW
      WHERE LIFNR = ST_DATA-LIFNR
        AND BUKRS = ST_DATA-BUKRS
        AND WITHT = ST_DATA-WITHT
        AND WT_EXNR = ST_DATA-WT_EXNR
        AND WT_WTEXRS = ST_DATA-WT_WTEXRS
        AND WT_EXDF = VL_WT_EXDF
        AND WT_EXDT = VL_WT_EXDT.
    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.

    PERFORM BDC_DYNPRO USING: 'SAPMF02K' '0101'.
    PERFORM BDC_FIELD  USING: 'BDC_OKCODE' '/00',
                              'RF02K-LIFNR' ST_DATA-LIFNR,
                              'RF02K-BUKRS' ST_DATA-BUKRS,
                              'RF02K-D0610' 'X'.

    PERFORM BDC_DYNPRO USING: 'SAPMF02K' '0610'.
    PERFORM BDC_FIELD  USING: 'BDC_OKCODE' '=POSZ',
                              'LFB1-QLAND' '528'.

    PERFORM BDC_DYNPRO USING: 'SAPMF02K' '2610'.
    PERFORM BDC_FIELD  USING: 'BDC_OKCODE' '/00',
                              'LFBW-WITHT' ST_DATA-WITHT.

    PERFORM BDC_DYNPRO USING: 'SAPMF02K' '0610'.
    PERFORM BDC_FIELD  USING: 'BDC_OKCODE' '/00',
                              'LFB1-QLAND' '528',
                              'LFBW-WT_EXRT(01)' VL_PORC(3).

    PERFORM BDC_FIELD  USING: 'LFBW-WT_EXNR(01)' ST_DATA-WT_EXNR.
    PERFORM BDC_FIELD  USING: 'LFBW-WT_WTEXRS(01)' ST_DATA-WT_WTEXRS,
                              'LFBW-WT_EXDF(01)' ST_DATA-WT_EXDF,
                              'LFBW-WT_SUBJCT(01)' ' ',
                              'LFBW-WT_EXDT(01)' ST_DATA-WT_EXDT.

    PERFORM BDC_DYNPRO USING: 'SAPMF02K' '0610'.
    PERFORM BDC_FIELD  USING: 'BDC_OKCODE' '=UPDA',
                              'LFB1-QLAND' '528'.

*   Llamar la transaccion
* ---> S4 Migration - 29/06/2023 - JS
*    CALL TRANSACTION 'XK02' USING  T_BDCDATA
*                            MODE   VL_MODO
*                            UPDATE 'S'
*                            MESSAGES INTO T_MESSTAB.

      DATA: lt_bdc    TYPE bdcdata_tab,
            lt_bdcmsg TYPE tab_bdcmsgcoll,
            wa_lfa1   type lfa1.

      DATA: lo_migbp TYPE REF TO /mignow/cl_migbp.

      lt_bdc = CONV #( T_BDCDATA[] ).

      CREATE OBJECT lo_migbp
        EXPORTING
          im_test    = abap_false
          im_tcode   = 'BP'
          it_bdcdata = lt_bdc.

      CALL METHOD lo_migbp->mt_bp_process_old_shdb(
        CHANGING
          ct_bdcmsg = lt_bdcmsg ).

      CALL METHOD lo_migbp->mt_set_data_directly( is_lfa1 = wa_lfa1 ).

      CALL METHOD lo_migbp->mt_bp_process_data( CHANGING ct_bdcmsg = lt_bdcmsg ).

      loop at lt_bdcmsg into data(wa_bdcmsg).
        move-corresponding wa_bdcmsg to T_MESSTAB.
        append T_MESSTAB.
        clear T_MESSTAB.
      endloop.
* <--- S4 Migration - 29/06/2023 - JS

* Se verifica que no se generaron errores
    READ TABLE T_MESSTAB
      WITH KEY MSGTYP = 'E'.

    IF SY-SUBRC = 0.
* Error en el proceso de actualización
      LOOP AT T_MESSTAB WHERE MSGTYP = 'E'.
        MESSAGE ID T_MESSTAB-MSGID TYPE T_MESSTAB-MSGTYP NUMBER T_MESSTAB-MSGNR INTO VL_TEXTO.
        CONCATENATE VL_TEXTO  T_MESSTAB-MSGV2 INTO VL_TEXTO SEPARATED BY SPACE.
        CONCATENATE ST_MENSAJE_E-TEXTO  VL_TEXTO INTO ST_MENSAJE_E-TEXTO SEPARATED BY ' - '.
      ENDLOOP.
      ST_MENSAJE_E-TIPO  = T_MESSTAB-MSGTYP.
    ELSE.
* No se generaron errores en la modificación
      ST_MENSAJE_E-TEXTO = 'El acreedor fue modificado correctamente.'(004).
      ST_MENSAJE_E-TIPO  = 'S'.
    ENDIF.

* Se obtienen los datos del proveedor
    READ TABLE T_LFA1 INTO ST_LFA1
    WITH KEY LIFNR = ST_DATA-LIFNR.

* Se cargan los datos en la salida
    ST_MENSAJE_E-LIFNR = ST_DATA-LIFNR.
    ST_MENSAJE_E-BUKRS = ST_DATA-BUKRS.
    ST_MENSAJE_E-NAME1 = ST_LFA1-NAME1.
    ST_MENSAJE_E-STCD1 = ST_LFA1-STCD1.
    ST_MENSAJE_E-FECHA = SY-DATUM.
    ST_MENSAJE_E-HORA  = SY-UZEIT.

    IF P_SUSS EQ C_X.
      ST_MENSAJE_E-INDICADOR = 'SUSS'.
    ELSEIF P_GCIAS EQ C_X.
      ST_MENSAJE_E-INDICADOR = 'Ganancia'.
    ELSEIF P_G2681 EQ C_X.
      ST_MENSAJE_E-INDICADOR = 'Gcias_2681'.
    ELSEIF P_IVA EQ C_X.
      ST_MENSAJE_E-INDICADOR = 'IVA'.
    ENDIF.

    APPEND ST_MENSAJE_E TO T_MENSAJE_E.

  ENDLOOP.

ENDFORM.                    " F_batch_INPUT
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR T_BDCDATA.
  T_BDCDATA-PROGRAM  = PROGRAM.
  T_BDCDATA-DYNPRO   = DYNPRO.
  T_BDCDATA-DYNBEGIN = 'X'.
  APPEND T_BDCDATA.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR T_BDCDATA.
  T_BDCDATA-FNAM = FNAM.
  T_BDCDATA-FVAL = FVAL.
  APPEND T_BDCDATA.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  F_GRISADO_CAMPOS
*&---------------------------------------------------------------------*
FORM F_GRISADO_CAMPOS .

  LOOP AT SCREEN.
    IF RB_001 EQ 'X'.
      REFRESH SO_FECHA.
      IF SCREEN-NAME = 'P_PATH'.
        SCREEN-INPUT = '0'.
      ENDIF.
      IF SCREEN-NAME = 'P_SUSS'.
        SCREEN-INPUT = '1'.
      ENDIF.
      IF SCREEN-NAME = 'P_GCIAS'.
        SCREEN-INPUT = '1'.
      ENDIF.
      IF SCREEN-NAME = 'P_G2681'.
        SCREEN-INPUT = '1'.
      ENDIF.
      IF SCREEN-NAME = 'P_CHECK'.
        SCREEN-INPUT = '1'.
      ENDIF.
      IF SCREEN-NAME = 'P_IVA'.
        SCREEN-INPUT = '1'.
      ENDIF.
      IF SCREEN-NAME = 'SO_FECHA-LOW'.
        SCREEN-INPUT = '0'.
      ENDIF.
      IF SCREEN-NAME = 'SO_FECHA-HIGH'.
        SCREEN-INPUT = '0'.
      ENDIF.
    ELSE.
      CLEAR: P_PATH, P_CHECK.
      IF SCREEN-NAME = 'P_CHECK'.
        SCREEN-INPUT = '0'.
      ENDIF.
      IF SCREEN-NAME = 'P_PATH'.
        SCREEN-INPUT = '0'.
      ENDIF.
      IF SCREEN-NAME = 'P_GCIAS'.
        SCREEN-INPUT = '0'.
      ENDIF.
      IF SCREEN-NAME = 'P_G2681'.
        SCREEN-INPUT = '0'.
      ENDIF.
      IF SCREEN-NAME = 'P_IVA'.
        SCREEN-INPUT = '0'.
      ENDIF.
      IF SCREEN-NAME = 'P_SUSS'.
        SCREEN-INPUT = '0'.
      ENDIF.
      IF SCREEN-NAME = 'SO_fecha-LOW'.
        SCREEN-INPUT = '1'.
      ENDIF.
      IF SCREEN-NAME = 'SO_fecha-HIGH'.
        SCREEN-INPUT = '1'.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME = 'P_PATH'.
      IF P_CHECK IS INITIAL.
        SCREEN-INPUT = '0'.
      ELSE.
        SCREEN-INPUT = '1'.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " F_GRISADO_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  F_ARMO_TABLA_batch_IVA
*&---------------------------------------------------------------------*
FORM F_ARMO_TABLA_BATCH_IVA .

  LOOP AT T_IVA INTO ST_IVA.
*   Valido el cliente si existe en sap
    LOOP AT T_LFA1 INTO ST_LFA1 WHERE STCD1 = ST_IVA-CUIT.
      DO 10 TIMES.
        REPLACE '/' WITH '' INTO ST_IVA-F_EMISION.
        REPLACE '.' WITH '' INTO ST_IVA-F_EMISION.
        REPLACE '/' WITH '' INTO ST_IVA-F_VIGENCIA.
        REPLACE '.' WITH '' INTO ST_IVA-F_VIGENCIA.
      ENDDO.
      CONDENSE ST_IVA-F_EMISION  NO-GAPS.
      CONDENSE ST_IVA-F_VIGENCIA NO-GAPS.
*    lleno la tabla que vamos a usar en el batch input.
      ST_DATA-BUKRS = ST_LFA1-BUKRS.
      ST_DATA-LIFNR = ST_LFA1-LIFNR.
      ST_DATA-WT_EXNR = 'NO POSEE'.
      ST_DATA-WT_EXRT = ST_IVA-PORCENTAJE.
      ST_DATA-WT_WTEXRS = '2'.
      ST_DATA-WT_EXDF = ST_IVA-F_EMISION.
      ST_DATA-WT_EXDT = ST_IVA-F_VIGENCIA.

      IF ST_LFA1-IV EQ C_X.
        ST_DATA-WITHT = C_IV.
        APPEND ST_DATA TO T_DATA.
      ENDIF.
      CLEAR ST_DATA.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " F_ARMO_TABLA_batch_IVA
*&---------------------------------------------------------------------*
*&      Form  F_SEL_zfiyt_exclusion
*&---------------------------------------------------------------------*
FORM F_SEL_ZFIYT_EXCLUSION.
  DATA: ST_EXCLUSION  TYPE ZFIYT_EXCLUSION,
        STL_MENSAJE_E TYPE TY_MENSAJE_E.

  SELECT *
  FROM ZFIYT_EXCLUSION
  INTO TABLE T_EXCLUSION
  WHERE BUKRS IN SO_BUKRS
  AND   FECHA IN SO_FECHA
  AND   TIPO  EQ 'E'.

  SORT T_EXCLUSION BY BUKRS FECHA HORA LIFNR.
  IF SY-SUBRC EQ 0.
    LOOP AT T_EXCLUSION INTO ST_EXCLUSION.
      CLEAR: ST_MENSAJE_E.
      MOVE:
        ST_EXCLUSION-LIFNR      TO ST_MENSAJE_E-LIFNR     ,
        ST_EXCLUSION-BUKRS      TO ST_MENSAJE_E-BUKRS     ,
        ST_EXCLUSION-NAME1      TO ST_MENSAJE_E-NAME1     ,
        ST_EXCLUSION-STCD1      TO ST_MENSAJE_E-STCD1     ,
        ST_EXCLUSION-FECHA      TO ST_MENSAJE_E-FECHA     ,
        ST_EXCLUSION-HORA       TO ST_MENSAJE_E-HORA      ,
        ST_EXCLUSION-TIPO       TO ST_MENSAJE_E-TIPO      ,
        ST_EXCLUSION-INDICADOR  TO ST_MENSAJE_E-INDICADOR ,
        ST_EXCLUSION-TEXTO      TO ST_MENSAJE_E-TEXTO     .
      READ TABLE T_MENSAJE_E INTO STL_MENSAJE_E
      WITH KEY   LIFNR     = ST_MENSAJE_E-LIFNR
                 INDICADOR = ST_MENSAJE_E-INDICADOR.
      IF SY-SUBRC NE 0.
        APPEND ST_MENSAJE_E       TO T_MENSAJE_E            .
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT T_MENSAJE_E BY BUKRS LIFNR FECHA HORA INDICADOR.
ENDFORM.                    " F_SEL_zfiyt_exclusion
*&---------------------------------------------------------------------*
*&      Form  F_SUBIDA_ARCHIVO_SERVIDOR
*&---------------------------------------------------------------------*
FORM F_SUBIDA_ARCHIVO_SERVIDOR USING PI_INDICADOR.

  DATA: VL_LENG  TYPE I,
        VL_FNAME TYPE CHAR200,
        VL_DSN   TYPE CHAR200,
        VL_GCIAS TYPE TY_GCIAS,
        VL_LINE  TYPE STRING,
        WA       TYPE TY_OUT_AUX.
  FIELD-SYMBOLS <HEX_CONTAINER> TYPE TY_OUT_AUX .

  ASSIGN WA TO <HEX_CONTAINER> CASTING.

* Bajo los datos correctos
  CLEAR: VL_DSN,
         ST_DATA.

* Muevo al servidor el archivo.
  CONCATENATE V_PATH '\Finanzas\Exclusiones\A_procesar\'
         INTO VL_FNAME.
* Fin 26.07.2010

  CASE PI_INDICADOR.
    WHEN 'SUSS'.
      CONCATENATE VL_FNAME
                'SALIDA.txt'
            INTO VL_DSN.
    WHEN 'IVA'.
      CONCATENATE VL_FNAME
                'rg17.txt'
            INTO VL_DSN.

    WHEN 'GCIAS'.
      CONCATENATE VL_FNAME
                'rg830n1.txt'
            INTO VL_DSN.

    WHEN 'G2681'.
      CONCATENATE VL_FNAME
                'G2681.txt'
            INTO VL_DSN.

  ENDCASE.

*   Abro el servidor
  CASE PI_INDICADOR.
    WHEN 'SUSS'.
      OPEN DATASET VL_DSN FOR INPUT IN TEXT MODE ENCODING DEFAULT.
      CHECK SY-SUBRC EQ 0.
      DO.
        READ DATASET VL_DSN INTO VL_LINE LENGTH VL_LENG.
        IF SY-SUBRC <> 0.
          EXIT.
        ELSE.
          WRITE  VL_LINE TO  ST_OUT_AUX-LINEA.
          APPEND ST_OUT_AUX TO  T_OUT_AUX.
        ENDIF.
      ENDDO.
    WHEN 'IVA'.
      OPEN DATASET VL_DSN FOR INPUT IN TEXT MODE ENCODING  NON-UNICODE .
      CHECK SY-SUBRC EQ 0.
      ASSIGN WA TO <HEX_CONTAINER> CASTING.
      DO.
        READ DATASET VL_DSN INTO <HEX_CONTAINER> LENGTH VL_LENG.
        IF SY-SUBRC <> 0.
          EXIT.
        ELSE.
          WRITE  WA-LINEA TO  ST_OUT_AUX-LINEA.
          APPEND ST_OUT_AUX-LINEA TO  T_OUT_AUX.
        ENDIF.
      ENDDO.
      CLOSE DATASET VL_DSN.
    WHEN 'GCIAS'.
      OPEN DATASET VL_DSN FOR INPUT IN TEXT MODE ENCODING  NON-UNICODE .
      CHECK SY-SUBRC EQ 0.
      ASSIGN WA TO <HEX_CONTAINER> CASTING.
      DO.
        READ DATASET VL_DSN INTO <HEX_CONTAINER> LENGTH VL_LENG.
        IF SY-SUBRC <> 0.
          EXIT.
        ELSE.
          WRITE  WA-LINEA TO  ST_OUT_AUX-LINEA.
          APPEND ST_OUT_AUX-LINEA TO  T_OUT_AUX.
        ENDIF.
      ENDDO.
      CLOSE DATASET VL_DSN.
    WHEN 'G2681'.
      OPEN DATASET VL_DSN FOR INPUT IN TEXT MODE ENCODING  NON-UNICODE .
      CHECK SY-SUBRC EQ 0.
      ASSIGN WA TO <HEX_CONTAINER> CASTING.
      DO.
        READ DATASET VL_DSN INTO <HEX_CONTAINER> LENGTH VL_LENG.
        IF SY-SUBRC <> 0.
          EXIT.
        ELSE.
          WRITE  WA-LINEA TO  ST_OUT_AUX-LINEA.
          APPEND ST_OUT_AUX-LINEA TO  T_OUT_AUX.
        ENDIF.
      ENDDO.
      CLOSE DATASET VL_DSN.
  ENDCASE.
ENDFORM.                    " F_SUBIDA_ARCHIVO_SERVIDOR

*&---------------------------------------------------------------------*
*&      Form  DATASET
*&---------------------------------------------------------------------*
FORM DATASET  USING    VL_CHOW
                       VL_DSN.
  DATA PARCOM_LOC TYPE CHAR100.
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

ENDFORM.                    "dataset
*&---------------------------------------------------------------------*
*&      Form  F_BAJADA_SERVIDOR
*&---------------------------------------------------------------------*
FORM F_BAJADA_SERVIDOR USING PI_INDICADOR PI_E.
  DATA: BEGIN OF TABL OCCURS 0,
          LINE(560),
        END OF TABL.
  DATA: ST_E       TYPE TY_OUT_AUX,
        VL_RUTA    TYPE CHAR100 VALUE '\Finanzas\Exclusiones\Procesados_con_errores\',
        VL_RUTA2   TYPE CHAR100 VALUE '\Finanzas\Exclusiones\Procesados_con_exito\',
        VL_PADRON  TYPE CHAR100 VALUE 'Padron_',
        VL_DSN     TYPE CHAR200,
        VL_CHOW    TYPE CHAR17 VALUE 'chown pagnrl:spa ',
        PARCOM_LOC TYPE CHAR100.

  IF PI_E EQ 'E'.

    CASE PI_INDICADOR.
      WHEN 'SUSS'.
*Exclusion SUSS 1904
        CONCATENATE V_PATH VL_RUTA
                    'salida_'  SY-DATUM '.txt'
        INTO VL_DSN.
      WHEN 'IVA'.
*Exclusion IVA 17 =
        CONCATENATE V_PATH VL_RUTA
                    'rg17_'  SY-DATUM '.txt'
        INTO VL_DSN.
      WHEN 'GCIAS'.
*Exclusion Ganancias 830 = ´´
        CONCATENATE V_PATH VL_RUTA
                    'rg830n1_'  SY-DATUM '.txt'
        INTO VL_DSN.
      WHEN 'G2681'.
*Exclusion Ganancias 2681 = ´´
        CONCATENATE V_PATH VL_RUTA
                    'G2681_'  SY-DATUM '.txt'
        INTO VL_DSN.
    ENDCASE.
  ELSE.

    CASE PI_INDICADOR.
      WHEN 'SUSS'.
*Exclusion SUSS 1904
        CONCATENATE V_PATH VL_RUTA2
                    'salida_' SY-DATUM '.txt'
        INTO VL_DSN.
      WHEN 'IVA'.
*Exclusion IVA 17 =
        CONCATENATE V_PATH VL_RUTA2
                    'rg17_' SY-DATUM '.txt'
        INTO VL_DSN.
      WHEN 'GCIAS'.
*Exclusion Ganancias 830 = ´´
        CONCATENATE V_PATH VL_RUTA2
                    'rg830n1_' SY-DATUM '.txt'
        INTO VL_DSN.
      WHEN 'G2681'.
*Exclusion Ganancias 2681 = ´´
        CONCATENATE V_PATH VL_RUTA2
                    'G2681_' SY-DATUM '.txt'
        INTO VL_DSN.
    ENDCASE.
  ENDIF.

* Los archivos se crean en caso de contener datos para grabar
  CHECK T_IVA_E   IS NOT INITIAL
     OR T_SUSS_E  IS NOT INITIAL
     OR T_GCIAS_E IS NOT INITIAL.

  OPEN DATASET VL_DSN FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  CHECK SY-SUBRC EQ 0.

  DATA:V_TEXT TYPE T100-TEXT.

  CASE PI_INDICADOR.
    WHEN 'SUSS'.
      LOOP AT T_SUSS_E INTO  ST_E .
        TRANSFER  ST_E-LINEA TO  VL_DSN.
        IF SY-SUBRC <> 0.
* Incluído en 06.12.2010 - Diego
          SELECT SINGLE TEXT INTO V_TEXT
            FROM T100 WHERE SPRSL = SY-LANGU
                        AND ARBGB = '6P'
                        AND MSGNR = '601'.
          REPLACE '&' WITH VL_DSN INTO V_TEXT.
          WRITE: /05 V_TEXT.
* Fin 06.12.2010 ...............................
          MESSAGE I601(6P) WITH VL_DSN.
          STOP.
        ENDIF.
      ENDLOOP.
    WHEN 'IVA'.
      LOOP AT T_IVA_E INTO  ST_E .
        TRANSFER ST_E-LINEA TO  VL_DSN.
        IF SY-SUBRC <> 0.
* Incluído en 06.12.2010 - Diego
          SELECT SINGLE TEXT INTO V_TEXT
            FROM T100 WHERE SPRSL = SY-LANGU
                        AND ARBGB = '6P'
                        AND MSGNR = '601'.
          REPLACE '&' WITH VL_DSN INTO V_TEXT.
          WRITE: /05 V_TEXT.
* Fin 06.12.2010 ...............................
          MESSAGE I601(6P) WITH VL_DSN.
          STOP.
        ENDIF.
      ENDLOOP.

    WHEN 'GCIAS'.
      LOOP AT T_GCIAS_E INTO  ST_E .
        TRANSFER ST_E-LINEA TO  VL_DSN.
        IF SY-SUBRC <> 0.
* Incluído en 06.12.2010 - Diego
          SELECT SINGLE TEXT INTO V_TEXT
            FROM T100 WHERE SPRSL = SY-LANGU
                        AND ARBGB = '6P'
                        AND MSGNR = '601'.
          REPLACE '&' WITH VL_DSN INTO V_TEXT.
          WRITE: /05 V_TEXT.
* Fin 06.12.2010 ...............................
          MESSAGE I601(6P) WITH VL_DSN.
          STOP.
        ENDIF.
      ENDLOOP.

    WHEN 'G2681'.
      LOOP AT T_GCIAS_E INTO  ST_E .
        TRANSFER ST_E-LINEA TO  VL_DSN.
        IF SY-SUBRC <> 0.
* Incluído en 06.12.2010 - Diego
          SELECT SINGLE TEXT INTO V_TEXT
            FROM T100 WHERE SPRSL = SY-LANGU
                        AND ARBGB = '6P'
                        AND MSGNR = '601'.
          REPLACE '&' WITH VL_DSN INTO V_TEXT.
          WRITE: /05 V_TEXT.
* Fin 06.12.2010 ...............................
          MESSAGE I601(6P) WITH VL_DSN.
          STOP.
        ENDIF.
      ENDLOOP.

  ENDCASE.

  PERFORM DATASET USING VL_CHOW
                        VL_DSN.

ENDFORM.                    " F_BAJADA_SERVIDOR
*&---------------------------------------------------------------------*
*&      Form  F_BORRO_TABLA_PROVEEDORES
*&---------------------------------------------------------------------*
*FORM F_BORRO_TABLA_PROVEEDORES  USING    PI_MENSAJE_E TYPE ZFIYT_EXCLUSION.
*
*  DATA: STL_REPROCESO  TYPE ZFIYT_EXCLUSION.
*  CLEAR:   T_EXCLUSION.
*  REFRESH: T_EXCLUSION.
*
*  SELECT *
*  FROM ZFIYT_EXCLUSION
*  INTO TABLE T_REPROCESO
*  WHERE BUKRS     EQ PI_MENSAJE_E-BUKRS
*    AND LIFNR     EQ PI_MENSAJE_E-LIFNR
*    AND INDICADOR EQ PI_MENSAJE_E-INDICADOR
*    AND  TIPO     EQ 'E'.
*  IF SY-SUBRC EQ 0.
*    LOOP AT T_REPROCESO INTO STL_REPROCESO.
*      DELETE FROM ZFIYT_EXCLUSION
*      WHERE BUKRS     EQ PI_MENSAJE_E-BUKRS
*        AND LIFNR     EQ PI_MENSAJE_E-LIFNR
*        AND INDICADOR EQ PI_MENSAJE_E-INDICADOR.
*      IF SY-SUBRC EQ 0.
*        COMMIT WORK AND WAIT.
*      ELSE.
*        ROLLBACK WORK .
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*ENDFORM.                    " F_BORRO_TABLA_PROVEEDORES
*&---------------------------------------------------------------------*
*&      Form  F_F4_SERVER_FILE
*&---------------------------------------------------------------------*
FORM F_F4_SERVER_FILE.

  CONCATENATE P_PATH 'Exclusiones\a_procesar\'
         INTO P_PATH.

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
   EXPORTING
     DIRECTORY              = P_PATH
*     FILEMASK              = ' '
   IMPORTING
     SERVERFILE             = P_PATH
   EXCEPTIONS
     CANCELED_BY_USER       = 1
     OTHERS                 = 2
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_F4_SERVER_FILE

*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_INITIALIZATION CHANGING P_P_PATH TYPE LOCALFILE
                               P_V_PATH TYPE LOCALFILE.

* Se obtiene la ruta inicial del servidor
  SELECT SINGLE PATHSERVER
    INTO P_V_PATH
    FROM ZBCY_AFIP
    WHERE REPID EQ SY-REPID.

* Se muestra por default la ruta del servidor
  P_P_PATH = P_V_PATH.

ENDFORM.                    " F_INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  F_TIPO_ARCHIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_TIPO_ARCHIVO CHANGING P_ARCHIVO TYPE CHAR5.

  IF P_GCIAS EQ 'X'.
    MOVE 'GCIAS' TO P_ARCHIVO.
  ELSEIF P_SUSS EQ 'X'.
    MOVE 'SUSS' TO P_ARCHIVO.
  ELSEIF P_IVA EQ 'X'.
    MOVE 'IVA' TO P_ARCHIVO.
  ELSEIF P_G2681 EQ 'X'.
    MOVE 'G2681' TO P_ARCHIVO.
  ENDIF.

ENDFORM.                    " F_TIPO_ARCHIVO

*&---------------------------------------------------------------------*
*&      Form  F_CONVERTIR_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CONVERTIR_DATOS USING P_SMS     TYPE CHAR1
                             P_ARCHIVO TYPE CHAR5.

  IF P_SMS EQ C_X.
    CLEAR P_SMS.
* Incluído en 06.12.2010 - Diego
    WRITE: /05 TEXT-E02.
* Fin 06.12.2010 ...............................
    MESSAGE TEXT-E02 TYPE 'S' DISPLAY LIKE 'E' .
  ELSE.
    CASE P_ARCHIVO.
      WHEN 'GCIAS'.
        PERFORM F_FORMAT_FILE USING P_ARCHIVO.
      WHEN 'G2681'.
        PERFORM F_FORMAT_FILE USING P_ARCHIVO.
      WHEN 'SUSS'.
        PERFORM F_FORMAT_FILE USING P_ARCHIVO.
      WHEN 'IVA'.
        PERFORM F_FORMAT_FILE USING P_ARCHIVO.
    ENDCASE.
  ENDIF.

ENDFORM.                    " F_CONVERTIR_DATOS

*&---------------------------------------------------------------------*
*&      Form  F_batch_INPUT_VK02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_BATCH_INPUT_VK02 .

  IF T_DATA[] IS INITIAL.
* Incluído en 06.12.2010 - Diego
    WRITE: /05 TEXT-E04.
* Fin 06.12.2010 ...............................
    MESSAGE TEXT-E04 TYPE 'S' DISPLAY LIKE 'E' .
  ELSE.
    PERFORM F_BATCH_INPUT.
  ENDIF.

ENDFORM.                    " F_batch_INPUT_VK02

*&---------------------------------------------------------------------*
*&      Form  F_BAJAR_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_BAJAR_RESULT .

  DATA: TL_MENSAJE_E TYPE STANDARD TABLE OF ZFIYT_EXCLUSION.

  CHECK T_MENSAJE_E IS NOT INITIAL.

* Dejo solo los proveedores con errores.
*  LOOP AT T_MENSAJE_E INTO ST_MENSAJE_E
*  WHERE TIPO EQ 'S'.
*-------Cuando se reprocesa, Buscamos que los archivos modificados se borren de la tabla z
*    PERFORM F_BORRO_TABLA_PROVEEDORES USING ST_MENSAJE_E.
*  ENDLOOP.

  REFRESH: T_IVA_E, T_SUSS_E, T_GCIAS_E.
  LOOP AT T_MENSAJE_E INTO ST_MENSAJE_E WHERE TIPO EQ 'S'.
    CLEAR ST_BAJADA.
    CASE V_ARCHIVO.
      WHEN 'GCIAS'.
        CONCATENATE ST_MENSAJE_E-BUKRS
                    ST_MENSAJE_E-LIFNR
                    ST_MENSAJE_E-FECHA
                    ST_MENSAJE_E-HORA
                    ST_MENSAJE_E-INDICADOR
                    ST_MENSAJE_E-TEXTO
                    ST_MENSAJE_E-TIPO
                    ST_MENSAJE_E-STCD1
                    ST_MENSAJE_E-NAME1
             INTO   ST_BAJADA-LINEA  SEPARATED BY ';'.
        APPEND ST_BAJADA TO T_GCIAS_E.

      WHEN 'G2681'.
        CONCATENATE ST_MENSAJE_E-BUKRS
                    ST_MENSAJE_E-LIFNR
                    ST_MENSAJE_E-FECHA
                    ST_MENSAJE_E-HORA
                    ST_MENSAJE_E-INDICADOR
                    ST_MENSAJE_E-TEXTO
                    ST_MENSAJE_E-TIPO
                    ST_MENSAJE_E-STCD1
                    ST_MENSAJE_E-NAME1
             INTO   ST_BAJADA-LINEA  SEPARATED BY ';'.
        APPEND ST_BAJADA TO T_GCIAS_E.

      WHEN 'IVA'.
        CONCATENATE ST_MENSAJE_E-BUKRS
                    ST_MENSAJE_E-LIFNR
                    ST_MENSAJE_E-FECHA
                    ST_MENSAJE_E-HORA
                    ST_MENSAJE_E-INDICADOR
                    ST_MENSAJE_E-TEXTO
                    ST_MENSAJE_E-TIPO
                    ST_MENSAJE_E-STCD1
                    ST_MENSAJE_E-NAME1
             INTO   ST_BAJADA-LINEA  SEPARATED BY ';'.
        APPEND ST_BAJADA TO T_IVA_E.
      WHEN 'SUSS'.
        CONCATENATE ST_MENSAJE_E-BUKRS
                    ST_MENSAJE_E-LIFNR
                    ST_MENSAJE_E-FECHA
                    ST_MENSAJE_E-HORA
                    ST_MENSAJE_E-INDICADOR
                    ST_MENSAJE_E-TEXTO
                    ST_MENSAJE_E-TIPO
                    ST_MENSAJE_E-STCD1
                    ST_MENSAJE_E-NAME1
             INTO   ST_BAJADA-LINEA  SEPARATED BY ';'.
        APPEND ST_BAJADA TO T_SUSS_E.
    ENDCASE.
  ENDLOOP.
*-------Bajo archivo sin errores
  PERFORM F_BAJADA_SERVIDOR USING V_ARCHIVO 'S'.


  REFRESH: T_IVA_E, T_SUSS_E, T_GCIAS_E.
  LOOP AT T_MENSAJE_E INTO ST_MENSAJE_E WHERE TIPO EQ 'E'.
    CLEAR ST_BAJADA.
    CASE V_ARCHIVO.
      WHEN 'GCIAS'.
        CONCATENATE ST_MENSAJE_E-BUKRS
                    ST_MENSAJE_E-LIFNR
                    ST_MENSAJE_E-FECHA
                    ST_MENSAJE_E-HORA
                    ST_MENSAJE_E-INDICADOR
                    ST_MENSAJE_E-TEXTO
                    ST_MENSAJE_E-TIPO
                    ST_MENSAJE_E-STCD1
                    ST_MENSAJE_E-NAME1
             INTO   ST_BAJADA-LINEA  SEPARATED BY ';'.
        APPEND ST_BAJADA TO T_GCIAS_E.
      WHEN 'IVA'.
        CONCATENATE ST_MENSAJE_E-BUKRS
                    ST_MENSAJE_E-LIFNR
                    ST_MENSAJE_E-FECHA
                    ST_MENSAJE_E-HORA
                    ST_MENSAJE_E-INDICADOR
                    ST_MENSAJE_E-TEXTO
                    ST_MENSAJE_E-TIPO
                    ST_MENSAJE_E-STCD1
                    ST_MENSAJE_E-NAME1
             INTO   ST_BAJADA-LINEA  SEPARATED BY ';'.
        APPEND ST_BAJADA TO T_IVA_E.
      WHEN 'SUSS'.
        CONCATENATE ST_MENSAJE_E-BUKRS
                    ST_MENSAJE_E-LIFNR
                    ST_MENSAJE_E-FECHA
                    ST_MENSAJE_E-HORA
                    ST_MENSAJE_E-INDICADOR
                    ST_MENSAJE_E-TEXTO
                    ST_MENSAJE_E-TIPO
                    ST_MENSAJE_E-STCD1
                    ST_MENSAJE_E-NAME1
             INTO   ST_BAJADA-LINEA  SEPARATED BY ';'.
        APPEND ST_BAJADA TO T_SUSS_E.
    ENDCASE.
  ENDLOOP.

*-------Bajo archivo con errores
  PERFORM F_BAJADA_SERVIDOR USING V_ARCHIVO 'E'.

  LOOP AT T_MENSAJE_E INTO ST_MENSAJE_E
  WHERE TIPO NE 'S'.
*-------Cuando se reprosesa, Buscamos que lo archivos modificado se borren de la tabla z
    APPEND ST_MENSAJE_E TO TL_MENSAJE_E.
  ENDLOOP.

*  INSERT ZFIYT_EXCLUSION FROM TABLE TL_MENSAJE_E .
*  IF SY-SUBRC EQ 0.
*    COMMIT WORK AND WAIT.
*  ELSE.
*    ROLLBACK WORK.
*  ENDIF.

ENDFORM.                    " F_BAJAR_RESULT

*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_INDICADORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_VALIDAR_INDICADORES  USING PI_INDICADOR
                                  STL_LFA1 TYPE TY_LFA1
                            CHANGING P_NOPROC.

  CLEAR P_NOPROC.

  CASE PI_INDICADOR.
    WHEN 'SUSS'.
      IF ( STL_LFA1-CO IS INITIAL
      AND  STL_LFA1-EG IS INITIAL
      AND  STL_LFA1-EE IS INITIAL
      AND  STL_LFA1-EL IS INITIAL
      AND  STL_LFA1-ES IS INITIAL ).
        PERFORM F_MENSAJES_ERROR USING STL_LFA1 TEXT-E03.
        P_NOPROC = 'X'.
      ENDIF.
    WHEN 'IVA'.
      IF  STL_LFA1-IV IS INITIAL.
        PERFORM F_MENSAJES_ERROR USING STL_LFA1 TEXT-E03.
        P_NOPROC = 'X'.
      ENDIF.
    WHEN 'GCIAS'.
      IF ( STL_LFA1-G1 IS INITIAL
     AND   STL_LFA1-G2 IS INITIAL ).
        PERFORM F_MENSAJES_ERROR USING STL_LFA1 TEXT-E03.
        P_NOPROC = 'X'.
      ENDIF.
  ENDCASE.

ENDFORM.                    " F_VALIDAR_INDICADORES

*&---------------------------------------------------------------------*
*&      Form  F_GENERAR_DATOS_batch
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GENERAR_DATOS_BATCH USING P_SMS     TYPE CHAR1
                                P_ARCHIVO TYPE CHAR5.

  IF P_SMS EQ C_X.
    CLEAR P_SMS.
* Incluído en 06.12.2010 - Diego
    WRITE: /05 TEXT-E02.
* Fin 06.12.2010 ...............................
    MESSAGE TEXT-E02 TYPE 'S' DISPLAY LIKE 'E' .
  ELSE.
    CASE P_ARCHIVO.
      WHEN 'GCIAS'.
        PERFORM F_ARMO_TABLA_BATCH_GANANCIA.
      WHEN 'G2681'.
        PERFORM F_ARMO_TABLA_BATCH_G2681.
      WHEN 'SUSS'.
        PERFORM F_ARMO_TABLA_BATCH_SUSS.
      WHEN 'IVA'.
        PERFORM F_ARMO_TABLA_BATCH_IVA.
    ENDCASE.
  ENDIF.

ENDFORM.                    " F_GENERAR_DATOS_batch

*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_PROV_SUSS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_VALIDAR_PROV_SUSS  USING    P_STCD1       TYPE STCD1
                          CHANGING P_AGREGAR_MSG TYPE CHAR1.

  READ TABLE T_SUSS
    WITH KEY CUIT = P_STCD1
    TRANSPORTING NO FIELDS.

  IF SY-SUBRC <> 0.
    P_AGREGAR_MSG = 'X'.
  ELSE.
    CLEAR P_AGREGAR_MSG.
  ENDIF.

ENDFORM.                    " F_VALIDAR_PROV_SUSS

*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_PROV_GCIAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_VALIDAR_PROV_GCIAS USING    P_STCD1       TYPE STCD1
                          CHANGING P_AGREGAR_MSG TYPE CHAR1.

  READ TABLE T_GCIAS
    WITH KEY CUIT = P_STCD1
    TRANSPORTING NO FIELDS.

  IF SY-SUBRC <> 0.
    P_AGREGAR_MSG = 'X'.
  ELSE.
    CLEAR P_AGREGAR_MSG.
  ENDIF.

ENDFORM.                    " F_VALIDAR_PROV_GCIAS

*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_PROV_IVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_VALIDAR_PROV_IVA USING    P_STCD1       TYPE STCD1
                        CHANGING P_AGREGAR_MSG TYPE CHAR1.

  READ TABLE T_IVA
    WITH KEY CUIT = P_STCD1
    TRANSPORTING NO FIELDS.

  IF SY-SUBRC <> 0.
    P_AGREGAR_MSG = 'X'.
  ELSE.
    CLEAR P_AGREGAR_MSG.
  ENDIF.

ENDFORM.                    " F_VALIDAR_PROV_IVA
*&---------------------------------------------------------------------*
*&      Form  F_ACTUALIZA_FECHA_GS02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ACTUALIZA_FECHA_GS02 .

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

ENDFORM.                    " F_ACTUALIZA_FECHA_GS02
