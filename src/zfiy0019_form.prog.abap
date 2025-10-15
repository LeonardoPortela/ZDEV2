*&---------------------------------------------------------------------*
*&  Include           ZFIY0019_FORM
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
FORM F_FORMAT_FILE.

  CONSTANTS: C_SEP TYPE C VALUE ';'.

  LOOP AT T_OUT_AUX INTO ST_OUT_AUX.

    CONDENSE ST_OUT_AUX NO-GAPS.
    SPLIT ST_OUT_AUX-LINEA AT C_SEP INTO ST_RECORD-F01
                                         ST_RECORD-F02
                                         ST_RECORD-F03
                                         ST_RECORD-F04
                                         ST_RECORD-F05
                                         ST_RECORD-F06
                                         ST_RECORD-F07
                                         ST_RECORD-F08
                                         ST_RECORD-F09
                                         ST_RECORD-F10
                                         ST_RECORD-F11
                                         ST_RECORD-F12
                                         ST_RECORD-F13.

    APPEND ST_RECORD TO T_RECORD.

  ENDLOOP.

*  ENDIF.

ENDFORM.                    " F_FORMAT_FILE

*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD
*&---------------------------------------------------------------------*
FORM F_UPLOAD USING P_WT_SUBIDA TYPE RLGRAP-FILENAME.

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


ENDFORM.                    " F_UPLOAD

*&---------------------------------------------------------------------*
*&      Form  F_SELE_LFA1
*&---------------------------------------------------------------------*
FORM F_SELE_LFA1.

*  Busco los proveedores.

  SELECT STCD1 LIFNR NAME1 KTOKK BRSCH
    FROM LFA1
    INTO TABLE T_LFA1
    WHERE KTOKK IN R_KTOKK
      AND BRSCH IN R_BRSCH.
  CHECK SY-SUBRC EQ 0.
  SORT  T_LFA1 BY LIFNR.

* BUsco la sociedad que esta cada proveedor.
  SELECT LIFNR BUKRS
    FROM LFB1
    INTO TABLE T_LFB1
    FOR ALL ENTRIES IN T_LFA1
    WHERE LIFNR EQ T_LFA1-LIFNR
      AND BUKRS IN SO_BUKRS.

  SORT T_LFB1 BY LIFNR BUKRS.

* Busco los impuestos del cliente
  PERFORM F_SEL_LFBW.

* Armo la tabla con todo los datos del proveedor.
  LOOP AT T_LFB1 INTO ST_LFB1.

    LOOP AT T_LFA1 INTO ST_LFA1 WHERE LIFNR = ST_LFB1-LIFNR.

      ST_LFA1-BUKRS = ST_LFB1-BUKRS.

      READ TABLE T_RECORD INTO ST_RECORD
      WITH KEY F01 = ST_LFA1-STCD1.

      IF SY-SUBRC IS INITIAL.

        APPEND ST_LFA1 TO TL_LFA1.

*     Indico si tiene indicador de ganancia el proveedor
        LOOP AT T_LFBW INTO ST_LFBW
        WHERE LIFNR = ST_LFB1-LIFNR
          AND BUKRS = ST_LFB1-BUKRS.

          CASE ST_RECORD-F04.
            WHEN ''. "Arquivo de 3 colunas
              IF ST_LFBW-WITHT EQ 'BP'.
                ST_LFA1-BP = C_X.
              ENDIF.
            WHEN C_EXC.
* Ganancias
              IF ST_LFBW-WITHT EQ C_GB.
*                ST_LFA1-G2 = C_X.
*             ENDIF

                IF ST_LFA1-KTOKK EQ C_YCER.
                  IF ST_LFA1-BRSCH EQ C_100  AND
                ( ST_RECORD-F03 EQ C_CONT OR
                  ST_RECORD-F03 EQ C_PROD ).
                    ST_LFA1-G2 = C_X.
                  ELSEIF ST_LFA1-BRSCH EQ C_103  AND
                       ( ST_RECORD-F03 EQ C_ACOP OR
                         ST_RECORD-F03 EQ C_OTRO OR
                         ST_RECORD-F03 EQ C_PROV ).
                    ST_LFA1-G2 = C_X.
                  ELSEIF ST_LFA1-BRSCH EQ C_102  AND
                         ST_RECORD-F03 EQ C_ACOP.
                    ST_LFA1-G2 = C_X.
                  ENDIF.
                ELSEIF ST_LFA1-KTOKK EQ C_YCOR.
                  IF ST_LFA1-BRSCH EQ C_101  AND
                     ST_RECORD-F03 EQ C_CORR.
                    ST_LFA1-G2 = C_X.
                  ELSEIF ST_LFA1-BRSCH EQ C_102  AND
                    ST_RECORD-F03 EQ C_CORR.
                    ST_LFA1-G2 = C_X.
                  ENDIF.
                ENDIF.

              ENDIF.
* IVA
              " Cambios 06.11.2012
              IF ST_LFBW-WITHT EQ C_IW.
*               ST_LFA1-I2 = C_X.
*             ENDIF.

                IF ST_LFA1-KTOKK EQ C_YCER.
                  IF ST_LFA1-BRSCH EQ C_100  AND
                ( ST_RECORD-F03 EQ C_CONT OR
                  ST_RECORD-F03 EQ C_PROD ).
                    ST_LFA1-I2 = C_X.
                  ELSEIF ST_LFA1-BRSCH EQ C_103  AND
                       ( ST_RECORD-F03 EQ C_ACOP OR
                         ST_RECORD-F03 EQ C_OTRO OR
                         ST_RECORD-F03 EQ C_PROV ).
                    ST_LFA1-I2 = C_X.
                  ELSEIF ST_LFA1-BRSCH EQ C_102  AND
                         ST_RECORD-F03 EQ C_ACOP.
                    ST_LFA1-I2 = C_X.
                  ENDIF.
                ELSEIF ST_LFA1-KTOKK EQ C_YCOR.
                  IF ST_LFA1-BRSCH EQ C_101  AND
                     ST_RECORD-F03 EQ C_CORR.
                    ST_LFA1-I2 = C_X.
                  ELSEIF ST_LFA1-BRSCH EQ C_102  AND
                    ST_RECORD-F03 EQ C_CORR.
                    ST_LFA1-I2 = C_X.
                  ENDIF.
                ENDIF.

              ENDIF.
              " Fin Cambios 06.11.2012

            WHEN C_ACT OR C_SUS.
* Ganancias
              IF ST_LFBW-WITHT EQ C_GB.
                IF ST_LFA1-KTOKK EQ C_YCER.
                  IF ST_LFA1-BRSCH EQ C_100  AND
                ( ST_RECORD-F03 EQ C_CONT OR
                  ST_RECORD-F03 EQ C_PROD ).
                    ST_LFA1-G1 = C_X.
                  ELSEIF ST_LFA1-BRSCH EQ C_103  AND
                       ( ST_RECORD-F03 EQ C_ACOP OR
                         ST_RECORD-F03 EQ C_OTRO OR
                         ST_RECORD-F03 EQ C_PROV ).
                    ST_LFA1-G4 = C_X.
                  ELSEIF ST_LFA1-BRSCH EQ C_102  AND
                         ST_RECORD-F03 EQ C_ACOP.
                    ST_LFA1-G4 = C_X.
                  ENDIF.
                ELSEIF ST_LFA1-KTOKK EQ C_YCOR.
                  IF ST_LFA1-BRSCH EQ C_101  AND
                     ST_RECORD-F03 EQ C_CORR.
                    ST_LFA1-G3 = C_X.
                  ELSEIF ST_LFA1-BRSCH EQ C_102  AND
                    ST_RECORD-F03 EQ C_CORR.
                    ST_LFA1-G3 = C_X.
                  ENDIF.
                ENDIF.
              ENDIF.
* IVA
              IF ST_LFBW-WITHT EQ C_IW.

                IF ST_RECORD-F04 EQ C_ACT.
                  " Cambios 06.11.2012
*                  ST_LFA1-I1 = C_X.
*               ELSE.
*                  ST_LFA1-I2 = C_X.
*               ENDIF.
                  IF ST_LFA1-KTOKK EQ C_YCER.
                    IF ST_LFA1-BRSCH EQ C_100  AND
                  ( ST_RECORD-F03 EQ C_CONT OR
                    ST_RECORD-F03 EQ C_PROD ).
                      ST_LFA1-I1 = C_X.
                    ELSEIF ST_LFA1-BRSCH EQ C_103  AND
                         ( ST_RECORD-F03 EQ C_ACOP OR
                           ST_RECORD-F03 EQ C_OTRO OR
                           ST_RECORD-F03 EQ C_PROV ).
                      ST_LFA1-I1 = C_X.
                    ELSEIF ST_LFA1-BRSCH EQ C_102  AND
                           ST_RECORD-F03 EQ C_ACOP.
                      ST_LFA1-I1 = C_X.
                    ENDIF.
                  ELSEIF ST_LFA1-KTOKK EQ C_YCOR.
                    IF ST_LFA1-BRSCH EQ C_101  AND
                       ST_RECORD-F03 EQ C_CORR.
                      ST_LFA1-I1 = C_X.
                    ELSEIF ST_LFA1-BRSCH EQ C_102  AND
                      ST_RECORD-F03 EQ C_CORR.
                      ST_LFA1-I1 = C_X.
                    ENDIF.
                  ENDIF.

                ELSEIF ST_RECORD-F04 EQ C_SUS.

                  IF ST_LFA1-KTOKK EQ C_YCER.
                    IF ST_LFA1-BRSCH EQ C_100  AND
                  ( ST_RECORD-F03 EQ C_CONT OR
                    ST_RECORD-F03 EQ C_PROD ).
                      ST_LFA1-I2 = C_X.
                    ELSEIF ST_LFA1-BRSCH EQ C_103  AND
                         ( ST_RECORD-F03 EQ C_ACOP OR
                           ST_RECORD-F03 EQ C_OTRO OR
                           ST_RECORD-F03 EQ C_PROV ).
                      ST_LFA1-I2 = C_X.
                    ELSEIF ST_LFA1-BRSCH EQ C_102  AND
                           ST_RECORD-F03 EQ C_ACOP.
                      ST_LFA1-I2 = C_X.
                    ENDIF.
                  ELSEIF ST_LFA1-KTOKK EQ C_YCOR.
                    IF ST_LFA1-BRSCH EQ C_101  AND
                       ST_RECORD-F03 EQ C_CORR.
                      ST_LFA1-I2 = C_X.
                    ELSEIF ST_LFA1-BRSCH EQ C_102  AND
                      ST_RECORD-F03 EQ C_CORR.
                      ST_LFA1-I2 = C_X.
                    ENDIF.
                  ENDIF.

                ENDIF.

              ENDIF.
              " Fin Cambios 06.11.2012
            WHEN OTHERS.
          ENDCASE.

          MODIFY TL_LFA1 FROM ST_LFA1
          TRANSPORTING G1 G2 G3 G4 I1 I2 BP
          WHERE STCD1 EQ ST_LFA1-STCD1.

        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " F_SELE_LFA1

*&---------------------------------------------------------------------*
*&      Form  F_ARMO_TABLA_BATCH
*&---------------------------------------------------------------------*
FORM F_ARMO_TABLA_BATCH.

  DATA: VL_WITHT TYPE WITHT,
        VL_STCD1 TYPE STCD1.

  LOOP AT T_RECORD INTO ST_RECORD.

*   Validacion de proveedor
    LOOP AT TL_LFA1 INTO ST_LFA1 WHERE STCD1 = ST_RECORD-F01.
*     Completa la tabla que vamos a usar en el batch input.
      CLEAR ST_DATA.

      ST_DATA-BUKRS = ST_LFA1-BUKRS.
      ST_DATA-LIFNR = ST_LFA1-LIFNR.
      ST_DATA-CATEGORIA = ST_RECORD-F03.
      ST_DATA-SITUACION = ST_RECORD-F04.

      IF ST_LFA1-BP EQ C_X.
        ST_DATA-WITHT = 'BP'.
        IF '1_2_3_4_5' CS ST_RECORD-F02+0(1) .
          ST_DATA-WT_WITHCD = 'B1'.
          APPEND ST_DATA TO T_DATA.
        ELSEIF ST_RECORD-F02+0(1) = '6'.
          ST_DATA-WT_WITHCD = 'B7'.
          APPEND ST_DATA TO T_DATA.
        ENDIF.
      ENDIF.

      IF ST_LFA1-G1 EQ C_X.
        ST_DATA-WITHT = C_GB.
        ST_DATA-WT_WITHCD = C_G1.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-G2 EQ C_X.
        ST_DATA-WITHT = C_GB.
        ST_DATA-WT_WITHCD = C_G2.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-G3 EQ C_X.
        ST_DATA-WITHT = C_GB.
        ST_DATA-WT_WITHCD = C_G3.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-G4 EQ C_X.
        ST_DATA-WITHT = C_GB.
        ST_DATA-WT_WITHCD = C_G4.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-I1 EQ C_X.
        ST_DATA-WITHT = C_IW.
        ST_DATA-WT_WITHCD = C_I1.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

      IF ST_LFA1-I2 EQ C_X.
        ST_DATA-WITHT = C_IW.
        ST_DATA-WT_WITHCD = C_I2.
        APPEND ST_DATA TO T_DATA.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " F_ARMO_TABLA_BATCH
*&---------------------------------------------------------------------*
*&      Form  F_INICIALIZO_TABLAS
*&---------------------------------------------------------------------*
FORM F_INICIALIZO_TABLAS .

  DATA VL_FECHA TYPE SY-DATUM.
  CLEAR: ST_DATA,
         ST_OUTPUT,
         ST_RECORD,
         ST_DOWNLOAD,
         ST_LFA1,
         ST_LFB1,
         ST_LFBW,
         V_SMS.

  REFRESH: T_RECORD,
           T_DOWNLOAD,
           T_OUTPUT,
           T_LFA1,
           T_LFBW,
           T_LFB1,
           T_DATA.

ENDFORM.                    " F_INICIALIZO_TABLAS

*&---------------------------------------------------------------------*
*&      Form  F_SEL_LFBW
*&---------------------------------------------------------------------*
FORM F_SEL_LFBW.

  REFRESH T_LFBW.

  SELECT *
    FROM LFBW
    INTO TABLE T_LFBW
    FOR ALL ENTRIES IN T_LFB1
    WHERE LIFNR EQ T_LFB1-LIFNR " Proveedor
      AND BUKRS EQ T_LFB1-BUKRS " Sociedad
      AND WITHT IN SO_WITHT.    "indicador de impuesto

ENDFORM.                    " F_SEL_LFBW


*&---------------------------------------------------------------------*
*&      Form  F_BATCH_INPUT
*&---------------------------------------------------------------------*
FORM F_BATCH_INPUT .

  DATA: VL_MODO  TYPE CHAR1 VALUE 'N',
        VL_LIFNR TYPE LIFNR,
        VL_TEXTO TYPE CHAR200.

  LOOP AT T_DATA INTO ST_DATA.

* Se reinician las tablas para realizar el bach input
    REFRESH: T_BDCDATA,
             T_MESSTAB.
    CLEAR:   T_BDCDATA,
             T_MESSTAB.

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
                              'LFBW-WITHT(01)'     ST_DATA-WITHT,
                              'LFBW-WT_WITHCD(01)' ST_DATA-WT_WITHCD.

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

    READ TABLE T_MESSTAB
      WITH KEY MSGTYP = 'S'.
    IF SY-SUBRC IS INITIAL.

* Se obtienen los datos del proveedor
      READ TABLE T_LFA1 INTO ST_LFA1
      WITH KEY LIFNR = ST_DATA-LIFNR.

* Se cargan los datos en la salida
      ST_OUTPUT-LIFNR = ST_DATA-LIFNR.
      ST_OUTPUT-NAME1 = ST_LFA1-NAME1.
      ST_OUTPUT-STCD1 = ST_LFA1-STCD1.
      ST_OUTPUT-CATEGORIA = ST_DATA-CATEGORIA.
      ST_OUTPUT-SITUACION = ST_DATA-SITUACION.
      ST_OUTPUT-KTOKK = ST_LFA1-KTOKK.
      ST_OUTPUT-BRSCH = ST_LFA1-BRSCH.
      ST_OUTPUT-WITHT     = ST_DATA-WITHT.
      ST_OUTPUT-WT_WITHCD = ST_DATA-WT_WITHCD.

      APPEND ST_OUTPUT TO T_OUTPUT.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_BACH_INPUT
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

    IF SCREEN-NAME = 'P_PATH'.
      SCREEN-INPUT = '0'.
    ENDIF.
    IF SCREEN-NAME = 'P_CHECK'.
      SCREEN-INPUT = '1'.
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
