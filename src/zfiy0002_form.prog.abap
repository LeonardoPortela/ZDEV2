*&---------------------------------------------------------------------*
*&  Include           ZFIY0002_FORM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  KD_GET_FILENAME_ON_F4
*&---------------------------------------------------------------------*
FORM KD_GET_FILENAME_ON_F4 CHANGING PO_PATH.
  DATA: LC_PERIODO(7).

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      FILE_NAME = PO_PATH.
ENDFORM. " KD_GET_FILENAME_ON_F4


*&---------------------------------------------------------------------*
*&      Form  F_SUBIDA_TXT
*&---------------------------------------------------------------------*
FORM F_SUBIDA_TXT USING P_WT_SUBIDA TYPE RLGRAP-FILENAME.
  DATA: LV_PATH TYPE STRING.

  IF P_PATH IS NOT INITIAL.
    LV_PATH = P_PATH.
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

ENDFORM. " F_SUBIDA_TXT

*&---------------------------------------------------------------------*
*&      Form  F_SUBIDA_ARCHIVO_SERVIDOR
*&---------------------------------------------------------------------*
FORM F_SUBIDA_ARCHIVO_SERVIDOR USING PI_PATH.

  DATA: VL_LENG  TYPE I,
        VL_FNAME TYPE CHAR200,
        VL_DSN   TYPE CHAR200,
        VL_LINE  TYPE  STRING.

  DATA: VL_RUTA      TYPE CHAR100  VALUE '/Finanzas/Padrones_Arba/'.

  IF R_CAP EQ 'X'.
    CLEAR: VL_DSN.
    CONCATENATE P_PATH VL_RUTA
                'Padrones_Reducidos_CABA_IIBB/Datos_Correctos'
                '.txt'
    INTO VL_DSN.
  ELSE.
    CLEAR: VL_DSN.
    CONCATENATE P_PATH VL_RUTA
                'Padrones_Reducidos_Arba_IIBB/Datos_Correctos'
                '.txt'
    INTO VL_DSN.
  ENDIF.


  OPEN DATASET VL_DSN FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  DO.

*  Copio los datos del archivo a una tabla
    READ DATASET VL_DSN INTO VL_LINE LENGTH VL_LENG.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSE.
      WRITE  VL_LINE TO  ST_OUT_AUX-LINE.
      APPEND ST_OUT_AUX TO  T_OUT_AUX.
    ENDIF.
  ENDDO.

  CLOSE DATASET VL_FNAME.

ENDFORM. " F_SUBIDA_ARCHIVO_SERVIDOR

*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_FILE
*&---------------------------------------------------------------------*
FORM F_FORMAT_FILE .
  DATA: W_SEP TYPE C VALUE ';'.
  DATA: W_INDICE TYPE SY-TABIX.

  CLEAR:  ST_DATA,
           T_DATA.

  LOOP AT T_OUT_AUX INTO ST_OUT_AUX.

    IF R_CAP EQ 'X'.

      CONDENSE ST_OUT_AUX NO-GAPS.
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
                                     ST_DATA-GRUPO
                                     ST_DATA-LIFNR
                                     ST_DATA-BUKRS.
*      IF ST_DATA-ARQ NE 'R'. "filtrar somente retenções
*        CONTINUE.
*      ENDIF.
      IF R_PROV EQ 'X'.
        PERFORM F_CONVERSION_GRUPOS USING ST_DATA-GRUPO  CHANGING ST_DATA-RET ST_DATA-RET_A.
      ELSE.
        ST_DATA-RET    = 'CB'.
        ST_DATA-RET_A  = ST_DATA-GRUPO.
        "PERFORM F_CONVERSION_CAP    USING ST_DATA-ALICU  CHANGING ST_DATA-RET ST_DATA-RET_A.
      ENDIF.
      IF V_ANS IS INITIAL.
        PERFORM F_POPUP_TO_DECIDE  USING ST_DATA-PUBLI CHANGING V_ANS.
        IF V_ANS EQ 1.
*          PERFORM FECHA_EJECUCION  USING ST_DATA-PUBLI CHANGING V_ANS.
        ENDIF.
      ENDIF.

*   Completo la tabla si acepta la fecha de publicacion
      IF V_ANS EQ 1.
        SELECT SINGLE LIFNR
          INTO ST_DATA-LIFNR
          FROM LFA1
          WHERE LIFNR EQ ST_DATA-LIFNR
            AND STKZA EQ SPACE.
        IF SY-SUBRC IS INITIAL.
          APPEND ST_DATA TO T_DATA.
        ENDIF.
      ELSE.
        MESSAGE TEXT-E01 TYPE 'S' DISPLAY LIKE 'E' .
        EXIT.
      ENDIF.

    ELSEIF R_PROV EQ 'X'.

      CONDENSE ST_OUT_AUX NO-GAPS.
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
                                     ST_DATA_ARBA-GRUPO
                                     ST_DATA_ARBA-LIFNR
                                     ST_DATA_ARBA-BUKRS.
      IF ST_DATA_ARBA-ARQ NE 'R'. "filtrar somente retenções
        CONTINUE.
      ENDIF.

*      PERFORM F_CONVERSION_GRUPOS USING ST_DATA_ARBA-GRUPO  CHANGING ST_DATA_ARBA-RET ST_DATA_ARBA-RET_A.

      " novos Indicadores "IB" 11/02/2019 ALRS
      ST_DATA_ARBA-RET   = 'IB'.
      ST_DATA_ARBA-RET_A = ST_DATA_ARBA-GRUPO.

      IF V_ANS IS INITIAL.
        PERFORM F_POPUP_TO_DECIDE  USING ST_DATA_ARBA-PUBLI CHANGING V_ANS.
        IF V_ANS EQ 1.
*          PERFORM FECHA_EJECUCION  USING ST_DATA_ARBA-PUBLI CHANGING V_ANS.
        ENDIF.
      ENDIF.

*   Completo la tabla si acepta la fecha de publicacion
      IF V_ANS EQ 1.
        SELECT SINGLE LIFNR
          INTO ST_DATA_ARBA-LIFNR
          FROM LFA1
          WHERE LIFNR EQ ST_DATA_ARBA-LIFNR
            AND STKZA EQ SPACE.
        IF SY-SUBRC IS INITIAL.
          APPEND ST_DATA_ARBA TO T_DATA_ARBA.
        ENDIF.
      ELSE.
        MESSAGE TEXT-E01 TYPE 'S' DISPLAY LIKE 'E' .
        EXIT.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM. " F_FORMAT_FILE


*&---------------------------------------------------------------------*
*&      Form  FECHA_EJECUCION
*&---------------------------------------------------------------------*
FORM FECHA_EJECUCION USING PI_PUBLI CHANGING PO_ANS.
  CLEAR:
          V_FECHA  ,
          V_VALFROM.
  DATA: VL_SET TYPE CHAR10.

  IF R_PROV EQ 'X'.
    MOVE 'ZIIBB_BSAS' TO VL_SET .
  ENDIF.
  IF R_CAP EQ 'X'.
    MOVE 'ZIIBB_CAP' TO VL_SET .
  ENDIF.

  CLEAR ST_SETLEAF.
  SELECT SINGLE *
           FROM SETLEAF CLIENT SPECIFIED
           INTO  ST_SETLEAF
           WHERE MANDT    = SY-MANDT
             AND SETCLASS = '0000'            "ZIIBB_BSAS'
             AND SETNAME  = VL_SET.

  IF SY-SUBRC EQ 0.
    CONCATENATE PI_PUBLI+4(4)
                PI_PUBLI+2(2)
                PI_PUBLI(2)
    INTO  V_FECHA .
    CONDENSE ST_SETLEAF-VALFROM NO-GAPS.
    MOVE ST_SETLEAF-VALFROM TO V_VALFROM.

    IF V_FECHA  EQ V_VALFROM.
      PO_ANS = '1'.
    ELSE.
      PO_ANS = '2'.
    ENDIF.
  ELSE.
    PO_ANS = '2'.
  ENDIF.

  IF PO_ANS EQ '2'.
    MESSAGE TEXT-E04 TYPE 'I' DISPLAY LIKE 'E' .
    EXIT.
  ENDIF.

ENDFORM. " FECHA_EJECUCION

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
ENDFORM. " F_POPUP_TO_DECIDE

*&---------------------------------------------------------------------*
*&      Form  F_LLENO_TABLA_T_MES_SAL
*&---------------------------------------------------------------------*
FORM F_LLENO_TABLA_T_MES_SAL USING PI_MODO
                                   PI_LIFNR
                                   PI_BUKRS
                                   PI_STCD1
                                   PI_SUBRC
                                   PI_RET.
  IF PI_SUBRC EQ 0.
    MOVE ICON_GREEN_LIGHT TO ST_MES-SEMAFORO.
  ELSE.
    MOVE ICON_RED_LIGHT   TO ST_MES-SEMAFORO.
  ENDIF.

  MOVE:
      PI_LIFNR        TO ST_MES-LIFNR,
      PI_BUKRS        TO ST_MES-BUKRS,
      PI_RET          TO ST_MES-RETD,
      PI_STCD1        TO ST_MES-STCD1 .

  CLEAR  ST_MES-RETA.
  IF R_PROV EQ 'X'.

* Prov. de Bs As
    LOOP AT T_LFBW INTO ST_LFBW
      WHERE LIFNR = PI_LIFNR
        AND BUKRS = PI_BUKRS.
      IF ST_LFBW-WITHT(1) = 'H'.
        ST_MES-RETA = ST_LFBW-WITHT.
        EXIT.
      ENDIF.
    ENDLOOP.

  ELSE.

** CABA
*    READ TABLE T_LFBW
*    INTO ST_LFBW
*    WITH KEY LIFNR = ST_DATA-LIFNR
*             WITHT = ST_DATA-RET.
*    IF SY-SUBRC = 0.
*
*    ENDIF.

  ENDIF.

  CASE PI_MODO.
    WHEN 'R'.
      ST_MES-MODO = 'NO SE HA ACTUALIZADO EL INDICADOR'.
      MOVE ICON_YELLOW_LIGHT TO ST_MES-SEMAFORO.
    WHEN 'A'.
      ST_MES-MODO = 'AGREGA INDICADOR'.
    WHEN 'M'.
      ST_MES-MODO = 'INDICADOR MODIFICA'.
    WHEN 'B'.
      ST_MES-MODO = 'INDICADOR BORRADO'.
  ENDCASE.
  APPEND ST_MES TO T_MES_SAL.

ENDFORM. " F_LLENO_TABLA_T_MES_SAL

*&---------------------------------------------------------------------*
*&      Form  F_SEL_LFBW
*&---------------------------------------------------------------------*
FORM F_SEL_LFBW.

  IF T_DATA[] IS NOT INITIAL.

    SELECT W~MANDT
           W~LIFNR
           W~BUKRS
           W~WITHT
           W~WT_SUBJCT
           W~QSREC
           W~WT_WTSTCD
           W~WT_WITHCD
           W~WT_EXNR
           W~WT_EXRT
           W~WT_EXDF
           W~WT_EXDT
           W~WT_WTEXRS
      FROM LFBW AS W
      INNER JOIN LFA1 AS P
              ON W~LIFNR = P~LIFNR
      INTO TABLE T_LFBW
      FOR ALL ENTRIES IN T_DATA
      WHERE W~LIFNR EQ T_DATA-LIFNR
        AND W~BUKRS EQ T_DATA-BUKRS
        AND P~STKZA NE 'X'.

  ELSEIF T_DATA_ARBA[] IS NOT INITIAL.

    SELECT W~MANDT
           W~LIFNR
           W~BUKRS
           W~WITHT
           W~WT_SUBJCT
           W~QSREC
           W~WT_WTSTCD
           W~WT_WITHCD
           W~WT_EXNR
           W~WT_EXRT
           W~WT_EXDF
           W~WT_EXDT
           W~WT_WTEXRS
      FROM LFBW AS W
      INNER JOIN LFA1 AS P
              ON W~LIFNR = P~LIFNR
      INTO TABLE T_LFBW
      FOR ALL ENTRIES IN T_DATA_ARBA
      WHERE W~LIFNR EQ T_DATA_ARBA-LIFNR
        AND W~BUKRS EQ T_DATA_ARBA-BUKRS
        AND P~STKZA NE 'X'.

  ENDIF.

ENDFORM. " F_SEL_LFBW

*&---------------------------------------------------------------------*
*&      Form  F_MODIFICO_RET_XK02
*&---------------------------------------------------------------------*
FORM F_MODIFICO_RET_XK02 USING SI_DATA TYPE TY_DATA_PAR
                               STL_LFBW TYPE TY_LFBW.

  DATA:  VL_SUBRC  TYPE SY-SUBRC,
         ST_LFBW   TYPE LFBW,
         T_LFBW_AX TYPE STANDARD TABLE OF TY_LFBW,
         TL_LFBW   TYPE STANDARD TABLE OF TY_LFBW,
         TL_LFBW_A TYPE STANDARD TABLE OF LFBW.

  CLEAR:
           VL_SUBRC ,
           T_LFBW_AX,
           TL_LFBW_A,
           STL_LFBW .

  REFRESH: TL_LFBW_A,
           T_LFBW_AX.

  IF R_PROV EQ 'X'.
    MOVE:   SY-MANDT      TO STL_LFBW-MANDT    ,
            SI_DATA-LIFNR TO STL_LFBW-LIFNR    ,
            SI_DATA-BUKRS TO STL_LFBW-BUKRS    ,
            SI_DATA-RET   TO STL_LFBW-WITHT    ,
            'X'           TO STL_LFBW-WT_SUBJCT,
            SI_DATA-RET   TO STL_LFBW-WT_WITHCD.

    APPEND  STL_LFBW TO TL_LFBW_A.

    SELECT *
    FROM LFBW
    INTO TABLE T_LFBW_AX
    WHERE LIFNR EQ STL_LFBW-LIFNR
      AND BUKRS EQ STL_LFBW-BUKRS.

    LOOP AT T_LFBW_AX INTO ST_LFBW.
      IF ST_LFBW-WITHT(1) EQ 'H'.
        DELETE FROM LFBW
        WHERE LIFNR EQ ST_LFBW-LIFNR
          AND BUKRS EQ ST_LFBW-BUKRS
          AND WITHT EQ ST_LFBW-WITHT.
        IF SY-SUBRC EQ 0.
          VL_SUBRC = SY-SUBRC.
          COMMIT WORK AND WAIT.
        ELSE.
          VL_SUBRC = SY-SUBRC.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ENDLOOP.

    INSERT   LFBW FROM TABLE  TL_LFBW_A.

    PERFORM F_COMMIT_WORK USING SY-SUBRC
                       CHANGING VL_SUBRC .

** Mensaje de error.
    PERFORM F_LLENO_TABLA_T_MES_SAL USING 'M'
                                          SI_DATA-LIFNR
                                          SI_DATA-BUKRS
                                          SI_DATA-STCD1
                                          VL_SUBRC
                                          SI_DATA-RET.

  ELSE.

    MOVE:   SY-MANDT      TO STL_LFBW-MANDT    ,
            SI_DATA-LIFNR TO STL_LFBW-LIFNR    ,
            SI_DATA-BUKRS TO STL_LFBW-BUKRS    ,
            SI_DATA-RET_A TO STL_LFBW-WITHT    ,
            'X'           TO STL_LFBW-WT_SUBJCT,
            SI_DATA-RET   TO STL_LFBW-WT_WITHCD.

    APPEND  STL_LFBW TO TL_LFBW_A.

    SELECT *
    FROM LFBW
    INTO TABLE T_LFBW_AX
    WHERE LIFNR EQ STL_LFBW-LIFNR
      AND BUKRS EQ STL_LFBW-BUKRS.

    LOOP AT T_LFBW_AX INTO ST_LFBW.
      IF ST_LFBW-WITHT EQ SI_DATA-RET_A .

*     Busco los datos que quiero actualizar
        READ TABLE TL_LFBW_A INTO STL_LFBW
        WITH KEY LIFNR = STL_LFBW-LIFNR
                 BUKRS = STL_LFBW-BUKRS.

        IF SY-SUBRC EQ 0.
          DELETE FROM LFBW
          WHERE LIFNR EQ STL_LFBW-LIFNR
            AND BUKRS EQ STL_LFBW-BUKRS
            AND WITHT EQ STL_LFBW-WITHT.

          PERFORM F_COMMIT_WORK USING SY-SUBRC
                       CHANGING VL_SUBRC .
        ENDIF.

      ENDIF.
    ENDLOOP.

    INSERT   LFBW FROM TABLE  TL_LFBW_A.
    CLEAR VL_SUBRC.
    PERFORM F_COMMIT_WORK USING SY-SUBRC
                       CHANGING VL_SUBRC .

** Mensaje de error.
    PERFORM F_LLENO_TABLA_T_MES_SAL USING 'M'
                                          SI_DATA-LIFNR
                                          SI_DATA-BUKRS
                                          SI_DATA-STCD1
                                          VL_SUBRC
                                          SI_DATA-RET.
  ENDIF.

ENDFORM. " F_MODIFICO_RET_XK02

*&---------------------------------------------------------------------*
*&      Form  F_BORRAR_RET_XK02
*&---------------------------------------------------------------------*
FORM F_BORRAR_RET_XK02 USING SI_DATA TYPE TY_DATA_PAR
                             STL_LFBW TYPE TY_LFBW.

  DATA:  VL_SUBRC   TYPE SY-SUBRC.

  CLEAR: VL_SUBRC,
         STL_LFBW.

  MOVE:   SI_DATA-LIFNR TO STL_LFBW-LIFNR,
          SI_DATA-RET   TO STL_LFBW-WITHT,
          SI_DATA-BUKRS TO STL_LFBW-BUKRS.

  DELETE FROM  LFBW
          WHERE LIFNR EQ STL_LFBW-LIFNR
            AND BUKRS EQ STL_LFBW-BUKRS
            AND WITHT EQ STL_LFBW-WITHT.

  IF SY-SUBRC EQ 0.
    VL_SUBRC = SY-SUBRC.
    COMMIT WORK AND WAIT.

  ELSE.
    VL_SUBRC = SY-SUBRC.
    ROLLBACK WORK.
  ENDIF.

** Mensaje de error.
  PERFORM F_LLENO_TABLA_T_MES_SAL USING 'B'
                                        SI_DATA-LIFNR
                                        SI_DATA-BUKRS
                                        SI_DATA-STCD1
                                        VL_SUBRC
                                        STL_LFBW-WITHT.
ENDFORM. " F_BORRAR_RET_XK02

*&---------------------------------------------------------------------*
*&      Form  F_BACH_XK02
*&---------------------------------------------------------------------*
FORM F_AGREGO_RET_XK02 USING SI_DATA TYPE TY_DATA_PAR.

  DATA: "tl_lfbw    TYPE STANDARD TABLE OF ty_lfbw   ,
    STL_LFBW TYPE TY_LFBW,
    VL_SUBRC TYPE SY-SUBRC.

  CLEAR:"tl_lfbw ,
         VL_SUBRC,
         STL_LFBW.

  STL_LFBW-MANDT = SY-MANDT.
  STL_LFBW-LIFNR = SI_DATA-LIFNR.
  STL_LFBW-BUKRS = SI_DATA-BUKRS.
  STL_LFBW-WITHT = SI_DATA-RET.
  STL_LFBW-WT_SUBJCT = 'X'.
  STL_LFBW-WT_WITHCD = SI_DATA-RET_A.

*  APPEND stl_lfbw TO tl_lfbw.

  INSERT LFBW FROM STL_LFBW.    "FROM TABLE tl_lfbw.

  IF SY-SUBRC EQ 0.
    VL_SUBRC = SY-SUBRC.
    COMMIT WORK AND WAIT.
  ELSE.
    VL_SUBRC = SY-SUBRC.
    ROLLBACK WORK.
  ENDIF.

  PERFORM F_LLENO_TABLA_T_MES_SAL USING 'A'
                                        SI_DATA-LIFNR
                                        SI_DATA-BUKRS
                                        SI_DATA-STCD1
                                        VL_SUBRC
                                        SI_DATA-RET.

ENDFORM. " f_agrego_RET_xk02

*&---------------------------------------------------------------------*
*&      Form  F_CONVERSION_GRUPOS
*&---------------------------------------------------------------------*
FORM F_CONVERSION_GRUPOS USING PI_RET
                          CHANGING PO_RET PO_RET_A.
  CASE PI_RET.
    WHEN '00'.
      PO_RET = 'H0'.
    WHEN '01'.
      PO_RET = 'H1'.
    WHEN '02'.
      PO_RET = 'H2'.
    WHEN '03'.
      PO_RET = 'H3'.
    WHEN '04'.
      PO_RET = 'H4'.
    WHEN '05'.
      PO_RET = 'H5'.
    WHEN '06'.
      PO_RET = 'H6'.
    WHEN '07'.
      PO_RET = 'H7'.
    WHEN '08'.
      PO_RET = 'H8'.
    WHEN '09'.
      PO_RET = 'H9'.
    WHEN '10'.
      PO_RET = 'HA'.
    WHEN '11'.
      PO_RET = 'HB'.
    WHEN '12'.
      PO_RET = 'HC'.
    WHEN '13'.
      PO_RET = 'HD'.
    WHEN '14'.
      PO_RET = 'HE'.
    WHEN '15'.
      PO_RET = 'HF'.
  ENDCASE.

  IF PO_RET IS NOT INITIAL.
    PO_RET_A = PO_RET.
  ENDIF.


ENDFORM. " F_CONVERSION_GRUPOS

*&---------------------------------------------------------------------*
*&      Form  F_ACTUALIZO_FECHA_GS02
*&---------------------------------------------------------------------*
FORM F_ACTUALIZO_FECHA_GS02 .

  V_FECHA = V_FECHA + 2.
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

  LOOP AT SCREEN.

    P_PATH = V_PATH.

    IF R1 EQ 'X'.
      IF SCREEN-NAME = 'P_PATH'.
        MOVE '0' TO SCREEN-INPUT.
      ENDIF.
    ELSE.
      IF SCREEN-NAME = 'P_PATH'.
        MOVE '1' TO SCREEN-INPUT.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM. " F_GRISADO_CAMPO_PANTALLA

*&---------------------------------------------------------------------*
*&      Form  F_CONVERSION_CAP
*&---------------------------------------------------------------------*
FORM F_CONVERSION_CAP USING PI_RET
                       CHANGING PO_RET PO_RET_A.
  DATA:   VL_QSATZ TYPE QSATZ,
          SL_T059Z TYPE T059Z.

*   Se formater el valor char a %
  SHIFT ST_DATA-ALICU  RIGHT DELETING TRAILING SPACE.

  DO 10 TIMES.
    REPLACE '.'   WITH ' ' INTO ST_DATA-ALICU.
    REPLACE ','   WITH ' ' INTO ST_DATA-ALICU.
  ENDDO.

  CONDENSE ST_DATA-ALICU  NO-GAPS.

  SHIFT ST_DATA-ALICU  RIGHT DELETING TRAILING ''.

  VL_QSATZ = ST_DATA-ALICU / 100.

*se busca el tipo de retención
  SELECT SINGLE *
  FROM T059Z
  INTO SL_T059Z
  WHERE WITHT EQ 'IO'
  AND   QSATZ EQ VL_QSATZ
  AND   LAND1 = 'AR'.

  IF SY-SUBRC = 0.
    PO_RET = 'IO'.
    PO_RET_A = SL_T059Z-WT_WITHCD.
  ENDIF.

ENDFORM. " F_CONVERSION_CAP

*&---------------------------------------------------------------------*
*&      Form  F_COMMIT_WORK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_COMMIT_WORK USING PI_SUBRC CHANGING PO_SUBRC.
  IF PI_SUBRC EQ 0.
    PO_SUBRC = SY-SUBRC.
    COMMIT WORK AND WAIT.

  ELSE.
    PO_SUBRC = SY-SUBRC.
    ROLLBACK WORK.
  ENDIF.
ENDFORM. " F_COMMIT_WORK


*&---------------------------------------------------------------------*
*&      Form  F_ABM_INDICADORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ABM_INDICADORES .

  DATA: VL_EXISTE_IND TYPE CHAR1.


  IF R_CAP EQ 'X'.

* Actualizo la tabla de los proveedores
    LOOP AT T_DATA INTO ST_DATA.

      IF ST_DATA-TIPO EQ 'B'.
*   Si es baja el proveedor y el indicador existen en el maestro
        READ TABLE T_LFBW
        INTO ST_LFBW
        WITH KEY LIFNR = ST_DATA-LIFNR
                 WITHT = ST_DATA-RET.

*     Baja de la retencion
        CLEAR: ST_DATA_PAR.
        MOVE-CORRESPONDING ST_DATA TO ST_DATA_PAR.

        PERFORM F_BORRAR_RET_XK02 USING ST_DATA_PAR
                                        ST_LFBW.
      ELSE.
*   Verifico la existencia del indicador de ret.
        PERFORM F_VERIF_INDICADOR USING ST_DATA-LIFNR
                                         ST_DATA-BUKRS
                                   CHANGING VL_EXISTE_IND.

        IF VL_EXISTE_IND = 'X'.
*       Modifica
          CLEAR: ST_DATA_PAR.
          MOVE-CORRESPONDING ST_DATA TO ST_DATA_PAR.
          PERFORM F_MODIFICAR_RET_XK02 USING ST_DATA_PAR.
        ELSE.
*       Agrega

          CLEAR: ST_DATA_PAR.
          MOVE-CORRESPONDING ST_DATA TO ST_DATA_PAR.

          PERFORM F_AGREGO_RET_XK02 USING ST_DATA_PAR .
        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSEIF R_PROV EQ 'X'.

* Actualizo la tabla de los proveedores
    LOOP AT T_DATA_ARBA INTO ST_DATA_ARBA.

      IF ST_DATA_ARBA-TIPO EQ 'B'.
*   Si es baja el proveedor y el indicador existen en el maestro
        READ TABLE T_LFBW
        INTO ST_LFBW
        WITH KEY LIFNR = ST_DATA_ARBA-LIFNR
                 WITHT = ST_DATA_ARBA-RET.

*     Baja de la retencion
        CLEAR: ST_DATA_PAR.
        MOVE-CORRESPONDING ST_DATA_ARBA TO ST_DATA_PAR.
        PERFORM F_BORRAR_RET_XK02 USING ST_DATA_PAR
                                        ST_LFBW.
      ELSE.
*   Verifico la existencia del indicador de ret.
        PERFORM F_VERIF_INDICADOR USING ST_DATA_ARBA-LIFNR
                                        ST_DATA_ARBA-BUKRS
                               CHANGING VL_EXISTE_IND.

        IF VL_EXISTE_IND = 'X'.
*       Modifica
          CLEAR: ST_DATA_PAR.
          MOVE-CORRESPONDING ST_DATA_ARBA TO ST_DATA_PAR.
          PERFORM F_MODIFICAR_RET_XK02 USING ST_DATA_PAR.
        ELSE.
*       Agrega
          CLEAR: ST_DATA_PAR.
          MOVE-CORRESPONDING ST_DATA_ARBA TO ST_DATA_PAR.
          PERFORM F_AGREGO_RET_XK02 USING ST_DATA_PAR .
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.


ENDFORM. " F_ABM_INDICADORES


*&---------------------------------------------------------------------*
*&      Form  F_VERIF_EINDICADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_VERIF_INDICADOR USING P_LIFNR TYPE LIFNR
                                 P_BUKRS TYPE BUKRS
                        CHANGING P_EXISTE_IND TYPE CHAR1.

  CLEAR P_EXISTE_IND.

  IF R_PROV EQ 'X'.

* Prov. de Bs As
    LOOP AT T_LFBW INTO ST_LFBW
      WHERE LIFNR = P_LIFNR
        AND BUKRS = P_BUKRS.
*      IF ST_LFBW-WITHT(1) = 'H'. "ALRS
      IF ST_LFBW-WITHT(2) = 'IB'.
        P_EXISTE_IND = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

  ELSE.

* CABA
    READ TABLE T_LFBW
    INTO ST_LFBW
    WITH KEY LIFNR = ST_DATA-LIFNR
             WITHT = ST_DATA-RET.
    IF SY-SUBRC = 0.
      P_EXISTE_IND = 'X'.
    ENDIF.

  ENDIF.

ENDFORM. " F_VERIF_EINDICADOR


*&---------------------------------------------------------------------*
*&      Form  F_MODIFICAR_RET_XK02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_MODIFICAR_RET_XK02 USING SI_DATA TYPE TY_DATA_PAR.

  DATA:  VL_SUBRC    TYPE SY-SUBRC,
         VL_NO_ACT   TYPE CHAR1,
         STL_LFBW    TYPE LFBW,
         STL_LFBW_UP TYPE LFBW,
         TL_LFBW     TYPE STANDARD TABLE OF TY_LFBW,
         TL_LFBW_A   TYPE STANDARD TABLE OF LFBW,
         TL_LFBW_AX  TYPE STANDARD TABLE OF LFBW.

  CLEAR: VL_SUBRC,
         VL_NO_ACT,
         TL_LFBW_A,
         TL_LFBW_AX,
         STL_LFBW.

  REFRESH: TL_LFBW_A,
           TL_LFBW_AX.

  IF R_PROV EQ 'X'.
* Prov. Bs As
    STL_LFBW-MANDT     = SY-MANDT.
    STL_LFBW-LIFNR     = SI_DATA-LIFNR.
    STL_LFBW-BUKRS     = SI_DATA-BUKRS.
    STL_LFBW-WITHT     = SI_DATA-RET.
    STL_LFBW-WT_SUBJCT = 'X'.
    STL_LFBW-WT_WITHCD = SI_DATA-RET_A. "SI_DATA-RET.
    APPEND  STL_LFBW TO TL_LFBW_A.

    SELECT *
    FROM LFBW
    INTO TABLE TL_LFBW_AX
    WHERE LIFNR EQ STL_LFBW-LIFNR
      AND BUKRS EQ STL_LFBW-BUKRS.

    LOOP AT TL_LFBW_AX INTO ST_LFBW.
*      IF ST_LFBW-WITHT(1) EQ 'H'. "ALRS
      IF ST_LFBW-WITHT(2) EQ 'IB'.
        IF ST_LFBW-WITHT     = SI_DATA-RET AND
           ST_LFBW-WT_WITHCD = SI_DATA-RET_A. "ALRS
* El índice no fué modificado
          VL_NO_ACT = 'X'.
          EXIT.
        ELSE.
          DELETE FROM LFBW
          WHERE LIFNR EQ ST_LFBW-LIFNR
            AND BUKRS EQ ST_LFBW-BUKRS
            AND WITHT EQ ST_LFBW-WITHT.
          IF SY-SUBRC EQ 0.
            VL_SUBRC = SY-SUBRC.
            COMMIT WORK AND WAIT.
          ELSE.
            VL_SUBRC = SY-SUBRC.
            ROLLBACK WORK.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF VL_NO_ACT = 'X'.
      PERFORM F_LLENO_TABLA_T_MES_SAL USING 'R'
                                            ST_LFBW-LIFNR
                                            ST_LFBW-BUKRS
                                            SI_DATA-STCD1
                                            '0'
                                            SI_DATA-RET.
    ELSE.
* Se actualiza la tabla con las modificaciones
      INSERT LFBW FROM TABLE  TL_LFBW_A.
      PERFORM F_COMMIT_WORK USING SY-SUBRC
                         CHANGING VL_SUBRC .

** Mensaje de error.
      PERFORM F_LLENO_TABLA_T_MES_SAL USING 'M'
                                            SI_DATA-LIFNR
                                            SI_DATA-BUKRS
                                            SI_DATA-STCD1
                                            VL_SUBRC
                                            SI_DATA-RET.
    ENDIF.

  ELSE.

* CABA
    STL_LFBW_UP-MANDT = SY-MANDT.
    STL_LFBW_UP-LIFNR = SI_DATA-LIFNR.
    STL_LFBW_UP-BUKRS = SI_DATA-BUKRS.
    STL_LFBW_UP-WITHT = SI_DATA-RET.
    STL_LFBW_UP-WT_WITHCD = SI_DATA-RET_A.
    STL_LFBW_UP-WT_SUBJCT = 'X'.

    SELECT SINGLE *
      INTO STL_LFBW
      FROM LFBW
      WHERE LIFNR EQ STL_LFBW_UP-LIFNR
        AND BUKRS EQ STL_LFBW_UP-BUKRS
        AND WITHT EQ STL_LFBW_UP-WITHT.

    IF SY-SUBRC <> 0.
* Error, no existe la entrada a modificar
    ENDIF.

    CASE STL_LFBW-WT_WITHCD.
      WHEN 'I0'.
        STL_LFBW_UP-WT_WITHCD = 'I1'.
      WHEN 'I1'.
        IF SI_DATA-ALTA_SUJETO = 'B'.
          STL_LFBW_UP-WT_WITHCD = 'I0'.
        ELSE.
          STL_LFBW_UP-WT_WITHCD = 'I1'.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    IF STL_LFBW_UP-WT_WITHCD <> STL_LFBW-WT_WITHCD.
* Si se realizaron cambios con el indicador se actualiza la tabla
      DELETE FROM LFBW
      WHERE LIFNR EQ STL_LFBW-LIFNR
        AND BUKRS EQ STL_LFBW-BUKRS
        AND WITHT EQ STL_LFBW-WITHT.

      PERFORM F_COMMIT_WORK USING SY-SUBRC
                   CHANGING VL_SUBRC .

      APPEND  STL_LFBW_UP TO TL_LFBW_A.

      INSERT   LFBW FROM TABLE  TL_LFBW_A.
      CLEAR VL_SUBRC.
      PERFORM F_COMMIT_WORK USING SY-SUBRC
                         CHANGING VL_SUBRC .

** Mensaje de error.
      PERFORM F_LLENO_TABLA_T_MES_SAL USING 'M'
                                            SI_DATA-LIFNR
                                            SI_DATA-BUKRS
                                            SI_DATA-STCD1
                                            VL_SUBRC
                                            SI_DATA-RET.
    ELSE.
* No se realizaron cambios en el indicador
      PERFORM F_LLENO_TABLA_T_MES_SAL USING 'R'
                                            ST_LFBW-LIFNR
                                            ST_LFBW-BUKRS
                                            SI_DATA-STCD1
                                            '0'
                                            SI_DATA-RET.
    ENDIF.
  ENDIF.

ENDFORM. " F_MODIFICAR_RET_XK02
