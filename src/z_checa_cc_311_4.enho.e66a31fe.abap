"Name: \PR:SAPMM07R\FO:FCODE_BEARBEITEN\SE:BEGIN\EI
ENHANCEMENT 0 Z_CHECA_CC_311_4.
*

DATA: WA_ZMMT0039 TYPE ZMMT0039,
        WA_CSKS     TYPE CSKS,
        WA_MARA     type MARA,
        WA_MARC     type MARC,
        V_KOSTL     TYPE RKPF-KOSTL,
        WA_ZMMT0085 type ZMMT0085,
        WRESB       type resb.

   DATA VALUESNUM  TYPE TABLE OF BAPI1003_ALLOC_VALUES_NUM.
   DATA VALUESCHAR TYPE TABLE OF BAPI1003_ALLOC_VALUES_CHAR.
   DATA VALUESCURR TYPE TABLE OF BAPI1003_ALLOC_VALUES_CURR.
   DATA VALUECA    TYPE BAPI1003_ALLOC_VALUES_CHAR-VALUE_CHAR.
   DATA VALUEPERI  TYPE p LENGTH 4.
   DATA VALUEVALCA TYPE  BAPI1003_ALLOC_VALUES_NUM-VALUE_FROM.
   DATA VALUEVALCAC(50).
   DATA VALUEVALCAD type SY-DATUM..
   DATA RETURN     TYPE TABLE OF  BAPIRET2.

   DATA: T_SET TYPE STANDARD TABLE OF SETLEAF  WITH HEADER LINE.
   DATA: T_LAY TYPE STANDARD TABLE OF SETLINET WITH HEADER LINE.
   DATA: T_values  LIKE rgsb4 OCCURS 0 WITH HEADER LINE.
   DATA: r_KOSTL TYPE RANGE OF CSKS-KOSTL  WITH HEADER LINE.

   data lv_check type c.



 SELECT *
      FROM  SETLEAF
      INTO TABLE T_SET
      WHERE SETCLASS      = '0000'
      AND   SETNAME        = 'MAGGI_BIOMETRIA'.

  SELECT *
    FROM SETLINET
    INTO TABLE T_LAY
    FOR ALL ENTRIES IN T_SET
    WHERE SETCLASS   = T_SET-SETCLASS
    AND SUBCLASS     = T_SET-SUBCLASS
    AND SETNAME      = T_SET-SETNAME
    AND LANGU        = 'P'
    AND LINEID       = T_SET-LINEID.

  IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  REFRESH T_values.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr         = 'MAGGI_MB21'
      class         = '0000'
    TABLES
      set_values    = T_values
    EXCEPTIONS
      set_not_found = 1.

 IF sy-subrc IS INITIAL.
    LOOP AT T_values.
      r_KOSTL-sign   = 'I'.
      r_KOSTL-option = 'EQ'.
      r_KOSTL-low    = T_values-from.
      APPEND r_KOSTL.
    ENDLOOP.
  ENDIF.

*CHECK ( SY-TCODE EQ 'MB21' ).

if SY-TCODE EQ 'MB21' or SY-TCODE EQ 'MB22' or SY-TCODE EQ 'ZMM0123'.
  READ TABLE r_kostl into data(s_kostl) with key low = RKPF-KOSTL.
  IF sy-subrc eq 0.
   lv_check = abap_true.
  ENDIF.

  if lv_check = abap_true.
    LOOP AT YRSEG into data(lw_YRSEG2).
      lw_YRSEG2-XWAOK = 'X'.
      MODIFY YRSEG from lw_YRSEG2.
    ENDLOOP.
  endif.
ENDIF.

if SY-TCODE EQ 'MB21' or SY-TCODE EQ 'MB22'.

  IF RKPF-BWART = '311'.
      SELECT SINGLE *
        FROM CSKS
        INTO WA_CSKS
         WHERE KOKRS EQ 'MAGI'
           AND KOSTL EQ RKPF-WEMPF.
      IF SY-SUBRC NE 0.
        MESSAGE E000(Z01) WITH 'Centro de custo em Recebedor / CC' RKPF-WEMPF
                               ' não existe' .
      ENDIF.
  ELSE.
      LOOP AT YRSEG into data(lw_YRSEG).

        IF lw_YRSEG-lgort is INITIAL.
*          MESSAGE E897(SD) WITH  'Informe o depósito para '
*                                  lw_YRSEG-MATNR '/'
*                                  lw_YRSEG-WERKS.
          MESSAGE i897(SD) WITH   'Informe o depósito para '
                                  lw_YRSEG-MATNR '/'
                                  lw_YRSEG-WERKS DISPLAY LIKE 'E'.
         RETURN.
*        LEAVE TO SCREEN 0521.

        ENDIF.
        clear WA_MARC.
        SELECT SINGLE * FROM MARC  INTO WA_MARC WHERE MATNR = lw_YRSEG-MATNR and WERKS =  lw_YRSEG-WERKS.
        IF WA_MARC-LVORM = 'X'.
            MESSAGE E897(SD) WITH  'Este Material esta marcado para eliminação no centro'
                                  lw_YRSEG-MATNR '/'
                                  lw_YRSEG-WERKS.
        ENDIF.
        "
        SELECT SINGLE * FROM MARA  INTO WA_MARA WHERE MATNR = lw_YRSEG-MATNR.
        IF WA_MARA-LVORM = 'X'.
            MESSAGE E897(SD) WITH  'Este Material esta marcado para eliminação '
                                  lw_YRSEG-MATNR.
        ENDIF.
        SELECT SINGLE * FROM ZMMT0039 INTO WA_ZMMT0039 WHERE MATKL EQ WA_MARA-MATKL.

        IF ( SY-SUBRC EQ 0 ) AND ( lw_YRSEG-SAKNR NE WA_ZMMT0039-SAKNR ).
          MESSAGE I000(Z01) WITH 'Para este grupo mercadoria  a conta é '
                                 WA_ZMMT0039-SAKNR
                                 lw_YRSEG-MATNR.

        ENDIF.
        "
        IF lw_YRSEG-XLOEK IS NOT INITIAL..
          CONTINUE.
        ENDIF.
        SELECT SINGLE *
          FROM RESB
          INTO WRESB
          WHERE RSNUM = lw_YRSEG-RSNUM
          AND   RSPOS = lw_YRSEG-RSPOS.
        IF SY-SUBRC = 0.
          IF WRESB-XWAOK IS INITIAL. "Se não estiver liberado
            CONTINUE.
          ENDIF.
        ENDIF.
        READ TABLE T_SET WITH KEY VALFROM = lw_YRSEG-WERKS.
        IF SY-SUBRC = 0.
         READ TABLE T_LAY WITH KEY  SETCLASS   = T_SET-SETCLASS
                                    SUBCLASS   = T_SET-SUBCLASS
                                    SETNAME    = T_SET-SETNAME
                                    LINEID     = T_SET-LINEID.

          SELECT SINGLE *
            FROM ZMMT0085
            INTO WA_ZMMT0085
            WHERE MATNR = lw_YRSEG-MATNR.
          IF SY-SUBRC = 0.
            IF T_LAY-DESCRIPT IS INITIAL.
              MESSAGE E897(SD) WITH  'Este Material é de EPI '
                                    lw_YRSEG-MATNR
                                     ' Utilize a ZMM00123 ' .
            EXIT.
            else.
              MESSAGE W897(SD) WITH  'Este Material é de EPI '
                       lw_YRSEG-MATNR
                        ' Utilize a ZMM00123 ' .
            endif.
*            "Checar valiudade CA
*             CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*                EXPORTING
*                  OBJECTKEY       = CONV OBJNUM( YRSEG-MATNR )
*                  OBJECTTABLE     = 'MARA'
*                  CLASSNUM        = 'MATEPI'
*                  CLASSTYPE       = '023'
*                TABLES
*                  ALLOCVALUESNUM  = VALUESNUM
*                  ALLOCVALUESCHAR = VALUESCHAR
*                  ALLOCVALUESCURR = VALUESCURR
*                  RETURN          = RETURN.
*
*              TRY.
*                  VALUECA = VALUESCHAR[ CHARACT = 'ZEPI_CA' ]-VALUE_CHAR.
*                CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*              ENDTRY.
*             TRY.
*                  VALUEPERI = VALUESNUM[ CHARACT = 'ZEPI_PERI' ]-VALUE_FROM.
*                CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*             ENDTRY.
*             "
*             TRY.
*                VALUEVALCA = VALUESNUM[ CHARACT = 'ZEPI_VALCA' ]-VALUE_FROM.
*                CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*             ENDTRY.
*             IF VALUECA is INITIAL.
*                MESSAGE E897(SD) WITH  'Este Material é de EPI '
*                                    YRSEG-MATNR
*                                     ' esta sem CA, procure SSO ' .
*             ENDIF.
*             IF VALUEPERI is INITIAL.
*                MESSAGE E897(SD) WITH  'Este Material é de EPI '
*                                    YRSEG-MATNR
*                                     ' esta sem periodicidade, procure SSO ' .
*             ENDIF.
*             IF VALUEVALCA is INITIAL.
*                MESSAGE E897(SD) WITH  'Este Material é de EPI '
*                                    YRSEG-MATNR
*                                     ' esta sem validade, procure SSO ' .
*            ELSE.
*                 WRITE: VALUEVALCA               TO VALUEVALCAC.
*                 REPLACE ALL OCCURRENCES OF        '.'     IN VALUEVALCAC WITH ' '.
*                 REPLACE ALL OCCURRENCES OF        ','     IN VALUEVALCAC WITH ' '.
*                 CONDENSE VALUEVALCAC NO-GAPS.
*                 VALUEVALCAD = VALUEVALCAC+0(8).
*                 if VALUEVALCAD LT SY-DATUM.
*                     MESSAGE E897(SD) WITH  'Este Material é de EPI '
*                                        YRSEG-MATNR
*                                         ' esta vencido, procure SSO ' .
*                 Endif.
*            ENDIF.
          else.
              SELECT SINGLE * FROM MARA  INTO WA_MARA WHERE MATNR = lw_YRSEG-MATNR.
              if WA_mara-mtart = 'ZEPI'.
                 MESSAGE E897(SD) WITH  'Este Material é de EPI '
                                       lw_YRSEG-MATNR
                                        ' esta sem vinculação, procure SSO '.
              endif.
          ENDIF.
        ENDIF.
      ENDLOOP.
  ENDIF.
endif.

ENDENHANCEMENT.
