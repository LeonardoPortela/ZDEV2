*&---------------------------------------------------------------------*
*&  Include           ZFIY0003_FORM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_MEDIOS_DE_PAGOS
*&---------------------------------------------------------------------*
FORM F_MEDIOS_DE_PAGOS  USING    PI_BUKRS
                                 PI_GJAHR
                                 PI_AUGBL.

  DATA: LS_MPAGOS TYPE ZFIYS_ORDEN_PAGOS_MPG,
        STL_BSEG  TYPE BSEG,
        WA_BKPF   TYPE BKPF,
        LS_PAGOS  TYPE ZFIYS_ORDEN_PAGOS_MPG.

  DATA: LV_BLART TYPE BKPF-BLART,
        LV_MCOD1 TYPE SKAT-MCOD1.

  LOOP AT T_BSEG INTO STL_BSEG WHERE BELNR = PI_AUGBL
                                 AND BUKRS = PI_BUKRS.

* MUEVO EL ANTICIPO A T_PAGO_POS  *************** INI
    IF STL_BSEG-UMSKZ EQ 'A'.
      CLEAR: ST_PAGO_POS.
      MOVE-CORRESPONDING STL_BSEG TO ST_PAGO_POS.
      ST_PAGO_POS-IMP_NETO =  ST_PAGO_POS-DMBTR - ST_PAGO_POS-WT_QBSHH.
      CLEAR: WA_BKPF.
      SELECT SINGLE BLART XBLNR WAERS KURSF
               FROM BKPF
               INTO (ST_PAGO_POS-BLART, ST_PAGO_POS-XBLNR,
                     ST_PAGO_POS-WAERS, ST_PAGO_POS-KURSF )
             WHERE BUKRS = STL_BSEG-BUKRS
               AND BELNR = STL_BSEG-BELNR
               AND GJAHR = STL_BSEG-GJAHR.
      APPEND ST_PAGO_POS TO T_PAGO_POS.
    ENDIF.
    CHECK STL_BSEG-UMSKZ NE 'A'.
* MUEVO EL ANTICIPO A T_PAGO_POS  *************** FIN

    IF STL_BSEG-UMSKS = 'W'   AND
       STL_BSEG-BSCHL = '39' .

      CLEAR LS_MPAGOS.

      LS_MPAGOS-UBHKT = STL_BSEG-HKONT.
      LS_MPAGOS-DMBTR = STL_BSEG-DMBTR.

* Modificación 03/10/2011 - DEVELOPER - Asigno el cheque.
      LS_MPAGOS-CHECT = STL_BSEG-ZUONR.
      LS_MPAGOS-FECHA = STL_BSEG-ZFBDT.

    ELSEIF STL_BSEG-HKONT <> '0011130003'   AND
           STL_BSEG-BSCHL = '50'            AND
           STL_BSEG-MWSKZ EQ ' '     AND
           STL_BSEG-QSSKZ EQ ' '     AND
           STL_BSEG-DMBTR >  0.

      CLEAR LS_MPAGOS.

      LS_MPAGOS-UBHKT = STL_BSEG-HKONT.
      LS_MPAGOS-DMBTR = STL_BSEG-DMBTR.

* Modificación 03/10/2011 - Asigno el cheque.
      LS_MPAGOS-CHECT = STL_BSEG-ZUONR.
      LS_MPAGOS-FECHA = STL_BSEG-VALUT.

* Buscar si es un documento de pago
    ELSE.

      SELECT SINGLE BLART
        FROM BKPF
        INTO LV_BLART
        WHERE BUKRS = STL_BSEG-BUKRS
        AND   BELNR = STL_BSEG-BELNR
        AND   GJAHR = STL_BSEG-GJAHR.

      IF SY-SUBRC = 0
        AND  LV_BLART = 'KZ'
         AND STL_BSEG-UMSKZ NE '  '
         AND STL_BSEG-BELNR NE STL_BSEG-AUGBL. " si el doc.cont es distinto que el doc.compens.

* Calcular con los valores de la l_docdata.
        SELECT SINGLE MCOD1
          FROM SKAT
          INTO LV_MCOD1
          WHERE SPRAS = 'S'
          AND   KTOPL = 'INT'
          AND   SAKNR = STL_BSEG-HKONT.

* Mover las retenciones efectuadas.
        CLEAR ST_PAGO_RET .

        MOVE :  STL_BSEG-HKONT TO LS_MPAGOS-UBHKT,
                STL_BSEG-DMBTR TO LS_MPAGOS-DMBTR,
                LV_MCOD1       TO LS_MPAGOS-DESCRIP  ."HKONT
        APPEND ST_PAGO_RET TO T_PAGO_RET.
      ENDIF.
    ENDIF.
* Modificacion  16/02/2012 -
*    ENDIF.

*    SELECT SINGLE bldat
*    FROM bkpf
*    INTO ls_mpagos-fecha
*    WHERE bukrs EQ pi_bukrs
*    AND   gjahr EQ pi_gjahr
*    AND   belnr EQ pi_augbl.
* Fin Modificación 03/10/2011 -

    READ TABLE T_MPAGOS INTO LS_PAGOS
    WITH KEY UBHKT = LS_MPAGOS-UBHKT
             DMBTR = LS_MPAGOS-DMBTR
             CHECT = LS_MPAGOS-CHECT.
    IF SY-SUBRC NE 0.
* Modificación 03/10/2011 -
      APPEND LS_MPAGOS TO T_MPAGOS.
    ENDIF.

  ENDLOOP.
* Fin Modificación 03/10/2011

  IF T_MPAGOS IS INITIAL.
    SELECT  HKONT ZUONR DMBTR ZFBDT
          FROM BSIS
          INTO (LS_MPAGOS-UBHKT, LS_MPAGOS-CHECT, LS_MPAGOS-DMBTR, LS_MPAGOS-FECHA)
        WHERE     BUKRS EQ PI_BUKRS
            AND   GJAHR EQ PI_GJAHR
            AND   BSCHL EQ '39'
          AND   MWSKZ EQ ' '
            AND   DMBTR >  0
            AND   BELNR EQ PI_AUGBL.
      IF SY-SUBRC EQ 0.
        READ TABLE T_MPAGOS INTO LS_PAGOS
        WITH KEY UBHKT = LS_MPAGOS-UBHKT
                 CHECT = LS_MPAGOS-CHECT.
        IF SY-SUBRC NE 0.
          APPEND LS_MPAGOS TO T_MPAGOS.
        ENDIF.
      ENDIF.
    ENDSELECT.
  ENDIF.

*  SELECT  HKONT ZUONR DMBTR ZFBDT   "valut   Modificado por Diego
*        FROM BSEG
*        INTO (LS_MPAGOS-UBHKT, LS_MPAGOS-CHECT, LS_MPAGOS-DMBTR, LS_MPAGOS-FECHA)
*      WHERE     BUKRS EQ PI_BUKRS
*          AND   AUGBL EQ PI_AUGBL
*          AND   GJAHR EQ PI_GJAHR
*          AND   BSCHL EQ '40'
*          AND   HKONT EQ '0011130003'.
*    IF SY-SUBRC EQ 0.
*      READ TABLE T_MPAGOS INTO LS_PAGOS
*      WITH KEY UBHKT = LS_MPAGOS-UBHKT
*               CHECT = LS_MPAGOS-CHECT
*               DMBTR = LS_MPAGOS-DMBTR.
*      IF SY-SUBRC NE 0.
*        APPEND LS_MPAGOS TO T_MPAGOS.
*      ENDIF.
*    ENDIF.
*  ENDSELECT.

  LOOP AT T_BSEG INTO STL_BSEG WHERE BELNR = PI_AUGBL AND BUKRS = PI_BUKRS AND UMSKS = 'W' AND BSCHL = '39'.

    CLEAR LS_MPAGOS.

    LS_MPAGOS-UBHKT = STL_BSEG-HKONT.
    LS_MPAGOS-DMBTR = STL_BSEG-DMBTR.

    LS_MPAGOS-CHECT = STL_BSEG-ZUONR.
    LS_MPAGOS-FECHA = STL_BSEG-ZFBDT.

    READ TABLE T_MPAGOS INTO LS_PAGOS
    WITH KEY UBHKT = LS_MPAGOS-UBHKT
             DMBTR = LS_MPAGOS-DMBTR.
    IF SY-SUBRC NE 0.

      APPEND LS_MPAGOS TO T_MPAGOS.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_MEDIOS_DE_PAGOS

*&---------------------------------------------------------------------*
*&      Form  OBTENER_CHEQUE
*&---------------------------------------------------------------------*
FORM OBTENER_CHEQUE  USING   LS_DOCDATA_ALL TYPE TY_DOCDATA
                             L_XVORL TYPE XVORL
                    CHANGING LS_MPAGOS TYPE ZFIYS_ORDEN_PAGOS_MPG.

  DATA: LT_PAYR  TYPE STANDARD TABLE OF PAYR,
        VL_KTOPL TYPE KTOPL,
        VL_SPRAS TYPE SPRAS,
        VL_SAKNR TYPE SAKNR,
        VL_HKONT TYPE HKONT,
        STL_BSEG TYPE BSEG,
        LS_PAYR  TYPE PAYR.
*  Plan de cuenta
  PERFORM F_PLAN_DE_CUENTA USING   LS_DOCDATA_ALL-BUKRS
                        CHANGING VL_KTOPL VL_SPRAS.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_PAYR
    FROM PAYR
    WHERE ZBUKR = LS_DOCDATA_ALL-BUKRS
      AND VBLNR = LS_DOCDATA_ALL-AUGBL
*      AND rzawe EQ 'C'
      AND VOIDR = '00'.
  IF SY-SUBRC EQ 0.
* Tomar el registro con fecha de emisión mas reciente
    SORT LT_PAYR BY PRIDT PRITI DESCENDING.

    LOOP AT LT_PAYR INTO LS_PAYR .
      IF LS_PAYR-RWBTR < 0.
        LS_PAYR-RWBTR = LS_PAYR-RWBTR * ( - 1 ).
      ENDIF.
      IF LS_PAYR-XMANU EQ 'X'.
* Fecha de cheque pago manual
        LS_MPAGOS-FECHA = LS_PAYR-ZALDT.
      ELSE.
* Fecha de cheque pago automatico
        SELECT SINGLE VALUT
        FROM REGUH
        INTO LS_MPAGOS-FECHA
        WHERE ZBUKR = LS_DOCDATA_ALL-BUKRS
        AND   VBLNR = LS_DOCDATA_ALL-AUGBL.
* Si no encuentra cheque pago manual o pago automatico
        IF SY-SUBRC NE 0.
          LS_MPAGOS-FECHA = ST_EMISOR-BUDAT.
        ENDIF.
      ENDIF.
      LS_MPAGOS-CHECT = LS_PAYR-CHECT.
      LS_MPAGOS-DMBTR = LS_PAYR-RWBTR.
*Cuenta
      IF LS_PAYR-UBHKT  NE '0008111016'.
        IF LS_PAYR-UBHKT  IS NOT INITIAL
       AND LS_PAYR-HKTID  IS NOT INITIAL.

          READ TABLE T_BSEG INTO STL_BSEG
          WITH KEY BELNR = LS_DOCDATA_ALL-AUGBL
                   BUKRS = LS_DOCDATA_ALL-BUKRS
                   DMBTR = LS_MPAGOS-DMBTR
                   UMSKZ = 'W'
                   BSCHL = '39'.

          IF SY-SUBRC NE 0.
            CONCATENATE  LS_PAYR-UBHKT+1(4)
                         LS_PAYR-HKTID
                        '1'
                  INTO LS_MPAGOS-UBHKT.

          ELSE.
            DELETE T_BSEG INDEX SY-TABIX.
            MOVE STL_BSEG-HKONT TO LS_MPAGOS-UBHKT.
          ENDIF.



          IF LS_PAYR-ZALDT > LS_PAYR-PRIDT.
            LS_MPAGOS-ZLSCH = 'D'.
          ENDIF.
        ENDIF.
* Descripcion Medio de Pagos
        PERFORM F_DESCRIPCION_PAGO USING  VL_KTOPL
                                          VL_SPRAS
                                          LS_MPAGOS-UBHKT
                                          LS_DOCDATA_ALL-AUGBL
                                          LS_DOCDATA_ALL
                                 CHANGING LS_MPAGOS-DESCRIP.
        APPEND LS_MPAGOS TO T_MPAGOS.

      ELSE.

        LOOP AT T_BSEG INTO STL_BSEG
        WHERE (  BELNR EQ LS_DOCDATA_ALL-AUGBL
        AND      AUGBL NE LS_DOCDATA_ALL-AUGBL
        AND      BUKRS EQ LS_DOCDATA_ALL-BUKRS
        AND      DMBTR EQ LS_MPAGOS-DMBTR
        AND      QSSKZ EQ '  '
        AND      BSCHL EQ '50' )
        OR    (  BELNR EQ LS_DOCDATA_ALL-AUGBL
        AND      AUGBL NE LS_DOCDATA_ALL-AUGBL
        AND      BUKRS EQ LS_DOCDATA_ALL-BUKRS
        AND      DMBTR EQ LS_MPAGOS-DMBTR
        AND      UMSKZ EQ 'W'
        AND      BSCHL EQ '39' ).

          DELETE T_BSEG INDEX SY-TABIX.

          MOVE: STL_BSEG-HKONT TO LS_MPAGOS-UBHKT,
                STL_BSEG-DMBTR TO LS_MPAGOS-DMBTR.
* Descripcion Medio de Pagos
          PERFORM F_DESCRIPCION_PAGO USING  VL_KTOPL
                                            VL_SPRAS
                                            LS_MPAGOS-UBHKT
                                            LS_DOCDATA_ALL-AUGBL
                                            LS_DOCDATA_ALL
                                   CHANGING LS_MPAGOS-DESCRIP.
          IF LS_PAYR-ZALDT > LS_PAYR-PRIDT.
            LS_MPAGOS-ZLSCH = 'D'.
          ENDIF.

          APPEND LS_MPAGOS TO T_MPAGOS.
          EXIT.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " OBTENER_CHEQUE
*&---------------------------------------------------------------------*
*&      Form  PLAN_DE_CUENTA
*&---------------------------------------------------------------------*
FORM F_PLAN_DE_CUENTA  USING    PI_BUKRS
                     CHANGING PO_KTOPL
                              PO_SPRAS.
  SELECT SINGLE KTOPL SPRAS
      FROM T001
      INTO (PO_KTOPL, PO_SPRAS)
      WHERE BUKRS EQ PI_BUKRS.

ENDFORM.                    " PLAN_DE_CUENTA
*&---------------------------------------------------------------------*
*&      Form  F_DESCRIPCION_PAGO
*&---------------------------------------------------------------------*
FORM F_DESCRIPCION_PAGO  USING    PI_KTOPL
                                  PI_SPRAS
                                  PI_UBHKT
                                  PI_BELNR
                                  LS_DOCDATA_ALL TYPE TY_DOCDATA
                         CHANGING PO_DESCRIP.
  DATA: STL_BSED  TYPE BSED,
        STL_REGUH TYPE REGUH,
        VL_UBHKT  TYPE UBHKT,
        STL_BNKA  TYPE BNKA,
        VL_SUBRC  TYPE SY-SUBRC.


  IF PI_UBHKT IS INITIAL.
*---> S4 Migration - 16/06/2023 - MA
*Não tem todos os campos chave
    SELECT SINGLE HKONT
    FROM BSEG
    INTO PI_UBHKT
    WHERE BELNR = PI_BELNR
    AND   UMSKZ = 'W'
    AND   BSCHL = '39'."#EC CI_DB_OPERATION_OK[2431747]
* <--- S4 Migration - 16/06/2023 - MA
    IF SY-SUBRC EQ 0.
      VL_SUBRC = SY-SUBRC.
      SELECT SINGLE *
      FROM BSED
      INTO STL_BSED
      WHERE BUKRS EQ LS_DOCDATA_ALL-BUKRS
      AND   BELNR EQ LS_DOCDATA_ALL-AUGBL
      AND   GJAHR EQ LS_DOCDATA_ALL-GJAHR.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = PI_UBHKT
      IMPORTING
        OUTPUT = PI_UBHKT.

* Texto de la cuenta banco
    SELECT SINGLE TXT50
      FROM SKAT
    INTO PO_DESCRIP
    WHERE KTOPL EQ PI_KTOPL
    AND   SPRAS EQ PI_SPRAS
    AND   SAKNR EQ PI_UBHKT.

    IF VL_SUBRC EQ 0 AND STL_BSED-WBANK IS NOT INITIAL.
      CONCATENATE PO_DESCRIP '-' STL_BSED-WBANK   INTO PO_DESCRIP SEPARATED BY SPACE.
    ENDIF.

  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = PI_UBHKT
      IMPORTING
        OUTPUT = PI_UBHKT.

* Texto de la cuenta banco
    SELECT SINGLE TXT50
      FROM SKAT
    INTO PO_DESCRIP
    WHERE KTOPL EQ PI_KTOPL
    AND   SPRAS EQ PI_SPRAS
    AND   SAKNR EQ PI_UBHKT.
    IF SY-SUBRC EQ 0.
      CLEAR VL_UBHKT.
* ---> S4 Migration - 16/06/2023 - MA
*      Não tem todos os campos chave
      SELECT SINGLE HKONT
      FROM BSEG
      INTO VL_UBHKT
      WHERE BELNR = PI_BELNR
      AND   UMSKZ = 'W'
      AND   BSCHL = '39'. "#EC CI_DB_OPERATION_OK[2431747]
* <--- S4 Migration - 16/06/2023 - MA
      IF  VL_UBHKT EQ PI_UBHKT.

        SELECT SINGLE *
        FROM REGUH
        INTO STL_REGUH
        WHERE VBLNR EQ LS_DOCDATA_ALL-AUGBL
        AND ZBUKR EQ LS_DOCDATA_ALL-BUKRS
        AND RUMSK NE ' '.
        IF SY-SUBRC EQ 0.
          SELECT SINGLE *
          FROM BNKA
          INTO STL_BNKA
          WHERE BNKLZ EQ STL_REGUH-HBKID+2
          AND BANKS EQ STL_REGUH-UBNKS  .
          IF SY-SUBRC EQ 0.
            IF STL_BNKA-BANKA IS NOT INITIAL.
              CONCATENATE PO_DESCRIP '-' STL_BNKA-BANKA   INTO PO_DESCRIP SEPARATED BY SPACE.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_DESCRIPCION_PAGO
*&---------------------------------------------------------------------*
*&      Form  OBTENER_CTA_BANCARIA
*&---------------------------------------------------------------------*
FORM OBTENER_CTA_BANCARIA  USING    LS_DOCDATA_ALL TYPE TY_DOCDATA
                                    L_XVORL        TYPE XVORL
                                    LS_AUGBL       TYPE ZFIYS_ORDEN_PAGOS_CAB.

  DATA: L_ZBNKN TYPE REGUH-ZBNKN,
        L_ZBNKL TYPE REGUH-ZBNKL.


  SELECT SINGLE ZBNKN ZBNKL
     INTO (L_ZBNKN, L_ZBNKL)
     FROM REGUH
   WHERE XVORL = L_XVORL
     AND ZBUKR = LS_DOCDATA_ALL-BUKRS
     AND VBLNR = LS_DOCDATA_ALL-AUGBL
     AND LIFNR = LS_AUGBL-LIFNR.

ENDFORM.                    " OBTENER_CTA_BANCARIA
*&---------------------------------------------------------------------*
*&      Form  OBTENER_FECHA_Y_CLASE
*&---------------------------------------------------------------------*
FORM OBTENER_FECHA_Y_CLASE  USING    LS_DOCDATA_ALL TYPE TY_DOCDATA
                            CHANGING LS_EMISOR-BLDAT TYPE BLDAT.
*                                     ls_emisor-blart type blart.
  SELECT SINGLE BLDAT "blart
    INTO LS_EMISOR-BLDAT ", ls_emisor-blart)
    FROM BKPF
    WHERE BUKRS = LS_DOCDATA_ALL-BUKRS
      AND BELNR = LS_DOCDATA_ALL-AUGBL
      AND GJAHR = LS_DOCDATA_ALL-GJAHR.
ENDFORM.                    " OBTENER_FECHA_Y_CLASE
*&---------------------------------------------------------------------*
*&      Form  F_BUSCO_RETENCIONES
*&---------------------------------------------------------------------*
FORM F_BUSCO_RETENCIONES USING L_DOCDATA TYPE TY_DOCDATA.

  DATA: TL_RET      TYPE STANDARD TABLE OF TY_RET_SUM,
        LS_MPAGOS   TYPE ZFIYS_ORDEN_PAGOS_MPG,
        STL_SUM     TYPE TY_RET_SUM,
        STL_RET     TYPE TY_RET,
        ST_RET      TYPE TY_RET,
        LV_TAB      TYPE XFELD,
        LV_LINES(2) TYPE C,
        LV_BLART    TYPE BKPF-BLART,
        LV_MCOD1    TYPE SKAT-MCOD1,
        LV_HKONT    TYPE BSEG-HKONT.

  SELECT  AUGBL
          BUKRS
          BELNR
          GJAHR
          BUZEI
          WITHT
          WT_WITHCD
          WT_QBSHH
          CTNUMBER
  FROM    WITH_ITEM
  APPENDING TABLE T_RET
  WHERE BUKRS    EQ L_DOCDATA-BUKRS
  AND   BELNR    EQ L_DOCDATA-AUGBL
  AND   GJAHR    EQ L_DOCDATA-GJAHR.

  IF SY-SUBRC EQ 0.

    LOOP AT T_RET INTO STL_RET.
      PERFORM F_RETENCIONES USING SY-TABIX STL_RET.
    ENDLOOP.

*   Suma todas las Retenciones iguales
    LOOP AT T_RET INTO ST_RET.

      CLEAR: ST_RET-BELNR.
      MOVE:
        ST_RET-WITHT     TO STL_SUM-WITHT,
        ST_RET-WT_WITHCD TO STL_SUM-WT_WITHCD,
        ST_RET-WT_QBSHH  TO STL_SUM-WT_QBSHH .

      COLLECT STL_SUM INTO TL_RET.

    ENDLOOP.

    SORT TL_RET BY  WITHT
                    WT_WITHCD.

* Armo la tabla de las retenciones


    LOOP AT TL_RET INTO STL_SUM.
      CLEAR ST_PAGO_RET.

      READ TABLE T_RET INTO ST_RET
      WITH KEY WITHT     = STL_SUM-WITHT
               WT_WITHCD = STL_SUM-WT_WITHCD.
      MOVE:
         STL_SUM-WT_QBSHH TO ST_PAGO_RET-WT_QBSHH,
         ST_RET-CTNUMBER  TO ST_PAGO_RET-CTNUMBER,
         ST_RET-TIPO      TO ST_PAGO_RET-TIPO,
         ST_RET-CONCEP    TO ST_PAGO_RET-CONCEP.

*    Fondo de reparo.
      CLEAR  LS_MPAGOS.
      IF ST_RET-WITHT EQ 'FR' AND ST_RET-WT_QBSHH IS NOT INITIAL.

        MOVE: STL_SUM-WT_QBSHH TO LS_MPAGOS-DMBTR,
              ST_EMISOR-BUDAT TO LS_MPAGOS-FECHA.
        LS_MPAGOS-CHECT = ' '.
        READ TABLE T_PAGO_POS INTO ST_PAGO_POS
        WITH KEY   BELNR = ST_RET-BELNR.

        CONCATENATE ST_RET-TIPO  '(' ST_BSEG-XBLNR ')'
               INTO LS_MPAGOS-DESCRIP SEPARATED BY SPACE .

        CONCATENATE '               ' LS_MPAGOS-DESCRIP
                     INTO LS_MPAGOS-DESCRIP SEPARATED BY SPACE RESPECTING BLANKS.

        COLLECT LS_MPAGOS INTO T_MPAGOS.
      ELSE.
        APPEND ST_PAGO_RET TO T_PAGO_RET.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " F_BUSCO_RETENCIONES

*&---------------------------------------------------------------------*
*&      Form  f_busco_retenciones_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_DOCDATA  text
*----------------------------------------------------------------------*
FORM F_BUSCO_RETENCIONES_2  USING L_DOCDATA TYPE TY_DOCDATA.

  DATA:
    TL_RET    TYPE STANDARD TABLE OF TY_RET_SUM,
    LS_MPAGOS TYPE ZFIYS_ORDEN_PAGOS_MPG,
    STL_SUM   TYPE TY_RET_SUM,
    STL_RET   TYPE TY_RET,
    ST_RET    TYPE TY_RET,
    LV_TAB    TYPE XFELD,
    LV_BLART  TYPE BKPF-BLART,
    LV_UMSKZ  TYPE BSEG-UMSKZ,
    LV_MCOD1  TYPE SKAT-MCOD1,
    LV_HKONT  TYPE BSEG-HKONT.

* busco si es un documento de pago
  SELECT SINGLE BLART
    FROM BKPF
    INTO LV_BLART
    WHERE BUKRS = L_DOCDATA-BUKRS
    AND   BELNR = L_DOCDATA-BELNR
    AND   GJAHR = L_DOCDATA-GJAHR.

  IF SY-SUBRC = 0
    AND  LV_BLART = 'KZ'.

    DATA ETL584C4R8415 TYPE TABLE OF BSEG.
DATA RLDNR_L584C4R9879 TYPE RLDNR.
CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
  IMPORTING E_RLDNR = RLDNR_L584C4R9879
  EXCEPTIONS NOT_FOUND     = 1
             MORE_THAN_ONE = 2.
IF SY-SUBRC = 0.
CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
  EXPORTING
    I_RLDNR = RLDNR_L584C4R9879
    I_BUKRS = L_DOCDATA-BUKRS
    I_BELNR = L_DOCDATA-BELNR
    I_GJAHR = L_DOCDATA-GJAHR
    I_BUZEI = L_DOCDATA-BUZEI
  IMPORTING
    ET_BSEG = ETL584C4R8415
  EXCEPTIONS NOT_FOUND = 1.
ENDIF.
IF SY-SUBRC = 0 AND LINES( ETL584C4R8415 ) = 1.
  BSEG = ETL584C4R8415[ 1 ].
  SY-DBCNT = 1.
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


    IF SY-SUBRC EQ 0
      AND BSEG-UMSKZ NE '  '
      AND BSEG-BELNR NE L_DOCDATA-AUGBL. " si el doc.cont es distinto que el doc.compens.

      SELECT  AUGBL
              BUKRS
              BELNR
              GJAHR
              BUZEI
              WITHT
              WT_WITHCD
              WT_QBSHH
              CTNUMBER
      FROM    WITH_ITEM
      APPENDING TABLE T_RET
      WHERE BUKRS EQ L_DOCDATA-BUKRS
      AND   BELNR EQ L_DOCDATA-BELNR
      AND   GJAHR EQ L_DOCDATA-GJAHR.

      IF SY-SUBRC EQ 0.

        CLEAR ST_PAGO_RET .
        LOOP AT T_RET INTO STL_RET.
          PERFORM F_RETENCIONES USING SY-TABIX STL_RET.
        ENDLOOP.

* Suma todas las retenciones iguales.
        LOOP AT T_RET INTO ST_RET.
          CLEAR: ST_RET-BELNR.
          MOVE:
            ST_RET-WITHT     TO STL_SUM-WITHT,
            ST_RET-WT_WITHCD TO STL_SUM-WT_WITHCD,
            ST_RET-DMBTR     TO STL_SUM-WT_QBSHH .
          COLLECT STL_SUM INTO TL_RET.
        ENDLOOP.

        SORT TL_RET BY  WITHT
                        WT_WITHCD.

* Armo la tabla de las retenciones
        LOOP AT TL_RET INTO STL_SUM.
          CLEAR ST_PAGO_RET.

          READ TABLE T_RET INTO ST_RET
          WITH KEY WITHT     = STL_SUM-WITHT
                   WT_WITHCD = STL_SUM-WT_WITHCD.
          MOVE:
             STL_SUM-WT_QBSHH TO ST_PAGO_RET-WT_QBSHH,
             ST_RET-CTNUMBER  TO ST_PAGO_RET-CTNUMBER,
             ST_RET-TIPO      TO ST_PAGO_RET-TIPO    ,
             ST_RET-CONCEP    TO ST_PAGO_RET-CONCEP  .

* Fondo de reparo.
          CLEAR  LS_MPAGOS.
          IF ST_RET-WITHT EQ 'FR' AND ST_RET-WT_QBSHH IS NOT INITIAL.

            MOVE: STL_SUM-WT_QBSHH TO LS_MPAGOS-DMBTR,
                  ST_EMISOR-BUDAT  TO LS_MPAGOS-FECHA.
            LS_MPAGOS-CHECT = ' '.
            READ TABLE T_PAGO_POS INTO ST_PAGO_POS
            WITH KEY BELNR = ST_RET-BELNR.

            CONCATENATE ST_RET-TIPO  '(' ST_BSEG-XBLNR ')'
                   INTO LS_MPAGOS-DESCRIP SEPARATED BY SPACE .

            CONCATENATE '               ' LS_MPAGOS-DESCRIP
                         INTO LS_MPAGOS-DESCRIP SEPARATED BY SPACE RESPECTING BLANKS.

            COLLECT LS_MPAGOS INTO T_MPAGOS.
          ELSE.
            APPEND ST_PAGO_RET TO T_PAGO_RET.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDIF.
ENDFORM.                    " F_BUSCO_RETENCIONES_2

*&---------------------------------------------------------------------*
*&      Form  F_RETENCIONES
*&---------------------------------------------------------------------*
FORM F_RETENCIONES USING VL_INDEX2
                         ST_RET TYPE TY_RET.

* Tipo de Retencion
  SELECT SINGLE TEXT40
    FROM T059U
    INTO ST_RET-TIPO
    WHERE LAND1 EQ 'AR'
      AND SPRAS EQ 'S'
      AND WITHT EQ ST_RET-WITHT.

* Concepto de Retencion
  SELECT SINGLE TEXT40
    FROM T059ZT
    INTO ST_RET-CONCEP
    WHERE LAND1 EQ 'AR'
      AND SPRAS EQ 'S'
      AND WITHT EQ ST_RET-WITHT
      AND WT_WITHCD EQ ST_RET-WT_WITHCD.

  IF ST_RET-WT_QBSHH < 0.
    ST_RET-WT_QBSHH = ST_RET-WT_QBSHH * ( - 1 ).
  ENDIF.
  IF ST_RET-WITHT EQ 'FR'.
    ST_BSEG-WT_QBSHH = ST_BSEG-WT_QBSHH .
  ELSE.
    ST_BSEG-WT_QBSHH = ST_BSEG-WT_QBSHH + ST_RET-WT_QBSHH.
  ENDIF.

  SELECT SINGLE CTNUMBER
    FROM WITH_ITEM
    INTO  ST_RET-CTNUMBER
    WHERE BUKRS     EQ ST_RET-BUKRS
      AND BELNR     EQ ST_RET-BELNR
      AND GJAHR     EQ ST_RET-GJAHR
      AND WITHT     EQ ST_RET-WITHT
      AND WT_WITHCD EQ ST_RET-WT_WITHCD.

  MODIFY T_RET INDEX VL_INDEX2 FROM ST_RET.


ENDFORM.                    " F_RETENCIONES
*&---------------------------------------------------------------------*
*&      Form  F_CERTIFICADO_RETENCION
*&---------------------------------------------------------------------
FORM F_CERTIFICADO_RETENCION USING PI_DOCDATA_ALL TYPE TY_DOCDATA.

  SUBMIT ZFIY0004
    WITH BR_BUKRS-LOW = PI_DOCDATA_ALL-BUKRS
    WITH BR_BELNR-LOW = PI_DOCDATA_ALL-AUGBL
    WITH BR_GJAHR-LOW = PI_DOCDATA_ALL-GJAHR
    WITH RB_VIEW      = RB_VIEW
    WITH RB_PRNT      = RB_PRNT
    WITH P_PATH       = P_PATH
    WITH S_COPY       = S_COPY
    WITH P_IMPR       = P_IMPR
    WITH PCALLD       = SY-CALLD
    AND  RETURN.

ENDFORM.                    " F_CERTIFICADO_RETENCION
*&---------------------------------------------------------------------*
*&      Form  LLAMO_SMARTFORMS
*&---------------------------------------------------------------------*
FORM LLAMO_SMARTFORMS .

*--- Internal tables, Structures and Variables used for PDF conversion
  DATA: IT_OTF                  TYPE STANDARD TABLE OF ITCOO,
        IT_DOCS                 TYPE STANDARD TABLE OF DOCS,
        IT_LINES                TYPE STANDARD TABLE OF TLINE,
        V_LINES                 TYPE TLINE,
        ST_JOB_OUTPUT_INFO      TYPE SSFCRESCL,
        ST_DOCUMENT_OUTPUT_INFO TYPE SSFCRESPD,
        ST_JOB_OUTPUT_OPTIONS   TYPE SSFCRESOP,
        ST_OUTPUT_OPTIONS       TYPE SSFCOMPOP,
        ST_CONTROL_PARAMETERS   TYPE SSFCTRLOP,
        V_LEN_IN                TYPE SO_OBJ_LEN,
        V_LANGUAGE              TYPE SFLANGU VALUE 'S',
        V_E_DEVTYPE             TYPE RSPOPTYPE,
        V_BIN_FILESIZE          TYPE I,
        V_NAME                  TYPE STRING,
        V_PATH                  TYPE STRING,

        V_FULLPATH              TYPE STRING,
        V_FILTER                TYPE STRING,
        V_UACT                  TYPE I,
        V_GUIOBJ                TYPE REF TO CL_GUI_FRONTEND_SERVICES,
        V_FILENAME              TYPE STRING,
        V_FILENAME_S            TYPE STRING,
        ROOT                    TYPE REF TO ZCL_MEMORY_VARIAVEIS,
        OREF                    TYPE REF TO ZCL_MEMORY_VARIAVEIS.

  DATA: LV_ACTIVE     TYPE TDBOOL,
        WGC_SMARTFORM TYPE RS38L_FNAM,
        LS_CONTROL    TYPE SSFCTRLOP,
        L_HKONT       TYPE HKONT,
        LS_OPTIONS    TYPE SSFCOMPOP.
  DATA:
    V_WRBTR_T    TYPE DMBTR,
    V_DMBTR_T    TYPE DMBTR,
    V_WT_QBSHH_T TYPE DMBTR,
    V_IMP_NETO_T TYPE DMBTR,
    V_ENDERECO   TYPE ZPARAMETROS-VALOR.

  CONSTANTS: C_PRINTER(7) TYPE C VALUE 'PRINTER'.

  DATA : FORM_NAME    TYPE FPNAME,
         GV_FNAME(30).

  CONSTANTS: CS_FORM TYPE NA_FNAME VALUE 'ZFIY0001',
             C_X     TYPE C VALUE 'X'.

  CALL FUNCTION 'SSF_STATUS_INFO'
    EXPORTING
      I_FORMNAME = CS_FORM
    IMPORTING
      O_ACTIVE   = LV_ACTIVE.

  IF LV_ACTIVE IS INITIAL.
    STOP.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = CS_FORM
    IMPORTING
      FM_NAME            = WGC_SMARTFORM
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  Impresora
  LS_CONTROL-NO_DIALOG = 'X'.     "Evita la pantalla de opciones de salida del formulario
  LS_OPTIONS-TDDEST    = P_IMPR.  "'LOCL'.
  LS_OPTIONS-TDIMMED   = C_X.
  LS_OPTIONS-TDNEWID   = C_X.
  LS_OPTIONS-TDNOARCH  = C_X.
  LS_OPTIONS-TDCOPIES  = S_COPIA.

* Job o On-line
  CONCATENATE ST_EMISOR-BUKRS ST_EMISOR-AUGBL ST_EMISOR-GJAHR INTO DATA(NM_INSTANCE).
  TRY.
      DATA(HANDLE) = ZCL_MEMORY_VARIAVEIS_AREA=>ATTACH_FOR_READ( INST_NAME = CONV #( NM_INSTANCE ) ).
      DATA(CK_INSTANCE) = ABAP_TRUE.
      HANDLE->DETACH( ).
    CATCH CX_SHM_ATTACH_ERROR.
      CK_INSTANCE = ABAP_FALSE.
  ENDTRY.

  IF SY-BATCH IS INITIAL OR SY-TCODE = 'ZFIY0036' OR CK_INSTANCE EQ ABAP_TRUE.
    IF RB_PRNT EQ 'X' .
      LS_CONTROL-PREVIEW = SPACE.
      LS_CONTROL-DEVICE  = C_PRINTER.
      LS_CONTROL-GETOTF  = 'X'.
    ELSE.
      LS_CONTROL-PREVIEW = 'X'.
      LS_CONTROL-DEVICE  = C_PRINTER.
      LS_CONTROL-GETOTF  = ' '.
    ENDIF.
  ELSE.
    LS_CONTROL-PREVIEW = SPACE.
    LS_CONTROL-DEVICE  = C_PRINTER.
    LS_CONTROL-GETOTF  = 'X'.
  ENDIF.

  PERFORM F_LOGO.
*  Borro la cuenta Puente de los activos fijos son todas
*  las cuentas contables que son  8111014
  LOOP AT T_MPAGOS INTO ST_MPAGOS.
    IF  ST_MPAGOS-UBHKT EQ '0008111014'.
      DELETE T_MPAGOS   INDEX SY-TABIX.
    ENDIF.
  ENDLOOP.
  LOOP AT T_PAGO_POS INTO ST_PAGO_POS.
    V_WRBTR_T     = V_WRBTR_T    + ST_PAGO_POS-WRBTR.
    V_DMBTR_T     = V_DMBTR_T    + ST_PAGO_POS-DMBTR.
    V_WT_QBSHH_T  = V_WT_QBSHH_T + ST_PAGO_POS-WT_QBSHH.
    V_IMP_NETO_T  = V_IMP_NETO_T + ST_PAGO_POS-IMP_NETO.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM T_PAGO_POS.

  PERFORM F_BORRAR_DUPLICADOS_T_PAGO_POS.

  IF SY-BATCH IS NOT INITIAL.
    CONCATENATE P_PATH 'OP_' ST_EMISOR-AUGBL '_' ST_EMISOR-LIFNR '.pdf' INTO V_FULLPATH.
  ELSE.
    CONCATENATE P_PATH '\OP_' ST_EMISOR-AUGBL '_' ST_EMISOR-LIFNR '.pdf' INTO V_FULLPATH.
  ENDIF.

  EXPORT V_FULLPATH TO MEMORY ID 'ZV_FULLPATH'. "(Colocas en memoria)

* Se llama al Smartform
  CALL FUNCTION WGC_SMARTFORM
    EXPORTING
      USER_SETTINGS        = ' '
      CONTROL_PARAMETERS   = LS_CONTROL
      OUTPUT_OPTIONS       = LS_OPTIONS
      GV_CHK_FECHA_IMP     = ''
      GS_EMISOR            = ST_EMISOR
      GS_SADR              = SADR
      WRBTR_T              = V_WRBTR_T
      DMBTR_T              = V_DMBTR_T
      WT_QBSHH_T           = V_WT_QBSHH_T
      IMP_NETO_T           = V_IMP_NETO_T
    IMPORTING
      DOCUMENT_OUTPUT_INFO = ST_DOCUMENT_OUTPUT_INFO
      JOB_OUTPUT_INFO      = ST_JOB_OUTPUT_INFO
      JOB_OUTPUT_OPTIONS   = ST_JOB_OUTPUT_OPTIONS
    TABLES
      GT_MPAGOS            = T_MPAGOS
      GT_PAGO_RET          = T_PAGO_RET
      GT_PAGO_POS          = T_PAGO_POS
    EXCEPTIONS
      FORMATTING_ERROR     = 1
      INTERNAL_ERROR       = 2
      SEND_ERROR           = 3
      USER_CANCELED        = 4
      OTHERS               = 5.

  IF SY-SUBRC <> 0.

*    MESSAGE ID SY-MSGID TYPE SY-MSGTY
*    NUMBER SY-MSGNO
*      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ELSE.

    IF SY-BATCH IS NOT INITIAL OR SY-TCODE EQ 'ZFIY0036' OR CK_INSTANCE EQ ABAP_TRUE.

      DATA: WA_ZLEST0007    TYPE ZLEST0007,
            WA_ZSDYT0052    TYPE ZSDYT0052,
            BIN_FILESIZE    TYPE I,
            PDF_TAB         LIKE TLINE OCCURS 0 WITH HEADER LINE,
            LS_PDF_STRING_X TYPE  XSTRING,
            LT_PDF          TYPE TABLE OF CHAR80,
            LS_PDF          TYPE CHAR80,
            FILENAME        TYPE STRING.

      SELECT SINGLE * INTO WA_ZLEST0007
        FROM ZLEST0007
       WHERE ID_INTERFACE = '33'
         AND ID_CTG       = 'PDF'
         AND PREFIX       = 'OP'.

      IF SY-SUBRC IS INITIAL.

        SELECT SINGLE * INTO WA_ZSDYT0052
          FROM ZSDYT0052
         WHERE BUKRS EQ ST_EMISOR-BUKRS
           AND GJAHR EQ ST_EMISOR-GJAHR
           AND AUGBL EQ ST_EMISOR-AUGBL.

        IF SY-SUBRC IS NOT INITIAL.
          WA_ZSDYT0052-BUKRS = ST_EMISOR-BUKRS.
          WA_ZSDYT0052-GJAHR = ST_EMISOR-GJAHR.
          WA_ZSDYT0052-AUGBL = ST_EMISOR-AUGBL.
        ENDIF.

        CONCATENATE 'OP_' ST_EMISOR-AUGBL '_' ST_EMISOR-LIFNR '.pdf' INTO WA_ZSDYT0052-NM_ARQUIVO.
*        IF SY-BATCH EQ ABAP_TRUE.
        CONCATENATE WA_ZLEST0007-PATHUNIX WA_ZSDYT0052-NM_ARQUIVO INTO FILENAME.

        CALL FUNCTION 'CONVERT_OTF'
          EXPORTING
            FORMAT        = 'PDF'
            MAX_LINEWIDTH = 132
          IMPORTING
            BIN_FILESIZE  = BIN_FILESIZE
            BIN_FILE      = LS_PDF_STRING_X
          TABLES
            OTF           = ST_JOB_OUTPUT_INFO-OTFDATA[]
            LINES         = PDF_TAB.

        IF SY-SUBRC IS INITIAL.

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              BUFFER     = LS_PDF_STRING_X
            TABLES
              BINARY_TAB = LT_PDF.

          IF CK_INSTANCE EQ ABAP_TRUE.
            HANDLE = ZCL_MEMORY_VARIAVEIS_AREA=>ATTACH_FOR_WRITE( INST_NAME = CONV #( NM_INSTANCE ) ).
            CREATE OBJECT ROOT AREA HANDLE HANDLE.
            HANDLE->SET_ROOT( ROOT ).
            CREATE OBJECT ROOT AREA HANDLE HANDLE TYPE ZCL_MEMORY_VARIAVEIS.
            OREF ?= ROOT.
            OREF->SET_TEXTO_XSTRING( I_XSTRING = LS_PDF_STRING_X ).
            OREF->SET_TEXTO_OTF( I_OTF = ST_JOB_OUTPUT_INFO-OTFDATA ).
            CLEAR OREF.
            HANDLE->SET_ROOT( ROOT ).
            HANDLE->DETACH_COMMIT( ).
            LEAVE PROGRAM.
          ENDIF.

          IF CK_INSTANCE EQ ABAP_FALSE.

            DATA: OBJ_ORDEM_PAGO TYPE REF TO ZCL_ORDEM_PAGO.
            CREATE OBJECT OBJ_ORDEM_PAGO.
            OBJ_ORDEM_PAGO->SET_DOCUMENTO(
              EXPORTING
                I_BUKRS      = ST_EMISOR-BUKRS
                I_GJAHR      = ST_EMISOR-GJAHR
                I_BELNR      = ST_EMISOR-AUGBL
                )->SET_WRITE_PDF( I_TEXTO_X = LS_PDF_STRING_X ).

*              OPEN DATASET FILENAME FOR OUTPUT IN BINARY MODE.
*              IF SY-SUBRC IS INITIAL.
*                LOOP AT LT_PDF INTO LS_PDF.
*                  TRANSFER LS_PDF TO FILENAME NO END OF LINE.
*                ENDLOOP.
*                CLOSE DATASET FILENAME.
*              ENDIF.
*
*              "Registrar que gerou
*              IF SY-SUBRC IS INITIAL.
*                WA_ZSDYT0052-CK_PRINT   = ABAP_TRUE.
*                MODIFY ZSDYT0052 FROM WA_ZSDYT0052.
*                COMMIT WORK.
*              ENDIF.

          ENDIF.

        ENDIF.

*        ELSE.
*          CONCATENATE WA_ZLEST0007-PATHWIN_REDE WA_ZSDYT0052-NM_ARQUIVO INTO FILENAME.
*
**--- Convert OTF to PDF
*          CALL FUNCTION 'CONVERT_OTF_2_PDF'
*            IMPORTING
*              BIN_FILESIZE           = V_BIN_FILESIZE
*            TABLES
*              OTF                    = ST_JOB_OUTPUT_INFO-OTFDATA
*              DOCTAB_ARCHIVE         = IT_DOCS
*              LINES                  = IT_LINES
*            EXCEPTIONS
*              ERR_CONV_NOT_POSSIBLE  = 1
*              ERR_OTF_MC_NOENDMARKER = 2
*              OTHERS                 = 3.
*          IF SY-SUBRC <> 0.
**      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*          ENDIF.
*
**--- Download PDF to local PC
*          MOVE FILENAME TO V_FILENAME.
*          CALL FUNCTION 'GUI_DOWNLOAD'
*            EXPORTING
*              BIN_FILESIZE            = V_BIN_FILESIZE
*              FILENAME                = V_FILENAME
*              FILETYPE                = 'BIN'
*            TABLES
*              DATA_TAB                = IT_LINES
*            EXCEPTIONS
*              FILE_WRITE_ERROR        = 1
*              NO_BATCH                = 2
*              GUI_REFUSE_FILETRANSFER = 3
*              INVALID_TYPE            = 4
*              NO_AUTHORITY            = 5
*              UNKNOWN_ERROR           = 6
*              HEADER_NOT_ALLOWED      = 7
*              SEPARATOR_NOT_ALLOWED   = 8
*              FILESIZE_NOT_ALLOWED    = 9
*              HEADER_TOO_LONG         = 10
*              DP_ERROR_CREATE         = 11
*              DP_ERROR_SEND           = 12
*              DP_ERROR_WRITE          = 13
*              UNKNOWN_DP_ERROR        = 14
*              ACCESS_DENIED           = 15
*              DP_OUT_OF_MEMORY        = 16
*              DISK_FULL               = 17
*              DP_TIMEOUT              = 18
*              FILE_NOT_FOUND          = 19
*              DATAPROVIDER_EXCEPTION  = 20
*              CONTROL_FLUSH_ERROR     = 21
*              OTHERS                  = 22.
*
*          IF SY-SUBRC IS INITIAL AND SY-TCODE EQ 'ZFIY0036'.
*            WA_ZSDYT0052-CK_PRINT   = ABAP_TRUE.
*            MODIFY ZSDYT0052 FROM WA_ZSDYT0052.
*            COMMIT WORK.
*          ENDIF.
*
*        ENDIF.

      ENDIF.
    ENDIF.

    IF SY-BATCH IS INITIAL AND RB_PRNT EQ 'X' AND SY-TCODE NE 'ZFIY0036' AND CK_INSTANCE EQ ABAP_FALSE.

*--- Convert OTF to PDF
      CALL FUNCTION 'CONVERT_OTF_2_PDF'
        IMPORTING
          BIN_FILESIZE           = V_BIN_FILESIZE
        TABLES
          OTF                    = ST_JOB_OUTPUT_INFO-OTFDATA
          DOCTAB_ARCHIVE         = IT_DOCS
          LINES                  = IT_LINES
        EXCEPTIONS
          ERR_CONV_NOT_POSSIBLE  = 1
          ERR_OTF_MC_NOENDMARKER = 2
          OTHERS                 = 3.
      IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

*--- Download PDF to local PC
      MOVE V_FULLPATH TO V_FILENAME.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          BIN_FILESIZE            = V_BIN_FILESIZE
          FILENAME                = V_FILENAME
          FILETYPE                = 'BIN'
        TABLES
          DATA_TAB                = IT_LINES
        EXCEPTIONS
          FILE_WRITE_ERROR        = 1
          NO_BATCH                = 2
          GUI_REFUSE_FILETRANSFER = 3
          INVALID_TYPE            = 4
          NO_AUTHORITY            = 5
          UNKNOWN_ERROR           = 6
          HEADER_NOT_ALLOWED      = 7
          SEPARATOR_NOT_ALLOWED   = 8
          FILESIZE_NOT_ALLOWED    = 9
          HEADER_TOO_LONG         = 10
          DP_ERROR_CREATE         = 11
          DP_ERROR_SEND           = 12
          DP_ERROR_WRITE          = 13
          UNKNOWN_DP_ERROR        = 14
          ACCESS_DENIED           = 15
          DP_OUT_OF_MEMORY        = 16
          DISK_FULL               = 17
          DP_TIMEOUT              = 18
          FILE_NOT_FOUND          = 19
          DATAPROVIDER_EXCEPTION  = 20
          CONTROL_FLUSH_ERROR     = 21
          OTHERS                  = 22.
      IF SY-SUBRC = 0.
*          MESSAGE 'Se han grabado todas las OP' TYPE 'S'.
*        ELSE.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      SELECT SINGLE VALOR
        INTO V_ENDERECO
        FROM ZPARAMETROS
       WHERE NOME_PARAMETRO = 'ENDERECO_PDF_PAGO' .

      IF SY-SUBRC IS INITIAL.
*--- Download PDF para servidor
        CONCATENATE V_ENDERECO V_FILENAME INTO V_FILENAME_S.
        "MOVE V_ENDERECO V_FILENAME TO V_FILENAME.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            BIN_FILESIZE            = V_BIN_FILESIZE
            FILENAME                = V_FILENAME_S
            FILETYPE                = 'BIN'
          TABLES
            DATA_TAB                = IT_LINES
          EXCEPTIONS
            FILE_WRITE_ERROR        = 1
            NO_BATCH                = 2
            GUI_REFUSE_FILETRANSFER = 3
            INVALID_TYPE            = 4
            NO_AUTHORITY            = 5
            UNKNOWN_ERROR           = 6
            HEADER_NOT_ALLOWED      = 7
            SEPARATOR_NOT_ALLOWED   = 8
            FILESIZE_NOT_ALLOWED    = 9
            HEADER_TOO_LONG         = 10
            DP_ERROR_CREATE         = 11
            DP_ERROR_SEND           = 12
            DP_ERROR_WRITE          = 13
            UNKNOWN_DP_ERROR        = 14
            ACCESS_DENIED           = 15
            DP_OUT_OF_MEMORY        = 16
            DISK_FULL               = 17
            DP_TIMEOUT              = 18
            FILE_NOT_FOUND          = 19
            DATAPROVIDER_EXCEPTION  = 20
            CONTROL_FLUSH_ERROR     = 21
            OTHERS                  = 22.
        IF SY-SUBRC = 0.
*            MESSAGE 'Se han grabado todas las OP' TYPE 'S'.
*          ELSE.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ENDIF.


      IF RB_PRNT EQ 'X'.
        LS_CONTROL-GETOTF = ''.
        "Impressão-- Para mandar para impressora tenho que gerar novamente com o campo getotf em branco
        CALL FUNCTION WGC_SMARTFORM
          EXPORTING
            USER_SETTINGS        = ' '
            CONTROL_PARAMETERS   = LS_CONTROL
            OUTPUT_OPTIONS       = LS_OPTIONS
            GV_CHK_FECHA_IMP     = ''
            GS_EMISOR            = ST_EMISOR
            GS_SADR              = SADR
            WRBTR_T              = V_WRBTR_T
            DMBTR_T              = V_DMBTR_T
            WT_QBSHH_T           = V_WT_QBSHH_T
            IMP_NETO_T           = V_IMP_NETO_T
          IMPORTING
            DOCUMENT_OUTPUT_INFO = ST_DOCUMENT_OUTPUT_INFO
            JOB_OUTPUT_INFO      = ST_JOB_OUTPUT_INFO
            JOB_OUTPUT_OPTIONS   = ST_JOB_OUTPUT_OPTIONS
          TABLES
            GT_MPAGOS            = T_MPAGOS
            GT_PAGO_RET          = T_PAGO_RET
            GT_PAGO_POS          = T_PAGO_POS
          EXCEPTIONS
            FORMATTING_ERROR     = 1
            INTERNAL_ERROR       = 2
            SEND_ERROR           = 3
            USER_CANCELED        = 4
            OTHERS               = 5.

        IF SY-SUBRC <> 0.

*             MESSAGE ID SY-MSGID TYPE SY-MSGTY
*             NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

        ELSE.

          MOVE V_FULLPATH TO V_FILENAME.
          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              BIN_FILESIZE            = V_BIN_FILESIZE
              FILENAME                = V_FILENAME
              FILETYPE                = 'BIN'
            TABLES
              DATA_TAB                = IT_LINES
            EXCEPTIONS
              FILE_WRITE_ERROR        = 1
              NO_BATCH                = 2
              GUI_REFUSE_FILETRANSFER = 3
              INVALID_TYPE            = 4
              NO_AUTHORITY            = 5
              UNKNOWN_ERROR           = 6
              HEADER_NOT_ALLOWED      = 7
              SEPARATOR_NOT_ALLOWED   = 8
              FILESIZE_NOT_ALLOWED    = 9
              HEADER_TOO_LONG         = 10
              DP_ERROR_CREATE         = 11
              DP_ERROR_SEND           = 12
              DP_ERROR_WRITE          = 13
              UNKNOWN_DP_ERROR        = 14
              ACCESS_DENIED           = 15
              DP_OUT_OF_MEMORY        = 16
              DISK_FULL               = 17
              DP_TIMEOUT              = 18
              FILE_NOT_FOUND          = 19
              DATAPROVIDER_EXCEPTION  = 20
              CONTROL_FLUSH_ERROR     = 21
              OTHERS                  = 22.
          IF SY-SUBRC = 0.
*              MESSAGE 'Se han grabado todas las OP' TYPE 'S'.
*            ELSE.
*              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        ENDIF  .

      ENDIF.

    ENDIF.

  ENDIF.

  REFRESH: T_MPAGOS,
           T_PAGO_RET,
           T_PAGO_POS.

ENDFORM.                    " LLAMO_SMARTFORMS
*&---------------------------------------------------------------------*
*&      Form  F_EMPRESA_EMISORA
*&---------------------------------------------------------------------*
FORM F_EMPRESA_EMISORA USING PI_BUKRS
                              LS_VENDOR_ADDRESS TYPE DKADR.

  READ TABLE T_EMPRESA INTO ST_EMPRESA
  WITH KEY BUKRS = PI_BUKRS.
  IF SY-SUBRC EQ 0.
    ST_EMISOR-BUKRS   = ST_EMPRESA-BUKRS.
    ST_EMISOR-NAME1_1 = ST_EMPRESA-NAME1_1.
    ST_EMISOR-NAME2_1 = ST_EMPRESA-NAME2_1.
    ST_EMISOR-NAME3_1 = ST_EMPRESA-NAME3_1.
    ST_EMISOR-NAME4_1 = ST_EMPRESA-NAME4_1.
    ST_EMISOR-STRAS_1 = ST_EMPRESA-STRAS_1.
    ST_EMISOR-ORT01_1 = ST_EMPRESA-ORT01_1.
    ST_EMISOR-ORT02_1 = ST_EMPRESA-ORT02_1.
    ST_EMISOR-STCD1_1 = ST_EMPRESA-STCD1_1.
  ENDIF.

ENDFORM.                    "f_empresa_emisora
*&---------------------------------------------------------------------*
*&      Form  OBTENER_PROVEEDOR
*&---------------------------------------------------------------------*
FORM OBTENER_PROVEEDOR  USING    LS_DOCDATA_ALL TYPE TY_DOCDATA
                        CHANGING LS_AUGBL  TYPE ZFIYS_ORDEN_PAGOS_CAB.


  DATA: LS_XLFA1     TYPE LFA1,
        LS_INFO_DATA TYPE J_1AI02.

  LOOP AT GT_INFO_DATA INTO LS_INFO_DATA
     WHERE AUGBL = LS_DOCDATA_ALL-AUGBL.
    IF LS_INFO_DATA-STCD1_2 IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDLOOP.

  DATA STL_BSEG TYPE BSEG.

  LOOP AT T_BSEG INTO STL_BSEG
    WHERE BELNR EQ LS_DOCDATA_ALL-AUGBL
      AND LIFNR NE SPACE.
    EXIT.
  ENDLOOP.

  READ TABLE XLFA1 INTO LS_XLFA1
  WITH KEY LIFNR  = STL_BSEG-LIFNR.

  MOVE:
       LS_DOCDATA_ALL-BUKRS TO LS_AUGBL-BUKRS,
       LS_DOCDATA_ALL-GJAHR TO LS_AUGBL-GJAHR,
       LS_XLFA1-STCD1       TO LS_AUGBL-STCD1,
       LS_XLFA1-NAME1       TO LS_AUGBL-NAME1,
       LS_XLFA1-NAME2       TO LS_AUGBL-NAME2,
       LS_XLFA1-NAME3       TO LS_AUGBL-NAME3,
       LS_XLFA1-NAME4       TO LS_AUGBL-NAME4,
       LS_XLFA1-STRAS       TO LS_AUGBL-STRAS,
       LS_XLFA1-ORT01       TO LS_AUGBL-ORT01,
       LS_XLFA1-ORT02       TO LS_AUGBL-ORT02,
       LS_XLFA1-TELF1       TO LS_AUGBL-TELF1,
       LS_XLFA1-LIFNR       TO LS_AUGBL-LIFNR.

  MOVE LS_DOCDATA_ALL-AUGBL TO LS_AUGBL-AUGBL.

ENDFORM.                    " OBTENER_PROVEEDOR

*&---------------------------------------------------------------------*
*&      Form  READ_COMPANY_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM READ_COMPANY_DATA.

  SELECT SINGLE * FROM T001 WHERE BUKRS EQ BKPF-BUKRS.

  CLEAR: ADDR1_SEL, SADR.
  ADDR1_SEL-ADDRNUMBER = T001-ADRNR.                        "SADR40A

  CALL FUNCTION 'ADDR_GET'
    EXPORTING
      ADDRESS_SELECTION = ADDR1_SEL
      ADDRESS_GROUP     = 'CA01'
*     read_sadr_only    = 'X'
    IMPORTING
      SADR              = SADR.

  MOVE SADR-ANRED TO GS_COMPANY_ADDRESS-ANRED.
  MOVE SADR-NAME1 TO GS_COMPANY_ADDRESS-NAME1.
  MOVE SADR-NAME2 TO GS_COMPANY_ADDRESS-NAME2.
  MOVE SADR-NAME3 TO GS_COMPANY_ADDRESS-NAME3.
  MOVE SADR-NAME4 TO GS_COMPANY_ADDRESS-NAME4.
  MOVE SADR-STRAS TO GS_COMPANY_ADDRESS-STRAS.
  MOVE SADR-PFACH TO GS_COMPANY_ADDRESS-PFACH.
  MOVE SADR-PSTL2 TO GS_COMPANY_ADDRESS-PSTL2.
  MOVE SADR-ORT01 TO GS_COMPANY_ADDRESS-ORT02.
  MOVE SADR-ORT02 TO GS_COMPANY_ADDRESS-ORT02.
  MOVE SADR-PSTLZ TO GS_COMPANY_ADDRESS-PSTLZ.
  MOVE SADR-LAND1 TO GS_COMPANY_ADDRESS-LAND1.
  MOVE SADR-REGIO TO GS_COMPANY_ADDRESS-REGIO.
  MOVE SADR-ADRNR TO GS_COMPANY_ADDRESS-ADRNR.

* Buscar el cuit de la empresa de emision
  CLEAR: V_CUIT.

  SELECT SINGLE PAVAL
  FROM T001Z
  INTO V_CUIT
  WHERE BUKRS EQ T001-BUKRS
  AND   PARTY EQ 'J1AIDN'.

  MOVE:
        T001-BUKRS TO ST_EMPRESA-BUKRS,
        SADR-NAME1 TO ST_EMPRESA-NAME1_1,
        SADR-NAME2 TO ST_EMPRESA-NAME2_1,
        SADR-NAME3 TO ST_EMPRESA-NAME3_1,
        SADR-NAME4 TO ST_EMPRESA-NAME4_1,
        SADR-STRAS TO ST_EMPRESA-STRAS_1,
        SADR-ORT01 TO ST_EMPRESA-ORT01_1,
        SADR-ORT02 TO ST_EMPRESA-ORT02_1.

  CONCATENATE V_CUIT(2)   '-'
              V_CUIT+2(8) '-'
              V_CUIT+10(1)
         INTO ST_EMPRESA-STCD1_1.

  COLLECT ST_EMPRESA INTO T_EMPRESA.


  SELECT SINGLE * FROM T001Z WHERE BUKRS = T001-BUKRS
                             AND   PARTY = 'J1ATID'.

  XT001-STCDT = T001Z-PAVAL.

  CLEAR T001Z.
  SELECT SINGLE * FROM T001Z WHERE BUKRS = T001-BUKRS
                             AND   PARTY = 'J1AIDN'.

  XT001-STCD1 = T001Z-PAVAL.

  CLEAR T001Z.
  SELECT SINGLE * FROM T001Z WHERE BUKRS = T001-BUKRS
                             AND   PARTY = 'J1AFTV'.

  XT001-FITYP = T001Z-PAVAL.

  CLEAR T001Z.
  SELECT SINGLE * FROM T001Z WHERE BUKRS = T001-BUKRS
                             AND   PARTY = 'J1AGIN'.

  XSTCD2 = T001Z-PAVAL.
  CALL FUNCTION 'J_1A_PUT_DASHES_TO_STCD2'
    EXPORTING
      I_STCD2 = XSTCD2
    IMPORTING
      E_STCD2 = XT001-STCD2.

  CLEAR T001Z.
  SELECT SINGLE * FROM T001Z WHERE BUKRS = T001-BUKRS
                             AND   PARTY = 'J1AFDT'.

  XT001-FND_DATE = T001Z-PAVAL.

ENDFORM.                               " READ_COMPANY_DATA
*&---------------------------------------------------------------------*
*&      Form  OPEN_SAPSCRIPT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM OPEN_SAPSCRIPT.

*  itcpo-tdimmed    = '*'.              " kz sofort drucken
*  itcpo-tddelete   = '*'.              " kz freigbe nach Druck
*  itcpo-tdlifetime = '7'.              " verfalltage
*  itcpo-tdpreview  = 'X'.              " druckansicht
*
*  CALL FUNCTION 'OPEN_FORM'            " open form for output
*       EXPORTING form    = s_form
*                 dialog  = 'X'
*                 OPTIONS = itcpo
*                 device  = 'PRINTER'.

ENDFORM.                               " OPEN_SAPSCRIPT


*&---------------------------------------------------------------------*
*&      Form  READ_WITHHLD_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM READ_WITHHLD_DATA USING F_BELNR F_GJAHR F_BUZEI F_QBSHB F_QBSHH.
* read the withholding data segment
  SELECT * FROM WITH_ITEM WHERE BUKRS     EQ BKPF-BUKRS
                         AND   BELNR      EQ F_BELNR
                         AND   GJAHR      EQ F_GJAHR
                         AND   BUZEI      EQ F_BUZEI
                         AND   WT_WITHCD  NE SPACE
                         AND   WT_STAT    EQ SPACE
                         AND   WT_GRUWTPD EQ SPACE
                         AND  WITHT NE 'FR'
                         AND   WT_QBSHB   NE 0.

    PERFORM READ_WITHHLD_CODE.                             " Note 457832
    CHECK XT059Z-WT_POSIN NE '2'.         " no grossing up - Note 457832

    ADD: WITH_ITEM-WT_QBSHB TO F_QBSHB,
         WITH_ITEM-WT_QBSHH TO F_QBSHH.
  ENDSELECT.
ENDFORM.                               " READ_WITHHLD_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_CPD_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM READ_CPD_DATA USING F_BELNR F_BUZEI F_GJAHR.
  CLEAR XBSEC.
  SELECT SINGLE * FROM BSEC WHERE BUKRS = BKPF-BUKRS
                            AND   BELNR = F_BELNR
                            AND   GJAHR = F_GJAHR
                            AND   BUZEI = F_BUZEI.

  MOVE-CORRESPONDING BSEC TO XBSEC.

* foreign id?
  IF BSEC-LAND1 NE T001-LAND1.
    PERFORM READ_FOREIGN_ID
            USING BSEC-LAND1 BSEC-STKZN
            CHANGING XBSEC-STCD1.
  ENDIF.

  APPEND XBSEC.
ENDFORM.                               " READ_CPD_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_MASTER_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM READ_MASTER_DATA.
  CLEAR: LFA1, XLFA1, XBSEC.

  IF XXCPDD NE SPACE.
    LOOP AT XBSEC WHERE EMPFG = CLRDTAB-EMPFG.
* MOD GB - Begin 08/11/2010
      MOVE-CORRESPONDING XBSEC TO XLFA1.
      APPEND XLFA1.
* MOD GB - End 08/11/2010
      EXIT.
    ENDLOOP.
    CHECK 1 = 2.
  ENDIF.

  READ TABLE XLFA1 WITH KEY MANDT = SY-MANDT
                            LIFNR = CLRDTAB-LIFNR.

  CHECK SY-SUBRC NE 0.

  SELECT SINGLE * FROM LFA1 WHERE LIFNR = CLRDTAB-LIFNR.

  IF NOT LFA1-FISKN IS INITIAL.
    SELECT SINGLE * FROM LFA1 WHERE LIFNR = LFA1-FISKN.
  ENDIF.

  MOVE-CORRESPONDING LFA1 TO XLFA1.

* foreign id?
  IF LFA1-LAND1 NE T001-LAND1.
    PERFORM READ_FOREIGN_ID
            USING LFA1-LAND1 LFA1-STKZN
            CHANGING XLFA1-STCD1.
  ENDIF.

  APPEND XLFA1.
ENDFORM.                               " READ_MASTER_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_DOCUMENT_TYPE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM READ_DOCUMENT_TYPE USING F_BLART.
  CHECK XT003-BLART NE F_BLART.

  CLEAR XT003.

  READ TABLE XT003 WITH KEY F_BLART.

  CHECK SY-SUBRC NE 0.

  CLEAR: T003T.

* read the document type text
  SELECT SINGLE * FROM T003T WHERE SPRAS = 'S'
                             AND   BLART = F_BLART.

  MOVE-CORRESPONDING: T003T TO XT003.

  APPEND XT003.
ENDFORM.                               " READ_DOCUMENT_TYPE
*&---------------------------------------------------------------------*
*&      Form  START_SAPSCRIPT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM START_SAPSCRIPT.

*  CALL FUNCTION 'START_FORM'
*    EXPORTING
*      form      = s_form
*      startpage = 'PAGE1'.

ENDFORM.                               " START_SAPSCRIPT
*&---------------------------------------------------------------------*
*&      Form  FILL_GENERAL_FIELDS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM FILL_GENERAL_FIELDS.
  CLEAR: J_1AI02, BSEC, LFA1.

* window prt_char
  J_1AI02-PRTCHR  = XPRTCHR-J_1APRTCHR.


  MOVE J_1AI02-PRTCHR  TO GS_INFO_DATA-PRTCHR.

* window cc_data1
  PERFORM READ_FISCAL_VAT USING XT001-FITYP.
  J_1AI02-TEXT1 = XFITPVT-TEXT60.


  MOVE J_1AI02-TEXT1  TO GS_INFO_DATA-TEXT1.

* window info-1, info-2
  IF S_PDATE IS INITIAL.
    J_1AI02-PRDATE  = BKPF-BUDAT.


    MOVE J_1AI02-PRDATE  TO GS_INFO_DATA-PRDATE.

  ELSE.
    J_1AI02-PRDATE  = S_PDATE.


    MOVE J_1AI02-PRDATE  TO GS_INFO_DATA-PRDATE.

  ENDIF.

  J_1AI02-AUGBL   = BKPF-BELNR.
  J_1AI02-ISNR    = BKPF-XBLNR(4).
  J_1AI02-OFFNUM  = BKPF-XBLNR+5(8).
  J_1AI02-TODC_1  = XT001-STCDT.
  J_1AI02-STCD1_1 = XT001-STCD1.
  J_1AI02-STCD2_1 = XT001-STCD2.
  J_1AI02-FNDDATE = XT001-FND_DATE.


  MOVE J_1AI02-AUGBL  TO GS_INFO_DATA-AUGBL.
  MOVE J_1AI02-ISNR  TO GS_INFO_DATA-ISNR.
  MOVE J_1AI02-OFFNUM  TO GS_INFO_DATA-OFFNUM.
  MOVE J_1AI02-TODC_1  TO GS_INFO_DATA-TODC_1.
  MOVE J_1AI02-STCD1_1  TO GS_INFO_DATA-STCD1_1.
  MOVE J_1AI02-STCD2_1  TO GS_INFO_DATA-STCD2_1.
  MOVE J_1AI02-FNDDATE  TO GS_INFO_DATA-FNDDATE.

* window v_data1
  CLEAR DKADR.
  IF XXCPDD NE SPACE.
    MOVE-CORRESPONDING XBSEC TO DKADR.

    DKADR-INLND     = T001-LAND1.
    DKADR-KONTO     = CLRDTAB-LIFNR.
    J_1AI02-TODC_2  = XBSEC-STCDT.
    J_1AI02-STCD1_2 = XBSEC-STCD1.


    MOVE DKADR-INLND TO GS_VENDOR_ADDRESS1-INLND.
    MOVE DKADR-KONTO TO GS_VENDOR_ADDRESS1-KONTO.
    MOVE DKADR-ANRED TO GS_VENDOR_ADDRESS1-ANRED.
    MOVE DKADR-NAME1 TO GS_VENDOR_ADDRESS1-NAME1.
    MOVE DKADR-NAME2 TO GS_VENDOR_ADDRESS1-NAME2.
    MOVE DKADR-NAME3 TO GS_VENDOR_ADDRESS1-NAME3.
    MOVE DKADR-NAME4 TO GS_VENDOR_ADDRESS1-NAME4.
    MOVE DKADR-STRAS TO GS_VENDOR_ADDRESS1-STRAS.
    MOVE DKADR-PFACH TO GS_VENDOR_ADDRESS1-PFACH.
    MOVE DKADR-PSTL2 TO GS_VENDOR_ADDRESS1-PSTL2.
    MOVE DKADR-ORT01 TO GS_VENDOR_ADDRESS1-ORT01.
    MOVE DKADR-ORT02 TO GS_VENDOR_ADDRESS1-ORT02.
    MOVE DKADR-PSTLZ TO GS_VENDOR_ADDRESS1-PSTLZ.
    MOVE DKADR-LAND1 TO GS_VENDOR_ADDRESS1-LAND1.
    MOVE DKADR-REGIO TO GS_VENDOR_ADDRESS1-REGIO.
* MOD GB - Begin 08/11/2010
    MOVE J_1AI02-AUGBL TO GS_VENDOR_ADDRESS1-AUGBL.
* MOD GB - End 08/11/2010

* MOD GB - Begin 08/11/2010
    APPEND GS_VENDOR_ADDRESS1 TO GT_VENDOR_ADDRESS.
* MOD GB - End 08/11/2010


    IF NOT XBSEC-FITYP IS INITIAL.
      PERFORM READ_FISCAL_VAT USING XBSEC-FITYP.
      J_1AI02-TEXT2 = J_1AFITPVT-TEXT60.
    ENDIF.

    CALL FUNCTION 'J_1A_PUT_DASHES_TO_STCD2'
      EXPORTING
        I_STCD2 = XBSEC-STCD2
      IMPORTING
        E_STCD2 = J_1AI02-STCD2_2.

    MOVE-CORRESPONDING XBSEC TO BSEC.
  ELSE.
    MOVE-CORRESPONDING XLFA1 TO DKADR.

    DKADR-INLND     = T001-LAND1.
    DKADR-KONTO     = XLFA1-LIFNR.
    J_1AI02-TODC_2  = XLFA1-STCDT.
    J_1AI02-STCD1_2 = XLFA1-STCD1.


    MOVE DKADR-INLND TO GS_VENDOR_ADDRESS1-INLND.
    MOVE DKADR-KONTO TO GS_VENDOR_ADDRESS1-KONTO.
    MOVE DKADR-ANRED TO GS_VENDOR_ADDRESS1-ANRED.
    MOVE DKADR-NAME1 TO GS_VENDOR_ADDRESS1-NAME1.
    MOVE DKADR-NAME2 TO GS_VENDOR_ADDRESS1-NAME2.
    MOVE DKADR-NAME3 TO GS_VENDOR_ADDRESS1-NAME3.
    MOVE DKADR-NAME4 TO GS_VENDOR_ADDRESS1-NAME4.
    MOVE DKADR-STRAS TO GS_VENDOR_ADDRESS1-STRAS.
    MOVE DKADR-PFACH TO GS_VENDOR_ADDRESS1-PFACH.
    MOVE DKADR-PSTL2 TO GS_VENDOR_ADDRESS1-PSTL2.
    MOVE DKADR-ORT01 TO GS_VENDOR_ADDRESS1-ORT01.
    MOVE DKADR-ORT02 TO GS_VENDOR_ADDRESS1-ORT02.
    MOVE DKADR-PSTLZ TO GS_VENDOR_ADDRESS1-PSTLZ.
    MOVE DKADR-LAND1 TO GS_VENDOR_ADDRESS1-LAND1.
    MOVE DKADR-REGIO TO GS_VENDOR_ADDRESS1-REGIO.
    MOVE J_1AI02-AUGBL TO GS_VENDOR_ADDRESS1-AUGBL.

    APPEND GS_VENDOR_ADDRESS1 TO GT_VENDOR_ADDRESS.


    IF NOT XLFA1-FITYP IS INITIAL.
      PERFORM READ_FISCAL_VAT USING XLFA1-FITYP.
      J_1AI02-TEXT2 = J_1AFITPVT-TEXT60.
    ENDIF.

    CALL FUNCTION 'J_1A_PUT_DASHES_TO_STCD2'
      EXPORTING
        I_STCD2 = XLFA1-STCD2
      IMPORTING
        E_STCD2 = J_1AI02-STCD2_2.

    MOVE-CORRESPONDING XLFA1 TO LFA1.
  ENDIF.

* window copy_no
  J_1AI02-COPYNO  = COPY_NO.

* window text1
  J_1AI02-BUDAT   = BKPF-BUDAT.


  MOVE J_1AI02-BUDAT TO GS_INFO_DATA-BUDAT.
  MOVE J_1AI02-COPYNO TO GS_INFO_DATA-COPYNO.
  MOVE BKPF-BELNR TO GS_INFO_DATA-BELNR.


ENDFORM.                               " FILL_GENERAL_FIELDS
*&---------------------------------------------------------------------*
*&      Form  READ_FISCAL_VAT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM READ_FISCAL_VAT USING F_FISCAL_VAT.
  CLEAR: J_1AFITPVT, XFITPVT.

  READ TABLE XFITPVT WITH KEY MANDT    = SY-MANDT
                              SPRAS    = 'S'
                             J_1AFITP = F_FISCAL_VAT.

  CHECK SY-SUBRC NE 0.

  SELECT SINGLE * FROM J_1AFITPVT WHERE SPRAS    = 'S'
                                 AND   J_1AFITP = F_FISCAL_VAT.

  CHECK SY-SUBRC EQ 0.

  MOVE-CORRESPONDING J_1AFITPVT TO XFITPVT.
  APPEND XFITPVT.
ENDFORM.                               " READ_FISCAL_VAT
*&---------------------------------------------------------------------*
*&      Form  PRINT_CLRDTAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM PRINT_CLRDTAB.

* read clreared documents
  LOOP AT CLRDTAB.
    AT NEW LIFNR.
      PERFORM READ_MASTER_DATA.


*      IF NOT p_script IS INITIAL.
*        PERFORM start_sapscript.
*      ENDIF.

      PERFORM FILL_GENERAL_FIELDS.

* call customer-function '002'
      CALL FUNCTION 'J_1A_EXIT_J_1AF012'
        EXPORTING
          I_BKPF    = BKPF
          I_SADR    = SADR
          I_DKADR   = DKADR
          I_LFA1    = LFA1
          I_BSEC    = BSEC
          I_J_1AI02 = J_1AI02
        IMPORTING
          E_J_1AI02 = J_1AI02
          E_SADR    = SADR
          E_DKADR   = DKADR.


      IF NOT P_SCRIPT IS INITIAL.
        IF NOT COPY_NO IS INITIAL.
*          CALL FUNCTION 'WRITE_FORM'
*            EXPORTING
*              window  = 'COPY_NO'
*              element = 'LINE'.
        ENDIF.

        MOVE XPRTCHR-J_1ADISVAT TO GV_VAT_NUMBER.


        IF NOT XPRTCHR-J_1ADISVAT IS INITIAL AND
           XUMSKS                 NE SPACE.

*          CALL FUNCTION 'WRITE_FORM'
*            EXPORTING
*              window  = 'HEADER'
*              element = 'NET'.

        ELSE.

*          CALL FUNCTION 'WRITE_FORM'
*            EXPORTING
*              window  = 'HEADER'
*              element = 'GROSS'.

        ENDIF.

      ENDIF.
      SUM.

* window text1
      J_1AI02-TEXT4   = BKPF-WAERS.
      XWRBTR = CLRDTAB-WRBTR + CLRDTAB-WMWST + CLRDTAB-QBSHB.

      WRITE XWRBTR CURRENCY BKPF-WAERS TO J_1AI02-TEXT4+10.

      CONDENSE J_1AI02-TEXT4.


      MOVE J_1AI02-TEXT4 TO GS_INFO_DATA-TEXT4.
      MOVE J_1AI02-STCD1_2 TO GS_INFO_DATA-STCD1_2.
      MOVE J_1AI02-STCD2_2 TO GS_INFO_DATA-STCD2_2.
      MOVE J_1AI02-TEXT2 TO GS_INFO_DATA-TEXT2.

      APPEND GS_INFO_DATA TO GT_INFO_DATA .

* end
    ENDAT.

    AT NEW BLART.
      PERFORM READ_DOCUMENT_TYPE USING CLRDTAB-BLART.
    ENDAT.

    GS_DOCDATA-BUKRS = BKPF-BUKRS.
    J_1AI02-ISNR2   = CLRDTAB-XBLNR(4).
    J_1AI02-PRTCHR2 = CLRDTAB-XBLNR+4(1).
    J_1AI02-OFFNUM2 = CLRDTAB-XBLNR+5(8).
    J_1AI02-TEXT3   = XT003-LTEXT.
    J_1AI02-BELNR   = CLRDTAB-BELNR.
    J_1AI02-BUDAT2  = CLRDTAB-BUDAT.
    J_1AI02-FWSTE   = CLRDTAB-WMWST.
    J_1AI02-QBSHB   = CLRDTAB-QBSHB.
    J_1AI02-WRBTR   = CLRDTAB-WRBTR.
    J_1AI02-WAERS   = BKPF-WAERS.
    MOVE: GS_DOCDATA-BUKRS TO GS_TOTAL-BUKRS,
          GS_DOCDATA-BUKRS TO J_1AI02-BUKRS.
    MOVE J_1AI02-ISNR2 TO GS_DOCDATA-ISNR2.
    MOVE J_1AI02-PRTCHR2 TO GS_DOCDATA-PRTCHR2.
    MOVE J_1AI02-OFFNUM2 TO GS_DOCDATA-OFFNUM2.
    MOVE J_1AI02-TEXT3 TO GS_DOCDATA-TEXT3.
    MOVE J_1AI02-BELNR TO GS_DOCDATA-BELNR.
    MOVE J_1AI02-BUDAT2 TO GS_DOCDATA-BUDAT2.
    MOVE J_1AI02-FWSTE TO GS_DOCDATA-FWSTE.
    MOVE J_1AI02-QBSHB TO GS_DOCDATA-QBSHB.
    MOVE J_1AI02-QBSHB TO GS_TOTAL-QBSHB.
    MOVE J_1AI02-WRBTR TO GS_DOCDATA-WRBTR.
    MOVE J_1AI02-WRBTR TO GS_TOTAL-WRBTR.
    MOVE J_1AI02-WAERS TO GS_DOCDATA-WAERS.
    MOVE J_1AI02-WAERS TO GS_TOTAL-WAERS.
    MOVE J_1AI02-AUGBL TO GS_DOCDATA-AUGBL.
    MOVE T001-WAERS TO GV_COMPCURRENCY.


    IF CLRDTAB-QBSHB IS INITIAL.
      CLEAR J_1AI02-PDATA.
    ELSE.
      J_1AI02-PDATA = 'X'.
    ENDIF.

    IF NOT XPRTCHR-J_1ADISVAT IS INITIAL AND
       XUMSKS                 NE SPACE.
      PERFORM PRINT_NET.
    ELSE.
      PERFORM PRINT_GROSS.
    ENDIF.

    AT END OF LIFNR.
      SUM.
      CLEAR J_1AI02-WAERS2.

      J_1AI02-QBSHB = CLRDTAB-QBSHB.
      J_1AI02-WRBTR = CLRDTAB-WRBTR + CLRDTAB-WMWST.
      J_1AI02-WAERS = BKPF-WAERS.


      MOVE J_1AI02-QBSHB TO GS_DOCDATA-QBSHB.
      MOVE J_1AI02-QBSHB TO GS_TOTAL-QBSHB.
      MOVE J_1AI02-WRBTR TO GS_DOCDATA-WRBTR.
      MOVE J_1AI02-WRBTR TO GS_TOTAL-WRBTR.
      MOVE J_1AI02-WAERS TO GS_TOTAL-WAERS.


      IF T001-WAERS NE BKPF-WAERS.
        J_1AI02-QBSHH  = CLRDTAB-QBSHH.
        J_1AI02-DMBTR  = CLRDTAB-DMBTR + CLRDTAB-MWSTS.
        J_1AI02-WAERS2 = T001-WAERS.


        MOVE J_1AI02-QBSHH TO GS_DOCDATA-QBSHH.
        MOVE J_1AI02-QBSHH TO GS_TOTAL-QBSHH.
        MOVE J_1AI02-DMBTR  TO GS_DOCDATA-DMBTR.
        MOVE J_1AI02-DMBTR  TO GS_TOTAL-DMBTR .
        MOVE J_1AI02-WAERS2 TO GS_DOCDATA-WAERS2.
        MOVE J_1AI02-WAERS2 TO GS_TOTAL-WAERS2.

      ENDIF.

      MOVE J_1AI02-AUGBL TO GS_TOTAL-AUGBL.
      APPEND GS_TOTAL TO GT_TOTAL.

      IF NOT P_SCRIPT IS INITIAL.

*        CALL FUNCTION 'WRITE_FORM'
*          EXPORTING
*            element = 'TOTAL'.
*        CALL FUNCTION 'END_FORM'.

      ENDIF.
    ENDAT.
  ENDLOOP.

ENDFORM.                               " PRINT_CLRDTAB

*&---------------------------------------------------------------------*
*&      Form  PRINT_GROSS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*

FORM PRINT_GROSS.

  J_1AI02-WRBTR = CLRDTAB-WRBTR + CLRDTAB-WMWST.


  MOVE J_1AI02-WRBTR TO GS_DOCDATA-WRBTR.

  IF T001-WAERS = BKPF-WAERS.
    IF NOT P_SCRIPT IS INITIAL.
*      CALL FUNCTION 'WRITE_FORM'
*        EXPORTING
*          element = 'GROSS_DC'.
    ENDIF.

  ELSE.
    J_1AI02-QBSHH  = CLRDTAB-QBSHH.
    J_1AI02-DMBTR  = CLRDTAB-DMBTR + CLRDTAB-MWSTS.
    J_1AI02-WAERS2 = T001-WAERS.


    MOVE J_1AI02-QBSHH TO GS_DOCDATA-QBSHH.
    MOVE J_1AI02-QBSHH TO GS_TOTAL-QBSHH.
    MOVE J_1AI02-DMBTR TO GS_DOCDATA-DMBTR.
    MOVE J_1AI02-DMBTR TO GS_TOTAL-DMBTR.
    MOVE J_1AI02-WAERS2 TO GS_DOCDATA-WAERS2.
    MOVE J_1AI02-WAERS2 TO GS_TOTAL-WAERS2.

    IF NOT P_SCRIPT IS INITIAL.
*      CALL FUNCTION 'WRITE_FORM'
*        EXPORTING
*          element = 'GROSS_LC'.
    ENDIF.
  ENDIF.

  MOVE: BKPF-GJAHR   TO GS_DOCDATA-GJAHR.
  MOVE CLRDTAB-DMBTR TO GS_DOCDATA-DMBTR.
  MOVE:  CLRDTAB-BUZEI TO GS_DOCDATA-BUZEI.
  APPEND GS_DOCDATA TO GT_DOCDATA_ALL.


ENDFORM.                               " PRINT_GROSS

*&---------------------------------------------------------------------*
*&      Form  PRINT_NET
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM PRINT_NET.

  IF NOT P_SCRIPT IS INITIAL.
*    CALL FUNCTION 'WRITE_FORM'
**      EXPORTING
*        element = 'NET_DC'.
  ENDIF.

*  CHECK t001-waers NE bkpf-waers.

  MOVE J_1AI02-HWSTE TO GS_DOCDATA-HWSTE.
  MOVE J_1AI02-QBSHH TO GS_DOCDATA-QBSHH.
  MOVE J_1AI02-QBSHH TO GS_TOTAL-QBSHH.
  MOVE J_1AI02-DMBTR TO GS_DOCDATA-DMBTR.
  MOVE J_1AI02-DMBTR TO GS_TOTAL-DMBTR.
  MOVE J_1AI02-WAERS TO GS_DOCDATA-WAERS.
  MOVE J_1AI02-WAERS TO GS_TOTAL-WAERS.
  MOVE J_1AI02-WAERS TO GS_DOCDATA-WAERS2.

  IF T001-WAERS = BKPF-WAERS.
    MOVE:  BKPF-GJAHR   TO GS_DOCDATA-GJAHR.
    MOVE:  CLRDTAB-BUZEI TO GS_DOCDATA-BUZEI.
    APPEND GS_DOCDATA TO GT_DOCDATA_ALL.
  ENDIF.

  CHECK T001-WAERS NE BKPF-WAERS.

  J_1AI02-HWSTE = CLRDTAB-MWSTS.
  J_1AI02-QBSHH = CLRDTAB-QBSHH.
  J_1AI02-DMBTR = CLRDTAB-DMBTR.
  J_1AI02-WAERS = T001-WAERS.


  IF T001-WAERS NE BKPF-WAERS.
    MOVE J_1AI02-HWSTE TO GS_DOCDATA-HWSTE.
    MOVE J_1AI02-QBSHH TO GS_DOCDATA-QBSHH.
    MOVE J_1AI02-DMBTR TO GS_DOCDATA-DMBTR.
    MOVE J_1AI02-WAERS TO GS_DOCDATA-WAERS2.
    MOVE: BKPF-GJAHR   TO GS_DOCDATA-GJAHR.
    MOVE:  CLRDTAB-BUZEI TO GS_DOCDATA-BUZEI.
    APPEND GS_DOCDATA TO GT_DOCDATA_ALL.
  ENDIF.

  IF NOT P_SCRIPT IS INITIAL.
*    CALL FUNCTION 'WRITE_FORM'
*      EXPORTING
*        element = 'NET_LC'.
  ENDIF.


ENDFORM.                               " PRINT_NET
*&---------------------------------------------------------------------*
*&      Form  READ_CLRD_DOCUMENTS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM READ_CLRD_DOCUMENTS.
  DATA: HLP_TABIX LIKE SY-TABIX.                           " Note 414234
  DATA: HLP_SUBRC LIKE SY-SUBRC.                           " Note 414234
  CLEAR DP_PAYMNT.

  LOOP AT PAYTAB.
* Note 414234:
* hlp_tabix will only be filled in case there is a partial payment
* Indicator: REBZG isn't empty and REBZT is equal to 'Z'
    CLEAR: HLP_TABIX, HLP_SUBRC.                           " Note 414234
    IF XKUNNR NE SPACE.
      CLEAR: XBSAD, XBSAK.
      REFRESH: XBSAD, XBSAK.
      SELECT * FROM BSAD INTO TABLE XBSAD WHERE BUKRS EQ BKPF-BUKRS
                         AND   KUNNR EQ XKUNNR
                         AND   AUGDT EQ PAYTAB-AUGDT
                         AND   AUGBL EQ BKPF-BELNR
                         AND   BELNR NE BKPF-BELNR.

      LOOP AT XBSAD.
        MOVE-CORRESPONDING XBSAD TO XBSAK.
        XBSAK-LIFNR = XBSAD-KUNNR.
        PERFORM CREATE_CLRDTAB.
      ENDLOOP.

      IF SY-SUBRC = 0.
        CLEAR XKUNNR.
      ENDIF.
    ENDIF.

    IF PAYTAB-UMSKS = 'A' AND
       PAYTAB-XZAHL NE SPACE.
      DP_PAYMNT = 'X'.
    ENDIF.

* DEVELOPER
    IF PAYTAB-BSCHL = '21' OR
       PAYTAB-BSCHL = '29' OR "Diego ?
       PAYTAB-BSCHL = '36'.   "Diego
      DP_PAYMNT = 'X'.
    ENDIF.

    AT NEW AUGDT.
      CLEAR: NO_CLRD_DOCM, XBSAK.
      REFRESH XBSAK.

      IF PAYTAB-UMSKS = 'A' AND
         DP_PAYMNT    NE SPACE.
        CLEAR DP_PAYMNT.
        PAYTAB-AUGDT = BKPF-BUDAT.
      ENDIF.

      SELECT * FROM BSAK INTO TABLE XBSAK WHERE BUKRS EQ BKPF-BUKRS
                                          AND   LIFNR EQ PAYTAB-LIFNR
                                          AND   UMSKS EQ PAYTAB-UMSKS
                                          AND   AUGDT EQ PAYTAB-AUGDT
                                          AND   AUGBL EQ BKPF-BELNR
                                          AND   BELNR NE BKPF-BELNR.

* Incluido - Diego ------------------------------------------------------
      IF XBSAK[] IS INITIAL.

        SELECT * FROM BSIK INTO TABLE XBSAK WHERE BUKRS EQ BKPF-BUKRS
                                            AND   LIFNR EQ PAYTAB-LIFNR
                                            AND   UMSKS EQ PAYTAB-UMSKS
                                            AND   BELNR EQ BKPF-BELNR.

      ENDIF.
*-----------------------------------------------------------------------

      LOOP AT XBSAK.
        PERFORM CREATE_CLRDTAB.
      ENDLOOP.

* If no clearing line: hlp_subrc will contain 4.           " Note 414234
* Only if event "at new augdt" was passed.                 " Note 414234
      HLP_SUBRC = SY-SUBRC.                                " Note 414234
      IF HLP_SUBRC NE 0.                                   " Note 414234
        NO_CLRD_DOCM = 'X'.
      ELSE.
* BEGIN: Note 414234
* Clearing line exists. Problem: If partial payment exists on the
* same day, it will not be recognized by the report => hence miss in the
* printed document.
        HLP_TABIX = SY-TABIX.  " Get index; check after "at new augdt"
        READ TABLE PAYTAB INDEX HLP_TABIX.
        IF ( PAYTAB-REBZT = 'Z' OR PAYTAB-REBZT = 'P' )
            AND ( NOT PAYTAB-REBZG IS INITIAL ).
          NO_CLRD_DOCM = 'X'.
        ENDIF.
* END: Note 414234
      ENDIF.
    ENDAT.

* BEGIN: Note 414234
* If "at new augdt" was never passed; or if flag no_clrd_docm was set
* normally, this condition will never be met.
* But: If first line of paytab contains reference to partial payment
* AND cleared lines exist for that AUGDT, no_clrd_docm is set above
* and needs to be cleared for following paytab lines with same AUGDT.
    IF ( HLP_TABIX EQ 0 ) AND ( HLP_SUBRC EQ 0 ).
* Note 427565: Only clear the flag no_clrd_docm if no partial
* payment anymore
* Indicator fpr part. paym.: REBZG isn't empty and REBZT is equal to 'Z'
      IF PAYTAB-REBZG IS INITIAL OR                      " Note 427565
         ( PAYTAB-REBZT NE 'Z' AND PAYTAB-REBZT NE 'P' ).
        CLEAR NO_CLRD_DOCM.
      ENDIF.                                             " Note 427565
    ENDIF.
* END: Note 414234

    CHECK: ( NO_CLRD_DOCM   NE SPACE AND
           PAYTAB-XZAHL NE SPACE ) OR ( PAYTAB-UMSKS = 'A' AND
           NOT PAYTAB-REBZG IS INITIAL )
* DEVELOPER
          OR DP_PAYMNT = 'X'.
*
    .

    IF NOT PAYTAB-REBZG IS INITIAL.
      PERFORM READ_DOCUMENT_HEADER
              USING PAYTAB-REBZG PAYTAB-REBZJ.

      MOVE-CORRESPONDING XBKPF TO CLRDTAB.
    ELSE.
      MOVE-CORRESPONDING BKPF TO CLRDTAB.
    ENDIF.

    MOVE-CORRESPONDING PAYTAB TO CLRDTAB.

    COLLECT CLRDTAB.
  ENDLOOP.
ENDFORM.                               " READ_CLRD_DOCUMENTS
*&---------------------------------------------------------------------*
*&      Form  PRINT_CERTIFICATE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM PRINT_CERTIFICATE.

  SORT CLRDTAB.
  CLEAR COPY_NO.

  DO S_COPY TIMES.
    PERFORM PRINT_CLRDTAB.
    ADD 1 TO COPY_NO.
  ENDDO.

ENDFORM.                               " PRINT_CERTIFICATE
*&---------------------------------------------------------------------*
*&      Form  READ_PRINTING_CHAR
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM READ_PRINTING_CHAR.
  CHECK XPRTCHR-J_1APRTCHR NE BKPF-XBLNR+4(1).

  READ TABLE XPRTCHR WITH KEY MANDT = SY-MANDT
                              J_1APRTCHR = BKPF-XBLNR+4(1).

  CHECK SY-SUBRC NE 0.

  SELECT SINGLE * FROM J_1APRTCHR WHERE J_1APRTCHR = BKPF-XBLNR+4(1).

  MOVE-CORRESPONDING J_1APRTCHR TO XPRTCHR.


  APPEND XPRTCHR.
ENDFORM.                               " READ_PRINTING_CHAR
*&---------------------------------------------------------------------*
*&      Form  CHECK_CURRENCIES
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM CHECK_CURRENCIES.

  DATA : L_KURSF LIKE BKPF-KURSF.     " Note 755666

  CLEAR : L_KURSF.         " Note 755666

  IF T001-WAERS EQ BKPF-WAERS.
* payment is in lc
    IF XBSAK-WAERS NE BKPF-WAERS.
* cleared document is created in fc
* the withhld.amnt. of the clrd. document is already converted
      CLRDTAB-QBSHB = CLRDTAB-QBSHH.

* Note 755666 Changes starts

*      perform convert_into_local_curr using
*         clrdtab-wrbtr clrdtab-wrbtr xbsak-waers bkpf-waers '0'.
*      perform convert_into_local_curr using
*         clrdtab-wmwst clrdtab-wmwst xbsak-waers bkpf-waers '0'.

      SELECT SINGLE KURSF INTO L_KURSF FROM BKPF
                           WHERE BUKRS = XBSAK-BUKRS
                           AND BELNR = XBSAK-BELNR
                           AND GJAHR = XBSAK-GJAHR.

      PERFORM CONVERT_INTO_LOCAL_CURR USING
         CLRDTAB-WRBTR CLRDTAB-WRBTR XBSAK-WAERS BKPF-WAERS L_KURSF.
      PERFORM CONVERT_INTO_LOCAL_CURR USING
         CLRDTAB-WMWST CLRDTAB-WMWST XBSAK-WAERS BKPF-WAERS L_KURSF.

* Note 755666 Changes ends.

    ENDIF.
  ELSEIF XBSAK-WAERS EQ BKPF-WAERS.
* payment is in fc and cleared document is in the same fc
    PERFORM CONVERT_INTO_LOCAL_CURR USING
       CLRDTAB-WRBTR CLRDTAB-DMBTR XBSAK-WAERS T001-WAERS BKPF-KURSF.
    PERFORM CONVERT_INTO_LOCAL_CURR USING
       CLRDTAB-WMWST CLRDTAB-MWSTS XBSAK-WAERS T001-WAERS BKPF-KURSF.
*    perform convert_into_local_curr using
*       clrdtab-qbshb clrdtab-qbshh bsak-waers t001-waers bkpf-kursf.
  ELSE.
    IF XBSAK-WAERS NE T001-WAERS.
* payment is in fc and cleared document is in other fc
* so convert at first into lc
* the withhld.amnt. of the clrd. document is already converted

* Note 755666 Changes starts
*      perform convert_into_local_curr using
*         clrdtab-wrbtr clrdtab-dmbtr xbsak-waers t001-waers '0'.
*      perform convert_into_local_curr using
*         clrdtab-wmwst clrdtab-mwsts xbsak-waers t001-waers '0'.

      SELECT SINGLE KURSF INTO L_KURSF FROM BKPF
                           WHERE BUKRS = XBSAK-BUKRS
                           AND BELNR = XBSAK-BELNR
                           AND GJAHR = XBSAK-GJAHR.

      PERFORM CONVERT_INTO_LOCAL_CURR USING
         CLRDTAB-WRBTR CLRDTAB-DMBTR XBSAK-WAERS T001-WAERS L_KURSF.
      PERFORM CONVERT_INTO_LOCAL_CURR USING
         CLRDTAB-WMWST CLRDTAB-MWSTS XBSAK-WAERS T001-WAERS L_KURSF.

* Note 755666 Changes ends.

* convert now into foreign curr.
      PERFORM CONVERT_INTO_FOREIGN_CURR USING
         CLRDTAB-DMBTR CLRDTAB-WRBTR T001-WAERS BKPF-WAERS BKPF-KURSF.
      PERFORM CONVERT_INTO_FOREIGN_CURR USING
         CLRDTAB-MWSTS CLRDTAB-WMWST T001-WAERS BKPF-WAERS BKPF-KURSF.
      PERFORM CONVERT_INTO_FOREIGN_CURR USING
         CLRDTAB-QBSHH CLRDTAB-QBSHB T001-WAERS BKPF-WAERS BKPF-KURSF.
    ELSE.
* payment is in fc and cleared document is in lc
      PERFORM CONVERT_INTO_FOREIGN_CURR USING
         CLRDTAB-WRBTR CLRDTAB-DMBTR T001-WAERS BKPF-WAERS BKPF-KURSF.

      XWRBTR        = CLRDTAB-WRBTR.
      CLRDTAB-WRBTR = CLRDTAB-DMBTR.
      CLRDTAB-DMBTR = XWRBTR.

      PERFORM CONVERT_INTO_FOREIGN_CURR USING
         CLRDTAB-WMWST CLRDTAB-MWSTS T001-WAERS BKPF-WAERS BKPF-KURSF.

      XWRBTR        = CLRDTAB-WMWST.
      CLRDTAB-WMWST = CLRDTAB-MWSTS.
      CLRDTAB-MWSTS = XWRBTR.

      PERFORM CONVERT_INTO_FOREIGN_CURR USING
         CLRDTAB-QBSHB CLRDTAB-QBSHH T001-WAERS BKPF-WAERS BKPF-KURSF.

      XWRBTR        = CLRDTAB-QBSHB.
      CLRDTAB-QBSHB = CLRDTAB-QBSHH.
      CLRDTAB-QBSHH = XWRBTR.
    ENDIF.
  ENDIF.
ENDFORM.                               " CHECK_CURRENCIES
*&---------------------------------------------------------------------*
*&      Form  CONVERT_INTO_LOCAL_CURR
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM CONVERT_INTO_LOCAL_CURR USING F_WRBTR F_DMBTR F_F_WAERS F_L_WAERS
                                   F_KURSF.

  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
    EXPORTING
      DATE             = BKPF-WWERT
      FOREIGN_AMOUNT   = F_WRBTR
      FOREIGN_CURRENCY = F_F_WAERS
      RATE             = F_KURSF
      LOCAL_CURRENCY   = F_L_WAERS
    IMPORTING
      LOCAL_AMOUNT     = F_DMBTR.
ENDFORM.                               " CONVERT_INTO_LOCAL_CURR
*&---------------------------------------------------------------------*
*&      Form  CONVERT_INTO_FOREIGN_CURR
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM CONVERT_INTO_FOREIGN_CURR USING F_DMBTR F_WRBTR F_L_WAERS F_F_WAERS
                                     F_KURSF.

  CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
    EXPORTING
      DATE             = BKPF-WWERT
      LOCAL_AMOUNT     = F_DMBTR
      LOCAL_CURRENCY   = F_L_WAERS
      RATE             = F_KURSF
      FOREIGN_CURRENCY = F_F_WAERS
    IMPORTING
      FOREIGN_AMOUNT   = F_WRBTR.
ENDFORM.                               " CONVERT_INTO_FOREIGN_CURR
*&---------------------------------------------------------------------*
*&      Form  CREATE_PAYTAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM CREATE_PAYTAB.

  CLEAR: PAYTAB, DP_PAYTAB.

  IF BSEG-XCPDD NE SPACE AND
     LFA1-XCPDK NE SPACE.
    PERFORM READ_CPD_DATA USING BKPF-BELNR BSEG-BUZEI BKPF-GJAHR.
    MOVE-CORRESPONDING XBSEC TO PAYTAB.
  ENDIF.

  MOVE-CORRESPONDING: BSEG TO PAYTAB.

  CLEAR: PAYTAB-QBSHB, PAYTAB-QBSHH.

  PERFORM READ_WITHHLD_DATA USING BSEG-BELNR BSEG-GJAHR BSEG-BUZEI
                                  PAYTAB-QBSHB PAYTAB-QBSHH.

  IF BSEG-SHKZG = 'H'.
    PAYTAB-DMBTR = PAYTAB-DMBTR * -1.
    PAYTAB-WRBTR = PAYTAB-WRBTR * -1.
    PAYTAB-MWSTS = PAYTAB-MWSTS * -1.
    PAYTAB-WMWST = PAYTAB-WMWST * -1.
  ENDIF.

  IF ( PAYTAB-WRBTR < 0 AND PAYTAB-QBSHB < 0 ) OR
     ( PAYTAB-WRBTR > 0 AND PAYTAB-QBSHB > 0 ).
    PAYTAB-QBSHB = PAYTAB-QBSHB * -1.
    PAYTAB-QBSHH = PAYTAB-QBSHH * -1.
  ENDIF.

  IF BSEG-UMSKS = 'A' AND BSEG-XZAHL NE SPACE.
*down payment
    MOVE-CORRESPONDING PAYTAB TO DP_PAYTAB.
    DP_PAYTAB-BUZEI = BSEG-BUZEI.
    COLLECT DP_PAYTAB.
  ENDIF.

* Partial payment lines cleared by other payment doc.
* must be treated like non-cleared items
  IF BSEG-AUGBL NE BSEG-BELNR.
    CLEAR PAYTAB-AUGDT.
  ENDIF.

  COLLECT PAYTAB.
ENDFORM.                               " CREATE_PAYTAB
*&---------------------------------------------------------------------*
*&      Form  READ_DOCUMENT_HEADER
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM READ_DOCUMENT_HEADER USING F_BELNR F_GJAHR.
  CHECK XBKPF-BUKRS NE BKPF-BUKRS OR
        XBKPF-BELNR NE F_BELNR    OR
        XBKPF-GJAHR NE F_GJAHR.

  CLEAR: XBKPF, *BKPF.

  READ TABLE XBKPF WITH KEY MANDT = SY-MANDT
                            BUKRS = BKPF-BUKRS
                            BELNR = F_BELNR
                            GJAHR = F_GJAHR.

  CHECK SY-SUBRC NE 0.

  SELECT SINGLE * FROM BKPF INTO *BKPF
                 WHERE BUKRS = BKPF-BUKRS
                 AND   BELNR = F_BELNR
                 AND   GJAHR = F_GJAHR.

  MOVE-CORRESPONDING *BKPF TO XBKPF.
  APPEND XBKPF.
ENDFORM.                               " READ_DOCUMENT_HEADER
*&---------------------------------------------------------------------*
*&      Form  CREATE_CLRDTAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM CREATE_CLRDTAB.

  IF XBSAK-UMSKS IS INITIAL.
    CLEAR: XBSAK-MWSTS, XBSAK-WMWST.
  ELSEIF BKPF-XBLNR IS INITIAL.
* no official document no. and printing char. in down payment document
    BKPF-XBLNR = XBSAK-XBLNR.
  ELSEIF BKPF-XBLNR(4)   IS INITIAL AND
         BKPF-XBLNR+5(8) IS INITIAL.
* no official document no. in down payment document
    BKPF-XBLNR(4)   = XBSAK-XBLNR(4).
    BKPF-XBLNR+5(8) = XBSAK-XBLNR+5(8).
  ENDIF.

  CLEAR: CLRDTAB.

  IF XBSAK-XCPDD NE SPACE AND
     LFA1-XCPDK  NE SPACE.
    PERFORM READ_CPD_DATA USING XBSAK-BELNR XBSAK-BUZEI XBSAK-GJAHR.
    CHECK XBSEC-EMPFG = PAYTAB-EMPFG.

    MOVE-CORRESPONDING XBSEC TO CLRDTAB.
  ENDIF.

  MOVE-CORRESPONDING: XBSAK TO CLRDTAB.

  PERFORM FILL_CLRDTAB.

  IF XBSAK-XZAHL NE SPACE.
    IF XBSAK-UMSKS = 'A'.
* cleared down payment
      CLEAR XBKPF.
      MOVE-CORRESPONDING XBSAK TO XBKPF.
      PERFORM READ_DOWNPAYMENT USING ' ' XBSAK-BELNR XBSAK-GJAHR
                                 XBSAK-LIFNR XBSAK-WRBTR XBSAK-WMWST.
      CHECK 1 = 2.
    ENDIF.
    PERFORM READ_DOCUMENT_POS USING XBSAK-BELNR XBSAK-GJAHR XBSAK-BUZEI.

    IF *BSEG-REBZT = 'U' AND XBSAK-BUZEI > 1.       " internal document
* Clearing line

      XBUZEI = XBSAK-BUZEI - 1.
      PERFORM READ_DOCUMENT_POS USING XBSAK-BELNR XBSAK-GJAHR XBUZEI.

      IF *BSEG-UMSKS = 'A' AND
         NOT *BSEG-REBZG IS INITIAL.
* downpayment clearing refering to downpayment
* read down payment posting, to get the amounts
        PERFORM READ_DOCUMENT_POS USING *BSEG-REBZG *BSEG-REBZJ
                                        *BSEG-REBZZ.
        PERFORM READ_DOWNPAYMENT USING 'X' *BSEG-BELNR *BSEG-GJAHR
                                 *BSEG-LIFNR *BSEG-WRBTR *BSEG-WMWST.
        CHECK 1 = 2.
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM READ_DOCUMENT_HEADER USING XBSAK-BELNR XBSAK-GJAHR.
  MOVE-CORRESPONDING XBKPF TO CLRDTAB.

  COLLECT CLRDTAB.

ENDFORM.                               " CREATE_CLRDTAB
*&---------------------------------------------------------------------*
*&      Form  READ_FOREIGN_ID
*&---------------------------------------------------------------------*
*      Retrive code for foreign persons
*----------------------------------------------------------------------*
FORM READ_FOREIGN_ID USING    XLAND1
                              XSTKZN
                     CHANGING XXSTCD1      .
  CLEAR: XFRID, J_1AFRID.
  READ TABLE XFRID WITH KEY MANDT = SY-MANDT
                            LAND1 = XLAND1
                            STKZN = XSTKZN.
* entry exists?
  IF SY-SUBRC NE 0.

    SELECT SINGLE * FROM J_1AFRID
                    WHERE LAND1 = XLAND1
                    AND   STKZN = XSTKZN.
    IF SY-SUBRC EQ 0.
      MOVE-CORRESPONDING J_1AFRID TO XFRID.
      APPEND XFRID.
    ENDIF.
  ENDIF.
  IF SY-SUBRC EQ 0.
    XXSTCD1 = XFRID-J_1AFPID.
  ENDIF.

ENDFORM.                               " READ_FOREIGN_ID
*&---------------------------------------------------------------------*
*&      Form  FILL_CLRDTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_CLRDTAB.
  IF XBSAK-UMSKS = 'A' AND XBSAK-BSTAT = 'S'.
*down payment request
    LOOP AT DP_PAYTAB WHERE EMPFG = PAYTAB-EMPFG
                      AND   LIFNR = PAYTAB-LIFNR
                      AND   DMBTR = XBSAK-DMBTR
                      AND   MWSTS = XBSAK-MWSTS.
* Begin note 524181
* If cleared document is in other currency than original document
* -> Do not overwrite amounts in document currency!! (filled from XBSAK)
      IF BKPF-WAERS NE XBSAK-WAERS.
        CLRDTAB-EMPFG = DP_PAYTAB-EMPFG.
        CLRDTAB-LIFNR = DP_PAYTAB-LIFNR.
        CLRDTAB-DMBTR = DP_PAYTAB-DMBTR.
        CLRDTAB-MWSTS = DP_PAYTAB-MWSTS.
        CLRDTAB-QBSHH = DP_PAYTAB-QBSHH.
        CLRDTAB-QBSHB = DP_PAYTAB-QBSHB.
      ELSE.
        MOVE-CORRESPONDING DP_PAYTAB TO CLRDTAB.
      ENDIF.
* End note 524181
      EXIT.
    ENDLOOP.
  ELSE.
    CLEAR: CLRDTAB-QBSHB, CLRDTAB-QBSHH.
  ENDIF.

  IF XBSAK-XZAHL IS INITIAL AND
     XBSAK-UMSKS NE 'A'.               " not for down payments
    PERFORM READ_WITHHLD_DATA USING XBSAK-BELNR XBSAK-GJAHR XBSAK-BUZEI
                                    CLRDTAB-QBSHB CLRDTAB-QBSHH.
  ENDIF.

  IF XBSAK-SHKZG = 'S'.
    CLRDTAB-DMBTR = CLRDTAB-DMBTR * -1.
    CLRDTAB-WRBTR = CLRDTAB-WRBTR * -1.
    CLRDTAB-MWSTS = CLRDTAB-MWSTS * -1.
    CLRDTAB-WMWST = CLRDTAB-WMWST * -1.
  ENDIF.

  IF ( CLRDTAB-WRBTR < 0 AND CLRDTAB-QBSHB < 0 ) OR
     ( CLRDTAB-WRBTR > 0 AND CLRDTAB-QBSHB > 0 ).
    CLRDTAB-QBSHB = CLRDTAB-QBSHB * -1.
    CLRDTAB-QBSHH = CLRDTAB-QBSHH * -1.
  ENDIF.

  PERFORM CHECK_CURRENCIES.

*  Note 802396 changes starts.
  IF NOT XBSAK-PYAMT IS INITIAL.
    CLRDTAB-WRBTR = XBSAK-PYAMT.
    IF T001-WAERS NE XBSAK-PYCUR.
      PERFORM CONVERT_INTO_LOCAL_CURR USING
       CLRDTAB-WRBTR CLRDTAB-DMBTR XBSAK-WAERS BKPF-WAERS BKPF-KURSF.
    ENDIF.
  ENDIF.
*  Note 802396 changes ends.
ENDFORM.                               " FILL_CLRDTAB
*&---------------------------------------------------------------------*
*&      Form  READ_DOCUMENT_POS
*&---------------------------------------------------------------------*
*       Read BSEG for document segment
*----------------------------------------------------------------------*
FORM READ_DOCUMENT_POS USING    P_BELNR
                                P_GJAHR
                                P_BUZEI.

  DATA ETL2591C2R6865 TYPE TABLE OF BSEG.
DATA RLDNR_L2591C2R1731 TYPE RLDNR.
CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
  IMPORTING E_RLDNR = RLDNR_L2591C2R1731
  EXCEPTIONS NOT_FOUND     = 1
             MORE_THAN_ONE = 2.
IF SY-SUBRC = 0.
CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
  EXPORTING
    I_RLDNR = RLDNR_L2591C2R1731
    I_BUKRS = BKPF-BUKRS
    I_BELNR = P_BELNR
    I_GJAHR = P_GJAHR
    I_BUZEI = P_BUZEI
  IMPORTING
    ET_BSEG = ETL2591C2R6865
  EXCEPTIONS NOT_FOUND = 1.
ENDIF.
IF SY-SUBRC = 0 AND LINES( ETL2591C2R6865 ) = 1.
  *BSEG = ETL2591C2R6865[ 1 ].
  SY-DBCNT = 1.
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


  IF SY-SUBRC NE 0.
    CLEAR *BSEG.
  ENDIF.
ENDFORM.                               " READ_DOCUMENT_POS
*&---------------------------------------------------------------------*
*&      Form  READ_DOWNPAYMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM READ_DOWNPAYMENT USING F_READHD F_BELNR F_GJAHR
                            F_LIFNR  F_WRBTR F_WMWST.

  CLEAR: YBSAK.
  REFRESH YBSAK.

  IF F_READHD NE SPACE.
    PERFORM READ_DOCUMENT_HEADER USING F_BELNR F_GJAHR.
  ENDIF.
  MOVE-CORRESPONDING XBKPF TO CLRDTAB.

  SELECT * FROM BSAK INTO TABLE YBSAK WHERE BUKRS EQ BKPF-BUKRS
                                      AND   LIFNR EQ F_LIFNR
                                      AND   UMSKS EQ 'A'
                                      AND   AUGDT EQ XBKPF-BUDAT
                                      AND   AUGBL EQ XBKPF-BELNR
                                      AND   BELNR NE XBKPF-BELNR
                                      AND   WRBTR EQ F_WRBTR
                                      AND   WMWST EQ F_WMWST.
  LOOP AT YBSAK.
    PERFORM READ_DOCUMENT_HEADER USING YBSAK-BELNR YBSAK-GJAHR.

    IF CLRDTAB-XBLNR IS INITIAL.
* no official document no. and printing char. in down payment document
      CLRDTAB-XBLNR = XBKPF-XBLNR.
    ELSEIF CLRDTAB-XBLNR(4)   IS INITIAL AND
           CLRDTAB-XBLNR+5(8) IS INITIAL.
* no official document no. in down payment document
      CLRDTAB-XBLNR(4)   = XBKPF-XBLNR(4).
      CLRDTAB-XBLNR+5(8) = XBKPF-XBLNR+5(8).
    ENDIF.

    CLRDTAB-BLART = XBKPF-BLART.
    CLRDTAB-BELNR = XBKPF-BELNR.
    CLRDTAB-BUDAT = XBKPF-BUDAT.

    READ TABLE CLRDTAB WITH KEY BELNR = CLRDTAB-BELNR
                                BUZEI = CLRDTAB-BUZEI
                   TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0. " el documento ya se guardo con otra posicion
      " no guardar nuevamente.
    ELSE.
      COLLECT CLRDTAB.
    ENDIF.

    EXIT.
  ENDLOOP.

  IF SY-SUBRC NE 0.

    READ TABLE CLRDTAB WITH KEY BELNR = CLRDTAB-BELNR
                                BUZEI = CLRDTAB-BUZEI
                   TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0. " el documento ya se guardo con otra posicion
      " no guardar nuevamente.
    ELSE.
      COLLECT CLRDTAB.
    ENDIF.

  ENDIF.
ENDFORM.                               " READ_DOWNPAYMENT
*&---------------------------------------------------------------------*
*&      Form  READ_WITHHLD_CODE
*&---------------------------------------------------------------------*
*       Whole form inserted with Note 457832
*----------------------------------------------------------------------*
FORM READ_WITHHLD_CODE.                                    " Note 457832
  CLEAR XT059Z.

  READ TABLE XT059Z WITH KEY LAND1      = T001-LAND1
                             WITHT      = WITH_ITEM-WITHT
                             WT_WITHCD = WITH_ITEM-WT_WITHCD.

  CHECK SY-SUBRC NE 0.

  SELECT SINGLE * FROM T059Z WHERE LAND1    = T001-LAND1
                             AND   WITHT    = WITH_ITEM-WITHT
                             AND WT_WITHCD  = WITH_ITEM-WT_WITHCD.

  MOVE-CORRESPONDING T059Z TO XT059Z.

  APPEND XT059Z.
ENDFORM.                    " READ_WITHHLD_CODE              Note 457832



*&---------------------------------------------------------------------*
*&      Form  F_LOGO
*&---------------------------------------------------------------------*
FORM F_LOGO .

*  SELECCIONA LOGO
*  SELECT SINGLE name1
*  FROM zfi_t_logo
*  INTO st_emisor-form
*  WHERE bukrs EQ st_emisor-bukrs .

ENDFORM.                    " F_LOGO
*&---------------------------------------------------------------------*
*&      Form  F_GET_BKPF
*&---------------------------------------------------------------------*
FORM F_GET_BKPF .

  CHECK SELECT-OPTIONS.
  CHECK BKPF-STBLG IS INITIAL.

*  IF PDODIA IS NOT INITIAL.
*    CHECK BKPF-BUDAT EQ SY-DATUM.
*  ENDIF.

  CLEAR: XXCPDD, XUMSKS, XKUNNR, DOCM_SEL.

ENDFORM.                    " F_GET_BKPF
*&---------------------------------------------------------------------*
*&      Form  F_GET_BSEG
*&---------------------------------------------------------------------*
FORM F_GET_BSEG .
  APPEND BSEG TO T_BSEG_AUX.
  CHECK BSEG-KOART = 'K' OR
        BSEG-KOART = 'D'.

  IF BSEG-KUNNR NE SPACE.
    XKUNNR = BSEG-KUNNR.
  ENDIF.

  CHECK BSEG-KOART = 'K' AND BSEG-REBZT NE 'U'.

  IF BSEG-XCPDD NE SPACE AND
     XXCPDD     EQ SPACE.
    CALL FUNCTION 'VENDOR_READ'
      EXPORTING
        I_BUKRS = SPACE
        I_LIFNR = BSEG-LIFNR
      IMPORTING
        E_LFA1  = LFA1.

    IF LFA1-XCPDK NE SPACE.
      XXCPDD = 'X'.
    ENDIF.
  ENDIF.

  IF XUMSKS IS INITIAL.
    XUMSKS = BSEG-UMSKS.
  ENDIF.


  MOVE XUMSKS TO GV_TRANSACTION_TYPE.


*  IF bseg-xzahl NE space.
  DOCM_SEL = 'X'.
*  ENDIF.

  PERFORM CREATE_PAYTAB.

ENDFORM.                    " F_GET_BSEG
*&---------------------------------------------------------------------*
*&      Form  F_BKPF_LATE
*&---------------------------------------------------------------------*
FORM F_BKPF_LATE .
  CHECK SELECT-OPTIONS.
  CHECK BKPF-STBLG IS INITIAL.

*  CHECK docm_sel NE space.
  ON CHANGE OF BKPF-BUKRS.
    PERFORM READ_COMPANY_DATA.
  ENDON.

  PERFORM READ_CLRD_DOCUMENTS.

  ON CHANGE OF BKPF-XBLNR+4(1).
    PERFORM READ_PRINTING_CHAR.
  ENDON.

  PERFORM PRINT_CERTIFICATE.

  REFRESH: PAYTAB, DP_PAYTAB, CLRDTAB, XBSEC.
ENDFORM.                    " F_BKPF_LATE
*&---------------------------------------------------------------------*
*&      Form  F_BUSCO_XBLNRO_BLART_BLDAT
*&---------------------------------------------------------------------*
FORM F_BUSCO_XBLNRO_BLART_BLDAT  USING    GSL_DOCDATA TYPE TY_DOCDATA
                                 "  gsl_docdata-buzei TYPE bseg-buzei
                                 CHANGING PO_XBLNR
                                          PO_BLART
                                          PO_BLDAT.
  SELECT SINGLE XBLNR BLART BLDAT
        FROM BKPF
        INTO (PO_XBLNR, PO_BLART, PO_BLDAT)
        WHERE BUKRS EQ GSL_DOCDATA-BUKRS
          AND BELNR EQ GSL_DOCDATA-BELNR.
*          AND gjahr EQ gsl_docdata-gjahr.

ENDFORM.                    " F_BUSCO_XBLNRO_BLART_BLDAT
*&---------------------------------------------------------------------*
*&      Form  F_SABER_SI_ES_ANTICIPO
*----------------------------------------------------------------------*
FORM F_SABER_SI_ES_ANTICIPO USING    GSL_DOCDATA TYPE TY_DOCDATA
                            CHANGING PO_XBLNR.

  DATA: LS_BSEG TYPE BSEG.
  READ TABLE T_BSEG_AUX INTO LS_BSEG
  WITH KEY BUKRS = GSL_DOCDATA-BUKRS
           BELNR = GSL_DOCDATA-BELNR
           BSCHL = '21'                                     "'29'
           GJAHR = GSL_DOCDATA-GJAHR.

  IF LS_BSEG-UMSKZ NE ' '.
    PO_XBLNR = 'ANTICIPO'.
  ENDIF.

ENDFORM.                    " F_SABER_SI_ES_ANTICIPO
*&---------------------------------------------------------------------*
*&      Form  F_VALIDACION_MEDIO_PAGO
*&---------------------------------------------------------------------*
FORM F_VALIDACION_MEDIO_PAGO  USING    LS_PAGOS TYPE ZFIYS_ORDEN_PAGOS_MPG
                              CHANGING PO_SUBRC.

  READ TABLE T_MPAGOS INTO LS_PAGOS
  WITH KEY UBHKT = LS_MPAGOS-UBHKT
           DMBTR = LS_MPAGOS-DMBTR.
  MOVE SY-SUBRC TO PO_SUBRC.
ENDFORM.                    " F_VALIDACION_MEDIO_PAGO

*&---------------------------------------------------------------------*
*&      Form  F_BORRAR_DUPLICADOS_T_PAGO_POS
*&---------------------------------------------------------------------*

FORM F_BORRAR_DUPLICADOS_T_PAGO_POS .

  DATA: E_PAGO_POS  LIKE LINE OF T_PAGO_POS,
        E_PAGO_POS2 LIKE LINE OF T_PAGO_POS.

  LOOP AT T_PAGO_POS INTO E_PAGO_POS.

    "belnr

    LOOP AT T_PAGO_POS INTO E_PAGO_POS2 WHERE BELNR EQ E_PAGO_POS-BELNR.

      IF E_PAGO_POS2 <> E_PAGO_POS.

        DELETE T_PAGO_POS INDEX SY-TABIX.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " F_BORRAR_DUPLICADOS_T_PAGO_POS

*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILENAME  text
*      -->P_ST_EMISOR  text
*----------------------------------------------------------------------*
FORM ENVIAR_EMAIL  USING    P_FILENAME      TYPE STRING
                            P_ST_EMISOR     TYPE ZFIYS_ORDEN_PAGOS_CAB
                            LS_PDF_STRING_X TYPE XSTRING.

  DATA: I_HTML_ENTRA      TYPE STRING,
        LO_CREATE_MAIL    TYPE REF TO CL_CRM_EMAIL_DATA,
        LS_MAIL_BODY      TYPE CRMS_EMAIL_MIME_STRUC,
        LS_RECEP          TYPE CRMS_EMAIL_RECIPIENT,
        LV_ACTIVITY       TYPE SYSUUID_X,
        MAIL_DESTINATARIO TYPE STRING,
        WA_LIFNR          TYPE LFA1,
        WA_ADR6           TYPE ADR6.

  SELECT SINGLE * INTO WA_LIFNR FROM LFA1 WHERE LIFNR EQ P_ST_EMISOR-LIFNR.

  CONCATENATE I_HTML_ENTRA '<html>'  INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<head>'  INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '</head>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<body>'  INTO I_HTML_ENTRA.

  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma>Estimado:' WA_LIFNR-NAME1 '</FONT></DIV>' INTO I_HTML_ENTRA SEPARATED BY SPACE.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma>Tenemos el agrado de enviar  las novedades ocurridas  en su cuenta corriente.</FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma>Este es un e-mail automático, por informaciones adicional por favor contactar:' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<a href="mailto:cuentas@amaggi.com.ar">cuentas@amaggi.com.ar</a></FONT></DIV>' INTO I_HTML_ENTRA SEPARATED BY SPACE.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma>CORDIALES SALUDOS</FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma color:#1F497D>AMAGGI ARGENTINA SA</FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma><a href="http://www.amaggi.com.br">www.amaggi.com.br</a></FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma><a href="http://www.amaggi.com.ar">www.amaggi.com.ar</a></FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma color:#1F497D>Antes de imprimir, piense en su responsabilidad con el MEDIO AMBIENTE !!!!!</FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma color:#1F497D>Before printing, think about your responsibility for the ENVIRONMENT!!!</FONT></DIV>' INTO I_HTML_ENTRA.

  CONCATENATE I_HTML_ENTRA '</body>'     INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '</html>'     INTO I_HTML_ENTRA.

  MAIL_DESTINATARIO = PEMAIL.

  IF PEMAIL IS INITIAL.
    IF SY-SYSID EQ 'PRD'.
      IF WA_LIFNR-ADRNR IS NOT INITIAL.
        SELECT SINGLE * INTO WA_ADR6
          FROM ADR6
         WHERE ADDRNUMBER EQ WA_LIFNR-ADRNR.
        IF SY-SUBRC IS INITIAL AND WA_ADR6-SMTP_ADDR IS NOT INITIAL.
          MAIL_DESTINATARIO = WA_ADR6-SMTP_ADDR.
        ENDIF.
      ENDIF.
    ELSE.

      DATA: IT_BAPIRET2   TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
            IT_BAPIADSMTP TYPE TABLE OF BAPIADSMTP WITH HEADER LINE.

      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          USERNAME = SY-UNAME
        TABLES
          RETURN   = IT_BAPIRET2
          ADDSMTP  = IT_BAPIADSMTP.

      READ TABLE IT_BAPIADSMTP INDEX 1.
      IF SY-SUBRC IS INITIAL AND IT_BAPIADSMTP-E_MAIL IS NOT INITIAL.
        MAIL_DESTINATARIO = IT_BAPIADSMTP-E_MAIL.
      ENDIF.

    ENDIF.
  ENDIF.

  SY-SUBRC = 1.
  CHECK MAIL_DESTINATARIO IS NOT INITIAL.

  CREATE OBJECT LO_CREATE_MAIL.

  CLEAR: LO_CREATE_MAIL->SUBJECT.
  LO_CREATE_MAIL->SUBJECT = 'Orden de Pago'.

  CLEAR LS_MAIL_BODY.
  LS_MAIL_BODY-CONTENT_ASCII = I_HTML_ENTRA.
  LS_MAIL_BODY-MIME_TYPE     = 'text/html'.
  APPEND  LS_MAIL_BODY TO LO_CREATE_MAIL->BODY.

  CLEAR LS_MAIL_BODY.
  LS_MAIL_BODY-IS_ATTACHMENT = 'X'.
  LS_MAIL_BODY-FILE_NAME     = P_FILENAME.
  LS_MAIL_BODY-MIME_TYPE     = 'application/pdf'.
  LS_MAIL_BODY-CONTENT_BIN   = LS_PDF_STRING_X.
  APPEND  LS_MAIL_BODY TO LO_CREATE_MAIL->BODY.

  CLEAR LS_RECEP.
  LS_RECEP-ADDRESS = MAIL_DESTINATARIO.
  APPEND LS_RECEP TO LO_CREATE_MAIL->TO.

  CLEAR LS_RECEP.
  LS_RECEP-NAME    = 'AMAGGI'.
  "LS_RECEP-ADDRESS = 'suporte.desenv@amaggi.com.br'.
  LS_RECEP-ADDRESS = 'cuentas@amaggi.com.ar'.
  MOVE LS_RECEP TO LO_CREATE_MAIL->FROM.

  "CALL METHOD CL_CRM_EMAIL_UTILITY_BASE=>CREATE_REPLY.

  CALL METHOD CL_CRM_EMAIL_UTILITY_BASE=>SEND_EMAIL
    EXPORTING
      IV_MAIL_DATA       = LO_CREATE_MAIL
    RECEIVING
      EV_SEND_REQUEST_ID = LV_ACTIVITY.

  COMMIT WORK.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_EMISOR  text
*----------------------------------------------------------------------*
FORM ENVIAR_EMAIL_DOC  USING P_DOC TYPE ZFIYS_ORDEN_PAGOS_CAB.

*  DATA: BEGIN OF ITAB OCCURS 0,
*          TABLINE(5000),
*        END OF ITAB.

  DATA: I_HTML_ENTRA      TYPE STRING,
        LO_CREATE_MAIL    TYPE REF TO CL_CRM_EMAIL_DATA,
        LS_MAIL_BODY      TYPE CRMS_EMAIL_MIME_STRUC,
        LS_RECEP          TYPE CRMS_EMAIL_RECIPIENT,
        LV_ACTIVITY       TYPE SYSUUID_X,
        MAIL_DESTINATARIO TYPE STRING,
        WA_LIFNR          TYPE LFA1,
        WA_ADR6           TYPE ADR6,
        ARQUIVO           TYPE STRING,
        "I_FILETYPE        TYPE CHAR10,
        LS_PDF_STRING_X   TYPE XSTRING,
        E_BINARY_LENGTH   TYPE I.

  DATA: WA_ZLEST0007 TYPE ZLEST0007,
        WA_ZSDYT0052 TYPE ZSDYT0052,
        WA_ZSDYT0055 TYPE ZSDYT0055,
        IT_ZSDYT0053 TYPE TABLE OF ZSDYT0053 WITH HEADER LINE.

  CLEAR: WA_ZLEST0007,
         WA_ZSDYT0052,
         IT_ZSDYT0053,
         IT_ZSDYT0053[].

  CONCATENATE P_DOC-BUKRS P_DOC-AUGBL P_DOC-GJAHR INTO DATA(NM_INSTANCE).
  TRY.
      DATA(HANDLE) = ZCL_MEMORY_VARIAVEIS_AREA=>ATTACH_FOR_READ( INST_NAME = CONV #( NM_INSTANCE ) ).
      DATA(CK_INSTANCE) = ABAP_TRUE.
      HANDLE->DETACH( ).
    CATCH CX_SHM_ATTACH_ERROR.
      CK_INSTANCE = ABAP_FALSE.
  ENDTRY.

  CHECK CK_INSTANCE EQ ABAP_FALSE.

  CHECK ( SY-BATCH IS NOT INITIAL OR SY-TCODE = 'ZFIY0036' ).

  SELECT SINGLE * INTO WA_ZLEST0007
    FROM ZLEST0007
   WHERE ID_INTERFACE = '33'
     AND ID_CTG       = 'PDF'
     AND PREFIX       = 'OP'.

  CHECK SY-SUBRC IS INITIAL.

  SELECT SINGLE * INTO WA_ZSDYT0052
    FROM ZSDYT0052
   WHERE BUKRS    EQ P_DOC-BUKRS
     AND GJAHR    EQ P_DOC-GJAHR
     AND AUGBL    EQ P_DOC-AUGBL
     AND CK_PRINT EQ 'X'.

  CHECK SY-SUBRC IS INITIAL.

  SELECT * INTO TABLE IT_ZSDYT0053
    FROM ZSDYT0053
   WHERE BUKRS    EQ P_DOC-BUKRS
     AND GJAHR_O  EQ P_DOC-GJAHR
     AND BELNR_O  EQ P_DOC-AUGBL
     AND CK_PRINT EQ 'X'.

  SELECT SINGLE * INTO WA_LIFNR FROM LFA1 WHERE LIFNR EQ P_DOC-LIFNR.

  CONCATENATE I_HTML_ENTRA '<html>'  INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<head>'  INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '</head>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<body>'  INTO I_HTML_ENTRA.

  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma>Estimado cliente:' WA_LIFNR-NAME1 '</FONT></DIV>' INTO I_HTML_ENTRA SEPARATED BY SPACE.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma>Enviamos informaciones sobre su Orden de Pago.</FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma>Este es un e-mail automático, por otras informaciones favor contactar:' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<a href="mailto:cuentas@amaggi.com.ar">cuentas@amaggi.com.ar</a></FONT></DIV>' INTO I_HTML_ENTRA SEPARATED BY SPACE.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma>Atentamente</FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma color:#1F497D>Amaggi Argentina SA</FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma><a href="http://www.amaggi.com.br">www.amaggi.com.br</a></FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma><a href="http://www.amaggi.com.ar">www.amaggi.com.ar</a></FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma color:#1F497D>Antes de imprimir, piense en su responsabilidad con el MEDIO AMBIENTE!</FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma color:#1F497D>Antes de imprimir, pense em sua responsabilidade e compromisso com o MEIO AMBIENTE! </FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma color:#1F497D>Before printing, think about your responsibility for the ENVIRONMENT!!!</FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '</body>'     INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '</html>'     INTO I_HTML_ENTRA.

  MAIL_DESTINATARIO = PEMAIL.

  IF PEMAIL IS INITIAL.
    IF SY-SYSID EQ 'PRD'.
      IF WA_LIFNR-ADRNR IS NOT INITIAL.
        SELECT SINGLE * INTO WA_ADR6
          FROM ADR6
         WHERE ADDRNUMBER EQ WA_LIFNR-ADRNR.
        IF SY-SUBRC IS INITIAL AND WA_ADR6-SMTP_ADDR IS NOT INITIAL.
          MAIL_DESTINATARIO = WA_ADR6-SMTP_ADDR.
        ENDIF.
      ENDIF.
    ELSE.

      DATA: IT_BAPIRET2   TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
            IT_BAPIADSMTP TYPE TABLE OF BAPIADSMTP WITH HEADER LINE.

      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          USERNAME = SY-UNAME
        TABLES
          RETURN   = IT_BAPIRET2
          ADDSMTP  = IT_BAPIADSMTP.

      READ TABLE IT_BAPIADSMTP INDEX 1.
      IF SY-SUBRC IS INITIAL AND IT_BAPIADSMTP-E_MAIL IS NOT INITIAL.
        MAIL_DESTINATARIO = IT_BAPIADSMTP-E_MAIL.
      ENDIF.
    ENDIF.
  ENDIF.

  SY-SUBRC = 1.
  CHECK MAIL_DESTINATARIO IS NOT INITIAL.

  CREATE OBJECT LO_CREATE_MAIL.

  CLEAR: LO_CREATE_MAIL->SUBJECT.
  LO_CREATE_MAIL->SUBJECT = 'Orden de Pago'.

  CLEAR LS_MAIL_BODY.
  LS_MAIL_BODY-CONTENT_ASCII = I_HTML_ENTRA.
  LS_MAIL_BODY-MIME_TYPE     = 'text/html'.
  APPEND  LS_MAIL_BODY TO LO_CREATE_MAIL->BODY.

  CONCATENATE WA_ZLEST0007-PATHUNIX WA_ZSDYT0052-NM_ARQUIVO INTO ARQUIVO.

  CLEAR LS_MAIL_BODY.
  LS_MAIL_BODY-IS_ATTACHMENT = 'X'.
  LS_MAIL_BODY-FILE_NAME     = WA_ZSDYT0052-NM_ARQUIVO.
  LS_MAIL_BODY-MIME_TYPE     = 'application/pdf'.

  OPEN DATASET ARQUIVO FOR INPUT IN BINARY MODE.
  READ DATASET ARQUIVO INTO LS_PDF_STRING_X  .
  CLOSE DATASET ARQUIVO.
  LS_MAIL_BODY-CONTENT_BIN   = LS_PDF_STRING_X.
  APPEND LS_MAIL_BODY TO LO_CREATE_MAIL->BODY.

  LOOP AT IT_ZSDYT0053.
*    CLEAR: ITAB.
    CONCATENATE WA_ZLEST0007-PATHUNIX IT_ZSDYT0053-NM_ARQUIVO INTO ARQUIVO.

    CLEAR LS_MAIL_BODY.
    LS_MAIL_BODY-IS_ATTACHMENT = 'X'.
    LS_MAIL_BODY-FILE_NAME     = IT_ZSDYT0053-NM_ARQUIVO.
    LS_MAIL_BODY-MIME_TYPE     = 'application/pdf'.
    OPEN DATASET ARQUIVO FOR INPUT IN BINARY MODE.
    READ DATASET ARQUIVO INTO LS_PDF_STRING_X.
    CLOSE DATASET ARQUIVO.
    LS_MAIL_BODY-CONTENT_BIN   = LS_PDF_STRING_X.
    APPEND LS_MAIL_BODY TO LO_CREATE_MAIL->BODY.
  ENDLOOP.

  CLEAR LS_RECEP.
  LS_RECEP-ADDRESS = MAIL_DESTINATARIO.
  APPEND LS_RECEP TO LO_CREATE_MAIL->TO.

  CLEAR LS_RECEP.
  LS_RECEP-NAME    = 'AMAGGI'.
  "LS_RECEP-ADDRESS = 'suporte.desenv@amaggi.com.br'.
  LS_RECEP-ADDRESS = 'cuentas@amaggi.com.ar'.
  MOVE LS_RECEP TO LO_CREATE_MAIL->FROM.

  "CALL METHOD CL_CRM_EMAIL_UTILITY_BASE=>CREATE_REPLY.
  IF PTESTEE EQ ABAP_FALSE.
    CALL METHOD CL_CRM_EMAIL_UTILITY_BASE=>SEND_EMAIL
      EXPORTING
        IV_MAIL_DATA       = LO_CREATE_MAIL
      RECEIVING
        EV_SEND_REQUEST_ID = LV_ACTIVITY.
  ELSE.
    SY-SUBRC = 0.
  ENDIF.

  IF SY-SUBRC IS INITIAL.
    WA_ZSDYT0052-CK_MAIL = ABAP_TRUE.
    MODIFY ZSDYT0052 FROM WA_ZSDYT0052.

    MOVE-CORRESPONDING WA_ZSDYT0052 TO WA_ZSDYT0055.
    WA_ZSDYT0055-E_MAIL    = MAIL_DESTINATARIO.
    WA_ZSDYT0055-DT_ENVIO  = SY-DATUM.
    WA_ZSDYT0055-HR_ENVIO  = SY-UZEIT.
    WA_ZSDYT0055-US_ENVIO  = SY-UNAME.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR = '01'
        OBJECT      = 'ZIDOPAGL'
      IMPORTING
        NUMBER      = WA_ZSDYT0055-ID_SEQUENCIA.
    MODIFY ZSDYT0055 FROM WA_ZSDYT0055.

    LOOP AT IT_ZSDYT0053.
      IT_ZSDYT0053-CK_MAIL = ABAP_TRUE.
      MODIFY ZSDYT0053 FROM IT_ZSDYT0053.
    ENDLOOP.
    COMMIT WORK.
  ENDIF.

ENDFORM.
