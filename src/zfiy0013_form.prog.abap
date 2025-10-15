*&---------------------------------------------------------------------*
*&  Include           ZFIY0013_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  buscar_datos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM buscar_datos.

  DATA: w_alicu TYPE bset-kbetr.

  SELECT bukrs gjahr belnr bldat xblnr blart knumv
    FROM bkpf INTO TABLE i_bkpf
      WHERE bukrs IN s_bukrs  AND
            bldat IN s_bldat  AND
            blart IN ('DR', 'DY', 'DG', 'D1', 'D2', 'D3').

  CHECK sy-subrc = 0.

  DATA ETL24C2R7948 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L24C2R8898 TYPE FAGL_T_FIELD.
LT_FIELDS_L24C2R8898 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'GJAHR' )
 ( LINE = 'BELNR' )
 ( LINE = 'KUNNR' )
 ).

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = I_BKPF[]
              I_WHERE_CLAUSE = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND KUNNR NE SPACE|
              IT_FIELDLIST = LT_FIELDS_L24C2R8898
    IMPORTING ET_BSEG = ETL24C2R7948
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL24C2R7948 ) > 0.
  MOVE-CORRESPONDING ETL24C2R7948 TO I_BSEG[].
  SY-DBCNT = LINES( ETL24C2R7948 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


  SELECT bukrs gjahr belnr kschl fwbas kbetr fwste hwbas hwste
    FROM bset INTO TABLE i_bset
      FOR ALL ENTRIES IN i_bkpf
      WHERE bukrs = i_bkpf-bukrs AND
            gjahr = i_bkpf-gjahr AND
            belnr = i_bkpf-belnr AND
            kschl IN s_kschl.

  SELECT bukrs gjahr belnr kschl fwbas kbetr fwste hwste
    FROM bset INTO TABLE i_bset2
      FOR ALL ENTRIES IN i_bkpf
      WHERE bukrs = i_bkpf-bukrs AND
            gjahr = i_bkpf-gjahr AND
            belnr = i_bkpf-belnr AND
            ktosl = 'MWS'.

  SELECT kunnr stcd1 fityp stcd2 stcdt
    FROM kna1 INTO TABLE i_kna1
      FOR ALL ENTRIES IN i_bseg
        WHERE kunnr = i_bseg-kunnr.

  SORT i_kna1 BY kunnr.
  SORT i_konv BY knumv kschl.

  LOOP AT i_bkpf.
    CLEAR i_archivo-tipop.               "tipo operacion
    CLEAR i_archivo-fecre.              "Fecha de retencion
    CLEAR i_archivo-incpret.             "Inc por el que retiene
    CLEAR i_archivo-tipco.               "Tipo de comprobante
    CLEAR i_archivo-letco.               "Letra de comprobante
    CLEAR i_archivo-numco.              "Numero de comprobante
    CLEAR i_archivo-fecco.              "Fecha del comprobante
    CLEAR i_archivo-monco.              "Monto del comprobante
    CLEAR i_archivo-tipdoc.              "tipo de documento
    CLEAR i_archivo-numdoc.             "numero de documento
    CLEAR i_archivo-condib.              "cond frente a IIBB
    CLEAR i_archivo-nroins.             "numero inscripcion en IIBB
    CLEAR i_archivo-sitiva.              "Situacion frente a IVA
    CLEAR i_archivo-marog.               "marca inscripcion O grav
    CLEAR i_archivo-mardr.               "marca inscripcion D rel
    CLEAR i_archivo-impgr.              "Importe otros gravemenes
    CLEAR i_archivo-impiva.             "Importe IVA
    CLEAR i_archivo-basimp.             "Base imponible para el calculo
    CLEAR i_archivo-alicu.               "Alicuota
    CLEAR i_archivo-impdet.             "Impuesto determinado
    CLEAR i_archivo-derinsp.            "Derecho registro e Inspec
    CLEAR i_archivo-montret.            "Monto Retenido
    CLEAR i_archivo-artcal.              "Art Inc para el Calculo


    i_archivo-tipop = '2'.

    i_archivo-fecre(2)   = i_bkpf-bldat+6(2).
    i_archivo-fecre+2(1) = '/'.
    i_archivo-fecre+3(2) = i_bkpf-bldat+4(2).
    i_archivo-fecre+5(1) = '/'.
    i_archivo-fecre+6(4) = i_bkpf-bldat(4).

    i_archivo-incpret = '029'.

    CASE i_bkpf-blart.
      WHEN 'DR' OR 'D1'.
        i_archivo-tipco = '01'.
      WHEN 'DY' OR 'D3'.
        i_archivo-tipco = '02'.
      WHEN 'DG' OR 'D2'.
        i_archivo-tipco = '09'.
    ENDCASE.

    i_archivo-letco = i_bkpf-xblnr+4(1).

    i_archivo-numco(4)   = i_bkpf-xblnr(4).
    i_archivo-numco+4(8) = i_bkpf-xblnr+5(8).


    i_archivo-fecco(2)   = i_bkpf-bldat+6(2).
    i_archivo-fecco+2(1) = '/'.
    i_archivo-fecco+3(2) = i_bkpf-bldat+4(2).
    i_archivo-fecco+5(1) = '/'.
    i_archivo-fecco+6(4) = i_bkpf-bldat(4).



    CLEAR i_bset.
    READ TABLE i_bset WITH KEY bukrs = i_bkpf-bukrs
                               gjahr = i_bkpf-gjahr
                               belnr = i_bkpf-belnr.

    IF sy-subrc IS INITIAL.

      WRITE i_bset-fwbas TO i_archivo-monco RIGHT-JUSTIFIED.
      REPLACE ',' WITH space INTO i_archivo-monco.
      REPLACE '.' WITH space INTO i_archivo-monco.
      CONDENSE i_archivo-monco NO-GAPS.
      WRITE i_archivo-monco TO i_archivo-monco RIGHT-JUSTIFIED.
      SHIFT i_archivo-monco LEFT.
      WRITE i_archivo-monco TO i_archivo-monco
        USING EDIT MASK '_________,__'.
      IF i_bkpf-blart EQ 'DG' OR i_bkpf-blart EQ 'D2'.
        OVERLAY i_archivo-monco WITH '-00000000000'.
      ELSE.
        OVERLAY i_archivo-monco WITH '000000000000'.
      ENDIF.

      CLEAR i_bseg.
      READ TABLE i_bseg WITH KEY bukrs = i_bkpf-bukrs
                                 gjahr = i_bkpf-gjahr
                                 belnr = i_bkpf-belnr.

      IF sy-subrc EQ 0.
        READ TABLE i_kna1 WITH KEY kunnr = i_bseg-kunnr.
        IF sy-subrc EQ 0.

          IF i_kna1-stcdt EQ '80'.
            i_archivo-tipdoc = '3'.
          ELSEIF i_kna1-stcdt EQ '86'.
            i_archivo-tipdoc = '2'.
          ELSE.
            i_archivo-tipdoc = '1'.
          ENDIF.

          i_archivo-numdoc = i_kna1-stcd1.

          IF i_kna1-stcd2 IS INITIAL.
            i_archivo-condib = '3'.
            OVERLAY i_archivo-nroins WITH '0000000000'.
          ELSE.
            i_archivo-condib = '1'.
            WRITE i_kna1-stcd2 TO i_archivo-nroins RIGHT-JUSTIFIED.
          ENDIF.

          CASE i_kna1-fityp.
            WHEN '01'.
              i_archivo-sitiva = '1'.
            WHEN '02'.
              i_archivo-sitiva = '2'.
            WHEN '04'.
              i_archivo-sitiva = '3'.
            WHEN '06'.
              i_archivo-sitiva = '4'.
          ENDCASE.


          i_archivo-marog = '0'.
          i_archivo-mardr = '0'.
          OVERLAY i_archivo-impgr WITH '000000000000'.
          WRITE i_archivo-impgr TO i_archivo-impgr
            USING EDIT MASK '_________,__'.
        ENDIF.
      ENDIF.

      CLEAR i_bset2.
      READ TABLE i_bset2 WITH KEY bukrs = i_bkpf-bukrs
                                  gjahr = i_bkpf-gjahr
                                  belnr = i_bkpf-belnr.

      i_archivo-impiva = i_bset2-hwste.
      REPLACE ',' WITH space INTO i_archivo-impiva.
      REPLACE '.' WITH space INTO i_archivo-impiva.
      CONDENSE i_archivo-impiva NO-GAPS.
      WRITE i_archivo-impiva TO i_archivo-impiva RIGHT-JUSTIFIED.
      SHIFT i_archivo-impiva LEFT.
      WRITE i_archivo-impiva TO i_archivo-impiva
        USING EDIT MASK '_________,__'.
      OVERLAY i_archivo-impiva WITH '000000000000'.


      i_archivo-basimp = i_bset-hwbas.
      REPLACE ',' WITH space INTO i_archivo-basimp.
      REPLACE '.' WITH space INTO i_archivo-basimp.
      CONDENSE i_archivo-basimp NO-GAPS.
      WRITE i_archivo-basimp TO i_archivo-basimp RIGHT-JUSTIFIED.
      SHIFT i_archivo-basimp LEFT.
      WRITE i_archivo-basimp TO i_archivo-basimp
        USING EDIT MASK '_________,__'.
      OVERLAY i_archivo-basimp WITH '000000000000'.




      w_alicu = i_bset-kbetr / 10.
      WRITE w_alicu TO i_archivo-alicu RIGHT-JUSTIFIED.
      REPLACE ',' WITH space INTO i_archivo-alicu.
      REPLACE '.' WITH space INTO i_archivo-alicu.
      CONDENSE i_archivo-alicu NO-GAPS.
      WRITE i_archivo-alicu TO i_archivo-alicu RIGHT-JUSTIFIED.
      SHIFT i_archivo-alicu LEFT.
      WRITE i_archivo-alicu TO i_archivo-alicu
      USING EDIT MASK '__,__'.
      OVERLAY i_archivo-alicu WITH '000000'.

      i_archivo-impdet = i_bset-hwste.
      REPLACE ',' WITH space INTO i_archivo-impdet.
      REPLACE '.' WITH space INTO i_archivo-impdet.
      CONDENSE i_archivo-impdet NO-GAPS.
      WRITE i_archivo-impdet TO i_archivo-impdet RIGHT-JUSTIFIED.
      SHIFT i_archivo-impdet LEFT.
      WRITE i_archivo-impdet TO i_archivo-impdet
        USING EDIT MASK '_________,__'.
      OVERLAY i_archivo-impdet WITH '000000000000'.


      OVERLAY i_archivo-derinsp WITH '000000000,00'.

      i_archivo-montret = i_bset-hwste.
      REPLACE ',' WITH space INTO i_archivo-montret.
      REPLACE '.' WITH space INTO i_archivo-montret.
      CONDENSE i_archivo-montret NO-GAPS.
      WRITE i_archivo-montret TO i_archivo-montret RIGHT-JUSTIFIED.
      SHIFT i_archivo-montret LEFT.
      WRITE i_archivo-montret TO i_archivo-montret.
      WRITE i_archivo-montret TO i_archivo-montret
        USING EDIT MASK '_________,__'.
      IF i_bkpf-blart EQ 'DG' OR i_bkpf-blart EQ 'D2'.
        OVERLAY i_archivo-montret WITH '-00000000000'.
      ELSE.
        OVERLAY i_archivo-montret WITH '000000000000'.
      ENDIF.

      i_archivo-artcal = '009'.

      APPEND i_archivo.

    ENDIF.

  ENDLOOP.

ENDFORM. " buscar_datos


*&---------------------------------------------------------------------*
*&      Form  descarga_archivo
*&---------------------------------------------------------------------*

FORM descarga_archivo.

  DATA: lv_path TYPE string.

  lv_path = p_file.

  IF NOT p_pc IS INITIAL.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename = lv_path
      TABLES
        data_tab = i_archivo.
    IF sy-subrc <> 0.
    ENDIF.

  ELSE.

    OPEN DATASET p_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    LOOP AT i_archivo.
      TRANSFER i_archivo TO p_file.
    ENDLOOP.
    CLOSE DATASET p_file.

  ENDIF.

ENDFORM. " descarga_archivo
