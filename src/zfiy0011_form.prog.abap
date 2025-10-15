*&---------------------------------------------------------------------*
*&  Include           ZFIY0011_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  buscar_datos
*&---------------------------------------------------------------------*

FORM buscar_datos.

  DATA: w_monco TYPE vbrk-netwr,
        w_impiv TYPE bset-fwbas,
        w_impde TYPE bset-fwste,
        w_alicu TYPE bset-kbetr,
        w_basim TYPE konv-kwert,
        v_renglon(5) TYPE i.
*{   INSERT         DE5K901430                                        1
*tomar los valores de la tabla BKPF-AWKEY (sólo los 10 primeros dígitos) con estos datos ir a la tabla RBKP-BELNR y si el
*campo RBKP-STBLG<> blanco no levantar dicho valor en la levantada de documentos de la BKPF.

  data: w_belnr like LINE OF ti_belnr.
  data: w_belnr2 like LINE OF ti_belnr.

  SELECT awkey
    FROM bkpf
    into table ti_awkey
     WHERE bukrs EQ s_bukrs AND
            budat IN s_bldat.

       LOOP at ti_awkey INTO w_awkey.
       w_awkey2-awkey = w_awkey-awkey(10).
       APPEND w_awkey2 to ti_awkey2.
       ENDLOOP.

       SELECT belnr
         FROM rbkp
         INTO TABLE ti_belnr
         for ALL ENTRIES IN ti_awkey2
         WHERE belnr = ti_awkey2-awkey and
               stblg ne ' '.

    LOOP at ti_belnr INTO w_belnr.
           CONCATENATE w_belnr-belnr '2012' into w_belnr2-belnr.
           APPEND w_belnr2 to ti_belnr2.
           ENDLOOP.

  if  ti_belnr2 is NOT INITIAL.
           SELECT belnr
           from bkpf
           INTO TABLE ti_belnr3
           FOR ALL ENTRIES IN ti_belnr2
           WHERE awkey = ti_belnr2-belnr.

             ENDif.

*}   INSERT

  SELECT bukrs gjahr belnr bldat xblnr blart knumv
    FROM bkpf INTO TABLE i_bkpf
      WHERE bukrs EQ s_bukrs  AND
            budat IN s_bldat  AND
            blart IN ('KR', 'KY', 'RE', 'GB', 'AZ', 'IB', 'IC').

*{   INSERT         DE5K901430                                        2
 LOOP at ti_belnr3.
 delete i_bkpf WHERE belnr = ti_belnr3-belnr.
 ENDLOOP.
*}   INSERT

*{   REPLACE        DE5K901539                                        3
*\  CHECK sy-subrc = 0.
*CHECK sy-subrc = 0.
*}   REPLACE

  DATA ETL74C2R8401 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L74C2R1911 TYPE FAGL_T_FIELD.
LT_FIELDS_L74C2R1911 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'GJAHR' )
 ( LINE = 'BELNR' )
 ( LINE = 'LIFNR' )
 ).

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = I_BKPF[]
              I_WHERE_CLAUSE = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND LIFNR NE SPACE|
              IT_FIELDLIST = LT_FIELDS_L74C2R1911
    IMPORTING ET_BSEG = ETL74C2R8401
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL74C2R8401 ) > 0.
  CLEAR I_BSEG.
  TYPES: BEGIN OF TYL74C2R6112,
    BUKRS TYPE BSEG-BUKRS,
    GJAHR TYPE BSEG-GJAHR,
    BELNR TYPE BSEG-BELNR,
    LIFNR TYPE BSEG-LIFNR,
  END OF TYL74C2R6112.
  DATA: LML74C2R1088 TYPE TYL74C2R6112,
        LWL74C2R8132 LIKE LINE OF I_BSEG.
  LOOP AT ETL74C2R8401 REFERENCE INTO DATA(LDRL74C2R4038).
    LML74C2R1088-BUKRS = LDRL74C2R4038->BUKRS.
    LML74C2R1088-GJAHR = LDRL74C2R4038->GJAHR.
    LML74C2R1088-BELNR = LDRL74C2R4038->BELNR.
    LML74C2R1088-LIFNR = LDRL74C2R4038->LIFNR.
    LWL74C2R8132 = LML74C2R1088.
    APPEND LWL74C2R8132 TO I_BSEG.
  ENDLOOP.
  SY-DBCNT = LINES( ETL74C2R8401 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


  SELECT bukrs gjahr belnr kschl
* Modificado en 25.01.2011 - Diego
*        fwbas kbetr fwste shkzg
         hwbas kbetr hwste shkzg
* Fin 25.01.2011
    FROM bset INTO TABLE i_bset
      FOR ALL ENTRIES IN i_bkpf
      WHERE bukrs = i_bkpf-bukrs AND
            gjahr = i_bkpf-gjahr AND
            belnr = i_bkpf-belnr AND
            ktosl IN s_ktosl.

  SELECT lifnr stcd1 fityp stcd2 name1
    FROM lfa1 INTO TABLE i_kna1
      FOR ALL ENTRIES IN i_bseg
        WHERE lifnr = i_bseg-kunnr.

  SORT i_kna1 BY kunnr.
  SORT i_konv BY knumv kschl.
  v_renglon = 1.

  REFRESH i_descarga.

*{   REPLACE        DE5K901577                                        4
*\  LOOP AT i_bkpf.
  SORT i_bkpf by bukrs gjahr belnr.
  SORT i_bset by bukrs gjahr belnr.

  LOOP AT i_bset.


   read TABLE i_bkpf WITH key bukrs = i_bset-bukrs
                              gjahr = i_bset-gjahr
                              belnr = i_bset-belnr BINARY SEARCH.
 if sy-subrc = 0.
*}   REPLACE
    CLEAR:  i_archivo-qscod,
            i_archivo-cuit,
            i_archivo-fecha,
            i_archivo-nro1,
            i_archivo-nro2,
            i_archivo-imp,
            i_archivo-neg.
    SHIFT i_archivo-nro1 RIGHT DELETING TRAILING ' '.
    SHIFT i_archivo-nro2 RIGHT DELETING TRAILING ' '.

    i_archivo-nro1       = i_bkpf-xblnr(4).
    i_archivo-nro2       = i_bkpf-xblnr+5(8).
    i_archivo-fecha(2)   = i_bkpf-bldat+6(2).
    i_archivo-fecha+2(1) = '/'.
    i_archivo-fecha+3(2) = i_bkpf-bldat+4(2).
    i_archivo-fecha+5(1) = '/'.
    i_archivo-fecha+6(4) = i_bkpf-bldat(4).

    i_archivo-qscod = '493'.
    OVERLAY i_archivo-nro1  WITH '000000000000'.
    OVERLAY i_archivo-nro2  WITH '000000000000'.

*{   DELETE         DE5K901619                                       14
*\    CLEAR i_bset.
*}   DELETE

*{   DELETE         DE5K901589                                        7
*\    READ TABLE i_bset WITH KEY bukrs = i_bkpf-bukrs
*}   DELETE
*{   DELETE         DE5K901589                                        8
*\                               gjahr = i_bkpf-gjahr
*\                               belnr = i_bkpf-belnr.
*}   DELETE
*{   INSERT         DE5K901603                                       13

*
*READ TABLE i_bset WITH KEY bukrs = i_bkpf-bukrs
*                           gjahr = i_bkpf-gjahr
*                           belnr = i_bkpf-belnr.
*}   INSERT

* Modificado en 25.01.2011 - Diego
*  WRITE i_bset-fwste TO i_archivo-imp RIGHT-JUSTIFIED.
   WRITE i_bset-hwste TO i_archivo-imp RIGHT-JUSTIFIED.
* Fin 25.01.2011
    REPLACE ',' WITH space INTO i_archivo-imp.
    REPLACE '.' WITH space INTO i_archivo-imp.
    CONDENSE i_archivo-imp NO-GAPS.
    WRITE i_archivo-imp TO i_archivo-imp RIGHT-JUSTIFIED.
    SHIFT i_archivo-imp LEFT.
    WRITE i_archivo-imp TO i_archivo-imp
      USING EDIT MASK '_____________.__'.
    OVERLAY i_archivo-imp  WITH '0000000000000000'.

    IF i_bset-shkzg = 'H'.
      i_archivo-neg = '*'.
    ENDIF.

    CLEAR i_bseg.

    READ TABLE i_bseg WITH KEY bukrs = i_bkpf-bukrs
                               gjahr = i_bkpf-gjahr
                               belnr = i_bkpf-belnr.
    CLEAR i_kna1.
*{   REPLACE        DE5K901601                                       11
*\    READ TABLE i_kna1 WITH KEY kunnr = i_bseg-kunnr BINARY SEARCH.
    READ TABLE i_kna1 WITH KEY kunnr = i_bseg-kunnr BINARY SEARCH.

*}   REPLACE
*{   INSERT         DE5K901601                                       10
*
*}   INSERT
*{   DELETE         DE5K901619                                       15
*\    IF sy-subrc = 0
*}   DELETE
* Incluído en 25.01.2011 - Diego
*{   DELETE         DE5K901619                                       17
*\      and not i_kna1-stcd1 is initial
*\      and not i_kna1-stcd1 eq '0'.
*}   DELETE
* Fin 25.01.2011
      WRITE i_kna1-stcd1 TO i_archivo-cuit    USING EDIT MASK '__-________-_'.
*{   DELETE         DE5K901619                                       16
*\    ENDIF.
*}   DELETE

* Modificado en 25.01.2011 - Diego
*   IF i_bset-fwste NE 0.
*{   DELETE         DE5K901599                                        9
*\    IF i_bset-hwste NE 0.
*}   DELETE
* Fin 25.01.2011
      APPEND i_archivo.
      CLEAR st_salida.

      MOVE: i_bkpf-bukrs    TO st_salida-bukrs,
            i_bkpf-belnr    TO st_salida-belnr,
            i_bkpf-gjahr    TO st_salida-gjahr,

* Inicio GB - 01/09/2010

            i_bkpf-blart    TO st_salida-blart,

* Fin GB - 01/09/2010

            i_archivo-cuit  TO st_salida-cuit ,
            i_kna1-name1    TO st_salida-name1,
            i_bset-kschl    TO st_salida-kschl,
* Modificado en 25.01.2011 - Diego
*           i_bset-fwbas    TO st_salida-fwbas,
            i_bset-hwbas    TO st_salida-hwbas,
* Fin 25.01.2011
            i_bset-kbetr    TO st_salida-kbetr,
* Modificado en 25.01.2011 - Diego
*           i_bset-fwste    TO st_salida-fwste.
            i_bset-hwste    TO st_salida-hwste.
* Fin 25.01.2011
      APPEND st_salida TO t_salida.



      CLEAR i_descarga-linea.
      CONCATENATE
                 i_archivo-qscod
                 i_archivo-cuit
                 i_archivo-fecha
                 i_archivo-nro1
                 i_archivo-nro2
                 i_archivo-imp

* Inicio GB - 01/09/2010

                 i_archivo-neg

* Fin GB - 01/09/2010

            INTO i_descarga-linea.

      APPEND i_descarga.
*{   INSERT         DE5K901581                                        6
      ELSE.
        CONTINUE.
   ENDIF.
*}   INSERT
*{   DELETE         DE5K901601                                       12
*\    ENDIF.
*}   DELETE
  ENDLOOP.
*{   INSERT         DE5K901579                                        5

*}   INSERT

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
        filetype = 'DAT' "VSS
      TABLES
        data_tab = i_descarga.
    IF sy-subrc <> 0.
    ENDIF.

  ELSE.

    OPEN DATASET p_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    LOOP AT i_archivo.
      TRANSFER i_descarga TO p_file.
    ENDLOOP.
    CLOSE DATASET p_file.

  ENDIF.
ENDFORM. " descarga_archivo
