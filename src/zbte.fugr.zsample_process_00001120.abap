FUNCTION zsample_process_00001120.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_BKDF) TYPE  BKDF OPTIONAL
*"  TABLES
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEG STRUCTURE  BSEG
*"      T_BKPFSUB STRUCTURE  BKPF_SUBST
*"      T_BSEGSUB STRUCTURE  BSEG_SUBST
*"      T_BSEC STRUCTURE  BSEC OPTIONAL
*"  CHANGING
*"     REFERENCE(I_BKDFSUB) TYPE  BKDF_SUBST OPTIONAL
*"----------------------------------------------------------------------
*  message e000(zfi) with 'teste bte'.
  DATA: t_ekko TYPE ekko,
        t_ekpo TYPE ekpo,
        t_t052 TYPE t052,
        t_bsik TYPE bsik OCCURS 0 WITH HEADER LINE,
        t_bsak TYPE bsak OCCURS 0 WITH HEADER LINE,
        ti_bkpf TYPE bkpf OCCURS 0 WITH HEADER LINE,
        t_lfa1 TYPE lfa1.

  DATA: xadtoreal TYPE bsak-dmbtr,
        xvlradto  TYPE bsak-dmbtr,
        xperc     TYPE t052-zprz1.
  break abap.
  IF sy-tcode = 'FBA6'.

    LOOP AT t_bseg.

      SELECT *
        FROM lfa1
        INTO t_lfa1
        WHERE lifnr = t_bseg-lifnr.
      ENDSELECT.

      IF sy-subrc = 0.

        IF t_lfa1-ktokk = 'ZFNF' OR
           t_lfa1-ktokk = 'ZFNJ'.

          IF t_bseg-ebeln IS INITIAL.

            MESSAGE e021(zfi).
*   Informar o número do Pedido de Compra

          ENDIF.

        ENDIF.

      ENDIF.

      SELECT *
        FROM ekko
        INTO t_ekko
        WHERE ebeln = t_bseg-ebeln.
      ENDSELECT.

      IF sy-subrc = 0.

        SELECT *
          FROM t052
          INTO t_t052
          WHERE zterm = t_ekko-zterm.
        ENDSELECT.

      ENDIF.

      SELECT *
        FROM ekpo
        INTO t_ekpo
        WHERE ebeln = t_bseg-ebeln.
      ENDSELECT.

      xperc = t_t052-zprz1 / 100.

      SELECT *
        FROM bsik
        INTO TABLE t_bsik
        WHERE bukrs = t_ekko-bukrs
          AND lifnr = t_ekko-lifnr
          AND umskz = 'F'
          AND ebeln = t_ekpo-ebeln
          AND ebelp = t_ekpo-ebelp.

      IF sy-subrc = 0.

        LOOP AT t_bsik.

          xadtoreal = xadtoreal + t_bsik-dmbtr.

        ENDLOOP.

      ELSE.

        SELECT *
          FROM bsak
          INTO TABLE t_bsak
          WHERE bukrs = t_ekko-bukrs
            AND lifnr = t_ekko-lifnr
            AND umskz = 'F'
            AND ebeln = t_ekpo-ebeln
            AND ebelp = t_ekpo-ebelp.

        IF sy-subrc = 0.

          SELECT *
            FROM bkpf
            INTO TABLE ti_bkpf
            FOR ALL ENTRIES IN t_bsak
            WHERE bukrs = t_bsak-bukrs
              AND belnr = t_bsak-augbl
              AND gjahr = t_bsak-gjahr.

          LOOP AT t_bsak.

            READ TABLE ti_bkpf WITH KEY bukrs = t_bsak-bukrs
                                       belnr = t_bsak-augbl
                                       gjahr = t_bsak-gjahr
                                       tcode = 'FB08'.

            IF sy-subrc <> 0.

              xadtoreal = xadtoreal + t_bsak-dmbtr.

            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

      xvlradto = ( t_ekpo-netwr * xperc ) - xadtoreal.

      IF t_bseg-dmbtr > xvlradto.

        MESSAGE e020(zfi).
*   O Valor do adiantamento informado é maior que o permitido.

      ENDIF.

      CLEAR: xadtoreal, t_bsak, t_bsik.

    ENDLOOP.

  ENDIF.

ENDFUNCTION.
