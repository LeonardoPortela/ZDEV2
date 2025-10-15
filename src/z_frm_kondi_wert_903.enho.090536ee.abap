"Name: \PR:SAPLV61A\FO:FRM_KONDI_WERT_903\SE:BEGIN\EI
ENHANCEMENT 0 Z_FRM_KONDI_WERT_903.


    FIELD-SYMBOLS: <field_1> TYPE ANY TABLE.

* Definição de tipo ----------------------------------------------------
  TYPES :
     BEGIN OF y_vbfa_1,
             vbelv    TYPE vbfa-vbelv,
             posnv    TYPE vbfa-posnv,
             vbeln    TYPE j_1bnflin-refkey,
             posnn    TYPE j_1bnflin-itmnum,
             vbtyp_n  TYPE vbfa-vbtyp_n,
             vbtyp_v  TYPE vbfa-vbtyp_v,
     END OF y_vbfa_1,

     BEGIN OF y_lfbw_1,
             lifnr      TYPE lfbw-lifnr,
             witht      TYPE lfbw-witht,
             wt_withcd  TYPE lfbw-wt_withcd,
     END OF y_lfbw_1,

     BEGIN OF y_vttp_1,
             tknum   TYPE vttk-tknum,
    END OF y_vttp_1.

  DATA:  ti_0059_1  TYPE TABLE OF t059fb  WITH HEADER LINE,
         ti_komv_1  TYPE TABLE OF komv    WITH HEADER LINE,
         ti_vttp_1  TYPE TABLE OF y_vttp_1  WITH HEADER LINE,
         ti_vfsi_1  TYPE TABLE OF vfsivb  WITH HEADER LINE,

         vl_vbfa_1  TYPE y_vbfa_1,
         vl_abfer_1 TYPE vttk-abfer,
         vl_tplst_1 TYPE vttk-TPLST,
         vl_lfbw_1  TYPE y_lfbw_1,
         vl_tknum_1 TYPE vttk-tknum,
         LWA_ZSDT_DEPARA_CEN TYPE ZSDT_DEPARA_CEN,

         vl_bukrs  TYPE j_1bbranch-bukrs,

         vl_lifnr_1 TYPE vtpa-lifnr,
         vl_wtbex_1 TYPE t059minmax-wt_wtbex,
         vl_field_1(20) TYPE c.


  CLEAR: vl_vbfa_1, vl_lfbw_1, vl_tknum_1, vl_lifnr_1, vl_wtbex_1, vl_field_1, vl_abfer_1,
         ti_komv_1, ti_0059_1, vl_bukrs.

  REFRESH: ti_0059_1,  ti_komv_1.

  ti_komv_1[] = xkomv[].
  DELETE ti_komv_1 WHERE kschl <> 'ZNFF'.

* Determinação do documento de transporte
  vl_field_1 = '(SAPLV54B)G_VFSI[]'.
  ASSIGN (vl_field_1) TO <field_1>.

* Controle de remessa
  IF <field_1> IS ASSIGNED.

    ti_vfsi_1[] = <field_1>[].

* Condições: dados independentes da dimensão
    SELECT tknum
    INTO TABLE ti_vttp_1
    FROM vttp
    FOR ALL ENTRIES IN ti_vfsi_1
    WHERE vbeln = ti_vfsi_1-vbeln.

    IF sy-subrc IS INITIAL.

      DELETE ADJACENT DUPLICATES FROM ti_vttp_1.

* Documento de transporte
      READ TABLE ti_vttp_1 INTO vl_tknum_1 INDEX 1.

* Condições: dados independentes da dimensão
      SELECT SINGLE abfer tplst
      INTO     ( vl_abfer_1 , vl_tplst_1 )
      FROM   vttk
      WHERE  tknum = vl_tknum_1.


         "( vl_abfer_1 , vl_tplst_1 )


      SELECT SINGLE bukrs
        FROM J_1BBRANCH  INTO vl_bukrs
       WHERE BRANCH = vl_tplst_1.

      IF SY-SUBRC NE 0.
        SELECT SINGLE *
          FROM ZSDT_DEPARA_CEN INTO LWA_ZSDT_DEPARA_CEN
         WHERE CENTROV_1 eq vl_tplst_1.

        IF SY-SUBRC eq 0 .
          SELECT SINGLE bukrs
            FROM J_1BBRANCH  INTO vl_bukrs
            WHERE BRANCH = LWA_ZSDT_DEPARA_CEN-centro_real.
        ENDIF.
      ENDIF.

      IF  vl_abfer_1 = '1' OR vl_abfer_1 = '3'.

* Proprietário do veículo
        SELECT SINGLE lifnr
        INTO   vl_lifnr_1
        FROM   vtpa
        WHERE  vbeln =  vl_tknum_1     AND
               posnr = '000000'      AND
               parvw = 'PV'.

* Proprietário do veículo
        SELECT SINGLE lifnr witht wt_withcd
        INTO   vl_lfbw_1
        FROM   lfbw
        WHERE  lifnr  = vl_lifnr_1      AND
               bukrs  = vl_bukrs AND
               witht  IN ( 'IC' ).

        IF sy-subrc IS INITIAL.

* Montantes mínimos/máximos por código IRF
          SELECT *
          INTO  TABLE ti_0059_1
          FROM  t059fb
          WHERE land1     = 'BR'                 AND
                witht     = vl_lfbw_1-witht        AND
                wt_withcd = vl_lfbw_1-wt_withcd
          ORDER BY wt_valid DESCENDING wt_bbasb ASCENDING.

          IF sy-subrc IS INITIAL.

            READ TABLE ti_komv_1 INDEX 1.

            LOOP AT ti_0059_1.
              IF ti_0059_1-wt_bbasb > ti_komv_1-kawrt.
                EXIT.
              ENDIF.
            ENDLOOP.

            xkomv-kbetr =  ti_0059_1-qmbar .
            xkomv-kwert =  ti_0059_1-qmbar .
            xkwert = xkomv-kwert.
            MODIFY xkomv.

          ELSE.

            xkomv-kbetr =  0.
            xkomv-kwert =  0.
            xkwert = xkomv-kwert.
            MODIFY xkomv.

          ENDIF.

        ELSE.

          xkomv-kbetr =  0.
          xkomv-kwert =  0.
          xkwert = xkomv-kwert.
          MODIFY xkomv.
        ENDIF.

      ELSE.

* Proprietário do veículo
        SELECT SINGLE lifnr
        INTO   vl_lifnr_1
        FROM   vtpa
        WHERE  vbeln =  vl_tknum_1    AND
               posnr = '000000'      AND
               parvw = 'PV'.

* Proprietário do veículo
        SELECT SINGLE lifnr witht wt_withcd
        INTO   vl_lfbw_1
        FROM   lfbw
        WHERE  lifnr  = vl_lifnr_1      AND
               bukrs  = vl_bukrs        AND
               witht  IN ( 'IC' ).

        IF sy-subrc IS INITIAL.

* Montantes mínimos/máximos por código IRF
          SELECT *
          INTO  TABLE ti_0059_1
          FROM  t059fb
          WHERE land1     = 'BR'                 AND
                witht     = vl_lfbw_1-witht        AND
                wt_withcd = vl_lfbw_1-wt_withcd
          ORDER BY wt_valid DESCENDING.

          IF sy-subrc IS INITIAL.

            READ TABLE ti_komv_1 INDEX 1.

            LOOP AT ti_0059_1.
              IF ti_0059_1-wt_bbasb > ti_komv_1-kwert.
                EXIT.
              ENDIF.
            ENDLOOP.

            xkomv-kbetr =  ti_0059_1-qmbar .
            xkomv-kwert =  ti_0059_1-qmbar .
            xkwert = xkomv-kwert.
            MODIFY xkomv.

          ELSE.

            xkomv-kbetr =  0.
            xkomv-kwert =  0.
            xkwert = xkomv-kwert.
            MODIFY xkomv.

          ENDIF.

        ELSE.

          xkomv-kbetr =  0.
          xkomv-kwert =  0.
          xkwert = xkomv-kwert.
          MODIFY xkomv.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.



  exit.


ENDENHANCEMENT.
