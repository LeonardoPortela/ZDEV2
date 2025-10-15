"Name: \PR:SAPLV61A\FO:FRM_KOND_BASIS_900\SE:BEGIN\EI
ENHANCEMENT 0 Z_IRRF.
*
  FIELD-SYMBOLS: <field_2> TYPE ANY TABLE.

* Definição de tipo ----------------------------------------------------
  TYPES :
     BEGIN OF y_vbfa_2,
             vbelv    TYPE vbfa-vbelv,
             posnv    TYPE vbfa-posnv,
             vbeln    TYPE j_1bnflin-refkey,
             posnn    TYPE j_1bnflin-itmnum,
             vbtyp_n  TYPE vbfa-vbtyp_n,
             vbtyp_v  TYPE vbfa-vbtyp_v,
     END OF y_vbfa_2,

     BEGIN OF y_lfbw_2,
             lifnr      TYPE lfbw-lifnr,
             witht      TYPE lfbw-witht,
             wt_withcd  TYPE lfbw-wt_withcd,
     END OF y_lfbw_2,

     BEGIN OF y_vttp_2,
           tknum   TYPE vttk-tknum,
     END OF y_vttp_2.


  DATA:  ti_0059_2      TYPE TABLE OF t059fb WITH HEADER LINE,
         ti_komv_2      TYPE TABLE OF komv   WITH HEADER LINE,
         ti_vfsi_2      TYPE TABLE OF vfsivb WITH HEADER LINE,
         ti_vttp_2      TYPE TABLE OF y_vttp_2 WITH HEADER LINE,

         vl_vbfa_2      TYPE y_vbfa_2,
         vl_lfbw_2      TYPE y_lfbw_2,
         vl_tknum_2     TYPE vttk-tknum,
         vl_lifnr_2     TYPE vtpa-lifnr,
         vl_wtbex_2     TYPE t059minmax-wt_wtbex,
         vl_field_2(20) TYPE c.


  CLEAR: vl_vbfa_2, vl_lfbw_2, vl_tknum_2, vl_lifnr_2, vl_wtbex_2, ti_0059_2, ti_komv_2.

  REFRESH: ti_0059_2, ti_komv_2.

  ti_komv_2[] = xkomv[].
  DELETE ti_komv_2 WHERE kschl <> 'ZNFF'.


* Determinação do documento de transporte
  vl_field_2 = '(SAPLV54B)G_VFSI[]'.
  ASSIGN (vl_field_2) TO <field_2>.

* Controle de remessa
  IF <field_2> IS  ASSIGNED.
    ti_vfsi_2[] = <field_2>[].

* Condições: dados independentes da dimensão
    SELECT tknum
    INTO TABLE ti_vttp_2
    FROM vttp
    FOR ALL ENTRIES IN ti_vfsi_2
    WHERE vbeln = ti_vfsi_2-vbeln.

    IF sy-subrc IS INITIAL.

*---> 06/07/2023 - Migração S4 - WS
  SORT ti_vttp_2.
*<--- 06/07/2023 - Migração S4 - WS
      DELETE ADJACENT DUPLICATES FROM ti_vttp_2.

* Documento de transporte
      READ TABLE ti_vttp_2 INTO vl_tknum_2 INDEX 1.

* Proprietário do veículo
      SELECT SINGLE lifnr
      INTO   vl_lifnr_2
      FROM   vtpa
      WHERE  vbeln =  vl_tknum_2     AND
             posnr = '000000'      AND
             parvw = 'PV'.

      IF sy-subrc IS INITIAL.

* Proprietário do veículo
        SELECT SINGLE lifnr witht wt_withcd
        INTO   vl_lfbw_2
        FROM   lfbw
        WHERE  lifnr  = vl_lifnr_2      AND
               witht  IN ('CI', 'IC').

        IF sy-subrc IS INITIAL.

* Montantes mínimos/máximos por código IRF
          SELECT *
          INTO  TABLE ti_0059_2
          FROM  t059fb
          WHERE land1     = 'BR'                 AND
                witht     = vl_lfbw_2-witht        AND
                wt_withcd = vl_lfbw_2-wt_withcd
          ORDER BY wt_valid DESCENDING
                   wt_bbasb ASCENDING.

          IF sy-subrc IS INITIAL.

            READ TABLE ti_komv_2 INDEX 1.

            LOOP AT ti_0059_2.
              IF ti_0059_2-wt_bbasb > ti_komv_2-kawrt.  "#EC CI_FLDEXT_OK[2610650]
                EXIT.
              ENDIF.
            ENDLOOP.

            xkomv-kbetr = ti_0059_2-qsatz / 10.
            MODIFY xkomv.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CHECK 1 eq 2.

ENDENHANCEMENT.
