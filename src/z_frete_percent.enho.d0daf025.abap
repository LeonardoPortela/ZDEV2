"Name: \PR:SAPLV61A\FO:USEREXIT_XKOMV_BEWERTEN_END\SE:BEGIN\EI
ENHANCEMENT 0 Z_FRETE_PERCENT.
*======================================================================*
*                      HISTORICO DE MUDANÇAS                           *
*======================================================================*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*======================================================================*
*15/10/2024|NSEGATIN  |DEVK9A232E   |Cálc. impostos usando fórmula 961 *
*----------------------------------------------------------------------*
"Frete Lotação Também
IF xkomv[] IS INITIAL.
  EXIT.
ENDIF.
DATA: xkomv_aux   TYPE STANDARD TABLE OF komv_index WITH HEADER LINE.
DATA: fkbetr      TYPE komv-kbetr.
DATA: skbetr      TYPE komv-kbetr.

IF preisfindungsart NE 'E' AND komp-kposn NE 0
                                AND xkomv-kinak NE 'A'.
  IF ( sy-tcode+0(2) = 'VA') OR ( sy-tcode+0(2) = 'VF').
    xkomv_aux[] = xkomv[].
    LOOP AT xkomv.
      IF 'ZFUN_ZFUO_ZFU1_ZFU2' CS xkomv-kschl AND xkomv-kschl IS NOT INITIAL .
        READ TABLE xkomv_aux WITH KEY kschl = 'IBRX' .
        IF  xkomv_aux-kawrt GT 0.
          IF xkomv-kschl = 'ZFUN'.
            fkbetr = xkomv-kbetr.
          ENDIF.
          xkomv-kwert = ( xkomv_aux-kawrt * fkbetr ) / 10000.
          IF xkomv-kbetr LT 0.
            MULTIPLY xkomv-kwert BY -1.
          ENDIF.
          MODIFY xkomv.
        ENDIF.
      ELSEIF 'ZSEN_ZSEO_ZSE1_ZSE2' CS xkomv-kschl AND xkomv-kschl IS NOT INITIAL .
        IF xkomv-kschl = 'ZSEN'.
          skbetr = xkomv-kbetr.
        ENDIF.
        xkomv-kwert = ( ykmeng * skbetr ) / 100000.
        IF xkomv-kbetr LT 0.
          MULTIPLY xkomv-kwert BY -1.
        ENDIF.
        MODIFY xkomv.
      ENDIF.
    ENDLOOP.
    "
**<<<------"152475 - NMS - INI------>>>
* Cálculo do Imposto Retido.
    LOOP AT xkomv WHERE kofrm EQ '0000961'.
      DATA(lv_tabix) = sy-tabix.
* calculation formula
      IF xkomv-kofrm NE 0.
        IF xkomv-kofrm LE 999.
          xkwert = xkomv-kwert.
          frm_kondi_wert-nr = xkomv-kofrm.
          PERFORM (frm_kondi_wert) IN PROGRAM saplv61a IF FOUND.
          xkomv-kwert = xkwert.
          MODIFY xkomv INDEX lv_tabix TRANSPORTING kwert.
          CLEAR xkomv.
        ENDIF.

      ENDIF.

    ENDLOOP.
**<<<------"152475 - NMS - FIM------>>>
  ENDIF.
ENDIF.

*-#133089-21.02.2024-JT-inicio
DATA: t_callstack  TYPE abap_callstack,
      lc_fat_autom TYPE char01.

CALL FUNCTION 'SYSTEM_CALLSTACK'
  EXPORTING
    max_level = 0
  IMPORTING
    callstack = t_callstack.

*------------------
* faturamento automatico
*------------------
READ TABLE t_callstack INTO DATA(w_callstack) WITH KEY mainprogram = 'ZLESR0180_JOB'.
IF sy-subrc = 0.
  lc_fat_autom = abap_true.
ENDIF.
*-#133089-21.02.2024-JT-fim

IF sy-tcode EQ 'VI01'
OR sy-tcode EQ 'VI02'
OR sy-tcode EQ 'VI03'
OR sy-tcode EQ 'VT01N'
OR sy-tcode EQ 'VT01'
OR sy-tcode EQ 'VT02N'
OR sy-tcode EQ 'VT02'
OR sy-tcode EQ 'VT03N'
OR sy-tcode EQ 'VT03'
OR sy-tcode EQ 'ZLES0136'
OR sy-tcode EQ 'ZLES0115'
OR lc_fat_autom = abap_true.  "*-#133089-21.02.2024-JT

  FIELD-SYMBOLS: <field> TYPE ANY TABLE.
  FIELD-SYMBOLS: <field2> TYPE any.

  TYPES : BEGIN OF y_vbfa,
            vbelv   TYPE vbfa-vbelv,
            posnv   TYPE vbfa-posnv,
            vbeln   TYPE j_1bnflin-refkey,
            posnn   TYPE j_1bnflin-itmnum,
            vbtyp_n TYPE vbfa-vbtyp_n,
            vbtyp_v TYPE vbfa-vbtyp_v,
          END OF y_vbfa,

          BEGIN OF y_vttp,
            tknum TYPE vttk-tknum,
            vbeln TYPE vttp-vbeln,
          END OF y_vttp,

          BEGIN OF ty_vbfa.
            INCLUDE STRUCTURE vbfa.
  TYPES: refkey TYPE j_1bnflin-refkey,
          END OF ty_vbfa.


  DATA: ti_vfsi      TYPE TABLE OF vfsivb  WITH HEADER LINE,
        ti_vttp      TYPE TABLE OF y_vttp  WITH HEADER LINE,
        ti_vbfa      TYPE TABLE OF ty_vbfa    WITH HEADER LINE,
        ti_bnflin    TYPE TABLE OF j_1bnflin  WITH HEADER LINE,
        wa_bnflin    TYPE j_1bnflin,
        wl_lfa1_aux  TYPE lfa1,
        wl_a911      TYPE a911,
        wl_konp      TYPE konp,
        wl_vttk      TYPE vttk,
        wl_vttk_aux  TYPE vttk,
        vl_tknum     TYPE vttk-tknum,
        vl_field(20) TYPE c,
        wl_total     TYPE j_1bnflin-netwr,
        wl_vlr_aux2  TYPE j_1bnflin-netwr,
        wl_vlr_aux   TYPE j_1bnflin-netwr,
        wa_komv      TYPE konv.

* Determinação do documento de transporte
  vl_field = '(SAPLV54B)G_VFSI[]'.
  ASSIGN (vl_field) TO <field>.

  CLEAR: vl_tknum, vl_field, ti_vfsi, ti_vttp, wl_total, wl_a911, wl_vttk, wl_vttk_aux, wl_konp.

  REFRESH: ti_vfsi, ti_vttp, ti_vbfa, ti_bnflin.

* Controle de remessa
  IF <field> IS  ASSIGNED.

    ti_vfsi[] = <field>[].

    IF ti_vfsi[] IS NOT INITIAL.
* Condições: dados independentes da dimensão
      SELECT tknum vbeln
      INTO TABLE ti_vttp
      FROM vttp
      FOR ALL ENTRIES IN ti_vfsi
      WHERE vbeln = ti_vfsi-vbeln.

*---> 06/07/2023 - Migração S4 - WS
      SORT ti_vttp.
*<--- 06/07/2023 - Migração S4 - WS
      DELETE ADJACENT DUPLICATES FROM ti_vttp.

      IF ti_vttp[] IS NOT INITIAL.
        SELECT *
          FROM vttk UP TO 1 ROWS
          INTO wl_vttk
           FOR ALL ENTRIES IN ti_vttp
            WHERE tknum EQ ti_vttp-tknum.

        ENDSELECT.
      ENDIF.

      ASSIGN ('(SAPMV56A)VTTK') TO <field2>.
      IF <field2> IS ASSIGNED.
        MOVE-CORRESPONDING: <field2> TO wl_vttk_aux .
      ENDIF.
      IF wl_vttk-shtyp IS INITIAL.
        MOVE wl_vttk_aux-shtyp TO wl_vttk-shtyp.
      ENDIF.
      IF wl_vttk-tdlnr IS INITIAL.
        MOVE wl_vttk_aux-tdlnr TO wl_vttk-tdlnr.
      ENDIF.

      IF wl_vttk-route IS INITIAL.
        MOVE wl_vttk_aux-route TO wl_vttk-route.
      ENDIF.

      IF wl_vttk-sdabw IS INITIAL.
        MOVE wl_vttk_aux-sdabw TO wl_vttk-sdabw.
      ENDIF.
*break-point.

      SELECT SINGLE *
        FROM a911
        INTO  wl_a911
       WHERE kappl EQ 'F'
         AND kschl EQ 'ZFRE'
         AND sdabw EQ '0003' "frete percentual
         AND shtyp EQ wl_vttk-shtyp
         AND route EQ wl_vttk-route
         AND tdlnr EQ wl_vttk-tdlnr
         AND datbi GE sy-datum
         AND datab LE sy-datum.

      IF sy-subrc IS INITIAL.

        SELECT *
          FROM vbfa
          INTO TABLE ti_vbfa
           FOR ALL ENTRIES IN ti_vfsi
           WHERE vbelv EQ ti_vfsi-vbeln
             AND vbtyp_n EQ 'M'
             AND vbtyp_v EQ 'J'.

        IF sy-subrc IS INITIAL.
          LOOP AT ti_vbfa.
            MOVE: ti_vbfa-vbeln TO ti_vbfa-refkey.
            MODIFY ti_vbfa.
          ENDLOOP.

          SELECT *
            FROM j_1bnflin
            INTO TABLE ti_bnflin
            FOR ALL ENTRIES IN ti_vbfa
          WHERE refkey EQ ti_vbfa-refkey.


          IF ( sy-subrc EQ 0 ).

            DATA: qtd_item TYPE konv-kposn.

            LOOP AT ti_bnflin.

              CLEAR: wl_total.

              ADD ti_bnflin-netwr TO wl_total.

              ADD 1 TO qtd_item.

              DELETE xkomv WHERE kschl EQ 'ZFRE' AND kinak EQ 'Y'.

              LOOP AT xkomv WHERE kschl EQ 'ZFRE' AND kposn EQ qtd_item.

                IF xkomv-kinak NE 'Y'.

                  SELECT SINGLE * FROM konp
                    INTO wl_konp
                   WHERE knumh EQ wl_a911-knumh
                     AND loevm_ko EQ ''.

                  IF sy-subrc = 0.
                    wl_vlr_aux = ( wl_konp-kbetr * 100 ).
                    wl_vlr_aux = ( wl_vlr_aux / 1000 ).
                    wl_konp-kbetr =  wl_vlr_aux.
                    xkomv-kawrt =  wl_total.
                    xkomv-kwert = ( ( wl_total *  wl_konp-kbetr ) / 10000 ) .

                    MODIFY xkomv.
                  ENDIF.

                ENDIF.

                CLEAR: wl_vlr_aux,wl_total, xkomv-kawrt, xkomv-kwert.
              ENDLOOP.
            ENDLOOP.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


ENDIF.

ENDENHANCEMENT.
