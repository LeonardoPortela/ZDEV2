*&---------------------------------------------------------------------*
*& Include ZFI_COMPENSACAO_NFE_AUTO
*&---------------------------------------------------------------------*
**BUG solto FI 184553 - Compensação de documento de faturamento sem NF Autorizada - RGA - 28.07.2025

  DATA lv_fatura TYPE vbfa-vbeln.
  DATA ls_nflin  TYPE j_1bnflin.
  DATA lv_docnum TYPE j_1bnflin-docnum.



  IF sy-tcode EQ 'FB05' OR sy-tcode EQ 'FB1D'.

    IF xbseg-vbel2 IS NOT INITIAL.

      " Buscar documentos de faturamento
      SELECT SINGLE vbeln
        FROM vbfa
        INTO lv_fatura
        WHERE vbelv = xbseg-vbel2
          AND vbtyp_n = 'M'.

      IF sy-subrc EQ 0.

        " Buscar NFe relacionadas
        SELECT SINGLE lin~docnum
          FROM j_1bnflin AS lin
            INNER JOIN j_1bnfdoc AS doc
            ON doc~docnum EQ lin~docnum
            INNER JOIN j_1bnfe_active AS jact
            ON jact~docnum EQ lin~docnum
          INTO lv_docnum
          WHERE lin~refkey = lv_fatura
            AND   doc~nfe    = 'X'
            AND ( doc~form = 'NF55'
            OR    doc~form = 'NF57' )
            AND   jact~docsta = '1'
            AND   jact~code   = '100'. "autorizada

        IF sy-subrc NE 0.
          MESSAGE e134(zfi) WITH xausz3-belnr.
        ENDIF.

      ENDIF.

    ENDIF.
  ENDIF.
