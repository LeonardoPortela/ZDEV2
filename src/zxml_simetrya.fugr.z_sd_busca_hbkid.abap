FUNCTION z_sd_busca_hbkid.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(DOC_NUMERO) TYPE  J_1BDOCNUM
*"  EXPORTING
*"     REFERENCE(HBKID) TYPE  HBKID
*"----------------------------------------------------------------------

  DATA: lva_hbkid TYPE t012-hbkid.

  DATA: lwa_j_1bnflin  TYPE j_1bnflin,
        lwa_vbfa       TYPE vbfa,
        lwa_zsdt0053   TYPE zsdt0053,
        lwa_zsdt0052   TYPE zsdt0052,
        lwa_zfiwrt0008 TYPE zfiwrt0008,
        lwa_zfiwrt0011 TYPE zfiwrt0011.

* selecionar refkey
  SELECT SINGLE * INTO lwa_j_1bnflin
    FROM j_1bnflin
   WHERE docnum EQ doc_numero.

  IF sy-subrc = 0.

    SELECT SINGLE * INTO lwa_vbfa
      FROM vbfa
     WHERE  vbeln = lwa_j_1bnflin-refkey
      AND vbtyp_v = 'C'
      AND vbtyp_n = 'M'.

    IF sy-subrc = 0.

      SELECT SINGLE * INTO lwa_zsdt0053
        FROM zsdt0053
       WHERE vbeln = lwa_vbfa-vbelv.

      IF sy-subrc = 0.

        SELECT SINGLE * INTO lwa_zsdt0052
          FROM zsdt0052
         WHERE nro_sol_ov = lwa_zsdt0053-nro_sol_ov.

        IF lwa_zsdt0052 IS NOT INITIAL.
          lva_hbkid =   lwa_zsdt0052-hbkid.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

* BUG - 85053 - Inicio
  IF lva_hbkid IS INITIAL.

    SELECT SINGLE * INTO lwa_zfiwrt0008
      FROM zfiwrt0008
     WHERE docnum EQ doc_numero.

    IF sy-subrc = 0.

      SELECT SINGLE * INTO lwa_zfiwrt0011
        FROM zfiwrt0011
      WHERE  seq_lcto  = lwa_zfiwrt0008-seq_lcto
        AND zlsch = 'D'
        AND estorno <> 'X'.

      lva_hbkid =   lwa_zfiwrt0011-hbkid.

    ENDIF.
  ENDIF.

  hbkid = lva_hbkid .

ENDFUNCTION.
